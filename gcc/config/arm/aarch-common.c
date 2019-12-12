/* Dependency checks for instruction scheduling, shared between ARM and
   AARCH64.

   Copyright (C) 1991-2019 Free Software Foundation, Inc.
   Contributed by ARM Ltd.

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
#include "insn-modes.h"
#include "tm.h"
#include "rtl.h"
#include "rtl-iter.h"
#include "memmodel.h"
#include "diagnostic.h"
#include "tree.h"
#include "expr.h"
#include "function.h"
#include "emit-rtl.h"

/* Return TRUE if X is either an arithmetic shift left, or
   is a multiplication by a power of two.  */
bool
arm_rtx_shift_left_p (rtx x)
{
  enum rtx_code code = GET_CODE (x);

  if (code == MULT && CONST_INT_P (XEXP (x, 1))
      && exact_log2 (INTVAL (XEXP (x, 1))) > 0)
    return true;

  if (code == ASHIFT)
    return true;

  return false;
}

static rtx_code shift_rtx_codes[] =
  { ASHIFT, ROTATE, ASHIFTRT, LSHIFTRT,
    ROTATERT, ZERO_EXTEND, SIGN_EXTEND };

/* Traverse PATTERN looking for a sub-rtx with RTX_CODE CODE.
   If FIND_ANY_SHIFT then we are interested in anything which can
   reasonably be described as a SHIFT RTX.  */
static rtx
arm_find_sub_rtx_with_code (rtx pattern, rtx_code code, bool find_any_shift)
{
  subrtx_var_iterator::array_type array;
  FOR_EACH_SUBRTX_VAR (iter, array, pattern, NONCONST)
    {
      rtx x = *iter;
      if (find_any_shift)
	{
	  /* Left shifts might have been canonicalized to a MULT of some
	     power of two.  Make sure we catch them.  */
	  if (arm_rtx_shift_left_p (x))
	    return x;
	  else
	    for (unsigned int i = 0; i < ARRAY_SIZE (shift_rtx_codes); i++)
	      if (GET_CODE (x) == shift_rtx_codes[i])
		return x;
	}

      if (GET_CODE (x) == code)
	return x;
    }
  return NULL_RTX;
}

/* Traverse PATTERN looking for any sub-rtx which looks like a shift.  */
static rtx
arm_find_shift_sub_rtx (rtx pattern)
{
  return arm_find_sub_rtx_with_code (pattern, ASHIFT, true);
}

/* PRODUCER and CONSUMER are two potentially dependant RTX.  PRODUCER
   (possibly) contains a SET which will provide a result we can access
   using the SET_DEST macro.  We will place the RTX which would be
   written by PRODUCER in SET_SOURCE.
   Similarly, CONSUMER (possibly) contains a SET which has an operand
   we can access using SET_SRC.  We place this operand in
   SET_DESTINATION.

   Return nonzero if we found the SET RTX we expected.  */
static int
arm_get_set_operands (rtx producer, rtx consumer,
		      rtx *set_source, rtx *set_destination)
{
  rtx set_producer = arm_find_sub_rtx_with_code (PATTERN (producer),
						 SET, false);
  rtx set_consumer = arm_find_sub_rtx_with_code (PATTERN (consumer),
						 SET, false);

  if (set_producer && set_consumer)
    {
      *set_source = SET_DEST (set_producer);
      *set_destination = SET_SRC (set_consumer);
      return 1;
    }
  return 0;
}

bool
aarch_rev16_shright_mask_imm_p (rtx val, machine_mode mode)
{
  return CONST_INT_P (val)
         && INTVAL (val)
            == trunc_int_for_mode (HOST_WIDE_INT_C (0xff00ff00ff00ff),
                                   mode);
}

bool
aarch_rev16_shleft_mask_imm_p (rtx val, machine_mode mode)
{
  return CONST_INT_P (val)
         && INTVAL (val)
            == trunc_int_for_mode (HOST_WIDE_INT_C (0xff00ff00ff00ff00),
                                   mode);
}


static bool
aarch_rev16_p_1 (rtx lhs, rtx rhs, machine_mode mode)
{
  if (GET_CODE (lhs) == AND
         && GET_CODE (XEXP (lhs, 0)) == ASHIFT
            && CONST_INT_P (XEXP (XEXP (lhs, 0), 1))
            && INTVAL (XEXP (XEXP (lhs, 0), 1)) == 8
            && REG_P (XEXP (XEXP (lhs, 0), 0))
         && CONST_INT_P (XEXP (lhs, 1))
      && GET_CODE (rhs) == AND
         && GET_CODE (XEXP (rhs, 0)) == LSHIFTRT
            && REG_P (XEXP (XEXP (rhs, 0), 0))
            && CONST_INT_P (XEXP (XEXP (rhs, 0), 1))
            && INTVAL (XEXP (XEXP (rhs, 0), 1)) == 8
         && CONST_INT_P (XEXP (rhs, 1))
      && REGNO (XEXP (XEXP (rhs, 0), 0)) == REGNO (XEXP (XEXP (lhs, 0), 0)))

    {
      rtx lhs_mask = XEXP (lhs, 1);
      rtx rhs_mask = XEXP (rhs, 1);

      return aarch_rev16_shright_mask_imm_p (rhs_mask, mode)
             && aarch_rev16_shleft_mask_imm_p (lhs_mask, mode);
    }

  return false;
}

/* Recognise a sequence of bitwise operations corresponding to a rev16 operation.
   These will be of the form:
     ((x >> 8) & 0x00ff00ff)
   | ((x << 8) & 0xff00ff00)
   for SImode and with similar but wider bitmasks for DImode.
   The two sub-expressions of the IOR can appear on either side so check both
   permutations with the help of aarch_rev16_p_1 above.  */

bool
aarch_rev16_p (rtx x)
{
  rtx left_sub_rtx, right_sub_rtx;
  bool is_rev = false;

  if (GET_CODE (x) != IOR)
    return false;

  left_sub_rtx = XEXP (x, 0);
  right_sub_rtx = XEXP (x, 1);

  /* There are no canonicalisation rules for the position of the two shifts
     involved in a rev, so try both permutations.  */
  is_rev = aarch_rev16_p_1 (left_sub_rtx, right_sub_rtx, GET_MODE (x));

  if (!is_rev)
    is_rev = aarch_rev16_p_1 (right_sub_rtx, left_sub_rtx, GET_MODE (x));

  return is_rev;
}

/* Return non-zero if the RTX representing a memory model is a memory model
   that needs acquire semantics.  */
bool
aarch_mm_needs_acquire (rtx const_int)
{
  enum memmodel model = memmodel_from_int (INTVAL (const_int));
  return !(is_mm_relaxed (model)
	   || is_mm_consume (model)
	   || is_mm_release (model));
}

/* Return non-zero if the RTX representing a memory model is a memory model
   that needs release semantics.  */
bool
aarch_mm_needs_release (rtx const_int)
{
  enum memmodel model = memmodel_from_int (INTVAL (const_int));
  return !(is_mm_relaxed (model)
	   || is_mm_consume (model)
	   || is_mm_acquire (model));
}

/* Return nonzero if the CONSUMER instruction (a load) does need
   PRODUCER's value to calculate the address.  */
int
arm_early_load_addr_dep (rtx producer, rtx consumer)
{
  rtx value, addr;

  if (!arm_get_set_operands (producer, consumer, &value, &addr))
    return 0;

  return reg_overlap_mentioned_p (value, addr);
}

/* Return nonzero if the CONSUMER instruction (a load) does need
   a Pmode PRODUCER's value to calculate the address.  */

int
arm_early_load_addr_dep_ptr (rtx producer, rtx consumer)
{
  rtx value = arm_find_sub_rtx_with_code (PATTERN (producer), SET, false);
  rtx addr = arm_find_sub_rtx_with_code (PATTERN (consumer), SET, false);

  if (!value || !addr || !MEM_P (SET_SRC (value)))
    return 0;

  value = SET_DEST (value);
  addr = SET_SRC (addr);

  return GET_MODE (value) == Pmode && reg_overlap_mentioned_p (value, addr);
}

/* Return nonzero if the CONSUMER instruction (an ALU op) does not
   have an early register shift value or amount dependency on the
   result of PRODUCER.  */
int
arm_no_early_alu_shift_dep (rtx producer, rtx consumer)
{
  rtx value, op;
  rtx early_op;

  if (!arm_get_set_operands (producer, consumer, &value, &op))
    return 0;

  if ((early_op = arm_find_shift_sub_rtx (op)))
    return !reg_overlap_mentioned_p (value, early_op);

  return 0;
}

/* Return nonzero if the CONSUMER instruction (an ALU op) does not
   have an early register shift value dependency on the result of
   PRODUCER.  */
int
arm_no_early_alu_shift_value_dep (rtx producer, rtx consumer)
{
  rtx value, op;
  rtx early_op;

  if (!arm_get_set_operands (producer, consumer, &value, &op))
    return 0;

  if ((early_op = arm_find_shift_sub_rtx (op)))
    /* We want to check the value being shifted.  */
    if (!reg_overlap_mentioned_p (value, XEXP (early_op, 0)))
      return 1;

  return 0;
}

/* Return nonzero if the CONSUMER (a mul or mac op) does not
   have an early register mult dependency on the result of
   PRODUCER.  */
int
arm_no_early_mul_dep (rtx producer, rtx consumer)
{
  rtx value, op;

  if (!arm_get_set_operands (producer, consumer, &value, &op))
    return 0;

  if (GET_CODE (op) == PLUS || GET_CODE (op) == MINUS)
    {
      if (GET_CODE (XEXP (op, 0)) == MULT)
	return !reg_overlap_mentioned_p (value, XEXP (op, 0));
      else
	return !reg_overlap_mentioned_p (value, XEXP (op, 1));
    }

  return 0;
}

/* Return nonzero if the CONSUMER instruction (a store) does not need
   PRODUCER's value to calculate the address.  */

int
arm_no_early_store_addr_dep (rtx producer, rtx consumer)
{
  rtx value = arm_find_sub_rtx_with_code (PATTERN (producer), SET, false);
  rtx addr = arm_find_sub_rtx_with_code (PATTERN (consumer), SET, false);

  if (value)
    value = SET_DEST (value);

  if (addr)
    addr = SET_DEST (addr);

  if (!value || !addr)
    return 0;

  return !reg_overlap_mentioned_p (value, addr);
}

/* Return nonzero if the CONSUMER instruction (a store) does need
   PRODUCER's value to calculate the address.  */

int
arm_early_store_addr_dep (rtx producer, rtx consumer)
{
  return !arm_no_early_store_addr_dep (producer, consumer);
}

/* Return nonzero if the CONSUMER instruction (a store) does need
   a Pmode PRODUCER's value to calculate the address.  */

int
arm_early_store_addr_dep_ptr (rtx producer, rtx consumer)
{
  rtx value = arm_find_sub_rtx_with_code (PATTERN (producer), SET, false);
  rtx addr = arm_find_sub_rtx_with_code (PATTERN (consumer), SET, false);

  if (!value || !addr || !MEM_P (SET_SRC (value)))
    return 0;

  value = SET_DEST (value);
  addr = SET_DEST (addr);

  return GET_MODE (value) == Pmode && reg_overlap_mentioned_p (value, addr);
}

/* Return non-zero iff the consumer (a multiply-accumulate or a
   multiple-subtract instruction) has an accumulator dependency on the
   result of the producer and no other dependency on that result.  It
   does not check if the producer is multiply-accumulate instruction.  */
int
arm_mac_accumulator_is_result (rtx producer, rtx consumer)
{
  rtx result;
  rtx op0, op1, acc;

  producer = PATTERN (producer);
  consumer = PATTERN (consumer);

  if (GET_CODE (producer) == COND_EXEC)
    producer = COND_EXEC_CODE (producer);
  if (GET_CODE (consumer) == COND_EXEC)
    consumer = COND_EXEC_CODE (consumer);

  if (GET_CODE (producer) != SET)
    return 0;

  result = XEXP (producer, 0);

  if (GET_CODE (consumer) != SET)
    return 0;

  /* Check that the consumer is of the form
     (set (...) (plus (mult ...) (...)))
     or
     (set (...) (minus (...) (mult ...))).  */
  if (GET_CODE (XEXP (consumer, 1)) == PLUS)
    {
      if (GET_CODE (XEXP (XEXP (consumer, 1), 0)) != MULT)
        return 0;

      op0 = XEXP (XEXP (XEXP (consumer, 1), 0), 0);
      op1 = XEXP (XEXP (XEXP (consumer, 1), 0), 1);
      acc = XEXP (XEXP (consumer, 1), 1);
    }
  else if (GET_CODE (XEXP (consumer, 1)) == MINUS)
    {
      if (GET_CODE (XEXP (XEXP (consumer, 1), 1)) != MULT)
        return 0;

      op0 = XEXP (XEXP (XEXP (consumer, 1), 1), 0);
      op1 = XEXP (XEXP (XEXP (consumer, 1), 1), 1);
      acc = XEXP (XEXP (consumer, 1), 0);
    }
  else
    return 0;

  return (reg_overlap_mentioned_p (result, acc)
          && !reg_overlap_mentioned_p (result, op0)
          && !reg_overlap_mentioned_p (result, op1));
}

/* Return non-zero if the destination of PRODUCER feeds the accumulator
   operand of an MLA-like operation.  */

int
aarch_accumulator_forwarding (rtx_insn *producer, rtx_insn *consumer)
{
  rtx producer_set = single_set (producer);
  rtx consumer_set = single_set (consumer);

  /* We are looking for a SET feeding a SET.  */
  if (!producer_set || !consumer_set)
    return 0;

  rtx dest = SET_DEST (producer_set);
  rtx mla = SET_SRC (consumer_set);

  /* We're looking for a register SET.  */
  if (!REG_P (dest))
    return 0;

  rtx accumulator;

  /* Strip a zero_extend.  */
  if (GET_CODE (mla) == ZERO_EXTEND)
    mla = XEXP (mla, 0);

  switch (GET_CODE (mla))
    {
    case PLUS:
      /* Possibly an MADD.  */
      if (GET_CODE (XEXP (mla, 0)) == MULT)
	accumulator = XEXP (mla, 1);
      else
	return 0;
      break;
    case MINUS:
      /* Possibly an MSUB.  */
      if (GET_CODE (XEXP (mla, 1)) == MULT)
	accumulator = XEXP (mla, 0);
      else
	return 0;
      break;
    case FMA:
	{
	  /* Possibly an FMADD/FMSUB/FNMADD/FNMSUB.  */
	  if (REG_P (XEXP (mla, 1))
	      && REG_P (XEXP (mla, 2))
	      && (REG_P (XEXP (mla, 0))
		  || GET_CODE (XEXP (mla, 0)) == NEG))

	    {
	      /* FMADD/FMSUB.  */
	      accumulator = XEXP (mla, 2);
	    }
	  else if (REG_P (XEXP (mla, 1))
		   && GET_CODE (XEXP (mla, 2)) == NEG
		   && (REG_P (XEXP (mla, 0))
		       || GET_CODE (XEXP (mla, 0)) == NEG))
	    {
	      /* FNMADD/FNMSUB.  */
	      accumulator = XEXP (XEXP (mla, 2), 0);
	    }
	  else
	    return 0;
	  break;
	}
      default:
	/* Not an MLA-like operation.  */
	return 0;
    }

  if (GET_CODE (accumulator) == SUBREG)
    accumulator = SUBREG_REG (accumulator);

  if (!REG_P (accumulator))
    return 0;

  return (REGNO (dest) == REGNO (accumulator));
}

/* Return non-zero if the consumer (a multiply-accumulate instruction)
   has an accumulator dependency on the result of the producer (a
   multiplication instruction) and no other dependency on that result.  */
int
arm_mac_accumulator_is_mul_result (rtx producer, rtx consumer)
{
  rtx mul = PATTERN (producer);
  rtx mac = PATTERN (consumer);
  rtx mul_result;
  rtx mac_op0, mac_op1, mac_acc;

  if (GET_CODE (mul) == COND_EXEC)
    mul = COND_EXEC_CODE (mul);
  if (GET_CODE (mac) == COND_EXEC)
    mac = COND_EXEC_CODE (mac);

  /* Check that mul is of the form (set (...) (mult ...))
     and mla is of the form (set (...) (plus (mult ...) (...))).  */
  if ((GET_CODE (mul) != SET || GET_CODE (XEXP (mul, 1)) != MULT)
      || (GET_CODE (mac) != SET || GET_CODE (XEXP (mac, 1)) != PLUS
          || GET_CODE (XEXP (XEXP (mac, 1), 0)) != MULT))
    return 0;

  mul_result = XEXP (mul, 0);
  mac_op0 = XEXP (XEXP (XEXP (mac, 1), 0), 0);
  mac_op1 = XEXP (XEXP (XEXP (mac, 1), 0), 1);
  mac_acc = XEXP (XEXP (mac, 1), 1);

  return (reg_overlap_mentioned_p (mul_result, mac_acc)
          && !reg_overlap_mentioned_p (mul_result, mac_op0)
          && !reg_overlap_mentioned_p (mul_result, mac_op1));
}

/* Worker function for TARGET_MD_ASM_ADJUST.
   We implement asm flag outputs.  */

rtx_insn *
arm_md_asm_adjust (vec<rtx> &outputs, vec<rtx> &/*inputs*/,
		    vec<const char *> &constraints,
		    vec<rtx> &/*clobbers*/, HARD_REG_SET &/*clobbered_regs*/)
{
  bool saw_asm_flag = false;

  start_sequence ();
  for (unsigned i = 0, n = outputs.length (); i < n; ++i)
    {
      const char *con = constraints[i];
      if (strncmp (con, "=@cc", 4) != 0)
	continue;
      con += 4;
      if (strchr (con, ',') != NULL)
	{
	  error ("alternatives not allowed in %<asm%> flag output");
	  continue;
	}

      machine_mode mode;
      rtx_code code;
      int con01 = 0;

#define C(X, Y)  (unsigned char)(X) * 256 + (unsigned char)(Y)

      /* All of the condition codes are two characters.  */
      if (con[0] != 0 && con[1] != 0 && con[2] == 0)
	con01 = C(con[0], con[1]);

      switch (con01)
	{
	case C('c', 'c'):
	case C('l', 'o'):
	  mode = CC_Cmode, code = GEU;
	  break;
	case C('c', 's'):
	case C('h', 's'):
	  mode = CC_Cmode, code = LTU;
	  break;
	case C('e', 'q'):
	  mode = CC_NZmode, code = EQ;
	  break;
	case C('g', 'e'):
	  mode = CCmode, code = GE;
	  break;
	case C('g', 't'):
	  mode = CCmode, code = GT;
	  break;
	case C('h', 'i'):
	  mode = CCmode, code = GTU;
	  break;
	case C('l', 'e'):
	  mode = CCmode, code = LE;
	  break;
	case C('l', 's'):
	  mode = CCmode, code = LEU;
	  break;
	case C('l', 't'):
	  mode = CCmode, code = LT;
	  break;
	case C('m', 'i'):
	  mode = CC_NZmode, code = LT;
	  break;
	case C('n', 'e'):
	  mode = CC_NZmode, code = NE;
	  break;
	case C('p', 'l'):
	  mode = CC_NZmode, code = GE;
	  break;
	case C('v', 'c'):
	  mode = CC_Vmode, code = EQ;
	  break;
	case C('v', 's'):
	  mode = CC_Vmode, code = NE;
	  break;
	default:
	  error ("unknown %<asm%> flag output %qs", constraints[i]);
	  continue;
	}

#undef C

      rtx dest = outputs[i];
      machine_mode dest_mode = GET_MODE (dest);
      if (!SCALAR_INT_MODE_P (dest_mode))
	{
	  error ("invalid type for %<asm%> flag output");
	  continue;
	}

      if (!saw_asm_flag)
	{
	  /* This is the first asm flag output.  Here we put the flags
	     register in as the real output and adjust the condition to
	     allow it.  */
	  constraints[i] = "=c";
	  outputs[i] = gen_rtx_REG (CCmode, CC_REGNUM);
	  saw_asm_flag = true;
	}
      else
	{
	  /* We don't need the flags register as output twice.  */
	  constraints[i] = "=X";
	  outputs[i] = gen_rtx_SCRATCH (word_mode);
	}

      rtx x = gen_rtx_REG (mode, CC_REGNUM);
      x = gen_rtx_fmt_ee (code, word_mode, x, const0_rtx);

      if (dest_mode == word_mode)
	emit_insn (gen_rtx_SET (dest, x));
      else
	{
	  rtx tmp = gen_reg_rtx (word_mode);
	  emit_insn (gen_rtx_SET (tmp, x));

	  tmp = convert_modes (dest_mode, word_mode, tmp, true);
	  emit_move_insn (dest, tmp);
	}
    }
  rtx_insn *seq = get_insns ();
  end_sequence ();

  return saw_asm_flag ? seq : NULL;
}
