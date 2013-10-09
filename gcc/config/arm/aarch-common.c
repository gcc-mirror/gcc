/* Dependency checks for instruction scheduling, shared between ARM and
   AARCH64.

   Copyright (C) 1991-2013 Free Software Foundation, Inc.
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


#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tm.h"
#include "tm_p.h"
#include "rtl.h"
#include "tree.h"
#include "c-family/c-common.h"
#include "rtl.h"

/* Return nonzero if the CONSUMER instruction (a load) does need
   PRODUCER's value to calculate the address.  */

int
arm_early_load_addr_dep (rtx producer, rtx consumer)
{
  rtx value = PATTERN (producer);
  rtx addr = PATTERN (consumer);

  if (GET_CODE (value) == COND_EXEC)
    value = COND_EXEC_CODE (value);
  if (GET_CODE (value) == PARALLEL)
    value = XVECEXP (value, 0, 0);
  value = XEXP (value, 0);
  if (GET_CODE (addr) == COND_EXEC)
    addr = COND_EXEC_CODE (addr);
  if (GET_CODE (addr) == PARALLEL)
    {
      if (GET_CODE (XVECEXP (addr, 0, 0)) == RETURN)
        addr = XVECEXP (addr, 0, 1);
      else
        addr = XVECEXP (addr, 0, 0);
    }
  addr = XEXP (addr, 1);

  return reg_overlap_mentioned_p (value, addr);
}

/* Return nonzero if the CONSUMER instruction (an ALU op) does not
   have an early register shift value or amount dependency on the
   result of PRODUCER.  */

int
arm_no_early_alu_shift_dep (rtx producer, rtx consumer)
{
  rtx value = PATTERN (producer);
  rtx op = PATTERN (consumer);
  rtx early_op;

  if (GET_CODE (value) == COND_EXEC)
    value = COND_EXEC_CODE (value);
  if (GET_CODE (value) == PARALLEL)
    value = XVECEXP (value, 0, 0);
  value = XEXP (value, 0);
  if (GET_CODE (op) == COND_EXEC)
    op = COND_EXEC_CODE (op);
  if (GET_CODE (op) == PARALLEL)
    op = XVECEXP (op, 0, 0);
  op = XEXP (op, 1);

  early_op = XEXP (op, 0);
  /* This is either an actual independent shift, or a shift applied to
     the first operand of another operation.  We want the whole shift
     operation.  */
  if (REG_P (early_op))
    early_op = op;

  return !reg_overlap_mentioned_p (value, early_op);
}

/* Return nonzero if the CONSUMER instruction (an ALU op) does not
   have an early register shift value dependency on the result of
   PRODUCER.  */

int
arm_no_early_alu_shift_value_dep (rtx producer, rtx consumer)
{
  rtx value = PATTERN (producer);
  rtx op = PATTERN (consumer);
  rtx early_op;

  if (GET_CODE (value) == COND_EXEC)
    value = COND_EXEC_CODE (value);
  if (GET_CODE (value) == PARALLEL)
    value = XVECEXP (value, 0, 0);
  value = XEXP (value, 0);
  if (GET_CODE (op) == COND_EXEC)
    op = COND_EXEC_CODE (op);
  if (GET_CODE (op) == PARALLEL)
    op = XVECEXP (op, 0, 0);
  op = XEXP (op, 1);

  early_op = XEXP (op, 0);

  /* This is either an actual independent shift, or a shift applied to
     the first operand of another operation.  We want the value being
     shifted, in either case.  */
  if (!REG_P (early_op))
    early_op = XEXP (early_op, 0);

  return !reg_overlap_mentioned_p (value, early_op);
}

/* Return nonzero if the CONSUMER (a mul or mac op) does not
   have an early register mult dependency on the result of
   PRODUCER.  */

int
arm_no_early_mul_dep (rtx producer, rtx consumer)
{
  rtx value = PATTERN (producer);
  rtx op = PATTERN (consumer);

  if (GET_CODE (value) == COND_EXEC)
    value = COND_EXEC_CODE (value);
  if (GET_CODE (value) == PARALLEL)
    value = XVECEXP (value, 0, 0);
  value = XEXP (value, 0);
  if (GET_CODE (op) == COND_EXEC)
    op = COND_EXEC_CODE (op);
  if (GET_CODE (op) == PARALLEL)
    op = XVECEXP (op, 0, 0);
  op = XEXP (op, 1);

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
  rtx value = PATTERN (producer);
  rtx addr = PATTERN (consumer);

  if (GET_CODE (value) == COND_EXEC)
    value = COND_EXEC_CODE (value);
  if (GET_CODE (value) == PARALLEL)
    value = XVECEXP (value, 0, 0);
  value = XEXP (value, 0);
  if (GET_CODE (addr) == COND_EXEC)
    addr = COND_EXEC_CODE (addr);
  if (GET_CODE (addr) == PARALLEL)
    addr = XVECEXP (addr, 0, 0);
  addr = XEXP (addr, 0);

  return !reg_overlap_mentioned_p (value, addr);
}

/* Return nonzero if the CONSUMER instruction (a store) does need
   PRODUCER's value to calculate the address.  */

int
arm_early_store_addr_dep (rtx producer, rtx consumer)
{
  return !arm_no_early_store_addr_dep (producer, consumer);
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
