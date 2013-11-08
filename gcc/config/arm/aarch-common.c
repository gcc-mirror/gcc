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

typedef struct
{
  rtx_code search_code;
  rtx search_result;
  bool find_any_shift;
} search_term;

/* Return TRUE if X is either an arithmetic shift left, or
   is a multiplication by a power of two.  */
static bool
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

/* Callback function for arm_find_sub_rtx_with_code.
   DATA is safe to treat as a SEARCH_TERM, ST.  This will
   hold a SEARCH_CODE.  PATTERN is checked to see if it is an
   RTX with that code.  If it is, write SEARCH_RESULT in ST
   and return 1.  Otherwise, or if we have been passed a NULL_RTX
   return 0.  If ST.FIND_ANY_SHIFT then we are interested in
   anything which can reasonably be described as a SHIFT RTX.  */
static int
arm_find_sub_rtx_with_search_term (rtx *pattern, void *data)
{
  search_term *st = (search_term *) data;
  rtx_code pattern_code;
  int found = 0;

  gcc_assert (pattern);
  gcc_assert (st);

  /* Poorly formed patterns can really ruin our day.  */
  if (*pattern == NULL_RTX)
    return 0;

  pattern_code = GET_CODE (*pattern);

  if (st->find_any_shift)
    {
      unsigned i = 0;

      /* Left shifts might have been canonicalized to a MULT of some
	 power of two.  Make sure we catch them.  */
      if (arm_rtx_shift_left_p (*pattern))
	found = 1;
      else
	for (i = 0; i < ARRAY_SIZE (shift_rtx_codes); i++)
	  if (pattern_code == shift_rtx_codes[i])
	    found = 1;
    }

  if (pattern_code == st->search_code)
    found = 1;

  if (found)
    st->search_result = *pattern;

  return found;
}

/* Traverse PATTERN looking for a sub-rtx with RTX_CODE CODE.  */
static rtx
arm_find_sub_rtx_with_code (rtx pattern, rtx_code code, bool find_any_shift)
{
  search_term st;
  int result = 0;

  gcc_assert (pattern != NULL_RTX);
  st.search_code = code;
  st.search_result = NULL_RTX;
  st.find_any_shift = find_any_shift;
  result = for_each_rtx (&pattern, arm_find_sub_rtx_with_search_term, &st);
  if (result)
    return st.search_result;
  else
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
  rtx set_producer = arm_find_sub_rtx_with_code (producer, SET, false);
  rtx set_consumer = arm_find_sub_rtx_with_code (consumer, SET, false);

  if (set_producer && set_consumer)
    {
      *set_source = SET_DEST (set_producer);
      *set_destination = SET_SRC (set_consumer);
      return 1;
    }
  return 0;
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
    {
      if (REG_P (early_op))
	early_op = op;

      return !reg_overlap_mentioned_p (value, early_op);
    }

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
  rtx value = arm_find_sub_rtx_with_code (producer, SET, false);
  rtx addr = arm_find_sub_rtx_with_code (consumer, SET, false);

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
