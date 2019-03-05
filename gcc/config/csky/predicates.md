;; Predicates for C-SKY.
;; Copyright (C) 2018-2019 Free Software Foundation, Inc.
;; Contributed by C-SKY Microsystems and Mentor Graphics.
;;
;; This file is part of GCC.
;;
;; GCC is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; GCC is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GCC; see the file COPYING3.  If not see
;; <http://www.gnu.org/licenses/>.  */

;; Return 1 if OP is a load multiple operation.

(define_predicate "csky_load_multiple_operation"
  (match_code "parallel")
{
  int count = XVECLEN (op, 0);
  int dest_regno;
  rtx src_addr;
  int i;

  /* Perform a quick check so we don't blow up below.  */
  if (count <= 1
      || GET_CODE (XVECEXP (op, 0, 0)) != SET
      || GET_CODE (SET_DEST (XVECEXP (op, 0, 0))) != REG
      || GET_CODE (SET_SRC (XVECEXP (op, 0, 0))) != MEM
      || GET_CODE (XEXP (SET_SRC (XVECEXP (op, 0, 0)), 0)) != REG
      || XEXP (SET_SRC (XVECEXP (op, 0, 0)), 0) != stack_pointer_rtx)
    return 0;

  dest_regno = REGNO (SET_DEST (XVECEXP (op, 0, 0)));
  src_addr = XEXP (SET_SRC (XVECEXP (op, 0, 0)), 0);

  for (i = 1; i < count; i++)
    {
      rtx elt = XVECEXP (op, 0, i);

      if (GET_CODE (elt) != SET
	  || GET_CODE (SET_DEST (elt)) != REG
	  || GET_MODE (SET_DEST (elt)) != SImode
	  || REGNO (SET_DEST (elt)) != (unsigned) (dest_regno + i)
	  || GET_CODE (SET_SRC (elt)) != MEM
	  || GET_MODE (SET_SRC (elt)) != SImode
	  || GET_CODE (XEXP (SET_SRC (elt), 0)) != PLUS
	  || ! rtx_equal_p (XEXP (XEXP (SET_SRC (elt), 0), 0), src_addr)
	  || GET_CODE (XEXP (XEXP (SET_SRC (elt), 0), 1)) != CONST_INT
	  || INTVAL (XEXP (XEXP (SET_SRC (elt), 0), 1)) != i * 4)
	return 0;
    }
  return 1;
})

;; Similar, for store multiple.

(define_predicate "csky_store_multiple_operation"
  (match_code "parallel")
{
  int count = XVECLEN (op, 0);
  int src_regno;
  rtx dest_addr;
  int i;

  /* Perform a quick check so we don't blow up below.  */
  if (count <= 1
      || GET_CODE (XVECEXP (op, 0, 0)) != SET
      || GET_CODE (SET_DEST (XVECEXP (op, 0, 0))) != MEM
      || GET_CODE (XEXP (SET_DEST (XVECEXP (op, 0, 0)), 0)) != REG
      || XEXP (SET_DEST (XVECEXP (op, 0, 0)), 0) != stack_pointer_rtx
      || GET_CODE (SET_SRC (XVECEXP (op, 0, 0))) != REG)
    return 0;

  src_regno = REGNO (SET_SRC (XVECEXP (op, 0, 0)));
  dest_addr = XEXP (SET_DEST (XVECEXP (op, 0, 0)), 0);

  for (i = 1; i < count; i++)
    {
      rtx elt = XVECEXP (op, 0, i);

      if (GET_CODE (elt) != SET
	  || GET_CODE (SET_SRC (elt)) != REG
	  || GET_MODE (SET_SRC (elt)) != SImode
	  || REGNO (SET_SRC (elt)) != (unsigned) (src_regno + i)
	  || GET_CODE (SET_DEST (elt)) != MEM
	  || GET_MODE (SET_DEST (elt)) != SImode
	  || GET_CODE (XEXP (SET_DEST (elt), 0)) != PLUS
	  || ! rtx_equal_p (XEXP (XEXP (SET_DEST (elt), 0), 0), dest_addr)
	  || GET_CODE (XEXP (XEXP (SET_DEST (elt), 0), 1)) != CONST_INT
	  || INTVAL (XEXP (XEXP (SET_DEST (elt), 0), 1)) != i * 4)
	return 0;
    }
  return 1;
})


(define_predicate "csky_arith_K_operand"
  (match_code "reg,subreg,const_int")
  {
    if (register_operand (op, mode))
      return 1;
    if (CONST_INT_P (op) && CSKY_CONST_OK_FOR_K (INTVAL (op)))
      return 1;
    return 0;
  })

(define_predicate "csky_literal_K_operand"
  (match_code "const_int")
  {
    if (CONST_INT_P (op) && CSKY_CONST_OK_FOR_K (INTVAL (op)))
      return 1;
    return 0;
  })

(define_predicate "csky_literal_I_operand"
  (match_code "const_int")
  {
    if (CONST_INT_P (op) && CSKY_CONST_OK_FOR_I (INTVAL (op)))
      return 1;
    return 0;
  })

(define_predicate "csky_literal_J_operand"
  (match_code "const_int")
  {
    if (CONST_INT_P (op) && CSKY_CONST_OK_FOR_J (INTVAL (op)))
      return 1;
    return 0;
  })

(define_predicate "csky_literal_Uk_operand"
  (match_code "const_int")
  {
    if (CONST_INT_P (op) && CSKY_CONST_OK_FOR_Uk (INTVAL (op)))
      return 1;
    return 0;
  })

;; Nonzero if OP is a register or constant value of 1

(define_predicate "csky_arith_int1_operand"
  (match_code "reg,subreg,const_int")
  {
    if (register_operand (op, mode))
	return 1;
    if (op == const1_rtx)
	return 1;
    return 0;
  })


;; Nonzero if OP is legal address for function call

(define_predicate "csky_call_address_operand"
  (match_code "reg,subreg,symbol_ref")
  {
    if (!flag_pic && (GET_CODE (op) == SYMBOL_REF))
      return 1;
    if (register_operand (op, mode))
      return 1;
    return 0;
  })

;; Nonzero if OP is a valid source operand for a compare operation.

(define_predicate "csky_compare_operand"
  (match_code "const_int,reg,subreg")
  {
    if (register_operand (op, mode))
      return 1;
    if (GET_CODE (op) == CONST_INT && INTVAL (op) == 0)
      return 1;
    return 0;
  })

(define_predicate "csky_literal_K_Uh_operand"
  (match_code "const_int")
  {
    if (CONST_INT_P (op)
	&& (CSKY_CONST_OK_FOR_K (INTVAL (op))
	    || CSKY_CONST_OK_FOR_Uh (INTVAL (op))))
	  return 1;
    return 0;
  })

;; True if OP is a mem with an reg + optional displacement address.

(define_predicate "csky_simple_mem_operand"
  (and (match_operand 0 "memory_operand")
       (match_test "csky_simple_addr_operand_p (XEXP (op, 0))")))

(define_predicate "csky_arith_any_imm_operand"
  (match_code "const_int,reg,subreg")
  {
    if (register_operand (op, mode))
      return 1;
    if (CONST_INT_P (op))
      return 1;
    return 0;
  })

(define_predicate "csky_arith_O_operand"
  (match_code "reg,subreg,const_int")
  {
    if (register_operand (op, mode))
      return 1;
    if (CONST_INT_P (op) && CSKY_CONST_OK_FOR_O (INTVAL (op)))
      return 1;
    return 0;
  })

(define_predicate "csky_unspec_operand"
  (match_code "unspec")
  {
    if (op == NULL || GET_CODE(op) != UNSPEC)
      return 0;
    return 1;
  }
)

(define_predicate "csky_const_float1_operand"
  (and (match_code "const_double")
       (match_test "(op == CONST1_RTX (mode))")))

(define_predicate "csky_arith_float1_operand"
  (ior (match_operand 0 "register_operand")
       (match_operand 0 "csky_const_float1_operand")))

(define_predicate "csky_const_float0_operand"
  (and (match_code "const_double")
       (match_test "(op == CONST0_RTX (mode))")))

(define_predicate "csky_compare_operand_float"
  (ior (match_operand 0 "register_operand")
       (match_operand 0 "csky_const_float0_operand")))

(define_special_predicate "registers_push"
  (match_code "parallel")
{
  if ((GET_CODE (XVECEXP (op, 0, 0)) != SET)
      || (GET_CODE (SET_SRC (XVECEXP (op, 0, 0))) != UNSPEC)
      || (XINT (SET_SRC (XVECEXP (op, 0, 0)), 1) != UNSPEC_PUSHPOP_MULT))
    return false;
  return true;
})

(define_special_predicate "registers_pop"
  (match_code "parallel")
{
  if ((GET_CODE (XVECEXP (op, 0, 1)) != SET)
      || (GET_CODE (SET_SRC (XVECEXP (op, 0, 1))) != UNSPEC)
      || (XINT (SET_SRC (XVECEXP (op, 0, 1)), 1) != UNSPEC_PUSHPOP_MULT))
    return false;
  return true;
})

(define_predicate "push_memory_operand"
  (match_code "mem")
{
  rtx x = XEXP (op, 0);
  if (GET_CODE (x) != PRE_MODIFY)
    return false;
  if (XEXP (x, 0) != stack_pointer_rtx)
    return false;
  x = XEXP (x, 1);
  if (GET_CODE (x) != PLUS)
    return false;
  if (XEXP (x, 0) != stack_pointer_rtx)
    return false;
  return CONST_INT_P (XEXP (x, 1));
})

(define_predicate "pop_memory_operand"
  (match_code "mem")
{
  rtx x = XEXP (op, 0);
  if (GET_CODE (x) != POST_MODIFY)
    return false;
  if (XEXP (x, 0) != stack_pointer_rtx)
    return false;
  x = XEXP (x, 1);
  if (GET_CODE (x) != PLUS)
    return false;
  if (XEXP (x, 0) != stack_pointer_rtx)
    return false;
  return CONST_INT_P (XEXP (x, 1));
})

(define_special_predicate "csky_float_comparison_operator"
  (match_code "eq,ne,le,lt,ge,gt,geu,gtu,leu,ltu,
	       unordered,ordered"))
