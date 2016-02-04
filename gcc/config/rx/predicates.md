;; Predicate definitions for Renesas RX.
;; Copyright (C) 2008-2016 Free Software Foundation, Inc.
;; Contributed by Red Hat.
;;
;; This file is part of GCC.
;;
;; GCC is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; GCC is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GCC; see the file COPYING3.  If not see
;; <http://www.gnu.org/licenses/>.



;; Check that the operand is suitable for a call insn.
;; Only registers and symbol refs are allowed.

(define_predicate "rx_call_operand"
  (ior (match_code "reg")
       (and (match_test "!TARGET_JSR")
	    (match_code "symbol_ref")))
)

;; For sibcall operations we can only use a symbolic address.

(define_predicate "rx_symbolic_call_operand"
  (match_code "symbol_ref")
)

;; Check that the operand is suitable for a shift insn
;; Only small integers or a value in a register are permitted.

(define_predicate "rx_shift_operand"
  (ior (match_operand 0 "register_operand")
       (and (match_code "const_int")
	    (match_test "IN_RANGE (INTVAL (op), 0, 31)")))
)

(define_predicate "rx_constshift_operand"
  (and (match_code "const_int")
       (match_test "IN_RANGE (INTVAL (op), 0, 31)"))
)

(define_predicate "rx_restricted_mem_operand"
  (and (match_code "mem")
       (match_test "rx_is_restricted_memory_address (XEXP (op, 0), mode)"))
)

;; Check that the operand is suitable as the source operand
;; for a logic or arithmeitc instruction.  Registers, integers
;; and a restricted subset of memory addresses are allowed.

(define_predicate "rx_source_operand"
  (ior (match_operand 0 "register_operand")
       (match_operand 0 "immediate_operand")
       (match_operand 0 "rx_restricted_mem_operand"))
)

;; Check that the operand is suitable as the source operand
;; for a comparison instruction.  This is the same as
;; rx_source_operand except that SUBREGs are allowed but
;; CONST_INTs are not.

(define_predicate "rx_compare_operand"
  (ior (match_operand 0 "register_operand")
       (match_operand 0 "rx_restricted_mem_operand"))
)

;; Check that the operand is suitable as the source operand
;; for a min/max instruction.  This is the same as
;; rx_source_operand except that CONST_INTs are allowed but
;; REGs and SUBREGs are not.

(define_predicate "rx_minmaxex_operand"
  (ior (match_operand 0 "immediate_operand")
       (match_operand 0 "rx_restricted_mem_operand"))
)

;; Return true if OP is a store multiple operation.  This looks like:
;;
;;   [(set (SP) (MINUS (SP) (INT)))
;;    (set (MEM (SP)) (REG))
;;    (set (MEM (MINUS (SP) (INT))) (REG)) {optionally repeated}
;;   ]

(define_special_predicate "rx_store_multiple_vector"
  (match_code "parallel")
{
  int count = XVECLEN (op, 0);
  unsigned int src_regno;
  rtx element;
  int i;

  /* Perform a quick check so we don't blow up below.  */
  if (count <= 2)
    return false;

  /* Check that the first element of the vector is the stack adjust.  */
  element = XVECEXP (op, 0, 0);
  if (   ! SET_P (element)
      || ! REG_P (SET_DEST (element))
      ||   REGNO (SET_DEST (element)) != SP_REG
      ||   GET_CODE (SET_SRC (element)) != MINUS
      || ! REG_P (XEXP (SET_SRC (element), 0))
      ||   REGNO (XEXP (SET_SRC (element), 0)) != SP_REG
      || ! CONST_INT_P (XEXP (SET_SRC (element), 1)))
    return false;
	 
  /* Check that the next element is the first push.  */
  element = XVECEXP (op, 0, 1);
  if (   ! SET_P (element)
      || ! REG_P (SET_SRC (element))
      || GET_MODE (SET_SRC (element)) != SImode
      || ! MEM_P (SET_DEST (element))
      || GET_MODE (SET_DEST (element)) != SImode
      || GET_CODE (XEXP (SET_DEST (element), 0)) != MINUS
      || ! REG_P (XEXP (XEXP (SET_DEST (element), 0), 0))
      ||   REGNO (XEXP (XEXP (SET_DEST (element), 0), 0)) != SP_REG
      || ! CONST_INT_P (XEXP (XEXP (SET_DEST (element), 0), 1))
      || INTVAL (XEXP (XEXP (SET_DEST (element), 0), 1))
        != GET_MODE_SIZE (SImode))
    return false;

  src_regno = REGNO (SET_SRC (element));

  /* Check that the remaining elements use SP-<disp>
     addressing and decreasing register numbers.  */
  for (i = 2; i < count; i++)
    {
      element = XVECEXP (op, 0, i);

      if (   ! SET_P (element)
	  || ! REG_P (SET_SRC (element))
	  || GET_MODE (SET_SRC (element)) != SImode
	  || REGNO (SET_SRC (element)) != src_regno - (i - 1)
	  || ! MEM_P (SET_DEST (element))
	  || GET_MODE (SET_DEST (element)) != SImode
	  || GET_CODE (XEXP (SET_DEST (element), 0)) != MINUS
          || ! REG_P (XEXP (XEXP (SET_DEST (element), 0), 0))
          ||   REGNO (XEXP (XEXP (SET_DEST (element), 0), 0)) != SP_REG
	  || ! CONST_INT_P (XEXP (XEXP (SET_DEST (element), 0), 1))
	  || INTVAL (XEXP (XEXP (SET_DEST (element), 0), 1))
	     != i * GET_MODE_SIZE (SImode))
	return false;
    }
  return true;
})

;; Return true if OP is a load multiple operation.
;; This looks like:
;;  [(set (SP) (PLUS (SP) (INT)))
;;   (set (REG) (MEM (SP)))
;;   (set (REG) (MEM (PLUS (SP) (INT)))) {optionally repeated}
;;  ]

(define_special_predicate "rx_load_multiple_vector"
  (match_code "parallel")
{
  int count = XVECLEN (op, 0);
  unsigned int dest_regno;
  rtx element;
  int i;

  /* Perform a quick check so we don't blow up below.  */
  if (count <= 2)
    return false;

  /* Check that the first element of the vector is the stack adjust.  */
  element = XVECEXP (op, 0, 0);
  if (   ! SET_P (element)
      || ! REG_P (SET_DEST (element))
      ||   REGNO (SET_DEST (element)) != SP_REG
      ||   GET_CODE (SET_SRC (element)) != PLUS
      || ! REG_P (XEXP (SET_SRC (element), 0))
      ||   REGNO (XEXP (SET_SRC (element), 0)) != SP_REG
      || ! CONST_INT_P (XEXP (SET_SRC (element), 1)))
    return false;
	 
  /* Check that the next element is the first push.  */
  element = XVECEXP (op, 0, 1);
  if (   ! SET_P (element)
      || ! REG_P (SET_DEST (element))
      || ! MEM_P (SET_SRC (element))
      || ! REG_P (XEXP (SET_SRC (element), 0))
      ||   REGNO (XEXP (SET_SRC (element), 0)) != SP_REG)
    return false;

  dest_regno = REGNO (SET_DEST (element));

  /* Check that the remaining elements use SP+<disp>
     addressing and incremental register numbers.  */
  for (i = 2; i < count; i++)
    {
      element = XVECEXP (op, 0, i);

      if (   ! SET_P (element)
	  || ! REG_P (SET_DEST (element))
	  || GET_MODE (SET_DEST (element)) != SImode
	  || REGNO (SET_DEST (element)) != dest_regno + (i - 1)
	  || ! MEM_P (SET_SRC (element))
	  || GET_MODE (SET_SRC (element)) != SImode
	  || GET_CODE (XEXP (SET_SRC (element), 0)) != PLUS
          || ! REG_P (XEXP (XEXP (SET_SRC (element), 0), 0))
          ||   REGNO (XEXP (XEXP (SET_SRC (element), 0), 0)) != SP_REG
	  || ! CONST_INT_P (XEXP (XEXP (SET_SRC (element), 0), 1))
	  || INTVAL (XEXP (XEXP (SET_SRC (element), 0), 1))
	     != (i - 1) * GET_MODE_SIZE (SImode))
	return false;
    }
  return true;
})

;; Return true if OP is a pop-and-return load multiple operation.
;; This looks like:
;;  [(set (SP) (PLUS (SP) (INT)))
;;   (set (REG) (MEM (SP)))
;;   (set (REG) (MEM (PLUS (SP) (INT)))) {optional and possibly repeated}
;;   (return)
;;  ]

(define_special_predicate "rx_rtsd_vector"
  (match_code "parallel")
{
  int count = XVECLEN (op, 0);
  unsigned int dest_regno;
  rtx element;
  int i;

  /* Perform a quick check so we don't blow up below.  */
  if (count <= 2)
    return false;

  /* Check that the first element of the vector is the stack adjust.  */
  element = XVECEXP (op, 0, 0);
  if (   ! SET_P (element)
      || ! REG_P (SET_DEST (element))
      ||   REGNO (SET_DEST (element)) != SP_REG
      ||   GET_CODE (SET_SRC (element)) != PLUS
      || ! REG_P (XEXP (SET_SRC (element), 0))
      ||   REGNO (XEXP (SET_SRC (element), 0)) != SP_REG
      || ! CONST_INT_P (XEXP (SET_SRC (element), 1)))
    return false;
	 
  /* Check that the next element is the first push.  */
  element = XVECEXP (op, 0, 1);
  if (   ! SET_P (element)
      || ! REG_P (SET_DEST (element))
      || ! MEM_P (SET_SRC (element))
      || ! REG_P (XEXP (SET_SRC (element), 0))
      ||   REGNO (XEXP (SET_SRC (element), 0)) != SP_REG)
    return false;

  dest_regno = REGNO (SET_DEST (element));

  /* Check that the remaining elements, if any, and except
     for the last one, use SP+<disp> addressing and incremental
     register numbers.  */
  for (i = 2; i < count - 1; i++)
    {
      element = XVECEXP (op, 0, i);

      if (   ! SET_P (element)
	  || ! REG_P (SET_DEST (element))
	  || GET_MODE (SET_DEST (element)) != SImode
	  || REGNO (SET_DEST (element)) != dest_regno + (i - 1)
	  || ! MEM_P (SET_SRC (element))
	  || GET_MODE (SET_SRC (element)) != SImode
	  || GET_CODE (XEXP (SET_SRC (element), 0)) != PLUS
          || ! REG_P (XEXP (XEXP (SET_SRC (element), 0), 0))
          ||   REGNO (XEXP (XEXP (SET_SRC (element), 0), 0)) != SP_REG
	  || ! CONST_INT_P (XEXP (XEXP (SET_SRC (element), 0), 1))
	  || INTVAL (XEXP (XEXP (SET_SRC (element), 0), 1))
	     != (i - 1) * GET_MODE_SIZE (SImode))
	return false;
    }

  /* The last element must be a RETURN.  */    
  element = XVECEXP (op, 0, count - 1);
  return GET_CODE (element) == RETURN;
})

(define_predicate "label_ref_operand"
  (match_code "label_ref")
)

(define_predicate "rx_z_comparison_operator"
  (match_code "eq,ne")
)

(define_predicate "rx_zs_comparison_operator"
  (match_code "eq,ne,lt,ge")
)

;; GT and LE omitted due to operand swap required.
(define_predicate "rx_fp_comparison_operator"
  (match_code "eq,ne,lt,ge,ordered,unordered")
)

(define_predicate "rshift_operator"
  (match_code "ashiftrt,lshiftrt")
)
