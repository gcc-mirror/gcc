;; Predicate definitions for TI PRU.
;; Copyright (C) 2014-2024 Free Software Foundation, Inc.
;; Contributed by Dimitar Dimitrov <dimitar@dinux.eu>
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

(define_predicate "const_1_operand"
  (and (match_code "const_int")
       (match_test "INTVAL (op) == 1")))

(define_predicate "const_0_operand"
  (and (match_code "const_int")
       (match_test "INTVAL (op) == 0")))

; Note: Always pass a valid mode!
(define_predicate "const_ubyte_operand"
  (match_code "const_int")
{
  gcc_assert (mode != VOIDmode);
  return IN_RANGE (INTVAL (op) & GET_MODE_MASK (mode), 0, 0xff);
})

(define_predicate "const_uhword_operand"
  (match_code "const_int")
{
  gcc_assert (mode != VOIDmode);
  return IN_RANGE (INTVAL (op) & GET_MODE_MASK (mode), 0, 0xffff);
})

; TRUE for comparisons we support.
(define_predicate "pru_cmp_operator"
  (match_code "eq,ne,leu,ltu,geu,gtu"))

; TRUE for signed comparisons that need special handling for PRU.
(define_predicate "pru_signed_cmp_operator"
  (match_code "ge,gt,le,lt"))

;; FP Comparisons handled by pru_expand_pru_compare.
(define_predicate "pru_fp_comparison_operator"
  (match_code "eq,ne,lt,gt,le,ge"))

;; TRUE for comparisons supported by PRU's cstore.
(define_predicate "pru_cstore_comparison_operator"
  (match_code "eq,ne,gtu"))

;; Return true if OP is a constant that contains only one 1 in its
;; binary representation.
(define_predicate "single_one_operand"
  (and (match_code "const_int")
       (match_test "exact_log2 (INTVAL (op) & GET_MODE_MASK (mode)) >= 0")))

;; Return true if OP is a constant that contains only one 0 in its
;; binary representation.
(define_predicate "single_zero_operand"
  (and (match_code "const_int")
       (match_test "exact_log2 (~INTVAL (op) & GET_MODE_MASK (mode)) >= 0")))

(define_predicate "pru_muldst_operand"
  (match_code "subreg,reg")
{
  if (register_operand (op, mode))
    {
      int regno;

      if (REG_P (op))
	regno = REGNO (op);
      else if (GET_CODE (op) == SUBREG && REG_P (SUBREG_REG (op)))
	regno = REGNO (SUBREG_REG (op));
      else
	return 0;

      return REGNO_REG_CLASS (regno) == MULDST_REGS
	     || regno >= FIRST_PSEUDO_REGISTER;
    }
  return 0;
})

(define_predicate "pru_mulsrc0_operand"
  (match_code "subreg,reg")
{
  if (register_operand (op, mode))
    {
      int regno;

      if (REG_P (op))
	regno = REGNO (op);
      else if (GET_CODE (op) == SUBREG && REG_P (SUBREG_REG (op)))
	regno = REGNO (SUBREG_REG (op));
      else
	return 0;

      return REGNO_REG_CLASS (regno) == MULSRC0_REGNUM
	     || regno >= FIRST_PSEUDO_REGISTER;
    }
  return 0;
})

(define_predicate "pru_mulsrc1_operand"
  (match_code "subreg,reg")
{
  if (register_operand (op, mode))
    {
      int regno;

      if (REG_P (op))
	regno = REGNO (op);
      else if (GET_CODE (op) == SUBREG && REG_P (SUBREG_REG (op)))
	regno = REGNO (SUBREG_REG (op));
      else
	return 0;

      return REGNO_REG_CLASS (regno) == MULSRC1_REGNUM
	     || regno >= FIRST_PSEUDO_REGISTER;
    }
  return 0;
})

(define_predicate "regio_operand"
  (match_code "subreg,reg")
{
  if (register_operand (op, mode))
    {
      int regno;

      if (REG_P (op))
	regno = REGNO (op);
      else if (GET_CODE (op) == SUBREG && REG_P (SUBREG_REG (op)))
	regno = REGNO (SUBREG_REG (op));
      else
	return 0;

      return REGNO_REG_CLASS (regno) == REGIO_REGS;
    }
  return 0;
})

(define_predicate "reg_or_const_int_operand"
  (ior (match_operand 0 "const_int_operand")
       (match_operand 0 "register_operand")))

(define_predicate "reg_or_ubyte_operand"
  (ior (match_operand 0 "const_ubyte_operand")
       (match_operand 0 "register_operand")))

(define_predicate "reg_or_const_1_operand"
  (ior (match_operand 0 "const_1_operand")
       (match_operand 0 "register_operand")))

(define_predicate "const_shift_operand"
  (and (match_code "const_int")
       (match_test "SHIFT_INT (INTVAL (op))")))

(define_predicate "shift_operand"
  (ior (match_operand 0 "const_shift_operand")
       (match_operand 0 "register_operand")))

(define_predicate "ctable_addr_operand"
  (and (match_code "const_int")
       (match_test "pru_get_ctable_base_index (INTVAL (op)) >= 0")))

(define_predicate "ctable_base_operand"
  (and (match_code "const_int")
       (match_test "pru_get_ctable_exact_base_index (INTVAL (op)) >= 0")))

;; Ideally we should enforce a restriction to all text labels to fit in
;; 16bits, as required by the PRU ISA.  But for the time being we'll rely on
;; binutils to catch text segment overflows.
(define_predicate "call_operand"
  (ior (match_operand 0 "immediate_operand")
       (match_operand 0 "register_operand")))

;; Return true if OP is a text segment reference.
;; This is needed for program memory address expressions.  Borrowed from AVR.
(define_predicate "text_segment_operand"
  (match_code "code_label,label_ref,symbol_ref,plus,minus")
{
  poly_int64 offset;
  rtx base = strip_offset (op, &offset);

  switch (GET_CODE (base))
    {
    case CODE_LABEL:
      /* Why AVR lists this as a valid option?  Let's catch it.  */
      gcc_unreachable ();
      return false;
    case LABEL_REF:
      return true;
    case SYMBOL_REF:
      return SYMBOL_REF_FUNCTION_P (base);
    case PLUS:
    case MINUS:
      /* Handle constructs like (&&label1 - &&label2).  See pr70460.c.  */
      return text_segment_operand (XEXP (op, 0), VOIDmode);
    default:
      return false;
    }
})

;; Return true if OP is a load multiple operation.  It is known to be a
;; PARALLEL and the first section will be tested.

(define_special_predicate "load_multiple_operation"
  (match_code "parallel")
{
  machine_mode elt_mode;
  int count = XVECLEN (op, 0);
  unsigned int dest_regno;
  rtx src_addr, base_reg;
  poly_int64 base_offs;
  int i;

  /* Perform a quick check so we don't blow up below.  */
  if (GET_CODE (XVECEXP (op, 0, 0)) != SET
      || GET_CODE (SET_DEST (XVECEXP (op, 0, 0))) != REG
      || GET_CODE (SET_SRC (XVECEXP (op, 0, 0))) != MEM)
    return false;

  dest_regno = REGNO (SET_DEST (XVECEXP (op, 0, 0)));
  src_addr = XEXP (SET_SRC (XVECEXP (op, 0, 0)), 0);
  elt_mode = GET_MODE (SET_DEST (XVECEXP (op, 0, 0)));

  base_reg = strip_offset (src_addr, &base_offs);
  if (GET_CODE (base_reg) != REG)
    return false;

  for (i = 1; i < count; i++)
    {
      rtx elt_reg;
      poly_int64 elt_offs;
      rtx elt = XVECEXP (op, 0, i);

      if (GET_CODE (elt) != SET
	  || GET_CODE (SET_DEST (elt)) != REG
	  || GET_MODE (SET_DEST (elt)) != elt_mode
	  || REGNO (SET_DEST (elt)) != dest_regno + i * GET_MODE_SIZE (elt_mode)
	  || GET_CODE (SET_SRC (elt)) != MEM
	  || GET_MODE (SET_SRC (elt)) != elt_mode)
	return false;

      elt_reg = strip_offset (XEXP (SET_SRC (elt), 0), &elt_offs);

      if (GET_CODE (elt_reg) != REG
	  || ! rtx_equal_p (elt_reg, base_reg)
	  || elt_offs != base_offs + i * GET_MODE_SIZE (elt_mode))
	return false;
    }

  return true;
})

;; Return true if OP is a store multiple operation.  It is known to be a
;; PARALLEL and the first section will be tested.

(define_special_predicate "store_multiple_operation"
  (match_code "parallel")
{
  machine_mode elt_mode;
  int count = XVECLEN (op, 0);
  unsigned int src_regno;
  rtx dest_addr, base_reg;
  poly_int64 base_offs;
  int i;

  /* Perform a quick check so we don't blow up below.  */
  if (GET_CODE (XVECEXP (op, 0, 0)) != SET
      || GET_CODE (SET_DEST (XVECEXP (op, 0, 0))) != MEM
      || GET_CODE (SET_SRC (XVECEXP (op, 0, 0))) != REG)
    return false;

  src_regno = REGNO (SET_SRC (XVECEXP (op, 0, 0)));
  dest_addr = XEXP (SET_DEST (XVECEXP (op, 0, 0)), 0);
  elt_mode = GET_MODE (SET_SRC (XVECEXP (op, 0, 0)));

  base_reg = strip_offset (dest_addr, &base_offs);
  if (GET_CODE (base_reg) != REG)
    return false;

  for (i = 1; i < count; i++)
    {
      rtx elt_reg;
      poly_int64 elt_offs;
      rtx elt = XVECEXP (op, 0, i);

      if (GET_CODE (elt) != SET
	  || GET_CODE (SET_SRC (elt)) != REG
	  || GET_MODE (SET_SRC (elt)) != elt_mode
	  || REGNO (SET_SRC (elt)) != src_regno + i * GET_MODE_SIZE (elt_mode)
	  || GET_CODE (SET_DEST (elt)) != MEM
	  || GET_MODE (SET_DEST (elt)) != elt_mode)
	return false;

      elt_reg = strip_offset (XEXP (SET_DEST (elt), 0), &elt_offs);

      if (GET_CODE (elt_reg) != REG
	  || ! rtx_equal_p (elt_reg, base_reg)
	  || elt_offs != base_offs + i * GET_MODE_SIZE (elt_mode))
	return false;
    }
  return true;
})

;; Return true if OP is a constant integer with one single consecutive
;; range of bytes with value 0xff, and the rest of the bytes are 0x00.
(define_predicate "const_fillbytes_operand"
  (match_code "const_int")
{
  gcc_assert (mode != VOIDmode);

  pru_byterange r = pru_calc_byterange (INTVAL (op), mode);
  return r.start >=0 && r.nbytes > 0;
})

;; Return true if OP is a constant integer with one single consecutive
;; range of bytes with value 0x00, and the rest of the bytes are 0xff.
(define_predicate "const_zerobytes_operand"
  (match_code "const_int")
{
  gcc_assert (mode != VOIDmode);

  pru_byterange r = pru_calc_byterange (~INTVAL (op), mode);
  return r.start >=0 && r.nbytes > 0;
})
