;; Predicate definitions for ATMEL AVR micro controllers.
;; Copyright (C) 2006-2013 Free Software Foundation, Inc.
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

;; Registers from r0 to r15.
(define_predicate "l_register_operand"
  (and (match_code "reg")
       (match_test "REGNO (op) <= 15")))

;; Registers from r16 to r31.
(define_predicate "d_register_operand"
  (and (match_code "reg")
       (match_test "REGNO (op) >= 16 && REGNO (op) <= 31")))

(define_predicate "even_register_operand"
  (and (match_code "reg")
       (and (match_test "REGNO (op) <= 31")
            (match_test "(REGNO (op) & 1) == 0"))))

(define_predicate "odd_register_operand"
  (and (match_code "reg")
       (and (match_test "REGNO (op) <= 31")
            (match_test "(REGNO (op) & 1) != 0"))))

;; SP register.
(define_predicate "stack_register_operand"
  (and (match_code "reg")
       (match_test "REGNO (op) == REG_SP")))

;; Return true if OP is a valid address for lower half of I/O space.
(define_predicate "low_io_address_operand"
  (and (match_code "const_int")
       (match_test "IN_RANGE (INTVAL (op) - avr_current_arch->sfr_offset,
                              0, 0x1f)")))

;; Return true if OP is a valid address for high half of I/O space.
(define_predicate "high_io_address_operand"
  (and (match_code "const_int")
       (match_test "IN_RANGE (INTVAL (op) - avr_current_arch->sfr_offset,
                              0x20, 0x3F)")))

;; Return true if OP is a valid address of I/O space.
(define_predicate "io_address_operand"
  (and (match_code "const_int")
       (match_test "IN_RANGE (INTVAL (op) - avr_current_arch->sfr_offset,
                              0, 0x40 - GET_MODE_SIZE (mode))")))

;; Return 1 if OP is a general operand not in flash memory
(define_predicate "nop_general_operand"
  (and (match_operand 0 "general_operand")
       (match_test "!avr_mem_flash_p (op)")))

;; Return 1 if OP is an "ordinary" general operand, i.e. a general
;; operand whose load is not handled by a libgcc call or ELPM.
(define_predicate "nox_general_operand"
  (and (match_operand 0 "general_operand")
       (not (match_test "avr_load_libgcc_p (op)"))
       (not (match_test "avr_mem_memx_p (op)"))))

;; Return 1 if OP is a memory operand in one of the __flash* address spaces
(define_predicate "flash_operand"
  (and (match_operand 0 "memory_operand")
       (match_test "Pmode == mode")
       (ior (match_test "!MEM_P (op)")
            (match_test "avr_mem_flash_p (op)"))))

;; Return 1 if OP is the zero constant for MODE.
(define_predicate "const0_operand"
  (and (match_code "const_int,const_fixed,const_double")
       (match_test "op == CONST0_RTX (mode)")))

;; Return 1 if OP is the one constant integer for MODE.
(define_predicate "const1_operand"
  (and (match_code "const_int")
       (match_test "op == CONST1_RTX (mode)")))


;; Return 1 if OP is constant integer 0..7 for MODE.
(define_predicate "const_0_to_7_operand"
  (and (match_code "const_int")
       (match_test "IN_RANGE (INTVAL (op), 0, 7)")))

;; Return 1 if OP is constant integer 2..7 for MODE.
(define_predicate "const_2_to_7_operand"
  (and (match_code "const_int")
       (match_test "IN_RANGE (INTVAL (op), 2, 7)")))

;; Return 1 if OP is constant integer 1..6 for MODE.
(define_predicate "const_1_to_6_operand"
  (and (match_code "const_int")
       (match_test "IN_RANGE (INTVAL (op), 1, 6)")))

;; Return 1 if OP is constant integer 2..6 for MODE.
(define_predicate "const_2_to_6_operand"
  (and (match_code "const_int")
       (match_test "IN_RANGE (INTVAL (op), 2, 6)")))

;; Returns true if OP is either the constant zero or a register.
(define_predicate "reg_or_0_operand"
  (ior (match_operand 0 "register_operand")
       (match_operand 0 "const0_operand")))

;; Returns 1 if OP is a SYMBOL_REF.
(define_predicate "symbol_ref_operand"
  (match_code "symbol_ref"))

;; Return true if OP is a text segment reference.
;; This is needed for program memory address expressions.
(define_predicate "text_segment_operand"
  (match_code "code_label,label_ref,symbol_ref,plus,const")
{
  switch (GET_CODE (op))
    {
    case CODE_LABEL:
      return true;
    case LABEL_REF :
      return true;
    case SYMBOL_REF :
      return SYMBOL_REF_FUNCTION_P (op);
    case PLUS :
      /* Assume canonical format of symbol + constant.
	 Fall through.  */
    case CONST :
      return text_segment_operand (XEXP (op, 0), VOIDmode);
    default :
      return false;
    }
})

;; Return true if OP is a constant that contains only one 1 in its
;; binary representation.
(define_predicate "single_one_operand"
  (and (match_code "const_int")
       (match_test "exact_log2(INTVAL (op) & GET_MODE_MASK (mode)) >= 0")))

;; Return true if OP is a constant that contains only one 0 in its
;; binary representation.
(define_predicate "single_zero_operand"
  (and (match_code "const_int")
       (match_test "exact_log2(~INTVAL (op) & GET_MODE_MASK (mode)) >= 0")))

;;
(define_predicate "avr_sp_immediate_operand"
  (and (match_code "const_int")
       (match_test "satisfies_constraint_Csp (op)")))

;; True for EQ & NE
(define_predicate "eqne_operator"
  (match_code "eq,ne"))

;; True for GE & LT
(define_predicate "gelt_operator"
  (match_code "ge,lt"))

;; True for GT, GTU, LE & LEU
(define_predicate "difficult_comparison_operator"
  (match_code "gt,gtu,le,leu"))

;; False for GT, GTU, LE & LEU
(define_predicate "simple_comparison_operator"
  (and (match_operand 0 "comparison_operator")
       (not (match_code "gt,gtu,le,leu"))))

;; Return true if OP is a valid call operand.
(define_predicate "call_insn_operand"
  (and (match_code "mem")
       (ior (match_test "register_operand (XEXP (op, 0), mode)")
            (match_test "CONSTANT_ADDRESS_P (XEXP (op, 0))"))))

;; For some insns we must ensure that no hard register is inserted
;; into their operands because the insns are split and the split
;; involves hard registers.  An example are divmod insn that are
;; split to insns that represent implicit library calls.

;; True for register that is pseudo register.
(define_predicate "pseudo_register_operand"
  (and (match_operand 0 "register_operand")
       (not (and (match_code "reg")
                 (match_test "HARD_REGISTER_P (op)")))))

;; True for operand that is pseudo register or CONST_INT.
(define_predicate "pseudo_register_or_const_int_operand"
  (ior (match_operand 0 "const_int_operand")
       (match_operand 0 "pseudo_register_operand")))

;; We keep combiner from inserting hard registers into the input of sign- and
;; zero-extends.  A hard register in the input operand is not wanted because
;; 32-bit multiply patterns clobber some hard registers and extends with a
;; hard register that overlaps these clobbers won't combine to a widening
;; multiplication.  There is no need for combine to propagate or insert
;; hard registers, register allocation can do it just as well.

;; True for operand that is pseudo register at combine time.
(define_predicate "combine_pseudo_register_operand"
  (ior (match_operand 0 "pseudo_register_operand")
       (and (match_operand 0 "register_operand")
            (match_test "reload_completed || reload_in_progress"))))

;; Return true if OP is a constant integer that is either
;; 8 or 16 or 24.
(define_predicate "const_8_16_24_operand"
  (and (match_code "const_int")
       (match_test "8 == INTVAL(op) || 16 == INTVAL(op) || 24 == INTVAL(op)")))

;; Unsigned CONST_INT that fits in 8 bits, i.e. 0..255.
(define_predicate "u8_operand"
  (and (match_code "const_int")
       (match_test "IN_RANGE (INTVAL (op), 0, 255)")))

;; Signed CONST_INT that fits in 8 bits, i.e. -128..127.
(define_predicate "s8_operand"
  (and (match_code "const_int")
       (match_test "IN_RANGE (INTVAL (op), -128, 127)")))

;; One-extended CONST_INT that fits in 8 bits, i.e. -256..-1.
(define_predicate "o8_operand"
  (and (match_code "const_int")
       (match_test "IN_RANGE (INTVAL (op), -256, -1)")))

;; Signed CONST_INT that fits in 9 bits, i.e. -256..255.
(define_predicate "s9_operand"
  (and (match_code "const_int")
       (match_test "IN_RANGE (INTVAL (op), -256, 255)")))

(define_predicate "register_or_s9_operand"
  (ior (match_operand 0 "register_operand")
       (match_operand 0 "s9_operand")))

;; Unsigned CONST_INT that fits in 16 bits, i.e. 0..65536.
(define_predicate "u16_operand"
  (and (match_code "const_int")
       (match_test "IN_RANGE (INTVAL (op), 0, (1<<16)-1)")))

;; Signed CONST_INT that fits in 16 bits, i.e. -32768..32767.
(define_predicate "s16_operand"
  (and (match_code "const_int")
       (match_test "IN_RANGE (INTVAL (op), -(1<<15), (1<<15)-1)")))

;; One-extended CONST_INT that fits in 16 bits, i.e. -65536..-1.
(define_predicate "o16_operand"
  (and (match_code "const_int")
       (match_test "IN_RANGE (INTVAL (op), -(1<<16), -1)")))

;; Const int, fixed, or double operand
(define_predicate "const_operand"
  (ior (match_code "const_fixed")
       (match_code "const_double")
       (match_operand 0 "const_int_operand")))

;; Const int, const fixed, or const double operand
(define_predicate "nonmemory_or_const_operand"
  (ior (match_code "const_fixed")
       (match_code "const_double")
       (match_operand 0 "nonmemory_operand")))

;; Immediate, const fixed, or const double operand
(define_predicate "const_or_immediate_operand"
  (ior (match_code "const_fixed")
       (match_code "const_double")
       (match_operand 0 "immediate_operand")))
