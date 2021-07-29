;; Constraint definitions for LoongArch.
;; Copyright (C) 2020-2021 Free Software Foundation, Inc.
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

;; Register constraints

;; "a" "A constant call global and noplt address."
;; "b" <-----unused
;; "c" "A constant call local address."
;; "d" GR_REGS
;; "e" JALR_REGS
;; "f" FP_REGS
;; "g" <-----unused
;; "h" "A constant call plt address."
;; "i" "Matches a general integer constant." (Global non-architectural)
;; "j" SIBCALL_REGS
;; "k" <-----unused
;; "l" "A signed 16-bit constant ."
;; "m" "A memory operand whose address is formed by a base register and offset
;;      that is suitable for use in instructions with the same addressing mode
;;      as @code{st.w} and @code{ld.w}."
;; "n" "Matches a non-symbolic integer constant." (Global non-architectural)
;; "o" "Matches an offsettable memory reference." (Global non-architectural)
;; "p" "Matches a general address." (Global non-architectural)
;; "q" CSR_REGS
;; "r" GENERAL_REGS (Global non-architectural)
;; "s" "Matches a symbolic integer constant." (Global non-architectural)
;; "t" "A constant call weak address"
;; "u" "A signed 52bit constant and low 32-bit is zero (for logic instructions)"
;; "v" "A signed 64-bit constant and low 44-bit is zero (for logic instructions)."
;; "w" "Matches any valid memory."
;; "x" <-----unused
;; "y" <-----unused
;; "z" ST_REGS
;; "A" <-----unused
;; "B" <-----unused
;; "C" <-----unused
;; "D" <-----unused
;; "E" "Matches a floating-point constant." (Global non-architectural)
;; "F" "Matches a floating-point constant." (Global non-architectural)
;; "G" "Floating-point zero."
;; "H" <-----unused
;; "I" "A signed 12-bit constant (for arithmetic instructions)."
;; "J" "Integer zero."
;; "K" "An unsigned 12-bit constant (for logic instructions)."
;; "L" <-----unused
;; "M" <-----unused
;; "N" <-----unused
;; "O" <-----unused
;; "P" <-----unused
;; "Q" "A signed 12-bit constant"
;; "R" <-----unused
;; "S" <-----unused
;; "T" <-----unused
;; "U" <-----unused
;; "V" "Matches a non-offsettable memory reference." (Global non-architectural)
;; "W" "A memory address based on a member of @code{BASE_REG_CLASS}.  This is
;;     true for all references (although it can sometimes be implicit
;;     if @samp{!TARGET_EXPLICIT_RELOCS})."
;; "X" "Matches anything." (Global non-architectural)
;; "Y" -
;;    "Yd"
;;       "A constant @code{move_operand} that can be safely loaded into @code{$25}
;;       using @code{la}."
;;    "Yx"
;; "Z" -
;;    "ZC"
;;      "A memory operand whose address is formed by a base register and offset
;;       that is suitable for use in instructions with the same addressing mode
;;       as @code{ll.w} and @code{sc.w}."
;;    "ZB"
;;      "An address that is held in a general-purpose register.
;;      The offset is zero"
;; "<" "Matches a pre-dec or post-dec operand." (Global non-architectural)
;; ">" "Matches a pre-inc or post-inc operand." (Global non-architectural)

(define_constraint "a"
  "@internal
   A constant call global and noplt address."
  (match_operand 0 "is_const_call_global_noplt_symbol"))

(define_constraint "c"
  "@internal
   A constant call local address."
  (match_operand 0 "is_const_call_local_symbol"))

(define_register_constraint "d" "GR_REGS"
  "A general-purpose register.  This is equivalent to @code{r}.")

(define_register_constraint "e" "JALR_REGS"
  "@internal")

(define_register_constraint "f" "TARGET_HARD_FLOAT ? FP_REGS : NO_REGS"
  "A floating-point register (if available).")

(define_constraint "h"
  "@internal
   A constant call plt address."
  (match_operand 0 "is_const_call_plt_symbol"))

(define_register_constraint "j" "SIBCALL_REGS"
  "@internal")

(define_constraint "l"
"A signed 16-bit constant ."
(and (match_code "const_int")
     (match_test "IMM16_OPERAND (ival)")))

(define_memory_constraint "m"
  "A memory operand whose address is formed by a base register and offset
   that is suitable for use in instructions with the same addressing mode
   as @code{st.w} and @code{ld.w}."
  (and (match_code "mem")
       (match_test "loongarch_12bit_offset_address_p (XEXP (op, 0), mode)")))

(define_register_constraint "q" "CSR_REGS"
  "A general-purpose register except for $r0 and $r1 for lcsr.")

(define_constraint "t"
  "@internal
   A constant call weak address."
  (match_operand 0 "is_const_call_weak_symbol"))

(define_constraint "u"
  "A signed 52bit constant and low 32-bit is zero (for logic instructions)."
  (and (match_code "const_int")
       (match_test "LU32I_OPERAND (ival)")))

(define_constraint "v"
  "A nsigned 64-bit constant and low 44-bit is zero (for logic instructions)."
  (and (match_code "const_int")
       (match_test "LU52I_OPERAND (ival)")))

(define_register_constraint "z" "ST_REGS"
  "A floating-point condition code register.")

;; Floating-point constraints

(define_constraint "G"
  "Floating-point zero."
  (and (match_code "const_double")
       (match_test "op == CONST0_RTX (mode)")))

;; Integer constraints

(define_constraint "I"
  "A signed 12-bit constant (for arithmetic instructions)."
  (and (match_code "const_int")
       (match_test "SMALL_OPERAND (ival)")))

(define_constraint "J"
  "Integer zero."
  (and (match_code "const_int")
       (match_test "ival == 0")))

(define_constraint "K"
  "An unsigned 12-bit constant (for logic instructions)."
  (and (match_code "const_int")
       (match_test "SMALL_OPERAND_UNSIGNED (ival)")))

;; General constraints

(define_constraint "Q"
  "@internal"
  (match_operand 0 "const_arith_operand"))

(define_memory_constraint "W"
  "@internal
   A memory address based on a member of @code{BASE_REG_CLASS}.  This is
   true for allreferences (although it can sometimes be implicit
   if @samp{!TARGET_EXPLICIT_RELOCS})."
  (and (match_code "mem")
       (match_operand 0 "memory_operand")
	    (and (not (match_operand 0 "stack_operand"))
		 (not (match_test "CONSTANT_P (XEXP (op, 0))")))))

(define_constraint "Yd"
  "@internal
   A constant @code{move_operand} that can be safely loaded into @code{$25}
   using @code{la}."
  (and (match_operand 0 "move_operand")
       (match_test "CONSTANT_P (op)")))

(define_constraint "Yx"
   "@internal"
   (match_operand 0 "low_bitmask_operand"))

(define_memory_constraint "ZC"
  "A memory operand whose address is formed by a base register and offset
   that is suitable for use in instructions with the same addressing mode
   as @code{ll.w} and @code{sc.w}."
  (and (match_code "mem")
       (match_test "loongarch_14bit_shifted_offset_address_p (XEXP (op, 0), mode)")))

(define_memory_constraint "ZB"
  "@internal
  An address that is held in a general-purpose register.
  The offset is zero"
  (and (match_code "mem")
       (match_test "GET_CODE(XEXP(op,0)) == REG")))

