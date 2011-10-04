;; Constraint definitions for SPARC.
;; Copyright (C) 2008, 2010 Free Software Foundation, Inc.
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

;;; Unused letters:
;;;    AB                       
;;;    a        jkl    q  tuvwxyz


;; Register constraints

(define_register_constraint "b" "(TARGET_V9 && TARGET_VIS ? EXTRA_FP_REGS : NO_REGS)"
 "Any floating-point register in VIS mode")

(define_register_constraint "c" "FPCC_REGS"
 "Floating-point condition code register")

(define_register_constraint "d" "(TARGET_V9 && TARGET_VIS ? FP_REGS : NO_REGS)"
 "Lower floating-point register in VIS mode")

;; In the non-V9 case, coerce V9 'e' class to 'f', so we can use 'e' in the
;; MD file for V8 and V9.
(define_register_constraint "e" "(TARGET_FPU ? (TARGET_V9 ? EXTRA_FP_REGS : FP_REGS) : NO_REGS)"
 "Any floating-point register")

(define_register_constraint "f" "(TARGET_FPU ? FP_REGS : NO_REGS)"
 "Lower floating-point register")
 
(define_register_constraint "h" "(TARGET_V9 && TARGET_V8PLUS ? I64_REGS : NO_REGS)"
 "64-bit global or out register in V8+ mode")


;; Floating-point constant constraints

(define_constraint "G"
 "The floating-point zero constant"
 (and (match_code "const_double")
      (match_test "const_zero_operand (op, mode)")))

(define_constraint "C"
 "The floating-point all-ones constant"
 (and (match_code "const_double")
      (match_test "const_all_ones_operand (op, mode)")))

;; Integer constant constraints

(define_constraint "H"
 "Valid operand of double arithmetic operation"
 (and (match_code "const_double")
      (match_test "arith_double_operand (op, DImode)")))

(define_constraint "I"
 "Signed 13-bit integer constant"
 (and (match_code "const_int")
      (match_test "SPARC_SIMM13_P (ival)")))

(define_constraint "J"
 "The integer zero constant"
 (and (match_code "const_int")
      (match_test "ival == 0")))

(define_constraint "K"
 "Signed 32-bit constant that can be loaded with a sethi instruction"
 (and (match_code "const_int")
      (match_test "SPARC_SETHI32_P (ival)")))

(define_constraint "L"
 "Signed 11-bit integer constant"
 (and (match_code "const_int")
      (match_test "SPARC_SIMM11_P (ival)")))

(define_constraint "M"
 "Signed 10-bit integer constant"
 (and (match_code "const_int")
      (match_test "SPARC_SIMM10_P (ival)")))

(define_constraint "N"
 "Signed constant that can be loaded with a sethi instruction"
 (and (match_code "const_int")
      (match_test "SPARC_SETHI_P (ival)")))

(define_constraint "O"
 "The 4096 constant"
 (and (match_code "const_int")
      (match_test "ival == 4096")))

(define_constraint "P"
 "The integer constant -1"
 (and (match_code "const_int")
      (match_test "ival == -1")))

;; Extra constraints
;; Our memory extra constraints have to emulate the behavior of 'm' and 'o',
;; i.e. accept pseudo-registers during reload.

(define_constraint "D"
 "const_vector"
  (and (match_code "const_vector")
       (match_test "GET_MODE_CLASS (GET_MODE (op)) == MODE_VECTOR_INT")))

(define_constraint "Q"
 "Floating-point constant that can be loaded with a sethi instruction"
 (and (match_code "const_double")
      (match_test "fp_sethi_p (op)")))

(define_constraint "R"
 "Floating-point constant that can be loaded with a move instruction"
 (and (match_code "const_double")
      (match_test "fp_mov_p (op)")))

(define_constraint "S"
 "Floating-point constant that can be loaded with a high/lo_sum sequence"
 (and (match_code "const_double")
      (match_test "fp_high_losum_p (op)")))

;; Not needed in 64-bit mode
(define_constraint "T"
 "Memory reference whose address is aligned to 8-byte boundary"
 (and (match_test "TARGET_ARCH32")
      (match_code "mem,reg")
      (match_test "memory_ok_for_ldd (op)")))

;; Not needed in 64-bit mode
(define_constraint "U"
 "Pseudo-register or hard even-numbered integer register"
 (and (match_test "TARGET_ARCH32")
      (match_code "reg")
      (ior (match_test "REGNO (op) < FIRST_PSEUDO_REGISTER")
	   (not (match_test "reload_in_progress && reg_renumber [REGNO (op)] < 0")))
      (match_test "register_ok_for_ldd (op)")))

;; Equivalent to 'T' but available in 64-bit mode
(define_constraint "W"
 "Memory reference for 'e' constraint floating-point register"
 (and (match_code "mem,reg")
      (match_test "memory_ok_for_ldd (op)")))

(define_constraint "Y"
 "The vector zero constant"
 (and (match_code "const_vector")
      (match_test "const_zero_operand (op, mode)")))

(define_constraint "Z"
 "The vector all ones constant"
 (and (match_code "const_vector")
      (match_test "const_all_ones_operand (op, mode)")))
