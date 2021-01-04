;; Constraint definitions for Tilera TILE-Gx.
;; Copyright (C) 2011-2021 Free Software Foundation, Inc.
;; Contributed by Walter Lee (walt@tilera.com)
;;
;; This file is part of GCC.
;;
;; GCC is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published
;; by the Free Software Foundation; either version 3, or (at your
;; option) any later version.
;;
;; GCC is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
;; License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GCC; see the file COPYING3.  If not see
;; <http://www.gnu.org/licenses/>.

(define_register_constraint "R00" "R0_REGS"  "r0")
(define_register_constraint "R01" "R1_REGS"  "r1")
(define_register_constraint "R02" "R2_REGS"  "r2")
(define_register_constraint "R03" "R3_REGS"  "r3")
(define_register_constraint "R04" "R4_REGS"  "r4")
(define_register_constraint "R05" "R5_REGS"  "r5")
(define_register_constraint "R06" "R6_REGS"  "r6")
(define_register_constraint "R07" "R7_REGS"  "r7")
(define_register_constraint "R08" "R8_REGS"  "r8")
(define_register_constraint "R09" "R9_REGS"  "r9")
(define_register_constraint "R10" "R10_REGS" "r10")

(define_constraint "I"
  "A signed 8 bit constant"
  (and (match_code "const_int")
       (match_test "ival >= -128 && ival <= 127")))

(define_constraint "J"
  "Signed 16-bit integer constant"
  (and (match_code "const_int")
       (match_test "ival >= -32768 && ival <= 32767")))

(define_constraint "K"
  "Unsigned 16-bit integer constant"
  (and (match_code "const_int")
       (match_test "(ival >= 0 && ival <= 65535)")))

(define_constraint "L"
  "Integer constant that fits in one signed byte when incremented"
  (and (match_code "const_int")
       (match_test "ival >= -129 && ival <= 126")))

(define_constraint "M"
  "A bit mask suitable for 'bfins'"
  (and (match_code "const_int")
       (match_test "tilegx_bitfield_operand_p (ival, NULL, NULL)")))

(define_constraint "N"
  "Integer constant that is a byte tiled out eight times"
  (and (match_code "const_int")
       (match_test "(ival == (ival & 0xFF) * 0x0101010101010101LL)")))

(define_constraint "O"
 "The integer zero constant"
 (and (match_code "const_int")
      (match_test "ival == 0")))

(define_constraint "P"
  "Integer constant that is a sign-extended byte tiled out as four shorts"
  (and (match_code "const_int")
       (match_test "(ival
                     == ((trunc_int_for_mode (ival, QImode) & 0xFFFF)
                         * 0x0001000100010001LL))")))

(define_constraint "Q"
  "Integer constant that fits in one signed byte when incremented, but not -1"
  (and (match_code "const_int")
       (match_test "ival >= -129 && ival <= 126 && ival != -1")))

(define_constraint "S"
  "Integer constant that has all 1 bits consecutive and starting at bit 0"
  (and (match_code "const_int")
       (match_test "ival != 0 && (ival & (ival + 1)) == 0")))

(define_constraint "T"
  "An unspec wrapper for a symbolc operand"
  (ior (match_operand 0 "const_last_symbolic_operand")
       (match_operand 0 "const_symbolic_operand")))

(define_memory_constraint "U"
  "Non-auto-incrementing memory"
  (and (match_code "mem")
       (match_test "GET_RTX_CLASS (GET_CODE (XEXP (op, 0))) != RTX_AUTOINC")))

(define_constraint "W"
  "An 8-element vector constant with identical elements"
  (and (match_code "const_vector")
       (match_test "CONST_VECTOR_NUNITS (op) == 8")
       (match_test "const_vec_duplicate_p (op)")))

(define_constraint "Y"
  "A 4-element vector constant with identical elements"
  (and (match_code "const_vector")
       (match_test "CONST_VECTOR_NUNITS (op) == 4")
       (match_test "const_vec_duplicate_p (op)")))

(define_constraint "Z0"
 "The integer constant 0xffffffff"
 (and (match_code "const_int")
      (match_test "ival == 0xffffffff")))

(define_constraint "Z1"
 "The integer constant 0xffffffff00000000"
 (and (match_code "const_int")
      (match_test "ival == (HOST_WIDE_INT)0xffffffff00000000LL")))
