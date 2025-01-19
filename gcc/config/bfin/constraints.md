;; Constraint definitions for Blackfin
;; Copyright (C) 2008-2025 Free Software Foundation, Inc.
;; Contributed by Analog Devices

;; This file is part of GCC.

;; GCC is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published
;; by the Free Software Foundation; either version 3, or (at your
;; option) any later version.

;; GCC is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
;; License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GCC; see the file COPYING3.  If not see
;; <http://www.gnu.org/licenses/>.

(define_register_constraint "a" "PREGS"
  "A Pn register.")

(define_register_constraint "d" "DREGS"
  "A Rn register.")

(define_register_constraint "z" "PREGS_CLOBBERED"
  "A call clobbered Pn register.")

(define_register_constraint "D" "EVEN_DREGS"
  "An even-numbered Rn register.")

(define_register_constraint "W" "ODD_DREGS"
  "An odd-numbered Rn register.")

(define_register_constraint "e" "AREGS"
  "An accumulator register.")

(define_register_constraint "A" "EVEN_AREGS"
  "An even-numbered accumulator; A0.")

(define_register_constraint "B" "ODD_AREGS"
  "An odd-numbered accumulator; A1.")

(define_register_constraint "b" "IREGS"
  "An I register.")

(define_register_constraint "v" "BREGS"
  "A B register.")

(define_register_constraint "f" "MREGS"
  "An M register.")

(define_register_constraint "c" "CIRCREGS"
  "A register used for circular buffering, i.e. I, B, or L registers.")

(define_register_constraint "C" "CCREGS"
  "The CC register.")

(define_register_constraint "t" "LT_REGS"
  "LT0 or LT1.")

(define_register_constraint "u" "LB_REGS"
  "LB0 or LB1.")

(define_register_constraint "k" "LC_REGS"
  "LC0 or LC1.")

(define_register_constraint "x" "MOST_REGS"
  "Any R, P, B, M, I or L register.")

(define_register_constraint "y" "PROLOGUE_REGS"
  "Additional registers typically used only in prologues and epilogues:
   RETS, RETN, RETI, RETX, RETE, ASTAT, SEQSTAT and USP.")

(define_register_constraint "w" "NON_A_CC_REGS"
  "Any register except accumulators or CC.")

(define_register_constraint "Z" "FDPIC_REGS"
  "@internal The FD-PIC GOT pointer; P3.")

(define_register_constraint "Y" "FDPIC_FPTR_REGS"
  "@internal The FD-PIC function pointer register; P1.")

(define_register_constraint "q0" "D0REGS"
  "The register R0.")

(define_register_constraint "q1" "D1REGS"
  "The register R1.")

(define_register_constraint "q2" "D2REGS"
  "The register R2.")

(define_register_constraint "q3" "D3REGS"
  "The register R3.")

(define_register_constraint "q4" "D4REGS"
  "The register R4.")

(define_register_constraint "q5" "D5REGS"
  "The register R5.")

(define_register_constraint "q6" "D6REGS"
  "The register R6.")

(define_register_constraint "q7" "D7REGS"
  "The register R7.")

(define_register_constraint "qA" "P0REGS"
  "The register P0.")

;; Constant constraints.

(define_constraint "J"
  "A constant value of the form 2**N, where N 5-bit wide."
  (and (match_code "const_int")
       (match_test "log2constp (ival)")))

(define_constraint "Ks3"
  "A signed 3 bit immediate."
  (and (match_code "const_int")
       (match_test "ival >= -4 && ival <= 3")))

(define_constraint "Ku3"
  "An unsigned 3 bit immediate."
  (and (match_code "const_int")
       (match_test "ival >= 0 && ival <= 7")))

(define_constraint "Ks4"
  "A signed 4 bit immediate."
  (and (match_code "const_int")
       (match_test "ival >= -8 && ival <= 7")))

(define_constraint "Ku4"
  "An unsigned 4 bit immediate."
  (and (match_code "const_int")
       (match_test "ival >= 0 && ival <= 15")))

(define_constraint "Ks5"
  "A signed 5 bit immediate."
  (and (match_code "const_int")
       (match_test "ival >= -16 && ival <= 15")))

(define_constraint "Ku5"
  "An unsigned 5 bit immediate."
  (and (match_code "const_int")
       (match_test "ival >= 0 && ival <= 31")))

(define_constraint "Ks7"
  "A signed 7 bit immediate."
  (and (match_code "const_int")
       (match_test "ival >= -64 && ival <= 63")))

(define_constraint "KN7"
  "A constant that when negated is a signed 7 bit immediate."
  (and (match_code "const_int")
       (match_test "ival >= -63 && ival <= 64")))

(define_constraint "Ksh"
  "A signed 16 bit immediate."
  (and (match_code "const_int")
       (match_test "ival >= -32768 && ival <= 32767")))

(define_constraint "Kuh"
  "An unsigned 16 bit immediate."
  (and (match_code "const_int")
       (match_test "ival >= 0 && ival <= 65535")))

(define_constraint "L"
  "A constant value of the form ~(2**N)."
  (and (match_code "const_int")
       (match_test "log2constp (~ival)")))

(define_constraint "M1"
  "An integer with the value 255."
  (and (match_code "const_int")
       (match_test "ival == 255")))

(define_constraint "M2"
  "An integer with the value 65535."
  (and (match_code "const_int")
       (match_test "ival == 65535")))

(define_constraint "P0"
  "An integer with the value 0."
  (and (match_code "const_int")
       (match_test "ival == 0")))

(define_constraint "P1"
  "An integer with the value 1."
  (and (match_code "const_int")
       (match_test "ival == 1")))

(define_constraint "P2"
  "An integer with the value 2."
  (and (match_code "const_int")
       (match_test "ival == 2")))

(define_constraint "P3"
  "An integer with the value 3."
  (and (match_code "const_int")
       (match_test "ival == 3")))

(define_constraint "P4"
  "An integer with the value 4."
  (and (match_code "const_int")
       (match_test "ival == 4")))

(define_constraint "PA"
  "An integer constant describing any macflag except variants involving M."
  (and (match_code "const_int")
       (match_test "ival != MACFLAG_M && ival != MACFLAG_IS_M")))

(define_constraint "PB"
  "An integer constant describing any macflag involving M."
  (and (match_code "const_int")
       (match_test "ival == MACFLAG_M || ival == MACFLAG_IS_M")))


;; Extra constraints

(define_constraint "Q"
  "A SYMBOL_REF."
  (match_code "symbol_ref"))

