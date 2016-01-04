;; DFA scheduling description of the Synopsys DesignWare ARC HS cpu
;; for GNU C compiler
;; Copyright (C) 2007-2016 Free Software Foundation, Inc.
;; Contributor: Claudiu Zissulescu <claudiu.zissulescu@synopsys.com>

;; This file is part of GCC.

;; GCC is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; GCC is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GCC; see the file COPYING3.  If not see
;; <http://www.gnu.org/licenses/>.

(define_automaton "ARCHS")

(define_cpu_unit "hs_issue, hs_ld_st, divrem_hs, mul_hs, x1, x2" "ARCHS")

(define_insn_reservation "hs_data_load" 4
  (and (match_test "TARGET_HS")
       (eq_attr "type" "load"))
  "hs_issue+hs_ld_st,hs_ld_st,nothing*2")

(define_insn_reservation "hs_data_store" 1
  (and (match_test "TARGET_HS")
       (eq_attr "type" "store"))
  "hs_issue+hs_ld_st")

(define_insn_reservation "hs_alu0" 2
  (and (match_test "TARGET_HS")
       (eq_attr "type" "cc_arith, two_cycle_core, shift, lr, sr"))
  "hs_issue+x1,x2")

(define_insn_reservation "hs_alu1" 4
  (and (match_test "TARGET_HS")
       (eq_attr "type" "move, cmove, unary, binary, compare, misc"))
  "hs_issue+x1, nothing*3")

(define_insn_reservation "hs_divrem" 13
  (and (match_test "TARGET_HS")
       (match_test "TARGET_DIVREM")
       (eq_attr "type" "div_rem"))
  "hs_issue+divrem_hs, (divrem_hs)*12")

(define_insn_reservation "hs_mul" 3
  (and (match_test "TARGET_HS")
       (eq_attr "type" "mul16_em, multi, umulti"))
  "hs_issue+mul_hs, nothing*3")

;; BYPASS EALU ->
(define_bypass 1 "hs_alu0" "hs_divrem")
(define_bypass 1 "hs_alu0" "hs_mul")

;; BYPASS BALU ->
(define_bypass 1 "hs_alu1" "hs_alu1")
(define_bypass 1 "hs_alu1" "hs_data_store" "store_data_bypass_p")

;; BYPASS LD ->
(define_bypass 1 "hs_data_load" "hs_alu1")
(define_bypass 3 "hs_data_load" "hs_divrem")
(define_bypass 3 "hs_data_load" "hs_data_load")
(define_bypass 3 "hs_data_load" "hs_mul")
(define_bypass 1 "hs_data_load" "hs_data_store" "store_data_bypass_p")

;; BYPASS MPY ->
;;(define_bypass 3 "hs_mul" "hs_mul")
(define_bypass 1 "hs_mul" "hs_alu1")
(define_bypass 3 "hs_mul" "hs_divrem")
(define_bypass 1 "hs_mul" "hs_data_store" "store_data_bypass_p")
