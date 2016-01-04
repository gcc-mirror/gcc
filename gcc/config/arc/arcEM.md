;; DFA scheduling description of the Synopsys DesignWare ARC EM cpu
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

(define_automaton "ARCEM")

(define_cpu_unit "em_issue, ld_st, mul_em, divrem_em" "ARCEM")

(define_insn_reservation "em_data_load" 2
  (and (match_test "TARGET_EM")
       (eq_attr "type" "load"))
  "em_issue+ld_st,nothing")

(define_insn_reservation "em_data_store" 1
  (and (match_test "TARGET_EM")
       (eq_attr "type" "store"))
  "em_issue+ld_st")

;; Multipliers options
(define_insn_reservation "mul_em_mpyw_1" 1
  (and (match_test "TARGET_EM")
       (match_test "arc_mpy_option > 0")
       (match_test "arc_mpy_option <= 2")
       (eq_attr "type" "mul16_em"))
  "em_issue+mul_em")

(define_insn_reservation "mul_em_mpyw_2" 2
  (and (match_test "TARGET_EM")
       (match_test "arc_mpy_option > 2")
       (match_test "arc_mpy_option <= 5")
       (eq_attr "type" "mul16_em"))
  "em_issue+mul_em, nothing")

(define_insn_reservation "mul_em_mpyw_4" 4
  (and (match_test "TARGET_EM")
       (match_test "arc_mpy_option == 6")
       (eq_attr "type" "mul16_em"))
  "em_issue+mul_em, mul_em*3")

(define_insn_reservation "mul_em_multi_wlh1" 1
  (and (match_test "TARGET_EM")
       (match_test "arc_mpy_option == 2")
       (eq_attr "type" "multi,umulti"))
  "em_issue+mul_em")

(define_insn_reservation "mul_em_multi_wlh2" 2
  (and (match_test "TARGET_EM")
       (match_test "arc_mpy_option == 3")
       (eq_attr "type" "multi,umulti"))
  "em_issue+mul_em, nothing")

(define_insn_reservation "mul_em_multi_wlh3" 3
  (and (match_test "TARGET_EM")
       (match_test "arc_mpy_option == 4")
       (eq_attr "type" "multi,umulti"))
  "em_issue+mul_em, mul_em*2")

;; FIXME! Make the difference between MPY and MPYM for WLH4
(define_insn_reservation "mul_em_multi_wlh4" 4
  (and (match_test "TARGET_EM")
       (match_test "arc_mpy_option == 5")
       (eq_attr "type" "multi,umulti"))
  "em_issue+mul_em, mul_em*4")

(define_insn_reservation "mul_em_multi_wlh5" 9
  (and (match_test "TARGET_EM")
       (match_test "arc_mpy_option == 6")
       (eq_attr "type" "multi,umulti"))
  "em_issue+mul_em, mul_em*8")

;; Radix-4 divider timing
(define_insn_reservation "em_divrem" 3
  (and (match_test "TARGET_EM")
       (match_test "TARGET_DIVREM")
       (eq_attr "type" "div_rem"))
  "em_issue+mul_em+divrem_em, (mul_em+divrem_em)*2")
