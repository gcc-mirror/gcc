;; DFA scheduling description of the Synopsys DesignWare ARC HS4x cpu
;; for GNU C compiler
;; Copyright (C) 2017-2023 Free Software Foundation, Inc.

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

(define_automaton "ARCHS4x")

(define_cpu_unit "hs4x_issue0" "ARCHS4x")
(define_cpu_unit "hs4x_issue1" "ARCHS4x")
(define_cpu_unit "hs4x_ld_st" "ARCHS4x")
(define_cpu_unit "hs4x_divrem" "ARCHS4x")
(define_cpu_unit "hs4x_mult" "ARCHS4x")
(define_cpu_unit "hs4x_x1, hs4x_x2" "ARCHS4x")
(define_cpu_unit "hs4x_y1, hs4x_y2" "ARCHS4x")
(define_cpu_unit "hs4x_brcc0, hs4x_brcc1" "ARCHS4x")

(define_insn_reservation "hs4x_brj_op" 1
  (and (match_test "TARGET_HS")
       (eq_attr "tune" "archs4x, archs4xd")
       (eq_attr "type" "call, call_no_delay_slot, uncond_branch, jump, \
branch, sfunc"))
  "hs4x_issue0")

(define_insn_reservation "hs4x_brcc_op" 1
  (and (match_test "TARGET_HS")
       (eq_attr "tune" "archs4x, archs4xd")
       (eq_attr "type" "brcc,brcc_no_delay_slot,loop_end"))
  "hs4x_issue0 + hs4x_brcc0 + hs4x_brcc1")

(define_insn_reservation "hs4x_data_load_op" 4
  (and (match_test "TARGET_HS")
       (eq_attr "tune" "archs4x, archs4xd")
       (eq_attr "type" "load"))
  "hs4x_issue1 + hs4x_ld_st,hs4x_ld_st")

(define_insn_reservation "hs4x_data_store_op" 1
  (and (match_test "TARGET_HS")
       (eq_attr "tune_store" "normal")
       (eq_attr "type" "store"))
  "hs4x_issue1 + hs4x_ld_st")

(define_insn_reservation "hs4x_data_store_1_op" 2
  (and (match_test "TARGET_HS")
       (eq_attr "tune_store" "rel31a")
       (eq_attr "type" "store"))
  "hs4x_issue1 + hs4x_ld_st + hs4x_brcc0, hs4x_brcc1")

;; Advanced ALU
(define_insn_reservation "hs4x_adv_alue_op" 4
  (and (match_test "TARGET_HS")
       (eq_attr "tune" "archs4x, archs4xd")
       (eq_attr "type" "cc_arith, two_cycle_core, shift, lr, sr"))
  "(hs4x_issue0 | hs4x_issue1), hs4x_x1")

(define_insn_reservation "hs4x_adv_alul_op" 6
  (and (match_test "TARGET_HS")
       (eq_attr "tune" "archs4xd")
       (eq_attr "type" "cc_arith, two_cycle_core, shift, lr, sr"))
  "(hs4x_issue0 | hs4x_issue1), nothing*2, hs4x_x2")

;; Basic ALU
(define_insn_reservation "hs4x_basic_alue_op" 1
  (and (match_test "TARGET_HS")
       (eq_attr "tune" "archs4x, archs4xd")
       (eq_attr "type" "move, cmove, unary, binary, compare, misc"))
  "(hs4x_issue0 | hs4x_issue1) + hs4x_y1")

(define_insn_reservation "hs4x_basic_alul_op" 4
  (and (match_test "TARGET_HS")
       (eq_attr "tune" "archs4x, archs4xd")
       (eq_attr "type" "move, cmove, unary, binary, compare, misc"))
  "(hs4x_issue0 | hs4x_issue1), nothing*2, hs4x_y2")

(define_insn_reservation "hs4x_divrem_op" 13
  (and (match_test "TARGET_HS")
       (eq_attr "tune" "archs4x, archs4xd")
       (eq_attr "type" "div_rem"))
  "hs4x_issue0 + hs4x_divrem, (hs4x_divrem)*12")

;;Consider the DSPMPY fast here
(define_insn_reservation "hs4x_mul_fast_op" 7
  (and (match_test "TARGET_HS")
       (eq_attr "tune_dspmpy" "fast")
       (eq_attr "type" "mul16_em, multi, umulti"))
  "hs4x_issue0 + hs4x_mult")

(define_insn_reservation "hs4x_mul_slow_op" 8
  (and (match_test "TARGET_HS")
       (eq_attr "tune_dspmpy" "slow")
       (eq_attr "type" "mul16_em, multi, umulti"))
  "hs4x_issue0 + hs4x_mult")

;; FPU unit
(define_insn_reservation "hs4x_fpu_op" 8
  (and (match_test "TARGET_HS")
       (eq_attr "tune" "archs4x, archs4xd")
       (eq_attr "type" "fpu"))
  "hs4x_issue0")

;; FPU FUSE unit
(define_insn_reservation "hs4x_fpu_fuse_op" 12
  (and (match_test "TARGET_HS")
       (eq_attr "tune" "archs4x, archs4xd")
       (eq_attr "type" "fpu_fuse"))
  "hs4x_issue0")

;; FPU SP SQRT/DIV unit
(define_insn_reservation "hs4x_fpu_sdiv_op" 20
  (and (match_test "TARGET_HS")
       (eq_attr "tune" "archs4x, archs4xd")
       (eq_attr "type" "fpu_sdiv"))
  "hs4x_issue0")

;; FPU DP SQRT/DIV unit
(define_insn_reservation "hs4x_fpu_ddiv_op" 34
  (and (match_test "TARGET_HS")
       (eq_attr "tune" "archs4x, archs4xd")
       (eq_attr "type" "fpu_ddiv"))
  "hs4x_issue0")

;; FPU CVT unit
(define_insn_reservation "hs4x_fpu_cvt_op" 5
  (and (match_test "TARGET_HS")
       (eq_attr "tune" "archs4x, archs4xd")
       (eq_attr "type" "fpu_cvt"))
  "hs4x_issue0")

;; BYPASS Advanced ALU ->
(define_bypass 1 "hs4x_adv_alue_op" "hs4x_divrem_op")
(define_bypass 1 "hs4x_adv_alue_op" "hs4x_mul_*op")
(define_bypass 2 "hs4x_adv_alue_op" "hs4x_adv_alue_op")
(define_bypass 1 "hs4x_adv_alue_op" "hs4x_basic_alue_op")
(define_bypass 1 "hs4x_adv_alue_op" "hs4x_basic_alul_op")
(define_bypass 1 "hs4x_adv_alue_op" "hs4x_data_load_op")
(define_bypass 0 "hs4x_adv_alue_op" "hs4x_data_store_op" "store_data_bypass_p")
(define_bypass 2 "hs4x_adv_alue_op" "hs4x_data_store_op")
(define_bypass 1 "hs4x_adv_alue_op" "hs4x_fpu_*op")

(define_bypass 2 "hs4x_adv_alul_op" "hs4x_basic_alul_op")
(define_bypass 2 "hs4x_adv_alul_op" "hs4x_adv_alul_op")
(define_bypass 2 "hs4x_adv_alul_op" "hs4x_mul_*op")
(define_bypass 0 "hs4x_adv_alul_op" "hs4x_data_store_op" "store_data_bypass_p")
(define_bypass 4 "hs4x_adv_alul_op" "hs4x_divrem_op")
(define_bypass 5 "hs4x_adv_alul_op" "hs4x_fpu_*op")

;; BYPASS Basic ALU ->
(define_bypass 0 "hs4x_basic_alue_op" "hs4x_data_store_op" "store_data_bypass_p")

(define_bypass 1 "hs4x_basic_alul_op" "hs4x_basic_alul_op")
(define_bypass 1 "hs4x_basic_alul_op" "hs4x_adv_alul_op")
(define_bypass 0 "hs4x_basic_alul_op" "hs4x_data_store_op" "store_data_bypass_p")
(define_bypass 1 "hs4x_basic_alul_op" "hs4x_mul_*op")
(define_bypass 3 "hs4x_basic_alul_op" "hs4x_divrem_op")
(define_bypass 3 "hs4x_basic_alul_op" "hs4x_fpu_*op")

;; BYPASS LD ->
(define_bypass 1 "hs4x_data_load_op" "hs4x_basic_alul_op")
(define_bypass 1 "hs4x_data_load_op" "hs4x_adv_alul_op")
(define_bypass 3 "hs4x_data_load_op" "hs4x_divrem_op")
(define_bypass 3 "hs4x_data_load_op" "hs4x_data_load_op")
(define_bypass 3 "hs4x_data_load_op" "hs4x_mul_*op")
(define_bypass 0 "hs4x_data_load_op" "hs4x_data_store_op" "store_data_bypass_p")
(define_bypass 3 "hs4x_data_load_op" "hs4x_fpu_*op")

;; BYPASS FAST MPY ->
(define_bypass 4 "hs4x_mul_fast_op" "hs4x_basic_alul_op")
(define_bypass 4 "hs4x_mul_fast_op" "hs4x_adv_alul_op")
(define_bypass 4 "hs4x_mul_fast_op" "hs4x_mul_fast_op")
(define_bypass 6 "hs4x_mul_fast_op" "hs4x_divrem_op")
(define_bypass 0 "hs4x_mul_fast_op" "hs4x_data_store_op" "store_data_bypass_p")
(define_bypass 6 "hs4x_mul_fast_op" "hs4x_fpu_*op")

;; BYPASS SLOW MPY ->
(define_bypass 5 "hs4x_mul_slow_op" "hs4x_basic_alul_op")
(define_bypass 5 "hs4x_mul_slow_op" "hs4x_adv_alul_op")
(define_bypass 5 "hs4x_mul_slow_op" "hs4x_mul_slow_op")
(define_bypass 7 "hs4x_mul_slow_op" "hs4x_divrem_op")
(define_bypass 0 "hs4x_mul_slow_op" "hs4x_data_store_op" "store_data_bypass_p")
(define_bypass 7 "hs4x_mul_slow_op" "hs4x_fpu_*op")

;;BYPASS FPU ->
(define_bypass 5 "hs4x_fpu_op" "hs4x_basic_alul_op")
(define_bypass 5 "hs4x_fpu_op" "hs4x_adv_alul_op")
(define_bypass 5 "hs4x_fpu_op" "hs4x_mul_*op")
(define_bypass 7 "hs4x_fpu_op" "hs4x_divrem_op")
(define_bypass 5 "hs4x_fpu_op" "hs4x_fpu_*op")
(define_bypass 0 "hs4x_fpu_op" "hs4x_data_store_op" "store_data_bypass_p")

;;BYPASS FPU FUSE ->
(define_bypass 9  "hs4x_fpu_fuse_op" "hs4x_basic_alul_op")
(define_bypass 9  "hs4x_fpu_fuse_op" "hs4x_adv_alul_op")
(define_bypass 9  "hs4x_fpu_fuse_op" "hs4x_mul_*op")
(define_bypass 11 "hs4x_fpu_fuse_op" "hs4x_divrem_op")
(define_bypass 11 "hs4x_fpu_fuse_op" "hs4x_fpu_*op")
(define_bypass 0  "hs4x_fpu_fuse_op" "hs4x_data_store_op" "store_data_bypass_p")

;;BYPASS FPU SP DIV ->
(define_bypass 16 "hs4x_fpu_sdiv_op" "hs4x_basic_alul_op")
(define_bypass 16 "hs4x_fpu_sdiv_op" "hs4x_adv_alul_op")
(define_bypass 16 "hs4x_fpu_sdiv_op" "hs4x_mul_*op")
(define_bypass 19 "hs4x_fpu_sdiv_op" "hs4x_divrem_op")
(define_bypass 19 "hs4x_fpu_sdiv_op" "hs4x_fpu_*op")
(define_bypass 0  "hs4x_fpu_sdiv_op" "hs4x_data_store_op" "store_data_bypass_p")

;;BYPASS FPU DP DIV ->
(define_bypass 31 "hs4x_fpu_ddiv_op" "hs4x_basic_alul_op")
(define_bypass 31 "hs4x_fpu_ddiv_op" "hs4x_adv_alul_op")
(define_bypass 31 "hs4x_fpu_ddiv_op" "hs4x_mul_*op")
(define_bypass 34 "hs4x_fpu_ddiv_op" "hs4x_divrem_op")
(define_bypass 34 "hs4x_fpu_ddiv_op" "hs4x_fpu_*op")
(define_bypass 0  "hs4x_fpu_ddiv_op" "hs4x_data_store_op" "store_data_bypass_p")

;;BYPASS FPU CVT ->
(define_bypass 1 "hs4x_fpu_cvt_op" "hs4x_basic_alul_op")
(define_bypass 1 "hs4x_fpu_cvt_op" "hs4x_adv_alul_op")
(define_bypass 1 "hs4x_fpu_cvt_op" "hs4x_mul_*op")
(define_bypass 4 "hs4x_fpu_cvt_op" "hs4x_divrem_op")
(define_bypass 4 "hs4x_fpu_cvt_op" "hs4x_fpu_*op")
(define_bypass 0 "hs4x_fpu_cvt_op" "hs4x_data_store_op" "store_data_bypass_p")
