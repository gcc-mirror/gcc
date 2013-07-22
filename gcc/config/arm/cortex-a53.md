;; ARM Cortex-A53 pipeline description
;; Copyright (C) 2013 Free Software Foundation, Inc.
;;
;; Contributed by ARM Ltd.
;;
;; This file is part of GCC.
;;
;; GCC is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; GCC is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GCC; see the file COPYING3.  If not see
;; <http://www.gnu.org/licenses/>.

(define_automaton "cortex_a53")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Functional units.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; There are two main integer execution pipelines, described as
;; slot 0 and issue slot 1.

(define_cpu_unit "cortex_a53_slot0" "cortex_a53")
(define_cpu_unit "cortex_a53_slot1" "cortex_a53")

(define_reservation "cortex_a53_slot_any" "cortex_a53_slot0|cortex_a53_slot1")
(define_reservation "cortex_a53_single_issue" "cortex_a53_slot0+cortex_a53_slot1")

;; The load/store pipeline.  Load/store instructions can dual-issue from
;; either pipeline, but two load/stores cannot simultaneously issue.

(define_cpu_unit "cortex_a53_ls" "cortex_a53")

;; The store pipeline.  Shared between both execution pipelines.

(define_cpu_unit "cortex_a53_store" "cortex_a53")

;; The branch pipeline.  Branches can dual-issue with other instructions
;; (except when those instructions take multiple cycles to issue).

(define_cpu_unit "cortex_a53_branch" "cortex_a53")

;; The integer divider.

(define_cpu_unit "cortex_a53_idiv" "cortex_a53")

;; The floating-point add pipeline used to model the usage
;; of the add pipeline by fmac instructions.

(define_cpu_unit "cortex_a53_fpadd_pipe" "cortex_a53")

;; Floating-point div/sqrt (long latency, out-of-order completion).

(define_cpu_unit "cortex_a53_fp_div_sqrt" "cortex_a53")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ALU instructions.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define_insn_reservation "cortex_a53_alu" 2
  (and (eq_attr "tune" "cortexa53")
       (eq_attr "type" "arlo_imm,arlo_reg,shift,shift_reg,\
                        mov_imm,mov_reg,mvn_imm,mvn_reg"))
  "cortex_a53_slot_any")

(define_insn_reservation "cortex_a53_alu_shift" 2
  (and (eq_attr "tune" "cortexa53")
       (eq_attr "type" "arlo_shift,arlo_shift_reg,\
                        mov_shift,mov_shift_reg,\
                        mvn_shift,mvn_shift_reg"))
  "cortex_a53_slot_any")

;; Forwarding path for unshifted operands.

(define_bypass 1 "cortex_a53_alu,cortex_a53_alu_shift"
  "cortex_a53_alu")

(define_bypass 1 "cortex_a53_alu,cortex_a53_alu_shift"
  "cortex_a53_alu_shift"
  "arm_no_early_alu_shift_dep")

;; The multiplier pipeline can forward results so there's no need to specify
;; bypasses. Multiplies can only single-issue currently.

(define_insn_reservation "cortex_a53_mul" 3
  (and (eq_attr "tune" "cortexa53")
       (ior (eq_attr "mul32" "yes")
            (eq_attr "mul64" "yes")))
  "cortex_a53_single_issue")

;; A multiply with a single-register result or an MLA, followed by an
;; MLA with an accumulator dependency, has its result forwarded so two
;; such instructions can issue back-to-back.

(define_bypass 1 "cortex_a53_mul"
               "cortex_a53_mul"
               "arm_mac_accumulator_is_mul_result")

;; Punt with a high enough latency for divides.
(define_insn_reservation "cortex_a53_udiv" 8
  (and (eq_attr "tune" "cortexa53")
       (eq_attr "type" "udiv"))
  "(cortex_a53_slot0+cortex_a53_idiv),cortex_a53_idiv*7")

(define_insn_reservation "cortex_a53_sdiv" 9
  (and (eq_attr "tune" "cortexa53")
       (eq_attr "type" "sdiv"))
  "(cortex_a53_slot0+cortex_a53_idiv),cortex_a53_idiv*8")


(define_bypass 2 "cortex_a53_mul,cortex_a53_udiv,cortex_a53_sdiv"
               "cortex_a53_alu")
(define_bypass 2 "cortex_a53_mul,cortex_a53_udiv,cortex_a53_sdiv"
               "cortex_a53_alu_shift"
               "arm_no_early_alu_shift_dep")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Load/store instructions.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Address-generation happens in the issue stage.

(define_insn_reservation "cortex_a53_load1" 3
  (and (eq_attr "tune" "cortexa53")
       (eq_attr "type" "load_byte,load1"))
  "cortex_a53_slot_any+cortex_a53_ls")

(define_insn_reservation "cortex_a53_store1" 2
  (and (eq_attr "tune" "cortexa53")
       (eq_attr "type" "store1"))
  "cortex_a53_slot_any+cortex_a53_ls+cortex_a53_store")

(define_insn_reservation "cortex_a53_load2" 3
  (and (eq_attr "tune" "cortexa53")
       (eq_attr "type" "load2"))
  "cortex_a53_single_issue+cortex_a53_ls")

(define_insn_reservation "cortex_a53_store2" 2
  (and (eq_attr "tune" "cortexa53")
       (eq_attr "type" "store2"))
  "cortex_a53_single_issue+cortex_a53_ls+cortex_a53_store")

(define_insn_reservation "cortex_a53_load3plus" 4
  (and (eq_attr "tune" "cortexa53")
       (eq_attr "type" "load3,load4"))
  "(cortex_a53_single_issue+cortex_a53_ls)*2")

(define_insn_reservation "cortex_a53_store3plus" 3
  (and (eq_attr "tune" "cortexa53")
       (eq_attr "type" "store3,store4"))
  "(cortex_a53_single_issue+cortex_a53_ls+cortex_a53_store)*2")

;; Load/store addresses are required early in Issue.
(define_bypass 3 "cortex_a53_load1,cortex_a53_load2,cortex_a53_load3plus,cortex_a53_alu,cortex_a53_alu_shift"
                 "cortex_a53_load*"
                 "arm_early_load_addr_dep")
(define_bypass 3 "cortex_a53_load1,cortex_a53_load2,cortex_a53_load3plus,cortex_a53_alu,cortex_a53_alu_shift"
                 "cortex_a53_store*"
                 "arm_early_store_addr_dep")

;; Load data can forward in the ALU pipeline
(define_bypass 2 "cortex_a53_load1,cortex_a53_load2"
               "cortex_a53_alu")
(define_bypass 2 "cortex_a53_load1,cortex_a53_load2"
               "cortex_a53_alu_shift"
               "arm_no_early_alu_shift_dep")

;; ALU ops can forward to stores.
(define_bypass 0 "cortex_a53_alu,cortex_a53_alu_shift"
                 "cortex_a53_store1,cortex_a53_store2,cortex_a53_store3plus"
                 "arm_no_early_store_addr_dep")

(define_bypass 1 "cortex_a53_mul,cortex_a53_udiv,cortex_a53_sdiv,cortex_a53_load1,cortex_a53_load2,cortex_a53_load3plus"
                 "cortex_a53_store1,cortex_a53_store2,cortex_a53_store3plus"
                 "arm_no_early_store_addr_dep")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Branches.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Currently models all branches as dual-issuable from either execution
;; slot, which isn't true for all cases. We still need to model indirect
;; branches.

(define_insn_reservation "cortex_a53_branch" 0
  (and (eq_attr "tune" "cortexa53")
       (eq_attr "type" "branch,call"))
  "cortex_a53_slot_any+cortex_a53_branch")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Floating-point arithmetic.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define_insn_reservation "cortex_a53_fpalu" 4
  (and (eq_attr "tune" "cortexa53")
       (eq_attr "type" "ffariths, fadds, ffarithd, faddd, fcpys, fmuls, f_cvt,\
			fcmps, fcmpd"))
  "cortex_a53_slot0+cortex_a53_fpadd_pipe")

(define_insn_reservation "cortex_a53_fconst" 2
  (and (eq_attr "tune" "cortexa53")
       (eq_attr "type" "fconsts,fconstd"))
  "cortex_a53_slot0+cortex_a53_fpadd_pipe")

(define_insn_reservation "cortex_a53_fpmul" 4
  (and (eq_attr "tune" "cortexa53")
       (eq_attr "type" "fmuls,fmuld"))
  "cortex_a53_slot0")

;; For single-precision multiply-accumulate, the add (accumulate) is issued after
;; the multiply completes. Model that accordingly.

(define_insn_reservation "cortex_a53_fpmac" 8
  (and (eq_attr "tune" "cortexa53")
       (eq_attr "type" "fmacs,fmacd,ffmas,ffmad"))
  "cortex_a53_slot0, nothing*3, cortex_a53_fpadd_pipe")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Floating-point divide/square root instructions.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; fsqrt really takes one cycle less, but that is not modelled.

(define_insn_reservation "cortex_a53_fdivs" 14
  (and (eq_attr "tune" "cortexa53")
       (eq_attr "type" "fdivs"))
  "cortex_a53_slot0, cortex_a53_fp_div_sqrt * 13")

(define_insn_reservation "cortex_a53_fdivd" 29
  (and (eq_attr "tune" "cortexa53")
       (eq_attr "type" "fdivd"))
  "cortex_a53_slot0, cortex_a53_fp_div_sqrt * 28")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; VFP to/from core transfers.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define_insn_reservation "cortex_a53_r2f" 4
  (and (eq_attr "tune" "cortexa53")
       (eq_attr "type" "r_2_f"))
  "cortex_a53_slot0")

(define_insn_reservation "cortex_a53_f2r" 2
  (and (eq_attr "tune" "cortexa53")
       (eq_attr "type" "f_2_r"))
  "cortex_a53_slot0")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; VFP flag transfer.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define_insn_reservation "cortex_a53_f_flags" 4
  (and (eq_attr "tune" "cortexa53")
       (eq_attr "type" "f_flag"))
  "cortex_a53_slot0")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; VFP load/store.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define_insn_reservation "cortex_a53_f_loads" 4
  (and (eq_attr "tune" "cortexa53")
       (eq_attr "type" "f_loads"))
  "cortex_a53_slot0")

(define_insn_reservation "cortex_a53_f_loadd" 5
  (and (eq_attr "tune" "cortexa53")
       (eq_attr "type" "f_loadd"))
  "cortex_a53_slot0")

(define_insn_reservation "cortex_a53_f_stores" 0
  (and (eq_attr "tune" "cortexa53")
       (eq_attr "type" "f_stores"))
  "cortex_a53_slot0")

(define_insn_reservation "cortex_a53_f_stored" 0
  (and (eq_attr "tune" "cortexa53")
       (eq_attr "type" "f_stored"))
  "cortex_a53_slot0")

;; Load-to-use for floating-point values has a penalty of one cycle,
;; i.e. a latency of two.

(define_bypass 2 "cortex_a53_f_loads"
                 "cortex_a53_fpalu, cortex_a53_fpmac, cortex_a53_fpmul,\
		  cortex_a53_fdivs, cortex_a53_fdivd,\
		  cortex_a53_f2r")

(define_bypass 2 "cortex_a53_f_loadd"
                 "cortex_a53_fpalu, cortex_a53_fpmac, cortex_a53_fpmul,\
		  cortex_a53_fdivs, cortex_a53_fdivd,\
		  cortex_a53_f2r")

