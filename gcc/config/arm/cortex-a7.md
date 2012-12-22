;; ARM Cortex-A7 pipeline description
;; Copyright (C) 2012 Free Software Foundation, Inc.
;;
;; Contributed by ARM Ltd.
;; Based on cortex-a5.md which was originally contributed by CodeSourcery.
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

(define_automaton "cortex_a7")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Functional units.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The Cortex-A7 pipeline integer and vfp pipeline.  
;; The decode is the same for all instructions, so do not model it. 
;; We only model the first execution stage because
;; instructions always advance one stage per cycle in order. 
;; We model all of the LS, Branch, ALU, MAC and FPU pipelines together. 

(define_cpu_unit "cortex_a7_ex1, cortex_a7_ex2" "cortex_a7")

(define_reservation "cortex_a7_both" "cortex_a7_ex1+cortex_a7_ex2")

(define_cpu_unit "cortex_a7_branch" "cortex_a7")

;; Cortex-A7 is in order and can dual-issue under limited circumstances.
;; ex2 can be reserved only after ex1 is reserved.

(final_presence_set "cortex_a7_ex2" "cortex_a7_ex1")

;; Pseudo-unit for blocking the multiply pipeline when a double-precision
;; multiply is in progress.

(define_cpu_unit "cortex_a7_fpmul_pipe" "cortex_a7")

;; The floating-point add pipeline (ex1/f1 stage), used to model the usage
;; of the add pipeline by fmac instructions, etc.

(define_cpu_unit "cortex_a7_fpadd_pipe" "cortex_a7")

;; Floating-point div/sqrt (long latency, out-of-order completion).

(define_cpu_unit "cortex_a7_fp_div_sqrt" "cortex_a7")

;; Neon pipeline
(define_cpu_unit "cortex_a7_neon" "cortex_a7")

(define_reservation "cortex_a7_all" "cortex_a7_both+\
                                     cortex_a7_fpmul_pipe+\
                                     cortex_a7_fpadd_pipe+\
                                     cortex_a7_fp_div_sqrt+\
                                     cortex_a7_neon")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Branches.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; A direct branch can dual issue either as younger or older instruction,
;; but branches cannot dual issue with branches.
;; No latency as there is no result.

(define_insn_reservation "cortex_a7_branch" 0
  (and (eq_attr "tune" "cortexa7")
       (and (eq_attr "type" "branch")
            (eq_attr "neon_type" "none")))
  "(cortex_a7_ex2|cortex_a7_ex1)+cortex_a7_branch")

;; A call reserves all issue slots. The result is available the next cycle.
(define_insn_reservation "cortex_a7_call" 1
  (and (eq_attr "tune" "cortexa7")
       (and (eq_attr "type" "call")
            (eq_attr "neon_type" "none")))
  "cortex_a7_all")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ALU instructions.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ALU instruction with an immediate operand can dual-issue.
(define_insn_reservation "cortex_a7_alu_imm" 2
  (and (eq_attr "tune" "cortexa7")
       (and (ior (eq_attr "type" "simple_alu_imm")
                 (ior (eq_attr "type" "simple_alu_shift")
                      (and (eq_attr "insn" "mov")
                           (not (eq_attr "length" "8")))))
            (eq_attr "neon_type" "none")))
  "cortex_a7_ex2|cortex_a7_ex1")

;; ALU instruction with register operands can dual-issue
;; with a younger immediate-based instruction.
(define_insn_reservation "cortex_a7_alu_reg" 2
  (and (eq_attr "tune" "cortexa7")
       (and (eq_attr "type" "alu_reg")
            (eq_attr "neon_type" "none")))
  "cortex_a7_ex1")

(define_insn_reservation "cortex_a7_alu_shift" 2
  (and (eq_attr "tune" "cortexa7")
       (and (eq_attr "type" "alu_shift,alu_shift_reg")
            (eq_attr "neon_type" "none")))
  "cortex_a7_ex1")

;; Forwarding path for unshifted operands.
(define_bypass 1 "cortex_a7_alu_imm,cortex_a7_alu_reg,cortex_a7_alu_shift"
  "cortex_a7_alu_imm,cortex_a7_alu_reg,cortex_a7_mul")

(define_bypass 1 "cortex_a7_alu_imm,cortex_a7_alu_reg,cortex_a7_alu_shift"
  "cortex_a7_store*"
  "arm_no_early_store_addr_dep")

(define_bypass 1 "cortex_a7_alu_imm,cortex_a7_alu_reg,cortex_a7_alu_shift"
  "cortex_a7_alu_shift"
  "arm_no_early_alu_shift_dep")

;; The multiplier pipeline can forward results from wr stage only so
;; there's no need to specify bypasses.
;; Multiply instructions cannot dual-issue.

(define_insn_reservation "cortex_a7_mul" 2
  (and (eq_attr "tune" "cortexa7")
       (and (eq_attr "type" "mult")
            (eq_attr "neon_type" "none")))
  "cortex_a7_both")

;; The latency depends on the operands, so we use an estimate here.
(define_insn_reservation "cortex_a7_idiv" 5
  (and (eq_attr "tune" "cortexa7")
       (eq_attr "insn" "udiv,sdiv"))
  "cortex_a7_all*5")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Load/store instructions.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Address-generation happens in the issue stage. 
;; Double-word accesses can be issued in a single cycle,
;; and occupy only one pipeline stage.

(define_insn_reservation "cortex_a7_load1" 2
  (and (eq_attr "tune" "cortexa7")
       (and (eq_attr "type" "load_byte,load1")
            (eq_attr "neon_type" "none")))
  "cortex_a7_ex1")

(define_insn_reservation "cortex_a7_store1" 0
  (and (eq_attr "tune" "cortexa7")
       (and (eq_attr "type" "store1")
            (eq_attr "neon_type" "none")))
  "cortex_a7_ex1")

(define_insn_reservation "cortex_a7_load2" 2
  (and (eq_attr "tune" "cortexa7")
       (and (eq_attr "type" "load2")
            (eq_attr "neon_type" "none")))
  "cortex_a7_both")

(define_insn_reservation "cortex_a7_store2" 0
  (and (eq_attr "tune" "cortexa7")
       (and (eq_attr "type" "store2")
            (eq_attr "neon_type" "none")))
  "cortex_a7_both")

(define_insn_reservation "cortex_a7_load3" 3
  (and (eq_attr "tune" "cortexa7")
       (and (eq_attr "type" "load3")
            (eq_attr "neon_type" "none")))
  "cortex_a7_both, cortex_a7_ex1")

(define_insn_reservation "cortex_a7_store3" 0
  (and (eq_attr "tune" "cortexa7")
       (and (eq_attr "type" "store4")
            (eq_attr "neon_type" "none")))
  "cortex_a7_both, cortex_a7_ex1")

(define_insn_reservation "cortex_a7_load4" 3
  (and (eq_attr "tune" "cortexa7")
       (and (eq_attr "type" "load4")
            (eq_attr "neon_type" "none")))
  "cortex_a7_both, cortex_a7_both")

(define_insn_reservation "cortex_a7_store4" 0
  (and (eq_attr "tune" "cortexa7")
       (and (eq_attr "type" "store3")
            (eq_attr "neon_type" "none")))
  "cortex_a7_both, cortex_a7_both")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Floating-point arithmetic.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define_insn_reservation "cortex_a7_fpalu" 4
  (and (eq_attr "tune" "cortexa7")
       (and (eq_attr "type" "ffariths, fadds, ffarithd, faddd, fcpys,\
                             f_cvt, fcmps, fcmpd")
            (eq_attr "neon_type" "none")))
  "cortex_a7_ex1+cortex_a7_fpadd_pipe")

;; For fconsts and fconstd, 8-bit immediate data is passed directly from
;; f1 to f3 (which I think reduces the latency by one cycle).

(define_insn_reservation "cortex_a7_fconst" 3
  (and (eq_attr "tune" "cortexa7")
       (and (eq_attr "type" "fconsts,fconstd")
            (eq_attr "neon_type" "none")))
  "cortex_a7_ex1+cortex_a7_fpadd_pipe")

;; We should try not to attempt to issue a single-precision multiplication in
;; the middle of a double-precision multiplication operation (the usage of
;; cortex_a7_fpmul_pipe).

(define_insn_reservation "cortex_a7_fpmuls" 4
  (and (eq_attr "tune" "cortexa7")
       (and (eq_attr "type" "fmuls")
            (eq_attr "neon_type" "none")))
  "cortex_a7_ex1+cortex_a7_fpmul_pipe")

;; For single-precision multiply-accumulate, the add (accumulate) is issued
;; whilst the multiply is in F4.  The multiply result can then be forwarded
;; from F5 to F1.  The issue unit is only used once (when we first start
;; processing the instruction), but the usage of the FP add pipeline could
;; block other instructions attempting to use it simultaneously.  We try to
;; avoid that using cortex_a7_fpadd_pipe.

(define_insn_reservation "cortex_a7_fpmacs" 8
  (and (eq_attr "tune" "cortexa7")
       (and (eq_attr "type" "fmacs")
            (eq_attr "neon_type" "none")))
  "cortex_a7_ex1+cortex_a7_fpmul_pipe, nothing*3, cortex_a7_fpadd_pipe")

;; Non-multiply instructions can issue between two cycles of a
;; double-precision multiply. 

(define_insn_reservation "cortex_a7_fpmuld" 7
  (and (eq_attr "tune" "cortexa7")
       (and (eq_attr "type" "fmuld")
            (eq_attr "neon_type" "none")))
  "cortex_a7_ex1+cortex_a7_fpmul_pipe, cortex_a7_fpmul_pipe*2,\
   cortex_a7_ex1+cortex_a7_fpmul_pipe")

(define_insn_reservation "cortex_a7_fpmacd" 11
  (and (eq_attr "tune" "cortexa7")
       (and (eq_attr "type" "fmacd")
            (eq_attr "neon_type" "none")))
  "cortex_a7_ex1+cortex_a7_fpmul_pipe, cortex_a7_fpmul_pipe*2,\
   cortex_a7_ex1+cortex_a7_fpmul_pipe, nothing*3, cortex_a7_fpadd_pipe")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Floating-point divide/square root instructions.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define_insn_reservation "cortex_a7_fdivs" 16
  (and (eq_attr "tune" "cortexa7")
       (and (eq_attr "type" "fdivs")
            (eq_attr "neon_type" "none")))
  "cortex_a7_ex1, cortex_a7_fp_div_sqrt * 14")

(define_insn_reservation "cortex_a7_fdivd" 29
  (and (eq_attr "tune" "cortexa7")
       (and (eq_attr "type" "fdivd")
            (eq_attr "neon_type" "none")))
  "cortex_a7_ex1, cortex_a7_fp_div_sqrt * 28")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; VFP to/from core transfers.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Core-to-VFP transfers.

(define_insn_reservation "cortex_a7_r2f" 4
  (and (eq_attr "tune" "cortexa7")
       (and (eq_attr "type" "r_2_f")
            (eq_attr "neon_type" "none")))
  "cortex_a7_both")

(define_insn_reservation "cortex_a7_f2r" 2
  (and (eq_attr "tune" "cortexa7")
       (and (eq_attr "type" "f_2_r")
            (eq_attr "neon_type" "none")))
  "cortex_a7_ex1")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; VFP flag transfer.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Fuxne: The flag forwarding from fmstat to the second instruction is
;; not modeled at present.

(define_insn_reservation "cortex_a7_f_flags" 4
  (and (eq_attr "tune" "cortexa7")
       (and (eq_attr "type" "f_flag")
            (eq_attr "neon_type" "none")))
  "cortex_a7_ex1")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; VFP load/store.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define_insn_reservation "cortex_a7_f_loads" 4
  (and (eq_attr "tune" "cortexa7")
       (and (eq_attr "type" "f_loads")
            (eq_attr "neon_type" "none")))
  "cortex_a7_ex1")

(define_insn_reservation "cortex_a7_f_loadd" 4
  (and (eq_attr "tune" "cortexa7")
       (and (eq_attr "type" "f_loadd")
            (eq_attr "neon_type" "none")))
  "cortex_a7_both")

(define_insn_reservation "cortex_a7_f_stores" 0
  (and (eq_attr "tune" "cortexa7")
       (and (eq_attr "type" "f_stores")
            (eq_attr "neon_type" "none")))
  "cortex_a7_ex1")

(define_insn_reservation "cortex_a7_f_stored" 0
  (and (eq_attr "tune" "cortexa7")
       (and (eq_attr "type" "f_stored")
            (eq_attr "neon_type" "none")))
  "cortex_a7_both")

;; Load-to-use for floating-point values has a penalty of one cycle,
;; i.e. a latency of two.

(define_bypass 2 "cortex_a7_f_loads, cortex_a7_f_loadd"
                 "cortex_a7_fpalu, cortex_a7_fpmacs, cortex_a7_fpmuld,\
		  cortex_a7_fpmacd, cortex_a7_fdivs, cortex_a7_fdivd,\
		  cortex_a7_f2r")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; NEON load/store.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define_insn_reservation "cortex_a7_neon" 4
  (and (eq_attr "tune" "cortexa7")
       (eq_attr "neon_type" "!none"))
  "cortex_a7_both*2")
