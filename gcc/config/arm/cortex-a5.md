;; ARM Cortex-A5 pipeline description
;; Copyright (C) 2010-2013 Free Software Foundation, Inc.
;; Contributed by CodeSourcery.
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

(define_automaton "cortex_a5")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Functional units.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The integer (ALU) pipeline.  There are five DPU pipeline
;; stages. However the decode/issue stages operate the same for all
;; instructions, so do not model them.  We only need to model the
;; first execute stage because instructions always advance one stage
;; per cycle in order.  Only branch instructions may dual-issue, so a
;; single unit covers all of the LS, ALU, MAC and FPU pipelines.

(define_cpu_unit "cortex_a5_ex1" "cortex_a5")

;; The branch pipeline.  Branches can dual-issue with other instructions
;; (except when those instructions take multiple cycles to issue).

(define_cpu_unit "cortex_a5_branch" "cortex_a5")

;; Pseudo-unit for blocking the multiply pipeline when a double-precision
;; multiply is in progress.

(define_cpu_unit "cortex_a5_fpmul_pipe" "cortex_a5")

;; The floating-point add pipeline (ex1/f1 stage), used to model the usage
;; of the add pipeline by fmac instructions, etc.

(define_cpu_unit "cortex_a5_fpadd_pipe" "cortex_a5")

;; Floating-point div/sqrt (long latency, out-of-order completion).

(define_cpu_unit "cortex_a5_fp_div_sqrt" "cortex_a5")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ALU instructions.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define_insn_reservation "cortex_a5_alu" 2
  (and (eq_attr "tune" "cortexa5")
       (eq_attr "type" "alu_imm,alus_imm,logic_imm,logics_imm,\
                        alu_reg,alus_reg,logic_reg,logics_reg,\
                        adc_imm,adcs_imm,adc_reg,adcs_reg,\
                        adr,bfm,rev,\
                        shift_imm,shift_reg,\
                        mov_imm,mov_reg,mvn_imm,mvn_reg,\
                        multiple,no_insn"))
  "cortex_a5_ex1")

(define_insn_reservation "cortex_a5_alu_shift" 2
  (and (eq_attr "tune" "cortexa5")
       (eq_attr "type" "extend,\
                        alu_shift_imm,alus_shift_imm,\
                        logic_shift_imm,logics_shift_imm,\
                        alu_shift_reg,alus_shift_reg,\
                        logic_shift_reg,logics_shift_reg,\
                        mov_shift,mov_shift_reg,\
                        mvn_shift,mvn_shift_reg"))
  "cortex_a5_ex1")

;; Forwarding path for unshifted operands.

(define_bypass 1 "cortex_a5_alu,cortex_a5_alu_shift"
  "cortex_a5_alu")

(define_bypass 1 "cortex_a5_alu,cortex_a5_alu_shift"
  "cortex_a5_alu_shift"
  "arm_no_early_alu_shift_dep")

;; The multiplier pipeline can forward results from wr stage only so 
;; there's no need to specify bypasses).

(define_insn_reservation "cortex_a5_mul" 2
  (and (eq_attr "tune" "cortexa5")
       (ior (eq_attr "mul32" "yes")
	    (eq_attr "mul64" "yes")))
  "cortex_a5_ex1")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Load/store instructions.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Address-generation happens in the issue stage, which is one stage behind
;; the ex1 stage (the first stage we care about for scheduling purposes). The
;; dc1 stage is parallel with ex1, dc2 with ex2 and rot with wr.

(define_insn_reservation "cortex_a5_load1" 2
  (and (eq_attr "tune" "cortexa5")
       (eq_attr "type" "load_byte,load1"))
  "cortex_a5_ex1")

(define_insn_reservation "cortex_a5_store1" 0
  (and (eq_attr "tune" "cortexa5")
       (eq_attr "type" "store1"))
  "cortex_a5_ex1")

(define_insn_reservation "cortex_a5_load2" 3
  (and (eq_attr "tune" "cortexa5")
       (eq_attr "type" "load2"))
  "cortex_a5_ex1+cortex_a5_branch, cortex_a5_ex1")

(define_insn_reservation "cortex_a5_store2" 0
  (and (eq_attr "tune" "cortexa5")
       (eq_attr "type" "store2"))
  "cortex_a5_ex1+cortex_a5_branch, cortex_a5_ex1")

(define_insn_reservation "cortex_a5_load3" 4
  (and (eq_attr "tune" "cortexa5")
       (eq_attr "type" "load3"))
  "cortex_a5_ex1+cortex_a5_branch, cortex_a5_ex1+cortex_a5_branch,\
   cortex_a5_ex1")

(define_insn_reservation "cortex_a5_store3" 0
  (and (eq_attr "tune" "cortexa5")
       (eq_attr "type" "store3"))
  "cortex_a5_ex1+cortex_a5_branch, cortex_a5_ex1+cortex_a5_branch,\
   cortex_a5_ex1")

(define_insn_reservation "cortex_a5_load4" 5
  (and (eq_attr "tune" "cortexa5")
       (eq_attr "type" "load3"))
  "cortex_a5_ex1+cortex_a5_branch, cortex_a5_ex1+cortex_a5_branch,\
   cortex_a5_ex1+cortex_a5_branch, cortex_a5_ex1")

(define_insn_reservation "cortex_a5_store4" 0
  (and (eq_attr "tune" "cortexa5")
       (eq_attr "type" "store3"))
  "cortex_a5_ex1+cortex_a5_branch, cortex_a5_ex1+cortex_a5_branch,\
   cortex_a5_ex1+cortex_a5_branch, cortex_a5_ex1")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Branches.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Direct branches are the only instructions we can dual-issue (also IT and
;; nop, but those aren't very interesting for scheduling).  (The latency here
;; is meant to represent when the branch actually takes place, but may not be
;; entirely correct.)

(define_insn_reservation "cortex_a5_branch" 3
  (and (eq_attr "tune" "cortexa5")
       (eq_attr "type" "branch,call"))
  "cortex_a5_branch")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Floating-point arithmetic.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define_insn_reservation "cortex_a5_fpalu" 4
  (and (eq_attr "tune" "cortexa5")
       (eq_attr "type" "ffariths, fadds, ffarithd, faddd, fcpys, fmuls,\
                        f_cvt,f_cvtf2i,f_cvti2f,\
			fcmps, fcmpd"))
  "cortex_a5_ex1+cortex_a5_fpadd_pipe")

;; For fconsts and fconstd, 8-bit immediate data is passed directly from
;; f1 to f3 (which I think reduces the latency by one cycle).

(define_insn_reservation "cortex_a5_fconst" 3
  (and (eq_attr "tune" "cortexa5")
       (eq_attr "type" "fconsts,fconstd"))
  "cortex_a5_ex1+cortex_a5_fpadd_pipe")

;; We should try not to attempt to issue a single-precision multiplication in
;; the middle of a double-precision multiplication operation (the usage of
;; cortex_a5_fpmul_pipe).

(define_insn_reservation "cortex_a5_fpmuls" 4
  (and (eq_attr "tune" "cortexa5")
       (eq_attr "type" "fmuls"))
  "cortex_a5_ex1+cortex_a5_fpmul_pipe")

;; For single-precision multiply-accumulate, the add (accumulate) is issued
;; whilst the multiply is in F4.  The multiply result can then be forwarded
;; from F5 to F1.  The issue unit is only used once (when we first start
;; processing the instruction), but the usage of the FP add pipeline could
;; block other instructions attempting to use it simultaneously.  We try to
;; avoid that using cortex_a5_fpadd_pipe.

(define_insn_reservation "cortex_a5_fpmacs" 8
  (and (eq_attr "tune" "cortexa5")
       (eq_attr "type" "fmacs,ffmas"))
  "cortex_a5_ex1+cortex_a5_fpmul_pipe, nothing*3, cortex_a5_fpadd_pipe")

;; Non-multiply instructions can issue in the middle two instructions of a
;; double-precision multiply.  Note that it isn't entirely clear when a branch
;; can dual-issue when a multi-cycle multiplication is in progress; we ignore
;; that for now though.

(define_insn_reservation "cortex_a5_fpmuld" 7
  (and (eq_attr "tune" "cortexa5")
       (eq_attr "type" "fmuld"))
  "cortex_a5_ex1+cortex_a5_fpmul_pipe, cortex_a5_fpmul_pipe*2,\
   cortex_a5_ex1+cortex_a5_fpmul_pipe")

(define_insn_reservation "cortex_a5_fpmacd" 11
  (and (eq_attr "tune" "cortexa5")
       (eq_attr "type" "fmacd,ffmad"))
  "cortex_a5_ex1+cortex_a5_fpmul_pipe, cortex_a5_fpmul_pipe*2,\
   cortex_a5_ex1+cortex_a5_fpmul_pipe, nothing*3, cortex_a5_fpadd_pipe")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Floating-point divide/square root instructions.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ??? Not sure if the 14 cycles taken for single-precision divide to complete
;; includes the time taken for the special instruction used to collect the
;; result to travel down the multiply pipeline, or not.  Assuming so.  (If
;; that's wrong, the latency should be increased by a few cycles.)

;; fsqrt takes one cycle less, but that is not modelled, nor is the use of the
;; multiply pipeline to collect the divide/square-root result.

(define_insn_reservation "cortex_a5_fdivs" 14
  (and (eq_attr "tune" "cortexa5")
       (eq_attr "type" "fdivs, fsqrts"))
  "cortex_a5_ex1, cortex_a5_fp_div_sqrt * 13")

;; ??? Similarly for fdivd.

(define_insn_reservation "cortex_a5_fdivd" 29
  (and (eq_attr "tune" "cortexa5")
       (eq_attr "type" "fdivd, fsqrtd"))
  "cortex_a5_ex1, cortex_a5_fp_div_sqrt * 28")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; VFP to/from core transfers.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; FP loads take data from wr/rot/f3.

;; Core-to-VFP transfers use the multiply pipeline.

(define_insn_reservation "cortex_a5_r2f" 4
  (and (eq_attr "tune" "cortexa5")
       (eq_attr "type" "f_mcr,f_mcrr"))
  "cortex_a5_ex1")

(define_insn_reservation "cortex_a5_f2r" 2
  (and (eq_attr "tune" "cortexa5")
       (eq_attr "type" "f_mrc,f_mrrc"))
  "cortex_a5_ex1")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; VFP flag transfer.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ??? The flag forwarding from fmstat to the ex2 stage of the second
;; instruction is not modeled at present.

(define_insn_reservation "cortex_a5_f_flags" 4
  (and (eq_attr "tune" "cortexa5")
       (eq_attr "type" "f_flag"))
  "cortex_a5_ex1")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; VFP load/store.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define_insn_reservation "cortex_a5_f_loads" 4
  (and (eq_attr "tune" "cortexa5")
       (eq_attr "type" "f_loads"))
  "cortex_a5_ex1")

(define_insn_reservation "cortex_a5_f_loadd" 5
  (and (eq_attr "tune" "cortexa5")
       (eq_attr "type" "f_loadd"))
  "cortex_a5_ex1+cortex_a5_branch, cortex_a5_ex1")

(define_insn_reservation "cortex_a5_f_stores" 0
  (and (eq_attr "tune" "cortexa5")
       (eq_attr "type" "f_stores"))
  "cortex_a5_ex1")

(define_insn_reservation "cortex_a5_f_stored" 0
  (and (eq_attr "tune" "cortexa5")
       (eq_attr "type" "f_stored"))
  "cortex_a5_ex1+cortex_a5_branch, cortex_a5_ex1")

;; Load-to-use for floating-point values has a penalty of one cycle,
;; i.e. a latency of two.

(define_bypass 2 "cortex_a5_f_loads"
                 "cortex_a5_fpalu, cortex_a5_fpmacs, cortex_a5_fpmuld,\
		  cortex_a5_fpmacd, cortex_a5_fdivs, cortex_a5_fdivd,\
		  cortex_a5_f2r")

(define_bypass 3 "cortex_a5_f_loadd"
                 "cortex_a5_fpalu, cortex_a5_fpmacs, cortex_a5_fpmuld,\
		  cortex_a5_fpmacd, cortex_a5_fdivs, cortex_a5_fdivd,\
		  cortex_a5_f2r")
