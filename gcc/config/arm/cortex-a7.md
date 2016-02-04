;; ARM Cortex-A7 pipeline description
;; Copyright (C) 2012-2016 Free Software Foundation, Inc.
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

(define_attr "cortex_a7_neon_type"
  "neon_mul, neon_mla, neon_other"
  (cond [
          (eq_attr "type" "neon_mul_b, neon_mul_b_q,\
	                   neon_mul_h, neon_mul_h_q,\
			   neon_mul_s, neon_mul_s_q,\
			   neon_mul_b_long, neon_mul_h_long,\
			   neon_mul_s_long, neon_mul_h_scalar,\
			   neon_mul_h_scalar_q, neon_mul_s_scalar,\
			   neon_mul_s_scalar_q, neon_mul_h_scalar_long,\
			   neon_mul_s_scalar_long,\
			   neon_sat_mul_b, neon_sat_mul_b_q,\
			   neon_sat_mul_h, neon_sat_mul_h_q,\
			   neon_sat_mul_s, neon_sat_mul_s_q,\
			   neon_sat_mul_b_long, neon_sat_mul_h_long,\
			   neon_sat_mul_s_long,\
			   neon_sat_mul_h_scalar, neon_sat_mul_h_scalar_q,\
			   neon_sat_mul_s_scalar, neon_sat_mul_s_scalar_q,\
			   neon_sat_mul_h_scalar_long,\
			   neon_sat_mul_s_scalar_long,\
			   neon_fp_mul_s, neon_fp_mul_s_q,\
			   neon_fp_mul_s_scalar, neon_fp_mul_s_scalar_q")
             (const_string "neon_mul")
          (eq_attr "type" "neon_mla_b, neon_mla_b_q, neon_mla_h,\
	                   neon_mla_h_q, neon_mla_s, neon_mla_s_q,\
			   neon_mla_b_long, neon_mla_h_long,\
                           neon_mla_s_long,\
			   neon_mla_h_scalar, neon_mla_h_scalar_q,\
			   neon_mla_s_scalar, neon_mla_s_scalar_q,\
			   neon_mla_h_scalar_long, neon_mla_s_scalar_long,\
			   neon_sat_mla_b_long, neon_sat_mla_h_long,\
			   neon_sat_mla_s_long,\
			   neon_sat_mla_h_scalar_long,\
                           neon_sat_mla_s_scalar_long,\
			   neon_fp_mla_s, neon_fp_mla_s_q,\
			   neon_fp_mla_s_scalar, neon_fp_mla_s_scalar_q")
             (const_string "neon_mla")]
           (const_string "neon_other")))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Branches.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; A direct branch can dual issue either as younger or older instruction,
;; but branches cannot dual issue with branches.
;; No latency as there is no result.

(define_insn_reservation "cortex_a7_branch" 0
  (and (eq_attr "tune" "cortexa7")
       (eq_attr "type" "branch"))
  "(cortex_a7_ex2|cortex_a7_ex1)+cortex_a7_branch")

;; Call cannot dual-issue as an older instruction. It can dual-issue
;; as a younger instruction, or single-issue.  Call cannot dual-issue
;; with another branch instruction.  The result is available the next
;; cycle.
(define_insn_reservation "cortex_a7_call" 1
  (and (eq_attr "tune" "cortexa7")
       (eq_attr "type" "call"))
  "(cortex_a7_ex2|cortex_a7_both)+cortex_a7_branch")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ALU instructions.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ALU instruction with an immediate operand can dual-issue.
(define_insn_reservation "cortex_a7_alu_imm" 2
  (and (eq_attr "tune" "cortexa7")
       (ior (eq_attr "type" "adr,alu_imm,alus_imm,logic_imm,logics_imm,\
                             mov_imm,mvn_imm,extend")
            (and (eq_attr "type" "mov_reg,mov_shift,mov_shift_reg")
                 (not (eq_attr "length" "8")))))
  "cortex_a7_ex2|cortex_a7_ex1")

;; ALU instruction with register operands can dual-issue
;; with a younger immediate-based instruction.
(define_insn_reservation "cortex_a7_alu_sreg" 2
  (and (eq_attr "tune" "cortexa7")
       (eq_attr "type" "alu_sreg,alus_sreg,logic_reg,logics_reg,\
                        adc_imm,adcs_imm,adc_reg,adcs_reg,\
                        bfm,clz,rbit,rev,alu_dsp_reg,\
                        shift_imm,shift_reg,mov_reg,mvn_reg"))
  "cortex_a7_ex1")

(define_insn_reservation "cortex_a7_alu_shift" 2
  (and (eq_attr "tune" "cortexa7")
       (eq_attr "type" "alu_shift_imm,alus_shift_imm,\
                        logic_shift_imm,logics_shift_imm,\
                        alu_shift_reg,alus_shift_reg,\
                        logic_shift_reg,logics_shift_reg,\
                        mov_shift,mov_shift_reg,\
                        mvn_shift,mvn_shift_reg,\
                        mrs,multiple,no_insn"))
  "cortex_a7_ex1")

;; Forwarding path for unshifted operands.
(define_bypass 1 "cortex_a7_alu_imm,cortex_a7_alu_sreg,cortex_a7_alu_shift"
  "cortex_a7_alu_imm,cortex_a7_alu_sreg,cortex_a7_mul")

(define_bypass 1 "cortex_a7_alu_imm,cortex_a7_alu_sreg,cortex_a7_alu_shift"
  "cortex_a7_store*"
  "arm_no_early_store_addr_dep")

(define_bypass 1 "cortex_a7_alu_imm,cortex_a7_alu_sreg,cortex_a7_alu_shift"
  "cortex_a7_alu_shift"
  "arm_no_early_alu_shift_dep")

;; The multiplier pipeline can forward results from wr stage only so
;; there's no need to specify bypasses.
;; Multiply instructions cannot dual-issue.

(define_insn_reservation "cortex_a7_mul" 2
  (and (eq_attr "tune" "cortexa7")
       (ior (eq_attr "mul32" "yes")
            (eq_attr "mul64" "yes")))
  "cortex_a7_both")

;; Forward the result of a multiply operation to the accumulator 
;; of the following multiply and accumulate instruction.
(define_bypass 1 "cortex_a7_mul"
                 "cortex_a7_mul"
                 "arm_mac_accumulator_is_result")

;; The latency depends on the operands, so we use an estimate here.
(define_insn_reservation "cortex_a7_idiv" 5
  (and (eq_attr "tune" "cortexa7")
       (eq_attr "type" "udiv,sdiv"))
  "cortex_a7_both*5")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Load/store instructions.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Address-generation happens in the issue stage. 
;; Double-word accesses can be issued in a single cycle,
;; and occupy only one pipeline stage.

(define_insn_reservation "cortex_a7_load1" 2
  (and (eq_attr "tune" "cortexa7")
       (eq_attr "type" "load_byte,load1"))
  "cortex_a7_ex1")

(define_insn_reservation "cortex_a7_store1" 0
  (and (eq_attr "tune" "cortexa7")
       (eq_attr "type" "store1"))
  "cortex_a7_ex1")

(define_insn_reservation "cortex_a7_load2" 2
  (and (eq_attr "tune" "cortexa7")
       (eq_attr "type" "load2"))
  "cortex_a7_both")

(define_insn_reservation "cortex_a7_store2" 0
  (and (eq_attr "tune" "cortexa7")
       (eq_attr "type" "store2"))
  "cortex_a7_both")

(define_insn_reservation "cortex_a7_load3" 3
  (and (eq_attr "tune" "cortexa7")
       (eq_attr "type" "load3"))
  "cortex_a7_both, cortex_a7_ex1")

(define_insn_reservation "cortex_a7_store3" 0
  (and (eq_attr "tune" "cortexa7")
       (eq_attr "type" "store4"))
  "cortex_a7_both, cortex_a7_ex1")

(define_insn_reservation "cortex_a7_load4" 3
  (and (eq_attr "tune" "cortexa7")
       (eq_attr "type" "load4"))
  "cortex_a7_both, cortex_a7_both")

(define_insn_reservation "cortex_a7_store4" 0
  (and (eq_attr "tune" "cortexa7")
       (eq_attr "type" "store3"))
  "cortex_a7_both, cortex_a7_both")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Floating-point arithmetic.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Neon integer, neon floating point, and single-precision floating
;; point instructions of the same type have the same timing
;; characteristics, but neon instructions cannot dual-issue.

(define_insn_reservation "cortex_a7_fpalu" 4
  (and (eq_attr "tune" "cortexa7")
       (eq_attr "type" "ffariths, fadds, ffarithd, faddd, fmov,\
                        f_cvt, f_cvtf2i, f_cvti2f, fcmps, fcmpd"))
  "cortex_a7_ex1+cortex_a7_fpadd_pipe")

;; For fconsts and fconstd, 8-bit immediate data is passed directly from
;; f1 to f3 (which I think reduces the latency by one cycle).

(define_insn_reservation "cortex_a7_fconst" 3
  (and (eq_attr "tune" "cortexa7")
       (eq_attr "type" "fconsts,fconstd"))
  "cortex_a7_ex1+cortex_a7_fpadd_pipe")

;; We should try not to attempt to issue a single-precision multiplication in
;; the middle of a double-precision multiplication operation (the usage of
;; cortex_a7_fpmul_pipe).

(define_insn_reservation "cortex_a7_fpmuls" 4
  (and (eq_attr "tune" "cortexa7")
       (eq_attr "type" "fmuls"))
  "cortex_a7_ex1+cortex_a7_fpmul_pipe")

(define_insn_reservation "cortex_a7_neon_mul" 4
  (and (eq_attr "tune" "cortexa7")
       (eq_attr "cortex_a7_neon_type" "neon_mul"))
  "(cortex_a7_both+cortex_a7_fpmul_pipe)*2")

(define_insn_reservation "cortex_a7_fpmacs" 8
  (and (eq_attr "tune" "cortexa7")
       (eq_attr "type" "fmacs,ffmas"))
  "cortex_a7_ex1+cortex_a7_fpmul_pipe")

(define_insn_reservation "cortex_a7_neon_mla" 8
  (and (eq_attr "tune" "cortexa7")
       (eq_attr "cortex_a7_neon_type" "neon_mla"))
  "cortex_a7_both+cortex_a7_fpmul_pipe")

(define_bypass 4 "cortex_a7_fpmacs,cortex_a7_neon_mla"
                 "cortex_a7_fpmacs,cortex_a7_neon_mla"
                 "arm_mac_accumulator_is_result")

;; Non-multiply instructions can issue between two cycles of a
;; double-precision multiply. 

(define_insn_reservation "cortex_a7_fpmuld" 7
  (and (eq_attr "tune" "cortexa7")
       (eq_attr "type" "fmuld"))
  "cortex_a7_ex1+cortex_a7_fpmul_pipe, cortex_a7_fpmul_pipe*3")

(define_insn_reservation "cortex_a7_fpmacd" 11
  (and (eq_attr "tune" "cortexa7")
       (eq_attr "type" "fmacd"))
  "cortex_a7_ex1+cortex_a7_fpmul_pipe, cortex_a7_fpmul_pipe*3")

(define_insn_reservation "cortex_a7_fpfmad" 8
  (and (eq_attr "tune" "cortexa7")
       (eq_attr "type" "ffmad"))
  "cortex_a7_ex1+cortex_a7_fpmul_pipe, cortex_a7_fpmul_pipe*4")

(define_bypass 7 "cortex_a7_fpmacd"
                 "cortex_a7_fpmacd,cortex_a7_fpfmad"
                 "arm_mac_accumulator_is_result")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Floating-point divide/square root instructions.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define_insn_reservation "cortex_a7_fdivs" 16
  (and (eq_attr "tune" "cortexa7")
       (eq_attr "type" "fdivs, fsqrts"))
  "cortex_a7_ex1+cortex_a7_fp_div_sqrt, cortex_a7_fp_div_sqrt * 13")

(define_insn_reservation "cortex_a7_fdivd" 31
  (and (eq_attr "tune" "cortexa7")
       (eq_attr "type" "fdivd, fsqrtd"))
  "cortex_a7_ex1+cortex_a7_fp_div_sqrt, cortex_a7_fp_div_sqrt * 28")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; VFP to/from core transfers.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Core-to-VFP transfers.

(define_insn_reservation "cortex_a7_r2f" 4
  (and (eq_attr "tune" "cortexa7")
       (eq_attr "type" "f_mcr,f_mcrr"))
  "cortex_a7_both")

(define_insn_reservation "cortex_a7_f2r" 2
  (and (eq_attr "tune" "cortexa7")
       (eq_attr "type" "f_mrc,f_mrrc"))
  "cortex_a7_ex1")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; VFP flag transfer.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Fuxne: The flag forwarding from fmstat to the second instruction is
;; not modeled at present.

(define_insn_reservation "cortex_a7_f_flags" 4
  (and (eq_attr "tune" "cortexa7")
       (eq_attr "type" "f_flag"))
  "cortex_a7_ex1")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; VFP load/store.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define_insn_reservation "cortex_a7_f_loads" 4
  (and (eq_attr "tune" "cortexa7")
       (eq_attr "type" "f_loads"))
  "cortex_a7_ex1")

(define_insn_reservation "cortex_a7_f_loadd" 4
  (and (eq_attr "tune" "cortexa7")
       (eq_attr "type" "f_loadd"))
  "cortex_a7_both")

(define_insn_reservation "cortex_a7_f_stores" 0
  (and (eq_attr "tune" "cortexa7")
       (eq_attr "type" "f_stores"))
  "cortex_a7_ex1")

(define_insn_reservation "cortex_a7_f_stored" 0
  (and (eq_attr "tune" "cortexa7")
       (eq_attr "type" "f_stored"))
  "cortex_a7_both")

;; Load-to-use for floating-point values has a penalty of one cycle,
;; i.e. a latency of two.

(define_bypass 2 "cortex_a7_f_loads, cortex_a7_f_loadd"
                  "cortex_a7_fpalu,\
                   cortex_a7_fpmuls,cortex_a7_fpmacs,\
                   cortex_a7_fpmuld,cortex_a7_fpmacd, cortex_a7_fpfmad,\
                   cortex_a7_fdivs, cortex_a7_fdivd,\
		   cortex_a7_f2r")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; NEON
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Simple modeling for all neon instructions not covered earlier.

(define_insn_reservation "cortex_a7_neon" 4
  (and (eq_attr "tune" "cortexa7")
       (and (eq_attr "is_neon_type" "yes")
            (eq_attr "cortex_a7_neon_type" "neon_other")))
  "cortex_a7_both*2")
