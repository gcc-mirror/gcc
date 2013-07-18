;; ARM Cortex-A9 pipeline description
;; Copyright (C) 2008-2013 Free Software Foundation, Inc.
;; Originally written by CodeSourcery for VFP.
;;
;; Rewritten by Ramana Radhakrishnan <ramana.radhakrishnan@arm.com>
;; Integer Pipeline description contributed by ARM Ltd.
;; VFP Pipeline description rewritten and contributed by ARM Ltd.

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

(define_automaton "cortex_a9")

;; The Cortex-A9 core is modelled as a dual issue pipeline that has
;; the following components.
;; 1. 1 Load Store Pipeline.
;; 2. P0 / main pipeline for data processing instructions.
;; 3. P1 / Dual pipeline for Data processing instructions.
;; 4. MAC pipeline for multiply as well as multiply
;;    and accumulate instructions.
;; 5. 1 VFP and an optional Neon unit.
;; The Load/Store, VFP and Neon issue pipeline are multiplexed.
;; The P0 / main pipeline and M1 stage of the MAC pipeline are
;;   multiplexed.
;; The P1 / dual pipeline and M2 stage of the MAC pipeline are
;;   multiplexed.
;; There are only 4 integer register read ports and hence at any point of
;; time we can't have issue down the E1 and the E2 ports unless
;; of course there are bypass paths that get exercised.
;; Both P0 and P1 have 2 stages E1 and E2.
;; Data processing instructions issue to E1 or E2 depending on
;; whether they have an early shift or not.

(define_cpu_unit "ca9_issue_vfp_neon, cortex_a9_ls" "cortex_a9")
(define_cpu_unit "cortex_a9_p0_e1, cortex_a9_p0_e2" "cortex_a9")
(define_cpu_unit "cortex_a9_p1_e1, cortex_a9_p1_e2" "cortex_a9")
(define_cpu_unit "cortex_a9_p0_wb, cortex_a9_p1_wb" "cortex_a9")
(define_cpu_unit "cortex_a9_mac_m1, cortex_a9_mac_m2" "cortex_a9")
(define_cpu_unit "cortex_a9_branch, cortex_a9_issue_branch" "cortex_a9")

(define_reservation "cortex_a9_p0_default" "cortex_a9_p0_e2, cortex_a9_p0_wb")
(define_reservation "cortex_a9_p1_default" "cortex_a9_p1_e2, cortex_a9_p1_wb")
(define_reservation "cortex_a9_p0_shift" "cortex_a9_p0_e1, cortex_a9_p0_default")
(define_reservation "cortex_a9_p1_shift" "cortex_a9_p1_e1, cortex_a9_p1_default")

(define_reservation "cortex_a9_multcycle1"
  "cortex_a9_p0_e2 + cortex_a9_mac_m1 + cortex_a9_mac_m2 + \
cortex_a9_p1_e2 + cortex_a9_p0_e1 + cortex_a9_p1_e1")

(define_reservation "cortex_a9_mult16"
  "cortex_a9_mac_m1, cortex_a9_mac_m2, cortex_a9_p0_wb")
(define_reservation "cortex_a9_mac16"
  "cortex_a9_multcycle1, cortex_a9_mac_m2, cortex_a9_p0_wb")
(define_reservation "cortex_a9_mult"
  "cortex_a9_mac_m1*2, cortex_a9_mac_m2, cortex_a9_p0_wb")
(define_reservation "cortex_a9_mac"
  "cortex_a9_multcycle1*2 ,cortex_a9_mac_m2, cortex_a9_p0_wb")
(define_reservation "cortex_a9_mult_long"
  "cortex_a9_mac_m1*3, cortex_a9_mac_m2, cortex_a9_p0_wb")

;; Issue at the same time along the load store pipeline and
;; the VFP / Neon pipeline is not possible.
(exclusion_set "cortex_a9_ls" "ca9_issue_vfp_neon")

;; Default data processing instruction without any shift
;; The only exception to this is the mov instruction
;; which can go down E2 without any problem.
(define_insn_reservation "cortex_a9_dp" 2
  (and (eq_attr "tune" "cortexa9")
         (ior (and (eq_attr "type" "arlo_imm,arlo_reg,shift,shift_reg")
                        (eq_attr "neon_type" "none"))
	      (and (and (eq_attr "type" "arlo_shift_reg,extend,arlo_shift")
			(eq_attr "insn" "mov"))
                 (eq_attr "neon_type" "none"))))
  "cortex_a9_p0_default|cortex_a9_p1_default")

;; An instruction using the shifter will go down E1.
(define_insn_reservation "cortex_a9_dp_shift" 3
   (and (eq_attr "tune" "cortexa9")
	(and (eq_attr "type" "arlo_shift_reg,extend,arlo_shift")
	     (not (eq_attr "insn" "mov"))))
   "cortex_a9_p0_shift | cortex_a9_p1_shift")

;; Loads have a latency of 4 cycles.
;; We don't model autoincrement instructions. These
;; instructions use the load store pipeline and 1 of
;; the E2 units to write back the result of the increment.

(define_insn_reservation "cortex_a9_load1_2" 4
  (and (eq_attr "tune" "cortexa9")
       (eq_attr "type" "load1, load2, load_byte, f_loads, f_loadd"))
  "cortex_a9_ls")

;; Loads multiples and store multiples can't be issued for 2 cycles in a
;; row. The description below assumes that addresses are 64 bit aligned.
;; If not, there is an extra cycle latency which is not modelled.

(define_insn_reservation "cortex_a9_load3_4" 5
  (and (eq_attr "tune" "cortexa9")
       (eq_attr "type" "load3, load4"))
  "cortex_a9_ls, cortex_a9_ls")

(define_insn_reservation "cortex_a9_store1_2" 0
  (and (eq_attr "tune" "cortexa9")
       (eq_attr "type" "store1, store2, f_stores, f_stored"))
  "cortex_a9_ls")

;; Almost all our store multiples use an auto-increment
;; form. Don't issue back to back load and store multiples
;; because the load store unit will stall.

(define_insn_reservation "cortex_a9_store3_4" 0
  (and (eq_attr "tune" "cortexa9")
       (eq_attr "type" "store3, store4"))
  "cortex_a9_ls+(cortex_a9_p0_default | cortex_a9_p1_default), cortex_a9_ls")

;; We get 16*16 multiply / mac results in 3 cycles.
(define_insn_reservation "cortex_a9_mult16" 3
  (and (eq_attr "tune" "cortexa9")
       (eq_attr "type" "smulxy"))
       "cortex_a9_mult16")

;; The 16*16 mac is slightly different that it
;; reserves M1 and M2 in the same cycle.
(define_insn_reservation "cortex_a9_mac16" 3
  (and (eq_attr "tune" "cortexa9")
       (eq_attr "type" "smlaxy"))
  "cortex_a9_mac16")

(define_insn_reservation "cortex_a9_multiply" 4
  (and (eq_attr "tune" "cortexa9")
       (eq_attr "type" "mul,smmul,smmulr"))
       "cortex_a9_mult")

(define_insn_reservation "cortex_a9_mac" 4
  (and (eq_attr "tune" "cortexa9")
       (eq_attr "type" "mla,smmla"))
       "cortex_a9_mac")

(define_insn_reservation "cortex_a9_multiply_long" 5
  (and (eq_attr "tune" "cortexa9")
       (eq_attr "type" "smull,umull,smulls,umulls,smlal,smlals,umlal,umlals"))
       "cortex_a9_mult_long")

;; An instruction with a result in E2 can be forwarded
;; to E2 or E1 or M1 or the load store unit in the next cycle.

(define_bypass 1 "cortex_a9_dp"
                 "cortex_a9_dp_shift, cortex_a9_multiply,
 cortex_a9_load1_2, cortex_a9_dp, cortex_a9_store1_2,
 cortex_a9_mult16, cortex_a9_mac16, cortex_a9_mac, cortex_a9_store3_4, cortex_a9_load3_4, 
 cortex_a9_multiply_long")

(define_bypass 2 "cortex_a9_dp_shift"
                 "cortex_a9_dp_shift, cortex_a9_multiply,
 cortex_a9_load1_2, cortex_a9_dp, cortex_a9_store1_2,
 cortex_a9_mult16, cortex_a9_mac16, cortex_a9_mac, cortex_a9_store3_4, cortex_a9_load3_4,
 cortex_a9_multiply_long")

;; An instruction in the load store pipeline can provide
;; read access to a DP instruction in the P0 default pipeline
;; before the writeback stage.

(define_bypass 3 "cortex_a9_load1_2" "cortex_a9_dp, cortex_a9_load1_2,
cortex_a9_store3_4, cortex_a9_store1_2")

(define_bypass 4 "cortex_a9_load3_4" "cortex_a9_dp, cortex_a9_load1_2,
cortex_a9_store3_4, cortex_a9_store1_2,  cortex_a9_load3_4")

;; Calls and branches.

;; Branch instructions

(define_insn_reservation "cortex_a9_branch" 0
  (and (eq_attr "tune" "cortexa9")
       (eq_attr "type" "branch"))
  "cortex_a9_branch")

;; Call latencies are essentially 0 but make sure
;; dual issue doesn't happen i.e the next instruction
;; starts at the next cycle.
(define_insn_reservation "cortex_a9_call"  0
  (and (eq_attr "tune" "cortexa9")
       (eq_attr "type" "call"))
  "cortex_a9_issue_branch + cortex_a9_multcycle1 + cortex_a9_ls + ca9_issue_vfp_neon")


;; Pipelining for VFP instructions.
;; Issue happens either along load store unit or the VFP / Neon unit.
;; Pipeline   Instruction Classification.
;; FPS - fcpys, ffariths, ffarithd,r_2_f,f_2_r
;; FP_ADD   - fadds, faddd, fcmps (1)
;; FPMUL   - fmul{s,d}, fmac{s,d}, ffma{s,d}
;; FPDIV - fdiv{s,d}
(define_cpu_unit "ca9fps" "cortex_a9")
(define_cpu_unit "ca9fp_add1, ca9fp_add2, ca9fp_add3, ca9fp_add4" "cortex_a9")
(define_cpu_unit "ca9fp_mul1, ca9fp_mul2 , ca9fp_mul3, ca9fp_mul4" "cortex_a9")
(define_cpu_unit "ca9fp_ds1" "cortex_a9")


;; fmrs, fmrrd, fmstat and fmrx - The data is available after 1 cycle.
(define_insn_reservation "cortex_a9_fps" 2
 (and (eq_attr "tune" "cortexa9")
      (eq_attr "type" "fcpys, fconsts, fconstd, ffariths, ffarithd, r_2_f, f_2_r, f_flag"))
 "ca9_issue_vfp_neon + ca9fps")

(define_bypass 1
  "cortex_a9_fps"
  "cortex_a9_fadd, cortex_a9_fps, cortex_a9_fcmp, cortex_a9_dp, cortex_a9_dp_shift, cortex_a9_multiply, cortex_a9_multiply_long")

;; Scheduling on the FP_ADD pipeline.
(define_reservation "ca9fp_add" "ca9_issue_vfp_neon + ca9fp_add1, ca9fp_add2, ca9fp_add3, ca9fp_add4")

(define_insn_reservation "cortex_a9_fadd" 4
  (and (eq_attr "tune" "cortexa9")
       (eq_attr "type" "fadds, faddd, f_cvt"))
  "ca9fp_add")

(define_insn_reservation "cortex_a9_fcmp" 1
  (and (eq_attr "tune" "cortexa9")
      (eq_attr "type" "fcmps, fcmpd"))
 "ca9_issue_vfp_neon + ca9fp_add1")

;; Scheduling for the Multiply and MAC instructions.
(define_reservation "ca9fmuls"
  "ca9fp_mul1 + ca9_issue_vfp_neon, ca9fp_mul2, ca9fp_mul3, ca9fp_mul4")

(define_reservation "ca9fmuld"
  "ca9fp_mul1 + ca9_issue_vfp_neon, (ca9fp_mul1 + ca9fp_mul2), ca9fp_mul2, ca9fp_mul3, ca9fp_mul4")

(define_insn_reservation "cortex_a9_fmuls" 4
  (and (eq_attr "tune" "cortexa9")
       (eq_attr "type" "fmuls"))
  "ca9fmuls")

(define_insn_reservation "cortex_a9_fmuld" 5
  (and (eq_attr "tune" "cortexa9")
       (eq_attr "type" "fmuld"))
  "ca9fmuld")

(define_insn_reservation "cortex_a9_fmacs" 8
  (and (eq_attr "tune" "cortexa9")
       (eq_attr "type" "fmacs,ffmas"))
  "ca9fmuls, ca9fp_add")

(define_insn_reservation "cortex_a9_fmacd" 9
  (and (eq_attr "tune" "cortexa9")
       (eq_attr "type" "fmacd,ffmad"))
  "ca9fmuld, ca9fp_add")

;; Division pipeline description.
(define_insn_reservation "cortex_a9_fdivs" 15
  (and (eq_attr "tune" "cortexa9")
       (eq_attr "type" "fdivs"))
  "ca9fp_ds1 + ca9_issue_vfp_neon, nothing*14")

(define_insn_reservation "cortex_a9_fdivd" 25
  (and (eq_attr "tune" "cortexa9")
       (eq_attr "type" "fdivd"))
  "ca9fp_ds1 + ca9_issue_vfp_neon, nothing*24")

;; Include Neon pipeline description
(include "cortex-a9-neon.md")
