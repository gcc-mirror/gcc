;; Scheduling description for GR5.
;; Copyright (C) 2013-2016 Free Software Foundation, Inc.
;;
;; This file is part of GCC.
;;
;; GCC is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; GCC is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GCC; see the file COPYING3.  If not see
;; <http://www.gnu.org/licenses/>.

;; GR5 is a single-issue processor.

;; CPU execution units:
;;
;; issue                  Only one instruction can be issued on a given cycle.
;;                        There is no need to model the CPU pipeline in any
;;                        more detail than this.
;;
;; mem                    Memory Unit: all accesses to memory.
;;
;; eam                    Extended Arithmetic Module: multiply, divide and
;;                        64-bit shifts.
;;
;; fp_slot[0|1|2|3]       The 4 FIFO slots of the floating-point unit.  Only
;;                        the instruction at slot 0 can execute, but an FP
;;                        instruction can issue if any of the slots is free.

(define_automaton "gr5,gr5_fpu")

(define_cpu_unit "gr5_issue" "gr5")
(define_cpu_unit "gr5_mem" "gr5")
(define_cpu_unit "gr5_eam" "gr5")
(define_cpu_unit "gr5_fp_slot0,gr5_fp_slot1,gr5_fp_slot2,gr5_fp_slot3" "gr5_fpu")

;; The CPU instructions which write to general registers and so do not totally
;; complete until they reach the store stage of the pipeline.  This is not the
;; complete storage register class: mem_reg, eam_reg and fpu_reg are excluded
;; since we must keep the reservation sets non-overlapping.
(define_insn_reservation "gr5_storage_register" 1
  (and (eq_attr "cpu" "gr5")
       (eq_attr "type" "imm_reg,arith,arith2,logic,call"))
  "gr5_issue")

(define_insn_reservation "gr5_read_mem" 1
  (and (eq_attr "cpu" "gr5")
       (eq_attr "type" "mem_reg"))
  "gr5_issue + gr5_mem")

;; The latency of 2 and the reservation of gr5_mem on the second cycle ensures
;; that no reads will be scheduled on the second cycle, which would otherwise
;; stall the pipeline for 1 cycle.
(define_insn_reservation "gr5_write_mem" 2
  (and (eq_attr "cpu" "gr5")
       (eq_attr "type" "reg_mem"))
  "gr5_issue, gr5_mem")

;; Try to avoid the pipeline hazard of addressing off a register that has
;; not yet been stored.
(define_bypass 2 "gr5_storage_register" "gr5_read_mem"   "gr5_hazard_bypass_p")
(define_bypass 2 "gr5_storage_register" "gr5_write_mem"  "gr5_hazard_bypass_p")
(define_bypass 2 "gr5_read_mem"         "gr5_read_mem"   "gr5_hazard_bypass_p")
(define_bypass 2 "gr5_read_mem"         "gr5_write_mem"  "gr5_hazard_bypass_p")

;; Other CPU instructions complete by the process stage.
(define_insn_reservation "gr5_cpu_other" 1
  (and (eq_attr "cpu" "gr5")
       (eq_attr "type" "abs_branch,branch,cmp,ret,rfi,dsi,nop"))
  "gr5_issue")

;; EAM instructions.

(define_insn_reservation "gr5_write_eam" 1
  (and (eq_attr "cpu" "gr5")
       (eq_attr "type" "reg_eam"))
  "gr5_issue")

(define_reservation "gr5_issue_eam" "(gr5_issue + gr5_eam)")

(define_insn_reservation "gr5_read_eam" 1
  (and (eq_attr "cpu" "gr5")
       (eq_attr "type" "eam_reg"))
  "gr5_issue_eam")

;; Try to avoid the pipeline hazard of addressing off a register that has
;; not yet been stored.
(define_bypass 2 "gr5_read_eam" "gr5_read_mem"  "gr5_hazard_bypass_p")
(define_bypass 2 "gr5_read_eam" "gr5_write_mem" "gr5_hazard_bypass_p")

(define_insn_reservation "gr5_shiftdi" 1
  (and (eq_attr "cpu" "gr5")
       (eq_attr "type" "shiftdi"))
  "gr5_issue_eam")

(define_insn_reservation "gr5_mul" 3
  (and (eq_attr "cpu" "gr5")
       (eq_attr "type" "mul"))
  "gr5_issue_eam, gr5_eam*2")

(define_insn_reservation "gr5_div" 34
  (and (eq_attr "cpu" "gr5")
       (eq_attr "type" "div"))
  "gr5_issue_eam, gr5_eam*33")

(define_insn_reservation "gr5_divd" 66
  (and (eq_attr "cpu" "gr5")
       (eq_attr "type" "divd"))
  "gr5_issue_eam, gr5_eam*65")

;; FPU instructions.

(define_reservation "gr5_fp_slotany" "(gr5_fp_slot0 | gr5_fp_slot1 | gr5_fp_slot2 | gr5_fp_slot3)")

(define_insn_reservation "gr5_fp_other" 1
  (and (eq_attr "cpu" "gr5")
       (eq_attr "type" "fp_reg,reg_fp,fcmp"))
  "gr5_issue")

(define_insn_reservation "gr5_fp_1cycle" 2
  (and (eq_attr "cpu" "gr5")
       (eq_attr "type" "fmove,ftoi"))
  "gr5_issue + gr5_fp_slotany, gr5_fp_slot0")

(define_insn_reservation "gr5_fp_2cycle" 3
  (and (eq_attr "cpu" "gr5")
       (eq_attr "type" "itof"))
  "gr5_issue + gr5_fp_slotany, gr5_fp_slot0*2")

(define_insn_reservation "gr5_fp_3cycle" 4
  (and (eq_attr "cpu" "gr5")
       (eq_attr "type" "fp"))
  "gr5_issue + gr5_fp_slotany, gr5_fp_slot0*3")

(define_insn_reservation "gr5_fp_30cycle" 31
  (and (eq_attr "cpu" "gr5")
       (eq_attr "type" "fdiv,fsqrt"))
  "gr5_issue + gr5_fp_slotany, gr5_fp_slot0*30")
