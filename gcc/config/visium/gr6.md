;; Scheduling description for GR6.
;; Copyright (C) 2013-2023 Free Software Foundation, Inc.
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

;; GR6 is a dual-issue, superscalar, out-of-order processor.
;;
;; The GR6 pipeline has 3 major components:
;;  1. The FETCH/DECODE/DISPATCH stages, an in-order front-end,
;;  2. The PROCESS stage, which is the out-of-order core,
;;  3. The STORE stage, an in-order register storage stage.
;;
;; The front-end and the back-end (PROCESS + STORE) are connected through a set
;; of reservation stations which, among other things, serve as buffers for the
;; decoded instructions.  The reservation stations are attached to a specific
;; execution unit of the PROCESS stage and the DISPATCH stage is responsible
;; for dispatching the decoded instructions to the appropriate stations.  Most
;; execution units have multiple reservation stations, thus making it possible
;; to dispatch two instructions per unit on a given cycle, but only one of them
;; can be executed on the next cycle.
;;
;; Since the core executes the instructions out of order, the most important
;; consideration for performance tuning is to make sure that enough decoded
;; instructions are ready for execution in the PROCESS stage while not stalling
;; the front-end, i.e while not trying to dispatch a decoded instruction to an
;; execution unit whose reservation stations are full.  Therefore, we do not
;; model the reservation stations (which is equivalent to pretending that there
;; is only one of them for each execution unit) but only the execution unit,
;; thus preserving some margin in case the unit itself stalls unexpectedly.

;; CPU execution units:
;;
;; inst[1|2]             The front-end: 2 instructions can be issued on a given
;;                       cycle by the FETCH/DECODE/DISPATCH stages, except for
;;                       the Block Move instructions.
;;
;; mov                   Move Execution Unit: immediate moves into registers.
;;
;; alu[1|2]              The 2 Arithmetic and Logic Units: other instructions
;;                       operating on the registers.
;;
;; bru                   Branch Resolution Unit: all branches.
;;
;; mem_wr                Memory Write Unit: all writes to memory.
;;
;; mem_rd                Memory Read Unit: all reads from memory.
;;
;; mem_eam               EAM interface: reads and writes from and to the EAM
;;                       and reads from the FP registers.
;;
;; eam                   Extended Arithmetic Module: multiply, divide and
;;                       64-bit shifts.
;;
;; fpcu                  Floating-Point Compare Unit: FP comparisons.
;;
;; fpu[1|2|3|4]          The 4 Floating-Point Units: all other instructions
;;                       operating on the FP registers.

(define_automaton "gr6,gr6_fpu")

(define_cpu_unit "gr6_inst1, gr6_inst2" "gr6")
(define_cpu_unit "gr6_mov" "gr6")
(define_cpu_unit "gr6_alu1,gr6_alu2" "gr6")
(define_cpu_unit "gr6_bru" "gr6")
(define_cpu_unit "gr6_mem_wr,gr6_mem_rd,gr6_mem_eam" "gr6")
(define_cpu_unit "gr6_eam" "gr6")
(define_cpu_unit "gr6_fpcu" "gr6")
(define_cpu_unit "gr6_fpu1,gr6_fpu2,gr6_fpu3,gr6_fpu4" "gr6_fpu")

(define_reservation "gr6_issue" "(gr6_inst1 | gr6_inst2)")
(define_reservation "gr6_single_issue" "gr6_inst1 + gr6_inst2")

(define_insn_reservation "gr6_immediate" 1
  (and (eq_attr "cpu" "gr6")
       (eq_attr "type" "imm_reg"))
  "gr6_issue + gr6_mov")

(define_insn_reservation "gr6_alu" 1
  (and (eq_attr "cpu" "gr6")
       (eq_attr "type" "arith,arith2,logic,cmp"))
  "gr6_issue + (gr6_alu1 | gr6_alu2)")

(define_insn_reservation "gr6_branch" 1
  (and (eq_attr "cpu" "gr6")
       (eq_attr "type" "abs_branch,branch,call,ret,rfi"))
  "gr6_issue + gr6_bru")

(define_insn_reservation "gr6_block_move" 16
  (and (eq_attr "cpu" "gr6")
       (eq_attr "type" "bmi"))
  "gr6_single_issue*16")

(define_insn_reservation "gr6_cpu_other" 1
  (and (eq_attr "cpu" "gr6")
       (eq_attr "type" "dsi,nop"))
  "gr6_issue")

(define_insn_reservation "gr6_write_mem" 1
  (and (eq_attr "cpu" "gr6")
       (eq_attr "type" "reg_mem"))
  "gr6_issue + gr6_mem_wr")

(define_insn_reservation "gr6_read_mem" 6
  (and (eq_attr "cpu" "gr6")
       (eq_attr "type" "mem_reg"))
  "gr6_issue + gr6_mem_rd, nothing*5")

;; EAM instructions.

(define_insn_reservation "gr6_write_eam" 2
  (and (eq_attr "cpu" "gr6")
       (eq_attr "type" "reg_eam"))
  "gr6_issue + gr6_mem_eam, nothing")

(define_reservation "gr6_issue_eam" "gr6_issue + gr6_mem_eam + gr6_eam")

(define_insn_reservation "gr6_read_eam" 2
  (and (eq_attr "cpu" "gr6")
       (eq_attr "type" "eam_reg"))
  "gr6_issue_eam, nothing")

(define_insn_reservation "gr6_shiftdi" 2
  (and (eq_attr "cpu" "gr6")
       (eq_attr "type" "shiftdi"))
  "gr6_issue_eam, gr6_eam")

(define_insn_reservation "gr6_mul" 3
  (and (eq_attr "cpu" "gr6")
       (eq_attr "type" "mul"))
  "gr6_issue_eam, gr6_eam*2")

(define_insn_reservation "gr6_div" 34
  (and (eq_attr "cpu" "gr6")
       (eq_attr "type" "div"))
  "gr6_issue_eam, gr6_eam*33")

(define_insn_reservation "gr6_divd" 66
  (and (eq_attr "cpu" "gr6")
       (eq_attr "type" "divd"))
  "gr6_issue_eam, gr6_eam*65")

;; FPU instructions.

(define_insn_reservation "gr6_read_fp" 2
  (and (eq_attr "cpu" "gr6")
       (eq_attr "type" "fp_reg"))
  "gr6_issue + gr6_mem_eam, nothing")

(define_insn_reservation "gr6_cmp_fp" 1
  (and (eq_attr "cpu" "gr6")
       (eq_attr "type" "fcmp"))
  "gr6_issue + gr6_fpcu")

(define_insn_reservation "gr6_fp_1cycle" 1
  (and (eq_attr "cpu" "gr6")
       (eq_attr "type" "fmove,ftoi,itof"))
  "gr6_issue + gr6_fpu1")

(define_insn_reservation "gr6_fp_3cycle" 3
  (and (eq_attr "cpu" "gr6")
       (eq_attr "type" "fp"))
  "gr6_issue + gr6_fpu2, nothing*2")

(define_insn_reservation "gr6_fp_17cycle" 17
  (and (eq_attr "cpu" "gr6")
       (eq_attr "type" "fdiv,fsqrt"))
  "gr6_issue + gr6_fpu3, gr6_fpu3*14, nothing*2")

(define_insn_reservation "gr6_write_fp" 1
  (and (eq_attr "cpu" "gr6")
       (eq_attr "type" "reg_fp"))
  "gr6_issue + gr6_fpu4")
