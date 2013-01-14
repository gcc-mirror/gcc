;; DFA-based pipeline description for the XLP.
;; Copyright (C) 2012-2013 Free Software Foundation, Inc.
;;
;; xlp.md   Machine Description for the Broadcom XLP Microprocessor
;; This file is part of GCC.

;; GCC is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published
;; by the Free Software Foundation; either version 3, or (at your
;; option) any later version.

;; GCC is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
;; License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GCC; see the file COPYING3.  If not see
;; <http://www.gnu.org/licenses/>.

(define_automaton "xlp_cpu")

;; CPU function units.
(define_cpu_unit "xlp_ex0" "xlp_cpu")
(define_cpu_unit "xlp_ex1" "xlp_cpu")
(define_cpu_unit "xlp_ex2" "xlp_cpu")
(define_cpu_unit "xlp_ex3" "xlp_cpu")

;; Integer Multiply Unit
(define_cpu_unit "xlp_div" "xlp_cpu")

;; ALU2 completion port.
(define_cpu_unit "xlp_ex2_wrb" "xlp_cpu")

(define_automaton "xlp_fpu")

;; Floating-point units.
(define_cpu_unit "xlp_fp" "xlp_fpu")

;; Floating Point Sqrt/Divide
(define_cpu_unit "xlp_divsq" "xlp_fpu")

;; FPU completion port.
(define_cpu_unit "xlp_fp_wrb" "xlp_fpu")

;; Define reservations for common combinations.

;;
;; The ordering of the instruction-execution-path/resource-usage
;; descriptions (also known as reservation RTL) is roughly ordered
;; based on the define attribute RTL for the "type" classification.
;; When modifying, remember that the first test that matches is the
;; reservation used!
;;
(define_insn_reservation "ir_xlp_unknown" 1
  (and (eq_attr "cpu" "xlp")
       (eq_attr "type" "unknown,multi"))
  "xlp_ex0+xlp_ex1+xlp_ex2+xlp_ex3")

(define_insn_reservation "ir_xlp_branch" 1
  (and (eq_attr "cpu" "xlp")
       (eq_attr "type" "branch,jump,call"))
  "xlp_ex3")

(define_insn_reservation "ir_xlp_prefetch" 1
  (and (eq_attr "cpu" "xlp")
       (eq_attr "type" "prefetch,prefetchx"))
  "xlp_ex0|xlp_ex1")

(define_insn_reservation "ir_xlp_load" 4
  (and (eq_attr "cpu" "xlp")
       (eq_attr "type" "load"))
  "xlp_ex0|xlp_ex1")

(define_insn_reservation "ir_xlp_fpload" 5
  (and (eq_attr "cpu" "xlp")
       (eq_attr "type" "fpload,fpidxload"))
  "xlp_ex0|xlp_ex1")

(define_insn_reservation "ir_xlp_alu" 1
  (and (eq_attr "cpu" "xlp")
       (eq_attr "type" "const,arith,shift,slt,clz,signext,logical,move,trap,nop"))
  "xlp_ex0|xlp_ex1|(xlp_ex2,xlp_ex2_wrb)|xlp_ex3")

(define_insn_reservation "ir_xlp_condmov" 1
  (and (eq_attr "cpu" "xlp")
       (eq_attr "type" "condmove")
       (eq_attr "mode" "SI,DI"))
  "xlp_ex2,xlp_ex2_wrb")

(define_insn_reservation "ir_xlp_mul" 5
  (and (eq_attr "cpu" "xlp")
       (eq_attr "type" "imul,imadd"))
  "xlp_ex2,nothing*4,xlp_ex2_wrb")

(define_insn_reservation "ir_xlp_mul3" 3
  (and (eq_attr "cpu" "xlp")
       (eq_attr "type" "imul3"))
  "xlp_ex2,nothing*2,xlp_ex2_wrb")

(define_insn_reservation "ir_xlp_div" 24
  (and (eq_attr "cpu" "xlp")
       (eq_attr "mode" "SI")
       (eq_attr "type" "idiv"))
  "xlp_ex2+xlp_div,xlp_div*23,xlp_ex2_wrb")

(define_insn_reservation "ir_xlp_ddiv" 48
  (and (eq_attr "cpu" "xlp")
       (eq_attr "mode" "DI")
       (eq_attr "type" "idiv"))
  "xlp_ex2+xlp_div,xlp_div*47,xlp_ex2_wrb")

(define_insn_reservation "ir_xlp_store" 1
  (and (eq_attr "cpu" "xlp")
       (eq_attr "type" "store,fpstore,fpidxstore"))
  "xlp_ex0|xlp_ex1")

(define_insn_reservation "ir_xlp_fpmove" 2
  (and (eq_attr "cpu" "xlp")
       (eq_attr "type" "mfc"))
 "xlp_ex3,xlp_fp,xlp_fp_wrb")

(define_insn_reservation "ir_xlp_mfhi" 1
  (and (eq_attr "cpu" "xlp")
       (eq_attr "type" "mfhi"))
  "xlp_ex2,xlp_ex2_wrb")

(define_insn_reservation "ir_xlp_mflo" 1
  (and (eq_attr "cpu" "xlp")
       (eq_attr "type" "mflo"))
  "xlp_ex2,xlp_ex2_wrb")

(define_insn_reservation "ir_xlp_mthi" 1
  (and (eq_attr "cpu" "xlp")
       (eq_attr "type" "mthi"))
  "xlp_ex2,xlp_ex2_wrb")

(define_insn_reservation "ir_xlp_mtlo" 3
  (and (eq_attr "cpu" "xlp")
       (eq_attr "type" "mtlo"))
  "xlp_ex2,nothing*2,xlp_ex2_wrb")

(define_insn_reservation "ir_xlp_fp2" 2
  (and (eq_attr "cpu" "xlp")
       (eq_attr "type" "fmove,fneg,fabs,condmove"))
  "xlp_fp,nothing,xlp_fp_wrb")

(define_insn_reservation "ir_xlp_fp3" 3
  (and (eq_attr "cpu" "xlp")
       (eq_attr "type" "fcmp"))
  "xlp_fp,nothing*2,xlp_fp_wrb")

(define_insn_reservation "ir_xlp_fp4" 4
  (and (eq_attr "cpu" "xlp")
       (eq_attr "type" "fcvt"))
  "xlp_fp,nothing*3,xlp_fp_wrb")

(define_insn_reservation "ir_xlp_fp5" 5
  (and (eq_attr "cpu" "xlp")
       (eq_attr "mode" "SF")
       (eq_attr "type" "fadd,fmul"))
  "xlp_fp,nothing*4,xlp_fp_wrb")

(define_insn_reservation "ir_xlp_fp6" 6
  (and (eq_attr "cpu" "xlp")
       (eq_attr "mode" "DF")
       (eq_attr "type" "fadd,fmul"))
  "xlp_fp,nothing*5,xlp_fp_wrb")

(define_insn_reservation "ir_xlp_fp9" 9
  (and (eq_attr "cpu" "xlp")
       (eq_attr "mode" "SF")
       (eq_attr "type" "fmadd"))
  "xlp_fp,nothing*3,xlp_fp,nothing*3,xlp_fp_wrb")

(define_insn_reservation "ir_xlp_fp11" 11
  (and (eq_attr "cpu" "xlp")
       (eq_attr "mode" "DF")
       (eq_attr "type" "fmadd"))
  "xlp_fp,nothing*4,xlp_fp,nothing*4,xlp_fp_wrb")

(define_insn_reservation "ir_xlp_fpcomplex_s" 23
  (and (eq_attr "cpu" "xlp")
       (eq_attr "mode" "SF")
       (eq_attr "type" "fdiv,frdiv,frdiv1,frdiv2,fsqrt,frsqrt,frsqrt1,frsqrt2"))
  "xlp_fp+xlp_divsq,xlp_divsq*22,xlp_fp_wrb")

(define_insn_reservation "ir_xlp_fpcomplex_d" 38
  (and (eq_attr "cpu" "xlp")
       (eq_attr "mode" "DF")
       (eq_attr "type" "fdiv,frdiv,frdiv1,frdiv2,fsqrt,frsqrt,frsqrt1,frsqrt2"))
  "xlp_fp+xlp_divsq,xlp_divsq*37,xlp_fp_wrb")

(define_bypass 3 "ir_xlp_mul" "ir_xlp_mfhi")

(define_insn_reservation "ir_xlp_atomic" 15
  (and (eq_attr "cpu" "xlp")
       (eq_attr "type" "atomic"))
  "xlp_ex0|xlp_ex1")

;; Sync loop consists of (in order)
;; (1) optional sync,
;; (2) LL instruction,
;; (3) branch and 1-2 ALU instructions,
;; (4) SC instruction,
;; (5) optional sync,
;; (6) branch and ALU instruction.
;; The net result of this reservation is a big delay with flush of
;; ALU pipeline and outgoing reservations discouraging use of EX3.
(define_insn_reservation "ir_xlp_sync_loop" 40
  (and (eq_attr "cpu" "xlp")
       (eq_attr "type" "syncloop"))
  "(xlp_ex0+xlp_ex1+xlp_ex2+xlp_ex3)*39,xlp_ex3+(xlp_ex0|xlp_ex1|(xlp_ex2,xlp_ex2_wrb))")
