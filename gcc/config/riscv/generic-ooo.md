;; RISC-V generic out-of-order core scheduling model.
;; Copyright (C) 2023-2025 Free Software Foundation, Inc.
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

(define_automaton "generic_ooo")

;; Regarding functional units we assume a three-way split:
;; - Integer ALU (IXU) - 4 symmetric units.
;; - Floating-point (FXU) - 2 symmetric units.
;; - Vector Unit (VXU) - 1 unit.

;; We assume 6-wide issue:
;; - 5-wide generic/integer issue.
;; - 1-wide vector issue.

;; For now, the only subunits are for non-pipelined integer division and
;; vector div/mult/sqrt.
;; No extra units for e.g. vector permutes, masking, everything is assumed to
;; be on the same pipelined execution unit.

;; Latency:
;; - Regular integer operations take 1 cycle.
;; - Multiplication/Division take multiple cycles.
;; - Float operations take 4-6 cycles.
;; - Regular vector operations take 2-6 cycles.
;;   (This assumes LMUL = 1, latency for LMUL = 2, 4, 8 is scaled accordingly
;;    by riscv_sched_adjust_cost when -madjust-lmul-cost is given)
;; - Load/Store:
;;   - To/From IXU: 4 cycles.
;;   - To/From FXU: 6 cycles.
;;   - To/From VXU: 6 cycles.

;; Integer/float issue queues.
(define_cpu_unit "issue0,issue1,issue2,issue3,issue4" "generic_ooo")

;; Integer/float execution units.
(define_cpu_unit "ixu0,ixu1,ixu2,ixu3" "generic_ooo")
(define_cpu_unit "fxu0,fxu1" "generic_ooo")

;; Integer subunit for division.
(define_cpu_unit "generic_ooo_div" "generic_ooo")

;; Shortcuts
(define_reservation "generic_ooo_issue" "issue0|issue1|issue2|issue3|issue4")
(define_reservation "generic_ooo_ixu_alu" "ixu0|ixu1|ixu2|ixu3")
(define_reservation "generic_ooo_fxu" "fxu0|fxu1")


;; Integer load/store
(define_insn_reservation "generic_ooo_int_load" 4
  (and (eq_attr "tune" "generic_ooo")
       (eq_attr "type" "load"))
  "generic_ooo_issue,generic_ooo_ixu_alu")

(define_insn_reservation "generic_ooo_int_store" 4
  (and (eq_attr "tune" "generic_ooo")
       (eq_attr "type" "store"))
  "generic_ooo_issue,generic_ooo_ixu_alu")

;; Float load/store
(define_insn_reservation "generic_ooo_float_load" 6
  (and (eq_attr "tune" "generic_ooo")
       (eq_attr "type" "fpload"))
  "generic_ooo_issue,generic_ooo_ixu_alu")

(define_insn_reservation "generic_ooo_float_store" 6
  (and (eq_attr "tune" "generic_ooo")
       (eq_attr "type" "fpstore"))
  "generic_ooo_issue,generic_ooo_fxu")

;; Generic integer instructions.
(define_insn_reservation "generic_ooo_alu" 1
  (and (eq_attr "tune" "generic_ooo")
       (eq_attr "type" "unknown,const,arith,shift,slt,multi,auipc,nop,logical,\
			move,bitmanip,rotate,min,max,minu,maxu,clz,ctz,atomic,\
			condmove,mvpair,zicond"))
  "generic_ooo_issue,generic_ooo_ixu_alu")

(define_insn_reservation "generic_ooo_sfb_alu" 2
  (and (eq_attr "tune" "generic_ooo")
       (eq_attr "type" "sfb_alu"))
  "generic_ooo_issue,generic_ooo_ixu_alu")

;; Branch instructions
(define_insn_reservation "generic_ooo_branch" 1
  (and (eq_attr "tune" "generic_ooo")
       (eq_attr "type" "branch,jump,call,jalr,ret,trap"))
  "generic_ooo_issue,generic_ooo_ixu_alu")

;; Float move, convert and compare.
(define_insn_reservation "generic_ooo_float_move" 3
  (and (eq_attr "tune" "generic_ooo")
       (eq_attr "type" "fmove"))
  "generic_ooo_issue,generic_ooo_fxu")

(define_insn_reservation "generic_ooo_fcvt" 3
  (and (eq_attr "tune" "generic_ooo")
       (eq_attr "type" "fcvt,fcvt_i2f,fcvt_f2i"))
  "generic_ooo_issue,generic_ooo_fxu")

(define_insn_reservation "generic_ooo_fcmp" 2
  (and (eq_attr "tune" "generic_ooo")
       (eq_attr "type" "fcmp"))
  "generic_ooo_issue,generic_ooo_fxu")

;; Integer multiplication.
(define_insn_reservation "generic_ooo_imul" 4
  (and (eq_attr "tune" "generic_ooo")
       (eq_attr "type" "imul"))
  "generic_ooo_issue,generic_ooo_ixu_alu,generic_ooo_ixu_alu")

;; Assume integer division is not pipelined.  Do not block the unit for more
;; than three cycles so the DFA does not get too large.  Similar for other
;; non-pipelined instructions.
(define_insn_reservation "generic_ooo_idiv" 16
  (and (eq_attr "tune" "generic_ooo")
       (eq_attr "type" "idiv"))
  "generic_ooo_issue,generic_ooo_ixu_alu,generic_ooo_div,generic_ooo_div*3")

;; Float addition and multiplication.
(define_insn_reservation "generic_ooo_faddmul" 4
  (and (eq_attr "tune" "generic_ooo")
       (eq_attr "type" "fadd,fmul"))
  "generic_ooo_issue,generic_ooo_fxu")

;; Float FMA.
(define_insn_reservation "generic_ooo_float_fma" 6
  (and (eq_attr "tune" "generic_ooo")
       (eq_attr "type" "fmadd"))
  "generic_ooo_issue,generic_ooo_fxu")

;; Assume float division and sqrt are not pipelined.
(define_insn_reservation "generic_ooo_float_div_half" 10
  (and (eq_attr "tune" "generic_ooo")
       (and (eq_attr "type" "fdiv,fsqrt")
	    (eq_attr "mode" "HF")))
  "generic_ooo_issue,generic_ooo_fxu,generic_ooo_div,generic_ooo_div*3")

(define_insn_reservation "generic_ooo_float_div_single" 12
  (and (eq_attr "tune" "generic_ooo")
       (and (eq_attr "type" "fdiv,fsqrt")
	    (eq_attr "mode" "SF")))
  "generic_ooo_issue,generic_ooo_fxu,generic_ooo_div,generic_ooo_div*3")

(define_insn_reservation "generic_ooo_float_div_double" 16
  (and (eq_attr "tune" "generic_ooo")
       (and (eq_attr "type" "fdiv,fsqrt")
	    (eq_attr "mode" "DF")))
  "generic_ooo_issue,generic_ooo_fxu,generic_ooo_div,generic_ooo_div*3")

;; Popcount and clmul.
(define_insn_reservation "generic_ooo_popcount" 2
  (and (eq_attr "tune" "generic_ooo")
       (eq_attr "type" "cpop,clmul"))
  "generic_ooo_issue,generic_ooo_ixu_alu")

;; Transfer from/to coprocessor.  Assume not pipelined.
(define_insn_reservation "generic_ooo_xfer" 4
  (and (eq_attr "tune" "generic_ooo")
       (eq_attr "type" "mfc,mtc"))
  "generic_ooo_issue,generic_ooo_ixu_alu,generic_ooo_ixu_alu*3")
