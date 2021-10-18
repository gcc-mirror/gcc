;; Scheduling description for LEON5.
;;   Copyright (C) 2021 Free Software Foundation, Inc.
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


;; The LEON5 can often dual issue instructions from the same 64-bit aligned
;; double word if there are no data dependencies.
;;
;; Avoid scheduling load/store, FPU, and multiply instructions back to
;; back, regardless of data dependencies.
;;
;; Push comparisons away from the associated branch instruction.
;;
;; Avoid scheduling ALU instructions with data dependencies back to back.
;;
;; Schedule three instructions between load and dependent instruction.

(define_automaton "leon5")

(define_cpu_unit "leon5_memory" "leon5")
(define_cpu_unit "leon5_mul" "leon5")
(define_cpu_unit "grfpu_d" "grfpu")
(define_cpu_unit "grfpu_s" "grfpu")

(define_insn_reservation "leon5_load" 4
  (and (eq_attr "cpu" "leon5")
  (eq_attr "type" "load,sload"))
  "leon5_memory * 2, nothing * 2")

(define_insn_reservation "leon5_fpload" 2
  (and (eq_attr "cpu" "leon5")
  (eq_attr "type" "fpload"))
  "leon5_memory * 2 + grfpu_alu * 2")

(define_insn_reservation "leon5_store" 2
  (and (eq_attr "cpu" "leon5")
  (eq_attr "type" "store"))
  "leon5_memory * 2")

(define_insn_reservation "leon5_fpstore" 2
  (and (eq_attr "cpu" "leon5")
  (eq_attr "type" "fpstore"))
  "leon5_memory * 2 + grfpu_alu * 2")

(define_insn_reservation "leon5_ialu" 2
  (and (eq_attr "cpu" "leon5")
  (eq_attr "type" "ialu, shift, ialuX"))
  "nothing * 2")

(define_insn_reservation "leon5_compare" 5
  (and (eq_attr "cpu" "leon5")
  (eq_attr "type" "compare"))
  "nothing * 5")

(define_insn_reservation "leon5_imul" 4
  (and (eq_attr "cpu" "leon5")
  (eq_attr "type" "imul"))
  "leon5_mul * 2, nothing * 2")

(define_insn_reservation "leon5_idiv" 35
  (and (eq_attr "cpu" "leon5")
  (eq_attr "type" "imul"))
  "nothing * 35")

(define_insn_reservation "leon5_fp_alu" 5
  (and (eq_attr "cpu" "leon5")
  (eq_attr "type" "fp,fpcmp,fpmul,fpmove"))
  "grfpu_alu * 2, nothing*3")

(define_insn_reservation "leon5_fp_divs" 17
  (and (eq_attr "cpu" "leon5")
  (eq_attr "type" "fpdivs"))
  "grfpu_alu * 2 + grfpu_d*16, nothing")

(define_insn_reservation "leon5_fp_divd" 18
  (and (eq_attr "cpu" "leon5")
  (eq_attr "type" "fpdivd"))
  "grfpu_alu * 2 + grfpu_d*17, nothing")

(define_insn_reservation "leon5_fp_sqrts" 25
  (and (eq_attr "cpu" "leon5")
  (eq_attr "type" "fpsqrts"))
  "grfpu_alu * 2 + grfpu_s*24, nothing")

(define_insn_reservation "leon5_fp_sqrtd" 26
  (and (eq_attr "cpu" "leon5")
  (eq_attr "type" "fpsqrtd"))
  "grfpu_alu * 2 + grfpu_s*25, nothing")
