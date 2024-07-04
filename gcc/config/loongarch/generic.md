;; Generic DFA-based pipeline description for LoongArch targets
;; Copyright (C) 2021-2024 Free Software Foundation, Inc.
;; Contributed by Loongson Ltd.
;; Based on MIPS target for GNU compiler.

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

(define_automaton "alu,imuldiv")

(define_cpu_unit "alu" "alu")
(define_cpu_unit "imuldiv" "imuldiv")

;; Ghost instructions produce no real code.
;; They exist purely to express an effect on dataflow.
(define_insn_reservation "ghost" 0
  (eq_attr "type" "ghost")
  "nothing")

(define_insn_reservation "generic_alu" 1
  (eq_attr "type" "unknown,prefetch,prefetchx,condmove,const,arith,
		   shift,slt,clz,trap,multi,nop,logical,signext,move")
  "alu")

(define_insn_reservation "generic_load" 3
  (eq_attr "type" "load,fpload,fpidxload")
  "alu")

(define_insn_reservation "generic_store" 1
  (eq_attr "type" "store,fpstore,fpidxstore")
  "alu")

(define_insn_reservation "generic_xfer" 2
  (eq_attr "type" "mftg,mgtf")
  "alu")

(define_insn_reservation "generic_branch" 1
  (eq_attr "type" "branch,jump,call")
  "alu")

(define_insn_reservation "generic_imul" 17
  (eq_attr "type" "imul")
  "imuldiv*17")

(define_insn_reservation "generic_fcvt" 1
  (eq_attr "type" "fcvt")
  "alu")

(define_insn_reservation "generic_fmove" 2
  (eq_attr "type" "fabs,fneg,fmove")
  "alu")

(define_insn_reservation "generic_fcmp" 3
  (eq_attr "type" "fcmp")
  "alu")

(define_insn_reservation "generic_fadd" 4
  (eq_attr "type" "fadd")
  "alu")

(define_insn_reservation "generic_fmul_single" 7
  (and (eq_attr "type" "fmul,fmadd")
       (eq_attr "mode" "SF"))
  "alu")

(define_insn_reservation "generic_fmul_double" 8
  (and (eq_attr "type" "fmul,fmadd")
       (eq_attr "mode" "DF"))
  "alu")

(define_insn_reservation "generic_fdiv_single" 23
  (and (eq_attr "type" "fdiv,frdiv")
       (eq_attr "mode" "SF"))
  "alu")

(define_insn_reservation "generic_fdiv_double" 36
  (and (eq_attr "type" "fdiv,frdiv")
       (eq_attr "mode" "DF"))
  "alu")

(define_insn_reservation "generic_fsqrt_single" 54
  (and (eq_attr "type" "fsqrt,frsqrt")
       (eq_attr "mode" "SF"))
  "alu")

(define_insn_reservation "generic_fsqrt_double" 112
  (and (eq_attr "type" "fsqrt,frsqrt")
       (eq_attr "mode" "DF"))
  "alu")

(define_insn_reservation "generic_atomic" 10
  (eq_attr "type" "atomic")
  "alu")

;; Sync loop consists of (in order)
;; (1) optional sync,
;; (2) LL instruction,
;; (3) branch and 1-2 ALU instructions,
;; (4) SC instruction,
;; (5) branch and ALU instruction.
;; The net result of this reservation is a big delay with a flush of
;; ALU pipeline.
(define_insn_reservation "generic_sync_loop" 40
  (eq_attr "type" "syncloop")
  "alu*39")
