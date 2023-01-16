;; Generic DFA-based pipeline description for RISC-V targets.
;; Copyright (C) 2011-2023 Free Software Foundation, Inc.
;; Contributed by Andrew Waterman (andrew@sifive.com).
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


(define_automaton "pipe0")
(define_cpu_unit "alu" "pipe0")
(define_cpu_unit "imuldiv" "pipe0")
(define_cpu_unit "fdivsqrt" "pipe0")

(define_insn_reservation "generic_alu" 1
  (and (eq_attr "tune" "generic")
       (eq_attr "type" "unknown,const,arith,shift,slt,multi,auipc,nop,logical,move"))
  "alu")

(define_insn_reservation "generic_load" 3
  (and (eq_attr "tune" "generic")
       (eq_attr "type" "load,fpload"))
  "alu")

(define_insn_reservation "generic_store" 1
  (and (eq_attr "tune" "generic")
       (eq_attr "type" "store,fpstore"))
  "alu")

(define_insn_reservation "generic_xfer" 3
  (and (eq_attr "tune" "generic")
       (eq_attr "type" "mfc,mtc,fcvt,fmove,fcmp"))
  "alu")

(define_insn_reservation "generic_branch" 1
  (and (eq_attr "tune" "generic")
       (eq_attr "type" "branch,jump,call"))
  "alu")

(define_insn_reservation "generic_imul" 10
  (and (eq_attr "tune" "generic")
       (eq_attr "type" "imul"))
  "imuldiv*10")

(define_insn_reservation "generic_idivsi" 34
  (and (eq_attr "tune" "generic")
       (and (eq_attr "type" "idiv")
	    (eq_attr "mode" "SI")))
  "imuldiv*34")

(define_insn_reservation "generic_idivdi" 66
  (and (eq_attr "tune" "generic")
       (and (eq_attr "type" "idiv")
	    (eq_attr "mode" "DI")))
  "imuldiv*66")

(define_insn_reservation "generic_fmul_single" 5
  (and (eq_attr "tune" "generic")
       (and (eq_attr "type" "fadd,fmul,fmadd")
	    (eq_attr "mode" "SF")))
  "alu")

(define_insn_reservation "generic_fmul_double" 7
  (and (eq_attr "tune" "generic")
       (and (eq_attr "type" "fadd,fmul,fmadd")
	    (eq_attr "mode" "DF")))
  "alu")

(define_insn_reservation "generic_fdiv" 20
  (and (eq_attr "tune" "generic")
       (eq_attr "type" "fdiv"))
  "fdivsqrt*20")

(define_insn_reservation "generic_fsqrt" 25
  (and (eq_attr "tune" "generic")
       (eq_attr "type" "fsqrt"))
  "fdivsqrt*25")
