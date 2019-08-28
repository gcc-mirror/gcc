;; DFA scheduling description of the Synopsys ARCv3 HS6x cpu
;; for GNU C compiler
;; Copyright (C) 2021 Free Software Foundation, Inc.

;; This file is part of GCC.

;; GCC is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; GCC is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GCC; see the file COPYING3.  If not see
;; <http://www.gnu.org/licenses/>.

(define_automaton "HS6x")

(define_cpu_unit "ALU0"    "HS6x")
(define_cpu_unit "ALU1"    "HS6x")
(define_cpu_unit "MPY32"   "HS6x")
(define_cpu_unit "MPY64"   "HS6x")
(define_cpu_unit "DIV"     "HS6x")
(define_cpu_unit "DMP"     "HS6x")
;;(define_cpu_unit "DMP_FPU" "HS6x")
;;(define_cpu_unit "SP_FPU"  "HS6x")
;;(define_cpu_unit "DP_FPU"  "HS6x")

;; Instruction reservation for arithmetic instructions (pipe A, pipe B).
(define_insn_reservation "alu_arith" 3
  (eq_attr "type" "abs, adcl, add, addhl, addl, and, andl, asl, asll,
		   asr, asrl, bclr, bic, bmsk, bset, bsetl, btst,
		   bxor, bxorl, cmp, ext, ffs, fls, lsr, lsrl, max,
		   maxl, min, minl, move, movecc, neg, nop, norm,
		   normh, norml, not, notl, or, orl, rol, ror, sbcl,
		   setcc, sex, sub, subl, swap, swape, swapel, swapl,
		   tst, vadd, vpack, vsub, xbfu, xor, xorl")
  "(ALU0 | ALU1), nothing*2")

(define_insn_reservation "jmp_insn" 1
  (eq_attr "type" "bbit, bi, bl, branch, branchcc, brcc, dbnz, jl,
		   jump, return, trap")
  "ALU0")

(define_insn_reservation "div_insn" 12
  (eq_attr "type" "div, divl, mod, modl, udiv, udivl, umod, umodl")
  "ALU0 + DIV, DIV*11")

(define_insn_reservation "mpy32_insn" 6
  (eq_attr "type" "dmpywh, mac, mpy, qmach, qmpyh, vmac2h, vmpy2h")
  "ALU0 + MPY32 + MPY64, nothing*5")

(define_insn_reservation "mpy64_insn" 9
  (eq_attr "type" "mpyl")
  "ALU0 + MPY32 + MPY64, MPY64*3, nothing*5")

(define_insn_reservation "load_insn" 3
  (eq_attr "type" "atldlop, atldop, ex, ld, llock")
  "ALU1 + DMP, nothing*2")

(define_insn_reservation "store_insn" 1
  (eq_attr "type" "scond, st")
  "ALU1 + DMP")

(define_insn_reservation "core" 1
  (eq_attr "type" "block, brk, dmb, flag, lr, sr, sync")
  "ALU0 + ALU1 + DMP + MPY32 + MPY64 + DIV")

;; Bypasses
(define_bypass 1 "load_insn" "alu_arith")
(define_bypass 1 "load_insn" "mpy*_insn")
(define_bypass 1 "load_insn" "store_insn" "store_data_bypass_p")
(define_bypass 2 "load_insn" "load_insn")
(define_bypass 1 "load_insn" "div_insn")

;;(define_bypass 5 "mpy64_insn" "alu_arith")
(define_bypass 6 "mpy64_insn" "mpy*_insn")
(define_bypass 6 "mpy64_insn" "store_insn" "store_data_bypass_p")
(define_bypass 6 "mpy64_insn" "div_insn")

(define_bypass 3 "mpy32_insn" "mpy*_insn")
(define_bypass 3 "mpy32_insn" "div_insn")

(define_bypass 1 "alu_arith" "mpy*_insn" "!accumulator_bypass_p")
(define_bypass 1 "alu_arith" "div_insn")
(define_bypass 1 "alu_arith" "store_insn" "store_data_bypass_p")

(define_bypass 1 "alu_arith" "alu_arith" "set_accumulator_p")
