;;  Octeon pipeline description.
;;  Copyright (C) 2008-2016 Free Software Foundation, Inc.

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
;;   Copyright (C) 2004, 2005, 2006 Cavium Networks.


;; Octeon is a dual-issue processor that can issue all instructions on
;; pipe0 and a subset on pipe1.

(define_automaton "octeon_main, octeon_mult, octeon_fpu")

(define_cpu_unit "octeon_pipe0" "octeon_main")
(define_cpu_unit "octeon_pipe1" "octeon_main")
(define_cpu_unit "octeon_mult" "octeon_mult")
(define_cpu_unit "octeon_fpu" "octeon_fpu")

(define_insn_reservation "octeon_arith" 1
  (and (eq_attr "cpu" "octeon,octeon2,octeon3")
       (eq_attr "type" "arith,const,logical,move,shift,signext,slt,nop"))
  "octeon_pipe0 | octeon_pipe1")

(define_insn_reservation "octeon_condmove_o1" 2
  (and (eq_attr "cpu" "octeon")
       (eq_attr "type" "condmove"))
  "octeon_pipe0 | octeon_pipe1")

(define_insn_reservation "octeon_condmove_o2" 3
  (and (eq_attr "cpu" "octeon2,octeon3")
       (eq_attr "type" "condmove")
       (not (eq_attr "mode" "SF, DF")))
  "octeon_pipe0 | octeon_pipe1")

;; movt/movf can only issue in pipe1
(define_insn_reservation "octeon_condmove_o3_int_on_cc" 3
  (and (eq_attr "cpu" "octeon2,octeon3")
       (eq_attr "type" "condmove")
       (not (eq_attr "mode" "SF, DF")))
  "octeon_pipe1")

(define_insn_reservation "octeon_load_o1" 2
  (and (eq_attr "cpu" "octeon")
       (eq_attr "type" "load,prefetch,mtc,mfc"))
  "octeon_pipe0")

(define_insn_reservation "octeon_load_o2" 3
  (and (eq_attr "cpu" "octeon2,octeon3")
       (eq_attr "type" "load,prefetch"))
  "octeon_pipe0")

;; ??? memory-related cop0 reads are pipe0 with 3-cycle latency.
;; Front-end-related ones are 1-cycle on pipe1.  Assume front-end for now.
(define_insn_reservation "octeon_cop_o2" 1
  (and (eq_attr "cpu" "octeon2,octeon3")
       (eq_attr "type" "mtc,mfc"))
  "octeon_pipe1")

(define_insn_reservation "octeon_store" 1
  (and (eq_attr "cpu" "octeon,octeon2,octeon3")
       (eq_attr "type" "store"))
  "octeon_pipe0")

(define_insn_reservation "octeon_brj_o1" 1
  (and (eq_attr "cpu" "octeon")
       (eq_attr "type" "branch,jump,call,trap"))
  "octeon_pipe0")

(define_insn_reservation "octeon_brj_o2" 2
  (and (eq_attr "cpu" "octeon2,octeon3")
       (eq_attr "type" "branch,jump,call,trap"))
  "octeon_pipe1")

(define_insn_reservation "octeon_imul3_o1" 5
  (and (eq_attr "cpu" "octeon")
       (eq_attr "type" "imul3,pop,clz"))
  "(octeon_pipe0 | octeon_pipe1) + octeon_mult")

(define_insn_reservation "octeon_imul3_o2" 6
  (and (eq_attr "cpu" "octeon2,octeon3")
       (eq_attr "type" "imul3,pop,clz"))
  "octeon_pipe1 + octeon_mult")

(define_insn_reservation "octeon_imul_o1" 2
  (and (eq_attr "cpu" "octeon")
       (eq_attr "type" "imul,mthi,mtlo"))
  "(octeon_pipe0 | octeon_pipe1) + octeon_mult, octeon_mult")

(define_insn_reservation "octeon_imul_o2" 1
  (and (eq_attr "cpu" "octeon2,octeon3")
       (eq_attr "type" "imul,mthi,mtlo"))
  "octeon_pipe1 + octeon_mult")

(define_insn_reservation "octeon_mfhilo_o1" 5
  (and (eq_attr "cpu" "octeon")
       (eq_attr "type" "mfhi,mflo"))
  "(octeon_pipe0 | octeon_pipe1) + octeon_mult")

(define_insn_reservation "octeon_mfhilo_o2" 6
  (and (eq_attr "cpu" "octeon2,octeon3")
       (eq_attr "type" "mfhi,mflo"))
  "octeon_pipe1 + octeon_mult")

(define_insn_reservation "octeon_imadd_o1" 4
  (and (eq_attr "cpu" "octeon")
       (eq_attr "type" "imadd"))
  "(octeon_pipe0 | octeon_pipe1) + octeon_mult, octeon_mult*3")

(define_insn_reservation "octeon_imadd_o2" 1
  (and (eq_attr "cpu" "octeon2,octeon3")
       (eq_attr "type" "imadd"))
  "octeon_pipe1 + octeon_mult")

(define_insn_reservation "octeon_idiv_o1" 72
  (and (eq_attr "cpu" "octeon")
       (eq_attr "type" "idiv"))
  "(octeon_pipe0 | octeon_pipe1) + octeon_mult, octeon_mult*71")

(define_insn_reservation "octeon_idiv_o2_si" 18
  (and (eq_attr "cpu" "octeon2,octeon3")
       (eq_attr "mode" "SI")
       (eq_attr "type" "idiv"))
  "octeon_pipe1 + octeon_mult, octeon_mult*17")

(define_insn_reservation "octeon_idiv_o2_di" 35
  (and (eq_attr "cpu" "octeon2,octeon3")
       (eq_attr "mode" "DI")
       (eq_attr "type" "idiv"))
  "octeon_pipe1 + octeon_mult, octeon_mult*34")

;; Assume both pipes are needed for unknown and multiple-instruction
;; patterns.

(define_insn_reservation "octeon_unknown" 1
  (and (eq_attr "cpu" "octeon,octeon2,octeon3")
       (eq_attr "type" "unknown,multi,atomic,syncloop"))
  "octeon_pipe0 + octeon_pipe1")

;; Octeon3 FPU

(define_insn_reservation "octeon3_faddsubcvt" 4
  (and (eq_attr "cpu" "octeon3")
       (eq_attr "type" "fadd, fcvt"))
  "octeon_pipe1 + octeon_fpu")

(define_insn_reservation "octeon3_fmul" 5
  (and (eq_attr "cpu" "octeon3")
       (eq_attr "type" "fmul"))
  "octeon_pipe1 + octeon_fpu")

(define_insn_reservation "octeon3_fmadd" 9
  (and (eq_attr "cpu" "octeon3")
       (eq_attr "type" "fmadd"))
  "octeon_pipe1 + octeon_fpu, octeon_fpu")

(define_insn_reservation "octeon3_div_sf" 12
  (and (eq_attr "cpu" "octeon3")
       (eq_attr "type" "fdiv, frdiv")
       (eq_attr "mode" "SF"))
  "octeon_pipe1 + octeon_fpu, octeon_fpu*8")

(define_insn_reservation "octeon3_div_df" 22
  (and (eq_attr "cpu" "octeon3")
       (eq_attr "type" "fdiv, frdiv")
       (eq_attr "mode" "SF"))
  "octeon_pipe1 + octeon_fpu, octeon_fpu*18")

(define_insn_reservation "octeon3_sqrt_sf" 16
  (and (eq_attr "cpu" "octeon3")
       (eq_attr "type" "fsqrt")
       (eq_attr "mode" "SF"))
  "octeon_pipe1 + octeon_fpu, octeon_fpu*12")

(define_insn_reservation "octeon3_sqrt_df" 30
  (and (eq_attr "cpu" "octeon3")
       (eq_attr "type" "fsqrt")
       (eq_attr "mode" "DF"))
  "octeon_pipe1 + octeon_fpu, octeon_fpu*26")

(define_insn_reservation "octeon3_rsqrt_sf" 27
  (and (eq_attr "cpu" "octeon3")
       (eq_attr "type" "frsqrt")
       (eq_attr "mode" "SF"))
  "octeon_pipe1 + octeon_fpu, octeon_fpu*23")

(define_insn_reservation "octeon3_rsqrt_df" 51
  (and (eq_attr "cpu" "octeon3")
       (eq_attr "type" "frsqrt")
       (eq_attr "mode" "DF"))
  "octeon_pipe1 + octeon_fpu, octeon_fpu*47")

(define_insn_reservation "octeon3_fabsnegmov" 2
  (and (eq_attr "cpu" "octeon3")
       (eq_attr "type" "fabs, fneg, fmove"))
  "octeon_pipe1 + octeon_fpu")

(define_insn_reservation "octeon_fcond" 1
  (and (eq_attr "cpu" "octeon3")
       (eq_attr "type" "fcmp"))
  "octeon_pipe1 + octeon_fpu")

(define_insn_reservation "octeon_fcondmov" 2
  (and (eq_attr "cpu" "octeon3")
       (eq_attr "type" "condmove")
       (eq_attr "mode" "SF,DF"))
  "octeon_pipe1 + octeon_fpu")

(define_insn_reservation "octeon_fpmtc1" 2
  (and (eq_attr "cpu" "octeon3")
       (eq_attr "type" "mtc"))
  "octeon_pipe1 + octeon_fpu")

(define_insn_reservation "octeon_fpmfc1" 6
  (and (eq_attr "cpu" "octeon3")
       (eq_attr "type" "mtc"))
  "octeon_pipe1 + octeon_fpu")

(define_insn_reservation "octeon_fpload" 3
  (and (eq_attr "cpu" "octeon3")
       (eq_attr "type" "fpload,fpidxload"))
  "octeon_pipe0 + octeon_fpu")

(define_insn_reservation "octeon_fpstore" 3
  (and (eq_attr "cpu" "octeon3")
       (eq_attr "type" "fpstore,fpidxstore"))
  "octeon_pipe0 + octeon_pipe1")
