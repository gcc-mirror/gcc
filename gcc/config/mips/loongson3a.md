;; Pipeline model for Loongson-3A cores.

;; Copyright (C) 2011-2016 Free Software Foundation, Inc.
;;
;; This file is part of GCC.
;;
;; GCC is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published
;; by the Free Software Foundation; either version 3, or (at your
;; option) any later version.
;;
;; GCC is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
;; License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GCC; see the file COPYING3.  If not see
;; <http://www.gnu.org/licenses/>.

;; Uncomment the following line to output automata for debugging.
;; (automata_option "v")

;; Automaton for integer instructions.
(define_automaton "ls3a_a_alu")

;; Automaton for floating-point instructions.
(define_automaton "ls3a_a_falu")

;; Automaton for memory operations.
(define_automaton "ls3a_a_mem")

;; Describe the resources.

(define_cpu_unit "ls3a_alu1" "ls3a_a_alu")
(define_cpu_unit "ls3a_alu2" "ls3a_a_alu")
(define_cpu_unit "ls3a_mem" "ls3a_a_mem")
(define_cpu_unit "ls3a_falu1" "ls3a_a_falu")
(define_cpu_unit "ls3a_falu2" "ls3a_a_falu")

;; Describe instruction reservations.

(define_insn_reservation "ls3a_arith" 1
  (and (eq_attr "cpu" "loongson_3a")
       (eq_attr "type" "arith,clz,const,logical,
                        move,nop,shift,signext,slt"))
  "ls3a_alu1 | ls3a_alu2")

(define_insn_reservation "ls3a_branch" 1
  (and (eq_attr "cpu" "loongson_3a")
       (eq_attr "type" "branch,jump,call,condmove,trap"))
  "ls3a_alu1")

(define_insn_reservation "ls3a_mfhilo" 1
  (and (eq_attr "cpu" "loongson_3a")
       (eq_attr "type" "mfhi,mflo,mthi,mtlo"))
  "ls3a_alu2")

;; Operation imul3nc is fully pipelined.
(define_insn_reservation "ls3a_imul3nc" 5
  (and (eq_attr "cpu" "loongson_3a")
       (eq_attr "type" "imul3nc"))
  "ls3a_alu2")
 
(define_insn_reservation "ls3a_imul" 7
  (and (eq_attr "cpu" "loongson_3a")
       (eq_attr "type" "imul,imadd"))
  "ls3a_alu2 * 7")
 
(define_insn_reservation "ls3a_idiv_si" 12
  (and (eq_attr "cpu" "loongson_3a")
       (and (eq_attr "type" "idiv")
            (eq_attr "mode" "SI")))
  "ls3a_alu2 * 12")

(define_insn_reservation "ls3a_idiv_di" 25
  (and (eq_attr "cpu" "loongson_3a")
       (and (eq_attr "type" "idiv")
            (eq_attr "mode" "DI")))
  "ls3a_alu2 * 25")

(define_insn_reservation "ls3a_load" 3
  (and (eq_attr "cpu" "loongson_3a")
       (eq_attr "type" "load"))
  "ls3a_mem")
 
(define_insn_reservation "ls3a_fpload" 4
  (and (eq_attr "cpu" "loongson_3a")
       (eq_attr "type" "load,mfc,mtc"))
  "ls3a_mem")

(define_insn_reservation "ls3a_prefetch" 0
  (and (eq_attr "cpu" "loongson_3a")
       (eq_attr "type" "prefetch,prefetchx"))
  "ls3a_mem")
 
(define_insn_reservation "ls3a_store" 0
  (and (eq_attr "cpu" "loongson_3a")
       (eq_attr "type" "store,fpstore,fpidxstore"))
  "ls3a_mem")

;; All the fp operations can be executed in FALU1.  Only fp add,
;; sub, mul, madd can be executed in FALU2.  Try FALU2 firstly.
(define_insn_reservation "ls3a_fadd" 6
  (and (eq_attr "cpu" "loongson_3a")
       (eq_attr "type" "fadd,fmul,fmadd"))
  "ls3a_falu2 | ls3a_falu1")

(define_insn_reservation "ls3a_fcmp" 2
  (and (eq_attr "cpu" "loongson_3a")
       (eq_attr "type" "fabs,fcmp,fmove,fneg"))
  "ls3a_falu1")

(define_insn_reservation "ls3a_fcvt" 4
  (and (eq_attr "cpu" "loongson_3a")
       (eq_attr "type" "fcvt"))
  "ls3a_falu1")

(define_insn_reservation "ls3a_fdiv_sf" 12
  (and (eq_attr "cpu" "loongson_3a")
       (and (eq_attr "type" "fdiv,frdiv,fsqrt,frsqrt")
            (eq_attr "mode" "SF")))
  "ls3a_falu1 * 12")
 
(define_insn_reservation "ls3a_fdiv_df" 19
  (and (eq_attr "cpu" "loongson_3a")
       (and (eq_attr "type" "fdiv,frdiv,fsqrt,frsqrt")
            (eq_attr "mode" "DF")))
  "ls3a_falu1 * 19")

;; Force single-dispatch for unknown or multi.
(define_insn_reservation "ls3a_unknown" 1
  (and (eq_attr "cpu" "loongson_3a")
       (eq_attr "type" "unknown,multi,atomic,syncloop"))
  "ls3a_alu1 + ls3a_alu2 + ls3a_falu1 + ls3a_falu2 + ls3a_mem")

;; End of DFA-based pipeline description for loongson_3a
