;; DFA-based pipeline description for the XLR.
;;   Copyright (C) 2008-2021 Free Software Foundation, Inc.
;;
;; xlr.md   Machine Description for the RMI XLR Microprocessor
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

(define_automaton "xlr_main,xlr_muldiv")

;; Definitions for xlr_main automaton.
(define_cpu_unit "xlr_main_pipe" "xlr_main")

(define_insn_reservation "ir_xlr_alu_slt" 2
  (and (eq_attr "cpu" "xlr") 
       (eq_attr "type" "slt"))
  "xlr_main_pipe")

(define_insn_reservation "ir_xlr_alu_clz" 2
  (and (eq_attr "cpu" "xlr") 
       (eq_attr "type" "clz"))
  "xlr_main_pipe")

;; Integer arithmetic instructions.
(define_insn_reservation "ir_xlr_alu" 1
  (and (eq_attr "cpu" "xlr") 
       (eq_attr "type" "move,arith,shift,logical,signext,const,unknown,multi,nop,trap,atomic,syncloop"))
  "xlr_main_pipe")

;; Integer arithmetic instructions.
(define_insn_reservation "ir_xlr_condmove" 2
  (and (eq_attr "cpu" "xlr") 
       (eq_attr "type" "condmove"))
  "xlr_main_pipe")

;; Load/store instructions.
(define_insn_reservation "ir_xlr_load" 4
  (and (eq_attr "cpu" "xlr") 
       (eq_attr "type" "load"))
  "xlr_main_pipe")

(define_insn_reservation "ir_xlr_store" 1
  (and  (eq_attr "cpu" "xlr") 
        (eq_attr "type" "store"))
  "xlr_main_pipe")

(define_insn_reservation "ir_xlr_prefetch_x" 1
  (and (eq_attr "cpu" "xlr")
       (eq_attr "type" "prefetch,prefetchx"))
  "xlr_main_pipe")

;; Branch instructions - use branch misprediction latency.
(define_insn_reservation "ir_xlr_branch" 1
  (and (eq_attr "cpu" "xlr") 
       (eq_attr "type" "branch,jump,call"))
  "xlr_main_pipe")

;; Coprocessor move instructions.
(define_insn_reservation "ir_xlr_xfer" 2
  (and (eq_attr "cpu" "xlr") 
       (eq_attr "type" "mtc,mfc"))
  "xlr_main_pipe")

(define_bypass 5 "ir_xlr_xfer" "ir_xlr_xfer")

;; Definitions for the xlr_muldiv automaton.
(define_cpu_unit "xlr_imuldiv_nopipe" "xlr_muldiv")

(define_insn_reservation "ir_xlr_imul" 8
  (and (eq_attr "cpu" "xlr") 
       (eq_attr "type" "imul,imul3,imadd"))
  "xlr_main_pipe,xlr_imuldiv_nopipe*6")

(define_insn_reservation "ir_xlr_div" 68
  (and (eq_attr "cpu" "xlr") 
       (eq_attr "type" "idiv"))
  "xlr_main_pipe,xlr_imuldiv_nopipe*67")

(define_insn_reservation "xlr_hilo" 2
  (and (eq_attr "cpu" "xlr") 
       (eq_attr "type" "mfhi,mflo,mthi,mtlo"))
  "xlr_imuldiv_nopipe")
