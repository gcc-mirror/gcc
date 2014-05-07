;; Scheduling description for the Xilinx PowerPC 405 APU Floating Point Unit.
;; Copyright (C) 2008-2014 Free Software Foundation, Inc.
;; Contributed by Michael Eager (eager@eagercon.com).
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

;;----------------------------------------------------
;; Xilinx APU FPU Pipeline Description
;;
;;  - attr 'type' and 'fp_type' should definitely
;;    be cleaned up at some point in the future.
;;    ddiv,sdiv,dmul,smul etc are quite confusing.
;;    Should use consistent fp* attrs. 'fp_type'
;;    should also go away, leaving us only with 'fp'
;;
;;----------------------------------------------------

;; -------------------------------------------------------------------------
;; Latencies
;; Latest latency figures (all in FCB cycles). PowerPC to FPU frequency ratio
;; assumed to be 1/2. (most common deployment)
;; Add 2 PPC cycles for (register file access + wb) and 2 PPC cycles 
;; for issue (from PPC)
;;                          SP          DP
;; Loads:                    4           6
;; Stores:                   1           2      (from availability of data)
;; Move/Abs/Neg:             1           1
;; Add/Subtract:             5           7
;; Multiply:                 4          11
;; Multiply-add:            10          19
;; Convert (any):            4           6
;; Divide/Sqrt:             27          56
;; Compares:                 1           2
;;
;; bypasses needed for forwarding capability of the FPU. 
;; Add this at some future time.
;; -------------------------------------------------------------------------
(define_automaton "Xfpu")
(define_cpu_unit "Xfpu_issue,Xfpu_addsub,Xfpu_mul,Xfpu_div,Xfpu_sqrt" "Xfpu")


(define_insn_reservation "fp-default" 2
  (and (and 
        (eq_attr "type" "fp")
        (eq_attr "fp_type" "fp_default"))
       (eq_attr "cpu" "ppc405"))
  "Xfpu_issue*2")

(define_insn_reservation "fp-compare" 6
  (and (eq_attr "type" "fpcompare")                     ;; Inconsistent naming
       (eq_attr "cpu" "ppc405"))
  "Xfpu_issue*2,Xfpu_addsub")

(define_insn_reservation "fp-addsub-s" 14
  (and (and
        (eq_attr "type" "fp")
        (eq_attr "fp_type" "fp_addsub_s"))
       (eq_attr "cpu" "ppc405"))
  "Xfpu_issue*2,Xfpu_addsub")

(define_insn_reservation "fp-addsub-d" 18
  (and (and
        (eq_attr "type" "fp")
        (eq_attr "fp_type" "fp_addsub_d"))
       (eq_attr "cpu" "ppc405"))
  "Xfpu_issue*2,Xfpu_addsub")

(define_insn_reservation "fp-mul-s" 12
  (and (and
        (eq_attr "type" "fp")
        (eq_attr "fp_type" "fp_mul_s"))
       (eq_attr "cpu" "ppc405"))
  "Xfpu_issue*2,Xfpu_mul")

(define_insn_reservation "fp-mul-d" 16    ;; Actually 28. Long latencies are killing the automaton formation. Need to figure out why.
  (and (and
        (eq_attr "type" "fp")
        (eq_attr "fp_type" "fp_mul_d"))
       (eq_attr "cpu" "ppc405"))
  "Xfpu_issue*2,Xfpu_mul")

(define_insn_reservation "fp-div-s" 24                   ;; Actually 34
   (and (eq_attr "type" "sdiv")                          ;; Inconsistent attr naming
        (eq_attr "cpu" "ppc405"))
   "Xfpu_issue*2,Xfpu_div*10")                           ;; Unpipelined

(define_insn_reservation "fp-div-d" 34                   ;; Actually 116
  (and (eq_attr "type" "ddiv")
       (eq_attr "cpu" "ppc405"))                         ;; Inconsistent attr naming
  "Xfpu_issue*2,Xfpu_div*10")                            ;; Unpipelined

(define_insn_reservation "fp-maddsub-s" 24
  (and (and
        (eq_attr "type" "fp")
        (eq_attr "fp_type" "fp_maddsub_s"))
       (eq_attr "cpu" "ppc405"))
  "Xfpu_issue*2,Xfpu_mul,nothing*7,Xfpu_addsub")

(define_insn_reservation "fp-maddsub-d" 34              ;; Actually 42
  (and (and
        (eq_attr "type" "dmul")                         ;; Inconsistent attr naming
        (eq_attr "fp_type" "fp_maddsub_d"))
       (eq_attr "cpu" "ppc405"))
  "Xfpu_issue*2,Xfpu_mul,nothing*7,Xfpu_addsub")

(define_insn_reservation "fp-load" 10                   ;; FIXME. Is double/single precision the same ?
  (and (eq_attr "type" "fpload")
       (eq_attr "cpu" "ppc405"))
  "Xfpu_issue*10")

(define_insn_reservation "fp-store" 4
  (and (eq_attr "type" "fpstore")
       (eq_attr "cpu" "ppc405"))
  "Xfpu_issue*4")

(define_insn_reservation "fp-sqrt-s" 24         ;; Actually 56
  (and (eq_attr "type" "ssqrt")
       (eq_attr "cpu" "ppc405"))
  "Xfpu_issue*2,Xfpu_sqrt*10")                  ;; Unpipelined


(define_insn_reservation "fp-sqrt-d" 34         ;; Actually 116
  (and (eq_attr "type" "dsqrt")
       (eq_attr "cpu" "ppc405"))
  "Xfpu_issue*2,Xfpu_sqrt*10")                  ;; Unpipelined

