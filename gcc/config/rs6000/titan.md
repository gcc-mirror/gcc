;; Pipeline description for the AppliedMicro Titan core.
;;   Copyright (C) 2010-2013 Free Software Foundation, Inc.
;;   Contributed by Theobroma Systems Design und Consulting GmbH
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

;; AppliedMicro Titan core complex

(define_automaton "titan_core,titan_fpu,titan_fxu,titan_bpu,titan_lsu")
(define_cpu_unit "titan_issue_0,titan_issue_1" "titan_core")

;; Some useful abbreviations.
(define_reservation "titan_issue" "titan_issue_0|titan_issue_1")

;; === FXU scheduling ===

(define_cpu_unit "titan_fxu_sh,titan_fxu_wb" "titan_fxu")

;; The 1-cycle adder executes add, addi, subf, neg, compare and trap
;; instructions. It provides its own, dedicated result-bus, so we
;; don't need the titan_fxu_wb reservation to complete.
(define_insn_reservation "titan_fxu_adder" 1
  (and (eq_attr "type" "cmp,fast_compare,trap")
       (eq_attr "cpu" "titan"))
  "titan_issue,titan_fxu_sh")

;; Keep the titan_imul and titan_mulhw (half-word) rules in order, to
;; ensure the proper match: the half-word instructions are tagged as
;; imul3 only, whereas regular multiplys will always carry a imul tag.

(define_insn_reservation "titan_imul" 5
  (and (eq_attr "type" "imul,imul2,imul_compare")
       (eq_attr "cpu" "titan"))       
  "titan_issue,titan_fxu_sh,nothing*5,titan_fxu_wb")  

(define_insn_reservation "titan_mulhw" 4
  (and (eq_attr "type" "imul3")
       (eq_attr "cpu" "titan"))
  "titan_issue,titan_fxu_sh,nothing*4,titan_fxu_wb")

(define_bypass 2 "titan_mulhw" "titan_mulhw")

(define_insn_reservation "titan_fxu_shift_and_rotate" 2
  (and (eq_attr "type" "insert_word,shift,var_shift_rotate,cntlz")
       (eq_attr "cpu" "titan"))
  "titan_issue,titan_fxu_sh,nothing*2,titan_fxu_wb")

;; We model the divider for the worst-case (i.e. a full 32-bit
;; divide).  To model the bypass for byte-wise completion, a
;; define_bypass with a guard-function could be used... however, this
;; would be an optimization of doubtful value, as a large number of
;; divides will operate on 32-bit variables.

;; To avoid an unmanagably large automata (generating the automata
;; would require well over 2GB in memory), we don't model the shared
;; result bus on this one. The divider-pipeline is thus modeled
;; through its latency and initial disptach bottlenecks (i.e. issue
;; slots and fxu scheduler availability)
(define_insn_reservation "titan_fxu_div" 34
  (and (eq_attr "type" "idiv")
       (eq_attr "cpu" "titan"))
  "titan_issue,titan_fxu_sh")

(define_insn_reservation "titan_fxu_alu" 1
  (and (eq_attr "type" "integer,exts")
       (eq_attr "cpu" "titan"))
  "titan_issue,titan_fxu_sh,nothing,titan_fxu_wb")

;; === BPU scheduling ===

(define_cpu_unit "titan_bpu_sh" "titan_bpu")

(define_insn_reservation "titan_bpu" 2
  (and (eq_attr "type" "branch,jmpreg,cr_logical,delayed_cr")
       (eq_attr "cpu" "titan"))
  "titan_issue,titan_bpu_sh")

;; === LSU scheduling ===

(define_cpu_unit "titan_lsu_sh" "titan_lsu")

;; Loads.
(define_insn_reservation "titan_lsu_load" 3
  (and (eq_attr "type" "load,load_ext,load_ext_u,load_ext_ux,load_ux,load_u,\
			load_l,sync")
       (eq_attr "cpu" "titan"))
  "titan_issue,titan_lsu_sh")

(define_insn_reservation "titan_lsu_fpload" 12
  (and (eq_attr "type" "fpload,fpload_ux,fpload_u")
       (eq_attr "cpu" "titan"))
  "titan_issue,titan_lsu_sh")

;; Note that the isync is not clearly placed within any execution
;; unit. We've made the assumption that it will be running out of the
;; LSU, as msync is also executed within the LSU.
(define_insn_reservation "titan_lsu_sync" 20
  (and (eq_attr "type" "sync")
       (eq_attr "cpu" "titan"))
  "titan_issue,titan_lsu_sh*20")

;; Stores.
(define_insn_reservation "titan_lsu_store" 12
  (and (eq_attr "type" "store,store_ux,store_u,store_c")
       (eq_attr "cpu" "titan"))
  "titan_issue,titan_lsu_sh")

(define_insn_reservation "titan_lsu_fpstore" 12
  (and (eq_attr "type" "fpstore,fpstore_ux,fpstore_u")
       (eq_attr "cpu" "titan"))
  "titan_issue,titan_lsu_sh")

;; === FPU scheduling ===

;; In order to keep the automaton for the Titan FPU efficient and
;; maintainable, we've kept in as concise as possible and created a
;; mapping for the main "choke points" only instead of modelling the
;; overall flow of instructions through the FP-pipeline(s).

;; The key elements modelled are:
;;  * each FP-instruction takes up one of the two issue slots 
;;  * the FPU runs at half the core frequency
;;  * divides are not pipelined (but execute in a separate unit)
;;  * the FPU has a shared result bus for all its units

(define_cpu_unit "titan_fp0,titan_fpdiv,titan_fpwb" "titan_fpu")

(define_insn_reservation "titan_fp_div_double" 72
  (and (eq_attr "type" "ddiv")
       (eq_attr "cpu" "titan"))
  "titan_issue,titan_fpdiv*72,titan_fpwb")

(define_insn_reservation "titan_fp_div_single" 46
  (and (eq_attr "type" "sdiv")
       (eq_attr "cpu" "titan"))
  "titan_issue,titan_fpdiv*46,titan_fpwb")

(define_insn_reservation "titan_fp_single" 12
  (and (eq_attr "fp_type" "fp_addsub_s,fp_mul_s,fp_maddsub_s")       
       (eq_attr "cpu" "titan"))
  "titan_issue,titan_fp0*2,nothing*10,titan_fpwb")

;; Make sure the "titan_fp" rule stays last, as it's a catch all for
;; double-precision and unclassified (e.g. fsel) FP-instructions
(define_insn_reservation "titan_fp" 10
  (and (eq_attr "type" "fpcompare,fp,dmul")
       (eq_attr "cpu" "titan"))
  "titan_issue,titan_fp0*2,nothing*8,titan_fpwb")

;; Please note, that the non-pipelined FP-instructions "mcrfs",
;; "mtfsb0[.]", "mtfsb1[.]", "mtfsf[.]", "mtfsfi[.]" are not
;; accessible from regular language constructs (i.e. they are not used
;; by the code generator, except for special purpose sequences defined
;; in rs6000.md), no special provisions are made for these.

