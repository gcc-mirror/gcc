;; ARM Cortex-M7 pipeline description
;; Copyright (C) 2014-2016 Free Software Foundation, Inc.
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

(define_automaton "cortex_m7")

;; We model the dual-issue constraints of this core with
;; following units.

(define_cpu_unit "cm7_i0, cm7_i1" "cortex_m7")
(define_cpu_unit "cm7_a0, cm7_a1" "cortex_m7")
(define_cpu_unit "cm7_branch,cm7_wb,cm7_ext,cm7_shf" "cortex_m7")
(define_cpu_unit "cm7_lsu" "cortex_m7")
(define_cpu_unit "cm7_mac" "cortex_m7")
(define_cpu_unit "cm7_fpu" "cortex_m7")

(define_reservation "cm7_all_units"
                    "cm7_i0+cm7_i1+cm7_a0+cm7_a1+cm7_branch\
                     +cm7_wb+cm7_ext+cm7_shf+cm7_lsu+cm7_mac\
                     +cm7_fpu")

;; Simple alu instruction without inline shift operation.
(define_insn_reservation "cortex_m7_alu_simple" 2
  (and (eq_attr "tune" "cortexm7")
       (eq_attr "type" "alu_imm,alus_imm,logic_imm,logics_imm,\
                        alu_sreg,alus_sreg,logic_reg,logics_reg,\
                        adc_imm,adcs_imm,adc_reg,adcs_reg,\
                        adr,bfm,rev,\
                        shift_imm,shift_reg,\
                        mov_imm,mov_reg,mvn_imm,mvn_reg,\
                        mov_shift_reg,mov_shift,\
                        mvn_shift,mvn_shift_reg,\
                        logic_shift_imm,logics_shift_imm,\
                        alu_shift_reg,alus_shift_reg,\
                        logic_shift_reg,logics_shift_reg,\
                        mrs,clz,f_mcr,f_mrc,multiple,no_insn"))
  "cm7_i0|cm7_i1,cm7_a0|cm7_a1")

;; Simple alu with inline shift operation.
(define_insn_reservation "cortex_m7_alu_shift" 2
   (and (eq_attr "tune" "cortexm7")
	(eq_attr "type" "alu_shift_imm,alus_shift_imm"))
   "cm7_i0|cm7_i1,(cm7_a0|cm7_a1)+cm7_shf+cm7_branch")

;; Only one ALU can be used for DSP instructions.
(define_insn_reservation "cortex_m7_dsp" 2
  (and (eq_attr "tune" "cortexm7")
       (eq_attr "type" "alu_dsp_reg,smlaxy,smlalxy,smulxy"))
  "cm7_i0|cm7_i1,cm7_a0")

;; The multiply instructions.
(define_insn_reservation "cortex_m7_multiply" 2
   (and (eq_attr "tune" "cortexm7")
        (eq_attr "type" "mul,muls,umull,smull"))
   "cm7_i0|cm7_i1,(cm7_a0|cm7_a1)+cm7_wb")

(define_insn_reservation "cortex_m7_idiv" 4
   (and (eq_attr "tune" "cortexm7")
        (eq_attr "type" "sdiv,udiv"))
   "cm7_all_units*4")

(define_insn_reservation "cortex_m7_alu_extend" 2
   (and (eq_attr "tune" "cortexm7")
        (eq_attr "type" "extend"))
   "cm7_i0|cm7_i1,(cm7_a0|cm7_a1)+cm7_ext+cm7_branch")

(define_insn_reservation "cortex_m7_mac" 2
   (and (eq_attr "tune" "cortexm7")
        (eq_attr "type" "mla,mlas"))
   "cm7_i0|cm7_i1,cm7_mac+cm7_wb")

;; The branch instructions.
(define_insn_reservation "cortex_m7_branch" 0
   (and (eq_attr "tune" "cortexm7")
        (eq_attr "type" "branch,call"))
   "cm7_i0|cm7_i1,cm7_branch")

;; The load instructions.
(define_insn_reservation "cortex_m7_load1" 2
   (and (eq_attr "tune" "cortexm7")
        (eq_attr "type" "load_byte, load1"))
   "cm7_i0|cm7_i1,cm7_lsu")

(define_insn_reservation "cortex_m7_load2" 2
   (and (eq_attr "tune" "cortexm7")
        (eq_attr "type" "load2"))
   "cm7_all_units")

(define_insn_reservation "cortex_m7_loadm" 2
   (and (eq_attr "tune" "cortexm7")
        (eq_attr "type" "load3,load4"))
   "cm7_all_units*2")

;; The store instructions.
(define_insn_reservation "cortex_m7_store1" 0
   (and (eq_attr "tune" "cortexm7")
        (eq_attr "type" "store1"))
   "cm7_i0|cm7_i1,cm7_lsu+cm7_wb")

(define_insn_reservation "cortex_m7_store2" 0
   (and (eq_attr "tune" "cortexm7")
        (eq_attr "type" "store2"))
   "cm7_all_units")

(define_insn_reservation "cortex_m7_storem" 0
   (and (eq_attr "tune" "cortexm7")
        (eq_attr "type" "store3,store4"))
   "cm7_all_units*2")

;; The FPU instructions.
(define_insn_reservation "cortex_m7_fpalu" 3
  (and (eq_attr "tune" "cortexm7")
       (eq_attr "type" "ffariths,ffarithd,fadds,faddd,fmov,fconsts,\
                        fconstd,fcmpd,f_cvt,f_cvtf2i,f_cvti2f, fcmps,\
                        fmuls,f_flag"))
  "cm7_i0|cm7_i1,cm7_fpu")

(define_insn_reservation "cortex_m7_fmacs" 6
  (and (eq_attr "tune" "cortexm7")
       (eq_attr "type" "fmacs,ffmas"))
  "cm7_i0|cm7_i1,cm7_fpu")

(define_insn_reservation "cortex_m7_fdivs" 16
  (and (eq_attr "tune" "cortexm7")
       (eq_attr "type" "fdivs, fsqrts"))
  "cm7_i0|cm7_i1, cm7_fpu*5")

(define_insn_reservation "cortex_m7_f_loads" 2
  (and (eq_attr "tune" "cortexm7")
       (eq_attr "type" "f_loads"))
  "cm7_i0|cm7_i1, cm7_lsu")

(define_insn_reservation "cortex_m7_f_stores" 0
  (and (eq_attr "tune" "cortexm7")
       (eq_attr "type" "f_stores"))
  "cm7_i0|cm7_i1, cm7_lsu+cm7_wb")

(define_insn_reservation "cortex_m7_fmuld" 6
  (and (eq_attr "tune" "cortexm7")
       (eq_attr "type" "fmuld"))
  "cm7_i0|cm7_i1,cm7_fpu*3")

(define_insn_reservation "cortex_m7_fmacd" 10
  (and (eq_attr "tune" "cortexm7")
       (eq_attr "type" "fmacd,ffmad"))
  "cm7_i0|cm7_i1,cm7_fpu*4")

(define_insn_reservation "cortex_m7_fdivd" 31
  (and (eq_attr "tune" "cortexm7")
       (eq_attr "type" "fdivd,fsqrtd"))
  "cm7_i0|cm7_i1,cm7_fpu*4")

(define_insn_reservation "cortex_m7_f_loadd" 3
  (and (eq_attr "tune" "cortexm7")
       (eq_attr "type" "f_loadd"))
  "cm7_all_units")

(define_insn_reservation "cortex_m7_f_stored" 0
  (and (eq_attr "tune" "cortexm7")
       (eq_attr "type" "f_stored"))
  "cm7_all_units")

(define_insn_reservation "cortex_m7_f_mcr" 1
  (and (eq_attr "tune" "cortexm7")
       (eq_attr "type" "f_mcrr,f_mrrc"))
  "cm7_all_units")
