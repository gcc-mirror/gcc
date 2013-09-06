;; Marvell ARM Processor Pipeline Description
;; Copyright (C) 2010-2013 Free Software Foundation, Inc.
;; Contributed by Marvell.

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

;; Pipeline description for the Marvell PJ4, aka "Flareon".
(define_automaton "pj4")

;; Issue resources
(define_cpu_unit    "pj4_is1,pj4_is2"        "pj4")
(define_reservation "pj4_is"             "(pj4_is1|pj4_is2)")
(define_reservation "pj4_isb"            "(pj4_is1+pj4_is2)")

;; Functional units
(define_cpu_unit "pj4_alu1,pj4_alu2,pj4_mul,pj4_div" "pj4")

;; Completion ports
(define_cpu_unit "pj4_w1,pj4_w2"             "pj4")

;; Complete/Retire control
(define_cpu_unit    "pj4_c1,pj4_c2"          "pj4")
(define_reservation "pj4_cp"             "(pj4_c1|pj4_c2)")
(define_reservation "pj4_cpb"            "(pj4_c1+pj4_c2)")

;; Integer arithmetic instructions

(define_insn_reservation "pj4_alu_e1" 1
  (and (eq_attr "tune" "marvell_pj4")
       (eq_attr "type" "mov_imm,mov_reg,mvn_imm,mvn_reg")
       (not (eq_attr "conds" "set")))
                               "pj4_is,(pj4_alu1,pj4_w1+pj4_cp)|(pj4_alu2,pj4_w2+pj4_cp)")

(define_insn_reservation "pj4_alu_e1_conds" 4
  (and (eq_attr "tune" "marvell_pj4")
       (eq_attr "type" "mov_imm,mov_reg,mvn_imm,mvn_reg")
       (eq_attr "conds" "set"))
                               "pj4_is,(pj4_alu1,pj4_w1+pj4_cp)|(pj4_alu2,pj4_w2+pj4_cp)")

(define_insn_reservation "pj4_alu" 1
  (and (eq_attr "tune" "marvell_pj4")
       (eq_attr "type" "alu_imm,alus_imm,alu_reg,alus_reg,\
                        logic_imm,logics_imm,logic_reg,logics_reg,\
                        adc_imm,adcs_imm,adc_reg,adcs_reg,\
                        adr,bfm,rev,\
                        shift_imm,shift_reg")
       (not (eq_attr "conds" "set")))
                               "pj4_is,(pj4_alu1,pj4_w1+pj4_cp)|(pj4_alu2,pj4_w2+pj4_cp)")

(define_insn_reservation "pj4_alu_conds" 4
  (and (eq_attr "tune" "marvell_pj4")
       (eq_attr "type" "alu_imm,alus_imm,alu_reg,alus_reg,\
                        logic_imm,logics_imm,logic_reg,logics_reg,\
                        adc_imm,adcs_imm,adc_reg,adcs_reg,\
                        adr,bfm,rev,\
                        shift_imm,shift_reg")
       (eq_attr "conds" "set"))
                               "pj4_is,(pj4_alu1,pj4_w1+pj4_cp)|(pj4_alu2,pj4_w2+pj4_cp)")

(define_insn_reservation "pj4_shift" 1
  (and (eq_attr "tune" "marvell_pj4")
       (eq_attr "type" "alu_shift_imm,logic_shift_imm,\
                        alus_shift_imm,logics_shift_imm,\
                        alu_shift_reg,logic_shift_reg,\
                        alus_shift_reg,logics_shift_reg,\
                        extend,\
                        mov_shift,mvn_shift,mov_shift_reg,mvn_shift_reg")
       (not (eq_attr "conds" "set"))
       (eq_attr "shift" "1"))  "pj4_is,(pj4_alu1,pj4_w1+pj4_cp)|(pj4_alu2,pj4_w2+pj4_cp)")

(define_insn_reservation "pj4_shift_conds" 4
  (and (eq_attr "tune" "marvell_pj4")
       (eq_attr "type" "alu_shift_imm,logic_shift_imm,\
                        alus_shift_imm,logics_shift_imm,\
                        alu_shift_reg,logic_shift_reg,\
                        alus_shift_reg,logics_shift_reg,\
                        extend,\
                        mov_shift,mvn_shift,mov_shift_reg,mvn_shift_reg")
       (eq_attr "conds" "set")
       (eq_attr "shift" "1"))  "pj4_is,(pj4_alu1,pj4_w1+pj4_cp)|(pj4_alu2,pj4_w2+pj4_cp)")

(define_insn_reservation "pj4_alu_shift" 1
  (and (eq_attr "tune" "marvell_pj4")
       (not (eq_attr "conds" "set"))
       (eq_attr "type" "alu_shift_imm,logic_shift_imm,\
                        alus_shift_imm,logics_shift_imm,\
                        alu_shift_reg,logic_shift_reg,\
                        alus_shift_reg,logics_shift_reg,\
                        extend,\
                        mov_shift,mvn_shift,mov_shift_reg,mvn_shift_reg"))
                               "pj4_is,(pj4_alu1,nothing,pj4_w1+pj4_cp)|(pj4_alu2,nothing,pj4_w2+pj4_cp)")

(define_insn_reservation "pj4_alu_shift_conds" 4
  (and (eq_attr "tune" "marvell_pj4")
       (eq_attr "conds" "set")
       (eq_attr "type" "alu_shift_imm,logic_shift_imm,alus_shift_imm,logics_shift_imm,\
                        alu_shift_reg,logic_shift_reg,alus_shift_reg,logics_shift_reg,\
                        extend,\
                        mov_shift,mvn_shift,mov_shift_reg,mvn_shift_reg"))
                               "pj4_is,(pj4_alu1,nothing,pj4_w1+pj4_cp)|(pj4_alu2,nothing,pj4_w2+pj4_cp)")

(define_bypass 2 "pj4_alu_shift,pj4_shift"
                 "pj4_ir_mul,pj4_ir_div,pj4_core_to_vfp")

(define_insn_reservation "pj4_ir_mul" 3
  (and (eq_attr "tune" "marvell_pj4")
       (ior (eq_attr "mul32" "yes")
            (eq_attr "mul64" "yes")))
                     "pj4_is,pj4_mul,nothing*2,pj4_cp")

(define_insn_reservation "pj4_ir_div" 20
  (and (eq_attr "tune" "marvell_pj4") 
       (eq_attr "type" "udiv,sdiv")) "pj4_is,pj4_div*19,pj4_cp")

;; Branches and calls.

(define_insn_reservation "pj4_branches" 0
  (and (eq_attr "tune" "marvell_pj4") (eq_attr "type" "branch")) "pj4_is")

(define_insn_reservation "pj4_calls" 32
  (and (eq_attr "tune" "marvell_pj4") (eq_attr "type" "call")) "pj4_is")

;; Load/store instructions

(define_insn_reservation "pj4_ldr"  3
  (and (eq_attr "tune" "marvell_pj4")
       (eq_attr "type" "load_byte,load1"))
                       "pj4_is,pj4_alu1,nothing*2,pj4_cp")

(define_insn_reservation "pj4_ldrd" 3
  (and (eq_attr "tune" "marvell_pj4")
       (eq_attr "type" "load2"))
                       "pj4_is,pj4_alu1,nothing*2,pj4_cpb")

(define_insn_reservation "pj4_str"  1
  (and (eq_attr "tune" "marvell_pj4")
       (eq_attr "type" "store1"))
                       "pj4_is,pj4_alu1,nothing*2,pj4_cp")

(define_insn_reservation "pj4_strd" 1
  (and (eq_attr "tune" "marvell_pj4")
       (eq_attr "type" "store2"))
                       "pj4_is,pj4_alu1,nothing*2,pj4_cpb")

(define_insn_reservation "pj4_ldm" 4
  (and (eq_attr "tune" "marvell_pj4")
       (eq_attr "type" "load3,load4")) "pj4_isb,pj4_isb+pj4_alu1,pj4_alu1,nothing,pj4_cp,pj4_cp")

(define_insn_reservation "pj4_stm" 2
  (and (eq_attr "tune" "marvell_pj4")
       (eq_attr "type" "store3,store4")) "pj4_isb,pj4_isb+pj4_alu1,pj4_alu1,nothing,pj4_cp,pj4_cp")

;; Loads forward at WR-stage to ALU pipes
(define_bypass 2 "pj4_ldr,pj4_ldrd" "pj4_alu")
(define_bypass 2 "pj4_ldr,pj4_ldrd" "pj4_alu_shift" "arm_no_early_alu_shift_dep")

(define_bypass 4 "pj4_ldr,pj4_ldrd" "pj4_ir_mul,pj4_ir_div,pj4_core_to_vfp")
(define_bypass 5 "pj4_ldm" "pj4_ir_mul,pj4_ir_div,pj4_core_to_vfp")

;; Loads to stores can back-to-back forward
(define_bypass 1 "pj4_ldr,pj4_ldrd" "pj4_str,pj4_strd" "arm_no_early_store_addr_dep")

;; PJ4 VFP floating point unit
(define_automaton "pj4_vfp")

(define_cpu_unit "vissue" "pj4_vfp")
(define_cpu_unit "vadd"   "pj4_vfp")
(define_cpu_unit "vmul"   "pj4_vfp")
(define_cpu_unit "vdiv"   "pj4_vfp")
(define_cpu_unit "vfast"  "pj4_vfp")

(define_insn_reservation "pj4_vfp_add"  5
  (and (eq_attr "tune" "marvell_pj4")
       (eq_attr "type" "fadds,faddd")) "pj4_is,nothing*2,vissue,vadd,nothing*3")

(define_insn_reservation "pj4_vfp_mul"  6
  (and (eq_attr "tune" "marvell_pj4")
       (eq_attr "type" "fmuls,fmuld")) "pj4_is,nothing*2,vissue,vmul,nothing*4")

(define_insn_reservation "pj4_vfp_divs" 20
  (and (eq_attr "tune" "marvell_pj4")
       (eq_attr "type" "fdivs, fsqrts"))       "pj4_is,nothing*2,vissue,vdiv*18,nothing")

(define_insn_reservation "pj4_vfp_divd" 34
  (and (eq_attr "tune" "marvell_pj4")
       (eq_attr "type" "fdivd, fsqrtd"))       "pj4_is,nothing*2,vissue,vdiv*32,nothing")

(define_insn_reservation "pj4_vfp_mac"  9
  (and (eq_attr "tune" "marvell_pj4")
       (eq_attr "type" "fmacs,fmacd"))
                       "pj4_is,nothing*2,vissue,vmul,nothing*3,vadd,nothing*3")

(define_bypass 5 "pj4_vfp_mac" "pj4_vfp_mac" "arm_no_early_mul_dep")

(define_insn_reservation "pj4_vfp_cpy"  4
  (and (eq_attr "tune" "marvell_pj4")
       (eq_attr "type" "fmov,ffariths,ffarithd,fconsts,fconstd,\
                        fcmps,fcmpd,f_cvt,f_cvtf2i,f_cvti2f"))
"pj4_is,nothing*2,vissue,vfast,nothing*2")

;; Enlarge latency, and wish that more nondependent insns are
;; scheduled immediately after VFP load.
(define_insn_reservation "pj4_vfp_load" 4
  (and (eq_attr "tune" "marvell_pj4")
       (eq_attr "type" "f_loads,f_loadd"))    "pj4_isb,pj4_alu1,nothing,vissue,pj4_cp")

(define_insn_reservation "pj4_vfp_store" 1
  (and (eq_attr "tune" "marvell_pj4")
       (eq_attr "type" "f_stores,f_stored"))  "pj4_isb,pj4_alu1,nothing,vissue,pj4_cp")

(define_insn_reservation "pj4_vfp_to_core" 7
  (and (eq_attr "tune" "marvell_pj4")
       (eq_attr "type" "f_mrc,f_mrrc,f_flag")) "pj4_isb,nothing,nothing,vissue,vfast,nothing*2")

(define_insn_reservation "pj4_core_to_vfp" 2
  (and (eq_attr "tune" "marvell_pj4")
       (eq_attr "type" "f_mcr,f_mcrr")) "pj4_isb,pj4_alu1,pj4_w1,vissue,pj4_cp")

