;; ARM Cortex-A17 pipeline description
;; Copyright (C) 2014-2017 Free Software Foundation, Inc.
;;
;; Contributed by ARM Ltd.
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


(define_automaton "cortex_a17")

(define_cpu_unit "ca17_ls0, ca17_ls1" "cortex_a17")
(define_cpu_unit "ca17_alu0, ca17_alu1" "cortex_a17")
(define_cpu_unit "ca17_mac" "cortex_a17")
(define_cpu_unit "ca17_idiv" "cortex_a17")
(define_cpu_unit "ca17_bx" "cortex_a17")

(define_reservation "ca17_alu" "(ca17_alu0|ca17_alu1)")



;; Simple Execution Unit:
;;
;; Simple ALU
(define_insn_reservation "cortex_a17_alu" 1
  (and (eq_attr "tune" "cortexa17")
       (eq_attr "type" "alu_imm,alus_imm,logic_imm,logics_imm,\
                        alu_sreg,alus_sreg,logic_reg,logics_reg,\
                        adc_imm,adcs_imm,adc_reg,adcs_reg,\
                        adr, mov_imm,mov_reg,\
                        mvn_imm,mvn_reg,extend,\
                        mrs,multiple,no_insn"))
  "ca17_alu")

(define_insn_reservation "cortex_a17_alu_shiftimm" 2
  (and (eq_attr "tune" "cortexa17")
       (eq_attr "type" "bfm,clz,rev,rbit, alu_shift_imm, alus_shift_imm,
                        logic_shift_imm,alu_dsp_reg, logics_shift_imm,shift_imm,\
                        shift_reg, mov_shift,mvn_shift"))
  "ca17_alu")


;; ALU ops with register controlled shift.
(define_insn_reservation "cortex_a17_alu_shift_reg" 2
  (and (eq_attr "tune" "cortexa17")
       (eq_attr "type" "alu_shift_reg,alus_shift_reg,\
                        logic_shift_reg,logics_shift_reg"))
  "ca17_alu0")


;; Multiply Execution Unit:

;; 32-bit multiplies
(define_insn_reservation "cortex_a17_mult32" 4
  (and (eq_attr "tune" "cortexa17")
       (eq_attr "type" "mul,muls,smmul,smmulr"))
  "ca17_alu0+ca17_mac")

(define_insn_reservation "cortex_a17_mac32" 4
  (and (eq_attr "tune" "cortexa17")
       (eq_attr "type" "mla,mlas,smmla"))
  "ca17_alu0+ca17_mac,ca17_mac")

(define_insn_reservation "cortex_a17_mac32_other" 3
  (and (eq_attr "tune" "cortexa17")
       (eq_attr "type" "smlad,smladx,smlsd,smlsdx,smuad,smuadx,smusd,smusdx"))
  "ca17_alu0+ca17_mac,ca17_mac")

;; 64-bit multiplies
(define_insn_reservation "cortex_a17_mac64" 4
  (and (eq_attr "tune" "cortexa17")
       (eq_attr "type" "smlal,smlals,umaal,umlal,umlals"))
  "ca17_alu0+ca17_mac,ca17_mac")

(define_insn_reservation "cortex_a17_mac64_other" 3
  (and (eq_attr "tune" "cortexa17")
       (eq_attr "type" "smlald,smlalxy,smlsld"))
  "ca17_alu0+ca17_mac,ca17_mac")

(define_insn_reservation "cortex_a17_mult64" 4
  (and (eq_attr "tune" "cortexa17")
       (eq_attr "type" "smull,smulls,umull,umulls"))
  "ca17_alu0+ca17_mac,ca17_mac")


(define_bypass 2 "cortex_a17_mult*, cortex_a17_mac*"
                 "cortex_a17_mult*, cortex_a17_mac*"
                 "arm_mac_accumulator_is_result")

;; Integer divide
(define_insn_reservation "cortex_a17_udiv" 19
  (and (eq_attr "tune" "cortexa17")
       (eq_attr "type" "udiv"))
  "ca17_alu1+ca17_idiv*10")

(define_insn_reservation "cortex_a17_sdiv" 20
  (and (eq_attr "tune" "cortexa17")
       (eq_attr "type" "sdiv"))
  "ca17_alu1+ca17_idiv*11")



;; Branch execution Unit
;;
;; Branches take one issue slot.
;; No latency as there is no result
(define_insn_reservation "cortex_a17_branch" 0
  (and (eq_attr "tune" "cortexa17")
       (eq_attr "type" "branch"))
  "ca17_bx")

;; Load-store execution Unit
;;
;; Loads of up to two words.
(define_insn_reservation "cortex_a17_load1" 4
  (and (eq_attr "tune" "cortexa17")
       (eq_attr "type" "load_byte,load1,load2"))
  "ca17_ls0|ca17_ls1")

;; Loads of three words.
(define_insn_reservation "cortex_a17_load3" 4
  (and (eq_attr "tune" "cortexa17")
       (eq_attr "type" "load3"))
  "ca17_ls0+ca17_ls1")

;; Loads of four words.
(define_insn_reservation "cortex_a17_load4" 4
  (and (eq_attr "tune" "cortexa17")
       (eq_attr "type" "load4"))
  "ca17_ls0+ca17_ls1")

;; Stores of up to two words.
(define_insn_reservation "cortex_a17_store1" 0
  (and (eq_attr "tune" "cortexa17")
       (eq_attr "type" "store1,store2"))
  "ca17_ls0|ca17_ls1")

;; Stores of three words
(define_insn_reservation "cortex_a17_store3" 0
  (and (eq_attr "tune" "cortexa17")
       (eq_attr "type" "store3"))
  "ca17_ls0+ca17_ls1")

;; Stores of four words.
(define_insn_reservation "cortex_a17_store4" 0
  (and (eq_attr "tune" "cortexa17")
       (eq_attr "type" "store4"))
  "ca17_ls0+ca17_ls1")

(define_insn_reservation "cortex_a17_call" 0
  (and (eq_attr "tune" "cortexa17")
       (eq_attr "type" "call"))
  "ca17_bx")


(include "../arm/cortex-a17-neon.md")
