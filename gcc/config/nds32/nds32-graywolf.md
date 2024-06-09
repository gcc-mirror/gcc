;; Pipeline descriptions of Andes NDS32 cpu for GNU compiler
;; Copyright (C) 2012-2024 Free Software Foundation, Inc.
;; Contributed by Andes Technology Corporation.
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

;; ------------------------------------------------------------------------
;; Define Graywolf pipeline settings.
;; ------------------------------------------------------------------------

(define_automaton "nds32_graywolf_machine")

(define_cpu_unit "gw_ii_0" "nds32_graywolf_machine")
(define_cpu_unit "gw_ii_1" "nds32_graywolf_machine")
(define_cpu_unit "gw_ex_p0" "nds32_graywolf_machine")
(define_cpu_unit "gw_mm_p0" "nds32_graywolf_machine")
(define_cpu_unit "gw_wb_p0" "nds32_graywolf_machine")
(define_cpu_unit "gw_ex_p1" "nds32_graywolf_machine")
(define_cpu_unit "gw_mm_p1" "nds32_graywolf_machine")
(define_cpu_unit "gw_wb_p1" "nds32_graywolf_machine")
(define_cpu_unit "gw_iq_p2" "nds32_graywolf_machine")
(define_cpu_unit "gw_rf_p2" "nds32_graywolf_machine")
(define_cpu_unit "gw_e1_p2" "nds32_graywolf_machine")
(define_cpu_unit "gw_e2_p2" "nds32_graywolf_machine")
(define_cpu_unit "gw_e3_p2" "nds32_graywolf_machine")
(define_cpu_unit "gw_e4_p2" "nds32_graywolf_machine")

(define_reservation "gw_ii" "gw_ii_0 | gw_ii_1")
(define_reservation "gw_ex" "gw_ex_p0 | gw_ex_p1")
(define_reservation "gw_mm" "gw_mm_p0 | gw_mm_p1")
(define_reservation "gw_wb" "gw_wb_p0 | gw_wb_p1")

(define_reservation "gw_ii_all" "gw_ii_0 + gw_ii_1")

(define_insn_reservation "nds_gw_unknown" 1
  (and (eq_attr "type" "unknown")
       (eq_attr "pipeline_model" "graywolf"))
  "gw_ii, gw_ex, gw_mm, gw_wb")

(define_insn_reservation "nds_gw_misc" 1
  (and (eq_attr "type" "misc")
       (eq_attr "pipeline_model" "graywolf"))
  "gw_ii, gw_ex, gw_mm, gw_wb")

(define_insn_reservation "nds_gw_mmu" 1
  (and (eq_attr "type" "mmu")
       (eq_attr "pipeline_model" "graywolf"))
  "gw_ii, gw_ex, gw_mm, gw_wb")

(define_insn_reservation "nds_gw_alu" 1
  (and (and (eq_attr "type" "alu")
            (match_test "!nds32::movd44_insn_p (insn)"))
       (eq_attr "pipeline_model" "graywolf"))
  "gw_ii, gw_ex, gw_mm, gw_wb")

(define_insn_reservation "nds_gw_movd44" 1
  (and (and (eq_attr "type" "alu")
            (match_test "nds32::movd44_insn_p (insn)"))
       (eq_attr "pipeline_model" "graywolf"))
  "gw_ii_1, gw_ex, gw_mm, gw_wb")

(define_insn_reservation "nds_gw_alu_shift" 1
  (and (eq_attr "type" "alu_shift")
       (eq_attr "pipeline_model" "graywolf"))
  "gw_ii, gw_ex*2, gw_mm, gw_wb")

(define_insn_reservation "nds_gw_pbsad" 1
  (and (eq_attr "type" "pbsad")
       (eq_attr "pipeline_model" "graywolf"))
  "gw_ii, gw_ex*3, gw_mm, gw_wb")

(define_insn_reservation "nds_gw_pbsada" 1
  (and (eq_attr "type" "pbsada")
       (eq_attr "pipeline_model" "graywolf"))
  "gw_ii, gw_ex*3, gw_mm, gw_wb")

(define_insn_reservation "nds_gw_load" 1
  (and (and (eq_attr "type" "load")
            (match_test "!nds32::post_update_insn_p (insn)"))
       (eq_attr "pipeline_model" "graywolf"))
  "gw_ii_1, gw_ex_p1, gw_mm_p1, gw_wb_p1")

(define_insn_reservation "nds_gw_load_2w" 1
  (and (and (eq_attr "type" "load")
            (match_test "nds32::post_update_insn_p (insn)"))
       (eq_attr "pipeline_model" "graywolf"))
  "gw_ii_all, gw_ex_p1, gw_mm_p1, gw_wb_p1")

(define_insn_reservation "nds_gw_store" 1
  (and (and (eq_attr "type" "store")
            (match_test "!nds32::store_offset_reg_p (insn)"))
       (eq_attr "pipeline_model" "graywolf"))
  "gw_ii_1, gw_ex_p1, gw_mm_p1, gw_wb_p1")

(define_insn_reservation "nds_gw_store_3r" 1
  (and (and (eq_attr "type" "store")
            (match_test "nds32::store_offset_reg_p (insn)"))
       (eq_attr "pipeline_model" "graywolf"))
  "gw_ii_all, gw_ex_p1, gw_mm_p1, gw_wb_p1")

(define_insn_reservation "nds_gw_load_multiple_1" 1
  (and (and (eq_attr "type" "load_multiple")
            (eq_attr "combo" "1"))
       (eq_attr "pipeline_model" "graywolf"))
  "gw_ii_1, gw_ex_p1, gw_mm_p1, gw_wb_p1")

(define_insn_reservation "nds_gw_load_multiple_2" 1
  (and (and (eq_attr "type" "load_multiple")
            (eq_attr "combo" "2"))
       (eq_attr "pipeline_model" "graywolf"))
  "gw_ii_1, gw_ex_p1*2, gw_mm_p1, gw_wb_p1")

(define_insn_reservation "nds_gw_load_multiple_3" 1
  (and (and (eq_attr "type" "load_multiple")
            (eq_attr "combo" "3"))
       (eq_attr "pipeline_model" "graywolf"))
  "gw_ii_1, gw_ex_p1*3, gw_mm_p1, gw_wb_p1")

(define_insn_reservation "nds_gw_load_multiple_4" 1
  (and (and (eq_attr "type" "load_multiple")
            (eq_attr "combo" "4"))
       (eq_attr "pipeline_model" "graywolf"))
  "gw_ii_1, gw_ex_p1*4, gw_mm_p1, gw_wb_p1")

(define_insn_reservation "nds_gw_load_multiple_5" 1
  (and (and (eq_attr "type" "load_multiple")
            (eq_attr "combo" "5"))
       (eq_attr "pipeline_model" "graywolf"))
  "gw_ii_1, gw_ex_p1*4, gw_mm_p1, gw_wb_p1")

(define_insn_reservation "nds_gw_load_multiple_6" 1
  (and (and (eq_attr "type" "load_multiple")
            (eq_attr "combo" "6"))
       (eq_attr "pipeline_model" "graywolf"))
  "gw_ii_1, gw_ex_p1*4, gw_mm_p1, gw_wb_p1")

(define_insn_reservation "nds_gw_load_multiple_7" 1
  (and (and (eq_attr "type" "load_multiple")
            (eq_attr "combo" "7"))
       (eq_attr "pipeline_model" "graywolf"))
  "gw_ii_1, gw_ex_p1*4, gw_mm_p1, gw_wb_p1")

(define_insn_reservation "nds_gw_load_multiple_8" 1
  (and (and (eq_attr "type" "load_multiple")
            (eq_attr "combo" "8"))
       (eq_attr "pipeline_model" "graywolf"))
  "gw_ii_1, gw_ex_p1*4, gw_mm_p1, gw_wb_p1")

(define_insn_reservation "nds_gw_load_multiple_12" 1
  (and (and (eq_attr "type" "load_multiple")
            (eq_attr "combo" "12"))
       (eq_attr "pipeline_model" "graywolf"))
  "gw_ii_1, gw_ex_p1*4, gw_mm_p1, gw_wb_p1")

(define_insn_reservation "nds_gw_store_multiple_1" 1
  (and (and (eq_attr "type" "store_multiple")
            (eq_attr "combo" "1"))
       (eq_attr "pipeline_model" "graywolf"))
  "gw_ii_1, gw_ex_p1, gw_mm_p1, gw_wb_p1")

(define_insn_reservation "nds_gw_store_multiple_2" 1
  (and (and (eq_attr "type" "store_multiple")
            (eq_attr "combo" "2"))
       (eq_attr "pipeline_model" "graywolf"))
  "gw_ii_1, gw_ex_p1*2, gw_mm_p1, gw_wb_p1")

(define_insn_reservation "nds_gw_store_multiple_3" 1
  (and (and (eq_attr "type" "store_multiple")
            (eq_attr "combo" "3"))
       (eq_attr "pipeline_model" "graywolf"))
  "gw_ii_1, gw_ex_p1*3, gw_mm_p1, gw_wb_p1")

(define_insn_reservation "nds_gw_store_multiple_4" 1
  (and (and (eq_attr "type" "store_multiple")
            (eq_attr "combo" "4"))
       (eq_attr "pipeline_model" "graywolf"))
  "gw_ii_1, gw_ex_p1*4, gw_mm_p1, gw_wb_p1")

(define_insn_reservation "nds_gw_store_multiple_5" 1
  (and (and (eq_attr "type" "store_multiple")
            (eq_attr "combo" "5"))
       (eq_attr "pipeline_model" "graywolf"))
  "gw_ii_1, gw_ex_p1*4, gw_mm_p1, gw_wb_p1")

(define_insn_reservation "nds_gw_store_multiple_6" 1
  (and (and (eq_attr "type" "store_multiple")
            (eq_attr "combo" "6"))
       (eq_attr "pipeline_model" "graywolf"))
  "gw_ii_1, gw_ex_p1*4, gw_mm_p1, gw_wb_p1")

(define_insn_reservation "nds_gw_store_multiple_7" 1
  (and (and (eq_attr "type" "store_multiple")
            (eq_attr "combo" "7"))
       (eq_attr "pipeline_model" "graywolf"))
  "gw_ii_1, gw_ex_p1*4, gw_mm_p1, gw_wb_p1")

(define_insn_reservation "nds_gw_store_multiple_8" 1
  (and (and (eq_attr "type" "store_multiple")
            (eq_attr "combo" "8"))
       (eq_attr "pipeline_model" "graywolf"))
  "gw_ii_1, gw_ex_p1*4, gw_mm_p1, gw_wb_p1")

(define_insn_reservation "nds_gw_store_multiple_12" 1
  (and (and (eq_attr "type" "store_multiple")
            (eq_attr "combo" "12"))
       (eq_attr "pipeline_model" "graywolf"))
  "gw_ii_1, gw_ex_p1*4, gw_mm_p1, gw_wb_p1")

(define_insn_reservation "nds_gw_mul_fast1" 1
  (and (match_test "nds32_mul_config == MUL_TYPE_FAST_1")
       (and (eq_attr "type" "mul")
       (eq_attr "pipeline_model" "graywolf")))
  "gw_ii_0, gw_ex_p0, gw_mm_p0, gw_wb_p0")

(define_insn_reservation "nds_gw_mul_fast2" 1
  (and (match_test "nds32_mul_config == MUL_TYPE_FAST_2")
       (and (eq_attr "type" "mul")
       (eq_attr "pipeline_model" "graywolf")))
  "gw_ii_0, gw_ex_p0*2, gw_mm_p0, gw_wb_p0")

(define_insn_reservation "nds_gw_mul_slow" 1
  (and (match_test "nds32_mul_config == MUL_TYPE_SLOW")
       (and (eq_attr "type" "mul")
       (eq_attr "pipeline_model" "graywolf")))
  "gw_ii_0, gw_ex_p0*4, gw_mm_p0, gw_wb_p0")

(define_insn_reservation "nds_gw_mac_fast1" 1
  (and (match_test "nds32_mul_config == MUL_TYPE_FAST_1")
       (and (eq_attr "type" "mac")
       (eq_attr "pipeline_model" "graywolf")))
  "gw_ii_all, gw_ex_p0, gw_mm_p0, gw_wb_p0")

(define_insn_reservation "nds_gw_mac_fast2" 1
  (and (match_test "nds32_mul_config == MUL_TYPE_FAST_2")
       (and (eq_attr "type" "mac")
       (eq_attr "pipeline_model" "graywolf")))
  "gw_ii_all, gw_ex_p0*2, gw_mm_p0, gw_wb_p0")

(define_insn_reservation "nds_gw_mac_slow" 1
  (and (match_test "nds32_mul_config == MUL_TYPE_SLOW")
       (and (eq_attr "type" "mac")
       (eq_attr "pipeline_model" "graywolf")))
  "gw_ii_all, gw_ex_p0*4, gw_mm_p0, gw_wb_p0")

(define_insn_reservation "nds_gw_div" 1
  (and (and (eq_attr "type" "div")
            (match_test "!nds32::divmod_p (insn)"))
       (eq_attr "pipeline_model" "graywolf"))
  "gw_ii_0, gw_ex_p0*4, gw_mm_p0, gw_wb_p0")

(define_insn_reservation "nds_gw_div_2w" 1
  (and (and (eq_attr "type" "div")
            (match_test "nds32::divmod_p (insn)"))
       (eq_attr "pipeline_model" "graywolf"))
  "gw_ii_all, gw_ex_p0*4, gw_mm_p0, gw_wb_p0")

(define_insn_reservation "nds_gw_branch" 1
  (and (eq_attr "type" "branch")
       (eq_attr "pipeline_model" "graywolf"))
  "gw_ii_0, gw_ex_p0, gw_mm_p0, gw_wb_p0")

(define_insn_reservation "nds_gw_dsp_alu" 1
  (and (eq_attr "type" "dalu")
       (eq_attr "pipeline_model" "graywolf"))
  "gw_ii, gw_ex, gw_mm, gw_wb")

(define_insn_reservation "nds_gw_dsp_alu64" 1
  (and (eq_attr "type" "dalu64")
       (eq_attr "pipeline_model" "graywolf"))
  "gw_ii_all, gw_ex_p0, gw_mm_p0, gw_wb_p0")

(define_insn_reservation "nds_gw_dsp_alu_round" 1
  (and (eq_attr "type" "daluround")
       (eq_attr "pipeline_model" "graywolf"))
  "gw_ii_0, gw_ex_p0, gw_mm_p0, gw_wb_p0")

(define_insn_reservation "nds_gw_dsp_cmp" 1
  (and (eq_attr "type" "dcmp")
       (eq_attr "pipeline_model" "graywolf"))
  "gw_ii_0, gw_ex_p0, gw_mm_p0, gw_wb_p0")

(define_insn_reservation "nds_gw_dsp_clip" 1
  (and (eq_attr "type" "dclip")
       (eq_attr "pipeline_model" "graywolf"))
  "gw_ii_0, gw_ex_p0, gw_mm_p0, gw_wb_p0")

(define_insn_reservation "nds_gw_dsp_mul" 1
  (and (eq_attr "type" "dmul")
       (eq_attr "pipeline_model" "graywolf"))
  "gw_ii_0, gw_ex_p0, gw_mm_p0, gw_wb_p0")

(define_insn_reservation "nds_gw_dsp_mac" 1
  (and (eq_attr "type" "dmac")
       (eq_attr "pipeline_model" "graywolf"))
  "gw_ii_all, gw_ex_p0, gw_mm_p0, gw_wb_p0")

(define_insn_reservation "nds_gw_dsp_insb" 1
  (and (eq_attr "type" "dinsb")
       (eq_attr "pipeline_model" "graywolf"))
  "gw_ii_0, gw_ex_p0, gw_mm_p0, gw_wb_p0")

(define_insn_reservation "nds_gw_dsp_pack" 1
  (and (eq_attr "type" "dpack")
       (eq_attr "pipeline_model" "graywolf"))
  "gw_ii_0, gw_ex_p0, gw_mm_p0, gw_wb_p0")

(define_insn_reservation "nds_gw_dsp_bpick" 1
  (and (eq_attr "type" "dbpick")
       (eq_attr "pipeline_model" "graywolf"))
  "gw_ii_0, gw_ex_p0, gw_mm_p0, gw_wb_p0")

(define_insn_reservation "nds_gw_dsp_wext" 1
  (and (eq_attr "type" "dwext")
       (eq_attr "pipeline_model" "graywolf"))
  "gw_ii_all, gw_ex_p0, gw_mm_p0, gw_wb_p0")

(define_insn_reservation "nds_gw_fpu_alu" 4
  (and (eq_attr "type" "falu")
       (eq_attr "pipeline_model" "graywolf"))
  "gw_ii, gw_iq_p2, gw_rf_p2, gw_e1_p2, gw_e2_p2, gw_e3_p2, gw_e4_p2")

(define_insn_reservation "nds_gw_fpu_muls" 4
  (and (eq_attr "type" "fmuls")
       (eq_attr "pipeline_model" "graywolf"))
  "gw_ii, gw_iq_p2, gw_rf_p2, gw_e1_p2, gw_e2_p2, gw_e3_p2, gw_e4_p2")

(define_insn_reservation "nds_gw_fpu_muld" 4
  (and (eq_attr "type" "fmuld")
       (eq_attr "pipeline_model" "graywolf"))
  "gw_ii, gw_iq_p2, gw_rf_p2, gw_e1_p2, gw_e2_p2*2, gw_e3_p2, gw_e4_p2")

(define_insn_reservation "nds_gw_fpu_macs" 4
  (and (eq_attr "type" "fmacs")
       (eq_attr "pipeline_model" "graywolf"))
  "gw_ii, gw_iq_p2, gw_rf_p2, gw_e1_p2, gw_e2_p2*3, gw_e3_p2, gw_e4_p2")

(define_insn_reservation "nds_gw_fpu_macd" 4
  (and (eq_attr "type" "fmacd")
       (eq_attr "pipeline_model" "graywolf"))
  "gw_ii, gw_iq_p2, gw_rf_p2, gw_e1_p2, gw_e2_p2*4, gw_e3_p2, gw_e4_p2")

(define_insn_reservation "nds_gw_fpu_divs" 4
  (and (ior (eq_attr "type" "fdivs")
	    (eq_attr "type" "fsqrts"))
       (eq_attr "pipeline_model" "graywolf"))
  "gw_ii, gw_iq_p2, gw_rf_p2, gw_e1_p2, gw_e2_p2*14, gw_e3_p2, gw_e4_p2")

(define_insn_reservation "nds_gw_fpu_divd" 4
  (and (ior (eq_attr "type" "fdivd")
	    (eq_attr "type" "fsqrtd"))
       (eq_attr "pipeline_model" "graywolf"))
  "gw_ii, gw_iq_p2, gw_rf_p2, gw_e1_p2, gw_e2_p2*28, gw_e3_p2, gw_e4_p2")

(define_insn_reservation "nds_gw_fpu_fast_alu" 2
  (and (ior (eq_attr "type" "fcmp")
	    (ior (eq_attr "type" "fabs")
		 (ior (eq_attr "type" "fcpy")
		      (eq_attr "type" "fcmov"))))
       (eq_attr "pipeline_model" "graywolf"))
  "gw_ii, gw_iq_p2, gw_rf_p2, gw_e1_p2, gw_e2_p2, gw_e3_p2, gw_e4_p2")

(define_insn_reservation "nds_gw_fpu_fmtsr" 1
  (and (eq_attr "type" "fmtsr")
       (eq_attr "pipeline_model" "graywolf"))
  "gw_ii, gw_iq_p2, gw_rf_p2, gw_e1_p2, gw_e2_p2, gw_e3_p2, gw_e4_p2")

(define_insn_reservation "nds_gw_fpu_fmtdr" 1
  (and (eq_attr "type" "fmtdr")
       (eq_attr "pipeline_model" "graywolf"))
  "gw_ii, gw_ii+gw_iq_p2, gw_iq_p2+gw_rf_p2, gw_rf_p2+gw_e1_p2, gw_e1_p2+gw_e2_p2, gw_e2_p2+gw_e3_p2, gw_e3_p2+gw_e4_p2, gw_e4_p2")

(define_insn_reservation "nds_gw_fpu_fmfsr" 1
  (and (eq_attr "type" "fmfsr")
       (eq_attr "pipeline_model" "graywolf"))
  "gw_ii, gw_iq_p2, gw_rf_p2, gw_e1_p2, gw_e2_p2, gw_e3_p2, gw_e4_p2")

(define_insn_reservation "nds_gw_fpu_fmfdr" 1
  (and (eq_attr "type" "fmfdr")
       (eq_attr "pipeline_model" "graywolf"))
  "gw_ii, gw_ii+gw_iq_p2, gw_iq_p2+gw_rf_p2, gw_rf_p2+gw_e1_p2, gw_e1_p2+gw_e2_p2, gw_e2_p2+gw_e3_p2, gw_e3_p2+gw_e4_p2, gw_e4_p2")

(define_insn_reservation "nds_gw_fpu_load" 3
  (and (eq_attr "type" "fload")
       (eq_attr "pipeline_model" "graywolf"))
  "gw_ii, gw_iq_p2, gw_rf_p2, gw_e1_p2, gw_e2_p2, gw_e3_p2, gw_e4_p2")

(define_insn_reservation "nds_gw_fpu_store" 1
  (and (eq_attr "type" "fstore")
       (eq_attr "pipeline_model" "graywolf"))
  "gw_ii, gw_iq_p2, gw_rf_p2, gw_e1_p2, gw_e2_p2, gw_e3_p2, gw_e4_p2")

;; FPU_ADDR_OUT -> FPU_ADDR_IN
;; Main pipeline rules don't need this because those default latency is 1.
(define_bypass 1
  "nds_gw_fpu_load, nds_gw_fpu_store"
  "nds_gw_fpu_load, nds_gw_fpu_store"
  "nds32_gw_ex_to_ex_p"
)

;; LD, MUL, MAC, DIV, DALU64, DMUL, DMAC, DALUROUND, DBPICK, DWEXT
;;   -> ALU, ALU_SHIFT_Rb, PBSAD, PBSADA_RaRb, MOVD44, MUL, MAC_RaRb, DIV, ADDR_IN, BR, MMU,
;;      DALU, DALUROUND, DMUL, DMAC_RaRb, DPACK, DINSB, DCMP, DCLIP, WEXT_O, BPICK_RaRb
(define_bypass 2
  "nds_gw_load, nds_gw_load_2w,\
   nds_gw_mul_fast1, nds_gw_mul_fast2, nds_gw_mul_slow,\
   nds_gw_mac_fast1, nds_gw_mac_fast2, nds_gw_mac_slow,\
   nds_gw_div, nds_gw_div_2w,\
   nds_gw_dsp_alu64, nds_gw_dsp_mul, nds_gw_dsp_mac,\
   nds_gw_dsp_alu_round, nds_gw_dsp_bpick, nds_gw_dsp_wext"
  "nds_gw_alu, nds_gw_movd44, nds_gw_alu_shift,\
   nds_gw_pbsad, nds_gw_pbsada,\
   nds_gw_mul_fast1, nds_gw_mul_fast2, nds_gw_mul_slow,\
   nds_gw_mac_fast1, nds_gw_mac_fast2, nds_gw_mac_slow,\
   nds_gw_branch,\
   nds_gw_div, nds_gw_div_2w,\
   nds_gw_load, nds_gw_load_2w, nds_gw_store, nds_gw_store_3r,\
   nds_gw_load_multiple_1,nds_gw_load_multiple_2, nds_gw_load_multiple_3,\
   nds_gw_load_multiple_4,nds_gw_load_multiple_5, nds_gw_load_multiple_6,\
   nds_gw_load_multiple_7,nds_gw_load_multiple_8, nds_gw_load_multiple_12,\
   nds_gw_store_multiple_1,nds_gw_store_multiple_2, nds_gw_store_multiple_3,\
   nds_gw_store_multiple_4,nds_gw_store_multiple_5, nds_gw_store_multiple_6,\
   nds_gw_store_multiple_7,nds_gw_store_multiple_8, nds_gw_store_multiple_12,\
   nds_gw_mmu,\
   nds_gw_dsp_alu, nds_gw_dsp_alu_round,\
   nds_gw_dsp_mul, nds_gw_dsp_mac, nds_gw_dsp_pack,\
   nds_gw_dsp_insb, nds_gw_dsp_cmp, nds_gw_dsp_clip,\
   nds_gw_dsp_wext, nds_gw_dsp_bpick"
  "nds32_gw_mm_to_ex_p"
)

;; LMW(N, N)
;;   -> ALU, ALU_SHIFT_Rb, PBSAD, PBSADA_RaRb, MOVD44, MUL, MAC_RaRb, DIV, ADDR_IN, BR, MMU
;;      DALU, DALUROUND, DMUL, DMAC_RaRb, DPACK, DINSB, DCMP, DCLIP, WEXT_O, BPICK_RaRb
(define_bypass 2
  "nds_gw_load_multiple_1,nds_gw_load_multiple_2, nds_gw_load_multiple_3,\
   nds_gw_load_multiple_4,nds_gw_load_multiple_5, nds_gw_load_multiple_6,\
   nds_gw_load_multiple_7,nds_gw_load_multiple_8, nds_gw_load_multiple_12"
  "nds_gw_alu, nds_gw_movd44, nds_gw_alu_shift,\
   nds_gw_pbsad, nds_gw_pbsada,\
   nds_gw_mul_fast1, nds_gw_mul_fast2, nds_gw_mul_slow,\
   nds_gw_mac_fast1, nds_gw_mac_fast2, nds_gw_mac_slow,\
   nds_gw_branch,\
   nds_gw_div, nds_gw_div_2w,\
   nds_gw_load, nds_gw_load_2w, nds_gw_store, nds_gw_store_3r,\
   nds_gw_load_multiple_1,nds_gw_load_multiple_2, nds_gw_load_multiple_3,\
   nds_gw_load_multiple_4,nds_gw_load_multiple_5, nds_gw_load_multiple_6,\
   nds_gw_load_multiple_7,nds_gw_load_multiple_8, nds_gw_load_multiple_12,\
   nds_gw_store_multiple_1,nds_gw_store_multiple_2, nds_gw_store_multiple_3,\
   nds_gw_store_multiple_4,nds_gw_store_multiple_5, nds_gw_store_multiple_6,\
   nds_gw_store_multiple_7,nds_gw_store_multiple_8, nds_gw_store_multiple_12,\
   nds_gw_mmu,\
   nds_gw_dsp_alu, nds_gw_dsp_alu_round,\
   nds_gw_dsp_mul, nds_gw_dsp_mac, nds_gw_dsp_pack,\
   nds_gw_dsp_insb, nds_gw_dsp_cmp, nds_gw_dsp_clip,\
   nds_gw_dsp_wext, nds_gw_dsp_bpick"
  "nds32_gw_last_load_to_ex_p"
)
