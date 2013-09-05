;; ARM Cortex-M4 FPU pipeline description
;; Copyright (C) 2010-2013 Free Software Foundation, Inc.
;; Contributed by CodeSourcery.
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

;; Use two artificial units to model FPU.
(define_cpu_unit "cortex_m4_v_a" "cortex_m4")
(define_cpu_unit "cortex_m4_v_b" "cortex_m4")

(define_reservation "cortex_m4_v" "cortex_m4_v_a+cortex_m4_v_b")
(define_reservation "cortex_m4_ex_v" "cortex_m4_ex+cortex_m4_v")
(define_reservation "cortex_m4_exa_va" "cortex_m4_a+cortex_m4_v_a")
(define_reservation "cortex_m4_exb_vb" "cortex_m4_b+cortex_m4_v_b")

;; Integer instructions following VDIV or VSQRT complete out-of-order.
(define_insn_reservation "cortex_m4_fdivs" 15
  (and (eq_attr "tune" "cortexm4")
       (eq_attr "type" "fdivs"))
  "cortex_m4_ex_v,cortex_m4_v*13")

(define_insn_reservation "cortex_m4_vmov_1" 1
  (and (eq_attr "tune" "cortexm4")
       (eq_attr "type" "fcpys,fconsts"))
  "cortex_m4_ex_v")

(define_insn_reservation "cortex_m4_vmov_2" 2
  (and (eq_attr "tune" "cortexm4")
       (eq_attr "type" "f_mrc,f_mrrc,f_mcr,f_mcrr"))
  "cortex_m4_ex_v*2")

(define_insn_reservation "cortex_m4_fmuls" 2
  (and (eq_attr "tune" "cortexm4")
       (eq_attr "type" "fmuls"))
  "cortex_m4_ex_v")

;; Integer instructions following multiply-accumulate instructions
;; complete out-of-order.
(define_insn_reservation "cortex_m4_fmacs" 4
  (and (eq_attr "tune" "cortexm4")
       (eq_attr "type" "fmacs,ffmas"))
  "cortex_m4_ex_v,cortex_m4_v*2")

(define_insn_reservation "cortex_m4_ffariths" 1
  (and (eq_attr "tune" "cortexm4")
       (eq_attr "type" "ffariths"))
  "cortex_m4_ex_v")

(define_insn_reservation "cortex_m4_fadds" 2
  (and (eq_attr "tune" "cortexm4")
       (eq_attr "type" "fadds"))
  "cortex_m4_ex_v")

(define_insn_reservation "cortex_m4_fcmps" 1
  (and (eq_attr "tune" "cortexm4")
       (eq_attr "type" "fcmps"))
  "cortex_m4_ex_v")

(define_insn_reservation "cortex_m4_f_flag" 1
  (and (eq_attr "tune" "cortexm4")
       (eq_attr "type" "f_flag"))
  "cortex_m4_ex_v")

(define_insn_reservation "cortex_m4_f_cvt" 2
  (and (eq_attr "tune" "cortexm4")
       (eq_attr "type" "f_cvt"))
  "cortex_m4_ex_v")

(define_insn_reservation "cortex_m4_f_load" 2
  (and (eq_attr "tune" "cortexm4")
       (eq_attr "type" "f_loads"))
  "cortex_m4_exa_va,cortex_m4_exb_vb")

(define_insn_reservation "cortex_m4_f_store" 1
  (and (eq_attr "tune" "cortexm4")
       (eq_attr "type" "f_stores"))
  "cortex_m4_exa_va")

(define_insn_reservation "cortex_m4_f_loadd" 3
  (and (eq_attr "tune" "cortexm4")
       (eq_attr "type" "f_loadd"))
  "cortex_m4_ex_v*3")

(define_insn_reservation "cortex_m4_f_stored" 3
  (and (eq_attr "tune" "cortexm4")
       (eq_attr "type" "f_stored"))
  "cortex_m4_ex_v*3")

;; MAC instructions consume their addend one cycle later. If the result
;; of an arithmetic instruction is consumed as the addend of the following
;; MAC instruction, the latency can be decreased by one.

(define_bypass 1 "cortex_m4_fadds,cortex_m4_fmuls,cortex_m4_f_cvt"
		 "cortex_m4_fmacs"
		 "arm_no_early_mul_dep")

(define_bypass 3 "cortex_m4_fmacs"
		 "cortex_m4_fmacs"
		 "arm_no_early_mul_dep")

(define_bypass 14 "cortex_m4_fdivs"
		  "cortex_m4_fmacs"
		  "arm_no_early_mul_dep")
