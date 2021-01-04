;; Marvell WMMX2 pipeline description
;; Copyright (C) 2011-2021 Free Software Foundation, Inc.
;; Written by Marvell, Inc.

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


(define_automaton "marvell_f_iwmmxt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Pipelines
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; This is a 7-stage pipelines:
;;
;;    MD | MI | ME1 | ME2 | ME3 | ME4 | MW
;;
;; There are various bypasses modelled to a greater or lesser extent.
;;
;; Latencies in this file correspond to the number of cycles after
;; the issue stage that it takes for the result of the instruction to
;; be computed, or for its side-effects to occur.

(define_cpu_unit "mf_iwmmxt_MD" "marvell_f_iwmmxt")
(define_cpu_unit "mf_iwmmxt_MI" "marvell_f_iwmmxt")
(define_cpu_unit "mf_iwmmxt_ME1" "marvell_f_iwmmxt")
(define_cpu_unit "mf_iwmmxt_ME2" "marvell_f_iwmmxt")
(define_cpu_unit "mf_iwmmxt_ME3" "marvell_f_iwmmxt")
(define_cpu_unit "mf_iwmmxt_ME4" "marvell_f_iwmmxt")
(define_cpu_unit "mf_iwmmxt_MW" "marvell_f_iwmmxt")

(define_reservation "mf_iwmmxt_ME"
      "mf_iwmmxt_ME1,mf_iwmmxt_ME2,mf_iwmmxt_ME3,mf_iwmmxt_ME4"
)

(define_reservation "mf_iwmmxt_pipeline"
      "mf_iwmmxt_MD, mf_iwmmxt_MI, mf_iwmmxt_ME, mf_iwmmxt_MW"
)

;; An attribute to indicate whether our reservations are applicable.
(define_attr "marvell_f_iwmmxt" "yes,no"
  (const (if_then_else (symbol_ref "arm_arch_iwmmxt")
                       (const_string "yes") (const_string "no"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; instruction classes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; An attribute appended to instructions for classification

(define_attr "wmmxt_shift" "yes,no"
  (if_then_else (eq_attr "type" "wmmx_wror, wmmx_wsll, wmmx_wsra, wmmx_wsrl")
		(const_string "yes") (const_string "no"))
)

(define_attr "wmmxt_pack" "yes,no"
  (if_then_else (eq_attr "type" "wmmx_waligni, wmmx_walignr, wmmx_wmerge,\
                                 wmmx_wpack, wmmx_wshufh, wmmx_wunpckeh,\
                                 wmmx_wunpckih, wmmx_wunpckel, wmmx_wunpckil")
		(const_string "yes") (const_string "no"))
)

(define_attr "wmmxt_mult_c1" "yes,no"
  (if_then_else (eq_attr "type" "wmmx_wmac, wmmx_wmadd, wmmx_wmiaxy,\
                                 wmmx_wmiawxy, wmmx_wmulw, wmmx_wqmiaxy,\
                                 wmmx_wqmulwm")
		(const_string "yes") (const_string "no"))
)

(define_attr "wmmxt_mult_c2" "yes,no"
  (if_then_else (eq_attr "type" "wmmx_wmul, wmmx_wqmulm")
		(const_string "yes") (const_string "no"))
)

(define_attr "wmmxt_alu_c1" "yes,no"
  (if_then_else (eq_attr "type" "wmmx_wabs, wmmx_wabsdiff, wmmx_wand,\
                                 wmmx_wandn, wmmx_wmov, wmmx_wor, wmmx_wxor")
	        (const_string "yes") (const_string "no"))
)

(define_attr "wmmxt_alu_c2" "yes,no"
  (if_then_else (eq_attr "type" "wmmx_wacc, wmmx_wadd, wmmx_waddsubhx,\
                                 wmmx_wavg2, wmmx_wavg4, wmmx_wcmpeq,\
                                 wmmx_wcmpgt, wmmx_wmax, wmmx_wmin,\
                                 wmmx_wsub, wmmx_waddbhus, wmmx_wsubaddhx")
		(const_string "yes") (const_string "no"))
)

(define_attr "wmmxt_alu_c3" "yes,no"
  (if_then_else (eq_attr "type" "wmmx_wsad")
	        (const_string "yes") (const_string "no"))
)

(define_attr "wmmxt_transfer_c1" "yes,no"
  (if_then_else (eq_attr "type" "wmmx_tbcst, wmmx_tinsr,\
                                 wmmx_tmcr, wmmx_tmcrr")
                (const_string "yes") (const_string "no"))
)

(define_attr "wmmxt_transfer_c2" "yes,no"
  (if_then_else (eq_attr "type" "wmmx_textrm, wmmx_tmovmsk,\
                                 wmmx_tmrc, wmmx_tmrrc")
	        (const_string "yes") (const_string "no"))
)

(define_attr "wmmxt_transfer_c3" "yes,no"
  (if_then_else (eq_attr "type" "wmmx_tmia, wmmx_tmiaph, wmmx_tmiaxy")
	        (const_string "yes") (const_string "no"))
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Main description
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define_insn_reservation "marvell_f_iwmmxt_alu_c1" 1
  (and (eq_attr "marvell_f_iwmmxt" "yes")
       (eq_attr "wmmxt_alu_c1" "yes"))
  "mf_iwmmxt_pipeline")

(define_insn_reservation "marvell_f_iwmmxt_pack" 1
  (and (eq_attr "marvell_f_iwmmxt" "yes")
       (eq_attr "wmmxt_pack" "yes"))
  "mf_iwmmxt_pipeline")

(define_insn_reservation "marvell_f_iwmmxt_shift" 1
  (and (eq_attr "marvell_f_iwmmxt" "yes")
       (eq_attr "wmmxt_shift" "yes"))
  "mf_iwmmxt_pipeline")

(define_insn_reservation "marvell_f_iwmmxt_transfer_c1" 1
  (and (eq_attr "marvell_f_iwmmxt" "yes")
       (eq_attr "wmmxt_transfer_c1" "yes"))
  "mf_iwmmxt_pipeline")

(define_insn_reservation "marvell_f_iwmmxt_transfer_c2" 5
  (and (eq_attr "marvell_f_iwmmxt" "yes")
       (eq_attr "wmmxt_transfer_c2" "yes"))
  "mf_iwmmxt_pipeline")

(define_insn_reservation "marvell_f_iwmmxt_alu_c2" 2
  (and (eq_attr "marvell_f_iwmmxt" "yes")
       (eq_attr "wmmxt_alu_c2" "yes"))
  "mf_iwmmxt_pipeline")

(define_insn_reservation "marvell_f_iwmmxt_alu_c3" 3
  (and (eq_attr "marvell_f_iwmmxt" "yes")
       (eq_attr "wmmxt_alu_c3" "yes"))
  "mf_iwmmxt_pipeline")

(define_insn_reservation "marvell_f_iwmmxt_transfer_c3" 4
  (and (eq_attr "marvell_f_iwmmxt" "yes")
       (eq_attr "wmmxt_transfer_c3" "yes"))
  "mf_iwmmxt_pipeline")

(define_insn_reservation "marvell_f_iwmmxt_mult_c1" 4
  (and (eq_attr "marvell_f_iwmmxt" "yes")
       (eq_attr "wmmxt_mult_c1" "yes"))
  "mf_iwmmxt_pipeline")

;There is a forwarding path from ME3 stage
(define_insn_reservation "marvell_f_iwmmxt_mult_c2" 3
  (and (eq_attr "marvell_f_iwmmxt" "yes")
       (eq_attr "wmmxt_mult_c2" "yes"))
  "mf_iwmmxt_pipeline")

(define_insn_reservation "marvell_f_iwmmxt_wstr" 0
  (and (eq_attr "marvell_f_iwmmxt" "yes")
       (eq_attr "type" "wmmx_wstr"))
  "mf_iwmmxt_pipeline")

;There is a forwarding path from MW stage
(define_insn_reservation "marvell_f_iwmmxt_wldr" 5
  (and (eq_attr "marvell_f_iwmmxt" "yes")
       (eq_attr "type" "wmmx_wldr"))
  "mf_iwmmxt_pipeline")
