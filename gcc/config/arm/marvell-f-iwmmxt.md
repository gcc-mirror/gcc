;; Marvell WMMX2 pipeline description
;; Copyright (C) 2011, 2012 Free Software Foundation, Inc.
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
  (if_then_else (eq_attr "wtype" "wror, wsll, wsra, wsrl")
		(const_string "yes") (const_string "no"))
)

(define_attr "wmmxt_pack" "yes,no"
  (if_then_else (eq_attr "wtype" "waligni, walignr, wmerge, wpack, wshufh, wunpckeh, wunpckih, wunpckel, wunpckil")
		(const_string "yes") (const_string "no"))
)

(define_attr "wmmxt_mult_c1" "yes,no"
  (if_then_else (eq_attr "wtype" "wmac, wmadd, wmiaxy, wmiawxy, wmulw, wqmiaxy, wqmulwm")
		(const_string "yes") (const_string "no"))
)

(define_attr "wmmxt_mult_c2" "yes,no"
  (if_then_else (eq_attr "wtype" "wmul, wqmulm")
		(const_string "yes") (const_string "no"))
)

(define_attr "wmmxt_alu_c1" "yes,no"
  (if_then_else (eq_attr "wtype" "wabs, wabsdiff, wand, wandn, wmov, wor, wxor")
	        (const_string "yes") (const_string "no"))
)

(define_attr "wmmxt_alu_c2" "yes,no"
  (if_then_else (eq_attr "wtype" "wacc, wadd, waddsubhx, wavg2, wavg4, wcmpeq, wcmpgt, wmax, wmin, wsub, waddbhus, wsubaddhx")
		(const_string "yes") (const_string "no"))
)

(define_attr "wmmxt_alu_c3" "yes,no"
  (if_then_else (eq_attr "wtype" "wsad")
	        (const_string "yes") (const_string "no"))
)

(define_attr "wmmxt_transfer_c1" "yes,no"
  (if_then_else (eq_attr "wtype" "tbcst, tinsr, tmcr, tmcrr")
                (const_string "yes") (const_string "no"))
)

(define_attr "wmmxt_transfer_c2" "yes,no"
  (if_then_else (eq_attr "wtype" "textrm, tmovmsk, tmrc, tmrrc")
	        (const_string "yes") (const_string "no"))
)

(define_attr "wmmxt_transfer_c3" "yes,no"
  (if_then_else (eq_attr "wtype" "tmia, tmiaph, tmiaxy")
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
       (eq_attr "wtype" "wstr"))
  "mf_iwmmxt_pipeline")

;There is a forwarding path from MW stage
(define_insn_reservation "marvell_f_iwmmxt_wldr" 5
  (and (eq_attr "marvell_f_iwmmxt" "yes")
       (eq_attr "wtype" "wldr"))
  "mf_iwmmxt_pipeline")
