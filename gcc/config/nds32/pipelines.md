;; Pipeline descriptions of Andes NDS32 cpu for GNU compiler
;; Copyright (C) 2012-2025 Free Software Foundation, Inc.
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
;; Include N7 pipeline settings.
;; ------------------------------------------------------------------------
(include "nds32-n7.md")


;; ------------------------------------------------------------------------
;; Include N8 pipeline settings.
;; ------------------------------------------------------------------------
(include "nds32-n8.md")


;; ------------------------------------------------------------------------
;; Include E8 pipeline settings.
;; ------------------------------------------------------------------------
(include "nds32-e8.md")


;; ------------------------------------------------------------------------
;; Include N9/N10 pipeline settings.
;; ------------------------------------------------------------------------
(include "nds32-n9-3r2w.md")
(include "nds32-n9-2r1w.md")


;; ------------------------------------------------------------------------
;; Include N10 pipeline settings.
;; ------------------------------------------------------------------------
(include "nds32-n10.md")


;; ------------------------------------------------------------------------
;; Include Graywolf pipeline settings.
;; ------------------------------------------------------------------------
(include "nds32-graywolf.md")


;; ------------------------------------------------------------------------
;; Include N12/N13 pipeline settings.
;; ------------------------------------------------------------------------
(include "nds32-n13.md")


;; ------------------------------------------------------------------------
;; Define simple pipeline settings.
;; ------------------------------------------------------------------------

(define_automaton "nds32_simple_machine")

(define_cpu_unit "simple_unit" "nds32_simple_machine")

(define_insn_reservation "simple_insn" 1
  (eq_attr "pipeline_model" "simple")
  "simple_unit")

;; ------------------------------------------------------------------------
