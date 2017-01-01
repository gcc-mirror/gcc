;; Scheduling description for LEON.
;;   Copyright (C) 2010-2017 Free Software Foundation, Inc.
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

;; Leon is a single-issue processor.

(define_automaton "leon")

(define_cpu_unit "leon_memory" "leon")

(define_insn_reservation "leon_load" 1
  (and (eq_attr "cpu" "leon,leon3,leon3v7")
       (and (eq_attr "fix_ut699" "false") (eq_attr "type" "load,sload")))
  "leon_memory")

;; Use a double reservation to work around the load pipeline hazard on UT699.
(define_insn_reservation "ut699_load" 1
  (and (eq_attr "cpu" "leon,leon3,leon3v7")
       (and (eq_attr "fix_ut699" "true") (eq_attr "type" "load,sload")))
  "leon_memory*2")

(define_insn_reservation "leon_store" 2
  (and (eq_attr "cpu" "leon,leon3,leon3v7") (eq_attr "type" "store"))
  "leon_memory*2")

;; This describes Gaisler Research's FPU

(define_automaton "grfpu")

(define_cpu_unit "grfpu_alu" "grfpu")
(define_cpu_unit "grfpu_ds" "grfpu")

(define_insn_reservation "leon_fp_alu" 4
  (and (eq_attr "cpu" "leon,leon3,leon3v7") (eq_attr "type" "fp,fpcmp,fpmul"))
  "grfpu_alu, nothing*3")

(define_insn_reservation "leon_fp_divs" 16
  (and (eq_attr "cpu" "leon,leon3,leon3v7") (eq_attr "type" "fpdivs"))
  "grfpu_ds*14, nothing*2")

(define_insn_reservation "leon_fp_divd" 17
  (and (eq_attr "cpu" "leon,leon3,leon3v7") (eq_attr "type" "fpdivd"))
  "grfpu_ds*15, nothing*2")

(define_insn_reservation "leon_fp_sqrts" 24
  (and (eq_attr "cpu" "leon,leon3,leon3v7") (eq_attr "type" "fpsqrts"))
  "grfpu_ds*22, nothing*2")

(define_insn_reservation "leon_fp_sqrtd" 25
  (and (eq_attr "cpu" "leon,leon3,leon3v7") (eq_attr "type" "fpsqrtd"))
  "grfpu_ds*23, nothing*2")
