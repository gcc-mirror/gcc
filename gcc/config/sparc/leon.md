;; Scheduling description for LEON.
;;   Copyright (C) 2010 Free Software Foundation, Inc.
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


(define_automaton "leon")

(define_cpu_unit "leon_memory, leon_fpalu" "leon")
(define_cpu_unit "leon_fpmds" "leon")
(define_cpu_unit "write_buf" "leon")

(define_insn_reservation "leon_load" 1
  (and (eq_attr "cpu" "leon")
    (eq_attr "type" "load,sload,fpload"))
  "leon_memory")

(define_insn_reservation "leon_store" 1
  (and (eq_attr "cpu" "leon")
    (eq_attr "type" "store,fpstore"))
  "leon_memory+write_buf")
  
(define_insn_reservation "leon_fp_alu" 1
  (and (eq_attr "cpu" "leon")
    (eq_attr "type" "fp,fpmove"))
  "leon_fpalu, nothing")

(define_insn_reservation "leon_fp_mult" 1
  (and (eq_attr "cpu" "leon")
    (eq_attr "type" "fpmul"))
  "leon_fpmds, nothing")

(define_insn_reservation "leon_fp_div" 16
  (and (eq_attr "cpu" "leon")
    (eq_attr "type" "fpdivs,fpdivd"))
  "leon_fpmds, nothing*15")

(define_insn_reservation "leon_fp_sqrt" 23
  (and (eq_attr "cpu" "leon")
    (eq_attr "type" "fpsqrts,fpsqrtd"))
  "leon_fpmds, nothing*21")

