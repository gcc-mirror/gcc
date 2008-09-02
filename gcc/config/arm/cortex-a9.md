;; ARM Cortex-A9 VFP pipeline description
;; Copyright (C) 2008 Free Software Foundation, Inc.
;; Written by CodeSourcery.
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

(define_automaton "cortex_a9")

;; FIXME: We model a single pipeline for all instructions.
;; Is dual-issue possible, and do we have other pipelines?
(define_cpu_unit "cortex_a9_vfp" "cortex_a9")

(define_insn_reservation "cortex_a9_ffarith" 1
 (and (eq_attr "tune" "cortexa9")
      (eq_attr "type" "fcpys,ffariths,ffarithd,fcmps,fcmpd,fconsts,fconstd"))
 "cortex_a9_vfp")

(define_insn_reservation "cortex_a9_fadd" 4
 (and (eq_attr "tune" "cortexa9")
      (eq_attr "type" "fadds,faddd,f_cvt"))
 "cortex_a9_vfp")

(define_insn_reservation "cortex_a9_fmuls" 5
 (and (eq_attr "tune" "cortexa9")
      (eq_attr "type" "fmuls"))
 "cortex_a9_vfp")

(define_insn_reservation "cortex_a9_fmuld" 6
 (and (eq_attr "tune" "cortexa9")
      (eq_attr "type" "fmuld"))
 "cortex_a9_vfp*2")

(define_insn_reservation "cortex_a9_fmacs" 8
 (and (eq_attr "tune" "cortexa9")
      (eq_attr "type" "fmacs"))
 "cortex_a9_vfp")

(define_insn_reservation "cortex_a9_fmacd" 8
 (and (eq_attr "tune" "cortexa9")
      (eq_attr "type" "fmacd"))
 "cortex_a9_vfp*2")

(define_insn_reservation "cortex_a9_fdivs" 15
 (and (eq_attr "tune" "cortexa9")
      (eq_attr "type" "fdivs"))
 "cortex_a9_vfp*10")

(define_insn_reservation "cortex_a9_fdivd" 25
 (and (eq_attr "tune" "cortexa9")
      (eq_attr "type" "fdivd"))
 "cortex_a9_vfp*20")
