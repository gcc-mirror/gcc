;; ARM VFP11 pipeline description
;; Copyright (C) 2003, 2005, 2007, 2008 Free Software Foundation, Inc.
;; Written by CodeSourcery.
;;
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

(define_automaton "vfp11")

;; There are 3 pipelines in the VFP11 unit.
;;
;; - A 8-stage FMAC pipeline (7 execute + writeback) with forward from
;;   fourth stage for simple operations.
;;
;; - A 5-stage DS pipeline (4 execute + writeback) for divide/sqrt insns.
;;   These insns also uses first execute stage of FMAC pipeline.
;;
;; - A 4-stage LS pipeline (execute + 2 memory + writeback) with forward from
;;   second memory stage for loads.

;; We do not model Write-After-Read hazards.
;; We do not do write scheduling with the arm core, so it is only necessary
;; to model the first stage of each pipeline
;; ??? Need to model LS pipeline properly for load/store multiple?
;; We do not model fmstat properly.  This could be done by modeling pipelines
;; properly and defining an absence set between a dummy fmstat unit and all
;; other vfp units.

(define_cpu_unit "fmac" "vfp11")

(define_cpu_unit "ds" "vfp11")

(define_cpu_unit "vfp_ls" "vfp11")

(define_cpu_unit "fmstat" "vfp11")

(exclusion_set "fmac,ds" "fmstat")

(define_insn_reservation "vfp_ffarith" 4
 (and (eq_attr "generic_vfp" "yes")
      (eq_attr "type" "fcpys,ffariths,ffarithd,fcmps,fcmpd"))
 "fmac")

(define_insn_reservation "vfp_farith" 8
 (and (eq_attr "generic_vfp" "yes")
      (eq_attr "type" "fadds,faddd,fconsts,fconstd,f_cvt,fmuls,fmacs"))
 "fmac")

(define_insn_reservation "vfp_fmul" 9
 (and (eq_attr "generic_vfp" "yes")
      (eq_attr "type" "fmuld,fmacd"))
 "fmac*2")

(define_insn_reservation "vfp_fdivs" 19
 (and (eq_attr "generic_vfp" "yes")
      (eq_attr "type" "fdivs"))
 "ds*15")

(define_insn_reservation "vfp_fdivd" 33
 (and (eq_attr "generic_vfp" "yes")
      (eq_attr "type" "fdivd"))
 "fmac+ds*29")

;; Moves to/from arm regs also use the load/store pipeline.
(define_insn_reservation "vfp_fload" 4
 (and (eq_attr "generic_vfp" "yes")
      (eq_attr "type" "f_loads,f_loadd,r_2_f"))
 "vfp_ls")

(define_insn_reservation "vfp_fstore" 4
 (and (eq_attr "generic_vfp" "yes")
      (eq_attr "type" "f_stores,f_stored,f_2_r"))
 "vfp_ls")

(define_insn_reservation "vfp_to_cpsr" 4
 (and (eq_attr "generic_vfp" "yes")
      (eq_attr "type" "f_flag"))
 "fmstat,vfp_ls*3")

