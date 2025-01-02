;; ARM Cortex-R4F VFP pipeline description
;; Copyright (C) 2007-2025 Free Software Foundation, Inc.
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

;; With the exception of simple VMOV <freg>, <freg> instructions and
;; the accululate operand of a multiply-accumulate instruction, all
;; registers are early registers.  Thus base latencies are 1 more than
;; those listed in the TRM.

;; We use the A, B abd C units from the integer core, plus two additional
;; units to enforce VFP dual issue constraints.

;;		  A B C	    V1	VMLA
;; fcpy		  1 2
;; farith	  1 2	    1
;; fmrc		  1 2
;; fconst	  1 2 *	    *
;; ffarith	  1 2 *	    *
;; fmac		  1 2	    1	2
;; fdiv		  1 2	    *
;; f_loads	  *   *	    *
;; f_stores	  *   *	    	*

(define_cpu_unit "cortex_r4_v1" "cortex_r4")

(define_cpu_unit "cortex_r4_vmla" "cortex_r4")

(define_reservation "cortex_r4_issue_ab"
		    "(cortex_r4_issue_a|cortex_r4_issue_b)")
(define_reservation "cortex_r4_single_issue"
		    "cortex_r4_issue_a+cortex_r4_issue_b")

(define_insn_reservation "cortex_r4_fcpys" 2
 (and (eq_attr "tune_cortexr4" "yes")
      (eq_attr "type" "fmov"))
 "cortex_r4_issue_ab")

(define_insn_reservation "cortex_r4_ffariths" 2
 (and (eq_attr "tune_cortexr4" "yes")
      (eq_attr "type" "ffariths,fconsts,fcmps"))
 "cortex_r4_issue_ab+cortex_r4_issue_c+cortex_r4_v1")

(define_insn_reservation "cortex_r4_fariths" 3
 (and (eq_attr "tune_cortexr4" "yes")
      (eq_attr "type" "fadds,fmuls"))
 "(cortex_r4_issue_a+cortex_r4_v1)|cortex_r4_issue_b")

(define_insn_reservation "cortex_r4_fmacs" 6
 (and (eq_attr "tune_cortexr4" "yes")
      (eq_attr "type" "fmacs,ffmas"))
 "(cortex_r4_issue_a+cortex_r4_v1)|(cortex_r4_issue_b+cortex_r4_vmla)")

(define_insn_reservation "cortex_r4_fdivs" 17
 (and (eq_attr "tune_cortexr4" "yes")
      (eq_attr "type" "fdivs, fsqrts"))
 "cortex_r4_issue_ab+cortex_r4_v1,cortex_r4_issue_a+cortex_r4_v1")

(define_insn_reservation "cortex_r4_floads" 2
 (and (eq_attr "tune_cortexr4" "yes")
      (eq_attr "type" "f_loads"))
 "cortex_r4_issue_a+cortex_r4_issue_c+cortex_r4_v1")

(define_insn_reservation "cortex_r4_fstores" 1
 (and (eq_attr "tune_cortexr4" "yes")
      (eq_attr "type" "f_stores"))
 "cortex_r4_issue_a+cortex_r4_issue_c+cortex_r4_vmla")

(define_insn_reservation "cortex_r4_mcr" 2
 (and (eq_attr "tune_cortexr4" "yes")
      (eq_attr "type" "f_mcr,f_mcrr"))
 "cortex_r4_issue_ab")

(define_insn_reservation "cortex_r4_mrc" 3
 (and (eq_attr "tune_cortexr4" "yes")
      (eq_attr "type" "f_mrc,f_mrrc"))
 "cortex_r4_issue_ab")

;; Bypasses for normal (not early) regs.
(define_bypass 1 "cortex_r4_ffariths,cortex_r4_fcpys,cortex_r4_mcr"
		 "cortex_r4_fcpys")
(define_bypass 2 "cortex_r4_fariths"
		 "cortex_r4_fcpys")
(define_bypass 5 "cortex_r4_fmacs"
		 "cortex_r4_fcpys")
(define_bypass 16 "cortex_r4_fdivs"
		  "cortex_r4_fcpys")

(define_bypass 1 "cortex_r4_ffariths,cortex_r4_fcpys,cortex_r4_mcr"
		 "cortex_r4_fmacs"
		 "arm_no_early_mul_dep")
(define_bypass 2 "cortex_r4_fariths"
		 "cortex_r4_fmacs"
		 "arm_no_early_mul_dep")
;; mac->mac has an extra forwarding path.
(define_bypass 3 "cortex_r4_fmacs"
		 "cortex_r4_fmacs"
		 "arm_no_early_mul_dep")
(define_bypass 16 "cortex_r4_fdivs"
		  "cortex_r4_fmacs"
		  "arm_no_early_mul_dep")

;; Double precision operations.  These cannot dual issue.

(define_insn_reservation "cortex_r4_fmacd" 20
 (and (eq_attr "tune_cortexr4" "yes")
      (eq_attr "type" "fmacd,ffmad"))
 "cortex_r4_single_issue*13")

(define_insn_reservation "cortex_r4_farith" 10
 (and (eq_attr "tune_cortexr4" "yes")
      (eq_attr "type" "faddd,fmuld"))
 "cortex_r4_single_issue*3")

;; FIXME: The short cycle count suggests these instructions complete
;; out of order.  Chances are this is not a pipelined operation.
(define_insn_reservation "cortex_r4_fdivd" 97
 (and (eq_attr "tune_cortexr4" "yes")
      (eq_attr "type" "fdivd, fsqrtd"))
 "cortex_r4_single_issue*3")

(define_insn_reservation "cortex_r4_ffarithd" 2
 (and (eq_attr "tune_cortexr4" "yes")
      (eq_attr "type" "ffarithd,fconstd"))
 "cortex_r4_single_issue")

(define_insn_reservation "cortex_r4_fcmpd" 2
 (and (eq_attr "tune_cortexr4" "yes")
      (eq_attr "type" "fcmpd"))
 "cortex_r4_single_issue*2")

(define_insn_reservation "cortex_r4_f_cvt" 8
 (and (eq_attr "tune_cortexr4" "yes")
      (eq_attr "type" "f_cvt,f_cvtf2i,f_cvti2f"))
 "cortex_r4_single_issue*3")

(define_insn_reservation "cortex_r4_f_memd" 8
 (and (eq_attr "tune_cortexr4" "yes")
      (eq_attr "type" "f_loadd,f_stored"))
 "cortex_r4_single_issue")

(define_insn_reservation "cortex_r4_f_flag" 1
 (and (eq_attr "tune_cortexr4" "yes")
      (eq_attr "type" "f_stores"))
 "cortex_r4_single_issue")

