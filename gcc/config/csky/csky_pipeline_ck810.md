;; Instruction scheduling information for C-SKY CK810 processors.
;; Copyright (C) 2018-2023 Free Software Foundation, Inc.
;; Contributed by C-SKY Microsystems and Mentor Graphics.
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
;; <http://www.gnu.org/licenses/>.  */


;;-------------------------------------------------------------
;; Pipeline descriptions for ck810
;;-------------------------------------------------------------

(define_attr "cycle" "1,2,not_used_yet"
    (const_string "1"))
(define_automaton "cskyv2_ck810")
(define_cpu_unit "pipeline_alu0" "cskyv2_ck810")
(define_insn_reservation "alu_one_cycle" 1
    (and (eq_attr "cycle" "1")
	 (not (ior (match_test "CSKY_TARGET_ARCH (CK803)")
		   (match_test "CSKY_TARGET_ARCH (CK802)"))))
    "pipeline_alu0")
