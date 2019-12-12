;; Scheduler information for C-SKY CK801 processors.
;; Copyright (C) 2018-2019 Free Software Foundation, Inc.
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

;; This is just a placeholder for a more accurate pipeline
;; description for CK801.

(define_automaton "ck801")

(define_cpu_unit "ck801_ex1" "ck801")
(define_cpu_unit "ck801_exit" "ck801")

(define_insn_reservation "ck801_generic" 1
  (and (match_test "CSKY_TARGET_ARCH (CK801)")
       (eq_attr "type" "alu,cmp,branch,cbranch,addsub,alu_ix,branch_jmp,call_jsr,call"))
  "ck801_ex1+ck801_exit")

(define_insn_reservation "ck801_load" 1
  (and (match_test "CSKY_TARGET_ARCH (CK801)")
       (and (eq_attr "type" "load")
	    (match_test "!csky_minipool_load_p (insn)")))
  "ck801_ex1+ck801_exit")

(define_insn_reservation "ck801_pool" 1
  (and (match_test "CSKY_TARGET_ARCH (CK801)")
       (and (eq_attr "type" "load")
	    (match_test "csky_minipool_load_p (insn)")))
  "ck801_ex1+ck801_exit")

(define_insn_reservation "ck801_store" 1
  (and (match_test "CSKY_TARGET_ARCH (CK801)")
       (eq_attr "type" "store"))
  "ck801_ex1+ck801_exit")

;; Switching between constant pool loads and loads/stores in the data section
;; carries an extra penalty.
(define_bypass 2 "ck801_load,ck801_store" "ck801_pool")
(define_bypass 2 "ck801_pool" "ck801_load,ck801_store")
