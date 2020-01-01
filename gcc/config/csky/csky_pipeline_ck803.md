;; Scheduler information for C-SKY CK803 processors.
;; Copyright (C) 2018-2020 Free Software Foundation, Inc.
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

(define_automaton "ck803")

(define_cpu_unit "ck803_ex1" "ck803")
(define_cpu_unit "ck803_exit" "ck803")

(define_insn_reservation "ck803_3cycle" 1
  (and (match_test "CSKY_TARGET_ARCH (CK803)")
       (eq_attr "type" "alu,cmp,branch,branch_jmp,call_jsr,call"))
  "ck803_ex1+ck803_exit")

(define_insn_reservation "ck803_alu1" 1
  (and (match_test "CSKY_TARGET_ARCH (CK803)")
       (eq_attr "type" "addsub,alu_ix"))
  "ck803_ex1+ck803_exit")

(define_insn_reservation "ck803_cbranch" 1
  (and (match_test "CSKY_TARGET_ARCH (CK803)")
       (eq_attr "type" "cbranch"))
  "ck803_ex1+ck803_exit")

(define_insn_reservation "ck803_load" 1
  (and (match_test "CSKY_TARGET_ARCH (CK803)")
       (and (eq_attr "type" "load")
	    (match_test "!csky_minipool_load_p (insn)")))
  "ck803_ex1+ck803_exit")

(define_insn_reservation "ck803_pool" 1
  (and (match_test "CSKY_TARGET_ARCH (CK803)")
       (and (eq_attr "type" "load")
	    (match_test "csky_minipool_load_p (insn)")))
  "ck803_ex1+ck803_exit")

(define_insn_reservation "ck803_store" 1
  (and (match_test "CSKY_TARGET_ARCH (CK803)")
       (eq_attr "type" "store"))
  "ck803_ex1+ck803_exit")

;; Switching between constant pool loads and loads/stores in the data section
;; carries an extra penalty.
(define_bypass 2 "ck803_load,ck803_store" "ck803_pool")
(define_bypass 2 "ck803_pool" "ck803_load,ck803_store")

(define_bypass 2 "ck803_3cycle,ck803_cbranch,ck803_load,ck803_store,ck803_pool"
		 "ck803_cbranch")
