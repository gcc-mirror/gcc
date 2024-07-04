;; Instruction scheduling information for C-SKY CK802 processors.
;; Copyright (C) 2018-2024 Free Software Foundation, Inc.
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

(define_automaton "csky_ck802")

(define_cpu_unit "csky_ck802_ex" "csky_ck802")
(define_cpu_unit "csky_ck802_wb" "csky_ck802")

(define_insn_reservation "ck802_alu" 2
  (and (match_test "CSKY_TARGET_ARCH (CK802)")
       (eq_attr "type" "alu"))
  "csky_ck802_ex, csky_ck802_wb")

(define_insn_reservation "ck802_branch" 2
  (and (match_test "CSKY_TARGET_ARCH (CK802)")
       (eq_attr "type" "branch, branch_jmp"))
  "csky_ck802_ex, csky_ck802_wb")

(define_insn_reservation "ck802_cmp" 2
  (and (match_test "CSKY_TARGET_ARCH (CK802)")
       (eq_attr "type" "cmp"))
  "csky_ck802_ex, csky_ck802_wb")

(define_insn_reservation "ck802_cbranch" 2
  (and (match_test "CSKY_TARGET_ARCH (CK802)")
       (eq_attr "type" "cbranch"))
  "csky_ck802_ex, csky_ck802_wb")

(define_insn_reservation "ck802_call" 2
  (and (match_test "CSKY_TARGET_ARCH (CK802)")
       (eq_attr "type" "call, call_jsr"))
  "csky_ck802_ex, csky_ck802_wb")

(define_insn_reservation "ck802_load" 2
  (and (match_test "CSKY_TARGET_ARCH (CK802)")
	(and (eq_attr "type" "load")
	     (match_test "!csky_minipool_load_p (insn)")))
  "csky_ck802_ex, csky_ck802_wb")

(define_insn_reservation "ck802_pool" 2
  (and (match_test "CSKY_TARGET_ARCH (CK802)")
       (and (eq_attr "type" "load")
	    (match_test "csky_minipool_load_p (insn)")))
  "csky_ck802_ex, csky_ck802_wb")

(define_insn_reservation "ck802_store" 2
  (and (match_test "CSKY_TARGET_ARCH (CK802)")
       (eq_attr "type" "store"))
  "csky_ck802_ex, csky_ck802_wb")

;; Switching between constant pool loads and loads/stores in the data section
;; carries an extra penalty.
(define_bypass 3 "ck802_load,ck802_store" "ck802_pool")
(define_bypass 3 "ck802_pool" "ck802_load,ck802_store")

(define_bypass 1 "ck802_alu,ck802_branch,ck802_cmp,ck802_cbranch,ck802_call,\
		  ck802_load,ck802_pool,ck802_store"
		 "ck802_alu")

(define_bypass 1 "ck802_alu,ck802_branch,ck802_cmp,ck802_cbranch,ck802_call,\
		  ck802_load,ck802_pool,ck802_store"
		 "ck802_branch")

(define_bypass 2 "ck802_cmp" "ck802_cbranch")
