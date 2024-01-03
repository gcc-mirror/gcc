;;  Machine Description for TI MSP43* processors
;;  Copyright (C) 2013-2024 Free Software Foundation, Inc.
;;  Contributed by Red Hat.

;; This file is part of GCC.

;; GCC is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; GCC is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GCC; see the file COPYING3.  If not see
;; <http://www.gnu.org/licenses/>.

(define_predicate "msp430_volatile_memory_operand"
  (and (match_code "mem")
       (match_test ("memory_address_addr_space_p (GET_MODE (op), XEXP (op, 0), MEM_ADDR_SPACE (op))")))
)

; TRUE if neither op nor op0 are a post_inc.  We cannot use post_inc for the
; dst operand so this must be used for any predicates which might allow a mem.
; Since we check both op and op0, this will be FALSE for both "(post_inc)" and
; "(mem (post_inc))"
(define_predicate "msp430_nonpostinc_operand"
  (not (ior (match_code "post_inc")
	    (and (ior (match_operand 0 "msp430_volatile_memory_operand")
		      (match_code "mem"))
		 (match_code "post_inc" "0")))))

; TRUE for any valid general operand.  We do this because
; general_operand refuses to match volatile memory refs.
(define_predicate "msp430_general_operand"
  (ior (match_operand 0 "general_operand")
       (match_operand 0 "msp430_volatile_memory_operand"))
)

; Likewise for nonimmediate_operand.
(define_predicate "msp430_nonimmediate_operand"
  (ior (match_operand 0 "nonimmediate_operand")
       (match_operand 0 "msp430_volatile_memory_operand"))
)

; Similar to msp430_nonimmediate_operand but disallow post_inc operands
(define_predicate "msp430_general_dst_operand"
  (and (match_operand 0 "msp430_nonpostinc_operand")
       (match_operand 0 "msp430_nonimmediate_operand")))

; Similar to msp430_general_dst_operand but disallow volatile memory references
; Note that msp430_nonpostinc_operand will allow a volatile mem but nonimmediate
; will not, so overall this predicate will behave as expected.
; The heuristic for deciding if we can allow volatile memory appears to be:
;   "If the number of references to the variable in the source code matches
;    the number of references to the variable in the assembly template, we can
;    safely allow a volatile memory reference".
;      - paraphrasing DJ Delorie here:
;	 https://gcc.gnu.org/ml/gcc-patches/2014-05/msg00870.html
; When applied to instruction patterns, this means that we can only allow
; volatile memory when the output assembler template contains only one
; instruction which references that volatile address.
(define_predicate "msp430_general_dst_nonv_operand"
  (and (match_operand 0 "msp430_nonpostinc_operand")
       (match_operand 0 "nonimmediate_operand")))

(define_predicate "ubyte_operand"
  (and (match_code "const_int")
       (match_test "IN_RANGE (INTVAL (op), 0, 255)")))

; TRUE for comparisons we support.
(define_predicate "msp430_cmp_operator"
  (match_code "eq,ne,lt,ltu,ge,geu"))

; TRUE for comparisons we need to reverse.
(define_predicate "msp430_reversible_cmp_operator"
  (match_code "gt,gtu,le,leu"))

; TRUE for constants the constant generator can produce
(define_predicate "msp430_constgen_operator"
  (and (match_code "const_int")
       (match_test ("   INTVAL (op) == 0
		     || INTVAL (op) == 1
		     || INTVAL (op) == 2
		     || INTVAL (op) == 4
		     || INTVAL (op) == 8
		     || INTVAL (op) == -1 "))))

; TRUE for constants the constant generator can produce
(define_predicate "msp430_inv_constgen_operator"
  (and (match_code "const_int")
       (match_test ("   INTVAL (op) == ~0
		     || INTVAL (op) == ~1
		     || INTVAL (op) == ~2
		     || INTVAL (op) == ~4
		     || INTVAL (op) == ~8
		     || INTVAL (op) == ~(-1) "))))

; See above note on post_inc
(define_predicate "msp430_nonsubreg_dst_operand"
  (and (match_operand 0 "msp430_nonpostinc_operand")
       (match_code "reg,mem")))

(define_predicate "msp430_nonsubreg_or_imm_operand"
  (ior (match_code "reg,mem")
       (match_operand 0 "immediate_operand")))

(define_predicate "msp430_nonsubregnonpostinc_or_imm_operand"
  (and (match_operand 0 "msp430_nonpostinc_operand")
       (ior (match_code "reg,mem")
	    (match_operand 0 "immediate_operand"))))

(define_predicate "const_1_to_8_operand"
  (and (match_code "const_int")
       (match_test ("   INTVAL (op) >= 1
		     && INTVAL (op) <= 8 "))))

(define_predicate "const_0_to_15_operand"
  (and (match_code "const_int")
       (match_test ("   INTVAL (op) >= 0
		     && INTVAL (op) <= 15 "))))

(define_predicate "const_1_to_19_operand"
  (and (match_code "const_int")
       (match_test ("   INTVAL (op) >= 1
		     && INTVAL (op) <= 19 "))))

(define_predicate "msp430_symbol_operand"
  (match_code "symbol_ref")
)

; Used in length attribute tests - if a source operand is a reg,
; (mem (post_inc)), or (mem (reg)) then it is cheap compared to other operand
; types.
(define_predicate "msp430_cheap_operand"
  (ior (match_code "reg")
       (and (match_code "mem")
	    (ior (match_code "reg" "0")
	    (match_code "post_inc" "0")))))

; Used for insn attributes only.  For insn patterns themselves, use constraints.
(define_predicate "msp430_high_memory_operand"
  (match_test "msp430x_insn_required (op)"))
