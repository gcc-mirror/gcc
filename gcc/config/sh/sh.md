;;- Machine description the Hitachi SH
;;  Copyright (C) 1993 Free Software Foundation, Inc.
;;  Contributed by Steve Chamberlain (sac@cygnus.com)

;; This file is part of GNU CC.

;; GNU CC is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; GNU CC is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU CC; see the file COPYING.  If not, write to
;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;;- See file "rtl.def" for documentation on define_insn, match_*, et. al.



;; -------------------------------------------------------------------------
;; Attributes
;; -------------------------------------------------------------------------

; Target CPU.

(define_attr "cpu" "sh0,sh1,sh2,sh3"
  (const (symbol_ref "sh_cpu")))

(define_attr "type" "cbranch,ctable,jump,arith,other,load,store,move,smpy,dmpy,return,pload"
  (const_string "other"))

; If a conditional branch destination is within -100..100 bytes away 
; from the instruction it can be 2 bytes long.  Something in the
; range -4000..4000 bytes can be 6 bytes long, all other conditional
; branches are 8 bytes long.

; An unconditional jump which can reach forward or back 4k can be 
; 6 bytes long (including the delay slot).  If it is too big, it
; must be 8 bytes long.

; All other instructions are two bytes long by default.

(define_attr "length" "" 
  (cond [(eq_attr "type" "cbranch")
	  (if_then_else (and (ge (minus (pc) (match_dup 0))
				 (const_int -100))
			     (le (minus (pc) (match_dup 0))
				 (const_int 100)))
			(const_int 2)
			(if_then_else (and (ge (minus (pc) (match_dup 0))
					       (const_int -4000))
					   (le (minus (pc) (match_dup 0))
					       (const_int 4000)))
				      (const_int 6)
				      (const_int 8)))

	 (eq_attr "type" "jump")
			(if_then_else (and (ge (minus (pc) (match_dup 0))
					       (const_int -4000))
					   (le (minus (pc) (match_dup 0))
					       (const_int 4000)))
				      (const_int 4)
				      (const_int 6))
	 ] (const_int 2)))

;; (define_function_unit {name} {num-units} {n-users} {test}
;;                       {ready-delay} {issue-delay} [{conflict-list}])
				      
(define_function_unit "memory" 1 1 (eq_attr "type" "load") 1 0)
(define_function_unit "mpy"    1 1 (eq_attr "type" "smpy") 3 0)
(define_function_unit "mpy"    1 1 (eq_attr "type" "dmpy") 5 0)

(define_attr "needs_delay_slot" "yes,no"
  (cond [(eq_attr "type" "jump") (const_string "yes")
	 (eq_attr "type" "return") (const_string "yes")]
	(const_string "no")))

(define_delay 
  (eq_attr "needs_delay_slot" "yes") 
  [(eq_attr "in_delay_slot" "yes") (nil) (nil)])


(define_attr "dump" "yes,no,must" (const_string "no"))
(define_attr "constneed" "yes,no" (const_string "no"))
(define_attr "smallestsize" "" (const_int 2))
(define_attr "largestsize" "" (const_int 8))
(define_attr "constantsize" "" (const_int 4))

(define_attr "in_delay_slot" "maybe,yes,no" 
  (cond [(eq_attr "type" "cbranch") (const_string "no")
	 (eq_attr "type" "jump") (const_string "no")
	 (eq_attr "type" "pload") (const_string "no")
	 (eq_attr "type" "return") (const_string "no")
	 (eq_attr "length" "2") (const_string "yes")
	 (eq_attr "length" "4,6,8,10,12") (const_string "no")
	 ] (const_string "yes")))



;; -------------------------------------------------------------------------
;; SImode signed integer comparisons
;; -------------------------------------------------------------------------

(define_insn ""
  [(set (match_operand:SI 0 "arith_reg_operand" "=r")
	(eq:SI (reg:SI 18)
	       (const_int 1)))]
  ""
  "movt	%0 !movt1")
  
(define_insn ""
  [(set (reg:SI 18) (gt (match_operand:SI 0 "arith_reg_operand" "r")
		  (const_int 0)))]
  ""
  "cmp/pl	%0")

(define_insn ""
  [(set (reg:SI 18) (ge (match_operand:SI 0 "arith_reg_operand" "r")
		  (const_int 0)))]
  ""
  "cmp/pz	%0")

(define_insn "cmpeqsi_t"
  [(set (reg:SI 18) (eq (match_operand:SI 0 "arith_reg_operand" "r,z")
		  (match_operand:SI 1 "arith_operand" "r,I")))]
  ""
  "cmp/eq	%1,%0")


(define_insn "cmpgtsi_t"
  [(set (reg:SI 18) (gt (match_operand:SI 0 "arith_reg_operand" "r")
		  (match_operand:SI 1 "arith_reg_operand" "r")))]
  ""
  "cmp/gt	%1,%0")

(define_insn "cmpgesi_t"
  [(set (reg:SI 18) (ge (match_operand:SI 0 "arith_reg_operand" "r")
		  (match_operand:SI 1 "arith_reg_operand" "r")))]
  ""
  "cmp/ge	%1,%0")

(define_insn "cmpltsi_t"
  [(set (reg:SI 18) (lt (match_operand:SI 0 "arith_reg_operand" "r")
		  (match_operand:SI 1 "arith_reg_operand" "r")))]
  ""
  "cmp/gt	%0,%1")

(define_insn "cmplesi_t"
  [(set (reg:SI 18) (le (match_operand:SI 0 "arith_reg_operand" "r")
			  (match_operand:SI 1 "arith_reg_operand" "r")))]
  ""
  "cmp/ge	%0,%1")


;; -------------------------------------------------------------------------
;; SImode unsigned integer comparisons
;; -------------------------------------------------------------------------

(define_insn "cmpgeusi_t"
  [(set (reg:SI 18) (geu (match_operand:SI 0 "arith_reg_operand" "r")
		   (match_operand:SI 1 "arith_reg_operand" "r")))]
  ""
  "cmp/hs	%1,%0")

(define_insn "cmpgtusi_t"
  [(set (reg:SI 18) (gtu (match_operand:SI 0 "arith_reg_operand" "r")
		   (match_operand:SI 1 "arith_reg_operand" "r")))]
  ""
  "cmp/hi	%1,%0")

(define_insn "cmpleusi_t"
  [(set (reg:SI 18) (leu (match_operand:SI 0 "arith_reg_operand" "r")
		   (match_operand:SI 1 "arith_reg_operand" "r")))]
  ""
  "cmp/hs	%0,%1")

(define_insn "cmpltusi_t"
  [(set (reg:SI 18) (ltu (match_operand:SI 0 "arith_reg_operand" "r")
		   (match_operand:SI 1 "arith_reg_operand" "r")))]
  ""
  "cmp/hi	%0,%1")

;; We save the compare operands in the cmpxx patterns and use them when
;; we generate the branch.

(define_expand "cmpsi"
  [(set (reg:SI 18) (compare (match_operand:SI 0 "arith_reg_operand" "")
			     (match_operand:SI 1 "arith_reg_operand" "")))]
  ""
  "
{ sh_compare_op0 = operands[0];
  sh_compare_op1 = operands[1];
  DONE;
}")


;; -------------------------------------------------------------------------
;; Addition instructions
;; -------------------------------------------------------------------------

(define_insn "adddi3"
  [(set (match_operand:DI 0 "arith_reg_operand" "=&r")
	(plus:DI (match_operand:DI 1 "arith_reg_operand" "%0")
		 (match_operand:DI 2 "arith_reg_operand" "r")))
   (clobber (reg:SI 18))]
  ""
  "clrt\;addc	%R2,%R0\;addc	%2,%0"
  [(set_attr "length" "6")
   (set_attr "in_delay_slot" "no")
   (set_attr "type" "arith")])


(define_insn "addsi3_i"
  [(set (match_operand:SI 0 "arith_reg_operand" "=r")
	(plus:SI (match_operand:SI 1 "arith_reg_operand" "%0")
		 (match_operand:SI 2 "arith_operand" "rI")))]
  ""
  "add	%2,%0"
  [(set_attr "length" "2")
   (set_attr "type" "arith")])

(define_expand "addsi3"
  [(set (match_operand:SI 0 "arith_reg_operand" "=r")
	(plus:SI (match_operand:SI 1 "arith_reg_operand" "%0")
		 (match_operand:SI 2 "arith_operand" "rI")))]
  ""
  "")


;; -------------------------------------------------------------------------
;; Subtraction instructions
;; -------------------------------------------------------------------------

(define_insn "subdi3"
  [(set (match_operand:DI 0 "arith_reg_operand" "=&r")
	(minus:DI (match_operand:DI 1 "arith_reg_operand" "0")
		  (match_operand:DI 2 "arith_reg_operand" "r")))
   (clobber (reg:SI 18))]
  ""
  "clrt\;subc	%R2,%R0\;subc	%2,%0"
  [(set_attr "length" "6")
   (set_attr "in_delay_slot" "no")
   (set_attr "type" "arith")])

(define_insn "subsi3"
  [(set (match_operand:SI 0 "arith_reg_operand" "=r,r")
	(minus:SI (match_operand:SI 1 "arith_reg_operand" "0,0")
		  (match_operand:SI 2 "arith_operand" "r,I")))]
  ""
  "@
	sub	%2,%0
	add	%M2,%0"
  [(set_attr "type" "arith")])


;; -------------------------------------------------------------------------
;; Multiplication instructions
;; -------------------------------------------------------------------------


(define_insn ""
  [(set (reg:SI 21)
	(mult:SI (zero_extend:SI
		  (match_operand:HI 1 "arith_reg_operand" "r"))
		 (zero_extend:SI
		  (match_operand:HI 2 "arith_reg_operand" "r"))))]
  ""
  "mulu	%2,%1"
  [(set_attr "type" "smpy")])

(define_insn ""
  [(set (reg:SI 21)
	(mult:SI (sign_extend:SI
		  (match_operand:HI 1 "arith_reg_operand" "r"))
		 (sign_extend:SI
		  (match_operand:HI 2 "arith_reg_operand" "r"))))]
  ""
  "muls	%2,%1"
  [(set_attr "type" "smpy")])

(define_expand "mulhisi3"
  [(set (reg:SI 21)
	(mult:SI (sign_extend:SI
		  (match_operand:HI 1 "arith_reg_operand" "r"))
		 (sign_extend:SI
		  (match_operand:HI 2 "arith_reg_operand" "r"))))
   (set (match_operand:SI 0 "arith_reg_operand" "=r")
	(reg:SI 21))]
  ""
  "")

(define_expand "umulhisi3"
  [(set (reg:SI 21)
	(mult:SI (zero_extend:SI
		  (match_operand:HI 1 "arith_reg_operand" "r"))
		 (zero_extend:SI
		  (match_operand:HI 2 "arith_reg_operand" "r"))))
   (set (match_operand:SI 0 "arith_reg_operand" "=r")
	(reg:SI 21))]
  ""
  "")


;; -------------------------------------------------------------------------
;; Logical operations
;; -------------------------------------------------------------------------

(define_insn "andsi3"
  [(set (match_operand:SI 0 "arith_reg_operand" "=r,z")
	(and:SI (match_operand:SI 1 "arith_reg_operand" "%0,0")
		(match_operand:SI 2 "logical_operand" "r,L")))]
  ""
  "and	%2,%0"
  [(set_attr "type" "arith")])

(define_insn "iorsi3"
  [(set (match_operand:SI 0 "arith_reg_operand" "=r,z")
	(ior:SI (match_operand:SI 1 "arith_reg_operand" "%0,0")
		(match_operand:SI 2 "logical_operand" "r,L")))]
  ""
  "or	%2,%0")

(define_insn "xorsi3"
  [(set (match_operand:SI 0 "arith_reg_operand" "=r,z")
	(xor:SI (match_operand:SI 1 "arith_reg_operand" "%0,0")
		(match_operand:SI 2 "logical_operand" "r,L")))]
  ""
  "xor	%2,%0"
  [(set_attr "type" "arith")])


;; -------------------------------------------------------------------------
;; Shifts and rotates
;; -------------------------------------------------------------------------

(define_insn "rotlsi3_k"
  [(set (match_operand:SI 0 "arith_reg_operand" "=r")
	(rotate:SI (match_operand:SI 1 "arith_reg_operand" "0")
		   (const_int 1)))
   (clobber (reg:SI 18))]
  ""
  "rotl	%0")

(define_expand "rotlsi3"
  [(parallel[(set (match_operand:SI 0 "arith_reg_operand" "")
		  (rotate:SI (match_operand:SI 1 "arith_reg_operand" "")
			     (match_operand:SI 2 "immediate_operand" "")))
	     (clobber (reg:SI 18))])]
  ""
  "{ if (GET_CODE(operands[2]) != CONST_INT || INTVAL(operands[2]) != 1) FAIL;}")

(define_insn "rotrsi3_k"
  [(set (match_operand:SI 0 "arith_reg_operand" "=r")
	(rotatert:SI (match_operand:SI 1 "arith_reg_operand" "0")
		     (const_int 1)))
   (clobber (reg:SI 18))]
  ""
  "rotr	%0")

(define_expand "rotrsi3"
  [(parallel[(set (match_operand:SI 0 "arith_reg_operand" "")
		   (rotatert:SI (match_operand:SI 1 "arith_reg_operand" "")
				(match_operand:SI 2 "immediate_operand" "")))
	      (clobber (reg:SI 18))])]
  ""
  "{ if (GET_CODE(operands[2]) != CONST_INT || INTVAL(operands[2]) != 1) FAIL;}")

(define_insn "ashlsi3_k"
  [(set (match_operand:SI 0 "arith_reg_operand" "=r,r")
	(ashift:SI (match_operand:SI 1 "arith_reg_operand" "0,0")
		   (match_operand:SI 2 "immediate_operand" "K,n")))
   (clobber (reg:SI 18))]
  ""
  "*return output_shift(\"shll\", operands[0], operands[2], ASHIFT);"
  [(set_attr "length" "2,12")
   (set_attr "in_delay_slot" "yes,no")
   (set_attr "type" "arith")])

(define_expand "ashlsi3"
  [(parallel[(set (match_operand:SI 0 "arith_reg_operand" "")
		  (ashift:SI (match_operand:SI 1 "arith_reg_operand" "")
			     (match_operand:SI 2 "immediate_operand" "")))
	     (clobber (reg:SI 18))])]
  ""
  "if (!ok_shift_value(operands[2], ASHIFT)) FAIL;")

(define_insn "ashrsi3_k"
  [(set (match_operand:SI 0 "arith_reg_operand" "=r")
	(ashiftrt:SI (match_operand:SI 1 "arith_reg_operand" "0")
		     (const_int 1)))
   (clobber (reg:SI 18))]
  ""
  "shar	%0"
  [(set_attr "type" "arith")])

(define_expand "ashrsi3"
  [(parallel[(set (match_operand:SI 0 "arith_reg_operand" "=r")
		  (ashiftrt:SI (match_operand:SI 1 "arith_reg_operand" "r")
			       (match_operand:SI 2 "nonmemory_operand" "M")))
	     (clobber (reg:SI 18))])]
  ""
  "
{
  if (GET_CODE (operands[2]) != CONST_INT ||
      INTVAL (operands[2]) != 1) FAIL;
}
")

(define_insn "lshrsi3_k"
  [(set (match_operand:SI 0 "arith_reg_operand" "=r,r")
	(lshiftrt:SI (match_operand:SI 1 "arith_reg_operand" "0,0")
		     (match_operand:SI 2 "immediate_operand" "K,n")))
   (clobber (reg:SI 18))]
  ""
  "* return output_shift (\"shlr\", operands[0], operands[2], LSHIFTRT);"
  [(set_attr "length" "2,12")
   (set_attr "in_delay_slot" "yes,no")
   (set_attr "type" "arith")])

(define_expand "lshrsi3"
  [(parallel[(set (match_operand:SI 0 "arith_reg_operand" "")
		  (lshiftrt:SI (match_operand:SI 1 "arith_reg_operand" "")
			       (match_operand:SI 2 "nonmemory_operand" "")))
	     (clobber (reg:SI 18))])]
  ""
  "if (!ok_shift_value (operands[2])) FAIL; ")

(define_insn "ashldi3_k"
  [(set (match_operand:DI 0 "arith_reg_operand" "=r")
	(ashift:DI (match_operand:DI 1 "arith_reg_operand" "0")
		   (match_operand:DI 2 "immediate_operand" "I")))
   (clobber (reg:SI 18))]
  ""
  "shll	%R0\;rotcl	%0"
  [(set_attr "length" "4")])

(define_expand "ashldi3"
  [(parallel[(set (match_operand:DI 0 "arith_reg_operand" "")
		  (ashift:DI (match_operand:DI 1 "arith_reg_operand" "")
			     (match_operand:DI 2 "immediate_operand" "")))
	     (clobber (reg:SI 18))])]
	    
  ""
  "{ if (GET_CODE (operands[2]) != CONST_INT 	
	|| INTVAL (operands[2]) != 1) FAIL;} ")

(define_insn "lshrdi3_k"
  [(set (match_operand:DI 0 "arith_reg_operand" "=r")
	(lshiftrt:DI (match_operand:DI 1 "arith_reg_operand" "0")
		     (match_operand:DI 2 "immediate_operand" "I")))
   (clobber (reg:SI 18))]
  ""
  "shlr	%0\;rotcr	%R0"
  [(set_attr "length" "4")])

(define_expand "lshrdi3"
  [(parallel[(set (match_operand:DI 0 "arith_reg_operand" "")
		  (lshiftrt:DI (match_operand:DI 1 "arith_reg_operand" "")
			       (match_operand:DI 2 "immediate_operand" "")))
	     (clobber (reg:SI 18))])]
  ""
  "{ if (GET_CODE (operands[2]) != CONST_INT 
	|| INTVAL (operands[2]) != 1) FAIL;} ")

(define_insn "ashrdi3_k"
  [(set (match_operand:DI 0 "arith_reg_operand" "=r")
	(ashiftrt:DI (match_operand:DI 1 "arith_reg_operand" "0")
		     (match_operand:DI 2 "immediate_operand" "")))
   (clobber (reg:SI 18))]
  ""
  "shar	%0\;rotcr	%R0"
  [(set_attr "length" "4")])

(define_expand "ashrdi3"
  [(parallel[(set (match_operand:DI 0 "arith_reg_operand" "")
		  (ashiftrt:DI (match_operand:DI 1 "arith_reg_operand" "")
			       (match_operand:DI 2 "immediate_operand" "")))
	     (clobber (reg:SI 18))])]
  ""
  "{ if (GET_CODE (operands[2]) != CONST_INT 
	|| INTVAL (operands[2]) != 1) FAIL; } ")



;; -------------------------------------------------------------------------
;; Unary arithmetic
;; -------------------------------------------------------------------------

(define_insn "negdi2"
  [(set (match_operand:DI 0 "arith_reg_operand" "=&r")
	(neg:DI (match_operand:DI 1 "arith_reg_operand" "0")))
   (clobber (reg:SI 18))]
  ""
  "clrt\;negc	%R1,%R0\;negc	%1,%0"
  [(set_attr "length" "6")
   (set_attr "type" "arith")])

(define_insn "negsi2"
  [(set (match_operand:SI 0 "arith_reg_operand" "=r")
	(neg:SI (match_operand:SI 1 "arith_reg_operand" "r")))]
  ""
  "neg	%1,%0"
  [(set_attr "type" "arith")])

(define_insn "one_cmplsi2"
  [(set (match_operand:SI 0 "arith_reg_operand" "=r")
	(not:SI (match_operand:SI 1 "arith_reg_operand" "r")))]
  ""
  "not	%1,%0"
[  (set_attr "type" "arith")])


;; -------------------------------------------------------------------------
;; Zero extension instructions
;; -------------------------------------------------------------------------

(define_insn "zero_extendhisi2"
  [(set (match_operand:SI 0 "arith_reg_operand" "=r")
	(zero_extend:SI (match_operand:HI 1 "arith_reg_operand" "r")))]
  ""
  "extu.w	%1,%0"
  [(set_attr "type" "arith")])

(define_insn "zero_extendqisi2"
  [(set (match_operand:SI 0 "arith_reg_operand" "=r")
	(zero_extend:SI (match_operand:QI 1 "arith_reg_operand" "r")))]
  ""
  "extu.b	%1,%0"
  [(set_attr "type" "arith")]) 

(define_insn "zero_extendqihi2"
  [(set (match_operand:HI 0 "arith_reg_operand" "=r")
	(zero_extend:HI (match_operand:QI 1 "arith_reg_operand" "r")))]
  ""
  "extu.b	%1,%0"
  [(set_attr "type" "arith")]) 


;; -------------------------------------------------------------------------
;; Sign extension instructions
;; -------------------------------------------------------------------------

(define_insn "extendsidi2"
  [(set (match_operand:DI 0 "arith_reg_operand" "=r")
	(sign_extend:DI (match_operand:SI 1 "arith_reg_operand" "0")))]
  ""
  "mov	%1,%0\;shll	%0\;subc	%0,%0"
  [(set_attr "length" "6")]) 

(define_insn "extendhisi2"
  [(set (match_operand:SI 0 "arith_reg_operand" "=r")
	(sign_extend:SI (match_operand:HI 1 "arith_reg_operand" "r")))]
  ""
  "exts.w	%1,%0")

(define_insn "extendqisi2"
  [(set (match_operand:SI 0 "arith_reg_operand" "=r")
	(sign_extend:SI (match_operand:QI 1 "arith_reg_operand" "r")))]
  ""
  "exts.b	%1,%0")

(define_insn "extendqihi2"
  [(set (match_operand:HI 0 "arith_reg_operand" "=r")
	(sign_extend:HI (match_operand:QI 1 "arith_reg_operand" "r")))]
  ""
  "exts.b	%1,%0")


;; -------------------------------------------------------------------------
;; Move instructions
;; -------------------------------------------------------------------------

(define_insn ""
  [(set (match_operand:SI 0 "push_operand" "=<,<")
	(match_operand:SI 1 "arith_reg_operand" "r,l"))]
  ""
  "@
	mov.l	%1,%0
	sts.l	pr,%0"
  [(set_attr "type" "store")])

(define_insn ""
  [(set	(match_operand:SI 0 "arith_reg_operand" "=r,l")
	(match_operand:SI 1 "pop_operand" "=>,>"))]
  ""
  "@
	mov.l	%1,%0
	lds.l	%1,pr"
  [(set_attr "type" "load,pload")])

(define_insn "push"
  [(set (mem:SI (pre_dec:SI (reg:SI 15)))
	(match_operand:SI 0 "register_operand" "r,l"))]
  ""
  "@
	mov.l	%0,@-r15
	sts.l	pr,@-r15")

(define_insn "pop"
  [(set (match_operand:SI 0 "register_operand" "=r,l")
	(mem:SI (post_inc:SI (reg:SI 15))))]
  ""
  "@
	mov.l	@r15+,%0
	lds.l	@r15+,pr"
  [(set_attr "type" "load,pload")])

; some constants are easier to generate with alu operations
; rather than loading from memory

(define_split
  [(set (match_operand:SI 0 "register_operand" "=r")
	(match_operand:SI 1 "painful_immediate_operand" "i"))]
  ""
  [(set (match_dup 0) (const_int 127))
   (set (match_dup 0) (plus:SI (match_dup 0)
			       (match_dup 2)))]
  "operands[2] = GEN_INT (INTVAL(operands[1]) - 127);" )

(define_insn "movsi_i"
  [(set (match_operand:SI 0 "general_operand" "=r,r,r,m,l,r,r,r,t,x")
	(match_operand:SI 1 "general_movsrc_operand"  "r,I,m,r,r,l,t,x,r,r"))]
  ""
  "@
	mov	%1,%0
	mov	%1,%0
	mov.l	%1,%0
	mov.l	%1,%0
	lds	%1,%0
	sts	%1,%0
        movt	%0
	sts	%1,%0
	tst	%1,%1\;bt	T%*\;bra	F%*\;sett\;T%*:clrt\;F%*:%^
	lds	%1,%0"
  [(set_attr "length" "2,2,2,2,2,2,2,2,10,2")
   (set_attr "type" "move,move,load,pload,move,move,move,move,move,move")])

(define_insn "movsi_pcrel"
  [(set (match_operand:SI 0 "arith_reg_operand" "=r")
	(match_operand:SI 1 "hard_immediate_operand" "i"))]
  ""
  "* return output_movepcrel (insn, operands, SImode);"
  [(set_attr "length" "2")
   (set_attr "in_delay_slot" "no")
   (set_attr "constneed" "yes")
   (set_attr "smallestsize" "2")
   (set_attr "largestsize" "8")
   (set_attr "type" "load")])

(define_expand "movsi"
  [(set (match_operand:SI 0 "general_operand" "")
	(match_operand:SI 1 "general_operand" ""))]
  ""
  "{ if(prepare_move_operands(operands, SImode)) DONE; } ")

(define_insn "movqi_i"
  [(set (match_operand:QI 0 "general_operand" "=r,r,r,m,r,m,r,r")
	(match_operand:QI 1 "general_operand"  "r,n,m,r,m,r,x,t"))]
  ""
  "@
	mov	%1,%0
	mov	%1,%0
	mov.b	%1,%0 
	mov.b	%1,%0 
	mov.b	%1,%0 
	mov.b	%1,%0 
	sts	%1,%0
	movt	%0")

(define_expand "movqi"
  [(set (match_operand:QI 0 "general_operand" "")
	(match_operand:QI 1 "general_operand"  ""))]
  ""
  "prepare_move_operands(operands, QImode);")
  
(define_insn "movhi_pcrel"
  [(set (match_operand:HI 0 "arith_reg_operand" "=r")
	(match_operand:HI 1 "hard_immediate_operand" "i"))]
  ""
  "* return output_movepcrel (insn, operands, SImode);"
  [(set_attr "length" "2")
   (set_attr "in_delay_slot" "no")
   (set_attr "constneed" "yes")
   (set_attr "smallestsize" "2")
   (set_attr "largestsize" "8")
   (set_attr "type" "load")])

(define_insn "movhi_i"
  [(set (match_operand:HI 0 "general_operand" "=r,r,m,z,m,r,r")
	(match_operand:HI 1 "general_operand"  "rI,m,r,m,z,x,t"))]
  ""
  "@
	mov	%1,%0
	mov.w	%1,%0
	mov.w	%1,%0
	mov.w	%1,%0
	mov.w	%1,%0
	sts	%1,%0
	movt	%0"
  [(set_attr "type" "move,load,store,load,store,move,move")])

(define_expand "movhi"
  [(set (match_operand:HI 0 "general_operand" "")
	(match_operand:HI 1 "general_operand"  ""))]
  ""
  "prepare_move_operands (operands, HImode);")
  
(define_insn ""
  [(set (match_operand:DI 0 "push_operand" "=<")
	(match_operand:DI 1 "arith_reg_operand" "r"))]
   ""
   "mov.l	%R1,%0\;mov.l	%1,%0"
   [(set_attr "length" "4")
    (set_attr "type" "store")])

(define_insn "movdi_pcrel"
  [(set (match_operand:DI 0 "arith_reg_operand" "=r")
	(match_operand:DI 1 "hard_immediate_operand" "i"))]
  ""
  "* return output_movepcrel (insn, operands, DImode);"
  [(set_attr "length" "4")
   (set_attr "in_delay_slot" "no")
   (set_attr "constneed" "yes")
   (set_attr "smallestsize" "4")
   (set_attr "constantsize" "8")
   (set_attr "largestsize" "18")
   (set_attr "type" "load")])

(define_insn "movdi_k"
  [(set (match_operand:DI 0 "general_operand" "=r,r,m,r,r,m,r")
	(match_operand:DI 1 "general_operand" "r,m,r,I,m,r,x"))]
  ""
  "* return output_movedouble(operands, DImode);"
  [(set_attr "length" "4")
   (set_attr "type" "move,load,store,move,load,store,load")])


(define_expand "movdi"
  [(set (match_operand:DI 0 "general_operand" "")
	(match_operand:DI 1 "general_operand" ""))]
  ""
  "prepare_move_operands (operands, DImode);")

(define_insn ""
  [(set (match_operand:DF 0 "push_operand" "=<")
	(match_operand:DF 1 "arith_reg_operand" "r"))]
   ""
   "mov.l	%R1,%0\;mov.l	%1,%0"
   [(set_attr "length" "4")
    (set_attr "type" "store")])

(define_insn "movdf_pcrel"
  [(set (match_operand:DF 0 "arith_reg_operand" "=r")
	(match_operand:DF 1 "hard_immediate_operand" "i"))]
  ""
  "* return output_movepcrel  (insn, operands, DFmode);"
  [(set_attr "length" "4")
   (set_attr "in_delay_slot" "no")
   (set_attr "constneed" "yes")
   (set_attr "smallestsize" "4")
   (set_attr "constantsize" "8")
   (set_attr "largestsize" "18")
   (set_attr "type" "load")])

(define_insn "movdf_k"
  [(set (match_operand:DF 0 "general_operand" "=r,r,m")
	(match_operand:DF 1 "general_operand" "r,m,r"))]
  ""
  "* return output_movedouble(operands, DFmode);"
  [(set_attr "length" "4")
   (set_attr "type" "move,load,store")])

(define_expand "movdf"
  [(set (match_operand:DF 0 "general_operand" "")
	(match_operand:DF 1 "general_operand" ""))]
  ""
  "prepare_move_operands(operands, DFmode);")

(define_insn ""
  [(set (match_operand:SF 0 "push_operand" "=<")
	(match_operand:SF 1 "arith_reg_operand" "r"))]
   ""
   "mov.l	%1,%0"
  [(set_attr "type" "store")])

(define_insn "movsf_pcrel"
  [(set (match_operand:SF 0 "arith_reg_operand" "=r")
	(match_operand:SF 1 "hard_immediate_operand" "i"))]
  ""
  "* return output_movepcrel (insn, operands, SFmode);"
  [(set_attr "length" "2")
   (set_attr "in_delay_slot" "no")
   (set_attr "constneed" "yes")
   (set_attr "smallestsize" "2")
   (set_attr "largestsize" "8")
   (set_attr "type" "load")])
		
(define_insn "movsf_i"
  [(set (match_operand:SF 0 "general_operand" "=r,r,r,m,l,r,m,r")
	(match_operand:SF 1 "general_operand"  "r,I,m,r,r,l,r,m"))]
  ""
  "@
	mov	%1,%0
	mov	%1,%0
	mov.l	%1,%0
	mov.l	%1,%0
	lds	%1,%0
	sts	%1,%0
	mov	%1,%0
	mov	%1,%0"
  [(set_attr "type" "move,move,load,store,move,move,move,move")])

(define_expand "movsf"
  [(set (match_operand:SF 0 "general_operand" "")
	(match_operand:SF 1 "general_operand" ""))]
  ""
  "prepare_move_operands(operands, SFmode);")


;; ------------------------------------------------------------------------
;; Define the real conditional branch instructions.
;; ------------------------------------------------------------------------

(define_insn "branch_true"
  [(set (pc) (if_then_else (eq (reg:SI 18) (const_int 1))
			   (label_ref (match_operand 0 "" ""))
			   (pc)))]
  ""
  "* return output_branch (1, insn);"
  [(set_attr "type" "cbranch")])

(define_insn "branch_false"
  [(set (pc) (if_then_else (ne (reg:SI 18) (const_int 1))
			   (label_ref (match_operand 0 "" ""))
			   (pc)))]
  ""
  "* return output_branch (0, insn);"
  [(set_attr "type" "cbranch")])

(define_insn "inverse_branch_true"
  [(set (pc) (if_then_else (eq (reg:SI 18) (const_int 1))
			   (pc)
			   (label_ref (match_operand 0 "" ""))))]
  ""
  "* return output_branch (0, insn);"
  [(set_attr "type" "cbranch")])

(define_insn "inverse_branch_false"
  [(set (pc) (if_then_else (ne (reg:SI 18) (const_int 1))
   			   (pc)
			   (label_ref (match_operand 0 "" ""))))]
  ""
  "* return output_branch (1, insn);"
  [(set_attr "type" "cbranch")])


;; Conditional branch insns

(define_expand "beq"
  [(set (reg:SI 18) (eq:SI (match_dup 1) (match_dup 2)))
   (set (pc)
	(if_then_else (eq (reg:SI 18)
			  (const_int 1))
		      (label_ref (match_operand 0 "" ""))
		      (pc)))]
  ""
  "
{
  operands[1] = sh_compare_op0;
  operands[2] = sh_compare_op1;
}")


; There is no bne compare, so we reverse the branch arms.

(define_expand "bne"
  [(set (reg:SI 18) (eq:SI (match_dup 1) (match_dup 2)))
   (set (pc)
	(if_then_else (eq (reg:SI 18)
			  (const_int 1))
		      (pc)
		      (label_ref (match_operand 0 "" ""))))]
  ""
  "
{
  operands[1] = sh_compare_op0;
  operands[2] = sh_compare_op1;
}")

(define_expand "bgt"
  [(set (reg:SI 18) (gt:SI (match_dup 1) (match_dup 2)))
   (set (pc)
	(if_then_else (eq (reg:SI 18)
			  (const_int 1))
		      (label_ref (match_operand 0 "" ""))
		      (pc)))]
  ""
  "
{
  operands[1] = sh_compare_op0;
  operands[2] = sh_compare_op1;
}")

(define_expand "blt"
  [(set (reg:SI 18) (lt:SI (match_dup 1) (match_dup 2)))
   (set (pc)
	(if_then_else (eq (reg:SI 18)
			  (const_int 1))
		      (label_ref (match_operand 0 "" ""))
		      (pc)))]
  ""
  "
{
  operands[1] = sh_compare_op0;
  operands[2] = sh_compare_op1;
}")

(define_expand "ble"
  [(set (reg:SI 18) (le:SI (match_dup 1) (match_dup 2)))
   (set (pc)				
	(if_then_else (eq (reg:SI 18)
			  (const_int 1))
		      (label_ref (match_operand 0 "" ""))
		      (pc)))]
  ""
  "
{
  operands[1] = sh_compare_op0;
  operands[2] = sh_compare_op1;
}")

(define_expand "bge"
  [(set (reg:SI 18) (ge:SI (match_dup 1) (match_dup 2)))
   (set (pc)
	(if_then_else (eq (reg:SI 18)
			  (const_int 1))
		      (label_ref (match_operand 0 "" ""))
		      (pc)))]
  ""
  "
{
  operands[1] = sh_compare_op0;
  operands[2] = sh_compare_op1;
}")

(define_expand "bgtu"
  [(set (reg:SI 18) (gtu:SI (match_dup 1) (match_dup 2)))
   (set (pc)
	(if_then_else (eq (reg:SI 18)
			   (const_int 1))
		      (label_ref (match_operand 0 "" ""))
		      (pc)))]
  ""
  "
{
  operands[1] = sh_compare_op0;
  operands[2] = sh_compare_op1;
}")

(define_expand "bltu"
  [(set (reg:SI 18) (ltu:SI (match_dup 1) (match_dup 2)))
   (set (pc)
	(if_then_else (eq (reg:SI 18)
			   (const_int 1))
		      (label_ref (match_operand 0 "" ""))
		      (pc)))]
  ""
  "
{
  operands[1] = sh_compare_op0;
  operands[2] = sh_compare_op1;
}")

(define_expand "bgeu"
  [(set (reg:SI 18) (geu:SI (match_dup 1) (match_dup 2)))
   (set (pc)
	(if_then_else (eq (reg:SI 18)
			   (const_int 1))
		      (label_ref (match_operand 0 "" ""))
		      (pc)))]
  ""
  "
{
  operands[1] = sh_compare_op0;
  operands[2] = sh_compare_op1;
}")

(define_expand "bleu"
  [(set (reg:SI 18) (leu:SI (match_dup 1) (match_dup 2)))
   (set (pc)
	(if_then_else (eq (reg:SI 18)
			   (const_int 1))
		      (label_ref (match_operand 0 "" ""))
		      (pc)))]
  ""
  "
{
  operands[1] = sh_compare_op0;
  operands[2] = sh_compare_op1;
}")


;; ------------------------------------------------------------------------
;; Jump and linkage insns
;; ------------------------------------------------------------------------

(define_insn "jump_real"
  [(set (pc)
	(label_ref (match_operand 0 "" "")))]
  ""
  "*
{
  if (get_attr_length(insn) == 6) 
    {
      return \"mov.l	%I0,r13\;jmp	@r13%#\";
    }
  else
    {
      return   \"bra	%l0%#\";
    }
}"
  [(set_attr "type" "jump")
   (set_attr "needs_delay_slot" "yes")])

(define_expand "jump"
 [(set (pc) (label_ref (match_operand 0 "" "")))]
 ""
 "
{
  emit_insn(gen_jump_real(operand0));
  DONE;
}
")

(define_insn "calli"
  [(call (mem:SI (match_operand:SI 0 "arith_reg_operand" "r"))
	 (match_operand 1 "" ""))
   (clobber (reg:SI 17))]
  ""
  "jsr	@%0%#"
  [(set_attr "needs_delay_slot" "yes")
   (set_attr "in_delay_slot" "no")
   (set_attr "length" "4")])

(define_insn "call_valuei"
  [(set (match_operand 0 "" "=rf")
	(call (mem:SI (match_operand:SI 1 "arith_reg_operand" "r"))
	      (match_operand 2 "" "")))
   (clobber (reg:SI 17))]
  ""
  "jsr	@%1%#"
  [(set_attr "needs_delay_slot" "yes")
   (set_attr "in_delay_slot" "no")
   (set_attr "length" "4")])

(define_expand "call"
  [(parallel[(call (match_operand 0 "arith_reg_operand" "o")
		   (match_operand 1 "" ""))
	     (clobber (reg:SI 17))])]
  ""
  "
{
  if (GET_CODE (operands[0]) == MEM) 
    {
      operands[0] 
	= gen_rtx(MEM,GET_MODE (operands[0]),
		  force_reg (Pmode,
			    XEXP (operands[0], 0)));
    }
}")

(define_expand "call_value"
  [(parallel[(set (match_operand 0 "" "=rf")
		  (call (match_operand 1 "arith_reg_operand" "o")
			(match_operand 2 "" "")))
	     (clobber (reg:SI 17))])]
  ""
  "
{
  if (GET_CODE (operands[1]) == MEM) 
    {
      operands[1] 
	= gen_rtx (MEM, GET_MODE (operands[1]),
		   force_reg (Pmode,
			      XEXP (operands[1], 0)));
    }
}")

(define_insn "indirect_jump"
  [(set (pc)
	(match_operand:SI 0 "arith_reg_operand" "r"))]
  ""
  "jmp	@%0%#"
  [(set_attr "needs_delay_slot" "yes")
   (set_attr "in_delay_slot" "no")
   (set_attr "length" "4")])



;; ------------------------------------------------------------------------
;; Misc insns
;; ------------------------------------------------------------------------


(define_insn "nop"
  [(const_int 0)]
  ""
  "or	r0,r0")

(define_insn "tablejump"
  [(set (pc)
	(match_operand:SI 0 "arith_reg_operand" "r"))
   (use (label_ref (match_operand 1 "" "")))]
  ""
  "!table jump\;jmp	@%0\;or	r0,r0\;%!"
  [(set_attr "needs_delay_slot" "no")
   (set_attr "in_delay_slot" "no")
   (set_attr "type" "jump")
   (set_attr "dump" "no")])

(define_insn "return"
 [(return)]
 "reload_completed"
 "rts	%#"
 [(set_attr "type" "return")
  (set_attr "needs_delay_slot" "yes")
  (set_attr "dump" "yes")])

(define_expand "prologue"
  [(const_int 0)]
  ""
  "sh_expand_prologue (); DONE;")

(define_expand "epilogue"
  [(return)]
  ""
  "sh_expand_epilogue ();")

(define_insn "blockage"
  [(unspec_volatile [(const_int 0)] 0)]
  ""
  ""
  [(set_attr "length" "0")])


;; ------------------------------------------------------------------------
;; Scc instructions
;; ------------------------------------------------------------------------

(define_insn ""
  [(set (match_operand:SI 0 "arith_reg_operand" "=r")
	(eq (reg:SI 18) (const_int 1)))]
  ""
  "movt	%0 ! ")

(define_expand "seq"
  [(set (match_operand:SI 0 "arith_reg_operand" "")
	(match_dup 1))]
  ""
  "operands[1] = prepare_scc_operands (EQ);")

(define_expand "slt"
  [(set (match_operand:SI 0 "arith_reg_operand" "")
	(match_dup 1))]
  ""
  "operands[1] = prepare_scc_operands (LT);")

(define_expand "sle"
  [(set (match_operand:SI 0 "arith_reg_operand" "")
	(match_dup 1))]
  ""
  "operands[1] = prepare_scc_operands (LE);")

(define_expand "sgt"
  [(set (match_operand:SI 0 "arith_reg_operand" "")
	(match_dup 1))]
  ""
  "operands[1] = prepare_scc_operands (GT);")

(define_expand "sge"
  [(set (match_operand:SI 0 "arith_reg_operand" "")
	(match_dup 1))]
  ""
  "operands[1] = prepare_scc_operands (GE);")

(define_expand "sgtu"
  [(set (match_operand:SI 0 "arith_reg_operand" "")
	(match_dup 1))]
  ""
  "operands[1] = prepare_scc_operands (GTU);")

(define_expand "sltu"
  [(set (match_operand:SI 0 "arith_reg_operand" "")
	(match_dup 1))]
  ""
  "operands[1] = prepare_scc_operands (LTU);")

(define_expand "sleu"
  [(set (match_operand:SI 0 "arith_reg_operand" "")
	(match_dup 1))]
  ""
  "operands[1] = prepare_scc_operands (LEU);")

(define_expand "sgeu"
  [(set (match_operand:SI 0 "arith_reg_operand" "")
	(match_dup 1))]
  ""
  "operands[1] = prepare_scc_operands (GEU);")

(define_expand "sne"
  [(set (match_operand:SI 0 "arith_reg_operand" "")
	(match_dup 1))
   (set (match_dup 0) (xor:SI (match_dup 0) (const_int 1)))]
  ""
  "operands[1] = prepare_scc_operands (EQ);")

; these patterns give better code then gcc invents if
; left to its own devices

(define_insn "anddi3"
  [(set (match_operand:DI 0 "arith_reg_operand" "=r")
	(and:DI (match_operand:DI 1 "arith_reg_operand" "%0")
		(match_operand:DI 2 "arith_reg_operand" "r")))]
  ""
  "and	%2,%0\;and	%R2,%R0"
  [(set_attr "length" "4")])

(define_insn "iordi3"
  [(set (match_operand:DI 0 "arith_reg_operand" "=r")
	(ior:DI (match_operand:DI 1 "arith_reg_operand" "%0")
		(match_operand:DI 2 "arith_reg_operand" "r")))]
  ""
  "or	%2,%0\;or	%R2,%R0"
  [(set_attr "length" "4")])

(define_insn "xordi3"
  [(set (match_operand:DI 0 "arith_reg_operand" "=r")
	(xor:DI (match_operand:DI 1 "arith_reg_operand" "%0")
		(match_operand:DI 2 "arith_reg_operand" "r")))]
  ""
  "xor	%2,%0\;xor	%R2,%R0"
  [(set_attr "length" "4")])


;; ------------------------------------------------------------------------
;; Block move
;; ------------------------------------------------------------------------

(define_expand "movstrsi"
  [(parallel [(set (mem:BLK (match_operand:BLK 0 "general_operand" ""))
		   (mem:BLK (match_operand:BLK 1 "general_operand" "")))
	     (use (match_operand:SI 2 "general_operand" ""))
	     (use (match_operand:SI 3 "immediate_operand" ""))
  ])]
  ""
  "
{
  rtx dest_mem = operands[0];
  rtx src_mem = operands[1];
  operands[0] = copy_to_mode_reg (SImode, XEXP (operands[0], 0));
  operands[1] = copy_to_mode_reg (SImode, XEXP (operands[1], 0));
  expand_block_move (dest_mem, src_mem, operands);
  DONE;
}")




   
;; -------------------------------------------------------------------------
;; Peepholes
;; -------------------------------------------------------------------------


(define_peephole 
  [(set (match_operand:QI 0 "arith_reg_operand" "")
	(mem:QI (match_operand:SI 1 "arith_reg_operand" "")))
   (set (match_dup 1) (plus:SI (match_dup 1) (const_int 1)))]
  "REGNO(operands[1]) != REGNO(operands[0])"
  "mov.b	@%1+,%0")

(define_peephole 
  [(set (match_operand:HI 0 "arith_reg_operand" "")
	(mem:HI (match_operand:SI 1 "arith_reg_operand" "")))
   (set (match_dup 1) (plus:SI (match_dup 1) (const_int 2)))]
  "REGNO(operands[1]) != REGNO(operands[0])"
  "mov.w	@%1+,%0")

(define_peephole 
  [(set (match_operand:SI 0 "arith_reg_operand" "")
	(mem:SI (match_operand:SI 1 "arith_reg_operand" "")))
   (set (match_dup 1) (plus:SI (match_dup 1) (const_int 4)))]
  "REGNO(operands[1]) != REGNO(operands[0])"
  "mov.l	@%1+,%0")

(define_peephole
  [(set (match_operand:QI 0 "register_operand" "=r")
	(match_operand:QI 1 "general_operand" "g"))
   (set (match_operand:SI 2 "register_operand" "=r")
	(sign_extend:SI (match_dup 0)))]
  "REGNO(operands[0]) == REGNO(operands[2])"
  "mov.b	%1,%0")

(define_peephole 
  [(set (match_operand:QI 0 "register_operand" "=r")
	(match_operand:QI 1 "general_operand" "g"))
   (set (match_operand:SI 2 "register_operand" "=r")
	(sign_extend:SI (match_dup 0)))]
  "REGNO(operands[0]) != REGNO(operands[2]) 
   && dead_or_set_p (insn, operands[0])"
  "mov.b	%1,%2")

; notice when a mov.b could be done with a displacement

(define_peephole
  [(set (match_operand:SI 0 "register_operand" "=r")
	(plus:SI (match_dup 0)
		 (match_operand:SI 1 "byte_index_operand" "i")))
   (set (mem:QI (match_dup 0))	(reg:QI 0))]
  "dead_or_set_p(insn, operands[0])"
  "mov.b	r0,@(%O1,%0)")

(define_peephole
  [(set (match_operand:SI 0 "register_operand" "=r")
	(plus:SI (match_dup 0)
		 (match_operand:SI 1 "byte_index_operand" "i")))
   (set (reg:QI 0) (mem:QI (match_dup 0)))]
  "dead_or_set_p(insn, operands[0])"
  "mov.b	@(%O1,%0),r0")
  



