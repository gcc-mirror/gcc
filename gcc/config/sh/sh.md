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



;; Special constraints for SH machine description:
;;
;;    t -- T 
;;    x -- mac 
;;    l -- pr 
;;    z -- r0
;;
;; Special formats used for outputting SH instructions:
;;
;;   %.  --  print a .s if insn needs delay slot
;;   %*  --  print a local label
;;   %^  --  increment the local label number
;;   %#  --  output a nop if there is nothing to put in the delay slot
;;   %R  --  print the next register or memory location along, ie the lsw in
;;           a double word value
;;   %O  --  print a constant without the #
;;   %M  --  print a constant as its negative
;;
;;
;; Special predicates:
;;
;;  arith_operand          -- operand is valid source for arithmetic op
;;  arith_reg_operand      -- operand is valid register for arithmetic op
;;  byte_index_operand     -- operand is ok as an index in a mov.b
;;  general_movdst_operand -- operand is valid move destination
;;  general_movsrc_operand -- operand is valid move source
;;  logical_operand        -- operand is valid source for logical op
;;  pop_operand            -- operand is a pop from the stack
;;  system_reg_operand     -- operand is MACL, MACH, or PR
;; -------------------------------------------------------------------------
;; Attributes
;; -------------------------------------------------------------------------

; Target CPU.

(define_attr "cpu" "sh0,sh1,sh2,sh3"
  (const (symbol_ref "sh_cpu")))

(define_attr "interrupt_function" "no,yes"
  (const (symbol_ref "pragma_interrupt")))
;;
;; cbranch	conditional branch instructions
;; jump		unconditional jumps
;; arith	ordinary arithmetic
;; load		from memory
;; store	to memory
;; move		register to register
;; smpy		single precision integer multiply
;; dmpy		double precision integer multiply
;; return	rts
;; pload	load of pr reg (can't be put into delay slot of rts)
;; pcloadsi	pc relative load of SI value
;; pcloadhi 	pc relative load of HI value
;; rte		return from exception
;; sfunc	special function call with known used registers

(define_attr "type" 
 "cbranch,jump,arith,other,load,store,move,smpy,dmpy,return,pload,pcloadsi,pcloadhi,rte,sfunc"
  (const_string "other"))

; If a conditional branch destination is within -120..120 bytes away 
; from the instruction it can be 2 bytes long.  Something in the
; range -4090..4090 bytes can be 6 bytes long, all other conditional
; branches are 8 bytes long.

; An unconditional jump which can reach forward or back 4k can be 
; 6 bytes long (including the delay slot).  If it is too big, it
; must be 10 bytes long.

; If a pcrel instruction is within 500 bytes of the constant, then the insn is 
; 2 bytes long, otherwise 12 bytes
; All other instructions are two bytes long by default.

(define_attr "length" "" 
  (cond [(eq_attr "type" "cbranch")
	 (if_then_else (and (ge (minus (pc) (match_dup 0))
				(const_int -122))
			    (le (minus (pc) (match_dup 0))
				(const_int 122)))
		       (const_int 2)
		       (if_then_else (and (ge (minus (pc) (match_dup 0))
					      (const_int -4090))
					  (le (minus (pc) (match_dup 0))
					      (const_int 4090)))
				     (const_int 6)
				     (const_int 16)))

	 (eq_attr "type" "jump")
	 (if_then_else (and (ge (minus (pc) (match_dup 0))
				(const_int -4090))
			    (le (minus (pc) (match_dup 0))
				(const_int 4090)))
		       (const_int 4)
		       (const_int 10))
	 (eq_attr "type" "pcloadsi")
	 (if_then_else (gt (pc) (minus (match_dup 0) (const_int 1000)))
		       (const_int 2)
		       (const_int 12))
	 (eq_attr "type" "pcloadhi")
	 (if_then_else (gt (pc) (minus (match_dup 0) (const_int 500)))
		       (const_int 2)
		       (const_int 12))

	 ] (const_int 2)))

;; (define_function_unit {name} {num-units} {n-users} {test}
;;                       {ready-delay} {issue-delay} [{conflict-list}])
				      
(define_function_unit "memory" 1 0 (eq_attr "type" "load,pcloadsi,pcloadhi") 2 0)
(define_function_unit "mpy"    1 0 (eq_attr "type" "smpy") 3 0)
(define_function_unit "mpy"    1 0 (eq_attr "type" "dmpy") 5 0)

(define_attr "needs_delay_slot" "yes,no"
  (cond [(eq_attr "type" "jump")   (const_string "yes")
	 (eq_attr "type" "return") (const_string "yes")]
	(const_string "no")))

(define_delay 
  (eq_attr "needs_delay_slot" "yes")
  [(eq_attr "in_delay_slot" "yes") (nil) (nil)])

(define_delay 
  (eq_attr "type" "return")
  [(and (eq_attr "in_delay_slot" "yes") 
	(ior (eq_attr "interrupt_function" "no")
	    (eq_attr "hit_stack" "no"))) (nil) (nil)])

(define_attr "hit_stack" "yes,no" (const_string "no"))

(define_delay 
  (and (eq_attr "type" "cbranch") 
       (eq_attr "cpu" "sh2"))
  [(eq_attr "in_delay_slot" "yes") (nil) (nil)])

(define_attr "in_delay_slot" "maybe,yes,no" 
  (cond [(eq_attr "type" "cbranch") (const_string "no")
	 (eq_attr "type" "jump") (const_string "no")
	 (eq_attr "type" "pload") (const_string "no")
	 (eq_attr "type" "pcloadsi") (const_string "no")
	 (eq_attr "type" "pcloadhi") (const_string "no")
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
  [(set (reg:SI 18) (gt:SI (match_operand:SI 0 "arith_reg_operand" "r")
			   (const_int 0)))]
  ""
  "cmp/pl	%0")

(define_insn ""
  [(set (reg:SI 18) (ge:SI (match_operand:SI 0 "arith_reg_operand" "r")
			   (const_int 0)))]
  ""
  "cmp/pz	%0")

(define_insn "cmpeq_0"
  [(set (reg:SI 18) (eq:SI (match_operand:SI 0 "arith_reg_operand" "r")
			   (const_int 0)))]
  ""
  "tst	%0,%0 ! t0")

(define_insn "cmpeqsi_t"
  [(set (reg:SI 18) (eq:SI (match_operand:SI 0 "arith_operand" "r,N,z,r")
			   (match_operand:SI 1 "arith_operand" "N,r,rI,r")))]
  ""
  "@
	tst	%0,%0 !t1
	tst	%1,%1 !t2
	cmp/eq	%1,%0
	cmp/eq	%1,%0")

(define_insn "cmpgtsi_t"
  [(set (reg:SI 18) (gt:SI (match_operand:SI 0 "arith_reg_operand" "r,r")
			   (match_operand:SI 1 "arith_operand" "N,r")))]
  ""
  "@
	cmp/pl	%0
	cmp/gt	%1,%0")

(define_insn "cmpgesi_t"
  [(set (reg:SI 18) (ge:SI (match_operand:SI 0 "arith_reg_operand" "r,r")
			   (match_operand:SI 1 "arith_operand" "N,r")))]
  ""
  "@
	cmp/pz	%0
	cmp/ge	%1,%0")


;; -------------------------------------------------------------------------
;; SImode unsigned integer comparisons
;; -------------------------------------------------------------------------

(define_insn "cmpgeusi_t"
  [(set (reg:SI 18) (geu:SI (match_operand:SI 0 "arith_reg_operand" "r,r")
			    (match_operand:SI 1 "arith_operand" "N,r")))]
  ""
  "@
	cmp/pz	%1
	cmp/hs	%1,%0")

(define_insn "cmpgtusi_t"
  [(set (reg:SI 18) (gtu:SI (match_operand:SI 0 "arith_operand" "r,r")
			    (match_operand:SI 1 "arith_operand" "N,r")))]
  ""
  "@
	cmp/pl	%1
	cmp/hi	%1,%0")

;; We save the compare operands in the cmpxx patterns and use them when
;; we generate the branch.

(define_expand "cmpsi"
  [(set (reg:SI 18) (compare (match_operand:SI 0 "arith_operand" "")
			     (match_operand:SI 1 "arith_operand" "")))]
  ""
  "
{
  sh_compare_op0 = operands[0];
  sh_compare_op1 = operands[1];
  DONE;
}")


;; -------------------------------------------------------------------------
;; Addition instructions
;; -------------------------------------------------------------------------

;; this should be a define split.

(define_insn "adddi3"
  [(set (match_operand:DI 0 "register_operand" "=r")
	(plus:DI (match_operand:DI 1 "register_operand" "%0")
		 (match_operand:DI 2 "register_operand" "r")))
   (clobber (reg:SI 18))]
  ""
  "clrt\;addc	%R2,%R0\;addc	%2,%0"
  [(set_attr "length" "6")])


(define_insn "addsi3_real"
  [(set (match_operand:SI 0 "arith_reg_operand" "=r")
	(plus:SI (match_operand:SI 1 "arith_reg_operand" "%0")
		 (match_operand:SI 2 "arith_operand" "rI")))]
  ""
  "add	%2,%0"
  [(set_attr "length" "2")
   (set_attr "type" "arith")])

(define_expand "addsi3"
  [(set (match_operand:SI 0 "arith_reg_operand" "=r")
	(plus:SI (match_operand:SI 1 "arith_operand" "%0")
		 (match_operand:SI 2 "arith_operand" "rI")))]
  ""
  "")


;; -------------------------------------------------------------------------
;; Subtraction instructions
;; -------------------------------------------------------------------------


(define_insn "subdi3"
  [(set (match_operand:DI 0 "register_operand" "=r")
	(minus:DI (match_operand:DI 1 "register_operand" "0")
		 (match_operand:DI 2 "register_operand" "r")))
   (clobber (reg:SI 18))]
  ""
  "clrt\;subc	%R2,%R0\;subc	%2,%0"
  [(set_attr "length" "6")])

(define_insn "subsi3"
  [(set (match_operand:SI 0 "arith_reg_operand" "=r")
	(minus:SI (match_operand:SI 1 "arith_reg_operand" "0")
		  (match_operand:SI 2 "arith_operand" "r")))]
  ""
  "sub	%2,%0"
  [(set_attr "type" "arith")])


;; -------------------------------------------------------------------------
;; Division instructions
;; -------------------------------------------------------------------------


;; we take advantage of the library routines which don't clobber as many
;; registers as a normal function call would.


(define_insn ""
  [(set (reg:SI 0)
	(udiv:SI (reg:SI 4) (reg:SI 5)))
   (clobber (reg:SI 18))
   (clobber (reg:SI 17))
   (clobber (reg:SI 6))
   (clobber (reg:SI 4))
   (use (match_operand:SI 0 "register_operand" "r"))]
  ""
  "jsr	@%0%#"
  [(set_attr "type" "sfunc")
   (set_attr "length" "4")
   (set_attr "needs_delay_slot" "yes")])

(define_expand "udivsi3"
  [(set (reg:SI 4) (match_operand:SI 1 "general_operand" "g"))
   (set (reg:SI 5) (match_operand:SI 2 "general_operand" "g"))
   (set (match_dup 3) (symbol_ref:SI "__udivsi3"))
   (parallel[(set (reg:SI 0)
		  (udiv:SI (reg:SI 4)
			   (reg:SI 5)))
	     (clobber (reg:SI 18))
	     (clobber (reg:SI 17))
	     (clobber (reg:SI 6))
	     (clobber (reg:SI 4))
	     (use (match_dup 3))])
   (set (match_operand:SI 0 "general_operand" "=g") 
	(reg:SI 0))]
  ""
  "operands[3] = gen_reg_rtx(SImode);")


(define_insn ""
  [(set (reg:SI 0)
	(div:SI (reg:SI 4) (reg:SI 5)))
   (clobber (reg:SI 18))
   (clobber (reg:SI 17))
   (clobber (reg:SI 1))
   (clobber (reg:SI 2))
   (clobber (reg:SI 3))
   (use (match_operand:SI 0 "register_operand" "r"))]
  ""
  "jsr	@%0%#"
  [(set_attr "type" "sfunc")
   (set_attr "length" "4")
   (set_attr "needs_delay_slot" "yes")])

(define_expand "divsi3"
  [(set (reg:SI 4) (match_operand:SI 1 "general_operand" "g"))
   (set (reg:SI 5) (match_operand:SI 2 "general_operand" "g"))
   (set (match_dup 3) (symbol_ref:SI "__sdivsi3"))
   (parallel[(set (reg:SI 0)
		  (div:SI (reg:SI 4)
			   (reg:SI 5)))
	     (clobber (reg:SI 18))
	     (clobber (reg:SI 17))
	     (clobber (reg:SI 1))
	     (clobber (reg:SI 2))
	     (clobber (reg:SI 3))
	     (use (match_dup 3))])
   (set (match_operand:SI 0 "general_operand" "=g") 
	(reg:SI 0))]
  ""
  "operands[3] = gen_reg_rtx(SImode);")
	    

;; -------------------------------------------------------------------------
;; Multiplication instructions
;; -------------------------------------------------------------------------

(define_insn ""
  [(set (reg:SI 21)
	(mult:SI (zero_extend:SI (match_operand:HI 1 "arith_reg_operand" "r"))
		 (zero_extend:SI (match_operand:HI 2 "arith_reg_operand" "r"))))]
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
		  (match_operand:HI 1 "mac_operand" "r"))
		 (sign_extend:SI
		  (match_operand:HI 2 "mac_operand" "r"))))
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

;; mulsi3 on the SH2 can be done in one instruction, on the SH1 we generate
;; a call to a routine which clobbers known registers.

(define_insn ""
  [(set (reg:SI 0)
	(mult:SI (reg:SI 4) (reg:SI 5)))
   (clobber (reg:SI 21))
   (clobber (reg:SI 18))
   (clobber (reg:SI 17))
   (clobber (reg:SI 3))
   (clobber (reg:SI 2))
   (clobber (reg:SI 1))
   (use (match_operand:SI 0 "register_operand" "r"))]
  ""
  "jsr	@%0%#"
  [(set_attr "type" "sfunc")
   (set_attr "length" "4")
   (set_attr "needs_delay_slot" "yes")])

(define_expand "mulsi3_call"
  [(set (reg:SI 4) (match_operand:SI 1 "general_operand" "g"))
   (set (reg:SI 5) (match_operand:SI 2 "general_operand" "g"))
   (set (match_dup 3) (symbol_ref:SI "__mulsi3"))
   (parallel[(set (reg:SI 0)
		  (mult:SI (reg:SI 4)
			   (reg:SI 5)))
	     (clobber (reg:SI 21))
	     (clobber (reg:SI 18))
	     (clobber (reg:SI 17))
	     (clobber (reg:SI 3))
	     (clobber (reg:SI 2))
	     (clobber (reg:SI 1))
	     (use (match_dup 3))])
   (set (match_operand:SI 0 "general_operand" "=g") 
	(reg:SI 0))]
  ""
  "operands[3] = gen_reg_rtx(SImode);")
	
(define_insn "mul_l"
  [(set (reg:SI 21)
	(mult:SI (match_operand:SI 0 "arith_reg_operand" "r")
		 (match_operand:SI 1 "arith_reg_operand" "r")))]
  "TARGET_SH2"
  "mul.l	%1,%0"
  [(set_attr "type" "smpy")])

(define_expand "mulsi3"
  [(set (reg:SI 21)
	(mult:SI  (match_operand:SI 1 "arith_reg_operand" "r")
		  (match_operand:SI 2 "arith_reg_operand" "r")))
   (set (match_operand:SI 0 "arith_reg_operand" "=r")
	(reg:SI 21))]
  "TARGET_SH2"
  "")

(define_insn ""
  [(set (reg:DI 20)
	(mult:DI (sign_extend:DI (match_operand:SI 1 "arith_reg_operand" "r"))
		 (sign_extend:DI (match_operand:SI 2 "arith_reg_operand" "r"))))]
  "TARGET_SH2"
  "dmuls.l	%2,%1"
  [(set_attr "type" "dmpy")])

(define_expand "mulsidi3"
  [(set (reg:DI 20)
	(mult:DI (sign_extend:DI (match_operand:SI 1 "arith_reg_operand" "r"))
		 (sign_extend:DI (match_operand:SI 2 "arith_reg_operand" "r"))))
   (set (match_operand:DI 0 "arith_reg_operand" "=r")
	(reg:DI 20))]
  "TARGET_SH2"
  "")

(define_insn ""
  [(set (reg:DI 20)
	(mult:DI (zero_extend:DI (match_operand:SI 1 "arith_reg_operand" "r"))
		 (zero_extend:DI (match_operand:SI 2 "arith_reg_operand" "r"))))]
  "TARGET_SH2"
  "dmulu.l	%2,%1"
  [(set_attr "type" "dmpy")])

(define_expand "umulsidi3"
  [(set (reg:DI 20)
	(mult:DI (zero_extend:DI (match_operand:SI 1 "arith_reg_operand" "r"))
		 (zero_extend:DI (match_operand:SI 2 "arith_reg_operand" "r"))))
   (set (match_operand:DI 0 "arith_reg_operand" "=r")
	(reg:DI 20))]
  "TARGET_SH2"
  "")


;; -------------------------------------------------------------------------
;; Logical operations
;; -------------------------------------------------------------------------

(define_insn "and_ffff"
 [(set (match_operand:SI 0 "arith_reg_operand" "=r")
       (and:SI (match_operand:SI 1 "arith_reg_operand" "r")
	       (const_int 65535)))]
 ""
 "extu.w	%1,%0"
 [(set_attr "type" "arith")])

(define_insn "and_ff"
 [(set (match_operand:SI 0 "arith_reg_operand" "=r")
       (and:SI (match_operand:SI 1 "arith_reg_operand" "r")
	       (const_int 255)))]
 ""
 "extu.b	%1,%0"
 [(set_attr "type" "arith")])
   
(define_insn ""
  [(set (match_operand:SI 0 "arith_reg_operand" "=r,z")
	(and:SI (match_operand:SI 1 "arith_reg_operand" "%0,0")
		(match_operand:SI 2 "logical_operand" "r,L")))]
  ""
  "and	%2,%0"
  [(set_attr "type" "arith")])

(define_expand "andsi3"
  [(set (match_operand:SI 0 "arith_reg_operand" "")
	(and:SI (match_operand:SI 1 "arith_reg_operand" "")
		(match_operand:SI 2 "logical_operand" "")))]
  ""
  "")

(define_insn "iorsi3"
  [(set (match_operand:SI 0 "arith_reg_operand" "=r,z")
	(ior:SI (match_operand:SI 1 "arith_reg_operand" "%0,0")
		(match_operand:SI 2 "logical_operand" "r,L")))]
  ""
  "or	%2,%0")

(define_insn "xorsi3"
  [(set (match_operand:SI 0 "arith_reg_operand" "=z,r")
	(xor:SI (match_operand:SI 1 "arith_reg_operand" "%0,0")
		(match_operand:SI 2 "logical_operand" "L,r")))]
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


;;
;; shift left

(define_insn "ashlsi3_k"
  [(set (match_operand:SI 0 "arith_reg_operand" "=r,r")
	(ashift:SI (match_operand:SI 1 "arith_reg_operand" "0,0")
		   (match_operand:SI 2 "immediate_operand" "M,K")))
   (clobber (reg:SI 18))]
  "CONST_OK_FOR_K (INTVAL (operands[2]))"
  "@
	shll	%0
	shll%O2	%0")

; seperate pattern for shifts by any N.  Look at pnum_clobbers
; to see if this is being recognised inside combine.  If so, dont
; match, since combine will try and merge shifts, which will break
; scheduling

(define_insn "ashlsi3_n"
  [(set (match_operand:SI 0 "arith_reg_operand" "=r")
	(ashift:SI (match_operand:SI 1 "arith_reg_operand" "0")
		   (match_operand:SI 2 "immediate_operand" "n")))
   (clobber (reg:SI 18))]
  "fake_shift()"
  "*return output_shift(\"shll\", operands[0], operands[2], ASHIFT);"
  [(set_attr "length" "12")
   (set_attr "in_delay_slot" "no")
   (set_attr "type" "arith")])

(define_expand "ashlsi3"
  [(parallel[(set (match_operand:SI 0 "arith_reg_operand" "")
		  (ashift:SI (match_operand:SI 1 "arith_reg_operand" "")
			     (match_operand:SI 2 "immediate_operand" "")))
	     (clobber (reg:SI 18))])]
  ""
  "if (gen_shifty_op (ASHIFT, operands)) DONE; else FAIL;")

;
; arithmetic shift right
;

(define_insn "ashrsi3_k"
  [(set (match_operand:SI 0 "arith_reg_operand" "=r")
	(ashiftrt:SI (match_operand:SI 1 "arith_reg_operand" "0")
		     (match_operand:SI 2 "immediate_operand" "M")))
   (clobber (reg:SI 18))]
  "INTVAL(operands[2]) == 1"
  "shar	%0"
  [(set_attr "type" "arith")])

(define_insn "ashrsi3_16"
  [(set (match_operand:SI 0 "arith_reg_operand" "=r")
	(ashiftrt:SI (match_operand:SI 1 "arith_reg_operand" "0")
		     (match_operand:SI 2 "immediate_operand" "i")))
   (clobber (reg:SI 18))]
  "INTVAL(operands[2]) == 16"
  "shlr16	%0\;exts.w	%0,%0"
  [(set_attr "type" "arith")
   (set_attr "length" "4")])

; an arithmetic shift right by 16 is better as a logical shift and a 
; sign extend

;(define_split 
;  [(set (match_operand:SI 0 "arith_reg_operand" "=r")
;	(ashiftrt:SI (match_operand:SI 1 "arith_reg_operand" "0")
;		     (const_int 16)))
;   (clobber (reg:SI 18))]
;  ""
;  [(set (match_dup 3) (match_dup 0))
;    (set (match_dup 3) (lshiftrt:SI (match_dup 3) (const_int 16)))
;    (set (match_dup 0) (sign_extend:SI (subreg:HI (match_dup 3) 0)))]
;  "operands[3] = gen_reg_rtx (SImode);")

(define_insn "ashrsi3_n"
  [(set (reg:SI 4)
	(ashiftrt:SI (reg:SI 4)
		     (match_operand:SI 0 "immediate_operand" "i")))
   (clobber (reg:SI 18))
   (clobber (reg:SI 17))
   (use (match_operand:SI 1 "arith_reg_operand" "r"))]
  ""
  "jsr	@%1%#"
  [(set_attr "type" "sfunc")
   (set_attr "in_delay_slot" "no")
   (set_attr "length" "4")
   (set_attr "needs_delay_slot" "yes")])

(define_expand "ashrsi3"
  [(parallel[(set (match_operand:SI 0 "arith_reg_operand" "=r")
		  (ashiftrt:SI (match_operand:SI 1 "arith_reg_operand" "r")
			       (match_operand:SI 2 "nonmemory_operand" "M")))
	     (clobber (reg:SI 18))])]
  ""
  "if (gen_shifty_op (ASHIFTRT, operands)) DONE; else FAIL;")

; logical shift right
;

(define_insn "lshrsi3_k"
  [(set (match_operand:SI 0 "arith_reg_operand" "=r,r")
	(lshiftrt:SI (match_operand:SI 1 "arith_reg_operand" "0,0")
		     (match_operand:SI 2 "immediate_operand" "M,K")))
   (clobber (reg:SI 18))]
  "CONST_OK_FOR_K (INTVAL (operands[2]))"
  "@
	shlr	%0
	shlr%O2	%0")

; seperate pattern for shifts by any N. 

(define_insn "lshrsi3_n"
  [(set (match_operand:SI 0 "arith_reg_operand" "=r")
	(lshiftrt:SI (match_operand:SI 1 "arith_reg_operand" "0")
		     (match_operand:SI 2 "immediate_operand" "n")))
   (clobber (reg:SI 18))]
  "fake_shift()"
  "* return output_shift (\"shlr\", operands[0], operands[2], LSHIFTRT);"
  [(set_attr "length" "12")
   (set_attr "in_delay_slot" "no")
   (set_attr "type" "arith")])

(define_expand "lshrsi3"
  [(parallel[(set (match_operand:SI 0 "arith_reg_operand" "")
		  (lshiftrt:SI (match_operand:SI 1 "arith_reg_operand" "")
			       (match_operand:SI 2 "immediate_operand" "")))
	     (clobber (reg:SI 18))])]
  ""
  "if (gen_shifty_op (LSHIFTRT, operands)) DONE; else FAIL;") 

(define_insn "ashldi3_k"
  [(set (match_operand:DI 0 "arith_reg_operand" "=r")
	(ashift:DI (match_operand:DI 1 "arith_reg_operand" "0")
		   (const_int 1)))
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
		     (const_int 1)))
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
		     (const_int 1)))
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


(define_insn "negc"
  [(set (match_operand:SI 0 "arith_reg_operand" "=r")
	(neg:SI (plus:SI (reg:SI 18) (match_operand:SI 1 "arith_reg_operand" "r"))))]
  ""
  "negc	%1,%0"
  [(set_attr "length" "2")
   (set_attr "type" "arith")])

(define_expand "negdi2"
  [(set (match_operand:DI 0 "arith_reg_operand" "=r")
	(neg:DI (match_operand:DI 1 "arith_reg_operand" "r")))
   (clobber (reg:SI 18))]
  ""
  "{
   rtx low_src = operand_subword (operands[1], 1, 0, DImode);
   rtx high_src = operand_subword (operands[1], 0, 0, DImode);

   rtx low_dst = operand_subword (operands[0], 1, 1, DImode);
   rtx high_dst = operand_subword (operands[0], 0, 1, DImode);

   emit_insn (gen_clrt ());
   emit_insn (gen_negc (low_dst, low_src));
   emit_insn (gen_negc (high_dst, high_src));
   DONE;
   }
   ")


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
  [(set_attr "type" "arith")])


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
  [(set (match_operand:DI 0 "arith_reg_operand" "=r,r")
	(sign_extend:DI (match_operand:SI 1 "arith_reg_operand" "0,r")))
   (clobber (reg:SI 18))]
  ""
  "@
	mov	%1,%0\;shll	%0\;subc	%0,%0 ! b sidi2
	mov	%1,%0\;mov	%1,%R0\;shll	%0\;subc	%0,%0 ! a sidi2"
  [(set_attr "length" "6,8")]) 

(define_insn "extendhisi2"
  [(set (match_operand:SI 0 "arith_reg_operand" "=r,z,r")
	(sign_extend:SI (match_operand:HI 1 "arith_operand" "r,u,m")))]
  ""
  "@
	exts.w	%1,%0
   	mov.w	%1,%0
   	mov.w	%1,%0"
  [(set_attr "type" "arith,load,load")])

(define_insn "extendqisi2"
  [(set (match_operand:SI 0 "arith_reg_operand" "=r,z,r")
	(sign_extend:SI (match_operand:QI 1 "general_movsrc_operand" "r,U,m")))]
  ""
  "@
	exts.b	%1,%0
	mov.b	%1,%0 !p9
	mov.b	%1,%0 !p8"
  [(set_attr "type" "arith,load,load")])

(define_insn "extendqihi2"
  [(set (match_operand:HI 0 "arith_reg_operand" "=r,z,r")
	(sign_extend:HI (match_operand:QI 1 "general_movsrc_operand" "r,U,m")))]
  ""
  "@
	exts.b	%1,%0
	mov.b	%1,%0  !p7
	mov.b	%1,%0 ! p6"
  [(set_attr "type" "arith,load,load")])


;; -------------------------------------------------------------------------
;; Move instructions
;; -------------------------------------------------------------------------

;; define push and pop so it is easy for sh.c

(define_insn "push"
  [(set (mem:SI (pre_dec:SI (reg:SI 15)))
	(match_operand:SI 0 "register_operand" "r,lx"))]
  ""
  "@
	mov.l	%0,@-r15
	sts.l	%0,@-r15 ! push"
  [(set_attr "type" "store")
   (set_attr "hit_stack" "yes")])

(define_insn "pop"
  [(set (match_operand:SI 0 "register_operand" "=r,lx")
	(mem:SI (post_inc:SI (reg:SI 15))))]
  ""
  "@
	mov.l	@r15+,%0
	lds.l	@r15+,%0"
  [(set_attr "type" "load,pload")
   (set_attr "hit_stack" "yes")])

(define_insn ""
  [(set (match_operand:SI 0 "push_operand" "=<,<")
	(match_operand:SI 1 "arith_reg_operand" "r,xl"))]
  ""
  "@
	mov.l	%1,%0
	sts.l	%1,%0"
  [(set_attr "type" "store")])

(define_insn ""
  [(set	(match_operand:SI 0 "arith_reg_operand" "=r,xl")
	(match_operand:SI 1 "pop_operand" "=>,>"))]
  ""
  "@
	mov.l	%1,%0
	lds.l	%1,%0"
  [(set_attr "type" "load")])

(define_insn "clrt"
  [(set (reg:SI 18) (const_int 0))]
  ""
  "clrt")

;(define_insn "movsi_pi"
;  [(set (match_operand:SI 0 "general_movdst_operand" "=r")
;	(mem:SI (post_inc (match_operand:SI 1 "register_operand" "r"))))]
;  ""
;  "mov.l	@%1,%0\;add	#4,%1"
;  [(set_attr "length" "4")])


(define_insn "movsi_i"
  [(set (match_operand:SI 0 "general_movdst_operand" "=r,r,r,r,r,<m,<,xl,xl,t,r")
	(match_operand:SI 1 "general_movsrc_operand" "Q,rI,>m,xl,t,r,xl,r,>,r,i"))]
  ""
  "@
	mov.l	%1,%0
	mov	%1,%0
	mov.l	%1,%0
	sts	%1,%0
	movt	%0
	mov.l	%1,%0
	sts.l	%1,%0
	lds	%1,%0
	lds.l	%1,%0
	tst	%1,%1\;bt	T%*\;bra	F%*\;sett\;T%*:clrt\;F%*:%^
	fake %1,%0"
  [(set_attr "type" "pcloadsi,move,load,move,store,store,move,load,move,move,move")])
			  
(define_expand "movsi"
  [(set (match_operand:SI 0 "general_movdst_operand" "")
	(match_operand:SI 1 "general_movsrc_operand" ""))]
  ""

  "{ if (prepare_move_operands(operands, SImode)) DONE; } ")


(define_insn "movqi_i"
  [(set (match_operand:QI 0 "general_movdst_operand" "=r,r,>m,r,r,l")
	(match_operand:QI 1 "general_movsrc_operand"  "ri,<m,r,t,l,r"))]
  "arith_reg_operand (operands[0], QImode) 
   || arith_reg_operand (operands[1], QImode)"
  "@
	mov	%1,%0
	mov.b	%1,%0
	mov.b	%1,%0
	movt	%0
	sts	%1,%0
	lds	%1,%0"
 [(set_attr "length" "2,2,2,2,2,2")
  (set_attr "type" "move,load,store,move,move,move")])

(define_expand "movqi"
  [(set (match_operand:QI 0 "general_operand" "")
	(match_operand:QI 1 "general_operand"  ""))]
  ""
  "if (prepare_move_operands(operands, QImode)) DONE; ")

(define_insn "movhi_i"
  [(set (match_operand:HI 0 "general_movdst_operand" "=r,r,r,r,<m,r,r,l")
	(match_operand:HI 1 "general_movsrc_operand" "Q,rI,>m,t,r,i,l,r"))]
  ""
  "@
	mov.w	%1,%0
	mov	%1,%0
	mov.w	%1,%0
	movt	%0
	mov.w	%1,%0
	fake %1,%0
	sts	%1,%0
	lds	%1,%0"
  [(set_attr "length" "*,2,2,2,2,2,2,2")
   (set_attr "type" "pcloadhi,move,load,move,store,move,move,move")])

(define_expand "movhi"
  [(set (match_operand:HI 0 "general_movdst_operand" "")
	(match_operand:HI 1 "general_movsrc_operand"  ""))]
  ""
  "if (prepare_move_operands (operands, HImode)) DONE;")

(define_insn ""
  [(set (match_operand:DI 0 "push_operand" "=<")
	(match_operand:DI 1 "arith_reg_operand" "r"))]
   ""
   "mov.l	%R1,%0\;mov.l	%1,%0"
   [(set_attr "length" "4")
    (set_attr "type" "store")])

(define_insn ""
  [(set (match_operand:DI 0 "general_movdst_operand" "=r,r,r,m,r")
	(match_operand:DI 1 "general_movsrc_operand" "Q,r,m,r,i"))]
  "register_operand (operands[0], DImode)
  || register_operand (operands[1], DImode)"
  "* return output_movedouble (insn, operands, DImode);"
  [(set_attr "length" "*,4,4,4,4")
   (set_attr "type" "pcloadsi,move,load,store,move")])

;; If the output is a register and the input is memory, we have to be careful
;; and see which word needs to be loaded first.
;;
(define_split
  [(set (match_operand:DI 0 "general_movdst_operand" "")
	(match_operand:DI 1 "general_movsrc_operand" ""))]
 "!  (GET_CODE (operands[0]) == REG
	         && REGNO (operands[0]) >= FIRST_PSEUDO_REGISTER)
   && ! (GET_CODE (operands[1]) == REG
         && REGNO (operands[1]) >= FIRST_PSEUDO_REGISTER)
   && ! (GET_CODE (operands[0]) == REG && GET_CODE (operands[1]) == REG
   && ! reload_completed
   && reg_overlap_mentioned_p (operands[0], operands[1]))
   && ! EXTRA_CONSTRAINT_Q (operands[1])"
  [(set (match_dup 2) (match_dup 3))
   (set (match_dup 4) (match_dup 5))]
  "
{ if (GET_CODE (operands[0]) != REG
      || ! refers_to_regno_p (REGNO (operands[0]), REGNO (operands[0]) + 1,
			      operands[1], 0))
    {
      operands[2] = operand_subword (operands[0], 0, 0, DImode);
      operands[3] = operand_subword (operands[1], 0, 0, DImode);
      operands[4] = operand_subword (operands[0], 1, 0, DImode);
      operands[5] = operand_subword (operands[1], 1, 0, DImode);
    }
  else
    {
      operands[2] = operand_subword (operands[0], 1, 0, DImode);
      operands[3] = operand_subword (operands[1], 1, 0, DImode);
      operands[4] = operand_subword (operands[0], 0, 0, DImode);
      operands[5] = operand_subword (operands[1], 0, 0, DImode);
    }

  if (operands[2] == 0 || operands[3] == 0
      || operands[4] == 0 || operands[5] == 0)
    FAIL;
}")

	
(define_expand "movdi"
  [(set (match_operand:DI 0 "general_movdst_operand" "")
	(match_operand:DI 1 "general_movsrc_operand" ""))]
  ""
  "if ( prepare_move_operands(operands, DImode)) DONE; ")

(define_insn ""
  [(set (match_operand:DF 0 "push_operand" "=<")
	(match_operand:DF 1 "arith_reg_operand" "r"))]
   ""
  "mov.l	%R1,%0\;mov.l	%1,%0"
   [(set_attr "length" "4")
    (set_attr "type" "store")])

(define_insn "movdf_k"
  [(set (match_operand:DF 0 "general_movdst_operand" "=r,r,m")
	(match_operand:DF 1 "general_movsrc_operand" "r,m,r"))]
  "register_operand (operands[0], DFmode)
   || register_operand (operands[1], DFmode)"
  "* return output_movedouble (insn, operands, DFmode);"
  [(set_attr "length" "4")
   (set_attr "type" "move,load,store")])

;; If the output is a register and the input is memory, we have to be careful
;; and see which word needs to be loaded first.
;;
(define_split
  [(set (match_operand:DF 0 "general_movdst_operand" "")
	(match_operand:DF 1 "general_movsrc_operand" ""))]
 "!  (GET_CODE (operands[0]) == REG
	         && REGNO (operands[0]) >= FIRST_PSEUDO_REGISTER)
   && ! (GET_CODE (operands[1]) == REG
         && REGNO (operands[1]) >= FIRST_PSEUDO_REGISTER)
   && ! (GET_CODE (operands[0]) == REG && GET_CODE (operands[1]) == REG
   && ! reload_completed
   && reg_overlap_mentioned_p (operands[0], operands[1]))"
  [(set (match_dup 2) (match_dup 3))
   (set (match_dup 4) (match_dup 5))]
  "
{ if (GET_CODE (operands[0]) != REG
      || ! refers_to_regno_p (REGNO (operands[0]), REGNO (operands[0]) + 1,
			      operands[1], 0))
    {
      operands[2] = operand_subword (operands[0], 0, 0, DFmode);
      operands[3] = operand_subword (operands[1], 0, 0, DFmode);
      operands[4] = operand_subword (operands[0], 1, 0, DFmode);
      operands[5] = operand_subword (operands[1], 1, 0, DFmode);
    }
  else
    {
      operands[2] = operand_subword (operands[0], 1, 0, DFmode);
      operands[3] = operand_subword (operands[1], 1, 0, DFmode);
      operands[4] = operand_subword (operands[0], 0, 0, DFmode);
      operands[5] = operand_subword (operands[1], 0, 0, DFmode);
    }

  if (operands[2] == 0 || operands[3] == 0
      || operands[4] == 0 || operands[5] == 0)
    FAIL;
}")


(define_expand "movdf"
  [(set (match_operand:DF 0 "general_movdst_operand" "")
	(match_operand:DF 1 "general_movsrc_operand" ""))]
  ""
 "{ if (prepare_move_operands(operands, DFmode)) DONE; } ")

(define_insn ""
  [(set (match_operand:SF 0 "push_operand" "=<")
	(match_operand:SF 1 "arith_reg_operand" "r"))]
   ""
   "mov.l	%1,%0"
  [(set_attr "type" "store")])
		
(define_insn "movsf_i"
  [(set (match_operand:SF 0 "general_movdst_operand" "=>,r,r,r,r,m,l,r")
	(match_operand:SF 1 "general_movsrc_operand"  "r,<,r,I,m,r,r,l"))]
  ""
  "@
        mov.l	%1,@%N0\;add	#4,%N0 !bad
        add	#-4,%1\;mov.l	@%N1,%0 !bad
	mov	%1,%0
	mov	%1,%0
	mov.l	%1,%0
	mov.l	%1,%0
	lds	%1,%0
	sts	%1,%0"
  [(set_attr "type" "store,load,move,move,load,store,move,move")
   (set_attr "length" "4,4,*,*,*,*,*,*")])

(define_expand "movsf"
  [(set (match_operand:SF 0 "general_movdst_operand" "")
	(match_operand:SF 1 "general_movsrc_operand" ""))]
  ""
  "if (prepare_move_operands (operands, SFmode)) DONE;")


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
  "from_compare (operands, EQ);")

; There is no bne compare, so we reverse the branch arms.

(define_expand "bne"
  [(set (reg:SI 18) (eq:SI (match_dup 1) (match_dup 2)))
   (set (pc)
	(if_then_else (eq (reg:SI 18)
			  (const_int 1))
		      (pc)
		      (label_ref (match_operand 0 "" ""))))]
  ""
  "from_compare (operands, NE);")

(define_expand "bgt"
  [(set (reg:SI 18) (gt:SI (match_dup 1) (match_dup 2)))
   (set (pc)
	(if_then_else (eq (reg:SI 18)
			  (const_int 1))
		      (label_ref (match_operand 0 "" ""))
		      (pc))) ]
  ""
  "from_compare (operands, GT);")

(define_expand "blt"
  [(set (reg:SI 18) (ge:SI (match_dup 1) (match_dup 2)))
   (set (pc)
	(if_then_else (eq (reg:SI 18)(const_int 1))
		      (pc)
		      (label_ref (match_operand 0 "" ""))))]
  ""
  "from_compare (operands, LT);")

(define_expand "ble"
  [(set (reg:SI 18) (gt:SI (match_dup 1) (match_dup 2)))
   (set (pc)
	(if_then_else (eq (reg:SI 18)
			  (const_int 1))
		      (pc)
		      (label_ref (match_operand 0 "" ""))))
   ]
  ""
  "from_compare (operands, LE);")

(define_expand "bge"
  [(set (reg:SI 18) (ge:SI (match_dup 1) (match_dup 2)))
   (set (pc)
	(if_then_else (eq (reg:SI 18)
			  (const_int 1))
		      (label_ref (match_operand 0 "" ""))
		      (pc))) ]
  ""
  "from_compare (operands, GE);")

(define_expand "bgtu"
  [(set (reg:SI 18) (gtu:SI (match_dup 1) (match_dup 2)))
   (set (pc)
	(if_then_else (eq (reg:SI 18)
			  (const_int 1))
		      (label_ref (match_operand 0 "" ""))
		      (pc)))]
  ""
  "from_compare (operands, GTU); ")

(define_expand "bltu"
  [(set (reg:SI 18) (geu:SI (match_dup 1) (match_dup 2)))
   (set (pc)
		  (if_then_else (eq (reg:SI 18)
				    (const_int 1))
				(pc)
				(label_ref (match_operand 0 "" ""))))]
  ""
  "from_compare (operands, LTU);")

(define_expand "bgeu"
  [(set (reg:SI 18) (geu:SI (match_dup 1) (match_dup 2)))
   (set (pc)
	(if_then_else (eq (reg:SI 18)
			  (const_int 1))
		      (label_ref (match_operand 0 "" ""))
		      (pc)))
   ]
  ""
  "from_compare (operands, GEU);")

(define_expand "bleu"
  [(set (reg:SI 18) (gtu:SI (match_dup 1) (match_dup 2)))
   (set (pc)
	(if_then_else (eq (reg:SI 18)
			  (const_int 1))
		      (pc)
		      (label_ref (match_operand 0 "" ""))))]
  ""
  "from_compare (operands, LEU);")


;; ------------------------------------------------------------------------
;; Jump and linkage insns
;; ------------------------------------------------------------------------

(define_insn "jump"
  [(set (pc)
	(label_ref (match_operand 0 "" "")))]
  ""
  "*
{
  if (get_attr_length(insn) == 10) 
    {
      return output_far_jump(insn, operands[0]);
    }
  else
    {
      return   \"bra	%l0%#\";
    }
}"
  [(set_attr "type" "jump")
   (set_attr "needs_delay_slot" "yes")])


(define_insn "bsr"
  [(call (mem:SI (match_operand 0 "bsr_operand" "i"))
	 (match_operand 1 "" ""))
   (clobber (reg:SI 17))]
  "TARGET_BSR"
  "bsr	%O0%#"
  [(set_attr "needs_delay_slot" "yes")
   (set_attr "in_delay_slot" "no")
   (set_attr "length" "4")])

(define_insn "calli"
  [(call (mem:SI (match_operand:SI 0 "arith_reg_operand" "r"))
	 (match_operand 1 "" ""))
   (clobber (reg:SI 17))]
  ""
  "jsr	@%0%#"
  [(set_attr "needs_delay_slot" "yes")
   (set_attr "in_delay_slot" "no")
   (set_attr "length" "4")])

(define_insn "bsr_value"
  [(set (match_operand 0 "" "=rf")
	(call (mem:SI (match_operand 1 "bsr_operand" "i"))
	      (match_operand 2 "" "")))
   (clobber (reg:SI 17))]
  "TARGET_BSR"
  "bsr	%O1%#"
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
  "expand_acall(0, operands); DONE;")

(define_expand "call_value"
  [(parallel[(set (match_operand 0 "" "=rf")
		  (call (match_operand 1 "arith_reg_operand" "o")
			(match_operand 2 "" "")))
	     (clobber (reg:SI 17))])]
  ""
  "expand_acall(1, operands); DONE; ")

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

(define_insn "dect"
  [(parallel[
	     (set (reg:SI 18)
		  (eq:SI (match_operand:SI 0 "register_operand" "=r")
			 (const_int 1)))

	     (set (match_dup 0)
		  (plus:SI (match_dup 0)
			   (const_int -1)))])]
  "TARGET_SH2"
  "dt	%0")

(define_insn "nop"
  [(const_int 0)]
  ""
  "or	r0,r0")

; experimental use of auto inc and dec made these...
; can be deleted

(define_insn "fake"
  [(set (match_operand:QI 0 "register_operand" "=r")
	(mem:QI (pre_dec:SI (match_operand:SI 1 "register_operand" "r"))))]
  ""
  "add	#-1,%1\;mov.b	@%1,%0 !bad"
  [(set_attr "length" "4")])

;; Load address of a label. This is only generated by the casesi expand.

(define_insn "mova"
  [(set (reg:SI 0) 
	(label_ref (match_operand 0 "" "")))]
  ""
  "mova	%O0,r0"
  [(set_attr "in_delay_slot" "no")])

;; case instruction for switch statements.

;; Operand 0 is index
;; operand 1 is the minimum bound
;; operand 2 is the maximum bound - minimum bound + 1
;; operand 3 is CODE_LABEL for the table;
;; operand 4 is the CODE_LABEL to go to if index out of range.

(define_expand "casesi"
  [(set (match_dup 5) (match_operand:SI 0 "arith_reg_operand" ""))
   (set (match_dup 5) (minus:SI (match_dup 5)
				(match_operand:SI 1 "arith_operand" "")))
   (set (reg:SI 18)
	(gtu:SI (match_dup 5)
		(match_operand:SI 2 "arith_operand" "")))
   (set (pc)
	(if_then_else (eq (reg:SI 18)
			  (const_int 1))
		      (label_ref (match_operand 4 "" ""))
		      (pc)))
   (set (match_dup 6) (match_dup 5))
   (parallel[(set (match_dup 6) (ashift:SI (match_dup 6) (match_dup 7)))
		(clobber (reg:SI 18))])
   (set (reg:SI 0) (label_ref (match_operand 3 "" "")))
   (parallel[(set (reg:SI 0) (plus:SI (reg:SI 0)
				      (mem:HI (plus:SI (reg:SI 0)
						       (match_dup 6)))))
	     (set (match_dup 6) (mem:HI (plus:SI (reg:SI 0) (match_dup 6))))])
   (set (pc) (reg:SI 0))]
  ""
  "
{
  operands[1] = copy_to_mode_reg (SImode, operands[1]);
  operands[2] = copy_to_mode_reg (SImode, operands[2]);
  operands[5] = gen_reg_rtx (SImode);
  operands[6] = gen_reg_rtx (SImode);
  operands[7] = GEN_INT (TARGET_BIGTABLE  ? 2 : 1);
}")

(define_insn "casesi_worker"
  [(set (reg:SI 0) 
	(plus:SI (reg:SI 0) 
		 (mem:HI (plus:SI (reg:SI 0)
				  (match_operand:SI 0 "register_operand" "=r")))))
   (set (match_dup 0) (mem:HI (plus:SI (reg:SI 0)
				       (match_dup 0))))]
  ""
  "*
	if (TARGET_BIGTABLE) 
		return \"mov.l	@(r0,%0),%0\;add	%0,r0\";
	else
	   	return \"mov.w	@(r0,%0),%0\;add	%0,r0\";"
  [(set_attr "needs_delay_slot" "no")
   (set_attr "in_delay_slot" "no")
   (set_attr "length" "4")])


(define_insn "return"
  [(return)]
  "reload_completed"
  "%@	%#"
  [(set_attr "type" "return")])

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

(define_insn "movt"
  [(set (match_operand:SI 0 "arith_reg_operand" "=r")
	(eq:SI (reg:SI 18) (const_int 1)))]
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

;; -------------------------------------------------------------------------
;; Peepholes
;; -------------------------------------------------------------------------


(define_peephole 
  [(set (match_operand:QI 0 "arith_reg_operand" "")
	(mem:QI (match_operand:SI 1 "arith_reg_operand" "")))
   (set (match_dup 1) (plus:SI (match_dup 1) (const_int 1)))]
  "REGNO (operands[1]) != REGNO (operands[0])"
  "mov.b	@%1+,%0")

(define_peephole 
  [(set (match_operand:HI 0 "arith_reg_operand" "")
	(mem:HI (match_operand:SI 1 "arith_reg_operand" "")))
   (set (match_dup 1) (plus:SI (match_dup 1) (const_int 2)))]
  "REGNO (operands[1]) != REGNO (operands[0])"
  "mov.w	@%1+,%0")

(define_peephole 
  [(set (match_operand:SI 0 "arith_reg_operand" "")
	(mem:SI (match_operand:SI 1 "arith_reg_operand" "")))
   (set (match_dup 1) (plus:SI (match_dup 1) (const_int 4)))]
  "REGNO (operands[1]) != REGNO (operands[0])"
  "mov.l	@%1+,%0")

(define_peephole
  [(set (match_operand:QI 0 "register_operand" "=r")
	(match_operand:QI 1 "memory_operand" "g"))
   (set (match_operand:SI 2 "register_operand" "=r")
	(sign_extend:SI (match_dup 0)))]
  "REGNO (operands[0]) == REGNO (operands[2])"
  "mov.b	%1,%0 !p 5")

(define_peephole 
  [(set (match_operand:QI 0 "register_operand" "=r")
	(match_operand:QI 1 "general_movsrc_operand" "g"))
   (set (match_operand:SI 2 "register_operand" "=r")
	(sign_extend:SI (match_dup 0)))]
  "REGNO (operands[0]) != REGNO (operands[2]) 
   && 0 && dead_or_set_p (insn, operands[0])"
  "mov.b	%1,%2 ! p4")

  
;; -------------------------------------------------------------------------
;; Peepholes
;; -------------------------------------------------------------------------

(define_peephole 
  [(set (reg:SI 0) (label_ref (match_operand 0 "" "")))
   (set (match_operand:SI 1 "register_operand" "=r")
	(reg:SI 0))
   (set (reg:SI 0) (label_ref (match_dup 0)))
   (set (match_operand:SI 2 "register_operand" "=r")
	(reg:SI 0))]
   ""
   "mova	%O0,r0\;mov	r0,%1\;mov	r0,%2"
   [(set_attr "length" "6")
    (set_attr "in_delay_slot" "no")])


;; -------------------------------------------------------------------------
;; Combine patterns
;; -------------------------------------------------------------------------

(define_insn ""
  [(set (match_operand:SI 0 "register_operand" "=r")
	(plus:SI (match_operand:HI 1 "register_operand" "%0")
		 (match_operand:SI 2 "register_operand" "r")))]
  ""
  "add	%2,%0 ! why")

(define_insn "addc_2"
  [(set (match_operand:SI 0 "arith_reg_operand" "=&r")
	(plus:SI (reg:SI 18)
		 (match_operand:SI 1 "arith_reg_operand" "r")))
   (clobber (reg:SI 18))]
  ""
  "mov	#0,%0\;addc	%1,%0 ! addc1"
  [(set_attr "length" "4")])

(define_insn "combine_1"
  [(set (match_operand:SI 0 "arith_reg_operand" "=r")
	(sign_extend:SI (mem:QI (match_operand:SI 1 "arith_reg_operand" "r"))))]
  ""
  "mov.b	@%1,%0 ! why"
  [(set_attr "type" "load")])
  
(define_insn "combine_2"
  [(set (reg:SI 18)
	(eq:SI (and:SI (match_operand:SI 0 "arith_reg_operand" "z,r")
		    (match_operand:SI 1 "arith_operand" "L,r"))
	    (const_int 0)))]
  ""
  "tst	%1,%0 !t2c")

(define_split
  [(set (pc) 
	(if_then_else
	 (match_operator 2 "equality_operator" [(match_operand:SI 0 "arith_reg_operand" "r")
						(const_int 0)])
	 (label_ref (match_operand 1 "" ""))
	 (pc)))
   (clobber (reg:SI 18))]
  ""
  [(set (reg:SI 18) (eq:SI (and:SI (match_dup 0) (match_dup 0))
			(const_int 0)))
   (set (pc)
	(if_then_else (match_op_dup 2 [(reg:SI 18) (const_int 1)])
		      (label_ref (match_dup 1))
		      (pc)))]
  "")

;; -------------------------------------------------------------------------
;; Instructions to cope with inline literal tables
;; -------------------------------------------------------------------------

; 2 byte integer in line

(define_insn "consttable_2"
 [(unspec_volatile [(match_operand:SI 0 "general_operand" "=g")] 2)]
 ""
 "*
{
  assemble_integer (operands[0], 2, 1);
  return \"\";
}"
 [(set_attr "length" "2")
 (set_attr "in_delay_slot" "no")])

; 4 byte integer in line

(define_insn "consttable_4"
 [(unspec_volatile [(match_operand:SI 0 "general_operand" "=g")] 4)]
 ""
 "*
{
  assemble_integer (operands[0], 4, 1);
  return \"\";
}"
 [(set_attr "length" "4")
  (set_attr "in_delay_slot" "no")])

; 8 byte integer in line

(define_insn "consttable_8"
 [(unspec_volatile [(match_operand:SI 0 "general_operand" "=g")] 6)]
 ""
 "*
{
  assemble_integer (operands[0], 8, 1);
  return \"\";
}"
 [(set_attr "length" "8")
  (set_attr "in_delay_slot" "no")])

; align to a two byte boundary

(define_insn "align_2"
 [(unspec_volatile [(const_int 0)] 10)]
 ""
 ".align 1"
 [(set_attr "length" "0")
  (set_attr "in_delay_slot" "no")])

; align to a four byte boundary

(define_insn "align_4"
 [(unspec_volatile [(const_int 0)] 5)]
 ""
 ".align 2"
 [(set_attr "in_delay_slot" "no")])

; emitted at the end of the literal table, used to emit the
; 32bit branch labels if needed.

(define_insn "consttable_end"
  [(unspec_volatile [(const_int 0)] 11)]
  ""
  "* return output_jump_label_table ();"
  [(set_attr "in_delay_slot" "no")])


;(define_split 
;  [(set (subreg:SI (match_operand:QI 0 "register_operand" "=r") 0)
;	(plus:SI (subreg:SI (match_operand:QI 1 "general_operand" "g") 0)
;		 (subreg:SI (match_operand:QI 2 "general_operand" "g") 0)))]
;  ""
;  [(set (match_dup 3) (plus:SI (match_dup 1) (match_dup 2)))
;   (set (match_dup 1) (subreg:SI (match_dup 3) 0))]
;  "operands[3] = gen_reg_rtx(SImode);")


; byte arithmetic involving constants which need to be sign extended can be 
; fixed up...


(define_split 
  [(set (subreg:SI (match_operand:QI 0 "register_operand" "=r") 0)
	(plus:SI (subreg:SI (match_operand:QI 1 "register_operand" "0") 0)
		 (subreg:SI (match_operand 2 "immediate_operand" "n") 0)))]
  ""
  [(set (match_dup 4) (plus:SI (match_dup 2) (match_dup 3)))
   (set (match_dup 0) (and:SI (match_dup 0) (const_int 255)))]
  "{ int i = INTVAL(operands[2]) & 0xff;
   if (i > 127) i = i - 256;
   operands[3] = GEN_INT(i); 
   operands[4] = gen_reg_rtx(SImode);} ")


;; these instructions don't really exist - they are needed
;; before machine_dependent_reorg

(define_insn "movsi_k"
 [(set (match_operand:SI 0 "register_operand" "=r")
       (match_operand:SI 1 "immediate_operand" ""))]
  ""
  "! this is a fake")


(define_insn "movhi_k"
 [(set (match_operand:HI 0 "register_operand" "=r")
       (match_operand:HI 1 "immediate_operand" ""))]
  ""
  "! this is a fake")

(define_insn "movdi_k"
 [(set (match_operand:DI 0 "register_operand" "=r")
       (match_operand:DI 1 "immediate_operand" ""))]
  ""
  "! this is a fake")

;; -------------------------------------------------------------------------
;; Misc
;; -------------------------------------------------------------------------


;; String/block move insn.  

(define_expand "movstrsi"
  [(parallel [(set (mem:BLK (match_operand:BLK 0 "" ""))
		   (mem:BLK (match_operand:BLK 1 "" "")))
	      (use (match_operand:SI 2 "nonmemory_operand" ""))
	      (use (match_operand:SI 3 "immediate_operand" ""))
	      (clobber (reg:SI 17))
	      (clobber (reg:SI 4))
	      (clobber (reg:SI 5))
	      (clobber (reg:SI 0))])]
  ""
  "
{
  if(expand_block_move (operands))
     DONE;
  else FAIL;
}")

(define_insn "block_move_real"
  [(parallel [(set (mem:BLK (reg:SI 4))
		   (mem:BLK (reg:SI 5)))
	      (use (match_operand:SI 0 "arith_reg_operand" "r"))
	      (clobber (reg:SI 17))
	      (clobber (reg:SI 4))
	      (clobber (reg:SI 5))
	      (clobber (reg:SI 0))])]
  ""
  "jsr	@%0%#"
  [(set_attr "length" "4")
   (set_attr "type" "sfunc")
   (set_attr "needs_delay_slot" "yes")])

(define_insn "block_lump_real"
  [(parallel [(set (mem:BLK (reg:SI 4))
		   (mem:BLK (reg:SI 5)))
	      (use (match_operand:SI 0 "arith_reg_operand" "r"))
	      (use (reg:SI 6))
	      (clobber (reg:SI 17))
	      (clobber (reg:SI 4))
	      (clobber (reg:SI 5))
	      (clobber (reg:SI 6))
	      (clobber (reg:SI 0))])]
  ""
  "jsr	@%0%#"
  [(set_attr "length" "4")
   (set_attr "type" "sfunc")
   (set_attr "needs_delay_slot" "yes")])

(define_insn "mac"
  [(set (reg:SI 21)
	(mult:SI (sign_extend:SI (mem:HI (post_inc:SI 
					  (match_operand:SI 0 "arith_reg_operand" "r"))))
		 (sign_extend:SI (mem:HI (post_inc:SI
					  (match_operand:SI 1 "arith_reg_operand" "r"))))))]
  ""
  "mac.w	@%0+,@%1+")


			
