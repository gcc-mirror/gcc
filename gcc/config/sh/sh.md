;;- Machine description for the Hitachi SH.
;;  Copyright (C) 1993, 1994, 1995 Free Software Foundation, Inc.
;;  Contributed by Steve Chamberlain (sac@cygnus.com).
;;  Improved by Jim Wilson (wilson@cygnus.com).

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
;; the Free Software Foundation, 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.


;; ??? Should prepend a * to all pattern names which are not used.
;; This will make the compiler smaller, and rebuilds after changes faster.

;; ??? Should be enhanced to include support for many more GNU superoptimizer
;; sequences.  Especially the sequences for arithmetic right shifts.

;; ??? Should check all DImode patterns for consistency and usefulness.

;; ??? Should add support for using BSR for short function calls.

;; ??? The MAC.W and MAC.L instructions are not supported.  There is no
;; way to generate them.

;; ??? The BSR instruction is not supported.  It might be possible to
;; generate it by keeping track of function sizes (and hence relative
;; addresses), and then using it only if the target is earlier in the same
;; file, and is within range.  Better would be assembler/linker relaxing,
;; but that is much harder.

;; ??? The cmp/str instruction is not supported.  Perhaps it can be used
;; for a str* inline function.

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
;;   %@  --  print rte/rts if is/isn't an interrupt function
;;   %#  --  output a nop if there is nothing to put in the delay slot
;;   %O  --  print a constant without the #
;;   %R  --  print the lsw reg of a double
;;   %S  --  print the msw reg of a double
;;   %T  --  print next word of a double REG or MEM
;;
;; Special predicates:
;;
;;  arith_operand          -- operand is valid source for arithmetic op
;;  arith_reg_operand      -- operand is valid register for arithmetic op
;;  general_movdst_operand -- operand is valid move destination
;;  general_movsrc_operand -- operand is valid move source
;;  logical_operand        -- operand is valid source for logical op
;; -------------------------------------------------------------------------
;; Attributes
;; -------------------------------------------------------------------------

; Target CPU.

(define_attr "cpu" "sh0,sh1,sh2,sh3"
  (const (symbol_ref "sh_cpu_attr")))

;;
;; cbranch	conditional branch instructions
;; jump		unconditional jumps
;; arith	ordinary arithmetic
;; load		from memory
;; store	to memory
;; move		register to register
;; smpy		word precision integer multiply
;; dmpy		longword or doublelongword precision integer multiply
;; return	rts
;; pload	load of pr reg, which can't be put into delay slot of rts
;; pstore	store of pr reg, which can't be put into delay slot of jsr
;; pcload	pc relative load of constant value
;; rte		return from exception
;; sfunc	special function call with known used registers
;; call		function call

(define_attr "type"
 "cbranch,jump,arith,other,load,store,move,smpy,dmpy,return,pload,pstore,pcload,rte,sfunc,call"
  (const_string "other"))

; If a conditional branch destination is within -252..258 bytes away
; from the instruction it can be 2 bytes long.  Something in the
; range -4090..4100 bytes can be 6 bytes long.  All other conditional
; branches are 16 bytes long.

; An unconditional jump in the range -4092..4098 can be 2 bytes long.
; Otherwise, it must be 14 bytes long.

; All other instructions are two bytes long by default.

; All positive offsets have an adjustment added, which is the number of bytes
; difference between this instruction length and the next larger instruction
; length.  This is because shorten_branches starts with the largest
; instruction size and then tries to reduce them.

(define_attr "length" ""
  (cond [(eq_attr "type" "cbranch")
	 (if_then_else (and (ge (minus (match_dup 0) (pc))
				(const_int -252))
			    (le (minus (match_dup 0) (pc))
				(const_int 262)))
		       (const_int 2)
		       (if_then_else (and (ge (minus (match_dup 0) (pc))
					      (const_int -4090))
					  (le (minus (match_dup 0) (pc))
					      (const_int 4110)))
				     (const_int 6)
				     (const_int 16)))

	 (eq_attr "type" "jump")
	 (if_then_else (and (ge (minus (match_dup 0) (pc))
				(const_int -4092))
			    (le (minus (match_dup 0) (pc))
				(const_int 4110)))
		       (const_int 2)
		       (const_int 14))
	 ] (const_int 2)))

;; (define_function_unit {name} {num-units} {n-users} {test}
;;                       {ready-delay} {issue-delay} [{conflict-list}])

;; ??? These are probably not correct.
(define_function_unit "memory" 1 0 (eq_attr "type" "load,pcload,pload") 2 2)
(define_function_unit "mpy"    1 0 (eq_attr "type" "smpy") 2 2)
(define_function_unit "mpy"    1 0 (eq_attr "type" "dmpy") 3 3)

; Definitions for filling branch delay slots.

(define_attr "needs_delay_slot" "yes,no" (const_string "no"))

(define_attr "hit_stack" "yes,no" (const_string "no"))

(define_attr "interrupt_function" "no,yes"
  (const (symbol_ref "pragma_interrupt")))

(define_attr "in_delay_slot" "yes,no"
  (cond [(eq_attr "type" "cbranch") (const_string "no")
	 (eq_attr "type" "pcload") (const_string "no")
	 (eq_attr "needs_delay_slot" "yes") (const_string "no")
	 (eq_attr "length" "2") (const_string "yes")
	 ] (const_string "no")))

(define_delay
  (eq_attr "needs_delay_slot" "yes")
  [(eq_attr "in_delay_slot" "yes") (nil) (nil)])

;; On the SH and SH2, the rte instruction reads the return pc from the stack,
;; and thus we can't put a pop instruction in its delay slot.
;; ??? On the SH3, the rte instruction does not use the stack, so a pop
;; instruction can go in the delay slot.

;; Since a normal return (rts) implicitly uses the PR register,
;; we can't allow PR register loads in an rts delay slot.

(define_delay
  (eq_attr "type" "return")
  [(and (eq_attr "in_delay_slot" "yes")
	(ior (and (eq_attr "interrupt_function" "no")
		  (eq_attr "type" "!pload"))
	     (and (eq_attr "interrupt_function" "yes")
		  (eq_attr "hit_stack" "no")))) (nil) (nil)])

;; Since a call implicitly uses the PR register, we can't allow
;; a PR register store in a jsr delay slot.

(define_delay
  (ior (eq_attr "type" "call") (eq_attr "type" "sfunc"))
  [(and (eq_attr "in_delay_slot" "yes")
	(eq_attr "type" "!pstore")) (nil) (nil)])

;; Say that we have annulled true branches, since this gives smaller and
;; faster code when branches are predicted as not taken.

;; ??? Branches which are out-of-range actually have two delay slots,
;; the first is either always executed or else annulled false, and the
;; second is always annulled false.  Handling these differently from
;; in range branches would give better code.

(define_delay
  (and (eq_attr "type" "cbranch")
       (eq_attr "cpu" "sh2,sh3"))
  [(eq_attr "in_delay_slot" "yes") (eq_attr "in_delay_slot" "yes") (nil)])

;; -------------------------------------------------------------------------
;; SImode signed integer comparisons
;; -------------------------------------------------------------------------

(define_insn ""
  [(set (match_operand:SI 0 "arith_reg_operand" "=r")
	(eq:SI (reg:SI 18)
	       (const_int 1)))]
  ""
  "movt	%0")

;; ??? This combiner pattern does not work, because combine does not combine
;; instructions that set a hard register when SMALL_REGISTER_CLASSES is
;; defined.  Perhaps use a pseudo-reg for the T bit?

(define_insn ""
  [(set (reg:SI 18)
	(eq:SI (and:SI (match_operand:SI 0 "arith_reg_operand" "z,r")
		       (match_operand:SI 1 "arith_operand" "L,r"))
	       (const_int 0)))]
  ""
  "tst	%1,%0")

;; ??? Perhaps should only accept reg/constant if the register is reg 0.
;; That would still allow reload to create cmpi instructions, but would
;; perhaps allow forcing the constant into a register when that is better.
;; Probably should use r0 for mem/imm compares, but force constant into a
;; register for pseudo/imm compares.

(define_insn "cmpeqsi_t"
  [(set (reg:SI 18) (eq:SI (match_operand:SI 0 "arith_reg_operand" "r,z,r")
			   (match_operand:SI 1 "arith_operand" "N,rI,r")))]
  ""
  "@
	tst	%0,%0
	cmp/eq	%1,%0
	cmp/eq	%1,%0")

(define_insn "cmpgtsi_t"
  [(set (reg:SI 18) (gt:SI (match_operand:SI 0 "arith_reg_operand" "r,r")
			   (match_operand:SI 1 "arith_reg_or_0_operand" "r,N")))]
  ""
  "@
	cmp/gt	%1,%0
	cmp/pl	%0")

(define_insn "cmpgesi_t"
  [(set (reg:SI 18) (ge:SI (match_operand:SI 0 "arith_reg_operand" "r,r")
			   (match_operand:SI 1 "arith_reg_or_0_operand" "r,N")))]
  ""
  "@
	cmp/ge	%1,%0
	cmp/pz	%0")

;; -------------------------------------------------------------------------
;; SImode unsigned integer comparisons
;; -------------------------------------------------------------------------

(define_insn "cmpgeusi_t"
  [(set (reg:SI 18) (geu:SI (match_operand:SI 0 "arith_reg_operand" "r")
			    (match_operand:SI 1 "arith_reg_operand" "r")))]
  ""
  "cmp/hs	%1,%0")

(define_insn "cmpgtusi_t"
  [(set (reg:SI 18) (gtu:SI (match_operand:SI 0 "arith_reg_operand" "r")
			    (match_operand:SI 1 "arith_reg_operand" "r")))]
  ""
  "cmp/hi	%1,%0")

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

;; ??? This should be a define expand.

(define_insn "adddi3"
  [(set (match_operand:DI 0 "arith_reg_operand" "=r")
	(plus:DI (match_operand:DI 1 "arith_reg_operand" "%0")
		 (match_operand:DI 2 "arith_reg_operand" "r")))
   (clobber (reg:SI 18))]
  ""
  "clrt\;addc	%R2,%R0\;addc	%S2,%S0"
  [(set_attr "length" "6")])

(define_insn "addsi3"
  [(set (match_operand:SI 0 "arith_reg_operand" "=r")
	(plus:SI (match_operand:SI 1 "arith_operand" "%0")
		 (match_operand:SI 2 "arith_operand" "rI")))]
  ""
  "add	%2,%0"
  [(set_attr "type" "arith")])

;; -------------------------------------------------------------------------
;; Subtraction instructions
;; -------------------------------------------------------------------------

;; ??? This should be a define expand.

(define_insn "subdi3"
  [(set (match_operand:DI 0 "arith_reg_operand" "=r")
	(minus:DI (match_operand:DI 1 "arith_reg_operand" "0")
		 (match_operand:DI 2 "arith_reg_operand" "r")))
   (clobber (reg:SI 18))]
  ""
  "clrt\;subc	%R2,%R0\;subc	%S2,%S0"
  [(set_attr "length" "6")])

(define_insn "subsi3"
  [(set (match_operand:SI 0 "arith_reg_operand" "=r")
	(minus:SI (match_operand:SI 1 "arith_reg_operand" "0")
		  (match_operand:SI 2 "arith_reg_operand" "r")))]
  ""
  "sub	%2,%0"
  [(set_attr "type" "arith")])

;; -------------------------------------------------------------------------
;; Division instructions
;; -------------------------------------------------------------------------

;; We take advantage of the library routines which don't clobber as many
;; registers as a normal function call would.

;; We must use a pseudo-reg forced to reg 0 in the SET_DEST rather than
;; hard register 0.  If we used hard register 0, then the next instruction
;; would be a move from hard register 0 to a pseudo-reg.  If the pseudo-reg
;; gets allocated to a stack slot that needs its address reloaded, then
;; there is nothing to prevent reload from using r0 to reload the address.
;; This reload would clobber the value in r0 we are trying to store.
;; If we let reload allocate r0, then this problem can never happen.

(define_insn ""
  [(set (match_operand:SI 1 "register_operand" "=z")
	(udiv:SI (reg:SI 4) (reg:SI 5)))
   (clobber (reg:SI 18))
   (clobber (reg:SI 17))
   (clobber (reg:SI 4))
   (use (match_operand:SI 0 "arith_reg_operand" "r"))]
  ""
  "jsr	@%0%#"
  [(set_attr "type" "sfunc")
   (set_attr "needs_delay_slot" "yes")])

(define_expand "udivsi3"
  [(set (reg:SI 4) (match_operand:SI 1 "general_operand" ""))
   (set (reg:SI 5) (match_operand:SI 2 "general_operand" ""))
   (set (match_dup 3) (symbol_ref:SI "__udivsi3"))
   (parallel [(set (match_operand:SI 0 "register_operand" "")
		   (udiv:SI (reg:SI 4)
			    (reg:SI 5)))
	      (clobber (reg:SI 18))
	      (clobber (reg:SI 17))
	      (clobber (reg:SI 4))
	      (use (match_dup 3))])]
  ""
  "operands[3] = gen_reg_rtx(SImode);")

(define_insn ""
  [(set (match_operand:SI 1 "register_operand" "=z")
	(div:SI (reg:SI 4) (reg:SI 5)))
   (clobber (reg:SI 18))
   (clobber (reg:SI 17))
   (clobber (reg:SI 1))
   (clobber (reg:SI 2))
   (clobber (reg:SI 3))
   (use (match_operand:SI 0 "arith_reg_operand" "r"))]
  ""
  "jsr	@%0%#"
  [(set_attr "type" "sfunc")
   (set_attr "needs_delay_slot" "yes")])

(define_expand "divsi3"
  [(set (reg:SI 4) (match_operand:SI 1 "general_operand" ""))
   (set (reg:SI 5) (match_operand:SI 2 "general_operand" ""))
   (set (match_dup 3) (symbol_ref:SI "__sdivsi3"))
   (parallel [(set (match_operand:SI 0 "register_operand" "")
		   (div:SI (reg:SI 4)
			   (reg:SI 5)))
	      (clobber (reg:SI 18))
	      (clobber (reg:SI 17))
	      (clobber (reg:SI 1))
	      (clobber (reg:SI 2))
	      (clobber (reg:SI 3))
	      (use (match_dup 3))])]
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
		  (match_operand:HI 1 "arith_reg_operand" ""))
		 (sign_extend:SI
		  (match_operand:HI 2 "arith_reg_operand" ""))))
   (set (match_operand:SI 0 "arith_reg_operand" "")
	(reg:SI 21))]
  ""
  "")

(define_expand "umulhisi3"
  [(set (reg:SI 21)
	(mult:SI (zero_extend:SI
		  (match_operand:HI 1 "arith_reg_operand" ""))
		 (zero_extend:SI
		  (match_operand:HI 2 "arith_reg_operand" ""))))
   (set (match_operand:SI 0 "arith_reg_operand" "")
	(reg:SI 21))]
  ""
  "")

;; mulsi3 on the SH2 can be done in one instruction, on the SH1 we generate
;; a call to a routine which clobbers known registers.

(define_insn ""
  [(set (match_operand:SI 1 "register_operand" "=z")
	(mult:SI (reg:SI 4) (reg:SI 5)))
   (clobber (reg:SI 21))
   (clobber (reg:SI 18))
   (clobber (reg:SI 17))
   (clobber (reg:SI 3))
   (clobber (reg:SI 2))
   (clobber (reg:SI 1))
   (use (match_operand:SI 0 "arith_reg_operand" "r"))]
  ""
  "jsr	@%0%#"
  [(set_attr "type" "sfunc")
   (set_attr "needs_delay_slot" "yes")])

(define_expand "mulsi3_call"
  [(set (reg:SI 4) (match_operand:SI 1 "general_operand" ""))
   (set (reg:SI 5) (match_operand:SI 2 "general_operand" ""))
   (set (match_dup 3) (symbol_ref:SI "__mulsi3"))
   (parallel[(set (match_operand:SI 0 "register_operand" "")
		  (mult:SI (reg:SI 4)
			   (reg:SI 5)))
	     (clobber (reg:SI 21))
	     (clobber (reg:SI 18))
	     (clobber (reg:SI 17))
	     (clobber (reg:SI 3))
	     (clobber (reg:SI 2))
	     (clobber (reg:SI 1))
	     (use (match_dup 3))])]
  ""
  "operands[3] = gen_reg_rtx(SImode);")

(define_insn "mul_l"
  [(set (reg:SI 21)
	(mult:SI (match_operand:SI 0 "arith_reg_operand" "r")
		 (match_operand:SI 1 "arith_reg_operand" "r")))]
  "TARGET_SH2"
  "mul.l	%1,%0"
  [(set_attr "type" "dmpy")])

(define_expand "mulsi3"
  [(set (reg:SI 21)
	(mult:SI  (match_operand:SI 1 "arith_reg_operand" "")
		  (match_operand:SI 2 "arith_reg_operand" "")))
   (set (match_operand:SI 0 "arith_reg_operand" "")
	(reg:SI 21))]
  ""
  "
{
  if (!TARGET_SH2)
    {
      FAIL;
      /* ??? Does this give worse or better code?  */
      emit_insn (gen_mulsi3_call (operands[0], operands[1], operands[2]));
      DONE;
    }
}")

(define_insn ""
  [(set (reg:DI 20)
	(mult:DI (sign_extend:DI (match_operand:SI 1 "arith_reg_operand" "r"))
		 (sign_extend:DI (match_operand:SI 2 "arith_reg_operand" "r"))))]
  "TARGET_SH2"
  "dmuls.l	%2,%1"
  [(set_attr "type" "dmpy")])

(define_expand "mulsidi3"
  [(set (reg:DI 20)
	(mult:DI (sign_extend:DI (match_operand:SI 1 "arith_reg_operand" ""))
		 (sign_extend:DI (match_operand:SI 2 "arith_reg_operand" ""))))
   (set (match_operand:DI 0 "arith_reg_operand" "")
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
	(mult:DI (zero_extend:DI (match_operand:SI 1 "arith_reg_operand" ""))
		 (zero_extend:DI (match_operand:SI 2 "arith_reg_operand" ""))))
   (set (match_operand:DI 0 "arith_reg_operand" "")
	(reg:DI 20))]
  "TARGET_SH2"
  "")

(define_insn ""
  [(set (reg:SI 20)
	(truncate:SI
	 (lshiftrt:DI (mult:DI (sign_extend:DI (match_operand:SI 1 "arith_reg_operand" "r"))
			       (sign_extend:DI (match_operand:SI 2 "arith_reg_operand" "r")))
		      (const_int 32))))
   (clobber (reg:SI 21))]
  "TARGET_SH2"
  "dmuls.l	%2,%1"
  [(set_attr "type" "dmpy")])

(define_expand "smulsi3_highpart"
  [(parallel [(set (reg:SI 20)
		   (truncate:SI
		    (lshiftrt:DI (mult:DI (sign_extend:DI (match_operand:SI 1 "arith_reg_operand" ""))
					  (sign_extend:DI (match_operand:SI 2 "arith_reg_operand" "")))
				 (const_int 32))))
	      (clobber (reg:SI 21))])
   (set (match_operand:SI 0 "arith_reg_operand" "")
	(reg:SI 20))]
  "TARGET_SH2"
  "")

(define_insn ""
  [(set (reg:SI 20)
	(truncate:SI
	 (lshiftrt:DI (mult:DI (zero_extend:DI (match_operand:SI 1 "arith_reg_operand" "r"))
			       (zero_extend:DI (match_operand:SI 2 "arith_reg_operand" "r")))
		      (const_int 32))))
   (clobber (reg:SI 21))]
  "TARGET_SH2"
  "dmulu.l	%2,%1"
  [(set_attr "type" "dmpy")])

(define_expand "umulsi3_highpart"
  [(parallel [(set (reg:SI 20)
		   (truncate:SI
		    (lshiftrt:DI (mult:DI (zero_extend:DI (match_operand:SI 1 "arith_reg_operand" ""))
					  (zero_extend:DI (match_operand:SI 2 "arith_reg_operand" "")))
				 (const_int 32))))
	      (clobber (reg:SI 21))])
   (set (match_operand:SI 0 "arith_reg_operand" "")
	(reg:SI 20))]
  "TARGET_SH2"
  "")

;; -------------------------------------------------------------------------
;; Logical operations
;; -------------------------------------------------------------------------

(define_insn ""
  [(set (match_operand:SI 0 "arith_reg_operand" "=r,z")
	(and:SI (match_operand:SI 1 "arith_reg_operand" "%0,0")
		(match_operand:SI 2 "logical_operand" "r,L")))]
  ""
  "and	%2,%0"
  [(set_attr "type" "arith")])

;; If the constant is 255, then emit a extu.b instruction instead of an
;; and, since that will give better code.

(define_expand "andsi3"
  [(set (match_operand:SI 0 "arith_reg_operand" "")
	(and:SI (match_operand:SI 1 "arith_reg_operand" "")
		(match_operand:SI 2 "logical_operand" "")))]
  ""
  "
{
  if (GET_CODE (operands[2]) == CONST_INT && INTVAL (operands[2]) == 255)
    {
      emit_insn (gen_zero_extendqisi2 (operands[0],
				       gen_lowpart (QImode, operands[1])));
      DONE;
    }
}")

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

(define_insn "rotlsi3_1"
  [(set (match_operand:SI 0 "arith_reg_operand" "=r")
	(rotate:SI (match_operand:SI 1 "arith_reg_operand" "0")
		   (const_int 1)))
   (set (reg:SI 18)
	(lshiftrt:SI (match_dup 1) (const_int 31)))]
  ""
  "rotl	%0")

(define_insn "rotlsi3_31"
  [(set (match_operand:SI 0 "arith_reg_operand" "=r")
	(rotate:SI (match_operand:SI 1 "arith_reg_operand" "0")
		   (const_int 31)))
   (clobber (reg:SI 18))]
  ""
  "rotr	%0")

(define_insn ""
  [(set (match_operand:SI 0 "arith_reg_operand" "=r")
	(rotate:SI (match_operand:SI 1 "arith_reg_operand" "r")
		   (const_int 16)))]
  ""
  "swap.w	%1,%0")

(define_expand "rotlsi3"
  [(set (match_operand:SI 0 "arith_reg_operand" "")
	(rotate:SI (match_operand:SI 1 "arith_reg_operand" "")
		   (match_operand:SI 2 "immediate_operand" "")))]
  ""
  "
{
  if (GET_CODE (operands[2]) != CONST_INT)
    FAIL;

  if (INTVAL (operands[2]) == 1)
    {
      emit_insn (gen_rotlsi3_1 (operands[0], operands[1]));
      DONE;
    }
  else if (INTVAL (operands[2]) == 31)
    {
      emit_insn (gen_rotlsi3_31 (operands[0], operands[1]));
      DONE;
    }
  else if (INTVAL (operands[2]) != 16)
    FAIL;
}")

(define_insn ""
  [(set (match_operand:HI 0 "arith_reg_operand" "=r")
	(rotate:HI (match_operand:HI 1 "arith_reg_operand" "r")
		   (const_int 8)))]
  ""
  "swap.b	%1,%0")

(define_expand "rotlhi3"
  [(set (match_operand:HI 0 "arith_reg_operand" "")
	(rotate:HI (match_operand:HI 1 "arith_reg_operand" "")
		   (match_operand:HI 2 "immediate_operand" "")))]
  ""
  "
{
  if (GET_CODE (operands[2]) != CONST_INT || INTVAL (operands[2]) != 8)
    FAIL;
}")

;;
;; shift left

(define_insn "ashlsi3_d"
  [(set (match_operand:SI 0 "arith_reg_operand" "=r")
	(ashift:SI (match_operand:SI 1 "arith_reg_operand" "0")
		   (match_operand:SI 2 "arith_reg_operand" "r")))]
  "TARGET_SH3"
  "shld	%2,%0")

(define_insn "ashlsi3_k"
  [(set (match_operand:SI 0 "arith_reg_operand" "=r,r")
	(ashift:SI (match_operand:SI 1 "arith_reg_operand" "0,0")
		   (match_operand:SI 2 "const_int_operand" "M,K")))]
  "CONST_OK_FOR_K (INTVAL (operands[2]))"
  "@
	add	%0,%0
	shll%O2	%0")

(define_insn "ashlsi3_n"
  [(set (match_operand:SI 0 "arith_reg_operand" "=r")
	(ashift:SI (match_operand:SI 1 "arith_reg_operand" "0")
		   (match_operand:SI 2 "const_int_operand" "n")))
   (clobber (reg:SI 18))]
  ""
  "#"
  [(set (attr "length")
	(cond [(eq (symbol_ref "shift_insns_rtx (insn)") (const_int 1))
	       (const_string "2")
	       (eq (symbol_ref "shift_insns_rtx (insn)") (const_int 2))
	       (const_string "4")
	       (eq (symbol_ref "shift_insns_rtx (insn)") (const_int 3))
	       (const_string "6")]
	      (const_string "8")))
   (set_attr "type" "arith")])

(define_split
  [(set (match_operand:SI 0 "arith_reg_operand" "")
	(ashift:SI (match_operand:SI 1 "arith_reg_operand" "")
		   (match_operand:SI 2 "const_int_operand" "n")))
   (clobber (reg:SI 18))]
  ""
  [(use (reg:SI 0))]
  "
{
  gen_shifty_op (ASHIFT, operands);
  DONE;
}")

(define_expand "ashlsi3"
  [(parallel [(set (match_operand:SI 0 "arith_reg_operand" "")
		   (ashift:SI (match_operand:SI 1 "arith_reg_operand" "")
			      (match_operand:SI 2 "nonmemory_operand" "")))
	      (clobber (reg:SI 18))])]
  ""
  "
{
  if (TARGET_SH3 && arith_reg_operand (operands[2], GET_MODE (operands[2])))
    {
      emit_insn (gen_ashlsi3_d (operands[0], operands[1], operands[2]));
      DONE;
    }
  if (! immediate_operand (operands[2], GET_MODE (operands[2])))
    FAIL;
}")

;
; arithmetic shift right
;

(define_insn "ashrsi3_k"
  [(set (match_operand:SI 0 "arith_reg_operand" "=r")
	(ashiftrt:SI (match_operand:SI 1 "arith_reg_operand" "0")
		     (match_operand:SI 2 "const_int_operand" "M")))
   (clobber (reg:SI 18))]
  "INTVAL (operands[2]) == 1"
  "shar	%0"
  [(set_attr "type" "arith")])

;; ??? This should be a define expand.

(define_insn "ashrsi2_16"
  [(set (match_operand:SI 0 "arith_reg_operand" "=r")
        (ashiftrt:SI (match_operand:SI 1 "arith_reg_operand" "r")
                     (const_int 16)))]
  ""
  "swap.w	%1,%0\;exts.w	%0,%0"
  [(set_attr "length" "4")])

;; ??? This should be a define expand.

(define_insn "ashrsi2_31"
  [(set (match_operand:SI 0 "arith_reg_operand" "=r")
        (ashiftrt:SI (match_operand:SI 1 "arith_reg_operand" "0")
                     (const_int 31)))
   (clobber (reg:SI 18))]
  ""
  "@
   shll	%0\;subc	%0,%0"
  [(set_attr "length" "4")])

(define_insn "ashrsi3_d"
  [(set (match_operand:SI 0 "arith_reg_operand" "=r")
	(ashiftrt:SI (match_operand:SI 1 "arith_reg_operand" "0")
		     (neg:SI (match_operand:SI 2 "arith_reg_operand" "r"))))]
  "TARGET_SH3"
  "shad	%2,%1")

(define_insn "ashrsi3_n"
  [(set (reg:SI 4)
	(ashiftrt:SI (reg:SI 4)
		     (match_operand:SI 0 "const_int_operand" "i")))
   (clobber (reg:SI 18))
   (clobber (reg:SI 17))
   (use (match_operand:SI 1 "arith_reg_operand" "r"))]
  ""
  "jsr	@%1%#"
  [(set_attr "type" "sfunc")
   (set_attr "needs_delay_slot" "yes")])

(define_expand "ashrsi3"
  [(parallel [(set (match_operand:SI 0 "arith_reg_operand" "")
		   (ashiftrt:SI (match_operand:SI 1 "arith_reg_operand" "")
				(match_operand:SI 2 "nonmemory_operand" "")))
	      (clobber (reg:SI 18))])]
  ""
  "if (expand_ashiftrt (operands)) DONE; else FAIL;")

;; logical shift right

(define_insn "lshrsi3_d"
  [(set (match_operand:SI 0 "arith_reg_operand" "=r")
	(lshiftrt:SI (match_operand:SI 1 "arith_reg_operand" "0")
		     (neg:SI (match_operand:SI 2 "arith_reg_operand" "r"))))]
  "TARGET_SH3"
  "shld	%2,%0")

;;  Only the single bit shift clobbers the T bit.

(define_insn "lshrsi3_m"
  [(set (match_operand:SI 0 "arith_reg_operand" "=r")
	(lshiftrt:SI (match_operand:SI 1 "arith_reg_operand" "0")
		     (match_operand:SI 2 "const_int_operand" "M")))
   (clobber (reg:SI 18))]
  "CONST_OK_FOR_M (INTVAL (operands[2]))"
  "shlr	%0")

(define_insn "lshrsi3_k"
  [(set (match_operand:SI 0 "arith_reg_operand" "=r")
	(lshiftrt:SI (match_operand:SI 1 "arith_reg_operand" "0")
		     (match_operand:SI 2 "const_int_operand" "K")))]
  "CONST_OK_FOR_K (INTVAL (operands[2]))
   && ! CONST_OK_FOR_M (INTVAL (operands[2]))"
  "shlr%O2	%0")

(define_insn "lshrsi3_n"
  [(set (match_operand:SI 0 "arith_reg_operand" "=r")
	(lshiftrt:SI (match_operand:SI 1 "arith_reg_operand" "0")
		     (match_operand:SI 2 "const_int_operand" "n")))
   (clobber (reg:SI 18))]
  ""
  "#"
  [(set (attr "length")
	(cond [(eq (symbol_ref "shift_insns_rtx (insn)") (const_int 1))
	       (const_string "2")
	       (eq (symbol_ref "shift_insns_rtx (insn)") (const_int 2))
	       (const_string "4")
	       (eq (symbol_ref "shift_insns_rtx (insn)") (const_int 3))
	       (const_string "6")]
	      (const_string "8")))
   (set_attr "type" "arith")])

(define_split
  [(set (match_operand:SI 0 "arith_reg_operand" "")
	(lshiftrt:SI (match_operand:SI 1 "arith_reg_operand" "")
		     (match_operand:SI 2 "const_int_operand" "n")))
   (clobber (reg:SI 18))]
  ""
  [(use (reg:SI 0))]
  "
{
  gen_shifty_op (LSHIFTRT, operands);
  DONE;
}")

(define_expand "lshrsi3"
  [(parallel [(set (match_operand:SI 0 "arith_reg_operand" "")
		   (lshiftrt:SI (match_operand:SI 1 "arith_reg_operand" "")
				(match_operand:SI 2 "nonmemory_operand" "")))
	      (clobber (reg:SI 18))])]
  ""
  "
{
  if (TARGET_SH3 && arith_reg_operand (operands[2], GET_MODE (operands[2])))
    {
      rtx count = copy_to_mode_reg (SImode, operands[2]);
      emit_insn (gen_negsi2 (count, count));
      emit_insn (gen_ashlsi3_d (operands[0], operands[1], count));
      DONE;
    }
  if (! immediate_operand (operands[2], GET_MODE (operands[2])))
    FAIL;
}")

;; ??? This should be a define expand.

(define_insn "ashldi3_k"
  [(set (match_operand:DI 0 "arith_reg_operand" "=r")
	(ashift:DI (match_operand:DI 1 "arith_reg_operand" "0")
		   (const_int 1)))
   (clobber (reg:SI 18))]
  ""
  "shll	%R0\;rotcl	%S0"
  [(set_attr "length" "4")])

(define_expand "ashldi3"
  [(parallel [(set (match_operand:DI 0 "arith_reg_operand" "")
		   (ashift:DI (match_operand:DI 1 "arith_reg_operand" "")
			      (match_operand:DI 2 "immediate_operand" "")))
	      (clobber (reg:SI 18))])]
  ""
  "{ if (GET_CODE (operands[2]) != CONST_INT
	 || INTVAL (operands[2]) != 1) FAIL;} ")

;; ??? This should be a define expand.

(define_insn "lshrdi3_k"
  [(set (match_operand:DI 0 "arith_reg_operand" "=r")
	(lshiftrt:DI (match_operand:DI 1 "arith_reg_operand" "0")
		     (const_int 1)))
   (clobber (reg:SI 18))]
  ""
  "shlr	%S0\;rotcr	%R0"
  [(set_attr "length" "4")])

(define_expand "lshrdi3"
  [(parallel [(set (match_operand:DI 0 "arith_reg_operand" "")
		   (lshiftrt:DI (match_operand:DI 1 "arith_reg_operand" "")
			       (match_operand:DI 2 "immediate_operand" "")))
	     (clobber (reg:SI 18))])]
  ""
  "{ if (GET_CODE (operands[2]) != CONST_INT
	 || INTVAL (operands[2]) != 1) FAIL;} ")

;; ??? This should be a define expand.

(define_insn "ashrdi3_k"
  [(set (match_operand:DI 0 "arith_reg_operand" "=r")
	(ashiftrt:DI (match_operand:DI 1 "arith_reg_operand" "0")
		     (const_int 1)))
   (clobber (reg:SI 18))]
  ""
  "shar	%S0\;rotcr	%R0"
  [(set_attr "length" "4")])

(define_expand "ashrdi3"
  [(parallel [(set (match_operand:DI 0 "arith_reg_operand" "")
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
	(neg:SI (plus:SI (reg:SI 18)
			 (match_operand:SI 1 "arith_reg_operand" "r"))))
   (set (reg:SI 18)
	(ne:SI (ior:SI (reg:SI 18) (match_dup 1))
	       (const_int 0)))]
  ""
  "negc	%1,%0"
  [(set_attr "type" "arith")])

(define_expand "negdi2"
  [(set (match_operand:DI 0 "arith_reg_operand" "")
	(neg:DI (match_operand:DI 1 "arith_reg_operand" "")))
   (clobber (reg:SI 18))]
  ""
  "
{
  int low_word = (TARGET_LITTLE_ENDIAN ? 0 : 1);
  int high_word = (TARGET_LITTLE_ENDIAN ? 1 : 0);

  rtx low_src = operand_subword (operands[1], low_word, 0, DImode);
  rtx high_src = operand_subword (operands[1], high_word, 0, DImode);

  rtx low_dst = operand_subword (operands[0], low_word, 1, DImode);
  rtx high_dst = operand_subword (operands[0], high_word, 1, DImode);

  emit_insn (gen_clrt ());
  emit_insn (gen_negc (low_dst, low_src));
  emit_insn (gen_negc (high_dst, high_src));
  DONE;
}")

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

;; ??? This should be a define expand.
;; ??? Or perhaps it should be dropped?

(define_insn "extendsidi2"
  [(set (match_operand:DI 0 "arith_reg_operand" "=r")
	(sign_extend:DI (match_operand:SI 1 "arith_reg_operand" "r")))
   (clobber (reg:SI 18))]
  ""
  "mov	%1,%S0\;mov	%1,%R0\;shll	%S0\;subc	%S0,%S0"
  [(set_attr "length" "8")])

(define_insn "extendhisi2"
  [(set (match_operand:SI 0 "arith_reg_operand" "=r,r")
	(sign_extend:SI (match_operand:HI 1 "general_movsrc_operand" "r,m")))]
  ""
  "@
	exts.w	%1,%0
   	mov.w	%1,%0"
  [(set_attr "type" "arith,load")])

(define_insn "extendqisi2"
  [(set (match_operand:SI 0 "arith_reg_operand" "=r,r")
	(sign_extend:SI (match_operand:QI 1 "general_movsrc_operand" "r,m")))]
  ""
  "@
	exts.b	%1,%0
	mov.b	%1,%0"
  [(set_attr "type" "arith,load")])

(define_insn "extendqihi2"
  [(set (match_operand:HI 0 "arith_reg_operand" "=r,r")
	(sign_extend:HI (match_operand:QI 1 "general_movsrc_operand" "r,m")))]
  ""
  "@
	exts.b	%1,%0
	mov.b	%1,%0"
  [(set_attr "type" "arith,load")])

;; -------------------------------------------------------------------------
;; Move instructions
;; -------------------------------------------------------------------------

;; define push and pop so it is easy for sh.c

(define_insn "push"
  [(set (mem:SI (pre_dec:SI (reg:SI 15)))
	(match_operand:SI 0 "register_operand" "r,l,x"))]
  ""
  "@
	mov.l	%0,@-r15
	sts.l	%0,@-r15
	sts.l	%0,@-r15"
  [(set_attr "type" "store,pstore,store")
   (set_attr "hit_stack" "yes")])

(define_insn "pop"
  [(set (match_operand:SI 0 "register_operand" "=r,l,x")
	(mem:SI (post_inc:SI (reg:SI 15))))]
  ""
  "@
	mov.l	@r15+,%0
	lds.l	@r15+,%0
	lds.l	@r15+,%0"
  [(set_attr "type" "load,pload,load")
   (set_attr "hit_stack" "yes")])

;; These two patterns can happen as the result of optimization, when
;; comparisons get simplified to a move of zero or 1 into the T reg.
;; They don't disappear completely, because the T reg is a fixed hard reg.

(define_insn "clrt"
  [(set (reg:SI 18) (const_int 0))]
  ""
  "clrt")

(define_insn "sett"
  [(set (reg:SI 18) (const_int 1))]
  ""
  "sett")

;; t/z is first, so that it will be preferred over r/r when reloading a move
;; of a pseudo-reg into the T reg
(define_insn "movsi_i"
  [(set (match_operand:SI 0 "general_movdst_operand" "=t,r,r,r,r,r,m,<,xl,xl,r")
	(match_operand:SI 1 "general_movsrc_operand" "z,Q,rI,m,xl,t,r,xl,r,>,i"))]
  "register_operand (operands[0], SImode)
   || register_operand (operands[1], SImode)"
  "@
	tst	%1,%1\;rotcl	%1\;xor	#1,%1\;rotcr	%1
	mov.l	%1,%0
	mov	%1,%0
	mov.l	%1,%0
	sts	%1,%0
	movt	%0
	mov.l	%1,%0
	sts.l	%1,%0
	lds	%1,%0
	lds.l	%1,%0
	fake	%1,%0"
  [(set_attr "type" "move,pcload,move,load,move,store,store,move,load,move,move")
   (set_attr "length" "8,*,*,*,*,*,*,*,*,*,*")])

(define_expand "movsi"
  [(set (match_operand:SI 0 "general_movdst_operand" "")
	(match_operand:SI 1 "general_movsrc_operand" ""))]
  ""
  "{ if (prepare_move_operands (operands, SImode)) DONE; }")

(define_insn "movqi_i"
  [(set (match_operand:QI 0 "general_movdst_operand" "=r,r,m,r,r,l")
	(match_operand:QI 1 "general_movsrc_operand"  "ri,m,r,t,l,r"))]
  "arith_reg_operand (operands[0], QImode)
   || arith_reg_operand (operands[1], QImode)"
  "@
	mov	%1,%0
	mov.b	%1,%0
	mov.b	%1,%0
	movt	%0
	sts	%1,%0
	lds	%1,%0"
 [(set_attr "type" "move,load,store,move,move,move")])

(define_expand "movqi"
  [(set (match_operand:QI 0 "general_operand" "")
	(match_operand:QI 1 "general_operand"  ""))]
  ""
  "{ if (prepare_move_operands (operands, QImode)) DONE; }")

(define_insn "movhi_i"
  [(set (match_operand:HI 0 "general_movdst_operand" "=r,r,r,r,m,r,l,r")
	(match_operand:HI 1 "general_movsrc_operand" "Q,rI,m,t,r,l,r,i"))]
  "arith_reg_operand (operands[0], HImode)
   || arith_reg_operand (operands[1], HImode)"
  "@
	mov.w	%1,%0
	mov	%1,%0
	mov.w	%1,%0
	movt	%0
	mov.w	%1,%0
	sts	%1,%0
	lds	%1,%0
	fake	%1,%0"
  [(set_attr "type" "pcload,move,load,move,store,move,move,move")])

(define_expand "movhi"
  [(set (match_operand:HI 0 "general_movdst_operand" "")
	(match_operand:HI 1 "general_movsrc_operand"  ""))]
  ""
  "{ if (prepare_move_operands (operands, HImode)) DONE; }")

;; ??? This should be a define expand.

(define_insn ""
  [(set (match_operand:DI 0 "general_movdst_operand" "=r,r,r,m,r,r")
	(match_operand:DI 1 "general_movsrc_operand" "Q,r,m,r,i,x"))]
  "arith_reg_operand (operands[0], DImode)
   || arith_reg_operand (operands[1], DImode)"
  "* return output_movedouble (insn, operands, DImode);"
  [(set_attr "length" "4")
   (set_attr "type" "pcload,move,load,store,move,move")])

;; If the output is a register and the input is memory or a register, we have
;; to be careful and see which word needs to be loaded first.  

(define_split
  [(set (match_operand:DI 0 "general_movdst_operand" "")
	(match_operand:DI 1 "general_movsrc_operand" ""))]
  "reload_completed"
  [(set (match_dup 2) (match_dup 3))
   (set (match_dup 4) (match_dup 5))]
  "
{
  int regno;

  if ((GET_CODE (operands[0]) == MEM
       && GET_CODE (XEXP (operands[0], 0)) == PRE_DEC)
      || (GET_CODE (operands[1]) == MEM
	  && GET_CODE (XEXP (operands[1], 0)) == POST_INC))
    FAIL;

  if (GET_CODE (operands[0]) == REG)
    regno = REGNO (operands[0]);
  else if (GET_CODE (operands[0]) == SUBREG)
    regno = REGNO (SUBREG_REG (operands[0])) + SUBREG_WORD (operands[0]);
  else if (GET_CODE (operands[0]) == MEM)
    regno = -1;

  if (regno == -1
      || ! refers_to_regno_p (regno, regno + 1, operands[1], 0))
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
  "{ if ( prepare_move_operands (operands, DImode)) DONE; }")

;; ??? This should be a define expand.

(define_insn "movdf_k"
  [(set (match_operand:DF 0 "general_movdst_operand" "=r,r,m")
	(match_operand:DF 1 "general_movsrc_operand" "r,m,r"))]
  "arith_reg_operand (operands[0], DFmode)
   || arith_reg_operand (operands[1], DFmode)"
  "* return output_movedouble (insn, operands, DFmode);"
  [(set_attr "length" "4")
   (set_attr "type" "move,load,store")])

;; If the output is a register and the input is memory or a register, we have
;; to be careful and see which word needs to be loaded first.  

(define_split
  [(set (match_operand:DF 0 "general_movdst_operand" "")
	(match_operand:DF 1 "general_movsrc_operand" ""))]
  "reload_completed"
  [(set (match_dup 2) (match_dup 3))
   (set (match_dup 4) (match_dup 5))]
  "
{
  int regno;

  if ((GET_CODE (operands[0]) == MEM
       && GET_CODE (XEXP (operands[0], 0)) == PRE_DEC)
      || (GET_CODE (operands[1]) == MEM
	  && GET_CODE (XEXP (operands[1], 0)) == POST_INC))
    FAIL;

  if (GET_CODE (operands[0]) == REG)
    regno = REGNO (operands[0]);
  else if (GET_CODE (operands[0]) == SUBREG)
    regno = REGNO (SUBREG_REG (operands[0])) + SUBREG_WORD (operands[0]);
  else if (GET_CODE (operands[0]) == MEM)
    regno = -1;

  if (regno == -1
      || ! refers_to_regno_p (regno, regno + 1, operands[1], 0))
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
  "{ if (prepare_move_operands (operands, DFmode)) DONE; }")

(define_insn "movsf_i"
  [(set (match_operand:SF 0 "general_movdst_operand" "=r,r,r,m,l,r")
	(match_operand:SF 1 "general_movsrc_operand"  "r,I,m,r,r,l"))]
  "arith_reg_operand (operands[0], SFmode)
   || arith_reg_operand (operands[1], SFmode)"
  "@
	mov	%1,%0
	mov	%1,%0
	mov.l	%1,%0
	mov.l	%1,%0
	lds	%1,%0
	sts	%1,%0"
  [(set_attr "type" "move,move,load,store,move,move")])

(define_expand "movsf"
  [(set (match_operand:SF 0 "general_movdst_operand" "")
	(match_operand:SF 1 "general_movsrc_operand" ""))]
  ""
  "{ if (prepare_move_operands (operands, SFmode)) DONE; }")

;; ------------------------------------------------------------------------
;; Define the real conditional branch instructions.
;; ------------------------------------------------------------------------

(define_insn "branch_true"
  [(set (pc) (if_then_else (eq (reg:SI 18) (const_int 1))
			   (label_ref (match_operand 0 "" ""))
			   (pc)))]
  ""
  "* return output_branch (1, insn, operands);"
  [(set_attr "type" "cbranch")])

(define_insn "branch_false"
  [(set (pc) (if_then_else (ne (reg:SI 18) (const_int 1))
			   (label_ref (match_operand 0 "" ""))
			   (pc)))]
  ""
  "* return output_branch (0, insn, operands);"
  [(set_attr "type" "cbranch")])

(define_insn "inverse_branch_true"
  [(set (pc) (if_then_else (eq (reg:SI 18) (const_int 1))
			   (pc)
			   (label_ref (match_operand 0 "" ""))))]
  ""
  "* return output_branch (0, insn, operands);"
  [(set_attr "type" "cbranch")])

(define_insn "inverse_branch_false"
  [(set (pc) (if_then_else (ne (reg:SI 18) (const_int 1))
   			   (pc)
			   (label_ref (match_operand 0 "" ""))))]
  ""
  "* return output_branch (1, insn, operands);"
  [(set_attr "type" "cbranch")])

;; Conditional branch insns

(define_expand "beq"
  [(set (reg:SI 18) (eq:SI (match_dup 1) (match_dup 2)))
   (set (pc)
	(if_then_else (eq (reg:SI 18) (const_int 1))
		      (label_ref (match_operand 0 "" ""))
		      (pc)))]
  ""
  "from_compare (operands, EQ);")

; There is no bne compare, so we reverse the branch arms.

(define_expand "bne"
  [(set (reg:SI 18) (eq:SI (match_dup 1) (match_dup 2)))
   (set (pc)
	(if_then_else (eq (reg:SI 18) (const_int 1))
		      (pc)
		      (label_ref (match_operand 0 "" ""))))]
  ""
  "from_compare (operands, NE);")

(define_expand "bgt"
  [(set (reg:SI 18) (gt:SI (match_dup 1) (match_dup 2)))
   (set (pc)
	(if_then_else (eq (reg:SI 18) (const_int 1))
		      (label_ref (match_operand 0 "" ""))
		      (pc)))]
  ""
  "from_compare (operands, GT);")

(define_expand "blt"
  [(set (reg:SI 18) (ge:SI (match_dup 1) (match_dup 2)))
   (set (pc)
	(if_then_else (eq (reg:SI 18) (const_int 1))
		      (pc)
		      (label_ref (match_operand 0 "" ""))))]
  ""
  "from_compare (operands, LT);")

(define_expand "ble"
  [(set (reg:SI 18) (gt:SI (match_dup 1) (match_dup 2)))
   (set (pc)
	(if_then_else (eq (reg:SI 18) (const_int 1))
		      (pc)
		      (label_ref (match_operand 0 "" ""))))]
  ""
  "from_compare (operands, LE);")

(define_expand "bge"
  [(set (reg:SI 18) (ge:SI (match_dup 1) (match_dup 2)))
   (set (pc)
	(if_then_else (eq (reg:SI 18) (const_int 1))
		      (label_ref (match_operand 0 "" ""))
		      (pc)))]
  ""
  "from_compare (operands, GE);")

(define_expand "bgtu"
  [(set (reg:SI 18) (gtu:SI (match_dup 1) (match_dup 2)))
   (set (pc)
	(if_then_else (eq (reg:SI 18) (const_int 1))
		      (label_ref (match_operand 0 "" ""))
		      (pc)))]
  ""
  "from_compare (operands, GTU); ")

(define_expand "bltu"
  [(set (reg:SI 18) (geu:SI (match_dup 1) (match_dup 2)))
   (set (pc)
		  (if_then_else (eq (reg:SI 18) (const_int 1))
				(pc)
				(label_ref (match_operand 0 "" ""))))]
  ""
  "from_compare (operands, LTU);")

(define_expand "bgeu"
  [(set (reg:SI 18) (geu:SI (match_dup 1) (match_dup 2)))
   (set (pc)
	(if_then_else (eq (reg:SI 18) (const_int 1))
		      (label_ref (match_operand 0 "" ""))
		      (pc)))]
  ""
  "from_compare (operands, GEU);")

(define_expand "bleu"
  [(set (reg:SI 18) (gtu:SI (match_dup 1) (match_dup 2)))
   (set (pc)
	(if_then_else (eq (reg:SI 18) (const_int 1))
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
  /* The length is 16 if the delay slot is unfilled.  */
  if (get_attr_length(insn) >= 14)
    return output_far_jump(insn, operands[0]);
  else
    return   \"bra	%l0%#\";
}"
  [(set_attr "type" "jump")
   (set_attr "needs_delay_slot" "yes")])

(define_insn "calli"
  [(call (mem:SI (match_operand:SI 0 "arith_reg_operand" "r"))
	 (match_operand 1 "" ""))
   (clobber (reg:SI 17))]
  ""
  "jsr	@%0%#"
  [(set_attr "type" "call")
   (set_attr "needs_delay_slot" "yes")])

(define_insn "call_valuei"
  [(set (match_operand 0 "" "=rf")
	(call (mem:SI (match_operand:SI 1 "arith_reg_operand" "r"))
	      (match_operand 2 "" "")))
   (clobber (reg:SI 17))]
  ""
  "jsr	@%1%#"
  [(set_attr "type" "call")
   (set_attr "needs_delay_slot" "yes")])

(define_expand "call"
  [(parallel [(call (mem:SI (match_operand 0 "arith_reg_operand" ""))
			    (match_operand 1 "" ""))
	      (clobber (reg:SI 17))])]
  ""
  "operands[0] = force_reg (SImode, XEXP (operands[0], 0));")

(define_expand "call_value"
  [(parallel [(set (match_operand 0 "arith_reg_operand" "")
		   (call (mem:SI (match_operand 1 "arith_reg_operand" ""))
				 (match_operand 2 "" "")))
	      (clobber (reg:SI 17))])]
  ""
  "operands[1] = force_reg (SImode, XEXP (operands[1], 0));")

(define_insn "indirect_jump"
  [(set (pc)
	(match_operand:SI 0 "arith_reg_operand" "r"))]
  ""
  "jmp	@%0%#"
  [(set_attr "needs_delay_slot" "yes")])

;; ------------------------------------------------------------------------
;; Misc insns
;; ------------------------------------------------------------------------

;; ??? This combiner pattern does not work, because combine does not combine
;; instructions that set a hard register when SMALL_REGISTER_CLASSES is
;; defined.  Perhaps use a pseudo-reg for the T bit?

(define_insn "dect"
  [(parallel [(set (match_operand:SI 0 "arith_reg_operand" "=r")
		   (plus:SI (match_dup 0)
			    (const_int -1)))
	      (set (reg:SI 18)
		   (eq:SI (plus:SI (match_dup 0) (const_int -1))
			  (const_int 0)))])]
  "TARGET_SH2"
  "dt	%0")

(define_insn "nop"
  [(const_int 0)]
  ""
  "nop")

;; Load address of a label. This is only generated by the casesi expand.
;; This must use unspec, because this only works immediately before a casesi.

(define_insn "mova"
  [(set (reg:SI 0)
	(unspec [(label_ref (match_operand 0 "" ""))] 1))]
  ""
  "mova	%O0,r0"
  [(set_attr "in_delay_slot" "no")])

;; case instruction for switch statements.

;; Operand 0 is index
;; operand 1 is the minimum bound
;; operand 2 is the maximum bound - minimum bound + 1
;; operand 3 is CODE_LABEL for the table;
;; operand 4 is the CODE_LABEL to go to if index out of range.

;; ??? There should be a barrier after the jump at the end.

(define_expand "casesi"
  [(set (match_dup 5) (match_operand:SI 0 "arith_reg_operand" ""))
   (set (match_dup 5) (minus:SI (match_dup 5)
				(match_operand:SI 1 "arith_operand" "")))
   (set (reg:SI 18)
	(gtu:SI (match_dup 5)
		(match_operand:SI 2 "arith_reg_operand" "")))
   (set (pc)
	(if_then_else (eq (reg:SI 18)
			  (const_int 1))
		      (label_ref (match_operand 4 "" ""))
		      (pc)))
   (set (match_dup 6) (match_dup 5))
   (set (match_dup 6) (ashift:SI (match_dup 6) (match_dup 7)))
   (set (reg:SI 0) (unspec [(label_ref (match_operand 3 "" ""))] 1))
   (parallel [(set (reg:SI 0) (plus:SI (reg:SI 0)
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
				  (match_operand:SI 0 "arith_reg_operand" "+r")))))
   (set (match_dup 0) (mem:HI (plus:SI (reg:SI 0)
				       (match_dup 0))))]
  ""
  "*
{
  if (TARGET_BIGTABLE)
    return \"mov.l	@(r0,%0),%0\;add	%0,r0\";
  else
    return \"mov.w	@(r0,%0),%0\;add	%0,r0\";
}"
  [(set_attr "length" "4")])

(define_insn "return"
  [(return)]
  "reload_completed"
  "%@	%#"
  [(set_attr "type" "return")
   (set_attr "needs_delay_slot" "yes")])

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
  "movt	%0")

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
	      (clobber (reg:SI 0))])]
  ""
  "jsr	@%0%#"
  [(set_attr "type" "sfunc")
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
  [(set_attr "type" "sfunc")
   (set_attr "needs_delay_slot" "yes")])

;; -------------------------------------------------------------------------
;; Peepholes
;; -------------------------------------------------------------------------

;; This matches cases where a stack pointer increment at the start of the
;; epilogue combines with a stack slot read loading the return value.

(define_peephole
  [(set (match_operand:SI 0 "arith_reg_operand" "")
	(mem:SI (match_operand:SI 1 "arith_reg_operand" "")))
   (set (match_dup 1) (plus:SI (match_dup 1) (const_int 4)))]
  "REGNO (operands[1]) != REGNO (operands[0])"
  "mov.l	@%1+,%0")

;; See the comment on the dt combiner pattern above.

(define_peephole
  [(set (match_operand:SI 0 "arith_reg_operand" "=r")
	(plus:SI (match_dup 0)
		 (const_int -1)))
   (set (reg:SI 18)
	(eq:SI (match_dup 0)
	       (const_int 0)))]
  "TARGET_SH2"
  "dt	%0")

;; These convert sequences such as `mov #k,r0; add r15,r0; mov.l @r0,rn'
;; to `mov #k,r0; mov.l @(r0,r15),rn'.  These sequences are generated by
;; reload when the constant is too large for a reg+offset address.

;; ??? We would get much better code if this was done in reload.  This would
;; require modifying find_reloads_address to recognize that if the constant
;; is out-of-range for an immediate add, then we get better code by reloading
;; the constant into a register than by reloading the sum into a register,
;; since the former is one instruction shorter if the address does not need
;; to be offsettable.  Unfortunately this does not work, because there is
;; only one register, r0, that can be used as an index register.  This register
;; is also the function return value register.  So, if we try to force reload
;; to use double-reg addresses, then we end up with some instructions that
;; need to use r0 twice.  The only way to fix this is to change the calling
;; convention so that r0 is not used to return values.

(define_peephole
  [(set (match_operand:SI 0 "register_operand" "=r")
	(plus:SI (match_dup 0) (match_operand:SI 1 "register_operand" "r")))
   (set (mem:SI (match_dup 0))
	(match_operand:SI 2 "general_movsrc_operand" ""))]
  "REGNO (operands[0]) == 0 && reg_unused_after (operands[0], insn)"
  "mov.l	%2,@(%0,%1)")

(define_peephole
  [(set (match_operand:SI 0 "register_operand" "=r")
	(plus:SI (match_dup 0) (match_operand:SI 1 "register_operand" "r")))
   (set (match_operand:SI 2 "general_movdst_operand" "")
	(mem:SI (match_dup 0)))]
  "REGNO (operands[0]) == 0 && reg_unused_after (operands[0], insn)"
  "mov.l	@(%0,%1),%2")

(define_peephole
  [(set (match_operand:SI 0 "register_operand" "=r")
	(plus:SI (match_dup 0) (match_operand:SI 1 "register_operand" "r")))
   (set (mem:HI (match_dup 0))
	(match_operand:HI 2 "general_movsrc_operand" ""))]
  "REGNO (operands[0]) == 0 && reg_unused_after (operands[0], insn)"
  "mov.w	%2,@(%0,%1)")

(define_peephole
  [(set (match_operand:SI 0 "register_operand" "=r")
	(plus:SI (match_dup 0) (match_operand:SI 1 "register_operand" "r")))
   (set (match_operand:HI 2 "general_movdst_operand" "")
	(mem:HI (match_dup 0)))]
  "REGNO (operands[0]) == 0 && reg_unused_after (operands[0], insn)"
  "mov.w	@(%0,%1),%2")

(define_peephole
  [(set (match_operand:SI 0 "register_operand" "=r")
	(plus:SI (match_dup 0) (match_operand:SI 1 "register_operand" "r")))
   (set (mem:QI (match_dup 0))
	(match_operand:QI 2 "general_movsrc_operand" ""))]
  "REGNO (operands[0]) == 0 && reg_unused_after (operands[0], insn)"
  "mov.b	%2,@(%0,%1)")

(define_peephole
  [(set (match_operand:SI 0 "register_operand" "=r")
	(plus:SI (match_dup 0) (match_operand:SI 1 "register_operand" "r")))
   (set (match_operand:QI 2 "general_movdst_operand" "")
	(mem:QI (match_dup 0)))]
  "REGNO (operands[0]) == 0 && reg_unused_after (operands[0], insn)"
  "mov.b	@(%0,%1),%2")

(define_peephole
  [(set (match_operand:SI 0 "register_operand" "=r")
	(plus:SI (match_dup 0) (match_operand:SI 1 "register_operand" "r")))
   (set (mem:SF (match_dup 0))
	(match_operand:SF 2 "general_movsrc_operand" ""))]
  "REGNO (operands[0]) == 0 && reg_unused_after (operands[0], insn)"
  "mov.l	%2,@(%0,%1)")

(define_peephole
  [(set (match_operand:SI 0 "register_operand" "=r")
	(plus:SI (match_dup 0) (match_operand:SI 1 "register_operand" "r")))
   (set (match_operand:SF 2 "general_movdst_operand" "")

	(mem:SF (match_dup 0)))]
  "REGNO (operands[0]) == 0 && reg_unused_after (operands[0], insn)"
  "mov.l	@(%0,%1),%2")
