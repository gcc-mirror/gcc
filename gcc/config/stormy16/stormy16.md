;; XSTORMY16 Machine description template
;; Copyright (C) 1997-2025 Free Software Foundation, Inc.
;; Contributed by Red Hat, Inc.

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

;;- See file "rtl.def" for documentation on define_insn, match_*, et. al.

;; Constraints
;; a  $0
;; b  $1
;; c  $2
;; d  $8
;; e  $0..$7
;; t  $0..$1
;; z  $8..$9
;; I  0..3
;; J  2**N mask
;; K  2**N antimask
;; L  0..255
;; M  -255..0
;; N  -3..0
;; O  1..4
;; P  -4..-1
;; Q  post-inc mem (push)
;; R  pre-dec mem (pop)
;; S  immediate mem
;; T  Rx
;; U  -inf..1 or 16..inf
;; Z  0

(define_constants
  [
    (CARRY_REG 16)
  ]
)

(define_code_iterator any_lshift [ashift lshiftrt])
(define_code_iterator any_or_plus [plus ior xor])
(define_code_iterator any_rotate [rotate rotatert])

;; ::::::::::::::::::::
;; ::
;; :: Attributes
;; ::
;; ::::::::::::::::::::

; Categorize branches for the conditional in the length attribute.
(define_attr "branch_class" "notdirectbranch,br12,bcc12,bcc8p2,bcc8p4"
    (const_string "notdirectbranch"))

; The length of an instruction, used for branch shortening.
(define_attr "length" ""
  (cond
   [(eq_attr "branch_class" "br12")
     (if_then_else (and (ge (minus (match_dup 0) (pc)) (const_int -2046))
			(lt (minus (match_dup 0) (pc)) (const_int 2048)))
		   (const_int 2)
		   (const_int 4))
    (eq_attr "branch_class" "bcc12")
     (if_then_else (and (ge (minus (match_dup 0) (pc)) (const_int -2044))
			(lt (minus (match_dup 0) (pc)) (const_int 2048)))
		   (const_int 4)
		   (const_int 8))
    (eq_attr "branch_class" "bcc8p2")
     (if_then_else (and (ge (minus (match_dup 0) (pc)) (const_int -124))
			(lt (minus (match_dup 0) (pc)) (const_int 128)))
		   (const_int 4)
		   (const_int 8))
    (eq_attr "branch_class" "bcc8p4")
     (if_then_else (and (ge (minus (match_dup 0) (pc)) (const_int -122))
			(lt (minus (match_dup 0) (pc)) (const_int 128)))
		   (const_int 6)
		   (const_int 10))]
   (const_int 2)))

; The operand which determines the setting of Rpsw.
; The numbers indicate the operand number,
; 'clobber' indicates it is changed in some unspecified way
; 'nop' means it is not changed.
(define_attr "psw_operand" "clobber,nop,0,1,2,3,4" (const_string "0"))

(define_asm_attributes [(set_attr "length" "4")
			(set_attr "psw_operand" "clobber")])

(include "predicates.md")
(include "constraints.md")

;; ::::::::::::::::::::
;; ::
;; :: Moves
;; ::
;; ::::::::::::::::::::
;; push/pop qi and hi are here as separate insns rather than part of
;; the movqi/hi patterns because we need to ensure that reload isn't
;; passed anything it can't cope with.  Without these patterns, we
;; might end up with

;; (set (mem (post_inc (sp))) mem (post_inc (reg)))

;; If, in this example, reg needs reloading, reload will read reg from
;; the stack , adjust sp, and store reg back at what is now the wrong
;; offset.  By using separate patterns for push and pop we ensure that
;; insns like this one are never generated.

(define_insn "pushqi1"
  [(set (mem:QI (post_inc:HI (reg:HI 15)))
	(match_operand:QI 0 "register_operand" "r"))]
  ""
  "push %0"
  [(set_attr "psw_operand" "nop")
   (set_attr "length" "2")])

(define_insn "popqi1"
  [(set (match_operand:QI 0 "register_operand" "=r")
	(mem:QI (pre_dec:HI (reg:HI 15))))]
  ""
  "pop %0"
  [(set_attr "psw_operand" "nop")
   (set_attr "length" "2")])

(define_expand "movqi"
  [(set (match_operand:QI 0 "nonimmediate_nonstack_operand" "")
	(match_operand:QI 1 "general_operand" ""))]
  ""
  { xstormy16_expand_move (QImode, operands[0], operands[1]);
    DONE;
  })

(define_insn "movqi_internal"
  [(set (match_operand:QI 0 "nonimmediate_nonstack_operand" "=r,m,e,e,T,r,S,W,e")
	(match_operand:QI 1 "general_operand"       "r,e,m,i,i,i,i,ie,W"))]
  ""
  "@
   mov %0,%1
   mov.b %0,%1
   mov.b %0,%1
   mov %0,%1
   mov Rx,%1
   mov %0,%1
   mov.b %0,%1
   mov.b %0,%1
   mov.b %0,%1"
  [(set_attr_alternative "length"
	     [(const_int 2)
	      (if_then_else (match_operand:QI 0 "short_memory_operand" "")
			    (const_int 2)
			    (const_int 4))
	      (if_then_else (match_operand:QI 1 "short_memory_operand" "")
			    (const_int 2)
			    (const_int 4))
	      (const_int 2)
	      (const_int 2)
	      (const_int 4)
	      (const_int 4)
	      (const_int 2)
	      (const_int 2)])
   (set_attr "psw_operand" "0,0,0,0,nop,0,nop,0,0")])

(define_insn "pushhi1"
  [(set (mem:HI (post_inc:HI (reg:HI 15)))
	(match_operand:HI 0 "register_operand" "r"))]
  ""
  "push %0"
  [(set_attr "psw_operand" "nop")
   (set_attr "length" "2")])

(define_insn "pophi1"
  [(set (match_operand:HI 0 "register_operand" "=r")
	(mem:HI (pre_dec:HI (reg:HI 15))))]
  ""
  "pop %0"
  [(set_attr "psw_operand" "nop")
   (set_attr "length" "2")])

(define_expand "movhi"
  [(set (match_operand:HI 0 "nonimmediate_nonstack_operand" "")
	(match_operand:HI 1 "general_operand" ""))]
  ""
  { xstormy16_expand_move (HImode, operands[0], operands[1]);
    DONE;
  })

(define_insn "movhi_internal"
  [(set (match_operand:HI 0 "nonimmediate_nonstack_operand" "=r,m,e,e,T,r,S,W,e")
	(match_operand:HI 1 "general_operand"                "r,e,m,L,L,i,i,ie,W"))]
  ""
  "@
   mov %0,%1
   mov.w %0,%1
   mov.w %0,%1
   mov.w %0,%1
   mov.w Rx,%1
   mov.w %0,%1
   mov.w %0,%1
   mov.w %0,%1
   mov.w %0,%1"
  [(set_attr_alternative "length"
	     [(const_int 2)
	      (if_then_else (match_operand:QI 0 "short_memory_operand" "")
			    (const_int 2)
			    (const_int 4))
	      (if_then_else (match_operand:QI 1 "short_memory_operand" "")
			    (const_int 2)
			    (const_int 4))
	      (const_int 2)
	      (const_int 2)
	      (const_int 4)
	      (const_int 4)
	      (const_int 4)
	      (const_int 4)])
   (set_attr "psw_operand" "0,0,0,0,nop,0,nop,0,0")])

(define_expand "movsi"
  [(set (match_operand:SI 0 "nonimmediate_operand" "")
	(match_operand:SI 1 "general_operand" ""))]
  ""
  { xstormy16_expand_move (SImode, operands[0], operands[1]);
    DONE;
  })

(define_insn_and_split "*movsi_internal"
  [(set (match_operand:SI 0 "nonimmediate_operand" "=r,Q,r,m,e,&e,e,r,S")
	(match_operand:SI 1 "general_operand"       "r,r,R,e,o, V,L,i,i"))]
  ""
  "#"
  "reload_completed"
  [(pc)]
  { xstormy16_split_move (SImode, operands[0], operands[1]);
    DONE;
  }
  [(set_attr_alternative "length"
	     [(const_int 4)
	      (const_int 4)
	      (const_int 4)
	      (if_then_else (match_operand:QI 0 "short_memory_operand" "")
			    (const_int 6)
			    (const_int 8))
	      (if_then_else (match_operand:QI 1 "short_memory_operand" "")
			    (const_int 6)
			    (const_int 8))
	      (if_then_else (match_operand:QI 1 "short_memory_operand" "")
			    (const_int 6)
			    (const_int 8))
	      (const_int 4)
	      (const_int 8)
	      (const_int 8)])])

;; ::::::::::::::::::::
;; ::
;; :: Conversions
;; ::
;; ::::::::::::::::::::

(define_insn "extendqihi2"
  [(set (match_operand:HI 0 "register_operand" "=r")
	(sign_extend:HI (match_operand:QI 1 "register_operand" "0")))]
  ""
  "cbw %0")

(define_insn "extendhisi2"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(sign_extend:SI (match_operand:HI 1 "register_operand" "0")))]
  ""
  "mov %h0,%0 | asr %h0,#15"
  [(set_attr "length" "4")
   (set_attr "psw_operand" "clobber")])

(define_insn "zero_extendqihi2"
  [(set (match_operand:HI                 0 "register_operand" 	   "=e,T,r")
	(zero_extend:HI (match_operand:QI 1 "nonimmediate_operand" "m,0,0")))]
  ""
  "@
   mov.b %0,%1
   and Rx,#255
   shl %0,#8 | shr %0,#8"
  [(set_attr "psw_operand" "nop,nop,0")
   (set_attr_alternative "length"
	     [(const_int 4)
	      (const_int 2)
	      (const_int 4)])])

(define_insn "zero_extendhisi2"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(zero_extend:SI (match_operand:HI 1 "register_operand" "0")))]
  ""
  "mov %h0,#0"
  [(set_attr "length" "4")
   (set_attr "psw_operand" "clobber")])

;; ::::::::::::::::::::
;; ::
;; :: Bit field extraction
;; ::
;; ::::::::::::::::::::

;; Extract an unsigned bit field
;(define_insn "extzv"
;  [(set (match_operand:SI 0 "register_operand" "=r")
;	(zero_extract:SI (match_operand:SI 1 "register_operand" "r")
;			 (match_operand:SI 2 "const_int_operand" "n")
;			 (match_operand:SI 3 "const_int_operand" "n")))]
;  ""
;  "extzv %0,%1,%2,%3"
;  [(set_attr "length" "4")])

;; Insert a bit field
;(define_insn "insv"
;  [(set (zero_extract:SI (match_operand:SI 0 "register_operand" "+r")
;			 (match_operand:SI 1 "const_int_operand" "n")
;			 (match_operand:SI 2 "const_int_operand" "n"))
;	(match_operand:SI 3 "nonmemory_operand" "ri"))]
;  ""
;  "insv %0,%1,%2,%3"
;  [(set_attr "length" "4")])


;; ::::::::::::::::::::
;; ::
;; :: 16-bit Integer arithmetic
;; ::
;; ::::::::::::::::::::

;; Addition
; Note - the early clobber modifier is no longer needed on operand 3
; and in fact can cause some reload spill failures if it is present.
; Note that the 'Z' constraint matches "add $reg,0", which reload
; will occasionally emit.  We avoid the "add $reg,imm" match because
; it clobbers the carry.
(define_insn "addhi3"
  [(set (match_operand:HI 0 "register_operand" "=r,r,r,T,T,r,r,r")
	(plus:HI (match_operand:HI 1 "register_operand" "%0,0,0,0,0,0,0,0")
		 (match_operand:HI 2 "xs_hi_nonmemory_operand" "O,P,Z,L,M,Ir,N,i")))
   (clobber (reg:BI CARRY_REG))]
  ""
  "@
   inc %0,%o2
   dec %0,%O2
   ;
   add Rx,%2
   sub Rx,#%n2
   add %0,%2
   sub %0,#%n2
   add %0,%2"
  [(set_attr "length" "2,2,0,2,2,2,2,4")])

(define_insn "addchi4"
  [(set (match_operand:HI 0 "register_operand" "=T,r,r")
	(plus:HI (match_operand:HI 1 "register_operand" "%0,0,0")
		 (match_operand:HI 2 "xs_hi_nonmemory_operand" "L,Ir,i")))
   (set (reg:BI CARRY_REG)
        (truncate:BI (lshiftrt:SI (plus:SI (zero_extend:SI (match_dup 1))
					   (zero_extend:SI (match_dup 2)))
				  (const_int 16))))]
  ""
  "@
   add Rx,%2
   add %0,%2
   add %0,%2"
  [(set_attr "length" "2,2,4")])

(define_insn "addchi5"
  [(set (match_operand:HI 0 "register_operand" "=T,r,r")
	(plus:HI (plus:HI (match_operand:HI 1 "register_operand" "%0,0,0")
			  (zero_extend:HI (reg:BI CARRY_REG)))
		 (match_operand:HI 2 "xs_hi_nonmemory_operand" "L,Ir,i")))
   (set (reg:BI CARRY_REG)
        (truncate:BI (lshiftrt:SI (plus:SI (plus:SI
					    (zero_extend:SI (match_dup 1))
					    (zero_extend:SI (reg:BI CARRY_REG)))
					   (zero_extend:SI (match_dup 2)))
				  (const_int 16))))]
  ""
  "@
   adc Rx,%2
   adc %0,%2
   adc %0,%2"
  [(set_attr "length" "2,2,4")])

;; Subtraction
; Operand 3 is marked earlyclobber because that helps reload
; to generate better code---this pattern will never need the
; carry register as an input, and some output reloads or input
; reloads might need to use it.  In fact, without the '&' reload
; will fail in some cases.
(define_insn "subhi3"
  [(set (match_operand:HI 0 "register_operand" "=r,r,T,T,r,r,r")
	(minus:HI (match_operand:HI 1 "register_operand" "0,0,0,0,0,0,0")
		  (match_operand:HI 2 "xs_hi_nonmemory_operand" "O,P,L,M,rI,M,i")))
   (clobber (reg:BI CARRY_REG))]
  ""
  "@
   dec %0,%o2
   inc %0,%O2
   sub Rx,%2
   add Rx,#%n2
   sub %0,%2
   add %0,#%n2
   sub %0,%2"
  [(set_attr "length" "2,2,2,2,2,2,4")])

(define_insn "subchi4"
  [(set (match_operand:HI 0 "register_operand" "=T,r,r")
	(minus:HI (match_operand:HI 1 "register_operand" "0,0,0")
		  (match_operand:HI 2 "xs_hi_nonmemory_operand" "L,Ir,i")))
   (set (reg:BI CARRY_REG)
        (truncate:BI (lshiftrt:SI (minus:SI (zero_extend:SI (match_dup 1))
					    (zero_extend:SI (match_dup 2)))
				  (const_int 16))))]
  ""
  "@
   sub Rx,%2
   sub %0,%2
   sub %0,%2"
  [(set_attr "length" "2,2,4")])

(define_insn "subchi5"
  [(set (match_operand:HI 0 "register_operand" "=T,r,r")
	(minus:HI (minus:HI (match_operand:HI 1 "register_operand" "0,0,0")
			  (zero_extend:HI (reg:BI CARRY_REG)))
		 (match_operand:HI 2 "xs_hi_nonmemory_operand" "L,Ir,i")))
   (set (reg:BI CARRY_REG)
        (truncate:BI (lshiftrt:SI (minus:SI (minus:SI
					     (zero_extend:SI (match_dup 1))
					     (zero_extend:SI (reg:BI CARRY_REG)))
					    (zero_extend:SI (match_dup 2)))
				  (const_int 16))))]
  ""
  "@
   sbc Rx,%2
   sbc %0,%2
   sbc %0,%2"
  [(set_attr "length" "2,2,4")])

; Basic multiplication
(define_insn "mulhi3"
  [(set (match_operand:HI 0 "register_operand" "=a")
	(mult:HI (match_operand:HI 1 "register_operand" "%a")
		 (match_operand:HI 2 "register_operand" "c")))
   (clobber (match_scratch:HI 3 "=b"))
   ]
  ""
  "mul"
  [(set_attr "psw_operand" "nop")])

;; Unsigned multiplication producing 64-bit results from 32-bit inputs
; The constraint on operand 0 is 't' because it is actually two regs
; long, and both regs must match the constraint.
(define_insn "umulhisi3"
  [(set (match_operand:SI 0 "register_operand" "=t")
	(mult:SI (zero_extend:SI (match_operand:HI 1 "register_operand" "%a"))
		 (zero_extend:SI (match_operand:HI 2 "register_operand" "c"))))
   ]
  ""
  "mul"
  [(set_attr "psw_operand" "nop")])

;; Unsigned division giving both quotient and remainder
(define_insn "udivmodhi4"
  [(set (match_operand:HI 0 "register_operand" "=a")
	(udiv:HI (match_operand:HI 1 "register_operand" "a")
		 (match_operand:HI 2 "register_operand" "c")))
   (set (match_operand:HI 3 "register_operand" "=b")
	(umod:HI (match_dup 1)
		 (match_dup 2)))]
  ""
  "div"
  [(set_attr "psw_operand" "nop")])

;; Signed division giving both quotient and remainder
(define_insn "divmodhi4"
  [(set (match_operand:HI 0 "register_operand" "=a")
	(div:HI (match_operand:HI 1 "register_operand" "a")
		 (match_operand:HI 2 "register_operand" "c")))
   (set (match_operand:HI 3 "register_operand" "=b")
	(mod:HI (match_dup 1)
		 (match_dup 2)))]
  ""
  "sdiv"
  [(set_attr "psw_operand" "nop")])

;; Signed 32/16 division
(define_insn "sdivlh"
  [(set (match_operand:HI 0 "register_operand" "=a")
	(div:HI (match_operand:SI 2 "register_operand" "t")
		 (match_operand:HI 3 "register_operand" "c")))
   (set (match_operand:HI 1 "register_operand" "=b")
	(mod:HI (match_dup 2)
		 (match_dup 3)))]
  ""
  "sdivlh"
  [(set_attr "psw_operand" "nop")])

;; Unsigned 32/16 division
(define_insn "udivlh"
  [(set (match_operand:HI 0 "register_operand" "=a")
	(udiv:HI (match_operand:SI 2 "register_operand" "t")
		 (match_operand:HI 3 "register_operand" "c")))
   (set (match_operand:HI 1 "register_operand" "=b")
	(umod:HI (match_dup 2)
		 (match_dup 3)))]
  ""
  "divlh"
  [(set_attr "psw_operand" "nop")])

;; Negation

(define_insn "neghi2"
  [(set (match_operand:HI 0 "register_operand" "=r")
	(neg:HI (match_operand:HI 1 "register_operand" "0")))]
  ""
  "not %0 | inc %0"
  [(set_attr "length" "4")])

;; ::::::::::::::::::::
;; ::
;; :: 16-bit Integer Shifts and Rotates
;; ::
;; ::::::::::::::::::::

;; Arithmetic Shift Left
(define_insn "ashlhi3"
  [(set (match_operand:HI 0 "register_operand" "=r")
	(ashift:HI (match_operand:HI 1 "register_operand" "0")
		   (match_operand:HI 2 "nonmemory_operand" "ri")))
   (clobber (reg:BI CARRY_REG))]
  ""
  "shl %0,%2")

;; Arithmetic Shift Right
(define_insn "ashrhi3"
  [(set (match_operand:HI 0 "register_operand" "=r")
	(ashiftrt:HI (match_operand:HI 1 "register_operand" "0")
		     (match_operand:HI 2 "nonmemory_operand" "ri")))
   (clobber (reg:BI CARRY_REG))]
  ""
  "asr %0,%2")

;; Logical Shift Right
(define_insn "lshrhi3"
  [(set (match_operand:HI 0 "register_operand" "=r")
	(lshiftrt:HI (match_operand:HI 1 "register_operand" "0")
		     (match_operand:HI 2 "nonmemory_operand" "ri")))
   (clobber (reg:BI CARRY_REG))]
  ""
  "shr %0,%2")

;; HImode rotate left by 1 bit
(define_insn "*rotatehi_1"
  [(set (match_operand:HI 0 "register_operand" "=r")
	(rotate:HI (match_operand:HI 1 "register_operand" "0")
		   (const_int 1)))
   (clobber (reg:BI CARRY_REG))]
  ""
  "shl %0,#1 | adc %0,#0"
  [(set_attr "length" "4")])

;; HImode rotate left by 8 bits
(define_insn "*<code>hi_8"
  [(set (match_operand:HI 0 "register_operand" "=r")
	(any_rotate:HI (match_operand:HI 1 "register_operand" "0")
		       (const_int 8)))]
  ""
  "swpb %0")

;; ::::::::::::::::::::
;; ::
;; :: 16-Bit Integer Logical operations
;; ::
;; ::::::::::::::::::::

;; Logical AND, 16-bit integers
(define_insn "andhi3"
  [(set (match_operand:HI 0 "xstormy16_splittable_below100_or_register" "=T,r,r,r,W")
	(and:HI (match_operand:HI 1 "xstormy16_below100_or_register" "%0,0,0,0,0")
		(match_operand:HI 2 "nonmemory_operand" "L,r,K,i,K")))]
  ""
  "@
   and Rx,%2
   and %0,%2
   clr1 %0,%B2
   and %0,%2
   #"
  [(set_attr "length" "2,2,2,4,2")])

(define_split
  [(set (match_operand:HI 0 "xstormy16_below100_operand" "")
	(and:HI (match_operand:HI 1 "xstormy16_below100_operand" "")
		(match_operand:HI 2 "xstormy16_onebit_clr_operand" "")))]
  ""
  [(set (match_dup 3)
	(and:QI (match_dup 4)
		(match_dup 5)))]
  { int s = ((INTVAL (operands[2]) & 0xff) == 0xff) ? 1 : 0;
    operands[3] = simplify_gen_subreg (QImode, operands[0], HImode, s);
    operands[4] = simplify_gen_subreg (QImode, operands[1], HImode, s);
    operands[5] = simplify_gen_subreg (QImode, operands[2], HImode, s);
    operands[5] = GEN_INT (INTVAL (operands[5]) | ~ (HOST_WIDE_INT) 0xff);
  })

;; Inclusive OR, 16-bit integers
(define_insn "iorhi3"
  [(set (match_operand:HI 0 "xstormy16_splittable_below100_or_register" "=T,r,r,r,W")
	(ior:HI (match_operand:HI 1 "xstormy16_below100_or_register" "%0,0,0,0,0")
		(match_operand:HI 2 "nonmemory_operand" "L,r,J,i,J")))]
  ""
  "@
   or Rx,%2
   or %0,%2
   set1 %0,%B2
   or %0,%2
   #"
  [(set_attr "length" "2,2,2,4,2")])

(define_split
  [(set (match_operand:HI 0 "xstormy16_below100_operand" "")
	(ior:HI (match_operand:HI 1 "xstormy16_below100_operand" "")
		(match_operand:HI 2 "xstormy16_onebit_set_operand" "")))]
  ""
  [(set (match_dup 3)
	(ior:QI (match_dup 4)
		(match_dup 5)))]
  { int s = ((INTVAL (operands[2]) & 0xff) == 0x00) ? 1 : 0;
    operands[3] = simplify_gen_subreg (QImode, operands[0], HImode, s);
    operands[4] = simplify_gen_subreg (QImode, operands[1], HImode, s);
    operands[5] = simplify_gen_subreg (QImode, operands[2], HImode, s);
    operands[5] = GEN_INT (INTVAL (operands[5]) & 0xff);
  })

;; Exclusive OR, 16-bit integers
(define_insn "xorhi3"
  [(set (match_operand:HI 0 "register_operand" "=T,r,r")
	(xor:HI (match_operand:HI 1 "register_operand" "%0,0,0")
		(match_operand:HI 2 "nonmemory_operand" "L,r,i")))]
  ""
  "@
   xor Rx,%2
   xor %0,%2
   xor %0,%2"
  [(set_attr "length" "2,2,4")])

;; One's complement, 16-bit integers
(define_insn "one_cmplhi2"
  [(set (match_operand:HI 0 "register_operand" "=r")
	(not:HI (match_operand:HI 1 "register_operand" "0")))]
  ""
  "not %0")

;; ::::::::::::::::::::
;; ::
;; :: 32-bit Integer arithmetic
;; ::
;; ::::::::::::::::::::

;; Addition
(define_insn_and_split "addsi3"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(plus:SI (match_operand:SI 1 "register_operand" "%0")
		 (match_operand:SI 2 "nonmemory_operand" "ri")))
   (clobber (reg:BI CARRY_REG))]
  ""
  "#"
  "reload_completed"
  [(pc)]
  { xstormy16_expand_arith (SImode, PLUS, operands[0], operands[1],
			    operands[2]);
    DONE;
  }
  [(set_attr "length" "4")])

;; Subtraction
(define_insn_and_split "subsi3"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(minus:SI (match_operand:SI 1 "register_operand" "0")
		 (match_operand:SI 2 "nonmemory_operand" "ri")))
   (clobber (reg:BI CARRY_REG))]
  ""
  "#"
  "reload_completed"
  [(pc)]
  { xstormy16_expand_arith (SImode, MINUS, operands[0], operands[1],
			    operands[2]);
    DONE;
  }
  [(set_attr "length" "4")])

(define_expand "negsi2"
  [(parallel [(set (match_operand:SI 0 "register_operand" "")
		   (neg:SI (match_operand:SI 1 "register_operand" "")))
	      (clobber (reg:BI CARRY_REG))])]
  "")

(define_insn_and_split "*negsi2_internal"
  [(set (match_operand:SI 0 "register_operand" "=&r")
	(neg:SI (match_operand:SI 1 "register_operand" "r")))
   (clobber (reg:BI CARRY_REG))]
  ""
  "#"
  "reload_completed"
  [(pc)]
  { xstormy16_expand_arith (SImode, NEG, operands[0], operands[0],
			    operands[1]);
    DONE;
  })

;; ::::::::::::::::::::
;; ::
;; :: 32-bit Integer Shifts and Rotates
;; ::
;; ::::::::::::::::::::

;; Arithmetic Shift Left
(define_expand "ashlsi3"
  [(parallel [(set (match_operand:SI 0 "register_operand" "")
		   (ashift:SI (match_operand:SI 1 "register_operand" "")
			      (match_operand:SI 2 "const_int_operand" "")))
	      (clobber (reg:BI CARRY_REG))
	      (clobber (match_dup 3))])]
  ""
  { if (! const_int_operand (operands[2], SImode))
      FAIL;
    operands[3] = gen_reg_rtx (HImode);
  })

;; Arithmetic Shift Right
(define_expand "ashrsi3"
  [(parallel [(set (match_operand:SI 0 "register_operand" "")
		   (ashiftrt:SI (match_operand:SI 1 "register_operand" "")
			        (match_operand:SI 2 "const_int_operand" "")))
	      (clobber (reg:BI CARRY_REG))
	      (clobber (match_dup 3))])]
  ""
  { if (! const_int_operand (operands[2], SImode))
      FAIL;
    operands[3] = gen_reg_rtx (HImode);
  })

;; Logical Shift Right
(define_expand "lshrsi3"
  [(parallel [(set (match_operand:SI 0 "register_operand" "")
		   (lshiftrt:SI (match_operand:SI 1 "register_operand" "")
			        (match_operand:SI 2 "const_int_operand" "")))
	      (clobber (reg:BI CARRY_REG))
	      (clobber (match_dup 3))])]
  ""
  { if (! const_int_operand (operands[2], SImode))
      FAIL;
    operands[3] = gen_reg_rtx (HImode);
  })

(define_insn "*shiftsi"
  [(set (match_operand:SI 0 "register_operand" "=r,r")
	(match_operator:SI 4 "shift_operator"
	 [(match_operand:SI 1 "register_operand" "0,0")
	  (match_operand:SI 2 "const_int_operand" "U,n")]))
   (clobber (reg:BI CARRY_REG))
   (clobber (match_operand:HI 3 "" "=X,r"))]
  ""
  "* return xstormy16_output_shift (SImode, GET_CODE (operands[4]),
				    operands[0], operands[2], operands[3]);"
  [(set_attr "length" "6,10")
   (set_attr "psw_operand" "clobber,clobber")])


;; ::::::::::::::::::::
;; ::
;; :: Branches
;; ::
;; ::::::::::::::::::::

(define_expand "cbranchhi4"
  [(set (pc)
	(if_then_else (match_operator 0 "comparison_operator"
				      [(match_operand:HI 1 "register_operand" "")
				       (match_operand:HI 2 "nonmemory_operand" "")])
		      (label_ref (match_operand 3 "" ""))
		      (pc)))
   (clobber (reg:BI CARRY_REG))]
  ""
  {
  xstormy16_emit_cbranch (GET_CODE (operands[0]), operands[1], operands[2],
			  operands[3]);
  DONE;
})

(define_insn "cbranchhi"
  [(set (pc)
	(if_then_else (match_operator:HI 1 "comparison_operator"
				      [(match_operand:HI 2 "nonmemory_operand"
					"r,e,L")
				       (match_operand:HI 3 "nonmemory_operand"
						      "r,L,e")])
		      (label_ref (match_operand 0 "" ""))
		      (pc)))
   (clobber (reg:BI CARRY_REG))]
  ""
  "*
{
  return xstormy16_output_cbranch_hi (operands[1], \"%l0\", 0, insn);
}"
  [(set_attr "branch_class" "bcc12")
   (set_attr "psw_operand" "0,0,1")])

(define_insn "cbranchhi_neg"
  [(set (pc)
	(if_then_else (match_operator:HI 1 "comparison_operator"
				      [(match_operand:HI 2 "nonmemory_operand"
							 "r,e,L")
				       (match_operand:HI 3 "nonmemory_operand"
							 "r,L,e")])
		      (pc)
		      (label_ref (match_operand 0 "" ""))))
   (clobber (reg:BI CARRY_REG))]
  ""
  "*
{
  return xstormy16_output_cbranch_hi (operands[1], \"%l0\", 1, insn);
}"
  [(set_attr "branch_class" "bcc12")
   (set_attr "psw_operand" "0,0,1")])

(define_insn "*eqbranchsi"
  [(set (pc)
	(if_then_else (match_operator:SI 1 "equality_operator"
				      [(match_operand:SI 2 "register_operand"
							 "r")
				       (const_int 0)])
		      (label_ref (match_operand 0 "" ""))
		      (pc)))
   (clobber (match_operand:SI 3 "register_operand" "=2"))]
  ""
  "*
{
  return xstormy16_output_cbranch_si (operands[1], \"%l0\", 0, insn);
}"
  [(set_attr "branch_class" "bcc8p2")
   (set_attr "psw_operand" "clobber")])

(define_insn "*ineqbranch_1"
  [(set (pc)
	(if_then_else (match_operator:HI 4 "xstormy16_ineqsi_operator"
		       [(minus:HI (match_operand:HI 1 "register_operand" "T,r,r")
			   (zero_extend:HI (reg:BI CARRY_REG)))
			(match_operand:HI 3 "nonmemory_operand" "L,r,i")])
		      (label_ref (match_operand 0 "" ""))
		      (pc)))
   (set (match_operand:HI 2 "register_operand" "=1,1,1")
	(minus:HI (minus:HI (match_dup 1) (zero_extend:HI (reg:BI CARRY_REG)))
		  (match_dup 3)))
   (clobber (reg:BI CARRY_REG))]
  ""
  "*
{
  return xstormy16_output_cbranch_si (operands[4], \"%l0\", 0, insn);
}"
  [(set_attr "branch_class" "bcc8p2,bcc8p2,bcc8p4")
   (set_attr "psw_operand" "2,2,2")])

;; ::::::::::::::::::::
;; ::
;; :: Call and branch instructions
;; ::
;; ::::::::::::::::::::

;; Subroutine call instruction returning no value.  Operand 0 is the function
;; to call; operand 1 is the number of bytes of arguments pushed (in mode
;; `SImode', except it is normally a `const_int'); operand 2 is the number of
;; registers used as operands.

;; On most machines, operand 2 is not actually stored into the RTL pattern.  It
;; is supplied for the sake of some RISC machines which need to put this
;; information into the assembler code; they can put it in the RTL instead of
;; operand 1.

(define_expand "call"
  [(call (match_operand:HI 0 "memory_operand" "m")
	 (match_operand 1 "" ""))
   (use (match_operand 2 "immediate_operand" ""))]
  ""
  "xstormy16_expand_call (NULL_RTX, operands[0], operands[1]); DONE;")

;; Subroutine call instruction returning a value.  Operand 0 is the hard
;; register in which the value is returned.  There are three more operands, the
;; same as the three operands of the `call' instruction (but with numbers
;; increased by one).

;; Subroutines that return `BLKmode' objects use the `call' insn.

(define_expand "call_value"
  [(set (match_operand 0 "register_operand" "=r")
	(call (match_operand:HI 1 "memory_operand" "m")
	      (match_operand:SI 2 "" "")))
	(use (match_operand 3 "immediate_operand" ""))]
  ""
  "xstormy16_expand_call (operands[0], operands[1], operands[2]); DONE;")

(define_insn "*call_internal"
  [(call (mem:HI (match_operand:HI 0 "nonmemory_operand" "i,r"))
	 (match_operand 1 "" ""))
   (use (match_operand:HI 2 "nonmemory_operand" "X,z"))]
  ""
  "@
   callf %C0
   call %2,%0"
  [(set_attr "length" "4,2")
   (set_attr "psw_operand" "clobber")])

(define_insn "*call_value_internal"
  [(set (match_operand 3 "register_operand" "=r,r")
        (call (mem:HI (match_operand:HI 0 "nonmemory_operand" "i,r"))
	      (match_operand 1 "" "")))
   (use (match_operand:HI 2 "nonmemory_operand" "X,z"))]
  ""
  "@
   callf %C0
   call %2,%0"
  [(set_attr "length" "4,2")
   (set_attr "psw_operand" "clobber")])

;; Subroutine return
(define_expand "return"
  [(return)]
  "direct_return()"
  "")

(define_insn "return_internal"
  [(return)]
  ""
  "ret"
  [(set_attr "psw_operand" "nop")])

(define_insn "return_internal_interrupt"
  [(return)
   (unspec_volatile [(const_int 0)] 1)]
  ""
  "iret"
  [(set_attr "psw_operand" "clobber")])

;; Normal unconditional jump
(define_insn "jump"
  [(set (pc) (label_ref (match_operand 0 "" "")))]
  ""
  "*
{
  return xstormy16_output_cbranch_hi (NULL_RTX, \"%l0\", 0, insn);
}"
  [(set_attr "branch_class" "br12")
   (set_attr "psw_operand" "nop")])

;; Indirect jump through a register
(define_expand "indirect_jump"
  [(set (match_dup 1) (const_int 0))
   (parallel [(set (pc) (match_operand:HI 0 "register_operand" ""))
	      (use (match_dup 1))])]
  ""
  "operands[1] = gen_reg_rtx (HImode);")

(define_insn ""
  [(set (pc) (match_operand:HI 0 "register_operand" "r"))
   (use (match_operand:HI 1 "register_operand" "z"))]
  ""
  "jmp %1,%0"
  [(set_attr "length" "4")
   (set_attr "psw_operand" "nop")])

;; Table-based switch statements.
(define_expand "casesi"
  [(use (match_operand:SI 0 "register_operand" ""))
   (use (match_operand:SI 1 "immediate_operand" ""))
   (use (match_operand:SI 2 "immediate_operand" ""))
   (use (label_ref (match_operand 3 "" "")))
   (use (label_ref (match_operand 4 "" "")))]
  ""
  "
{
  xstormy16_expand_casesi (operands[0], operands[1], operands[2],
			  operands[3], operands[4]);
  DONE;
}")

(define_insn "tablejump_pcrel"
  [(set (pc) (mem:HI (plus:HI (pc)
			      (match_operand:HI 0 "register_operand" "r"))))
   (use (label_ref:SI (match_operand 1 "" "")))]
  ""
  "br %0"
  [(set_attr "psw_operand" "nop")])

;; ::::::::::::::::::::
;; ::
;; :: Prologue and Epilogue instructions
;; ::
;; ::::::::::::::::::::

;; Called after register allocation to add any instructions needed for
;; the prologue.  Using a prologue insn is favored compared to putting
;; all of the instructions in the TARGET_ASM_FUNCTION_PROLOGUE macro,
;; since it allows the scheduler to intermix instructions with the
;; saves of the caller saved registers.  In some cases, it might be
;; necessary to emit a barrier instruction as the last insn to prevent
;; such scheduling.
(define_expand "prologue"
  [(const_int 1)]
  ""
  {
    xstormy16_expand_prologue ();
    DONE;
  })

;; Called after register allocation to add any instructions needed for
;; the epilogue.  Using an epilogue insn is favored compared to putting
;; all of the instructions in the TARGET_ASM_FUNCTION_EPILOGUE macro,
;; since it allows the scheduler to intermix instructions with the
;; restores of the caller saved registers.  In some cases, it might be
;; necessary to emit a barrier instruction as the first insn to
;; prevent such scheduling.
(define_expand "epilogue"
  [(const_int 2)]
  ""
  {
    xstormy16_expand_epilogue ();
    DONE;
  })

;; ::::::::::::::::::::
;; ::
;; :: Miscellaneous instructions
;; ::
;; ::::::::::::::::::::

;; No operation, needed in case the user uses -g but not -O.
(define_insn "nop"
  [(const_int 0)]
  ""
  "nop"
  [(set_attr "psw_operand" "nop")])

;; Pseudo instruction that prevents the scheduler from moving code above this
;; point.
(define_insn "blockage"
  [(unspec_volatile [(const_int 0)] 0)]
  ""
  ""
  [(set_attr "length" "0")
   (set_attr "psw_operand" "nop")])

;;---------------------------------------------------------------------------

(define_expand "iorqi3"
  [(match_operand:QI 0 "xstormy16_below100_or_register" "")
   (match_operand:QI 1 "xstormy16_below100_or_register" "")
   (match_operand:QI 2 "nonmemory_operand" "")]
  ""
  {
    xstormy16_expand_iorqi3 (operands);
    DONE;
  })

(define_insn "iorqi3_internal"
  [(set (match_operand:QI 0 "xstormy16_below100_or_register" "=Wr")
	(ior:QI (match_operand:QI 1 "xstormy16_below100_or_register" "0")
		(match_operand:QI 2 "xstormy16_onebit_set_operand" "i")))]
  ""
  "set1 %0,%B2"
  [(set_attr "length" "2")
   (set_attr "psw_operand" "0")])

(define_peephole2
  [(set (match_operand:QI 0 "register_operand" "")
	(match_operand:QI 1 "xstormy16_below100_operand" ""))
   (set (match_operand:HI 2 "register_operand" "")
	(ior:HI (match_operand:HI 3 "register_operand" "")
		(match_operand:QI 4 "xstormy16_onebit_set_operand" "")))
   (set (match_operand:QI 5 "xstormy16_below100_operand" "")
	(match_operand:QI 6 "register_operand" ""))
   ]
  "REGNO (operands[0]) == REGNO (operands[2])
   && REGNO (operands[0]) == REGNO (operands[3])
   && REGNO (operands[0]) == REGNO (operands[6])
   && rtx_equal_p (operands[1], operands[5])"
  [(set (match_dup 1)
	(ior:QI (match_dup 1)
		(match_dup 4)))
   ]
  "")


(define_expand "andqi3"
  [(match_operand:QI 0 "xstormy16_below100_or_register" "")
   (match_operand:QI 1 "xstormy16_below100_or_register" "")
   (match_operand:QI 2 "nonmemory_operand" "")]
  ""
  {
    xstormy16_expand_andqi3 (operands);
    DONE;
  })

(define_insn "andqi3_internal"
  [(set (match_operand:QI 0 "xstormy16_below100_or_register" "=Wr")
	(and:QI (match_operand:QI 1 "xstormy16_below100_or_register" "0")
		(match_operand:QI 2 "xstormy16_onebit_clr_operand" "i")))]
  ""
  "clr1 %0,%B2"
  [(set_attr "length" "2")
   (set_attr "psw_operand" "0")])

(define_peephole2
  [(set (match_operand:HI 0 "register_operand" "")
	(and:HI (match_operand:HI 1 "register_operand" "")
		(match_operand 2 "immediate_operand" "")))
   (set (match_operand:HI 3 "register_operand" "")
	(zero_extend:HI (match_operand:QI 4 "register_operand" "")));
   ]
  "REGNO (operands[0]) == REGNO (operands[1])
   && REGNO (operands[0]) == REGNO (operands[3])
   && REGNO (operands[0]) == REGNO (operands[4])"
  [(set (match_dup 0)
	(and:HI (match_dup 1)
		(match_dup 5)))
   ]
  "operands[5] = GEN_INT (INTVAL (operands[2]) & 0xff);")

(define_peephole2
  [(set (match_operand:QI 0 "register_operand" "")
	(match_operand:QI 1 "xstormy16_below100_operand" ""))
   (set (match_operand:HI 2 "register_operand" "")
	(and:HI (match_operand:HI 3 "register_operand" "")
		(match_operand:QI 4 "xstormy16_onebit_clr_operand" "")))
   (set (match_operand:QI 5 "xstormy16_below100_operand" "")
	(match_operand:QI 6 "register_operand" ""))
   ]
  "REGNO (operands[0]) == REGNO (operands[2])
   && REGNO (operands[0]) == REGNO (operands[3])
   && REGNO (operands[0]) == REGNO (operands[6])
   && rtx_equal_p (operands[1], operands[5])"
  [(set (match_dup 1)
	(and:QI (match_dup 1)
		(match_dup 4)))
   ]
  "")

;; GCC uses different techniques to optimize MSB and LSB accesses, so
;; we have to code those separately.

(define_insn "*bclrx"
  [(set (pc)
	(if_then_else (eq:HI (and:QI (match_operand:QI 1 "xstormy16_below100_operand" "W")
				     (match_operand:HI 2 "immediate_operand" "i"))
			     (const_int 0))
		      (label_ref (match_operand 0 "" ""))
		      (pc)))
   (clobber (reg:BI CARRY_REG))]
  ""
  "bn %1,%B2,%l0"
  [(set_attr "length" "4")
   (set_attr "psw_operand" "nop")])

(define_insn "*bclrx2"
  [(set (pc)
	(if_then_else (zero_extract:HI
		       (xor:HI (subreg:HI
				(match_operand:QI 1 "xstormy16_below100_operand" "W") 0)
			       (match_operand:HI 2 "xstormy16_onebit_set_operand" "J"))
		       (const_int 1)
		       (match_operand:HI 3 "immediate_operand" "i"))
		      (label_ref (match_operand 0 "" ""))
		      (pc)))
   (clobber (reg:BI CARRY_REG))]
  ""
  "bn %1,%B2,%l0"
  [(set_attr "length" "4")
   (set_attr "psw_operand" "nop")])

(define_insn "*bclrx3"
  [(set (pc)
	(if_then_else (eq:HI (and:HI (zero_extend:HI (match_operand:QI 1 "xstormy16_below100_operand" "W"))
				     (match_operand:HI 2 "immediate_operand" "i"))
			     (const_int 0))
		      (label_ref (match_operand 0 "" ""))
		      (pc)))
   (clobber (reg:BI CARRY_REG))]
  ""
  "bn %1,%B2,%l0"
  [(set_attr "length" "4")
   (set_attr "psw_operand" "nop")])

(define_insn "*bclr7"
  [(set (pc)
	(if_then_else (xor:HI (lshiftrt:HI (subreg:HI
					    (match_operand:QI 1 "xstormy16_below100_operand" "W") 0)
					   (const_int 7))
			      (const_int 1))
		      (label_ref (match_operand 0 "" ""))
		      (pc)))
   (clobber (reg:BI CARRY_REG))]
  ""
  "bn %1,#7,%l0"
  [(set_attr "length" "4")
   (set_attr "psw_operand" "nop")])

(define_insn "*bclr15"
  [(set (pc)
	(if_then_else (ge:HI (sign_extend:HI (match_operand:QI 1 "xstormy16_below100_operand" "W"))
			     (const_int 0))
		      (label_ref (match_operand 0 "" ""))
		      (pc)))
   (clobber (reg:BI CARRY_REG))]
  ""
  "bn %1,#7,%l0"
  [(set_attr "length" "4")
   (set_attr "psw_operand" "nop")])

(define_insn "*bsetx"
  [(set (pc)
	(if_then_else (ne:HI (and:QI (match_operand:QI 1 "xstormy16_below100_operand" "W")
				     (match_operand:HI 2 "immediate_operand" "i"))
			     (const_int 0))
		      (label_ref (match_operand 0 "" ""))
		      (pc)))
   (clobber (reg:BI CARRY_REG))]
  ""
  "bp %1,%B2,%l0"
  [(set_attr "length" "4")
   (set_attr "psw_operand" "nop")])

(define_insn "*bsetx2"
  [(set (pc)
	(if_then_else (zero_extract:HI (match_operand:QI 1 "xstormy16_below100_operand" "W")
				       (const_int 1)
				       (match_operand:HI 2 "immediate_operand" "i"))
		      (label_ref (match_operand 0 "" ""))
		      (pc)))
   (clobber (reg:BI CARRY_REG))]
  ""
  "bp %1,%b2,%l0"
  [(set_attr "length" "4")
   (set_attr "psw_operand" "nop")])

(define_insn "*bsetx3"
  [(set (pc)
	(if_then_else (ne:HI (and:HI (zero_extend:HI (match_operand:QI 1 "xstormy16_below100_operand" "W"))
				     (match_operand:HI 2 "immediate_operand" "i"))
			     (const_int 0))
		      (label_ref (match_operand 0 "" ""))
		      (pc)))
   (clobber (reg:BI CARRY_REG))]
  ""
  "bp %1,%B2,%l0"
  [(set_attr "length" "4")
   (set_attr "psw_operand" "nop")])

(define_insn "*bset7"
  [(set (pc)
	(if_then_else (lshiftrt:HI (subreg:HI (match_operand:QI 1 "xstormy16_below100_operand" "W") 0)
				   (const_int 7))
		      (label_ref (match_operand 0 "" ""))
		      (pc)))
   (clobber (reg:BI CARRY_REG))]
  ""
  "bp %1,#7,%l0"
  [(set_attr "length" "4")
   (set_attr "psw_operand" "nop")])

(define_insn "*bset15"
  [(set (pc)
	(if_then_else (lt:HI (sign_extend:HI (match_operand:QI 1 "xstormy16_below100_operand" "W"))
			     (const_int 0))
		      (label_ref (match_operand 0 "" ""))
		      (pc)))
   (clobber (reg:BI CARRY_REG))]
  ""
  "bp %1,#7,%l0"
  [(set_attr "length" "4")
   (set_attr "psw_operand" "nop")])

(define_insn "bswaphi2"
  [(set (match_operand:HI 0 "register_operand" "=r")
	(bswap:HI (match_operand:HI 1 "register_operand" "0")))]
  ""
  "swpb %0")

(define_insn "bswapsi2"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(bswap:SI (match_operand:SI 1 "register_operand" "0")))]
  ""
  "swpb %0 | swpb %h0 | swpw %0,%h0"
  [(set_attr "length" "6")])

(define_insn "swaphi"
  [(set (match_operand:HI 0 "register_operand" "+r")
	(match_operand:HI 1 "register_operand" "+r"))
   (set (match_dup 1)
	(match_dup 0))]
  ""
  "swpw %0,%1")

(define_peephole2
  [(set (match_operand:HI 0 "register_operand")
	(match_operand:HI 1 "register_operand"))
   (set (match_dup 1)
	(match_operand:HI 2 "register_operand"))
   (set (match_dup 2)
	(match_dup 0))]
  "REGNO (operands[0]) != REGNO (operands[1])
   && REGNO (operands[0]) != REGNO (operands[2])
   && REGNO (operands[1]) != REGNO (operands[2])
   && peep2_reg_dead_p (3, operands[0])"
  [(parallel [(set (match_dup 2) (match_dup 1))
              (set (match_dup 1) (match_dup 2))])])

;; Recognize shl+and and shr+and as macro instructions.
(define_insn_and_split "*<code>_and_internal"
  [(set (match_operand:HI 0 "register_operand" "=r")
        (and:HI (any_lshift:HI (match_operand 1 "register_operand" "0")
			       (match_operand 2 "const_int_operand" "i"))
		(match_operand 3 "const_int_operand" "i")))
   (clobber (reg:BI CARRY_REG))]
  "IN_RANGE (INTVAL (operands[2]), 0, 15)"
  "#"
  "reload_completed"
  [(parallel [(set (match_dup 0) (any_lshift:HI (match_dup 1) (match_dup 2)))
	      (clobber (reg:BI CARRY_REG))])
   (set (match_dup 0) (and:HI (match_dup 0) (match_dup 3)))])

;; Swap nibbles instruction
(define_insn "*swpn"
  [(set (match_operand:HI 0 "register_operand" "=r")
	(any_or_plus:HI
	  (any_or_plus:HI
	    (and:HI (ashift:HI (match_operand:HI 1 "register_operand" "0")
			       (const_int 4))
		    (const_int 240))
	    (and:HI (lshiftrt:HI (match_dup 1) (const_int 4))
		    (const_int 15)))
	  (and:HI (match_dup 1) (const_int -256))))]
  ""
  "swpn %0")

(define_insn "*swpn_zext"
  [(set (match_operand:HI 0 "register_operand" "=r")
	(any_or_plus:HI
	  (and:HI (ashift:HI (match_operand:HI 1 "register_operand" "0")
			     (const_int 4))
		  (const_int 240))
	  (and:HI (lshiftrt:HI (match_dup 1) (const_int 4))
		  (const_int 15))))]
  ""
  "swpn %0 | and %0,#255"
  [(set_attr "length" "6")])

;; Alternate form when we use PLUS instead of IOR early in the
;; expanders.
(define_insn "*swpn_zext"
  [(set (match_operand:HI 0 "register_operand" "=r")
	(zero_extend:HI
	  (subreg:QI
	    (any_or_plus:HI
	      (ashift:HI (match_operand:HI 1 "register_operand" "0")
			 (const_int 4))
	      (lshiftrt:HI (match_dup 1) (const_int 4))) 0)))]
  ""
  "swpn %0 | and %0,#255"
  [(set_attr "length" "6")])

(define_insn "*swpn_sext"
  [(set (match_operand:HI 0 "register_operand" "=r")
	(sign_extend:HI
	  (rotate:QI (subreg:QI (match_operand:HI 1 "register_operand" "0") 0)
		     (const_int 4))))]
  ""
  "swpn %0 | cbw %0"
  [(set_attr "length" "4")])

(define_insn "*swpn_sext_2"
  [(set (match_operand:HI 0 "register_operand" "=r")
	(sign_extend:HI
	  (subreg:QI
	    (any_or_plus:HI
	      (ashift:HI (match_operand:HI 1 "register_operand" "0")
			 (const_int 4))
	      (subreg:HI (lshiftrt:QI (subreg:QI (match_dup 1) 0)
				      (const_int 4)) 0)) 0)))]
  ""
  "swpn %0 | cbw %0"
  [(set_attr "length" "4")])

;; Recognize swpn_zext+ior as a macro instruction.
(define_insn_and_split "*swpn_zext_ior"
  [(set (match_operand:HI 0 "register_operand")
	(any_or_plus:HI
	  (any_or_plus:HI
	    (and:HI (ashift:HI (match_operand:HI 1 "register_operand")
			       (const_int 4))
		    (const_int 240))
	    (and:HI (lshiftrt:HI (match_dup 1) (const_int 4))
		    (const_int 15)))
	  (match_operand:HI 2 "nonmemory_operand")))]
  "can_create_pseudo_p ()"
  "#"
  "&& 1"
 [(set (match_dup 3) (ior:HI (and:HI (ashift:HI (match_dup 1) (const_int 4))
				     (const_int 240))
			     (and:HI (lshiftrt:HI (match_dup 1) (const_int 4))
				     (const_int 15))))
  (set (match_dup 0) (ior:HI (match_dup 3) (match_dup 2)))]
  "operands[3] = gen_reg_rtx (HImode);")

