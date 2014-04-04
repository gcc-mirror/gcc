;; Machine description for Moxie
;; Copyright (C) 2009-2014 Free Software Foundation, Inc.
;; Contributed by Anthony Green <green@moxielogic.com>

;; This file is part of GCC.

;; GCC is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published
;; by the Free Software Foundation; either version 3, or (at your
;; option) any later version.

;; GCC is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
;; License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GCC; see the file COPYING3.  If not see
;; <http://www.gnu.org/licenses/>.

;; -------------------------------------------------------------------------
;; Moxie specific constraints, predicates and attributes
;; -------------------------------------------------------------------------

(include "constraints.md")
(include "predicates.md")

; Most instructions are two bytes long.
(define_attr "length" "" (const_int 2))

;; -------------------------------------------------------------------------
;; nop instruction
;; -------------------------------------------------------------------------

(define_insn "nop"
  [(const_int 0)]
  ""
  "nop")

;; -------------------------------------------------------------------------
;; Arithmetic instructions
;; -------------------------------------------------------------------------

(define_insn "addsi3"
  [(set (match_operand:SI 0 "register_operand" "=r,r,r")
	  (plus:SI
	   (match_operand:SI 1 "register_operand" "0,0,0")
	   (match_operand:SI 2 "moxie_add_operand" "I,N,r")))]
  ""
  "@
  inc    %0, %2
  dec	 %0, -%2
  add.l  %0, %2")

(define_insn "subsi3"
  [(set (match_operand:SI 0 "register_operand" "=r,r")
	  (minus:SI
	   (match_operand:SI 1 "register_operand" "0,0")
	   (match_operand:SI 2 "moxie_sub_operand" "I,r")))]
  ""
  "@
  dec    %0, %2
  sub.l  %0, %2")

(define_insn "mulsi3"
  [(set (match_operand:SI 0 "register_operand" "=r")
	  (mult:SI
	   (match_operand:SI 1 "register_operand" "0")
	   (match_operand:SI 2 "register_operand" "r")))]
  ""
  "mul.l  %0, %2")

(define_insn "divsi3"
  [(set (match_operand:SI 0 "register_operand" "=r")
	  (div:SI
	   (match_operand:SI 1 "register_operand" "0")
	   (match_operand:SI 2 "register_operand" "r")))]
  ""
  "div.l  %0, %2")

(define_insn "udivsi3"
  [(set (match_operand:SI 0 "register_operand" "=r")
	  (udiv:SI
	   (match_operand:SI 1 "register_operand" "0")
	   (match_operand:SI 2 "register_operand" "r")))]
  ""
  "udiv.l %0, %2")

(define_insn "modsi3"
  [(set (match_operand:SI 0 "register_operand" "=r")
	  (mod:SI
	   (match_operand:SI 1 "register_operand" "0")
	   (match_operand:SI 2 "register_operand" "r")))]
  ""
  "mod.l  %0, %2")

(define_insn "umodsi3"
  [(set (match_operand:SI 0 "register_operand" "=r")
	  (umod:SI
	   (match_operand:SI 1 "register_operand" "0")
	   (match_operand:SI 2 "register_operand" "r")))]
  ""
  "umod.l %0, %2")

;; -------------------------------------------------------------------------
;; Unary arithmetic instructions
;; -------------------------------------------------------------------------

(define_insn "negsi2"
  [(set (match_operand:SI 0 "register_operand" "=r")
	  (neg:SI (match_operand:SI 1 "register_operand" "r")))]
  ""
  "neg    %0, %1")

(define_insn "one_cmplsi2"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(not:SI (match_operand:SI 1 "register_operand" "r")))]
  ""
  "not    %0, %1")

;; -------------------------------------------------------------------------
;; Logical operators
;; -------------------------------------------------------------------------

(define_insn "andsi3"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(and:SI (match_operand:SI 1 "register_operand" "0")
		(match_operand:SI 2 "register_operand" "r")))]
  ""
{
  return "and    %0, %2";
})

(define_insn "xorsi3"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(xor:SI (match_operand:SI 1 "register_operand" "0")
		(match_operand:SI 2 "register_operand" "r")))]
  ""
{
  return "xor    %0, %2";
})

(define_insn "iorsi3"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(ior:SI (match_operand:SI 1 "register_operand" "0")
		(match_operand:SI 2 "register_operand" "r")))]
  ""
{
  return "or     %0, %2";
})

;; -------------------------------------------------------------------------
;; Shifters
;; -------------------------------------------------------------------------

(define_insn "ashlsi3"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(ashift:SI (match_operand:SI 1 "register_operand" "0")
		   (match_operand:SI 2 "register_operand" "r")))]
  ""
{
  return "ashl   %0, %2";
})

(define_insn "ashrsi3"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(ashiftrt:SI (match_operand:SI 1 "register_operand" "0")
		     (match_operand:SI 2 "register_operand" "r")))]
  ""
{
  return "ashr   %0, %2";
})

(define_insn "lshrsi3"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(lshiftrt:SI (match_operand:SI 1 "register_operand" "0")
		     (match_operand:SI 2 "register_operand" "r")))]
  ""
{
  return "lshr   %0, %2";
})

;; -------------------------------------------------------------------------
;; Move instructions
;; -------------------------------------------------------------------------

;; SImode

;; Push a register onto the stack
(define_insn "movsi_push"
  [(set (mem:SI (pre_dec:SI (reg:SI 1)))
  	(match_operand:SI 0 "register_operand" "r"))]
  ""
  "push   $sp, %0")

;; Pop a register from the stack
(define_insn "movsi_pop"
  [(set (match_operand:SI 1 "register_operand" "=r")
  	(mem:SI (post_inc:SI (match_operand:SI 0 "register_operand" "r"))))]
  ""
  "pop    %0, %1")

(define_expand "movsi"
   [(set (match_operand:SI 0 "general_operand" "")
 	(match_operand:SI 1 "general_operand" ""))]
   ""
  "
{
  /* If this is a store, force the value into a register.  */
  if (! (reload_in_progress || reload_completed))
  {
    if (MEM_P (operands[0]))
    {
      operands[1] = force_reg (SImode, operands[1]);
      if (MEM_P (XEXP (operands[0], 0)))
        operands[0] = gen_rtx_MEM (SImode, force_reg (SImode, XEXP (operands[0], 0)));
    }
    else 
      if (MEM_P (operands[1])
          && MEM_P (XEXP (operands[1], 0)))
        operands[1] = gen_rtx_MEM (SImode, force_reg (SImode, XEXP (operands[1], 0)));
  }
}")

(define_insn "*movsi"
  [(set (match_operand:SI 0 "nonimmediate_operand" "=r,r,r,W,A,r,r,B,r")
	(match_operand:SI 1 "moxie_general_movsrc_operand" "O,r,i,r,r,W,A,r,B"))]
  "register_operand (operands[0], SImode)
   || register_operand (operands[1], SImode)"
  "@
   xor    %0, %0
   mov    %0, %1
   ldi.l  %0, %1
   st.l   %0, %1
   sta.l  %0, %1
   ld.l   %0, %1
   lda.l  %0, %1
   sto.l  %0, %1
   ldo.l  %0, %1"
  [(set_attr "length"	"2,2,6,2,6,2,6,6,6")])

(define_insn_and_split "zero_extendqisi2"
  [(set (match_operand:SI 0 "register_operand" "=r,r,r,r")
	(zero_extend:SI (match_operand:QI 1 "nonimmediate_operand" "0,W,A,B")))]
  ""
  "@
   ;
   ld.b   %0, %1
   lda.b  %0, %1
   ldo.b  %0, %1"
  "reload_completed"
  [(set (match_dup 2) (match_dup 1))
   (set (match_dup 0) (zero_extend:SI (match_dup 2)))]
{
  operands[2] = gen_lowpart (QImode, operands[0]);
}
  [(set_attr "length" "0,2,6,6")])

(define_insn_and_split "zero_extendhisi2"
  [(set (match_operand:SI 0 "register_operand" "=r,r,r,r")
	(zero_extend:SI (match_operand:HI 1 "nonimmediate_operand" "0,W,A,B")))]
  ""
  "@
   ;
   ld.s   %0, %1
   lda.s  %0, %1
   ldo.s  %0, %1"
  "reload_completed"
  [(set (match_dup 2) (match_dup 1))
   (set (match_dup 0) (zero_extend:SI (match_dup 2)))]
{
  operands[2] = gen_lowpart (HImode, operands[0]);
}
  [(set_attr "length" "0,2,6,6")])

(define_insn "extendqisi2"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(sign_extend:SI (match_operand:QI 1 "nonimmediate_operand" "r")))]
  ""
  "@
   sex.b  %0, %1"
  [(set_attr "length" "2")])

(define_insn "extendhisi2"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(sign_extend:SI (match_operand:HI 1 "nonimmediate_operand" "r")))]
  ""
  "@
   sex.s  %0, %1"
  [(set_attr "length" "2")])

(define_expand "movqi"
  [(set (match_operand:QI 0 "general_operand" "")
	(match_operand:QI 1 "general_operand" ""))]
  ""
  "
{
  /* If this is a store, force the value into a register.  */
  if (MEM_P (operands[0]))
    operands[1] = force_reg (QImode, operands[1]);
}")

(define_insn "*movqi"
  [(set (match_operand:QI 0 "nonimmediate_operand" "=r,r,r,W,A,r,r,B,r")
	(match_operand:QI 1 "moxie_general_movsrc_operand" "O,r,i,r,r,W,A,r,B"))]
  "register_operand (operands[0], QImode)
   || register_operand (operands[1], QImode)"
  "@
   xor    %0, %0
   mov    %0, %1
   ldi.b  %0, %1
   st.b   %0, %1
   sta.b  %0, %1
   ld.b   %0, %1
   lda.b  %0, %1
   sto.b  %0, %1
   ldo.b  %0, %1"
  [(set_attr "length"	"2,2,6,2,6,2,6,6,6")])

(define_expand "movhi"
  [(set (match_operand:HI 0 "general_operand" "")
	(match_operand:HI 1 "general_operand" ""))]
  ""
  "
{
  /* If this is a store, force the value into a register.  */
  if (MEM_P (operands[0]))
    operands[1] = force_reg (HImode, operands[1]);
}")

(define_insn "*movhi"
  [(set (match_operand:HI 0 "nonimmediate_operand" "=r,r,r,W,A,r,r,B,r")
	(match_operand:HI 1 "moxie_general_movsrc_operand" "O,r,i,r,r,W,A,r,B"))]
  "(register_operand (operands[0], HImode)
    || register_operand (operands[1], HImode))"
  "@
   xor    %0, %0
   mov    %0, %1
   ldi.s  %0, %1
   st.s   %0, %1
   sta.s  %0, %1
   ld.s   %0, %1
   lda.s  %0, %1
   sto.s  %0, %1
   ldo.s  %0, %1"
  [(set_attr "length"	"2,2,6,2,6,2,6,6,6")])

;; -------------------------------------------------------------------------
;; Compare instructions
;; -------------------------------------------------------------------------

(define_constants
  [(CC_REG 11)])

(define_expand "cbranchsi4"
  [(set (reg:CC CC_REG)
        (compare:CC
         (match_operand:SI 1 "general_operand" "")
         (match_operand:SI 2 "general_operand" "")))
   (set (pc)
        (if_then_else (match_operator 0 "comparison_operator"
                       [(reg:CC CC_REG) (const_int 0)])
                      (label_ref (match_operand 3 "" ""))
                      (pc)))]
  ""
  "
  /* Force the compare operands into registers.  */
  if (GET_CODE (operands[1]) != REG)
	operands[1] = force_reg (SImode, operands[1]);
  if (GET_CODE (operands[2]) != REG)
	operands[2] = force_reg (SImode, operands[2]);
  ")

(define_insn "*cmpsi"
  [(set (reg:CC CC_REG)
	(compare
	 (match_operand:SI 0 "register_operand" "r")
	 (match_operand:SI 1 "register_operand"	"r")))]
  ""
  "cmp    %0, %1")


;; -------------------------------------------------------------------------
;; Branch instructions
;; -------------------------------------------------------------------------

(define_code_iterator cond [ne eq lt ltu gt gtu ge le geu leu])
(define_code_attr CC [(ne "ne") (eq "eq") (lt "lt") (ltu "ltu") 
		      (gt "gt") (gtu "gtu") (ge "ge") (le "le") 
		      (geu "geu") (leu "leu") ])
(define_code_attr rCC [(ne "eq") (eq "ne") (lt "ge") (ltu "geu") 
		       (gt "le") (gtu "leu") (ge "lt") (le "gt") 
		       (geu "ltu") (leu "gtu") ])

(define_insn "*b<cond:code>"
  [(set (pc)
	(if_then_else (cond (reg:CC CC_REG)
			    (const_int 0))
		      (label_ref (match_operand 0 "" ""))
		      (pc)))]
  ""
{
  if (get_attr_length (insn) == 2)
    return "b<CC>   %l0";
  else
    return "b<rCC>   .+6\n\tjmpa   %l0";
}
  [(set (attr "length")
        (if_then_else (lt (abs (minus (pc) (match_dup 0))) (const_int 1022))
                      (const_int 2) (const_int 8)))])

;; -------------------------------------------------------------------------
;; Call and Jump instructions
;; -------------------------------------------------------------------------

(define_expand "call"
  [(call (match_operand:QI 0 "memory_operand" "")
		(match_operand 1 "general_operand" ""))]
  ""
{
  gcc_assert (MEM_P (operands[0]));
})

(define_insn "*call"
  [(call (mem:QI (match_operand:SI
		  0 "nonmemory_operand" "i,r"))
	 (match_operand 1 "" ""))]
  ""
  "@
   jsra   %0
   jsr    %0"
  [(set_attr "length"	"6,2")])

(define_expand "call_value"
  [(set (match_operand 0 "" "")
		(call (match_operand:QI 1 "memory_operand" "")
		 (match_operand 2 "" "")))]
  ""
{
  gcc_assert (MEM_P (operands[1]));
})

(define_insn "*call_value"
  [(set (match_operand 0 "register_operand" "=r")
	(call (mem:QI (match_operand:SI
		       1 "immediate_operand" "i"))
	      (match_operand 2 "" "")))]
  ""
  "jsra   %1"
  [(set_attr "length"	"6")])

(define_insn "*call_value_indirect"
  [(set (match_operand 0 "register_operand" "=r")
	(call (mem:QI (match_operand:SI
		       1 "register_operand" "r"))
	      (match_operand 2 "" "")))]
  ""
  "jsr    %1")

(define_insn "indirect_jump"
  [(set (pc) (match_operand:SI 0 "nonimmediate_operand" "r"))]
  ""
  "jmp    %0")

(define_insn "jump"
  [(set (pc)
	(label_ref (match_operand 0 "" "")))]
  ""
  "jmpa   %l0"
  [(set_attr "length"	"6")])


;; -------------------------------------------------------------------------
;; Prologue & Epilogue
;; -------------------------------------------------------------------------

(define_expand "prologue"
  [(clobber (const_int 0))]
  ""
  "
{
  moxie_expand_prologue ();
  DONE;
}
")

(define_expand "epilogue"
  [(return)]
  ""
  "
{
  moxie_expand_epilogue ();
  DONE;
}
")

(define_insn "returner"
  [(return)]
  "reload_completed"
  "ret")
