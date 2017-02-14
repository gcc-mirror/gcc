;; Machine description for Moxie
;; Copyright (C) 2009-2017 Free Software Foundation, Inc.
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
  inc\\t%0, %2
  dec\\t%0, -%2
  add\\t%0, %2")

(define_insn "subsi3"
  [(set (match_operand:SI 0 "register_operand" "=r,r")
	  (minus:SI
	   (match_operand:SI 1 "register_operand" "0,0")
	   (match_operand:SI 2 "moxie_sub_operand" "I,r")))]
  ""
  "@
  dec\\t%0, %2
  sub\\t%0, %2")

(define_insn "mulsi3"
  [(set (match_operand:SI 0 "register_operand" "=r")
	  (mult:SI
	   (match_operand:SI 1 "register_operand" "0")
	   (match_operand:SI 2 "register_operand" "r")))]
  ""
  "mul\\t%0, %2")

(define_code_iterator EXTEND [sign_extend zero_extend])
(define_code_attr mul [(sign_extend "mul") (zero_extend "umul")])

(define_insn "<mul>si3_highpart"
  [(set (match_operand:SI 0 "register_operand"                       "=r")
        (truncate:SI
         (lshiftrt:DI
          (mult:DI (EXTEND:DI (match_operand:SI 1 "register_operand"  "0"))
                   (EXTEND:DI (match_operand:SI 2 "register_operand"  "r")))
          (const_int 32))))]
  "TARGET_HAS_MULX"
  "<mul>.x\\t%0, %2")

(define_expand "<mul>sidi3"
  [(set (match_operand:DI 0 "register_operand" "")
	(mult:DI (EXTEND:DI (match_operand:SI 1 "register_operand" "0"))
		 (EXTEND:DI (match_operand:SI 2 "register_operand" "r"))))]
  "TARGET_HAS_MULX"
{
  rtx hi = gen_reg_rtx (SImode);
  rtx lo = gen_reg_rtx (SImode);

  emit_insn (gen_<mul>si3_highpart (hi, operands[1], operands[2]));
  emit_insn (gen_mulsi3 (lo, operands[1], operands[2]));
  emit_move_insn (gen_lowpart (SImode, operands[0]), lo);
  emit_move_insn (gen_highpart (SImode, operands[0]), hi);
  DONE;
})

(define_insn "divsi3"
  [(set (match_operand:SI 0 "register_operand" "=r")
	  (div:SI
	   (match_operand:SI 1 "register_operand" "0")
	   (match_operand:SI 2 "register_operand" "r")))]
  ""
  "div\\t%0, %2")

(define_insn "udivsi3"
  [(set (match_operand:SI 0 "register_operand" "=r")
	  (udiv:SI
	   (match_operand:SI 1 "register_operand" "0")
	   (match_operand:SI 2 "register_operand" "r")))]
  ""
  "udiv\\t%0, %2")

(define_insn "modsi3"
  [(set (match_operand:SI 0 "register_operand" "=r")
	  (mod:SI
	   (match_operand:SI 1 "register_operand" "0")
	   (match_operand:SI 2 "register_operand" "r")))]
  ""
  "mod\\t%0, %2")

(define_insn "umodsi3"
  [(set (match_operand:SI 0 "register_operand" "=r")
	  (umod:SI
	   (match_operand:SI 1 "register_operand" "0")
	   (match_operand:SI 2 "register_operand" "r")))]
  ""
  "umod\\t%0, %2")

;; -------------------------------------------------------------------------
;; Unary arithmetic instructions
;; -------------------------------------------------------------------------

(define_insn "negsi2"
  [(set (match_operand:SI 0 "register_operand" "=r")
	  (neg:SI (match_operand:SI 1 "register_operand" "r")))]
  ""
  "neg\\t%0, %1")

(define_insn "one_cmplsi2"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(not:SI (match_operand:SI 1 "register_operand" "r")))]
  ""
  "not\\t%0, %1")

;; -------------------------------------------------------------------------
;; Logical operators
;; -------------------------------------------------------------------------

(define_insn "andsi3"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(and:SI (match_operand:SI 1 "register_operand" "0")
		(match_operand:SI 2 "register_operand" "r")))]
  ""
{
  return "and\\t%0, %2";
})

(define_insn "xorsi3"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(xor:SI (match_operand:SI 1 "register_operand" "0")
		(match_operand:SI 2 "register_operand" "r")))]
  ""
{
  return "xor\\t%0, %2";
})

(define_insn "iorsi3"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(ior:SI (match_operand:SI 1 "register_operand" "0")
		(match_operand:SI 2 "register_operand" "r")))]
  ""
{
  return "or\\t%0, %2";
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
  return "ashl\\t%0, %2";
})

(define_insn "ashrsi3"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(ashiftrt:SI (match_operand:SI 1 "register_operand" "0")
		     (match_operand:SI 2 "register_operand" "r")))]
  ""
{
  return "ashr\\t%0, %2";
})

(define_insn "lshrsi3"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(lshiftrt:SI (match_operand:SI 1 "register_operand" "0")
		     (match_operand:SI 2 "register_operand" "r")))]
  ""
{
  return "lshr\\t%0, %2";
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
  "push\\t$sp, %0")

;; Pop a register from the stack
(define_insn "movsi_pop"
  [(set (match_operand:SI 1 "register_operand" "=r")
  	(mem:SI (post_inc:SI (match_operand:SI 0 "register_operand" "r"))))]
  ""
  "pop\\t%0, %1")

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
   xor\\t%0, %0
   mov\\t%0, %1
   ldi.l\\t%0, %1
   st.l\\t%0, %1
   sta.l\\t%0, %1
   ld.l\\t%0, %1
   lda.l\\t%0, %1
   sto.l\\t%0, %1
   ldo.l\\t%0, %1"
  [(set_attr "length"	"2,2,6,2,6,2,6,4,4")])

(define_insn "zero_extendqisi2"
  [(set (match_operand:SI 0 "register_operand" "=r,r,r,r")
	(zero_extend:SI (match_operand:QI 1 "nonimmediate_operand" "r,W,A,B")))]
  ""
  "@
   zex.b\\t%0, %1
   ld.b\\t%0, %1
   lda.b\\t%0, %1
   ldo.b\\t%0, %1"
  [(set_attr "length" "2,2,6,4")])

(define_insn "zero_extendhisi2"
  [(set (match_operand:SI 0 "register_operand" "=r,r,r,r")
	(zero_extend:SI (match_operand:HI 1 "nonimmediate_operand" "r,W,A,B")))]
  ""
  "@
   zex.s\\t%0, %1
   ld.s\\t%0, %1
   lda.s\\t%0, %1
   ldo.s\\t%0, %1"
  [(set_attr "length" "2,2,6,4")])

(define_insn "extendqisi2"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(sign_extend:SI (match_operand:QI 1 "nonimmediate_operand" "r")))]
  ""
  "@
   sex.b\\t%0, %1"
  [(set_attr "length" "2")])

(define_insn "extendhisi2"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(sign_extend:SI (match_operand:HI 1 "nonimmediate_operand" "r")))]
  ""
  "@
   sex.s\\t%0, %1"
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
   xor\\t%0, %0
   mov\\t%0, %1
   ldi.b\\t%0, %1
   st.b\\t%0, %1
   sta.b\\t%0, %1
   ld.b\\t%0, %1
   lda.b\\t%0, %1
   sto.b\\t%0, %1
   ldo.b\\t%0, %1"
  [(set_attr "length"	"2,2,6,2,6,2,6,4,4")])

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
   xor\\t%0, %0
   mov\\t%0, %1
   ldi.s\\t%0, %1
   st.s\\t%0, %1
   sta.s\\t%0, %1
   ld.s\\t%0, %1
   lda.s\\t%0, %1
   sto.s\\t%0, %1
   ldo.s\\t%0, %1"
  [(set_attr "length"	"2,2,6,2,6,2,6,4,4")])

;; -------------------------------------------------------------------------
;; Compare instructions
;; -------------------------------------------------------------------------

(define_constants
  [(CC_REG 19)])

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
  "cmp\\t%0, %1")


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
    return "b<CC>\\t%l0";
  else
    return "b<rCC>\\t.+6\n\tjmpa   %l0";
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
   jsra\\t%0
   jsr\\t%0"
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
  "jsra\\t%1"
  [(set_attr "length"	"6")])

(define_insn "*call_value_indirect"
  [(set (match_operand 0 "register_operand" "=r")
	(call (mem:QI (match_operand:SI
		       1 "register_operand" "r"))
	      (match_operand 2 "" "")))]
  ""
  "jsr\\t%1")

(define_insn "indirect_jump"
  [(set (pc) (match_operand:SI 0 "nonimmediate_operand" "r"))]
  ""
  "jmp\\t%0")

(define_insn "jump"
  [(set (pc)
	(label_ref (match_operand 0 "" "")))]
  ""
  "jmpa\\t%l0"
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
