;; Machine description for DEC Alpha for GNU C compiler
;; Copyright (C) 1992, 93, 94, 95, 96, 1997 Free Software Foundation, Inc.
;; Contributed by Richard Kenner (kenner@vlsi1.ultra.nyu.edu)

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

;;- See file "rtl.def" for documentation on define_insn, match_*, et. al.

;; Processor type -- this attribute must exactly match the processor_type
;; enumeration in alpha.h.

(define_attr "cpu" "ev4,ev5"
  (const (symbol_ref "alpha_cpu")))

;; Define an insn type attribute.  This is used in function unit delay
;; computations, among other purposes.  For the most part, we use the names
;; defined in the EV4 documentation, but add a few that we have to know about
;; separately.

(define_attr "type"
  "ld,st,ibr,fbr,jsr,iadd,ilog,shift,cmov,icmp,imull,imulq,imulh,fadd,fmul,fcpys,fdivs,fdivt,ldsym,misc"
  (const_string "iadd"))

;; The TRAP_TYPE attribute marks instructions that may generate traps
;; (which are imprecise and may need a trapb if software complention
;; is desired).
(define_attr "trap" "no,yes" (const_string "no"))

;; For the EV4 we include four function units: ABOX, which computes
;; the address, BBOX, used for branches, EBOX, used for integer
;; operations, and FBOX, used for FP operations.

;; Memory delivers its result in three cycles.
(define_function_unit "ev4_abox" 1 0
  (and (eq_attr "cpu" "ev4")
       (eq_attr "type" "ld,ldsym,st"))
  3 1)

;; Branches have no delay cost, but do tie up the unit for two cycles.
(define_function_unit "ev4_bbox" 1 1
  (and (eq_attr "cpu" "ev4")
       (eq_attr "type" "ibr,fbr,jsr"))
  2 2)

;; Arithmetic insns are normally have their results available after
;; two cycles.  There are a number of exceptions.  They are encoded in
;; ADJUST_COST.  Some of the other insns have similar exceptions.

(define_function_unit "ev4_ebox" 1 0
  (and (eq_attr "cpu" "ev4")
       (eq_attr "type" "iadd,ilog,shift,cmov,icmp"))
  2 1)

;; These really don't take up the integer pipeline, but they do occupy
;; IBOX1; we approximate here.

(define_function_unit "ev4_ebox" 1 0
  (and (eq_attr "cpu" "ev4")
       (eq_attr "type" "imull"))
  21 1)

(define_function_unit "ev4_ebox" 1 0
  (and (eq_attr "cpu" "ev4")
       (eq_attr "type" "imulq,imulh"))
  23 1)

(define_function_unit "ev4_imult" 1 0
  (and (eq_attr "cpu" "ev4")
       (eq_attr "type" "imull"))
  21 19)

(define_function_unit "ev4_imult" 1 0
  (and (eq_attr "cpu" "ev4")
       (eq_attr "type" "imulq,imulh"))
  23 21)

(define_function_unit "ev4_fbox" 1 0
  (and (eq_attr "cpu" "ev4")
       (eq_attr "type" "fadd,fmul,fcpys"))
  6 1)

(define_function_unit "ev4_fbox" 1 0
  (and (eq_attr "cpu" "ev4")
       (eq_attr "type" "fdivs"))
  34 0)

(define_function_unit "ev4_fbox" 1 0
  (and (eq_attr "cpu" "ev4")
       (eq_attr "type" "fdivt"))
  63 0)

(define_function_unit "ev4_divider" 1 0
  (and (eq_attr "cpu" "ev4")
       (eq_attr "type" "fdivs"))
  34 30)

(define_function_unit "ev4_divider" 1 0
  (and (eq_attr "cpu" "ev4")
       (eq_attr "type" "fdivt"))
  64 59)

;; EV5 scheduling.  EV5 can issue 4 insns per clock.

;; EV5 has two asymetric integer units.  Model this with ebox,e0,e1.
;; Everything uses ebox, and those that require particular pipes grab
;; those as well.

(define_function_unit "ev5_ebox" 2 0
  (and (eq_attr "cpu" "ev5")
       (eq_attr "type" "iadd,ilog,icmp,st,shift,imull,imulq,imulh"))
  1 1)

;; Memory takes at least 2 clocks, and load cannot dual issue with stores.
(define_function_unit "ev5_ebox" 2 0
  (and (eq_attr "cpu" "ev5")
       (eq_attr "type" "ld,ldsym"))
  2 1)

(define_function_unit "ev5_e0" 1 0
  (and (eq_attr "cpu" "ev5")
       (eq_attr "type" "ld,ldsym"))
  0 1
  [(eq_attr "type" "st")])

;; Conditional moves always take 2 ticks.
(define_function_unit "ev5_ebox" 2 0
  (and (eq_attr "cpu" "ev5")
       (eq_attr "type" "cmov"))
  2 1)

;; Stores, shifts, and multiplies can only issue to E0
(define_function_unit "ev5_e0" 1 0
  (and (eq_attr "cpu" "ev5")
       (eq_attr "type" "st"))
  1 1)

;; But shifts and multiplies don't conflict with loads.
(define_function_unit "ev5_e0" 1 0
  (and (eq_attr "cpu" "ev5")
       (eq_attr "type" "shift,imull,imulq,imulh"))
  1 1
  [(eq_attr "type" "st,shift,imull,imulq,imulh")])

;; Branches can only issue to E1
(define_function_unit "ev5_e1" 1 0
  (and (eq_attr "cpu" "ev5")
       (eq_attr "type" "ibr,jsr"))
  1 1)

;; Multiplies also use the integer multiplier.
(define_function_unit "ev5_imult" 1 0
  (and (eq_attr "cpu" "ev5")
       (eq_attr "type" "imull"))
  8 4)

(define_function_unit "ev5_imult" 1 0
  (and (eq_attr "cpu" "ev5")
       (eq_attr "type" "imulq"))
  12 8)

(define_function_unit "ev5_imult" 1 0
  (and (eq_attr "cpu" "ev5")
       (eq_attr "type" "imulh"))
  14 8)

;; Similarly for the FPU we have two asymetric units.  But fcpys can issue
;; on either so we have to play the game again.

(define_function_unit "ev5_fpu" 2 0
  (and (eq_attr "cpu" "ev5")
       (eq_attr "type" "fadd,fmul,fcpys,fbr,fdivs,fdivt"))
  4 1)
  
;; Multiplies (resp. adds) also use the fmul (resp. fadd) units.
(define_function_unit "ev5_fm" 1 0
  (and (eq_attr "cpu" "ev5")
       (eq_attr "type" "fmul"))
  4 1)

(define_function_unit "ev5_fa" 1 0
  (and (eq_attr "cpu" "ev5")
       (eq_attr "type" "fadd"))
  4 1)

(define_function_unit "ev5_fa" 1 0
  (and (eq_attr "cpu" "ev5")
       (eq_attr "type" "fbr"))
  1 1)

(define_function_unit "ev5_fa" 1 0
  (and (eq_attr "cpu" "ev5")
       (eq_attr "type" "fdivs"))
  15 1)

(define_function_unit "ev5_fa" 1 0
  (and (eq_attr "cpu" "ev5")
       (eq_attr "type" "fdivt"))
  22 1)

;; First define the arithmetic insns.  Note that the 32-bit forms also
;; sign-extend.

;; Note that we can do sign extensions in both FP and integer registers.
;; However, the result must be in the same type of register as the input.
;; The register preferencing code can't handle this case very well, so, for
;; now, don't let the FP case show up here for preferencing.  Also,
;; sign-extends in FP registers take two instructions.
(define_insn "extendsidi2"
  [(set (match_operand:DI 0 "register_operand" "=r,r,*f")
	(sign_extend:DI (match_operand:SI 1 "nonimmediate_operand" "r,m,*f")))]
  ""
  "@
   addl %1,$31,%0
   ldl %0,%1
   cvtql %1,%0\;cvtlq %0,%0"
  [(set_attr "type" "iadd,ld,fadd")])

;; Do addsi3 the way expand_binop would do if we didn't have one.  This
;; generates better code.  We have the anonymous addsi3 pattern below in
;; case combine wants to make it.
(define_expand "addsi3"
  [(set (match_operand:SI 0 "register_operand" "")
	(plus:SI (match_operand:SI 1 "reg_or_0_operand" "")
		 (match_operand:SI 2 "add_operand" "")))]
  ""
  "
{ emit_insn (gen_rtx (SET, VOIDmode, gen_lowpart (DImode, operands[0]),
		      gen_rtx (PLUS, DImode,
			       gen_lowpart (DImode, operands[1]),
			       gen_lowpart (DImode, operands[2]))));
  DONE;
} ")

(define_insn ""
  [(set (match_operand:SI 0 "register_operand" "=r,r,r,r")
	(plus:SI (match_operand:SI 1 "reg_or_0_operand" "%rJ,rJ,rJ,rJ")
		 (match_operand:SI 2 "add_operand" "rI,O,K,L")))]
  ""
  "@
   addl %r1,%2,%0
   subl %r1,%n2,%0
   lda %0,%2(%r1)
   ldah %0,%h2(%r1)")

(define_split
  [(set (match_operand:SI 0 "register_operand" "")
	(plus:SI (match_operand:SI 1 "register_operand" "")
		 (match_operand:SI 2 "const_int_operand" "")))]
  "! add_operand (operands[2], SImode)"
  [(set (match_dup 0) (plus:SI (match_dup 1) (match_dup 3)))
   (set (match_dup 0) (plus:SI (match_dup 0) (match_dup 4)))]
  "
{
  HOST_WIDE_INT val = INTVAL (operands[2]);
  HOST_WIDE_INT low = (val & 0xffff) - 2 * (val & 0x8000);
  HOST_WIDE_INT rest = val - low;

  operands[3] = GEN_INT (rest);
  operands[4] = GEN_INT (low);
}")

(define_insn ""
  [(set (match_operand:DI 0 "register_operand" "=r,r")
	(sign_extend:DI
	 (plus:SI (match_operand:SI 1 "reg_or_0_operand" "%rJ,rJ")
		  (match_operand:SI 2 "sext_add_operand" "rI,O"))))]
  ""
  "@
   addl %r1,%2,%0
   subl %r1,%n2,%0")

(define_split
  [(set (match_operand:DI 0 "register_operand" "")
	(sign_extend:DI
	 (plus:SI (match_operand:SI 1 "register_operand" "")
		  (match_operand:SI 2 "const_int_operand" ""))))
   (clobber (match_operand:SI 3 "register_operand" ""))]
  "! sext_add_operand (operands[2], SImode) && INTVAL (operands[2]) > 0
   && INTVAL (operands[2]) % 4 == 0"
  [(set (match_dup 3) (match_dup 4))
   (set (match_dup 0) (sign_extend:DI (plus:SI (mult:SI (match_dup 3)
							(match_dup 5))
					       (match_dup 1))))]
  "
{
  HOST_WIDE_INT val = INTVAL (operands[2]) / 4;
  int mult = 4;

  if (val % 2 == 0)
    val /= 2, mult = 8;

  operands[4] = GEN_INT (val);
  operands[5] = GEN_INT (mult);
}")

(define_split
  [(set (match_operand:DI 0 "register_operand" "")
	(sign_extend:DI
	 (plus:SI (match_operator:SI 1 "comparison_operator"
				     [(match_operand 2 "" "")
				      (match_operand 3 "" "")])
		  (match_operand:SI 4 "add_operand" ""))))
   (clobber (match_operand:DI 5 "register_operand" ""))]
  ""
  [(set (match_dup 5) (match_dup 6))
   (set (match_dup 0) (sign_extend:DI (plus:SI (match_dup 7) (match_dup 4))))]
  "
{
  operands[6] = gen_rtx (GET_CODE (operands[1]), DImode,
			 operands[2], operands[3]);
  operands[7] = gen_lowpart (SImode, operands[5]);
}")

(define_insn "adddi3"
  [(set (match_operand:DI 0 "register_operand" "=r,r,r,r")
	(plus:DI (match_operand:DI 1 "reg_or_0_operand" "%rJ,rJ,rJ,rJ")
		 (match_operand:DI 2 "add_operand" "rI,O,K,L")))]
  ""
  "@
   addq %r1,%2,%0
   subq %r1,%n2,%0
   lda %0,%2(%r1)
   ldah %0,%h2(%r1)")

;; Don't do this if we are adjusting SP since we don't want to do
;; it in two steps. 
(define_split
  [(set (match_operand:DI 0 "register_operand" "")
	(plus:DI (match_operand:DI 1 "register_operand" "")
		 (match_operand:DI 2 "const_int_operand" "")))]
  "! add_operand (operands[2], DImode)
   && REGNO (operands[0]) != STACK_POINTER_REGNUM"
  [(set (match_dup 0) (plus:DI (match_dup 1) (match_dup 3)))
   (set (match_dup 0) (plus:DI (match_dup 0) (match_dup 4)))]
  "
{
  HOST_WIDE_INT val = INTVAL (operands[2]);
  HOST_WIDE_INT low = (val & 0xffff) - 2 * (val & 0x8000);
  HOST_WIDE_INT rest = val - low;

  operands[3] = GEN_INT (rest);
  operands[4] = GEN_INT (low);
}")

(define_insn ""
  [(set (match_operand:SI 0 "register_operand" "=r,r")
	(plus:SI (mult:SI (match_operand:SI 1 "reg_or_0_operand" "rJ,rJ")
			  (match_operand:SI 2 "const48_operand" "I,I"))
		 (match_operand:SI 3 "sext_add_operand" "rI,O")))]
  ""
  "@
   s%2addl %r1,%3,%0
   s%2subl %r1,%n3,%0")

(define_insn ""
  [(set (match_operand:DI 0 "register_operand" "=r,r")
	(sign_extend:DI
	 (plus:SI (mult:SI (match_operand:SI 1 "reg_or_0_operand" "rJ,rJ")
			   (match_operand:SI 2 "const48_operand" "I,I"))
		  (match_operand:SI 3 "sext_add_operand" "rI,O"))))]
  ""
  "@
   s%2addl %r1,%3,%0
   s%2subl %r1,%n3,%0")

(define_split
  [(set (match_operand:DI 0 "register_operand" "")
	(sign_extend:DI
	 (plus:SI (mult:SI (match_operator:SI 1 "comparison_operator"
					      [(match_operand 2 "" "")
					       (match_operand 3 "" "")])
			   (match_operand:SI 4 "const48_operand" ""))
		  (match_operand:SI 5 "add_operand" ""))))
   (clobber (match_operand:DI 6 "register_operand" ""))]
  ""
  [(set (match_dup 6) (match_dup 7))
   (set (match_dup 0)
	(sign_extend:DI (plus:SI (mult:SI (match_dup 8) (match_dup 4))
				 (match_dup 5))))]
  "
{
  operands[7] = gen_rtx (GET_CODE (operands[1]), DImode,
			 operands[2], operands[3]);
  operands[8] = gen_lowpart (SImode, operands[6]);
}")

(define_insn ""
  [(set (match_operand:DI 0 "register_operand" "=r,r")
	(plus:DI (mult:DI (match_operand:DI 1 "reg_or_0_operand" "rJ,rJ")
			  (match_operand:DI 2 "const48_operand" "I,I"))
		 (match_operand:DI 3 "reg_or_8bit_operand" "rI,O")))]
  ""
  "@
   s%2addq %r1,%3,%0
   s%2subq %1,%n3,%0")

;; These variants of the above insns can occur if the third operand
;; is the frame pointer.  This is a kludge, but there doesn't
;; seem to be a way around it.  Only recognize them while reloading.

(define_insn ""
  [(set (match_operand:DI 0 "some_operand" "=&r")
	(plus:DI (plus:DI (match_operand:DI 1 "some_operand" "r")
			  (match_operand:DI 2 "some_operand" "r"))
		 (match_operand:DI 3 "some_operand" "rIOKL")))]
  "reload_in_progress"
  "#")

(define_split
  [(set (match_operand:DI 0 "register_operand" "")
	(plus:DI (plus:DI (match_operand:DI 1 "register_operand" "")
			  (match_operand:DI 2 "register_operand" ""))
		 (match_operand:DI 3 "add_operand" "")))]
  "reload_completed"
  [(set (match_dup 0) (plus:DI (match_dup 1) (match_dup 2)))
   (set (match_dup 0) (plus:DI (match_dup 0) (match_dup 3)))]
  "")
					   
(define_insn ""
  [(set (match_operand:SI 0 "some_operand" "=&r")
	(plus:SI (plus:SI (mult:SI (match_operand:SI 1 "some_operand" "rJ")
				   (match_operand:SI 2 "const48_operand" "I"))
			  (match_operand:SI 3 "some_operand" "r"))
		 (match_operand:SI 4 "some_operand" "rIOKL")))]
  "reload_in_progress"
  "#")

(define_split
  [(set (match_operand:SI 0 "register_operand" "r")
	(plus:SI (plus:SI (mult:SI (match_operand:SI 1 "reg_or_0_operand" "")
				   (match_operand:SI 2 "const48_operand" ""))
			  (match_operand:SI 3 "register_operand" ""))
		 (match_operand:SI 4 "add_operand" "rIOKL")))]
  "reload_completed"
  [(set (match_dup 0)
	(plus:SI (mult:SI (match_dup 1) (match_dup 2)) (match_dup 3)))
   (set (match_dup 0) (plus:SI (match_dup 0) (match_dup 4)))]
  "")

(define_insn ""
  [(set (match_operand:DI 0 "some_operand" "=&r")
	(sign_extend:DI
	 (plus:SI (plus:SI
		   (mult:SI (match_operand:SI 1 "some_operand" "rJ")
			    (match_operand:SI 2 "const48_operand" "I"))
		   (match_operand:SI 3 "some_operand" "r"))
		  (match_operand:SI 4 "some_operand" "rIOKL"))))]
  "reload_in_progress"
  "#")

(define_split
  [(set (match_operand:DI 0 "register_operand" "")
	(sign_extend:DI
	 (plus:SI (plus:SI
		   (mult:SI (match_operand:SI 1 "reg_or_0_operand" "")
			    (match_operand:SI 2 "const48_operand" ""))
		   (match_operand:SI 3 "register_operand" ""))
		  (match_operand:SI 4 "add_operand" ""))))]
  "reload_completed"
  [(set (match_dup 5)
	(plus:SI (mult:SI (match_dup 1) (match_dup 2)) (match_dup 3)))
   (set (match_dup 0) (sign_extend:DI (plus:SI (match_dup 5) (match_dup 4))))]
  "
{ operands[5] = gen_lowpart (SImode, operands[0]);
}")

(define_insn ""
  [(set (match_operand:DI 0 "some_operand" "=&r")
	(plus:DI (plus:DI (mult:DI (match_operand:DI 1 "some_operand" "rJ")
				   (match_operand:DI 2 "const48_operand" "I"))
			  (match_operand:DI 3 "some_operand" "r"))
		 (match_operand:DI 4 "some_operand" "rIOKL")))]
  "reload_in_progress"
  "#")

(define_split
  [(set (match_operand:DI 0 "register_operand" "=")
	(plus:DI (plus:DI (mult:DI (match_operand:DI 1 "reg_or_0_operand" "")
				   (match_operand:DI 2 "const48_operand" ""))
			  (match_operand:DI 3 "register_operand" ""))
		 (match_operand:DI 4 "add_operand" "")))]
  "reload_completed"
  [(set (match_dup 0)
	(plus:DI (mult:DI (match_dup 1) (match_dup 2)) (match_dup 3)))
   (set (match_dup 0) (plus:DI (match_dup 0) (match_dup 4)))]
  "")

(define_insn "negsi2"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(neg:SI (match_operand:SI 1 "reg_or_8bit_operand" "rI")))]
  ""
  "subl $31,%1,%0")

(define_insn ""
  [(set (match_operand:DI 0 "register_operand" "=r")
	(sign_extend:DI (neg:SI
			 (match_operand:SI 1 "reg_or_8bit_operand" "rI"))))]
  ""
  "subl $31,%1,%0")

(define_insn "negdi2"
  [(set (match_operand:DI 0 "register_operand" "=r")
	(neg:DI (match_operand:DI 1 "reg_or_8bit_operand" "rI")))]
  ""
  "subq $31,%1,%0")

(define_expand "subsi3"
  [(set (match_operand:SI 0 "register_operand" "")
	(minus:SI (match_operand:SI 1 "reg_or_0_operand" "")
		  (match_operand:SI 2 "reg_or_8bit_operand" "")))]
  ""
  "
{ emit_insn (gen_rtx (SET, VOIDmode, gen_lowpart (DImode, operands[0]),
		      gen_rtx (MINUS, DImode,
			       gen_lowpart (DImode, operands[1]),
			       gen_lowpart (DImode, operands[2]))));
  DONE;

} ")

(define_insn ""
  [(set (match_operand:SI 0 "register_operand" "=r")
	(minus:SI (match_operand:SI 1 "reg_or_0_operand" "rJ")
		  (match_operand:SI 2 "reg_or_8bit_operand" "rI")))]
  ""
  "subl %r1,%2,%0")

(define_insn ""
  [(set (match_operand:DI 0 "register_operand" "=r")
	(sign_extend:DI (minus:SI (match_operand:SI 1 "reg_or_0_operand" "rJ")
				  (match_operand:SI 2 "reg_or_8bit_operand" "rI"))))]
  ""
  "subl %r1,%2,%0")

(define_insn "subdi3"
  [(set (match_operand:DI 0 "register_operand" "=r")
	(minus:DI (match_operand:DI 1 "reg_or_0_operand" "rJ")
		  (match_operand:DI 2 "reg_or_8bit_operand" "rI")))]
  ""
  "subq %r1,%2,%0")

(define_insn ""
  [(set (match_operand:SI 0 "register_operand" "=r")
	(minus:SI (mult:SI (match_operand:SI 1 "reg_or_0_operand" "rJ")
			   (match_operand:SI 2 "const48_operand" "I"))
		  (match_operand:SI 3 "reg_or_8bit_operand" "rI")))]
  ""
  "s%2subl %r1,%3,%0")

(define_insn ""
  [(set (match_operand:DI 0 "register_operand" "=r")
	(sign_extend:DI
	 (minus:SI (mult:SI (match_operand:SI 1 "reg_or_0_operand" "rJ")
			    (match_operand:SI 2 "const48_operand" "I"))
		   (match_operand:SI 3 "reg_or_8bit_operand" "rI"))))]
  ""
  "s%2subl %r1,%3,%0")

(define_insn ""
  [(set (match_operand:DI 0 "register_operand" "=r")
	(minus:DI (mult:DI (match_operand:DI 1 "reg_or_0_operand" "rJ")
			   (match_operand:DI 2 "const48_operand" "I"))
		  (match_operand:DI 3 "reg_or_8bit_operand" "rI")))]
  ""
  "s%2subq %r1,%3,%0")

(define_insn "mulsi3"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(mult:SI (match_operand:SI 1 "reg_or_0_operand" "%rJ")
		 (match_operand:SI 2 "reg_or_0_operand" "rJ")))]
  ""
  "mull %r1,%r2,%0"
  [(set_attr "type" "imull")])

(define_insn ""
  [(set (match_operand:DI 0 "register_operand" "=r")
	(sign_extend:DI (mult:SI (match_operand:SI 1 "reg_or_0_operand" "%rJ")
				 (match_operand:SI 2 "reg_or_0_operand" "rJ"))))]
  ""
  "mull %r1,%r2,%0"
  [(set_attr "type" "imull")])

(define_insn "muldi3"
  [(set (match_operand:DI 0 "register_operand" "=r")
	(mult:DI (match_operand:DI 1 "reg_or_0_operand" "%rJ")
		 (match_operand:DI 2 "reg_or_0_operand" "rJ")))]
  ""
  "mulq %r1,%r2,%0"
  [(set_attr "type" "imulq")])

(define_insn "umuldi3_highpart"
  [(set (match_operand:DI 0 "register_operand" "=r")
	(truncate:DI
	 (lshiftrt:TI
	  (mult:TI (zero_extend:TI (match_operand:DI 1 "register_operand" "r"))
		   (zero_extend:TI (match_operand:DI 2 "register_operand" "r")))
	  (const_int 64))))]
  ""
  "umulh %1,%2,%0"
  [(set_attr "type" "imulh")])

(define_insn ""
  [(set (match_operand:DI 0 "register_operand" "=r")
	(truncate:DI
	 (lshiftrt:TI
	  (mult:TI (zero_extend:TI (match_operand:DI 1 "register_operand" "r"))
		   (match_operand:TI 2 "cint8_operand" "I"))
	  (const_int 64))))]
  ""
  "umulh %1,%2,%0"
  [(set_attr "type" "imulh")])

;; The divide and remainder operations always take their inputs from
;; r24 and r25, put their output in r27, and clobber r23 and r28.

;; ??? comment out the divsi routines since the library functions
;; don't seem to do the right thing with the high 32-bits of a
;; register nonzero.

;(define_expand "divsi3"
;  [(set (reg:SI 24) (match_operand:SI 1 "input_operand" ""))
;   (set (reg:SI 25) (match_operand:SI 2 "input_operand" ""))
;   (parallel [(set (reg:SI 27)
;		   (div:SI (reg:SI 24)
;			   (reg:SI 25)))
;	      (clobber (reg:DI 23))
;	      (clobber (reg:DI 28))])
;   (set (match_operand:SI 0 "general_operand" "")
;	(reg:SI 27))]
;  "!TARGET_OPEN_VMS"
;  "")

;(define_expand "udivsi3"
;  [(set (reg:SI 24) (match_operand:SI 1 "input_operand" ""))
;   (set (reg:SI 25) (match_operand:SI 2 "input_operand" ""))
;   (parallel [(set (reg:SI 27)
;		   (udiv:SI (reg:SI 24)
;			    (reg:SI 25)))
;	      (clobber (reg:DI 23))
;	      (clobber (reg:DI 28))])
;   (set (match_operand:SI 0 "general_operand" "")
;	(reg:SI 27))]
;  "!TARGET_OPEN_VMS"
;  "")

;(define_expand "modsi3"
;  [(set (reg:SI 24) (match_operand:SI 1 "input_operand" ""))
;   (set (reg:SI 25) (match_operand:SI 2 "input_operand" ""))
;   (parallel [(set (reg:SI 27)
;		   (mod:SI (reg:SI 24)
;			   (reg:SI 25)))
;	      (clobber (reg:DI 23))
;	      (clobber (reg:DI 28))])
;   (set (match_operand:SI 0 "general_operand" "")
;	(reg:SI 27))]
;  "!TARGET_OPEN_VMS"
;  "")

;(define_expand "umodsi3"
;  [(set (reg:SI 24) (match_operand:SI 1 "input_operand" ""))
;   (set (reg:SI 25) (match_operand:SI 2 "input_operand" ""))
;   (parallel [(set (reg:SI 27)
;		   (umod:SI (reg:SI 24)
;			    (reg:SI 25)))
;	      (clobber (reg:DI 23))
;	      (clobber (reg:DI 28))])
;   (set (match_operand:SI 0 "general_operand" "")
;	(reg:SI 27))]
;  "!TARGET_OPEN_VMS"
;  "")

(define_expand "divdi3"
  [(set (reg:DI 24) (match_operand:DI 1 "input_operand" ""))
   (set (reg:DI 25) (match_operand:DI 2 "input_operand" ""))
   (parallel [(set (reg:DI 27)
		   (div:DI (reg:DI 24)
			   (reg:DI 25)))
	      (clobber (reg:DI 23))
	      (clobber (reg:DI 28))])
   (set (match_operand:DI 0 "general_operand" "")
	(reg:DI 27))]
  "!TARGET_OPEN_VMS"
  "")

(define_expand "udivdi3"
  [(set (reg:DI 24) (match_operand:DI 1 "input_operand" ""))
   (set (reg:DI 25) (match_operand:DI 2 "input_operand" ""))
   (parallel [(set (reg:DI 27)
		   (udiv:DI (reg:DI 24)
			    (reg:DI 25)))
	      (clobber (reg:DI 23))
	      (clobber (reg:DI 28))])
   (set (match_operand:DI 0 "general_operand" "")
	(reg:DI 27))]
  "!TARGET_OPEN_VMS"
  "")

(define_expand "moddi3"
  [(set (reg:DI 24) (match_operand:DI 1 "input_operand" ""))
   (set (reg:DI 25) (match_operand:DI 2 "input_operand" ""))
   (parallel [(set (reg:DI 27)
		   (mod:DI (reg:DI 24)
			   (reg:DI 25)))
	      (clobber (reg:DI 23))
	      (clobber (reg:DI 28))])
   (set (match_operand:DI 0 "general_operand" "")
	(reg:DI 27))]
  "!TARGET_OPEN_VMS"
  "")

(define_expand "umoddi3"
  [(set (reg:DI 24) (match_operand:DI 1 "input_operand" ""))
   (set (reg:DI 25) (match_operand:DI 2 "input_operand" ""))
   (parallel [(set (reg:DI 27)
		   (umod:DI (reg:DI 24)
			    (reg:DI 25)))
	      (clobber (reg:DI 23))
	      (clobber (reg:DI 28))])
   (set (match_operand:DI 0 "general_operand" "")
	(reg:DI 27))]
  "!TARGET_OPEN_VMS"
  "")

;(define_insn ""
;  [(set (reg:SI 27)
;	(match_operator:SI 1 "divmod_operator"
;			[(reg:SI 24) (reg:SI 25)]))
;   (clobber (reg:DI 23))
;   (clobber (reg:DI 28))]
;  "!TARGET_OPEN_VMS"
;  "%E1 $24,$25,$27"
;  [(set_attr "type" "jsr")])

(define_insn ""
  [(set (reg:DI 27)
	(match_operator:DI 1 "divmod_operator"
			[(reg:DI 24) (reg:DI 25)]))
   (clobber (reg:DI 23))
   (clobber (reg:DI 28))]
  "!TARGET_OPEN_VMS"
  "%E1 $24,$25,$27"
  [(set_attr "type" "jsr")])

;; Next are the basic logical operations.  These only exist in DImode.

(define_insn "anddi3"
  [(set (match_operand:DI 0 "register_operand" "=r,r,r")
	(and:DI (match_operand:DI 1 "reg_or_0_operand" "%rJ,rJ,rJ")
		(match_operand:DI 2 "and_operand" "rI,N,MH")))]
  ""
  "@
   and %r1,%2,%0
   bic %r1,%N2,%0
   zapnot %r1,%m2,%0"
  [(set_attr "type" "ilog,ilog,shift")])

;; There are times when we can split an AND into two AND insns.  This occurs
;; when we can first clear any bytes and then clear anything else.  For
;; example "I & 0xffff07" is "(I & 0xffffff) & 0xffffffffffffff07".
;; Only do this when running on 64-bit host since the computations are
;; too messy otherwise.

(define_split
  [(set (match_operand:DI 0 "register_operand" "")
	(and:DI (match_operand:DI 1 "register_operand" "")
		(match_operand:DI 2 "const_int_operand" "")))]
  "HOST_BITS_PER_WIDE_INT == 64 && ! and_operand (operands[2], DImode)"
  [(set (match_dup 0) (and:DI (match_dup 1) (match_dup 3)))
   (set (match_dup 0) (and:DI (match_dup 0) (match_dup 4)))]
  "
{
  unsigned HOST_WIDE_INT mask1 = INTVAL (operands[2]);
  unsigned HOST_WIDE_INT mask2 = mask1;
  int i;

  /* For each byte that isn't all zeros, make it all ones.  */
  for (i = 0; i < 64; i += 8)
    if ((mask1 & ((HOST_WIDE_INT) 0xff << i)) != 0)
      mask1 |= (HOST_WIDE_INT) 0xff << i;

  /* Now turn on any bits we've just turned off.  */
  mask2 |= ~ mask1;

  operands[3] = GEN_INT (mask1);
  operands[4] = GEN_INT (mask2);
}")

(define_insn "zero_extendqihi2"
  [(set (match_operand:HI 0 "register_operand" "=r")
	(zero_extend:HI (match_operand:QI 1 "register_operand" "r")))]
  ""
  "zapnot %1,1,%0"
  [(set_attr "type" "shift")])

(define_insn ""
  [(set (match_operand:SI 0 "register_operand" "=r,r")
	(zero_extend:SI (match_operand:QI 1 "nonimmediate_operand" "r,m")))]
  "TARGET_BYTE_OPS"
  "@
   zapnot %1,1,%0
   ldbu %0,%1"
  [(set_attr "type" "shift,ld")])

(define_insn ""
  [(set (match_operand:SI 0 "register_operand" "=r")
	(zero_extend:SI (match_operand:QI 1 "register_operand" "r")))]
  "! TARGET_BYTE_OPS"
  "zapnot %1,1,%0"
  [(set_attr "type" "shift")])

(define_expand "zero_extendqisi2"
  [(set (match_operand:SI 0 "register_operand" "")
	(zero_extend:SI (match_operand:QI 1 "register_operand" "")))]
  ""
  "")

(define_insn ""
  [(set (match_operand:DI 0 "register_operand" "=r,r")
	(zero_extend:DI (match_operand:QI 1 "nonimmediate_operand" "r,m")))]
  "TARGET_BYTE_OPS"
  "@
   zapnot %1,1,%0
   ldbu %0,%1"
  [(set_attr "type" "shift,ld")])

(define_insn ""
  [(set (match_operand:DI 0 "register_operand" "=r")
	(zero_extend:DI (match_operand:QI 1 "register_operand" "r")))]
  "! TARGET_BYTE_OPS"
  "zapnot %1,1,%0"
  [(set_attr "type" "shift")])
  
(define_expand "zero_extendqidi2"
  [(set (match_operand:DI 0 "register_operand" "")
	(zero_extend:DI (match_operand:QI 1 "register_operand" "")))]
  ""
  "")
  
(define_insn ""
  [(set (match_operand:SI 0 "register_operand" "=r,r")
	(zero_extend:SI (match_operand:HI 1 "nonimmediate_operand" "r,m")))]
  "TARGET_BYTE_OPS"
  "@
   zapnot %1,3,%0
   ldwu %0,%1"
  [(set_attr "type" "shift,ld")])

(define_insn ""
  [(set (match_operand:SI 0 "register_operand" "=r")
	(zero_extend:SI (match_operand:HI 1 "register_operand" "r")))]
  "! TARGET_BYTE_OPS"
  "zapnot %1,3,%0"
  [(set_attr "type" "shift")])

(define_expand "zero_extendhisi2"
  [(set (match_operand:SI 0 "register_operand" "")
	(zero_extend:SI (match_operand:HI 1 "register_operand" "")))]
  ""
  "")

(define_insn ""
  [(set (match_operand:DI 0 "register_operand" "=r,r")
	(zero_extend:DI (match_operand:HI 1 "nonimmediate_operand" "r,m")))]
  "TARGET_BYTE_OPS"
  "@
   zapnot %1,3,%0
   ldwu %0,%1"
  [(set_attr "type" "shift,ld")])

(define_insn ""
  [(set (match_operand:DI 0 "register_operand" "=r")
	(zero_extend:DI (match_operand:HI 1 "register_operand" "r")))]
  ""
  "zapnot %1,3,%0"
  [(set_attr "type" "shift")])

(define_expand "zero_extendhidi2"
  [(set (match_operand:DI 0 "register_operand" "")
	(zero_extend:DI (match_operand:HI 1 "register_operand" "")))]
  ""
  "")

(define_insn "zero_extendsidi2"
  [(set (match_operand:DI 0 "register_operand" "=r")
	(zero_extend:DI (match_operand:SI 1 "register_operand" "r")))]
  ""
  "zapnot %1,15,%0"
  [(set_attr "type" "shift")])

(define_insn  ""
  [(set (match_operand:DI 0 "register_operand" "=r")
	(and:DI (not:DI (match_operand:DI 1 "reg_or_8bit_operand" "rI"))
		(match_operand:DI 2 "reg_or_0_operand" "rJ")))]
  ""
  "bic %r2,%1,%0"
  [(set_attr "type" "ilog")])

(define_insn "iordi3"
  [(set (match_operand:DI 0 "register_operand" "=r,r")
	(ior:DI (match_operand:DI 1 "reg_or_0_operand" "%rJ,rJ")
		(match_operand:DI 2 "or_operand" "rI,N")))]
  ""
  "@
   bis %r1,%2,%0
   ornot %r1,%N2,%0"
  [(set_attr "type" "ilog")])

(define_insn "one_cmpldi2"
  [(set (match_operand:DI 0 "register_operand" "=r")
	(not:DI (match_operand:DI 1 "reg_or_8bit_operand" "rI")))]
  ""
  "ornot $31,%1,%0"
  [(set_attr "type" "ilog")])

(define_insn ""
  [(set (match_operand:DI 0 "register_operand" "=r")
	(ior:DI (not:DI (match_operand:DI 1 "reg_or_8bit_operand" "rI"))
		(match_operand:DI 2 "reg_or_0_operand" "rJ")))]
  ""
  "ornot %r2,%1,%0"
  [(set_attr "type" "ilog")])

(define_insn "xordi3"
  [(set (match_operand:DI 0 "register_operand" "=r,r")
	(xor:DI (match_operand:DI 1 "reg_or_0_operand" "%rJ,rJ")
		(match_operand:DI 2 "or_operand" "rI,N")))]
  ""
  "@
   xor %r1,%2,%0
   eqv %r1,%N2,%0"
  [(set_attr "type" "ilog")])

(define_insn ""
  [(set (match_operand:DI 0 "register_operand" "=r")
	(not:DI (xor:DI (match_operand:DI 1 "register_operand" "%rJ")
			(match_operand:DI 2 "register_operand" "rI"))))]
  ""
  "eqv %r1,%2,%0"
  [(set_attr "type" "ilog")])

;; Next come the shifts and the various extract and insert operations.

(define_insn "ashldi3"
  [(set (match_operand:DI 0 "register_operand" "=r,r")
	(ashift:DI (match_operand:DI 1 "reg_or_0_operand" "rJ,rJ")
		   (match_operand:DI 2 "reg_or_6bit_operand" "P,rI")))]
  ""
  "*
{
  switch (which_alternative)
    {
    case 0:
      if (operands[2] == const1_rtx)
	return \"addq %r1,%r1,%0\";
      else
	return \"s%P2addq %r1,0,%0\";
    case 1:
      return \"sll %r1,%2,%0\";
    }
}"
  [(set_attr "type" "iadd,shift")])

;; ??? The following pattern is made by combine, but earlier phases
;; (specifically flow) can't handle it.  This occurs in jump.c.  Deal
;; with this in a better way at some point.
;;(define_insn ""
;;  [(set (match_operand:DI 0 "register_operand" "=r")
;;	(sign_extend:DI
;;	 (subreg:SI (ashift:DI (match_operand:DI 1 "reg_or_0_operand" "rJ")
;;			       (match_operand:DI 2 "const_int_operand" "P"))
;;		    0)))]
;;  "INTVAL (operands[2]) >= 1 && INTVAL (operands[2]) <= 3"
;;  "*
;;{
;;  if (operands[2] == const1_rtx)
;;    return \"addl %r1,%r1,%0\";
;;  else
;;    return \"s%P2addl %r1,0,%0\";
;; }"
;;  [(set_attr "type" "iadd")])
			  
(define_insn "lshrdi3"
  [(set (match_operand:DI 0 "register_operand" "=r")
	(lshiftrt:DI (match_operand:DI 1 "reg_or_0_operand" "rJ")
		     (match_operand:DI 2 "reg_or_6bit_operand" "rI")))]
  ""
  "srl %r1,%2,%0"
  [(set_attr "type" "shift")])

(define_insn "ashrdi3"
  [(set (match_operand:DI 0 "register_operand" "=r")
	(ashiftrt:DI (match_operand:DI 1 "reg_or_0_operand" "rJ")
		     (match_operand:DI 2 "reg_or_6bit_operand" "rI")))]
  ""
  "sra %r1,%2,%0"
  [(set_attr "type" "shift")])

(define_expand "extendqihi2"
  [(set (match_dup 2)
	(ashift:DI (match_operand:QI 1 "some_operand" "")
		   (const_int 56)))
   (set (match_operand:HI 0 "register_operand" "")
	(ashiftrt:DI (match_dup 2)
		     (const_int 56)))]
  ""
  "
{
  if (TARGET_BYTE_OPS)
    {
      emit_insn (gen_extendqihi2x (operands[0],
				   force_reg (QImode, operands[1])));
      DONE;
    }
 
 /* If we have an unaligned MEM, extend to DImode (which we do
     specially) and then copy to the result.  */
  if (unaligned_memory_operand (operands[1], HImode))
    {
      rtx temp = gen_reg_rtx (DImode);

      emit_insn (gen_extendqidi2 (temp, operands[1]));
      emit_move_insn (operands[0], gen_lowpart (HImode, temp));
      DONE;
    }

  operands[0] = gen_lowpart (DImode, operands[0]);
  operands[1] = gen_lowpart (DImode, force_reg (QImode, operands[1]));
  operands[2] = gen_reg_rtx (DImode);
}")

(define_insn "extendqidi2x"
  [(set (match_operand:DI 0 "register_operand" "=r")
	(sign_extend:DI (match_operand:QI 1 "register_operand" "r")))]
  "TARGET_BYTE_OPS"
  "sextb %1,%0"
  [(set_attr "type" "shift")])

(define_insn "extendhidi2x"
  [(set (match_operand:DI 0 "register_operand" "=r")
	(sign_extend:DI (match_operand:HI 1 "register_operand" "r")))]
  "TARGET_BYTE_OPS"
  "sextw %1,%0"
  [(set_attr "type" "shift")])

(define_insn "extendqisi2x"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(sign_extend:SI (match_operand:QI 1 "register_operand" "r")))]
  "TARGET_BYTE_OPS"
  "sextb %1,%0"
  [(set_attr "type" "shift")])

(define_insn "extendhisi2x"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(sign_extend:SI (match_operand:HI 1 "register_operand" "r")))]
  "TARGET_BYTE_OPS"
  "sextw %1,%0"
  [(set_attr "type" "shift")])

(define_insn "extendqihi2x"
  [(set (match_operand:HI 0 "register_operand" "=r")
	(sign_extend:HI (match_operand:QI 1 "register_operand" "r")))]
  "TARGET_BYTE_OPS"
  "sextb %1,%0"
  [(set_attr "type" "shift")])

(define_expand "extendqisi2"
  [(set (match_dup 2)
	(ashift:DI (match_operand:QI 1 "some_operand" "")
		   (const_int 56)))
   (set (match_operand:SI 0 "register_operand" "")
	(ashiftrt:DI (match_dup 2)
		     (const_int 56)))]
  ""
  "
{
  if (TARGET_BYTE_OPS)
    {
      emit_insn (gen_extendqisi2x (operands[0],
				   force_reg (QImode, operands[1])));
      DONE;
    }

  /* If we have an unaligned MEM, extend to a DImode form of
     the result (which we do specially).  */
  if (unaligned_memory_operand (operands[1], QImode))
    {
      rtx temp = gen_reg_rtx (DImode);

      emit_insn (gen_extendqidi2 (temp, operands[1]));
      emit_move_insn (operands[0], gen_lowpart (SImode, temp));
      DONE;
    }

  operands[0] = gen_lowpart (DImode, operands[0]);
  operands[1] = gen_lowpart (DImode, force_reg (QImode, operands[1]));
  operands[2] = gen_reg_rtx (DImode);
}")

(define_expand "extendqidi2"
  [(set (match_dup 2)
	(ashift:DI (match_operand:QI 1 "some_operand" "")
		   (const_int 56)))
   (set (match_operand:DI 0 "register_operand" "")
	(ashiftrt:DI (match_dup 2)
		     (const_int 56)))]
  ""
  "
{ extern rtx get_unaligned_address ();

  if (TARGET_BYTE_OPS)
    {
      emit_insn (gen_extendqidi2x (operands[0],
				   force_reg (QImode, operands[1])));
      DONE;
    }

  if (unaligned_memory_operand (operands[1], QImode))
    {
      rtx seq
	= gen_unaligned_extendqidi (operands[0],
				    get_unaligned_address (operands[1], 1));

      alpha_set_memflags (seq, operands[1]);
      emit_insn (seq);
      DONE;
    }

  operands[1] = gen_lowpart (DImode, force_reg (QImode, operands[1]));
  operands[2] = gen_reg_rtx (DImode);
}")

(define_expand "extendhisi2"
  [(set (match_dup 2)
	(ashift:DI (match_operand:HI 1 "some_operand" "")
		   (const_int 48)))
   (set (match_operand:SI 0 "register_operand" "")
	(ashiftrt:DI (match_dup 2)
		     (const_int 48)))]
  ""
  "
{
  if (TARGET_BYTE_OPS)
    {
      emit_insn (gen_extendhisi2x (operands[0],
				   force_reg (HImode, operands[1])));
      DONE;
    }

  /* If we have an unaligned MEM, extend to a DImode form of
     the result (which we do specially).  */
  if (unaligned_memory_operand (operands[1], HImode))
    {
      rtx temp = gen_reg_rtx (DImode);

      emit_insn (gen_extendhidi2 (temp, operands[1]));
      emit_move_insn (operands[0], gen_lowpart (SImode, temp));
      DONE;
    }

  operands[0] = gen_lowpart (DImode, operands[0]);
  operands[1] = gen_lowpart (DImode, force_reg (HImode, operands[1]));
  operands[2] = gen_reg_rtx (DImode);
}")

(define_expand "extendhidi2"
  [(set (match_dup 2)
	(ashift:DI (match_operand:HI 1 "some_operand" "")
		   (const_int 48)))
   (set (match_operand:DI 0 "register_operand" "")
	(ashiftrt:DI (match_dup 2)
		     (const_int 48)))]
  ""
  "
{ extern rtx get_unaligned_address ();

  if (TARGET_BYTE_OPS)
    {
      emit_insn (gen_extendhidi2x (operands[0],
				   force_reg (HImode, operands[1])));
      DONE;
    }

  if (unaligned_memory_operand (operands[1], HImode))
    {
      rtx seq
	= gen_unaligned_extendhidi (operands[0],
				    get_unaligned_address (operands[1], 2));

      alpha_set_memflags (seq, operands[1]);
      emit_insn (seq);
      DONE;
    }

  operands[1] = gen_lowpart (DImode, force_reg (HImode, operands[1]));
  operands[2] = gen_reg_rtx (DImode);
}")

;; Here's how we sign extend an unaligned byte and halfword.  Doing this
;; as a pattern saves one instruction.  The code is similar to that for
;; the unaligned loads (see below).
;;
;; Operand 1 is the address + 1 (+2 for HI), operand 0 is the result.
(define_expand "unaligned_extendqidi"
  [(set (match_dup 2) (match_operand:DI 1 "address_operand" ""))
   (set (match_dup 3)
	(mem:DI (and:DI (plus:DI (match_dup 2) (const_int -1))
			(const_int -8))))
   (set (match_dup 4)
	(ashift:DI (match_dup 3)
		   (minus:DI (const_int 56)
			     (ashift:DI
			      (and:DI (plus:DI (match_dup 2) (const_int -1))
				      (const_int 7))
			      (const_int 3)))))
   (set (subreg:DI (match_operand:QI 0 "register_operand" "") 0)
	(ashiftrt:DI (match_dup 4) (const_int 56)))]
  ""
  "
{ operands[2] = gen_reg_rtx (DImode);
  operands[3] = gen_reg_rtx (DImode);
  operands[4] = gen_reg_rtx (DImode);
}")

(define_expand "unaligned_extendhidi"
  [(set (match_dup 2) (match_operand:DI 1 "address_operand" ""))
   (set (match_dup 3)
	(mem:DI (and:DI (plus:DI (match_dup 2) (const_int -2))
			(const_int -8))))
   (set (match_dup 4)
	(ashift:DI (match_dup 3)
		   (minus:DI (const_int 56)
			     (ashift:DI
			      (and:DI (plus:DI (match_dup 2) (const_int -1))
				      (const_int 7))
			      (const_int 3)))))
   (set (subreg:DI (match_operand:QI 0 "register_operand" "") 0)
	(ashiftrt:DI (match_dup 4) (const_int 48)))]
  ""
  "
{ operands[2] = gen_reg_rtx (DImode);
  operands[3] = gen_reg_rtx (DImode);
  operands[4] = gen_reg_rtx (DImode);
}")

(define_insn ""
  [(set (match_operand:DI 0 "register_operand" "=r")
	(zero_extract:DI (match_operand:DI 1 "reg_or_0_operand" "rJ")
			 (match_operand:DI 2 "mode_width_operand" "n")
			 (match_operand:DI 3 "mul8_operand" "I")))]
  ""
  "ext%M2l %r1,%s3,%0"
  [(set_attr "type" "shift")])

(define_insn ""
  [(set (match_operand:DI 0 "register_operand" "=r")
	(zero_extract:DI (match_operand:DI 1 "reg_or_0_operand" "rJ")
			 (match_operand:DI 2 "mode_width_operand" "n")
			 (ashift:DI (match_operand:DI 3 "reg_or_8bit_operand" "rI")
				    (const_int 3))))]
  ""
  "ext%M2l %r1,%3,%0"
  [(set_attr "type" "shift")])

(define_insn ""
  [(set (match_operand:DI 0 "register_operand" "=r")
	(ashift:DI
	 (match_operand:DI 1 "reg_or_0_operand" "rJ")
	  (minus:DI (const_int 56)
		    (ashift:DI
		     (and:DI
		      (plus:DI (match_operand:DI 2 "reg_or_8bit_operand" "rI")
			       (const_int -1))
		      (const_int 7))
		     (const_int 3)))))]
  ""
  "extqh %r1,%2,%0"
  [(set_attr "type" "shift")])

(define_insn ""
  [(set (match_operand:DI 0 "register_operand" "=r")
	(ashift:DI
	 (and:DI (match_operand:DI 1 "reg_or_0_operand" "rJ")
		 (const_int 2147483647))
	 (minus:DI (const_int 56)
		    (ashift:DI
		     (and:DI
		      (plus:DI (match_operand:DI 2 "reg_or_8bit_operand" "rI")
			       (const_int -1))
		      (const_int 7))
		     (const_int 3)))))]
  ""
  "extlh %r1,%2,%0"
  [(set_attr "type" "shift")])

(define_insn ""
  [(set (match_operand:DI 0 "register_operand" "=r")
	(ashift:DI
	 (and:DI (match_operand:DI 1 "reg_or_0_operand" "rJ")
		 (const_int 65535))
	 (minus:DI (const_int 56)
		    (ashift:DI
		     (and:DI
		      (plus:DI (match_operand:DI 2 "reg_or_8bit_operand" "rI")
			       (const_int -1))
		      (const_int 7))
		     (const_int 3)))))]
  ""
  "extwh %r1,%2,%0"
  [(set_attr "type" "shift")])

;; This converts an extXl into an extXh with an appropriate adjustment
;; to the address calculation.

;;(define_split
;;  [(set (match_operand:DI 0 "register_operand" "")
;;	(ashift:DI (zero_extract:DI (match_operand:DI 1 "register_operand" "")
;;				    (match_operand:DI 2 "mode_width_operand" "")
;;				    (ashift:DI (match_operand:DI 3 "" "")
;;					       (const_int 3)))
;;		   (match_operand:DI 4 "const_int_operand" "")))
;;   (clobber (match_operand:DI 5 "register_operand" ""))]
;;  "INTVAL (operands[4]) == 64 - INTVAL (operands[2])"
;;  [(set (match_dup 5) (match_dup 6))
;;   (set (match_dup 0)
;;	(ashift:DI (zero_extract:DI (match_dup 1) (match_dup 2)
;;				    (ashift:DI (plus:DI (match_dup 5)
;;							(match_dup 7))
;;					       (const_int 3)))
;;		   (match_dup 4)))]
;;  "
;;{
;;  operands[6] = plus_constant (operands[3], 
;;			       INTVAL (operands[2]) / BITS_PER_UNIT);
;;  operands[7] = GEN_INT (- INTVAL (operands[2]) / BITS_PER_UNIT);
;;}")
  
(define_insn ""
  [(set (match_operand:DI 0 "register_operand" "=r")
	(ashift:DI (zero_extend:DI (match_operand:QI 1 "register_operand" "r"))
		   (match_operand:DI 2 "mul8_operand" "I")))]
  ""
  "insbl %1,%s2,%0"
  [(set_attr "type" "shift")])

(define_insn ""
  [(set (match_operand:DI 0 "register_operand" "=r")
	(ashift:DI (zero_extend:DI (match_operand:HI 1 "register_operand" "r"))
		   (match_operand:DI 2 "mul8_operand" "I")))]
  ""
  "inswl %1,%s2,%0"
  [(set_attr "type" "shift")])

(define_insn ""
  [(set (match_operand:DI 0 "register_operand" "=r")
	(ashift:DI (zero_extend:DI (match_operand:SI 1 "register_operand" "r"))
		   (match_operand:DI 2 "mul8_operand" "I")))]
  ""
  "insll %1,%s2,%0"
  [(set_attr "type" "shift")])

(define_insn ""
  [(set (match_operand:DI 0 "register_operand" "=r")
	(ashift:DI (zero_extend:DI (match_operand:QI 1 "register_operand" "r"))
		   (ashift:DI (match_operand:DI 2 "reg_or_8bit_operand" "rI")
			      (const_int 3))))]
  ""
  "insbl %1,%2,%0"
  [(set_attr "type" "shift")])

(define_insn ""
  [(set (match_operand:DI 0 "register_operand" "=r")
	(ashift:DI (zero_extend:DI (match_operand:HI 1 "register_operand" "r"))
		   (ashift:DI (match_operand:DI 2 "reg_or_8bit_operand" "rI")
			      (const_int 3))))]
  ""
  "inswl %1,%2,%0"
  [(set_attr "type" "shift")])

(define_insn ""
  [(set (match_operand:DI 0 "register_operand" "=r")
	(ashift:DI (zero_extend:DI (match_operand:SI 1 "register_operand" "r"))
		   (ashift:DI (match_operand:DI 2 "reg_or_8bit_operand" "rI")
			      (const_int 3))))]
  ""
  "insll %1,%2,%0"
  [(set_attr "type" "shift")])

;; We do not include the insXh insns because they are complex to express
;; and it does not appear that we would ever want to generate them.

(define_insn ""
  [(set (match_operand:DI 0 "register_operand" "=r")
	(and:DI (not:DI (ashift:DI
			 (match_operand:DI 2 "mode_mask_operand" "n")
			 (ashift:DI
			  (match_operand:DI 3 "reg_or_8bit_operand" "rI")
			  (const_int 3))))
		(match_operand:DI 1 "reg_or_0_operand" "rJ")))]
  ""
  "msk%U2l %r1,%3,%0"
  [(set_attr "type" "shift")])

;; We do not include the mskXh insns because it does not appear we would ever
;; generate one.

;; Floating-point operations.  All the double-precision insns can extend
;; from single, so indicate that.  The exception are the ones that simply
;; play with the sign bits; it's not clear what to do there.

(define_insn "abssf2"
  [(set (match_operand:SF 0 "register_operand" "=f")
	(abs:SF (match_operand:SF 1 "reg_or_fp0_operand" "fG")))]
  "TARGET_FP"
  "cpys $f31,%R1,%0"
  [(set_attr "type" "fcpys")])

(define_insn "absdf2"
  [(set (match_operand:DF 0 "register_operand" "=f")
	(abs:DF (match_operand:DF 1 "reg_or_fp0_operand" "fG")))]
  "TARGET_FP"
  "cpys $f31,%R1,%0"
  [(set_attr "type" "fcpys")])

(define_insn "negsf2"
  [(set (match_operand:SF 0 "register_operand" "=f")
	(neg:SF (match_operand:SF 1 "reg_or_fp0_operand" "fG")))]
  "TARGET_FP"
  "cpysn %R1,%R1,%0"
  [(set_attr "type" "fadd")])

(define_insn "negdf2"
  [(set (match_operand:DF 0 "register_operand" "=f")
	(neg:DF (match_operand:DF 1 "reg_or_fp0_operand" "fG")))]
  "TARGET_FP"
  "cpysn %R1,%R1,%0"
  [(set_attr "type" "fadd")])

(define_insn ""
  [(set (match_operand:SF 0 "register_operand" "=&f")
	(plus:SF (match_operand:SF 1 "reg_or_fp0_operand" "%fG")
		 (match_operand:SF 2 "reg_or_fp0_operand" "fG")))]
  "TARGET_FP && alpha_tp == ALPHA_TP_INSN"
  "add%,%)%& %R1,%R2,%0"
  [(set_attr "type" "fadd")
   (set_attr "trap" "yes")])

(define_insn "addsf3"
  [(set (match_operand:SF 0 "register_operand" "=f")
	(plus:SF (match_operand:SF 1 "reg_or_fp0_operand" "%fG")
		 (match_operand:SF 2 "reg_or_fp0_operand" "fG")))]
  "TARGET_FP"
  "add%,%)%& %R1,%R2,%0"
  [(set_attr "type" "fadd")
   (set_attr "trap" "yes")])

(define_insn ""
  [(set (match_operand:DF 0 "register_operand" "=&f")
	(plus:DF (match_operand:DF 1 "reg_or_fp0_operand" "%fG")
		 (match_operand:DF 2 "reg_or_fp0_operand" "fG")))]
  "TARGET_FP && alpha_tp == ALPHA_TP_INSN"
  "add%-%)%& %R1,%R2,%0"
  [(set_attr "type" "fadd")
   (set_attr "trap" "yes")])

(define_insn "adddf3"
  [(set (match_operand:DF 0 "register_operand" "=f")
	(plus:DF (match_operand:DF 1 "reg_or_fp0_operand" "%fG")
		 (match_operand:DF 2 "reg_or_fp0_operand" "fG")))]
  "TARGET_FP"
  "add%-%)%& %R1,%R2,%0"
  [(set_attr "type" "fadd")
   (set_attr "trap" "yes")])

(define_insn ""
  [(set (match_operand:DF 0 "register_operand" "=f")
	(plus:DF (float_extend:DF
		  (match_operand:SF 1 "reg_or_fp0_operand" "fG"))
		 (match_operand:DF 2 "reg_or_fp0_operand" "fG")))]
  "TARGET_FP && alpha_tp != ALPHA_TP_INSN"
  "add%-%)%& %R1,%R2,%0"
  [(set_attr "type" "fadd")
   (set_attr "trap" "yes")])

(define_insn ""
  [(set (match_operand:DF 0 "register_operand" "=f")
	(plus:DF (float_extend:DF
		  (match_operand:SF 1 "reg_or_fp0_operand" "%fG"))
		 (float_extend:DF
		  (match_operand:SF 2 "reg_or_fp0_operand" "fG"))))]
  "TARGET_FP && alpha_tp != ALPHA_TP_INSN"
  "add%-%)%& %R1,%R2,%0"
  [(set_attr "type" "fadd")
   (set_attr "trap" "yes")])

(define_insn "fix_truncdfdi2"
  [(set (match_operand:DI 0 "register_operand" "=f")
	(fix:DI (match_operand:DF 1 "reg_or_fp0_operand" "fG")))]
  "TARGET_FP"
  "cvt%-qc %R1,%0"
  [(set_attr "type" "fadd")])

(define_insn "fix_truncsfdi2"
  [(set (match_operand:DI 0 "register_operand" "=f")
	(fix:DI (float_extend:DF
		 (match_operand:SF 1 "reg_or_fp0_operand" "fG"))))]
  "TARGET_FP"
  "cvt%-qc %R1,%0"
  [(set_attr "type" "fadd")])

(define_insn ""
  [(set (match_operand:SF 0 "register_operand" "=&f")
	(float:SF (match_operand:DI 1 "register_operand" "f")))]
  "TARGET_FP && alpha_tp == ALPHA_TP_INSN"
  "cvtq%,%+%& %1,%0"
  [(set_attr "type" "fadd")
   (set_attr "trap" "yes")])

(define_insn "floatdisf2"
  [(set (match_operand:SF 0 "register_operand" "=f")
	(float:SF (match_operand:DI 1 "register_operand" "f")))]
  "TARGET_FP"
  "cvtq%,%+%& %1,%0"
  [(set_attr "type" "fadd")
   (set_attr "trap" "yes")])

(define_insn ""
  [(set (match_operand:DF 0 "register_operand" "=&f")
	(float:DF (match_operand:DI 1 "register_operand" "f")))]
  "TARGET_FP && alpha_tp == ALPHA_TP_INSN"
  "cvtq%-%+%& %1,%0"
  [(set_attr "type" "fadd")
   (set_attr "trap" "yes")])

(define_insn "floatdidf2"
  [(set (match_operand:DF 0 "register_operand" "=f")
	(float:DF (match_operand:DI 1 "register_operand" "f")))]
  "TARGET_FP"
  "cvtq%-%+%& %1,%0"
  [(set_attr "type" "fadd")
   (set_attr "trap" "yes")])

(define_expand "extendsfdf2"
  [(use (match_operand:DF 0 "register_operand" ""))
   (use (match_operand:SF 1 "nonimmediate_operand" ""))]
  "TARGET_FP"
"
{
  if (alpha_tp == ALPHA_TP_INSN)
    emit_insn (gen_extendsfdf2_tp (operands[0],
				   force_reg (SFmode, operands[1])));
  else
    emit_insn (gen_extendsfdf2_no_tp (operands[0], operands[1]));

  DONE;
}")
;; FIXME
(define_insn "extendsfdf2_tp"
  [(set (match_operand:DF 0 "register_operand" "=&f")
	(float_extend:DF (match_operand:SF 1 "register_operand" "f")))]
  "TARGET_FP && alpha_tp == ALPHA_TP_INSN"
  "cvtsts %1,%0"
  [(set_attr "type" "fadd")
   (set_attr "trap" "yes")])

(define_insn "extendsfdf2_no_tp"
  [(set (match_operand:DF 0 "register_operand" "=f,f,m")
	(float_extend:DF (match_operand:SF 1 "nonimmediate_operand" "f,m,f")))]
  "TARGET_FP && alpha_tp != ALPHA_TP_INSN"
  "@
   cpys %1,%1,%0
   ld%, %0,%1
   st%- %1,%0"
  [(set_attr "type" "fcpys,ld,st")
   (set_attr "trap" "yes")])

(define_insn ""
  [(set (match_operand:SF 0 "register_operand" "=&f")
	(float_truncate:SF (match_operand:DF 1 "reg_or_fp0_operand" "fG")))]
  "TARGET_FP && alpha_tp == ALPHA_TP_INSN"
  "cvt%-%,%)%& %R1,%0"
  [(set_attr "type" "fadd")
   (set_attr "trap" "yes")])

(define_insn "truncdfsf2"
  [(set (match_operand:SF 0 "register_operand" "=f")
	(float_truncate:SF (match_operand:DF 1 "reg_or_fp0_operand" "fG")))]
  "TARGET_FP"
  "cvt%-%,%)%& %R1,%0"
  [(set_attr "type" "fadd")
   (set_attr "trap" "yes")])

(define_insn ""
  [(set (match_operand:SF 0 "register_operand" "=&f")
	(div:SF (match_operand:SF 1 "reg_or_fp0_operand" "fG")
		(match_operand:SF 2 "reg_or_fp0_operand" "fG")))]
  "TARGET_FP && alpha_tp == ALPHA_TP_INSN"
  "div%,%)%& %R1,%R2,%0"
  [(set_attr "type" "fdivs")
   (set_attr "trap" "yes")])

(define_insn "divsf3"
  [(set (match_operand:SF 0 "register_operand" "=f")
	(div:SF (match_operand:SF 1 "reg_or_fp0_operand" "fG")
		(match_operand:SF 2 "reg_or_fp0_operand" "fG")))]
  "TARGET_FP"
  "div%,%)%& %R1,%R2,%0"
  [(set_attr "type" "fdivs")
   (set_attr "trap" "yes")])

(define_insn ""
  [(set (match_operand:DF 0 "register_operand" "=&f")
	(div:DF (match_operand:DF 1 "reg_or_fp0_operand" "fG")
		(match_operand:DF 2 "reg_or_fp0_operand" "fG")))]
  "TARGET_FP && alpha_tp == ALPHA_TP_INSN"
  "div%-%)%& %R1,%R2,%0"
  [(set_attr "type" "fdivt")
   (set_attr "trap" "yes")])

(define_insn "divdf3"
  [(set (match_operand:DF 0 "register_operand" "=f")
	(div:DF (match_operand:DF 1 "reg_or_fp0_operand" "fG")
		(match_operand:DF 2 "reg_or_fp0_operand" "fG")))]
  "TARGET_FP"
  "div%-%)%& %R1,%R2,%0"
  [(set_attr "type" "fdivt")
   (set_attr "trap" "yes")])

(define_insn ""
  [(set (match_operand:DF 0 "register_operand" "=f")
	(div:DF (float_extend:DF (match_operand:SF 1 "reg_or_fp0_operand" "fG"))
		(match_operand:DF 2 "reg_or_fp0_operand" "fG")))]
  "TARGET_FP && alpha_tp != ALPHA_TP_INSN"
  "div%-%)%& %R1,%R2,%0"
  [(set_attr "type" "fdivt")
   (set_attr "trap" "yes")])

(define_insn ""
  [(set (match_operand:DF 0 "register_operand" "=f")
	(div:DF (match_operand:DF 1 "reg_or_fp0_operand" "fG")
		(float_extend:DF
		 (match_operand:SF 2 "reg_or_fp0_operand" "fG"))))]
  "TARGET_FP && alpha_tp != ALPHA_TP_INSN"
  "div%-%)%& %R1,%R2,%0"
  [(set_attr "type" "fdivt")
   (set_attr "trap" "yes")])

(define_insn ""
  [(set (match_operand:DF 0 "register_operand" "=f")
	(div:DF (float_extend:DF (match_operand:SF 1 "reg_or_fp0_operand" "fG"))
		(float_extend:DF (match_operand:SF 2 "reg_or_fp0_operand" "fG"))))]
  "TARGET_FP && alpha_tp != ALPHA_TP_INSN"
  "div%-%)%& %R1,%R2,%0"
  [(set_attr "type" "fdivt")
   (set_attr "trap" "yes")])

(define_insn ""
  [(set (match_operand:SF 0 "register_operand" "=&f")
	(mult:SF (match_operand:SF 1 "reg_or_fp0_operand" "%fG")
		 (match_operand:SF 2 "reg_or_fp0_operand" "fG")))]
  "TARGET_FP && alpha_tp == ALPHA_TP_INSN"
  "mul%,%)%& %R1,%R2,%0"
  [(set_attr "type" "fmul")
   (set_attr "trap" "yes")])

(define_insn "mulsf3"
  [(set (match_operand:SF 0 "register_operand" "=f")
	(mult:SF (match_operand:SF 1 "reg_or_fp0_operand" "%fG")
		 (match_operand:SF 2 "reg_or_fp0_operand" "fG")))]
  "TARGET_FP"
  "mul%,%)%& %R1,%R2,%0"
  [(set_attr "type" "fmul")
   (set_attr "trap" "yes")])

(define_insn ""
  [(set (match_operand:DF 0 "register_operand" "=&f")
	(mult:DF (match_operand:DF 1 "reg_or_fp0_operand" "%fG")
		 (match_operand:DF 2 "reg_or_fp0_operand" "fG")))]
  "TARGET_FP && alpha_tp == ALPHA_TP_INSN"
  "mul%-%)%& %R1,%R2,%0"
  [(set_attr "type" "fmul")
   (set_attr "trap" "yes")])

(define_insn "muldf3"
  [(set (match_operand:DF 0 "register_operand" "=f")
	(mult:DF (match_operand:DF 1 "reg_or_fp0_operand" "%fG")
		 (match_operand:DF 2 "reg_or_fp0_operand" "fG")))]
  "TARGET_FP"
  "mul%-%)%& %R1,%R2,%0"
  [(set_attr "type" "fmul")
   (set_attr "trap" "yes")])

(define_insn ""
  [(set (match_operand:DF 0 "register_operand" "=f")
	(mult:DF (float_extend:DF
		  (match_operand:SF 1 "reg_or_fp0_operand" "fG"))
		 (match_operand:DF 2 "reg_or_fp0_operand" "fG")))]
  "TARGET_FP && alpha_tp != ALPHA_TP_INSN"
  "mul%-%)%& %R1,%R2,%0"
  [(set_attr "type" "fmul")
   (set_attr "trap" "yes")])

(define_insn ""
  [(set (match_operand:DF 0 "register_operand" "=f")
	(mult:DF (float_extend:DF
		  (match_operand:SF 1 "reg_or_fp0_operand" "%fG"))
		 (float_extend:DF
		  (match_operand:SF 2 "reg_or_fp0_operand" "fG"))))]
  "TARGET_FP && alpha_tp != ALPHA_TP_INSN"
  "mul%-%)%& %R1,%R2,%0"
  [(set_attr "type" "fmul")
   (set_attr "trap" "yes")])

(define_insn ""
  [(set (match_operand:SF 0 "register_operand" "=&f")
	(minus:SF (match_operand:SF 1 "reg_or_fp0_operand" "fG")
		  (match_operand:SF 2 "reg_or_fp0_operand" "fG")))]
  "TARGET_FP && alpha_tp == ALPHA_TP_INSN"
  "sub%,%)%& %R1,%R2,%0"
  [(set_attr "type" "fadd")
   (set_attr "trap" "yes")])

(define_insn "subsf3"
  [(set (match_operand:SF 0 "register_operand" "=f")
	(minus:SF (match_operand:SF 1 "reg_or_fp0_operand" "fG")
		  (match_operand:SF 2 "reg_or_fp0_operand" "fG")))]
  "TARGET_FP"
  "sub%,%)%& %R1,%R2,%0"
  [(set_attr "type" "fadd")
   (set_attr "trap" "yes")])

(define_insn ""
  [(set (match_operand:DF 0 "register_operand" "=&f")
	(minus:DF (match_operand:DF 1 "reg_or_fp0_operand" "fG")
		  (match_operand:DF 2 "reg_or_fp0_operand" "fG")))]
  "TARGET_FP && alpha_tp == ALPHA_TP_INSN"
  "sub%-%)%& %R1,%R2,%0"
  [(set_attr "type" "fadd")
   (set_attr "trap" "yes")])

(define_insn "subdf3"
  [(set (match_operand:DF 0 "register_operand" "=f")
	(minus:DF (match_operand:DF 1 "reg_or_fp0_operand" "fG")
		  (match_operand:DF 2 "reg_or_fp0_operand" "fG")))]
  "TARGET_FP"
  "sub%-%)%& %R1,%R2,%0"
  [(set_attr "type" "fadd")
   (set_attr "trap" "yes")])

(define_insn ""
  [(set (match_operand:DF 0 "register_operand" "=f")
	(minus:DF (float_extend:DF
		   (match_operand:SF 1 "reg_or_fp0_operand" "fG"))
		  (match_operand:DF 2 "reg_or_fp0_operand" "fG")))]
  "TARGET_FP && alpha_tp != ALPHA_TP_INSN"
  "sub%-%)%& %R1,%R2,%0"
  [(set_attr "type" "fadd")
   (set_attr "trap" "yes")])

(define_insn ""
  [(set (match_operand:DF 0 "register_operand" "=f")
	(minus:DF (match_operand:DF 1 "reg_or_fp0_operand" "fG")
		  (float_extend:DF
		   (match_operand:SF 2 "reg_or_fp0_operand" "fG"))))]
  "TARGET_FP && alpha_tp != ALPHA_TP_INSN"
  "sub%-%)%& %R1,%R2,%0"
  [(set_attr "type" "fadd")
   (set_attr "trap" "yes")])

(define_insn ""
  [(set (match_operand:DF 0 "register_operand" "=f")
	(minus:DF (float_extend:DF
		   (match_operand:SF 1 "reg_or_fp0_operand" "fG"))
		  (float_extend:DF
		   (match_operand:SF 2 "reg_or_fp0_operand" "fG"))))]
  "TARGET_FP && alpha_tp != ALPHA_TP_INSN"
  "sub%-%)%& %R1,%R2,%0"
  [(set_attr "type" "fadd")
   (set_attr "trap" "yes")])

;; Next are all the integer comparisons, and conditional moves and branches
;; and some of the related define_expand's and define_split's.

(define_insn ""
  [(set (match_operand:DI 0 "register_operand" "=r")
	(match_operator:DI 1 "alpha_comparison_operator"
			   [(match_operand:DI 2 "reg_or_0_operand" "rJ")
			    (match_operand:DI 3 "reg_or_8bit_operand" "rI")]))]
  ""
  "cmp%C1 %r2,%3,%0"
  [(set_attr "type" "icmp")])

(define_insn ""
  [(set (match_operand:DI 0 "register_operand" "=r")
        (match_operator:DI 1 "alpha_swapped_comparison_operator"
			   [(match_operand:DI 2 "reg_or_8bit_operand" "rI")
			    (match_operand:DI 3 "reg_or_0_operand" "rJ")]))]
  ""
  "cmp%c1 %r3,%2,%0"
  [(set_attr "type" "icmp")])

;; This pattern exists so conditional moves of SImode values are handled.
;; Comparisons are still done in DImode though.

(define_insn ""
  [(set (match_operand:SI 0 "register_operand" "=r,r,r,r")
	(if_then_else:DI
	 (match_operator 2 "signed_comparison_operator"
			 [(match_operand:DI 3 "reg_or_0_operand" "rJ,rJ,J,J")
			  (match_operand:DI 4 "reg_or_0_operand" "J,J,rJ,rJ")])
	 (match_operand:SI 1 "reg_or_8bit_operand" "rI,0,rI,0")
	 (match_operand:SI 5 "reg_or_8bit_operand" "0,rI,0,rI")))]
  "operands[3] == const0_rtx || operands[4] == const0_rtx"
  "@
   cmov%C2 %r3,%1,%0
   cmov%D2 %r3,%5,%0
   cmov%c2 %r4,%1,%0
   cmov%d2 %r4,%5,%0"
  [(set_attr "type" "cmov")])

(define_insn ""
  [(set (match_operand:DI 0 "register_operand" "=r,r,r,r")
	(if_then_else:DI
	 (match_operator 2 "signed_comparison_operator"
			 [(match_operand:DI 3 "reg_or_0_operand" "rJ,rJ,J,J")
			  (match_operand:DI 4 "reg_or_0_operand" "J,J,rJ,rJ")])
	 (match_operand:DI 1 "reg_or_8bit_operand" "rI,0,rI,0")
	 (match_operand:DI 5 "reg_or_8bit_operand" "0,rI,0,rI")))]
  "operands[3] == const0_rtx || operands[4] == const0_rtx"
  "@
   cmov%C2 %r3,%1,%0
   cmov%D2 %r3,%5,%0
   cmov%c2 %r4,%1,%0
   cmov%d2 %r4,%5,%0"
  [(set_attr "type" "cmov")])

(define_insn ""
  [(set (match_operand:DI 0 "register_operand" "=r,r")
	(if_then_else:DI
	 (eq (zero_extract:DI (match_operand:DI 2 "reg_or_0_operand" "rJ,rJ")
			      (const_int 1)
			      (const_int 0))
	     (const_int 0))
	 (match_operand:DI 1 "reg_or_8bit_operand" "rI,0")
	 (match_operand:DI 3 "reg_or_8bit_operand" "0,rI")))]
  ""
  "@
   cmovlbc %r2,%1,%0
   cmovlbs %r2,%3,%0"
  [(set_attr "type" "cmov")])

(define_insn ""
  [(set (match_operand:DI 0 "register_operand" "=r,r")
	(if_then_else:DI
	 (ne (zero_extract:DI (match_operand:DI 2 "reg_or_0_operand" "rJ,rJ")
			      (const_int 1)
			      (const_int 0))
	     (const_int 0))
	 (match_operand:DI 1 "reg_or_8bit_operand" "rI,0")
	 (match_operand:DI 3 "reg_or_8bit_operand" "0,rI")))]
  ""
  "@
   cmovlbs %r2,%1,%0
   cmovlbc %r2,%3,%0"
  [(set_attr "type" "cmov")])

;; This form is added since combine thinks that an IF_THEN_ELSE with both
;; arms constant is a single insn, so it won't try to form it if combine
;; knows they are really two insns.  This occurs in divides by powers
;; of two.

(define_insn ""
  [(set (match_operand:DI 0 "register_operand" "=r")
	(if_then_else:DI
	 (match_operator 2 "signed_comparison_operator"
			 [(match_operand:DI 3 "reg_or_0_operand" "rJ")
			  (const_int 0)])
	 (plus:DI (match_dup 0)
		  (match_operand:DI 1 "reg_or_8bit_operand" "rI"))
	 (match_dup 0)))
   (clobber (match_scratch:DI 4 "=&r"))]
  ""
  "addq %0,%1,%4\;cmov%C2 %r3,%4,%0"
  [(set_attr "type" "cmov")])

(define_split
  [(set (match_operand:DI 0 "register_operand" "")
	(if_then_else:DI
	 (match_operator 2 "signed_comparison_operator"
			 [(match_operand:DI 3 "reg_or_0_operand" "")
			  (const_int 0)])
	 (plus:DI (match_dup 0)
		  (match_operand:DI 1 "reg_or_8bit_operand" ""))
	 (match_dup 0)))
   (clobber (match_operand:DI 4 "register_operand" ""))]
  ""
  [(set (match_dup 4) (plus:DI (match_dup 0) (match_dup 1)))
   (set (match_dup 0) (if_then_else:DI (match_op_dup 2
						     [(match_dup 3)
						      (const_int 0)])
				       (match_dup 4) (match_dup 0)))]
  "")

(define_split
  [(parallel
    [(set (match_operand:DI 0 "register_operand" "")
	  (if_then_else:DI
	   (match_operator 1 "comparison_operator"
			   [(zero_extract:DI (match_operand:DI 2 "register_operand" "")
					     (const_int 1)
					     (match_operand:DI 3 "const_int_operand" ""))
			    (const_int 0)])
	   (match_operand:DI 4 "reg_or_8bit_operand" "")
	   (match_operand:DI 5 "reg_or_8bit_operand" "")))
     (clobber (match_operand:DI 6 "register_operand" ""))])]
  "INTVAL (operands[3]) != 0"
  [(set (match_dup 6)
	(lshiftrt:DI (match_dup 2) (match_dup 3)))
   (set (match_dup 0)
	(if_then_else:DI (match_op_dup 1
				       [(zero_extract:DI (match_dup 6)
							 (const_int 1)
							 (const_int 0))
					(const_int 0)])
			 (match_dup 4)
			 (match_dup 5)))]
  "")

;; For ABS, we have two choices, depending on whether the input and output
;; registers are the same or not.
(define_expand "absdi2"
  [(set (match_operand:DI 0 "register_operand" "")
	(abs:DI (match_operand:DI 1 "register_operand" "")))]
  ""
  "
{ if (rtx_equal_p (operands[0], operands[1]))
    emit_insn (gen_absdi2_same (operands[0], gen_reg_rtx (DImode)));
  else
    emit_insn (gen_absdi2_diff (operands[0], operands[1]));

  DONE;
}")

(define_expand "absdi2_same"
  [(set (match_operand:DI 1 "register_operand" "")
	(neg:DI (match_operand:DI 0 "register_operand" "")))
   (set (match_dup 0)
	(if_then_else:DI (ge (match_dup 0) (const_int 0))
			 (match_dup 0)
			 (match_dup 1)))]
  ""
  "")

(define_expand "absdi2_diff"
  [(set (match_operand:DI 0 "register_operand" "")
	(neg:DI (match_operand:DI 1 "register_operand" "")))
   (set (match_dup 0)
	(if_then_else:DI (lt (match_dup 1) (const_int 0))
			 (match_dup 0)
			 (match_dup 1)))]
  ""
  "")

(define_split
  [(set (match_operand:DI 0 "register_operand" "")
	(abs:DI (match_dup 0)))
   (clobber (match_operand:DI 2 "register_operand" ""))]
  ""
  [(set (match_dup 1) (neg:DI (match_dup 0)))
   (set (match_dup 0) (if_then_else:DI (ge (match_dup 0) (const_int 0))
				       (match_dup 0) (match_dup 1)))]
  "")

(define_split
  [(set (match_operand:DI 0 "register_operand" "")
	(abs:DI (match_operand:DI 1 "register_operand" "")))]
  "! rtx_equal_p (operands[0], operands[1])"
  [(set (match_dup 0) (neg:DI (match_dup 1)))
   (set (match_dup 0) (if_then_else:DI (lt (match_dup 1) (const_int 0))
				       (match_dup 0) (match_dup 1)))]
  "")

(define_split
  [(set (match_operand:DI 0 "register_operand" "")
	(neg:DI (abs:DI (match_dup 0))))
   (clobber (match_operand:DI 2 "register_operand" ""))]
  ""
  [(set (match_dup 1) (neg:DI (match_dup 0)))
   (set (match_dup 0) (if_then_else:DI (le (match_dup 0) (const_int 0))
				       (match_dup 0) (match_dup 1)))]
  "")

(define_split
  [(set (match_operand:DI 0 "register_operand" "")
	(neg:DI (abs:DI (match_operand:DI 1 "register_operand" ""))))]
  "! rtx_equal_p (operands[0], operands[1])"
  [(set (match_dup 0) (neg:DI (match_dup 1)))
   (set (match_dup 0) (if_then_else:DI (gt (match_dup 1) (const_int 0))
				       (match_dup 0) (match_dup 1)))]
  "")

(define_expand "smaxdi3"
  [(set (match_dup 3)
	(le:DI (match_operand:DI 1 "reg_or_0_operand" "")
	       (match_operand:DI 2 "reg_or_8bit_operand" "")))
   (set (match_operand:DI 0 "register_operand" "")
	(if_then_else:DI (eq (match_dup 3) (const_int 0))
			 (match_dup 1) (match_dup 2)))]
  ""
  "
{ operands[3] = gen_reg_rtx (DImode);
}")

(define_split
  [(set (match_operand:DI 0 "register_operand" "")
	(smax:DI (match_operand:DI 1 "reg_or_0_operand" "")
		 (match_operand:DI 2 "reg_or_8bit_operand" "")))
   (clobber (match_operand:DI 3 "register_operand" ""))]
  "operands[2] != const0_rtx"
  [(set (match_dup 3) (le:DI (match_dup 1) (match_dup 2)))
   (set (match_dup 0) (if_then_else:DI (eq (match_dup 3) (const_int 0))
				       (match_dup 1) (match_dup 2)))]
  "")

(define_insn ""
  [(set (match_operand:DI 0 "register_operand" "=r")
	(smax:DI (match_operand:DI 1 "register_operand" "0")
		 (const_int 0)))]
  ""
  "cmovlt %0,0,%0"
  [(set_attr "type" "cmov")])

(define_expand "smindi3"
  [(set (match_dup 3)
	(lt:DI (match_operand:DI 1 "reg_or_0_operand" "")
	       (match_operand:DI 2 "reg_or_8bit_operand" "")))
   (set (match_operand:DI 0 "register_operand" "")
	(if_then_else:DI (ne (match_dup 3) (const_int 0))
			 (match_dup 1) (match_dup 2)))]
  ""
  "
{ operands[3] = gen_reg_rtx (DImode);
}")

(define_split
  [(set (match_operand:DI 0 "register_operand" "")
	(smin:DI (match_operand:DI 1 "reg_or_0_operand" "")
		 (match_operand:DI 2 "reg_or_8bit_operand" "")))
   (clobber (match_operand:DI 3 "register_operand" ""))]
  "operands[2] != const0_rtx"
  [(set (match_dup 3) (lt:DI (match_dup 1) (match_dup 2)))
   (set (match_dup 0) (if_then_else:DI (ne (match_dup 3) (const_int 0))
				       (match_dup 1) (match_dup 2)))]
  "")

(define_insn ""
  [(set (match_operand:DI 0 "register_operand" "=r")
	(smin:DI (match_operand:DI 1 "register_operand" "0")
		 (const_int 0)))]
  ""
  "cmovgt %0,0,%0"
  [(set_attr "type" "cmov")])

(define_expand "umaxdi3"
  [(set (match_dup 3) 
	(leu:DI (match_operand:DI 1 "reg_or_0_operand" "")
		(match_operand:DI 2 "reg_or_8bit_operand" "")))
   (set (match_operand:DI 0 "register_operand" "")
	(if_then_else:DI (eq (match_dup 3) (const_int 0))
			 (match_dup 1) (match_dup 2)))]
  ""
  "
{ operands[3] = gen_reg_rtx (DImode);
}")

(define_split
  [(set (match_operand:DI 0 "register_operand" "")
	(umax:DI (match_operand:DI 1 "reg_or_0_operand" "")
		 (match_operand:DI 2 "reg_or_8bit_operand" "")))
   (clobber (match_operand:DI 3 "register_operand" ""))]
  "operands[2] != const0_rtx"
  [(set (match_dup 3) (leu:DI (match_dup 1) (match_dup 2)))
   (set (match_dup 0) (if_then_else:DI (eq (match_dup 3) (const_int 0))
				       (match_dup 1) (match_dup 2)))]
  "")

(define_expand "umindi3"
  [(set (match_dup 3)
	(ltu:DI (match_operand:DI 1 "reg_or_0_operand" "")
		(match_operand:DI 2 "reg_or_8bit_operand" "")))
   (set (match_operand:DI 0 "register_operand" "")
	(if_then_else:DI (ne (match_dup 3) (const_int 0))
			 (match_dup 1) (match_dup 2)))]
  ""
  "
{ operands[3] = gen_reg_rtx (DImode);
}")

(define_split
  [(set (match_operand:DI 0 "register_operand" "")
	(umin:DI (match_operand:DI 1 "reg_or_0_operand" "")
		 (match_operand:DI 2 "reg_or_8bit_operand" "")))
   (clobber (match_operand:DI 3 "register_operand" ""))]
  "operands[2] != const0_rtx"
  [(set (match_dup 3) (ltu:DI (match_dup 1) (match_dup 2)))
   (set (match_dup 0) (if_then_else:DI (ne (match_dup 3) (const_int 0))
				       (match_dup 1) (match_dup 2)))]
  "")

(define_insn ""
  [(set (pc)
	(if_then_else
	 (match_operator 1 "signed_comparison_operator"
			 [(match_operand:DI 2 "reg_or_0_operand" "rJ")
			  (const_int 0)])
	 (label_ref (match_operand 0 "" ""))
	 (pc)))]
  ""
  "b%C1 %r2,%0"
  [(set_attr "type" "ibr")])

(define_insn ""
  [(set (pc)
	(if_then_else
	 (match_operator 1 "signed_comparison_operator"
			 [(const_int 0)
			  (match_operand:DI 2 "register_operand" "r")])
	 (label_ref (match_operand 0 "" ""))
	 (pc)))]
  ""
  "b%c1 %2,%0"
  [(set_attr "type" "ibr")])

(define_insn ""
  [(set (pc)
	(if_then_else
	 (ne (zero_extract:DI (match_operand:DI 1 "reg_or_0_operand" "rJ")
			      (const_int 1)
			      (const_int 0))
	     (const_int 0))
	 (label_ref (match_operand 0 "" ""))
	 (pc)))]
  ""
  "blbs %r1,%0"
  [(set_attr "type" "ibr")])

(define_insn ""
  [(set (pc)
	(if_then_else
	 (eq (zero_extract:DI (match_operand:DI 1 "reg_or_0_operand" "rJ")
			      (const_int 1)
			      (const_int 0))
	     (const_int 0))
	 (label_ref (match_operand 0 "" ""))
	 (pc)))]
  ""
  "blbc %r1,%0"
  [(set_attr "type" "ibr")])

(define_split
  [(parallel
    [(set (pc)
	  (if_then_else
	   (match_operator 1 "comparison_operator"
			   [(zero_extract:DI (match_operand:DI 2 "register_operand" "")
					     (const_int 1)
					     (match_operand:DI 3 "const_int_operand" ""))
			    (const_int 0)])
	   (label_ref (match_operand 0 "" ""))
	   (pc)))
     (clobber (match_operand:DI 4 "register_operand" ""))])]
  "INTVAL (operands[3]) != 0"
  [(set (match_dup 4)
	(lshiftrt:DI (match_dup 2) (match_dup 3)))
   (set (pc)
	(if_then_else (match_op_dup 1
				    [(zero_extract:DI (match_dup 4)
						      (const_int 1)
						      (const_int 0))
				     (const_int 0)])
		      (label_ref (match_dup 0))
		      (pc)))]
  "")

;; The following are the corresponding floating-point insns.  Recall
;; we need to have variants that expand the arguments from SF mode
;; to DFmode.

(define_insn ""
  [(set (match_operand:DF 0 "register_operand" "=&f")
	(match_operator:DF 1 "alpha_comparison_operator"
			   [(match_operand:DF 2 "reg_or_fp0_operand" "fG")
			    (match_operand:DF 3 "reg_or_fp0_operand" "fG")]))]
  "TARGET_FP && alpha_tp == ALPHA_TP_INSN"
  "cmp%-%C1%' %R2,%R3,%0"
  [(set_attr "type" "fadd")
   (set_attr "trap" "yes")])

(define_insn ""
  [(set (match_operand:DF 0 "register_operand" "=f")
	(match_operator:DF 1 "alpha_comparison_operator"
			   [(match_operand:DF 2 "reg_or_fp0_operand" "fG")
			    (match_operand:DF 3 "reg_or_fp0_operand" "fG")]))]
  "TARGET_FP && alpha_tp != ALPHA_TP_INSN"
  "cmp%-%C1%' %R2,%R3,%0"
  [(set_attr "type" "fadd")
   (set_attr "trap" "yes")])

(define_insn ""
  [(set (match_operand:DF 0 "register_operand" "=f")
	(match_operator:DF 1 "alpha_comparison_operator"
			   [(float_extend:DF
			     (match_operand:SF 2 "reg_or_fp0_operand" "fG"))
			    (match_operand:DF 3 "reg_or_fp0_operand" "fG")]))]
  "TARGET_FP && alpha_tp != ALPHA_TP_INSN"
  "cmp%-%C1%' %R2,%R3,%0"
  [(set_attr "type" "fadd")
   (set_attr "trap" "yes")])

(define_insn ""
  [(set (match_operand:DF 0 "register_operand" "=f")
	(match_operator:DF 1 "alpha_comparison_operator"
			   [(match_operand:DF 2 "reg_or_fp0_operand" "fG")
			    (float_extend:DF
			     (match_operand:SF 3 "reg_or_fp0_operand" "fG"))]))]
  "TARGET_FP && alpha_tp != ALPHA_TP_INSN"
  "cmp%-%C1%' %R2,%R3,%0"
  [(set_attr "type" "fadd")
   (set_attr "trap" "yes")])

(define_insn ""
  [(set (match_operand:DF 0 "register_operand" "=f")
	(match_operator:DF 1 "alpha_comparison_operator"
			   [(float_extend:DF
			     (match_operand:SF 2 "reg_or_fp0_operand" "fG"))
			    (float_extend:DF
			     (match_operand:SF 3 "reg_or_fp0_operand" "fG"))]))]
  "TARGET_FP && alpha_tp != ALPHA_TP_INSN"
  "cmp%-%C1%' %R2,%R3,%0"
  [(set_attr "type" "fadd")
   (set_attr "trap" "yes")])

(define_insn ""
  [(set (match_operand:DF 0 "register_operand" "=&f,f")
	(if_then_else:DF 
	 (match_operator 3 "signed_comparison_operator"
			 [(match_operand:DF 4 "reg_or_fp0_operand" "fG,fG")
			  (match_operand:DF 2 "fp0_operand" "G,G")])
	 (match_operand:DF 1 "reg_or_fp0_operand" "fG,0")
	 (match_operand:DF 5 "reg_or_fp0_operand" "0,fG")))]
  "TARGET_FP && alpha_tp == ALPHA_TP_INSN"
  "@
   fcmov%C3 %R4,%R1,%0
   fcmov%D3 %R4,%R5,%0"
  [(set_attr "type" "fadd")])

(define_insn ""
  [(set (match_operand:DF 0 "register_operand" "=f,f")
	(if_then_else:DF 
	 (match_operator 3 "signed_comparison_operator"
			 [(match_operand:DF 4 "reg_or_fp0_operand" "fG,fG")
			  (match_operand:DF 2 "fp0_operand" "G,G")])
	 (match_operand:DF 1 "reg_or_fp0_operand" "fG,0")
	 (match_operand:DF 5 "reg_or_fp0_operand" "0,fG")))]
  "TARGET_FP && alpha_tp != ALPHA_TP_INSN"
  "@
   fcmov%C3 %R4,%R1,%0
   fcmov%D3 %R4,%R5,%0"
  [(set_attr "type" "fadd")])

(define_insn ""
  [(set (match_operand:SF 0 "register_operand" "=&f,f")
	(if_then_else:SF 
	 (match_operator 3 "signed_comparison_operator"
			 [(match_operand:DF 4 "reg_or_fp0_operand" "fG,fG")
			  (match_operand:DF 2 "fp0_operand" "G,G")])
	 (match_operand:SF 1 "reg_or_fp0_operand" "fG,0")
	 (match_operand:SF 5 "reg_or_fp0_operand" "0,fG")))]
  "TARGET_FP && alpha_tp == ALPHA_TP_INSN"
  "@
   fcmov%C3 %R4,%R1,%0
   fcmov%D3 %R4,%R5,%0"
  [(set_attr "type" "fadd")])

(define_insn ""
  [(set (match_operand:SF 0 "register_operand" "=f,f")
	(if_then_else:SF 
	 (match_operator 3 "signed_comparison_operator"
			 [(match_operand:DF 4 "reg_or_fp0_operand" "fG,fG")
			  (match_operand:DF 2 "fp0_operand" "G,G")])
	 (match_operand:SF 1 "reg_or_fp0_operand" "fG,0")
	 (match_operand:SF 5 "reg_or_fp0_operand" "0,fG")))]
  "TARGET_FP && alpha_tp != ALPHA_TP_INSN"
  "@
   fcmov%C3 %R4,%R1,%0
   fcmov%D3 %R4,%R5,%0"
  [(set_attr "type" "fadd")])

(define_insn ""
  [(set (match_operand:DF 0 "register_operand" "=f,f")
	(if_then_else:DF 
	 (match_operator 3 "signed_comparison_operator"
			 [(match_operand:DF 4 "reg_or_fp0_operand" "fG,fG")
			  (match_operand:DF 2 "fp0_operand" "G,G")])
	 (float_extend:DF (match_operand:SF 1 "reg_or_fp0_operand" "fG,0"))
	 (match_operand:DF 5 "reg_or_fp0_operand" "0,fG")))]
  "TARGET_FP && alpha_tp != ALPHA_TP_INSN"
  "@
   fcmov%C3 %R4,%R1,%0
   fcmov%D3 %R4,%R5,%0"
  [(set_attr "type" "fadd")])

(define_insn ""
  [(set (match_operand:DF 0 "register_operand" "=f,f")
	(if_then_else:DF 
	 (match_operator 3 "signed_comparison_operator"
			 [(float_extend:DF 
			   (match_operand:SF 4 "reg_or_fp0_operand" "fG,fG"))
			  (match_operand:DF 2 "fp0_operand" "G,G")])
	 (match_operand:DF 1 "reg_or_fp0_operand" "fG,0")
	 (match_operand:DF 5 "reg_or_fp0_operand" "0,fG")))]
  "TARGET_FP && alpha_tp != ALPHA_TP_INSN"
  "@
   fcmov%C3 %R4,%R1,%0
   fcmov%D3 %R4,%R5,%0"
  [(set_attr "type" "fadd")])

(define_insn ""
  [(set (match_operand:SF 0 "register_operand" "=f,f")
	(if_then_else:SF 
	 (match_operator 3 "signed_comparison_operator"
			 [(float_extend:DF
			   (match_operand:SF 4 "reg_or_fp0_operand" "fG,fG"))
			  (match_operand:DF 2 "fp0_operand" "G,G")])
	 (match_operand:SF 1 "reg_or_fp0_operand" "fG,0")
	 (match_operand:SF 5 "reg_or_fp0_operand" "0,fG")))]
  "TARGET_FP && alpha_tp != ALPHA_TP_INSN"
  "@
   fcmov%C3 %R4,%R1,%0
   fcmov%D3 %R4,%R5,%0"
  [(set_attr "type" "fadd")])

(define_insn ""
  [(set (match_operand:DF 0 "register_operand" "=f,f")
	(if_then_else:DF 
	 (match_operator 3 "signed_comparison_operator"
			 [(float_extend:DF
			   (match_operand:SF 4 "reg_or_fp0_operand" "fG,fG"))
			  (match_operand:DF 2 "fp0_operand" "G,G")])
	 (float_extend:DF (match_operand:SF 1 "reg_or_fp0_operand" "fG,0"))
	 (match_operand:DF 5 "reg_or_fp0_operand" "0,fG")))]
  "TARGET_FP && alpha_tp != ALPHA_TP_INSN"
  "@
   fcmov%C3 %R4,%R1,%0
   fcmov%D3 %R4,%R5,%0"
  [(set_attr "type" "fadd")])

(define_expand "maxdf3"
  [(set (match_dup 3)
	(le:DF (match_operand:DF 1 "reg_or_fp0_operand" "")
	       (match_operand:DF 2 "reg_or_fp0_operand" "")))
   (set (match_operand:DF 0 "register_operand" "")
	(if_then_else:DF (eq (match_dup 3) (match_dup 4))
			 (match_dup 1) (match_dup 2)))]
  "TARGET_FP"
  "
{ operands[3] = gen_reg_rtx (DFmode);
  operands[4] = CONST0_RTX (DFmode);
}")

(define_expand "mindf3"
  [(set (match_dup 3)
	(lt:DF (match_operand:DF 1 "reg_or_fp0_operand" "")
	       (match_operand:DF 2 "reg_or_fp0_operand" "")))
   (set (match_operand:DF 0 "register_operand" "")
	(if_then_else:DF (ne (match_dup 3) (match_dup 4))
			 (match_dup 1) (match_dup 2)))]
  "TARGET_FP"
  "
{ operands[3] = gen_reg_rtx (DFmode);
  operands[4] = CONST0_RTX (DFmode);
}")

(define_expand "maxsf3"
  [(set (match_dup 3)
	(le:DF (float_extend:DF (match_operand:SF 1 "reg_or_fp0_operand" ""))
	       (float_extend:DF (match_operand:SF 2 "reg_or_fp0_operand" ""))))
   (set (match_operand:SF 0 "register_operand" "")
	(if_then_else:SF (eq (match_dup 3) (match_dup 4))
			 (match_dup 1) (match_dup 2)))]
  "TARGET_FP"
  "
{ operands[3] = gen_reg_rtx (DFmode);
  operands[4] = CONST0_RTX (DFmode);
}")

(define_expand "minsf3"
  [(set (match_dup 3)
	(lt:DF (float_extend:DF (match_operand:SF 1 "reg_or_fp0_operand" ""))
	       (float_extend:DF (match_operand:SF 2 "reg_or_fp0_operand" ""))))
   (set (match_operand:SF 0 "register_operand" "")
	(if_then_else:SF (ne (match_dup 3) (match_dup 4))
		      (match_dup 1) (match_dup 2)))]
  "TARGET_FP"
  "
{ operands[3] = gen_reg_rtx (DFmode);
  operands[4] = CONST0_RTX (DFmode);
}")

(define_insn ""
  [(set (pc)
	(if_then_else
	 (match_operator 1 "signed_comparison_operator"
			 [(match_operand:DF 2 "reg_or_fp0_operand" "fG")
			  (match_operand:DF 3 "fp0_operand" "G")])
	 (label_ref (match_operand 0 "" ""))
	 (pc)))]
  "TARGET_FP"
  "fb%C1 %R2,%0"
  [(set_attr "type" "fbr")])

(define_insn ""
  [(set (pc)
	(if_then_else
	 (match_operator 1 "signed_comparison_operator"
			 [(float_extend:DF
			   (match_operand:SF 2 "reg_or_fp0_operand" "fG"))
			  (match_operand:DF 3 "fp0_operand" "G")])
	 (label_ref (match_operand 0 "" ""))
	 (pc)))]
  "TARGET_FP"
  "fb%C1 %R2,%0"
  [(set_attr "type" "fbr")])

;; These are the main define_expand's used to make conditional branches
;; and compares.

(define_expand "cmpdf"
  [(set (cc0) (compare (match_operand:DF 0 "reg_or_fp0_operand" "")
		       (match_operand:DF 1 "reg_or_fp0_operand" "")))]
  "TARGET_FP"
  "
{
  alpha_compare_op0 = operands[0];
  alpha_compare_op1 = operands[1];
  alpha_compare_fp_p = 1;
  DONE;
}")

(define_expand "cmpdi"
  [(set (cc0) (compare (match_operand:DI 0 "reg_or_0_operand" "")
		       (match_operand:DI 1 "reg_or_8bit_operand" "")))]
  ""
  "
{
  alpha_compare_op0 = operands[0];
  alpha_compare_op1 = operands[1];
  alpha_compare_fp_p = 0;
  DONE;
}")

(define_expand "beq"
  [(set (match_dup 1) (match_dup 2))
   (set (pc)
	(if_then_else (match_dup 3)
		      (label_ref (match_operand 0 "" ""))
		      (pc)))]
  ""
  "
{
  enum machine_mode mode;
  enum rtx_code compare_code = EQ, branch_code = NE;

  if (alpha_compare_fp_p)
    mode = DFmode;
  else
    {
      mode = DImode;
      /* We want to use cmpeq/bne when we can, since there is a zero-delay
	 bypass between logicals and br/cmov on the 21164.  But we don't
	 want to force valid immediate constants into registers needlessly.  */
      if (GET_CODE (alpha_compare_op1) == CONST_INT
	  && ((INTVAL (alpha_compare_op1) >= -0x8000
	       && INTVAL (alpha_compare_op1) < 0)
	      || (INTVAL (alpha_compare_op1) > 0xff
		  && INTVAL (alpha_compare_op1) < 0x8000)))
	{
	  compare_code = PLUS, branch_code = EQ;
	  alpha_compare_op1 = GEN_INT (- INTVAL (alpha_compare_op1));
	}
    }

  operands[1] = gen_reg_rtx (mode);
  operands[2] = gen_rtx (compare_code, mode,
			 alpha_compare_op0, alpha_compare_op1);
  operands[3] = gen_rtx (branch_code, VOIDmode,
			 operands[1], CONST0_RTX (mode));
}")

(define_expand "bne"
  [(set (match_dup 1) (match_dup 2))
   (set (pc)
	(if_then_else (match_dup 3)
		      (label_ref (match_operand 0 "" ""))
		      (pc)))]
  ""
  "
{
  enum machine_mode mode;
  enum rtx_code compare_code = EQ, branch_code = EQ;

  if (alpha_compare_fp_p)
    mode = DFmode;
  else
    {
      mode = DImode;
      /* We want to use cmpeq/bne when we can, since there is a zero-delay
	 bypass between logicals and br/cmov on the 21164.  But we don't
	 want to force valid immediate constants into registers needlessly.  */
      if (GET_CODE (alpha_compare_op1) == CONST_INT
	  && ((INTVAL (alpha_compare_op1) >= -0x8000
	       && INTVAL (alpha_compare_op1) < 0)
	      || (INTVAL (alpha_compare_op1) > 0xff
		  && INTVAL (alpha_compare_op1) < 0x8000)))
	{
	  compare_code = PLUS, branch_code = NE;
	  alpha_compare_op1 = GEN_INT (- INTVAL (alpha_compare_op1));
	}
    }

  operands[1] = gen_reg_rtx (mode);
  operands[2] = gen_rtx (compare_code, mode,
			 alpha_compare_op0, alpha_compare_op1);
  operands[3] = gen_rtx (branch_code, VOIDmode,
			 operands[1], CONST0_RTX (mode));
}")

(define_expand "blt"
  [(set (match_dup 1) (match_dup 2))
   (set (pc)
	(if_then_else (match_dup 3)
		      (label_ref (match_operand 0 "" ""))
		      (pc)))]
  ""
  "
{
  enum machine_mode mode = alpha_compare_fp_p ? DFmode : DImode;
  operands[1] = gen_reg_rtx (mode);
  operands[2] = gen_rtx (LT, mode, alpha_compare_op0, alpha_compare_op1);
  operands[3] = gen_rtx (NE, VOIDmode, operands[1], CONST0_RTX (mode));
}")

(define_expand "ble"
  [(set (match_dup 1) (match_dup 2))
   (set (pc)
	(if_then_else (match_dup 3)
		      (label_ref (match_operand 0 "" ""))
		      (pc)))]
  ""
  "
{
  enum machine_mode mode = alpha_compare_fp_p ? DFmode : DImode;
  operands[1] = gen_reg_rtx (mode);
  operands[2] = gen_rtx (LE, mode, alpha_compare_op0, alpha_compare_op1);
  operands[3] = gen_rtx (NE, VOIDmode, operands[1], CONST0_RTX (mode));
}")

(define_expand "bgt"
  [(set (match_dup 1) (match_dup 2))
   (set (pc)
	(if_then_else (match_dup 3)
		      (label_ref (match_operand 0 "" ""))
		      (pc)))]
  ""
  "
{
  if (alpha_compare_fp_p)
    {
      operands[1] = gen_reg_rtx (DFmode);
      operands[2] = gen_rtx (LT, DFmode, alpha_compare_op1, alpha_compare_op0);
      operands[3] = gen_rtx (NE, VOIDmode, operands[1], CONST0_RTX (DFmode));
    }
  else
    {
      operands[1] = gen_reg_rtx (DImode);
      operands[2] = gen_rtx (LE, DImode, alpha_compare_op0, alpha_compare_op1);
      operands[3] = gen_rtx (EQ, VOIDmode, operands[1], const0_rtx);
    }
}")

(define_expand "bge"
  [(set (match_dup 1) (match_dup 2))
   (set (pc)
	(if_then_else (match_dup 3)
		      (label_ref (match_operand 0 "" ""))
		      (pc)))]
  ""
  "
{
  if (alpha_compare_fp_p)
    {
      operands[1] = gen_reg_rtx (DFmode);
      operands[2] = gen_rtx (LE, DFmode, alpha_compare_op1, alpha_compare_op0);
      operands[3] = gen_rtx (NE, VOIDmode, operands[1], CONST0_RTX (DFmode));
    }
  else
    {
      operands[1] = gen_reg_rtx (DImode);
      operands[2] = gen_rtx (LT, DImode, alpha_compare_op0, alpha_compare_op1);
      operands[3] = gen_rtx (EQ, VOIDmode, operands[1], const0_rtx);
    }
}")

(define_expand "bltu"
  [(set (match_dup 1) (match_dup 2))
   (set (pc)
	(if_then_else (match_dup 3)
		      (label_ref (match_operand 0 "" ""))
		      (pc)))]
  ""
  "
{
  operands[1] = gen_reg_rtx (DImode);
  operands[2] = gen_rtx (LTU, DImode, alpha_compare_op0, alpha_compare_op1);
  operands[3] = gen_rtx (NE, VOIDmode, operands[1], const0_rtx);
}")

(define_expand "bleu"
  [(set (match_dup 1) (match_dup 2))
   (set (pc)
	(if_then_else (match_dup 3)
		      (label_ref (match_operand 0 "" ""))
		      (pc)))]
  ""
  "
{
  operands[1] = gen_reg_rtx (DImode);
  operands[2] = gen_rtx (LEU, DImode, alpha_compare_op0, alpha_compare_op1);
  operands[3] = gen_rtx (NE, VOIDmode, operands[1], const0_rtx);
}")

(define_expand "bgtu"
  [(set (match_dup 1) (match_dup 2))
   (set (pc)
	(if_then_else (match_dup 3)
		      (label_ref (match_operand 0 "" ""))
		      (pc)))]
  ""
  "
{
  operands[1] = gen_reg_rtx (DImode);
  operands[2] = gen_rtx (LEU, DImode, alpha_compare_op0, alpha_compare_op1);
  operands[3] = gen_rtx (EQ, VOIDmode, operands[1], const0_rtx);
}")

(define_expand "bgeu"
  [(set (match_dup 1) (match_dup 2))
   (set (pc)
	(if_then_else (match_dup 3)
		      (label_ref (match_operand 0 "" ""))
		      (pc)))]
  ""
  "
{
  operands[1] = gen_reg_rtx (DImode);
  operands[2] = gen_rtx (LTU, DImode, alpha_compare_op0, alpha_compare_op1);
  operands[3] = gen_rtx (EQ, VOIDmode, operands[1], const0_rtx);
}")

(define_expand "seq"
  [(set (match_operand:DI 0 "register_operand" "")
	(match_dup 1))]
  ""
  "
{
  if (alpha_compare_fp_p)
    FAIL;

  operands[1] = gen_rtx (EQ, DImode, alpha_compare_op0, alpha_compare_op1);
}")

(define_expand "sne"
  [(set (match_operand:DI 0 "register_operand" "")
	(match_dup 1))
   (set (match_dup 0) (xor:DI (match_dup 0) (const_int 1)))]
  ""
  "
{
  if (alpha_compare_fp_p)
    FAIL;

  operands[1] = gen_rtx (EQ, DImode, alpha_compare_op0, alpha_compare_op1);
}")

(define_expand "slt"
  [(set (match_operand:DI 0 "register_operand" "")
	(match_dup 1))]
  ""
  "
{
  if (alpha_compare_fp_p)
    FAIL;

  operands[1] = gen_rtx (LT, DImode, alpha_compare_op0, alpha_compare_op1);
}")

(define_expand "sle"
  [(set (match_operand:DI 0 "register_operand" "")
	(match_dup 1))]
  ""
  "
{
  if (alpha_compare_fp_p)
    FAIL;

  operands[1] = gen_rtx (LE, DImode, alpha_compare_op0, alpha_compare_op1);
}")

(define_expand "sgt"
  [(set (match_operand:DI 0 "register_operand" "")
	(match_dup 1))]
  ""
  "
{
  if (alpha_compare_fp_p)
    FAIL;

  operands[1] = gen_rtx (LT, DImode, force_reg (DImode, alpha_compare_op1),
			 alpha_compare_op0);
}")

(define_expand "sge"
  [(set (match_operand:DI 0 "register_operand" "")
	(match_dup 1))]
  ""
  "
{
  if (alpha_compare_fp_p)
    FAIL;

  operands[1] = gen_rtx (LE, DImode, force_reg (DImode, alpha_compare_op1),
			 alpha_compare_op0);
}")

(define_expand "sltu"
  [(set (match_operand:DI 0 "register_operand" "")
	(match_dup 1))]
  ""
  "
{
  if (alpha_compare_fp_p)
    FAIL;

  operands[1] = gen_rtx (LTU, DImode, alpha_compare_op0, alpha_compare_op1);
}")

(define_expand "sleu"
  [(set (match_operand:DI 0 "register_operand" "")
	(match_dup 1))]
  ""
  "
{
  if (alpha_compare_fp_p)
    FAIL;

  operands[1] = gen_rtx (LEU, DImode, alpha_compare_op0, alpha_compare_op1);
}")

(define_expand "sgtu"
  [(set (match_operand:DI 0 "register_operand" "")
	(match_dup 1))]
  ""
  "
{
  if (alpha_compare_fp_p)
    FAIL;

  operands[1] = gen_rtx (LTU, DImode, force_reg (DImode, alpha_compare_op1),
			 alpha_compare_op0);
}")

(define_expand "sgeu"
  [(set (match_operand:DI 0 "register_operand" "")
	(match_dup 1))]
  ""
  "
{
  if (alpha_compare_fp_p)
    FAIL;

  operands[1] = gen_rtx (LEU, DImode, force_reg (DImode, alpha_compare_op1),
			 alpha_compare_op0);
}")

;; These are the main define_expand's used to make conditional moves.

(define_expand "movsicc"
  [(set (match_operand:SI 0 "register_operand" "")
	(if_then_else:DI (match_operand 1 "comparison_operator" "")
			 (match_operand:SI 2 "reg_or_8bit_operand" "")
			 (match_operand:SI 3 "reg_or_8bit_operand" "")))]
  ""
  "
{
  if ((operands[1] = alpha_emit_conditional_move (operands[1], SImode)) == 0)
    FAIL;
}")

(define_expand "movdicc"
  [(set (match_operand:DI 0 "register_operand" "")
	(if_then_else:DI (match_operand 1 "comparison_operator" "")
			 (match_operand:DI 2 "reg_or_8bit_operand" "")
			 (match_operand:DI 3 "reg_or_8bit_operand" "")))]
  ""
  "
{
  if ((operands[1] = alpha_emit_conditional_move (operands[1], DImode)) == 0)
    FAIL;
}")

(define_expand "movsfcc"
  [(set (match_operand:SF 0 "register_operand" "")
	(if_then_else:SF (match_operand 1 "comparison_operator" "")
			 (match_operand:SF 2 "reg_or_8bit_operand" "")
			 (match_operand:SF 3 "reg_or_8bit_operand" "")))]
  ""
  "
{
  if ((operands[1] = alpha_emit_conditional_move (operands[1], SFmode)) == 0)
    FAIL;
}")

(define_expand "movdfcc"
  [(set (match_operand:DF 0 "register_operand" "")
	(if_then_else:DF (match_operand 1 "comparison_operator" "")
			 (match_operand:DF 2 "reg_or_8bit_operand" "")
			 (match_operand:DF 3 "reg_or_8bit_operand" "")))]
  ""
  "
{
  if ((operands[1] = alpha_emit_conditional_move (operands[1], DFmode)) == 0)
    FAIL;
}")

;; These define_split definitions are used in cases when comparisons have
;; not be stated in the correct way and we need to reverse the second
;; comparison.  For example, x >= 7 has to be done as x < 6 with the
;; comparison that tests the result being reversed.  We have one define_split
;; for each use of a comparison.  They do not match valid insns and need
;; not generate valid insns.
;;
;; We can also handle equality comparisons (and inequality comparisons in
;; cases where the resulting add cannot overflow) by doing an add followed by
;; a comparison with zero.  This is faster since the addition takes one
;; less cycle than a compare when feeding into a conditional move.
;; For this case, we also have an SImode pattern since we can merge the add
;; and sign extend and the order doesn't matter.
;;
;; We do not do this for floating-point, since it isn't clear how the "wrong"
;; operation could have been generated.

(define_split
  [(set (match_operand:DI 0 "register_operand" "")
	(if_then_else:DI
	 (match_operator 1 "comparison_operator"
			 [(match_operand:DI 2 "reg_or_0_operand" "")
			  (match_operand:DI 3 "reg_or_cint_operand" "")])
	 (match_operand:DI 4 "reg_or_cint_operand" "")
	 (match_operand:DI 5 "reg_or_cint_operand" "")))
   (clobber (match_operand:DI 6 "register_operand" ""))]
  "operands[3] != const0_rtx"
  [(set (match_dup 6) (match_dup 7))
   (set (match_dup 0)
	(if_then_else:DI (match_dup 8) (match_dup 4) (match_dup 5)))]
  "
{ enum rtx_code code = GET_CODE (operands[1]);
  int unsignedp = (code == GEU || code == LEU || code == GTU || code == LTU);

  /* If we are comparing for equality with a constant and that constant
     appears in the arm when the register equals the constant, use the
     register since that is more likely to match (and to produce better code
     if both would).  */

  if (code == EQ && GET_CODE (operands[3]) == CONST_INT
      && rtx_equal_p (operands[4], operands[3]))
    operands[4] = operands[2];

  else if (code == NE && GET_CODE (operands[3]) == CONST_INT
	   && rtx_equal_p (operands[5], operands[3]))
    operands[5] = operands[2];

  if (code == NE || code == EQ
      || (extended_count (operands[2], DImode, unsignedp) >= 1
	  && extended_count (operands[3], DImode, unsignedp) >= 1))
    {
      if (GET_CODE (operands[3]) == CONST_INT)
	operands[7] = gen_rtx (PLUS, DImode, operands[2],
			       GEN_INT (- INTVAL (operands[3])));
      else
	operands[7] = gen_rtx (MINUS, DImode, operands[2], operands[3]);

      operands[8] = gen_rtx (code, VOIDmode, operands[6], const0_rtx);
    }

  else if (code == EQ || code == LE || code == LT
	   || code == LEU || code == LTU)
    {
      operands[7] = gen_rtx (code, DImode, operands[2], operands[3]);
      operands[8] = gen_rtx (NE, VOIDmode, operands[6], const0_rtx);
    }
  else
    {
      operands[7] = gen_rtx (reverse_condition (code), DImode, operands[2],
			     operands[3]);
      operands[8] = gen_rtx (EQ, VOIDmode, operands[6], const0_rtx);
    }
}")

(define_split
  [(set (match_operand:DI 0 "register_operand" "")
	(if_then_else:DI
	 (match_operator 1 "comparison_operator"
			 [(match_operand:SI 2 "reg_or_0_operand" "")
			  (match_operand:SI 3 "reg_or_cint_operand" "")])
	 (match_operand:DI 4 "reg_or_8bit_operand" "")
	 (match_operand:DI 5 "reg_or_8bit_operand" "")))
   (clobber (match_operand:DI 6 "register_operand" ""))]
  "operands[3] != const0_rtx
   && (GET_CODE (operands[1]) == EQ || GET_CODE (operands[1]) == NE)"
  [(set (match_dup 6) (match_dup 7))
   (set (match_dup 0)
	(if_then_else:DI (match_dup 8) (match_dup 4) (match_dup 5)))]
  "
{ enum rtx_code code = GET_CODE (operands[1]);
  int unsignedp = (code == GEU || code == LEU || code == GTU || code == LTU);
  rtx tem;

  if ((code != NE && code != EQ
       && ! (extended_count (operands[2], DImode, unsignedp) >= 1
	     && extended_count (operands[3], DImode, unsignedp) >= 1)))
    FAIL;
 
  if (GET_CODE (operands[3]) == CONST_INT)
    tem = gen_rtx (PLUS, SImode, operands[2],
		   GEN_INT (- INTVAL (operands[3])));
  else
    tem = gen_rtx (MINUS, SImode, operands[2], operands[3]);

  operands[7] = gen_rtx (SIGN_EXTEND, DImode, tem);
  operands[8] = gen_rtx (GET_CODE (operands[1]), VOIDmode, operands[6],
			 const0_rtx);
}")

(define_split
  [(set (pc)
	(if_then_else
	 (match_operator 1 "comparison_operator"
			 [(match_operand:DI 2 "reg_or_0_operand" "")
			  (match_operand:DI 3 "reg_or_cint_operand" "")])
	 (label_ref (match_operand 0 "" ""))
	 (pc)))
   (clobber (match_operand:DI 4 "register_operand" ""))]
  "operands[3] != const0_rtx"
  [(set (match_dup 4) (match_dup 5))
   (set (pc) (if_then_else (match_dup 6) (label_ref (match_dup 0)) (pc)))]
  "
{ enum rtx_code code = GET_CODE (operands[1]);
  int unsignedp = (code == GEU || code == LEU || code == GTU || code == LTU);

  if (code == NE || code == EQ
      || (extended_count (operands[2], DImode, unsignedp) >= 1
	  && extended_count (operands[3], DImode, unsignedp) >= 1))
    {
      if (GET_CODE (operands[3]) == CONST_INT)
	operands[5] = gen_rtx (PLUS, DImode, operands[2],
			       GEN_INT (- INTVAL (operands[3])));
      else
	operands[5] = gen_rtx (MINUS, DImode, operands[2], operands[3]);

      operands[6] = gen_rtx (code, VOIDmode, operands[4], const0_rtx);
    }

  else if (code == EQ || code == LE || code == LT
	   || code == LEU || code == LTU)
    {
      operands[5] = gen_rtx (code, DImode, operands[2], operands[3]);
      operands[6] = gen_rtx (NE, VOIDmode, operands[4], const0_rtx);
    }
  else
    {
      operands[5] = gen_rtx (reverse_condition (code), DImode, operands[2],
			     operands[3]);
      operands[6] = gen_rtx (EQ, VOIDmode, operands[4], const0_rtx);
    }
}")

(define_split
  [(set (pc)
	(if_then_else
	 (match_operator 1 "comparison_operator"
			 [(match_operand:SI 2 "reg_or_0_operand" "")
			  (match_operand:SI 3 "const_int_operand" "")])
	 (label_ref (match_operand 0 "" ""))
	 (pc)))
   (clobber (match_operand:DI 4 "register_operand" ""))]
  "operands[3] != const0_rtx
   && (GET_CODE (operands[1]) == EQ || GET_CODE (operands[1]) == NE)"
  [(set (match_dup 4) (match_dup 5))
   (set (pc) (if_then_else (match_dup 6) (label_ref (match_dup 0)) (pc)))]
  "
{ rtx tem;

  if (GET_CODE (operands[3]) == CONST_INT)
    tem = gen_rtx (PLUS, SImode, operands[2],
		   GEN_INT (- INTVAL (operands[3])));
  else
    tem = gen_rtx (MINUS, SImode, operands[2], operands[3]);
  
  operands[5] = gen_rtx (SIGN_EXTEND, DImode, tem);
  operands[6] = gen_rtx (GET_CODE (operands[1]), VOIDmode,
			 operands[4], const0_rtx);
}")

;; We can convert such things as "a > 0xffff" to "t = a & ~ 0xffff; t != 0".
;; This eliminates one, and sometimes two, insns when the AND can be done
;; with a ZAP.
(define_split
  [(set (match_operand:DI 0 "register_operand" "")
	(match_operator 1 "comparison_operator"
			[(match_operand:DI 2 "register_operand" "")
			 (match_operand:DI 3 "const_int_operand" "")]))
   (clobber (match_operand:DI 4 "register_operand" ""))]
  "exact_log2 (INTVAL (operands[3]) + 1) >= 0
   && (GET_CODE (operands[1]) == GTU
       || GET_CODE (operands[1]) == LEU
       || ((GET_CODE (operands[1]) == GT || GET_CODE (operands[1]) == LE)
	   && extended_count (operands[2], DImode, 1) > 0))"
  [(set (match_dup 4) (and:DI (match_dup 2) (match_dup 5)))
   (set (match_dup 0) (match_dup 6))]
  "
{
  operands[5] = GEN_INT (~ INTVAL (operands[3]));
  operands[6] = gen_rtx (((GET_CODE (operands[1]) == GTU
			   || GET_CODE (operands[1]) == GT)
			  ? NE : EQ),
			 DImode, operands[4], const0_rtx);
}")

;; Here are the CALL and unconditional branch insns.  Calls on NT and OSF
;; work differently, so we have different patterns for each.

(define_expand "call"
  [(use (match_operand:DI 0 "" ""))
   (use (match_operand 1 "" ""))
   (use (match_operand 2 "" ""))
   (use (match_operand 3 "" ""))]
  ""
  "
{ if (TARGET_WINDOWS_NT)
    emit_call_insn (gen_call_nt (operands[0], operands[1]));
  else if (TARGET_OPEN_VMS)
    emit_call_insn (gen_call_vms (operands[0], operands[2]));
  else
    emit_call_insn (gen_call_osf (operands[0], operands[1]));

  DONE;
}")

(define_expand "call_osf"
  [(parallel [(call (mem:DI (match_operand 0 "" ""))
		    (match_operand 1 "" ""))
	      (clobber (reg:DI 27))
	      (clobber (reg:DI 26))])]
  ""
  "
{ if (GET_CODE (operands[0]) != MEM)
    abort ();

  operands[0] = XEXP (operands[0], 0);

  if (GET_CODE (operands[0]) != SYMBOL_REF
      && ! (GET_CODE (operands[0]) == REG && REGNO (operands[0]) == 27))
    {
      rtx tem = gen_rtx (REG, DImode, 27);
      emit_move_insn (tem, operands[0]);
      operands[0] = tem;
    }
}")

(define_expand "call_nt"
  [(parallel [(call (mem:DI (match_operand 0 "" ""))
		    (match_operand 1 "" ""))
	      (clobber (reg:DI 26))])]
  ""
  "
{ if (GET_CODE (operands[0]) != MEM)
    abort ();

  operands[0] = XEXP (operands[0], 0);
  if (GET_CODE (operands[0]) != SYMBOL_REF && GET_CODE (operands[0]) != REG)
    operands[0] = force_reg (DImode, operands[0]);
}")

;;
;; call openvms/alpha
;; op 0: symbol ref for called function
;; op 1: next_arg_reg (argument information value for R25)
;;
(define_expand "call_vms"
  [(parallel [(call (mem:DI (match_operand 0 "" ""))
		    (match_operand 1 "" ""))
	      (use (match_dup 2))
	      (use (reg:DI 25))
	      (use (reg:DI 26))
	      (clobber (reg:DI 27))])]
  ""
  "
{ if (GET_CODE (operands[0]) != MEM)
    abort ();

  operands[0] = XEXP (operands[0], 0);

  /* Always load AI with argument information, then handle symbolic and
     indirect call differently.  Load RA and set operands[2] to PV in
     both cases.  */

  emit_move_insn (gen_rtx (REG, DImode, 25), operands[1]);
  if (GET_CODE (operands[0]) == SYMBOL_REF)
    {
      extern char *savealloc ();
      char *symbol = XSTR (operands[0], 0);
      char *linksym = savealloc (strlen (symbol) + 6);
      rtx linkage;

      alpha_need_linkage (symbol, 0);

      linksym[0] = '$';
      strcpy (linksym+1, symbol);
      strcat (linksym, \"..lk\");
      linkage = gen_rtx (SYMBOL_REF, Pmode, linksym);

      emit_move_insn (gen_rtx (REG, Pmode, 26), gen_rtx (MEM, Pmode, linkage));

      operands[2]
	= validize_mem (gen_rtx (MEM, Pmode, plus_constant (linkage, 8)));
    }
  else
    {
      emit_move_insn (gen_rtx (REG, Pmode, 26),
		      gen_rtx (MEM, Pmode, plus_constant (operands[0], 8)));

      operands[2] = operands[0];
    }

}")

(define_expand "call_value"
  [(use (match_operand 0 "" ""))
   (use (match_operand:DI 1 "" ""))
   (use (match_operand 2 "" ""))
   (use (match_operand 3 "" ""))
   (use (match_operand 4 "" ""))]
  ""
  "
{ if (TARGET_WINDOWS_NT)
    emit_call_insn (gen_call_value_nt (operands[0], operands[1], operands[2]));
  else if (TARGET_OPEN_VMS)
    emit_call_insn (gen_call_value_vms (operands[0], operands[1],
					operands[3]));
  else
    emit_call_insn (gen_call_value_osf (operands[0], operands[1],
					operands[2]));
  DONE;
}")

(define_expand "call_value_osf"
  [(parallel [(set (match_operand 0 "" "")
		   (call (mem:DI (match_operand 1 "" ""))
			 (match_operand 2 "" "")))
	      (clobber (reg:DI 27))
	      (clobber (reg:DI 26))])]
  ""
  "
{ if (GET_CODE (operands[1]) != MEM)
    abort ();

  operands[1] = XEXP (operands[1], 0);

  if (GET_CODE (operands[1]) != SYMBOL_REF
      && ! (GET_CODE (operands[1]) == REG && REGNO (operands[1]) == 27))
    {
      rtx tem = gen_rtx (REG, DImode, 27);
      emit_move_insn (tem, operands[1]);
      operands[1] = tem;
    }
}")

(define_expand "call_value_nt"
  [(parallel [(set (match_operand 0 "" "")
		   (call (mem:DI (match_operand 1 "" ""))
			 (match_operand 2 "" "")))
	      (clobber (reg:DI 26))])]
  ""
  "
{ if (GET_CODE (operands[1]) != MEM)
    abort ();

  operands[1] = XEXP (operands[1], 0);
  if (GET_CODE (operands[0]) != SYMBOL_REF && GET_CODE (operands[0]) != REG)
    operands[1] = force_reg (DImode, operands[1]);
}")

(define_expand "call_value_vms"
  [(parallel [(set (match_operand 0 "" "")
		   (call (mem:DI (match_operand:DI 1 "" ""))
			 (match_operand 2 "" "")))
	      (use (match_dup 3))
	      (use (reg:DI 25))
	      (use (reg:DI 26))
	      (clobber (reg:DI 27))])]
  ""
  "
{ if (GET_CODE (operands[1]) != MEM)
    abort ();

  operands[1] = XEXP (operands[1], 0);

  /* Always load AI with argument information, then handle symbolic and
     indirect call differently.  Load RA and set operands[3] to PV in
     both cases.  */

  emit_move_insn (gen_rtx (REG, DImode, 25), operands[2]);
  if (GET_CODE (operands[1]) == SYMBOL_REF)
    {
      extern char *savealloc ();
      char *symbol = XSTR (operands[1], 0);
      char *linksym = savealloc (strlen (symbol) + 6);
      rtx linkage;

      alpha_need_linkage (symbol, 0);
      linksym[0] = '$';
      strcpy (linksym+1, symbol);
      strcat (linksym, \"..lk\");
      linkage = gen_rtx (SYMBOL_REF, Pmode, linksym);

      emit_move_insn (gen_rtx (REG, Pmode, 26), gen_rtx (MEM, Pmode, linkage));

      operands[3]
	= validize_mem (gen_rtx (MEM, Pmode, plus_constant (linkage, 8)));
    }
  else
    {
      emit_move_insn (gen_rtx (REG, Pmode, 26),
		      gen_rtx (MEM, Pmode, plus_constant (operands[1], 8)));

      operands[3] = operands[1];
    }
}")

(define_insn ""
  [(call (mem:DI (match_operand:DI 0 "call_operand" "r,R,i"))
	 (match_operand 1 "" ""))
   (clobber (reg:DI 27))
   (clobber (reg:DI 26))]
  "! TARGET_WINDOWS_NT && ! TARGET_OPEN_VMS"
  "@
   jsr $26,($27),0\;ldgp $29,0($26)
   bsr $26,$%0..ng
   jsr $26,%0\;ldgp $29,0($26)"
  [(set_attr "type" "jsr")])
      
(define_insn ""
  [(call (mem:DI (match_operand:DI 0 "call_operand" "r,R,i"))
	 (match_operand 1 "" ""))
   (clobber (reg:DI 26))]
  "TARGET_WINDOWS_NT"
  "@
   jsr $26,(%0)
   bsr $26,%0
   jsr $26,%0"
  [(set_attr "type" "jsr")])
      
(define_insn ""
  [(call (mem:DI (match_operand:DI 0 "call_operand" "r,i"))
	 (match_operand 1 "" ""))
   (use (match_operand:DI 2 "general_operand" "r,m"))
   (use (reg:DI 25))
   (use (reg:DI 26))
   (clobber (reg:DI 27))]
  "TARGET_OPEN_VMS"
  "@
   bis %2,%2,$27\;jsr $26,0\;ldq $27,0($29)
   ldq $27,%2\;jsr $26,%0\;ldq $27,0($29)"
  [(set_attr "type" "jsr")])

(define_insn ""
  [(set (match_operand 0 "register_operand" "=rf,rf,rf")
	(call (mem:DI (match_operand:DI 1 "call_operand" "r,R,i"))
	      (match_operand 2 "" "")))
   (clobber (reg:DI 27))
   (clobber (reg:DI 26))]
  "! TARGET_WINDOWS_NT && ! TARGET_OPEN_VMS"
  "@
   jsr $26,($27),0\;ldgp $29,0($26)
   bsr $26,$%1..ng
   jsr $26,%1\;ldgp $29,0($26)"
  [(set_attr "type" "jsr")])

(define_insn ""
  [(set (match_operand 0 "register_operand" "=rf,rf,rf")
	(call (mem:DI (match_operand:DI 1 "call_operand" "r,R,i"))
	      (match_operand 2 "" "")))
   (clobber (reg:DI 26))]
  "TARGET_WINDOWS_NT"
  "@
   jsr $26,(%1)
   bsr $26,%1
   jsr $26,%1"
  [(set_attr "type" "jsr")])

(define_insn ""
  [(set (match_operand 0 "register_operand" "")
	(call (mem:DI (match_operand:DI 1 "call_operand" "r,i"))
	      (match_operand 2 "" "")))
   (use (match_operand:DI 3 "general_operand" "r,m"))
   (use (reg:DI 25))
   (use (reg:DI 26))
   (clobber (reg:DI 27))]
  "TARGET_OPEN_VMS"
  "@
   bis %3,%3,$27\;jsr $26,0\;ldq $27,0($29)
   ldq $27,%3\;jsr $26,%1\;ldq $27,0($29)"
  [(set_attr "type" "jsr")])

;; Call subroutine returning any type.

(define_expand "untyped_call"
  [(parallel [(call (match_operand 0 "" "")
		    (const_int 0))
	      (match_operand 1 "" "")
	      (match_operand 2 "" "")])]
  ""
  "
{
  int i;

  emit_call_insn (gen_call (operands[0], const0_rtx, NULL, const0_rtx));

  for (i = 0; i < XVECLEN (operands[2], 0); i++)
    {
      rtx set = XVECEXP (operands[2], 0, i);
      emit_move_insn (SET_DEST (set), SET_SRC (set));
    }

  /* The optimizer does not know that the call sets the function value
     registers we stored in the result block.  We avoid problems by
     claiming that all hard registers are used and clobbered at this
     point.  */
  emit_insn (gen_blockage ());

  DONE;
}")

;; UNSPEC_VOLATILE is considered to use and clobber all hard registers and
;; all of memory.  This blocks insns from being moved across this point.

(define_insn "blockage"
  [(unspec_volatile [(const_int 0)] 1)]
  ""
  "")

(define_insn "jump"
  [(set (pc)
	(label_ref (match_operand 0 "" "")))]
  ""
  "br $31,%l0"
  [(set_attr "type" "ibr")])

(define_insn "return"
  [(return)]
  "direct_return ()"
  "ret $31,($26),1"
  [(set_attr "type" "ibr")])

(define_insn "indirect_jump"
  [(set (pc) (match_operand:DI 0 "register_operand" "r"))]
  ""
  "jmp $31,(%0),0"
  [(set_attr "type" "ibr")])

(define_insn "nop"
  [(const_int 0)]
  ""
  "bis $31,$31,$31"
  [(set_attr "type" "ilog")])

(define_expand "tablejump"
  [(use (match_operand:SI 0 "register_operand" ""))
   (use (match_operand:SI 1 "" ""))]
  ""
  "
{
  if (TARGET_WINDOWS_NT)
    emit_jump_insn (gen_tablejump_nt (operands[0], operands[1]));
  else if (TARGET_OPEN_VMS)
    emit_jump_insn (gen_tablejump_vms (operands[0], operands[1]));
  else
    emit_jump_insn (gen_tablejump_osf (operands[0], operands[1]));

  DONE;
}")

(define_expand "tablejump_osf"
  [(set (match_dup 3)
	(sign_extend:DI (match_operand:SI 0 "register_operand" "")))
   (parallel [(set (pc)
		   (plus:DI (match_dup 3)
			    (label_ref:DI (match_operand 1 "" ""))))
	      (clobber (match_scratch:DI 2 "=r"))])]
  ""
  "
{ operands[3] = gen_reg_rtx (DImode); }")

(define_expand "tablejump_nt"
  [(set (match_dup 3)
	(sign_extend:DI (match_operand:SI 0 "register_operand" "")))
   (parallel [(set (pc)
		   (match_dup 3))
	      (use (label_ref (match_operand 1 "" "")))])]
  ""
  "
{ operands[3] = gen_reg_rtx (DImode); }")

;;
;; tablejump, openVMS way
;; op 0: offset
;; op 1: label preceding jump-table
;;
(define_expand "tablejump_vms"
  [(set (match_dup 2)
      (match_operand:DI 0 "register_operand" ""))
        (set (pc)
	(plus:DI (match_dup 2)
		(label_ref:DI (match_operand 1 "" ""))))]
  ""
  "
{ operands[2] = gen_reg_rtx (DImode); }")

(define_insn ""
  [(set (pc)
	(plus:DI (match_operand:DI 0 "register_operand" "r")
		 (label_ref:DI (match_operand 1 "" ""))))
   (clobber (match_scratch:DI 2 "=r"))]
  "! TARGET_WINDOWS_NT && ! TARGET_OPEN_VMS && next_active_insn (insn) != 0
   && GET_CODE (PATTERN (next_active_insn (insn))) == ADDR_DIFF_VEC
   && PREV_INSN (next_active_insn (insn)) == operands[1]"
  "*
{ rtx best_label = 0;
  rtx jump_table_insn = next_active_insn (operands[1]);

  if (GET_CODE (jump_table_insn) == JUMP_INSN
      && GET_CODE (PATTERN (jump_table_insn)) == ADDR_DIFF_VEC)
    {
      rtx jump_table = PATTERN (jump_table_insn);
      int n_labels = XVECLEN (jump_table, 1);
      int best_count = -1;
      int i, j;

      for (i = 0; i < n_labels; i++)
	{
	  int count = 1;

	  for (j = i + 1; j < n_labels; j++)
	    if (XEXP (XVECEXP (jump_table, 1, i), 0)
		== XEXP (XVECEXP (jump_table, 1, j), 0))
	      count++;

	  if (count > best_count)
	    best_count = count, best_label = XVECEXP (jump_table, 1, i);
	}
    }

  if (best_label)
    {
      operands[3] = best_label;
      return \"addq %0,$29,%2\;jmp $31,(%2),%3\";
    }
  else
    return \"addq %0,$29,%2\;jmp $31,(%2),0\";
}"
  [(set_attr "type" "ibr")])

(define_insn ""
  [(set (pc)
	(match_operand:DI 0 "register_operand" "r"))
   (use (label_ref (match_operand 1 "" "")))]
  "TARGET_WINDOWS_NT && next_active_insn (insn) != 0
   && GET_CODE (PATTERN (next_active_insn (insn))) == ADDR_DIFF_VEC
   && PREV_INSN (next_active_insn (insn)) == operands[1]"
  "*
{ rtx best_label = 0;
  rtx jump_table_insn = next_active_insn (operands[1]);

  if (GET_CODE (jump_table_insn) == JUMP_INSN
      && GET_CODE (PATTERN (jump_table_insn)) == ADDR_DIFF_VEC)
    {
      rtx jump_table = PATTERN (jump_table_insn);
      int n_labels = XVECLEN (jump_table, 1);
      int best_count = -1;
      int i, j;

      for (i = 0; i < n_labels; i++)
	{
	  int count = 1;

	  for (j = i + 1; j < n_labels; j++)
	    if (XEXP (XVECEXP (jump_table, 1, i), 0)
		== XEXP (XVECEXP (jump_table, 1, j), 0))
	      count++;

	  if (count > best_count)
	    best_count = count, best_label = XVECEXP (jump_table, 1, i);
	}
    }

  if (best_label)
    {
      operands[2] = best_label;
      return \"jmp $31,(%0),%2\";
    }
  else
    return \"jmp $31,(%0),0\";
}"
  [(set_attr "type" "ibr")])

;;
;; op 0 is table offset
;; op 1 is table label
;;

(define_insn ""
  [(set (pc)
	(plus:DI (match_operand 0 "register_operand" "r")
		(label_ref (match_operand 1 "" ""))))]
  "TARGET_OPEN_VMS"
  "jmp $31,(%0),0"
  [(set_attr "type" "ibr")])

;; Cache flush.  Used by INITIALIZE_TRAMPOLINE.  0x86 is PAL_imb, but we don't
;; want to have to include pal.h in our .s file.
;;
;; Technically the type for call_pal is jsr, but we use that for determining
;; if we need a GP.  Use ibr instead since it has the same scheduling 
;; characteristics.
(define_insn ""
  [(unspec_volatile [(const_int 0)] 0)]
  ""
  "call_pal 0x86"
  [(set_attr "type" "ibr")])

;; Finally, we have the basic data motion insns.  The byte and word insns
;; are done via define_expand.  Start with the floating-point insns, since
;; they are simpler.

(define_insn ""
  [(set (match_operand:SF 0 "nonimmediate_operand" "=r,r,m,f,f,f,m")
	(match_operand:SF 1 "input_operand" "rG,m,rG,f,G,m,fG"))]
  "register_operand (operands[0], SFmode)
   || reg_or_fp0_operand (operands[1], SFmode)"
  "@
   bis %r1,%r1,%0
   ldl %0,%1
   stl %r1,%0
   cpys %1,%1,%0
   cpys $f31,$f31,%0
   ld%, %0,%1
   st%, %R1,%0"
  [(set_attr "type" "ilog,ld,st,fcpys,fcpys,ld,st")])

(define_insn ""
  [(set (match_operand:DF 0 "nonimmediate_operand" "=r,r,m,f,f,f,m")
	(match_operand:DF 1 "input_operand" "rG,m,rG,f,G,m,fG"))]
  "register_operand (operands[0], DFmode)
   || reg_or_fp0_operand (operands[1], DFmode)"
  "@
   bis %r1,%r1,%0
   ldq %0,%1
   stq %r1,%0
   cpys %1,%1,%0
   cpys $f31,$f31,%0
   ld%- %0,%1
   st%- %R1,%0"
  [(set_attr "type" "ilog,ld,st,fcpys,fcpys,ld,st")])

(define_expand "movsf"
  [(set (match_operand:SF 0 "nonimmediate_operand" "")
	(match_operand:SF 1 "general_operand" ""))]
  ""
  "
{
  if (GET_CODE (operands[0]) == MEM
      && ! reg_or_fp0_operand (operands[1], SFmode))
    operands[1] = force_reg (SFmode, operands[1]);
}")

(define_expand "movdf"
  [(set (match_operand:DF 0 "nonimmediate_operand" "")
	(match_operand:DF 1 "general_operand" ""))]
  ""
  "
{
  if (GET_CODE (operands[0]) == MEM
      && ! reg_or_fp0_operand (operands[1], DFmode))
    operands[1] = force_reg (DFmode, operands[1]);
}")

(define_insn ""
  [(set (match_operand:SI 0 "nonimmediate_operand" "=r,r,r,r,r,r,m,f,f,f,m")
	(match_operand:SI 1 "input_operand" "r,J,I,K,L,m,rJ,f,J,m,fG"))]
  "! TARGET_WINDOWS_NT && ! TARGET_OPEN_VMS
   && (register_operand (operands[0], SImode)
       || reg_or_0_operand (operands[1], SImode))"
  "@
   bis %1,%1,%0
   bis $31,$31,%0
   bis $31,%1,%0
   lda %0,%1
   ldah %0,%h1
   ldl %0,%1
   stl %r1,%0
   cpys %1,%1,%0
   cpys $f31,$f31,%0
   lds %0,%1
   sts %R1,%0"
  [(set_attr "type" "ilog,ilog,ilog,iadd,iadd,ld,st,fcpys,fcpys,ld,st")])

(define_insn ""
  [(set (match_operand:SI 0 "nonimmediate_operand" "=r,r,r,r,r,r,r,m,f,f,f,m")
	(match_operand:SI 1 "input_operand" "r,J,I,K,L,s,m,rJ,f,J,m,fG"))]
  "(TARGET_WINDOWS_NT || TARGET_OPEN_VMS)
	&& (register_operand (operands[0], SImode)
			 || reg_or_0_operand (operands[1], SImode))"
  "@
   bis %1,%1,%0
   bis $31,$31,%0
   bis $31,%1,%0
   lda %0,%1
   ldah %0,%h1
   lda %0,%1
   ldl %0,%1
   stl %r1,%0
   cpys %1,%1,%0
   cpys $f31,$f31,%0
   lds %0,%1
   sts %R1,%0"
  [(set_attr "type" "ilog,ilog,ilog,iadd,iadd,ldsym,ld,st,fcpys,fcpys,ld,st")])

(define_insn ""
  [(set (match_operand:HI 0 "nonimmediate_operand" "=r,r,r,r,f,f")
	(match_operand:HI 1 "input_operand" "r,J,I,n,f,J"))]
  "! TARGET_BYTE_OPS
   && (register_operand (operands[0], HImode)
       || register_operand (operands[1], HImode))"
  "@
   bis %1,%1,%0
   bis $31,$31,%0
   bis $31,%1,%0
   lda %0,%L1
   cpys %1,%1,%0
   cpys $f31,$f31,%0"
  [(set_attr "type" "ilog,ilog,ilog,iadd,fcpys,fcpys")])

(define_insn ""
  [(set (match_operand:HI 0 "nonimmediate_operand" "=r,r,r,r,r,m,f,f")
	(match_operand:HI 1 "input_operand" "r,J,I,n,m,rJ,f,J"))]
  "TARGET_BYTE_OPS
   && (register_operand (operands[0], HImode)
       || reg_or_0_operand (operands[1], HImode))"
  "@
   bis %1,%1,%0
   bis $31,$31,%0
   bis $31,%1,%0
   lda %0,%L1
   ldwu %0,%1
   stw %r1,%0
   cpys %1,%1,%0
   cpys $f31,$f31,%0"
  [(set_attr "type" "ilog,ilog,ilog,iadd,ld,st,fcpys,fcpys")])

(define_insn ""
  [(set (match_operand:QI 0 "nonimmediate_operand" "=r,r,r,r,f,f")
	(match_operand:QI 1 "input_operand" "r,J,I,n,f,J"))]
  "! TARGET_BYTE_OPS
   && (register_operand (operands[0], QImode)
       || register_operand (operands[1], QImode))"
  "@
   bis %1,%1,%0
   bis $31,$31,%0
   bis $31,%1,%0
   lda %0,%L1
   cpys %1,%1,%0
   cpys $f31,$f31,%0"
  [(set_attr "type" "ilog,ilog,ilog,iadd,fcpys,fcpys")])

(define_insn ""
  [(set (match_operand:QI 0 "nonimmediate_operand" "=r,r,r,r,r,m,f,f")
	(match_operand:QI 1 "input_operand" "r,J,I,n,m,rJ,f,J"))]
  "TARGET_BYTE_OPS
   && (register_operand (operands[0], QImode)
       || reg_or_0_operand (operands[1], QImode))"
  "@
   bis %1,%1,%0
   bis $31,$31,%0
   bis $31,%1,%0
   lda %0,%L1
   ldbu %0,%1
   stb %r1,%0
   cpys %1,%1,%0
   cpys $f31,$f31,%0"
  [(set_attr "type" "ilog,ilog,ilog,iadd,ld,st,fcpys,fcpys")])

;; We do two major things here: handle mem->mem and construct long
;; constants.

(define_expand "movsi"
  [(set (match_operand:SI 0 "general_operand" "")
	(match_operand:SI 1 "general_operand" ""))]
  ""
  "
{
  if (GET_CODE (operands[0]) == MEM
      && ! reg_or_0_operand (operands[1], SImode))
    operands[1] = force_reg (SImode, operands[1]);

  if (! CONSTANT_P (operands[1]) || input_operand (operands[1], SImode))
    ;
  else if (GET_CODE (operands[1]) == CONST_INT)
    {
      operands[1]
	= alpha_emit_set_const (operands[0], SImode, INTVAL (operands[1]), 3);
      if (rtx_equal_p (operands[0], operands[1]))
	DONE;
    }
}")

;; Split a load of a large constant into the appropriate two-insn
;; sequence.

(define_split
  [(set (match_operand:SI 0 "register_operand" "")
	(match_operand:SI 1 "const_int_operand" ""))]
  "! add_operand (operands[1], SImode)"
  [(set (match_dup 0) (match_dup 2))
   (set (match_dup 0) (plus:SI (match_dup 0) (match_dup 3)))]
  "
{ rtx tem
    = alpha_emit_set_const (operands[0], SImode, INTVAL (operands[1]), 2);

  if (tem == operands[0])
    DONE;
  else
    FAIL;
}")

(define_insn ""
  [(set (match_operand:DI 0 "general_operand" "=r,r,r,r,r,r,r,m,f,f,f,Q")
	(match_operand:DI 1 "input_operand" "r,J,I,K,L,s,m,rJ,f,J,Q,fG"))]
  "register_operand (operands[0], DImode)
   || reg_or_0_operand (operands[1], DImode)"
  "@
   bis %1,%1,%0
   bis $31,$31,%0
   bis $31,%1,%0
   lda %0,%1
   ldah %0,%h1
   lda %0,%1
   ldq%A1 %0,%1
   stq%A0 %r1,%0
   cpys %1,%1,%0
   cpys $f31,$f31,%0
   ldt %0,%1
   stt %R1,%0"
  [(set_attr "type" "ilog,ilog,ilog,iadd,iadd,ldsym,ld,st,fcpys,fcpys,ld,st")])

;; We do three major things here: handle mem->mem, put 64-bit constants in
;; memory, and construct long 32-bit constants.

(define_expand "movdi"
  [(set (match_operand:DI 0 "general_operand" "")
	(match_operand:DI 1 "general_operand" ""))]
  ""
  "
{
  rtx tem;

  if (GET_CODE (operands[0]) == MEM
      && ! reg_or_0_operand (operands[1], DImode))
    operands[1] = force_reg (DImode, operands[1]);

  if (! CONSTANT_P (operands[1]) || input_operand (operands[1], DImode))
    ;
  else if (GET_CODE (operands[1]) == CONST_INT
	   && (tem = alpha_emit_set_const (operands[0], DImode,
					   INTVAL (operands[1]), 3)) != 0)
    {
      if (rtx_equal_p (tem, operands[0]))
	DONE;
      else
	operands[1] = tem;
    }
  else if (CONSTANT_P (operands[1]))
    {
      if (TARGET_BUILD_CONSTANTS)
	{
#if HOST_BITS_PER_WIDE_INT == 64
	  HOST_WIDE_INT i;

	  if (GET_CODE (operands[1]) == CONST_INT)
	    i = INTVAL (operands[1]);
	  else if (GET_CODE (operands[1]) == CONST_DOUBLE)
	    i = CONST_DOUBLE_LOW (operands[1]);
	  else
	    abort();
	  
          tem = alpha_emit_set_long_const (operands[0], i);
          if (rtx_equal_p (tem, operands[0]))
	    DONE;
          else
	    operands[1] = tem;
#else
          abort();
#endif
	}
      else
	{
	  operands[1] = force_const_mem (DImode, operands[1]);
	  if (reload_in_progress)
	    {
	      emit_move_insn (operands[0], XEXP (operands[1], 0));
	      operands[1] = copy_rtx (operands[1]);
	      XEXP (operands[1], 0) = operands[0];
	    }
	  else
	    operands[1] = validize_mem (operands[1]);
	}
    }
  else
    abort ();
}")

;; Split a load of a large constant into the appropriate two-insn
;; sequence.

(define_split
  [(set (match_operand:DI 0 "register_operand" "")
	(match_operand:DI 1 "const_int_operand" ""))]
  "! add_operand (operands[1], DImode)"
  [(set (match_dup 0) (match_dup 2))
   (set (match_dup 0) (plus:DI (match_dup 0) (match_dup 3)))]
  "
{ rtx tem
    = alpha_emit_set_const (operands[0], DImode, INTVAL (operands[1]), 2);

  if (tem == operands[0])
    DONE;
  else
    FAIL;
}")

;; These are the partial-word cases.
;;
;; First we have the code to load an aligned word.  Operand 0 is the register
;; in which to place the result.  It's mode is QImode or HImode.  Operand 1
;; is an SImode MEM at the low-order byte of the proper word.  Operand 2 is the
;; number of bits within the word that the value is.  Operand 3 is an SImode
;; scratch register.  If operand 0 is a hard register, operand 3 may be the
;; same register.  It is allowed to conflict with operand 1 as well.

(define_expand "aligned_loadqi"
  [(set (match_operand:SI 3 "register_operand" "")
	(match_operand:SI 1 "memory_operand" ""))
   (set (subreg:DI (match_operand:QI 0 "register_operand" "") 0)
	(zero_extract:DI (subreg:DI (match_dup 3) 0)
			 (const_int 8)
			 (match_operand:DI 2 "const_int_operand" "")))]
	 
  ""
  "")
  
(define_expand "aligned_loadhi"
  [(set (match_operand:SI 3 "register_operand" "")
	(match_operand:SI 1 "memory_operand" ""))
   (set (subreg:DI (match_operand:HI 0 "register_operand" "") 0)
	(zero_extract:DI (subreg:DI (match_dup 3) 0)
			 (const_int 16)
			 (match_operand:DI 2 "const_int_operand" "")))]
	 
  ""
  "")
  
;; Similar for unaligned loads, where we use the sequence from the
;; Alpha Architecture manual.
;;
;; Operand 1 is the address.  Operands 2 and 3 are temporaries, where
;; operand 3 can overlap the input and output registers.

(define_expand "unaligned_loadqi"
  [(set (match_operand:DI 2 "register_operand" "")
	(mem:DI (and:DI (match_operand:DI 1 "address_operand" "")
			(const_int -8))))
   (set (match_operand:DI 3 "register_operand" "")
	(match_dup 1))
   (set (subreg:DI (match_operand:QI 0 "register_operand" "") 0)
	(zero_extract:DI (match_dup 2)
			 (const_int 8)
			 (ashift:DI (match_dup 3) (const_int 3))))]
  ""
  "")

(define_expand "unaligned_loadhi"
  [(set (match_operand:DI 2 "register_operand" "")
	(mem:DI (and:DI (match_operand:DI 1 "address_operand" "")
			(const_int -8))))
   (set (match_operand:DI 3 "register_operand" "")
	(match_dup 1))
   (set (subreg:DI (match_operand:QI 0 "register_operand" "") 0)
	(zero_extract:DI (match_dup 2)
			 (const_int 16)
			 (ashift:DI (match_dup 3) (const_int 3))))]
  ""
  "")

;; Storing an aligned byte or word requires two temporaries.  Operand 0 is the
;; aligned SImode MEM.  Operand 1 is the register containing the 
;; byte or word to store.  Operand 2 is the number of bits within the word that
;; the value should be placed.  Operands 3 and 4 are SImode temporaries.

(define_expand "aligned_store"
  [(set (match_operand:SI 3 "register_operand" "")
	(match_operand:SI 0 "memory_operand" ""))
   (set (subreg:DI (match_dup 3) 0)
	(and:DI (subreg:DI (match_dup 3) 0) (match_dup 5)))
   (set (subreg:DI (match_operand:SI 4 "register_operand" "") 0)
	(ashift:DI (zero_extend:DI (match_operand 1 "register_operand" ""))
		   (match_operand:DI 2 "const_int_operand" "")))
   (set (subreg:DI (match_dup 4) 0)
	(ior:DI (subreg:DI (match_dup 4) 0) (subreg:DI (match_dup 3) 0)))
   (set (match_dup 0) (match_dup 4))]
  ""
  "
{ operands[5] = GEN_INT (~ (GET_MODE_MASK (GET_MODE (operands[1]))
			    << INTVAL (operands[2])));
}")

;; For the unaligned byte and halfword cases, we use code similar to that
;; in the ;; Architecture book, but reordered to lower the number of registers
;; required.  Operand 0 is the address.  Operand 1 is the data to store.
;; Operands 2, 3, and 4 are DImode temporaries, where operands 2 and 4 may
;; be the same temporary, if desired.  If the address is in a register,
;; operand 2 can be that register.

(define_expand "unaligned_storeqi"
  [(set (match_operand:DI 3 "register_operand" "")
	(mem:DI (and:DI (match_operand:DI 0 "address_operand" "")
			(const_int -8))))
   (set (match_operand:DI 2 "register_operand" "")
	(match_dup 0))
   (set (match_dup 3)
	(and:DI (not:DI (ashift:DI (const_int 255)
				   (ashift:DI (match_dup 2) (const_int 3))))
		(match_dup 3)))
   (set (match_operand:DI 4 "register_operand" "")
	(ashift:DI (zero_extend:DI (match_operand:QI 1 "register_operand" ""))
		   (ashift:DI (match_dup 2) (const_int 3))))
   (set (match_dup 4) (ior:DI (match_dup 4) (match_dup 3)))
   (set (mem:DI (and:DI (match_dup 0) (const_int -8)))
	(match_dup 4))]
  ""
  "")

(define_expand "unaligned_storehi"
  [(set (match_operand:DI 3 "register_operand" "")
	(mem:DI (and:DI (match_operand:DI 0 "address_operand" "")
			(const_int -8))))
   (set (match_operand:DI 2 "register_operand" "")
	(match_dup 0))
   (set (match_dup 3)
	(and:DI (not:DI (ashift:DI (const_int 65535)
				   (ashift:DI (match_dup 2) (const_int 3))))
		(match_dup 3)))
   (set (match_operand:DI 4 "register_operand" "")
	(ashift:DI (zero_extend:DI (match_operand:HI 1 "register_operand" ""))
		   (ashift:DI (match_dup 2) (const_int 3))))
   (set (match_dup 4) (ior:DI (match_dup 4) (match_dup 3)))
   (set (mem:DI (and:DI (match_dup 0) (const_int -8)))
	(match_dup 4))]
  ""
  "")

;; Here are the define_expand's for QI and HI moves that use the above
;; patterns.  We have the normal sets, plus the ones that need scratch
;; registers for reload.

(define_expand "movqi"
  [(set (match_operand:QI 0 "general_operand" "")
	(match_operand:QI 1 "general_operand" ""))]
  ""
  "
{ extern rtx get_unaligned_address ();

  if (TARGET_BYTE_OPS)
    {
      if (GET_CODE (operands[0]) == MEM
	  && ! reg_or_0_operand (operands[1], QImode))
	operands[1] = force_reg (QImode, operands[1]);

      if (GET_CODE (operands[1]) == CONST_INT
	       && ! input_operand (operands[1], QImode))
	{
	  operands[1] = alpha_emit_set_const (operands[0], QImode,
					      INTVAL (operands[1]), 3);

	  if (rtx_equal_p (operands[0], operands[1]))
	    DONE;
	}

      goto def;
    }

  /* If the output is not a register, the input must be.  */
  if (GET_CODE (operands[0]) == MEM)
    operands[1] = force_reg (QImode, operands[1]);

  /* Handle four memory cases, unaligned and aligned for either the input
     or the output.  The only case where we can be called during reload is
     for aligned loads; all other cases require temporaries.  */

  if (GET_CODE (operands[1]) == MEM
      || (GET_CODE (operands[1]) == SUBREG
	  && GET_CODE (SUBREG_REG (operands[1])) == MEM)
      || (reload_in_progress && GET_CODE (operands[1]) == REG
	  && REGNO (operands[1]) >= FIRST_PSEUDO_REGISTER)
      || (reload_in_progress && GET_CODE (operands[1]) == SUBREG
	  && GET_CODE (SUBREG_REG (operands[1])) == REG
	  && REGNO (SUBREG_REG (operands[1])) >= FIRST_PSEUDO_REGISTER))
    {
      if (aligned_memory_operand (operands[1], QImode))
	{
	  rtx aligned_mem, bitnum;
	  rtx scratch = (reload_in_progress
			 ? gen_rtx (REG, SImode, REGNO (operands[0]))
			 : gen_reg_rtx (SImode));

	  /* ??? This code creates a new MEM rtx.  If we were called during
	     reload, then we must be careful to make sure that the new rtx
	     will not need reloading.  */
	  if (reload_in_progress
	      && GET_CODE (operands[1]) == MEM
	      && ! strict_memory_address_p (SImode, XEXP (operands[1], 0)))
	    {
	      rtx tmp = gen_rtx (REG, Pmode, REGNO (operands[0]));
	      emit_insn (gen_move_insn (tmp, XEXP (operands[1], 0)));
	      XEXP (operands[1], 0) = tmp;
	    }

	  get_aligned_mem (operands[1], &aligned_mem, &bitnum);

	  emit_insn (gen_aligned_loadqi (operands[0], aligned_mem, bitnum,
					 scratch));
	}
      else
	{
	  /* Don't pass these as parameters since that makes the generated
	     code depend on parameter evaluation order which will cause
	     bootstrap failures.  */

	  rtx temp1 = gen_reg_rtx (DImode);
	  rtx temp2 = gen_reg_rtx (DImode);
	  rtx seq
	    = gen_unaligned_loadqi (operands[0],
				    get_unaligned_address (operands[1], 0),
				    temp1, temp2);

	  alpha_set_memflags (seq, operands[1]);
	  emit_insn (seq);
	}

      DONE;
    }

  else if (GET_CODE (operands[0]) == MEM
	   || (GET_CODE (operands[0]) == SUBREG 
	       && GET_CODE (SUBREG_REG (operands[0])) == MEM)
	   || (reload_in_progress && GET_CODE (operands[0]) == REG
	       && REGNO (operands[0]) >= FIRST_PSEUDO_REGISTER)
	   || (reload_in_progress && GET_CODE (operands[0]) == SUBREG
	       && GET_CODE (SUBREG_REG (operands[0])) == REG
	       && REGNO (operands[0]) >= FIRST_PSEUDO_REGISTER))
    {
      if (aligned_memory_operand (operands[0], QImode))
	{
	  rtx aligned_mem, bitnum;
	  rtx temp1 = gen_reg_rtx (SImode);
	  rtx temp2 = gen_reg_rtx (SImode);

	  get_aligned_mem (operands[0], &aligned_mem, &bitnum);

	  emit_insn (gen_aligned_store (aligned_mem, operands[1], bitnum,
					temp1, temp2));
	}
      else
	{
	  rtx temp1 = gen_reg_rtx (DImode);
	  rtx temp2 = gen_reg_rtx (DImode);
	  rtx temp3 = gen_reg_rtx (DImode);
	  rtx seq
	    = gen_unaligned_storeqi (get_unaligned_address (operands[0], 0),
					   operands[1], temp1, temp2, temp3);

	  alpha_set_memflags (seq, operands[0]);
	  emit_insn (seq);
	}
      DONE;
    }
 def:;
}")

(define_expand "movhi"
  [(set (match_operand:HI 0 "general_operand" "")
	(match_operand:HI 1 "general_operand" ""))]
  ""
  "
{ extern rtx get_unaligned_address ();

  if (TARGET_BYTE_OPS)
    {
      if (GET_CODE (operands[0]) == MEM
	  && ! reg_or_0_operand (operands[1], HImode))
	operands[1] = force_reg (HImode, operands[1]);

      if (GET_CODE (operands[1]) == CONST_INT
	       && ! input_operand (operands[1], HImode))
	{
	  operands[1] = alpha_emit_set_const (operands[0], HImode,
					      INTVAL (operands[1]), 3);

	  if (rtx_equal_p (operands[0], operands[1]))
	    DONE;
	}

      goto def;
    }

  /* If the output is not a register, the input must be.  */
  if (GET_CODE (operands[0]) == MEM)
    operands[1] = force_reg (HImode, operands[1]);

  /* Handle four memory cases, unaligned and aligned for either the input
     or the output.  The only case where we can be called during reload is
     for aligned loads; all other cases require temporaries.  */

  if (GET_CODE (operands[1]) == MEM
      || (GET_CODE (operands[1]) == SUBREG
	  && GET_CODE (SUBREG_REG (operands[1])) == MEM)
      || (reload_in_progress && GET_CODE (operands[1]) == REG
	  && REGNO (operands[1]) >= FIRST_PSEUDO_REGISTER)
      || (reload_in_progress && GET_CODE (operands[1]) == SUBREG
	  && GET_CODE (SUBREG_REG (operands[1])) == REG
	  && REGNO (SUBREG_REG (operands[1])) >= FIRST_PSEUDO_REGISTER))
    {
      if (aligned_memory_operand (operands[1], HImode))
	{
	  rtx aligned_mem, bitnum;
	  rtx scratch = (reload_in_progress
			 ? gen_rtx (REG, SImode, REGNO (operands[0]))
			 : gen_reg_rtx (SImode));

	  /* ??? This code creates a new MEM rtx.  If we were called during
	     reload, then we must be careful to make sure that the new rtx
	     will not need reloading.  */
	  if (reload_in_progress
	      && GET_CODE (operands[1]) == MEM
	      && ! strict_memory_address_p (SImode, XEXP (operands[1], 0)))
	    {
	      rtx tmp = gen_rtx (REG, Pmode, REGNO (operands[0]));
	      emit_insn (gen_move_insn (tmp, XEXP (operands[1], 0)));
	      XEXP (operands[1], 0) = tmp;
	    }

	  get_aligned_mem (operands[1], &aligned_mem, &bitnum);

	  emit_insn (gen_aligned_loadhi (operands[0], aligned_mem, bitnum,
					 scratch));
	}
      else
	{
	  /* Don't pass these as parameters since that makes the generated
	     code depend on parameter evaluation order which will cause
	     bootstrap failures.  */

	  rtx temp1 = gen_reg_rtx (DImode);
	  rtx temp2 = gen_reg_rtx (DImode);
	  rtx seq
	    = gen_unaligned_loadhi (operands[0],
				    get_unaligned_address (operands[1], 0),
				    temp1, temp2);

	  alpha_set_memflags (seq, operands[1]);
	  emit_insn (seq);
	}

      DONE;
    }

  else if (GET_CODE (operands[0]) == MEM
	   || (GET_CODE (operands[0]) == SUBREG 
	       && GET_CODE (SUBREG_REG (operands[0])) == MEM)
	   || (reload_in_progress && GET_CODE (operands[0]) == REG
	       && REGNO (operands[0]) >= FIRST_PSEUDO_REGISTER)
	   || (reload_in_progress && GET_CODE (operands[0]) == SUBREG
	       && GET_CODE (SUBREG_REG (operands[0])) == REG
	       && REGNO (operands[0]) >= FIRST_PSEUDO_REGISTER))
    {
      if (aligned_memory_operand (operands[0], HImode))
	{
	  rtx aligned_mem, bitnum;
	  rtx temp1 = gen_reg_rtx (SImode);
	  rtx temp2 = gen_reg_rtx (SImode);

	  get_aligned_mem (operands[0], &aligned_mem, &bitnum);

	  emit_insn (gen_aligned_store (aligned_mem, operands[1], bitnum,
					temp1, temp2));
	}
      else
	{
	  rtx temp1 = gen_reg_rtx (DImode);
	  rtx temp2 = gen_reg_rtx (DImode);
	  rtx temp3 = gen_reg_rtx (DImode);
	  rtx seq
	    = gen_unaligned_storehi (get_unaligned_address (operands[0], 0),
				     operands[1], temp1, temp2, temp3);

	  alpha_set_memflags (seq, operands[0]);
	  emit_insn (seq);
	}

      DONE;
    }
 def:;
}")

;; Here are the versions for reload.  Note that in the unaligned cases
;; we know that the operand must not be a pseudo-register because stack
;; slots are always aligned references.

(define_expand "reload_inqi"
  [(parallel [(match_operand:QI 0 "register_operand" "=r")
	      (match_operand:QI 1 "unaligned_memory_operand" "m")
	      (match_operand:TI 2 "register_operand" "=&r")])]
  "! TARGET_BYTE_OPS"
  "
{ extern rtx get_unaligned_address ();
  rtx addr, scratch, seq, tmp;

  /* It is possible that one of the registers we got for operands[2]
     might coincide with that of operands[0] (which is why we made
     it TImode).  Pick the other one to use as our scratch.  */
  scratch = gen_rtx (REG, DImode,
		     REGNO (operands[0]) == REGNO (operands[2]) 
		     ? REGNO (operands[2]) + 1 : REGNO (operands[2]));

  /* We must be careful to make sure that the new rtx won't need reloading.  */
  if (GET_CODE (operands[1]) == MEM &&
      ! strict_memory_address_p (DImode, XEXP (operands[1], 0)))
    {
      tmp = gen_rtx (REG, Pmode, REGNO (operands[0]));
      emit_insn (gen_move_insn (tmp, XEXP (operands[1], 0)));
      XEXP (operands[1], 0) = tmp;
    }
  addr = get_unaligned_address (operands[1], 0);

  seq = gen_unaligned_loadqi (operands[0], addr, scratch,
			      gen_rtx (REG, DImode, REGNO (operands[0])));

  alpha_set_memflags (seq, operands[1]);
  emit_insn (seq);
  DONE;
}")

(define_expand "reload_inhi"
  [(parallel [(match_operand:HI 0 "register_operand" "=r")
	      (match_operand:HI 1 "unaligned_memory_operand" "m")
	      (match_operand:TI 2 "register_operand" "=&r")])]
  "! TARGET_BYTE_OPS"
  "
{ extern rtx get_unaligned_address ();
  rtx scratch, seq, tmp, addr;

  /* It is possible that one of the registers we got for operands[2]
     might coincide with that of operands[0] (which is why we made
     it TImode).  Pick the other one to use as our scratch.  */
  scratch = gen_rtx (REG, DImode,
		     REGNO (operands[0]) == REGNO (operands[2]) 
		     ? REGNO (operands[2]) + 1 : REGNO (operands[2]));

  /* We must be careful to make sure that the new rtx won't need reloading.  */
  if (GET_CODE (operands[1]) == MEM &&
      ! strict_memory_address_p (DImode, XEXP (operands[1], 0)))
    {
      tmp = gen_rtx (REG, Pmode, REGNO (operands[0]));
      emit_insn (gen_move_insn (tmp, XEXP (operands[1], 0)));
      XEXP (operands[1], 0) = tmp;
    }
  addr = get_unaligned_address (operands[1], 0);

  seq = gen_unaligned_loadhi (operands[0], addr, scratch,
			      gen_rtx (REG, DImode, REGNO (operands[0])));

  alpha_set_memflags (seq, operands[1]);
  emit_insn (seq);
  DONE;
}")

(define_expand "reload_outqi"
  [(parallel [(match_operand:QI 0 "any_memory_operand" "=m")
	      (match_operand:QI 1 "register_operand" "r")
	      (match_operand:TI 2 "register_operand" "=&r")])]
  "! TARGET_BYTE_OPS"
  "
{ extern rtx get_unaligned_address ();

  /* Note that any_memory_operand allows pseudos during reload.  */
  if (GET_CODE (operands[0]) == MEM &&
      ! strict_memory_address_p (DImode, XEXP (operands[0], 0)))
    {
      rtx scratch1 = gen_rtx (REG, DImode, REGNO (operands[2]));
      emit_insn (gen_move_insn (scratch1, XEXP (operands[0], 0)));
      XEXP (operands[0], 0) = scratch1;
    }

  if (aligned_memory_operand (operands[0], QImode))
    {
      rtx aligned_mem, bitnum;

      get_aligned_mem (operands[0], &aligned_mem, &bitnum);

      emit_insn (gen_aligned_store (aligned_mem, operands[1], bitnum,
				    gen_rtx (REG, SImode, REGNO (operands[2])),
				    gen_rtx (REG, SImode,
					     REGNO (operands[2]) + 1)));
    }
  else
    {
      rtx addr = get_unaligned_address (operands[0], 0);
      rtx scratch1 = gen_rtx (REG, DImode, REGNO (operands[2]));
      rtx scratch2 = gen_rtx (REG, DImode, REGNO (operands[2]) + 1);
      rtx scratch3 = scratch1;
      rtx seq;

      if (GET_CODE (addr) == REG)
	scratch1 = addr;

      seq = gen_unaligned_storeqi (addr, operands[1], scratch1,
				   scratch2, scratch3);
      alpha_set_memflags (seq, operands[0]);
      emit_insn (seq);
    }

  DONE;
}")

(define_expand "reload_outhi"
  [(parallel [(match_operand:HI 0 "any_memory_operand" "=m")
	      (match_operand:HI 1 "register_operand" "r")
	      (match_operand:TI 2 "register_operand" "=&r")])]
  "! TARGET_BYTE_OPS"
  "
{ extern rtx get_unaligned_address ();

  /* Note that any_memory_operand allows pseudos during reload.  */
  if (GET_CODE (operands[0]) == MEM &&
      ! strict_memory_address_p (DImode, XEXP (operands[0], 0)))
    {
      rtx scratch1 = gen_rtx (REG, DImode, REGNO (operands[2]));
      emit_insn (gen_move_insn (scratch1, XEXP (operands[0], 0)));
      XEXP (operands[0], 0) = scratch1;
    }

  if (aligned_memory_operand (operands[0], HImode))
    {
      rtx aligned_mem, bitnum;

      get_aligned_mem (operands[0], &aligned_mem, &bitnum);

      emit_insn (gen_aligned_store (aligned_mem, operands[1], bitnum,
				    gen_rtx (REG, SImode, REGNO (operands[2])),
				    gen_rtx (REG, SImode,
					     REGNO (operands[2]) + 1)));
    }
  else
    {
      rtx addr = get_unaligned_address (operands[0], 0);
      rtx scratch1 = gen_rtx (REG, DImode, REGNO (operands[2]));
      rtx scratch2 = gen_rtx (REG, DImode, REGNO (operands[2]) + 1);
      rtx scratch3 = scratch1;
      rtx seq;

      if (GET_CODE (addr) == REG)
	scratch1 = addr;

      seq = gen_unaligned_storehi (addr, operands[1], scratch1,
				   scratch2, scratch3);
      alpha_set_memflags (seq, operands[0]);
      emit_insn (seq);
    }

  DONE;
}")

;; Subroutine of stack space allocation.  Perform a stack probe.
(define_expand "probe_stack"
  [(set (match_dup 1) (match_operand:DI 0 "const_int_operand" ""))]
  ""
  "
{
  operands[1] = gen_rtx (MEM, DImode, plus_constant (stack_pointer_rtx,
						     INTVAL (operands[0])));
  MEM_VOLATILE_P (operands[1]) = 1;

  operands[0] = const0_rtx;
}")

;; This is how we allocate stack space.  If we are allocating a
;; constant amount of space and we know it is less than 4096
;; bytes, we need do nothing.
;;
;; If it is more than 4096 bytes, we need to probe the stack
;; periodically. 
(define_expand "allocate_stack"
  [(set (reg:DI 30)
	(plus:DI (reg:DI 30)
		 (match_operand:DI 0 "reg_or_cint_operand" "")))]
  ""
  "
{
  if (GET_CODE (operands[0]) == CONST_INT
	   && INTVAL (operands[0]) < 32768)
    {
      if (INTVAL (operands[0]) >= 4096)
	{
	  /* We do this the same way as in the prologue and generate explicit
	     probes.  Then we update the stack by the constant.  */

	  int probed = 4096;

	  emit_insn (gen_probe_stack (GEN_INT (- probed)));
	  while (probed + 8192 < INTVAL (operands[0]))
	    emit_insn (gen_probe_stack (GEN_INT (- (probed += 8192))));

	  if (probed + 4096 < INTVAL (operands[0]))
	    emit_insn (gen_probe_stack (GEN_INT (- INTVAL(operands[0]))));
	}

      operands[0] = GEN_INT (- INTVAL (operands[0]));
    }
  else
    {
      rtx out_label = 0;
      rtx loop_label = gen_label_rtx ();
      rtx want = gen_reg_rtx (Pmode);
      rtx tmp = gen_reg_rtx (Pmode);
      rtx memref;

      emit_insn (gen_subdi3 (want, stack_pointer_rtx,
			     force_reg (Pmode, operands[0])));
      emit_insn (gen_adddi3 (tmp, stack_pointer_rtx, GEN_INT (-4096)));

      if (GET_CODE (operands[0]) != CONST_INT)
	{
	  out_label = gen_label_rtx ();
	  emit_insn (gen_cmpdi (want, tmp));
	  emit_jump_insn (gen_bgeu (out_label));
	}

      emit_label (loop_label);
      memref = gen_rtx (MEM, DImode, tmp);
      MEM_VOLATILE_P (memref) = 1;
      emit_move_insn (memref, const0_rtx);
      emit_insn (gen_adddi3 (tmp, tmp, GEN_INT(-8192)));
      emit_insn (gen_cmpdi (tmp, want));
      emit_jump_insn (gen_bgtu (loop_label));
      memref = gen_rtx (MEM, DImode, want);
      MEM_VOLATILE_P (memref) = 1;
      emit_move_insn (memref, const0_rtx);

      if (out_label)
	emit_label (out_label);

      emit_move_insn (stack_pointer_rtx, want);

      DONE;
    }
}")

;; Ideally we should be able to define nonlocal_goto and arrange
;; for the pc to be in a known place.  Or perhaps branch back via
;; br instead of jmp.
(define_insn "nonlocal_goto_receiver_osf"
  [(unspec_volatile [(const_int 0)] 2)]
  "! TARGET_OPEN_VMS && ! TARGET_WINDOWS_NT"
  "br $29,$LGOTO%=\\n$LGOTO%=:\;ldgp $29,0($29)")

(define_expand "nonlocal_goto_receiver_vms"
  [(unspec_volatile [(const_int 0)] 1)
   (set (reg:DI 27) (mem:DI (reg:DI 29)))
   (unspec_volatile [(const_int 0)] 1)
   (use (reg:DI 27))]
  "TARGET_OPEN_VMS"
  "")

(define_expand "nonlocal_goto_receiver"
  [(unspec_volatile [(const_int 0)] 2)]
  ""
  "
{
  if (TARGET_OPEN_VMS)
    emit_insn(gen_nonlocal_goto_receiver_vms ());
  else if (!TARGET_WINDOWS_NT)
    emit_insn(gen_nonlocal_goto_receiver_osf ());
  DONE;
}")

(define_insn "arg_home"
  [(unspec [(const_int 0)] 0)
   (use (reg:DI 1))
   (use (reg:DI 25))
   (use (reg:DI 16))
   (use (reg:DI 17))
   (use (reg:DI 18))
   (use (reg:DI 19))
   (use (reg:DI 20))
   (use (reg:DI 21))
   (use (reg:DI 48))
   (use (reg:DI 49))
   (use (reg:DI 50))
   (use (reg:DI 51))
   (use (reg:DI 52))
   (use (reg:DI 53))
   (clobber (mem:BLK (const_int 0)))
   (clobber (reg:DI 24))
   (clobber (reg:DI 25))
   (clobber (reg:DI 0))]
  "TARGET_OPEN_VMS"
  "lda $0,OTS$HOME_ARGS\;ldq $0,8($0)\;jsr $0,OTS$HOME_ARGS")

;; Close the trap shadow of preceeding instructions.  This is generated
;; by alpha_reorg.

(define_insn "trapb"
  [(unspec_volatile [(const_int 0)] 3)]
  ""
  "trapb"
  [(set_attr "type" "misc")])
