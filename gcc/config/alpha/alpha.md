;; Machine description for DEC Alpha for GNU C compiler
;; Copyright (C) 1992, 1993, 1994, 1995 Free Software Foundation, Inc.
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

;; Define an insn type attribute.  This is used in function unit delay
;; computations, among other purposes.  For the most part, we use the names
;; defined in the EV4 documentation, but add a few that we have to know about
;; separately.

(define_attr "type"
  "ld,st,ibr,fbr,jsr,iaddlog,shiftcm,icmp,imull,imulq,fpop,fdivs,fdivt,ldsym,isubr"
  (const_string "shiftcm"))

;; We include four function units: ABOX, which computes the address,
;; BBOX, used for branches, EBOX, used for integer operations, and FBOX,
;; used for FP operations.
;;
;; We assume that we have been successful in getting double issues and
;; hence multiply all costs by two insns per cycle.  The minimum time in
;; a function unit is 2 cycle, which will tend to produce the double
;; issues.

;; Memory delivers its result in three cycles.
(define_function_unit "abox" 1 0 (eq_attr "type" "ld,ldsym,st") 6 2)

;; Branches have no delay cost, but do tie up the unit for two cycles.
(define_function_unit "bbox" 1 1 (eq_attr "type" "ibr,fbr,jsr") 4 4)

;; Arithmetic insns are normally have their results available after two
;; cycles.  There are a number of exceptions.  They are encoded in
;; ADJUST_COST.  Some of the other insns have similar exceptions.

(define_function_unit "ebox" 1 0 (eq_attr "type" "iaddlog,shiftcm,icmp") 4 2)

;; These really don't take up the integer pipeline, but they do occupy
;; IBOX1; we approximate here.

(define_function_unit "ebox" 1 0 (eq_attr "type" "imull") 42 2)
(define_function_unit "ebox" 1 0 (eq_attr "type" "imulq") 46 2)

(define_function_unit "imult" 1 0 (eq_attr "type" "imull") 42 38)
(define_function_unit "imult" 1 0 (eq_attr "type" "imulq") 46 42)

(define_function_unit "fbox" 1 0 (eq_attr "type" "fpop") 12 2)

(define_function_unit "fbox" 1 0 (eq_attr "type" "fdivs") 68 0)
(define_function_unit "fbox" 1 0 (eq_attr "type" "fdivt") 126 0)

(define_function_unit "divider" 1 0 (eq_attr "type" "fdivs") 68 60)
(define_function_unit "divider" 1 0 (eq_attr "type" "fdivt") 126 118)

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
  [(set_attr "type" "iaddlog,ld,fpop")])

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
   ldah %0,%h2(%r1)"
  [(set_attr "type" "iaddlog")])

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
   subl %r1,%n2,%0"
  [(set_attr "type" "iaddlog")])

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
   ldah %0,%h2(%r1)"
  [(set_attr "type" "iaddlog")])

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
   s%2subl %r1,%n3,%0"
  [(set_attr "type" "iaddlog")])

(define_insn ""
  [(set (match_operand:DI 0 "register_operand" "=r,r")
	(sign_extend:DI
	 (plus:SI (mult:SI (match_operand:SI 1 "reg_or_0_operand" "rJ,rJ")
			   (match_operand:SI 2 "const48_operand" "I,I"))
		  (match_operand:SI 3 "sext_add_operand" "rI,O"))))]
  ""
  "@
   s%2addl %r1,%3,%0
   s%2subl %r1,%n3,%0"
  [(set_attr "type" "iaddlog")])

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
   s%2subq %1,%n3,%0"
  [(set_attr "type" "iaddlog")])

;; These variants of the above insns can occur if the third operand
;; is the frame pointer.  This is a kludge, but there doesn't
;; seem to be a way around it.  Only recognize them while reloading.

(define_insn ""
  [(set (match_operand:DI 0 "some_operand" "=&r")
	(plus:DI (plus:DI (match_operand:DI 1 "some_operand" "r")
			  (match_operand:DI 2 "some_operand" "r"))
		 (match_operand:DI 3 "some_operand" "rIOKL")))]
  "reload_in_progress"
  "#"
  [(set_attr "type" "iaddlog")])

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
  "#"
  [(set_attr "type" "iaddlog")])

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
  "#"
  [(set_attr "type" "iaddlog")])

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
  "#"
  [(set_attr "type" "iaddlog")])

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
  "subl $31,%1,%0"
  [(set_attr "type" "iaddlog")])

(define_insn ""
  [(set (match_operand:DI 0 "register_operand" "=r")
	(sign_extend:DI (neg:SI
			 (match_operand:SI 1 "reg_or_8bit_operand" "rI"))))]
  ""
  "subl $31,%1,%0"
  [(set_attr "type" "iaddlog")])

(define_insn "negdi2"
  [(set (match_operand:DI 0 "register_operand" "=r")
	(neg:DI (match_operand:DI 1 "reg_or_8bit_operand" "rI")))]
  ""
  "subq $31,%1,%0"
  [(set_attr "type" "iaddlog")])

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
  "subl %r1,%2,%0"
  [(set_attr "type" "iaddlog")])

(define_insn ""
  [(set (match_operand:DI 0 "register_operand" "=r")
	(sign_extend:DI (minus:SI (match_operand:SI 1 "reg_or_0_operand" "rJ")
				  (match_operand:SI 2 "reg_or_8bit_operand" "rI"))))]
  ""
  "subl %r1,%2,%0"
  [(set_attr "type" "iaddlog")])

(define_insn "subdi3"
  [(set (match_operand:DI 0 "register_operand" "=r")
	(minus:DI (match_operand:DI 1 "reg_or_0_operand" "rJ")
		  (match_operand:DI 2 "reg_or_8bit_operand" "rI")))]
  ""
  "subq %r1,%2,%0"
  [(set_attr "type" "iaddlog")])

(define_insn ""
  [(set (match_operand:SI 0 "register_operand" "=r")
	(minus:SI (mult:SI (match_operand:SI 1 "reg_or_0_operand" "rJ")
			   (match_operand:SI 2 "const48_operand" "I"))
		  (match_operand:SI 3 "reg_or_8bit_operand" "rI")))]
  ""
  "s%2subl %r1,%3,%0"
  [(set_attr "type" "iaddlog")])

(define_insn ""
  [(set (match_operand:DI 0 "register_operand" "=r")
	(sign_extend:DI
	 (minus:SI (mult:SI (match_operand:SI 1 "reg_or_0_operand" "rJ")
			    (match_operand:SI 2 "const48_operand" "I"))
		   (match_operand:SI 3 "reg_or_8bit_operand" "rI"))))]
  ""
  "s%2subl %r1,%3,%0"
  [(set_attr "type" "iaddlog")])

(define_insn ""
  [(set (match_operand:DI 0 "register_operand" "=r")
	(minus:DI (mult:DI (match_operand:DI 1 "reg_or_0_operand" "rJ")
			   (match_operand:DI 2 "const48_operand" "I"))
		  (match_operand:DI 3 "reg_or_8bit_operand" "rI")))]
  ""
  "s%2subq %r1,%3,%0"
  [(set_attr "type" "iaddlog")])

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
  [(set_attr "type" "imulq")])

(define_insn ""
  [(set (match_operand:DI 0 "register_operand" "=r")
	(truncate:DI
	 (lshiftrt:TI
	  (mult:TI (zero_extend:TI (match_operand:DI 1 "register_operand" "r"))
		   (match_operand:TI 2 "cint8_operand" "I"))
	  (const_int 64))))]
  ""
  "umulh %1,%2,%0"
  [(set_attr "type" "imulq")])

;; The divide and remainder operations always take their inputs from
;; r24 and r25, put their output in r27, and clobber r23 and r28.

(define_expand "divsi3"
  [(set (reg:SI 24) (match_operand:SI 1 "input_operand" ""))
   (set (reg:SI 25) (match_operand:SI 2 "input_operand" ""))
   (parallel [(set (reg:SI 27)
		   (div:SI (reg:SI 24)
			   (reg:SI 25)))
	      (clobber (reg:DI 23))
	      (clobber (reg:DI 28))])
   (set (match_operand:SI 0 "general_operand" "")
	(reg:SI 27))]
  ""
  "")

(define_expand "udivsi3"
  [(set (reg:SI 24) (match_operand:SI 1 "input_operand" ""))
   (set (reg:SI 25) (match_operand:SI 2 "input_operand" ""))
   (parallel [(set (reg:SI 27)
		   (udiv:SI (reg:SI 24)
			    (reg:SI 25)))
	      (clobber (reg:DI 23))
	      (clobber (reg:DI 28))])
   (set (match_operand:SI 0 "general_operand" "")
	(reg:SI 27))]
  ""
  "")

(define_expand "modsi3"
  [(set (reg:SI 24) (match_operand:SI 1 "input_operand" ""))
   (set (reg:SI 25) (match_operand:SI 2 "input_operand" ""))
   (parallel [(set (reg:SI 27)
		   (mod:SI (reg:SI 24)
			   (reg:SI 25)))
	      (clobber (reg:DI 23))
	      (clobber (reg:DI 28))])
   (set (match_operand:SI 0 "general_operand" "")
	(reg:SI 27))]
  ""
  "")

(define_expand "umodsi3"
  [(set (reg:SI 24) (match_operand:SI 1 "input_operand" ""))
   (set (reg:SI 25) (match_operand:SI 2 "input_operand" ""))
   (parallel [(set (reg:SI 27)
		   (umod:SI (reg:SI 24)
			    (reg:SI 25)))
	      (clobber (reg:DI 23))
	      (clobber (reg:DI 28))])
   (set (match_operand:SI 0 "general_operand" "")
	(reg:SI 27))]
  ""
  "")

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
  ""
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
  ""
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
  ""
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
  ""
  "")

(define_insn ""
  [(set (reg:SI 27)
	(match_operator:SI 1 "divmod_operator"
			[(reg:SI 24) (reg:SI 25)]))
   (clobber (reg:DI 23))
   (clobber (reg:DI 28))]
  ""
  "%E1 $24,$25,$27"
  [(set_attr "type" "isubr")])

(define_insn ""
  [(set (reg:DI 27)
	(match_operator:DI 1 "divmod_operator"
			[(reg:DI 24) (reg:DI 25)]))
   (clobber (reg:DI 23))
   (clobber (reg:DI 28))]
  ""
  "%E1 $24,$25,$27"
  [(set_attr "type" "isubr")])

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
  [(set_attr "type" "iaddlog,iaddlog,shiftcm")])

;; There are times when we can split and AND into two AND insns.  This occurs
;; when we can first clear any bytes and then clear anything else.  For
;; example "I & 0xffff07" is "(I & 0xffffff) & 0xffffffffffffff07".
;; Only to this when running on 64-bit host since the computations are
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
  [(set_attr "type" "iaddlog")])

(define_insn "zero_extendqisi2"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(zero_extend:SI (match_operand:QI 1 "register_operand" "r")))]
  ""
  "zapnot %1,1,%0"
  [(set_attr "type" "iaddlog")])

(define_insn "zero_extendqidi2"
  [(set (match_operand:DI 0 "register_operand" "=r")
	(zero_extend:DI (match_operand:QI 1 "register_operand" "r")))]
  ""
  "zapnot %1,1,%0"
  [(set_attr "type" "iaddlog")])

(define_insn "zero_extendhisi2"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(zero_extend:SI (match_operand:HI 1 "register_operand" "r")))]
  ""
  "zapnot %1,3,%0"
  [(set_attr "type" "iaddlog")])

(define_insn "zero_extendhidi2"
  [(set (match_operand:DI 0 "register_operand" "=r")
	(zero_extend:DI (match_operand:HI 1 "register_operand" "r")))]
  ""
  "zapnot %1,3,%0"
  [(set_attr "type" "iaddlog")])

(define_insn "zero_extendsidi2"
  [(set (match_operand:DI 0 "register_operand" "=r")
	(zero_extend:DI (match_operand:SI 1 "register_operand" "r")))]
  ""
  "zapnot %1,15,%0"
  [(set_attr "type" "iaddlog")])

(define_insn  ""
  [(set (match_operand:DI 0 "register_operand" "=r")
	(and:DI (not:DI (match_operand:DI 1 "reg_or_8bit_operand" "rI"))
		(match_operand:DI 2 "reg_or_0_operand" "rJ")))]
  ""
  "bic %r2,%1,%0"
  [(set_attr "type" "iaddlog")])

(define_insn "iordi3"
  [(set (match_operand:DI 0 "register_operand" "=r,r")
	(ior:DI (match_operand:DI 1 "reg_or_0_operand" "%rJ,rJ")
		(match_operand:DI 2 "or_operand" "rI,N")))]
  ""
  "@
   bis %r1,%2,%0
   ornot %r1,%N2,%0"
  [(set_attr "type" "iaddlog")])

(define_insn "one_cmpldi2"
  [(set (match_operand:DI 0 "register_operand" "=r")
	(not:DI (match_operand:DI 1 "reg_or_8bit_operand" "rI")))]
  ""
  "ornot $31,%1,%0"
  [(set_attr "type" "iaddlog")])

(define_insn ""
  [(set (match_operand:DI 0 "register_operand" "=r")
	(ior:DI (not:DI (match_operand:DI 1 "reg_or_8bit_operand" "rI"))
		(match_operand:DI 2 "reg_or_0_operand" "rJ")))]
  ""
  "ornot %r2,%1,%0"
  [(set_attr "type" "iaddlog")])

(define_insn "xordi3"
  [(set (match_operand:DI 0 "register_operand" "=r,r")
	(xor:DI (match_operand:DI 1 "reg_or_0_operand" "%rJ,rJ")
		(match_operand:DI 2 "or_operand" "rI,N")))]
  ""
  "@
   xor %r1,%2,%0
   eqv %r1,%N2,%0"
  [(set_attr "type" "iaddlog")])

(define_insn ""
  [(set (match_operand:DI 0 "register_operand" "=r")
	(not:DI (xor:DI (match_operand:DI 1 "register_operand" "%rJ")
			(match_operand:DI 2 "register_operand" "rI"))))]
  ""
  "eqv %r1,%2,%0"
  [(set_attr "type" "iaddlog")])

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
  [(set_attr "type" "iaddlog,shiftcm")])

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
;;  [(set_attr "type" "iaddlog")])
			  
(define_insn "lshrdi3"
  [(set (match_operand:DI 0 "register_operand" "=r")
	(lshiftrt:DI (match_operand:DI 1 "reg_or_0_operand" "rJ")
		     (match_operand:DI 2 "reg_or_6bit_operand" "rI")))]
  ""
  "srl %r1,%2,%0")

(define_insn "ashrdi3"
  [(set (match_operand:DI 0 "register_operand" "=r")
	(ashiftrt:DI (match_operand:DI 1 "reg_or_0_operand" "rJ")
		     (match_operand:DI 2 "reg_or_6bit_operand" "rI")))]
  ""
  "sra %r1,%2,%0")

(define_expand "extendqihi2"
  [(set (match_dup 2)
	(ashift:DI (match_operand:QI 1 "register_operand" "")
		   (const_int 56)))
   (set (match_operand:HI 0 "register_operand" "")
	(ashiftrt:DI (match_dup 2)
		     (const_int 56)))]
  ""
  "
{ operands[0] = gen_lowpart (DImode, operands[0]);
  operands[1] = gen_lowpart (DImode, operands[1]);
  operands[2] = gen_reg_rtx (DImode);
}")

(define_expand "extendqisi2"
  [(set (match_dup 2)
	(ashift:DI (match_operand:QI 1 "register_operand" "")
		   (const_int 56)))
   (set (match_operand:SI 0 "register_operand" "")
	(ashiftrt:DI (match_dup 2)
		     (const_int 56)))]
  ""
  "
{ operands[0] = gen_lowpart (DImode, operands[0]);
  operands[1] = gen_lowpart (DImode, operands[1]);
  operands[2] = gen_reg_rtx (DImode);
}")

(define_expand "extendqidi2"
  [(set (match_dup 2)
	(ashift:DI (match_operand:QI 1 "register_operand" "")
		   (const_int 56)))
   (set (match_operand:DI 0 "register_operand" "")
	(ashiftrt:DI (match_dup 2)
		     (const_int 56)))]
  ""
  "
{ operands[1] = gen_lowpart (DImode, operands[1]);
  operands[2] = gen_reg_rtx (DImode);
}")

(define_expand "extendhisi2"
  [(set (match_dup 2)
	(ashift:DI (match_operand:HI 1 "register_operand" "")
		   (const_int 48)))
   (set (match_operand:SI 0 "register_operand" "")
	(ashiftrt:DI (match_dup 2)
		     (const_int 48)))]
  ""
  "
{ operands[0] = gen_lowpart (DImode, operands[0]);
  operands[1] = gen_lowpart (DImode, operands[1]);
  operands[2] = gen_reg_rtx (DImode);
}")

(define_expand "extendhidi2"
  [(set (match_dup 2)
	(ashift:DI (match_operand:HI 1 "register_operand" "")
		   (const_int 48)))
   (set (match_operand:DI 0 "register_operand" "")
	(ashiftrt:DI (match_dup 2)
		     (const_int 48)))]
  ""
  "
{ operands[1] = gen_lowpart (DImode, operands[1]);
  operands[2] = gen_reg_rtx (DImode);
}")

(define_insn ""
  [(set (match_operand:DI 0 "register_operand" "=r")
	(zero_extract:DI (match_operand:DI 1 "reg_or_0_operand" "rJ")
			 (match_operand:DI 2 "mode_width_operand" "n")
			 (match_operand:DI 3 "mul8_operand" "I")))]
  ""
  "ext%M2l %r1,%s3,%0")

(define_insn ""
  [(set (match_operand:DI 0 "register_operand" "=r")
	(zero_extract:DI (match_operand:DI 1 "reg_or_0_operand" "rJ")
			 (match_operand:DI 2 "mode_width_operand" "n")
			 (ashift:DI (match_operand:DI 3 "reg_or_8bit_operand" "rI")
				    (const_int 3))))]
  ""
  "ext%M2l %r1,%3,%0")

(define_insn ""
  [(set (match_operand:DI 0 "register_operand" "=r")
	(ashift:DI
	 (zero_extract:DI (match_operand:DI 1 "reg_or_0_operand" "rJ")
			  (const_int 8)
			  (ashift:DI
			   (plus:DI
			    (match_operand:DI 2 "reg_or_8bit_operand" "rI")
			    (const_int -1))
			   (const_int 3)))
	 (const_int 56)))]
  ""
  "extqh %r1,%2,%0")

(define_insn ""
  [(set (match_operand:DI 0 "register_operand" "=r")
	(ashift:DI
	 (zero_extract:DI (match_operand:DI 1 "reg_or_0_operand" "rJ")
			  (const_int 16)
			  (ashift:DI
			   (plus:DI
			    (match_operand:DI 2 "reg_or_8bit_operand" "rI")
			    (const_int -2))
			   (const_int 3)))
	 (const_int 48)))]
  ""
  "extwh %r1,%2,%0")

(define_insn ""
  [(set (match_operand:DI 0 "register_operand" "=r")
	(ashift:DI
	 (zero_extract:DI (match_operand:DI 1 "reg_or_0_operand" "rJ")
			  (const_int 32)
			  (ashift:DI
			   (plus:DI
			    (match_operand:DI 2 "reg_or_8bit_operand" "rI")
			    (const_int -4))
			   (const_int 3)))
	 (const_int 32)))]
  ""
  "extlh %r1,%2,%0")

;; This converts an extXl into an extXh with an appropriate adjustment
;; to the address calculation.

(define_split
  [(set (match_operand:DI 0 "register_operand" "")
	(ashift:DI (zero_extract:DI (match_operand:DI 1 "register_operand" "")
				    (match_operand:DI 2 "mode_width_operand" "")
				    (ashift:DI (match_operand:DI 3 "" "")
					       (const_int 3)))
		   (match_operand:DI 4 "const_int_operand" "")))
   (clobber (match_operand:DI 5 "register_operand" ""))]
  "INTVAL (operands[4]) == 64 - INTVAL (operands[2])"
  [(set (match_dup 5) (match_dup 6))
   (set (match_dup 0)
	(ashift:DI (zero_extract:DI (match_dup 1) (match_dup 2)
				    (ashift:DI (plus:DI (match_dup 5)
							(match_dup 7))
					       (const_int 3)))
		   (match_dup 4)))]
  "
{
  operands[6] = plus_constant (operands[3], 
			       INTVAL (operands[2]) / BITS_PER_UNIT);
  operands[7] = GEN_INT (- INTVAL (operands[2]) / BITS_PER_UNIT);
}")
  
(define_insn ""
  [(set (match_operand:DI 0 "register_operand" "=r")
	(ashift:DI (zero_extend:DI (match_operand:QI 1 "register_operand" "r"))
		   (match_operand:DI 2 "mul8_operand" "I")))]
  ""
  "insbl %1,%s2,%0")

(define_insn ""
  [(set (match_operand:DI 0 "register_operand" "=r")
	(ashift:DI (zero_extend:DI (match_operand:HI 1 "register_operand" "r"))
		   (match_operand:DI 2 "mul8_operand" "I")))]
  ""
  "inswl %1,%s2,%0")

(define_insn ""
  [(set (match_operand:DI 0 "register_operand" "=r")
	(ashift:DI (zero_extend:DI (match_operand:SI 1 "register_operand" "r"))
		   (match_operand:DI 2 "mul8_operand" "I")))]
  ""
  "insll %1,%s2,%0")

(define_insn ""
  [(set (match_operand:DI 0 "register_operand" "=r")
	(ashift:DI (zero_extend:DI (match_operand:QI 1 "register_operand" "r"))
		   (ashift:DI (match_operand:DI 2 "reg_or_8bit_operand" "rI")
			      (const_int 3))))]
  ""
  "insbl %1,%2,%0")

(define_insn ""
  [(set (match_operand:DI 0 "register_operand" "=r")
	(ashift:DI (zero_extend:DI (match_operand:HI 1 "register_operand" "r"))
		   (ashift:DI (match_operand:DI 2 "reg_or_8bit_operand" "rI")
			      (const_int 3))))]
  ""
  "inswl %1,%2,%0")

(define_insn ""
  [(set (match_operand:DI 0 "register_operand" "=r")
	(ashift:DI (zero_extend:DI (match_operand:SI 1 "register_operand" "r"))
		   (ashift:DI (match_operand:DI 2 "reg_or_8bit_operand" "rI")
			      (const_int 3))))]
  ""
  "insll %1,%2,%0")

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
  "msk%U2l %r1,%3,%0")

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
  [(set_attr "type" "fpop")])

(define_insn "absdf2"
  [(set (match_operand:DF 0 "register_operand" "=f")
	(abs:DF (match_operand:DF 1 "reg_or_fp0_operand" "fG")))]
  "TARGET_FP"
  "cpys $f31,%R1,%0"
  [(set_attr "type" "fpop")])

(define_insn "negsf2"
  [(set (match_operand:SF 0 "register_operand" "=f")
	(neg:SF (match_operand:SF 1 "reg_or_fp0_operand" "fG")))]
  "TARGET_FP"
  "cpysn %R1,%R1,%0"
  [(set_attr "type" "fpop")])

(define_insn "negdf2"
  [(set (match_operand:DF 0 "register_operand" "=f")
	(neg:DF (match_operand:DF 1 "reg_or_fp0_operand" "fG")))]
  "TARGET_FP"
  "cpysn %R1,%R1,%0"
  [(set_attr "type" "fpop")])

(define_insn "addsf3"
  [(set (match_operand:SF 0 "register_operand" "=f")
	(plus:SF (match_operand:SF 1 "reg_or_fp0_operand" "%fG")
		 (match_operand:SF 2 "reg_or_fp0_operand" "fG")))]
  "TARGET_FP"
  "adds %R1,%R2,%0"
  [(set_attr "type" "fpop")])

(define_insn "adddf3"
  [(set (match_operand:DF 0 "register_operand" "=f")
	(plus:DF (match_operand:DF 1 "reg_or_fp0_operand" "%fG")
		 (match_operand:DF 2 "reg_or_fp0_operand" "fG")))]
  "TARGET_FP"
  "addt %R1,%R2,%0"
  [(set_attr "type" "fpop")])

(define_insn ""
  [(set (match_operand:DF 0 "register_operand" "=f")
	(plus:DF (float_extend:DF
		  (match_operand:SF 1 "reg_or_fp0_operand" "fG"))
		 (match_operand:DF 2 "reg_or_fp0_operand" "fG")))]
  "TARGET_FP"
  "addt %R1,%R2,%0"
  [(set_attr "type" "fpop")])

(define_insn ""
  [(set (match_operand:DF 0 "register_operand" "=f")
	(plus:DF (float_extend:DF
		  (match_operand:SF 1 "reg_or_fp0_operand" "%fG"))
		 (float_extend:DF
		  (match_operand:SF 2 "reg_or_fp0_operand" "fG"))))]
  "TARGET_FP"
  "addt %R1,%R2,%0"
  [(set_attr "type" "fpop")])

(define_insn "fix_truncdfdi2"
  [(set (match_operand:DI 0 "register_operand" "=f")
	(fix:DI (match_operand:DF 1 "reg_or_fp0_operand" "fG")))]
  "TARGET_FP"
  "cvttqc %R1,%0"
  [(set_attr "type" "fpop")])

(define_insn "fix_truncsfdi2"
  [(set (match_operand:DI 0 "register_operand" "=f")
	(fix:DI (float_extend:DF
		 (match_operand:SF 1 "reg_or_fp0_operand" "fG"))))]
  "TARGET_FP"
  "cvttqc %R1,%0"
  [(set_attr "type" "fpop")])

(define_insn "floatdisf2"
  [(set (match_operand:SF 0 "register_operand" "=f")
	(float:SF (match_operand:DI 1 "register_operand" "f")))]
  "TARGET_FP"
  "cvtqs %1,%0"
  [(set_attr "type" "fpop")])

(define_insn "floatdidf2"
  [(set (match_operand:DF 0 "register_operand" "=f")
	(float:DF (match_operand:DI 1 "register_operand" "f")))]
  "TARGET_FP"
  "cvtqt %1,%0"
  [(set_attr "type" "fpop")])

(define_insn "extendsfdf2"
  [(set (match_operand:DF 0 "register_operand" "=f,f")
	(float_extend:DF (match_operand:SF 1 "nonimmediate_operand" "f,m")))]
  "TARGET_FP"
  "@
   addt $f31,%1,%0
   lds %0,%1"
  [(set_attr "type" "fpop,ld")])

(define_insn "truncdfsf2"
  [(set (match_operand:SF 0 "register_operand" "=f")
	(float_truncate:SF (match_operand:DF 1 "reg_or_fp0_operand" "fG")))]
  "TARGET_FP"
  "cvtts %R1,%0"
  [(set_attr "type" "fpop")])

(define_insn "divsf3"
  [(set (match_operand:SF 0 "register_operand" "=f")
	(div:SF (match_operand:SF 1 "reg_or_fp0_operand" "fG")
		(match_operand:SF 2 "reg_or_fp0_operand" "fG")))]
  "TARGET_FP"
  "divs %R1,%R2,%0"
  [(set_attr "type" "fdivs")])

(define_insn "divdf3"
  [(set (match_operand:DF 0 "register_operand" "=f")
	(div:DF (match_operand:DF 1 "reg_or_fp0_operand" "fG")
		(match_operand:DF 2 "reg_or_fp0_operand" "fG")))]
  "TARGET_FP"
  "divt %R1,%R2,%0"
  [(set_attr "type" "fdivt")])

(define_insn ""
  [(set (match_operand:DF 0 "register_operand" "=f")
	(div:DF (float_extend:DF (match_operand:SF 1 "reg_or_fp0_operand" "fG"))
		(match_operand:DF 2 "reg_or_fp0_operand" "fG")))]
  "TARGET_FP"
  "divt %R1,%R2,%0"
  [(set_attr "type" "fdivt")])

(define_insn ""
  [(set (match_operand:DF 0 "register_operand" "=f")
	(div:DF (match_operand:DF 1 "reg_or_fp0_operand" "fG")
		(float_extend:DF
		 (match_operand:SF 2 "reg_or_fp0_operand" "fG"))))]
  "TARGET_FP"
  "divt %R1,%R2,%0"
  [(set_attr "type" "fdivt")])

(define_insn ""
  [(set (match_operand:DF 0 "register_operand" "=f")
	(div:DF (float_extend:DF (match_operand:SF 1 "reg_or_fp0_operand" "fG"))
		(float_extend:DF (match_operand:SF 2 "reg_or_fp0_operand" "fG"))))]
  "TARGET_FP"
  "divt %R1,%R2,%0"
  [(set_attr "type" "fdivt")])

(define_insn "mulsf3"
  [(set (match_operand:SF 0 "register_operand" "=f")
	(mult:SF (match_operand:SF 1 "reg_or_fp0_operand" "%fG")
		 (match_operand:SF 2 "reg_or_fp0_operand" "fG")))]
  "TARGET_FP"
  "muls %R1,%R2,%0"
  [(set_attr "type" "fpop")])

(define_insn "muldf3"
  [(set (match_operand:DF 0 "register_operand" "=f")
	(mult:DF (match_operand:DF 1 "reg_or_fp0_operand" "%fG")
		 (match_operand:DF 2 "reg_or_fp0_operand" "fG")))]
  "TARGET_FP"
  "mult %R1,%R2,%0"
  [(set_attr "type" "fpop")])

(define_insn ""
  [(set (match_operand:DF 0 "register_operand" "=f")
	(mult:DF (float_extend:DF
		  (match_operand:SF 1 "reg_or_fp0_operand" "fG"))
		 (match_operand:DF 2 "reg_or_fp0_operand" "fG")))]
  "TARGET_FP"
  "mult %R1,%R2,%0"
  [(set_attr "type" "fpop")])

(define_insn ""
  [(set (match_operand:DF 0 "register_operand" "=f")
	(mult:DF (float_extend:DF
		  (match_operand:SF 1 "reg_or_fp0_operand" "%fG"))
		 (float_extend:DF
		  (match_operand:SF 2 "reg_or_fp0_operand" "fG"))))]
  "TARGET_FP"
  "mult %R1,%R2,%0"
  [(set_attr "type" "fpop")])

(define_insn "subsf3"
  [(set (match_operand:SF 0 "register_operand" "=f")
	(minus:SF (match_operand:SF 1 "reg_or_fp0_operand" "fG")
		  (match_operand:SF 2 "reg_or_fp0_operand" "fG")))]
  "TARGET_FP"
  "subs %R1,%R2,%0"
  [(set_attr "type" "fpop")])

(define_insn "subdf3"
  [(set (match_operand:DF 0 "register_operand" "=f")
	(minus:DF (match_operand:DF 1 "reg_or_fp0_operand" "fG")
		  (match_operand:DF 2 "reg_or_fp0_operand" "fG")))]
  "TARGET_FP"
  "subt %R1,%R2,%0"
  [(set_attr "type" "fpop")])

(define_insn ""
  [(set (match_operand:DF 0 "register_operand" "=f")
	(minus:DF (float_extend:DF
		   (match_operand:SF 1 "reg_or_fp0_operand" "fG"))
		  (match_operand:DF 2 "reg_or_fp0_operand" "fG")))]
  "TARGET_FP"
  "subt %R1,%R2,%0"
  [(set_attr "type" "fpop")])

(define_insn ""
  [(set (match_operand:DF 0 "register_operand" "=f")
	(minus:DF (match_operand:DF 1 "reg_or_fp0_operand" "fG")
		  (float_extend:DF
		   (match_operand:SF 2 "reg_or_fp0_operand" "fG"))))]
  "TARGET_FP"
  "subt %R1,%R2,%0"
  [(set_attr "type" "fpop")])

(define_insn ""
  [(set (match_operand:DF 0 "register_operand" "=f")
	(minus:DF (float_extend:DF
		   (match_operand:SF 1 "reg_or_fp0_operand" "fG"))
		  (float_extend:DF
		   (match_operand:SF 2 "reg_or_fp0_operand" "fG"))))]
  "TARGET_FP"
  "subt %R1,%R2,%0"
  [(set_attr "type" "fpop")])

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

;; There are three important special-case that don't fit the above pattern
;; but which we want to handle here.

(define_insn ""
  [(set (match_operand:DI 0 "register_operand" "=r")
	(ne:DI (match_operand:DI 1 "register_operand" "r")
	       (const_int 0)))]
  ""
  "cmpult $31,%1,%0"
  [(set_attr "type" "icmp")])

(define_insn ""
  [(set (match_operand:DI 0 "register_operand" "=r")
	(gt:DI (match_operand:DI 1 "register_operand" "r")
	       (const_int 0)))]
  ""
  "cmplt $31,%1,%0"
  [(set_attr "type" "icmp")])

(define_insn ""
  [(set (match_operand:DI 0 "register_operand" "=r")
	(ge:DI (match_operand:DI 1 "register_operand" "r")
	       (const_int 0)))]
  ""
  "cmple $31,%1,%0"
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
   cmov%d2 %r4,%5,%0")

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
   cmov%d2 %r4,%5,%0")

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
   cmovlbs %r2,%3,%0")

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
   cmovlbc %r2,%3,%0")

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
  "addq %0,%1,%4\;cmov%C2 %r3,%4,%0")

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
  "cmovlt %0,0,%0")

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
  "cmovgt %0,0,%0")

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
  [(set (match_operand:DF 0 "register_operand" "=f")
	(match_operator:DF 1 "alpha_comparison_operator"
			   [(match_operand:DF 2 "reg_or_fp0_operand" "fG")
			    (match_operand:DF 3 "reg_or_fp0_operand" "fG")]))]
  "TARGET_FP"
  "cmpt%C1 %R2,%R3,%0"
  [(set_attr "type" "fpop")])

(define_insn ""
  [(set (match_operand:DF 0 "register_operand" "=f")
	(match_operator:DF 1 "alpha_comparison_operator"
			   [(float_extend:DF
			     (match_operand:SF 2 "reg_or_fp0_operand" "fG"))
			    (match_operand:DF 3 "reg_or_fp0_operand" "fG")]))]
  "TARGET_FP"
  "cmpt%C1 %R2,%R3,%0"
  [(set_attr "type" "fpop")])

(define_insn ""
  [(set (match_operand:DF 0 "register_operand" "=f")
	(match_operator:DF 1 "alpha_comparison_operator"
			   [(match_operand:DF 2 "reg_or_fp0_operand" "fG")
			    (float_extend:DF
			     (match_operand:SF 3 "reg_or_fp0_operand" "fG"))]))]
  "TARGET_FP"
  "cmpt%C1 %R2,%R3,%0"
  [(set_attr "type" "fpop")])

(define_insn ""
  [(set (match_operand:DF 0 "register_operand" "=f")
	(match_operator:DF 1 "alpha_comparison_operator"
			   [(float_extend:DF
			     (match_operand:SF 2 "reg_or_fp0_operand" "fG"))
			    (float_extend:DF
			     (match_operand:SF 3 "reg_or_fp0_operand" "fG"))]))]
  "TARGET_FP"
  "cmpt%C1 %R2,%R3,%0"
  [(set_attr "type" "fpop")])

(define_insn ""
  [(set (match_operand:DF 0 "register_operand" "=f,f")
	(if_then_else:DF 
	 (match_operator 3 "signed_comparison_operator"
			 [(match_operand:DF 4 "reg_or_fp0_operand" "fG,fG")
			  (match_operand:DF 2 "fp0_operand" "G,G")])
	 (match_operand:DF 1 "reg_or_fp0_operand" "fG,0")
	 (match_operand:DF 5 "reg_or_fp0_operand" "0,fG")))]
  "TARGET_FP"
  "@
   fcmov%C3 %R4,%R1,%0
   fcmov%D3 %R4,%R5,%0"
  [(set_attr "type" "fpop")])

(define_insn ""
  [(set (match_operand:SF 0 "register_operand" "=f,f")
	(if_then_else:SF 
	 (match_operator 3 "signed_comparison_operator"
			 [(match_operand:DF 4 "reg_or_fp0_operand" "fG,fG")
			  (match_operand:DF 2 "fp0_operand" "G,G")])
	 (match_operand:SF 1 "reg_or_fp0_operand" "fG,0")
	 (match_operand:SF 5 "reg_or_fp0_operand" "0,fG")))]
  "TARGET_FP"
  "@
   fcmov%C3 %R4,%R1,%0
   fcmov%D3 %R4,%R5,%0"
  [(set_attr "type" "fpop")])

(define_insn ""
  [(set (match_operand:DF 0 "register_operand" "=f,f")
	(if_then_else:DF 
	 (match_operator 3 "signed_comparison_operator"
			 [(match_operand:DF 1 "reg_or_fp0_operand" "fG,fG")
			  (match_operand:DF 2 "fp0_operand" "G,G")])
	 (float_extend:DF (match_operand:SF 4 "reg_or_fp0_operand" "fG,0"))
	 (match_operand:DF 5 "reg_or_fp0_operand" "0,fG")))]
  "TARGET_FP"
  "@
   fcmov%C3 %R4,%R1,%0
   fcmov%D3 %R4,%R5,%0"
  [(set_attr "type" "fpop")])

(define_insn ""
  [(set (match_operand:DF 0 "register_operand" "=f,f")
	(if_then_else:DF 
	 (match_operator 3 "signed_comparison_operator"
			 [(float_extend:DF 
			   (match_operand:SF 4 "reg_or_fp0_operand" "fG,fG"))
			  (match_operand:DF 2 "fp0_operand" "G,G")])
	 (match_operand:DF 1 "reg_or_fp0_operand" "fG,0")
	 (match_operand:DF 5 "reg_or_fp0_operand" "0,fG")))]
  "TARGET_FP"
  "@
   fcmov%C3 %R4,%R1,%0
   fcmov%D3 %R4,%R5,%0"
  [(set_attr "type" "fpop")])

(define_insn ""
  [(set (match_operand:SF 0 "register_operand" "=f,f")
	(if_then_else:SF 
	 (match_operator 3 "signed_comparison_operator"
			 [(float_extend:DF
			   (match_operand:SF 4 "reg_or_fp0_operand" "fG,fG"))
			  (match_operand:DF 2 "fp0_operand" "G,G")])
	 (match_operand:SF 1 "reg_or_fp0_operand" "fG,0")
	 (match_operand:SF 5 "reg_or_fp0_operand" "0,fG")))]
  "TARGET_FP"
  "@
   fcmov%C3 %R4,%R1,%0
   fcmov%D3 %R4,%R5,%0"
  [(set_attr "type" "fpop")])

(define_insn ""
  [(set (match_operand:DF 0 "register_operand" "=f,f")
	(if_then_else:DF 
	 (match_operator 3 "signed_comparison_operator"
			 [(float_extend:DF
			   (match_operand:SF 4 "reg_or_fp0_operand" "fG,fG"))
			  (match_operand:DF 2 "fp0_operand" "G,G")])
	 (float_extend:DF (match_operand:SF 1 "reg_or_fp0_operand" "fG,0"))
	 (match_operand:DF 5 "reg_or_fp0_operand" "0,fG")))]
  "TARGET_FP"
  "@
   fcmov%C3 %R4,%R1,%0
   fcmov%D3 %R4,%R5,%0"
  [(set_attr "type" "fpop")])

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
  enum rtx_code compare_code, branch_code;

  if (alpha_compare_fp_p)
    mode = DFmode, compare_code = EQ, branch_code = NE;
  else
    {
      mode = DImode, compare_code = MINUS, branch_code = EQ;
      if (GET_CODE (alpha_compare_op1) == CONST_INT)
	{
	  compare_code = PLUS;
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
  enum rtx_code compare_code, branch_code;

  if (alpha_compare_fp_p)
    mode = DFmode, compare_code = EQ, branch_code = EQ;
  else
    {
      mode = DImode, compare_code = MINUS, branch_code = NE;
      if (GET_CODE (alpha_compare_op1) == CONST_INT)
	{
	  compare_code = PLUS;
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
  [(set (match_dup 4) (match_operand 1 "comparison_operator" ""))
   (set (match_operand:SI 0 "register_operand" "")
	(if_then_else:DI (match_dup 5)
			 (match_operand:SI 2 "reg_or_8bit_operand" "")
			 (match_operand:SI 3 "reg_or_8bit_operand" "")))]
  ""
  "
{
  rtx op0,op1;
  enum rtx_code code = GET_CODE (operands[1]), code2 = NE;

  if (alpha_compare_fp_p)
    FAIL;
  switch (code)
    {
    case EQ: case LE: case LT:
      op0 = alpha_compare_op0;
      op1 = alpha_compare_op1;
      break;
    case NE:
      code = code2 = EQ;
      op0 = alpha_compare_op0;
      op1 = alpha_compare_op1;
      break;
    case GE:
      code = LE;
      op0 = force_reg (DImode, alpha_compare_op1);
      op1 = alpha_compare_op0;
      break;
    case GT:
      code = LT;
      op0 = force_reg (DImode, alpha_compare_op1);
      op1 = alpha_compare_op0;
      break;
    default:
      FAIL;
    }
  operands[1] = gen_rtx (code, DImode, op0, op1);
  operands[4] = gen_reg_rtx (DImode);
  operands[5] = gen_rtx (code2, VOIDmode, operands[4], CONST0_RTX (DImode));
}")

(define_expand "movdicc"
  [(set (match_dup 4) (match_operand 1 "comparison_operator" ""))
   (set (match_operand:DI 0 "register_operand" "")
	(if_then_else:DI (match_dup 5)
			 (match_operand:DI 2 "reg_or_8bit_operand" "")
			 (match_operand:DI 3 "reg_or_8bit_operand" "")))]
  ""
  "
{
  rtx op0,op1;
  enum rtx_code code = GET_CODE (operands[1]), code2 = NE;

  if (alpha_compare_fp_p)
    FAIL;
  switch (code)
    {
    case EQ: case LE: case LT:
      op0 = alpha_compare_op0;
      op1 = alpha_compare_op1;
      break;
    case NE:
      code = code2 = EQ;
      op0 = alpha_compare_op0;
      op1 = alpha_compare_op1;
      break;
    case GE:
      code = LE;
      op0 = force_reg (DImode, alpha_compare_op1);
      op1 = alpha_compare_op0;
      break;
    case GT:
      code = LT;
      op0 = force_reg (DImode, alpha_compare_op1);
      op1 = alpha_compare_op0;
      break;
    default:
      FAIL;
    }
  operands[1] = gen_rtx (code, DImode, op0, op1);
  operands[4] = gen_reg_rtx (DImode);
  operands[5] = gen_rtx (code2, VOIDmode, operands[4], CONST0_RTX (DImode));
}")

(define_expand "movsfcc"
  [(set (match_dup 4) (match_operand 1 "comparison_operator" ""))
   (set (match_operand:SF 0 "register_operand" "")
	(if_then_else:SF (match_dup 5)
			 (match_operand:SF 2 "reg_or_fp0_operand" "")
			 (match_operand:SF 3 "reg_or_fp0_operand" "")))]
  ""
  "
{
  rtx op0,op1;
  enum rtx_code code = GET_CODE (operands[1]), code2 = NE;

  if (!alpha_compare_fp_p)
    FAIL;
  switch (code)
    {
    case EQ: case LE: case LT:
      op0 = alpha_compare_op0;
      op1 = alpha_compare_op1;
      break;
    case NE:
      /* There isn't a cmptne insn.  */
      code = code2 = EQ;
      op0 = alpha_compare_op0;
      op1 = alpha_compare_op1;
      break;
    case GE:
      code = LE;
      op0 = force_reg (DFmode, alpha_compare_op1);
      op1 = alpha_compare_op0;
      break;
    case GT:
      code = LT;
      op0 = force_reg (DFmode, alpha_compare_op1);
      op1 = alpha_compare_op0;
      break;
    default:
      FAIL;
    }
  operands[1] = gen_rtx (code, DFmode, op0, op1);
  operands[4] = gen_reg_rtx (DFmode);
  operands[5] = gen_rtx (code2, VOIDmode, operands[4], CONST0_RTX (DFmode));
}")

(define_expand "movdfcc"
  [(set (match_dup 4) (match_operand 1 "comparison_operator" ""))
   (set (match_operand:DF 0 "register_operand" "")
	(if_then_else:DF (match_dup 5)
			 (match_operand:DF 2 "reg_or_fp0_operand" "")
			 (match_operand:DF 3 "reg_or_fp0_operand" "")))]
  ""
  "
{
  rtx op0,op1;
  enum rtx_code code = GET_CODE (operands[1]), code2 = NE;

  if (!alpha_compare_fp_p)
    FAIL;
  switch (code)
    {
    case EQ: case LE: case LT:
      op0 = alpha_compare_op0;
      op1 = alpha_compare_op1;
      break;
    case NE:
      /* There isn't a cmptne insn.  */
      code = code2 = EQ;
      op0 = alpha_compare_op0;
      op1 = alpha_compare_op1;
      break;
    case GE:
      code = LE;
      op0 = force_reg (DFmode, alpha_compare_op1);
      op1 = alpha_compare_op0;
      break;
    case GT:
      code = LT;
      op0 = force_reg (DFmode, alpha_compare_op1);
      op1 = alpha_compare_op0;
      break;
    default:
      FAIL;
    }
  operands[1] = gen_rtx (code, DFmode, op0, op1);
  operands[4] = gen_reg_rtx (DFmode);
  operands[5] = gen_rtx (code2, VOIDmode, operands[4], CONST0_RTX (DFmode));
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
   (use (match_operand 1 "" ""))]
  ""
  "
{ if (WINDOWS_NT)
    emit_call_insn (gen_call_nt (operands[0], operands[1]));
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
  [(parallel [(call (mem:DI (match_operand:DI 0 "" ""))
		    (match_operand 1 "" ""))
	      (clobber (reg:DI 26))])]
  ""
  "
{ if (GET_CODE (operands[0]) != MEM)
    abort ();
  operands[0] = XEXP (operands[0], 0);

  if (GET_CODE (operands[1]) != SYMBOL_REF
      && ! (GET_CODE (operands[1]) == REG && REGNO (operands[1]) == 27))
    {
      rtx tem = gen_rtx (REG, DImode, 27);
      emit_move_insn (tem, operands[1]);
      operands[1] = tem;
    }
}")

(define_expand "call_value"
  [(use (match_operand 0 "" ""))
   (use (match_operand:DI 1 "" ""))
   (use (match_operand 2 "" ""))]
  ""
  "
{ if (WINDOWS_NT)
    emit_call_insn (gen_call_value_nt (operands[0], operands[1], operands[2]));
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
		   (call (mem:DI (match_operand:DI 1 "" ""))
			 (match_operand 2 "" "")))
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

(define_insn ""
  [(call (mem:DI (match_operand:DI 0 "call_operand" "r,R,i"))
	 (match_operand 1 "" ""))
   (clobber (reg:DI 27))
   (clobber (reg:DI 26))]
  "! WINDOWS_NT"
  "@
   jsr $26,($27),0\;ldgp $29,0($26)
   bsr $26,%0..ng
   jsr $26,%0\;ldgp $29,0($26)"
  [(set_attr "type" "jsr,jsr,ibr")])
      
(define_insn ""
  [(call (mem:DI (match_operand:DI 0 "call_operand" "r,i"))
	 (match_operand 1 "" ""))
   (clobber (reg:DI 26))]
  "WINDOWS_NT"
  "@
   jsr $26,(%0)
   bsr $26,%0"
  [(set_attr "type" "jsr")])
      
(define_insn ""
  [(set (match_operand 0 "register_operand" "=rf,rf,rf")
	(call (mem:DI (match_operand:DI 1 "call_operand" "r,R,i"))
	      (match_operand 2 "" "")))
   (clobber (reg:DI 27))
   (clobber (reg:DI 26))]
  "! WINDOWS_NT"
  "@
   jsr $26,($27),0\;ldgp $29,0($26)
   bsr $26,%1..ng
   jsr $26,%1\;ldgp $29,0($26)"
  [(set_attr "type" "jsr,jsr,ibr")])

(define_insn ""
  [(set (match_operand 0 "register_operand" "=rf,rf")
	(call (mem:DI (match_operand:DI 1 "call_operand" "r,i"))
	      (match_operand 2 "" "")))
   (clobber (reg:DI 26))]
  "WINDOWS_NT"
  "@
   jsr $26,(%1)
   bsr $26,%1"
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
  [(set_attr "type" "iaddlog")])

(define_expand "tablejump"
  [(use (match_operand:SI 0 "register_operand" ""))
   (use (match_operand:SI 1 "" ""))]
  ""
  "
{
  if (WINDOWS_NT)
    emit_jump_insn (gen_tablejump_nt (operands[0], operands[1]));
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

(define_insn ""
  [(set (pc)
	(plus:DI (match_operand:DI 0 "register_operand" "r")
		 (label_ref:DI (match_operand 1 "" ""))))
   (clobber (match_scratch:DI 2 "=r"))]
  "! WINDOWS_NT && next_active_insn (insn) != 0
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
  "WINDOWS_NT && next_active_insn (insn) != 0
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

;; Cache flush.  Used by INITIALIZE_TRAMPOLINE.  0x86 is PAL_imb, but we don't
;; want to have to include pal.h in our .s file.
(define_insn ""
  [(unspec_volatile [(const_int 0)] 0)]
  ""
  "call_pal 0x86")

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
   lds %0,%1
   sts %R1,%0"
  [(set_attr "type" "iaddlog,ld,st,fpop,fpop,ld,st")])

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
   ldt %0,%1
   stt %R1,%0"
  [(set_attr "type" "iaddlog,ld,st,fpop,fpop,ld,st")])

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
  "! WINDOWS_NT && (register_operand (operands[0], SImode)
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
  [(set_attr "type" "iaddlog,iaddlog,iaddlog,iaddlog,iaddlog,ld,st,fpop,fpop,ld,st")])

(define_insn ""
  [(set (match_operand:SI 0 "nonimmediate_operand" "=r,r,r,r,r,r,r,m,f,f,f,m")
	(match_operand:SI 1 "input_operand" "r,J,I,K,L,s,m,rJ,f,J,m,fG"))]
  "WINDOWS_NT && (register_operand (operands[0], SImode)
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
  [(set_attr "type" "iaddlog,iaddlog,iaddlog,iaddlog,iaddlog,ldsym,ld,st,fpop,fpop,ld,st")])

(define_insn ""
  [(set (match_operand:HI 0 "nonimmediate_operand" "=r,r,r,r,f,f")
	(match_operand:HI 1 "input_operand" "r,J,I,n,f,J"))]
  "register_operand (operands[0], HImode)
   || register_operand (operands[1], HImode)"
  "@
   bis %1,%1,%0
   bis $31,$31,%0
   bis $31,%1,%0
   lda %0,%L1
   cpys %1,%1,%0
   cpys $f31,$f31,%0"
  [(set_attr "type" "iaddlog,iaddlog,iaddlog,iaddlog,fpop,fpop")])

(define_insn ""
  [(set (match_operand:QI 0 "nonimmediate_operand" "=r,r,r,r,f,f")
	(match_operand:QI 1 "input_operand" "r,J,I,n,f,J"))]
  "register_operand (operands[0], QImode)
   || register_operand (operands[1], QImode)"
  "@
   bis %1,%1,%0
   bis $31,$31,%0
   bis $31,%1,%0
   lda %0,%L1
   cpys %1,%1,%0
   cpys $f31,$f31,%0"
  [(set_attr "type" "iaddlog,iaddlog,iaddlog,iaddlog,fpop,fpop")])

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
  [(set_attr "type" "iaddlog,iaddlog,iaddlog,iaddlog,iaddlog,ldsym,ld,st,fpop,fpop,ld,st")])

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
  
;; Similar for unaligned loads.  For QImode, we use the sequence from the
;; Alpha Architecture manual.  However, for HImode, we do not.  HImode pointers
;; are normally aligned to the byte boundary, so an HImode object cannot
;; cross a longword boundary.  We could use a sequence similar to that for
;; QImode, but that would fail if the pointer, was, in fact, not aligned.
;; Instead, we clear bit 1 in the address and do an ldl.  If the low-order
;; bit was not aligned, this will trap and the trap handler will do what is
;; needed.
;;
;; Here operand 1 is the address.  Operands 2 and 3 are temporaries, where
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

;; For this, the address must already be in a register.  We also need two
;; DImode temporaries, neither of which may overlap the input (and hence the
;; output, since they might be the same register), but both of which may
;; be the same.

(define_expand "unaligned_loadhi"
  [(set (match_operand:DI 2 "register_operand" "")
	(and:DI (match_operand:DI 1 "register_operand" "")
		(const_int -7)))
   (set (match_operand:DI 3 "register_operand" "")
	(mem:DI (match_dup 2)))
   (set (match_operand:DI 4 "register_operand" "")
	(and:DI (match_dup 1) (const_int -2)))
   (set (subreg:DI (match_operand:HI 0 "register_operand" "") 0)
	(zero_extract:DI (match_dup 3)
			 (const_int 16)
			 (ashift:DI (match_dup 4) (const_int 3))))]
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

;; For the unaligned byte case, we use code similar to that in the
;; Architecture book, but reordered to lower the number of registers
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

;; This is the code for storing into an unaligned short.  It uses the same
;; trick as loading from an unaligned short.  It needs lots of temporaries.
;; However, during reload, we only have two registers available.  So we
;; repeat code so that only two temporaries are available.  During RTL
;; generation, we can use different pseudos for each temporary and CSE
;; will remove the redundancies.  During reload, we have to settle with
;; what we get.  Luckily, unaligned accesses of this kind produced during
;; reload are quite rare.
;;
;; Operand 0 is the address of the memory location.  Operand 1 contains the
;; data to store.  The rest of the operands are all temporaries, with
;; various overlap possibilities during reload.  See reload_outhi for
;; details of this use.

(define_expand "unaligned_storehi"
  [(set (match_operand:DI 2 "register_operand" "")
	(match_operand:DI 0 "address_operand" ""))
   (set (match_operand:DI 3 "register_operand" "")
	(and:DI (match_dup 2) (const_int -7)))
   (set (match_operand:DI 4 "register_operand" "")
	(mem:DI (match_dup 3)))
   (set (match_operand:DI 10 "register_operand" "")
	(and:DI (match_dup 2) (const_int -2)))
   (set (match_operand:DI 5 "register_operand" "")
	(and:DI (not:DI (ashift:DI (const_int 65535)
				   (ashift:DI (match_dup 10) (const_int 3))))
		(match_dup 4)))
   (set (match_operand:DI 6 "register_operand" "")
	(ashift:DI (zero_extend:DI (match_operand:HI 1 "register_operand" ""))
		   (ashift:DI (match_dup 10) (const_int 3))))
   (set (match_operand:DI 7 "register_operand" "")
	(ior:DI (match_dup 5) (match_dup 6)))
   (set (match_operand:DI 8 "register_operand" "") (match_dup 0))
   (set (match_operand:DI 9 "register_operand" "")
	(and:DI (match_dup 8) (const_int -7)))
   (set (mem:DI (match_dup 9)) (match_dup 7))]
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
	  rtx seq = gen_unaligned_loadqi (operands[0],
					  get_unaligned_address (operands[1]),
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
	  rtx seq = gen_unaligned_storeqi (get_unaligned_address (operands[0]),
					   operands[1], temp1, temp2, temp3);

	  alpha_set_memflags (seq, operands[0]);
	  emit_insn (seq);
	}
      DONE;
    }
}")

(define_expand "movhi"
  [(set (match_operand:HI 0 "general_operand" "")
	(match_operand:HI 1 "general_operand" ""))]
  ""
  "
{ extern rtx get_unaligned_address ();

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

	  get_aligned_mem (operands[1], &aligned_mem, &bitnum);

	  emit_insn (gen_aligned_loadhi (operands[0], aligned_mem, bitnum,
					 scratch));
	}
      else
	{
	  rtx addr
	    = force_reg (DImode,
			 force_operand (get_unaligned_address (operands[1]),
					NULL_RTX));
	  rtx scratch1 = gen_reg_rtx (DImode);
	  rtx scratch2 = gen_reg_rtx (DImode);
	  rtx scratch3 = gen_reg_rtx (DImode);

	  rtx seq = gen_unaligned_loadhi (operands[0], addr, scratch1,
					  scratch2, scratch3);

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
	  rtx temp4 = gen_reg_rtx (DImode);
	  rtx temp5 = gen_reg_rtx (DImode);
	  rtx temp6 = gen_reg_rtx (DImode);
	  rtx temp7 = gen_reg_rtx (DImode);
	  rtx temp8 = gen_reg_rtx (DImode);
	  rtx temp9 = gen_reg_rtx (DImode);

	  rtx seq = gen_unaligned_storehi (get_unaligned_address (operands[0]),
					   operands[1], temp1, temp2,temp3,
					   temp4, temp5, temp6,temp7,
					   temp8, temp9);

	  alpha_set_memflags (seq, operands[0]);
	  emit_insn (seq);
	}

      DONE;
    }
}")

;; Here are the versions for reload.  Note that in the unaligned cases
;; we know that the operand must not be a pseudo-register because stack
;; slots are always aligned references.

(define_expand "reload_inqi"
  [(parallel [(match_operand:QI 0 "register_operand" "=r")
	      (match_operand:QI 1 "unaligned_memory_operand" "m")
	      (match_operand:TI 2 "register_operand" "=&r")])]
  ""
  "
{ extern rtx get_unaligned_address ();
  rtx addr = get_unaligned_address (operands[1]);
  /* It is possible that one of the registers we got for operands[2]
     might coincide with that of operands[0] (which is why we made
     it TImode).  Pick the other one to use as our scratch.  */
  rtx scratch = gen_rtx (REG, DImode,
			 REGNO (operands[0]) == REGNO (operands[2]) 
			 ? REGNO (operands[2]) + 1 : REGNO (operands[2]));
  rtx seq = gen_unaligned_loadqi (operands[0], addr, scratch,
				  gen_rtx (REG, DImode, REGNO (operands[0])));

  alpha_set_memflags (seq, operands[1]);
  emit_insn (seq);
  DONE;
}")

(define_expand "reload_inhi"
  [(parallel [(match_operand:HI 0 "register_operand" "=r")
	      (match_operand:HI 1 "unaligned_memory_operand" "m")
	      (match_operand:TI 2 "register_operand" "=&r")])]
  ""
  "
{ extern rtx get_unaligned_address ();
  rtx addr = get_unaligned_address (operands[1]);
  rtx scratch1 = gen_rtx (REG, DImode, REGNO (operands[2]));
  rtx scratch2 = gen_rtx (REG, DImode, REGNO (operands[2]) + 1);
  rtx seq;

  if (GET_CODE (addr) != REG)
    {
      emit_insn (gen_rtx (SET, VOIDmode, scratch2, addr));
      addr = scratch2;
    }
      
  seq = gen_unaligned_loadhi (operands[0], addr, scratch1, scratch1, scratch2);
  alpha_set_memflags (seq, operands[1]);
  emit_insn (seq);
  DONE;
}")

(define_expand "reload_outqi"
  [(parallel [(match_operand:QI 0 "any_memory_operand" "=m")
	      (match_operand:QI 1 "register_operand" "r")
	      (match_operand:TI 2 "register_operand" "=&r")])]
  ""
  "
{ extern rtx get_unaligned_address ();

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
      rtx addr = get_unaligned_address (operands[0]);
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
  ""
  "
{ extern rtx get_unaligned_address ();

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
      rtx addr = get_unaligned_address (operands[0]);
      rtx scratch1 = gen_rtx (REG, DImode, REGNO (operands[2]));
      rtx scratch2 = gen_rtx (REG, DImode, REGNO (operands[2]) + 1);
      rtx scratch_a = GET_CODE (addr) == REG ? addr : scratch1;
      rtx seq;

      seq = gen_unaligned_storehi (addr, operands[1], scratch_a,
				   scratch2, scratch2, scratch2,
				   scratch1, scratch2, scratch_a,
				   scratch1, scratch_a);
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
