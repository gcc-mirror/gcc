;;- Machine description for DEC Alpha for GNU C compiler
;;   Copyright (C) 1992 Free Software Foundation, Inc.
;;   Contributed by Richard Kenner (kenner@nyu.edu)

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

;; Define an insn type attribute.  This is used in function unit delay
;; computations, among other purposes.  For the most part, we use the names
;; defined in the EV4 documentation, but add a few that we have to know about
;; separately.

(define_attr "type"
  "ld,st,ibr,fbr,jsr,iaddlog,shiftcm,icmp,imull,imulq,fpop,fdivs,fdivt,ldsym"
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
;; now, don't let the FP case show up here for preferencing.
(define_insn "extendsidi2"
  [(set (match_operand:DI 0 "register_operand" "=r,r,f")
	(sign_extend:DI (match_operand:SI 1 "nonimmediate_operand" "r,m,f")))]
  ""
  "@
   addl %1,$31,%0
   ldl %0,%1
   cvtlq %1,%0"
  [(set_attr "type" "iaddlog,ld,fpop")])

(define_insn "addsi3"
  [(set (match_operand:SI 0 "register_operand" "=r,r,r")
	(plus:SI (match_operand:SI 1 "reg_or_0_operand" "%rJ,%rJ,%rJ")
		 (match_operand:SI 2 "add_operand" "rI,K,L")))]
  ""
  "@
   addl %r1,%2,%0
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

(define_insn "adddi3"
  [(set (match_operand:DI 0 "register_operand" "=r,r,r")
	(plus:DI (match_operand:DI 1 "reg_or_0_operand" "%rJ,%rJ,%rJ")
		 (match_operand:DI 2 "add_operand" "rI,K,L")))]
  ""
  "@
   addq %r1,%2,%0
   lda %0,%2(%r1)
   ldah %0,%h2(%r1)"
  [(set_attr "type" "iaddlog")])

(define_split
  [(set (match_operand:DI 0 "register_operand" "")
	(plus:DI (match_operand:DI 1 "register_operand" "")
		 (match_operand:DI 2 "const_int_operand" "")))]
  "! add_operand (operands[2], DImode)"
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
  [(set (match_operand:SI 0 "register_operand" "=r")
	(plus:SI (mult:SI (match_operand:SI 1 "reg_or_0_operand" "rJ")
			  (match_operand:SI 2 "const48_operand" "I"))
		 (match_operand:SI 3 "reg_or_8bit_operand" "rI")))]
  ""
  "s%2addl %r1,%3,%0"
  [(set_attr "type" "iaddlog")])

(define_insn ""
  [(set (match_operand:DI 0 "register_operand" "=r")
	(sign_extend:DI
	 (plus:SI (mult:SI (match_operand:SI 1 "reg_or_0_operand" "rJ")
			   (match_operand:SI 2 "const48_operand" "I"))
		  (match_operand:SI 3 "reg_or_8bit_operand" "rI"))))]
  ""
  "s%2addl %r1,%3,%0"
  [(set_attr "type" "iaddlog")])

(define_insn ""
  [(set (match_operand:DI 0 "register_operand" "=r")
	(plus:DI (mult:DI (match_operand:DI 1 "reg_or_0_operand" "rJ")
			  (match_operand:DI 2 "const48_operand" "I"))
		 (match_operand:DI 3 "reg_or_8bit_operand" "rI")))]
  ""
  "s%2addq %r1,%3,%0"
  [(set_attr "type" "iaddlog")])

;; These variants of the above insns can occur if the third operand
;; is the frame pointer.  This is a kludge, but there doesn't
;; seem to be a way around it.  Only recognize them while reloading.

(define_insn ""
  [(set (match_operand:SI 0 "register_operand" "=&r")
	(plus:SI (plus:SI (mult:SI (match_operand:SI 1 "reg_or_0_operand" "rJ")
				   (match_operand:SI 2 "const48_operand" "I"))
			  (match_operand:SI 3 "register_operand" "r"))
		 (match_operand:SI 4 "const_int_operand" "rI")))]
  "reload_in_progress"
  "s%2addl %r1,%3,%0\;addl %0,%4,%0"
  [(set_attr "type" "iaddlog")])

(define_insn ""
  [(set (match_operand:DI 0 "register_operand" "=&r")
	(sign_extend:DI
	 (plus:SI (plus:SI
		   (mult:SI (match_operand:SI 1 "reg_or_0_operand" "rJ")
			    (match_operand:SI 2 "const48_operand" "I"))
		   (match_operand:SI 3 "register_operand" "r"))
		  (match_operand:SI 4 "const_int_operand" "rI"))))]
  "reload_in_progress"
  "s%2addl %r1,%3,%0\;addl %0,%4,%0"
  [(set_attr "type" "iaddlog")])

(define_insn ""
  [(set (match_operand:DI 0 "register_operand" "=&r")
	(plus:DI (plus:DI (mult:DI (match_operand:DI 1 "reg_or_0_operand" "rJ")
				   (match_operand:DI 2 "const48_operand" "I"))
			  (match_operand:DI 3 "register_operand" "r"))
		 (match_operand:DI 4 "const_int_operand" "rI")))]
  "reload_in_progress"
  "s%2addq %r1,%3,%0\;addq %0,%4,%0"
  [(set_attr "type" "iaddlog")])

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

(define_insn "subsi3"
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
		 (match_operand:SI 2 "reg_or_8bit_operand" "rI")))]
  ""
  "mull %r1,%2,%0"
  [(set_attr "type" "imull")])

(define_insn ""
  [(set (match_operand:DI 0 "register_operand" "=r")
	(sign_extend:DI (mult:SI (match_operand:SI 1 "reg_or_0_operand" "%rJ")
				 (match_operand:SI 2 "reg_or_8bit_operand" "rI"))))]
  ""
  "mull %r1,%2,%0"
  [(set_attr "type" "imull")])

(define_insn "muldi3"
  [(set (match_operand:DI 0 "register_operand" "=r")
	(mult:DI (match_operand:DI 1 "reg_or_0_operand" "%rJ")
		 (match_operand:DI 2 "reg_or_8bit_operand" "rI")))]
  ""
  "mulq %r1,%2,%0"
  [(set_attr "type" "imulq")])

;; The divide and remainder operations always take their inputs from
;; r24 and r25, put their output in r27, and clobber r23.

(define_expand "divsi3"
  [(parallel [(set (reg:SI 27)
		   (div:SI (match_operand:SI 1 "general_operand" "")
			   (match_operand:SI 2 "general_operand" "")))
	      (clobber (reg:DI 23))])
   (set (match_operand:SI 0 "general_operand" "")
	(reg:SI 27))]
  ""
  "
{ rtx in0 = gen_rtx (REG, SImode, 24);
  rtx in1 = gen_rtx (REG, SImode, 25);

  emit_move_insn (in0, operands[1]);
  emit_move_insn (in1, operands[2]);
  operands[1] = in0, operands[2] = in1;
}")

(define_expand "udivsi3"
  [(parallel [(set (reg:SI 27)
		   (udiv:SI (match_operand:SI 1 "general_operand" "")
			    (match_operand:SI 2 "general_operand" "")))
	      (clobber (reg:DI 23))])
   (set (match_operand:SI 0 "general_operand" "")
	(reg:SI 27))]
  ""
  "
{ rtx in0 = gen_rtx (REG, SImode, 24);
  rtx in1 = gen_rtx (REG, SImode, 25);

  emit_move_insn (in0, operands[1]);
  emit_move_insn (in1, operands[2]);
  operands[1] = in0, operands[2] = in1;
}")

(define_expand "modsi3"
  [(parallel [(set (reg:SI 27)
		   (mod:SI (match_operand:SI 1 "general_operand" "")
			   (match_operand:SI 2 "general_operand" "")))
	      (clobber (reg:DI 23))])
   (set (match_operand:SI 0 "general_operand" "")
	(reg:SI 27))]
  ""
  "
{ rtx in0 = gen_rtx (REG, SImode, 24);
  rtx in1 = gen_rtx (REG, SImode, 25);

  emit_move_insn (in0, operands[1]);
  emit_move_insn (in1, operands[2]);
  operands[1] = in0, operands[2] = in1;
}")

(define_expand "umodsi3"
  [(parallel [(set (reg:SI 27)
		   (umod:SI (match_operand:SI 1 "general_operand" "")
			    (match_operand:SI 2 "general_operand" "")))
	      (clobber (reg:DI 23))])
   (set (match_operand:SI 0 "general_operand" "")
	(reg:SI 27))]
  ""
  "
{ rtx in0 = gen_rtx (REG, SImode, 24);
  rtx in1 = gen_rtx (REG, SImode, 25);

  emit_move_insn (in0, operands[1]);
  emit_move_insn (in1, operands[2]);
  operands[1] = in0, operands[2] = in1;
}")

(define_expand "divdi3"
  [(parallel [(set (reg:DI 27)
		   (div:DI (match_operand:DI 1 "general_operand" "")
			   (match_operand:DI 2 "general_operand" "")))
	      (clobber (reg:DI 23))])
   (set (match_operand:DI 0 "general_operand" "")
	(reg:DI 27))]
  ""
  "
{ rtx in0 = gen_rtx (REG, DImode, 24);
  rtx in1 = gen_rtx (REG, DImode, 25);

  emit_move_insn (in0, operands[1]);
  emit_move_insn (in1, operands[2]);
  operands[1] = in0, operands[2] = in1;
}")

(define_expand "udivdi3"
  [(parallel [(set (reg:DI 27)
		   (udiv:DI (match_operand:DI 1 "general_operand" "")
			    (match_operand:DI 2 "general_operand" "")))
	      (clobber (reg:DI 23))])
   (set (match_operand:DI 0 "general_operand" "")
	(reg:DI 27))]
  ""
  "
{ rtx in0 = gen_rtx (REG, DImode, 24);
  rtx in1 = gen_rtx (REG, DImode, 25);

  emit_move_insn (in0, operands[1]);
  emit_move_insn (in1, operands[2]);
  operands[1] = in0, operands[2] = in1;
}")

(define_expand "moddi3"
  [(parallel [(set (reg:DI 27)
		   (mod:DI (match_operand:DI 1 "general_operand" "")
			   (match_operand:DI 2 "general_operand" "")))
	      (clobber (reg:DI 23))])
   (set (match_operand:DI 0 "general_operand" "")
	(reg:DI 27))]
  ""
  "
{ rtx in0 = gen_rtx (REG, DImode, 24);
  rtx in1 = gen_rtx (REG, DImode, 25);

  emit_move_insn (in0, operands[1]);
  emit_move_insn (in1, operands[2]);
  operands[1] = in0, operands[2] = in1;
}")

(define_expand "umoddi3"
  [(parallel [(set (reg:DI 27)
		   (umod:DI (match_operand:DI 1 "general_operand" "")
			    (match_operand:DI 2 "general_operand" "")))
	      (clobber (reg:DI 23))])
   (set (match_operand:DI 0 "general_operand" "")
	(reg:DI 27))]
  ""
  "
{ rtx in0 = gen_rtx (REG, DImode, 24);
  rtx in1 = gen_rtx (REG, DImode, 25);

  emit_move_insn (in0, operands[1]);
  emit_move_insn (in1, operands[2]);
  operands[1] = in0, operands[2] = in1;
}")

(define_insn ""
  [(set (reg:SI 27)
	(match_operator:SI 1 "divmod_operator"
			[(reg:SI 24) (reg:SI 25)]))
   (clobber (reg:DI 23))]
  ""
  "%E1 $24,$25,$27")

(define_insn ""
  [(set (reg:DI 27)
	(match_operator:DI 1 "divmod_operator"
			[(reg:DI 24) (reg:DI 25)]))
   (clobber (reg:DI 23))]
  ""
  "%E1 $24,$25,$27")

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
  [(set (match_operand:DI 0 "register_operand" "=r")
	(ior:DI (match_operand:DI 1 "reg_or_0_operand" "%rJ")
		(match_operand:DI 2 "reg_or_8bit_operand" "rI")))]
  ""
  "bis %r1,%2,%0"
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
  [(set (match_operand:DI 0 "register_operand" "=r")
	(xor:DI (match_operand:DI 1 "reg_or_0_operand" "rJ")
		(match_operand:DI 2 "reg_or_8bit_operand" "rI")))]
  ""
  "xor %r1,%2,%0"
  [(set_attr "type" "iaddlog")])

(define_insn ""
  [(set (match_operand:DI 0 "register_operand" "=r")
	(not:DI (xor:DI (match_operand:DI 1 "reg_or_0_operand" "rJ")
			(match_operand:DI 2 "reg_or_8bit_operand" "rI"))))]
  ""
  "eqv %r1,%2,%0"
  [(set_attr "type" "iaddlog")])

;; Next come the shifts and the various extract and insert operations.

(define_insn "ashldi3"
  [(set (match_operand:DI 0 "register_operand" "=r,r")
	(ashift:DI (match_operand:DI 1 "reg_or_0_operand" "rJ,rJ")
		   (match_operand:DI 2 "reg_or_8bit_operand" "P,rI")))]
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

;; This is the same as (sign_extend (shift X [123])).
(define_insn ""
  [(set (match_operand:DI 0 "register_operand" "=r")
	(ashiftrt:DI (ashift:DI (match_operand:DI 1 "reg_or_0_operand" "rJ")
				(match_operand:DI 2 "const_int_operand" "i"))
		     (const_int 32)))]
  "INTVAL (operands[2]) >= 33 && INTVAL (operands[2]) <= 35"
  "*
{
  switch (INTVAL (operands[2]))
    {
    case 33:
      return \"addl %r1,%r1,%0\";
    case 34:
      return \"s4addl %r1,0,%0\";
    case 35:
      return \"s8addl %r1,0,%0\";
    default:
      abort ();
    }
}"
  [(set_attr "type" "iaddlog")])
			  
(define_insn "lshrdi3"
  [(set (match_operand:DI 0 "register_operand" "=r")
	(lshiftrt:DI (match_operand:DI 1 "reg_or_0_operand" "rJ")
		     (match_operand:DI 2 "reg_or_8bit_operand" "rI")))]
  ""
  "srl %r1,%2,%0")

(define_insn "ashrdi3"
  [(set (match_operand:DI 0 "register_operand" "=r")
	(ashiftrt:DI (match_operand:DI 1 "reg_or_0_operand" "rJ")
		     (match_operand:DI 2 "reg_or_8bit_operand" "rI")))]
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
	(and:DI (ashift:DI
		 (match_operand:DI 2 "mode_mask_operand" "n")
		 (ashift:DI (match_operand:DI 3 "reg_or_8bit_operand" "rI")
			    (const_int 3)))
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
  "cpysn %1,%R1,%0"
  [(set_attr "type" "fpop")])

(define_insn "negdf2"
  [(set (match_operand:DF 0 "register_operand" "=f")
	(neg:DF (match_operand:DF 1 "reg_or_fp0_operand" "fG")))]
  "TARGET_FP"
  "cpysn %1,%R1,%0"
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
		  (match_operand:SF 1 "reg_or_fp0_operand" "%fG"))
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
  "cvttq %R1,%0"
  [(set_attr "type" "fpop")])

(define_insn "fix_truncsfdi2"
  [(set (match_operand:DI 0 "register_operand" "=f")
	(fix:DI (float_extend:DF
		 (match_operand:SF 1 "reg_or_fp0_operand" "fG"))))]
  "TARGET_FP"
  "cvttq %R1,%0"
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
	(mult:SF (match_operand:SF 1 "reg_or_fp0_operand" "fG")
		 (match_operand:SF 2 "reg_or_fp0_operand" "fG")))]
  "TARGET_FP"
  "muls %R1,%R2,%0"
  [(set_attr "type" "fpop")])

(define_insn "muldf3"
  [(set (match_operand:DF 0 "register_operand" "=f")
	(mult:DF (match_operand:DF 1 "reg_or_fp0_operand" "fG")
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
		  (match_operand:SF 1 "reg_or_fp0_operand" "fG"))
		 (float_extend:DF
		  (match_operand:SF 2 "reg_or_fp0_operand" "fG"))))]
  "TARGET_FP"
  "mult %R1,%R2,%0"
  [(set_attr "type" "fpop")])

(define_insn "subsf3"
  [(set (match_operand:SF 0 "register_operand" "=f")
	(minus:SF (match_operand:SF 1 "reg_or_fp0_operand" "%fG")
		  (match_operand:SF 2 "reg_or_fp0_operand" "fG")))]
  "TARGET_FP"
  "subs %R1,%R2,%0"
  [(set_attr "type" "fpop")])

(define_insn "subdf3"
  [(set (match_operand:DF 0 "register_operand" "=f")
	(minus:DF (match_operand:DF 1 "reg_or_fp0_operand" "%fG")
		  (match_operand:DF 2 "reg_or_fp0_operand" "fG")))]
  "TARGET_FP"
  "subt %R1,%R2,%0"
  [(set_attr "type" "fpop")])

(define_insn ""
  [(set (match_operand:DF 0 "register_operand" "=f")
	(minus:DF (float_extend:DF
		   (match_operand:SF 1 "reg_or_fp0_operand" "%fG"))
		  (match_operand:DF 2 "reg_or_fp0_operand" "fG")))]
  "TARGET_FP"
  "subt %R1,%R2,%0"
  [(set_attr "type" "fpop")])

(define_insn ""
  [(set (match_operand:DF 0 "register_operand" "=f")
	(minus:DF (match_operand:DF 1 "reg_or_fp0_operand" "%fG")
		  (float_extend:DF
		   (match_operand:SF 2 "reg_or_fp0_operand" "fG"))))]
  "TARGET_FP"
  "subt %R1,%R2,%0"
  [(set_attr "type" "fpop")])

(define_insn ""
  [(set (match_operand:DF 0 "register_operand" "=f")
	(minus:DF (float_extend:DF
		   (match_operand:SF 1 "reg_or_fp0_operand" "%fG"))
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

(define_insn ""
  [(set (match_operand:DI 0 "register_operand" "=r,r")
	(if_then_else:DI
	 (match_operator 2 "signed_comparison_operator"
			 [(match_operand:DI 3 "reg_or_0_operand" "rJ,rJ")
			  (const_int 0)])
	 (match_operand:DI 1 "reg_or_8bit_operand" "rI,0")
	 (match_operand:DI 4 "reg_or_8bit_operand" "0,rI")))]
  ""
  "@
   cmov%C2 %r3,%1,%0
   cmov%D2 %r3,%4,%0")

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

(define_expand "smaxdf3"
  [(set (match_dup 3)
	(le:DF (match_operand:DF 1 "reg_or_fp0_operand" "")
	       (match_operand:DF 2 "reg_or_fp0_operand" "")))
   (set (match_operand:DF 0 "register_operand" "")
	(if_then_else:DF (eq (match_dup 3) (const_int 0))
			 (match_dup 1) (match_dup 2)))]
  "TARGET_FP"
  "
{ operands[3] = gen_reg_rtx (DFmode);
}")

(define_expand "smindf3"
  [(set (match_dup 3)
	(lt:DF (match_operand:DF 1 "reg_or_fp0_operand" "")
	       (match_operand:DF 2 "reg_or_fp0_operand" "")))
   (set (match_operand:DF 0 "register_operand" "")
	(if_then_else:DF (ne (match_dup 3) (const_int 0))
			 (match_dup 1) (match_dup 2)))]
  "TARGET_FP"
  "
{ operands[3] = gen_reg_rtx (DFmode);
}")

(define_expand "smaxsf3"
  [(set (match_dup 3)
	(le:DF (match_operand:SF 1 "reg_or_fp0_operand" "")
	       (float_extend:DF (match_operand:SF 2 "reg_or_fp0_operand" ""))))
   (set (match_operand:SF 0 "register_operand" "")
	(if_then_else:SF (eq (match_dup 3) (const_int 0))
			 (match_dup 1) (match_dup 2)))]
  "TARGET_FP"
  "
{ operands[3] = gen_reg_rtx (SFmode);
}")

(define_expand "sminsf3"
  [(set (match_dup 3)
	(lt:DF (match_operand:SF 1 "reg_or_fp0_operand" "")
	       (float_extend:DF (match_operand:SF 2 "reg_or_fp0_operand" ""))))
   (set (match_operand:SF 0 "register_operand" "")
	(if_then_else:SF (ne (match_dup 3) (const_int 0))
		      (match_dup 1) (match_dup 2)))]
  "TARGET_FP"
  "
{ operands[3] = gen_reg_rtx (SFmode);
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
  ""
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
  enum machine_mode mode = alpha_compare_fp_p ? DFmode : DImode;
  operands[1] = gen_reg_rtx (mode);
  operands[2] = gen_rtx (EQ, mode, alpha_compare_op0, alpha_compare_op1);
  operands[3] = gen_rtx (NE, VOIDmode, operands[1], CONST0_RTX (mode));
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
  enum machine_mode mode = alpha_compare_fp_p ? DFmode : DImode;
  operands[1] = gen_reg_rtx (mode);
  operands[2] = gen_rtx (EQ, mode, alpha_compare_op0, alpha_compare_op1);
  operands[3] = gen_rtx (EQ, VOIDmode, operands[1], CONST0_RTX (mode));
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

;; These define_split definitions are used in cases when comparisons have
;; not be stated in the correct way and we need to reverse the second
;; comparison.  For example, x >= 7 has to be done as x < 6 with the
;; comparison that tests the result being reversed.  We have one define_split
;; for each use of a comparison.  They do not match valid insns and need
;; not generate valid insns.
;;
;; We can also handle equality comparisons (and inequality comparisons in
;; cases where the resulting add cannot overflow) with out-of-range numbers by
;; doing an add followed by a comparison with zero.  For this case, we
;; also have an SImode pattern since we can merge the add and sign
;; extend and the order doesn't matter.
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

  if ((code == NE || code == EQ
       || (extended_count (operands[2], DImode, unsignedp) >= 1
	   && extended_count (operands[3], DImode, unsignedp) >= 1))
      && GET_CODE (operands[3]) == CONST_INT
      && (unsigned) INTVAL (operands[3]) > 255)
    {
      operands[7] = gen_rtx (PLUS, DImode, operands[2],
			     GEN_INT (- INTVAL (operands[3])));
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
			  (match_operand:SI 3 "const_int_operand" "")])
	 (match_operand:DI 4 "reg_or_8bit_operand" "")
	 (match_operand:DI 5 "reg_or_8bit_operand" "")))
   (clobber (match_operand:DI 6 "register_operand" ""))]
  "(unsigned) INTVAL (operands[3]) > 255"
  [(set (match_dup 6) (match_dup 7))
   (set (match_dup 0)
	(if_then_else:DI (match_dup 8) (match_dup 4) (match_dup 5)))]
  "
{ enum rtx_code code = GET_CODE (operands[1]);
  int unsignedp = (code == GEU || code == LEU || code == GTU || code == LTU);

  if ((code != NE && code != EQ
       && ! (extended_count (operands[2], DImode, unsignedp) >= 1
	     && extended_count (operands[3], DImode, unsignedp) >= 1)))
    FAIL;
 
  operands[7] = gen_rtx (SIGN_EXTEND, DImode,
			 gen_rtx (PLUS, SImode, operands[2],
				  GEN_INT (- INTVAL (operands[3]))));
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

  if ((code == NE || code == EQ
       || (extended_count (operands[2], DImode, unsignedp) >= 1
	   && extended_count (operands[3], DImode, unsignedp) >= 1))
      && GET_CODE (operands[3]) == CONST_INT
      && (unsigned) INTVAL (operands[3]) > 255)
    {
      operands[5] = gen_rtx (PLUS, DImode, operands[2],
			     GEN_INT (- INTVAL (operands[3])));
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
  "INTVAL (operands[3]) < 0
   && (GET_CODE (operands[1]) == EQ || GET_CODE (operands[1]) == NE)"
  [(set (match_dup 4) (match_dup 5))
   (set (pc) (if_then_else (match_dup 6) (label_ref (match_dup 0)) (pc)))]
  "
{ operands[5] = gen_rtx (SIGN_EXTEND, DImode,
			 gen_rtx (PLUS, SImode, operands[2],
				  GEN_INT (- INTVAL (operands[3]))));
  operands[6] = gen_rtx (GET_CODE (operands[1]), VOIDmode,
			 operands[4], const0_rtx);
}")

;; Here are the CALL and unconditional branch insns.

(define_expand "call"
  [(parallel [(call (mem:DI (match_dup 2))
		    (match_operand 1 "" ""))
	      (use (match_operand:DI 0 "" ""))
	      (clobber (reg:DI 26))])]
  ""
  "
{ if (GET_CODE (operands[0]) != MEM)
    abort ();
  operands[0] = XEXP (operands[0], 0);

  operands[2] = gen_rtx (REG, DImode, 27);
  emit_move_insn (operands[2], operands[0]);

  if (GET_CODE (operands[0]) != SYMBOL_REF)
    operands[0] = const0_rtx;
}")

(define_expand "call_value"
  [(parallel [(set (match_operand 0 "" "")
		   (call (mem:DI (match_dup 3))
			 (match_operand 2 "" "")))
	      (use (match_operand:DI 1 "" ""))
	      (clobber (reg:DI 26))])]
  ""
  "
{ if (GET_CODE (operands[1]) != MEM)
    abort ();

  operands[1] = XEXP (operands[1], 0);

  operands[3] = gen_rtx (REG, DImode, 27);
  emit_move_insn (operands[3], operands[1]);

  if (GET_CODE (operands[1]) != SYMBOL_REF)
    operands[1] = const0_rtx;
}")

(define_insn ""
  [(call (mem:DI (reg:DI 27))
	 (match_operand 0 "" ""))
   (use (match_operand:DI 1 "" ""))
   (clobber (reg:DI 26))]
  ""
  "*
{ if (alpha_gp_dead_after (insn))
    return \"jsr $26,($27),%1\";
  else 
    return \"jsr $26,($27),%1\;ldgp $29,0($26)\";
}"
  [(set_attr "type" "jsr")])
      
(define_insn ""
  [(set (match_operand 0 "register_operand" "=rf")
	(call (mem:DI (reg:DI 27))
	      (match_operand 1 "" "")))
   (use (match_operand:DI 2 "" ""))
   (clobber (reg:DI 26))]
  ""
  "*
{ if (alpha_gp_dead_after (insn))
    return \"jsr $26,($27),%2\";
  else 
    return \"jsr $26,($27),%2\;ldgp $29,0($26)\";
}"
  [(set_attr "type" "jsr")])

(define_insn ""
  [(call (mem:DI (match_operand 1 "current_function_operand" "i"))
	 (match_operand 0 "" ""))
   (use (match_dup 1))
   (clobber (reg:DI 26))]
  ""
  "bsr $26,%F1"
  [(set_attr "type" "ibr")])
      
(define_insn ""
  [(set (match_operand 0 "register_operand" "=rf")
	(call (mem:DI (match_operand 1 "current_function_operand" "i"))
	      (match_operand 2 "" "")))
   (use (match_dup 1))
   (clobber (reg:DI 26))]
  ""
  "bsr $26,%F1"
  [(set_attr "type" "ibr")])

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
  [(set (match_dup 3)
	(sign_extend:DI (match_operand:SI 0 "register_operand" "")))
   (parallel [(set (pc) (plus:DI (match_dup 3) (reg:DI 29)))
	      (use (label_ref (match_operand 1 "" "")))
	      (clobber (match_scratch:DI 2 "=r"))])]
  ""
  "
{ operands[3] = gen_reg_rtx (DImode); }")

(define_insn ""
  [(set (pc)
	(plus:DI (match_operand:DI 0 "register_operand" "r")
		 (reg:DI 29)))
   (use (label_ref (match_operand 1 "" "")))
   (clobber (match_scratch:DI 2 "=r"))]
  ""
  "*
{ rtx best_label = 0;
  rtx jump_table_insn = next_active_insn (operands[1]);

  if (GET_CODE (jump_table_insn) == JUMP_INSN
      && GET_CODE (PATTERN (jump_table_insn)) == ADDR_VEC)
    {
      rtx jump_table = PATTERN (jump_table_insn);
      int n_labels = XVECLEN (jump_table, 0);
      int best_count = -1;
      int i, j;

      for (i = 0; i < n_labels; i++)
	{
	  int count = 1;

	  for (j = i + 1; j < n_labels; j++)
	    if (XEXP (XVECEXP (jump_table, 0, i), 0)
		== XEXP (XVECEXP (jump_table, 0, j), 0))
	      count++;

	  if (count > best_count)
	    best_count = count, best_label = XVECEXP (jump_table, 0, i);
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
	(match_operand:SF 1 "input_operand" "r,m,rG,f,G,m,fG"))]
  "register_operand (operands[0], SFmode)
   || reg_or_fp0_operand (operands[1], SFmode)"
  "@
   bis %1,%1,%0
   ldl %0,%1
   stl %r1,%0
   cpys %1,%1,%0
   cpys $f31,$f31,%0
   lds %0,%1
   sts %R1,%0"
  [(set_attr "type" "iaddlog,ld,st,fpop,fpop,ld,st")])

(define_insn ""
  [(set (match_operand:DF 0 "nonimmediate_operand" "=r,r,m,f,f,f,m")
	(match_operand:DF 1 "input_operand" "r,m,rG,f,G,m,fG"))]
  "register_operand (operands[0], DFmode)
   || reg_or_fp0_operand (operands[1], DFmode)"
  "@
   bis %1,%1,%0
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

;; There is a problem with 32-bit values in FP registers.  We keep such
;; values in the register as a quadword.  This is done on loads by using
;; the cvtlq instruction.  On stores, we can't do anything directly from
;; floating-point registers.  Disallow such an operation and let reload
;; use an integer register instead.  Don't encourage 32-bit values to
;; be placed in FP registers at all.

(define_insn ""
  [(set (match_operand:SI 0 "nonimmediate_operand" "=r,r,r,r,r,r,m,*f,*f,*f")
	(match_operand:SI 1 "input_operand" "r,J,I,K,L,m,rJ,*f,J,m"))]
  "register_operand (operands[0], SImode)
   || reg_or_0_operand (operands[1], SImode)"
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
   lds %0,%1\;cvtlq %0,%0"
  [(set_attr "type" "iaddlog,iaddlog,iaddlog,iaddlog,iaddlog,ld,st,fpop,fpop,ld")])

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
      if (alpha_emit_set_const (operands[0], INTVAL (operands[1]), 3))
	DONE;
      else
	abort ();
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
{ if (alpha_emit_set_const (operands[0], INTVAL (operands[1]), 2))
    DONE;
  else
    FAIL;
}")

(define_insn ""
  [(set (match_operand:DI 0 "general_operand" "=r,r,r,r,r,r,r,m,f,f,f,m")
	(match_operand:DI 1 "input_operand" "r,J,I,K,L,s,m,rJ,f,J,m,fG"))]
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
  if (GET_CODE (operands[0]) == MEM
      && ! reg_or_0_operand (operands[1], DImode))
    operands[1] = force_reg (DImode, operands[1]);

  if (! CONSTANT_P (operands[1]) || input_operand (operands[1], DImode))
    ;
  else if (GET_CODE (operands[1]) == CONST_INT
	   && alpha_emit_set_const (operands[0], INTVAL (operands[1]), 3))
    DONE;
  else if (CONSTANT_P (operands[1]))
    {
      operands[1] = force_const_mem (DImode, operands[1]);
      if (reload_in_progress)
	{
	  emit_move_insn (operands[0], XEXP (operands[1], 0));
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
{ if (alpha_emit_set_const (operands[0], INTVAL (operands[1]), 2))
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
   (set (subreg:DI (match_operand:HI 0 "register_operand" "") 0)
	(zero_extract:DI (match_dup 3)
			 (const_int 16)
			 (ashift:DI (match_dup 1) (const_int 3))))]
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
;; Operands 2, 3, and 4 are DImode temporaries, where the last two may
;; be the same temporary, if desired.  If the address is in a register,
;; operand 2 can be that register.

(define_expand "unaligned_storeqi"
  [(set (match_operand:DI 3 "register_operand" "")
	(mem:DI (and:DI (match_operand:DI 0 "address_operand" "")
			(const_int -8))))
   (set (match_operand:DI 2 "register_operand" "")
	(match_dup 0))
   (set (match_dup 3)
	(and:DI (ashift:DI (const_int 255)
			   (ashift:DI (match_dup 2) (const_int 3)))
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
   (set (match_operand:DI 5 "register_operand" "")
	(and:DI (ashift:DI (const_int 65535)
			   (ashift:DI (match_dup 2) (const_int 3)))
		(match_dup 4)))
   (set (match_operand:DI 6 "register_operand" "")
	(ashift:DI (zero_extend:DI (match_operand:HI 1 "register_operand" ""))
		   (ashift:DI (match_dup 2) (const_int 3))))
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
	  rtx seq = gen_unaligned_loadhi (operands[0], addr, scratch1,
					  scratch2);

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
	  rtx seq = gen_unaligned_storehi (get_unaligned_address (operands[0]),
					   operands[1], temp1, temp2,temp3,
					   temp4, temp5, temp6,temp7, temp8);

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
	      (match_operand:DI 2 "register_operand" "=&r")])]
  ""
  "
{ extern rtx get_unaligned_address ();
  rtx addr = get_unaligned_address (operands[1]);
  rtx seq = gen_unaligned_loadqi (operands[0], addr, operands[2],
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
      
  seq = gen_unaligned_loadhi (operands[0], addr, scratch1, scratch1);
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
      rtx seq;

      if (GET_CODE (addr) == REG)
	scratch1 = addr;

      seq = gen_unaligned_storeqi (addr, operands[1], scratch1,
				   scratch2, scratch2);
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
				   scratch1);
      alpha_set_memflags (seq, operands[0]);
      emit_insn (seq);
    }

  DONE;
}")

;;- Local variables:
;;- mode:emacs-lisp
;;- comment-start: ";;- "
;;- eval: (set-syntax-table (copy-sequence (syntax-table)))
;;- eval: (modify-syntax-entry ?[ "(]")
;;- eval: (modify-syntax-entry ?] ")[")
;;- eval: (modify-syntax-entry ?{ "(}")
;;- eval: (modify-syntax-entry ?} "){")
;;- End:
