;;- Machine description for GNU compiler, Vax Version
;;   Copyright (C) 1987, 1988, 1991, 1994, 1995 Free Software Foundation, Inc.

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


;;- Instruction patterns.  When multiple patterns apply,
;;- the first one in the file is chosen.
;;-
;;- See file "rtl.def" for documentation on define_insn, match_*, et. al.
;;-
;;- cpp macro #define NOTICE_UPDATE_CC in file tm.h handles condition code
;;- updates for most instructions.

;; We don't want to allow a constant operand for test insns because
;; (set (cc0) (const_int foo)) has no mode information.  Such insns will
;; be folded while optimizing anyway.

(define_insn "tstsi"
  [(set (cc0)
	(match_operand:SI 0 "nonimmediate_operand" "g"))]
  ""
  "tstl %0")

(define_insn "tsthi"
  [(set (cc0)
	(match_operand:HI 0 "nonimmediate_operand" "g"))]
  ""
  "tstw %0")

(define_insn "tstqi"
  [(set (cc0)
	(match_operand:QI 0 "nonimmediate_operand" "g"))]
  ""
  "tstb %0")

(define_insn "tstdf"
  [(set (cc0)
	(match_operand:DF 0 "general_operand" "gF"))]
  ""
  "tst%# %0")

(define_insn "tstsf"
  [(set (cc0)
	(match_operand:SF 0 "general_operand" "gF"))]
  ""
  "tstf %0")

(define_insn "cmpsi"
  [(set (cc0)
	(compare (match_operand:SI 0 "nonimmediate_operand" "g")
		 (match_operand:SI 1 "general_operand" "g")))]
  ""
  "cmpl %0,%1")

(define_insn "cmphi"
  [(set (cc0)
	(compare (match_operand:HI 0 "nonimmediate_operand" "g")
		 (match_operand:HI 1 "general_operand" "g")))]
  ""
  "cmpw %0,%1")

(define_insn "cmpqi"
  [(set (cc0)
	(compare (match_operand:QI 0 "nonimmediate_operand" "g")
		 (match_operand:QI 1 "general_operand" "g")))]
  ""
  "cmpb %0,%1")

(define_insn "cmpdf"
  [(set (cc0)
	(compare (match_operand:DF 0 "general_operand" "gF,gF")
		 (match_operand:DF 1 "general_operand" "G,gF")))]
  ""
  "@
   tst%# %0
   cmp%# %0,%1")

(define_insn "cmpsf"
  [(set (cc0)
	(compare (match_operand:SF 0 "general_operand" "gF,gF")
		 (match_operand:SF 1 "general_operand" "G,gF")))]
  ""
  "@
   tstf %0
   cmpf %0,%1")

(define_insn ""
  [(set (cc0)
	(and:SI (match_operand:SI 0 "general_operand" "g")
		(match_operand:SI 1 "general_operand" "g")))]
  ""
  "bitl %0,%1")

(define_insn ""
  [(set (cc0)
	(and:HI (match_operand:HI 0 "general_operand" "g")
		(match_operand:HI 1 "general_operand" "g")))]
  ""
  "bitw %0,%1")

(define_insn ""
  [(set (cc0)
	(and:QI (match_operand:QI 0 "general_operand" "g")
		(match_operand:QI 1 "general_operand" "g")))]
  ""
  "bitb %0,%1")

;; The vax has no sltu or sgeu patterns, but does have two-operand
;; add/subtract with carry.  This is still better than the alternative.
;; Since the cc0-using insn cannot be separated from the cc0-setting insn,
;; and the two are created independently, we can't just use a define_expand
;; to try to optimize this.  (The "movl" and "clrl" insns alter the cc0
;; flags, but leave the carry flag alone, but that can't easily be expressed.)
;;
;; Several two-operator combinations could be added to make slightly more
;; optimal code, but they'd have to cover all combinations of plus and minus
;; using match_dup.  If you want to do this, I'd suggest changing the "sgeu"
;; pattern to something like (minus (const_int 1) (ltu ...)), so fewer
;; patterns need to be recognized.
;; -- Ken Raeburn (Raeburn@Watch.COM) 24 August 1991.

(define_insn "sltu"
  [(set (match_operand:SI 0 "general_operand" "=ro")
	(ltu (cc0) (const_int 0)))]
  ""
  "clrl %0\;adwc $0,%0")

(define_insn "sgeu"
  [(set (match_operand:SI 0 "general_operand" "=ro")
	(geu (cc0) (const_int 0)))]
  ""
  "movl $1,%0\;sbwc $0,%0")

(define_insn "movdf"
  [(set (match_operand:DF 0 "general_operand" "=g,g")
	(match_operand:DF 1 "general_operand" "G,gF"))]
  ""
  "@
   clr%# %0
   mov%# %1,%0")

(define_insn "movsf"
  [(set (match_operand:SF 0 "general_operand" "=g,g")
	(match_operand:SF 1 "general_operand" "G,gF"))]
  ""
  "@
   clrf %0
   movf %1,%0")

;; Some vaxes don't support this instruction.
;;(define_insn "movti"
;;  [(set (match_operand:TI 0 "general_operand" "=g")
;;	(match_operand:TI 1 "general_operand" "g"))]
;;  ""
;;  "movh %1,%0")

(define_insn "movdi"
  [(set (match_operand:DI 0 "general_operand" "=g,g")
	(match_operand:DI 1 "general_operand" "I,g"))]
  ""
  "@
   clrq %0
   movq %D1,%0")

;; The VAX move instructions have space-time tradeoffs.  On a microVAX
;; register-register mov instructions take 3 bytes and 2 CPU cycles.  clrl
;; takes 2 bytes and 3 cycles.  mov from constant to register takes 2 cycles
;; if the constant is smaller than 4 bytes, 3 cycles for a longword
;; constant.  movz, mneg, and mcom are as fast as mov, so movzwl is faster
;; than movl for positive constants that fit in 16 bits but not 6 bits.  cvt
;; instructions take 4 cycles.  inc takes 3 cycles.  The machine description
;; is willing to trade 1 byte for 1 cycle (clrl instead of movl $0; cvtwl
;; instead of movl).

;; Cycle counts for other models may vary (on a VAX 750 they are similar,
;; but on a VAX 9000 most move and add instructions with one constant
;; operand take 1 cycle).

;;  Loads of constants between 64 and 128 used to be done with
;; "addl3 $63,#,dst" but this is slower than movzbl and takes as much space.

(define_insn "movsi"
  [(set (match_operand:SI 0 "general_operand" "=g")
	(match_operand:SI 1 "general_operand" "g"))]
  ""
  "*
{
  rtx link;
  if (operands[1] == const1_rtx
      && (link = find_reg_note (insn, REG_WAS_0, 0))
      /* Make sure the insn that stored the 0 is still present.  */
      && ! INSN_DELETED_P (XEXP (link, 0))
      && GET_CODE (XEXP (link, 0)) != NOTE
      /* Make sure cross jumping didn't happen here.  */
      && no_labels_between_p (XEXP (link, 0), insn)
      /* Make sure the reg hasn't been clobbered.  */
      && ! reg_set_between_p (operands[0], XEXP (link, 0), insn))
    return \"incl %0\";
  if (GET_CODE (operands[1]) == SYMBOL_REF || GET_CODE (operands[1]) == CONST)
    {
      if (push_operand (operands[0], SImode))
	return \"pushab %a1\";
      return \"movab %a1,%0\";
    }
  if (operands[1] == const0_rtx)
    return \"clrl %0\";
  if (GET_CODE (operands[1]) == CONST_INT
      && (unsigned) INTVAL (operands[1]) >= 64)
    {
      int i = INTVAL (operands[1]);
      if ((unsigned)(~i) < 64)
	return \"mcoml %N1,%0\";
      if ((unsigned)i < 0x100)
	return \"movzbl %1,%0\";
      if (i >= -0x80 && i < 0)
	return \"cvtbl %1,%0\";
      if ((unsigned)i < 0x10000)
	return \"movzwl %1,%0\";
      if (i >= -0x8000 && i < 0)
	return \"cvtwl %1,%0\";
    }
  if (push_operand (operands[0], SImode))
    return \"pushl %1\";
  return \"movl %1,%0\";
}")

(define_insn "movhi"
  [(set (match_operand:HI 0 "general_operand" "=g")
	(match_operand:HI 1 "general_operand" "g"))]
  ""
  "*
{
  rtx link;
  if (operands[1] == const1_rtx
      && (link = find_reg_note (insn, REG_WAS_0, 0))
      /* Make sure the insn that stored the 0 is still present.  */
      && ! INSN_DELETED_P (XEXP (link, 0))
      && GET_CODE (XEXP (link, 0)) != NOTE
      /* Make sure cross jumping didn't happen here.  */
      && no_labels_between_p (XEXP (link, 0), insn)
      /* Make sure the reg hasn't been clobbered.  */
      && ! reg_set_between_p (operands[0], XEXP (link, 0), insn))
    return \"incw %0\";

  if (GET_CODE (operands[1]) == CONST_INT)
    {
      int i = INTVAL (operands[1]);
      if (i == 0)
	return \"clrw %0\";
      else if ((unsigned int)i < 64)
	return \"movw %1,%0\";
      else if ((unsigned int)~i < 64)
	return \"mcomw %H1,%0\";
      else if ((unsigned int)i < 256)
	return \"movzbw %1,%0\";
    }
  return \"movw %1,%0\";
}")

(define_insn "movstricthi"
  [(set (strict_low_part (match_operand:HI 0 "register_operand" "=g"))
	(match_operand:HI 1 "general_operand" "g"))]
  ""
  "*
{
  if (GET_CODE (operands[1]) == CONST_INT)
    {
      int i = INTVAL (operands[1]);
      if (i == 0)
	return \"clrw %0\";
      else if ((unsigned int)i < 64)
	return \"movw %1,%0\";
      else if ((unsigned int)~i < 64)
	return \"mcomw %H1,%0\";
      else if ((unsigned int)i < 256)
	return \"movzbw %1,%0\";
    }
  return \"movw %1,%0\";
}")

(define_insn "movqi"
  [(set (match_operand:QI 0 "general_operand" "=g")
	(match_operand:QI 1 "general_operand" "g"))]
  ""
  "*
{
  rtx link;
  if (operands[1] == const1_rtx
      && (link = find_reg_note (insn, REG_WAS_0, 0))
      /* Make sure the insn that stored the 0 is still present.  */
      && ! INSN_DELETED_P (XEXP (link, 0))
      && GET_CODE (XEXP (link, 0)) != NOTE
      /* Make sure cross jumping didn't happen here.  */
      && no_labels_between_p (XEXP (link, 0), insn)
      /* Make sure the reg hasn't been clobbered.  */
      && ! reg_set_between_p (operands[0], XEXP (link, 0), insn))
    return \"incb %0\";

  if (GET_CODE (operands[1]) == CONST_INT)
    {
      int i = INTVAL (operands[1]);
      if (i == 0)
	return \"clrb %0\";
      else if ((unsigned int)~i < 64)
	return \"mcomb %B1,%0\";
    }
  return \"movb %1,%0\";
}")

(define_insn "movstrictqi"
  [(set (strict_low_part (match_operand:QI 0 "register_operand" "=g"))
	(match_operand:QI 1 "general_operand" "g"))]
  ""
  "*
{
  if (GET_CODE (operands[1]) == CONST_INT)
    {
      int i = INTVAL (operands[1]);
      if (i == 0)
	return \"clrb %0\";
      else if ((unsigned int)~i < 64)
	return \"mcomb %B1,%0\";
    }
  return \"movb %1,%0\";
}")

;; This is here to accept 4 arguments and pass the first 3 along
;; to the movstrhi1 pattern that really does the work.
(define_expand "movstrhi"
  [(set (match_operand:BLK 0 "general_operand" "=g")
	(match_operand:BLK 1 "general_operand" "g"))
   (use (match_operand:HI 2 "general_operand" "g"))
   (match_operand 3 "" "")]
  ""
  "
  emit_insn (gen_movstrhi1 (operands[0], operands[1], operands[2]));
  DONE;
")

;; The definition of this insn does not really explain what it does,
;; but it should suffice
;; that anything generated as this insn will be recognized as one
;; and that it won't successfully combine with anything.
(define_insn "movstrhi1"
  [(set (match_operand:BLK 0 "general_operand" "=g")
	(match_operand:BLK 1 "general_operand" "g"))
   (use (match_operand:HI 2 "general_operand" "g"))
   (clobber (reg:SI 0))
   (clobber (reg:SI 1))
   (clobber (reg:SI 2))
   (clobber (reg:SI 3))
   (clobber (reg:SI 4))
   (clobber (reg:SI 5))]
  ""
  "movc3 %2,%1,%0")

;; Extension and truncation insns.

(define_insn "truncsiqi2"
  [(set (match_operand:QI 0 "general_operand" "=g")
	(truncate:QI (match_operand:SI 1 "nonimmediate_operand" "g")))]
  ""
  "cvtlb %1,%0")

(define_insn "truncsihi2"
  [(set (match_operand:HI 0 "general_operand" "=g")
	(truncate:HI (match_operand:SI 1 "nonimmediate_operand" "g")))]
  ""
  "cvtlw %1,%0")

(define_insn "trunchiqi2"
  [(set (match_operand:QI 0 "general_operand" "=g")
	(truncate:QI (match_operand:HI 1 "nonimmediate_operand" "g")))]
  ""
  "cvtwb %1,%0")

(define_insn "extendhisi2"
  [(set (match_operand:SI 0 "general_operand" "=g")
	(sign_extend:SI (match_operand:HI 1 "nonimmediate_operand" "g")))]
  ""
  "cvtwl %1,%0")

(define_insn "extendqihi2"
  [(set (match_operand:HI 0 "general_operand" "=g")
	(sign_extend:HI (match_operand:QI 1 "nonimmediate_operand" "g")))]
  ""
  "cvtbw %1,%0")

(define_insn "extendqisi2"
  [(set (match_operand:SI 0 "general_operand" "=g")
	(sign_extend:SI (match_operand:QI 1 "nonimmediate_operand" "g")))]
  ""
  "cvtbl %1,%0")

(define_insn "extendsfdf2"
  [(set (match_operand:DF 0 "general_operand" "=g")
	(float_extend:DF (match_operand:SF 1 "general_operand" "gF")))]
  ""
  "cvtf%# %1,%0")

(define_insn "truncdfsf2"
  [(set (match_operand:SF 0 "general_operand" "=g")
	(float_truncate:SF (match_operand:DF 1 "general_operand" "gF")))]
  ""
  "cvt%#f %1,%0")

(define_insn "zero_extendhisi2"
  [(set (match_operand:SI 0 "general_operand" "=g")
	(zero_extend:SI (match_operand:HI 1 "nonimmediate_operand" "g")))]
  ""
  "movzwl %1,%0")

(define_insn "zero_extendqihi2"
  [(set (match_operand:HI 0 "general_operand" "=g")
	(zero_extend:HI (match_operand:QI 1 "nonimmediate_operand" "g")))]
  ""
  "movzbw %1,%0")

(define_insn "zero_extendqisi2"
  [(set (match_operand:SI 0 "general_operand" "=g")
	(zero_extend:SI (match_operand:QI 1 "nonimmediate_operand" "g")))]
  ""
  "movzbl %1,%0")

;; Fix-to-float conversion insns.

(define_insn "floatsisf2"
  [(set (match_operand:SF 0 "general_operand" "=g")
	(float:SF (match_operand:SI 1 "nonimmediate_operand" "g")))]
  ""
  "cvtlf %1,%0")

(define_insn "floatsidf2"
  [(set (match_operand:DF 0 "general_operand" "=g")
	(float:DF (match_operand:SI 1 "nonimmediate_operand" "g")))]
  ""
  "cvtl%# %1,%0")

(define_insn "floathisf2"
  [(set (match_operand:SF 0 "general_operand" "=g")
	(float:SF (match_operand:HI 1 "nonimmediate_operand" "g")))]
  ""
  "cvtwf %1,%0")

(define_insn "floathidf2"
  [(set (match_operand:DF 0 "general_operand" "=g")
	(float:DF (match_operand:HI 1 "nonimmediate_operand" "g")))]
  ""
  "cvtw%# %1,%0")

(define_insn "floatqisf2"
  [(set (match_operand:SF 0 "general_operand" "=g")
	(float:SF (match_operand:QI 1 "nonimmediate_operand" "g")))]
  ""
  "cvtbf %1,%0")

(define_insn "floatqidf2"
  [(set (match_operand:DF 0 "general_operand" "=g")
	(float:DF (match_operand:QI 1 "nonimmediate_operand" "g")))]
  ""
  "cvtb%# %1,%0")

;; Float-to-fix conversion insns.

(define_insn "fix_truncsfqi2"
  [(set (match_operand:QI 0 "general_operand" "=g")
	(fix:QI (fix:SF (match_operand:SF 1 "general_operand" "gF"))))]
  ""
  "cvtfb %1,%0")

(define_insn "fix_truncsfhi2"
  [(set (match_operand:HI 0 "general_operand" "=g")
	(fix:HI (fix:SF (match_operand:SF 1 "general_operand" "gF"))))]
  ""
  "cvtfw %1,%0")

(define_insn "fix_truncsfsi2"
  [(set (match_operand:SI 0 "general_operand" "=g")
	(fix:SI (fix:SF (match_operand:SF 1 "general_operand" "gF"))))]
  ""
  "cvtfl %1,%0")

(define_insn "fix_truncdfqi2"
  [(set (match_operand:QI 0 "general_operand" "=g")
	(fix:QI (fix:DF (match_operand:DF 1 "general_operand" "gF"))))]
  ""
  "cvt%#b %1,%0")

(define_insn "fix_truncdfhi2"
  [(set (match_operand:HI 0 "general_operand" "=g")
	(fix:HI (fix:DF (match_operand:DF 1 "general_operand" "gF"))))]
  ""
  "cvt%#w %1,%0")

(define_insn "fix_truncdfsi2"
  [(set (match_operand:SI 0 "general_operand" "=g")
	(fix:SI (fix:DF (match_operand:DF 1 "general_operand" "gF"))))]
  ""
  "cvt%#l %1,%0")

;;- All kinds of add instructions.

(define_insn "adddf3"
  [(set (match_operand:DF 0 "general_operand" "=g,g,g")
	(plus:DF (match_operand:DF 1 "general_operand" "0,gF,gF")
		 (match_operand:DF 2 "general_operand" "gF,0,gF")))]
  ""
  "@
   add%#2 %2,%0
   add%#2 %1,%0
   add%#3 %1,%2,%0")

(define_insn "addsf3"
  [(set (match_operand:SF 0 "general_operand" "=g,g,g")
	(plus:SF (match_operand:SF 1 "general_operand" "0,gF,gF")
		 (match_operand:SF 2 "general_operand" "gF,0,gF")))]
  ""
  "@
   addf2 %2,%0
   addf2 %1,%0
   addf3 %1,%2,%0")

/* The space-time-opcode tradeoffs for addition vary by model of VAX.

   On a VAX 3 "movab (r1)[r2],r3" is faster than "addl3 r1,r2,r3",
   but it not faster on other models.

   "movab #(r1),r2" is usually shorter than "addl3 #,r1,r2", and is
   faster on a VAX 3, but some VAXes (e.g. VAX 9000) will stall if
   a register is used in an address too soon after it is set.
   Compromise by using movab only when it is shorter than the add
   or the base register in the address is one of sp, ap, and fp,
   which are not modified very often.  */


(define_insn "addsi3"
  [(set (match_operand:SI 0 "general_operand" "=g")
	(plus:SI (match_operand:SI 1 "general_operand" "g")
		 (match_operand:SI 2 "general_operand" "g")))]
  ""
  "*
{
  if (rtx_equal_p (operands[0], operands[1]))
    {
      if (operands[2] == const1_rtx)
	return \"incl %0\";
      if (operands[2] == constm1_rtx)
	return \"decl %0\";
      if (GET_CODE (operands[2]) == CONST_INT
	  && (unsigned) (- INTVAL (operands[2])) < 64)
	return \"subl2 $%n2,%0\";
      if (GET_CODE (operands[2]) == CONST_INT
	  && (unsigned) INTVAL (operands[2]) >= 64
	  && GET_CODE (operands[1]) == REG
	  && ((INTVAL (operands[2]) < 32767 && INTVAL (operands[2]) > -32768)
	      || REGNO (operands[1]) > 11))
	return \"movab %c2(%1),%0\";
      return \"addl2 %2,%0\";
    }
  if (rtx_equal_p (operands[0], operands[2]))
    return \"addl2 %1,%0\";

  if (GET_CODE (operands[2]) == CONST_INT
      && INTVAL (operands[2]) < 32767
      && INTVAL (operands[2]) > -32768
      && GET_CODE (operands[1]) == REG
      && push_operand (operands[0], SImode))
    return \"pushab %c2(%1)\";

  if (GET_CODE (operands[2]) == CONST_INT
      && (unsigned) (- INTVAL (operands[2])) < 64)
    return \"subl3 $%n2,%1,%0\";

  if (GET_CODE (operands[2]) == CONST_INT
      && (unsigned) INTVAL (operands[2]) >= 64
      && GET_CODE (operands[1]) == REG
      && ((INTVAL (operands[2]) < 32767 && INTVAL (operands[2]) > -32768)
	  || REGNO (operands[1]) > 11))
    return \"movab %c2(%1),%0\";

  /* Add this if using gcc on a VAX 3xxx:
  if (REG_P (operands[1]) && REG_P (operands[2]))
    return \"movab (%1)[%2],%0\";
  */
  return \"addl3 %1,%2,%0\";
}")

(define_insn "addhi3"
  [(set (match_operand:HI 0 "general_operand" "=g")
	(plus:HI (match_operand:HI 1 "general_operand" "g")
		 (match_operand:HI 2 "general_operand" "g")))]
  ""
  "*
{
  if (rtx_equal_p (operands[0], operands[1]))
    {
      if (operands[2] == const1_rtx)
	return \"incw %0\";
      if (operands[2] == constm1_rtx)
	return \"decw %0\";
      if (GET_CODE (operands[2]) == CONST_INT
	  && (unsigned) (- INTVAL (operands[2])) < 64)
	return \"subw2 $%n2,%0\";
      return \"addw2 %2,%0\";
    }
  if (rtx_equal_p (operands[0], operands[2]))
    return \"addw2 %1,%0\";
  if (GET_CODE (operands[2]) == CONST_INT
      && (unsigned) (- INTVAL (operands[2])) < 64)
    return \"subw3 $%n2,%1,%0\";
  return \"addw3 %1,%2,%0\";
}")

(define_insn "addqi3"
  [(set (match_operand:QI 0 "general_operand" "=g")
	(plus:QI (match_operand:QI 1 "general_operand" "g")
		 (match_operand:QI 2 "general_operand" "g")))]
  ""
  "*
{
  if (rtx_equal_p (operands[0], operands[1]))
    {
      if (operands[2] == const1_rtx)
	return \"incb %0\";
      if (operands[2] == constm1_rtx)
	return \"decb %0\";
      if (GET_CODE (operands[2]) == CONST_INT
	  && (unsigned) (- INTVAL (operands[2])) < 64)
	return \"subb2 $%n2,%0\";
      return \"addb2 %2,%0\";
    }
  if (rtx_equal_p (operands[0], operands[2]))
    return \"addb2 %1,%0\";
  if (GET_CODE (operands[2]) == CONST_INT
      && (unsigned) (- INTVAL (operands[2])) < 64)
    return \"subb3 $%n2,%1,%0\";
  return \"addb3 %1,%2,%0\";
}")

;; The add-with-carry (adwc) instruction only accepts two operands.
(define_insn "adddi3"
  [(set (match_operand:DI 0 "general_operand" "=ro>,ro>")
	(plus:DI (match_operand:DI 1 "general_operand" "%0,ro>")
		 (match_operand:DI 2 "general_operand" "Fro,F")))]
  ""
  "*
{
  rtx low[3];
  char *pattern;
  int carry = 1;

  split_quadword_operands (operands, low, 3);
  /* Add low parts.  */
  if (rtx_equal_p (operands[0], operands[1]))
    {
      if (low[2] == const0_rtx)
	/* Should examine operand, punt if not POST_INC.  */
	pattern = \"tstl %0\", carry = 0;
      else if (low[2] == const1_rtx)
        pattern = \"incl %0\";
      else
        pattern = \"addl2 %2,%0\";
    }
  else
    {
      if (low[2] == const0_rtx)
	pattern = \"movl %1,%0\", carry = 0;
      else
	pattern = \"addl3 %2,%1,%0\";
    }
  if (pattern)
    output_asm_insn (pattern, low);
  if (!carry)
    /* If CARRY is 0, we don't have any carry value to worry about.  */
    return OUT_FCN (CODE_FOR_addsi3) (operands, insn);
  /* %0 = C + %1 + %2 */
  if (!rtx_equal_p (operands[0], operands[1]))
    output_asm_insn ((operands[1] == const0_rtx
		      ? \"clrl %0\"
		      : \"movl %1,%0\"), operands);
  return \"adwc %2,%0\";
}")

;;- All kinds of subtract instructions.

(define_insn "subdf3"
  [(set (match_operand:DF 0 "general_operand" "=g,g")
	(minus:DF (match_operand:DF 1 "general_operand" "0,gF")
		  (match_operand:DF 2 "general_operand" "gF,gF")))]
  ""
  "@
   sub%#2 %2,%0
   sub%#3 %2,%1,%0")

(define_insn "subsf3"
  [(set (match_operand:SF 0 "general_operand" "=g,g")
	(minus:SF (match_operand:SF 1 "general_operand" "0,gF")
		  (match_operand:SF 2 "general_operand" "gF,gF")))]
  ""
  "@
   subf2 %2,%0
   subf3 %2,%1,%0")

(define_insn "subsi3"
  [(set (match_operand:SI 0 "general_operand" "=g,g")
	(minus:SI (match_operand:SI 1 "general_operand" "0,g")
		  (match_operand:SI 2 "general_operand" "g,g")))]
  ""
  "@
   subl2 %2,%0
   subl3 %2,%1,%0")

(define_insn "subhi3"
  [(set (match_operand:HI 0 "general_operand" "=g,g")
	(minus:HI (match_operand:HI 1 "general_operand" "0,g")
		  (match_operand:HI 2 "general_operand" "g,g")))]
  ""
  "@
   subw2 %2,%0
   subw3 %2,%1,%0")

(define_insn "subqi3"
  [(set (match_operand:QI 0 "general_operand" "=g,g")
	(minus:QI (match_operand:QI 1 "general_operand" "0,g")
		  (match_operand:QI 2 "general_operand" "g,g")))]
  ""
  "@
   subb2 %2,%0
   subb3 %2,%1,%0")

;; The subtract-with-carry (sbwc) instruction only takes two operands.
(define_insn "subdi3"
  [(set (match_operand:DI 0 "general_operand" "=or>,or>")
	(minus:DI (match_operand:DI 1 "general_operand" "0,or>")
		  (match_operand:DI 2 "general_operand" "For,F")))]
  ""
  "*
{
  rtx low[3];
  char *pattern;
  int carry = 1;

  split_quadword_operands (operands, low, 3);
  /* Subtract low parts.  */
  if (rtx_equal_p (operands[0], operands[1]))
    {
      if (low[2] == const0_rtx)
	pattern = 0, carry = 0;
      else if (low[2] == constm1_rtx)
	pattern = \"decl %0\";
      else
	pattern = \"subl2 %2,%0\";
    }
  else
    {
      if (low[2] == constm1_rtx)
	pattern = \"decl %0\";
      else if (low[2] == const0_rtx)
	pattern = OUT_FCN (CODE_FOR_movsi) (low, insn), carry = 0;
      else
	pattern = \"subl3 %2,%1,%0\";
    }
  if (pattern)
    output_asm_insn (pattern, low);
  if (carry)
    {
      if (!rtx_equal_p (operands[0], operands[1]))
	return \"movl %1,%0\;sbwc %2,%0\";
      return \"sbwc %2,%0\";
      /* %0 = %2 - %1 - C */
    }
  return OUT_FCN (CODE_FOR_subsi3) (operands, insn);
}")

;;- Multiply instructions.

(define_insn "muldf3"
  [(set (match_operand:DF 0 "general_operand" "=g,g,g")
	(mult:DF (match_operand:DF 1 "general_operand" "0,gF,gF")
		 (match_operand:DF 2 "general_operand" "gF,0,gF")))]
  ""
  "@
   mul%#2 %2,%0
   mul%#2 %1,%0
   mul%#3 %1,%2,%0")

(define_insn "mulsf3"
  [(set (match_operand:SF 0 "general_operand" "=g,g,g")
	(mult:SF (match_operand:SF 1 "general_operand" "0,gF,gF")
		 (match_operand:SF 2 "general_operand" "gF,0,gF")))]
  ""
  "@
   mulf2 %2,%0
   mulf2 %1,%0
   mulf3 %1,%2,%0")

(define_insn "mulsi3"
  [(set (match_operand:SI 0 "general_operand" "=g,g,g")
	(mult:SI (match_operand:SI 1 "general_operand" "0,g,g")
		 (match_operand:SI 2 "general_operand" "g,0,g")))]
  ""
  "@
   mull2 %2,%0
   mull2 %1,%0
   mull3 %1,%2,%0")

(define_insn "mulhi3"
  [(set (match_operand:HI 0 "general_operand" "=g,g,")
	(mult:HI (match_operand:HI 1 "general_operand" "0,g,g")
		 (match_operand:HI 2 "general_operand" "g,0,g")))]
  ""
  "@
   mulw2 %2,%0
   mulw2 %1,%0
   mulw3 %1,%2,%0")

(define_insn "mulqi3"
  [(set (match_operand:QI 0 "general_operand" "=g,g,g")
	(mult:QI (match_operand:QI 1 "general_operand" "0,g,g")
		 (match_operand:QI 2 "general_operand" "g,0,g")))]
  ""
  "@
   mulb2 %2,%0
   mulb2 %1,%0
   mulb3 %1,%2,%0")

(define_insn "mulsidi3"
  [(set (match_operand:DI 0 "general_operand" "=g")
	(mult:DI (sign_extend:DI
		  (match_operand:SI 1 "nonimmediate_operand" "g"))
		 (sign_extend:DI
		  (match_operand:SI 2 "nonimmediate_operand" "g"))))]
  ""
  "emul %1,%2,$0,%0")

(define_insn ""
  [(set (match_operand:DI 0 "general_operand" "=g")
	(plus:DI
	 (mult:DI (sign_extend:DI
		   (match_operand:SI 1 "nonimmediate_operand" "g"))
		  (sign_extend:DI
		   (match_operand:SI 2 "nonimmediate_operand" "g")))
	 (sign_extend:DI (match_operand:SI 3 "nonimmediate_operand" "g"))))]
  ""
  "emul %1,%2,%3,%0")

;; 'F' constraint means type CONST_DOUBLE
(define_insn ""
  [(set (match_operand:DI 0 "general_operand" "=g")
	(plus:DI
	 (mult:DI (sign_extend:DI
		   (match_operand:SI 1 "nonimmediate_operand" "g"))
		  (sign_extend:DI
		   (match_operand:SI 2 "nonimmediate_operand" "g")))
	 (match_operand:DI 3 "immediate_operand" "F")))]
  "GET_CODE (operands[3]) == CONST_DOUBLE 
    && CONST_DOUBLE_HIGH (operands[3]) == (CONST_DOUBLE_LOW (operands[3]) >> 31)"
  "*
{
  if (CONST_DOUBLE_HIGH (operands[3]))
    operands[3] = gen_rtx (CONST_INT, VOIDmode, CONST_DOUBLE_LOW (operands[3]));
  return \"emul %1,%2,%3,%0\";
}")

;;- Divide instructions.

(define_insn "divdf3"
  [(set (match_operand:DF 0 "general_operand" "=g,g")
	(div:DF (match_operand:DF 1 "general_operand" "0,gF")
		(match_operand:DF 2 "general_operand" "gF,gF")))]
  ""
  "@
   div%#2 %2,%0
   div%#3 %2,%1,%0")

(define_insn "divsf3"
  [(set (match_operand:SF 0 "general_operand" "=g,g")
	(div:SF (match_operand:SF 1 "general_operand" "0,gF")
		(match_operand:SF 2 "general_operand" "gF,gF")))]
  ""
  "@
   divf2 %2,%0
   divf3 %2,%1,%0")

(define_insn "divsi3"
  [(set (match_operand:SI 0 "general_operand" "=g,g")
	(div:SI (match_operand:SI 1 "general_operand" "0,g")
		(match_operand:SI 2 "general_operand" "g,g")))]
  ""
  "@
   divl2 %2,%0
   divl3 %2,%1,%0")

(define_insn "divhi3"
  [(set (match_operand:HI 0 "general_operand" "=g,g")
	(div:HI (match_operand:HI 1 "general_operand" "0,g")
		(match_operand:HI 2 "general_operand" "g,g")))]
  ""
  "@
   divw2 %2,%0
   divw3 %2,%1,%0")

(define_insn "divqi3"
  [(set (match_operand:QI 0 "general_operand" "=g,g")
	(div:QI (match_operand:QI 1 "general_operand" "0,g")
		(match_operand:QI 2 "general_operand" "g,g")))]
  ""
  "@
   divb2 %2,%0
   divb3 %2,%1,%0")

;This is left out because it is very slow;
;we are better off programming around the "lack" of this insn.
;(define_insn "divmoddisi4"
;  [(set (match_operand:SI 0 "general_operand" "=g")
;	(div:SI (match_operand:DI 1 "general_operand" "g")
;		(match_operand:SI 2 "general_operand" "g")))
;   (set (match_operand:SI 3 "general_operand" "=g")
;	(mod:SI (match_operand:DI 1 "general_operand" "g")
;		(match_operand:SI 2 "general_operand" "g")))]
;  ""
;  "ediv %2,%1,%0,%3")

;; Bit-and on the vax is done with a clear-bits insn.
(define_expand "andsi3"
  [(set (match_operand:SI 0 "general_operand" "=g")
	(and:SI (not:SI (match_operand:SI 1 "general_operand" "g"))
		(match_operand:SI 2 "general_operand" "g")))]
  ""
  "
{
  rtx op1 = operands[1];

  /* If there is a constant argument, complement that one.  */
  if (GET_CODE (operands[2]) == CONST_INT && GET_CODE (op1) != CONST_INT)
    {
      operands[1] = operands[2];
      operands[2] = op1;
      op1 = operands[1];
    }

  if (GET_CODE (op1) == CONST_INT)
    operands[1] = gen_rtx (CONST_INT, VOIDmode, ~INTVAL (op1));
  else
    operands[1] = expand_unop (SImode, one_cmpl_optab, op1, 0, 1);
}")

(define_expand "andhi3"
  [(set (match_operand:HI 0 "general_operand" "=g")
	(and:HI (not:HI (match_operand:HI 1 "general_operand" "g"))
		(match_operand:HI 2 "general_operand" "g")))]
  ""
  "
{
  rtx op1 = operands[1];

  if (GET_CODE (operands[2]) == CONST_INT && GET_CODE (op1) != CONST_INT)
    {
      operands[1] = operands[2];
      operands[2] = op1;
      op1 = operands[1];
    }

  if (GET_CODE (op1) == CONST_INT)
    operands[1] = gen_rtx (CONST_INT, VOIDmode, 65535 & ~INTVAL (op1));
  else
    operands[1] = expand_unop (HImode, one_cmpl_optab, op1, 0, 1);
}")

(define_expand "andqi3"
  [(set (match_operand:QI 0 "general_operand" "=g")
	(and:QI (not:QI (match_operand:QI 1 "general_operand" "g"))
		(match_operand:QI 2 "general_operand" "g")))]
  ""
  "
{
  rtx op1 = operands[1];

  if (GET_CODE (operands[2]) == CONST_INT && GET_CODE (op1) != CONST_INT)
    {
     operands[1] = operands[2];
     operands[2] = op1;
     op1 = operands[1];
   }

  if (GET_CODE (op1) == CONST_INT)
    operands[1] = gen_rtx (CONST_INT, VOIDmode, 255 & ~INTVAL (op1));
  else
    operands[1] = expand_unop (QImode, one_cmpl_optab, op1, 0, 1);
}")

(define_insn ""
  [(set (match_operand:SI 0 "general_operand" "=g,g")
	(and:SI (not:SI (match_operand:SI 1 "general_operand" "g,g"))
		(match_operand:SI 2 "general_operand" "0,g")))]
  ""
  "@
   bicl2 %1,%0
   bicl3 %1,%2,%0")

(define_insn ""
  [(set (match_operand:HI 0 "general_operand" "=g,g")
	(and:HI (not:HI (match_operand:HI 1 "general_operand" "g,g"))
		(match_operand:HI 2 "general_operand" "0,g")))]
  ""
  "@
   bicw2 %1,%0
   bicw3 %1,%2,%0")

(define_insn ""
  [(set (match_operand:QI 0 "general_operand" "=g,g")
	(and:QI (not:QI (match_operand:QI 1 "general_operand" "g,g"))
		(match_operand:QI 2 "general_operand" "0,g")))]
  ""
  "@
   bicb2 %1,%0
   bicb3 %1,%2,%0")

;; The following used to be needed because constant propagation can
;; create them starting from the bic insn patterns above.  This is no
;; longer a problem.  However, having these patterns allows optimization
;; opportunities in combine.c.

(define_insn ""
  [(set (match_operand:SI 0 "general_operand" "=g,g")
	(and:SI (match_operand:SI 1 "general_operand" "0,g")
		(match_operand:SI 2 "const_int_operand" "n,n")))]
  ""
  "@
   bicl2 %N2,%0
   bicl3 %N2,%1,%0")

(define_insn ""
  [(set (match_operand:HI 0 "general_operand" "=g,g")
	(and:HI (match_operand:HI 1 "general_operand" "0,g")
		(match_operand:HI 2 "const_int_operand" "n,n")))]
  ""
  "@
   bicw2 %H2,%0
   bicw3 %H2,%1,%0")

(define_insn ""
  [(set (match_operand:QI 0 "general_operand" "=g,g")
	(and:QI (match_operand:QI 1 "general_operand" "0,g")
		(match_operand:QI 2 "const_int_operand" "n,n")))]
  ""
  "@
   bicb2 %B2,%0
   bicb3 %B2,%1,%0")

;;- Bit set instructions.

(define_insn "iorsi3"
  [(set (match_operand:SI 0 "general_operand" "=g,g,g")
	(ior:SI (match_operand:SI 1 "general_operand" "0,g,g")
		(match_operand:SI 2 "general_operand" "g,0,g")))]
  ""
  "@
   bisl2 %2,%0
   bisl2 %1,%0
   bisl3 %2,%1,%0")

(define_insn "iorhi3"
  [(set (match_operand:HI 0 "general_operand" "=g,g,g")
	(ior:HI (match_operand:HI 1 "general_operand" "0,g,g")
		(match_operand:HI 2 "general_operand" "g,0,g")))]
  ""
  "@
   bisw2 %2,%0
   bisw2 %1,%0
   bisw3 %2,%1,%0")

(define_insn "iorqi3"
  [(set (match_operand:QI 0 "general_operand" "=g,g,g")
	(ior:QI (match_operand:QI 1 "general_operand" "0,g,g")
		(match_operand:QI 2 "general_operand" "g,0,g")))]
  ""
  "@
   bisb2 %2,%0
   bisb2 %1,%0
   bisb3 %2,%1,%0")

;;- xor instructions.

(define_insn "xorsi3"
  [(set (match_operand:SI 0 "general_operand" "=g,g,g")
	(xor:SI (match_operand:SI 1 "general_operand" "0,g,g")
		(match_operand:SI 2 "general_operand" "g,0,g")))]
  ""
  "@
   xorl2 %2,%0
   xorl2 %1,%0
   xorl3 %2,%1,%0")

(define_insn "xorhi3"
  [(set (match_operand:HI 0 "general_operand" "=g,g,g")
	(xor:HI (match_operand:HI 1 "general_operand" "0,g,g")
		(match_operand:HI 2 "general_operand" "g,0,g")))]
  ""
  "@
   xorw2 %2,%0
   xorw2 %1,%0
   xorw3 %2,%1,%0")

(define_insn "xorqi3"
  [(set (match_operand:QI 0 "general_operand" "=g,g,g")
	(xor:QI (match_operand:QI 1 "general_operand" "0,g,g")
		(match_operand:QI 2 "general_operand" "g,0,g")))]
  ""
  "@
   xorb2 %2,%0
   xorb2 %1,%0
   xorb3 %2,%1,%0")

(define_insn "negdf2"
  [(set (match_operand:DF 0 "general_operand" "=g")
	(neg:DF (match_operand:DF 1 "general_operand" "gF")))]
  ""
  "mneg%# %1,%0")

(define_insn "negsf2"
  [(set (match_operand:SF 0 "general_operand" "=g")
	(neg:SF (match_operand:SF 1 "general_operand" "gF")))]
  ""
  "mnegf %1,%0")

(define_insn "negsi2"
  [(set (match_operand:SI 0 "general_operand" "=g")
	(neg:SI (match_operand:SI 1 "general_operand" "g")))]
  ""
  "mnegl %1,%0")

(define_insn "neghi2"
  [(set (match_operand:HI 0 "general_operand" "=g")
	(neg:HI (match_operand:HI 1 "general_operand" "g")))]
  ""
  "mnegw %1,%0")

(define_insn "negqi2"
  [(set (match_operand:QI 0 "general_operand" "=g")
	(neg:QI (match_operand:QI 1 "general_operand" "g")))]
  ""
  "mnegb %1,%0")

(define_insn "one_cmplsi2"
  [(set (match_operand:SI 0 "general_operand" "=g")
	(not:SI (match_operand:SI 1 "general_operand" "g")))]
  ""
  "mcoml %1,%0")

(define_insn "one_cmplhi2"
  [(set (match_operand:HI 0 "general_operand" "=g")
	(not:HI (match_operand:HI 1 "general_operand" "g")))]
  ""
  "mcomw %1,%0")

(define_insn "one_cmplqi2"
  [(set (match_operand:QI 0 "general_operand" "=g")
	(not:QI (match_operand:QI 1 "general_operand" "g")))]
  ""
  "mcomb %1,%0")

;; Arithmetic right shift on the vax works by negating the shift count,
;; then emitting a right shift with the shift count negated.  This means
;; that all actual shift counts in the RTL will be positive.  This 
;; prevents converting shifts to ZERO_EXTRACTs with negative positions,
;; which isn't valid.
(define_expand "ashrsi3"
  [(set (match_operand:SI 0 "general_operand" "=g")
	(ashiftrt:SI (match_operand:SI 1 "general_operand" "g")
		   (match_operand:QI 2 "general_operand" "g")))]
  ""
  "
{
  if (GET_CODE (operands[2]) != CONST_INT)
    operands[2] = gen_rtx (NEG, QImode, negate_rtx (QImode, operands[2]));
}")

(define_insn ""
  [(set (match_operand:SI 0 "general_operand" "=g")
	(ashiftrt:SI (match_operand:SI 1 "general_operand" "g")
		     (match_operand:QI 2 "const_int_operand" "n")))]
  ""
  "ashl $%n2,%1,%0")

(define_insn ""
  [(set (match_operand:SI 0 "general_operand" "=g")
	(ashiftrt:SI (match_operand:SI 1 "general_operand" "g")
		     (neg:QI (match_operand:QI 2 "general_operand" "g"))))]
  ""
  "ashl %2,%1,%0")

(define_insn "ashlsi3"
  [(set (match_operand:SI 0 "general_operand" "=g")
	(ashift:SI (match_operand:SI 1 "general_operand" "g")
		   (match_operand:QI 2 "general_operand" "g")))]
  ""
  "*
{
  if (operands[2] == const1_rtx && rtx_equal_p (operands[0], operands[1]))
    return \"addl2 %0,%0\";
  if (GET_CODE (operands[1]) == REG
      && GET_CODE (operands[2]) == CONST_INT)
    {
      int i = INTVAL (operands[2]);
      if (i == 1)
	return \"addl3 %1,%1,%0\";
      if (i == 2)
	return \"moval 0[%1],%0\";
      if (i == 3)
	return \"movad 0[%1],%0\";
    }
  return \"ashl %2,%1,%0\";
}")

;; Arithmetic right shift on the vax works by negating the shift count.
(define_expand "ashrdi3"
  [(set (match_operand:DI 0 "general_operand" "=g")
	(ashiftrt:DI (match_operand:DI 1 "general_operand" "g")
		     (match_operand:QI 2 "general_operand" "g")))]
  ""
  "
{
  operands[2] = gen_rtx (NEG, QImode, negate_rtx (QImode, operands[2]));
}")

(define_insn "ashldi3"
  [(set (match_operand:DI 0 "general_operand" "=g")
	(ashift:DI (match_operand:DI 1 "general_operand" "g")
		   (match_operand:QI 2 "general_operand" "g")))]
  ""
  "ashq %2,%1,%0")

(define_insn ""
  [(set (match_operand:DI 0 "general_operand" "=g")
	(ashiftrt:DI (match_operand:DI 1 "general_operand" "g")
		     (neg:QI (match_operand:QI 2 "general_operand" "g"))))]
  ""
  "ashq %2,%1,%0")

;; We used to have expand_shift handle logical right shifts by using extzv,
;; but this make it very difficult to do lshrdi3.  Since the VAX is the
;; only machine with this kludge, it's better to just do this with a
;; define_expand and remove that case from expand_shift.

(define_expand "lshrsi3"
  [(set (match_dup 3)
	(minus:QI (const_int 32)
		  (match_dup 4)))
   (set (match_operand:SI 0 "general_operand" "=g")
	(zero_extract:SI (match_operand:SI 1 "register_operand" "r")
			 (match_dup 3)
			 (match_operand:SI 2 "register_operand" "g")))]
  ""
  "
{
  operands[3] = gen_reg_rtx (QImode);
  operands[4] = gen_lowpart (QImode, operands[2]);
}")

;; Rotate right on the vax works by negating the shift count.
(define_expand "rotrsi3"
  [(set (match_operand:SI 0 "general_operand" "=g")
	(rotatert:SI (match_operand:SI 1 "general_operand" "g")
		     (match_operand:QI 2 "general_operand" "g")))]
  ""
  "
{
  if (GET_CODE (operands[2]) != CONST_INT)
    operands[2] = gen_rtx (NEG, QImode, negate_rtx (QImode, operands[2]));
}")

(define_insn "rotlsi3"
  [(set (match_operand:SI 0 "general_operand" "=g")
	(rotate:SI (match_operand:SI 1 "general_operand" "g")
		   (match_operand:QI 2 "general_operand" "g")))]
  ""
  "rotl %2,%1,%0")

(define_insn ""
  [(set (match_operand:SI 0 "general_operand" "=g")
	(rotatert:SI (match_operand:SI 1 "general_operand" "g")
		     (match_operand:QI 2 "const_int_operand" "n")))]
  ""
  "rotl $%R2,%1,%0")

(define_insn ""
  [(set (match_operand:SI 0 "general_operand" "=g")
	(rotatert:SI (match_operand:SI 1 "general_operand" "g")
		     (neg:QI (match_operand:QI 2 "general_operand" "g"))))]
  ""
  "rotl %2,%1,%0")

;This insn is probably slower than a multiply and an add.
;(define_insn ""
;  [(set (match_operand:SI 0 "general_operand" "=g")
;	(mult:SI (plus:SI (match_operand:SI 1 "general_operand" "g")
;			  (match_operand:SI 2 "general_operand" "g"))
;		 (match_operand:SI 3 "general_operand" "g")))]
;  ""
;  "index %1,$0x80000000,$0x7fffffff,%3,%2,%0")

;; Special cases of bit-field insns which we should
;; recognize in preference to the general case.
;; These handle aligned 8-bit and 16-bit fields,
;; which can usually be done with move instructions.

(define_insn ""
  [(set (zero_extract:SI (match_operand:SI 0 "nonimmediate_operand" "+ro")
			 (match_operand:QI 1 "const_int_operand" "n")
			 (match_operand:SI 2 "const_int_operand" "n"))
	(match_operand:SI 3 "general_operand" "g"))]
   "(INTVAL (operands[1]) == 8 || INTVAL (operands[1]) == 16)
   && INTVAL (operands[2]) % INTVAL (operands[1]) == 0
   && (GET_CODE (operands[0]) == REG
       || ! mode_dependent_address_p (XEXP (operands[0], 0)))"
  "*
{
  if (REG_P (operands[0]))
    {
      if (INTVAL (operands[2]) != 0)
	return \"insv %3,%2,%1,%0\";
    }
  else
    operands[0]
      = adj_offsettable_operand (operands[0], INTVAL (operands[2]) / 8);

  if (INTVAL (operands[1]) == 8)
    return \"movb %3,%0\";
  return \"movw %3,%0\";
}")

(define_insn ""
  [(set (match_operand:SI 0 "general_operand" "=&g")
	(zero_extract:SI (match_operand:SI 1 "nonimmediate_operand" "ro")
			 (match_operand:QI 2 "const_int_operand" "n")
			 (match_operand:SI 3 "const_int_operand" "n")))]
  "(INTVAL (operands[2]) == 8 || INTVAL (operands[2]) == 16)
   && INTVAL (operands[3]) % INTVAL (operands[2]) == 0
   && (GET_CODE (operands[1]) == REG
       || ! mode_dependent_address_p (XEXP (operands[1], 0)))"
  "*
{
  if (REG_P (operands[1]))
    {
      if (INTVAL (operands[3]) != 0)
	return \"extzv %3,%2,%1,%0\";
    }
  else
    operands[1]
      = adj_offsettable_operand (operands[1], INTVAL (operands[3]) / 8);

  if (INTVAL (operands[2]) == 8)
    return \"movzbl %1,%0\";
  return \"movzwl %1,%0\";
}")

(define_insn ""
  [(set (match_operand:SI 0 "general_operand" "=g")
	(sign_extract:SI (match_operand:SI 1 "nonimmediate_operand" "ro")
			 (match_operand:QI 2 "const_int_operand" "n")
			 (match_operand:SI 3 "const_int_operand" "n")))]
  "(INTVAL (operands[2]) == 8 || INTVAL (operands[2]) == 16)
   && INTVAL (operands[3]) % INTVAL (operands[2]) == 0
   && (GET_CODE (operands[1]) == REG
       || ! mode_dependent_address_p (XEXP (operands[1], 0)))"
  "*
{
  if (REG_P (operands[1]))
    {
      if (INTVAL (operands[3]) != 0)
	return \"extv %3,%2,%1,%0\";
    }
  else
    operands[1]
      = adj_offsettable_operand (operands[1], INTVAL (operands[3]) / 8);

  if (INTVAL (operands[2]) == 8)
    return \"cvtbl %1,%0\";
  return \"cvtwl %1,%0\";
}")

;; Register-only SImode cases of bit-field insns.

(define_insn ""
  [(set (cc0)
	(compare
	 (sign_extract:SI (match_operand:SI 0 "nonmemory_operand" "r")
			  (match_operand:QI 1 "general_operand" "g")
			  (match_operand:SI 2 "general_operand" "g"))
	 (match_operand:SI 3 "general_operand" "g")))]
  ""
  "cmpv %2,%1,%0,%3")

(define_insn ""
  [(set (cc0)
	(compare
	 (zero_extract:SI (match_operand:SI 0 "register_operand" "r")
			  (match_operand:QI 1 "general_operand" "g")
			  (match_operand:SI 2 "general_operand" "g"))
	 (match_operand:SI 3 "general_operand" "g")))]
  ""
  "cmpzv %2,%1,%0,%3")

;; When the field position and size are constant and the destination
;; is a register, extv and extzv are much slower than a rotate followed
;; by a bicl or sign extension.  Because we might end up choosing ext[z]v
;; anyway, we can't allow immediate values for the primary source operand.

(define_insn ""
  [(set (match_operand:SI 0 "general_operand" "=g")
	(sign_extract:SI (match_operand:SI 1 "nonimmediate_operand" "ro")
			 (match_operand:QI 2 "general_operand" "g")
			 (match_operand:SI 3 "general_operand" "g")))]
  ""
  "*
{
  if (GET_CODE (operands[3]) != CONST_INT || GET_CODE (operands[2]) != CONST_INT
      || GET_CODE (operands[0]) != REG
      || (INTVAL (operands[2]) != 8 && INTVAL (operands[2]) != 16))
    return \"extv %3,%2,%1,%0\";
  if (INTVAL (operands[2]) == 8)
    return \"rotl %R3,%1,%0\;cvtbl %0,%0\";
  return \"rotl %R3,%1,%0\;cvtwl %0,%0\";
}")

(define_insn ""
  [(set (match_operand:SI 0 "general_operand" "=g")
	(zero_extract:SI (match_operand:SI 1 "nonimmediate_operand" "ro")
			 (match_operand:QI 2 "general_operand" "g")
			 (match_operand:SI 3 "general_operand" "g")))]
  ""
  "*
{
  if (GET_CODE (operands[3]) != CONST_INT || GET_CODE (operands[2]) != CONST_INT
      || GET_CODE (operands[0]) != REG)
    return \"extzv %3,%2,%1,%0\";
  if (INTVAL (operands[2]) == 8)
    return \"rotl %R3,%1,%0\;movzbl %0,%0\";
  if (INTVAL (operands[2]) == 16)
    return \"rotl %R3,%1,%0\;movzwl %0,%0\";
  if (INTVAL (operands[3]) & 31)
    return \"rotl %R3,%1,%0\;bicl2 %M2,%0\";
  if (rtx_equal_p (operands[0], operands[1]))
    return \"bicl2 %M2,%0\";
  return \"bicl3 %M2,%1,%0\";
}")

;; Non-register cases.
;; nonimmediate_operand is used to make sure that mode-ambiguous cases
;; don't match these (and therefore match the cases above instead).

(define_insn ""
  [(set (cc0)
	(compare
	 (sign_extract:SI (match_operand:QI 0 "nonimmediate_operand" "rm")
			  (match_operand:QI 1 "general_operand" "g")
			  (match_operand:SI 2 "general_operand" "g"))
	 (match_operand:SI 3 "general_operand" "g")))]
  ""
  "cmpv %2,%1,%0,%3")

(define_insn ""
  [(set (cc0)
	(compare
	 (zero_extract:SI (match_operand:QI 0 "nonimmediate_operand" "rm")
			  (match_operand:QI 1 "general_operand" "g")
			  (match_operand:SI 2 "general_operand" "g"))
	 (match_operand:SI 3 "general_operand" "g")))]
  ""
  "cmpzv %2,%1,%0,%3")

(define_insn "extv"
  [(set (match_operand:SI 0 "general_operand" "=g")
	(sign_extract:SI (match_operand:QI 1 "nonimmediate_operand" "rm")
			 (match_operand:QI 2 "general_operand" "g")
			 (match_operand:SI 3 "general_operand" "g")))]
  ""
  "*
{
  if (GET_CODE (operands[0]) != REG || GET_CODE (operands[2]) != CONST_INT
      || GET_CODE (operands[3]) != CONST_INT
      || (INTVAL (operands[2]) != 8 && INTVAL (operands[2]) != 16)
      || INTVAL (operands[2]) + INTVAL (operands[3]) > 32
      || side_effects_p (operands[1])
      || (GET_CODE (operands[1]) == MEM
	  && mode_dependent_address_p (XEXP (operands[1], 0))))
    return \"extv %3,%2,%1,%0\";
  if (INTVAL (operands[2]) == 8)
    return \"rotl %R3,%1,%0\;cvtbl %0,%0\";
  return \"rotl %R3,%1,%0\;cvtwl %0,%0\";
}")

(define_insn "extzv"
  [(set (match_operand:SI 0 "general_operand" "=g")
	(zero_extract:SI (match_operand:QI 1 "nonimmediate_operand" "rm")
			 (match_operand:QI 2 "general_operand" "g")
			 (match_operand:SI 3 "general_operand" "g")))]
  ""
  "*
{
  if (GET_CODE (operands[0]) != REG || GET_CODE (operands[2]) != CONST_INT
      || GET_CODE (operands[3]) != CONST_INT
      || INTVAL (operands[2]) + INTVAL (operands[3]) > 32
      || side_effects_p (operands[1])
      || (GET_CODE (operands[1]) == MEM
	  && mode_dependent_address_p (XEXP (operands[1], 0))))
    return \"extzv %3,%2,%1,%0\";
  if (INTVAL (operands[2]) == 8)
    return \"rotl %R3,%1,%0\;movzbl %0,%0\";
  if (INTVAL (operands[2]) == 16)
    return \"rotl %R3,%1,%0\;movzwl %0,%0\";
  return \"rotl %R3,%1,%0\;bicl2 %M2,%0\";
}")

(define_insn "insv"
  [(set (zero_extract:SI (match_operand:QI 0 "nonimmediate_operand" "+g")
			 (match_operand:QI 1 "general_operand" "g")
			 (match_operand:SI 2 "general_operand" "g"))
	(match_operand:SI 3 "general_operand" "g"))]
  ""
  "insv %3,%2,%1,%0")

(define_insn ""
  [(set (zero_extract:SI (match_operand:SI 0 "nonimmediate_operand" "+r")
			 (match_operand:QI 1 "general_operand" "g")
			 (match_operand:SI 2 "general_operand" "g"))
	(match_operand:SI 3 "general_operand" "g"))]
  ""
  "insv %3,%2,%1,%0")

(define_insn "jump"
  [(set (pc)
	(label_ref (match_operand 0 "" "")))]
  ""
  "jbr %l0")

(define_insn "beq"
  [(set (pc)
	(if_then_else (eq (cc0)
			  (const_int 0))
		      (label_ref (match_operand 0 "" ""))
		      (pc)))]
  ""
  "jeql %l0")

(define_insn "bne"
  [(set (pc)
	(if_then_else (ne (cc0)
			  (const_int 0))
		      (label_ref (match_operand 0 "" ""))
		      (pc)))]
  ""
  "jneq %l0")

(define_insn "bgt"
  [(set (pc)
	(if_then_else (gt (cc0)
			  (const_int 0))
		      (label_ref (match_operand 0 "" ""))
		      (pc)))]
  ""
  "jgtr %l0")

(define_insn "bgtu"
  [(set (pc)
	(if_then_else (gtu (cc0)
			   (const_int 0))
		      (label_ref (match_operand 0 "" ""))
		      (pc)))]
  ""
  "jgtru %l0")

(define_insn "blt"
  [(set (pc)
	(if_then_else (lt (cc0)
			  (const_int 0))
		      (label_ref (match_operand 0 "" ""))
		      (pc)))]
  ""
  "jlss %l0")

(define_insn "bltu"
  [(set (pc)
	(if_then_else (ltu (cc0)
			   (const_int 0))
		      (label_ref (match_operand 0 "" ""))
		      (pc)))]
  ""
  "jlssu %l0")

(define_insn "bge"
  [(set (pc)
	(if_then_else (ge (cc0)
			  (const_int 0))
		      (label_ref (match_operand 0 "" ""))
		      (pc)))]
  ""
  "jgeq %l0")

(define_insn "bgeu"
  [(set (pc)
	(if_then_else (geu (cc0)
			   (const_int 0))
		      (label_ref (match_operand 0 "" ""))
		      (pc)))]
  ""
  "jgequ %l0")

(define_insn "ble"
  [(set (pc)
	(if_then_else (le (cc0)
			  (const_int 0))
		      (label_ref (match_operand 0 "" ""))
		      (pc)))]
  ""
  "jleq %l0")

(define_insn "bleu"
  [(set (pc)
	(if_then_else (leu (cc0)
			   (const_int 0))
		      (label_ref (match_operand 0 "" ""))
		      (pc)))]
  ""
  "jlequ %l0")

;; Recognize reversed jumps.
(define_insn ""
  [(set (pc)
	(if_then_else (match_operator 0 "comparison_operator"
				      [(cc0)
				       (const_int 0)])
		      (pc)
		      (label_ref (match_operand 1 "" ""))))]
  ""
  "j%C0 %l1") ; %C0 negates condition

;; Recognize jbs, jlbs, jbc and jlbc instructions.  Note that the operand
;; of jlbs and jlbc insns are SImode in the hardware.  However, if it is
;; memory, we use QImode in the insn.  So we can't use those instructions
;; for mode-dependent addresses.

(define_insn ""
  [(set (pc)
	(if_then_else
	 (ne (zero_extract:SI (match_operand:QI 0 "nonimmediate_operand" "rQ,g")
			      (const_int 1)
			      (match_operand:SI 1 "general_operand" "I,g"))
	     (const_int 0))
	 (label_ref (match_operand 2 "" ""))
	 (pc)))]
  ""
  "@
   jlbs %0,%l2
   jbs %1,%0,%l2")

(define_insn ""
  [(set (pc)
	(if_then_else
	 (eq (zero_extract:SI (match_operand:QI 0 "nonimmediate_operand" "rQ,g")
			      (const_int 1)
			      (match_operand:SI 1 "general_operand" "I,g"))
	     (const_int 0))
	 (label_ref (match_operand 2 "" ""))
	 (pc)))]
  ""
  "@
   jlbc %0,%l2
   jbc %1,%0,%l2")

(define_insn ""
  [(set (pc)
	(if_then_else
	 (ne (zero_extract:SI (match_operand:SI 0 "register_operand" "r,r")
			      (const_int 1)
			      (match_operand:SI 1 "general_operand" "I,g"))
	     (const_int 0))
	 (label_ref (match_operand 2 "" ""))
	 (pc)))]
  ""
  "@
   jlbs %0,%l2
   jbs %1,%0,%l2")

(define_insn ""
  [(set (pc)
	(if_then_else
	 (eq (zero_extract:SI (match_operand:SI 0 "register_operand" "r,r")
			      (const_int 1)
			      (match_operand:SI 1 "general_operand" "I,g"))
	     (const_int 0))
	 (label_ref (match_operand 2 "" ""))
	 (pc)))]
  ""
  "@
   jlbc %0,%l2
   jbc %1,%0,%l2")

;; Subtract-and-jump and Add-and-jump insns.
;; These are not used when output is for the Unix assembler
;; because it does not know how to modify them to reach far.

;; Normal sob insns.

(define_insn ""
  [(set (pc)
	(if_then_else
	 (gt (plus:SI (match_operand:SI 0 "general_operand" "+g")
		      (const_int -1))
	     (const_int 0))
	 (label_ref (match_operand 1 "" ""))
	 (pc)))
   (set (match_dup 0)
	(plus:SI (match_dup 0)
		 (const_int -1)))]
  "!TARGET_UNIX_ASM"
  "jsobgtr %0,%l1")

(define_insn ""
  [(set (pc)
	(if_then_else
	 (ge (plus:SI (match_operand:SI 0 "general_operand" "+g")
		      (const_int -1))
	     (const_int 0))
	 (label_ref (match_operand 1 "" ""))
	 (pc)))
   (set (match_dup 0)
	(plus:SI (match_dup 0)
		 (const_int -1)))]
  "!TARGET_UNIX_ASM"
  "jsobgeq %0,%l1")

;; Normal aob insns.  Define a version for when operands[1] is a constant.
(define_insn ""
  [(set (pc)
	(if_then_else
	 (lt (plus:SI (match_operand:SI 0 "general_operand" "+g")
		      (const_int 1))
	     (match_operand:SI 1 "general_operand" "g"))
	 (label_ref (match_operand 2 "" ""))
	 (pc)))
   (set (match_dup 0)
	(plus:SI (match_dup 0)
		 (const_int 1)))]
  "!TARGET_UNIX_ASM"
  "jaoblss %1,%0,%l2")

(define_insn ""
  [(set (pc)
	(if_then_else
	 (lt (match_operand:SI 0 "general_operand" "+g")
	     (match_operand:SI 1 "general_operand" "g"))
	 (label_ref (match_operand 2 "" ""))
	 (pc)))
   (set (match_dup 0)
	(plus:SI (match_dup 0)
		 (const_int 1)))]
  "!TARGET_UNIX_ASM && GET_CODE (operands[1]) == CONST_INT"
  "jaoblss %P1,%0,%l2")

(define_insn ""
  [(set (pc)
	(if_then_else
	 (le (plus:SI (match_operand:SI 0 "general_operand" "+g")
		      (const_int 1))
	     (match_operand:SI 1 "general_operand" "g"))
	 (label_ref (match_operand 2 "" ""))
	 (pc)))
   (set (match_dup 0)
	(plus:SI (match_dup 0)
		 (const_int 1)))]
  "!TARGET_UNIX_ASM"
  "jaobleq %1,%0,%l2")

(define_insn ""
  [(set (pc)
	(if_then_else
	 (le (match_operand:SI 0 "general_operand" "+g")
	     (match_operand:SI 1 "general_operand" "g"))
	 (label_ref (match_operand 2 "" ""))
	 (pc)))
   (set (match_dup 0)
	(plus:SI (match_dup 0)
		 (const_int 1)))]
  "!TARGET_UNIX_ASM && GET_CODE (operands[1]) == CONST_INT"
  "jaobleq %P1,%0,%l2")

;; Something like a sob insn, but compares against -1.
;; This finds `while (foo--)' which was changed to `while (--foo != -1)'.

(define_insn ""
  [(set (pc)
	(if_then_else
	 (ne (match_operand:SI 0 "general_operand" "g")
	     (const_int 0))
	 (label_ref (match_operand 1 "" ""))
	 (pc)))
   (set (match_dup 0)
	(plus:SI (match_dup 0)
		 (const_int -1)))]
  ""
  "decl %0\;jgequ %l1")

;; Note that operand 1 is total size of args, in bytes,
;; and what the call insn wants is the number of words.
(define_insn "call_pop"
  [(call (match_operand:QI 0 "memory_operand" "m")
	 (match_operand:QI 1 "general_operand" "g"))
   (set (reg:SI 14) (plus:SI (reg:SI 14)
			     (match_operand:SI 3 "immediate_operand" "i")))]
  ""
  "*
  if (GET_CODE (operands[1]) != CONST_INT || INTVAL (operands[1]) > 255 * 4)
    /* Vax `calls' really uses only one byte of #args, so pop explicitly.  */
    return \"calls $0,%0\;addl2 %1,sp\";
  operands[1] = gen_rtx (CONST_INT, VOIDmode, (INTVAL (operands[1]) + 3)/ 4);
  return \"calls %1,%0\";
")

(define_insn "call_value_pop"
  [(set (match_operand 0 "" "=g")
	(call (match_operand:QI 1 "memory_operand" "m")
	      (match_operand:QI 2 "general_operand" "g")))
   (set (reg:SI 14) (plus:SI (reg:SI 14)
			     (match_operand:SI 4 "immediate_operand" "i")))]
  ""
  "*
  if (GET_CODE (operands[2]) != CONST_INT || INTVAL (operands[2]) > 255 * 4)
    /* Vax `calls' really uses only one byte of #args, so pop explicitly.  */
    return \"calls $0,%1\;addl2 %2,sp\";
  operands[2] = gen_rtx (CONST_INT, VOIDmode, (INTVAL (operands[2]) + 3)/ 4);
  return \"calls %2,%1\";
")

;; Define another set of these for the case of functions with no
;; operands.  In that case, combine may simplify the adjustment of sp.
(define_insn ""
  [(call (match_operand:QI 0 "memory_operand" "m")
	 (match_operand:QI 1 "general_operand" "g"))
   (set (reg:SI 14) (reg:SI 14))]
  ""
  "*
  if (GET_CODE (operands[1]) != CONST_INT || INTVAL (operands[1]) > 255 * 4)
    /* Vax `calls' really uses only one byte of #args, so pop explicitly.  */
    return \"calls $0,%0\;addl2 %1,sp\";
  operands[1] = gen_rtx (CONST_INT, VOIDmode, (INTVAL (operands[1]) + 3)/ 4);
  return \"calls %1,%0\";
")

(define_insn ""
  [(set (match_operand 0 "" "=g")
	(call (match_operand:QI 1 "memory_operand" "m")
	      (match_operand:QI 2 "general_operand" "g")))
   (set (reg:SI 14) (reg:SI 14))]
  ""
  "*
  if (GET_CODE (operands[2]) != CONST_INT || INTVAL (operands[2]) > 255 * 4)
    /* Vax `calls' really uses only one byte of #args, so pop explicitly.  */
    return \"calls $0,%1\;addl2 %2,sp\";
  operands[2] = gen_rtx (CONST_INT, VOIDmode, (INTVAL (operands[2]) + 3)/ 4);
  return \"calls %2,%1\";
")

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

  emit_call_insn (gen_call_pop (operands[0], const0_rtx, NULL, const0_rtx));

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
  [(unspec_volatile [(const_int 0)] 0)]
  ""
  "")

(define_insn "return"
  [(return)]
  ""
  "ret")

(define_insn "nop"
  [(const_int 0)]
  ""
  "nop")

;; This had a wider constraint once, and it had trouble.
;; If you are tempted to try `g', please don't--it's not worth
;; the risk we will reopen the same bug.
(define_insn "indirect_jump"
  [(set (pc) (match_operand:SI 0 "general_operand" "r"))]
  ""
  "jmp (%0)")

;; This is here to accept 5 arguments (as passed by expand_end_case)
;; and pass the first 4 along to the casesi1 pattern that really does the work.
(define_expand "casesi"
  [(set (pc)
	(if_then_else (leu (minus:SI (match_operand:SI 0 "general_operand" "g")
				     (match_operand:SI 1 "general_operand" "g"))
			   (match_operand:SI 2 "general_operand" "g"))
		      (plus:SI (sign_extend:SI
				(mem:HI
				 (plus:SI (pc)
					  (mult:SI (minus:SI (match_dup 0)
							     (match_dup 1))
						   (const_int 2)))))
			       (label_ref:SI (match_operand 3 "" "")))
		      (pc)))
   (match_operand 4 "" "")]
  ""
  "
  emit_insn (gen_casesi1 (operands[0], operands[1], operands[2], operands[3]));
  DONE;
")

(define_insn "casesi1"
  [(set (pc)
	(if_then_else (leu (minus:SI (match_operand:SI 0 "general_operand" "g")
				     (match_operand:SI 1 "general_operand" "g"))
			   (match_operand:SI 2 "general_operand" "g"))
		      (plus:SI (sign_extend:SI
				(mem:HI
				 (plus:SI (pc)
					  (mult:SI (minus:SI (match_dup 0)
							     (match_dup 1))
						   (const_int 2)))))
			       (label_ref:SI (match_operand 3 "" "")))
		      (pc)))]
  ""
  "casel %0,%1,%2")

;; This used to arise from the preceding by simplification
;; if operand 1 is zero.  Perhaps it is no longer necessary.
(define_insn ""
  [(set (pc)
	(if_then_else (leu (match_operand:SI 0 "general_operand" "g")
			   (match_operand:SI 1 "general_operand" "g"))
		      (plus:SI (sign_extend:SI
				(mem:HI
				 (plus:SI (pc)
					  (mult:SI (minus:SI (match_dup 0)
							     (const_int 0))
						   (const_int 2)))))
			       (label_ref:SI (match_operand 3 "" "")))
		      (pc)))]
  ""
  "casel %0,$0,%1")

;;- load or push effective address 
;; These come after the move and add/sub patterns
;; because we don't want pushl $1 turned into pushad 1.
;; or addl3 r1,r2,r3 turned into movab 0(r1)[r2],r3.

;; It does not work to use constraints to distinguish pushes from moves,
;; because < matches any autodecrement, not just a push.

(define_insn ""
  [(set (match_operand:SI 0 "general_operand" "=g")
	(match_operand:QI 1 "address_operand" "p"))]
  ""
  "*
{
  if (push_operand (operands[0], SImode))
    return \"pushab %a1\";
  else
    return \"movab %a1,%0\";
}")

(define_insn ""
  [(set (match_operand:SI 0 "general_operand" "=g")
	(match_operand:HI 1 "address_operand" "p"))]
  ""
  "*
{
  if (push_operand (operands[0], SImode))
    return \"pushaw %a1\";
  else
    return \"movaw %a1,%0\";
}")

(define_insn ""
  [(set (match_operand:SI 0 "general_operand" "=g")
	(match_operand:SI 1 "address_operand" "p"))]
  ""
  "*
{
  if (push_operand (operands[0], SImode))
    return \"pushal %a1\";
  else
    return \"moval %a1,%0\";
}")

(define_insn ""
  [(set (match_operand:SI 0 "general_operand" "=g")
	(match_operand:DI 1 "address_operand" "p"))]
  ""
  "*
{
  if (push_operand (operands[0], SImode))
    return \"pushaq %a1\";
  else
    return \"movaq %a1,%0\";
}")

(define_insn ""
  [(set (match_operand:SI 0 "general_operand" "=g")
	(match_operand:SF 1 "address_operand" "p"))]
  ""
  "*
{
  if (push_operand (operands[0], SImode))
    return \"pushaf %a1\";
  else
    return \"movaf %a1,%0\";
}")

(define_insn ""
  [(set (match_operand:SI 0 "general_operand" "=g")
	(match_operand:DF 1 "address_operand" "p"))]
  ""
  "*
{
  if (push_operand (operands[0], SImode))
    return \"pushad %a1\";
  else
    return \"movad %a1,%0\";
}")

;; These used to be peepholes, but it is more straightforward to do them
;; as single insns.  However, we must force the output to be a register
;; if it is not an offsettable address so that we know that we can assign
;; to it twice. 

;; If we had a good way of evaluating the relative costs, these could be
;; machine-independent.

;; Optimize   extzv ...,z;    andl2 ...,z
;; or	      ashl ...,z;     andl2 ...,z
;; with other operands constant.  This is what the combiner converts the
;; above sequences to before attempting to recognize the new insn.

(define_insn ""
  [(set (match_operand:SI 0 "general_operand" "=ro")
	(and:SI (ashiftrt:SI (match_operand:SI 1 "general_operand" "g")
			     (match_operand:QI 2 "const_int_operand" "n"))
		(match_operand:SI 3 "const_int_operand" "n")))]
  "(INTVAL (operands[3]) & ~((1 << (32 - INTVAL (operands[2]))) - 1)) == 0"
  "*
{
  unsigned long mask1 = INTVAL (operands[3]);
  unsigned long mask2 = (1 << (32 - INTVAL (operands[2]))) - 1;

  if ((mask1 & mask2) != mask1)
    operands[3] = gen_rtx (CONST_INT, VOIDmode, mask1 & mask2);

  return \"rotl %R2,%1,%0\;bicl2 %N3,%0\";
}")

;; left-shift and mask
;; The only case where `ashl' is better is if the mask only turns off
;; bits that the ashl would anyways, in which case it should have been
;; optimized away.

(define_insn ""
  [(set (match_operand:SI 0 "general_operand" "=ro")
	(and:SI (ashift:SI (match_operand:SI 1 "general_operand" "g")
			   (match_operand:QI 2 "const_int_operand" "n"))
		(match_operand:SI 3 "const_int_operand" "n")))]
  ""
  "*
{
  operands[3] = gen_rtx (CONST_INT, VOIDmode,
			 INTVAL (operands[3]) & ~((1 << INTVAL (operands[2])) - 1));
  return \"rotl %2,%1,%0\;bicl2 %N3,%0\";
}")
