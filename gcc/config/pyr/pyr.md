;; GNU C machine description for Pyramid 90x, 9000, MIServer Series
;; Copyright (C) 1989, 1990, 1995, 1997 Free Software Foundation, Inc.

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

;; Instruction patterns.  When multiple patterns apply,
;; the first one in the file is chosen.
;;
;; See file "rtl.def" for documentation on define_insn, match_*, et. al.
;;
;; cpp macro #define NOTICE_UPDATE_CC in file tm.h handles condition code
;; updates for most instructions.

;; These comments are mostly obsolete.  Written for gcc version 1.XX.
;; * Try using define_insn instead of some peepholes in more places.
;; * Set REG_NOTES:REG_EQUIV for cvt[bh]w loads.  This would make the
;;   backward scan in sign_extend needless.
;; * Match (pc) (label_ref) case in peephole patterns.
;; * Should optimize
;;   "cmpX op1,op2;  b{eq,ne} LY;  ucmpX op1.op2;  b{lt,le,gt,ge} LZ"
;;   to
;;   "ucmpX op1,op2;  b{eq,ne} LY;  b{lt,le,gt,ge} LZ"
;;   by pre-scanning insn and running notice_update_cc for them.
;; * Is it necessary to do copy_rtx in the test and compare patterns?
;; * Fix true frame pointer omission.
;; * Make the jump tables contain branches, not addresses!  This would
;;   save us one instruction.
;; * Could the complicated scheme for compares be simplified, if we had
;;   no named cmpqi or cmphi patterns, and instead anonymous patterns for
;;   the less-than-word compare cases pyr can handle???
;; * The jump insn seems to accept more than just IR addressing.  Would
;;   we win by telling GCC?  Or can we use movw into the global reg which
;;   is a synonym for pc?
;; * More DImode patterns.
;; * Scan backwards in "zero_extendhisi2", "zero_extendqisi2" to find out
;;   if the extension can be omitted.
;; * "divmodsi" with Pyramid "ediv" insn.  Is it possible in rtl??
;; * Would "rcsp tmpreg; u?cmp[bh] op1_regdispl(tmpreg),op2" win in
;;   comparison with the two extensions and single test generated now?
;;   The rcsp insn could be expanded, and moved out of loops by the
;;   optimizer, making 1 (64 bit) insn of 3 (32 bit) insns in loops.
;;   The rcsp insn could be followed by an add insn, making non-displacement
;;   IR addressing sufficient.

;______________________________________________________________________
;
;	Test and Compare Patterns.
;______________________________________________________________________

; The argument for the rather complicated test and compare expansion
; scheme, is the irregular pyramid instructions for these operations.
; 1) Pyramid has different signed and unsigned compares.  2) HImode
; and QImode integers are memory-memory and immediate-memory only.  3)
; Unsigned HImode compares doesn't exist.  4) Only certain
; combinations of addresses are allowed for memory-memory compares.
; Whenever necessary, in order to fulfill these addressing
; constraints, the compare operands are swapped.

(define_expand "tstsi"
  [(set (cc0)
	(match_operand:SI 0 "general_operand" ""))]
  "" "operands[0] = force_reg (SImode, operands[0]);")

(define_insn ""
  [(set (cc0)
	(compare (match_operand:SI 0 "memory_operand" "m")
		 (match_operand:SI 1 "memory_operand" "m")))]
  "weird_memory_memory (operands[0], operands[1])"
  "*
{
  rtx br_insn = NEXT_INSN (insn);
  RTX_CODE br_code;

  if (GET_CODE (br_insn) != JUMP_INSN)
    abort();
  br_code =  GET_CODE (XEXP (XEXP (PATTERN (br_insn), 1), 0));

  weird_memory_memory (operands[0], operands[1]);

  if (swap_operands)
    {
      cc_status.flags = CC_REVERSED;
      if (TRULY_UNSIGNED_COMPARE_P (br_code))
	{
	  cc_status.mdep = CC_VALID_FOR_UNSIGNED;
	  return \"ucmpw %0,%1\";
	}
      return \"cmpw %0,%1\";
    }

  if (TRULY_UNSIGNED_COMPARE_P (br_code))
    {
      cc_status.mdep = CC_VALID_FOR_UNSIGNED;
      return \"ucmpw %1,%0\";
    }
  return \"cmpw %1,%0\";
}")

(define_insn "cmpsi"
  [(set (cc0)
	(compare (match_operand:SI 0 "nonimmediate_operand" "r,g")
		 (match_operand:SI 1 "general_operand" "g,r")))]
  ""
  "*
{
  rtx br_insn = NEXT_INSN (insn);
  RTX_CODE br_code;

  if (GET_CODE (br_insn) != JUMP_INSN)
    abort();
  br_code =  GET_CODE (XEXP (XEXP (PATTERN (br_insn), 1), 0));

  if (which_alternative != 0)
    {
      cc_status.flags = CC_REVERSED;
      if (TRULY_UNSIGNED_COMPARE_P (br_code))
	{
	  cc_status.mdep = CC_VALID_FOR_UNSIGNED;
	  return \"ucmpw %0,%1\";
	}
      return \"cmpw %0,%1\";
    }

  if (TRULY_UNSIGNED_COMPARE_P (br_code))
    {
      cc_status.mdep = CC_VALID_FOR_UNSIGNED;
      return \"ucmpw %1,%0\";
    }
  return \"cmpw %1,%0\";
}")

(define_insn ""
  [(set (cc0)
	(match_operand:SI 0 "nonimmediate_operand" "r"))]
  ""
  "*
{
#if 0
  cc_status.flags |= CC_NO_OVERFLOW;
  return \"cmpw $0,%0\";
#endif
  rtx br_insn = NEXT_INSN (insn);
  RTX_CODE br_code;

  if (GET_CODE (br_insn) != JUMP_INSN)
    abort();
  br_code =  GET_CODE (XEXP (XEXP (PATTERN (br_insn), 1), 0));

  if (TRULY_UNSIGNED_COMPARE_P (br_code))
    {
      cc_status.mdep = CC_VALID_FOR_UNSIGNED;
      return \"ucmpw $0,%0\";
    }
  return \"mtstw %0,%0\";
}")

(define_expand "cmphi"
  [(set (cc0)
	(compare (match_operand:HI 0 "nonimmediate_operand" "")
		 (match_operand:HI 1 "general_operand" "")))]
  ""
  "
{
  extern rtx test_op0, test_op1;  extern enum machine_mode test_mode;
  test_op0 = copy_rtx (operands[0]);
  test_op1 = copy_rtx (operands[1]);
  test_mode = HImode;
  DONE;
}")

(define_expand "tsthi"
  [(set (cc0)
	(match_operand:HI 0 "nonimmediate_operand" ""))]
  ""
  "
{
  extern rtx test_op0;  extern enum machine_mode test_mode;
  test_op0 = copy_rtx (operands[0]);
  test_mode = HImode;
  DONE;
}")

(define_insn ""
  [(set (cc0)
	(compare (match_operand:HI 0 "memory_operand" "m")
		 (match_operand:HI 1 "memory_operand" "m")))]
  "(!TRULY_UNSIGNED_COMPARE_P (GET_CODE (XEXP (SET_SRC (PATTERN (NEXT_INSN (insn))), 0))))
   && weird_memory_memory (operands[0], operands[1])"
  "*
{
  rtx br_insn = NEXT_INSN (insn);

  if (GET_CODE (br_insn) != JUMP_INSN)
    abort();

  weird_memory_memory (operands[0], operands[1]);

  if (swap_operands)
    {
      cc_status.flags = CC_REVERSED;
      return \"cmph %0,%1\";
    }

  return \"cmph %1,%0\";
}")

(define_insn ""
  [(set (cc0)
	(compare (match_operand:HI 0 "nonimmediate_operand" "r,m")
		 (match_operand:HI 1 "nonimmediate_operand" "m,r")))]
  "(!TRULY_UNSIGNED_COMPARE_P (GET_CODE (XEXP (SET_SRC (PATTERN (NEXT_INSN (insn))), 0))))
   && ((GET_CODE (operands[0]) == MEM) != (GET_CODE (operands[1]) == MEM))"
  "*
{
  rtx br_insn = NEXT_INSN (insn);

  if (GET_CODE (br_insn) != JUMP_INSN)
    abort();

  if (which_alternative != 0)
    {
      cc_status.flags = CC_REVERSED;
      return \"cmph %0,%1\";
    }

  return \"cmph %1,%0\";
}")

(define_expand "cmpqi"
  [(set (cc0)
	(compare (match_operand:QI 0 "nonimmediate_operand" "")
		 (match_operand:QI 1 "general_operand" "")))]
  ""
  "
{
  extern rtx test_op0, test_op1;  extern enum machine_mode test_mode;
  test_op0 = copy_rtx (operands[0]);
  test_op1 = copy_rtx (operands[1]);
  test_mode = QImode;
  DONE;
}")

(define_expand "tstqi"
  [(set (cc0)
	(match_operand:QI 0 "nonimmediate_operand" ""))]
  ""
  "
{
  extern rtx test_op0;  extern enum machine_mode test_mode;
  test_op0 = copy_rtx (operands[0]);
  test_mode = QImode;
  DONE;
}")

(define_insn ""
  [(set (cc0)
	(compare (match_operand:QI 0 "memory_operand" "m")
		 (match_operand:QI 1 "memory_operand" "m")))]
  "weird_memory_memory (operands[0], operands[1])"
  "*
{
  rtx br_insn = NEXT_INSN (insn);
  RTX_CODE br_code;

  if (GET_CODE (br_insn) != JUMP_INSN)
    abort();
  br_code =  GET_CODE (XEXP (XEXP (PATTERN (br_insn), 1), 0));

  weird_memory_memory (operands[0], operands[1]);

  if (swap_operands)
    {
      cc_status.flags = CC_REVERSED;
      if (TRULY_UNSIGNED_COMPARE_P (br_code))
	{
	  cc_status.mdep = CC_VALID_FOR_UNSIGNED;
	  return \"ucmpb %0,%1\";
	}
      return \"cmpb %0,%1\";
    }

  if (TRULY_UNSIGNED_COMPARE_P (br_code))
    {
      cc_status.mdep = CC_VALID_FOR_UNSIGNED;
      return \"ucmpb %1,%0\";
    }
  return \"cmpb %1,%0\";
}")

(define_insn ""
  [(set (cc0)
	(compare (match_operand:QI 0 "nonimmediate_operand" "r,m")
		 (match_operand:QI 1 "nonimmediate_operand" "m,r")))]
  "((GET_CODE (operands[0]) == MEM) != (GET_CODE (operands[1]) == MEM))"
  "*
{
  rtx br_insn = NEXT_INSN (insn);
  RTX_CODE br_code;

  if (GET_CODE (br_insn) != JUMP_INSN)
    abort();
  br_code =  GET_CODE (XEXP (XEXP (PATTERN (br_insn), 1), 0));

  if (which_alternative != 0)
    {
      cc_status.flags = CC_REVERSED;
      if (TRULY_UNSIGNED_COMPARE_P (br_code))
	{
	  cc_status.mdep = CC_VALID_FOR_UNSIGNED;
	  return \"ucmpb %0,%1\";
	}
      return \"cmpb %0,%1\";
    }

  if (TRULY_UNSIGNED_COMPARE_P (br_code))
    {
      cc_status.mdep = CC_VALID_FOR_UNSIGNED;
      return \"ucmpb %1,%0\";
    }
  return \"cmpb %1,%0\";
}")

(define_expand "bgt"
  [(set (pc) (if_then_else (gt (cc0) (const_int 0))
			   (label_ref (match_operand 0 "" "")) (pc)))]
  "" "extend_and_branch (SIGN_EXTEND);")

(define_expand "blt"
  [(set (pc) (if_then_else (lt (cc0) (const_int 0))
			   (label_ref (match_operand 0 "" "")) (pc)))]
  "" "extend_and_branch (SIGN_EXTEND);")

(define_expand "bge"
  [(set (pc) (if_then_else (ge (cc0) (const_int 0))
			   (label_ref (match_operand 0 "" "")) (pc)))]
  "" "extend_and_branch (SIGN_EXTEND);")

(define_expand "ble"
  [(set (pc) (if_then_else (le (cc0) (const_int 0))
			   (label_ref (match_operand 0 "" "")) (pc)))]
  "" "extend_and_branch (SIGN_EXTEND);")

(define_expand "beq"
  [(set (pc) (if_then_else (eq (cc0) (const_int 0))
			   (label_ref (match_operand 0 "" "")) (pc)))]
  "" "extend_and_branch (SIGN_EXTEND);")

(define_expand "bne"
  [(set (pc) (if_then_else (ne (cc0) (const_int 0))
			   (label_ref (match_operand 0 "" "")) (pc)))]
  "" "extend_and_branch (SIGN_EXTEND);")

(define_expand "bgtu"
  [(set (pc) (if_then_else (gtu (cc0) (const_int 0))
			   (label_ref (match_operand 0 "" "")) (pc)))]
  "" "extend_and_branch (ZERO_EXTEND);")

(define_expand "bltu"
  [(set (pc) (if_then_else (ltu (cc0) (const_int 0))
			   (label_ref (match_operand 0 "" "")) (pc)))]
  "" "extend_and_branch (ZERO_EXTEND);")

(define_expand "bgeu"
  [(set (pc) (if_then_else (geu (cc0) (const_int 0))
			   (label_ref (match_operand 0 "" "")) (pc)))]
  "" "extend_and_branch (ZERO_EXTEND);")

(define_expand "bleu"
  [(set (pc) (if_then_else (leu (cc0) (const_int 0))
			   (label_ref (match_operand 0 "" "")) (pc)))]
  "" "extend_and_branch (ZERO_EXTEND);")

(define_insn "cmpdf"
  [(set (cc0)
	(compare (match_operand:DF 0 "register_operand" "r")
		 (match_operand:DF 1 "register_operand" "r")))]
  ""
  "cmpd %1,%0")

(define_insn "cmpsf"
  [(set (cc0)
	(compare (match_operand:SF 0 "register_operand" "r")
		 (match_operand:SF 1 "register_operand" "r")))]
  ""
  "cmpf %1,%0")

(define_insn "tstdf"
  [(set (cc0)
       	(match_operand:DF 0 "register_operand" "r"))]
  ""
  "mtstd %0,%0")

(define_insn "tstsf"
  [(set (cc0)
       	(match_operand:SF 0 "register_operand" "r"))]
  ""
  "mtstf %0,%0")

;______________________________________________________________________
;
;	Fixed-point Arithmetic.
;______________________________________________________________________

(define_insn "addsi3"
  [(set (match_operand:SI 0 "register_operand" "=r,!r")
	(plus:SI (match_operand:SI 1 "general_operand" "%0,r")
		 (match_operand:SI 2 "general_operand" "g,rJ")))]
  ""
  "*
{
  if (which_alternative == 0)
    return (GET_CODE (operands[2]) == CONST_INT && INTVAL (operands[2]) == 32
	    ? \"subw %n2,%0\" : \"addw %2,%0\");
  else
    {
      forget_cc_if_dependent (operands[0]);
      return \"mova %a2[%1*1],%0\";
    }
}")

(define_insn "subsi3"
  [(set (match_operand:SI 0 "register_operand" "=r,r")
	(minus:SI (match_operand:SI 1 "general_operand" "0,g")
		  (match_operand:SI 2 "general_operand" "g,0")))]
  ""
  "* return (which_alternative == 0) ? \"subw %2,%0\" : \"rsubw %1,%0\";")

(define_insn "mulsi3"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(mult:SI (match_operand:SI 1 "general_operand" "%0")
		 (match_operand:SI 2 "general_operand" "g")))]
  ""
  "mulw %2,%0")

(define_insn "divsi3"
  [(set (match_operand:SI 0 "register_operand" "=r,r")
	(div:SI (match_operand:SI 1 "general_operand" "0,g")
		(match_operand:SI 2 "general_operand" "g,0")))]
  ""
  "* return (which_alternative == 0) ? \"divw %2,%0\" : \"rdivw %1,%0\";")

(define_insn "udivsi3"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(udiv:SI (match_operand:SI 1 "register_operand" "0")
		 (match_operand:SI 2 "general_operand" "g")))]
  ""
  "udivw %2,%0")

(define_insn "modsi3"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(mod:SI (match_operand:SI 1 "register_operand" "0")
		(match_operand:SI 2 "general_operand" "g")))]
  ""
  "modw %2,%0")

(define_insn "umodsi3"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(umod:SI (match_operand:SI 1 "register_operand" "0")
		 (match_operand:SI 2 "general_operand" "g")))]
  ""
  "umodw %2,%0")

(define_insn "negsi2"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(neg:SI (match_operand:SI 1 "nonimmediate_operand" "rm")))]
  ""
  "mnegw %1,%0")

(define_insn "one_cmplsi2"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(not:SI (match_operand:SI 1 "nonimmediate_operand" "rm")))]
  ""
  "mcomw %1,%0")

(define_insn "abssi2"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(abs:SI (match_operand:SI 1 "nonimmediate_operand" "rm")))]
  ""
  "mabsw %1,%0")

;______________________________________________________________________
;
;	Floating-point Arithmetic.
;______________________________________________________________________

(define_insn "adddf3"
  [(set (match_operand:DF 0 "register_operand" "=r")
	(plus:DF (match_operand:DF 1 "register_operand" "%0")
		 (match_operand:DF 2 "register_operand" "r")))]
  ""
  "addd %2,%0")

(define_insn "addsf3"
  [(set (match_operand:SF 0 "register_operand" "=r")
	(plus:SF (match_operand:SF 1 "register_operand" "%0")
		 (match_operand:SF 2 "register_operand" "r")))]
  ""
  "addf %2,%0")

(define_insn "subdf3"
  [(set (match_operand:DF 0 "register_operand" "=r")
	(minus:DF (match_operand:DF 1 "register_operand" "0")
		  (match_operand:DF 2 "register_operand" "r")))]
  ""
  "subd %2,%0")

(define_insn "subsf3"
  [(set (match_operand:SF 0 "register_operand" "=r")
	(minus:SF (match_operand:SF 1 "register_operand" "0")
		  (match_operand:SF 2 "register_operand" "r")))]
  ""
  "subf %2,%0")

(define_insn "muldf3"
  [(set (match_operand:DF 0 "register_operand" "=r")
	(mult:DF (match_operand:DF 1 "register_operand" "%0")
		 (match_operand:DF 2 "register_operand" "r")))]
  ""
  "muld %2,%0")

(define_insn "mulsf3"
  [(set (match_operand:SF 0 "register_operand" "=r")
	(mult:SF (match_operand:SF 1 "register_operand" "%0")
		 (match_operand:SF 2 "register_operand" "r")))]
  ""
  "mulf %2,%0")

(define_insn "divdf3"
  [(set (match_operand:DF 0 "register_operand" "=r")
	(div:DF (match_operand:DF 1 "register_operand" "0")
		(match_operand:DF 2 "register_operand" "r")))]
  ""
  "divd %2,%0")

(define_insn "divsf3"
  [(set (match_operand:SF 0 "register_operand" "=r")
	(div:SF (match_operand:SF 1 "register_operand" "0")
		(match_operand:SF 2 "register_operand" "r")))]
  ""
  "divf %2,%0")

(define_insn "negdf2"
  [(set (match_operand:DF 0 "register_operand" "=r")
	(neg:DF (match_operand:DF 1 "register_operand" "r")))]
  ""
  "mnegd %1,%0")

(define_insn "negsf2"
  [(set (match_operand:SF 0 "register_operand" "=r")
	(neg:SF (match_operand:SF 1 "register_operand" "r")))]
  ""
  "mnegf %1,%0")

(define_insn "absdf2"
  [(set (match_operand:DF 0 "register_operand" "=r")
	(abs:DF (match_operand:DF 1 "register_operand" "r")))]
  ""
  "mabsd %1,%0")

(define_insn "abssf2"
  [(set (match_operand:SF 0 "register_operand" "=r")
	(abs:SF (match_operand:SF 1 "register_operand" "r")))]
  ""
  "mabsf %1,%0")

;______________________________________________________________________
;
;	Logical and Shift Instructions.
;______________________________________________________________________

(define_insn ""
  [(set (cc0)
	(and:SI (match_operand:SI 0 "general_operand" "%r")
		(match_operand:SI 1 "general_operand" "g")))]
  ""
  "*
{
  cc_status.flags |= CC_NO_OVERFLOW;
  return \"bitw %1,%0\";
}")

(define_insn "andsi3"
  [(set (match_operand:SI 0 "register_operand" "=r,r")
	(and:SI (match_operand:SI 1 "general_operand" "%0,r")
		(match_operand:SI 2 "general_operand" "g,K")))]
  ""
  "*
{
  if (which_alternative == 0)
    return \"andw %2,%0\";

  cc_status.flags = CC_NOT_NEGATIVE;
  return (INTVAL (operands[2]) == 255
	  ? \"movzbw %1,%0\" : \"movzhw %1,%0\");
}")

(define_insn ""
  [(set (match_operand:SI 0 "register_operand" "=r")
	(and:SI (not:SI (match_operand:SI 1 "general_operand" "g"))
		(match_operand:SI 2 "register_operand" "0")))]
  ""
  "bicw %1,%0")

(define_insn "iorsi3"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(ior:SI (match_operand:SI 1 "general_operand" "%0")
		(match_operand:SI 2 "general_operand" "g")))]
  ""
  "orw %2,%0")

(define_insn "xorsi3"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(xor:SI (match_operand:SI 1 "general_operand" "%0")
		(match_operand:SI 2 "general_operand" "g")))]
  ""
  "xorw %2,%0")

; The arithmetic left shift instructions work strangely on pyramids.
; They fail to modify the sign bit.  Therefore, use logic shifts.

(define_insn "ashlsi3"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(ashift:SI (match_operand:SI 1 "register_operand" "0")
		   (match_operand:SI 2 "general_operand" "rnm")))]
  ""
  "*
{
  extern char *output_shift ();
  return output_shift (\"lshlw %2,%0\", operands[2], 32);
}")

(define_insn "ashrsi3"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(ashiftrt:SI (match_operand:SI 1 "register_operand" "0")
		     (match_operand:SI 2 "general_operand" "rnm")))]
  ""
  "*
{
  extern char *output_shift ();
  return output_shift (\"ashrw %2,%0\", operands[2], 32);
}")

(define_insn "ashrdi3"
  [(set (match_operand:DI 0 "register_operand" "=r")
	(ashiftrt:DI (match_operand:DI 1 "register_operand" "0")
		     (match_operand:SI 2 "general_operand" "rnm")))]
  ""
  "*
{
  extern char *output_shift ();
  return output_shift (\"ashrl %2,%0\", operands[2], 64);
}")

(define_insn "lshrsi3"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(lshiftrt:SI (match_operand:SI 1 "register_operand" "0")
		     (match_operand:SI 2 "general_operand" "rnm")))]
  ""
  "*
{
  extern char *output_shift ();
  return output_shift (\"lshrw %2,%0\", operands[2], 32);
}")

(define_insn "rotlsi3"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(rotate:SI (match_operand:SI 1 "register_operand" "0")
		   (match_operand:SI 2 "general_operand" "rnm")))]
  ""
  "*
{
  extern char *output_shift ();
  return output_shift (\"rotlw %2,%0\", operands[2], 32);
}")

(define_insn "rotrsi3"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(rotatert:SI (match_operand:SI 1 "register_operand" "0")
		     (match_operand:SI 2 "general_operand" "rnm")))]
  ""
  "*
{
  extern char *output_shift ();
  return output_shift (\"rotrw %2,%0\", operands[2], 32);
}")

;______________________________________________________________________
;
;	Fixed and Floating Moves.
;______________________________________________________________________

;; If the destination is a memory operand, indexed source operands are
;; disallowed.  Big DImode constants are always loaded into a reg pair,
;; although offsettable memory addresses really could be dealt with.

(define_insn ""
  [(set (match_operand:DI 0 "memory_operand" "=m")
	(match_operand:DI 1 "nonindexed_operand" "gF"))]
  "(GET_CODE (operands[1]) == CONST_DOUBLE
     ? ((CONST_DOUBLE_HIGH (operands[1]) == 0
	 && CONST_DOUBLE_LOW (operands[1]) >= 0)
	|| (CONST_DOUBLE_HIGH (operands[1]) == -1
	    && CONST_DOUBLE_LOW (operands[1]) < 0))
     : 1)"
  "*
{
  if (GET_CODE (operands[1]) == CONST_DOUBLE)
    operands[1] = GEN_INT (CONST_DOUBLE_LOW (operands[1]));
  return \"movl %1,%0\";
}")

;; Force the destination to a register, so all source operands are allowed.

(define_insn "movdi"
  [(set (match_operand:DI 0 "general_operand" "=r")
	(match_operand:DI 1 "general_operand" "gF"))]
  ""
  "*
{
  extern char *output_move_double ();
  return output_move_double (operands);
}")

;; If the destination is a memory address, indexed source operands are
;; disallowed.

(define_insn ""
  [(set (match_operand:SI 0 "memory_operand" "=m")
	(match_operand:SI 1 "nonindexed_operand" "g"))]
  ""
  "movw %1,%0")

;; Force the destination to a register, so all source operands are allowed.

(define_insn "movsi"
  [(set (match_operand:SI 0 "general_operand" "=r")
	(match_operand:SI 1 "general_operand" "g"))]
  ""
  "movw %1,%0")

;; If the destination is a memory address, indexed source operands are
;; disallowed.

(define_insn ""
  [(set (match_operand:HI 0 "memory_operand" "=m")
	(match_operand:HI 1 "nonindexed_operand" "g"))]
  ""
  "*
{
  if (REG_P (operands[1]))
    return \"cvtwh %1,%0\";		/* reg -> mem */
  else
    return \"movh %1,%0\";		/* mem imm -> mem */
}")

;; Force the destination to a register, so all source operands are allowed.

(define_insn "movhi"
  [(set (match_operand:HI 0 "general_operand" "=r")
	(match_operand:HI 1 "general_operand" "g"))]
  ""
  "*
{
  if (GET_CODE (operands[1]) != MEM)
    return \"movw %1,%0\";		/* reg imm -> reg  */
  return \"cvthw %1,%0\";		/* mem -> reg */
}")

;; If the destination is a memory address, indexed source operands are
;; disallowed.

(define_insn ""
  [(set (match_operand:QI 0 "memory_operand" "=m")
	(match_operand:QI 1 "nonindexed_operand" "g"))]
  ""
  "*
{
  if (REG_P (operands[1]))
    return \"cvtwb %1,%0\";		/* reg -> mem */
  else
    return \"movb %1,%0\";		/* mem imm -> mem */
}")

;; Force the destination to a register, so all source operands are allowed.

(define_insn "movqi"
  [(set (match_operand:QI 0 "general_operand" "=r")
	(match_operand:QI 1 "general_operand" "g"))]
  ""
  "*
{
  if (GET_CODE (operands[1]) != MEM)
    return \"movw %1,%0\";		/* reg imm -> reg  */
  return \"cvtbw %1,%0\";		/* mem -> reg */
}")

;; If the destination is a memory address, indexed source operands are
;; disallowed.

(define_insn ""
  [(set (match_operand:DF 0 "memory_operand" "=m")
	(match_operand:DF 1 "nonindexed_operand" "g"))]
  "GET_CODE (operands[1]) != CONST_DOUBLE"
  "movl %1,%0")

;; Force the destination to a register, so all source operands are allowed.

(define_insn "movdf"
  [(set (match_operand:DF 0 "general_operand" "=r")
	(match_operand:DF 1 "general_operand" "gF"))]
  ""
  "*
{
  extern char *output_move_double ();
  return output_move_double (operands);
}")

;; If the destination is a memory address, indexed source operands are
;; disallowed.

(define_insn ""
  [(set (match_operand:SF 0 "memory_operand" "=m")
	(match_operand:SF 1 "nonindexed_operand" "g"))]
  ""
  "movw %1,%0")

;; Force the destination to a register, so all source operands are allowed.

(define_insn "movsf"
  [(set (match_operand:SF 0 "general_operand" "=r")
	(match_operand:SF 1 "general_operand" "g"))]
  ""
  "movw %1,%0")

(define_insn ""
  [(set (match_operand:SI 0 "register_operand" "=r")
	(match_operand:QI 1 "address_operand" "p"))]
  ""
  "*
{
  forget_cc_if_dependent (operands[0]);
  return \"mova %a1,%0\";
}")

;______________________________________________________________________
;
;	Conversion patterns.
;______________________________________________________________________

;; The trunc patterns are used only when non compile-time constants are used.

(define_insn "truncsiqi2"
  [(set (match_operand:QI 0 "register_operand" "=r")
	(truncate:QI (match_operand:SI 1 "nonimmediate_operand" "rm")))]
  ""
  "*
{
  if (REG_P (operands[0]) && REG_P (operands[1])
      && REGNO (operands[0]) == REGNO (operands[1]))
    {
      cc_status = cc_prev_status;
      return \"\";
    }
  forget_cc_if_dependent (operands[0]);
  return \"movw %1,%0\";
}")

(define_insn "truncsihi2"
  [(set (match_operand:HI 0 "register_operand" "=r")
	(truncate:HI (match_operand:SI 1 "nonimmediate_operand" "rm")))]
  ""
  "*
{
  if (REG_P (operands[0]) && REG_P (operands[1])
      && REGNO (operands[0]) == REGNO (operands[1]))
    {
      cc_status = cc_prev_status;
      return \"\";
    }
  forget_cc_if_dependent (operands[0]);
  return \"movw %1,%0\";
}")

(define_insn "extendhisi2"
  [(set (match_operand:SI 0 "general_operand" "=r,m")
	(sign_extend:SI (match_operand:HI 1 "nonimmediate_operand" "rm,r")))]
  ""
  "*
{
  if (optimize && REG_P (operands[0]) && REG_P (operands[1])
      && REGNO (operands[0]) == REGNO (operands[1])
      && already_sign_extended (insn, HImode, operands[0]))
    {
      cc_status = cc_prev_status;
      return \"\";
    }
  return \"cvthw %1,%0\";
}")

(define_insn "extendqisi2"
  [(set (match_operand:SI 0 "general_operand" "=r,m")
	(sign_extend:SI (match_operand:QI 1 "nonimmediate_operand" "rm,r")))]
  ""
  "*
{
  if (optimize && REG_P (operands[0]) && REG_P (operands[1])
      && REGNO (operands[0]) == REGNO (operands[1])
      && already_sign_extended (insn, QImode, operands[0]))
    {
      cc_status = cc_prev_status;
      return \"\";
    }
  return \"cvtbw %1,%0\";
}")

; Pyramid doesn't have insns *called* "cvtbh" or "movzbh".
; But we can cvtbw/movzbw into a register, where there is no distinction
; between words and halfwords.

(define_insn "extendqihi2"
  [(set (match_operand:HI 0 "register_operand" "=r")
	(sign_extend:HI (match_operand:QI 1 "nonimmediate_operand" "rm")))]
  ""
  "cvtbw %1,%0")

(define_insn "zero_extendhisi2"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(zero_extend:SI (match_operand:HI 1 "nonimmediate_operand" "rm")))]
  ""
  "*
{
  cc_status.flags = CC_NOT_NEGATIVE;
  return \"movzhw %1,%0\";
}")

(define_insn "zero_extendqisi2"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(zero_extend:SI (match_operand:QI 1 "nonimmediate_operand" "rm")))]
  ""
  "*
{
  cc_status.flags = CC_NOT_NEGATIVE;
  return \"movzbw %1,%0\";
}")

(define_insn "zero_extendqihi2"
  [(set (match_operand:HI 0 "register_operand" "=r")
	(zero_extend:HI (match_operand:QI 1 "nonimmediate_operand" "rm")))]
  ""
  "*
{
  cc_status.flags = CC_NOT_NEGATIVE;
  return \"movzbw %1,%0\";
}")

(define_insn "extendsfdf2"
  [(set (match_operand:DF 0 "general_operand" "=&r,m")
	(float_extend:DF (match_operand:SF 1 "nonimmediate_operand" "rm,r")))]
  ""
  "cvtfd %1,%0")

(define_insn "truncdfsf2"
  [(set (match_operand:SF 0 "general_operand" "=&r,m")
	(float_truncate:SF (match_operand:DF 1 "nonimmediate_operand" "rm,r")))]
  ""
  "cvtdf %1,%0")

(define_insn "floatsisf2"
  [(set (match_operand:SF 0 "general_operand" "=&r,m")
	(float:SF (match_operand:SI 1 "nonimmediate_operand" "rm,r")))]
  ""
  "cvtwf %1,%0")

(define_insn "floatsidf2"
  [(set (match_operand:DF 0 "general_operand" "=&r,m")
	(float:DF (match_operand:SI 1 "nonimmediate_operand" "rm,r")))]
  ""
  "cvtwd %1,%0")

(define_insn "fix_truncsfsi2"
  [(set (match_operand:SI 0 "general_operand" "=&r,m")
	(fix:SI (fix:SF (match_operand:SF 1 "nonimmediate_operand" "rm,r"))))]
  ""
  "cvtfw %1,%0")

(define_insn "fix_truncdfsi2"
  [(set (match_operand:SI 0 "general_operand" "=&r,m")
	(fix:SI (fix:DF (match_operand:DF 1 "nonimmediate_operand" "rm,r"))))]
  ""
  "cvtdw %1,%0")

;______________________________________________________________________
;
;	Flow Control Patterns.
;______________________________________________________________________

;; Prefer "br" to "jump" for unconditional jumps, since it's faster.
;; (The assembler can manage with out-of-range branches.)

(define_insn "jump"
  [(set (pc)
	(label_ref (match_operand 0 "" "")))]
  ""
  "br %l0")

(define_insn ""
  [(set (pc)
	(if_then_else (match_operator 0 "relop" [(cc0) (const_int 0)])
		      (label_ref (match_operand 1 "" ""))
		      (pc)))]
  ""
  "*
{
  if (optimize)
    switch (GET_CODE (operands[0]))
      {
      case EQ: case NE:
	break;
      case LT: case LE: case GE: case GT:
	if (cc_prev_status.mdep == CC_VALID_FOR_UNSIGNED)
	  return 0;
	break;
      case LTU: case LEU: case GEU: case GTU:
	if (cc_prev_status.mdep != CC_VALID_FOR_UNSIGNED)
	  return 0;
	break;
      }

  return \"b%N0 %l1\";
}")

(define_insn ""
  [(set (pc)
	(if_then_else (match_operator 0 "relop" [(cc0) (const_int 0)])
		      (pc)
		      (label_ref (match_operand 1 "" ""))))]
  ""
  "*
{
  if (optimize)
    switch (GET_CODE (operands[0]))
      {
      case EQ: case NE:
	break;
      case LT: case LE: case GE: case GT:
	if (cc_prev_status.mdep == CC_VALID_FOR_UNSIGNED)
	  return 0;
	break;
      case LTU: case LEU: case GEU: case GTU:
	if (cc_prev_status.mdep != CC_VALID_FOR_UNSIGNED)
	  return 0;
	break;
      }

  return \"b%C0 %l1\";
}")

(define_insn "call"
  [(call (match_operand:QI 0 "memory_operand" "m")
	 (match_operand:SI 1 "immediate_operand" "n"))]
  ""
  "call %0")

(define_insn "call_value"
  [(set (match_operand 0 "" "=r")
	(call (match_operand:QI 1 "memory_operand" "m")
	      (match_operand:SI 2 "immediate_operand" "n")))]
  ;; Operand 2 not really used on Pyramid architecture.
  ""
  "call %1")

(define_insn "return"
  [(return)]
  ""
  "*
{
  if (get_frame_size () + current_function_pretend_args_size
      + current_function_args_size != 0
      || current_function_calls_alloca)
    {
      int dealloc_size = current_function_pretend_args_size;
      if (current_function_pops_args)
        dealloc_size += current_function_args_size;
      operands[0] = GEN_INT (dealloc_size);
      return \"retd %0\";
    }
  else
    return \"ret\";
}")

(define_insn "tablejump"
  [(set (pc) (match_operand:SI 0 "register_operand" "r"))
   (use (label_ref (match_operand 1 "" "")))]
  ""
  "jump (%0)")

(define_insn "nop"
  [(const_int 0)]
  ""
  "movw gr0,gr0  # nop")

;______________________________________________________________________
;
;	Peep-hole Optimization Patterns.
;______________________________________________________________________

;; Optimize fullword move followed by a test of the moved value.

(define_peephole
  [(set (match_operand:SI 0 "register_operand" "=r")
	(match_operand:SI 1 "nonimmediate_operand" "rm"))
   (set (cc0) (match_operand:SI 2 "nonimmediate_operand" "rm"))]
  "rtx_equal_p (operands[2], operands[0])
   || rtx_equal_p (operands[2], operands[1])"
  "*
  cc_status.flags |= CC_NO_OVERFLOW;
  return \"mtstw %1,%0\";
")

;; Optimize loops with an incremented/decremented variable.

(define_peephole
  [(set (match_operand:SI 0 "register_operand" "=r")
	(plus:SI (match_dup 0)
		 (const_int -1)))
   (set (cc0)
	(compare (match_operand:SI 1 "register_operand" "r")
		 (match_operand:SI 2 "nonmemory_operand" "ri")))
   (set (pc)
	(if_then_else (match_operator:SI 3 "signed_comparison"
			 [(cc0) (const_int 0)])
		      (label_ref (match_operand 4 "" ""))
		      (pc)))]
  "(GET_CODE (operands[2]) == CONST_INT
    ? (unsigned)INTVAL (operands[2]) + 32 >= 64
    : 1) && (rtx_equal_p (operands[0], operands[1])
	     || rtx_equal_p (operands[0], operands[2]))"
  "*
  if (rtx_equal_p (operands[0], operands[1]))
    {
      output_asm_insn (\"dcmpw %2,%0\", operands);
      return \"b%N3 %l4\";
    }
  else
    {
      output_asm_insn (\"dcmpw %1,%0\", operands);
      return \"b%R3 %l4\";
    }
")

(define_peephole
  [(set (match_operand:SI 0 "register_operand" "=r")
	(plus:SI (match_dup 0)
		 (const_int 1)))
   (set (cc0)
	(compare (match_operand:SI 1 "register_operand" "r")
		 (match_operand:SI 2 "nonmemory_operand" "ri")))
   (set (pc)
	(if_then_else (match_operator:SI 3 "signed_comparison"
			 [(cc0) (const_int 0)])
		      (label_ref (match_operand 4 "" ""))
		      (pc)))]
  "(GET_CODE (operands[2]) == CONST_INT
    ? (unsigned)INTVAL (operands[2]) + 32 >= 64
    : 1) && (rtx_equal_p (operands[0], operands[1])
	     || rtx_equal_p (operands[0], operands[2]))"
  "*
  if (rtx_equal_p (operands[0], operands[1]))
    {
      output_asm_insn (\"icmpw %2,%0\", operands);
      return \"b%N3 %l4\";
    }
  else
    {
      output_asm_insn (\"icmpw %1,%0\", operands);
      return \"b%R3 %l4\";
    }
")

;; Combine two word moves with consecutive operands into one long move.
;; Also combines immediate moves, if the high-order destination operand
;; is loaded with 0 or -1 and the low-order destination operand is loaded
;; with a constant with the same sign.

(define_peephole
  [(set (match_operand:SI 0 "general_operand" "=g")
	(match_operand:SI 1 "general_operand" "g"))
   (set (match_operand:SI 2 "general_operand" "=g")
	(match_operand:SI 3 "general_operand" "g"))]
  "movdi_possible (operands)"
  "*
{
  output_asm_insn (\"# COMBINE movw %1,%0\", operands);
  output_asm_insn (\"# COMBINE movw %3,%2\", operands);
  movdi_possible (operands);
  if (CONSTANT_P (operands[1]))
    return (swap_operands ? \"movl %3,%0\" : \"movl %1,%2\");

  return (swap_operands ? \"movl %1,%0\" : \"movl %3,%2\");
}")

;; Optimize certain tests after memory stores.

(define_peephole
  [(set (match_operand 0 "memory_operand" "=m")
	(match_operand 1 "register_operand" "r"))
   (set (match_operand:SI 2 "register_operand" "=r")
	(sign_extend:SI (match_dup 1)))
   (set (cc0)
	(match_dup 2))]
  "dead_or_set_p (insn, operands[2])"
  "*
  cc_status.flags |= CC_NO_OVERFLOW;
  if (GET_MODE (operands[0]) == QImode)
    return \"cvtwb %1,%0\";
  else
    return \"cvtwh %1,%0\";
")

;______________________________________________________________________
;
;	DImode Patterns.
;______________________________________________________________________

(define_expand "extendsidi2"
  [(set (subreg:SI (match_operand:DI 0 "register_operand" "=r") 1)
	(match_operand:SI 1 "general_operand" "g"))
   (set (subreg:SI (match_dup 0) 0)
	(subreg:SI (match_dup 0) 1))
   (set (subreg:SI (match_dup 0) 0)
	(ashiftrt:SI (subreg:SI (match_dup 0) 0)
		     (const_int 31)))]
  ""
  "")

(define_insn "adddi3"
  [(set (match_operand:DI 0 "register_operand" "=r")
	(plus:DI (match_operand:DI 1 "nonmemory_operand" "%0")
		 (match_operand:DI 2 "nonmemory_operand" "rF")))]
  ""
  "*
{
  rtx xoperands[2];
  CC_STATUS_INIT;
  xoperands[0] = gen_rtx (REG, SImode, REGNO (operands[0]) + 1);
  if (REG_P (operands[2]))
    xoperands[1] = gen_rtx (REG, SImode, REGNO (operands[2]) + 1);
  else
    {
      xoperands[1] = GEN_INT (CONST_DOUBLE_LOW (operands[2]));
      operands[2] = GEN_INT (CONST_DOUBLE_HIGH (operands[2]));
    }
  output_asm_insn (\"addw %1,%0\", xoperands);
  return \"addwc %2,%0\";
}")

(define_insn "subdi3"
  [(set (match_operand:DI 0 "register_operand" "=r")
	(minus:DI (match_operand:DI 1 "register_operand" "0")
		  (match_operand:DI 2 "nonmemory_operand" "rF")))]
  ""
  "*
{
  rtx xoperands[2];
  CC_STATUS_INIT;
  xoperands[0] = gen_rtx (REG, SImode, REGNO (operands[0]) + 1);
  if (REG_P (operands[2]))
    xoperands[1] = gen_rtx (REG, SImode, REGNO (operands[2]) + 1);
  else
    {
      xoperands[1] = GEN_INT (CONST_DOUBLE_LOW (operands[2]));
      operands[2] = GEN_INT (CONST_DOUBLE_HIGH (operands[2]));
    }
  output_asm_insn (\"subw %1,%0\", xoperands);
  return \"subwb %2,%0\";
}")

(define_insn "iordi3"
  [(set (match_operand:DI 0 "register_operand" "=r")
	(ior:DI (match_operand:DI 1 "nonmemory_operand" "%0")
		(match_operand:DI 2 "nonmemory_operand" "rF")))]
  ""
  "*
{
  rtx xoperands[2];
  CC_STATUS_INIT;
  xoperands[0] = gen_rtx (REG, SImode, REGNO (operands[0]) + 1);
  if (REG_P (operands[2]))
    xoperands[1] = gen_rtx (REG, SImode, REGNO (operands[2]) + 1);
  else
    {
      xoperands[1] = GEN_INT (CONST_DOUBLE_LOW (operands[2]));
      operands[2] = GEN_INT (CONST_DOUBLE_HIGH (operands[2]));
    }
  output_asm_insn (\"orw %1,%0\", xoperands);
  return \"orw %2,%0\";
}")

(define_insn "anddi3"
  [(set (match_operand:DI 0 "register_operand" "=r")
	(and:DI (match_operand:DI 1 "nonmemory_operand" "%0")
		(match_operand:DI 2 "nonmemory_operand" "rF")))]
  ""
  "*
{
  rtx xoperands[2];
  CC_STATUS_INIT;
  xoperands[0] = gen_rtx (REG, SImode, REGNO (operands[0]) + 1);
  if (REG_P (operands[2]))
    xoperands[1] = gen_rtx (REG, SImode, REGNO (operands[2]) + 1);
  else
    {
      xoperands[1] = GEN_INT (CONST_DOUBLE_LOW (operands[2]));
      operands[2] = GEN_INT (CONST_DOUBLE_HIGH (operands[2]));
    }
  output_asm_insn (\"andw %1,%0\", xoperands);
  return \"andw %2,%0\";
}")

(define_insn "xordi3"
  [(set (match_operand:DI 0 "register_operand" "=r")
	(xor:DI (match_operand:DI 1 "nonmemory_operand" "%0")
		(match_operand:DI 2 "nonmemory_operand" "rF")))]
  ""
  "*
{
  rtx xoperands[2];
  CC_STATUS_INIT;
  xoperands[0] = gen_rtx (REG, SImode, REGNO (operands[0]) + 1);
  if (REG_P (operands[2]))
    xoperands[1] = gen_rtx (REG, SImode, REGNO (operands[2]) + 1);
  else
    {
      xoperands[1] = GEN_INT (CONST_DOUBLE_LOW (operands[2]));
      operands[2] = GEN_INT (CONST_DOUBLE_HIGH (operands[2]));
    }
  output_asm_insn (\"xorw %1,%0\", xoperands);
  return \"xorw %2,%0\";
}")

;; My version, modelled after Jonathan Stone's and "tablejump" - S.P.
(define_insn "indirect_jump"
  [(set (pc) (match_operand:SI 0 "general_operand" "r"))]
  ""
  "jump (%0)")
