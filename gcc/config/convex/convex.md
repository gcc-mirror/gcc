;;- Machine description for GNU compiler
;;- Convex Version
;;   Copyright (C) 1991 Free Software Foundation, Inc.

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

;; Scheduling defs
;;
;; Insn scheduling is not used at present.  Scheduling increases
;; register pressure so much that many spills are generated
;; even for very small functions.

;; Compares

(define_insn "tstsi"
  [(set (cc0)
	(match_operand:SI 0 "register_operand" "r"))]
  ""
  "* return set_cmp (operands[0], const0_rtx, 'w');")

(define_insn "tsthi"
  [(set (cc0)
	(match_operand:HI 0 "register_operand" "r"))]
  ""
  "* return set_cmp (operands[0], const0_rtx, 'h');")

(define_expand "tstqi"
  [(set (match_dup 1)
	(sign_extend:SI (match_operand:QI 0 "register_operand" "r")))
   (set (cc0)
	(match_dup 1))]
  ""
  "operands[1] = gen_reg_rtx (SImode);")

(define_expand "tstdi"
  [(parallel [(set (cc0) (match_operand:DI 0 "register_operand" "d"))
	      (use (match_dup 1))])]
  ""
  "operands[1] = force_reg (DImode, const0_rtx);")

(define_insn ""
  [(set (cc0) (match_operand:DI 0 "register_operand" "d"))
   (use (match_operand:DI 1 "register_operand" "d"))]
  ""
  "* return set_cmp (operands[0], operands[1], 'l');")

(define_expand "tstdf"
  [(set (cc0)
	(compare (match_operand:DF 0 "register_operand" "d")
		 (match_dup 1)))]
  ""
  "operands[1] = force_reg (DFmode, CONST0_RTX (DFmode));")

(define_insn "tstsf"
  [(set (cc0)
	(match_operand:SF 0 "register_operand" "d"))]
  ""
  "* return set_cmp (operands[0], CONST0_RTX (SFmode), 's');")

(define_insn "cmpsi"
  [(set (cc0)
	(compare (match_operand:SI 0 "register_operand" "d,a,i,r")
		 (match_operand:SI 1 "nonmemory_operand" "d,a,r,i")))]
  ""
  "* return set_cmp (operands[0], operands[1], 'w');")

(define_insn "cmphi"
  [(set (cc0)
	(compare (match_operand:HI 0 "register_operand" "d,a,r,i")
		 (match_operand:HI 1 "nonmemory_operand" "d,a,i,r")))]
  ""
  "* return set_cmp (operands[0], operands[1], 'h');")

(define_insn "cmpqi"
  [(set (cc0)
	(compare (match_operand:QI 0 "register_operand" "d")
		 (match_operand:QI 1 "register_operand" "d")))]
  ""
  "* return set_cmp (operands[0], operands[1], 'b');")

(define_insn "cmpdi"
  [(set (cc0)
	(compare (match_operand:DI 0 "register_operand" "d")
		 (match_operand:DI 1 "register_operand" "d")))]
  ""
  "* return set_cmp (operands[0], operands[1], 'l');")

(define_insn "cmpdf"
  [(set (cc0)
	(compare (match_operand:DF 0 "register_operand" "d")
		 (match_operand:DF 1 "register_operand" "d")))]
  ""
  "* return set_cmp (operands[0], operands[1], 'd');")

(define_insn "cmpsf"
  [(set (cc0)
	(compare (match_operand:SF 0 "nonmemory_operand" "dF,d")
		 (match_operand:SF 1 "nonmemory_operand" "d,F")))]
  ""
  "* return set_cmp (operands[0], operands[1], 's');")

;; Moves

;(define_insn "movtf"
;  [(set (match_operand:TF 0 "general_operand" "=g,d")
;	(match_operand:TF 1 "general_operand" "d,g"))]
;  ""
;  "*
;{
;  rtx opaddr = 0;
;  rtx xoperands[4];
;  xoperands[0] = operands[0];
;  xoperands[2] = operands[1];
;
;  if (REG_P (operands[0]))
;    xoperands[1] = gen_rtx (REG, TFmode, REGNO (operands[0]) + 1);
;  else if (GET_CODE (XEXP (operands[0], 0)) == PRE_DEC)
;    xoperands[1] = 0;
;  else if (offsettable_memref_p (operands[0]))
;    xoperands[1] = adj_offsettable_operand (operands[0], 8);
;  else
;    {
;      opaddr = XEXP (operands[0], 0);
;      xoperands[0] = gen_rtx (MEM, TFmode, gen_rtx (REG, SImode, 13));
;      xoperands[1] = adj_offsettable_operand (xoperands[0], 8);
;    }
;
;  if (REG_P (operands[1]))
;    xoperands[3] = gen_rtx (REG, TFmode, REGNO (operands[1]) + 1);
;  else if (offsettable_memref_p (operands[1]))
;    xoperands[3] = adj_offsettable_operand (operands[1], 8);
;  else
;    {
;      opaddr = XEXP (operands[1], 0);
;      xoperands[2] = gen_rtx (MEM, TFmode, gen_rtx (REG, SImode, 13));
;      xoperands[3] = adj_offsettable_operand (xoperands[2], 8);
;    }
;
;  if (opaddr)
;    output_asm_insn (\"psh.w a5\;ld.w %0,a5\", &opaddr);
;  if (push_operand (operands[0], TFmode))
;    output_asm_insn (\"psh.l %3\;psh.l %2\", xoperands);
;  else if (GET_CODE (operands[0]) == MEM)
;    output_asm_insn (\"st.l %2,%0\;st.l %3,%1\", xoperands);
;  else if (GET_CODE (operands[1]) == REG)
;    output_asm_insn (\"mov %2,%0\;mov %3,%1\", xoperands);
;  else
;    output_asm_insn (\"ld.l %2,%0\;ld.l %3,%1\", xoperands);
;  if (opaddr)
;    output_asm_insn (\"pop.w a5\");
;  return \"\";
;}")

(define_insn "movdf"
  [(set (match_operand:DF 0 "general_operand" "=g,d")
	(match_operand:DF 1 "general_operand" "d,dmG"))]
  ""
  "*
{
  if (push_operand (operands[0], DFmode))
    return \"psh.l %1\";
  else if (GET_CODE (operands[0]) == MEM)
    return \"st.l %1,%0\";
  else if (GET_CODE (operands[1]) == REG)
    return \"mov %1,%0\";
  else if (GET_CODE (operands[1]) == CONST_DOUBLE && LD_D_P (operands[1]))
    {
      operands[1] = gen_rtx (CONST_INT, VOIDmode,
			     const_double_high_int (operands[1]));
      return \"ld.d %1,%0\";
    }
  else if (GET_CODE (operands[1]) == CONST_DOUBLE && LD_L_P (operands[1]))
    {
      operands[1] = gen_rtx (CONST_INT, VOIDmode,
			     const_double_low_int (operands[1]));
      return \"ld.l %1,%0\";
    }
  else
    return \"ld.l %1,%0\";
}")

(define_insn "movsf"
  [(set (match_operand:SF 0 "general_operand" "=g,d")
	(match_operand:SF 1 "general_operand" "d,gF"))]
  ""
  "*
{
  if (push_operand (operands[0], SFmode))
    return \"psh.w %1\";
  else if (GET_CODE (operands[0]) == MEM)
    return \"st.s %1,%0\";
  else if (GET_CODE (operands[1]) == REG)
    return \"mov.s %1,%0\";
  else
    return \"ld.s %1,%0\";
}")

(define_insn "movdi"
  [(set (match_operand:DI 0 "general_operand" "=g,d")
	(match_operand:DI 1 "general_operand" "d,dmiG"))]
  ""
  "*
{
  if (push_operand (operands[0], DImode))
    return \"psh.l %1\";
  else if (GET_CODE (operands[0]) == MEM)
    return \"st.l %1,%0\";
  else if (GET_CODE (operands[1]) == REG)
    return \"mov %1,%0\";
  else if (GET_CODE (operands[1]) == CONST_DOUBLE && LD_D_P (operands[1]))
    {
      operands[1] = gen_rtx (CONST_INT, VOIDmode,
			     const_double_high_int (operands[1]));
      return \"ld.d %1,%0\";
    }
  else
    return \"ld.l %1,%0\";
}")

;; Special case of movsi, needed to express A-reg preference.

(define_insn ""
  [(set (match_operand:SI 0 "push_operand" "=<")
	(plus:SI (match_operand:SI 1 "register_operand" "a")
		 (match_operand:SI 2 "immediate_operand" "i")))]
  "operands[1] != stack_pointer_rtx"
  "pshea %a2(%1)")

;; General movsi.  Constraints will be selected based on TARGET_INDIRECTS
;; to avoid indirect addressing on C3, where it is slow.

(define_expand "movsi"
  [(set (match_operand:SI 0 "general_operand" "")
	(match_operand:SI 1 "general_operand" ""))]
  ""
  "")

(define_insn ""
  [(set (match_operand:SI 0 "push_operand" "=<,<")
	(match_operand:SI 1 "general_operand" "Ad,io"))]
  ""
  "@
   psh.w %1
   pshea %a1")

(define_insn ""
  [(set (match_operand:SI 0 "general_operand" "=g,r,<")
	(match_operand:SI 1 "general_operand" "r,g,io"))]
  "TARGET_INDIRECTS"
  "*
{ 
  if (push_operand (operands[0], SImode))
    {
      if (GET_CODE (operands[1]) == REG)
	return \"psh.w %1\";
      else
        return \"pshea %a1\";
    }
  if (GET_CODE (operands[0]) == MEM)
    return \"st.w %1,%0\";
  if (GET_CODE (operands[1]) != REG)
    return \"ld.w %1,%0\";
  if (S_REG_P (operands[0]) && S_REG_P (operands[1]))
    return \"mov.w %1,%0\";
  return \"mov %1,%0\";
}")

(define_insn ""
  [(set (match_operand:SI 0 "general_operand" "=g,r,<")
	(match_operand:SI 1 "general_operand" "r,g,i"))]
  "! TARGET_INDIRECTS"
  "*
{ 
  if (push_operand (operands[0], SImode))
    {
      if (GET_CODE (operands[1]) == REG)
	return \"psh.w %1\";
      else
        return \"pshea %a1\";
    }
  if (GET_CODE (operands[0]) == MEM)
    return \"st.w %1,%0\";
  if (GET_CODE (operands[1]) != REG)
    return \"ld.w %1,%0\";
  if (S_REG_P (operands[0]) && S_REG_P (operands[1]))
    return \"mov.w %1,%0\";
  return \"mov %1,%0\";
}")

(define_insn "movstrictsi"
  [(set (strict_low_part (match_operand:SI 0 "general_operand" "+g,r"))
	(match_operand:SI 1 "general_operand" "r,g"))]
  ""
  "*
{ 
  if (GET_CODE (operands[0]) == MEM)
    return \"st.w %1,%0\";
  if (GET_CODE (operands[1]) != REG)
    return \"ld.w %1,%0\";
  if (S_REG_P (operands[0]) && S_REG_P (operands[1]))
    return \"mov.w %1,%0\";
  return \"mov %1,%0\";
}")

(define_insn "movhi"
  [(set (match_operand:HI 0 "general_operand" "=g,r")
	(match_operand:HI 1 "general_operand" "r,g"))]
  ""
  "*
{
  if (push_operand (operands[0], HImode))
    abort ();
  else if (GET_CODE (operands[0]) == MEM)
    return \"st.h %1,%0\";
  else if (GET_CODE (operands[1]) == REG) 
    {
      if (S_REG_P (operands[0]) && S_REG_P (operands[1]))
	return \"mov.w %1,%0\";
      else
        return \"mov %1,%0\";
    }
  else if (GET_CODE (operands[1]) == CONST_INT)
    return \"ld.w %1,%0\";
  else
    return \"ld.h %1,%0\";
}")

(define_insn "movqi"
  [(set (match_operand:QI 0 "general_operand" "=g,r")
	(match_operand:QI 1 "general_operand" "r,g"))]
  ""
  "*
{
  if (push_operand (operands[0], QImode))
    abort ();
  else if (GET_CODE (operands[0]) == MEM)
    return \"st.b %1,%0\";
  else if (GET_CODE (operands[1]) == REG)
    {
      if (S_REG_P (operands[0]) && S_REG_P (operands[1]))
	return \"mov.w %1,%0\";
      else
        return \"mov %1,%0\";
    }
  else if (GET_CODE (operands[1]) == CONST_INT)
    return \"ld.w %1,%0\";
  else
    return \"ld.b %1,%0\";
}")

;; Extension and truncation insns.
;; Those for integer source operand
;; are ordered widest source type first.

(define_insn "truncsiqi2"
  [(set (match_operand:QI 0 "register_operand" "=d,a")
	(truncate:QI (match_operand:SI 1 "register_operand" "d,a")))]
  ""
  "cvtw.b %1,%0")

(define_insn "truncsihi2"
  [(set (match_operand:HI 0 "register_operand" "=d,a")
	(truncate:HI (match_operand:SI 1 "register_operand" "d,a")))]
  ""
  "cvtw.h %1,%0")

(define_insn "trunchiqi2"
  [(set (match_operand:QI 0 "register_operand" "=r")
	(truncate:QI (match_operand:HI 1 "register_operand" "0")))]
  ""
  "")

(define_insn "truncdisi2"
  [(set (match_operand:SI 0 "register_operand" "=d")
	(truncate:SI (match_operand:DI 1 "register_operand" "d")))]
  ""
  "cvtl.w %1,%0")

(define_insn "extendsidi2"
  [(set (match_operand:DI 0 "register_operand" "=d")
	(sign_extend:DI (match_operand:SI 1 "register_operand" "d")))]
  ""
  "cvtw.l %1,%0")

(define_insn "extendhisi2"
  [(set (match_operand:SI 0 "register_operand" "=d,a")
	(sign_extend:SI (match_operand:HI 1 "register_operand" "d,a")))]
  ""
  "cvth.w %1,%0")

(define_insn "extendqihi2"
  [(set (match_operand:HI 0 "register_operand" "=d,a")
	(sign_extend:HI (match_operand:QI 1 "register_operand" "d,a")))]
  ""
  "cvtb.w %1,%0")

(define_insn "extendqisi2"
  [(set (match_operand:SI 0 "register_operand" "=d,a")
	(sign_extend:SI (match_operand:QI 1 "register_operand" "d,a")))]
  ""
  "cvtb.w %1,%0")

(define_insn "extendsfdf2"
  [(set (match_operand:DF 0 "register_operand" "=d")
	(float_extend:DF (match_operand:SF 1 "register_operand" "d")))]
  ""
  "cvts.d %1,%0")

(define_insn "truncdfsf2"
  [(set (match_operand:SF 0 "register_operand" "=d")
	(float_truncate:SF (match_operand:DF 1 "register_operand" "d")))]
  ""
  "cvtd.s %1,%0")

(define_insn "zero_extendhisi2"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(zero_extend:SI (match_operand:HI 1 "register_operand" "0")))]
  ""
  "and #0xffff,%0")

(define_insn "zero_extendqihi2"
  [(set (match_operand:HI 0 "register_operand" "=r")
	(zero_extend:HI (match_operand:QI 1 "register_operand" "0")))]
  ""
  "and #0xff,%0")

(define_insn "zero_extendqisi2"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(zero_extend:SI (match_operand:QI 1 "register_operand" "0")))]
  ""
  "and #0xff,%0")

(define_insn "zero_extendsidi2"
  [(set (match_operand:DI 0 "register_operand" "=d")
	(zero_extend:DI (match_operand:SI 1 "register_operand" "0")))]
  ""
  "ld.u #0,%0")

;; Fix-to-float conversion insns.
;; Note that the ones that start with SImode come first.
;; That is so that an operand that is a CONST_INT
;; (and therefore lacks a specific machine mode).
;; will be recognized as SImode (which is always valid)
;; rather than as QImode or HImode.

(define_insn "floatsisf2"
  [(set (match_operand:SF 0 "register_operand" "=d")
	(float:SF (match_operand:SI 1 "register_operand" "d")))]
  ""
  "cvtw.s %1,%0")

(define_insn "floatdisf2"
  [(set (match_operand:SF 0 "register_operand" "=d")
	(float:SF (match_operand:DI 1 "register_operand" "d")))]
  ""
  "cvtl.s %1,%0")

(define_insn "floatsidf2"
  [(set (match_operand:DF 0 "register_operand" "=d")
	(float:DF (match_operand:SI 1 "register_operand" "d")))]
  "TARGET_C2"
  "cvtw.d %1,%0")

(define_insn "floatdidf2"
  [(set (match_operand:DF 0 "register_operand" "=d")
	(float:DF (match_operand:DI 1 "register_operand" "d")))]
  ""
  "cvtl.d %1,%0")

;; Float-to-fix conversion insns.

(define_insn "fix_truncsfsi2"
  [(set (match_operand:SI 0 "register_operand" "=d")
	(fix:SI (fix:SF (match_operand:SF 1 "register_operand" "d"))))]
  ""
  "cvts.w %1,%0")

(define_insn "fix_truncsfdi2"
  [(set (match_operand:DI 0 "register_operand" "=d")
	(fix:DI (fix:SF (match_operand:SF 1 "register_operand" "d"))))]
  ""
  "cvts.l %1,%0")

(define_insn "fix_truncdfsi2"
  [(set (match_operand:SI 0 "register_operand" "=d")
	(fix:SI (fix:DF (match_operand:DF 1 "register_operand" "d"))))]
  ""
  "*
{
  if (TARGET_C2)
    return \"cvtd.w %1,%0\";
  return \"cvtd.l %1,%0\";
}")

(define_insn "fix_truncdfdi2"
  [(set (match_operand:DI 0 "register_operand" "=d")
	(fix:DI (fix:DF (match_operand:DF 1 "register_operand" "d"))))]
  ""
  "cvtd.l %1,%0")

;;- All kinds of add instructions.

(define_insn "adddf3"
  [(set (match_operand:DF 0 "register_operand" "=d")
	(plus:DF (match_operand:DF 1 "register_operand" "%0")
		 (match_operand:DF 2 "register_operand" "d")))]
  ""
  "add.d %2,%0")

(define_insn "addsf3"
  [(set (match_operand:SF 0 "register_operand" "=d")
	(plus:SF (match_operand:SF 1 "register_operand" "%0")
		 (match_operand:SF 2 "nonmemory_operand" "dF")))]
  ""
  "add.s %2,%0")

(define_insn "adddi3"
  [(set (match_operand:DI 0 "register_operand" "=d")
	(plus:DI (match_operand:DI 1 "register_operand" "%0")
		 (match_operand:DI 2 "register_operand" "d")))]
  ""
  "add.l %2,%0")

;; special case of addsi3, needed to specify an A reg for the destination 
;; when the source is a sum involving FP or AP.

(define_insn ""
  [(set (match_operand:SI 0 "register_operand" "=a")
	(plus:SI (match_operand:SI 1 "register_operand" "%a")
		 (match_operand:SI 2 "immediate_operand" "i")))]
  "operands[1] == frame_pointer_rtx || operands[1] == arg_pointer_rtx"
  "ldea %a2(%1),%0")

(define_insn "addsi3"
  [(set (match_operand:SI 0 "register_operand" "=d,a,a")
	(plus:SI (match_operand:SI 1 "nonmemory_operand" "%0,0,a")
		 (match_operand:SI 2 "nonmemory_operand" "di,ri,i")))]
  ""
  "* switch (which_alternative) 
{
 case 0:
 case 1: 
   return \"add.w %2,%0\";
 case 2:
   if ((TARGET_C2 || A_REG_P (operands[0]))
       && operands[1] != stack_pointer_rtx)
     return \"ldea %a2(%1),%0\";
   else
     return \"mov %1,%0\;add.w %2,%0\";
}")

(define_insn "addhi3"
  [(set (match_operand:HI 0 "register_operand" "=d,a")
	(plus:HI (match_operand:HI 1 "register_operand" "%0,0")
		 (match_operand:HI 2 "nonmemory_operand" "di,ai")))]
  ""
  "add.h %2,%0")

(define_insn "addqi3"
  [(set (match_operand:QI 0 "register_operand" "=d")
	(plus:QI (match_operand:QI 1 "register_operand" "%0")
		 (match_operand:QI 2 "register_operand" "d")))]
  ""
  "add.b %2,%0")

;;- All kinds of subtract instructions.

(define_insn "subdf3"
  [(set (match_operand:DF 0 "register_operand" "=d")
	(minus:DF (match_operand:DF 1 "register_operand" "0")
		  (match_operand:DF 2 "register_operand" "d")))]
  ""
  "sub.d %2,%0")

(define_insn "subsf3"
  [(set (match_operand:SF 0 "register_operand" "=d")
	(minus:SF (match_operand:SF 1 "register_operand" "0")
		  (match_operand:SF 2 "nonmemory_operand" "dF")))]
  ""
  "sub.s %2,%0")

(define_insn "subdi3"
  [(set (match_operand:DI 0 "register_operand" "=d")
	(minus:DI (match_operand:DI 1 "register_operand" "0")
		  (match_operand:DI 2 "register_operand" "d")))]
  ""
  "sub.l %2,%0")

(define_insn "subsi3"
  [(set (match_operand:SI 0 "register_operand" "=d,a")
	(minus:SI (match_operand:SI 1 "register_operand" "0,0")
		  (match_operand:SI 2 "nonmemory_operand" "di,ai")))]
  ""
  "sub.w %2,%0")

(define_insn "subhi3"
  [(set (match_operand:HI 0 "register_operand" "=d,a")
	(minus:HI (match_operand:HI 1 "register_operand" "0,0")
		  (match_operand:HI 2 "nonmemory_operand" "di,ai")))]
  ""
  "sub.h %2,%0")

(define_insn "subqi3"
  [(set (match_operand:QI 0 "register_operand" "=d")
	(minus:QI (match_operand:QI 1 "register_operand" "0")
		  (match_operand:QI 2 "register_operand" "d")))]
  ""
  "sub.b %2,%0")

;;- Multiply instructions.

(define_insn "muldf3"
  [(set (match_operand:DF 0 "register_operand" "=d")
	(mult:DF (match_operand:DF 1 "register_operand" "%0")
		 (match_operand:DF 2 "register_operand" "d")))]
  ""
  "mul.d %2,%0")

(define_insn "mulsf3"
  [(set (match_operand:SF 0 "register_operand" "=d")
	(mult:SF (match_operand:SF 1 "register_operand" "%0")
		 (match_operand:SF 2 "nonmemory_operand" "dF")))]
  ""
  "mul.s %2,%0")

(define_insn "muldi3"
  [(set (match_operand:DI 0 "register_operand" "=d")
	(mult:DI (match_operand:DI 1 "register_operand" "%0")
		 (match_operand:DI 2 "register_operand" "d")))]
  ""
  "mul.l %2,%0")

(define_insn "mulsi3"
  [(set (match_operand:SI 0 "register_operand" "=d,a")
	(mult:SI (match_operand:SI 1 "register_operand" "%0,0")
		 (match_operand:SI 2 "nonmemory_operand" "di,ai")))]
  ""
  "mul.w %2,%0")

(define_insn "mulhi3"
  [(set (match_operand:HI 0 "register_operand" "=d,a")
	(mult:HI (match_operand:HI 1 "register_operand" "%0,0")
		 (match_operand:HI 2 "nonmemory_operand" "di,ai")))]
  ""
  "mul.h %2,%0")

(define_insn "mulqi3"
  [(set (match_operand:QI 0 "register_operand" "=d")
	(mult:QI (match_operand:QI 1 "register_operand" "%0")
		 (match_operand:QI 2 "register_operand" "d")))]
  ""
  "mul.b %2,%0")

;;- Divide instructions.

(define_insn "divdf3"
  [(set (match_operand:DF 0 "register_operand" "=d")
	(div:DF (match_operand:DF 1 "register_operand" "0")
		(match_operand:DF 2 "register_operand" "d")))]
  ""
  "div.d %2,%0")

(define_insn "divsf3"
  [(set (match_operand:SF 0 "register_operand" "=d")
	(div:SF (match_operand:SF 1 "register_operand" "0")
		(match_operand:SF 2 "nonmemory_operand" "dF")))]
  ""
  "div.s %2,%0")

(define_insn "divdi3"
  [(set (match_operand:DI 0 "register_operand" "=d")
	(div:DI (match_operand:DI 1 "register_operand" "0")
		(match_operand:DI 2 "register_operand" "d")))]
  ""
  "div.l %2,%0")

(define_insn "udivdi3"
  [(set (match_operand:DI 0 "register_operand" "=d")
	(udiv:DI (match_operand:DI 1 "register_operand" "d")
		 (match_operand:DI 2 "register_operand" "d")))]
  ""
  "psh.l %2\;psh.l %1\;callq udiv64\;pop.l %0\;add.w #8,sp")

(define_insn "divsi3"
  [(set (match_operand:SI 0 "register_operand" "=d,a")
	(div:SI (match_operand:SI 1 "register_operand" "0,0")
		(match_operand:SI 2 "nonmemory_operand" "di,ai")))]
  ""
  "div.w %2,%0")

(define_insn "divhi3"
  [(set (match_operand:HI 0 "register_operand" "=d,a")
	(div:HI (match_operand:HI 1 "register_operand" "0,0")
		(match_operand:HI 2 "nonmemory_operand" "di,ai")))]
  ""
  "div.h %2,%0")

(define_insn "divqi3"
  [(set (match_operand:QI 0 "register_operand" "=d")
	(div:QI (match_operand:QI 1 "register_operand" "0")
		(match_operand:QI 2 "register_operand" "d")))]
  ""
  "div.b %2,%0")

;; - and, or, xor

(define_insn ""
  [(set (match_operand:DI 0 "register_operand" "=d")
	(and:DI (match_operand:DI 1 "register_operand" "%0")
		(match_operand:DI 2 "immediate_operand" "Fn")))]
  "(GET_CODE (operands[2]) == CONST_INT && INTVAL (operands[2]) < 0)
   || (GET_CODE (operands[2]) == CONST_DOUBLE
       && CONST_DOUBLE_HIGH (operands[2]) == -1)"
  "and %2,%0")

(define_insn "anddi3"
  [(set (match_operand:DI 0 "register_operand" "=d")
	(and:DI (match_operand:DI 1 "register_operand" "%0")
		(match_operand:DI 2 "register_operand" "d")))]
  ""
  "and %2,%0")

(define_insn "andsi3"
  [(set (match_operand:SI 0 "register_operand" "=d,a")
	(and:SI (match_operand:SI 1 "register_operand" "%0,0")
		(match_operand:SI 2 "nonmemory_operand" "di,ai")))]
  ""
  "and %2,%0")

(define_insn "andhi3"
  [(set (match_operand:HI 0 "register_operand" "=d,a")
	(and:HI (match_operand:HI 1 "register_operand" "%0,0")
		(match_operand:HI 2 "nonmemory_operand" "di,ai")))]
  ""
  "and %2,%0")

(define_insn "andqi3"
  [(set (match_operand:QI 0 "register_operand" "=d,a")
	(and:QI (match_operand:QI 1 "register_operand" "%0,0")
		(match_operand:QI 2 "nonmemory_operand" "di,ai")))]
  ""
  "and %2,%0")

;;- Bit set instructions.

(define_insn ""
  [(set (match_operand:DI 0 "register_operand" "=d")
	(ior:DI (match_operand:DI 1 "register_operand" "%0")
		(match_operand:DI 2 "immediate_operand" "Fn")))]
  "(GET_CODE (operands[2]) == CONST_INT && INTVAL (operands[2]) >= 0)
   || (GET_CODE (operands[2]) == CONST_DOUBLE
       && CONST_DOUBLE_HIGH (operands[2]) == 0)"
  "or %2,%0")

(define_insn "iordi3"
  [(set (match_operand:DI 0 "register_operand" "=d")
	(ior:DI (match_operand:DI 1 "register_operand" "%0")
		(match_operand:DI 2 "register_operand" "d")))]
  ""
  "or %2,%0")

(define_insn "iorsi3"
  [(set (match_operand:SI 0 "register_operand" "=d,a")
	(ior:SI (match_operand:SI 1 "register_operand" "%0,0")
		(match_operand:SI 2 "nonmemory_operand" "di,ai")))]
  ""
  "or %2,%0")

(define_insn "iorhi3"
  [(set (match_operand:HI 0 "register_operand" "=d,a")
	(ior:HI (match_operand:HI 1 "register_operand" "%0,0")
		(match_operand:HI 2 "nonmemory_operand" "di,ai")))]
  ""
  "or %2,%0")

(define_insn "iorqi3"
  [(set (match_operand:QI 0 "register_operand" "=d,a")
	(ior:QI (match_operand:QI 1 "register_operand" "%0,0")
		(match_operand:QI 2 "nonmemory_operand" "di,ai")))]
  ""
  "or %2,%0")

;;- xor instructions.

(define_insn ""
  [(set (match_operand:DI 0 "register_operand" "=d")
	(xor:DI (match_operand:DI 1 "register_operand" "%0")
		(match_operand:DI 2 "immediate_operand" "Fn")))]
  "(GET_CODE (operands[2]) == CONST_INT && INTVAL (operands[2]) >= 0)
   || (GET_CODE (operands[2]) == CONST_DOUBLE
       && CONST_DOUBLE_HIGH (operands[2]) == 0)"
  "xor %2,%0")

(define_insn "xordi3"
  [(set (match_operand:DI 0 "register_operand" "=d")
	(xor:DI (match_operand:DI 1 "register_operand" "%0")
		(match_operand:DI 2 "register_operand" "d")))]
  ""
  "xor %2,%0")

(define_insn "xorsi3"
  [(set (match_operand:SI 0 "register_operand" "=d,a")
	(xor:SI (match_operand:SI 1 "register_operand" "%0,0")
		(match_operand:SI 2 "nonmemory_operand" "di,ai")))]
  ""
  "xor %2,%0")

(define_insn "xorhi3"
  [(set (match_operand:HI 0 "register_operand" "=d,a")
	(xor:HI (match_operand:HI 1 "register_operand" "%0,0")
		(match_operand:HI 2 "nonmemory_operand" "di,ai")))]
  ""
  "xor %2,%0")

(define_insn "xorqi3"
  [(set (match_operand:QI 0 "register_operand" "=d,a")
	(xor:QI (match_operand:QI 1 "register_operand" "%0,0")
		(match_operand:QI 2 "nonmemory_operand" "di,ai")))]
  ""
  "xor %2,%0")

(define_insn "negdf2"
  [(set (match_operand:DF 0 "register_operand" "=d")
	(neg:DF (match_operand:DF 1 "register_operand" "d")))]
  ""
  "neg.d %1,%0")

(define_insn "negsf2"
  [(set (match_operand:SF 0 "register_operand" "=d")
	(neg:SF (match_operand:SF 1 "register_operand" "d")))]
  ""
  "neg.s %1,%0")

(define_insn "negdi2"
  [(set (match_operand:DI 0 "register_operand" "=d")
	(neg:DI (match_operand:DI 1 "register_operand" "d")))]
  ""
  "neg.l %1,%0")

(define_insn "negsi2"
  [(set (match_operand:SI 0 "register_operand" "=d,a")
	(neg:SI (match_operand:SI 1 "register_operand" "d,a")))]
  ""
  "neg.w %1,%0")

(define_insn "neghi2"
  [(set (match_operand:HI 0 "register_operand" "=d,a")
	(neg:HI (match_operand:HI 1 "register_operand" "d,a")))]
  ""
  "neg.h %1,%0")

(define_insn "negqi2"
  [(set (match_operand:QI 0 "register_operand" "=d")
	(neg:QI (match_operand:QI 1 "register_operand" "d")))]
  ""
  "neg.b %1,%0")

(define_insn "one_cmpldi2"
  [(set (match_operand:DI 0 "register_operand" "=d")
	(not:DI (match_operand:DI 1 "register_operand" "d")))]
  ""
  "not %1,%0")

(define_insn "one_cmplsi2"
  [(set (match_operand:SI 0 "register_operand" "=d,a")
	(not:SI (match_operand:SI 1 "register_operand" "d,a")))]
  ""
  "not %1,%0")

(define_insn "one_cmplhi2"
  [(set (match_operand:HI 0 "register_operand" "=d,a")
	(not:HI (match_operand:HI 1 "register_operand" "d,a")))]
  ""
  "not %1,%0")

(define_insn "one_cmplqi2"
  [(set (match_operand:QI 0 "register_operand" "=d,a")
	(not:QI (match_operand:QI 1 "register_operand" "d,a")))]
  ""
  "not %1,%0")

;;- shifts
;;
;; Convex shift instructions are logical shifts.
;; To make signed right shifts:
;; for SImode, sign extend to DImode and shift, works for 0..32
;; for DImode, shift and then extend the sign, works for 0..63 -- but not 64

(define_insn "lshlsi3"
  [(set (match_operand:SI 0 "register_operand" "=d,a")
	(lshift:SI (match_operand:SI 1 "register_operand" "0,0")
		   (match_operand:SI 2 "nonmemory_operand" "di,ai")))]
  ""
  "*
{
  if (operands[2] == const1_rtx)
    return \"add.w %0,%0\";
  else if (TARGET_C2 && S_REG_P (operands[0]))
    return \"shf.w %2,%0\";
  else
    return \"shf %2,%0\";
}")

(define_insn "ashlsi3"
  [(set (match_operand:SI 0 "register_operand" "=d,a")
	(ashift:SI (match_operand:SI 1 "register_operand" "0,0")
		   (match_operand:SI 2 "nonmemory_operand" "di,ai")))]
  ""
  "*
{
  if (operands[2] == const1_rtx)
    return \"add.w %0,%0\";
  else if (TARGET_C2 && S_REG_P (operands[0]))
    return \"shf.w %2,%0\";
  else
    return \"shf %2,%0\";
}")

(define_expand "lshrsi3"
  [(set (match_operand:SI 0 "register_operand" "")
	(lshiftrt:SI (match_operand:SI 1 "register_operand" "")
		     (neg:SI (match_operand:SI 2 "nonmemory_operand" ""))))]
  ""
  "operands[2] = negate_rtx (SImode, operands[2]);")

(define_insn ""
  [(set
    (match_operand:SI 0 "register_operand" "=d,a")
    (lshiftrt:SI (match_operand:SI 1 "register_operand" "0,0")
		 (neg:SI (match_operand:SI 2 "nonmemory_operand" "di,ai"))))]
  ""
  "*
{
  if (A_REG_P (operands[0]))
    return \"shf %2,%0\";
  else if (TARGET_C2)
    return \"shf.w %2,%0\";
  else
    return \"ld.u #0,%0\;shf %2,%0\";
}")

(define_insn ""
  [(set
    (match_operand:SI 0 "register_operand" "=r")
    (lshiftrt:SI (match_operand:SI 1 "register_operand" "0")
		 (match_operand:SI 2 "immediate_operand" "i")))]
  ""
  "*
{
  if (A_REG_P (operands[0]))
    return \"shf #%n2,%0\";
  else if (TARGET_C2)
    return \"shf.w #%n2,%0\";
  else
    return \"ld.u #0,%0\;shf #%n2,%0\";
}")

(define_expand "ashrsi3"
  [(set (match_operand:SI 0 "register_operand" "=d")
	(ashiftrt:SI (match_operand:SI 1 "register_operand" "d")
		     (neg:SI (match_operand:SI 2 "nonmemory_operand" "di"))))]
  ""
  "operands[2] = negate_rtx (SImode, operands[2]);")

(define_insn ""
  [(set (match_operand:SI 0 "register_operand" "=&d")
	(ashiftrt:SI (match_operand:SI 1 "register_operand" "d")
		     (neg:SI (match_operand:SI 2 "nonmemory_operand" "di"))))]
  ""
  "cvtw.l %1,%0\;shf %2,%0")

(define_insn ""
  [(set (match_operand:SI 0 "register_operand" "=&d")
	(ashiftrt:SI (match_operand:SI 1 "register_operand" "d")
		     (match_operand:SI 2 "immediate_operand" "i")))]
  ""
  "cvtw.l %1,%0\;shf #%n2,%0")

(define_insn "lshldi3"
  [(set (match_operand:DI 0 "register_operand" "=d")
	(lshift:DI (match_operand:DI 1 "register_operand" "0")
		   (match_operand:SI 2 "nonmemory_operand" "di")))]
  ""
  "shf %2,%0")

(define_insn "ashldi3"
  [(set (match_operand:DI 0 "register_operand" "=d")
	(ashift:DI (match_operand:DI 1 "register_operand" "0")
		   (match_operand:SI 2 "nonmemory_operand" "di")))]
  ""
  "shf %2,%0")

(define_expand "lshrdi3"
  [(set (match_operand:DI 0 "register_operand" "=d")
	(lshiftrt:DI (match_operand:DI 1 "register_operand" "0")
		     (neg:SI (match_operand:SI 2 "nonmemory_operand" "di"))))]
  ""
  "operands[2] = negate_rtx (SImode, operands[2]);")

(define_insn ""
  [(set (match_operand:DI 0 "register_operand" "=d")
	(lshiftrt:DI (match_operand:DI 1 "register_operand" "0")
		     (neg:SI (match_operand:SI 2 "nonmemory_operand" "di"))))]
  ""
  "shf %2,%0")

(define_insn ""
  [(set (match_operand:DI 0 "register_operand" "=d")
	(lshiftrt:DI (match_operand:DI 1 "register_operand" "0")
		     (match_operand:SI 2 "immediate_operand" "i")))]
  ""
  "shf #%n2,%0")

;; signed  a >> b  is
;;     ((a >> b) ^ signbit) - signbit
;; where signbit is (1 << 63) >> b

(define_expand "ashrdi3"
  [(match_operand:DI 0 "register_operand" "")
   (match_operand:DI 1 "register_operand" "")
   (match_operand:SI 2 "nonmemory_operand" "")
   (match_dup 3)]
  ""
  "
{
  if (GET_CODE (operands[2]) == CONST_INT)
    {
      int rshift = INTVAL (operands[2]);
      if (rshift < 0)
	operands[3] = force_reg (DImode, immed_double_const (0, 0, DImode));
      else if (rshift < 32)
	operands[3] =
	  force_reg (DImode,
		     immed_double_const (0, 1 << (31 - rshift), DImode));
      else if (rshift < 64)
	operands[3] =
	  force_reg (DImode,
		     immed_double_const (1 << (63 - rshift), 0, DImode));
      else
	operands[3] = force_reg (DImode, immed_double_const (0, 0, DImode));
    }
  else
    {
      operands[3] =
	  force_reg (DImode, immed_double_const (0, 1 << 31, DImode));
      emit_insn (gen_lshrdi3 (operands[3], operands[3], operands[2]));
    }

  emit_insn (gen_lshrdi3 (operands[0], operands[1], operands[2]));
  emit_insn (gen_rtx (SET, VOIDmode, operands[0],
		      gen_rtx (XOR, DImode, operands[0], operands[3])));
  emit_insn (gen_rtx (SET, VOIDmode, operands[0],
		      gen_rtx (MINUS, DImode, operands[0], operands[3])));
  DONE;
}")

;; __builtin instructions

(define_insn "sqrtdf2"
  [(set (match_operand:DF 0 "register_operand" "=d")
	(sqrt:DF (match_operand:DF 1 "register_operand" "0")))]
  "TARGET_C2"
  "sqrt.d %0")

(define_insn "sqrtsf2"
  [(set (match_operand:SF 0 "register_operand" "=d")
	(sqrt:SF (match_operand:SF 1 "register_operand" "0")))]
  "TARGET_C2"
  "sqrt.s %0")

;(define_insn ""
;  [(set (match_operand:SI 0 "register_operand" "=d")
;	(minus:SI (ffs:SI (match_operand:SI 1 "register_operand" "d"))
;		  (const_int 1)))]
;  ""
;  "tzc %1,%0\;le.w #32,%0\;jbrs.f .+6\;ld.w #-1,%0")
;
;(define_expand "ffssi2"
;  [(set (match_operand:SI 0 "register_operand" "=d")
;	(minus:SI (ffs:SI (match_operand:SI 1 "register_operand" "d"))
;		  (const_int 1)))
;   (set (match_dup 0)
;	(plus:SI (match_dup 0)
;		 (const_int 1)))]
;  ""
;  "")

(define_insn "abssf2"
  [(set (match_operand:SF 0 "register_operand" "=d")
	(abs:SF (match_operand:SF 1 "register_operand" "0")))]
  ""
  "and #0x7fffffff,%0")

(define_expand "absdf2"
  [(set (subreg:DI (match_operand:DF 0 "register_operand" "=d") 0)
	(and:DI (subreg:DI (match_operand:DF 1 "register_operand" "d") 0)
		(match_dup 2)))]
  ""
  "operands[2] = force_reg (DImode,
			    immed_double_const (-1, 0x7fffffff, DImode));")

;; Jumps

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
  "* return gen_cmp (operands[0], \"eq\", 't'); ")

(define_insn "bne"
  [(set (pc)
	(if_then_else (ne (cc0)
			  (const_int 0))
		      (label_ref (match_operand 0 "" ""))
		      (pc)))]
  ""
  "* return gen_cmp (operands[0], \"eq\", 'f'); ")

(define_insn "bgt"
  [(set (pc)
	(if_then_else (gt (cc0)
			  (const_int 0))
		      (label_ref (match_operand 0 "" ""))
		      (pc)))]
  ""
  "* return gen_cmp (operands[0], \"le\", 'f'); ")

(define_insn "bgtu"
  [(set (pc)
	(if_then_else (gtu (cc0)
			   (const_int 0))
		      (label_ref (match_operand 0 "" ""))
		      (pc)))]
  ""
  "* return gen_cmp (operands[0], \"leu\", 'f'); ")

(define_insn "blt"
  [(set (pc)
	(if_then_else (lt (cc0)
			  (const_int 0))
		      (label_ref (match_operand 0 "" ""))
		      (pc)))]
  ""
  "* return gen_cmp (operands[0], \"lt\", 't'); ")

(define_insn "bltu"
  [(set (pc)
	(if_then_else (ltu (cc0)
			   (const_int 0))
		      (label_ref (match_operand 0 "" ""))
		      (pc)))]
  ""
  "* return gen_cmp (operands[0], \"ltu\", 't'); ")

(define_insn "bge"
  [(set (pc)
	(if_then_else (ge (cc0)
			  (const_int 0))
		      (label_ref (match_operand 0 "" ""))
		      (pc)))]
  ""
  "* return gen_cmp (operands[0], \"lt\", 'f'); ")

(define_insn "bgeu"
  [(set (pc)
	(if_then_else (geu (cc0)
			   (const_int 0))
		      (label_ref (match_operand 0 "" ""))
		      (pc)))]
  ""
  "* return gen_cmp (operands[0], \"ltu\", 'f'); ")

(define_insn "ble"
  [(set (pc)
	(if_then_else (le (cc0)
			  (const_int 0))
		      (label_ref (match_operand 0 "" ""))
		      (pc)))]
  ""
  "* return gen_cmp (operands[0], \"le\", 't'); ")

(define_insn "bleu"
  [(set (pc)
	(if_then_else (leu (cc0)
			   (const_int 0))
		      (label_ref (match_operand 0 "" ""))
		      (pc)))]
  ""
  "* return gen_cmp (operands[0], \"leu\", 't'); ")

(define_insn ""
  [(set (pc)
	(if_then_else (eq (cc0)
			  (const_int 0))
		      (pc)
		      (label_ref (match_operand 0 "" ""))))]
  ""
  "* return gen_cmp (operands[0], \"eq\", 'f'); ")

(define_insn ""
  [(set (pc)
	(if_then_else (ne (cc0)
			  (const_int 0))
		      (pc)
		      (label_ref (match_operand 0 "" ""))))]
  ""
  "* return gen_cmp (operands[0], \"eq\", 't'); ")

(define_insn ""
  [(set (pc)
	(if_then_else (gt (cc0)
			  (const_int 0))
		      (pc)
		      (label_ref (match_operand 0 "" ""))))]
  ""
  "* return gen_cmp (operands[0], \"le\", 't'); ")

(define_insn ""
  [(set (pc)
	(if_then_else (gtu (cc0)
			   (const_int 0))
		      (pc)
		      (label_ref (match_operand 0 "" ""))))]
  ""
  "* return gen_cmp (operands[0], \"leu\", 't'); ")

(define_insn ""
  [(set (pc)
	(if_then_else (lt (cc0)
			  (const_int 0))
		      (pc)
		      (label_ref (match_operand 0 "" ""))))]
  ""
  "* return gen_cmp (operands[0], \"lt\", 'f'); ")

(define_insn ""
  [(set (pc)
	(if_then_else (ltu (cc0)
			   (const_int 0))
		      (pc)
		      (label_ref (match_operand 0 "" ""))))]
  ""
  "* return gen_cmp (operands[0], \"ltu\", 'f'); ")

(define_insn ""
  [(set (pc)
	(if_then_else (ge (cc0)
			  (const_int 0))
		      (pc)
		      (label_ref (match_operand 0 "" ""))))]
  ""
  "* return gen_cmp (operands[0], \"lt\", 't'); ")

(define_insn ""
  [(set (pc)
	(if_then_else (geu (cc0)
			   (const_int 0))
		      (pc)
		      (label_ref (match_operand 0 "" ""))))]
  ""
  "* return gen_cmp (operands[0], \"ltu\", 't'); ")

(define_insn ""
  [(set (pc)
	(if_then_else (le (cc0)
			  (const_int 0))
		      (pc)
		      (label_ref (match_operand 0 "" ""))))]
  ""
  "* return gen_cmp (operands[0], \"le\", 'f'); ")

(define_insn ""
  [(set (pc)
	(if_then_else (leu (cc0)
			   (const_int 0))
		      (pc)
		      (label_ref (match_operand 0 "" ""))))]
  ""
  "* return gen_cmp (operands[0], \"leu\", 'f'); ")

;;  - Calls
;;
;; arg count word may be omitted to save a push and let gcc try to
;; combine the arg list pop.  RETURN_POPS_ARGS from tm.h decides this.

(define_insn "call"
  [(call (match_operand:QI 0 "memory_operand" "m")
	 (match_operand 1 "" "g"))]
  ""
  "* return output_call (insn, operands[0], operands[1]);")

(define_insn "call_value"
  [(set (match_operand 0 "" "=g")
	(call (match_operand:QI 1 "memory_operand" "m")
	      (match_operand 2 "" "g")))]
  ""
  "* return output_call (insn, operands[1], operands[2]);")

(define_insn "return"
  [(return)]
  ""
  "rtn")

(define_insn "nop"
  [(const_int 0)]
  ""
  "nop")

(define_insn "tablejump"
  [(set (pc) (match_operand:SI 0 "address_operand" "p"))
   (use (label_ref (match_operand 1 "" "")))]
  ""
  "jmp %a0")

(define_insn "indirect_jump"
  [(set (pc) (match_operand:SI 0 "address_operand" "p"))]
  ""
  "jmp %a0")

;;- Local variables:
;;- mode:emacs-lisp
;;- comment-start: ";;- "
;;- eval: (set-syntax-table (copy-sequence (syntax-table)))
;;- eval: (modify-syntax-entry ?[ "(]")
;;- eval: (modify-syntax-entry ?] ")[")
;;- eval: (modify-syntax-entry ?{ "(}")
;;- eval: (modify-syntax-entry ?} "){")
;;- End:
