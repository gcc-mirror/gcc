;;- Machine description for GNU compiler, Elxsi Version
;;  Copyright (C) 1987, 1988, 1992, 1994, 2000 Free Software Foundation, Inc.
;;  Contributed by Mike Stump <mrs@cygnus.com> in 1988, and is the first
;;  64 bit port of GNU CC.
;;  Based upon the VAX port.

;; This file is part of GNU CC.

;; GNU CC is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 1, or (at your option)
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


(define_insn ""
  [(set (reg:SI 15)
	 (plus:SI (reg:SI 15)
		  (match_operand:SI 0 "general_operand" "g")))]
  ""
  "add.64\\t.sp,%0")

(define_insn ""
  [(set (reg:SI 15)
	 (plus:SI (match_operand:SI 0 "general_operand" "g")
		  (reg:SI 15)))]
  ""
  "add.64\\t.sp,%0")

(define_insn ""
  [(set (match_operand:SI 0 "register_operand" "=r")
	 (plus:SI (reg:SI 15)
		  (match_operand:SI 1 "general_operand" "g")))]
  ""
  "ld.32\\t%0,.sp\;add.64\\t%0,%1")

(define_insn ""
  [(set (match_operand:SI 0 "register_operand" "=r")
	 (plus:SI (match_operand:SI 1 "general_operand" "g")
		  (reg:SI 15)))]
  ""
  "ld.32\\t%0,.sp\;add.64\\t%0,%1")

(define_insn ""
  [(set (reg:SI 15)
	 (minus:SI (reg:SI 15)
		   (match_operand:SI 0 "general_operand" "g")))]
  ""
  "sub.64\\t.sp,%0")

(define_insn ""
  [(set (reg:SI 15)
	 (match_operand:SI 0 "general_operand" "rm"))]
  ""
  "ld.32\\t.sp,%0")

(define_insn ""
  [(set (match_operand:SI 0 "nonimmediate_operand" "=m,r")
	 (reg:SI 15))]
  ""
  "@
   st.32\\t.sp,%0
   ld.32\\t%0,.sp")

; tstdi is first test insn so that it is the one to match
; a constant argument.

(define_insn "tstdi"
  [(set (cc0)
	(match_operand:DI 0 "register_operand" "r"))]
  ""
  "*
    extern rtx cmp_op0, cmp_op1;
    cmp_op0=operands[0]; cmp_op1=0;
    return \";\\ttstdi\\t%0\";
")

(define_insn "tstdf"
  [(set (cc0)
	(match_operand:DF 0 "register_operand" "r"))]
  ""
  "*
    extern rtx cmp_op0, cmp_op1;
    cmp_op0=operands[0]; cmp_op1=0;
    return \";\\ttstdf\\t%0\";
")

(define_insn "tstsf"
  [(set (cc0)
	(match_operand:SF 0 "register_operand" "r"))]
  ""
  "*
    extern rtx cmp_op0, cmp_op1;
    cmp_op0=operands[0]; cmp_op1=0;
    return \";\\ttstsf\\t%0\";
")

(define_insn "cmpdi"
  [(set (cc0)
	(compare (match_operand:DI 0 "register_operand" "r")
		 (match_operand:DI 1 "general_operand" "rm")))]
  ""
  "*
    extern rtx cmp_op0, cmp_op1;
    cmp_op0=operands[0]; cmp_op1=operands[1];
    return \";\\tcmpdi\\t%0,%1\";
")

(define_insn "cmpdf"
  [(set (cc0)
	(compare (match_operand:DF 0 "register_operand" "r")
		 (match_operand:DF 1 "general_operand" "rm")))]
  ""
  "*
    extern rtx cmp_op0, cmp_op1;
    cmp_op0=operands[0]; cmp_op1=operands[1];
    return \";\\tcmpdf\\t%0,%1\";
")

(define_insn "cmpsf"
  [(set (cc0)
	(compare (match_operand:SF 0 "register_operand" "r")
		 (match_operand:SF 1 "general_operand" "rm")))]
  ""
  "*
    extern rtx cmp_op0, cmp_op1;
    cmp_op0=operands[0]; cmp_op1=operands[1];
    return \";\\tcmpsf\\t%0,%1\";
")

(define_insn ""
  [(set (match_operand:DI 0 "register_operand" "=r")
        (eq (match_operand:DI 1 "register_operand" "r")
	         (match_operand:DI 2 "general_operand" "g")))]
  ""
  "cmp.64\\t%0,%1,%2:eq")

(define_insn ""
  [(set (match_operand:DI 0 "register_operand" "=r")
        (ne (match_operand:DI 1 "register_operand" "r")
	         (match_operand:DI 2 "general_operand" "g")))]
  ""
  "cmp.64\\t%0,%1,%2:ne")

(define_insn ""
  [(set (match_operand:DI 0 "register_operand" "=r")
        (le (match_operand:DI 1 "register_operand" "r")
	         (match_operand:DI 2 "general_operand" "g")))]
  ""
  "cmp.64\\t%0,%1,%2:le")

(define_insn ""
  [(set (match_operand:DI 0 "register_operand" "=r")
        (leu (match_operand:DI 1 "register_operand" "r")
	         (match_operand:DI 2 "general_operand" "g")))]
  ""
  "cmpu.64\\t%0,%1,%2:le")

(define_insn ""
  [(set (match_operand:DI 0 "register_operand" "=r")
        (lt (match_operand:DI 1 "register_operand" "r")
	         (match_operand:DI 2 "general_operand" "g")))]
  ""
  "cmp.64\\t%0,%1,%2:lt")

(define_insn ""
  [(set (match_operand:DI 0 "register_operand" "=r")
        (ltu (match_operand:DI 1 "register_operand" "r")
	         (match_operand:DI 2 "general_operand" "g")))]
  ""
  "cmpu.64\\t%0,%1,%2:lt")

(define_insn ""
  [(set (match_operand:DI 0 "register_operand" "=r")
        (ge (match_operand:DI 1 "register_operand" "r")
	         (match_operand:DI 2 "general_operand" "g")))]
  ""
  "cmp.64\\t%0,%1,%2:ge")

(define_insn ""
  [(set (match_operand:DI 0 "register_operand" "=r")
        (geu (match_operand:DI 1 "register_operand" "r")
	         (match_operand:DI 2 "general_operand" "g")))]
  ""
  "cmpu.64\\t%0,%1,%2:ge")

(define_insn ""
  [(set (match_operand:DI 0 "register_operand" "=r")
        (gt (match_operand:DI 1 "register_operand" "r")
	         (match_operand:DI 2 "general_operand" "g")))]
  ""
  "cmp.64\\t%0,%1,%2:gt")

(define_insn ""
  [(set (match_operand:DI 0 "register_operand" "=r")
        (gtu (match_operand:DI 1 "register_operand" "r")
	         (match_operand:DI 2 "general_operand" "g")))]
  ""
  "cmpu.64\\t%0,%1,%2:gt")

(define_insn "seq"
  [(set (match_operand:DI 0 "register_operand" "=r")
	(eq (cc0) (const_int 0)))]
  ""
  "* return cmp_set(\"\", \"eq\", operands[0]); ")

(define_insn "sne"
  [(set (match_operand:DI 0 "register_operand" "=r")
	(ne (cc0) (const_int 0)))]
  ""
  "* return cmp_set(\"\", \"ne\", operands[0]); ")

(define_insn "sle"
  [(set (match_operand:DI 0 "register_operand" "=r")
	(le (cc0) (const_int 0)))]
  ""
  "* return cmp_set(\"\", \"le\", operands[0]); ")

(define_insn "sleu"
  [(set (match_operand:DI 0 "register_operand" "=r")
	(leu (cc0) (const_int 0)))]
  ""
  "* return cmp_set(\"u\", \"le\", operands[0]); ")

(define_insn "slt"
  [(set (match_operand:DI 0 "register_operand" "=r")
	(lt (cc0) (const_int 0)))]
  ""
  "* return cmp_set(\"\", \"lt\", operands[0]); ")

(define_insn "sltu"
  [(set (match_operand:DI 0 "register_operand" "=r")
	(ltu (cc0) (const_int 0)))]
  ""
  "* return cmp_set(\"u\", \"lt\", operands[0]); ")

(define_insn "sge"
  [(set (match_operand:DI 0 "register_operand" "=r")
	(ge (cc0) (const_int 0)))]
  ""
  "* return cmp_set(\"\", \"ge\", operands[0]); ")

(define_insn "sgeu"
  [(set (match_operand:DI 0 "register_operand" "=r")
	(geu (cc0) (const_int 0)))]
  ""
  "* return cmp_set(\"u\", \"ge\", operands[0]); ")

(define_insn "sgt"
  [(set (match_operand:DI 0 "register_operand" "=r")
	(gt (cc0) (const_int 0)))]
  ""
  "* return cmp_set(\"\", \"gt\", operands[0]); ")

(define_insn "sgtu"
  [(set (match_operand:DI 0 "register_operand" "=r")
	(gtu (cc0) (const_int 0)))]
  ""
  "* return cmp_set(\"u\", \"gt\", operands[0]); ")

(define_insn ""
  [(set (match_operand:SI 0 "register_operand" "=r")
	(eq (match_operand:SI 1 "register_operand" "r")
	    (match_operand:SI 2 "general_operand" "m")))]
  ""
  "cmp.32\\t%0,%1,%2:eq")

(define_insn ""
  [(set (match_operand:SI 0 "register_operand" "=r")
	(ne (match_operand:SI 1 "register_operand" "r")
	    (match_operand:SI 2 "general_operand" "m")))]
  ""
  "cmp.32\\t%0,%1,%2:ne")

(define_insn ""
  [(set (match_operand:SI 0 "register_operand" "=r")
	(le (match_operand:SI 1 "register_operand" "r")
	    (match_operand:SI 2 "general_operand" "m")))]
  ""
  "cmp.32\\t%0,%1,%2:le")

(define_insn ""
  [(set (match_operand:SI 0 "register_operand" "=r")
	(leu (match_operand:SI 1 "register_operand" "r")
	    (match_operand:SI 2 "general_operand" "m")))]
  ""
  "cmpu.32\\t%0,%1,%2:le")

(define_insn ""
  [(set (match_operand:SI 0 "register_operand" "=r")
	(lt (match_operand:SI 1 "register_operand" "r")
	    (match_operand:SI 2 "general_operand" "m")))]
  ""
  "cmp.32\\t%0,%1,%2:lt")

(define_insn ""
  [(set (match_operand:SI 0 "register_operand" "=r")
	(ltu (match_operand:SI 1 "register_operand" "r")
	    (match_operand:SI 2 "general_operand" "m")))]
  ""
  "cmpu.32\\t%0,%1,%2:lt")

(define_insn ""
  [(set (match_operand:SI 0 "register_operand" "=r")
	(ge (match_operand:SI 1 "register_operand" "r")
	    (match_operand:SI 2 "general_operand" "m")))]
  ""
  "cmp.32\\t%0,%1,%2:ge")

(define_insn ""
  [(set (match_operand:SI 0 "register_operand" "=r")
	(geu (match_operand:SI 1 "register_operand" "r")
	    (match_operand:SI 2 "general_operand" "m")))]
  ""
  "cmpu.32\\t%0,%1,%2:ge")

(define_insn ""
  [(set (match_operand:SI 0 "register_operand" "=r")
	(gt (match_operand:SI 1 "register_operand" "r")
	    (match_operand:SI 2 "general_operand" "m")))]
  ""
  "cmp.32\\t%0,%1,%2:gt")

(define_insn ""
  [(set (match_operand:SI 0 "register_operand" "=r")
	(gtu (match_operand:SI 1 "register_operand" "r")
	    (match_operand:SI 2 "general_operand" "m")))]
  ""
  "cmpu.32\\t%0,%1,%2:gt")

(define_insn ""
  [(set (match_operand:HI 0 "register_operand" "=r")
	(eq (match_operand:HI 1 "register_operand" "r")
	    (match_operand:HI 2 "general_operand" "m")))]
  ""
  "cmp.16\\t%0,%1,%2:eq")

(define_insn ""
  [(set (match_operand:HI 0 "register_operand" "=r")
	(ne (match_operand:HI 1 "register_operand" "r")
	    (match_operand:HI 2 "general_operand" "m")))]
  ""
  "cmp.16\\t%0,%1,%2:ne")

(define_insn ""
  [(set (match_operand:HI 0 "register_operand" "=r")
	(le (match_operand:HI 1 "register_operand" "r")
	    (match_operand:HI 2 "general_operand" "m")))]
  ""
  "cmp.16\\t%0,%1,%2:le")

(define_insn ""
  [(set (match_operand:HI 0 "register_operand" "=r")
	(leu (match_operand:HI 1 "register_operand" "r")
	    (match_operand:HI 2 "general_operand" "m")))]
  ""
  "cmpu.16\\t%0,%1,%2:le")

(define_insn ""
  [(set (match_operand:HI 0 "register_operand" "=r")
	(lt (match_operand:HI 1 "register_operand" "r")
	    (match_operand:HI 2 "general_operand" "m")))]
  ""
  "cmp.16\\t%0,%1,%2:lt")

(define_insn ""
  [(set (match_operand:HI 0 "register_operand" "=r")
	(ltu (match_operand:HI 1 "register_operand" "r")
	    (match_operand:HI 2 "general_operand" "m")))]
  ""
  "cmpu.16\\t%0,%1,%2:lt")

(define_insn ""
  [(set (match_operand:HI 0 "register_operand" "=r")
	(ge (match_operand:HI 1 "register_operand" "r")
	    (match_operand:HI 2 "general_operand" "m")))]
  ""
  "cmp.16\\t%0,%1,%2:ge")

(define_insn ""
  [(set (match_operand:HI 0 "register_operand" "=r")
	(geu (match_operand:HI 1 "register_operand" "r")
	    (match_operand:HI 2 "general_operand" "m")))]
  ""
  "cmpu.16\\t%0,%1,%2:ge")

(define_insn ""
  [(set (match_operand:HI 0 "register_operand" "=r")
	(gt (match_operand:HI 1 "register_operand" "r")
	    (match_operand:HI 2 "general_operand" "m")))]
  ""
  "cmp.16\\t%0,%1,%2:gt")

(define_insn ""
  [(set (match_operand:HI 0 "register_operand" "=r")
	(gtu (match_operand:HI 1 "register_operand" "r")
	    (match_operand:HI 2 "general_operand" "m")))]
  ""
  "cmpu.16\\t%0,%1,%2:gt")

(define_insn ""
  [(set (match_operand:QI 0 "register_operand" "=r")
	(eq (match_operand:QI 1 "register_operand" "r")
	    (match_operand:QI 2 "general_operand" "m")))]
  ""
  "cmp.8\\t%0,%1,%2:eq")

(define_insn ""
  [(set (match_operand:QI 0 "register_operand" "=r")
	(ne (match_operand:QI 1 "register_operand" "r")
	    (match_operand:QI 2 "general_operand" "m")))]
  ""
  "cmp.8\\t%0,%1,%2:ne")

(define_insn ""
  [(set (match_operand:QI 0 "register_operand" "=r")
	(le (match_operand:QI 1 "register_operand" "r")
	    (match_operand:QI 2 "general_operand" "m")))]
  ""
  "cmp.8\\t%0,%1,%2:le")

(define_insn ""
  [(set (match_operand:QI 0 "register_operand" "=r")
	(leu (match_operand:QI 1 "register_operand" "r")
	    (match_operand:QI 2 "general_operand" "m")))]
  ""
  "cmpu.8\\t%0,%1,%2:le")

(define_insn ""
  [(set (match_operand:QI 0 "register_operand" "=r")
	(lt (match_operand:QI 1 "register_operand" "r")
	    (match_operand:QI 2 "general_operand" "m")))]
  ""
  "cmp.8\\t%0,%1,%2:lt")

(define_insn ""
  [(set (match_operand:QI 0 "register_operand" "=r")
	(ltu (match_operand:QI 1 "register_operand" "r")
	    (match_operand:QI 2 "general_operand" "m")))]
  ""
  "cmpu.8\\t%0,%1,%2:lt")

(define_insn ""
  [(set (match_operand:QI 0 "register_operand" "=r")
	(ge (match_operand:QI 1 "register_operand" "r")
	    (match_operand:QI 2 "general_operand" "m")))]
  ""
  "cmp.8\\t%0,%1,%2:ge")

(define_insn ""
  [(set (match_operand:QI 0 "register_operand" "=r")
	(geu (match_operand:QI 1 "register_operand" "r")
	    (match_operand:QI 2 "general_operand" "m")))]
  ""
  "cmpu.8\\t%0,%1,%2:ge")

(define_insn ""
  [(set (match_operand:QI 0 "register_operand" "=r")
	(gt (match_operand:QI 1 "register_operand" "r")
	    (match_operand:QI 2 "general_operand" "m")))]
  ""
  "cmp.8\\t%0,%1,%2:gt")

(define_insn ""
  [(set (match_operand:QI 0 "register_operand" "=r")
	(gtu (match_operand:QI 1 "register_operand" "r")
	    (match_operand:QI 2 "general_operand" "m")))]
  ""
  "cmpu.8\\t%0,%1,%2:gt")



(define_insn "movdf"  [(set (match_operand:DF 0 "general_operand" "=r,m")
	(match_operand:DF 1 "general_operand" "rm,r"))]
  ""
  "*
{
  if (which_alternative == 0)
    return \"ld.64\\t%0,%1\";
  return \"st.64\\t%1,%0\";
}")

(define_insn "movsf"
  [(set (match_operand:SF 0 "general_operand" "=r,m")
	(match_operand:SF 1 "general_operand" "rm,r"))]
  ""
  "*
{
  if (which_alternative == 0)
    return \"ld.32\\t%0,%1\";
  return \"st.32\\t%1,%0\";
}")

(define_insn "movdi"
  [(set (match_operand:DI 0 "general_operand" "=r,m,rm")
	(match_operand:DI 1 "general_operand" "g,r,I"))]
  ""
  "*
  if (which_alternative == 0)
    return \"ld.64\\t%0,%1\";
  else if (which_alternative == 1)
    return \"st.64\\t%1,%0\";
  else
    if (GET_CODE(operands[1])==CONST_INT) {
      if (INTVAL(operands[1]) >= 0)
        return \"sti.64\\t%c1,%0\";
      else
        return \"stin.64\\t%n1,%0\";
    }
  else
    abort();
")

(define_insn "movsi"
  [(set (match_operand:SI 0 "general_operand" "=r,m,r")
	(match_operand:SI 1 "general_operand" "rm,rI,i"))]
  ""
  "*
  if (which_alternative == 0)
    return \"ld.32\\t%0,%1\";
  else if (which_alternative == 1) {
    if (GET_CODE(operands[1])==CONST_INT) {
      if (INTVAL(operands[1]) >= 0)
        return \"sti.32\\t%c1,%0\";
      else
        return \"stin.32\\t%n1,%0\";
    }
    return \"st.32\\t%1,%0\";
  } else
    return \"ld.64\\t%0,%1 ; I only want 32\";
")

(define_insn "movhi"
  [(set (match_operand:HI 0 "general_operand" "=r,m,r")
	(match_operand:HI 1 "general_operand" "m,rI,ri"))]
  ""
  "*
{
  if (which_alternative == 0)
    return \"ld.16\\t%0,%1\";
  if (which_alternative == 2)
    return \"ld.64\\t%0,%1\\t; I only want 16\";
  if (GET_CODE(operands[1])==CONST_INT) {
    if (INTVAL(operands[1]) >= 0)
      return \"sti.16\\t%c1,%0\";
    else
      return \"stin.16\\t%n1,%0\";
  }
  return \"st.16\\t%1,%0\";
}")

(define_insn "movqi"
  [(set (match_operand:QI 0 "general_operand" "=r,m,r")
	(match_operand:QI 1 "general_operand" "m,rI,ri"))]
  ""
  "*
{
  if (which_alternative == 0)
    return \"ld.8\\t%0,%1\";
  if (which_alternative == 2)
    return \"ld.64\\t%0,%1\\t; I only want 8\";
  if (GET_CODE(operands[1])==CONST_INT) {
    if (INTVAL(operands[1]) >= 0)
      return \"sti.8\\t%c1,%0\";
    else
      return \"stin.8\\t%n1,%0\";
  }
  return \"st.8\\t%1,%0\";
}")

;; Extension and truncation insns.
;; Those for integer source operand
;; are ordered widest source type first.

(define_insn "truncdfsf2"
  [(set (match_operand:SF 0 "register_operand" "=r")
	(truncate:SF (match_operand:DF 1 "general_operand" "rm")))]
  ""
  "cvt.ds\\t%0,%1")

(define_insn "truncdiqi2"
  [(set (match_operand:QI 0 "general_operand" "=r,m,r")
	(truncate:QI (match_operand:DI 1 "general_operand" "m,r,0")))]
  ""
  "*
{
  if (which_alternative == 0)
    return \"ld.8\\t%0,%1\";
  else if (which_alternative == 1)
    return \"st.8\\t%1,%0\";
  return \"\";
}")

(define_insn "truncdihi2"
  [(set (match_operand:HI 0 "general_operand" "=r,m,r")
	(truncate:HI (match_operand:DI 1 "general_operand" "m,r,0")))]
  ""
  "*
{
  if (which_alternative == 0)
    return \"ld.16\\t%0,%1\";
  if (which_alternative == 1)
    return \"st.16\\t%1,%0\";
  return \"\";
}")

(define_insn "truncdisi2"
  [(set (match_operand:SI 0 "general_operand" "=r,m")
	(truncate:SI (match_operand:DI 1 "general_operand" "rm,r")))]
  ""
  "*
{
  if (which_alternative == 0)
    return \"ld.32\\t%0,%1\";
  return \"st.32\\t%1,%0\";
}")

(define_insn "truncsiqi2"
  [(set (match_operand:QI 0 "general_operand" "=r,m,r")
	(truncate:QI (match_operand:SI 1 "general_operand" "m,r,0")))]
  ""
  "*
{
  if (which_alternative == 0)
    return \"ld.8\\t%0,%1\";
  if (which_alternative == 1)
    return \"st.8\\t%1,%0\";
  return \"\";
}")

(define_insn "truncsihi2"
  [(set (match_operand:HI 0 "general_operand" "=r,m,r")
	(truncate:HI (match_operand:SI 1 "general_operand" "m,r,0")))]
  ""
  "*
{
  if (which_alternative == 0)
    return \"ld.16\\t%0,%1\";
  if (which_alternative == 1)
    return \"st.16\\t%1,%0\";
  return \"\";
}")

(define_insn "trunchiqi2"
  [(set (match_operand:QI 0 "general_operand" "=r,m,r")
	(truncate:QI (match_operand:HI 1 "general_operand" "m,r,0")))]
  ""
  "*
{
  if (which_alternative == 0)
    return \"ld.8\\t%0,%1\";
  if (which_alternative == 1)
    return \"st.8\\t%1,%0\";
  return \"\";
}")

(define_insn "extendsfdf2"
  [(set (match_operand:DF 0 "register_operand" "=r")
	(sign_extend:DF (match_operand:SF 1 "general_operand" "rm")))]
  ""
  "cvt.sd\\t%0,%1")

(define_insn "extendsidi2"
  [(set (match_operand:DI 0 "register_operand" "=r")
	(sign_extend:DI (match_operand:SI 1 "general_operand" "rm")))]
  ""
  "ld.32\\t%0,%1")

(define_insn "extendhisi2"
  [(set (match_operand:SI 0 "register_operand" "=r,r")
	(sign_extend:SI (match_operand:HI 1 "general_operand" "m,r")))]
  ""
  "*
     if (which_alternative==0)
       return \"ld.16\\t%0,%1\";
     return \"extract\\t%0,%1:bit 48,16\";
")

(define_insn "extendhidi2"
  [(set (match_operand:DI 0 "register_operand" "=r,r")
	(sign_extend:DI (match_operand:HI 1 "general_operand" "m,r")))]
  ""
  "*
     if (which_alternative==0)
       return \"ld.16\\t%0,%1\";
     return \"extract\\t%0,%1:bit 48,16\";
")

(define_insn "extendqihi2"
  [(set (match_operand:HI 0 "register_operand" "=r,r")
	(sign_extend:HI (match_operand:QI 1 "general_operand" "m,r")))]
  ""
  "*
     if (which_alternative==0)
       return \"ld.8\\t%0,%1\";
     return \"extract\\t%0,%1:bit 56,8\";
")

(define_insn "extendqisi2"
  [(set (match_operand:SI 0 "register_operand" "=r,r")
	(sign_extend:SI (match_operand:QI 1 "general_operand" "m,r")))]
  ""
  "*
     if (which_alternative==0)
       return \"ld.8\\t%0,%1\";
     return \"extract\\t%0,%1:bit 56,8\";
")

(define_insn "extendqidi2"
  [(set (match_operand:DI 0 "register_operand" "=r,r")
	(sign_extend:DI (match_operand:QI 1 "general_operand" "m,r")))]
  ""
  "*
     if (which_alternative==0)
       return \"ld.8\\t%0,%1\";
     return \"extract\\t%0,%1:bit 56,8\";
")

(define_insn "zero_extendsidi2"
  [(set (match_operand:DI 0 "register_operand" "=r")
	(zero_extend:DI (match_operand:SI 1 "general_operand" "rm")))]
  ""
  "ldz.32\\t%0,%1")


(define_insn "zero_extendhisi2"
  [(set (match_operand:SI 0 "register_operand" "=r,r")
	(zero_extend:SI (match_operand:HI 1 "general_operand" "m,r")))]
  ""
  "*
     if (which_alternative==0)
       return \"ldz.16\\t%0,%1\";
     return \"extractz\\t%0,%1:bit 48,16\";
")

(define_insn "zero_extendhidi2"
  [(set (match_operand:DI 0 "register_operand" "=r,r")
	(zero_extend:DI (match_operand:HI 1 "general_operand" "m,r")))]
  ""
  "*
     if (which_alternative==0)
       return \"ldz.16\\t%0,%1\";
     return \"extractz\\t%0,%1:bit 48,16\";
")

(define_insn "zero_extendqihi2"
  [(set (match_operand:HI 0 "register_operand" "=r,r")
	(zero_extend:HI (match_operand:QI 1 "general_operand" "m,r")))]
  ""
  "*
     if (which_alternative==0)
       return \"ldz.8\\t%0,%1\";
     return \"extractz\\t%0,%1:bit 56,8\";
")

(define_insn "zero_extendqisi2"
  [(set (match_operand:SI 0 "register_operand" "=r,r")
	(zero_extend:SI (match_operand:QI 1 "general_operand" "m,r")))]
  ""
  "*
     if (which_alternative==0)
       return \"ldz.8\\t%0,%1\";
     return \"extractz\\t%0,%1:bit 56,8\";
")

(define_insn "zero_extendqidi2"
  [(set (match_operand:DI 0 "register_operand" "=r,r")
	(zero_extend:DI (match_operand:QI 1 "general_operand" "m,r")))]
  ""
  "*
     if (which_alternative==0)
       return \"ldz.8\\t%0,%1\";
     return \"extractz\\t%0,%1:bit 56,8\";
")


(define_insn "ashrdi3"
  [(set (match_operand:DI 0 "register_operand" "=r")
	(ashiftrt:DI (match_operand:DI 1 "register_operand" "r")
		   (match_operand:SI 2 "general_operand" "rn")))]
  ""
  "sra\\t%0,%1,%2")

(define_insn "lshrdi3"
  [(set (match_operand:DI 0 "register_operand" "=r")
	(lshiftrt:DI (match_operand:DI 1 "register_operand" "r")
		   (match_operand:SI 2 "general_operand" "rn")))]
  ""
  "srl\\t%0,%1,%2")

(define_insn "ashldi3"
  [(set (match_operand:DI 0 "register_operand" "=r")
	(ashift:DI (match_operand:DI 1 "register_operand" "r")
		   (match_operand:SI 2 "general_operand" "rn")))]
  ""
  "sla\\t%0,%1,%2")

(define_insn "anddi3"
  [(set (match_operand:DI 0 "register_operand" "=r,r")
	(and:DI (match_operand:DI 1 "general_operand" "%0,r")
		(match_operand:DI 2 "general_operand" "g,g")))]
  "1 /*which_alternative == 0 || check356(operands[2])*/"
  "*
    if (which_alternative == 0)
      return \"and\\t%0,%2\";
    return \"and\\t%0,%1,%2\";
")

(define_insn "iordi3"
  [(set (match_operand:DI 0 "register_operand" "=r,r")
	(ior:DI (match_operand:DI 1 "general_operand" "%0,r")
		(match_operand:DI 2 "general_operand" "g,g")))]
  "1 /*which_alternative == 0 || check356(operands[2])*/"
  "*
    if (which_alternative == 0)
      return \"or\\t%0,%2\";
    return \"or\\t%0,%1,%2\";
")

(define_insn "xordi3"
  [(set (match_operand:DI 0 "register_operand" "=r,r")
	(xor:DI (match_operand:DI 1 "general_operand" "%0,r")
		(match_operand:DI 2 "general_operand" "g,g")))]
  "1 /*which_alternative == 0 || check356(operands[2])*/"
  "*
    if (which_alternative == 0)
      return \"xor\\t%0,%2\";
    return \"xor\\t%0,%1,%2\";
")

(define_insn "one_cmpldi2"
  [(set (match_operand:DI 0 "register_operand" "=r")
	(not:DI (match_operand:DI 1 "general_operand" "rm")))]
  ""
  "not\\t%0,%1")

;; gcc 2.1 does not widen ~si into ~di.
(define_insn "one_cmplsi2"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(not:SI (match_operand:SI 1 "register_operand" "r")))]
  ""
  "not\\t%0,%1")

(define_insn "negdi2"
  [(set (match_operand:DI 0 "register_operand" "=r")
	(neg:DI (match_operand:DI 1 "general_operand" "rm")))]
  ""
  "neg.64\\t%0,%1")

(define_insn "negsi2"
  [(set (match_operand:SI 0 "register_operand" "=r,r")
	(neg:SI (match_operand:SI 1 "general_operand" "m,r")))]
  ""
  "*
    if (which_alternative == 0)
      return \"neg.32\\t%0,%1\";
    return \"neg.64\\t%0,%1 ; I only want 32\";
")

(define_insn "neghi2"
  [(set (match_operand:HI 0 "register_operand" "=r,r")
	(neg:HI (match_operand:HI 1 "general_operand" "m,r")))]
  ""
  "*
    if (which_alternative == 0)
      return \"neg.16\\t%0,%1\";
    return \"neg.64\\t%0,%1 ; I only want 16\";
")

(define_insn "adddf3"
  [(set (match_operand:DF 0 "register_operand" "=r")
	(plus:DF (match_operand:DF 1 "general_operand" "%0")
		  (match_operand:DF 2 "general_operand" "rm")))]
  ""
  "fadd.64\\t%0,%2")

(define_insn "addsf3"
  [(set (match_operand:SF 0 "register_operand" "=r")
	(plus:SF (match_operand:SF 1 "general_operand" "%0")
		 (match_operand:SF 2 "general_operand" "rm")))]
  ""
  "fadd.32\\t%0,%2")

;; There is also an addi.64  4,.r0'' optimization
(define_insn "adddi3"
  [(set (match_operand:DI 0 "register_operand" "=r,r")
	(plus:DI (match_operand:DI 1 "general_operand" "%0,r")
		 (match_operand:DI 2 "general_operand" "g,g")))]
  "1 /*which_alternative == 0 || check356(operands[2])*/"
  "*
    if (which_alternative == 0)
      return \"add.64\\t%0,%2\";
    return \"add.64\\t%0,%1,%2\";
")

(define_insn "addsi3"
  [(set (match_operand:SI 0 "register_operand" "=r,r,r")
	(plus:SI (match_operand:SI 1 "general_operand" "%0,r,0")
		 (match_operand:SI 2 "general_operand" "m,m,g")))]
  "1 /*which_alternative != 1 || check356(operands[2])*/"
  "*
    if (which_alternative == 0)
      return \"add.32\\t%0,%2\";
    if (which_alternative == 1)
      return \"add.32\\t%0,%1,%2\";
    return \"add.64\\t%0,%2 ; I only want 32\";
")

(define_insn "addhi3"
  [(set (match_operand:HI 0 "register_operand" "=r,r,r")
	(plus:HI (match_operand:HI 1 "general_operand" "%0,r,0")
		 (match_operand:HI 2 "general_operand" "m,m,g")))]
  "1 /*which_alternative != 1 || check356(operands[2])*/"
  "*
    if (which_alternative == 0)
      return \"add.16\\t%0,%2\";
    if (which_alternative == 1)
      return \"add.16\\t%0,%1,%2\";
    return \"add.64\\t%0,%2 ; I only want 16\";
")

(define_insn "subdf3"
  [(set (match_operand:DF 0 "register_operand" "=r")
	(minus:DF (match_operand:DF 1 "general_operand" "0")
		  (match_operand:DF 2 "general_operand" "rm")))]
  ""
  "fsub.64\\t%0,%2")

(define_insn "subsf3"
  [(set (match_operand:SF 0 "register_operand" "=r")
	(minus:SF (match_operand:SF 1 "general_operand" "0")
		  (match_operand:SF 2 "general_operand" "rm")))]
  ""
  "fsub.32\\t%0,%2")

(define_insn "subdi3"
  [(set (match_operand:DI 0 "register_operand" "=r,r,r")
	(minus:DI (match_operand:DI 1 "general_operand" "0,g,r")
		  (match_operand:DI 2 "general_operand" "g,r,g")))]
  "1 /*which_alternative == 0 || check356(operands[which_alternative])*/"
  "*
    if (which_alternative == 0)
      return \"sub.64\\t%0,%2\";
    else if (which_alternative == 1)
      return \"subr.64\\t%0,%2,%1\";
    else
      return \"sub.64\\t%0,%1,%2\";
")

(define_insn "subsi3"
  [(set (match_operand:SI 0 "register_operand" "=r,r,r,r")
	(minus:SI (match_operand:SI 1 "general_operand" "0,m,r,0")
		  (match_operand:SI 2 "general_operand" "m,r,m,g")))]
  "1 /*which_alternative == 0 || check356(operands[which_alternative])*/"
  "*
    if (which_alternative == 0)
      return \"sub.32\\t%0,%2\";
    else if (which_alternative == 1)
      return \"subr.32\\t%0,%2,%1\";
    else if (which_alternative == 2)
      return \"sub.32\\t%0,%1,%2\";
    else
      return \"sub.64\\t%0,%2 ; I only want 32\";
")

(define_insn "subhi3"
  [(set (match_operand:HI 0 "register_operand" "=r,r,r,r")
	(minus:HI (match_operand:HI 1 "general_operand" "0,m,r,0")
		  (match_operand:HI 2 "general_operand" "m,r,m,g")))]
  "1 /*which_alternative == 0 || check356(operands[which_alternative])*/"
  "*
    if (which_alternative == 0)
      return \"sub.16\\t%0,%2\";
    else if (which_alternative == 1)
      return \"subr.16\\t%0,%2,%1\";
    else if (which_alternative == 2)
      return \"sub.16\\t%0,%1,%2\";
    else
      return \"sub.64\\t%0,%2 ; I only want 16\";
")

(define_insn "muldf3"
  [(set (match_operand:DF 0 "register_operand" "=r")
	(mult:DF (match_operand:DF 1 "general_operand" "%0")
		 (match_operand:DF 2 "general_operand" "rm")))]
  ""
  "fmul.64\\t%0,%2")

(define_insn "mulsf3"
  [(set (match_operand:SF 0 "register_operand" "=r")
	(mult:SF (match_operand:SF 1 "general_operand" "%0")
		 (match_operand:SF 2 "general_operand" "rm")))]
  ""
  "fmul.32\\t%0,%2")

(define_insn "muldi3"
  [(set (match_operand:DI 0 "register_operand" "=r,r")
	(mult:DI (match_operand:DI 1 "general_operand" "%0,r")
		 (match_operand:DI 2 "general_operand" "g,g")))]
  "1 /*which_alternative == 0 || check356(operands[2])*/"
  "*
    if (which_alternative == 0)
      return \"mul.64\\t%0,%2\";
    return \"mul.64\\t%0,%1,%2\";
")

(define_insn "mulsi3"
  [(set (match_operand:SI 0 "register_operand" "=r,r,r")
	(mult:SI (match_operand:SI 1 "general_operand" "%0,r,0")
		 (match_operand:SI 2 "general_operand" "m,m,g")))]
  "1 /*which_alternative == 0 || check356(operands[2])*/"
  "*
    if (which_alternative == 0)
      return \"mul.32\\t%0,%2\";
    else if (which_alternative == 1)
      return \"mul.32\\t%0,%1,%2\";
    else
      return \"mul.64\\t%0,%2 ; I only want 32\";
")

(define_insn "mulhi3"
  [(set (match_operand:HI 0 "register_operand" "=r,r,r")
	(mult:HI (match_operand:HI 1 "general_operand" "%0,r,0")
		 (match_operand:HI 2 "general_operand" "m,m,g")))]
  "1 /*which_alternative == 0 || check356(operands[2])*/"
  "*
    if (which_alternative == 0)
      return \"mul.16\\t%0,%2\";
    else if (which_alternative == 1)
      return \"mul.16\\t%0,%1,%2\";
    else
      return \"mul.64\\t%0,%2 ; I only want 16\";
")

(define_insn "divdf3"
  [(set (match_operand:DF 0 "register_operand" "=r")
	(div:DF (match_operand:DF 1 "general_operand" "0")
		(match_operand:DF 2 "general_operand" "rm")))]
  ""
  "fdiv.64\\t%0,%2")

(define_insn "divsf3"
  [(set (match_operand:SF 0 "register_operand" "=r")
	(div:SF (match_operand:SF 1 "general_operand" "0")
		(match_operand:SF 2 "general_operand" "rm")))]
  ""
  "fdiv.32\\t%0,%2")

(define_insn "divdi3"
  [(set (match_operand:DI 0 "register_operand" "=r,r,r")
	(div:DI (match_operand:DI 1 "general_operand" "0,g,r")
		(match_operand:DI 2 "general_operand" "g,r,g")))]
  "1 /*which_alternative == 0 || check356(operands[which_alternative])*/"
  "*
    if (which_alternative == 0)
      return \"div.64\\t%0,%2\";
    else if (which_alternative == 1)
      return \"divr.64\\t%0,%2,%1\";
    else
      return \"div.64\\t%0,%1,%2\";
")

(define_insn "divsi3"
  [(set (match_operand:SI 0 "register_operand" "=r,r,r,r")
	(div:SI (match_operand:SI 1 "general_operand" "0,m,r,0")
		(match_operand:SI 2 "general_operand" "m,r,m,g")))]
  "1 /*which_alternative == 0 || check356(operands[which_alternative])*/"
  "*
/* We don't ignore high bits. */
if (0) {
    if (which_alternative == 0)
      return \"div.32\\t%0,%2\";
    else if (which_alternative == 1)
      return \"divr.32\\t%0,%2,%1\";
    else if (which_alternative == 2)
      return \"div.32\\t%0,%1,%2\";
    else
      return \"ld.32\\t%0,%0\;div.64\\t%0,%2 ; I only want 32\";
} else {
    if (which_alternative == 0)
      return \"ld.32\\t%0,%0\;div.32\\t%0,%2\";
    else if (which_alternative == 1)
      return \"ld.32\\t%2,%2\;divr.32\\t%0,%2,%1\";
    else if (which_alternative == 2)
      return \"ld.32\\t%1,%1\;div.32\\t%0,%1,%2\";
    else
      return \"ld.32\\t%0,%0\;div.64\\t%0,%2 ; I only want 32\";
}
")

(define_insn "divhi3"
  [(set (match_operand:HI 0 "register_operand" "=r,r,r,r,r")
	(div:HI (match_operand:HI 1 "general_operand" "0,m,r,0,0")
		(match_operand:HI 2 "general_operand" "m,r,m,r,i")))]
  "1 /*which_alternative == 0 || check356(operands[which_alternative])*/"
  "*
    if (which_alternative == 0)
      return \"extract\\t%0,%0:bit 48,16\;div.16\\t%0,%2\";
    else if (which_alternative == 1)
      return \"extract\\t%2,%2:bit 48,16\;divr.16\\t%0,%2,%1\";
    else if (which_alternative == 2)
      return \"extract\\t%1,%1:bit 48,16\;div.16\\t%0,%1,%2\";
    else if (which_alternative == 3)
      return \"extract\\t%0,%0:bit 48,16\;extract\\t%2,%2:bit 48,16\;div.64\\t%0,%2 ; I only want 16\";
    else
      return \"extract\\t%0,%0:bit 48,16\;div.64\\t%0,%2 ; I only want 16\";
")

(define_insn "modhi3"
  [(set (match_operand:HI 0 "register_operand" "=r,r,r,r,r")
	(mod:HI (match_operand:HI 1 "general_operand" "0,m,r,0,0")
		(match_operand:HI 2 "general_operand" "m,r,m,r,i")))]
  "1 /*which_alternative == 0 || check356(operands[which_alternative])*/"
  "*
    if (which_alternative == 0)
      return \"extract\\t%0,%0:bit 48,16\;rem.16\\t%0,%2\";
    else if (which_alternative == 1)
      return \"extract\\t%2,%2:bit 48,16\;remr.16\\t%0,%2,%1\";
    else if (which_alternative == 2)
      return \"extract\\t%1,%1:bit 48,16\;rem.16\\t%0,%1,%2\";
    else if (which_alternative == 3)
      return \"extract\\t%0,%0:bit 48,16\;extract\\t%2,%2:bit 48,16\;rem.64\\t%0,%2 ; I only want 16\";
    else
      return \"extract\\t%0,%0:bit 48,16\;rem.64\\t%0,%2 ; I only want 16\";
")

(define_insn "moddi3"
  [(set (match_operand:DI 0 "register_operand" "=r,r,r")
	(mod:DI (match_operand:DI 1 "general_operand" "0,g,r")
		(match_operand:DI 2 "general_operand" "g,r,g")))]
  "1 /*which_alternative == 0 || check356(operands[which_alternative])*/"
  "*
    if (which_alternative == 0)
      return \"rem.64\\t%0,%2\";
    else if (which_alternative == 1)
      return \"remr.64\\t%0,%2,%1\";
    else
      return \"rem.64\\t%0,%1,%2\";
")

(define_insn "modsi3"
  [(set (match_operand:SI 0 "register_operand" "=r,r,r,r")
	(mod:SI (match_operand:SI 1 "general_operand" "0,m,r,0")
		(match_operand:SI 2 "general_operand" "m,r,m,g")))]
  "1 /*which_alternative == 0 || check356(operands[which_alternative])*/"
  "*
/* There is a micro code bug with the below... */
if (0) {
    if (which_alternative == 0)
      return \"rem.32\\t%0,%2\";
    else if (which_alternative == 1)
      return \"remr.32\\t%0,%2,%1\";
    else if (which_alternative == 2)
      return \"rem.32\\t%0,%1,%2\";
    else
      return \"ld.32\\t%0,%0\;rem.64\\t%0,%2 ; I only want 32\";
} else {
    if (which_alternative == 0)
      return \"ld.32\\t%0,%0\;rem.32\\t%0,%2\";
    else if (which_alternative == 1)
      return \"ld.32\\t%2,%2\;remr.32\\t%0,%2,%1\";
    else if (which_alternative == 2)
      return \"ld.32\\t%1,%1\;rem.32\\t%0,%1,%2\";
    else
      return \"ld.32\\t%0,%0\;rem.64\\t%0,%2 ; I only want 32\";
}
")


(define_insn "jump"
  [(set (pc)
	(label_ref (match_operand 0 "" "")))]
  ""
  "jmp\\t%l0")

(define_insn "indirect_jump"
  [(set (pc) (match_operand:SI 0 "register_operand" "r"))]
  ""
;; Maybe %l0 is better, maybe we can relax register only.
  "verify this before use ld.32\\t.r0,%0\;br.reg\\t.r0")

(define_insn "beq"
  [(set (pc)
	(if_then_else (eq (cc0)
			  (const_int 0))
		      (label_ref (match_operand 0 "" ""))
		      (pc)))]
  ""
  "* return cmp_jmp(\"\", 2, operands[0]); ")

(define_insn "bne"
  [(set (pc)
	(if_then_else (ne (cc0)
			  (const_int 0))
		      (label_ref (match_operand 0 "" ""))
		      (pc)))]
  ""
  "* return cmp_jmp(\"\", 8, operands[0]); ")

(define_insn "bgt"
  [(set (pc)
	(if_then_else (gt (cc0)
			  (const_int 0))
		      (label_ref (match_operand 0 "" ""))
		      (pc)))]
  ""
  "* return cmp_jmp(\"\", 0, operands[0]); ")

(define_insn "bgtu"
  [(set (pc)
	(if_then_else (gtu (cc0)
			  (const_int 0))
		      (label_ref (match_operand 0 "" ""))
		      (pc)))]
  ""
  "* return cmp_jmp(\"u\", 0, operands[0]); ")

(define_insn "blt"
  [(set (pc)
	(if_then_else (lt (cc0)
			  (const_int 0))
		      (label_ref (match_operand 0 "" ""))
		      (pc)))]
  ""
  "* return cmp_jmp(\"\", 6, operands[0]); ")

(define_insn "bltu"
  [(set (pc)
	(if_then_else (ltu (cc0)
			  (const_int 0))
		      (label_ref (match_operand 0 "" ""))
		      (pc)))]
  ""
  "* return cmp_jmp(\"u\", 6, operands[0]); ")

(define_insn "bge"
  [(set (pc)
	(if_then_else (ge (cc0)
			  (const_int 0))
		      (label_ref (match_operand 0 "" ""))
		      (pc)))]
  ""
  "* return cmp_jmp(\"\", 4, operands[0]); ")

(define_insn "bgeu"
  [(set (pc)
	(if_then_else (geu (cc0)
			  (const_int 0))
		      (label_ref (match_operand 0 "" ""))
		      (pc)))]
  ""
  "* return cmp_jmp(\"u\", 4, operands[0]); ")

(define_insn "ble"
  [(set (pc)
	(if_then_else (le (cc0)
			  (const_int 0))
		      (label_ref (match_operand 0 "" ""))
		      (pc)))]
  ""
  "* return cmp_jmp(\"\", 10, operands[0]); ")

(define_insn "bleu"
  [(set (pc)
	(if_then_else (leu (cc0)
			  (const_int 0))
		      (label_ref (match_operand 0 "" ""))
		      (pc)))]
  ""
  "* return cmp_jmp(\"u\", 10, operands[0]); ")

(define_insn ""
  [(set (pc)
	(if_then_else (eq (cc0)
			  (const_int 0))
		      (pc)
		      (label_ref (match_operand 0 "" ""))))]
  ""
  "* return cmp_jmp(\"\", 8, operands[0]); ")

(define_insn ""
  [(set (pc)
	(if_then_else (ne (cc0)
			  (const_int 0))
		      (pc)
		      (label_ref (match_operand 0 "" ""))))]
  ""
  "* return cmp_jmp(\"\", 2, operands[0]); ")

(define_insn ""
  [(set (pc)
	(if_then_else (gt (cc0)
			  (const_int 0))
		      (pc)
		      (label_ref (match_operand 0 "" ""))))]
  ""
  "* return cmp_jmp(\"\", 10, operands[0]); ")

(define_insn ""
  [(set (pc)
	(if_then_else (gtu (cc0)
			  (const_int 0))
		      (pc)
		      (label_ref (match_operand 0 "" ""))))]
  ""
  "* return cmp_jmp(\"u\", 10, operands[0]); ")

(define_insn ""
  [(set (pc)
	(if_then_else (lt (cc0)
			  (const_int 0))
		      (pc)
		      (label_ref (match_operand 0 "" ""))))]
  ""
  "* return cmp_jmp(\"\", 4, operands[0]); ")

(define_insn ""
  [(set (pc)
	(if_then_else (ltu (cc0)
			  (const_int 0))
		      (pc)
		      (label_ref (match_operand 0 "" ""))))]
  ""
  "* return cmp_jmp(\"u\", 4, operands[0]); ")

(define_insn ""
  [(set (pc)
	(if_then_else (ge (cc0)
			  (const_int 0))
		      (pc)
		      (label_ref (match_operand 0 "" ""))))]
  ""
  "* return cmp_jmp(\"\", 6, operands[0]); ")

(define_insn ""
  [(set (pc)
	(if_then_else (geu (cc0)
			  (const_int 0))
		      (pc)
		      (label_ref (match_operand 0 "" ""))))]
  ""
  "* return cmp_jmp(\"u\", 6, operands[0]); ")

(define_insn ""
  [(set (pc)
	(if_then_else (le (cc0)
			  (const_int 0))
		      (pc)
		      (label_ref (match_operand 0 "" ""))))]
  ""
  "* return cmp_jmp(\"\", 0, operands[0]); ")

(define_insn ""
  [(set (pc)
	(if_then_else (leu (cc0)
			  (const_int 0))
		      (pc)
		      (label_ref (match_operand 0 "" ""))))]
  ""
  "* return cmp_jmp(\"u\", 0, operands[0]); ")

;; Note that operand 1 is total size of args, in bytes,
;; and what the call insn wants is the number of words.
(define_insn "call"
  [(call (match_operand:QI 0 "general_operand" "m")
	 (match_operand:QI 1 "general_operand" "g"))]
  ""
  "*
  if (GET_CODE (operands[0]) == MEM && GET_CODE (XEXP (operands[0], 0)) == REG)
    if (REGNO (XEXP (operands[0], 0)) != 0)
      return \"add.64\\t.sp,=-4\;ld.64\\t.r0,=.+11\;st.32\\t.r0,[.sp]\;br.reg\\t%r0\;add.64\\t.sp,=4\;add.64\\t.sp,%1\";
    else
      return \"add.64\\t.sp,=-4\;ld.64\\t.r1,=.+11\;st.32\\t.r1,[.sp]\;br.reg\\t%r0\;add.64\\t.sp,=4\;add.64\\t.sp,%1\";
  else
    return \"add.64\\t.sp,=-4\;call\\t%0\;add.64\\t.sp,=4\;add.64\\t.sp,%1\";
  ")

(define_insn "call_value"
  [(set (match_operand 0 "" "")
	(call (match_operand:QI 1 "general_operand" "m")
	      (match_operand:QI 2 "general_operand" "g")))]
  ""
  "*
  if (GET_CODE (operands[1]) == MEM && GET_CODE (XEXP (operands[1], 0)) == REG)
    if (REGNO (XEXP (operands[1], 0)) != 0)
      return \"add.64\\t.sp,=-4\;ld.64\\t.r0,=.+11\;st.32\\t.r0,[.sp]\;br.reg\\t%r1\;add.64\\t.sp,=4\;add.64\\t.sp,%2\";
    else
      return \"add.64\\t.sp,=-4\;ld.64\\t.r1,=.+11\;st.32\\t.r1,[.sp]\;br.reg\\t%r1\;add.64\\t.sp,=4\;add.64\\t.sp,%2\";
  else
    return \"add.64\\t.sp,=-4\;call\\t%1\;add.64\\t.sp,=4\;add.64\\t.sp,%2\";
  ")

(define_insn "tablejump"
  [(set (pc) (match_operand:SI 0 "register_operand" "r"))
   (use (label_ref (match_operand 1 "" "")))]
  ""
  "br.reg\\t%0")

(define_insn "nop"
  [(const_int 0)]
  ""
  "nop")
