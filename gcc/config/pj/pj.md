;; Machine description for GNU compiler, picoJava Version
;; Copyright (C) 2000 Free Software Foundation, Inc.
;; Contributed by Steve Chamberlain, of Transmeta (sac@pobox.com).

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

;; Move instructions.

(define_insn "movsi"
  [(set (match_operand:SI 0 "nonimmediate_operand" "=gD")
        (match_operand:SI 1 "pj_source_operand" "gS"))]
  ""
  "%S1%R0")

(define_insn "movhi"
  [(set (match_operand:HI 0 "nonimmediate_operand" "=gD")
        (match_operand:HI 1 "pj_source_operand" "gS"))]
  ""
  "%S1%R0")

(define_insn "movqi"
  [(set (match_operand:QI 0 "nonimmediate_operand" "=gD")
        (match_operand:QI 1 "pj_source_operand" "gS"))]
  ""
  "%S1%R0")

(define_insn "movdi"
  [(set (match_operand:DI 0 "nonimmediate_operand" "=gD")
        (match_operand:DI 1 "pj_source_operand" "gS"))]
  ""
  "%D1%*%R0")

(define_insn "movdf"
  [(set (match_operand:DF 0 "nonimmediate_operand" "=gD")
        (match_operand:DF 1 "pj_source_operand" "gS"))]
  ""
  "%D1%R0")

(define_insn "movsf"
  [(set (match_operand:SF 0 "nonimmediate_operand" "=gD")
        (match_operand:SF 1 "pj_source_operand" "gS"))]
  ""
  "%S1%R0")


;; Arithmetic.

(define_insn "addsi3"
  [(set (match_operand:SI 0 "nonimmediate_operand" "=gD")
        (plus:SI (match_operand:SI 1 "pj_source_operand" "%gS")
                 (match_operand:SI 2 "pj_source_operand" "gS")))]
  ""
  "* return pj_output_addsi3 (operands);")

(define_insn "adddi3"
  [(set (match_operand:DI 0 "nonimmediate_operand" "=gD")
        (plus:DI (match_operand:DI 1 "pj_source_operand" "%gS")
                 (match_operand:DI 2 "pj_source_operand" "gS")))]
  ""
  "%D1%D2%*ladd%R0")

(define_insn "addsf3"
  [(set (match_operand:SF 0 "nonimmediate_operand" "=gD")
        (plus:SF (match_operand:SF 1 "pj_source_operand" "%gS")
                 (match_operand:SF 2 "pj_source_operand" "gS")))]
  ""
  "%S1%S2%*fadd%R0")

(define_insn "adddf3"
  [(set (match_operand:DF 0 "nonimmediate_operand" "=gD")
        (plus:DF (match_operand:DF 1 "pj_source_operand" "%gS")
                 (match_operand:DF 2 "pj_source_operand" "gS")))]
  ""
  "%D1%D2%*dadd%R0")

(define_insn "negsi2"
  [(set (match_operand:SI 0 "nonimmediate_operand" "=gD")
        (neg:SI (match_operand:SI 1 "pj_source_operand" "gS")))]
  ""
  "%S1%*ineg%R0")

(define_insn "negdi2"
  [(set (match_operand:DI 0 "nonimmediate_operand" "=gD")
        (neg:DI (match_operand:DI 1 "pj_source_operand" "gS")))]
  ""
  "%S1%*lneg%R0")

(define_insn "negsf2"
  [(set (match_operand:SF 0 "nonimmediate_operand" "=gD")
        (neg:SF (match_operand:SF 1 "pj_source_operand" "gS")))]
  ""
  "%S1%*fneg%R0")

(define_insn "negdf2"
  [(set (match_operand:DF 0 "nonimmediate_operand" "=gD")
        (neg:DF (match_operand:DF 1 "pj_source_operand" "gS")))]
  ""
  "%D1%*dneg%R0")

(define_insn "subsi3"
  [(set (match_operand:SI 0 "nonimmediate_operand" "=gD")
        (minus:SI (match_operand:SI 1 "pj_source_operand" "gS")
                  (match_operand:SI 2 "pj_source_operand" "gS")))]
  ""
  "%S1%S2%*isub%R0")

(define_insn "subdi3"
  [(set (match_operand:DI 0 "nonimmediate_operand" "=gD")
        (minus:DI (match_operand:DI 1 "pj_source_operand" "gS")
                  (match_operand:DI 2 "pj_source_operand" "gS")))]
  ""
  "%D1%D2%*lsub%R0")

(define_insn "subsf3"
  [(set (match_operand:SF 0 "nonimmediate_operand" "=gD")
        (minus:SF (match_operand:SF 1 "pj_source_operand" "gS")
                 (match_operand:SF 2 "pj_source_operand" "gS")))]
  ""
  "%S1%S2%*fsub%R0")

(define_insn "subdf3"
  [(set (match_operand:DF 0 "nonimmediate_operand" "=gD")
        (minus:DF (match_operand:DF 1 "pj_source_operand" "gS")
                 (match_operand:DF 2 "pj_source_operand" "gS")))]
  ""
  "%D1%D2%*dsub%R0")

(define_insn "divsi3"
  [(set (match_operand:SI 0 "nonimmediate_operand" "=gD")
        (div:SI (match_operand:SI 1 "pj_source_operand" "gS")
                (match_operand:SI 2 "pj_source_operand" "gS")))]
  ""
  "%S1%S2%*idiv%R0")

(define_insn "divdi3"
  [(set (match_operand:DI 0 "nonimmediate_operand" "=gD")
        (div:DI (match_operand:DI 1 "pj_source_operand" "gS")
                (match_operand:DI 2 "pj_source_operand" "gS")))]
  ""
  "%D1%D2%*ldiv%R0")

(define_insn "divsf3"
  [(set (match_operand:SF 0 "nonimmediate_operand" "=gD")
        (div:SF (match_operand:SF 1 "pj_source_operand" "gS")
		(match_operand:SF 2 "pj_source_operand" "gS")))]
  ""
  "%S1%S2%*fdiv%R0")

(define_insn "divdf3"
  [(set (match_operand:DF 0 "nonimmediate_operand" "=gD")
        (div:DF (match_operand:DF 1 "pj_source_operand" "gS")
		(match_operand:DF 2 "pj_source_operand" "gS")))]
  ""
  "%D1%D2%*ddiv%R0")

(define_insn "udivsi3"
  [(set (match_operand:SI 0 "nonimmediate_operand" "=gD")
        (udiv:SI (match_operand:SI 1 "pj_source_operand" "gS")
                (match_operand:SI 2 "pj_source_operand" "gS")))]
  ""
  "%P1%P2%*ldiv%*l2i%R0")

(define_insn "mulsi3"
  [(set (match_operand:SI 0 "nonimmediate_operand" "=gD")
        (mult:SI (match_operand:SI 1 "pj_source_operand" "gS")
                 (match_operand:SI 2 "pj_source_operand" "gS")))]
  ""
  "%S1%S2%*imul%R0")

(define_insn "muldi3"
  [(set (match_operand:DI 0 "nonimmediate_operand" "=gD")
        (mult:DI (match_operand:DI 1 "pj_source_operand" "gS")
                 (match_operand:DI 2 "pj_source_operand" "gS")))]
  ""
  "%D1%D2%*lmul%R0")

(define_insn "muldf3"
  [(set (match_operand:DF 0 "nonimmediate_operand" "=gD")
        (mult:DF (match_operand:DF 1 "pj_source_operand" "%gS")
		(match_operand:DF 2 "pj_source_operand" "gS")))]
  ""
  "%D1%D2%*dmul%R0")

(define_insn "mulsf3"
  [(set (match_operand:SF 0 "nonimmediate_operand" "=gD")
        (mult:SF (match_operand:SF 1 "pj_source_operand" "%gS")
		(match_operand:SF 2 "pj_source_operand" "gS")))]
  ""
  "%S1%S2%*fmul%R0")

(define_insn "modsi3"
  [(set (match_operand:SI 0 "nonimmediate_operand" "=gD")
        (mod:SI (match_operand:SI 1 "pj_source_operand" "gS")
                (match_operand:SI 2 "pj_source_operand" "gS")))]
  ""
  "%S1%S2%*irem%R0")

(define_insn "moddi3"
  [(set (match_operand:DI 0 "nonimmediate_operand" "=gD")
        (mod:DI (match_operand:DI 1 "pj_source_operand" "gS")
                (match_operand:DI 2 "pj_source_operand" "gS")))]
  ""
  "%D1%D2%*lrem%R0")

(define_insn "moddf3"
  [(set (match_operand:DF 0 "nonimmediate_operand" "=gD")
        (mod:DF (match_operand:DF 1 "pj_source_operand" "gS")
		(match_operand:DF 2 "pj_source_operand" "gS")))]
  ""
  "%D1%D2%*drem%R0")

(define_insn "modsf3"
  [(set (match_operand:SF 0 "nonimmediate_operand" "=gD")
        (mod:SF (match_operand:SF 1 "pj_source_operand" "gS")
		(match_operand:SF 2 "pj_source_operand" "gS")))]
  ""
  "%S1%S2%*frem%R0")

(define_insn "umodsi3"
  [(set (match_operand:SI 0 "nonimmediate_operand" "=gD")
        (umod:SI (match_operand:SI 1 "pj_source_operand" "gS")
                (match_operand:SI 2 "pj_source_operand" "gS")))]
  ""
  "%P1%P2%*lrem%*l2i%R0")


;; Logical operations.

(define_insn "andsi3"
  [(set (match_operand:SI 0 "nonimmediate_operand" "=gD")
        (and:SI (match_operand:SI 1 "pj_source_operand" "%gS")
                (match_operand:SI 2 "pj_source_operand" "gS")))]
  ""
  "%S1%S2%*iand%R0")

(define_insn "anddi3"
  [(set (match_operand:DI 0 "nonimmediate_operand" "=gD")
        (and:DI (match_operand:DI 1 "pj_source_operand" "%gS")
                (match_operand:DI 2 "pj_source_operand" "gS")))]
  ""
  "%D1%D2%*land%R0")

(define_insn "iorsi3"
  [(set (match_operand:SI 0 "nonimmediate_operand" "=gD")
        (ior:SI (match_operand:SI 1 "pj_source_operand" "%gS")
                (match_operand:SI 2 "pj_source_operand" "gS")))]
  ""
  "%S1%S2%*ior%R0")

(define_insn "iordi3"
  [(set (match_operand:DI 0 "nonimmediate_operand" "=gD")
        (ior:DI (match_operand:DI 1 "pj_source_operand" "%gS")
                (match_operand:DI 2 "pj_source_operand" "gS")))]
  ""
  "%D1%D2%*lor%R0")

(define_insn "xorsi3"
  [(set (match_operand:SI 0 "nonimmediate_operand" "=gD")
        (xor:SI (match_operand:SI 1 "pj_source_operand" "%gS")
                (match_operand:SI 2 "pj_source_operand" "gS")))]
  ""
  "%S1%S2%*ixor%R0")

(define_insn "xordi3"
  [(set (match_operand:DI 0 "nonimmediate_operand" "=gD")
        (xor:DI (match_operand:DI 1 "pj_source_operand" "%gS")
                (match_operand:DI 2 "pj_source_operand" "gS")))]
  ""
  "%D1%D2%*lxor%R0")

(define_insn "one_cmplsi2"
  [(set (match_operand:SI 0 "nonimmediate_operand" "=gD")
        (not:SI (match_operand:SI 1 "pj_source_operand" "gS")))]
  ""
  "%S1%*iconst_m1%*ixor%R0")

(define_insn "one_cmpldi2"
  [(set (match_operand:DI 0 "nonimmediate_operand" "=gD")
        (not:DI (match_operand:DI 1 "pj_source_operand" "gS")))]
  ""
  "%S1%*iconst_m1%*iconst_m1%*lxor%R0")


;; Shift instructions.

(define_insn "ashlsi3"
  [(set (match_operand:SI 0 "nonimmediate_operand" "=gD")
        (ashift:SI (match_operand:SI 1 "pj_source_operand" "gS")
                   (match_operand:SI 2 "pj_source_operand" "gS")))]
  ""
  "%S1%S2%*ishl%R0")


(define_insn "ashldi3"
  [(set (match_operand:DI 0 "nonimmediate_operand" "=gD")
        (ashift:DI (match_operand:DI 1 "pj_source_operand" "gS")
                   (match_operand:SI 2 "pj_source_operand" "gS")))]
  ""
  "%D1%S2%*lshl%R0")

(define_insn "ashrsi3"
  [(set (match_operand:SI 0 "nonimmediate_operand" "=gD")
        (ashiftrt:SI (match_operand:SI 1 "pj_source_operand" "gS")
                     (match_operand:SI 2 "pj_source_operand" "gS")))]
  ""
  "%S1%S2%*ishr%R0")

(define_insn "ashrdi3"
  [(set (match_operand:DI 0 "nonimmediate_operand" "=gD")
        (ashiftrt:DI (match_operand:DI 1 "pj_source_operand" "gS")
                     (match_operand:SI 2 "pj_source_operand" "gS")))]
  ""
  "%D1%S2%*lshr%R0")

(define_insn "lshrsi3"
  [(set (match_operand:SI 0 "nonimmediate_operand" "=gD")
        (lshiftrt:SI (match_operand:SI 1 "pj_source_operand" "gS")
                     (match_operand:SI 2 "pj_source_operand" "gS")))]
  ""
  "%S1%S2%*iushr%R0")

(define_insn "lshrdi3"
  [(set (match_operand:DI 0 "nonimmediate_operand" "=gD")
        (lshiftrt:DI (match_operand:DI 1 "pj_source_operand" "gS")
                     (match_operand:SI 2 "pj_source_operand" "gS")))]
  ""
  "%D1%S2%*lushr%R0")


;; Comparisons.

(define_expand "cmpsi"
  [(set (cc0) (compare (match_operand:SI 0 "pj_source_operand" "gS")
                       (match_operand:SI 1 "pj_source_operand" "gS")))]
  ""
  "{ 
     pj_cmp_op0 = operands[0];
     pj_cmp_op1 = operands[1];
     pj_cmp_mode = SImode;
     DONE;
   }")

(define_expand "cmpdi"
  [(set (cc0) (compare (match_operand:DI 0 "pj_source_operand" "gS")
                       (match_operand:DI 1 "pj_source_operand" "gS")))]
  ""
  "{ 
     pj_cmp_op0 = operands[0];
     pj_cmp_op1 = operands[1];
     pj_cmp_mode = DImode; 
     DONE;
   }")

(define_expand "cmpsf"
  [(set (cc0) (compare (match_operand:SF 0 "pj_source_operand" "gS")
                       (match_operand:SF 1 "pj_source_operand" "gS")))]
  ""
  "{ 
     pj_cmp_op0 = operands[0];
     pj_cmp_op1 = operands[1];
     pj_cmp_mode = SFmode;
     DONE;
   }")

(define_expand "cmpdf"
  [(set (cc0) (compare (match_operand:DF 0 "pj_source_operand" "gS")
                       (match_operand:DF 1 "pj_source_operand" "gS")))]
  ""
  "{ 
     pj_cmp_op0 = operands[0];
     pj_cmp_op1 = operands[1];
     pj_cmp_mode = DFmode;
     DONE;
   }")


;; Conversions.

(define_insn "truncsiqi2"
  [(set (match_operand:QI 0 "nonimmediate_operand" "=gD")
        (truncate:QI (match_operand:SI 1 "pj_source_operand" "gS")))]
  ""
  "%S1%*%R0")

(define_insn "truncsihi2"
  [(set (match_operand:HI 0 "nonimmediate_operand" "=gD")
        (truncate:HI (match_operand:SI 1 "pj_source_operand" "gS")))]
  ""
  "%S1%*i2c%R0")

(define_insn "truncdisi2"
  [(set (match_operand:SI 0 "nonimmediate_operand" "=gD")
        (truncate:SI (match_operand:DI 1 "pj_source_operand" "gS")))]
  ""
  "%D1%*l2i%R0")

(define_insn "fix_truncsfsi2"
  [(set (match_operand:SI 0 "nonimmediate_operand" "=gD")
	(fix:SI (match_operand:SF 1 "pj_source_operand" "gS")))]
  ""
  "%S1%*f2i%R0")

(define_insn "fix_truncsfdi2"
  [(set (match_operand:DI 0 "nonimmediate_operand" "=gD")
	(fix:DI (match_operand:SF 1 "pj_source_operand" "gS")))]
  ""
  "%S1%*f2l%R0")

(define_insn "truncdfsf2"
  [(set (match_operand:SF 0 "nonimmediate_operand" "=gD")
	(float_truncate:SF (match_operand:DF  1 "pj_source_operand" "gS")))]
  ""
  "%D1%*d2f%R0")

(define_insn "fix_truncdfsi2"
  [(set (match_operand:SI 0 "nonimmediate_operand" "=gD")
	(fix:SI (match_operand:DF 1 "pj_source_operand" "gS")))]
  ""
  "%D1%*d2i%R0")

(define_insn "fix_truncdfdi2"
  [(set (match_operand:DI 0 "nonimmediate_operand" "=gD")
	(fix:DI (match_operand:DF 1 "pj_source_operand" "gS")))]
  ""
  "%D1%*d2l%R0")

(define_insn "floatsisf2"
  [(set (match_operand:SF 0 "nonimmediate_operand" "=gD")
	(float:SF (match_operand:SI 1 "pj_source_operand" "gS")))]
  ""
  "%S1%*i2f%R0")

(define_insn "floatsidf2"
  [(set (match_operand:DF 0 "nonimmediate_operand" "=gD")
	(float:DF (match_operand:SI 1 "pj_source_operand" "gS")))]
  ""
  "%S1%*i2d%R0")

(define_insn "floatdisf2"
  [(set (match_operand:SF 0 "nonimmediate_operand" "=gD")
	(float:SF (match_operand:DI 1 "pj_source_operand" "gS")))]
  ""
  "%D1%*l2f%R0")

(define_insn "floatdidf2"
  [(set (match_operand:DF 0 "nonimmediate_operand" "=gD")
	(float:DF (match_operand:DI 1 "pj_source_operand" "gS")))]
  ""
  "%D1%*l2d%R0")


;; Zero-extend move instructions.

(define_insn "zero_extendsidi2"
  [(set (match_operand:DI 0 "nonimmediate_operand" "=gD")
        (zero_extend:DI (match_operand:SI 1 "pj_source_operand" "gS")))]
  ""
  "%P1%R0")

(define_insn "zero_extendhisi2"
  [(set (match_operand:SI 0 "nonimmediate_operand" "=gD")
        (zero_extend:SI (match_operand:HI 1 "pj_source_operand" "gS")))]
  ""
  "%S1%*i2c%R0")

(define_insn "zero_extendqisi2"
  [(set (match_operand:SI 0 "nonimmediate_operand" "=gD")
        (zero_extend:SI (match_operand:QI 1 "pj_source_operand" "gS")))]
  ""
  "%S1%*sipush 0xff%*iand%R0")


;; Conditional branch instructions.

(define_expand "beq"
  [(set (pc) (if_then_else (match_op_dup 3  [(match_dup 1) (match_dup 2)])
                           (label_ref (match_operand 0 "" ""))
                           (pc)))]
  ""
  "operands[3] = gen_rtx (EQ, pj_cmp_mode);
   operands[1] = pj_cmp_op0;
   operands[2] = pj_cmp_op1;")

(define_expand "bne"
  [(set (pc) (if_then_else (match_op_dup 3  [(match_dup 1) (match_dup 2)])
                           (label_ref (match_operand 0 "" ""))
                           (pc)))]
  ""
  "operands[3] = gen_rtx (NE, pj_cmp_mode);
   operands[1] = pj_cmp_op0;
   operands[2] = pj_cmp_op1;")

(define_expand "bgt"
  [(set (pc) (if_then_else (match_op_dup 3  [(match_dup 1) (match_dup 2)])
                           (label_ref (match_operand 0 "" ""))
                           (pc)))]
  ""
  "operands[3] = gen_rtx (GT, pj_cmp_mode);
   operands[1] = pj_cmp_op0;
   operands[2] = pj_cmp_op1;")

(define_expand "blt"
  [(set (pc) (if_then_else (match_op_dup 3  [(match_dup 1) (match_dup 2)])
                           (label_ref (match_operand 0 "" ""))
                           (pc)))]
  ""
  "operands[3] = gen_rtx (LT, pj_cmp_mode);
   operands[1] = pj_cmp_op0;
   operands[2] = pj_cmp_op1;")

(define_expand "bge"
  [(set (pc) (if_then_else (match_op_dup 3  [(match_dup 1) (match_dup 2)])
                           (label_ref (match_operand 0 "" ""))
                           (pc)))]
  ""
  "operands[3] = gen_rtx (GE, pj_cmp_mode);
   operands[1] = pj_cmp_op0;
   operands[2] = pj_cmp_op1;")

(define_expand "ble"
  [(set (pc) (if_then_else (match_op_dup 3  [(match_dup 1) (match_dup 2)])
                           (label_ref (match_operand 0 "" ""))
                           (pc)))]
  ""
  "operands[3] = gen_rtx (LE, pj_cmp_mode);
   operands[1] = pj_cmp_op0;
   operands[2] = pj_cmp_op1;")

(define_expand "bgtu"
  [(set (pc) (if_then_else (match_op_dup 3  [(match_dup 1) (match_dup 2)])
                           (label_ref (match_operand 0 "" ""))
                           (pc)))]
  ""
  "operands[3] = gen_rtx (GTU, pj_cmp_mode);
   operands[1] = pj_cmp_op0;
   operands[2] = pj_cmp_op1;")

(define_expand "bltu"
  [(set (pc) (if_then_else (match_op_dup 3  [(match_dup 1) (match_dup 2)])
                           (label_ref (match_operand 0 "" ""))
                           (pc)))]
  ""
  "operands[3] = gen_rtx (LTU, pj_cmp_mode);
   operands[1] = pj_cmp_op0;
   operands[2] = pj_cmp_op1;")

(define_expand "bgeu"
  [(set (pc) (if_then_else (match_op_dup 3  [(match_dup 1) (match_dup 2)])
                           (label_ref (match_operand 0 "" ""))
                           (pc)))]
  ""
  "operands[3] = gen_rtx (GEU, pj_cmp_mode);
   operands[1] = pj_cmp_op0;
   operands[2] = pj_cmp_op1;")

(define_expand "bleu"
  [(set (pc) (if_then_else (match_op_dup 3  [(match_dup 1) (match_dup 2)])
                           (label_ref (match_operand 0 "" ""))
                           (pc)))]
  ""
  "operands[3] = gen_rtx (LEU, pj_cmp_mode);
   operands[1] = pj_cmp_op0;
   operands[2] = pj_cmp_op1;")

(define_insn "*bop"
  [(set (pc) (if_then_else (match_operand:SI 0 "pj_source_operand" "gS")
			   (label_ref (match_operand 1 "" ""))
			   (pc)))]
  ""
  "%S0%*ifne %1")

(define_insn "*rev_bop"
  [(set (pc) (if_then_else (match_operand:SI 0 "pj_source_operand" "gS")
			   (pc)
			   (label_ref (match_operand 1 "" ""))))]

  ""
  "%S0%*ifeq %1")

(define_insn "*blopsi"
  [(set (pc) 
	(if_then_else 
	 (match_operator:SI 3 "pj_signed_comparison_operator" 
			    [(match_operand:SI 0 "pj_source_operand" "gS,gS")
			     (match_operand:SI 1 "pj_source_operand" "K,gS")])
	 (label_ref (match_operand 2 "" ""))
	 (pc)))]
  ""
  "@
	%S0%*if%Y3 %2
	%S0%S1%*if_icmp%Y3 %2")

(define_insn "*rev_blopsi"
  [(set (pc) 
	(if_then_else 
	 (match_operator:SI 3 "pj_signed_comparison_operator" 
			    [(match_operand:SI 0 "pj_source_operand" "gS,gS")
			     (match_operand:SI 1 "pj_source_operand" "K,gS")])
	 (pc)
	 (label_ref (match_operand 2 "" ""))))]
  ""
  "@
	%S0%*if%Z3 %2
	%S0%S1%*if_icmp%Z3 %2")

(define_insn "*bluopsi"
  [(set (pc) 
	(if_then_else 
	 (match_operator:SI 3 "pj_unsigned_comparison_operator" 
			    [(match_operand:SI 0 "pj_source_operand" "gS")
			     (match_operand:SI 1 "pj_source_operand" "gS")])
	 (label_ref (match_operand 2 "" ""))
	 (pc)))]
  ""
  "%S0%S1%*iucmp%*if%Y3 %2")

(define_insn "*rev_bluopsi"
  [(set (pc) 
	(if_then_else 
	 (match_operator:SI 3 "pj_unsigned_comparison_operator" 
			    [(match_operand:SI 0 "pj_source_operand" "gS")
			     (match_operand:SI 1 "pj_source_operand" "gS")])
	 (pc)
	 (label_ref (match_operand 2 "" ""))))]
  ""
  "%S0%S1%*iucmp%*if%Z3 %2")

(define_insn "*blopdi"
  [(set (pc) 
	(if_then_else 
	 (match_operator:DI 3 "pj_signed_comparison_operator" 
			    [(match_operand:DI 0 "pj_source_operand" "gS")
			     (match_operand:DI 1 "pj_source_operand" "gS")])
	 (label_ref (match_operand 2 "" ""))
	 (pc)))]
  ""
  "%D0%D1%*lcmp%*if%Y3 %2")

(define_insn "*rev_blopdi"
  [(set (pc)
	(if_then_else 
	 (match_operator:DI 3 "pj_signed_comparison_operator" 
			    [(match_operand:DI 0 "pj_source_operand" "gS")
			     (match_operand:DI 1 "pj_source_operand" "gS")])
	 (pc)
	 (label_ref (match_operand 2 "" ""))))]
  ""
  "%D0%D1%*lcmp%*if%Z3 %2")

(define_insn "*bluopdi"
  [(set (pc)
	(if_then_else 
	 (match_operator:DI 3 "pj_unsigned_comparison_operator" 
			    [(match_operand:DI 0 "pj_source_operand" "gS")
			     (match_operand:DI 1 "pj_source_operand" "gS")])
	 (label_ref (match_operand 2 "" ""))
	 (pc)))]
  ""
  "%D0%D1%*ipush __pjucmpdi2%*bipush 6%*call%*if%Y3 %2")

(define_insn "*rev_bluopdi"
  [(set (pc) 
	(if_then_else 
	 (match_operator:DI 3 "pj_unsigned_comparison_operator" 
			    [(match_operand:DI 0 "pj_source_operand" "gS")
			     (match_operand:DI 1 "pj_source_operand" "gS")])
	 (pc)
	 (label_ref (match_operand 2 "" ""))))]
  ""
  "%D0%D1%*ipush __pjucmpdi2%*bipush 6%*call%*if%Z3 %2")

(define_insn "*blopsf"
  [(set (pc) 
	(if_then_else 
	 (match_operator:SF 3 "comparison_operator" 
			    [(match_operand:SF 0 "pj_source_operand" "gS")
			     (match_operand:SF 1 "pj_source_operand" "gS")])
	 (label_ref (match_operand 2 "" ""))
	 (pc)))]
  ""
  "%S0%S1%*fcmp%X3%*if%Y3 %2")

(define_insn "*rev_bluopsf"
  [(set (pc) 
	(if_then_else 
	 (match_operator:SF 3 "comparison_operator" 
			    [(match_operand:SF 0 "pj_source_operand" "gS")
			     (match_operand:SF 1 "pj_source_operand" "gS")])
	 (pc)
	 (label_ref (match_operand 2 "" ""))))]
  ""
  "%S0%S1%*fcmp%X3%*if%Z3 %2")

(define_insn "*blopdf"
  [(set (pc) 
	(if_then_else 
	 (match_operator:DF 3 "comparison_operator" 
			    [(match_operand:DF 0 "pj_source_operand" "gS")
			     (match_operand:DF 1 "pj_source_operand" "gS")])
	 (label_ref (match_operand 2 "" ""))
	 (pc)))]
  ""
  "%D0%D1%*dcmp%X3%*if%Y3 %2")

(define_insn "*rev_bluopdf"
  [(set (pc) 
	(if_then_else 
	 (match_operator:DF 3 "comparison_operator" 
			    [(match_operand:DF 0 "pj_source_operand" "gS")
			     (match_operand:DF 1 "pj_source_operand" "gS")])
	 (pc)
	 (label_ref (match_operand 2 "" ""))))]
  ""
  "%D0%D1%*dcmp%X3%*if%Z3 %2")


;; call instructions

(define_insn "pj_call"
  [(call (mem:QI (match_operand:SI 0 "pj_source_operand" "gS"))
	         (match_operand:SI 1 "immediate_operand" "i"))]
  ""
  "%C0%E1%S0%S1%*call")

(define_insn "pj_call_value"
  [(set (match_operand 0 "nonimmediate_operand" "=gD")
	(call (mem:QI (match_operand:SI 1 "pj_source_operand" "gS"))
		      (match_operand:SI 2 "immediate_operand" "i")))]

  ""
  "%C1%E2%S1%S2%*call")

(define_expand "call"
  [(call (match_operand:SI 0 "pj_source_operand" "gS")
         (match_operand:SI 1 "immediate_operand" "i"))
   (use (match_operand:SI 2 "register_operand" "r"))
   (use (match_operand:SI 3 "" ""))]
  ""
  "{
     emit_call_insn (gen_pj_call (XEXP (operands[0], 0),
		                   pj_workout_arg_words (operands[1],
	                                         operands[2])));
     DONE;
   }")

(define_expand "call_value"
  [(set (match_operand:SI 0 "nonimmediate_operand" "gS")
	(call (match_operand:SI 1 "pj_source_operand" "gS")
         (match_operand:SI 2 "immediate_operand" "i")))
   (use (match_operand:SI 3 "register_operand" "r"))
   (use (match_operand:SI 4 "" ""))]
  ""
  "{
     emit_call_insn (gen_pj_call_value (operands[0], 
	            	                 XEXP (operands[1], 0),
	                              	 pj_workout_arg_words (operands[2],
	                                               operands[3])));
     DONE;
   }")


;; No-op instruction.

(define_insn "nop"
  [(const_int 0)]
  ""
  "nop")


;; Jump instructions

(define_insn "jump"
  [(set (pc) (label_ref (match_operand 0 "" "")))]
  ""
  "%*goto %l0")

(define_insn "indirect_jump"
  [(set (pc) (match_operand:SI 0 "pj_source_operand" "gS"))]
  ""
  "%S0%*write_pc")

(define_insn "casesi"
  [(set (pc)
	(if_then_else
	 (leu (minus:SI (match_operand:SI 0 "pj_source_operand" "gS")
			(match_operand:SI 1 "immediate_operand" "i"))
	      (match_operand:SI 2 "immediate_operand" "i"))
	 (plus:SI (sign_extend:SI
		   (mem:SI
		    (plus:SI (pc)
			     (mult:SI (minus:SI (match_dup 0)
						(match_dup 1))
				      (const_int 4)))))
		  (label_ref (match_operand 3 "" "")))
	 (label_ref (match_operand 4 "" ""))))]
  ""
  "%S0\\n%3i:%*tableswitch\\n\\t%*.align 2%*.long %4-%3i%*.long %1%*.long %1+%2")

;; Sign-extend move instructions.

(define_insn "extendsfdf2"
  [(set (match_operand:DF 0 "nonimmediate_operand" "=gD")
	 (float_extend:DF (match_operand:SF 1 "pj_source_operand" "gS")))]
  ""
  "%S1%*f2d%R0")

(define_insn "extendhisi2"
  [(set (match_operand:SI 0 "nonimmediate_operand" "=gD")
        (sign_extend:SI (match_operand:HI 1 "pj_source_operand" "gS")))]
  ""
  "%S1%*bipush 16%*ishl%*bipush 16%*ishr%R0")

(define_insn "extendqisi2"
  [(set (match_operand:SI 0 "nonimmediate_operand" "=gD")
        (sign_extend:SI (match_operand:QI 1 "pj_source_operand" "gS")))]
  ""
  "%S1%*bipush 24%*ishl%*bipush 24%*ishr%R0")

(define_insn "extendqihi2"
  [(set (match_operand:HI 0 "nonimmediate_operand" "=gD")
        (sign_extend:HI (match_operand:QI 1 "pj_source_operand" "gS")))]
  ""
  "%S1%*bipush 24%*ishl%*bipush 24%*ishr%R0")

(define_insn "extendsidi2"
  [(set (match_operand:DI 0 "nonimmediate_operand" "=gD")
        (sign_extend:DI (match_operand:SI 1 "pj_source_operand" "gS")))]
  ""
  "%S1%*i2l%R0")


;; non local control flow.

(define_expand "save_stack_nonlocal"
  [(set (match_operand 0 "nonimmediate_operand" "=gD")
	(match_operand 1 "pj_source_operand" "gS"))]
  ""
  "{
     rtx reg = force_reg (Pmode, XEXP (operands[0], 0));
     rtx addr0 = gen_rtx_MEM (SImode,reg);
     rtx addr1 = gen_rtx_MEM (SImode, gen_rtx_PRE_INC (SImode, reg)); 
     rtx addr2 = gen_rtx_MEM (SImode, gen_rtx_PRE_INC (SImode, reg));

     emit_move_insn (addr0, gen_rtx_REG (SImode, 64));
     emit_move_insn (addr1, gen_rtx_REG (SImode, 72));
     emit_move_insn (addr2, gen_rtx_REG (SImode, 73));

     DONE;
   }")

(define_insn "restore_stack_nonlocal_helper"
  [(set (reg:SI 64) (mem:SI (match_operand:SI 0 "register_operand" "r")))
   (set (reg:SI 72) (mem:SI (pre_inc:SI (match_dup 0))))
   (set (reg:SI 73) (mem:SI (pre_inc:SI (match_dup 0))))]
  ""
  "%S0%*load_word%*write_global0%*iinc %J0,4%S0%*load_word%*iinc %J0,4%S0%*load_word%*write_vars%*write_optop")

(define_expand "restore_stack_nonlocal"
  [(set (match_operand 0 "nonimmediate_operand" "=gD")
	(match_operand 1 "pj_source_operand" "gS"))]
  ""
  "{
    rtx reg = force_reg (Pmode, XEXP (operands[1], 0));
    emit_insn (gen_restore_stack_nonlocal_helper (reg));
    DONE;
  }")

(define_insn "nonlocal_goto_helper"
  [(set (reg:SI 64) (mem:SI (match_operand:SI 0 "register_operand" "r")))
   (set (reg:SI 72) (mem:SI (pre_inc:SI (match_dup 0))))
   (set (reg:SI 73) (mem:SI (pre_inc:SI (match_dup 0))))
   (set (pc) (match_operand:SI 1 "pj_source_operand" "gS"))]
  ""
  "%S0%*load_word%*write_global0%*iinc %J0,4%*%S0%*load_word%*%S1%*iinc %J0,4%*%S0%*load_word%*iinc %J0,4%*write_vars%*return0")

(define_expand "nonlocal_goto"
  [(match_operand:SI 0 "pj_source_operand" "")
   (match_operand:SI 1 "pj_source_operand" "")
   (match_operand:SI 2 "pj_source_operand" "")
   (match_operand:SI 3 "" "")]
  ""
  "{ 
     rtx addr;
     rtx temp;
     emit_move_insn (hard_frame_pointer_rtx, operands[0]);

     temp = copy_to_reg (replace_rtx (operands[1], 
	                              virtual_stack_vars_rtx,
         		  	      hard_frame_pointer_rtx));
	  
     addr = replace_rtx (copy_rtx (operands[2]),
  	 		 virtual_stack_vars_rtx,
 		 	 hard_frame_pointer_rtx);

     emit_insn (gen_rtx_USE (VOIDmode, stack_pointer_rtx));
     emit_insn (gen_nonlocal_goto_helper (force_reg (Pmode, XEXP (addr, 0)),
		                          temp));
     emit_barrier ();
     DONE;
    }")

;; Function overhead.

(define_expand "prologue"
  [(const_int 0)]
  ""
  "pj_expand_prologue (); DONE;")

(define_expand "epilogue"
  [(return)]
  ""
  "pj_expand_epilogue();")

(define_insn "return"
  [(return)]
  "reload_completed"
  "%*return0")

(define_insn "tm_frame"
  [(use  (match_operand:SI 0 "pj_source_operand" "gS"))
   (set (reg:SI 73) 
	(minus:SI (reg:SI 73) 
		  (mult:SI (match_operand:SI 1 "pj_source_operand" "gS")
			   (const_int 4))))]

  ""
  "%S0%S1%*tm_frame")

(define_insn "varargs"
  [(unspec_volatile [(match_operand:SI 0 "pj_source_operand" "gS")] 10)]
  ""
  "%S0%*jsr_w __vhelper")

(define_insn "varargs_finish"
  [(unspec_volatile [(match_operand:SI 0 "pj_source_operand" "gS")] 11)]
  ""
  "%*iload %J0%*write_global0")

;; Extensions to picoJava.

(define_insn "strlensi"
  [(set (match_operand:SI 0 "nonimmediate_operand" "=g")
	(unspec:SI [(match_operand:BLK 1 "memory_operand" "gS")
		    (match_operand:QI 2 "pj_source_operand" "gS")
		    (match_operand:SI 3 "immediate_operand" "gS")] 0))
   (clobber (reg:SI 65))]
  "TARGET_TM_EXTENSIONS"
  "%I1%S2%S3%*iconst_0%*write_global1%*tm_strlensi%R0")

(define_insn "movstrsi"
  [(set (match_operand:BLK 0 "memory_operand" "=gS")
	(match_operand:BLK 1 "memory_operand" "gS"))
   (use (match_operand:SI 2 "pj_source_operand" "gS"))
   (use (match_operand:SI 3 "pj_source_operand" "gS"))
   (clobber (reg:SI 65))]
  "TARGET_TM_EXTENSIONS"
  "%I0%I1%S2%S3%*iconst_0%*write_global1%*tm_movstrsi")

(define_insn "clrstrsi"
  [(set (match_operand:BLK 0 "memory_operand" "=gS")
	(const_int 0))
   (use (match_operand:SI 1 "pj_source_operand" "gS"))
   (use (match_operand:SI 2 "pj_source_operand" "gS"))
   (clobber (reg:SI 65))]
  "TARGET_TM_EXTENSIONS"
  "%I0%*iconst_0%S1%S2%*iconst_0%*write_global1%*tm_memsetsi")

(define_insn "cmpstrsi"
  [(set (match_operand:SI 0 "nonimmediate_operand" "=gD")
	(compare:SI (match_operand:BLK 1 "memory_operand" "g")
		    (match_operand:BLK 2 "memory_operand" "g")))
   (use (match_operand:SI 3 "pj_source_operand" "gS"))
   (use (match_operand:SI 4 "pj_source_operand" "gS"))
   (clobber (reg:SI 65))]
  "TARGET_TM_EXTENSIONS"
  "%I1%I2%S3%S4%*iconst_0%*write_global1%*tm_cmpstrsi%R0")
