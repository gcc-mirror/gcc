;;- Machine description for GNU compiler, Convex Version
;;  Copyright (C) 1988, 1994 Free Software Foundation, Inc.

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

;; Attribute specifications

; Target CPU
(define_attr "cpu" "c1,c32,c34,c38"
  (const (symbol_ref "(enum attr_cpu) target_cpu")))

;; Instruction classification

(define_attr "type"
  "alu,xalu,mldw,mldl,mldb,mst,adds,addd,mulw,mull,muls,muld,divw,divl,divs,divd,shfw,shfl,cvts,cvtd"
  (const_string "alu"))

;; Instruction times

(define_function_unit "mem" 1 0
  (and (eq_attr "cpu" "c1") (eq_attr "type" "mldw")) 2 0)
(define_function_unit "mem" 1 0
  (and (eq_attr "cpu" "c1") (eq_attr "type" "mldl")) 4 0)
(define_function_unit "mem" 1 0
  (and (eq_attr "cpu" "c32") (eq_attr "type" "mldw,mldl")) 2 0)
(define_function_unit "mem" 1 0
  (and (eq_attr "cpu" "c34") (eq_attr "type" "mldw,mldl")) 4 0)
(define_function_unit "mem" 1 0
  (and (eq_attr "cpu" "c38") (eq_attr "type" "mldw,mldl")) 2 0)

(define_function_unit "mem" 1 0
  (and (eq_attr "cpu" "c32") (eq_attr "type" "mldb")) 9 0)
(define_function_unit "mem" 1 0
  (and (eq_attr "cpu" "c34") (eq_attr "type" "mldb")) 36 0)
(define_function_unit "mem" 1 0
  (and (eq_attr "cpu" "c38") (eq_attr "type" "mldb")) 21 0)

(define_function_unit "mem" 1 0
  (and (eq_attr "cpu" "c1") (eq_attr "type" "xalu")) 1 0)
(define_function_unit "mem" 1 0
  (and (eq_attr "cpu" "c32") (eq_attr "type" "xalu")) 1 0)
(define_function_unit "mem" 1 0
  (and (eq_attr "cpu" "c34") (eq_attr "type" "xalu")) 5 0)
(define_function_unit "mem" 1 0
  (and (eq_attr "cpu" "c38") (eq_attr "type" "xalu")) 2 0)

(define_function_unit "add" 1 0
  (and (eq_attr "cpu" "c1") (eq_attr "type" "adds,addd")) 3 2)
(define_function_unit "add" 1 0
  (and (eq_attr "cpu" "c32") (eq_attr "type" "adds,addd")) 2 1)
(define_function_unit "add" 1 0
  (and (eq_attr "cpu" "c34") (eq_attr "type" "adds,addd")) 5 2)
(define_function_unit "add" 1 0
  (and (eq_attr "cpu" "c38") (eq_attr "type" "adds,addd")) 2 1)

(define_function_unit "mul" 1 0
  (and (eq_attr "cpu" "c1") (eq_attr "type" "mulw,muls")) 3 2)
(define_function_unit "mul" 1 0
  (and (eq_attr "cpu" "c32") (eq_attr "type" "mulw,muls")) 4 2)
(define_function_unit "mul" 1 0
  (and (eq_attr "cpu" "c34") (eq_attr "type" "mulw,muls")) 6 2)
(define_function_unit "mul" 1 0
  (and (eq_attr "cpu" "c38") (eq_attr "type" "mulw,muls")) 3 2)

(define_function_unit "mul" 1 0
  (and (eq_attr "cpu" "c1") (eq_attr "type" "mull,muld")) 4 3)
(define_function_unit "mul" 1 0
  (and (eq_attr "cpu" "c32") (eq_attr "type" "mull")) 10 7)
(define_function_unit "mul" 1 0
  (and (eq_attr "cpu" "c32") (eq_attr "type" "muld")) 5 2)
(define_function_unit "mul" 1 0
  (and (eq_attr "cpu" "c34") (eq_attr "type" "mull,muld")) 7 3)
(define_function_unit "mul" 1 0
  (and (eq_attr "cpu" "c38") (eq_attr "type" "mull,muld")) 4 3)

(define_function_unit "div" 1 0
  (and (eq_attr "cpu" "c1") (eq_attr "type" "divw")) 24 24)
(define_function_unit "div" 1 0
  (and (eq_attr "cpu" "c32") (eq_attr "type" "divw")) 44 6)
(define_function_unit "div" 1 0
  (and (eq_attr "cpu" "c34") (eq_attr "type" "divw")) 14 10)
(define_function_unit "div" 1 0
  (and (eq_attr "cpu" "c38") (eq_attr "type" "divw")) 11 10)

(define_function_unit "div" 1 0
  (and (eq_attr "cpu" "c1") (eq_attr "type" "divl")) 41 42)
(define_function_unit "div" 1 0
  (and (eq_attr "cpu" "c32") (eq_attr "type" "divl")) 76 5)
(define_function_unit "div" 1 0
  (and (eq_attr "cpu" "c34") (eq_attr "type" "divl")) 22 18)
(define_function_unit "div" 1 0
  (and (eq_attr "cpu" "c38") (eq_attr "type" "divl")) 19 18)

(define_function_unit "div" 1 0
  (and (eq_attr "cpu" "c1") (eq_attr "type" "divs")) 22 22)
(define_function_unit "div" 1 0
  (and (eq_attr "cpu" "c32") (eq_attr "type" "divs")) 8 6)
(define_function_unit "div" 1 0
  (and (eq_attr "cpu" "c34") (eq_attr "type" "divs")) 13 9)
(define_function_unit "div" 1 0
  (and (eq_attr "cpu" "c38") (eq_attr "type" "divs")) 10 9)

(define_function_unit "div" 1 0
  (and (eq_attr "cpu" "c1") (eq_attr "type" "divd")) 37 38)
(define_function_unit "div" 1 0
  (and (eq_attr "cpu" "c32") (eq_attr "type" "divd")) 12 8)
(define_function_unit "div" 1 0
  (and (eq_attr "cpu" "c34") (eq_attr "type" "divd")) 20 16)
(define_function_unit "div" 1 0
  (and (eq_attr "cpu" "c38") (eq_attr "type" "divd")) 17 16)

(define_function_unit "misc" 1 0
  (and (eq_attr "cpu" "c1") (eq_attr "type" "cvts,cvtd")) 4 3)
(define_function_unit "misc" 1 0
  (and (eq_attr "cpu" "c32") (eq_attr "type" "cvts")) 9 7)
(define_function_unit "misc" 1 0
  (and (eq_attr "cpu" "c32") (eq_attr "type" "cvtd")) 9 6)
(define_function_unit "misc" 1 0
  (and (eq_attr "cpu" "c34") (eq_attr "type" "cvts")) 6 2)
(define_function_unit "misc" 1 0
  (and (eq_attr "cpu" "c34") (eq_attr "type" "cvtd")) 6 1)
(define_function_unit "misc" 1 0
  (and (eq_attr "cpu" "c38") (eq_attr "type" "cvts,cvtd")) 3 1)

(define_function_unit "misc" 1 0
  (and (eq_attr "cpu" "c1") (eq_attr "type" "shfw,shfl")) 3 2)
(define_function_unit "misc" 1 0
  (and (eq_attr "cpu" "c32") (eq_attr "type" "shfw")) 7 5)
(define_function_unit "misc" 1 0
  (and (eq_attr "cpu" "c32") (eq_attr "type" "shfl")) 7 4)
(define_function_unit "misc" 1 0
  (and (eq_attr "cpu" "c38") (eq_attr "type" "shfw,shfl")) 3 1)

(define_function_unit "mystery_latch" 1 1
  (and (eq_attr "type" "!alu,mldw,mldl,adds,addd") (eq_attr "cpu" "c32")) 2 2)

;(define_function_unit "ip" 1 1
;  (and (eq_attr "cpu" "c1")
;       (eq_attr "type" "divw,divl,divs,divd,xalu")) 2 2)
;(define_function_unit "ip" 1 1
;  (and (eq_attr "cpu" "c1")
;       (eq_attr "type" "!divw,divl,divs,divd,xalu")) 1 1)
;(define_function_unit "ip" 1 1
;  (and (eq_attr "cpu" "c32")
;       (eq_attr "type" "mull,muld,divl,divd,shfl,cvtd,xalu")) 2 2)
;(define_function_unit "ip" 1 1
;  (and (eq_attr "cpu" "c32")
;       (eq_attr "type" "!mull,muld,divl,divd,shfl,cvtd,xalu")) 1 1)
;(define_function_unit "ip" 1 1
;  (and (eq_attr "cpu" "c34")
;       (eq_attr "type" "addd,mull,muld,divl,divd,cvtd,xalu")) 2 2)
;(define_function_unit "ip" 1 1
;  (and (eq_attr "cpu" "c34")
;       (eq_attr "type" "!addd,mull,muld,divl,divd,cvtd,xalu")) 1 1)

;; Make the first thing a real insn in case of genattrtab bug

(define_insn "nop"
  [(const_int 0)]
  ""
  "nop")

;; Moves

(define_expand "movdf"
  [(set (match_operand:DF 0 "general_operand" "")
	(match_operand:DF 1 "general_operand" ""))]
  ""
  "if (GET_CODE (operands[0]) != REG)
     operands[1] = force_reg (DFmode, operands[1]);")

(define_insn ""
  [(set (match_operand:DF 0 "general_operand" "=d,d,d,d,d,<,m")
	(match_operand:DF 1 "general_operand"  "d,Q,m,G,H,d,d"))]
  "register_operand (operands[0], DFmode)
   || register_operand (operands[1], DFmode)"
  "@
   mov %1,%0
   ldb.d %1,%0
   ld.d %1,%0
   ld.d %u1,%0
   ld.l %v1,%0
   psh.l %1
   st.d %1,%0"
  [(set_attr "type" "alu,mldb,mldl,alu,alu,alu,mst")])

;; This is here so we can load any result of RTL constant folding
;; but do not use it on constants that can be loaded from memory.
;; It is never better and can be worse.

(define_insn ""
  [(set (match_operand:DF 0 "register_operand" "=d")
	(match_operand:DF 1 "const_double_operand" "F"))]
  "CONST_DOUBLE_MEM (operands[1]) == const0_rtx"
  "ld.u %u1,%0\;ld.w %v1,%0"
  [(set_attr "type" "xalu")])

(define_expand "movsf"
  [(set (match_operand:SF 0 "general_operand" "")
	(match_operand:SF 1 "general_operand" ""))]
  ""
  "if (GET_CODE (operands[0]) != REG)
     operands[1] = force_reg (SFmode, operands[1]);")

(define_insn ""
  [(set (match_operand:SF 0 "general_operand" "=d,d,d,d,<,m")
	(match_operand:SF 1 "general_operand" "d,Q,m,F,d,d"))]
  "register_operand (operands[0], SFmode)
   || register_operand (operands[1], SFmode)"
  "@
   mov.s %1,%0
   ldb.s %1,%0
   ld.s %1,%0
   ld.s %1,%0
   psh.w %1
   st.s %1,%0"
  [(set_attr "type" "alu,mldb,mldw,alu,alu,mst")])

(define_expand "movdi"
  [(set (match_operand:DI 0 "general_operand" "")
	(match_operand:DI 1 "general_operand" ""))]
  ""
  "if (GET_CODE (operands[0]) != REG)
     operands[1] = force_reg (DImode, operands[1]);")

(define_insn ""
  [(set (match_operand:DI 0 "general_operand" "=d,d,d,d,d,<,m")
	(match_operand:DI 1 "general_operand" "d,Q,m,G,HI,d,d"))]
  "register_operand (operands[0], DImode)
   || register_operand (operands[1], DImode)"
  "@
   mov %1,%0
   ldb.l %1,%0
   ld.l %1,%0
   ld.d %u1,%0
   ld.l %1,%0
   psh.l %1
   st.l %1,%0"
  [(set_attr "type" "alu,mldb,mldl,alu,alu,alu,mst")])

;; This is here so we can load any result of RTL constant folding
;; but do not use it on constants that can be loaded from memory.
;; It is never better and can be worse.

(define_insn ""
  [(set (match_operand:DI 0 "register_operand" "=d")
	(match_operand:DI 1 "const_double_operand" "F"))]
  "CONST_DOUBLE_MEM (operands[1]) == const0_rtx"
  "ld.u %u1,%0\;ld.w %v1,%0"
  [(set_attr "type" "xalu")])

(define_expand "movsi"
  [(set (match_operand:SI 0 "general_operand" "")
	(match_operand:SI 1 "general_operand" ""))]
  ""
  "if (GET_CODE (operands[0]) != REG)
     operands[1] = force_reg (SImode, operands[1]);")

(define_insn ""
  [(set (match_operand:SI 0 "push_operand" "=<,<")
	(match_operand:SI 1 "nonmemory_operand" "Ad,i"))]
  ""
  "@
   psh.w %1
   pshea %a1")

(define_insn ""
  [(set (match_operand:SI 0 "general_operand" "=d,r,d,r,r,m")
	(match_operand:SI 1 "general_operand" "d,r,Q,m,i,r"))]
  "register_operand (operands[0], SImode)
   || register_operand (operands[1], SImode)"
  "@
   mov.w %1,%0
   mov %1,%0
   ldb.w %1,%0
   ld.w %1,%0
   ld.w %1,%0
   st.w %1,%0"
  [(set_attr "type" "alu,alu,mldb,mldw,alu,mst")])

(define_expand "movstrictsi"
  [(set (strict_low_part (match_operand:SI 0 "general_operand" ""))
	(match_operand:SI 1 "general_operand" ""))]
  ""
  "if (GET_CODE (operands[0]) != REG)
     operands[1] = force_reg (SImode, operands[1]);")

(define_insn ""
  [(set (strict_low_part (match_operand:SI 0 "general_operand" "=d,r,d,r,r,m"))
	(match_operand:SI 1 "general_operand" "d,r,Q,m,i,r"))]
  "register_operand (operands[0], SImode)
   || register_operand (operands[1], SImode)"
  "@
   mov.w %1,%0
   mov %1,%0
   ldb.w %1,%0
   ld.w %1,%0
   ld.w %1,%0
   st.w %1,%0"
  [(set_attr "type" "alu,alu,mldb,mldw,alu,mst")])

(define_expand "movhi"
  [(set (match_operand:HI 0 "general_operand" "")
	(match_operand:HI 1 "general_operand" ""))]
  ""
  "if (GET_CODE (operands[0]) != REG)
     operands[1] = force_reg (HImode, operands[1]);")

(define_insn ""
  [(set (match_operand:HI 0 "general_operand" "=d,r,d,r,r,<,m")
	(match_operand:HI 1 "general_operand" "d,r,Q,m,i,Ad,r"))]
  "register_operand (operands[0], HImode)
   || register_operand (operands[1], HImode)"
  "@
   mov.w %1,%0
   mov %1,%0
   ldb.h %1,%0
   ld.h %1,%0
   ld.w %1,%0
   psh.w %1
   st.h %1,%0"
  [(set_attr "type" "alu,alu,mldb,mldw,alu,alu,mst")])

(define_expand "movqi"
  [(set (match_operand:QI 0 "general_operand" "")
	(match_operand:QI 1 "general_operand" ""))]
  ""
  "if (GET_CODE (operands[0]) != REG)
     operands[1] = force_reg (QImode, operands[1]);")

(define_insn ""
  [(set (match_operand:QI 0 "general_operand" "=d,r,d,r,r,<,m")
	(match_operand:QI 1 "general_operand" "d,r,Q,m,i,Ad,r"))]
  "register_operand (operands[0], QImode)
   || register_operand (operands[1], QImode)"
  "@
   mov.w %1,%0
   mov %1,%0
   ldb.b %1,%0
   ld.b %1,%0
   ld.w %1,%0
   psh.w %1
   st.b %1,%0"
  [(set_attr "type" "alu,alu,mldb,mldw,alu,alu,mst")])

;; Expand block moves manually to get code that pipelines the loads.

(define_expand "movstrsi"
  [(set (match_operand:BLK 0 "memory_operand" "=m")
	(match_operand:BLK 1 "memory_operand" "m"))
   (use (match_operand:SI 2 "const_int_operand" "i"))
   (use (match_operand:SI 3 "const_int_operand" "i"))]
  ""
  " expand_movstr (operands); DONE; ")

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
  "cvts.d %1,%0"
  [(set_attr "type" "cvts")])

(define_insn "truncdfsf2"
  [(set (match_operand:SF 0 "register_operand" "=d")
	(float_truncate:SF (match_operand:DF 1 "register_operand" "d")))]
  ""
  "cvtd.s %1,%0"
  [(set_attr "type" "cvtd")])

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
  "cvtw.s %1,%0"
  [(set_attr "type" "cvts")])

(define_insn "floatdisf2"
  [(set (match_operand:SF 0 "register_operand" "=d")
	(float:SF (match_operand:DI 1 "register_operand" "d")))]
  ""
  "cvtl.s %1,%0"
  [(set_attr "type" "cvtd")])

(define_insn "floatsidf2"
  [(set (match_operand:DF 0 "register_operand" "=d")
	(float:DF (match_operand:SI 1 "register_operand" "d")))]
  "! TARGET_C1"
  "cvtw.d %1,%0"
  [(set_attr "type" "cvts")])

(define_insn "floatdidf2"
  [(set (match_operand:DF 0 "register_operand" "=d")
	(float:DF (match_operand:DI 1 "register_operand" "d")))]
  ""
  "cvtl.d %1,%0"
  [(set_attr "type" "cvtd")])

;; These are a little slower than gcc's normal way of doing unsigned
;; DI floats (if the DI number is "negative") but they avoid double
;; rounding and they avoid explicit constants.

(define_expand "floatunsdidf2"
  [(set (match_operand:DF 0 "register_operand" "=d")
	(float:DF (match_operand:DI 1 "register_operand" "d")))
   (set (cc0) (compare:DI (match_dup 3) (match_dup 1)))
   (set (pc)
	(if_then_else (le (cc0) (const_int 0))
		      (label_ref (match_dup 4))
		      (pc)))
   (set (match_dup 2) (lshiftrt:DI (match_dup 1) (const_int 1)))
   (set (match_dup 0) (float:DF (match_dup 2)))
   (set (match_dup 0) (plus:DF (match_dup 0) (match_dup 0)))
   (match_dup 4)
   (set (match_dup 0) (match_dup 0))]
  ""
  "
{
  operands[2] = gen_reg_rtx (DImode);
  operands[3] = force_reg (DImode, const0_rtx);
  operands[4] = gen_label_rtx ();
}")

(define_expand "floatunsdisf2"
  [(set (match_operand:SF 0 "register_operand" "=d")
	(float:SF (match_operand:DI 1 "register_operand" "d")))
   (set (cc0) (compare:DI (match_dup 3) (match_dup 1)))
   (set (pc)
	(if_then_else (le (cc0) (const_int 0))
		      (label_ref (match_dup 4))
		      (pc)))
   (set (match_dup 2) (lshiftrt:DI (match_dup 1) (const_int 1)))
   (set (match_dup 0) (float:SF (match_dup 2)))
   (set (match_dup 0) (plus:SF (match_dup 0) (match_dup 0)))
   (match_dup 4)
   (set (match_dup 0) (match_dup 0))]
  ""
  "
{
  operands[2] = gen_reg_rtx (DImode);
  operands[3] = force_reg (DImode, const0_rtx);
  operands[4] = gen_label_rtx ();
}")

;; These patterns are identical to gcc's default action 
;; if DI->DF and DI->SF are not present.  There are here
;; only to prevent SI->*F from promoting to DI->*F.

(define_expand "floatunssidf2"
  [(set (match_dup 2)
	(zero_extend:DI (match_operand:SI 1 "register_operand" "")))
   (set (match_operand:DF 0 "register_operand" "")
	(float:DF (match_dup 2)))]
  ""
  "operands[2] = gen_reg_rtx (DImode);")

(define_expand "floatunssisf2"
  [(set (match_dup 2)
        (zero_extend:DI (match_operand:SI 1 "register_operand" "")))
   (set (match_operand:SF 0 "register_operand" "")
        (float:SF (match_dup 2)))]
  ""
  "operands[2] = gen_reg_rtx (DImode);")

;; Float-to-fix conversion insns.

(define_insn "fix_truncsfsi2"
  [(set (match_operand:SI 0 "register_operand" "=d")
	(fix:SI (fix:SF (match_operand:SF 1 "register_operand" "d"))))]
  ""
  "cvts.w %1,%0"
  [(set_attr "type" "cvts")])

(define_insn "fix_truncsfdi2"
  [(set (match_operand:DI 0 "register_operand" "=d")
	(fix:DI (fix:SF (match_operand:SF 1 "register_operand" "d"))))]
  ""
  "cvts.l %1,%0"
  [(set_attr "type" "cvts")])

(define_insn "fix_truncdfsi2"
  [(set (match_operand:SI 0 "register_operand" "=d")
	(fix:SI (fix:DF (match_operand:DF 1 "register_operand" "d"))))]
  ""
  "cvtd.l %1,%0"
  [(set_attr "type" "cvtd")])

(define_insn "fix_truncdfdi2"
  [(set (match_operand:DI 0 "register_operand" "=d")
	(fix:DI (fix:DF (match_operand:DF 1 "register_operand" "d"))))]
  ""
  "cvtd.l %1,%0"
  [(set_attr "type" "cvtd")])

;;- All kinds of add instructions.

(define_insn "adddf3"
  [(set (match_operand:DF 0 "register_operand" "=d")
	(plus:DF (match_operand:DF 1 "register_operand" "%0")
		 (match_operand:DF 2 "register_operand" "d")))]
  ""
  "add.d %2,%0"
  [(set_attr "type" "addd")])

(define_insn "addsf3"
  [(set (match_operand:SF 0 "register_operand" "=d")
	(plus:SF (match_operand:SF 1 "register_operand" "%0")
		 (match_operand:SF 2 "nonmemory_operand" "dF")))]
  ""
  "add.s %2,%0"
  [(set_attr "type" "adds")])

(define_insn "adddi3"
  [(set (match_operand:DI 0 "register_operand" "=d")
	(plus:DI (match_operand:DI 1 "register_operand" "%0")
		 (match_operand:DI 2 "register_operand" "d")))]
  ""
  "add.l %2,%0")

(define_expand "addsi3"
  [(set (match_operand:SI 0 "register_operand" "")
	(plus:SI (match_operand:SI 1 "register_operand" "")
		 (match_operand:SI 2 "nonmemory_operand" "")))]
  ""
  "")

(define_insn ""
  [(set (match_operand:SI 0 "register_operand" "=a")
	(plus:SI (match_operand:SI 1 "register_operand" "%A")
		 (match_operand:SI 2 "immediate_operand" "i")))]
  "operands[1] == frame_pointer_rtx || operands[1] == arg_pointer_rtx"
  "ldea %a2(%1),%0")

(define_insn ""
  [(set (match_operand:SI 0 "register_operand" "=a")
	(plus:SI (match_operand:SI 1 "register_operand" "%a")
		 (match_operand:SI 2 "nonmemory_operand" "ri")))]
  "operands[1] == stack_pointer_rtx && operands[0] != stack_pointer_rtx"
  "mov %1,%0\;add.w %2,%0")

(define_insn ""
  [(set (match_operand:SI 0 "push_operand" "=<")
	(plus:SI (match_operand:SI 1 "register_operand" "A")
		 (match_operand:SI 2 "immediate_operand" "i")))]
  "operands[1] != stack_pointer_rtx"
  "pshea %a2(%1)"
  [(set_attr "type" "mst")])

(define_insn ""
  [(set (match_operand:SI 0 "register_operand" "=d,a,a")
	(plus:SI (match_operand:SI 1 "register_operand" "%0,0,A")
		 (match_operand:SI 2 "nonmemory_operand" "di,ri,i")))]
  "TARGET_C1"
  "@
   add.w %2,%0
   add.w %2,%0
   ldea %a2(%1),%0")

(define_insn ""
  [(set (match_operand:SI 0 "register_operand" "=d,a,r")
	(plus:SI (match_operand:SI 1 "register_operand" "%0,0,A")
		 (match_operand:SI 2 "nonmemory_operand" "di,ri,i")))]
  ""
  "@
   add.w %2,%0
   add.w %2,%0
   ldea %a2(%1),%0")

(define_insn "addhi3"
  [(set (match_operand:HI 0 "register_operand" "=d,a")
	(plus:HI (match_operand:HI 1 "register_operand" "%0,0")
		 (match_operand:HI 2 "nonmemory_operand" "di,ai")))]
  ""
  "add.h %2,%0")

(define_insn "addqi3"
  [(set (match_operand:QI 0 "register_operand" "=d,d")
	(plus:QI (match_operand:QI 1 "register_operand" "%0,0")
		 (match_operand:QI 2 "nonmemory_operand" "d,i")))]
  ""
  "@
   add.b %2,%0
   add.w %2,%0")

;;- All kinds of subtract instructions.

(define_insn "subdf3"
  [(set (match_operand:DF 0 "register_operand" "=d")
	(minus:DF (match_operand:DF 1 "register_operand" "0")
		  (match_operand:DF 2 "register_operand" "d")))]
  ""
  "sub.d %2,%0"
  [(set_attr "type" "addd")])

(define_insn "subsf3"
  [(set (match_operand:SF 0 "register_operand" "=d")
	(minus:SF (match_operand:SF 1 "register_operand" "0")
		  (match_operand:SF 2 "nonmemory_operand" "dF")))]
  ""
  "sub.s %2,%0"
  [(set_attr "type" "adds")])

(define_insn "subdi3"
  [(set (match_operand:DI 0 "register_operand" "=d")
	(minus:DI (match_operand:DI 1 "register_operand" "0")
		  (match_operand:DI 2 "register_operand" "d")))]
  ""
  "sub.l %2,%0")

(define_insn "subsi3"
  [(set (match_operand:SI 0 "register_operand" "=d,a,?d,?a")
	(minus:SI (match_operand:SI 1 "nonmemory_operand" "0,0,di,ai")
		  (match_operand:SI 2 "nonmemory_operand" "di,ai,0,0")))]
  ""
  "@
  sub.w %2,%0
  sub.w %2,%0
  sub.w %1,%0\;neg.w %0,%0
  sub.w %1,%0\;neg.w %0,%0")

(define_insn "subhi3"
  [(set (match_operand:HI 0 "register_operand" "=d,a")
	(minus:HI (match_operand:HI 1 "register_operand" "0,0")
		  (match_operand:HI 2 "nonmemory_operand" "di,ai")))]
  ""
  "sub.h %2,%0")

(define_insn "subqi3"
  [(set (match_operand:QI 0 "register_operand" "=d,d")
	(minus:QI (match_operand:QI 1 "register_operand" "0,0")
		  (match_operand:QI 2 "nonmemory_operand" "d,i")))]
  ""
  "@
   sub.b %2,%0
   sub.w %2,%0")

;;- Multiply instructions.

(define_insn "muldf3"
  [(set (match_operand:DF 0 "register_operand" "=d")
	(mult:DF (match_operand:DF 1 "register_operand" "%0")
		 (match_operand:DF 2 "register_operand" "d")))]
  ""
  "mul.d %2,%0"
  [(set_attr "type" "muld")])

(define_insn "mulsf3"
  [(set (match_operand:SF 0 "register_operand" "=d")
	(mult:SF (match_operand:SF 1 "register_operand" "%0")
		 (match_operand:SF 2 "nonmemory_operand" "dF")))]
  ""
  "mul.s %2,%0"
  [(set_attr "type" "muls")])

(define_insn "muldi3"
  [(set (match_operand:DI 0 "register_operand" "=d")
	(mult:DI (match_operand:DI 1 "register_operand" "%0")
		 (match_operand:DI 2 "register_operand" "d")))]
  ""
  "mul.l %2,%0"
  [(set_attr "type" "mull")])

(define_insn "mulsi3"
  [(set (match_operand:SI 0 "register_operand" "=d,a")
	(mult:SI (match_operand:SI 1 "register_operand" "%0,0")
		 (match_operand:SI 2 "nonmemory_operand" "di,ai")))]
  ""
  "mul.w %2,%0"
  [(set_attr "type" "mulw")])

(define_insn "mulhi3"
  [(set (match_operand:HI 0 "register_operand" "=d,a")
	(mult:HI (match_operand:HI 1 "register_operand" "%0,0")
		 (match_operand:HI 2 "nonmemory_operand" "di,ai")))]
  ""
  "mul.h %2,%0"
  [(set_attr "type" "mulw")])

(define_insn "mulqi3"
  [(set (match_operand:QI 0 "register_operand" "=d,d")
	(mult:QI (match_operand:QI 1 "register_operand" "%0,0")
		 (match_operand:QI 2 "nonmemory_operand" "d,i")))]
  ""
  "@
   mul.b %2,%0
   mul.w %2,%0"
  [(set_attr "type" "mulw,mulw")])

;;- Divide instructions.

(define_insn "divdf3"
  [(set (match_operand:DF 0 "register_operand" "=d")
	(div:DF (match_operand:DF 1 "register_operand" "0")
		(match_operand:DF 2 "register_operand" "d")))]
  ""
  "div.d %2,%0"
  [(set_attr "type" "divd")])

(define_insn "divsf3"
  [(set (match_operand:SF 0 "register_operand" "=d")
	(div:SF (match_operand:SF 1 "register_operand" "0")
		(match_operand:SF 2 "nonmemory_operand" "dF")))]
  ""
  "div.s %2,%0"
  [(set_attr "type" "divs")])

(define_insn "divdi3"
  [(set (match_operand:DI 0 "register_operand" "=d")
	(div:DI (match_operand:DI 1 "register_operand" "0")
		(match_operand:DI 2 "register_operand" "d")))]
  ""
  "div.l %2,%0"
  [(set_attr "type" "divl")])

(define_expand "udivsi3"
  [(set (match_dup 3)
	(zero_extend:DI (match_operand:SI 1 "register_operand" "")))
   (set (match_dup 4)
	(zero_extend:DI (match_operand:SI 2 "register_operand" "")))
   (set (match_dup 3)
	(div:DI (match_dup 3) (match_dup 4)))
   (set (match_operand:SI 0 "register_operand" "")
	(subreg:SI (match_dup 3) 0))]
  ""
  "operands[3] = gen_reg_rtx (DImode);
   operands[4] = gen_reg_rtx (DImode); ")

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
  "div.w %2,%0"
  [(set_attr "type" "divw")])

(define_insn "divhi3"
  [(set (match_operand:HI 0 "register_operand" "=d,a")
	(div:HI (match_operand:HI 1 "register_operand" "0,0")
		(match_operand:HI 2 "nonmemory_operand" "di,ai")))]
  ""
  "div.h %2,%0"
  [(set_attr "type" "divw")])

(define_insn "divqi3"
  [(set (match_operand:QI 0 "register_operand" "=d")
	(div:QI (match_operand:QI 1 "register_operand" "0")
		(match_operand:QI 2 "register_operand" "d")))]
  ""
  "div.b %2,%0"
  [(set_attr "type" "divw")])

;;- Bit clear instructions.

(define_insn ""
  [(set (match_operand:DI 0 "register_operand" "=d")
	(and:DI (match_operand:DI 1 "register_operand" "%0")
		(match_operand:DI 2 "" "")))]
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
		(match_operand:DI 2 "" "")))]
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
		(match_operand:DI 2 "" "")))]
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
  "neg.d %1,%0"
  [(set_attr "type" "addd")])

(define_insn "negsf2"
  [(set (match_operand:SF 0 "register_operand" "=d")
	(neg:SF (match_operand:SF 1 "register_operand" "d")))]
  ""
  "neg.s %1,%0"
  [(set_attr "type" "adds")])

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

;;- Shifts
;;
;; The extreme profusion of patterns here is due to the different-speed
;; shifts on different machines, and the C1's lack of word shift S-register
;; instructions.

;; SImode

;; Arithmetic left 1, 1 cycle on all machines via add

(define_insn ""
  [(set (match_operand:SI 0 "register_operand" "=r")
	(ashift:SI (match_operand:SI 1 "register_operand" "0")
		   (const_int 1)))]
  ""
  "add.w %0,%0")

;; C34 general shift is 1 cycle

(define_insn ""
  [(set (match_operand:SI 0 "register_operand" "=d,a")
	(ashift:SI (match_operand:SI 1 "register_operand" "0,0")
		   (match_operand:SI 2 "nonmemory_operand" "di,ai")))]
  "TARGET_C34"
  "@
   shf.w %2,%0
   shf %2,%0"
  [(set_attr "type" "shfw,shfw")])

;; else shift left 0..7 is 1 cycle if we use an A register

(define_insn ""
  [(set (match_operand:SI 0 "register_operand" "=a,?d")
	(ashift:SI (match_operand:SI 1 "register_operand" "0,0")
		   (match_operand:SI 2 "immediate_operand" "ai,di")))]
  "TARGET_C1 && INTVAL (operands[2]) < (unsigned) 8"
  "@
   shf %2,%0
   shf %2,%0"
  [(set_attr "type" "alu,shfl")])

(define_insn ""
  [(set (match_operand:SI 0 "register_operand" "=a,?d")
	(ashift:SI (match_operand:SI 1 "register_operand" "0,0")
		   (match_operand:SI 2 "immediate_operand" "ai,di")))]
  "INTVAL (operands[2]) < (unsigned) 8"
  "@
   shf %2,%0
   shf.w %2,%0"
  [(set_attr "type" "alu,shfw")])

;; else general left shift

(define_insn ""
  [(set (match_operand:SI 0 "register_operand" "=d,a")
	(ashift:SI (match_operand:SI 1 "register_operand" "0,0")
		   (match_operand:SI 2 "nonmemory_operand" "di,ai")))]
  "TARGET_C1"
  "@
   shf %2,%0
   shf %2,%0"
  [(set_attr "type" "shfl,shfw")])

;; but C2 left shift by a constant is faster via multiply

(define_insn ""
  [(set (match_operand:SI 0 "register_operand" "=r")
	(ashift:SI (match_operand:SI 1 "register_operand" "0")
		   (match_operand:SI 2 "const_int_operand" "i")))]
  "TARGET_C2 && INTVAL (operands[2]) < (unsigned) 32"
  "mul.w %z2,%0"
  [(set_attr "type" "mulw")])

(define_insn "ashlsi3"
  [(set (match_operand:SI 0 "register_operand" "=d,a")
	(ashift:SI (match_operand:SI 1 "register_operand" "0,0")
		   (match_operand:SI 2 "nonmemory_operand" "di,ai")))]
  ""
  "@
   shf.w %2,%0
   shf %2,%0"
  [(set_attr "type" "shfw,shfw")])

;; Logical right, general
;; The hardware wants the negative of the shift count

(define_expand "lshrsi3"
  [(set (match_operand:SI 0 "register_operand" "")
	(lshiftrt:SI (match_operand:SI 1 "register_operand" "")
		     (neg:SI (match_operand:SI 2 "nonmemory_operand" ""))))]
  ""
  "operands[2] = negate_rtx (SImode, operands[2]);")

;; C1 lacks word shift S reg

(define_insn ""
  [(set
    (match_operand:SI 0 "register_operand" "=a,?d")
    (lshiftrt:SI (match_operand:SI 1 "register_operand" "0,0")
		 (neg:SI (match_operand:SI 2 "nonmemory_operand" "ai,di"))))]
  "TARGET_C1"
  "@
   shf %2,%0
   ld.u #0,%0\;shf %2,%0"
  [(set_attr "type" "shfw,shfl")])

;; general case

(define_insn ""
  [(set
    (match_operand:SI 0 "register_operand" "=d,a")
    (lshiftrt:SI (match_operand:SI 1 "register_operand" "0,0")
		 (neg:SI (match_operand:SI 2 "nonmemory_operand" "di,ai"))))]
  ""
  "@
   shf.w %2,%0
   shf %2,%0"
  [(set_attr "type" "shfw,shfw")])

;; Patterns without neg produced by constant folding

(define_insn ""
  [(set
    (match_operand:SI 0 "register_operand" "=a,?d")
    (lshiftrt:SI (match_operand:SI 1 "register_operand" "0,0")
		 (match_operand:SI 2 "immediate_operand" "i,i")))]
  "TARGET_C1"
  "@
   shf #%n2,%0
   ld.u #0,%0\;shf #%n2,%0"
  [(set_attr "type" "shfw,shfl")])

(define_insn ""
  [(set
    (match_operand:SI 0 "register_operand" "=d,a")
    (lshiftrt:SI (match_operand:SI 1 "register_operand" "0,0")
		 (match_operand:SI 2 "immediate_operand" "i,i")))]
  ""
  "@
   shf.w #%n2,%0
   shf #%n2,%0"
  [(set_attr "type" "shfw,shfw")])

;; Arithmetic right, general
;; Sign-extend to 64 bits, then shift that.  Works for 0..32.

(define_expand "ashrsi3"
  [(set (match_operand:SI 0 "register_operand" "")
	(ashiftrt:SI (match_operand:SI 1 "register_operand" "")
		     (neg:SI (match_operand:SI 2 "nonmemory_operand" ""))))]
  ""
  "operands[2] = negate_rtx (SImode, operands[2]);")

(define_insn ""
  [(set (match_operand:SI 0 "register_operand" "=d,&d")
	(ashiftrt:SI (match_operand:SI 1 "register_operand" "0,d")
		     (neg:SI
		      (match_operand:SI 2 "nonmemory_operand" "di,di"))))]
  ""
  "cvtw.l %1,%0\;shf %2,%0"
  [(set_attr "type" "shfl,shfl")])

(define_insn ""
  [(set (match_operand:SI 0 "register_operand" "=d")
	(ashiftrt:SI (match_operand:SI 1 "register_operand" "d")
		     (match_operand:SI 2 "immediate_operand" "i")))]
  ""
  "cvtw.l %1,%0\;shf #%n2,%0"
  [(set_attr "type" "shfl")])

;; DImode
;; Arithmetic left, 1-cycle

(define_insn ""
  [(set (match_operand:DI 0 "register_operand" "=d")
	(ashift:DI (match_operand:DI 1 "register_operand" "0")
		   (const_int 1)))]
  ""
  "add.l %0,%0")

;; Arithmetic left, general

(define_insn "ashldi3"
  [(set (match_operand:DI 0 "register_operand" "=d")
	(ashift:DI (match_operand:DI 1 "register_operand" "0")
		   (match_operand:SI 2 "nonmemory_operand" "di")))]
  ""
  "shf %2,%0"
  [(set_attr "type" "shfl")])

;; Can omit zero- or sign-extend if shift is 32 or more.

(define_insn ""
  [(set (match_operand:DI 0 "register_operand" "=d")
	(ashift:DI (zero_extend:DI (match_operand:SI 1 "register_operand" "0"))
		   (match_operand:SI 2 "const_int_operand" "i")))]
  "INTVAL (operands[2]) >= 32"
  "shf %2,%0"
  [(set_attr "type" "shfl")])

(define_insn ""
  [(set (match_operand:DI 0 "register_operand" "=d")
	(ashift:DI (sign_extend:DI (match_operand:SI 1 "register_operand" "0"))
		   (match_operand:SI 2 "const_int_operand" "i")))]
  "INTVAL (operands[2]) >= 32"
  "shf %2,%0"
  [(set_attr "type" "shfl")])

;; Logical right, general

(define_expand "lshrdi3"
  [(set (match_operand:DI 0 "register_operand" "")
	(lshiftrt:DI (match_operand:DI 1 "register_operand" "")
		     (neg:SI (match_operand:SI 2 "nonmemory_operand" ""))))]
  ""
  "operands[2] = negate_rtx (SImode, operands[2]);")

(define_insn ""
  [(set (match_operand:DI 0 "register_operand" "=d")
	(lshiftrt:DI (match_operand:DI 1 "register_operand" "0")
		     (neg:SI (match_operand:SI 2 "nonmemory_operand" "di"))))]
  ""
  "shf %2,%0"
  [(set_attr "type" "shfl")])

(define_insn ""
  [(set (match_operand:DI 0 "register_operand" "=d")
	(lshiftrt:DI (match_operand:DI 1 "register_operand" "0")
		     (match_operand:SI 2 "immediate_operand" "i")))]
  ""
  "shf #%n2,%0"
  [(set_attr "type" "shfl")])

;; Arithmetic right, general
;; Use
;;     ((a >> b) ^ signbit) - signbit
;; where signbit is (1 << 63) >> b
;; Works for 0..63.  Does not work for 64; unfortunate but legal.

(define_expand "ashrdi3"
  [(set (match_operand:DI 0 "register_operand" "")
	(lshiftrt:DI (match_operand:DI 1 "register_operand" "")
		     (neg:SI (match_operand:SI 2 "nonmemory_operand" ""))))
   (set (match_dup 3) (lshiftrt:DI (match_dup 3) (neg:SI (match_dup 2))))
   (set (match_dup 0) (xor:DI (match_dup 0) (match_dup 3)))
   (set (match_dup 0) (minus:DI (match_dup 0) (match_dup 3)))]
  ""
  "
{
  if (GET_CODE (operands[2]) == CONST_INT)
    switch (INTVAL (operands[2]))
      {
      case 32:
	emit_insn (gen_ashrdi3_32 (operands[0], operands[1]));
	DONE;
      }

  operands[2] = negate_rtx (SImode, operands[2]);
  operands[3] = force_reg (DImode, immed_double_const (0, 1 << 31, DImode));
}")

;; Arithmetic right 32, a common case that can save a couple of insns.

(define_expand "ashrdi3_32"
  [(set (match_operand:DI 0 "register_operand" "")
	(lshiftrt:DI (match_operand:DI 1 "register_operand" "")
		     (const_int 32)))
   (set (match_dup 0)
	(sign_extend:DI (subreg:SI (match_dup 0) 0)))]
  ""
  "")

;; __builtin instructions

(define_insn "sqrtdf2"
  [(set (match_operand:DF 0 "register_operand" "=d")
	(sqrt:DF (match_operand:DF 1 "register_operand" "0")))]
  "! TARGET_C1 && flag_fast_math"
  "sqrt.d %0"
  [(set_attr "type" "divd")])

(define_insn "sqrtsf2"
  [(set (match_operand:SF 0 "register_operand" "=d")
	(sqrt:SF (match_operand:SF 1 "register_operand" "0")))]
  "! TARGET_C1 && flag_fast_math"
  "sqrt.s %0"
  [(set_attr "type" "divs")])

(define_insn "sindf2"
  [(set (match_operand:DF 0 "register_operand" "=d")
	(unspec:DF [(match_operand:DF 1 "register_operand" "0")] 1))]
  "! TARGET_C1 && flag_fast_math"
  "sin.d %0")

(define_insn "sinsf2"
  [(set (match_operand:SF 0 "register_operand" "=d")
	(unspec:SF [(match_operand:SF 1 "register_operand" "0")] 1))]
  "! TARGET_C1 && flag_fast_math"
  "sin.s %0")

(define_insn "cosdf2"
  [(set (match_operand:DF 0 "register_operand" "=d")
	(unspec:DF [(match_operand:DF 1 "register_operand" "0")] 2))]
  "! TARGET_C1 && flag_fast_math"
  "cos.d %0")

(define_insn "cossf2"
  [(set (match_operand:SF 0 "register_operand" "=d")
	(unspec:SF [(match_operand:SF 1 "register_operand" "0")] 2))]
  "! TARGET_C1 && flag_fast_math"
  "cos.s %0")

(define_insn "ftruncdf2"
  [(set (match_operand:DF 0 "register_operand" "=d")
	(fix:DF (match_operand:DF 1 "register_operand" "d")))]
  "! TARGET_C1"
  "frint.d %1,%0"
  [(set_attr "type" "cvtd")])

(define_insn "ftruncsf2"
  [(set (match_operand:SF 0 "register_operand" "=d")
	(fix:SF (match_operand:SF 1 "register_operand" "d")))]
  "! TARGET_C1"
  "frint.s %1,%0"
  [(set_attr "type" "cvts")])

(define_insn ""
  [(set (match_operand:SI 0 "register_operand" "=d")
	(minus:SI (ffs:SI (match_operand:SI 1 "register_operand" "d"))
		  (const_int 1)))]
  ""
  "tzc %1,%0\;le.w #32,%0\;jbrs.f L0%=\;ld.w #-1,%0\\nL0%=:")

(define_expand "ffssi2"
  [(set (match_operand:SI 0 "register_operand" "=d")
	(minus:SI (ffs:SI (match_operand:SI 1 "register_operand" "d"))
		  (const_int 1)))
   (set (match_dup 0)
	(plus:SI (match_dup 0)
		 (const_int 1)))]
  ""
  "")

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

;;- Compares

(define_insn "cmpdi"
  [(set (cc0)
	(compare (match_operand:DI 0 "register_operand" "d")
		 (match_operand:DI 1 "register_operand" "d")))]
  ""
  "* return output_cmp (operands[0], operands[1], 'l');")

(define_insn ""
  [(set (cc0) (match_operand:DI 0 "register_operand" "d"))
   (clobber (match_scratch:DI 1 "=d"))]
  "next_insn_tests_no_inequality (insn)"
  "* return output_cmp (operands[0], operands[1], 'L');")

(define_insn "cmpsi"
  [(set (cc0)
	(compare (match_operand:SI 0 "register_operand" "d,a")
		 (match_operand:SI 1 "nonmemory_operand" "di,ai")))]
  ""
  "* return output_cmp (operands[0], operands[1], 'w');")

(define_insn "cmphi"
  [(set (cc0)
	(compare (match_operand:HI 0 "register_operand" "d,a")
		 (match_operand:HI 1 "nonmemory_operand" "di,ai")))]
  ""
  "* return output_cmp (operands[0], operands[1], 'h');")

; cmpqi is intentionally omitted.
;
; gcc will sign-extend or zero-extend the operands to the next
; wider mode, HImode.
;
; For reg .cmp. constant, we just go with the halfword immediate
; instruction.  Perhaps the widening insn can be cse'd or combined away.
; If not, we're still as good as loading a byte constant into a register
; to do a reg-reg byte compare.
;
; The following patterns pick up cases that can use reg .cmp. reg after all.

(define_insn ""
  [(set (cc0)
	(compare
	 (sign_extend:HI (match_operand:QI 0 "register_operand" "d"))
	 (sign_extend:HI (match_operand:QI 1 "register_operand" "d"))))]
  ""
  "* return output_cmp (operands[0], operands[1], 'b');")

(define_insn ""
  [(set (cc0)
	(compare
	 (ashift:HI (subreg:HI (match_operand:QI 0 "register_operand" "d") 0)
		    (const_int 8))
	 (ashift:HI (subreg:HI (match_operand:QI 1 "register_operand" "d") 0)
		    (const_int 8))))]
  ""
  "* return output_cmp (operands[0], operands[1], 'b');")

(define_insn ""
  [(set (cc0)
	(compare (match_operand:QI 0 "register_operand" "d")
		 (match_operand:QI 1 "register_operand" "d")))]
  ""
  "* return output_cmp (operands[0], operands[1], 'b');")

(define_insn ""
  [(set (cc0) (match_operand:QI 0 "register_operand" "d"))
   (clobber (match_scratch:QI 1 "=d"))]
  "next_insn_tests_no_inequality (insn)"
  "* return output_cmp (operands[0], operands[1], 'B');")

(define_insn ""
  [(set (cc0) (subreg (match_operand:QI 0 "register_operand" "d") 0))
   (clobber (match_scratch:QI 1 "=d"))]
  "next_insn_tests_no_inequality (insn)"
  "* return output_cmp (operands[0], operands[1], 'B');")

(define_insn ""
  [(set (cc0)
	(zero_extend (subreg (match_operand:QI 0 "register_operand" "d") 0)))
   (clobber (match_scratch:QI 1 "=d"))]
  "next_insn_tests_no_inequality (insn)"
  "* return output_cmp (operands[0], operands[1], 'B');")

(define_insn "cmpdf"
  [(set (cc0)
	(compare (match_operand:DF 0 "register_operand" "d")
		 (match_operand:DF 1 "register_operand" "d")))]
  ""
  "* return output_cmp (operands[0], operands[1], 'd');")

(define_insn "cmpsf"
  [(set (cc0)
	(compare (match_operand:SF 0 "register_operand" "d")
		 (match_operand:SF 1 "nonmemory_cmpsf_operand" "dF")))]
  ""
  "* return output_cmp (operands[0], operands[1], 's');")

;; decrement-and-set-cc0 insns.
;;
;; The most important case where we can use the carry bit from an
;; arithmetic insn to eliminate a redundant compare is the decrement in
;; constructs like while (n--) and while (--n >= 0).  
;;
;; We do it with combine patterns instead of NOTICE_UPDATE_CC because
;; the decrement needs to be kept at the end of the block during scheduling.
;; 
;; These patterns must have memory alternatives because reload refuses
;; to do output reloads for an insn that sets cc0 (since it does not
;; want to clobber cc0 with its moves).  Convex moves do not clobber
;; cc0, but there is no evident way to get reload to know that.

(define_insn ""
  [(set (cc0)
	(match_operand:SI 0 "register_operand" "+r,*m"))
   (set (match_dup 0)
	(plus:SI (match_dup 0)
		 (const_int -1)))]
  "next_insn_tests_no_inequality (insn)"
  "*
{
  if (which_alternative == 0)
    {
      output_cmp (operands[0], constm1_rtx, 'W');
      return \"add.w #-1,%0\";
    }
  else
    {
      output_cmp (gen_rtx (REG, SImode, 7), constm1_rtx, 'W');
      return \"psh.w s7\;ld.w %0,s7\;add.w #-1,s7\;st.w s7,%0\;pop.w s7\";
    }
}")
     
(define_insn ""
  [(set (cc0)
	(plus:SI (match_operand:SI 0 "register_operand" "+r,*m")
		 (const_int -1)))
   (set (match_dup 0)
	(plus:SI (match_dup 0)
		 (const_int -1)))]
  "find_reg_note (next_cc0_user (insn), REG_NONNEG, 0)"
  "*
{
  if (which_alternative == 0)
    {
      output_cmp (operands[0], const0_rtx, 'W');
      return \"add.w #-1,%0\";
    }
  else
    {
      output_cmp (gen_rtx (REG, SImode, 7), const0_rtx, 'W');
      return \"psh.w s7\;ld.w %0,s7\;add.w #-1,s7\;st.w s7,%0\;pop.w s7\";
    }
}")

(define_insn ""
  [(set (cc0)
	(match_operand:HI 0 "register_operand" "+r,*m"))
   (set (match_dup 0)
	(plus:HI (match_dup 0)
		 (const_int -1)))]
  "next_insn_tests_no_inequality (insn)"
  "*
{
  if (which_alternative == 0)
    {
      output_cmp (operands[0], constm1_rtx, 'H');
      return \"add.h #-1,%0\";
    }
  else
    {
      output_cmp (gen_rtx (REG, HImode, 7), constm1_rtx, 'H');
      return \"psh.w s7\;ld.h %0,s7\;add.h #-1,s7\;st.h s7,%0\;pop.w s7\";
    }
}")
     
(define_insn ""
  [(set (cc0)
	(plus:HI (match_operand:HI 0 "register_operand" "+r,*m")
		 (const_int -1)))
   (set (match_dup 0)
	(plus:HI (match_dup 0)
		 (const_int -1)))]
  "find_reg_note (next_cc0_user (insn), REG_NONNEG, 0)"
  "*
{
  if (which_alternative == 0)
    {
      output_cmp (operands[0], const0_rtx, 'H');
      return \"add.h #-1,%0\";
    }
  else
    {
      output_cmp (gen_rtx (REG, HImode, 7), const0_rtx, 'H');
      return \"psh.w s7\;ld.h %0,s7\;add.h #-1,s7\;st.h s7,%0\;pop.w s7\";
    }
}")

;;- Jumps

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
  "* return output_condjump (operands[0], \"eq\", 't'); ")

(define_insn "bne"
  [(set (pc)
	(if_then_else (ne (cc0)
			  (const_int 0))
		      (label_ref (match_operand 0 "" ""))
		      (pc)))]
  ""
  "* return output_condjump (operands[0], \"eq\", 'f'); ")

(define_insn "bgt"
  [(set (pc)
	(if_then_else (gt (cc0)
			  (const_int 0))
		      (label_ref (match_operand 0 "" ""))
		      (pc)))]
  ""
  "* return output_condjump (operands[0], \"le\", 'f'); ")

(define_insn "bgtu"
  [(set (pc)
	(if_then_else (gtu (cc0)
			   (const_int 0))
		      (label_ref (match_operand 0 "" ""))
		      (pc)))]
  ""
  "* return output_condjump (operands[0], \"leu\", 'f'); ")

(define_insn "blt"
  [(set (pc)
	(if_then_else (lt (cc0)
			  (const_int 0))
		      (label_ref (match_operand 0 "" ""))
		      (pc)))]
  ""
  "* return output_condjump (operands[0], \"lt\", 't'); ")

(define_insn "bltu"
  [(set (pc)
	(if_then_else (ltu (cc0)
			   (const_int 0))
		      (label_ref (match_operand 0 "" ""))
		      (pc)))]
  ""
  "* return output_condjump (operands[0], \"ltu\", 't'); ")

(define_insn "bge"
  [(set (pc)
	(if_then_else (ge (cc0)
			  (const_int 0))
		      (label_ref (match_operand 0 "" ""))
		      (pc)))]
  ""
  "* return output_condjump (operands[0], \"lt\", 'f'); ")

(define_insn "bgeu"
  [(set (pc)
	(if_then_else (geu (cc0)
			   (const_int 0))
		      (label_ref (match_operand 0 "" ""))
		      (pc)))]
  ""
  "* return output_condjump (operands[0], \"ltu\", 'f'); ")

(define_insn "ble"
  [(set (pc)
	(if_then_else (le (cc0)
			  (const_int 0))
		      (label_ref (match_operand 0 "" ""))
		      (pc)))]
  ""
  "* return output_condjump (operands[0], \"le\", 't'); ")

(define_insn "bleu"
  [(set (pc)
	(if_then_else (leu (cc0)
			   (const_int 0))
		      (label_ref (match_operand 0 "" ""))
		      (pc)))]
  ""
  "* return output_condjump (operands[0], \"leu\", 't'); ")

(define_insn ""
  [(set (pc)
	(if_then_else (eq (cc0)
			  (const_int 0))
		      (pc)
		      (label_ref (match_operand 0 "" ""))))]
  ""
  "* return output_condjump (operands[0], \"eq\", 'f'); ")

(define_insn ""
  [(set (pc)
	(if_then_else (ne (cc0)
			  (const_int 0))
		      (pc)
		      (label_ref (match_operand 0 "" ""))))]
  ""
  "* return output_condjump (operands[0], \"eq\", 't'); ")

(define_insn ""
  [(set (pc)
	(if_then_else (gt (cc0)
			  (const_int 0))
		      (pc)
		      (label_ref (match_operand 0 "" ""))))]
  ""
  "* return output_condjump (operands[0], \"le\", 't'); ")

(define_insn ""
  [(set (pc)
	(if_then_else (gtu (cc0)
			   (const_int 0))
		      (pc)
		      (label_ref (match_operand 0 "" ""))))]
  ""
  "* return output_condjump (operands[0], \"leu\", 't'); ")

(define_insn ""
  [(set (pc)
	(if_then_else (lt (cc0)
			  (const_int 0))
		      (pc)
		      (label_ref (match_operand 0 "" ""))))]
  ""
  "* return output_condjump (operands[0], \"lt\", 'f'); ")

(define_insn ""
  [(set (pc)
	(if_then_else (ltu (cc0)
			   (const_int 0))
		      (pc)
		      (label_ref (match_operand 0 "" ""))))]
  ""
  "* return output_condjump (operands[0], \"ltu\", 'f'); ")

(define_insn ""
  [(set (pc)
	(if_then_else (ge (cc0)
			  (const_int 0))
		      (pc)
		      (label_ref (match_operand 0 "" ""))))]
  ""
  "* return output_condjump (operands[0], \"lt\", 't'); ")

(define_insn ""
  [(set (pc)
	(if_then_else (geu (cc0)
			   (const_int 0))
		      (pc)
		      (label_ref (match_operand 0 "" ""))))]
  ""
  "* return output_condjump (operands[0], \"ltu\", 't'); ")

(define_insn ""
  [(set (pc)
	(if_then_else (le (cc0)
			  (const_int 0))
		      (pc)
		      (label_ref (match_operand 0 "" ""))))]
  ""
  "* return output_condjump (operands[0], \"le\", 'f'); ")

(define_insn ""
  [(set (pc)
	(if_then_else (leu (cc0)
			   (const_int 0))
		      (pc)
		      (label_ref (match_operand 0 "" ""))))]
  ""
  "* return output_condjump (operands[0], \"leu\", 'f'); ")

;;- Calls

(define_expand "call_pop"
  [(parallel [(call (match_operand:QI 0 "memory_operand" "m")
		    (match_operand:SI 1 "const_int_operand" "i"))
	      (match_operand:SI 2 "const_int_operand" "i")
	      (match_operand:SI 3 "const_int_operand" "i")
	      (reg:SI 8)])]
  ""
  "")

(define_insn ""
  [(call (match_operand:QI 0 "memory_operand" "m")
	 (match_operand:SI 1 "const_int_operand" "i"))
   (match_operand:SI 2 "const_int_operand" "i")
   (match_operand:SI 3 "const_int_operand" "i")
   (match_operand:SI 4 "" "")]
  ""
  "* return output_call (insn, &operands[0]);")

(define_expand "call_value_pop"
  [(parallel [(set (match_operand 0 "" "=g")
		   (call (match_operand:QI 1 "memory_operand" "m")
			 (match_operand:SI 2 "const_int_operand" "i")))
	      (match_operand:SI 3 "const_int_operand" "i")
	      (match_operand:SI 4 "const_int_operand" "i")
	      (reg:SI 8)])]
  ""
  "")

(define_insn ""
  [(set (match_operand 0 "" "=g")
	(call (match_operand:QI 1 "memory_operand" "m")
	      (match_operand:SI 2 "const_int_operand" "i")))
   (match_operand:SI 3 "const_int_operand" "i")
   (match_operand:SI 4 "const_int_operand" "i")
   (match_operand:SI 5 "" "")]
  ""
  "* return output_call (insn, &operands[1]); ")

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

  emit_call_insn (gen_call_pop (operands[0], const0_rtx,
				const0_rtx, const0_rtx));

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

(define_expand "return"
  [(return)]
  ""
  " replace_arg_pushes (); ")

(define_insn ""
  [(return)]
  ""
  "rtn")

(define_expand "prologue"
  [(const_int 0)]
  ""
  "
{
  emit_ap_optimizations ();
  DONE; 
}")

(define_insn "tablejump"
  [(set (pc) (match_operand:SI 0 "address_operand" "p"))
   (use (label_ref (match_operand 1 "" "")))]
  ""
  "jmp %a0")

(define_insn "indirect_jump"
  [(set (pc) (match_operand:SI 0 "address_operand" "p"))]
  ""
  "jmp %a0")
