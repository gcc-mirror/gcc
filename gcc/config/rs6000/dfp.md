;; Decimal Floating Point (DFP) patterns.
;; Copyright (C) 2007, 2008
;; Free Software Foundation, Inc.
;; Contributed by Ben Elliston (bje@au.ibm.com) and Peter Bergner
;; (bergner@vnet.ibm.com).

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

;;
;; UNSPEC usage
;;

(define_constants
  [(UNSPEC_MOVSD_LOAD		400)
   (UNSPEC_MOVSD_STORE		401)
  ])


(define_expand "movsd"
  [(set (match_operand:SD 0 "nonimmediate_operand" "")
	(match_operand:SD 1 "any_operand" ""))]
  "TARGET_HARD_FLOAT && TARGET_FPRS"
  "{ rs6000_emit_move (operands[0], operands[1], SDmode); DONE; }")

(define_split
  [(set (match_operand:SD 0 "gpc_reg_operand" "")
	(match_operand:SD 1 "const_double_operand" ""))]
  "reload_completed
   && ((GET_CODE (operands[0]) == REG && REGNO (operands[0]) <= 31)
       || (GET_CODE (operands[0]) == SUBREG
	   && GET_CODE (SUBREG_REG (operands[0])) == REG
	   && REGNO (SUBREG_REG (operands[0])) <= 31))"
  [(set (match_dup 2) (match_dup 3))]
  "
{
  long l;
  REAL_VALUE_TYPE rv;

  REAL_VALUE_FROM_CONST_DOUBLE (rv, operands[1]);
  REAL_VALUE_TO_TARGET_DECIMAL32 (rv, l);

  if (! TARGET_POWERPC64)
    operands[2] = operand_subword (operands[0], 0, 0, SDmode);
  else
    operands[2] = gen_lowpart (SImode, operands[0]);

  operands[3] = gen_int_mode (l, SImode);
}")

(define_insn "movsd_hardfloat"
  [(set (match_operand:SD 0 "nonimmediate_operand" "=r,r,m,f,*c*l,*q,!r,*h,!r,!r")
	(match_operand:SD 1 "input_operand"        "r,m,r,f,r,r,h,0,G,Fn"))]
  "(gpc_reg_operand (operands[0], SDmode)
   || gpc_reg_operand (operands[1], SDmode))
   && (TARGET_HARD_FLOAT && TARGET_FPRS)"
  "@
   mr %0,%1
   {l%U1%X1|lwz%U1%X1} %0,%1
   {st%U0%X0|stw%U0%X0} %1,%0
   fmr %0,%1
   mt%0 %1
   mt%0 %1
   mf%1 %0
   {cror 0,0,0|nop}
   #
   #"
  [(set_attr "type" "*,load,store,fp,mtjmpr,*,mfjmpr,*,*,*")
   (set_attr "length" "4,4,4,4,4,4,4,4,4,8")])

(define_insn "movsd_softfloat"
  [(set (match_operand:SD 0 "nonimmediate_operand" "=r,cl,q,r,r,m,r,r,r,r,r,*h")
	(match_operand:SD 1 "input_operand" "r,r,r,h,m,r,I,L,R,G,Fn,0"))]
  "(gpc_reg_operand (operands[0], SDmode)
   || gpc_reg_operand (operands[1], SDmode))
   && (TARGET_SOFT_FLOAT || !TARGET_FPRS)"
  "@
   mr %0,%1
   mt%0 %1
   mt%0 %1
   mf%1 %0
   {l%U1%X1|lwz%U1%X1} %0,%1
   {st%U0%X0|stw%U0%X0} %1,%0
   {lil|li} %0,%1
   {liu|lis} %0,%v1
   {cal|la} %0,%a1
   #
   #
   {cror 0,0,0|nop}"
  [(set_attr "type" "*,mtjmpr,*,mfjmpr,load,store,*,*,*,*,*,*")
   (set_attr "length" "4,4,4,4,4,4,4,4,4,4,8,4")])

(define_insn "movsd_store"
  [(set (match_operand:DD 0 "nonimmediate_operand" "=m")
	(unspec:DD [(match_operand:SD 1 "input_operand" "d")]
		   UNSPEC_MOVSD_STORE))]
  "(gpc_reg_operand (operands[0], DDmode)
   || gpc_reg_operand (operands[1], SDmode))
   && TARGET_HARD_FLOAT && TARGET_FPRS"
  "stfd%U0%X0 %1,%0"
  [(set_attr "type" "fpstore")
   (set_attr "length" "4")])

(define_insn "movsd_load"
  [(set (match_operand:SD 0 "nonimmediate_operand" "=f")
	(unspec:SD [(match_operand:DD 1 "input_operand" "m")]
		   UNSPEC_MOVSD_LOAD))]
  "(gpc_reg_operand (operands[0], SDmode)
   || gpc_reg_operand (operands[1], DDmode))
   && TARGET_HARD_FLOAT && TARGET_FPRS"
  "lfd%U1%X1 %0,%1"
  [(set_attr "type" "fpload")
   (set_attr "length" "4")])

;; Hardware support for decimal floating point operations.

(define_insn "extendsddd2"
  [(set (match_operand:DD 0 "gpc_reg_operand" "=d")
	(float_extend:DD (match_operand:SD 1 "gpc_reg_operand" "f")))]
  "TARGET_DFP"
  "dctdp %0,%1"
  [(set_attr "type" "fp")])

(define_expand "extendsdtd2"
  [(set (match_operand:TD 0 "gpc_reg_operand" "=d")
	(float_extend:TD (match_operand:SD 1 "gpc_reg_operand" "d")))]
  "TARGET_DFP"
{
  rtx tmp = gen_reg_rtx (DDmode);
  emit_insn (gen_extendsddd2 (tmp, operands[1]));
  emit_insn (gen_extendddtd2 (operands[0], tmp));
  DONE;
})

(define_insn "truncddsd2"
  [(set (match_operand:SD 0 "gpc_reg_operand" "=f")
	(float_truncate:SD (match_operand:DD 1 "gpc_reg_operand" "d")))]
  "TARGET_DFP"
  "drsp %0,%1"
  [(set_attr "type" "fp")])

(define_expand "negdd2"
  [(set (match_operand:DD 0 "gpc_reg_operand" "")
	(neg:DD (match_operand:DD 1 "gpc_reg_operand" "")))]
  "TARGET_HARD_FLOAT && TARGET_FPRS"
  "")

(define_insn "*negdd2_fpr"
  [(set (match_operand:DD 0 "gpc_reg_operand" "=d")
	(neg:DD (match_operand:DD 1 "gpc_reg_operand" "d")))]
  "TARGET_HARD_FLOAT && TARGET_FPRS"
  "fneg %0,%1"
  [(set_attr "type" "fp")])

(define_expand "absdd2"
  [(set (match_operand:DD 0 "gpc_reg_operand" "")
	(abs:DD (match_operand:DD 1 "gpc_reg_operand" "")))]
  "TARGET_HARD_FLOAT && TARGET_FPRS"
  "")

(define_insn "*absdd2_fpr"
  [(set (match_operand:DD 0 "gpc_reg_operand" "=d")
	(abs:DD (match_operand:DD 1 "gpc_reg_operand" "d")))]
  "TARGET_HARD_FLOAT && TARGET_FPRS"
  "fabs %0,%1"
  [(set_attr "type" "fp")])

(define_insn "*nabsdd2_fpr"
  [(set (match_operand:DD 0 "gpc_reg_operand" "=d")
	(neg:DD (abs:DD (match_operand:DD 1 "gpc_reg_operand" "d"))))]
  "TARGET_HARD_FLOAT && TARGET_FPRS"
  "fnabs %0,%1"
  [(set_attr "type" "fp")])

(define_expand "movdd"
  [(set (match_operand:DD 0 "nonimmediate_operand" "")
	(match_operand:DD 1 "any_operand" ""))]
  ""
  "{ rs6000_emit_move (operands[0], operands[1], DDmode); DONE; }")

(define_split
  [(set (match_operand:DD 0 "gpc_reg_operand" "")
	(match_operand:DD 1 "const_int_operand" ""))]
  "! TARGET_POWERPC64 && reload_completed
   && ((GET_CODE (operands[0]) == REG && REGNO (operands[0]) <= 31)
       || (GET_CODE (operands[0]) == SUBREG
	   && GET_CODE (SUBREG_REG (operands[0])) == REG
	   && REGNO (SUBREG_REG (operands[0])) <= 31))"
  [(set (match_dup 2) (match_dup 4))
   (set (match_dup 3) (match_dup 1))]
  "
{
  int endian = (WORDS_BIG_ENDIAN == 0);
  HOST_WIDE_INT value = INTVAL (operands[1]);

  operands[2] = operand_subword (operands[0], endian, 0, DDmode);
  operands[3] = operand_subword (operands[0], 1 - endian, 0, DDmode);
#if HOST_BITS_PER_WIDE_INT == 32
  operands[4] = (value & 0x80000000) ? constm1_rtx : const0_rtx;
#else
  operands[4] = GEN_INT (value >> 32);
  operands[1] = GEN_INT (((value & 0xffffffff) ^ 0x80000000) - 0x80000000);
#endif
}")

(define_split
  [(set (match_operand:DD 0 "gpc_reg_operand" "")
	(match_operand:DD 1 "const_double_operand" ""))]
  "! TARGET_POWERPC64 && reload_completed
   && ((GET_CODE (operands[0]) == REG && REGNO (operands[0]) <= 31)
       || (GET_CODE (operands[0]) == SUBREG
	   && GET_CODE (SUBREG_REG (operands[0])) == REG
	   && REGNO (SUBREG_REG (operands[0])) <= 31))"
  [(set (match_dup 2) (match_dup 4))
   (set (match_dup 3) (match_dup 5))]
  "
{
  int endian = (WORDS_BIG_ENDIAN == 0);
  long l[2];
  REAL_VALUE_TYPE rv;

  REAL_VALUE_FROM_CONST_DOUBLE (rv, operands[1]);
  REAL_VALUE_TO_TARGET_DECIMAL64 (rv, l);

  operands[2] = operand_subword (operands[0], endian, 0, DDmode);
  operands[3] = operand_subword (operands[0], 1 - endian, 0, DDmode);
  operands[4] = gen_int_mode (l[endian], SImode);
  operands[5] = gen_int_mode (l[1 - endian], SImode);
}")

(define_split
  [(set (match_operand:DD 0 "gpc_reg_operand" "")
	(match_operand:DD 1 "const_double_operand" ""))]
  "TARGET_POWERPC64 && reload_completed
   && ((GET_CODE (operands[0]) == REG && REGNO (operands[0]) <= 31)
       || (GET_CODE (operands[0]) == SUBREG
	   && GET_CODE (SUBREG_REG (operands[0])) == REG
	   && REGNO (SUBREG_REG (operands[0])) <= 31))"
  [(set (match_dup 2) (match_dup 3))]
  "
{
  int endian = (WORDS_BIG_ENDIAN == 0);
  long l[2];
  REAL_VALUE_TYPE rv;
#if HOST_BITS_PER_WIDE_INT >= 64
  HOST_WIDE_INT val;
#endif

  REAL_VALUE_FROM_CONST_DOUBLE (rv, operands[1]);
  REAL_VALUE_TO_TARGET_DECIMAL64 (rv, l);

  operands[2] = gen_lowpart (DImode, operands[0]);
  /* HIGHPART is lower memory address when WORDS_BIG_ENDIAN.  */
#if HOST_BITS_PER_WIDE_INT >= 64
  val = ((HOST_WIDE_INT)(unsigned long)l[endian] << 32
	 | ((HOST_WIDE_INT)(unsigned long)l[1 - endian]));

  operands[3] = gen_int_mode (val, DImode);
#else
  operands[3] = immed_double_const (l[1 - endian], l[endian], DImode);
#endif
}")

;; Don't have reload use general registers to load a constant.  First,
;; it might not work if the output operand is the equivalent of
;; a non-offsettable memref, but also it is less efficient than loading
;; the constant into an FP register, since it will probably be used there.
;; The "??" is a kludge until we can figure out a more reasonable way
;; of handling these non-offsettable values.
(define_insn "*movdd_hardfloat32"
  [(set (match_operand:DD 0 "nonimmediate_operand" "=!r,??r,m,d,d,m,!r,!r,!r")
	(match_operand:DD 1 "input_operand" "r,m,r,d,m,d,G,H,F"))]
  "! TARGET_POWERPC64 && TARGET_HARD_FLOAT && TARGET_FPRS
   && (gpc_reg_operand (operands[0], DDmode)
       || gpc_reg_operand (operands[1], DDmode))"
  "*
{
  switch (which_alternative)
    {
    default:
      gcc_unreachable ();
    case 0:
    case 1:
    case 2:
      return \"#\";
    case 3:
      return \"fmr %0,%1\";
    case 4:
      return \"lfd%U1%X1 %0,%1\";
    case 5:
      return \"stfd%U0%X0 %1,%0\";
    case 6:
    case 7:
    case 8:
      return \"#\";
    }
}"
  [(set_attr "type" "two,load,store,fp,fpload,fpstore,*,*,*")
   (set_attr "length" "8,16,16,4,4,4,8,12,16")])

(define_insn "*movdd_softfloat32"
  [(set (match_operand:DD 0 "nonimmediate_operand" "=r,r,m,r,r,r")
	(match_operand:DD 1 "input_operand" "r,m,r,G,H,F"))]
  "! TARGET_POWERPC64 && (TARGET_SOFT_FLOAT || !TARGET_FPRS)
   && (gpc_reg_operand (operands[0], DDmode)
       || gpc_reg_operand (operands[1], DDmode))"
  "#"
  [(set_attr "type" "two,load,store,*,*,*")
   (set_attr "length" "8,8,8,8,12,16")])

; ld/std require word-aligned displacements -> 'Y' constraint.
; List Y->r and r->Y before r->r for reload.
(define_insn "*movdd_hardfloat64_mfpgpr"
  [(set (match_operand:DD 0 "nonimmediate_operand" "=Y,r,!r,d,d,m,*c*l,!r,*h,!r,!r,!r,r,d")
	(match_operand:DD 1 "input_operand" "r,Y,r,d,m,d,r,h,0,G,H,F,d,r"))]
  "TARGET_POWERPC64 && TARGET_MFPGPR && TARGET_HARD_FLOAT && TARGET_FPRS
   && (gpc_reg_operand (operands[0], DDmode)
       || gpc_reg_operand (operands[1], DDmode))"
  "@
   std%U0%X0 %1,%0
   ld%U1%X1 %0,%1
   mr %0,%1
   fmr %0,%1
   lfd%U1%X1 %0,%1
   stfd%U0%X0 %1,%0
   mt%0 %1
   mf%1 %0
   {cror 0,0,0|nop}
   #
   #
   #
   mftgpr %0,%1
   mffgpr %0,%1"
  [(set_attr "type" "store,load,*,fp,fpload,fpstore,mtjmpr,mfjmpr,*,*,*,*,mftgpr,mffgpr")
   (set_attr "length" "4,4,4,4,4,4,4,4,4,8,12,16,4,4")])

; ld/std require word-aligned displacements -> 'Y' constraint.
; List Y->r and r->Y before r->r for reload.
(define_insn "*movdd_hardfloat64"
  [(set (match_operand:DD 0 "nonimmediate_operand" "=Y,r,!r,d,d,m,*c*l,!r,*h,!r,!r,!r")
	(match_operand:DD 1 "input_operand" "r,Y,r,d,m,d,r,h,0,G,H,F"))]
  "TARGET_POWERPC64 && !TARGET_MFPGPR && TARGET_HARD_FLOAT && TARGET_FPRS
   && (gpc_reg_operand (operands[0], DDmode)
       || gpc_reg_operand (operands[1], DDmode))"
  "@
   std%U0%X0 %1,%0
   ld%U1%X1 %0,%1
   mr %0,%1
   fmr %0,%1
   lfd%U1%X1 %0,%1
   stfd%U0%X0 %1,%0
   mt%0 %1
   mf%1 %0
   {cror 0,0,0|nop}
   #
   #
   #"
  [(set_attr "type" "store,load,*,fp,fpload,fpstore,mtjmpr,mfjmpr,*,*,*,*")
   (set_attr "length" "4,4,4,4,4,4,4,4,4,8,12,16")])

(define_insn "*movdd_softfloat64"
  [(set (match_operand:DD 0 "nonimmediate_operand" "=r,Y,r,cl,r,r,r,r,*h")
	(match_operand:DD 1 "input_operand" "Y,r,r,r,h,G,H,F,0"))]
  "TARGET_POWERPC64 && (TARGET_SOFT_FLOAT || !TARGET_FPRS)
   && (gpc_reg_operand (operands[0], DDmode)
       || gpc_reg_operand (operands[1], DDmode))"
  "@
   ld%U1%X1 %0,%1
   std%U0%X0 %1,%0
   mr %0,%1
   mt%0 %1
   mf%1 %0
   #
   #
   #
   {cror 0,0,0|nop}"
  [(set_attr "type" "load,store,*,mtjmpr,mfjmpr,*,*,*,*")
   (set_attr "length" "4,4,4,4,4,8,12,16,4")])

(define_expand "negtd2"
  [(set (match_operand:TD 0 "gpc_reg_operand" "")
	(neg:TD (match_operand:TD 1 "gpc_reg_operand" "")))]
  "TARGET_HARD_FLOAT && TARGET_FPRS"
  "")

(define_insn "*negtd2_fpr"
  [(set (match_operand:TD 0 "gpc_reg_operand" "=d")
	(neg:TD (match_operand:TD 1 "gpc_reg_operand" "d")))]
  "TARGET_HARD_FLOAT && TARGET_FPRS"
  "fneg %0,%1"
  [(set_attr "type" "fp")])

(define_expand "abstd2"
  [(set (match_operand:TD 0 "gpc_reg_operand" "")
	(abs:TD (match_operand:TD 1 "gpc_reg_operand" "")))]
  "TARGET_HARD_FLOAT && TARGET_FPRS"
  "")

(define_insn "*abstd2_fpr"
  [(set (match_operand:TD 0 "gpc_reg_operand" "=d")
	(abs:TD (match_operand:TD 1 "gpc_reg_operand" "d")))]
  "TARGET_HARD_FLOAT && TARGET_FPRS"
  "fabs %0,%1"
  [(set_attr "type" "fp")])

(define_insn "*nabstd2_fpr"
  [(set (match_operand:TD 0 "gpc_reg_operand" "=d")
	(neg:TD (abs:TD (match_operand:TD 1 "gpc_reg_operand" "d"))))]
  "TARGET_HARD_FLOAT && TARGET_FPRS"
  "fnabs %0,%1"
  [(set_attr "type" "fp")])

(define_expand "movtd"
  [(set (match_operand:TD 0 "general_operand" "")
	(match_operand:TD 1 "any_operand" ""))]
  "TARGET_HARD_FLOAT && TARGET_FPRS"
  "{ rs6000_emit_move (operands[0], operands[1], TDmode); DONE; }")

; It's important to list the o->f and f->o moves before f->f because
; otherwise reload, given m->f, will try to pick f->f and reload it,
; which doesn't make progress.  Likewise r->Y must be before r->r.
(define_insn_and_split "*movtd_internal"
  [(set (match_operand:TD 0 "nonimmediate_operand" "=o,d,d,r,Y,r")
	(match_operand:TD 1 "input_operand"         "d,o,d,YGHF,r,r"))]
  "TARGET_HARD_FLOAT && TARGET_FPRS
   && (gpc_reg_operand (operands[0], TDmode)
       || gpc_reg_operand (operands[1], TDmode))"
  "#"
  "&& reload_completed"
  [(pc)]
{ rs6000_split_multireg_move (operands[0], operands[1]); DONE; }
  [(set_attr "length" "8,8,8,20,20,16")])

;; Hardware support for decimal floating point operations.

(define_insn "extendddtd2"
  [(set (match_operand:TD 0 "gpc_reg_operand" "=d")
	(float_extend:TD (match_operand:DD 1 "gpc_reg_operand" "d")))]
  "TARGET_DFP"
  "dctqpq %0,%1"
  [(set_attr "type" "fp")])

;; The result of drdpq is an even/odd register pair with the converted
;; value in the even register and zero in the odd register.
;; FIXME: Avoid the register move by using a reload constraint to ensure
;; that the result is the first of the pair receiving the result of drdpq.

(define_insn "trunctddd2"
  [(set (match_operand:DD 0 "gpc_reg_operand" "=d")
	(float_truncate:DD (match_operand:TD 1 "gpc_reg_operand" "d")))
   (clobber (match_scratch:TD 2 "=d"))]
  "TARGET_DFP"
  "drdpq %2,%1\;fmr %0,%2"
  [(set_attr "type" "fp")])

(define_insn "adddd3"
  [(set (match_operand:DD 0 "gpc_reg_operand" "=d")
	(plus:DD (match_operand:DD 1 "gpc_reg_operand" "%d")
		 (match_operand:DD 2 "gpc_reg_operand" "d")))]
  "TARGET_DFP"
  "dadd %0,%1,%2"
  [(set_attr "type" "fp")])

(define_insn "addtd3"
  [(set (match_operand:TD 0 "gpc_reg_operand" "=d")
	(plus:TD (match_operand:TD 1 "gpc_reg_operand" "%d")
		 (match_operand:TD 2 "gpc_reg_operand" "d")))]
  "TARGET_DFP"
  "daddq %0,%1,%2"
  [(set_attr "type" "fp")])

(define_insn "subdd3"
  [(set (match_operand:DD 0 "gpc_reg_operand" "=d")
	(minus:DD (match_operand:DD 1 "gpc_reg_operand" "d")
		  (match_operand:DD 2 "gpc_reg_operand" "d")))]
  "TARGET_DFP"
  "dsub %0,%1,%2"
  [(set_attr "type" "fp")])

(define_insn "subtd3"
  [(set (match_operand:TD 0 "gpc_reg_operand" "=d")
	(minus:TD (match_operand:TD 1 "gpc_reg_operand" "d")
		  (match_operand:TD 2 "gpc_reg_operand" "d")))]
  "TARGET_DFP"
  "dsubq %0,%1,%2"
  [(set_attr "type" "fp")])

(define_insn "muldd3"
  [(set (match_operand:DD 0 "gpc_reg_operand" "=d")
	(mult:DD (match_operand:DD 1 "gpc_reg_operand" "%d")
		 (match_operand:DD 2 "gpc_reg_operand" "d")))]
  "TARGET_DFP"
  "dmul %0,%1,%2"
  [(set_attr "type" "fp")])

(define_insn "multd3"
  [(set (match_operand:TD 0 "gpc_reg_operand" "=d")
	(mult:TD (match_operand:TD 1 "gpc_reg_operand" "%d")
		 (match_operand:TD 2 "gpc_reg_operand" "d")))]
  "TARGET_DFP"
  "dmulq %0,%1,%2"
  [(set_attr "type" "fp")])

(define_insn "divdd3"
  [(set (match_operand:DD 0 "gpc_reg_operand" "=d")
	(div:DD (match_operand:DD 1 "gpc_reg_operand" "d")
		(match_operand:DD 2 "gpc_reg_operand" "d")))]
  "TARGET_DFP"
  "ddiv %0,%1,%2"
  [(set_attr "type" "fp")])

(define_insn "divtd3"
  [(set (match_operand:TD 0 "gpc_reg_operand" "=d")
	(div:TD (match_operand:TD 1 "gpc_reg_operand" "d")
		(match_operand:TD 2 "gpc_reg_operand" "d")))]
  "TARGET_DFP"
  "ddivq %0,%1,%2"
  [(set_attr "type" "fp")])

(define_insn "*cmpdd_internal1"
  [(set (match_operand:CCFP 0 "cc_reg_operand" "=y")
	(compare:CCFP (match_operand:DD 1 "gpc_reg_operand" "d")
		      (match_operand:DD 2 "gpc_reg_operand" "d")))]
  "TARGET_DFP"
  "dcmpu %0,%1,%2"
  [(set_attr "type" "fpcompare")])

(define_insn "*cmptd_internal1"
  [(set (match_operand:CCFP 0 "cc_reg_operand" "=y")
	(compare:CCFP (match_operand:TD 1 "gpc_reg_operand" "d")
		      (match_operand:TD 2 "gpc_reg_operand" "d")))]
  "TARGET_DFP"
  "dcmpuq %0,%1,%2"
  [(set_attr "type" "fpcompare")])

(define_insn "floatditd2"
  [(set (match_operand:TD 0 "gpc_reg_operand" "=d")
	(float:TD (match_operand:DI 1 "gpc_reg_operand" "d")))]
  "TARGET_DFP"
  "dcffixq %0,%1"
  [(set_attr "type" "fp")])

;; Convert a decimal64 to a decimal64 whose value is an integer.
;; This is the first stage of converting it to an integer type.

(define_insn "ftruncdd2"
  [(set (match_operand:DD 0 "gpc_reg_operand" "=d")
	(fix:DD (match_operand:DD 1 "gpc_reg_operand" "d")))]
  "TARGET_DFP"
  "drintn. 0,%0,%1,1"
  [(set_attr "type" "fp")])

;; Convert a decimal64 whose value is an integer to an actual integer.
;; This is the second stage of converting decimal float to integer type.

(define_insn "fixdddi2"
  [(set (match_operand:DI 0 "gpc_reg_operand" "=d")
	(fix:DI (match_operand:DD 1 "gpc_reg_operand" "d")))]
  "TARGET_DFP"
  "dctfix %0,%1"
  [(set_attr "type" "fp")])

;; Convert a decimal128 to a decimal128 whose value is an integer.
;; This is the first stage of converting it to an integer type.

(define_insn "ftrunctd2"
  [(set (match_operand:TD 0 "gpc_reg_operand" "=d")
	(fix:TD (match_operand:TD 1 "gpc_reg_operand" "d")))]
  "TARGET_DFP"
  "drintnq. 0,%0,%1,1"
  [(set_attr "type" "fp")])

;; Convert a decimal128 whose value is an integer to an actual integer.
;; This is the second stage of converting decimal float to integer type.

(define_insn "fixtddi2"
  [(set (match_operand:DI 0 "gpc_reg_operand" "=d")
	(fix:DI (match_operand:TD 1 "gpc_reg_operand" "d")))]
  "TARGET_DFP"
  "dctfixq %0,%1"
  [(set_attr "type" "fp")])
