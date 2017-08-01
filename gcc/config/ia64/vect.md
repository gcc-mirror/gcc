;; IA-64 machine description for vector operations.
;; Copyright (C) 2004-2017 Free Software Foundation, Inc.
;;
;; This file is part of GCC.
;;
;; GCC is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; GCC is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GCC; see the file COPYING3.  If not see
;; <http://www.gnu.org/licenses/>.


;; Integer vector operations

(define_mode_iterator VEC [V8QI V4HI V2SI V2SF])
(define_mode_iterator VECINT [V8QI V4HI V2SI])
(define_mode_iterator VECINT12 [V8QI V4HI])
(define_mode_iterator VECINT24 [V4HI V2SI])
(define_mode_attr vecsize [(V8QI "1") (V4HI "2") (V2SI "4")])
(define_mode_attr vecwider [(V8QI "V4HI") (V4HI "V2SI")])
(define_mode_attr vecint
  [(V8QI "V8QI") (V4HI "V4HI") (V2SI "V2SI") (V2SF "V2SI")])

(define_expand "mov<mode>"
  [(set (match_operand:VECINT 0 "general_operand" "")
        (match_operand:VECINT 1 "general_operand" ""))]
  ""
{
  rtx op1 = ia64_expand_move (operands[0], operands[1]);
  if (!op1)
    DONE;
  operands[1] = op1;
})

(define_insn "*mov<mode>_internal"
  [(set (match_operand:VECINT 0 "destination_operand"
					"=r,r,r,r,m ,*f ,*f,Q ,r ,*f")
	(match_operand:VECINT 1 "move_operand"
					"rU,W,i,m,rU,U*f,Q ,*f,*f,r "))]
  "ia64_move_ok (operands[0], operands[1])"
  "@
   mov %0 = %r1
   addl %0 = %v1, r0
   movl %0 = %v1
   ld8%O1 %0 = %1%P1
   st8%Q0 %0 = %r1%P0
   mov %0 = %F1
   ldf8 %0 = %1%P1
   stf8 %0 = %1%P0
   getf.sig %0 = %1
   setf.sig %0 = %1"
  [(set_attr "itanium_class" "ialu,ialu,long_i,ld,st,fmisc,fld,stf,frfr,tofr")])

(define_insn "one_cmpl<mode>2"
  [(set (match_operand:VECINT 0 "gr_register_operand" "=r")
	(not:VECINT (match_operand:VECINT 1 "gr_register_operand" "r")))]
  ""
  "andcm %0 = -1, %1"
  [(set_attr "itanium_class" "ilog")])

(define_insn "and<mode>3"
  [(set (match_operand:VECINT 0 "grfr_register_operand" "=r,*f")
	(and:VECINT
	  (match_operand:VECINT 1 "grfr_register_operand" "r,*f")
	  (match_operand:VECINT 2 "grfr_reg_or_8bit_operand" "r,*f")))]
  ""
  "@
   and %0 = %2, %1
   fand %0 = %2, %1"
  [(set_attr "itanium_class" "ilog,fmisc")])

(define_insn "*andnot<mode>"
  [(set (match_operand:VECINT 0 "grfr_register_operand" "=r,*f")
	(and:VECINT
	  (not:VECINT (match_operand:VECINT 1 "grfr_register_operand" "r,*f"))
	  (match_operand:VECINT 2 "grfr_reg_or_8bit_operand" "r,*f")))]
  ""
  "@
   andcm %0 = %2, %1
   fandcm %0 = %2, %1"
  [(set_attr "itanium_class" "ilog,fmisc")])

(define_insn "ior<mode>3"
  [(set (match_operand:VECINT 0 "grfr_register_operand" "=r,*f")
	(ior:VECINT
	  (match_operand:VECINT 1 "grfr_register_operand" "r,*f")
	  (match_operand:VECINT 2 "grfr_reg_or_8bit_operand" "r,*f")))]
  ""
  "@
   or %0 = %2, %1
   for %0 = %2, %1"
  [(set_attr "itanium_class" "ilog,fmisc")])

(define_insn "xor<mode>3"
  [(set (match_operand:VECINT 0 "grfr_register_operand" "=r,*f")
	(xor:VECINT
	  (match_operand:VECINT 1 "grfr_register_operand" "r,*f")
	  (match_operand:VECINT 2 "grfr_reg_or_8bit_operand" "r,*f")))]
  ""
  "@
   xor %0 = %2, %1
   fxor %0 = %2, %1"
  [(set_attr "itanium_class" "ilog,fmisc")])

(define_insn "neg<mode>2"
  [(set (match_operand:VECINT 0 "gr_register_operand" "=r")
	(neg:VECINT (match_operand:VECINT 1 "gr_register_operand" "r")))]
  ""
  "psub<vecsize> %0 = r0, %1"
  [(set_attr "itanium_class" "mmalua")])

(define_insn "add<mode>3"
  [(set (match_operand:VECINT 0 "gr_register_operand" "=r")
	(plus:VECINT (match_operand:VECINT 1 "gr_register_operand" "r")
		     (match_operand:VECINT 2 "gr_register_operand" "r")))]
  ""
  "padd<vecsize> %0 = %1, %2"
  [(set_attr "itanium_class" "mmalua")])

(define_insn "*ssadd<mode>3"
  [(set (match_operand:VECINT12 0 "gr_register_operand" "=r")
	(ss_plus:VECINT12
	  (match_operand:VECINT12 1 "gr_register_operand" "r")
	  (match_operand:VECINT12 2 "gr_register_operand" "r")))]
  ""
  "padd<vecsize>.sss %0 = %1, %2"
  [(set_attr "itanium_class" "mmalua")])

(define_insn "*usadd<mode>3"
  [(set (match_operand:VECINT12 0 "gr_register_operand" "=r")
	(us_plus:VECINT12
	  (match_operand:VECINT12 1 "gr_register_operand" "r")
	  (match_operand:VECINT12 2 "gr_register_operand" "r")))]
  ""
  "padd<vecsize>.uuu %0 = %1, %2"
  [(set_attr "itanium_class" "mmalua")])

(define_insn "sub<mode>3"
  [(set (match_operand:VECINT 0 "gr_register_operand" "=r")
	(minus:VECINT (match_operand:VECINT 1 "gr_register_operand" "r")
		      (match_operand:VECINT 2 "gr_register_operand" "r")))]
  ""
  "psub<vecsize> %0 = %1, %2"
  [(set_attr "itanium_class" "mmalua")])

(define_insn "*sssub<mode>3"
  [(set (match_operand:VECINT12 0 "gr_register_operand" "=r")
	(ss_minus:VECINT12
	  (match_operand:VECINT12 1 "gr_register_operand" "r")
	  (match_operand:VECINT12 2 "gr_register_operand" "r")))]
  ""
  "psub<vecsize>.sss %0 = %1, %2"
  [(set_attr "itanium_class" "mmalua")])

(define_insn "*ussub<mode>3"
  [(set (match_operand:VECINT12 0 "gr_register_operand" "=r")
	(us_minus:VECINT12
	  (match_operand:VECINT12 1 "gr_register_operand" "r")
	  (match_operand:VECINT12 2 "gr_register_operand" "r")))]
  ""
  "psub<vecsize>.uuu %0 = %1, %2"
  [(set_attr "itanium_class" "mmalua")])

(define_expand "mulv8qi3"
  [(set (match_operand:V8QI 0 "gr_register_operand" "")
	(mult:V8QI (match_operand:V8QI 1 "gr_register_operand" "r")
		   (match_operand:V8QI 2 "gr_register_operand" "r")))]
  ""
{
  rtx l = gen_reg_rtx (V4HImode);
  rtx h = gen_reg_rtx (V4HImode);
  emit_insn (gen_vec_widen_umult_lo_v8qi (l, operands[1], operands[2]));
  emit_insn (gen_vec_widen_umult_hi_v8qi (h, operands[1], operands[2]));
  if (TARGET_BIG_ENDIAN)
    emit_insn (gen_vec_pack_trunc_v4hi (operands[0], h, l));
  else
    emit_insn (gen_vec_pack_trunc_v4hi (operands[0], l, h));
  DONE;
})

(define_expand "vec_widen_umult_lo_v8qi"
  [(match_operand:V4HI 0 "gr_register_operand" "")
   (match_operand:V8QI 1 "gr_register_operand" "")
   (match_operand:V8QI 2 "gr_register_operand" "")]
  ""
{
  rtx op1 = gen_reg_rtx (V4HImode);
  rtx op2 = gen_reg_rtx (V4HImode);
  emit_insn (gen_vec_unpacku_lo_v8qi (op1, operands[1]));
  emit_insn (gen_vec_unpacku_lo_v8qi (op2, operands[2]));
  emit_insn (gen_mulv4hi3 (operands[0], op1, op2));
  DONE;
});
  
(define_expand "vec_widen_umult_hi_v8qi"
  [(match_operand:V4HI 0 "gr_register_operand" "")
   (match_operand:V8QI 1 "gr_register_operand" "")
   (match_operand:V8QI 2 "gr_register_operand" "")]
  ""
{
  rtx op1 = gen_reg_rtx (V4HImode);
  rtx op2 = gen_reg_rtx (V4HImode);
  emit_insn (gen_vec_unpacku_hi_v8qi (op1, operands[1]));
  emit_insn (gen_vec_unpacku_hi_v8qi (op2, operands[2]));
  emit_insn (gen_mulv4hi3 (operands[0], op1, op2));
  DONE;
});
  
(define_expand "vec_widen_smult_lo_v8qi"
  [(match_operand:V4HI 0 "gr_register_operand" "")
   (match_operand:V8QI 1 "gr_register_operand" "")
   (match_operand:V8QI 2 "gr_register_operand" "")]
  ""
{
  rtx op1 = gen_reg_rtx (V4HImode);
  rtx op2 = gen_reg_rtx (V4HImode);
  emit_insn (gen_vec_unpacks_lo_v8qi (op1, operands[1]));
  emit_insn (gen_vec_unpacks_lo_v8qi (op2, operands[2]));
  emit_insn (gen_mulv4hi3 (operands[0], op1, op2));
  DONE;
});
  
(define_expand "vec_widen_smult_hi_v8qi"
  [(match_operand:V4HI 0 "gr_register_operand" "")
   (match_operand:V8QI 1 "gr_register_operand" "")
   (match_operand:V8QI 2 "gr_register_operand" "")]
  ""
{
  rtx op1 = gen_reg_rtx (V4HImode);
  rtx op2 = gen_reg_rtx (V4HImode);
  emit_insn (gen_vec_unpacks_hi_v8qi (op1, operands[1]));
  emit_insn (gen_vec_unpacks_hi_v8qi (op2, operands[2]));
  emit_insn (gen_mulv4hi3 (operands[0], op1, op2));
  DONE;
});
  
(define_insn "mulv4hi3"
  [(set (match_operand:V4HI 0 "gr_register_operand" "=r")
	(mult:V4HI (match_operand:V4HI 1 "gr_register_operand" "r")
		   (match_operand:V4HI 2 "gr_register_operand" "r")))]
  ""
  "pmpyshr2 %0 = %1, %2, 0"
  [(set_attr "itanium_class" "mmmul")])

(define_insn "pmpyshr2"
  [(set (match_operand:V4HI 0 "gr_register_operand" "=r")
	(truncate:V4HI
	  (ashiftrt:V4SI
	    (mult:V4SI
	      (sign_extend:V4SI
		(match_operand:V4HI 1 "gr_register_operand" "r"))
	      (sign_extend:V4SI
		(match_operand:V4HI 2 "gr_register_operand" "r")))
	    (match_operand:SI 3 "pmpyshr_operand" "n"))))]
  ""
  "pmpyshr2 %0 = %1, %2, %3"
  [(set_attr "itanium_class" "mmmul")])

(define_insn "pmpyshr2_u"
  [(set (match_operand:V4HI 0 "gr_register_operand" "=r")
	(truncate:V4HI
	  (lshiftrt:V4SI
	    (mult:V4SI
	      (zero_extend:V4SI
		(match_operand:V4HI 1 "gr_register_operand" "r"))
	      (zero_extend:V4SI
		(match_operand:V4HI 2 "gr_register_operand" "r")))
	    (match_operand:SI 3 "pmpyshr_operand" "n"))))]
  ""
  "pmpyshr2.u %0 = %1, %2, %3"
  [(set_attr "itanium_class" "mmmul")])

(define_expand "smulv4hi3_highpart"
  [(match_operand:V4HI 0 "gr_register_operand")
   (match_operand:V4HI 1 "gr_register_operand")
   (match_operand:V4HI 2 "gr_register_operand")]
  ""
{
  emit_insn (gen_pmpyshr2 (operands[0], operands[1],
			   operands[2], GEN_INT (16)));
  DONE;
})

(define_expand "umulv4hi3_highpart"
  [(match_operand:V4HI 0 "gr_register_operand")
   (match_operand:V4HI 1 "gr_register_operand")
   (match_operand:V4HI 2 "gr_register_operand")]
  ""
{
  emit_insn (gen_pmpyshr2_u (operands[0], operands[1],
			     operands[2], GEN_INT (16)));
  DONE;
})

(define_insn "vec_widen_smult_even_v4hi"
  [(set (match_operand:V2SI 0 "gr_register_operand" "=r")
	(mult:V2SI
	  (vec_select:V2SI
	    (sign_extend:V4SI
	      (match_operand:V4HI 1 "gr_register_operand" "r"))
	    (parallel [(const_int 0) (const_int 2)]))
	  (vec_select:V2SI
	    (sign_extend:V4SI
	      (match_operand:V4HI 2 "gr_register_operand" "r"))
	    (parallel [(const_int 0) (const_int 2)]))))]
  ""
{
  /* Recall that vector elements are numbered in memory order.  */
  if (TARGET_BIG_ENDIAN)
    return "%,pmpy2.l %0 = %1, %2";
  else
    return "%,pmpy2.r %0 = %1, %2";
}
  [(set_attr "itanium_class" "mmshf")])

(define_insn "vec_widen_smult_odd_v4hi"
  [(set (match_operand:V2SI 0 "gr_register_operand" "=r")
	(mult:V2SI
	  (vec_select:V2SI
	    (sign_extend:V4SI
	      (match_operand:V4HI 1 "gr_register_operand" "r"))
	    (parallel [(const_int 1) (const_int 3)]))
	  (vec_select:V2SI
	    (sign_extend:V4SI
	      (match_operand:V4HI 2 "gr_register_operand" "r"))
	    (parallel [(const_int 1) (const_int 3)]))))]
  ""
{
  /* Recall that vector elements are numbered in memory order.  */
  if (TARGET_BIG_ENDIAN)
    return "%,pmpy2.r %0 = %1, %2";
  else
    return "%,pmpy2.l %0 = %1, %2";
}
  [(set_attr "itanium_class" "mmshf")])

(define_expand "vec_widen_smult_lo_v4hi"
  [(match_operand:V2SI 0 "gr_register_operand" "")
   (match_operand:V4HI 1 "gr_register_operand" "")
   (match_operand:V4HI 2 "gr_register_operand" "")]
  ""
{
  rtx l = gen_reg_rtx (V4HImode);
  rtx h = gen_reg_rtx (V4HImode);
  emit_insn (gen_mulv4hi3 (l, operands[1], operands[2]));
  emit_insn (gen_pmpyshr2 (h, operands[1], operands[2], GEN_INT (16)));
  ia64_unpack_assemble (operands[0], l, h, false);
  DONE;
})

(define_expand "vec_widen_smult_hi_v4hi"
  [(match_operand:V2SI 0 "gr_register_operand" "")
   (match_operand:V4HI 1 "gr_register_operand" "")
   (match_operand:V4HI 2 "gr_register_operand" "")]
  ""
{
  rtx l = gen_reg_rtx (V4HImode);
  rtx h = gen_reg_rtx (V4HImode);
  emit_insn (gen_mulv4hi3 (l, operands[1], operands[2]));
  emit_insn (gen_pmpyshr2 (h, operands[1], operands[2], GEN_INT (16)));
  ia64_unpack_assemble (operands[0], l, h, true);
  DONE;
})

(define_expand "vec_widen_umult_lo_v4hi"
  [(match_operand:V2SI 0 "gr_register_operand" "")
   (match_operand:V4HI 1 "gr_register_operand" "")
   (match_operand:V4HI 2 "gr_register_operand" "")]
  ""
{
  rtx l = gen_reg_rtx (V4HImode);
  rtx h = gen_reg_rtx (V4HImode);
  emit_insn (gen_mulv4hi3 (l, operands[1], operands[2]));
  emit_insn (gen_pmpyshr2_u (h, operands[1], operands[2], GEN_INT (16)));
  ia64_unpack_assemble (operands[0], l, h, false);
  DONE;
})

(define_expand "vec_widen_umult_hi_v4hi"
  [(match_operand:V2SI 0 "gr_register_operand" "")
   (match_operand:V4HI 1 "gr_register_operand" "")
   (match_operand:V4HI 2 "gr_register_operand" "")]
  ""
{
  rtx l = gen_reg_rtx (V4HImode);
  rtx h = gen_reg_rtx (V4HImode);
  emit_insn (gen_mulv4hi3 (l, operands[1], operands[2]));
  emit_insn (gen_pmpyshr2_u (h, operands[1], operands[2], GEN_INT (16)));
  ia64_unpack_assemble (operands[0], l, h, true);
  DONE;
})

(define_expand "mulv2si3"
  [(set (match_operand:V2SI 0 "gr_register_operand" "")
	(mult:V2SI (match_operand:V2SI 1 "gr_register_operand" "r")
		   (match_operand:V2SI 2 "gr_register_operand" "r")))]
  ""
{
  rtx t0, t1, t2, t3, t4, t5, t6, t7, x;
  rtx op1h = gen_lowpart (V4HImode, operands[1]);
  rtx op2h = gen_lowpart (V4HImode, operands[2]);

  t0 = gen_reg_rtx (V4HImode);
  t1 = gen_reg_rtx (V4HImode);
  t2 = gen_reg_rtx (V4HImode);
  t3 = gen_reg_rtx (V4HImode);
  t4 = gen_reg_rtx (V2SImode);
  t5 = gen_reg_rtx (V2SImode);
  t6 = gen_reg_rtx (V2SImode);
  t7 = gen_reg_rtx (V2SImode);

  /* Consider the HImode components of op1 = DCBA, op2 = ZYXW.
     Consider .l and .h suffixes below the low and high 16 bits
     of the full 32-bit product.  */

  /* T0 = CDBA.  */
  x = gen_rtx_PARALLEL (VOIDmode, gen_rtvec (4, const1_rtx, const0_rtx,
					     GEN_INT (3), const2_rtx));
  x = gen_rtx_VEC_SELECT (V4HImode, op1h, x);
  emit_insn (gen_rtx_SET (t0, x));

  /* T1 = DZ.l, CY.l, BX.l, AW.l.  */
  emit_insn (gen_mulv4hi3 (t1, op1h, op2h));

  /* T2 = DZ.h, CY.h, BX.h, AW.h.  */
  emit_insn (gen_pmpyshr2_u (t2, op1h, op2h, GEN_INT (16)));

  /* T3 = CZ.l, DY.l, AX.l, BW.l.  */
  emit_insn (gen_mulv4hi3 (t3, t0, op2h));

  /* T4 = CY.h, CY.l, AW.h, AW.l = CY, AW.  */
  x = gen_lowpart (V4HImode, t4);
  if (TARGET_BIG_ENDIAN)
    x = gen_mix2_odd (x, t2, t1);
  else
    x = gen_mix2_even (x, t1, t2);
  emit_insn (x);

  /* T5 = CZ.l, 0, AX.l, 0 = CZ << 16, AX << 16.  */
  x = gen_lowpart (V4HImode, t5);
  if (TARGET_BIG_ENDIAN)
    x = gen_mix2_even (x, t3, CONST0_RTX (V4HImode));
  else
    x = gen_mix2_odd (x, CONST0_RTX (V4HImode), t3);
  emit_insn (x);

  /* T6 = DY.l, 0, BW.l, 0 = DY << 16, BW << 16.  */
  x = gen_lowpart (V4HImode, t6);
  if (TARGET_BIG_ENDIAN)
    x = gen_mix2_odd (x, t3, CONST0_RTX (V4HImode));
  else
    x = gen_mix2_even (x, CONST0_RTX (V4HImode), t3);
  emit_insn (x);

  emit_insn (gen_addv2si3 (t7, t4, t5));
  emit_insn (gen_addv2si3 (operands[0], t6, t7));
  DONE;
})

(define_expand "umax<mode>3"
  [(set (match_operand:VECINT 0 "gr_register_operand" "")
	(umax:VECINT (match_operand:VECINT 1 "gr_register_operand" "")
		     (match_operand:VECINT 2 "gr_register_operand" "")))]
  ""
{
  if (ia64_expand_vecint_minmax (UMAX, <MODE>mode, operands))
    DONE;
})

(define_expand "smax<mode>3"
  [(set (match_operand:VECINT 0 "gr_register_operand" "")
	(smax:VECINT (match_operand:VECINT 1 "gr_reg_or_0_operand" "")
		     (match_operand:VECINT 2 "gr_reg_or_0_operand" "")))]
  ""
{
  if (ia64_expand_vecint_minmax (SMAX, <MODE>mode, operands))
    DONE;
})

(define_expand "umin<mode>3"
  [(set (match_operand:VECINT 0 "gr_register_operand" "")
	(umin:VECINT (match_operand:VECINT 1 "gr_register_operand" "")
		     (match_operand:VECINT 2 "gr_register_operand" "")))]
  ""
{
  if (ia64_expand_vecint_minmax (UMIN, <MODE>mode, operands))
    DONE;
})

(define_expand "smin<mode>3"
  [(set (match_operand:VECINT 0 "gr_register_operand" "")
	(smin:VECINT (match_operand:VECINT 1 "gr_reg_or_0_operand" "")
		     (match_operand:VECINT 2 "gr_reg_or_0_operand" "")))]
  ""
{
  if (ia64_expand_vecint_minmax (SMIN, <MODE>mode, operands))
    DONE;
})

(define_insn "*umaxv8qi3"
  [(set (match_operand:V8QI 0 "gr_register_operand" "=r")
	(umax:V8QI (match_operand:V8QI 1 "gr_register_operand" "r")
		   (match_operand:V8QI 2 "gr_register_operand" "r")))]
  ""
  "pmax1.u %0 = %1, %2"
  [(set_attr "itanium_class" "mmshf")])

(define_insn "*smaxv4hi3"
  [(set (match_operand:V4HI 0 "gr_register_operand" "=r")
	(smax:V4HI (match_operand:V4HI 1 "gr_reg_or_0_operand" "rU")
		   (match_operand:V4HI 2 "gr_reg_or_0_operand" "rU")))]
  ""
  "pmax2 %0 = %r1, %r2"
  [(set_attr "itanium_class" "mmshf")])

(define_insn "*uminv8qi3"
  [(set (match_operand:V8QI 0 "gr_register_operand" "=r")
	(umin:V8QI (match_operand:V8QI 1 "gr_register_operand" "r")
		   (match_operand:V8QI 2 "gr_register_operand" "r")))]
  ""
  "pmin1.u %0 = %1, %2"
  [(set_attr "itanium_class" "mmshf")])

(define_insn "*sminv4hi3"
  [(set (match_operand:V4HI 0 "gr_register_operand" "=r")
	(smin:V4HI (match_operand:V4HI 1 "gr_reg_or_0_operand" "rU")
		   (match_operand:V4HI 2 "gr_reg_or_0_operand" "rU")))]
  ""
  "pmin2 %0 = %r1, %r2"
  [(set_attr "itanium_class" "mmshf")])

(define_insn "ashl<mode>3"
  [(set (match_operand:VECINT24 0 "gr_register_operand" "=r")
	(ashift:VECINT24
	  (match_operand:VECINT24 1 "gr_register_operand" "r")
	  (match_operand:DI 2 "gr_reg_or_5bit_operand" "rn")))]
  ""
  "pshl<vecsize> %0 = %1, %2"
  [(set_attr "itanium_class" "mmshf")])

(define_insn "ashr<mode>3"
  [(set (match_operand:VECINT24 0 "gr_register_operand" "=r")
	(ashiftrt:VECINT24
	  (match_operand:VECINT24 1 "gr_register_operand" "r")
	  (match_operand:DI 2 "gr_reg_or_5bit_operand" "rn")))]
  ""
  "pshr<vecsize> %0 = %1, %2"
  [(set_attr "itanium_class" "mmshf")])

(define_insn "lshr<mode>3"
  [(set (match_operand:VECINT24 0 "gr_register_operand" "=r")
	(lshiftrt:VECINT24
	  (match_operand:VECINT24 1 "gr_register_operand" "r")
	  (match_operand:DI 2 "gr_reg_or_5bit_operand" "rn")))]
  ""
  "pshr<vecsize>.u %0 = %1, %2"
  [(set_attr "itanium_class" "mmshf")])

(define_expand "vec_shl_<mode>"
  [(set (match_operand:VECINT 0 "gr_register_operand" "")
	(ashift:DI (match_operand:VECINT 1 "gr_register_operand" "")
		   (match_operand:DI 2 "gr_reg_or_6bit_operand" "")))]
  ""
{
  operands[0] = gen_lowpart (DImode, operands[0]);
  operands[1] = gen_lowpart (DImode, operands[1]);
})

(define_expand "vec_shr_<mode>"
  [(set (match_operand:VECINT 0 "gr_register_operand" "")
        (lshiftrt:DI (match_operand:VECINT 1 "gr_register_operand" "")
                     (match_operand:DI 2 "gr_reg_or_6bit_operand" "")))]
  ""
{
  operands[0] = gen_lowpart (DImode, operands[0]);
  operands[1] = gen_lowpart (DImode, operands[1]);
})

(define_expand "widen_usumv8qi3"
  [(match_operand:V4HI 0 "gr_register_operand" "")
   (match_operand:V8QI 1 "gr_register_operand" "")
   (match_operand:V4HI 2 "gr_register_operand" "")]
  ""
{
  ia64_expand_widen_sum (operands, true);
  DONE;
})

(define_expand "widen_usumv4hi3"
  [(match_operand:V2SI 0 "gr_register_operand" "")
   (match_operand:V4HI 1 "gr_register_operand" "")
   (match_operand:V2SI 2 "gr_register_operand" "")]
  ""
{
  ia64_expand_widen_sum (operands, true);
  DONE;
})

(define_expand "widen_ssumv8qi3"
  [(match_operand:V4HI 0 "gr_register_operand" "")
   (match_operand:V8QI 1 "gr_register_operand" "")
   (match_operand:V4HI 2 "gr_register_operand" "")]
  ""
{
  ia64_expand_widen_sum (operands, false);
  DONE;
})

(define_expand "widen_ssumv4hi3"
  [(match_operand:V2SI 0 "gr_register_operand" "")
   (match_operand:V4HI 1 "gr_register_operand" "")
   (match_operand:V2SI 2 "gr_register_operand" "")]
  ""
{
  ia64_expand_widen_sum (operands, false);
  DONE;
})

(define_expand "vcond<mode><mode>"
  [(set (match_operand:VECINT 0 "gr_register_operand" "")
	(if_then_else:VECINT
	  (match_operator 3 "" 
	    [(match_operand:VECINT 4 "gr_reg_or_0_operand" "")
	     (match_operand:VECINT 5 "gr_reg_or_0_operand" "")])
	  (match_operand:VECINT 1 "gr_reg_or_0_operand" "")
	  (match_operand:VECINT 2 "gr_reg_or_0_operand" "")))]
  ""
{
  ia64_expand_vecint_cmov (operands);
  DONE;
})

(define_expand "vcondu<mode><mode>"
  [(set (match_operand:VECINT 0 "gr_register_operand" "")
	(if_then_else:VECINT
	  (match_operator 3 "" 
	    [(match_operand:VECINT 4 "gr_reg_or_0_operand" "")
	     (match_operand:VECINT 5 "gr_reg_or_0_operand" "")])
	  (match_operand:VECINT 1 "gr_reg_or_0_operand" "")
	  (match_operand:VECINT 2 "gr_reg_or_0_operand" "")))]
  ""
{
  ia64_expand_vecint_cmov (operands);
  DONE;
})

(define_insn "*cmpeq_<mode>"
  [(set (match_operand:VECINT 0 "gr_register_operand" "=r")
	(eq:VECINT (match_operand:VECINT 1 "gr_reg_or_0_operand" "rU")
		   (match_operand:VECINT 2 "gr_reg_or_0_operand" "rU")))]
  ""
  "pcmp<vecsize>.eq %0 = %r1, %r2"
  [(set_attr "itanium_class" "mmalua")])

(define_insn "*cmpgt_<mode>"
  [(set (match_operand:VECINT 0 "gr_register_operand" "=r")
	(gt:VECINT (match_operand:VECINT 1 "gr_reg_or_0_operand" "rU")
		   (match_operand:VECINT 2 "gr_reg_or_0_operand" "rU")))]
  ""
  "pcmp<vecsize>.gt %0 = %r1, %r2"
  [(set_attr "itanium_class" "mmalua")])

(define_insn "vec_pack_ssat_v4hi"
  [(set (match_operand:V8QI 0 "gr_register_operand" "=r")
	(vec_concat:V8QI
	  (ss_truncate:V4QI
	    (match_operand:V4HI 1 "gr_reg_or_0_operand" "rU"))
	  (ss_truncate:V4QI
	    (match_operand:V4HI 2 "gr_reg_or_0_operand" "rU"))))]
  ""
{
  /* Recall that vector elements are numbered in memory order.  */
  if (TARGET_BIG_ENDIAN)
    return "%,pack2.sss %0 = %r2, %r1";
  else
    return "%,pack2.sss %0 = %r1, %r2";
}
  [(set_attr "itanium_class" "mmshf")])

(define_insn "vec_pack_usat_v4hi"
  [(set (match_operand:V8QI 0 "gr_register_operand" "=r")
	(vec_concat:V8QI
	  (us_truncate:V4QI
	    (match_operand:V4HI 1 "gr_reg_or_0_operand" "rU"))
	  (us_truncate:V4QI
	    (match_operand:V4HI 2 "gr_reg_or_0_operand" "rU"))))]
  ""
{
  /* Recall that vector elements are numbered in memory order.  */
  if (TARGET_BIG_ENDIAN)
    return "%,pack2.uss %0 = %r2, %r1";
  else
    return "%,pack2.uss %0 = %r1, %r2";
}
  [(set_attr "itanium_class" "mmshf")])

(define_insn "vec_pack_ssat_v2si"
  [(set (match_operand:V4HI 0 "gr_register_operand" "=r")
	(vec_concat:V4HI
	  (ss_truncate:V2HI
	    (match_operand:V2SI 1 "gr_reg_or_0_operand" "rU"))
	  (ss_truncate:V2HI
	    (match_operand:V2SI 2 "gr_reg_or_0_operand" "rU"))))]
  ""
{
  /* Recall that vector elements are numbered in memory order.  */
  if (TARGET_BIG_ENDIAN)
    return "%,pack4.sss %0 = %r2, %r1";
  else
    return "%,pack4.sss %0 = %r1, %r2";
}
  [(set_attr "itanium_class" "mmshf")])

(define_insn "*vec_interleave_lowv8qi"
  [(set (match_operand:V8QI 0 "gr_register_operand" "=r")
	(vec_select:V8QI
	  (vec_concat:V16QI
	    (match_operand:V8QI 1 "gr_reg_or_0_operand" "rU")
	    (match_operand:V8QI 2 "gr_reg_or_0_operand" "rU"))
	  (parallel [(const_int 0) (const_int 8)
		     (const_int 1) (const_int 9)
		     (const_int 2) (const_int 10)
		     (const_int 3) (const_int 11)])))]
  ""
{
  /* Recall that vector elements are numbered in memory order.  */
  if (TARGET_BIG_ENDIAN)
    return "%,unpack1.l %0 = %r1, %r2";
  else
    return "%,unpack1.l %0 = %r2, %r1";
}
  [(set_attr "itanium_class" "mmshf")])

(define_insn "*vec_interleave_highv8qi"
  [(set (match_operand:V8QI 0 "gr_register_operand" "=r")
	(vec_select:V8QI
	  (vec_concat:V16QI
	    (match_operand:V8QI 1 "gr_reg_or_0_operand" "rU")
	    (match_operand:V8QI 2 "gr_reg_or_0_operand" "rU"))
	  (parallel [(const_int 4) (const_int 12)
		     (const_int 5) (const_int 13)
		     (const_int 6) (const_int 14)
		     (const_int 7) (const_int 15)])))]
  ""
{
  /* Recall that vector elements are numbered in memory order.  */
  if (TARGET_BIG_ENDIAN)
    return "%,unpack1.h %0 = %r1, %r2";
  else
    return "%,unpack1.h %0 = %r2, %r1";
}
  [(set_attr "itanium_class" "mmshf")])

(define_insn "*mix1_even"
  [(set (match_operand:V8QI 0 "gr_register_operand" "=r")
	(vec_select:V8QI
	  (vec_concat:V16QI
	    (match_operand:V8QI 1 "gr_reg_or_0_operand" "rU")
	    (match_operand:V8QI 2 "gr_reg_or_0_operand" "rU"))
	  (parallel [(const_int 0) (const_int 8)
		     (const_int 2) (const_int 10)
		     (const_int 4) (const_int 12)
		     (const_int 6) (const_int 14)])))]
  ""
{
  /* Recall that vector elements are numbered in memory order.  */
  if (TARGET_BIG_ENDIAN)
    return "%,mix1.l %0 = %r1, %r2";
  else
    return "%,mix1.r %0 = %r2, %r1";
}
  [(set_attr "itanium_class" "mmshf")])

(define_insn "*mix1_odd"
  [(set (match_operand:V8QI 0 "gr_register_operand" "=r")
	(vec_select:V8QI
	  (vec_concat:V16QI
	    (match_operand:V8QI 1 "gr_reg_or_0_operand" "rU")
	    (match_operand:V8QI 2 "gr_reg_or_0_operand" "rU"))
	  (parallel [(const_int 1) (const_int 9)
		     (const_int 3) (const_int 11)
		     (const_int 5) (const_int 13)
		     (const_int 7) (const_int 15)])))]
  ""
{
  /* Recall that vector elements are numbered in memory order.  */
  if (TARGET_BIG_ENDIAN)
    return "%,mix1.r %0 = %r1, %r2";
  else
    return "%,mix1.l %0 = %r2, %r1";
}
  [(set_attr "itanium_class" "mmshf")])

(define_insn "*mux1_rev"
  [(set (match_operand:V8QI 0 "gr_register_operand" "=r")
	(vec_select:V8QI
	  (match_operand:V8QI 1 "gr_register_operand" "r")
	  (parallel [(const_int 7) (const_int 6)
		     (const_int 5) (const_int 4)
		     (const_int 3) (const_int 2)
		     (const_int 1) (const_int 0)])))]
  ""
  "mux1 %0 = %1, @rev"
  [(set_attr "itanium_class" "mmshf")])

(define_insn "*mux1_mix"
  [(set (match_operand:V8QI 0 "gr_register_operand" "=r")
	(vec_select:V8QI
	  (match_operand:V8QI 1 "gr_register_operand" "r")
	  (parallel [(const_int 0) (const_int 4)
		     (const_int 2) (const_int 6)
		     (const_int 1) (const_int 5)
		     (const_int 3) (const_int 7)])))]
  ""
  "mux1 %0 = %1, @mix"
  [(set_attr "itanium_class" "mmshf")])

(define_insn "*mux1_shuf"
  [(set (match_operand:V8QI 0 "gr_register_operand" "=r")
	(vec_select:V8QI
	  (match_operand:V8QI 1 "gr_register_operand" "r")
	  (parallel [(const_int 0) (const_int 4)
		     (const_int 1) (const_int 5)
		     (const_int 2) (const_int 6)
		     (const_int 3) (const_int 7)])))]
  ""
  "mux1 %0 = %1, @shuf"
  [(set_attr "itanium_class" "mmshf")])

(define_insn "*mux1_alt"
  [(set (match_operand:V8QI 0 "gr_register_operand" "=r")
	(vec_select:V8QI
	  (match_operand:V8QI 1 "gr_register_operand" "r")
	  (parallel [(const_int 0) (const_int 2)
		     (const_int 4) (const_int 6)
		     (const_int 1) (const_int 3)
		     (const_int 5) (const_int 7)])))]
  ""
  "mux1 %0 = %1, @alt"
  [(set_attr "itanium_class" "mmshf")])

(define_insn "*mux1_brcst_v8qi"
  [(set (match_operand:V8QI 0 "gr_register_operand" "=r")
	(vec_select:V8QI
	  (match_operand:V8QI 1 "gr_register_operand" "r")
	  (parallel [(match_operand 2 "mux1_brcst_element" "")
		     (match_dup 2)
		     (match_dup 2)
		     (match_dup 2)
		     (match_dup 2)
		     (match_dup 2)
		     (match_dup 2)
		     (match_dup 2)])))]
  ""
  "mux1 %0 = %1, @brcst"
  [(set_attr "itanium_class" "mmshf")])

(define_insn "mux1_brcst_qi"
  [(set (match_operand:V8QI 0 "gr_register_operand" "=r")
	(vec_duplicate:V8QI
	  (match_operand:QI 1 "gr_register_operand" "r")))]
  ""
  "mux1 %0 = %1, @brcst"
  [(set_attr "itanium_class" "mmshf")])

(define_insn "*vec_interleave_lowv4hi"
  [(set (match_operand:V4HI 0 "gr_register_operand" "=r")
	(vec_select:V4HI
	  (vec_concat:V8HI
	    (match_operand:V4HI 1 "gr_reg_or_0_operand" "rU")
	    (match_operand:V4HI 2 "gr_reg_or_0_operand" "rU"))
	  (parallel [(const_int 0) (const_int 4)
		     (const_int 1) (const_int 5)])))]
  ""
{
  /* Recall that vector elements are numbered in memory order.  */
  if (TARGET_BIG_ENDIAN)
    return "%,unpack2.l %0 = %r1, %r2";
  else
    return "%,unpack2.l %0 = %r2, %r1";
}
  [(set_attr "itanium_class" "mmshf")])

(define_insn "*vec_interleave_highv4hi"
  [(set (match_operand:V4HI 0 "gr_register_operand" "=r")
	(vec_select:V4HI
	  (vec_concat:V8HI
	    (match_operand:V4HI 1 "gr_reg_or_0_operand" "rU")
	    (match_operand:V4HI 2 "gr_reg_or_0_operand" "rU"))
	  (parallel [(const_int 2) (const_int 6)
		     (const_int 3) (const_int 7)])))]
  ""
{
  /* Recall that vector elements are numbered in memory order.  */
  if (TARGET_BIG_ENDIAN)
    return "%,unpack2.h %0 = %r1, %r2";
  else
    return "%,unpack2.h %0 = %r2, %r1";
}
  [(set_attr "itanium_class" "mmshf")])

(define_insn "mix2_even"
  [(set (match_operand:V4HI 0 "gr_register_operand" "=r")
	(vec_select:V4HI
	  (vec_concat:V8HI
	    (match_operand:V4HI 1 "gr_reg_or_0_operand" "rU")
	    (match_operand:V4HI 2 "gr_reg_or_0_operand" "rU"))
	  (parallel [(const_int 0) (const_int 4)
		     (const_int 2) (const_int 6)])))]
  ""
{
  /* Recall that vector elements are numbered in memory order.  */
  if (TARGET_BIG_ENDIAN)
    return "%,mix2.l %0 = %r1, %r2";
  else
    return "%,mix2.r %0 = %r2, %r1";
}
  [(set_attr "itanium_class" "mmshf")])

(define_insn "mix2_odd"
  [(set (match_operand:V4HI 0 "gr_register_operand" "=r")
	(vec_select:V4HI
	  (vec_concat:V8HI
	    (match_operand:V4HI 1 "gr_reg_or_0_operand" "rU")
	    (match_operand:V4HI 2 "gr_reg_or_0_operand" "rU"))
	  (parallel [(const_int 1) (const_int 5)
		     (const_int 3) (const_int 7)])))]
  ""
{
  /* Recall that vector elements are numbered in memory order.  */
  if (TARGET_BIG_ENDIAN)
    return "%,mix2.r %0 = %r1, %r2";
  else
    return "%,mix2.l %0 = %r2, %r1";
}
  [(set_attr "itanium_class" "mmshf")])

(define_insn "*mux2"
  [(set (match_operand:V4HI 0 "gr_register_operand" "=r")
	(vec_select:V4HI
	  (match_operand:V4HI 1 "gr_register_operand" "r")
	  (parallel [(match_operand 2 "const_int_2bit_operand" "")
		     (match_operand 3 "const_int_2bit_operand" "")
		     (match_operand 4 "const_int_2bit_operand" "")
		     (match_operand 5 "const_int_2bit_operand" "")])))]
  ""
{
  int mask = 0;
  if (TARGET_BIG_ENDIAN)
    {
      mask |= (3 - INTVAL (operands[2])) << 6;
      mask |= (3 - INTVAL (operands[3])) << 4;
      mask |= (3 - INTVAL (operands[4])) << 2;
      mask |= 3 - INTVAL (operands[5]);
    }
  else
    {
      mask |= INTVAL (operands[2]);
      mask |= INTVAL (operands[3]) << 2;
      mask |= INTVAL (operands[4]) << 4;
      mask |= INTVAL (operands[5]) << 6;
    }
  operands[2] = GEN_INT (mask);
  return "%,mux2 %0 = %1, %2";
}
  [(set_attr "itanium_class" "mmshf")])

(define_insn "*mux2_brcst_hi"
  [(set (match_operand:V4HI 0 "gr_register_operand" "=r")
	(vec_duplicate:V4HI
	  (match_operand:HI 1 "gr_register_operand" "r")))]
  ""
  "mux2 %0 = %1, 0"
  [(set_attr "itanium_class" "mmshf")])

(define_insn "*vec_interleave_lowv2si"
  [(set (match_operand:V2SI 0 "gr_register_operand" "=r")
	(vec_select:V2SI
	  (vec_concat:V4SI
	    (match_operand:V2SI 1 "gr_reg_or_0_operand" "rU")
	    (match_operand:V2SI 2 "gr_reg_or_0_operand" "rU"))
	  (parallel [(const_int 0) (const_int 2)])))]
  ""
{
  /* Recall that vector elements are numbered in memory order.  */
  if (TARGET_BIG_ENDIAN)
    return "%,unpack4.l %0 = %r1, %r2";
  else
    return "%,unpack4.l %0 = %r2, %r1";
}
  [(set_attr "itanium_class" "mmshf")])

(define_insn "*vec_interleave_highv2si"
  [(set (match_operand:V2SI 0 "gr_register_operand" "=r")
	(vec_select:V2SI
	  (vec_concat:V4SI
	    (match_operand:V2SI 1 "gr_reg_or_0_operand" "rU")
	    (match_operand:V2SI 2 "gr_reg_or_0_operand" "rU"))
	  (parallel [(const_int 1) (const_int 3)])))]
  ""
{
  /* Recall that vector elements are numbered in memory order.  */
  if (TARGET_BIG_ENDIAN)
    return "%,unpack4.h %0 = %r1, %r2";
  else
    return "%,unpack4.h %0 = %r2, %r1";
}
  [(set_attr "itanium_class" "mmshf")])

(define_expand "vec_initv2sisi"
  [(match_operand:V2SI 0 "gr_register_operand" "")
   (match_operand 1 "" "")]
  ""
{
  rtx op1 = XVECEXP (operands[1], 0, 0);
  rtx op2 = XVECEXP (operands[1], 0, 1);
  rtx x;

  if (GET_CODE (op1) == CONST_INT && GET_CODE (op2) == CONST_INT)
    {
      x = gen_rtx_CONST_VECTOR (V2SImode, XVEC (operands[1], 0));
      emit_move_insn (operands[0], x);
      DONE;
    }

  if (!gr_reg_or_0_operand (op1, SImode))
    op1 = force_reg (SImode, op1);
  if (!gr_reg_or_0_operand (op2, SImode))
    op2 = force_reg (SImode, op2);

  x = gen_rtx_VEC_CONCAT (V2SImode, op1, op2);
  emit_insn (gen_rtx_SET (operands[0], x));
  DONE;
})

(define_insn "*vecinit_v2si"
  [(set (match_operand:V2SI 0 "gr_register_operand" "=r")
	(vec_concat:V2SI
	  (match_operand:SI 1 "gr_reg_or_0_operand" "rO")
	  (match_operand:SI 2 "gr_reg_or_0_operand" "rO")))]
  ""
{
  /* Recall that vector elements are numbered in memory order.  */
  if (TARGET_BIG_ENDIAN)
    return "%,unpack4.l %0 = %r1, %r2";
  else
    return "%,unpack4.l %0 = %r2, %r1";
}
  [(set_attr "itanium_class" "mmshf")])

;; Missing operations
;; padd.uus
;; pavg
;; pavgsub
;; psad
;; pshladd
;; pshradd
;; psub.uus

;; Floating point vector operations

(define_expand "movv2sf"
  [(set (match_operand:V2SF 0 "general_operand" "")
        (match_operand:V2SF 1 "general_operand" ""))]
  ""
{
  rtx op1 = ia64_expand_move (operands[0], operands[1]);
  if (!op1)
    DONE;
  operands[1] = op1;
})

(define_insn "*movv2sf_internal"
  [(set (match_operand:V2SF 0 "destination_operand"
					"=f,f,f,Q,*r ,*r,*r,*r,m ,f ,*r")
	(match_operand:V2SF 1 "move_operand"
					"fU,Y,Q,f,U*r,W ,i ,m ,*r,*r,f "))]
  "ia64_move_ok (operands[0], operands[1])"
{
  static const char * const alt[] = {
    "%,mov %0 = %F1",
    "%,fpack %0 = %F2, %F1",
    "%,ldf8 %0 = %1%P1",
    "%,stf8 %0 = %1%P0",
    "%,mov %0 = %r1",
    "%,addl %0 = %v1, r0",
    "%,movl %0 = %v1",
    "%,ld8%O1 %0 = %1%P1",
    "%,st8%Q0 %0 = %r1%P0",
    "%,setf.sig %0 = %1",
    "%,getf.sig %0 = %1"
  };

  if (which_alternative == 1)
    {
      operands[2] = XVECEXP (operands[1], 0, TARGET_BIG_ENDIAN ? 0 : 1);
      operands[1] = XVECEXP (operands[1], 0, TARGET_BIG_ENDIAN ? 1 : 0);
    }

  return alt[which_alternative];
}
  [(set_attr "itanium_class" "fmisc,fmisc,fld,stf,ialu,ialu,long_i,ld,st,tofr,frfr")])

(define_insn "absv2sf2"
  [(set (match_operand:V2SF 0 "fr_register_operand" "=f")
	(abs:V2SF (match_operand:V2SF 1 "fr_register_operand" "f")))]
  ""
  "fpabs %0 = %1"
  [(set_attr "itanium_class" "fmisc")])

(define_insn "negv2sf2"
  [(set (match_operand:V2SF 0 "fr_register_operand" "=f")
	(neg:V2SF (match_operand:V2SF 1 "fr_register_operand" "f")))]
  ""
  "fpneg %0 = %1"
  [(set_attr "itanium_class" "fmisc")])

(define_insn "*negabsv2sf2"
  [(set (match_operand:V2SF 0 "fr_register_operand" "=f")
	(neg:V2SF
	  (abs:V2SF (match_operand:V2SF 1 "fr_register_operand" "f"))))]
  ""
  "fpnegabs %0 = %1"
  [(set_attr "itanium_class" "fmisc")])

(define_expand "addv2sf3"
  [(set (match_operand:V2SF 0 "fr_register_operand" "")
	(fma:V2SF (match_operand:V2SF 1 "fr_register_operand" "")
		  (match_dup 3)
		  (match_operand:V2SF 2 "fr_register_operand" "")))]
  ""
{
  rtvec v = gen_rtvec (2, CONST1_RTX (SFmode), CONST1_RTX (SFmode));
  operands[3] = force_reg (V2SFmode, gen_rtx_CONST_VECTOR (V2SFmode, v));
})

(define_expand "subv2sf3"
  [(set (match_operand:V2SF 0 "fr_register_operand" "")
	(fma:V2SF
	  (match_operand:V2SF 1 "fr_register_operand" "")
	  (match_dup 3)
	  (neg:V2SF (match_operand:V2SF 2 "fr_register_operand" ""))))]
  ""
{
  rtvec v = gen_rtvec (2, CONST1_RTX (SFmode), CONST1_RTX (SFmode));
  operands[3] = force_reg (V2SFmode, gen_rtx_CONST_VECTOR (V2SFmode, v));
})

(define_insn "mulv2sf3"
  [(set (match_operand:V2SF 0 "fr_register_operand" "=f")
	(mult:V2SF (match_operand:V2SF 1 "fr_register_operand" "f")
		   (match_operand:V2SF 2 "fr_register_operand" "f")))]
  ""
  "fpmpy %0 = %1, %2"
  [(set_attr "itanium_class" "fmac")])

(define_insn "fmav2sf4"
  [(set (match_operand:V2SF 0 "fr_register_operand" "=f")
	(fma:V2SF
	  (match_operand:V2SF 1 "fr_register_operand" "f")
	  (match_operand:V2SF 2 "fr_register_operand" "f")
	  (match_operand:V2SF 3 "fr_register_operand" "f")))]
  ""
  "fpma %0 = %1, %2, %3"
  [(set_attr "itanium_class" "fmac")])

(define_insn "fmsv2sf4"
  [(set (match_operand:V2SF 0 "fr_register_operand" "=f")
	(fma:V2SF
	  (match_operand:V2SF 1 "fr_register_operand" "f")
	  (match_operand:V2SF 2 "fr_register_operand" "f")
	  (neg:V2SF (match_operand:V2SF 3 "fr_register_operand" "f"))))]
  ""
  "fpms %0 = %1, %2, %3"
  [(set_attr "itanium_class" "fmac")])

(define_insn "*fpnmpy"
  [(set (match_operand:V2SF 0 "fr_register_operand" "=f")
	(neg:V2SF
	  (mult:V2SF (match_operand:V2SF 1 "fr_register_operand" "f")
		     (match_operand:V2SF 2 "fr_register_operand" "f"))))]
  ""
  "fpnmpy %0 = %1, %2"
  [(set_attr "itanium_class" "fmac")])

(define_insn "fnmav2sf4"
  [(set (match_operand:V2SF 0 "fr_register_operand" "=f")
	(fma:V2SF
	  (neg:V2SF (match_operand:V2SF 1 "fr_register_operand" "f"))
	  (match_operand:V2SF 2 "fr_register_operand" "f")
	  (match_operand:V2SF 3 "fr_register_operand" "f")))]
  ""
  "fpnma %0 = %1, %2, %3"
  [(set_attr "itanium_class" "fmac")])

(define_insn "smaxv2sf3"
  [(set (match_operand:V2SF 0 "fr_register_operand" "=f")
	(smax:V2SF (match_operand:V2SF 1 "fr_register_operand" "f")
		   (match_operand:V2SF 2 "fr_register_operand" "f")))]
  ""
  "fpmax %0 = %1, %2"
  [(set_attr "itanium_class" "fmisc")])

(define_insn "sminv2sf3"
  [(set (match_operand:V2SF 0 "fr_register_operand" "=f")
	(smin:V2SF (match_operand:V2SF 1 "fr_register_operand" "f")
		   (match_operand:V2SF 2 "fr_register_operand" "f")))]
  ""
  "fpmin %0 = %1, %2"
  [(set_attr "itanium_class" "fmisc")])

(define_expand "reduc_splus_v2sf"
  [(match_operand:V2SF 0 "fr_register_operand" "")
   (match_operand:V2SF 1 "fr_register_operand" "")]
  ""
{
  rtx tmp = gen_reg_rtx (V2SFmode);
  if (TARGET_BIG_ENDIAN)
    emit_insn (gen_fswap (tmp, CONST0_RTX (V2SFmode), operands[1]));
  else
    emit_insn (gen_fswap (tmp, operands[1], CONST0_RTX (V2SFmode)));
  emit_insn (gen_addv2sf3 (operands[0], operands[1], tmp));
  DONE;
})

(define_expand "reduc_smax_v2sf"
  [(match_operand:V2SF 0 "fr_register_operand" "")
   (match_operand:V2SF 1 "fr_register_operand" "")]
  ""
{
  rtx tmp = gen_reg_rtx (V2SFmode);
  if (TARGET_BIG_ENDIAN)
    emit_insn (gen_fswap (tmp, CONST0_RTX (V2SFmode), operands[1]));
  else
    emit_insn (gen_fswap (tmp, operands[1], CONST0_RTX (V2SFmode)));
  emit_insn (gen_smaxv2sf3 (operands[0], operands[1], tmp));
  DONE;
})

(define_expand "reduc_smin_v2sf"
  [(match_operand:V2SF 0 "fr_register_operand" "")
   (match_operand:V2SF 1 "fr_register_operand" "")]
  ""
{
  rtx tmp = gen_reg_rtx (V2SFmode);
  if (TARGET_BIG_ENDIAN)
    emit_insn (gen_fswap (tmp, CONST0_RTX (V2SFmode), operands[1]));
  else
    emit_insn (gen_fswap (tmp, operands[1], CONST0_RTX (V2SFmode)));
  emit_insn (gen_sminv2sf3 (operands[0], operands[1], tmp));
  DONE;
})

(define_expand "vcondv2sfv2sf"
  [(set (match_operand:V2SF 0 "fr_register_operand" "")
	(if_then_else:V2SF
	  (match_operator 3 "" 
	    [(match_operand:V2SF 4 "fr_reg_or_0_operand" "")
	     (match_operand:V2SF 5 "fr_reg_or_0_operand" "")])
	  (match_operand:V2SF 1 "fr_reg_or_0_operand" "")
	  (match_operand:V2SF 2 "fr_reg_or_0_operand" "")))]
  ""
{
  rtx x, cmp;

  cmp = gen_reg_rtx (V2SFmode);
  PUT_MODE (operands[3], V2SFmode);
  emit_insn (gen_rtx_SET (cmp, operands[3]));

  x = gen_rtx_IF_THEN_ELSE (V2SFmode, cmp, operands[1], operands[2]);
  emit_insn (gen_rtx_SET (operands[0], x));
  DONE;
})

(define_insn "*fpcmp"
  [(set (match_operand:V2SF 0 "fr_register_operand" "=f")
	(match_operator:V2SF 3 "comparison_operator"
	  [(match_operand:V2SF 1 "fr_reg_or_0_operand" "fU")
	   (match_operand:V2SF 2 "fr_reg_or_0_operand" "fU")]))]
  ""
  "fpcmp.%D3 %0 = %F1, %F2"
  [(set_attr "itanium_class" "fmisc")])

(define_insn "*fselect"
  [(set (match_operand:V2SF 0 "fr_register_operand" "=f")
	(if_then_else:V2SF
	  (match_operand:V2SF 1 "fr_register_operand" "f")
	  (match_operand:V2SF 2 "fr_reg_or_0_operand" "fU")
	  (match_operand:V2SF 3 "fr_reg_or_0_operand" "fU")))]
  ""
  "fselect %0 = %F2, %F3, %1"
  [(set_attr "itanium_class" "fmisc")])

(define_expand "vec_initv2sfsf"
  [(match_operand:V2SF 0 "fr_register_operand" "")
   (match_operand 1 "" "")]
  ""
{
  rtx op1 = XVECEXP (operands[1], 0, 0);
  rtx op2 = XVECEXP (operands[1], 0, 1);
  rtx x;

  if (GET_CODE (op1) == CONST_DOUBLE && GET_CODE (op2) == CONST_DOUBLE)
    {
      x = gen_rtx_CONST_VECTOR (V2SFmode, XVEC (operands[1], 0));
      emit_move_insn (operands[0], x);
      DONE;
    }

  if (!fr_reg_or_fp01_operand (op1, SFmode))
    op1 = force_reg (SFmode, op1);
  if (!fr_reg_or_fp01_operand (op2, SFmode))
    op2 = force_reg (SFmode, op2);

  emit_insn (gen_fpack (operands[0], op1, op2));
  DONE;
})

(define_insn "fpack"
  [(set (match_operand:V2SF 0 "fr_register_operand" "=f")
	(vec_concat:V2SF
	  (match_operand:SF 1 "fr_reg_or_fp01_operand" "fG")
	  (match_operand:SF 2 "fr_reg_or_fp01_operand" "fG")))]
  ""
{
  /* Recall that vector elements are numbered in memory order.  */
  if (TARGET_BIG_ENDIAN)
    return "%,fpack %0 = %F1, %F2";
  else
    return "%,fpack %0 = %F2, %F1";
}
  [(set_attr "itanium_class" "fmisc")])

(define_insn "fswap"
  [(set (match_operand:V2SF 0 "fr_register_operand" "=f")
	(vec_select:V2SF
	  (vec_concat:V4SF
	    (match_operand:V2SF 1 "fr_reg_or_0_operand" "fU")
	    (match_operand:V2SF 2 "fr_reg_or_0_operand" "fU"))
	  (parallel [(const_int 1) (const_int 2)])))]
  ""
{
  /* Recall that vector elements are numbered in memory order.  */
  if (TARGET_BIG_ENDIAN)
    return "%,fswap %0 = %F2, %F1";
  else
    return "%,fswap %0 = %F1, %F2";
}
  [(set_attr "itanium_class" "fmisc")])

(define_insn "*vec_interleave_highv2sf"
  [(set (match_operand:V2SF 0 "fr_register_operand" "=f")
	(vec_select:V2SF
	  (vec_concat:V4SF
	    (match_operand:V2SF 1 "fr_reg_or_0_operand" "fU")
	    (match_operand:V2SF 2 "fr_reg_or_0_operand" "fU"))
	  (parallel [(const_int 1) (const_int 3)])))]
  ""
{
  /* Recall that vector elements are numbered in memory order.  */
  if (TARGET_BIG_ENDIAN)
    return "%,fmix.l %0 = %F1, %F2";
  else
    return "%,fmix.l %0 = %F2, %F1";
}
  [(set_attr "itanium_class" "fmisc")])

(define_insn "*vec_interleave_lowv2sf"
  [(set (match_operand:V2SF 0 "fr_register_operand" "=f")
	(vec_select:V2SF
	  (vec_concat:V4SF
	    (match_operand:V2SF 1 "fr_reg_or_0_operand" "fU")
	    (match_operand:V2SF 2 "fr_reg_or_0_operand" "fU"))
	  (parallel [(const_int 0) (const_int 2)])))]
  ""
{
  /* Recall that vector elements are numbered in memory order.  */
  if (TARGET_BIG_ENDIAN)
    return "%,fmix.r %0 = %F1, %F2";
  else
    return "%,fmix.r %0 = %F2, %F1";
}
  [(set_attr "itanium_class" "fmisc")])

(define_insn "fmix_lr"
  [(set (match_operand:V2SF 0 "fr_register_operand" "=f")
	(vec_select:V2SF
	  (vec_concat:V4SF
	    (match_operand:V2SF 1 "fr_reg_or_0_operand" "fU")
	    (match_operand:V2SF 2 "fr_reg_or_0_operand" "fU"))
	  (parallel [(const_int 0) (const_int 3)])))]
  ""
{
  /* Recall that vector elements are numbered in memory order.  */
  if (TARGET_BIG_ENDIAN)
    return "%,fmix.lr %0 = %F1, %F2";
  else
    return "%,fmix.lr %0 = %F2, %F1";
}
  [(set_attr "itanium_class" "fmisc")])

(define_expand "vec_setv2sf"
  [(match_operand:V2SF 0 "fr_register_operand" "")
   (match_operand:SF 1 "fr_register_operand" "")
   (match_operand 2 "const_int_operand" "")]
  ""
{
  ia64_expand_vec_setv2sf (operands);
  DONE;
})

(define_insn_and_split "*vec_extractv2sf_0_le"
  [(set (match_operand:SF 0 "nonimmediate_operand" "=r,f,m")
	(unspec:SF [(match_operand:V2SF 1 "nonimmediate_operand" "rfm,rm,r")
		    (const_int 0)]
		   UNSPEC_VECT_EXTR))]
  "!TARGET_BIG_ENDIAN"
  "#"
  "reload_completed"
  [(set (match_dup 0) (match_dup 1))]
{
  if (REG_P (operands[1]) && FR_REGNO_P (REGNO (operands[1])))
    operands[0] = gen_rtx_REG (V2SFmode, REGNO (operands[0]));
  else if (MEM_P (operands[1]))
    operands[1] = adjust_address (operands[1], SFmode, 0);
  else
    operands[1] = gen_rtx_REG (SFmode, REGNO (operands[1]));
})

(define_insn_and_split "*vec_extractv2sf_0_be"
  [(set (match_operand:SF 0 "register_operand" "=rf,r")
	(unspec:SF [(match_operand:V2SF 1 "nonimmediate_operand" "m,r")
		    (const_int 0)]
		   UNSPEC_VECT_EXTR))]
  "TARGET_BIG_ENDIAN"
  "#"
  "reload_completed"
  [(set (match_dup 0) (match_dup 1))]
{
  if (MEM_P (operands[1]))
    operands[1] = adjust_address (operands[1], SFmode, 0);
  else
    {
      emit_insn (gen_lshrdi3 (operands[0], operands[1], GEN_INT (32)));
      DONE;
    }
})

(define_insn_and_split "*vec_extractv2sf_1_le"
  [(set (match_operand:SF 0 "register_operand" "=r")
	(unspec:SF [(match_operand:V2SF 1 "register_operand" "r")
		    (const_int 1)]
		   UNSPEC_VECT_EXTR))]
  "!TARGET_BIG_ENDIAN"
  "#"
  "&& reload_completed"
  [(const_int 0)]
{
  operands[0] = gen_rtx_REG (DImode, REGNO (operands[0]));
  operands[1] = gen_rtx_REG (DImode, REGNO (operands[1]));
  emit_insn (gen_lshrdi3 (operands[0], operands[1], GEN_INT (32)));
  DONE;
})

(define_insn_and_split "*vec_extractv2sf_1_be"
  [(set (match_operand:SF 0 "register_operand" "=rf")
	(unspec:SF [(match_operand:V2SF 1 "register_operand" "r")
		    (const_int 1)]
		   UNSPEC_VECT_EXTR))]
  "TARGET_BIG_ENDIAN"
  "#"
  "&& reload_completed"
  [(set (match_dup 0) (match_dup 1))]
{
  operands[1] = gen_rtx_REG (SFmode, REGNO (operands[1]));
})

(define_expand "vec_extractv2sfsf"
  [(set (match_operand:SF 0 "register_operand" "")
	(unspec:SF [(match_operand:V2SF 1 "register_operand" "")
		    (match_operand:DI 2 "const_int_operand" "")]
		   UNSPEC_VECT_EXTR))]
  ""
  "")

(define_expand "vec_unpacku_lo_<mode>"
  [(match_operand:<vecwider> 0 "register_operand" "")
   (match_operand:VECINT12   1 "register_operand" "")]
  ""
{
  ia64_expand_unpack (operands, true, false);
  DONE;
})

(define_expand "vec_unpacku_hi_<mode>"
  [(match_operand:<vecwider> 0 "register_operand" "")
   (match_operand:VECINT12   1 "register_operand" "")]
  ""
{
  ia64_expand_unpack (operands, true, true);
  DONE;
})

(define_expand "vec_unpacks_lo_<mode>"
  [(match_operand:<vecwider> 0 "register_operand" "")
   (match_operand:VECINT12   1 "register_operand" "")]
  ""
{
  ia64_expand_unpack (operands, false, false);
  DONE;
})

(define_expand "vec_unpacks_hi_<mode>"
  [(match_operand:<vecwider> 0 "register_operand" "")
   (match_operand:VECINT12   1 "register_operand" "")]
  ""
{
  ia64_expand_unpack (operands, false, true);
  DONE;
})

(define_expand "vec_pack_trunc_v4hi"
  [(match_operand:V8QI 0 "gr_register_operand" "")
   (match_operand:V4HI 1 "gr_register_operand" "")
   (match_operand:V4HI 2 "gr_register_operand" "")]
  ""
{
  rtx op1 = gen_lowpart (V8QImode, operands[1]);
  rtx op2 = gen_lowpart (V8QImode, operands[2]);
  ia64_expand_vec_perm_even_odd (operands[0], op1, op2, TARGET_BIG_ENDIAN);
  DONE;
})

(define_expand "vec_pack_trunc_v2si"
  [(match_operand:V4HI 0 "gr_register_operand" "")
   (match_operand:V2SI 1 "gr_register_operand" "")
   (match_operand:V2SI 2 "gr_register_operand" "")]
  ""
{
  rtx op1 = gen_lowpart (V4HImode, operands[1]);
  rtx op2 = gen_lowpart (V4HImode, operands[2]);
  ia64_expand_vec_perm_even_odd (operands[0], op1, op2, TARGET_BIG_ENDIAN);
  DONE;
})

(define_expand "vec_perm_const<mode>"
  [(match_operand:VEC 0 "register_operand" "")
   (match_operand:VEC 1 "register_operand" "")
   (match_operand:VEC 2 "register_operand" "")
   (match_operand:<vecint> 3 "" "")]
  ""
{
  if (ia64_expand_vec_perm_const (operands))
    DONE;
  else
    FAIL;
})

;; Missing operations
;; fprcpa
;; fpsqrta
