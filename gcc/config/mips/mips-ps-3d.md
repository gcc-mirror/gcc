;; MIPS Paired-Single Floating and MIPS-3D Instructions.
;; Copyright (C) 2004 Free Software Foundation, Inc.
;;
;; This file is part of GCC.
;;
;; GCC is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; GCC is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GCC; see the file COPYING.  If not, write to
;; the Free Software Foundation, 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

(define_insn "*movcc_v2sf_di"
  [(set (match_operand:V2SF 0 "register_operand" "=f,f")
	(if_then_else:V2SF
	 (match_operator:DI 4 "equality_operator"
			 [(match_operand:DI 1 "register_operand" "d,d")
			  (const_int 0)])
	 (match_operand:V2SF 2 "register_operand" "f,0")
	 (match_operand:V2SF 3 "register_operand" "0,f")))]
  "TARGET_PAIRED_SINGLE_FLOAT"
  "@
    mov%T4.ps\t%0,%2,%1
    mov%t4.ps\t%0,%3,%1"
  [(set_attr "type" "condmove")
   (set_attr "mode" "SF")])

(define_insn "*movcc_v2sf_si"
  [(set (match_operand:V2SF 0 "register_operand" "=f,f")
	(if_then_else:V2SF
	 (match_operator:SI 4 "equality_operator"
			 [(match_operand:SI 1 "register_operand" "d,d")
			  (const_int 0)])
	 (match_operand:V2SF 2 "register_operand" "f,0")
	 (match_operand:V2SF 3 "register_operand" "0,f")))]
  "TARGET_PAIRED_SINGLE_FLOAT"
  "@
    mov%T4.ps\t%0,%2,%1
    mov%t4.ps\t%0,%3,%1"
  [(set_attr "type" "condmove")
   (set_attr "mode" "SF")])

(define_insn "mips_cond_move_tf_ps"
  [(set (match_operand:V2SF 0 "register_operand" "=f,f")
	(unspec:V2SF [(match_operand:V2SF 1 "register_operand" "f,0")
		      (match_operand:V2SF 2 "register_operand" "0,f")
		      (match_operand:CCV2 3 "register_operand" "z,z")]
		     UNSPEC_MOVE_TF_PS))]
  "TARGET_PAIRED_SINGLE_FLOAT"
  "@
    movt.ps\t%0,%1,%Q3
    movf.ps\t%0,%2,%Q3"
  [(set_attr "type" "condmove")
   (set_attr "mode" "SF")])

(define_expand "movv2sfcc"
  [(set (match_dup 4) (match_operand 1 "comparison_operator"))
   (set (match_operand:V2SF 0 "register_operand")
	(if_then_else:V2SF (match_dup 5)
			   (match_operand:V2SF 2 "register_operand")
			   (match_operand:V2SF 3 "register_operand")))]
  "TARGET_PAIRED_SINGLE_FLOAT"
{
  /* We can only support MOVN.PS and MOVZ.PS.
     NOTE: MOVT.PS and MOVF.PS have different semantics from MOVN.PS and 
	   MOVZ.PS.  MOVT.PS and MOVF.PS depend on two CC values and move 
	   each item independently.  */

  if (GET_MODE_CLASS (GET_MODE (cmp_operands[0])) != MODE_INT)
    FAIL;

  gen_conditional_move (operands);
  DONE;
})

; pul.ps - Pair Upper Lower
(define_insn "mips_pul_ps"
  [(set (match_operand:V2SF 0 "register_operand" "=f")
	(vec_merge:V2SF
	 (match_operand:V2SF 1 "register_operand" "f")
	 (match_operand:V2SF 2 "register_operand" "f")
	 (const_int 2)))]
  "TARGET_PAIRED_SINGLE_FLOAT"
  "pul.ps\t%0,%1,%2"
  [(set_attr "type" "fmove")
   (set_attr "mode" "SF")])

; puu.ps - Pair upper upper
(define_insn "mips_puu_ps"
  [(set (match_operand:V2SF 0 "register_operand" "=f")
	(vec_merge:V2SF
	 (match_operand:V2SF 1 "register_operand" "f")
	 (vec_select:V2SF (match_operand:V2SF 2 "register_operand" "f")
			  (parallel [(const_int 1)
				     (const_int 0)]))
	 (const_int 2)))]
  "TARGET_PAIRED_SINGLE_FLOAT"
  "puu.ps\t%0,%1,%2"
  [(set_attr "type" "fmove")
   (set_attr "mode" "SF")])

; pll.ps - Pair Lower Lower
(define_insn "mips_pll_ps"
  [(set (match_operand:V2SF 0 "register_operand" "=f")
	(vec_merge:V2SF
	 (vec_select:V2SF (match_operand:V2SF 1 "register_operand" "f")
			  (parallel [(const_int 1)
				     (const_int 0)]))
	 (match_operand:V2SF 2 "register_operand" "f")
	 (const_int 2)))]
  "TARGET_PAIRED_SINGLE_FLOAT"
  "pll.ps\t%0,%1,%2"
  [(set_attr "type" "fmove")
   (set_attr "mode" "SF")])

; plu.ps - Pair Lower Upper
(define_insn "mips_plu_ps"
  [(set (match_operand:V2SF 0 "register_operand" "=f")
	(vec_merge:V2SF
	 (vec_select:V2SF (match_operand:V2SF 1 "register_operand" "f")
			  (parallel [(const_int 1)
				     (const_int 0)]))
	 (vec_select:V2SF (match_operand:V2SF 2 "register_operand" "f")
			  (parallel [(const_int 1)
				     (const_int 0)]))
	 (const_int 2)))]
  "TARGET_PAIRED_SINGLE_FLOAT"
  "plu.ps\t%0,%1,%2"
  [(set_attr "type" "fmove")
   (set_attr "mode" "SF")])

; vec_init
(define_expand "vec_initv2sf"
  [(match_operand:V2SF 0 "register_operand")
   (match_operand:V2SF 1 "")]
  "TARGET_PAIRED_SINGLE_FLOAT"
{
  rtx op0 = force_reg (SFmode, XVECEXP (operands[1], 0, 0));
  rtx op1 = force_reg (SFmode, XVECEXP (operands[1], 0, 1));
  emit_insn (gen_vec_initv2sf_internal (operands[0], op0, op1));
  DONE;
})

(define_insn "vec_initv2sf_internal"
  [(set (match_operand:V2SF 0 "register_operand" "=f")
	(vec_concat:V2SF
	 (match_operand:SF 1 "register_operand" "f")
	 (match_operand:SF 2 "register_operand" "f")))]
  "TARGET_PAIRED_SINGLE_FLOAT"
{
  if (BYTES_BIG_ENDIAN)
    return "cvt.ps.s\t%0,%1,%2";
  else
    return "cvt.ps.s\t%0,%2,%1";
}
  [(set_attr "type" "fcvt")
   (set_attr "mode" "SF")])

;; ??? This is only generated if we perform a vector operation that has to be
;; emulated.  There is no other way to get a vector mode bitfield extract
;; currently.

(define_insn "vec_extractv2sf"
  [(set (match_operand:SF 0 "register_operand" "=f")
	(vec_select:SF (match_operand:V2SF 1 "register_operand" "f")
		       (parallel
			[(match_operand 2 "const_0_or_1_operand" "")])))]
  "TARGET_PAIRED_SINGLE_FLOAT"
{
  if (INTVAL (operands[2]) == !BYTES_BIG_ENDIAN)
    return "cvt.s.pu\t%0,%1";
  else
    return "cvt.s.pl\t%0,%1";
}
  [(set_attr "type" "fcvt")
   (set_attr "mode" "SF")])

;; ??? This is only generated if we disable the vec_init pattern.  There is
;; no other way to get a vector mode bitfield store currently.

(define_expand "vec_setv2sf"
  [(match_operand:V2SF 0 "register_operand")
   (match_operand:SF 1 "register_operand")
   (match_operand 2 "const_0_or_1_operand")]
  "TARGET_PAIRED_SINGLE_FLOAT"
{
  rtx temp;

  /* We don't have an insert instruction, so we duplicate the float, and
     then use a PUL instruction.  */
  temp = gen_reg_rtx (V2SFmode);
  emit_insn (gen_mips_cvt_ps_s (temp, operands[1], operands[1]));
  if (INTVAL (operands[2]) == !BYTES_BIG_ENDIAN)
    emit_insn (gen_mips_pul_ps (operands[0], temp, operands[0]));
  else
    emit_insn (gen_mips_pul_ps (operands[0], operands[0], temp));
  DONE;
})

; cvt.ps.s - Floating Point Convert Pair to Paired Single
(define_expand "mips_cvt_ps_s"
  [(match_operand:V2SF 0 "register_operand")
   (match_operand:SF 1 "register_operand")
   (match_operand:SF 2 "register_operand")]
  "TARGET_PAIRED_SINGLE_FLOAT"
{
  if (BYTES_BIG_ENDIAN)
    emit_insn (gen_vec_initv2sf_internal (operands[0], operands[1],
	       operands[2]));
  else
    emit_insn (gen_vec_initv2sf_internal (operands[0], operands[2],
	       operands[1]));
  DONE;
})

; cvt.s.pl - Floating Point Convert Pair Lower to Single Floating Point
(define_expand "mips_cvt_s_pl"
  [(set (match_operand:SF 0 "register_operand")
	(vec_select:SF (match_operand:V2SF 1 "register_operand")
		       (parallel [(match_dup 2)])))]
  "TARGET_PAIRED_SINGLE_FLOAT"
  { operands[2] = GEN_INT (BYTES_BIG_ENDIAN); })

; cvt.s.pu - Floating Point Convert Pair Upper to Single Floating Point
(define_expand "mips_cvt_s_pu"
  [(set (match_operand:SF 0 "register_operand")
	(vec_select:SF (match_operand:V2SF 1 "register_operand")
		       (parallel [(match_dup 2)])))]
  "TARGET_PAIRED_SINGLE_FLOAT"
  { operands[2] = GEN_INT (!BYTES_BIG_ENDIAN); })

; alnv.ps - Floating Point Align Variable
(define_insn "mips_alnv_ps"
  [(set (match_operand:V2SF 0 "register_operand" "=f")
	(unspec:V2SF [(match_operand:V2SF 1 "register_operand" "f")
		      (match_operand:V2SF 2 "register_operand" "f")
		      (match_operand:SI 3 "register_operand" "d")]
		     UNSPEC_ALNV_PS))]
  "TARGET_PAIRED_SINGLE_FLOAT"
  "alnv.ps\t%0,%1,%2,%3"
  [(set_attr "type" "fmove")
   (set_attr "mode" "SF")])

; addr.ps - Floating Point Reduction Add
(define_insn "mips_addr_ps"
  [(set (match_operand:V2SF 0 "register_operand" "=f")
	(unspec:V2SF [(match_operand:V2SF 1 "register_operand" "f")
		      (match_operand:V2SF 2 "register_operand" "f")]
		     UNSPEC_ADDR_PS))]
  "TARGET_MIPS3D"
  "addr.ps\t%0,%1,%2"
  [(set_attr "type" "fadd")
   (set_attr "mode" "SF")])

; cvt.pw.ps - Floating Point Convert Paired Single to Paired Word
(define_insn "mips_cvt_pw_ps"
  [(set (match_operand:V2SF 0 "register_operand" "=f")
	(unspec:V2SF [(match_operand:V2SF 1 "register_operand" "f")]
		     UNSPEC_CVT_PW_PS))]
  "TARGET_MIPS3D"
  "cvt.pw.ps\t%0,%1"
  [(set_attr "type" "fcvt")
   (set_attr "mode" "SF")])

; cvt.ps.pw - Floating Point Convert Paired Word to Paired Single
(define_insn "mips_cvt_ps_pw"
  [(set (match_operand:V2SF 0 "register_operand" "=f")
	(unspec:V2SF [(match_operand:V2SF 1 "register_operand" "f")]
		     UNSPEC_CVT_PS_PW))]
  "TARGET_MIPS3D"
  "cvt.ps.pw\t%0,%1"
  [(set_attr "type" "fcvt")
   (set_attr "mode" "SF")])

; mulr.ps - Floating Point Reduction Multiply
(define_insn "mips_mulr_ps"
  [(set (match_operand:V2SF 0 "register_operand" "=f")
	(unspec:V2SF [(match_operand:V2SF 1 "register_operand" "f")
		      (match_operand:V2SF 2 "register_operand" "f")]
		     UNSPEC_MULR_PS))]
  "TARGET_MIPS3D"
  "mulr.ps\t%0,%1,%2"
  [(set_attr "type" "fmul")
   (set_attr "mode" "SF")])

;----------------------------------------------------------------------------
; Floating Point Absolute Comparisions for Singles
;----------------------------------------------------------------------------

(define_insn "mips_cabs_f_s"
  [(set (match_operand:CC 0 "register_operand" "=z")
	(unspec:CC [(match_operand:SF 1 "register_operand" "f")
		    (match_operand:SF 2 "register_operand" "f")]
		   UNSPEC_CABS_F))]
  "TARGET_MIPS3D"
  "cabs.f.s\t%Q0,%1,%2"
  [(set_attr "type" "fcmp")
   (set_attr "mode" "FPSW")])

(define_insn "mips_cabs_un_s"
  [(set (match_operand:CC 0 "register_operand" "=z")
	(unspec:CC [(match_operand:SF 1 "register_operand" "f")
		    (match_operand:SF 2 "register_operand" "f")]
		   UNSPEC_CABS_UN))]
  "TARGET_MIPS3D"
  "cabs.un.s\t%Q0,%1,%2"
  [(set_attr "type" "fcmp")
   (set_attr "mode" "FPSW")])

(define_insn "mips_cabs_eq_s"
  [(set (match_operand:CC 0 "register_operand" "=z")
	(unspec:CC [(match_operand:SF 1 "register_operand" "f")
		    (match_operand:SF 2 "register_operand" "f")]
		   UNSPEC_CABS_EQ))]
  "TARGET_MIPS3D"
  "cabs.eq.s\t%Q0,%1,%2"
  [(set_attr "type" "fcmp")
   (set_attr "mode" "FPSW")])

(define_insn "mips_cabs_ueq_s"
  [(set (match_operand:CC 0 "register_operand" "=z")
	(unspec:CC [(match_operand:SF 1 "register_operand" "f")
		    (match_operand:SF 2 "register_operand" "f")]
		   UNSPEC_CABS_UEQ))]
  "TARGET_MIPS3D"
  "cabs.ueq.s\t%Q0,%1,%2"
  [(set_attr "type" "fcmp")
   (set_attr "mode" "FPSW")])

(define_insn "mips_cabs_olt_s"
  [(set (match_operand:CC 0 "register_operand" "=z")
	(unspec:CC [(match_operand:SF 1 "register_operand" "f")
		    (match_operand:SF 2 "register_operand" "f")]
		   UNSPEC_CABS_OLT))]
  "TARGET_MIPS3D"
  "cabs.olt.s\t%Q0,%1,%2"
  [(set_attr "type" "fcmp")
   (set_attr "mode" "FPSW")])

(define_insn "mips_cabs_ult_s"
  [(set (match_operand:CC 0 "register_operand" "=z")
	(unspec:CC [(match_operand:SF 1 "register_operand" "f")
		    (match_operand:SF 2 "register_operand" "f")]
		   UNSPEC_CABS_ULT))]
  "TARGET_MIPS3D"
  "cabs.ult.s\t%Q0,%1,%2"
  [(set_attr "type" "fcmp")
   (set_attr "mode" "FPSW")])

(define_insn "mips_cabs_ole_s"
  [(set (match_operand:CC 0 "register_operand" "=z")
	(unspec:CC [(match_operand:SF 1 "register_operand" "f")
		    (match_operand:SF 2 "register_operand" "f")]
		   UNSPEC_CABS_OLE))]
  "TARGET_MIPS3D"
  "cabs.ole.s\t%Q0,%1,%2"
  [(set_attr "type" "fcmp")
   (set_attr "mode" "FPSW")])

(define_insn "mips_cabs_ule_s"
  [(set (match_operand:CC 0 "register_operand" "=z")
	(unspec:CC [(match_operand:SF 1 "register_operand" "f")
		    (match_operand:SF 2 "register_operand" "f")]
		   UNSPEC_CABS_ULE))]
  "TARGET_MIPS3D"
  "cabs.ule.s\t%Q0,%1,%2"
  [(set_attr "type" "fcmp")
   (set_attr "mode" "FPSW")])

(define_insn "mips_cabs_sf_s"
  [(set (match_operand:CC 0 "register_operand" "=z")
	(unspec:CC [(match_operand:SF 1 "register_operand" "f")
		    (match_operand:SF 2 "register_operand" "f")]
		   UNSPEC_CABS_SF))]
  "TARGET_MIPS3D"
  "cabs.sf.s\t%Q0,%1,%2"
  [(set_attr "type" "fcmp")
   (set_attr "mode" "FPSW")])

(define_insn "mips_cabs_ngle_s"
  [(set (match_operand:CC 0 "register_operand" "=z")
	(unspec:CC [(match_operand:SF 1 "register_operand" "f")
		    (match_operand:SF 2 "register_operand" "f")]
		   UNSPEC_CABS_NGLE))]
  "TARGET_MIPS3D"
  "cabs.ngle.s\t%Q0,%1,%2"
  [(set_attr "type" "fcmp")
   (set_attr "mode" "FPSW")])

(define_insn "mips_cabs_seq_s"
  [(set (match_operand:CC 0 "register_operand" "=z")
	(unspec:CC [(match_operand:SF 1 "register_operand" "f")
		    (match_operand:SF 2 "register_operand" "f")]
		   UNSPEC_CABS_SEQ))]
  "TARGET_MIPS3D"
  "cabs.seq.s\t%Q0,%1,%2"
  [(set_attr "type" "fcmp")
   (set_attr "mode" "FPSW")])

(define_insn "mips_cabs_ngl_s"
  [(set (match_operand:CC 0 "register_operand" "=z")
	(unspec:CC [(match_operand:SF 1 "register_operand" "f")
		    (match_operand:SF 2 "register_operand" "f")]
		   UNSPEC_CABS_NGL))]
  "TARGET_MIPS3D"
  "cabs.ngl.s\t%Q0,%1,%2"
  [(set_attr "type" "fcmp")
   (set_attr "mode" "FPSW")])

(define_insn "mips_cabs_lt_s"
  [(set (match_operand:CC 0 "register_operand" "=z")
	(unspec:CC [(match_operand:SF 1 "register_operand" "f")
		    (match_operand:SF 2 "register_operand" "f")]
		   UNSPEC_CABS_LT))]
  "TARGET_MIPS3D"
  "cabs.lt.s\t%Q0,%1,%2"
  [(set_attr "type" "fcmp")
   (set_attr "mode" "FPSW")])

(define_insn "mips_cabs_nge_s"
  [(set (match_operand:CC 0 "register_operand" "=z")
	(unspec:CC [(match_operand:SF 1 "register_operand" "f")
		    (match_operand:SF 2 "register_operand" "f")]
		   UNSPEC_CABS_NGE))]
  "TARGET_MIPS3D"
  "cabs.nge.s\t%Q0,%1,%2"
  [(set_attr "type" "fcmp")
   (set_attr "mode" "FPSW")])

(define_insn "mips_cabs_le_s"
  [(set (match_operand:CC 0 "register_operand" "=z")
	(unspec:CC [(match_operand:SF 1 "register_operand" "f")
		    (match_operand:SF 2 "register_operand" "f")]
		   UNSPEC_CABS_LE))]
  "TARGET_MIPS3D"
  "cabs.le.s\t%Q0,%1,%2"
  [(set_attr "type" "fcmp")
   (set_attr "mode" "FPSW")])

(define_insn "mips_cabs_ngt_s"
  [(set (match_operand:CC 0 "register_operand" "=z")
	(unspec:CC [(match_operand:SF 1 "register_operand" "f")
		    (match_operand:SF 2 "register_operand" "f")]
		   UNSPEC_CABS_NGT))]
  "TARGET_MIPS3D"
  "cabs.ngt.s\t%Q0,%1,%2"
  [(set_attr "type" "fcmp")
   (set_attr "mode" "FPSW")])

;----------------------------------------------------------------------------
; Floating Point Absolute Comparisions for Doubles
;----------------------------------------------------------------------------
(define_insn "mips_cabs_f_d"
  [(set (match_operand:CC 0 "register_operand" "=z")
	(unspec:CC [(match_operand:DF 1 "register_operand" "f")
		    (match_operand:DF 2 "register_operand" "f")]
		   UNSPEC_CABS_F))]
  "TARGET_MIPS3D"
  "cabs.f.d\t%Q0,%1,%2"
  [(set_attr "type" "fcmp")
   (set_attr "mode" "FPSW")])

(define_insn "mips_cabs_un_d"
  [(set (match_operand:CC 0 "register_operand" "=z")
	(unspec:CC [(match_operand:DF 1 "register_operand" "f")
		    (match_operand:DF 2 "register_operand" "f")]
		   UNSPEC_CABS_UN))]
  "TARGET_MIPS3D"
  "cabs.un.d\t%Q0,%1,%2"
  [(set_attr "type" "fcmp")
   (set_attr "mode" "FPSW")])

(define_insn "mips_cabs_eq_d"
  [(set (match_operand:CC 0 "register_operand" "=z")
	(unspec:CC [(match_operand:DF 1 "register_operand" "f")
		    (match_operand:DF 2 "register_operand" "f")]
		   UNSPEC_CABS_EQ))]
  "TARGET_MIPS3D"
  "cabs.eq.d\t%Q0,%1,%2"
  [(set_attr "type" "fcmp")
   (set_attr "mode" "FPSW")])

(define_insn "mips_cabs_ueq_d"
  [(set (match_operand:CC 0 "register_operand" "=z")
	(unspec:CC [(match_operand:DF 1 "register_operand" "f")
		    (match_operand:DF 2 "register_operand" "f")]
		   UNSPEC_CABS_UEQ))]
  "TARGET_MIPS3D"
  "cabs.ueq.d\t%Q0,%1,%2"
  [(set_attr "type" "fcmp")
   (set_attr "mode" "FPSW")])

(define_insn "mips_cabs_olt_d"
  [(set (match_operand:CC 0 "register_operand" "=z")
	(unspec:CC [(match_operand:DF 1 "register_operand" "f")
		    (match_operand:DF 2 "register_operand" "f")]
		   UNSPEC_CABS_OLT))]
  "TARGET_MIPS3D"
  "cabs.olt.d\t%Q0,%1,%2"
  [(set_attr "type" "fcmp")
   (set_attr "mode" "FPSW")])

(define_insn "mips_cabs_ult_d"
  [(set (match_operand:CC 0 "register_operand" "=z")
	(unspec:CC [(match_operand:DF 1 "register_operand" "f")
		    (match_operand:DF 2 "register_operand" "f")]
		   UNSPEC_CABS_ULT))]
  "TARGET_MIPS3D"
  "cabs.ult.d\t%Q0,%1,%2"
  [(set_attr "type" "fcmp")
   (set_attr "mode" "FPSW")])

(define_insn "mips_cabs_ole_d"
  [(set (match_operand:CC 0 "register_operand" "=z")
	(unspec:CC [(match_operand:DF 1 "register_operand" "f")
		    (match_operand:DF 2 "register_operand" "f")]
		   UNSPEC_CABS_OLE))]
  "TARGET_MIPS3D"
  "cabs.ole.d\t%Q0,%1,%2"
  [(set_attr "type" "fcmp")
   (set_attr "mode" "FPSW")])

(define_insn "mips_cabs_ule_d"
  [(set (match_operand:CC 0 "register_operand" "=z")
	(unspec:CC [(match_operand:DF 1 "register_operand" "f")
		    (match_operand:DF 2 "register_operand" "f")]
		   UNSPEC_CABS_ULE))]
  "TARGET_MIPS3D"
  "cabs.ule.d\t%Q0,%1,%2"
  [(set_attr "type" "fcmp")
   (set_attr "mode" "FPSW")])

(define_insn "mips_cabs_sf_d"
  [(set (match_operand:CC 0 "register_operand" "=z")
	(unspec:CC [(match_operand:DF 1 "register_operand" "f")
		    (match_operand:DF 2 "register_operand" "f")]
		   UNSPEC_CABS_SF))]
  "TARGET_MIPS3D"
  "cabs.sf.d\t%Q0,%1,%2"
  [(set_attr "type" "fcmp")
   (set_attr "mode" "FPSW")])

(define_insn "mips_cabs_ngle_d"
  [(set (match_operand:CC 0 "register_operand" "=z")
	(unspec:CC [(match_operand:DF 1 "register_operand" "f")
		    (match_operand:DF 2 "register_operand" "f")]
		   UNSPEC_CABS_NGLE))]
  "TARGET_MIPS3D"
  "cabs.ngle.d\t%Q0,%1,%2"
  [(set_attr "type" "fcmp")
   (set_attr "mode" "FPSW")])

(define_insn "mips_cabs_seq_d"
  [(set (match_operand:CC 0 "register_operand" "=z")
	(unspec:CC [(match_operand:DF 1 "register_operand" "f")
		    (match_operand:DF 2 "register_operand" "f")]
		   UNSPEC_CABS_SEQ))]
  "TARGET_MIPS3D"
  "cabs.seq.d\t%Q0,%1,%2"
  [(set_attr "type" "fcmp")
   (set_attr "mode" "FPSW")])

(define_insn "mips_cabs_ngl_d"
  [(set (match_operand:CC 0 "register_operand" "=z")
	(unspec:CC [(match_operand:DF 1 "register_operand" "f")
		    (match_operand:DF 2 "register_operand" "f")]
		   UNSPEC_CABS_NGL))]
  "TARGET_MIPS3D"
  "cabs.ngl.d\t%Q0,%1,%2"
  [(set_attr "type" "fcmp")
   (set_attr "mode" "FPSW")])

(define_insn "mips_cabs_lt_d"
  [(set (match_operand:CC 0 "register_operand" "=z")
	(unspec:CC [(match_operand:DF 1 "register_operand" "f")
		    (match_operand:DF 2 "register_operand" "f")]
		   UNSPEC_CABS_LT))]
  "TARGET_MIPS3D"
  "cabs.lt.d\t%Q0,%1,%2"
  [(set_attr "type" "fcmp")
   (set_attr "mode" "FPSW")])

(define_insn "mips_cabs_nge_d"
  [(set (match_operand:CC 0 "register_operand" "=z")
	(unspec:CC [(match_operand:DF 1 "register_operand" "f")
		    (match_operand:DF 2 "register_operand" "f")]
		   UNSPEC_CABS_NGE))]
  "TARGET_MIPS3D"
  "cabs.nge.d\t%Q0,%1,%2"
  [(set_attr "type" "fcmp")
   (set_attr "mode" "FPSW")])

(define_insn "mips_cabs_le_d"
  [(set (match_operand:CC 0 "register_operand" "=z")
	(unspec:CC [(match_operand:DF 1 "register_operand" "f")
		    (match_operand:DF 2 "register_operand" "f")]
		   UNSPEC_CABS_LE))]
  "TARGET_MIPS3D"
  "cabs.le.d\t%Q0,%1,%2"
  [(set_attr "type" "fcmp")
   (set_attr "mode" "FPSW")])

(define_insn "mips_cabs_ngt_d"
  [(set (match_operand:CC 0 "register_operand" "=z")
	(unspec:CC [(match_operand:DF 1 "register_operand" "f")
		    (match_operand:DF 2 "register_operand" "f")]
		   UNSPEC_CABS_NGT))]
  "TARGET_MIPS3D"
  "cabs.ngt.d\t%Q0,%1,%2"
  [(set_attr "type" "fcmp")
   (set_attr "mode" "FPSW")])

;----------------------------------------------------------------------------
; Floating Point Comparisions for Four Singles
;----------------------------------------------------------------------------

(define_insn "mips_c_f_4s"
  [(set (match_operand:CCV4 0 "register_operand" "=z")
	(unspec:CCV4 [(match_operand:V2SF 1 "register_operand" "f")
		      (match_operand:V2SF 2 "register_operand" "f")
		      (match_operand:V2SF 3 "register_operand" "f")
		      (match_operand:V2SF 4 "register_operand" "f")]
		     UNSPEC_C_F))]
  "TARGET_PAIRED_SINGLE_FLOAT"
  "c.f.ps\t%v0,%1,%2\n\tc.f.ps\t%V0,%3,%4"
  [(set_attr "type" "fcmp")
   (set_attr "length" "8")
   (set_attr "mode" "FPSW")])

(define_insn "mips_c_un_4s"
  [(set (match_operand:CCV4 0 "register_operand" "=z")
	(unspec:CCV4 [(match_operand:V2SF 1 "register_operand" "f")
		      (match_operand:V2SF 2 "register_operand" "f")
		      (match_operand:V2SF 3 "register_operand" "f")
		      (match_operand:V2SF 4 "register_operand" "f")]
		     UNSPEC_C_UN))]
  "TARGET_PAIRED_SINGLE_FLOAT"
  "c.un.ps\t%v0,%1,%2\n\tc.un.ps\t%V0,%3,%4"
  [(set_attr "type" "fcmp")
   (set_attr "length" "8")
   (set_attr "mode" "FPSW")])

(define_insn "mips_c_eq_4s"
  [(set (match_operand:CCV4 0 "register_operand" "=z")
	(unspec:CCV4 [(match_operand:V2SF 1 "register_operand" "f")
		      (match_operand:V2SF 2 "register_operand" "f")
		      (match_operand:V2SF 3 "register_operand" "f")
		      (match_operand:V2SF 4 "register_operand" "f")]
		     UNSPEC_C_EQ))]
  "TARGET_PAIRED_SINGLE_FLOAT"
  "c.eq.ps\t%v0,%1,%2\n\tc.eq.ps\t%V0,%3,%4"
  [(set_attr "type" "fcmp")
   (set_attr "length" "8")
   (set_attr "mode" "FPSW")])

(define_insn "mips_c_ueq_4s"
  [(set (match_operand:CCV4 0 "register_operand" "=z")
	(unspec:CCV4 [(match_operand:V2SF 1 "register_operand" "f")
		      (match_operand:V2SF 2 "register_operand" "f")
		      (match_operand:V2SF 3 "register_operand" "f")
		      (match_operand:V2SF 4 "register_operand" "f")]
		     UNSPEC_C_UEQ))]
  "TARGET_PAIRED_SINGLE_FLOAT"
  "c.ueq.ps\t%v0,%1,%2\n\tc.ueq.ps\t%V0,%3,%4"
  [(set_attr "type" "fcmp")
   (set_attr "length" "8")
   (set_attr "mode" "FPSW")])

(define_insn "mips_c_olt_4s"
  [(set (match_operand:CCV4 0 "register_operand" "=z")
	(unspec:CCV4 [(match_operand:V2SF 1 "register_operand" "f")
		      (match_operand:V2SF 2 "register_operand" "f")
		      (match_operand:V2SF 3 "register_operand" "f")
		      (match_operand:V2SF 4 "register_operand" "f")]
		     UNSPEC_C_OLT))]
  "TARGET_PAIRED_SINGLE_FLOAT"
  "c.olt.ps\t%v0,%1,%2\n\tc.olt.ps\t%V0,%3,%4"
  [(set_attr "type" "fcmp")
   (set_attr "length" "8")
   (set_attr "mode" "FPSW")])

(define_insn "mips_c_ult_4s"
  [(set (match_operand:CCV4 0 "register_operand" "=z")
	(unspec:CCV4 [(match_operand:V2SF 1 "register_operand" "f")
		      (match_operand:V2SF 2 "register_operand" "f")
		      (match_operand:V2SF 3 "register_operand" "f")
		      (match_operand:V2SF 4 "register_operand" "f")]
		     UNSPEC_C_ULT))]
  "TARGET_PAIRED_SINGLE_FLOAT"
  "c.ult.ps\t%v0,%1,%2\n\tc.ult.ps\t%V0,%3,%4"
  [(set_attr "type" "fcmp")
   (set_attr "length" "8")
   (set_attr "mode" "FPSW")])

(define_insn "mips_c_ole_4s"
  [(set (match_operand:CCV4 0 "register_operand" "=z")
	(unspec:CCV4 [(match_operand:V2SF 1 "register_operand" "f")
		      (match_operand:V2SF 2 "register_operand" "f")
		      (match_operand:V2SF 3 "register_operand" "f")
		      (match_operand:V2SF 4 "register_operand" "f")]
		     UNSPEC_C_OLE))]
  "TARGET_PAIRED_SINGLE_FLOAT"
  "c.ole.ps\t%v0,%1,%2\n\tc.ole.ps\t%V0,%3,%4"
  [(set_attr "type" "fcmp")
   (set_attr "length" "8")
   (set_attr "mode" "FPSW")])

(define_insn "mips_c_ule_4s"
  [(set (match_operand:CCV4 0 "register_operand" "=z")
	(unspec:CCV4 [(match_operand:V2SF 1 "register_operand" "f")
		      (match_operand:V2SF 2 "register_operand" "f")
		      (match_operand:V2SF 3 "register_operand" "f")
		      (match_operand:V2SF 4 "register_operand" "f")]
		     UNSPEC_C_ULE))]
  "TARGET_PAIRED_SINGLE_FLOAT"
  "c.ule.ps\t%v0,%1,%2\n\tc.ule.ps\t%V0,%3,%4"
  [(set_attr "type" "fcmp")
   (set_attr "length" "8")
   (set_attr "mode" "FPSW")])

(define_insn "mips_c_sf_4s"
  [(set (match_operand:CCV4 0 "register_operand" "=z")
	(unspec:CCV4 [(match_operand:V2SF 1 "register_operand" "f")
		      (match_operand:V2SF 2 "register_operand" "f")
		      (match_operand:V2SF 3 "register_operand" "f")
		      (match_operand:V2SF 4 "register_operand" "f")]
		     UNSPEC_C_SF))]
  "TARGET_PAIRED_SINGLE_FLOAT"
  "c.sf.ps\t%v0,%1,%2\n\tc.sf.ps\t%V0,%3,%4"
  [(set_attr "type" "fcmp")
   (set_attr "length" "8")
   (set_attr "mode" "FPSW")])

(define_insn "mips_c_ngle_4s"
  [(set (match_operand:CCV4 0 "register_operand" "=z")
	(unspec:CCV4 [(match_operand:V2SF 1 "register_operand" "f")
		      (match_operand:V2SF 2 "register_operand" "f")
		      (match_operand:V2SF 3 "register_operand" "f")
		      (match_operand:V2SF 4 "register_operand" "f")]
		     UNSPEC_C_NGLE))]
  "TARGET_PAIRED_SINGLE_FLOAT"
  "c.ngle.ps\t%v0,%1,%2\n\tc.ngle.ps\t%V0,%3,%4"
  [(set_attr "type" "fcmp")
   (set_attr "length" "8")
   (set_attr "mode" "FPSW")])

(define_insn "mips_c_seq_4s"
  [(set (match_operand:CCV4 0 "register_operand" "=z")
	(unspec:CCV4 [(match_operand:V2SF 1 "register_operand" "f")
		      (match_operand:V2SF 2 "register_operand" "f")
		      (match_operand:V2SF 3 "register_operand" "f")
		      (match_operand:V2SF 4 "register_operand" "f")]
		     UNSPEC_C_SEQ))]
  "TARGET_PAIRED_SINGLE_FLOAT"
  "c.seq.ps\t%v0,%1,%2\n\tc.seq.ps\t%V0,%3,%4"
  [(set_attr "type" "fcmp")
   (set_attr "length" "8")
   (set_attr "mode" "FPSW")])

(define_insn "mips_c_ngl_4s"
  [(set (match_operand:CCV4 0 "register_operand" "=z")
	(unspec:CCV4 [(match_operand:V2SF 1 "register_operand" "f")
		      (match_operand:V2SF 2 "register_operand" "f")
		      (match_operand:V2SF 3 "register_operand" "f")
		      (match_operand:V2SF 4 "register_operand" "f")]
		     UNSPEC_C_NGL))]
  "TARGET_PAIRED_SINGLE_FLOAT"
  "c.ngl.ps\t%v0,%1,%2\n\tc.ngl.ps\t%V0,%3,%4"
  [(set_attr "type" "fcmp")
   (set_attr "length" "8")
   (set_attr "mode" "FPSW")])

(define_insn "mips_c_lt_4s"
  [(set (match_operand:CCV4 0 "register_operand" "=z")
	(unspec:CCV4 [(match_operand:V2SF 1 "register_operand" "f")
		      (match_operand:V2SF 2 "register_operand" "f")
		      (match_operand:V2SF 3 "register_operand" "f")
		      (match_operand:V2SF 4 "register_operand" "f")]
		     UNSPEC_C_LT))]
  "TARGET_PAIRED_SINGLE_FLOAT"
  "c.lt.ps\t%v0,%1,%2\n\tc.lt.ps\t%V0,%3,%4"
  [(set_attr "type" "fcmp")
   (set_attr "length" "8")
   (set_attr "mode" "FPSW")])

(define_insn "mips_c_nge_4s"
  [(set (match_operand:CCV4 0 "register_operand" "=z")
	(unspec:CCV4 [(match_operand:V2SF 1 "register_operand" "f")
		      (match_operand:V2SF 2 "register_operand" "f")
		      (match_operand:V2SF 3 "register_operand" "f")
		      (match_operand:V2SF 4 "register_operand" "f")]
		     UNSPEC_C_NGE))]
  "TARGET_PAIRED_SINGLE_FLOAT"
  "c.nge.ps\t%v0,%1,%2\n\tc.nge.ps\t%V0,%3,%4"
  [(set_attr "type" "fcmp")
   (set_attr "length" "8")
   (set_attr "mode" "FPSW")])

(define_insn "mips_c_le_4s"
  [(set (match_operand:CCV4 0 "register_operand" "=z")
	(unspec:CCV4 [(match_operand:V2SF 1 "register_operand" "f")
		      (match_operand:V2SF 2 "register_operand" "f")
		      (match_operand:V2SF 3 "register_operand" "f")
		      (match_operand:V2SF 4 "register_operand" "f")]
		     UNSPEC_C_LE))]
  "TARGET_PAIRED_SINGLE_FLOAT"
  "c.le.ps\t%v0,%1,%2\n\tc.le.ps\t%V0,%3,%4"
  [(set_attr "type" "fcmp")
   (set_attr "length" "8")
   (set_attr "mode" "FPSW")])

(define_insn "mips_c_ngt_4s"
  [(set (match_operand:CCV4 0 "register_operand" "=z")
	(unspec:CCV4 [(match_operand:V2SF 1 "register_operand" "f")
		      (match_operand:V2SF 2 "register_operand" "f")
		      (match_operand:V2SF 3 "register_operand" "f")
		      (match_operand:V2SF 4 "register_operand" "f")]
		     UNSPEC_C_NGT))]
  "TARGET_PAIRED_SINGLE_FLOAT"
  "c.ngt.ps\t%v0,%1,%2\n\tc.ngt.ps\t%V0,%3,%4"
  [(set_attr "type" "fcmp")
   (set_attr "length" "8")
   (set_attr "mode" "FPSW")])

;----------------------------------------------------------------------------
; Floating Point Absolute Comparisions for Four Singles
;----------------------------------------------------------------------------
(define_insn "mips_cabs_f_4s"
  [(set (match_operand:CCV4 0 "register_operand" "=z")
	(unspec:CCV4 [(match_operand:V2SF 1 "register_operand" "f")
		      (match_operand:V2SF 2 "register_operand" "f")
		      (match_operand:V2SF 3 "register_operand" "f")
		      (match_operand:V2SF 4 "register_operand" "f")]
		     UNSPEC_CABS_F))]
  "TARGET_MIPS3D"
  "cabs.f.ps\t%v0,%1,%2\n\tcabs.f.ps\t%V0,%3,%4"
  [(set_attr "type" "fcmp")
   (set_attr "length" "8")
   (set_attr "mode" "FPSW")])

(define_insn "mips_cabs_un_4s"
  [(set (match_operand:CCV4 0 "register_operand" "=z")
	(unspec:CCV4 [(match_operand:V2SF 1 "register_operand" "f")
		      (match_operand:V2SF 2 "register_operand" "f")
		      (match_operand:V2SF 3 "register_operand" "f")
		      (match_operand:V2SF 4 "register_operand" "f")]
		     UNSPEC_CABS_UN))]
  "TARGET_MIPS3D"
  "cabs.un.ps\t%v0,%1,%2\n\tcabs.un.ps\t%V0,%3,%4"
  [(set_attr "type" "fcmp")
   (set_attr "length" "8")
   (set_attr "mode" "FPSW")])

(define_insn "mips_cabs_eq_4s"
  [(set (match_operand:CCV4 0 "register_operand" "=z")
	(unspec:CCV4 [(match_operand:V2SF 1 "register_operand" "f")
		      (match_operand:V2SF 2 "register_operand" "f")
		      (match_operand:V2SF 3 "register_operand" "f")
		      (match_operand:V2SF 4 "register_operand" "f")]
		     UNSPEC_CABS_EQ))]
  "TARGET_MIPS3D"
  "cabs.eq.ps\t%v0,%1,%2\n\tcabs.eq.ps\t%V0,%3,%4"
  [(set_attr "type" "fcmp")
   (set_attr "length" "8")
   (set_attr "mode" "FPSW")])

(define_insn "mips_cabs_ueq_4s"
  [(set (match_operand:CCV4 0 "register_operand" "=z")
	(unspec:CCV4 [(match_operand:V2SF 1 "register_operand" "f")
		      (match_operand:V2SF 2 "register_operand" "f")
		      (match_operand:V2SF 3 "register_operand" "f")
		      (match_operand:V2SF 4 "register_operand" "f")]
		     UNSPEC_CABS_UEQ))]
  "TARGET_MIPS3D"
  "cabs.ueq.ps\t%v0,%1,%2\n\tcabs.ueq.ps\t%V0,%3,%4"
  [(set_attr "type" "fcmp")
   (set_attr "length" "8")
   (set_attr "mode" "FPSW")])

(define_insn "mips_cabs_olt_4s"
  [(set (match_operand:CCV4 0 "register_operand" "=z")
	(unspec:CCV4 [(match_operand:V2SF 1 "register_operand" "f")
		      (match_operand:V2SF 2 "register_operand" "f")
		      (match_operand:V2SF 3 "register_operand" "f")
		      (match_operand:V2SF 4 "register_operand" "f")]
		     UNSPEC_CABS_OLT))]
  "TARGET_MIPS3D"
  "cabs.olt.ps\t%v0,%1,%2\n\tcabs.olt.ps\t%V0,%3,%4"
  [(set_attr "type" "fcmp")
   (set_attr "length" "8")
   (set_attr "mode" "FPSW")])

(define_insn "mips_cabs_ult_4s"
  [(set (match_operand:CCV4 0 "register_operand" "=z")
	(unspec:CCV4 [(match_operand:V2SF 1 "register_operand" "f")
		      (match_operand:V2SF 2 "register_operand" "f")
		      (match_operand:V2SF 3 "register_operand" "f")
		      (match_operand:V2SF 4 "register_operand" "f")]
		     UNSPEC_CABS_ULT))]
  "TARGET_MIPS3D"
  "cabs.ult.ps\t%v0,%1,%2\n\tcabs.ult.ps\t%V0,%3,%4"
  [(set_attr "type" "fcmp")
   (set_attr "length" "8")
   (set_attr "mode" "FPSW")])

(define_insn "mips_cabs_ole_4s"
  [(set (match_operand:CCV4 0 "register_operand" "=z")
	(unspec:CCV4 [(match_operand:V2SF 1 "register_operand" "f")
		      (match_operand:V2SF 2 "register_operand" "f")
		      (match_operand:V2SF 3 "register_operand" "f")
		      (match_operand:V2SF 4 "register_operand" "f")]
		     UNSPEC_CABS_OLE))]
  "TARGET_MIPS3D"
  "cabs.ole.ps\t%v0,%1,%2\n\tcabs.ole.ps\t%V0,%3,%4"
  [(set_attr "type" "fcmp")
   (set_attr "length" "8")
   (set_attr "mode" "FPSW")])

(define_insn "mips_cabs_ule_4s"
  [(set (match_operand:CCV4 0 "register_operand" "=z")
	(unspec:CCV4 [(match_operand:V2SF 1 "register_operand" "f")
		      (match_operand:V2SF 2 "register_operand" "f")
		      (match_operand:V2SF 3 "register_operand" "f")
		      (match_operand:V2SF 4 "register_operand" "f")]
		     UNSPEC_CABS_ULE))]
  "TARGET_MIPS3D"
  "cabs.ule.ps\t%v0,%1,%2\n\tcabs.ule.ps\t%V0,%3,%4"
  [(set_attr "type" "fcmp")
   (set_attr "length" "8")
   (set_attr "mode" "FPSW")])

(define_insn "mips_cabs_sf_4s"
  [(set (match_operand:CCV4 0 "register_operand" "=z")
	(unspec:CCV4 [(match_operand:V2SF 1 "register_operand" "f")
		      (match_operand:V2SF 2 "register_operand" "f")
		      (match_operand:V2SF 3 "register_operand" "f")
		      (match_operand:V2SF 4 "register_operand" "f")]
		     UNSPEC_CABS_SF))]
  "TARGET_MIPS3D"
  "cabs.sf.ps\t%v0,%1,%2\n\tcabs.sf.ps\t%V0,%3,%4"
  [(set_attr "type" "fcmp")
   (set_attr "length" "8")
   (set_attr "mode" "FPSW")])

(define_insn "mips_cabs_ngle_4s"
  [(set (match_operand:CCV4 0 "register_operand" "=z")
	(unspec:CCV4 [(match_operand:V2SF 1 "register_operand" "f")
		      (match_operand:V2SF 2 "register_operand" "f")
		      (match_operand:V2SF 3 "register_operand" "f")
		      (match_operand:V2SF 4 "register_operand" "f")]
		     UNSPEC_CABS_NGLE))]
  "TARGET_MIPS3D"
  "cabs.ngle.ps\t%v0,%1,%2\n\tcabs.ngle.ps\t%V0,%3,%4"
  [(set_attr "type" "fcmp")
   (set_attr "length" "8")
   (set_attr "mode" "FPSW")])

(define_insn "mips_cabs_seq_4s"
  [(set (match_operand:CCV4 0 "register_operand" "=z")
	(unspec:CCV4 [(match_operand:V2SF 1 "register_operand" "f")
		      (match_operand:V2SF 2 "register_operand" "f")
		      (match_operand:V2SF 3 "register_operand" "f")
		      (match_operand:V2SF 4 "register_operand" "f")]
		     UNSPEC_CABS_SEQ))]
  "TARGET_MIPS3D"
  "cabs.seq.ps\t%v0,%1,%2\n\tcabs.seq.ps\t%V0,%3,%4"
  [(set_attr "type" "fcmp")
   (set_attr "length" "8")
   (set_attr "mode" "FPSW")])

(define_insn "mips_cabs_ngl_4s"
  [(set (match_operand:CCV4 0 "register_operand" "=z")
	(unspec:CCV4 [(match_operand:V2SF 1 "register_operand" "f")
		      (match_operand:V2SF 2 "register_operand" "f")
		      (match_operand:V2SF 3 "register_operand" "f")
		      (match_operand:V2SF 4 "register_operand" "f")]
		     UNSPEC_CABS_NGL))]
  "TARGET_MIPS3D"
  "cabs.ngl.ps\t%v0,%1,%2\n\tcabs.ngl.ps\t%V0,%3,%4"
  [(set_attr "type" "fcmp")
   (set_attr "length" "8")
   (set_attr "mode" "FPSW")])

(define_insn "mips_cabs_lt_4s"
  [(set (match_operand:CCV4 0 "register_operand" "=z")
	(unspec:CCV4 [(match_operand:V2SF 1 "register_operand" "f")
		      (match_operand:V2SF 2 "register_operand" "f")
		      (match_operand:V2SF 3 "register_operand" "f")
		      (match_operand:V2SF 4 "register_operand" "f")]
		     UNSPEC_CABS_LT))]
  "TARGET_MIPS3D"
  "cabs.lt.ps\t%v0,%1,%2\n\tcabs.lt.ps\t%V0,%3,%4"
  [(set_attr "type" "fcmp")
   (set_attr "length" "8")
   (set_attr "mode" "FPSW")])

(define_insn "mips_cabs_nge_4s"
  [(set (match_operand:CCV4 0 "register_operand" "=z")
	(unspec:CCV4 [(match_operand:V2SF 1 "register_operand" "f")
		      (match_operand:V2SF 2 "register_operand" "f")
		      (match_operand:V2SF 3 "register_operand" "f")
		      (match_operand:V2SF 4 "register_operand" "f")]
		     UNSPEC_CABS_NGE))]
  "TARGET_MIPS3D"
  "cabs.nge.ps\t%v0,%1,%2\n\tcabs.nge.ps\t%V0,%3,%4"
  [(set_attr "type" "fcmp")
   (set_attr "length" "8")
   (set_attr "mode" "FPSW")])

(define_insn "mips_cabs_le_4s"
  [(set (match_operand:CCV4 0 "register_operand" "=z")
	(unspec:CCV4 [(match_operand:V2SF 1 "register_operand" "f")
		      (match_operand:V2SF 2 "register_operand" "f")
		      (match_operand:V2SF 3 "register_operand" "f")
		      (match_operand:V2SF 4 "register_operand" "f")]
		     UNSPEC_CABS_LE))]
  "TARGET_MIPS3D"
  "cabs.le.ps\t%v0,%1,%2\n\tcabs.le.ps\t%V0,%3,%4"
  [(set_attr "type" "fcmp")
   (set_attr "length" "8")
   (set_attr "mode" "FPSW")])

(define_insn "mips_cabs_ngt_4s"
  [(set (match_operand:CCV4 0 "register_operand" "=z")
	(unspec:CCV4 [(match_operand:V2SF 1 "register_operand" "f")
		      (match_operand:V2SF 2 "register_operand" "f")
		      (match_operand:V2SF 3 "register_operand" "f")
		      (match_operand:V2SF 4 "register_operand" "f")]
		     UNSPEC_CABS_NGT))]
  "TARGET_MIPS3D"
  "cabs.ngt.ps\t%v0,%1,%2\n\tcabs.ngt.ps\t%V0,%3,%4"
  [(set_attr "type" "fcmp")
   (set_attr "length" "8")
   (set_attr "mode" "FPSW")])

;----------------------------------------------------------------------------
; Floating Point Comparisions for Paired Singles
;----------------------------------------------------------------------------
(define_insn "mips_c_f_ps"
  [(set (match_operand:CCV2 0 "register_operand" "=z")
	(unspec:CCV2 [(match_operand:V2SF 1 "register_operand" "f")
		      (match_operand:V2SF 2 "register_operand" "f")] 
		     UNSPEC_C_F))]
  "TARGET_PAIRED_SINGLE_FLOAT"
  "c.f.ps\t%Z0%1,%2"
  [(set_attr "type" "fcmp")
   (set_attr "mode" "FPSW")])

(define_insn "mips_c_un_ps"
  [(set (match_operand:CCV2 0 "register_operand" "=z")
	(unspec:CCV2 [(match_operand:V2SF 1 "register_operand" "f")
		      (match_operand:V2SF 2 "register_operand" "f")] 
		     UNSPEC_C_UN))]
  "TARGET_PAIRED_SINGLE_FLOAT"
  "c.un.ps\t%Z0%1,%2"
  [(set_attr "type" "fcmp")
   (set_attr "mode" "FPSW")])

(define_insn "mips_c_eq_ps"
  [(set (match_operand:CCV2 0 "register_operand" "=z")
	(unspec:CCV2 [(match_operand:V2SF 1 "register_operand" "f")
		      (match_operand:V2SF 2 "register_operand" "f")] 
		     UNSPEC_C_EQ))]
  "TARGET_PAIRED_SINGLE_FLOAT"
  "c.eq.ps\t%Z0%1,%2"
  [(set_attr "type" "fcmp")
   (set_attr "mode" "FPSW")])

(define_insn "mips_c_ueq_ps"
  [(set (match_operand:CCV2 0 "register_operand" "=z")
	(unspec:CCV2 [(match_operand:V2SF 1 "register_operand" "f")
		      (match_operand:V2SF 2 "register_operand" "f")] 
		     UNSPEC_C_UEQ))]
  "TARGET_PAIRED_SINGLE_FLOAT"
  "c.ueq.ps\t%Z0%1,%2"
  [(set_attr "type" "fcmp")
   (set_attr "mode" "FPSW")])

(define_insn "mips_c_olt_ps"
  [(set (match_operand:CCV2 0 "register_operand" "=z")
	(unspec:CCV2 [(match_operand:V2SF 1 "register_operand" "f")
		      (match_operand:V2SF 2 "register_operand" "f")] 
		     UNSPEC_C_OLT))]
  "TARGET_PAIRED_SINGLE_FLOAT"
  "c.olt.ps\t%Z0%1,%2"
  [(set_attr "type" "fcmp")
   (set_attr "mode" "FPSW")])

(define_insn "mips_c_ult_ps"
  [(set (match_operand:CCV2 0 "register_operand" "=z")
	(unspec:CCV2 [(match_operand:V2SF 1 "register_operand" "f")
		      (match_operand:V2SF 2 "register_operand" "f")] 
		     UNSPEC_C_ULT))]
  "TARGET_PAIRED_SINGLE_FLOAT"
  "c.ult.ps\t%Z0%1,%2"
  [(set_attr "type" "fcmp")
   (set_attr "mode" "FPSW")])

(define_insn "mips_c_ole_ps"
  [(set (match_operand:CCV2 0 "register_operand" "=z")
	(unspec:CCV2 [(match_operand:V2SF 1 "register_operand" "f")
		      (match_operand:V2SF 2 "register_operand" "f")] 
		     UNSPEC_C_OLE))]
  "TARGET_PAIRED_SINGLE_FLOAT"
  "c.ole.ps\t%Z0%1,%2"
  [(set_attr "type" "fcmp")
   (set_attr "mode" "FPSW")])

(define_insn "mips_c_ule_ps"
  [(set (match_operand:CCV2 0 "register_operand" "=z")
	(unspec:CCV2 [(match_operand:V2SF 1 "register_operand" "f")
		      (match_operand:V2SF 2 "register_operand" "f")] 
		     UNSPEC_C_ULE))]
  "TARGET_PAIRED_SINGLE_FLOAT"
  "c.ule.ps\t%Z0%1,%2"
  [(set_attr "type" "fcmp")
   (set_attr "mode" "FPSW")])

(define_insn "mips_c_sf_ps"
  [(set (match_operand:CCV2 0 "register_operand" "=z")
	(unspec:CCV2 [(match_operand:V2SF 1 "register_operand" "f")
		      (match_operand:V2SF 2 "register_operand" "f")] 
		     UNSPEC_C_SF))]
  "TARGET_PAIRED_SINGLE_FLOAT"
  "c.sf.ps\t%Z0%1,%2"
  [(set_attr "type" "fcmp")
   (set_attr "mode" "FPSW")])

(define_insn "mips_c_ngle_ps"
  [(set (match_operand:CCV2 0 "register_operand" "=z")
	(unspec:CCV2 [(match_operand:V2SF 1 "register_operand" "f")
		      (match_operand:V2SF 2 "register_operand" "f")] 
		     UNSPEC_C_NGLE))]
  "TARGET_PAIRED_SINGLE_FLOAT"
  "c.ngle.ps\t%Z0%1,%2"
  [(set_attr "type" "fcmp")
   (set_attr "mode" "FPSW")])

(define_insn "mips_c_seq_ps"
  [(set (match_operand:CCV2 0 "register_operand" "=z")
	(unspec:CCV2 [(match_operand:V2SF 1 "register_operand" "f")
		      (match_operand:V2SF 2 "register_operand" "f")] 
		     UNSPEC_C_SEQ))]
  "TARGET_PAIRED_SINGLE_FLOAT"
  "c.seq.ps\t%Z0%1,%2"
  [(set_attr "type" "fcmp")
   (set_attr "mode" "FPSW")])

(define_insn "mips_c_ngl_ps"
  [(set (match_operand:CCV2 0 "register_operand" "=z")
	(unspec:CCV2 [(match_operand:V2SF 1 "register_operand" "f")
		      (match_operand:V2SF 2 "register_operand" "f")] 
		     UNSPEC_C_NGL))]
  "TARGET_PAIRED_SINGLE_FLOAT"
  "c.ngl.ps\t%Z0%1,%2"
  [(set_attr "type" "fcmp")
   (set_attr "mode" "FPSW")])

(define_insn "mips_c_lt_ps"
  [(set (match_operand:CCV2 0 "register_operand" "=z")
	(unspec:CCV2 [(match_operand:V2SF 1 "register_operand" "f")
		      (match_operand:V2SF 2 "register_operand" "f")] 
		     UNSPEC_C_LT))]
  "TARGET_PAIRED_SINGLE_FLOAT"
  "c.lt.ps\t%Z0%1,%2"
  [(set_attr "type" "fcmp")
   (set_attr "mode" "FPSW")])

(define_insn "mips_c_nge_ps"
  [(set (match_operand:CCV2 0 "register_operand" "=z")
	(unspec:CCV2 [(match_operand:V2SF 1 "register_operand" "f")
		      (match_operand:V2SF 2 "register_operand" "f")] 
		     UNSPEC_C_NGE))]
  "TARGET_PAIRED_SINGLE_FLOAT"
  "c.nge.ps\t%Z0%1,%2"
  [(set_attr "type" "fcmp")
   (set_attr "mode" "FPSW")])

(define_insn "mips_c_le_ps"
  [(set (match_operand:CCV2 0 "register_operand" "=z")
	(unspec:CCV2 [(match_operand:V2SF 1 "register_operand" "f")
		      (match_operand:V2SF 2 "register_operand" "f")] 
		     UNSPEC_C_LE))]
  "TARGET_PAIRED_SINGLE_FLOAT"
  "c.le.ps\t%Z0%1,%2"
  [(set_attr "type" "fcmp")
   (set_attr "mode" "FPSW")])

(define_insn "mips_c_ngt_ps"
  [(set (match_operand:CCV2 0 "register_operand" "=z")
	(unspec:CCV2 [(match_operand:V2SF 1 "register_operand" "f")
		      (match_operand:V2SF 2 "register_operand" "f")] 
		     UNSPEC_C_NGT))]
  "TARGET_PAIRED_SINGLE_FLOAT"
  "c.ngt.ps\t%Z0%1,%2"
  [(set_attr "type" "fcmp")
   (set_attr "mode" "FPSW")])

;----------------------------------------------------------------------------
; Floating Point Absolute Comparisions for Paired Singles
;----------------------------------------------------------------------------
(define_insn "mips_cabs_f_ps"
  [(set (match_operand:CCV2 0 "register_operand" "=z")
	(unspec:CCV2 [(match_operand:V2SF 1 "register_operand" "f")
		      (match_operand:V2SF 2 "register_operand" "f")] 
		     UNSPEC_CABS_F))]
  "TARGET_MIPS3D"
  "cabs.f.ps\t%Q0,%1,%2"
  [(set_attr "type" "fcmp")
   (set_attr "mode" "FPSW")])

(define_insn "mips_cabs_un_ps"
  [(set (match_operand:CCV2 0 "register_operand" "=z")
	(unspec:CCV2 [(match_operand:V2SF 1 "register_operand" "f")
		      (match_operand:V2SF 2 "register_operand" "f")] 
		     UNSPEC_CABS_UN))]
  "TARGET_MIPS3D"
  "cabs.un.ps\t%Q0,%1,%2"
  [(set_attr "type" "fcmp")
   (set_attr "mode" "FPSW")])

(define_insn "mips_cabs_eq_ps"
  [(set (match_operand:CCV2 0 "register_operand" "=z")
	(unspec:CCV2 [(match_operand:V2SF 1 "register_operand" "f")
		      (match_operand:V2SF 2 "register_operand" "f")] 
		     UNSPEC_CABS_EQ))]
  "TARGET_MIPS3D"
  "cabs.eq.ps\t%Q0,%1,%2"
  [(set_attr "type" "fcmp")
   (set_attr "mode" "FPSW")])

(define_insn "mips_cabs_ueq_ps"
  [(set (match_operand:CCV2 0 "register_operand" "=z")
	(unspec:CCV2 [(match_operand:V2SF 1 "register_operand" "f")
		      (match_operand:V2SF 2 "register_operand" "f")] 
		     UNSPEC_CABS_UEQ))]
  "TARGET_MIPS3D"
  "cabs.ueq.ps\t%Q0,%1,%2"
  [(set_attr "type" "fcmp")
   (set_attr "mode" "FPSW")])

(define_insn "mips_cabs_olt_ps"
  [(set (match_operand:CCV2 0 "register_operand" "=z")
	(unspec:CCV2 [(match_operand:V2SF 1 "register_operand" "f")
		      (match_operand:V2SF 2 "register_operand" "f")] 
		     UNSPEC_CABS_OLT))]
  "TARGET_MIPS3D"
  "cabs.olt.ps\t%Q0,%1,%2"
  [(set_attr "type" "fcmp")
   (set_attr "mode" "FPSW")])

(define_insn "mips_cabs_ult_ps"
  [(set (match_operand:CCV2 0 "register_operand" "=z")
	(unspec:CCV2 [(match_operand:V2SF 1 "register_operand" "f")
		      (match_operand:V2SF 2 "register_operand" "f")] 
		     UNSPEC_CABS_ULT))]
  "TARGET_MIPS3D"
  "cabs.ult.ps\t%Q0,%1,%2"
  [(set_attr "type" "fcmp")
   (set_attr "mode" "FPSW")])

(define_insn "mips_cabs_ole_ps"
  [(set (match_operand:CCV2 0 "register_operand" "=z")
	(unspec:CCV2 [(match_operand:V2SF 1 "register_operand" "f")
		      (match_operand:V2SF 2 "register_operand" "f")] 
		     UNSPEC_CABS_OLE))]
  "TARGET_MIPS3D"
  "cabs.ole.ps\t%Q0,%1,%2"
  [(set_attr "type" "fcmp")
   (set_attr "mode" "FPSW")])

(define_insn "mips_cabs_ule_ps"
  [(set (match_operand:CCV2 0 "register_operand" "=z")
	(unspec:CCV2 [(match_operand:V2SF 1 "register_operand" "f")
		      (match_operand:V2SF 2 "register_operand" "f")] 
		     UNSPEC_CABS_ULE))]
  "TARGET_MIPS3D"
  "cabs.ule.ps\t%Q0,%1,%2"
  [(set_attr "type" "fcmp")
   (set_attr "mode" "FPSW")])

(define_insn "mips_cabs_sf_ps"
  [(set (match_operand:CCV2 0 "register_operand" "=z")
	(unspec:CCV2 [(match_operand:V2SF 1 "register_operand" "f")
		      (match_operand:V2SF 2 "register_operand" "f")] 
		     UNSPEC_CABS_SF))]
  "TARGET_MIPS3D"
  "cabs.sf.ps\t%Q0,%1,%2"
  [(set_attr "type" "fcmp")
   (set_attr "mode" "FPSW")])

(define_insn "mips_cabs_ngle_ps"
  [(set (match_operand:CCV2 0 "register_operand" "=z")
	(unspec:CCV2 [(match_operand:V2SF 1 "register_operand" "f")
		      (match_operand:V2SF 2 "register_operand" "f")] 
		     UNSPEC_CABS_NGLE))]
  "TARGET_MIPS3D"
  "cabs.ngle.ps\t%Q0,%1,%2"
  [(set_attr "type" "fcmp")
   (set_attr "mode" "FPSW")])

(define_insn "mips_cabs_seq_ps"
  [(set (match_operand:CCV2 0 "register_operand" "=z")
	(unspec:CCV2 [(match_operand:V2SF 1 "register_operand" "f")
		      (match_operand:V2SF 2 "register_operand" "f")] 
		     UNSPEC_CABS_SEQ))]
  "TARGET_MIPS3D"
  "cabs.seq.ps\t%Q0,%1,%2"
  [(set_attr "type" "fcmp")
   (set_attr "mode" "FPSW")])

(define_insn "mips_cabs_ngl_ps"
  [(set (match_operand:CCV2 0 "register_operand" "=z")
	(unspec:CCV2 [(match_operand:V2SF 1 "register_operand" "f")
		      (match_operand:V2SF 2 "register_operand" "f")] 
		     UNSPEC_CABS_NGL))]
  "TARGET_MIPS3D"
  "cabs.ngl.ps\t%Q0,%1,%2"
  [(set_attr "type" "fcmp")
   (set_attr "mode" "FPSW")])

(define_insn "mips_cabs_lt_ps"
  [(set (match_operand:CCV2 0 "register_operand" "=z")
	(unspec:CCV2 [(match_operand:V2SF 1 "register_operand" "f")
		      (match_operand:V2SF 2 "register_operand" "f")] 
		     UNSPEC_CABS_LT))]
  "TARGET_MIPS3D"
  "cabs.lt.ps\t%Q0,%1,%2"
  [(set_attr "type" "fcmp")
   (set_attr "mode" "FPSW")])

(define_insn "mips_cabs_nge_ps"
  [(set (match_operand:CCV2 0 "register_operand" "=z")
	(unspec:CCV2 [(match_operand:V2SF 1 "register_operand" "f")
		      (match_operand:V2SF 2 "register_operand" "f")] 
		     UNSPEC_CABS_NGE))]
  "TARGET_MIPS3D"
  "cabs.nge.ps\t%Q0,%1,%2"
  [(set_attr "type" "fcmp")
   (set_attr "mode" "FPSW")])

(define_insn "mips_cabs_le_ps"
  [(set (match_operand:CCV2 0 "register_operand" "=z")
	(unspec:CCV2 [(match_operand:V2SF 1 "register_operand" "f")
		      (match_operand:V2SF 2 "register_operand" "f")] 
		     UNSPEC_CABS_LE))]
  "TARGET_MIPS3D"
  "cabs.le.ps\t%Q0,%1,%2"
  [(set_attr "type" "fcmp")
   (set_attr "mode" "FPSW")])

(define_insn "mips_cabs_ngt_ps"
  [(set (match_operand:CCV2 0 "register_operand" "=z")
	(unspec:CCV2 [(match_operand:V2SF 1 "register_operand" "f")
		      (match_operand:V2SF 2 "register_operand" "f")] 
		     UNSPEC_CABS_NGT))]
  "TARGET_MIPS3D"
  "cabs.ngt.ps\t%Q0,%1,%2"
  [(set_attr "type" "fcmp")
   (set_attr "mode" "FPSW")])

;----------------------------------------------------------------------------
; Floating Point Branch Instructions.
;----------------------------------------------------------------------------

; Branch on Any of Four Floating Point Condition Codes True
(define_insn "bc1any4t"
  [(set (pc)
	(if_then_else (ne:CCV4 (match_operand:CCV4 0 "register_operand" "z")
			       (const_int 0))
		      (label_ref (match_operand 1 "" ""))
		      (pc)))]
  "TARGET_MIPS3D"
  "%*bc1any4t\t%Q0,%1%/"
  [(set_attr "type" "branch")
   (set_attr "mode" "none")])

; Branch on Any of Four Floating Point Condition Codes False
(define_insn "bc1any4f"
  [(set (pc)
	(if_then_else (ne:CCV4 (match_operand:CCV4 0 "register_operand" "z")
			       (const_int -1))
		      (label_ref (match_operand 1 "" ""))
		      (pc)))]
  "TARGET_MIPS3D"
  "%*bc1any4f\t%Q0,%1%/"
  [(set_attr "type" "branch")
   (set_attr "mode" "none")])

; Branch on Any of Two Floating Point Condition Codes True
(define_insn "bc1any2t"
  [(set (pc)
	(if_then_else (ne:CCV2 (match_operand:CCV2 0 "register_operand" "z")
			       (const_int 0))
		      (label_ref (match_operand 1 "" ""))
		      (pc)))]
  "TARGET_MIPS3D"
  "%*bc1any2t\t%Q0,%1%/"
  [(set_attr "type" "branch")
   (set_attr "mode" "none")])

; Branch on Any of Two Floating Point Condition Codes False
(define_insn "bc1any2f"
  [(set (pc)
	(if_then_else (ne:CCV2 (match_operand:CCV2 0 "register_operand" "z")
			       (const_int -1))
		      (label_ref (match_operand 1 "" ""))
		      (pc)))]
  "TARGET_MIPS3D"
  "%*bc1any2f\t%Q0,%1%/"
  [(set_attr "type" "branch")
   (set_attr "mode" "none")])

;----------------------------------------------------------------------------
; Floating Point Reduced Precision Reciprocal Square Root Instructions.
;----------------------------------------------------------------------------

; Floating Point Reduced Precision Reciprocal Square Root
; for Single (Sequence Step 1)
(define_insn "mips_rsqrt1_s"
  [(set (match_operand:SF 0 "register_operand" "=f")
	(unspec:SF [(match_operand:SF 1 "register_operand" "f")]
		   UNSPEC_RSQRT1_S))]
  "TARGET_MIPS3D"
  "rsqrt1.s\t%0,%1"
  [(set_attr "type" "frsqrt")
   (set_attr "mode" "SF")])

; Floating Point Reduced Precision Reciprocal Square Root
; for Double (Sequence Step 1)
(define_insn "mips_rsqrt1_d"
  [(set (match_operand:DF 0 "register_operand" "=f")
	(unspec:DF [(match_operand:DF 1 "register_operand" "f")]
		   UNSPEC_RSQRT1_D))]
  "TARGET_MIPS3D"
  "rsqrt1.d\t%0,%1"
  [(set_attr "type" "frsqrt")
   (set_attr "mode" "DF")])

; Floating Point Reduced Precision Reciprocal Square Root
; for Paired Singles (Sequence Step 1)
(define_insn "mips_rsqrt1_ps"
  [(set (match_operand:V2SF 0 "register_operand" "=f")
	(unspec:V2SF [(match_operand:V2SF 1 "register_operand" "f")]
		   UNSPEC_RSQRT1_PS))]
  "TARGET_MIPS3D"
  "rsqrt1.ps\t%0,%1"
  [(set_attr "type" "frsqrt")
   (set_attr "mode" "SF")])

; Floating Point Reduced Precision Reciprocal Square Root
; for Single (Sequence Step 2)
(define_insn "mips_rsqrt2_s"
  [(set (match_operand:SF 0 "register_operand" "=f")
	(unspec:SF [(match_operand:SF 1 "register_operand" "f")
		    (match_operand:SF 2 "register_operand" "f")]
		   UNSPEC_RSQRT2_S))]
  "TARGET_MIPS3D"
  "rsqrt2.s\t%0,%1,%2"
  [(set_attr "type" "frsqrt")
   (set_attr "mode" "SF")])

; Floating Point Reduced Precision Reciprocal Square Root
; for Double (Sequence Step 2)
(define_insn "mips_rsqrt2_d"
  [(set (match_operand:DF 0 "register_operand" "=f")
	(unspec:DF [(match_operand:DF 1 "register_operand" "f")
		    (match_operand:DF 2 "register_operand" "f")]
		   UNSPEC_RSQRT2_D))]
  "TARGET_MIPS3D"
  "rsqrt2.d\t%0,%1,%2"
  [(set_attr "type" "frsqrt")
   (set_attr "mode" "DF")])

; Floating Point Reduced Precision Reciprocal Square Root 
; for Paired Singles (Sequence Step 2)
(define_insn "mips_rsqrt2_ps"
  [(set (match_operand:V2SF 0 "register_operand" "=f")
	(unspec:V2SF [(match_operand:V2SF 1 "register_operand" "f")
		      (match_operand:V2SF 2 "register_operand" "f")]
		     UNSPEC_RSQRT2_PS))]
  "TARGET_MIPS3D"
  "rsqrt2.ps\t%0,%1,%2"
  [(set_attr "type" "frsqrt")
   (set_attr "mode" "SF")])

; Floating Point Reduced Precision Reciprocal for Single (Sequence Step 1)
(define_insn "mips_recip1_s"
  [(set (match_operand:SF 0 "register_operand" "=f")
	(unspec:SF [(match_operand:SF 1 "register_operand" "f")]
		   UNSPEC_RECIP1_S))]
  "TARGET_MIPS3D"
  "recip1.s\t%0,%1"
  [(set_attr "type" "frdiv")
   (set_attr "mode" "SF")])

; Floating Point Reduced Precision Reciprocal for Double (Sequence Step 1)
(define_insn "mips_recip1_d"
  [(set (match_operand:DF 0 "register_operand" "=f")
	(unspec:DF [(match_operand:DF 1 "register_operand" "f")]
		   UNSPEC_RECIP1_D))]
  "TARGET_MIPS3D"
  "recip1.d\t%0,%1"
  [(set_attr "type" "frdiv")
   (set_attr "mode" "DF")])

; Floating Point Reduced Precision Reciprocal for Paired Singles 
; (Sequence Step 1)
(define_insn "mips_recip1_ps"
  [(set (match_operand:V2SF 0 "register_operand" "=f")
	(unspec:V2SF [(match_operand:V2SF 1 "register_operand" "f")]
		     UNSPEC_RECIP1_PS))]
  "TARGET_MIPS3D"
  "recip1.ps\t%0,%1"
  [(set_attr "type" "frdiv")
   (set_attr "mode" "SF")])

; Floating Point Reduced Precision Reciprocal for Single (Sequence Step 2)
(define_insn "mips_recip2_s"
  [(set (match_operand:SF 0 "register_operand" "=f")
	(unspec:SF [(match_operand:SF 1 "register_operand" "f")
		    (match_operand:SF 2 "register_operand" "f")]
		   UNSPEC_RECIP2_S))]
  "TARGET_MIPS3D"
  "recip2.s\t%0,%1,%2"
  [(set_attr "type" "frdiv")
   (set_attr "mode" "SF")])

; Floating Point Reduced Precision Reciprocal for Double (Sequence Step 2)
(define_insn "mips_recip2_d"
  [(set (match_operand:DF 0 "register_operand" "=f")
	(unspec:DF [(match_operand:DF 1 "register_operand" "f")
		    (match_operand:DF 2 "register_operand" "f")]
		   UNSPEC_RECIP2_D))]
  "TARGET_MIPS3D"
  "recip2.d\t%0,%1,%2"
  [(set_attr "type" "frdiv")
   (set_attr "mode" "DF")])

; Floating Point Reduced Precision Reciprocal for Paired Singles 
; (Sequence Step 2)
(define_insn "mips_recip2_ps"
  [(set (match_operand:V2SF 0 "register_operand" "=f")
	(unspec:V2SF [(match_operand:V2SF 1 "register_operand" "f")
		      (match_operand:V2SF 2 "register_operand" "f")]
		     UNSPEC_RECIP2_PS))]
  "TARGET_MIPS3D"
  "recip2.ps\t%0,%1,%2"
  [(set_attr "type" "frdiv")
   (set_attr "mode" "SF")])
