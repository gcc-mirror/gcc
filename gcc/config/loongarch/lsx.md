;; Machine Description for LARCH Loongson SX ASE
;;
;; Copyright (C) 2018-2024 Free Software Foundation, Inc.
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
;;

(define_c_enum "unspec" [
  UNSPEC_LSX_ABSD_S
  UNSPEC_LSX_VABSD_U
  UNSPEC_LSX_VAVG_S
  UNSPEC_LSX_VAVG_U
  UNSPEC_LSX_VAVGR_S
  UNSPEC_LSX_VAVGR_U
  UNSPEC_LSX_VBITCLR
  UNSPEC_LSX_VBITCLRI
  UNSPEC_LSX_VBITREV
  UNSPEC_LSX_VBITREVI
  UNSPEC_LSX_VBITSET
  UNSPEC_LSX_VBITSETI
  UNSPEC_LSX_BRANCH_V
  UNSPEC_LSX_BRANCH
  UNSPEC_LSX_VFCLASS
  UNSPEC_LSX_VFCVT
  UNSPEC_LSX_VFCVTH
  UNSPEC_LSX_VFCVTL
  UNSPEC_LSX_VFLOGB
  UNSPEC_LSX_VFRECIP
  UNSPEC_LSX_VFRECIPE
  UNSPEC_LSX_VFRINT
  UNSPEC_LSX_VFRSQRT
  UNSPEC_LSX_VFRSQRTE
  UNSPEC_LSX_VFTINT_U
  UNSPEC_LSX_VSAT_S
  UNSPEC_LSX_VSAT_U
  UNSPEC_LSX_VREPLVEI
  UNSPEC_LSX_VSRAR
  UNSPEC_LSX_VSRARI
  UNSPEC_LSX_VSRLR
  UNSPEC_LSX_VSRLRI
  UNSPEC_LSX_VSHUF
  UNSPEC_LSX_VEXTW_S
  UNSPEC_LSX_VEXTW_U
  UNSPEC_LSX_VSLLWIL_S
  UNSPEC_LSX_VSLLWIL_U
  UNSPEC_LSX_VSRAN
  UNSPEC_LSX_VSSRAN_S
  UNSPEC_LSX_VSSRAN_U
  UNSPEC_LSX_VSRAIN
  UNSPEC_LSX_VSRAINS_S
  UNSPEC_LSX_VSRAINS_U
  UNSPEC_LSX_VSRARN
  UNSPEC_LSX_VSRLN
  UNSPEC_LSX_VSRLRN
  UNSPEC_LSX_VSSRLRN_U
  UNSPEC_LSX_VFRSTPI
  UNSPEC_LSX_VFRSTP
  UNSPEC_LSX_VSHUF4I
  UNSPEC_LSX_VBSRL_V
  UNSPEC_LSX_VBSLL_V
  UNSPEC_LSX_VEXTRINS
  UNSPEC_LSX_VMSKLTZ
  UNSPEC_LSX_VSIGNCOV
  UNSPEC_LSX_VFTINT_W_D
  UNSPEC_LSX_VFFINT_S_L
  UNSPEC_LSX_VFTINTRZ_W_D
  UNSPEC_LSX_VFTINTRP_W_D
  UNSPEC_LSX_VFTINTRM_W_D
  UNSPEC_LSX_VFTINTRNE_W_D
  UNSPEC_LSX_VFTINTL_L_S
  UNSPEC_LSX_VFFINTH_D_W
  UNSPEC_LSX_VFFINTL_D_W
  UNSPEC_LSX_VFTINTRZL_L_S
  UNSPEC_LSX_VFTINTRZH_L_S
  UNSPEC_LSX_VFTINTRPL_L_S
  UNSPEC_LSX_VFTINTRPH_L_S
  UNSPEC_LSX_VFTINTRMH_L_S
  UNSPEC_LSX_VFTINTRML_L_S
  UNSPEC_LSX_VFTINTRNEL_L_S
  UNSPEC_LSX_VFTINTRNEH_L_S
  UNSPEC_LSX_VFTINTH_L_H
  UNSPEC_LSX_VSSRARN_S
  UNSPEC_LSX_VSSRARN_U
  UNSPEC_LSX_VSSRLN_U
  UNSPEC_LSX_VSSRLN
  UNSPEC_LSX_VSSRLRN
  UNSPEC_LSX_VLDI
  UNSPEC_LSX_VSHUF_B
  UNSPEC_LSX_VLDX
  UNSPEC_LSX_VSTX
  UNSPEC_LSX_VEXTL_QU_DU
  UNSPEC_LSX_VSETEQZ_V
  UNSPEC_LSX_VADDWEV
  UNSPEC_LSX_VADDWEV2
  UNSPEC_LSX_VADDWEV3
  UNSPEC_LSX_VADDWOD
  UNSPEC_LSX_VADDWOD2
  UNSPEC_LSX_VADDWOD3
  UNSPEC_LSX_VSUBWEV
  UNSPEC_LSX_VSUBWEV2
  UNSPEC_LSX_VSUBWOD
  UNSPEC_LSX_VSUBWOD2
  UNSPEC_LSX_VMULWEV
  UNSPEC_LSX_VMULWEV2
  UNSPEC_LSX_VMULWEV3
  UNSPEC_LSX_VMULWOD
  UNSPEC_LSX_VMULWOD2
  UNSPEC_LSX_VMULWOD3
  UNSPEC_LSX_VHADDW_Q_D
  UNSPEC_LSX_VHADDW_QU_DU
  UNSPEC_LSX_VHSUBW_Q_D
  UNSPEC_LSX_VHSUBW_QU_DU
  UNSPEC_LSX_VMADDWEV
  UNSPEC_LSX_VMADDWEV2
  UNSPEC_LSX_VMADDWEV3
  UNSPEC_LSX_VMADDWOD
  UNSPEC_LSX_VMADDWOD2
  UNSPEC_LSX_VMADDWOD3
  UNSPEC_LSX_VADD_Q
  UNSPEC_LSX_VSUB_Q
  UNSPEC_LSX_VEXTH_Q_D
  UNSPEC_LSX_VEXTH_QU_DU
  UNSPEC_LSX_VMSKGEZ
  UNSPEC_LSX_VMSKNZ
  UNSPEC_LSX_VEXTL_Q_D
  UNSPEC_LSX_VSRLNI
  UNSPEC_LSX_VSRLRNI
  UNSPEC_LSX_VSSRLNI
  UNSPEC_LSX_VSSRLNI2
  UNSPEC_LSX_VSSRLRNI
  UNSPEC_LSX_VSSRLRNI2
  UNSPEC_LSX_VSRANI
  UNSPEC_LSX_VSRARNI
  UNSPEC_LSX_VSSRANI
  UNSPEC_LSX_VSSRANI2
  UNSPEC_LSX_VSSRARNI
  UNSPEC_LSX_VSSRARNI2
  UNSPEC_LSX_VPERMI
  UNSPEC_LSX_VILVL_INTERNAL
  UNSPEC_LSX_VREPLVEI_MIRROR
])

;; This attribute gives suffix for integers in VHMODE.
(define_mode_attr dlsxfmt
  [(V2DI "q")
   (V4SI "d")
   (V8HI "w")
   (V16QI "h")])

(define_mode_attr dlsxfmt_u
  [(V2DI "qu")
   (V4SI "du")
   (V8HI "wu")
   (V16QI "hu")])

(define_mode_attr d2lsxfmt
  [(V4SI "q")
   (V8HI "d")
   (V16QI "w")])

(define_mode_attr d2lsxfmt_u
  [(V4SI "qu")
   (V8HI "du")
   (V16QI "wu")])

;; The attribute gives two double modes for vector modes.
(define_mode_attr VD2MODE
  [(V4SI "V2DI")
   (V8HI "V2DI")
   (V16QI "V4SI")])

;; All vector modes with 128 bits.
(define_mode_iterator LSX      [V2DF V4SF V2DI V4SI V8HI V16QI])

;; Only used for vilvh and splitting insert_d and copy_{u,s}.d.
(define_mode_iterator LSX_D    [V2DI V2DF])

;; Only used for copy_{u,s}.w and vilvh.
(define_mode_iterator LSX_W    [V4SI V4SF])

;; As ILSX but excludes V16QI.
(define_mode_iterator ILSX_DWH [V2DI V4SI V8HI])

;; As LSX but excludes V16QI.
(define_mode_iterator LSX_DWH  [V2DF V4SF V2DI V4SI V8HI])

;; As ILSX but excludes V2DI.
(define_mode_iterator ILSX_WHB [V4SI V8HI V16QI])

;; Only integer modes equal or larger than a word.
(define_mode_iterator ILSX_DW  [V2DI V4SI])

;; Only integer modes smaller than a word.
(define_mode_iterator ILSX_HB  [V8HI V16QI])

;;;; Only integer modes for fixed-point madd_q/maddr_q.
;;(define_mode_iterator ILSX_WH  [V4SI V8HI])

;; Only used for immediate set shuffle elements instruction.
(define_mode_iterator LSX_WHB_W [V4SI V8HI V16QI V4SF])

;; The attribute gives half modes for vector modes.
(define_mode_attr VHMODE
  [(V8HI "V16QI")
   (V4SI "V8HI")
   (V2DI "V4SI")])

;; The attribute gives double modes for vector modes.
(define_mode_attr VDMODE
  [(V2DI "V2DI")
   (V4SI "V2DI")
   (V8HI "V4SI")
   (V16QI "V8HI")])

;; The attribute gives half modes with same number of elements for vector modes.
(define_mode_attr VTRUNCMODE
  [(V8HI "V8QI")
   (V4SI "V4HI")
   (V2DI "V2SI")])

;; Double-sized Vector MODE with same elemet type. "Vector, Enlarged-MODE"
(define_mode_attr VEMODE
  [(V4SF "V8SF")
   (V4SI "V8SI")
   (V2DI "V4DI")
   (V2DF "V4DF")])

;; This attribute gives the mode of the result for "vpickve2gr_b, copy_u_b" etc.
(define_mode_attr VRES
  [(V2DF "DF")
   (V4SF "SF")
   (V2DI "DI")
   (V4SI "SI")
   (V8HI "SI")
   (V16QI "SI")])

;; Only used with LSX_D iterator.
(define_mode_attr lsx_d
  [(V2DI "reg_or_0")
   (V2DF "register")])

;; This attribute gives the integer vector mode with same size.
(define_mode_attr mode_i
  [(V2DF "v2di")
   (V4SF "v4si")
   (V2DI "v2di")
   (V4SI "v4si")
   (V8HI "v8hi")
   (V16QI "v16qi")])

;; This attribute gives suffix for LSX instructions.
(define_mode_attr lsxfmt
  [(V2DF "d")
   (V4SF "w")
   (V2DI "d")
   (V4SI "w")
   (V8HI "h")
   (V16QI "b")])

;; This attribute gives suffix for LSX instructions.
(define_mode_attr lsxfmt_u
  [(V2DF "du")
   (V4SF "wu")
   (V2DI "du")
   (V4SI "wu")
   (V8HI "hu")
   (V16QI "bu")])

;; This attribute gives suffix for integers in VHMODE.
(define_mode_attr hlsxfmt
  [(V2DI "w")
   (V4SI "h")
   (V8HI "b")])

;; This attribute gives suffix for integers in VHMODE.
(define_mode_attr hlsxfmt_u
  [(V2DI "wu")
   (V4SI "hu")
   (V8HI "bu")])

;; This attribute gives define_insn suffix for LSX instructions that need
;; distinction between integer and floating point.
(define_mode_attr lsxfmt_f
  [(V2DF "d_f")
   (V4SF "w_f")
   (V2DI "d")
   (V4SI "w")
   (V8HI "h")
   (V16QI "b")])

(define_mode_attr flsxfmt_f
  [(V2DF "d_f")
   (V4SF "s_f")
   (V2DI "d")
   (V4SI "w")
   (V8HI "h")
   (V16QI "b")])

(define_mode_attr flsxfmt
  [(V2DF "d")
   (V4SF "s")
   (V2DI "d")
   (V4SI "s")])

(define_mode_attr flsxfrint
  [(V2DF "d")
   (V4SF "s")])

(define_mode_attr ilsxfmt
  [(V2DF "l")
   (V4SF "w")])

(define_mode_attr ilsxfmt_u
  [(V2DF "lu")
   (V4SF "wu")])

;; This is used to form an immediate operand constraint using
;; "const_<indeximm>_operand".
(define_mode_attr indeximm
  [(V2DF "0_or_1")
   (V4SF "0_to_3")
   (V2DI "0_or_1")
   (V4SI "0_to_3")
   (V8HI "uimm3")
   (V16QI "uimm4")])

;; This attribute represents bitmask needed for vec_merge using
;; "const_<bitmask>_operand".
(define_mode_attr bitmask
  [(V2DF "exp_2")
   (V4SF "exp_4")
   (V2DI "exp_2")
   (V4SI "exp_4")
   (V8HI "exp_8")
   (V16QI "exp_16")])

(define_expand "vec_init<mode><unitmode>"
  [(match_operand:LSX 0 "register_operand")
   (match_operand:LSX 1 "")]
  "ISA_HAS_LSX"
{
  loongarch_expand_vector_init (operands[0], operands[1]);
  DONE;
})

;; vpickev pattern with implicit type conversion.
(define_insn "vec_pack_trunc_<mode>"
  [(set (match_operand:<VHMODE> 0 "register_operand" "=f")
	(vec_concat:<VHMODE>
	  (truncate:<VTRUNCMODE>
	    (match_operand:ILSX_DWH 1 "register_operand" "f"))
	  (truncate:<VTRUNCMODE>
	    (match_operand:ILSX_DWH 2 "register_operand" "f"))))]
  "ISA_HAS_LSX"
  "vpickev.<hlsxfmt>\t%w0,%w2,%w1"
  [(set_attr "type" "simd_permute")
   (set_attr "mode" "<MODE>")])

(define_expand "vec_unpacks_hi_v4sf"
  [(set (match_operand:V2DF 0 "register_operand" "=f")
	(float_extend:V2DF
	  (vec_select:V2SF
	    (match_operand:V4SF 1 "register_operand" "f")
	    (match_dup 2))))]
  "ISA_HAS_LSX"
{
  operands[2] = loongarch_lsx_vec_parallel_const_half (V4SFmode,
      true/*high_p*/);
})

(define_expand "vec_unpacks_lo_v4sf"
  [(set (match_operand:V2DF 0 "register_operand" "=f")
	(float_extend:V2DF
	  (vec_select:V2SF
	    (match_operand:V4SF 1 "register_operand" "f")
	    (match_dup 2))))]
  "ISA_HAS_LSX"
{
  operands[2] = loongarch_lsx_vec_parallel_const_half (V4SFmode,
      false/*high_p*/);
})

(define_expand "vec_unpacks_hi_<mode>"
  [(match_operand:<VDMODE> 0 "register_operand")
   (match_operand:ILSX_WHB 1 "register_operand")]
  "ISA_HAS_LSX"
{
  loongarch_expand_vec_unpack (operands, false/*unsigned_p*/, true/*high_p*/);
  DONE;
})

(define_expand "vec_unpacks_lo_<mode>"
  [(match_operand:<VDMODE> 0 "register_operand")
   (match_operand:ILSX_WHB 1 "register_operand")]
  "ISA_HAS_LSX"
{
  loongarch_expand_vec_unpack (operands, false/*unsigned_p*/, false/*high_p*/);
  DONE;
})

(define_expand "vec_unpacku_hi_<mode>"
  [(match_operand:<VDMODE> 0 "register_operand")
   (match_operand:ILSX_WHB 1 "register_operand")]
  "ISA_HAS_LSX"
{
  loongarch_expand_vec_unpack (operands, true/*unsigned_p*/, true/*high_p*/);
  DONE;
})

(define_expand "vec_unpacku_lo_<mode>"
  [(match_operand:<VDMODE> 0 "register_operand")
   (match_operand:ILSX_WHB 1 "register_operand")]
  "ISA_HAS_LSX"
{
  loongarch_expand_vec_unpack (operands, true/*unsigned_p*/, false/*high_p*/);
  DONE;
})

(define_expand "vec_extract<mode><unitmode>"
  [(match_operand:<UNITMODE> 0 "register_operand")
   (match_operand:ILSX 1 "register_operand")
   (match_operand 2 "const_<indeximm>_operand")]
  "ISA_HAS_LSX"
{
  if (<UNITMODE>mode == QImode || <UNITMODE>mode == HImode)
    {
      rtx dest1 = gen_reg_rtx (SImode);
      emit_insn (gen_lsx_vpickve2gr_<lsxfmt> (dest1, operands[1], operands[2]));
      emit_move_insn (operands[0],
		      gen_lowpart (<UNITMODE>mode, dest1));
    }
  else
    emit_insn (gen_lsx_vpickve2gr_<lsxfmt> (operands[0], operands[1], operands[2]));
  DONE;
})

(define_expand "vec_extract<mode><unitmode>"
  [(match_operand:<UNITMODE> 0 "register_operand")
   (match_operand:FLSX 1 "register_operand")
   (match_operand 2 "const_<indeximm>_operand")]
  "ISA_HAS_LSX"
{
  rtx temp;
  HOST_WIDE_INT val = INTVAL (operands[2]);

  if (val == 0)
    temp = operands[1];
  else
    {
      rtx n = GEN_INT (val * GET_MODE_SIZE (<UNITMODE>mode));
      temp = gen_reg_rtx (<MODE>mode);
      emit_insn (gen_lsx_vbsrl_<lsxfmt_f> (temp, operands[1], n));
    }
  emit_insn (gen_lsx_vec_extract_<lsxfmt_f> (operands[0], temp));
  DONE;
})

(define_insn_and_split "lsx_vec_extract_<lsxfmt_f>"
  [(set (match_operand:<UNITMODE> 0 "register_operand" "=f")
	(vec_select:<UNITMODE>
	  (match_operand:FLSX 1 "register_operand" "f")
	  (parallel [(const_int 0)])))]
  "ISA_HAS_LSX"
  "#"
  "&& reload_completed"
  [(set (match_dup 0) (match_dup 1))]
{
  operands[1] = gen_rtx_REG (<UNITMODE>mode, REGNO (operands[1]));
}
  [(set_attr "move_type" "fmove")
   (set_attr "mode" "<UNITMODE>")])

(define_expand "vec_set<mode>"
  [(match_operand:ILSX 0 "register_operand")
   (match_operand:<UNITMODE> 1 "reg_or_0_operand")
   (match_operand 2 "const_<indeximm>_operand")]
  "ISA_HAS_LSX"
{
  rtx index = GEN_INT (1 << INTVAL (operands[2]));
  emit_insn (gen_lsx_vinsgr2vr_<lsxfmt> (operands[0], operands[1],
					 operands[0], index));
  DONE;
})

(define_expand "vec_set<mode>"
  [(match_operand:FLSX 0 "register_operand")
   (match_operand:<UNITMODE> 1 "register_operand")
   (match_operand 2 "const_<indeximm>_operand")]
  "ISA_HAS_LSX"
{
  rtx index = GEN_INT (1 << INTVAL (operands[2]));
  emit_insn (gen_lsx_vextrins_<lsxfmt_f>_scalar (operands[0], operands[1],
						 operands[0], index));
  DONE;
})

(define_expand "vec_cmp<mode><mode_i>"
  [(set (match_operand:<VIMODE> 0 "register_operand")
	(match_operator 1 ""
	  [(match_operand:LSX 2 "register_operand")
	   (match_operand:LSX 3 "register_operand")]))]
  "ISA_HAS_LSX"
{
  loongarch_expand_vec_cmp (operands);
  DONE;
})

(define_expand "vec_cmpu<ILSX:mode><mode_i>"
  [(set (match_operand:<VIMODE> 0 "register_operand")
	(match_operator 1 ""
	  [(match_operand:ILSX 2 "register_operand")
	   (match_operand:ILSX 3 "register_operand")]))]
  "ISA_HAS_LSX"
{
  loongarch_expand_vec_cmp (operands);
  DONE;
})

(define_expand "vcond_mask_<mode><mode_i>"
  [(match_operand:LSX 0 "register_operand")
   (match_operand:LSX 1 "reg_or_m1_operand")
   (match_operand:LSX 2 "reg_or_0_operand")
   (match_operand:<VIMODE> 3 "register_operand")]
  "ISA_HAS_LSX"
{
  loongarch_expand_vec_cond_mask_expr (<MODE>mode,
				       <VIMODE>mode, operands);
  DONE;
})

(define_insn "lsx_vinsgr2vr_<lsxfmt>"
  [(set (match_operand:ILSX 0 "register_operand" "=f")
	(vec_merge:ILSX
	  (vec_duplicate:ILSX
	    (match_operand:<UNITMODE> 1 "reg_or_0_operand" "rJ"))
	  (match_operand:ILSX 2 "register_operand" "0")
	  (match_operand 3 "const_<bitmask>_operand" "")))]
  "ISA_HAS_LSX"
{
  return "vinsgr2vr.<lsxfmt>\t%w0,%z1,%y3";
}
  [(set_attr "type" "simd_insert")
   (set_attr "mode" "<MODE>")])

(define_insn "lsx_vextrins_<lsxfmt_f>_internal"
  [(set (match_operand:LSX 0 "register_operand" "=f")
	(vec_merge:LSX
	  (vec_duplicate:LSX
	    (vec_select:<UNITMODE>
	      (match_operand:LSX 1 "register_operand" "f")
	      (parallel [(const_int 0)])))
	  (match_operand:LSX 2 "register_operand" "0")
	  (match_operand 3 "const_<bitmask>_operand" "")))]
  "ISA_HAS_LSX"
  "vextrins.<lsxfmt>\t%w0,%w1,%y3<<4"
  [(set_attr "type" "simd_insert")
   (set_attr "mode" "<MODE>")])

;; Operand 3 is a scalar.
(define_insn "lsx_vextrins_<lsxfmt_f>_scalar"
  [(set (match_operand:FLSX 0 "register_operand" "=f")
	(vec_merge:FLSX
	  (vec_duplicate:FLSX
	    (match_operand:<UNITMODE> 1 "register_operand" "f"))
	  (match_operand:FLSX 2 "register_operand" "0")
	  (match_operand 3 "const_<bitmask>_operand" "")))]
  "ISA_HAS_LSX"
  "vextrins.<lsxfmt>\t%w0,%w1,%y3<<4"
  [(set_attr "type" "simd_insert")
   (set_attr "mode" "<MODE>")])

(define_insn "lsx_vpickve2gr_<lsxfmt><u>"
  [(set (match_operand:<VRES> 0 "register_operand" "=r")
	(any_extend:<VRES>
	  (vec_select:<UNITMODE>
	    (match_operand:ILSX_HB 1 "register_operand" "f")
	    (parallel [(match_operand 2 "const_<indeximm>_operand" "")]))))]
  "ISA_HAS_LSX"
  "vpickve2gr.<lsxfmt><u>\t%0,%w1,%2"
  [(set_attr "type" "simd_copy")
   (set_attr "mode" "<MODE>")])

(define_insn "lsx_vpickve2gr_<lsxfmt_f><u>"
  [(set (match_operand:<UNITMODE> 0 "register_operand" "=r")
	(any_extend:<UNITMODE>
	  (vec_select:<UNITMODE>
	    (match_operand:LSX_W 1 "register_operand" "f")
	    (parallel [(match_operand 2 "const_<indeximm>_operand" "")]))))]
  "ISA_HAS_LSX"
  "vpickve2gr.<lsxfmt><u>\t%0,%w1,%2"
  [(set_attr "type" "simd_copy")
   (set_attr "mode" "<MODE>")])

(define_insn "lsx_vpickve2gr_du"
  [(set (match_operand:DI 0 "register_operand" "=r")
	(vec_select:DI
	  (match_operand:V2DI 1 "register_operand" "f")
	  (parallel [(match_operand 2 "const_0_or_1_operand" "")])))]
  "ISA_HAS_LSX"
  "vpickve2gr.du\t%0,%w1,%2"
  [(set_attr "type" "simd_copy")
   (set_attr "mode" "V2DI")])

(define_insn "lsx_vpickve2gr_<lsxfmt_f>"
  [(set (match_operand:<UNITMODE> 0 "register_operand" "=r")
	(vec_select:<UNITMODE>
	  (match_operand:LSX_D 1 "register_operand" "f")
	  (parallel [(match_operand 2 "const_<indeximm>_operand" "")])))]
  "ISA_HAS_LSX"
  "vpickve2gr.<lsxfmt>\t%0,%w1,%2"
  [(set_attr "type" "simd_copy")
   (set_attr "mode" "<MODE>")])

(define_expand "neg<mode>2"
  [(set (match_operand:ILSX 0 "register_operand")
	(neg:ILSX (match_operand:ILSX 1 "register_operand")))]
  "ISA_HAS_LSX"
{
  emit_insn (gen_vneg<mode>2 (operands[0], operands[1]));
  DONE;
})

(define_expand "lsx_vrepli<mode>"
  [(match_operand:ILSX 0 "register_operand")
   (match_operand 1 "const_imm10_operand")]
  "ISA_HAS_LSX"
{
  if (<MODE>mode == V16QImode)
    operands[1] = GEN_INT (trunc_int_for_mode (INTVAL (operands[1]),
					       <UNITMODE>mode));
  emit_move_insn (operands[0],
		  loongarch_gen_const_int_vector (<MODE>mode, INTVAL (operands[1])));
  DONE;
})

(define_expand "vec_perm<mode>"
 [(match_operand:LSX 0 "register_operand")
  (match_operand:LSX 1 "register_operand")
  (match_operand:LSX 2 "register_operand")
  (match_operand:<VIMODE> 3 "register_operand")]
  "ISA_HAS_LSX"
{
  loongarch_expand_vec_perm (operands[0], operands[1],
			     operands[2], operands[3]);
  DONE;
})

(define_insn "lsx_vshuf_<lsxfmt_f>"
  [(set (match_operand:LSX_DWH 0 "register_operand" "=f")
	(unspec:LSX_DWH [(match_operand:LSX_DWH 1 "register_operand" "0")
			 (match_operand:LSX_DWH 2 "register_operand" "f")
			 (match_operand:LSX_DWH 3 "register_operand" "f")]
			UNSPEC_LSX_VSHUF))]
  "ISA_HAS_LSX"
  "vshuf.<lsxfmt>\t%w0,%w2,%w3"
  [(set_attr "type" "simd_sld")
   (set_attr "mode" "<MODE>")])

(define_expand "mov<mode>"
  [(set (match_operand:LSX 0)
	(match_operand:LSX 1))]
  "ISA_HAS_LSX"
{
  if (loongarch_legitimize_move (<MODE>mode, operands[0], operands[1]))
    DONE;
})

(define_expand "movmisalign<mode>"
  [(set (match_operand:LSX 0)
	(match_operand:LSX 1))]
  "ISA_HAS_LSX"
{
  if (loongarch_legitimize_move (<MODE>mode, operands[0], operands[1]))
    DONE;
})

(define_insn "mov<mode>_lsx"
  [(set (match_operand:LSX 0 "nonimmediate_operand" "=f,f,R,*r,*f,*r")
	(match_operand:LSX 1 "move_operand" "fYGYI,R,f,*f,*r,*r"))]
  "ISA_HAS_LSX"
{ return loongarch_output_move (operands[0], operands[1]); }
  [(set_attr "type" "simd_move,simd_load,simd_store,simd_copy,simd_insert,simd_copy")
   (set_attr "mode" "<MODE>")])

(define_split
  [(set (match_operand:LSX 0 "nonimmediate_operand")
	(match_operand:LSX 1 "move_operand"))]
  "reload_completed && ISA_HAS_LSX
   && loongarch_split_move_p (operands[0], operands[1])"
  [(const_int 0)]
{
  loongarch_split_move (operands[0], operands[1]);
  DONE;
})

;; Integer operations
(define_insn "add<mode>3"
  [(set (match_operand:ILSX 0 "register_operand" "=f,f,f")
	(plus:ILSX
	  (match_operand:ILSX 1 "register_operand" "f,f,f")
	  (match_operand:ILSX 2 "reg_or_vector_same_ximm5_operand" "f,Unv5,Uuv5")))]
  "ISA_HAS_LSX"
{
  switch (which_alternative)
    {
    case 0:
      return "vadd.<lsxfmt>\t%w0,%w1,%w2";
    case 1:
      {
	HOST_WIDE_INT val = INTVAL (CONST_VECTOR_ELT (operands[2], 0));

	operands[2] = GEN_INT (-val);
	return "vsubi.<lsxfmt_u>\t%w0,%w1,%d2";
      }
    case 2:
      return "vaddi.<lsxfmt_u>\t%w0,%w1,%E2";
    default:
      gcc_unreachable ();
    }
}
  [(set_attr "alu_type" "simd_add")
   (set_attr "type" "simd_int_arith")
   (set_attr "mode" "<MODE>")])

(define_insn "sub<mode>3"
  [(set (match_operand:ILSX 0 "register_operand" "=f,f")
	(minus:ILSX
	  (match_operand:ILSX 1 "register_operand" "f,f")
	  (match_operand:ILSX 2 "reg_or_vector_same_uimm5_operand" "f,Uuv5")))]
  "ISA_HAS_LSX"
  "@
   vsub.<lsxfmt>\t%w0,%w1,%w2
   vsubi.<lsxfmt_u>\t%w0,%w1,%E2"
  [(set_attr "alu_type" "simd_add")
   (set_attr "type" "simd_int_arith")
   (set_attr "mode" "<MODE>")])

(define_insn "mul<mode>3"
  [(set (match_operand:ILSX 0 "register_operand" "=f")
	(mult:ILSX (match_operand:ILSX 1 "register_operand" "f")
		   (match_operand:ILSX 2 "register_operand" "f")))]
  "ISA_HAS_LSX"
  "vmul.<lsxfmt>\t%w0,%w1,%w2"
  [(set_attr "type" "simd_mul")
   (set_attr "mode" "<MODE>")])

(define_insn "lsx_vmadd_<lsxfmt>"
  [(set (match_operand:ILSX 0 "register_operand" "=f")
	(plus:ILSX (mult:ILSX (match_operand:ILSX 2 "register_operand" "f")
			      (match_operand:ILSX 3 "register_operand" "f"))
		   (match_operand:ILSX 1 "register_operand" "0")))]
  "ISA_HAS_LSX"
  "vmadd.<lsxfmt>\t%w0,%w2,%w3"
  [(set_attr "type" "simd_mul")
   (set_attr "mode" "<MODE>")])

(define_insn "lsx_vmsub_<lsxfmt>"
  [(set (match_operand:ILSX 0 "register_operand" "=f")
	(minus:ILSX (match_operand:ILSX 1 "register_operand" "0")
		    (mult:ILSX (match_operand:ILSX 2 "register_operand" "f")
			       (match_operand:ILSX 3 "register_operand" "f"))))]
  "ISA_HAS_LSX"
  "vmsub.<lsxfmt>\t%w0,%w2,%w3"
  [(set_attr "type" "simd_mul")
   (set_attr "mode" "<MODE>")])

(define_insn "div<mode>3"
  [(set (match_operand:ILSX 0 "register_operand" "=f")
	(div:ILSX (match_operand:ILSX 1 "register_operand" "f")
		  (match_operand:ILSX 2 "register_operand" "f")))]
  "ISA_HAS_LSX"
{ return loongarch_lsx_output_division ("vdiv.<lsxfmt>\t%w0,%w1,%w2", operands); }
  [(set_attr "type" "simd_div")
   (set_attr "mode" "<MODE>")])

(define_insn "udiv<mode>3"
  [(set (match_operand:ILSX 0 "register_operand" "=f")
	(udiv:ILSX (match_operand:ILSX 1 "register_operand" "f")
		   (match_operand:ILSX 2 "register_operand" "f")))]
  "ISA_HAS_LSX"
{ return loongarch_lsx_output_division ("vdiv.<lsxfmt_u>\t%w0,%w1,%w2", operands); }
  [(set_attr "type" "simd_div")
   (set_attr "mode" "<MODE>")])

(define_insn "mod<mode>3"
  [(set (match_operand:ILSX 0 "register_operand" "=f")
	(mod:ILSX (match_operand:ILSX 1 "register_operand" "f")
		  (match_operand:ILSX 2 "register_operand" "f")))]
  "ISA_HAS_LSX"
{ return loongarch_lsx_output_division ("vmod.<lsxfmt>\t%w0,%w1,%w2", operands); }
  [(set_attr "type" "simd_div")
   (set_attr "mode" "<MODE>")])

(define_insn "umod<mode>3"
  [(set (match_operand:ILSX 0 "register_operand" "=f")
	(umod:ILSX (match_operand:ILSX 1 "register_operand" "f")
		   (match_operand:ILSX 2 "register_operand" "f")))]
  "ISA_HAS_LSX"
{ return loongarch_lsx_output_division ("vmod.<lsxfmt_u>\t%w0,%w1,%w2", operands); }
  [(set_attr "type" "simd_div")
   (set_attr "mode" "<MODE>")])

(define_insn "xor<mode>3"
  [(set (match_operand:LSX 0 "register_operand" "=f,f,f")
	(xor:LSX
	  (match_operand:LSX 1 "register_operand" "f,f,f")
	  (match_operand:LSX 2 "reg_or_vector_same_val_operand" "f,YC,Urv8")))]
  "ISA_HAS_LSX"
  "@
   vxor.v\t%w0,%w1,%w2
   vbitrevi.%v0\t%w0,%w1,%V2
   vxori.b\t%w0,%w1,%B2"
  [(set_attr "type" "simd_logic,simd_bit,simd_logic")
   (set_attr "mode" "<MODE>")])

(define_insn "ior<mode>3"
  [(set (match_operand:LSX 0 "register_operand" "=f,f,f")
	(ior:LSX
	  (match_operand:LSX 1 "register_operand" "f,f,f")
	  (match_operand:LSX 2 "reg_or_vector_same_val_operand" "f,YC,Urv8")))]
  "ISA_HAS_LSX"
  "@
   vor.v\t%w0,%w1,%w2
   vbitseti.%v0\t%w0,%w1,%V2
   vori.b\t%w0,%w1,%B2"
  [(set_attr "type" "simd_logic,simd_bit,simd_logic")
   (set_attr "mode" "<MODE>")])

(define_insn "and<mode>3"
  [(set (match_operand:LSX 0 "register_operand" "=f,f,f")
	(and:LSX
	  (match_operand:LSX 1 "register_operand" "f,f,f")
	  (match_operand:LSX 2 "reg_or_vector_same_val_operand" "f,YZ,Urv8")))]
  "ISA_HAS_LSX"
{
  switch (which_alternative)
    {
    case 0:
      return "vand.v\t%w0,%w1,%w2";
    case 1:
      {
	rtx elt0 = CONST_VECTOR_ELT (operands[2], 0);
	unsigned HOST_WIDE_INT val = ~UINTVAL (elt0);
	operands[2] = loongarch_gen_const_int_vector (<MODE>mode, val & (-val));
	return "vbitclri.%v0\t%w0,%w1,%V2";
      }
    case 2:
      return "vandi.b\t%w0,%w1,%B2";
    default:
      gcc_unreachable ();
    }
}
  [(set_attr "type" "simd_logic,simd_bit,simd_logic")
   (set_attr "mode" "<MODE>")])

(define_insn "one_cmpl<mode>2"
  [(set (match_operand:ILSX 0 "register_operand" "=f")
	(not:ILSX (match_operand:ILSX 1 "register_operand" "f")))]
  "ISA_HAS_LSX"
  "vnor.v\t%w0,%w1,%w1"
  [(set_attr "type" "simd_logic")
   (set_attr "mode" "TI")])

(define_insn "vlshr<mode>3"
  [(set (match_operand:ILSX 0 "register_operand" "=f,f")
	(lshiftrt:ILSX
	  (match_operand:ILSX 1 "register_operand" "f,f")
	  (match_operand:ILSX 2 "reg_or_vector_same_uimm6_operand" "f,Uuv6")))]
  "ISA_HAS_LSX"
  "@
   vsrl.<lsxfmt>\t%w0,%w1,%w2
   vsrli.<lsxfmt>\t%w0,%w1,%E2"
  [(set_attr "type" "simd_shift")
   (set_attr "mode" "<MODE>")])

(define_insn "vashr<mode>3"
  [(set (match_operand:ILSX 0 "register_operand" "=f,f")
	(ashiftrt:ILSX
	  (match_operand:ILSX 1 "register_operand" "f,f")
	  (match_operand:ILSX 2 "reg_or_vector_same_uimm6_operand" "f,Uuv6")))]
  "ISA_HAS_LSX"
  "@
   vsra.<lsxfmt>\t%w0,%w1,%w2
   vsrai.<lsxfmt>\t%w0,%w1,%E2"
  [(set_attr "type" "simd_shift")
   (set_attr "mode" "<MODE>")])

(define_insn "vashl<mode>3"
  [(set (match_operand:ILSX 0 "register_operand" "=f,f")
	(ashift:ILSX
	  (match_operand:ILSX 1 "register_operand" "f,f")
	  (match_operand:ILSX 2 "reg_or_vector_same_uimm6_operand" "f,Uuv6")))]
  "ISA_HAS_LSX"
  "@
   vsll.<lsxfmt>\t%w0,%w1,%w2
   vslli.<lsxfmt>\t%w0,%w1,%E2"
  [(set_attr "type" "simd_shift")
   (set_attr "mode" "<MODE>")])

;; Floating-point operations
(define_insn "add<mode>3"
  [(set (match_operand:FLSX 0 "register_operand" "=f")
	(plus:FLSX (match_operand:FLSX 1 "register_operand" "f")
		   (match_operand:FLSX 2 "register_operand" "f")))]
  "ISA_HAS_LSX"
  "vfadd.<flsxfmt>\t%w0,%w1,%w2"
  [(set_attr "type" "simd_fadd")
   (set_attr "mode" "<MODE>")])

(define_insn "sub<mode>3"
  [(set (match_operand:FLSX 0 "register_operand" "=f")
	(minus:FLSX (match_operand:FLSX 1 "register_operand" "f")
		    (match_operand:FLSX 2 "register_operand" "f")))]
  "ISA_HAS_LSX"
  "vfsub.<flsxfmt>\t%w0,%w1,%w2"
  [(set_attr "type" "simd_fadd")
   (set_attr "mode" "<MODE>")])

(define_insn "mul<mode>3"
  [(set (match_operand:FLSX 0 "register_operand" "=f")
	(mult:FLSX (match_operand:FLSX 1 "register_operand" "f")
		   (match_operand:FLSX 2 "register_operand" "f")))]
  "ISA_HAS_LSX"
  "vfmul.<flsxfmt>\t%w0,%w1,%w2"
  [(set_attr "type" "simd_fmul")
   (set_attr "mode" "<MODE>")])

(define_expand "div<mode>3"
  [(set (match_operand:FLSX 0 "register_operand")
    (div:FLSX (match_operand:FLSX 1 "reg_or_vecotr_1_operand")
	      (match_operand:FLSX 2 "register_operand")))]
  "ISA_HAS_LSX"
{
  if (<MODE>mode == V4SFmode
    && TARGET_RECIP_VEC_DIV
    && optimize_insn_for_speed_p ()
    && flag_finite_math_only && !flag_trapping_math
    && flag_unsafe_math_optimizations)
  {
    loongarch_emit_swdivsf (operands[0], operands[1],
	operands[2], V4SFmode);
    DONE;
  }
})

(define_insn "*div<mode>3"
  [(set (match_operand:FLSX 0 "register_operand" "=f")
	(div:FLSX (match_operand:FLSX 1 "register_operand" "f")
		  (match_operand:FLSX 2 "register_operand" "f")))]
  "ISA_HAS_LSX"
  "vfdiv.<flsxfmt>\t%w0,%w1,%w2"
  [(set_attr "type" "simd_fdiv")
   (set_attr "mode" "<MODE>")])

(define_insn "fma<mode>4"
  [(set (match_operand:FLSX 0 "register_operand" "=f")
	(fma:FLSX (match_operand:FLSX 1 "register_operand" "f")
		  (match_operand:FLSX 2 "register_operand" "f")
		  (match_operand:FLSX 3 "register_operand" "f")))]
  "ISA_HAS_LSX"
  "vfmadd.<flsxfmt>\t%w0,%w1,%w2,%w3"
  [(set_attr "type" "simd_fmadd")
   (set_attr "mode" "<MODE>")])

(define_insn "fnma<mode>4"
  [(set (match_operand:FLSX 0 "register_operand" "=f")
	(fma:FLSX (neg:FLSX (match_operand:FLSX 1 "register_operand" "f"))
		  (match_operand:FLSX 2 "register_operand" "f")
		  (match_operand:FLSX 3 "register_operand" "0")))]
  "ISA_HAS_LSX"
  "vfnmsub.<flsxfmt>\t%w0,%w1,%w2,%w0"
  [(set_attr "type" "simd_fmadd")
   (set_attr "mode" "<MODE>")])

(define_expand "sqrt<mode>2"
  [(set (match_operand:FLSX 0 "register_operand")
    (sqrt:FLSX (match_operand:FLSX 1 "register_operand")))]
  "ISA_HAS_LSX"
{
  if (<MODE>mode == V4SFmode
      && TARGET_RECIP_VEC_SQRT
      && flag_unsafe_math_optimizations
      && optimize_insn_for_speed_p ()
      && flag_finite_math_only && !flag_trapping_math)
    {
      loongarch_emit_swrsqrtsf (operands[0], operands[1], V4SFmode, 0);
      DONE;
    }
})

(define_insn "*sqrt<mode>2"
  [(set (match_operand:FLSX 0 "register_operand" "=f")
	(sqrt:FLSX (match_operand:FLSX 1 "register_operand" "f")))]
  "ISA_HAS_LSX"
  "vfsqrt.<flsxfmt>\t%w0,%w1"
  [(set_attr "type" "simd_fdiv")
   (set_attr "mode" "<MODE>")])

;; Built-in functions
(define_insn "lsx_vadda_<lsxfmt>"
  [(set (match_operand:ILSX 0 "register_operand" "=f")
	(plus:ILSX (abs:ILSX (match_operand:ILSX 1 "register_operand" "f"))
		   (abs:ILSX (match_operand:ILSX 2 "register_operand" "f"))))]
  "ISA_HAS_LSX"
  "vadda.<lsxfmt>\t%w0,%w1,%w2"
  [(set_attr "type" "simd_int_arith")
   (set_attr "mode" "<MODE>")])

(define_insn "ssadd<mode>3"
  [(set (match_operand:ILSX 0 "register_operand" "=f")
	(ss_plus:ILSX (match_operand:ILSX 1 "register_operand" "f")
		      (match_operand:ILSX 2 "register_operand" "f")))]
  "ISA_HAS_LSX"
  "vsadd.<lsxfmt>\t%w0,%w1,%w2"
  [(set_attr "type" "simd_int_arith")
   (set_attr "mode" "<MODE>")])

(define_insn "usadd<mode>3"
  [(set (match_operand:ILSX 0 "register_operand" "=f")
	(us_plus:ILSX (match_operand:ILSX 1 "register_operand" "f")
		      (match_operand:ILSX 2 "register_operand" "f")))]
  "ISA_HAS_LSX"
  "vsadd.<lsxfmt_u>\t%w0,%w1,%w2"
  [(set_attr "type" "simd_int_arith")
   (set_attr "mode" "<MODE>")])

(define_insn "lsx_vabsd_s_<lsxfmt>"
  [(set (match_operand:ILSX 0 "register_operand" "=f")
	(unspec:ILSX [(match_operand:ILSX 1 "register_operand" "f")
		      (match_operand:ILSX 2 "register_operand" "f")]
		     UNSPEC_LSX_ABSD_S))]
  "ISA_HAS_LSX"
  "vabsd.<lsxfmt>\t%w0,%w1,%w2"
  [(set_attr "type" "simd_int_arith")
   (set_attr "mode" "<MODE>")])

(define_insn "lsx_vabsd_u_<lsxfmt_u>"
  [(set (match_operand:ILSX 0 "register_operand" "=f")
	(unspec:ILSX [(match_operand:ILSX 1 "register_operand" "f")
		      (match_operand:ILSX 2 "register_operand" "f")]
		     UNSPEC_LSX_VABSD_U))]
  "ISA_HAS_LSX"
  "vabsd.<lsxfmt_u>\t%w0,%w1,%w2"
  [(set_attr "type" "simd_int_arith")
   (set_attr "mode" "<MODE>")])

(define_insn "lsx_vavg_s_<lsxfmt>"
  [(set (match_operand:ILSX 0 "register_operand" "=f")
	(unspec:ILSX [(match_operand:ILSX 1 "register_operand" "f")
		      (match_operand:ILSX 2 "register_operand" "f")]
		     UNSPEC_LSX_VAVG_S))]
  "ISA_HAS_LSX"
  "vavg.<lsxfmt>\t%w0,%w1,%w2"
  [(set_attr "type" "simd_int_arith")
   (set_attr "mode" "<MODE>")])

(define_insn "lsx_vavg_u_<lsxfmt_u>"
  [(set (match_operand:ILSX 0 "register_operand" "=f")
	(unspec:ILSX [(match_operand:ILSX 1 "register_operand" "f")
		      (match_operand:ILSX 2 "register_operand" "f")]
		     UNSPEC_LSX_VAVG_U))]
  "ISA_HAS_LSX"
  "vavg.<lsxfmt_u>\t%w0,%w1,%w2"
  [(set_attr "type" "simd_int_arith")
   (set_attr "mode" "<MODE>")])

(define_insn "lsx_vavgr_s_<lsxfmt>"
  [(set (match_operand:ILSX 0 "register_operand" "=f")
	(unspec:ILSX [(match_operand:ILSX 1 "register_operand" "f")
		      (match_operand:ILSX 2 "register_operand" "f")]
		     UNSPEC_LSX_VAVGR_S))]
  "ISA_HAS_LSX"
  "vavgr.<lsxfmt>\t%w0,%w1,%w2"
  [(set_attr "type" "simd_int_arith")
   (set_attr "mode" "<MODE>")])

(define_insn "lsx_vavgr_u_<lsxfmt_u>"
  [(set (match_operand:ILSX 0 "register_operand" "=f")
	(unspec:ILSX [(match_operand:ILSX 1 "register_operand" "f")
		      (match_operand:ILSX 2 "register_operand" "f")]
		     UNSPEC_LSX_VAVGR_U))]
  "ISA_HAS_LSX"
  "vavgr.<lsxfmt_u>\t%w0,%w1,%w2"
  [(set_attr "type" "simd_int_arith")
   (set_attr "mode" "<MODE>")])

(define_insn "lsx_vbitclr_<lsxfmt>"
  [(set (match_operand:ILSX 0 "register_operand" "=f")
	(unspec:ILSX [(match_operand:ILSX 1 "register_operand" "f")
		      (match_operand:ILSX 2 "register_operand" "f")]
		     UNSPEC_LSX_VBITCLR))]
  "ISA_HAS_LSX"
  "vbitclr.<lsxfmt>\t%w0,%w1,%w2"
  [(set_attr "type" "simd_bit")
   (set_attr "mode" "<MODE>")])

(define_insn "lsx_vbitclri_<lsxfmt>"
  [(set (match_operand:ILSX 0 "register_operand" "=f")
	(unspec:ILSX [(match_operand:ILSX 1 "register_operand" "f")
		      (match_operand 2 "const_<bitimm>_operand" "")]
		     UNSPEC_LSX_VBITCLRI))]
  "ISA_HAS_LSX"
  "vbitclri.<lsxfmt>\t%w0,%w1,%2"
  [(set_attr "type" "simd_bit")
   (set_attr "mode" "<MODE>")])

(define_insn "lsx_vbitrev_<lsxfmt>"
  [(set (match_operand:ILSX 0 "register_operand" "=f")
	(unspec:ILSX [(match_operand:ILSX 1 "register_operand" "f")
		      (match_operand:ILSX 2 "register_operand" "f")]
		     UNSPEC_LSX_VBITREV))]
  "ISA_HAS_LSX"
  "vbitrev.<lsxfmt>\t%w0,%w1,%w2"
  [(set_attr "type" "simd_bit")
   (set_attr "mode" "<MODE>")])

(define_insn "lsx_vbitrevi_<lsxfmt>"
  [(set (match_operand:ILSX 0 "register_operand" "=f")
	(unspec:ILSX [(match_operand:ILSX 1 "register_operand" "f")
		       (match_operand 2 "const_lsx_branch_operand" "")]
		     UNSPEC_LSX_VBITREVI))]
  "ISA_HAS_LSX"
  "vbitrevi.<lsxfmt>\t%w0,%w1,%2"
  [(set_attr "type" "simd_bit")
   (set_attr "mode" "<MODE>")])

(define_insn "lsx_vbitsel_<lsxfmt>"
  [(set (match_operand:ILSX 0 "register_operand" "=f")
	(ior:ILSX (and:ILSX (not:ILSX
			      (match_operand:ILSX 3 "register_operand" "f"))
			    (match_operand:ILSX 1 "register_operand" "f"))
		  (and:ILSX (match_dup 3)
			    (match_operand:ILSX 2 "register_operand" "f"))))]
  "ISA_HAS_LSX"
  "vbitsel.v\t%w0,%w1,%w2,%w3"
  [(set_attr "type" "simd_bitmov")
   (set_attr "mode" "<MODE>")])

(define_insn "lsx_vbitseli_b"
  [(set (match_operand:V16QI 0 "register_operand" "=f")
	(ior:V16QI (and:V16QI (not:V16QI
				(match_operand:V16QI 1 "register_operand" "0"))
			      (match_operand:V16QI 2 "register_operand" "f"))
		   (and:V16QI (match_dup 1)
			      (match_operand:V16QI 3 "const_vector_same_val_operand" "Urv8"))))]
  "ISA_HAS_LSX"
  "vbitseli.b\t%w0,%w2,%B3"
  [(set_attr "type" "simd_bitmov")
   (set_attr "mode" "V16QI")])

(define_insn "lsx_vbitset_<lsxfmt>"
  [(set (match_operand:ILSX 0 "register_operand" "=f")
	(unspec:ILSX [(match_operand:ILSX 1 "register_operand" "f")
		      (match_operand:ILSX 2 "register_operand" "f")]
		     UNSPEC_LSX_VBITSET))]
  "ISA_HAS_LSX"
  "vbitset.<lsxfmt>\t%w0,%w1,%w2"
  [(set_attr "type" "simd_bit")
   (set_attr "mode" "<MODE>")])

(define_insn "lsx_vbitseti_<lsxfmt>"
  [(set (match_operand:ILSX 0 "register_operand" "=f")
	(unspec:ILSX [(match_operand:ILSX 1 "register_operand" "f")
		      (match_operand 2 "const_<bitimm>_operand" "")]
		     UNSPEC_LSX_VBITSETI))]
  "ISA_HAS_LSX"
  "vbitseti.<lsxfmt>\t%w0,%w1,%2"
  [(set_attr "type" "simd_bit")
   (set_attr "mode" "<MODE>")])

(define_code_iterator ICC [eq le leu lt ltu])

(define_code_attr icc
  [(eq  "eq")
   (le  "le")
   (leu "le")
   (lt  "lt")
   (ltu "lt")])

(define_code_attr icci
  [(eq  "eqi")
   (le  "lei")
   (leu "lei")
   (lt  "lti")
   (ltu "lti")])

(define_code_attr cmpi
  [(eq   "s")
   (le   "s")
   (leu  "u")
   (lt   "s")
   (ltu  "u")])

(define_code_attr cmpi_1
  [(eq   "")
   (le   "")
   (leu  "u")
   (lt   "")
   (ltu  "u")])

(define_insn "lsx_vs<ICC:icc>_<ILSX:lsxfmt><ICC:cmpi_1>"
  [(set (match_operand:ILSX 0 "register_operand" "=f,f")
	(ICC:ILSX
	  (match_operand:ILSX 1 "register_operand" "f,f")
	  (match_operand:ILSX 2 "reg_or_vector_same_<ICC:cmpi>imm5_operand" "f,U<ICC:cmpi>v5")))]
  "ISA_HAS_LSX"
  "@
   vs<ICC:icc>.<ILSX:lsxfmt><ICC:cmpi_1>\t%w0,%w1,%w2
   vs<ICC:icci>.<ILSX:lsxfmt><ICC:cmpi_1>\t%w0,%w1,%E2"
  [(set_attr "type" "simd_int_arith")
   (set_attr "mode" "<MODE>")])

(define_insn "lsx_vfclass_<flsxfmt>"
  [(set (match_operand:<VIMODE> 0 "register_operand" "=f")
	(unspec:<VIMODE> [(match_operand:FLSX 1 "register_operand" "f")]
			 UNSPEC_LSX_VFCLASS))]
  "ISA_HAS_LSX"
  "vfclass.<flsxfmt>\t%w0,%w1"
  [(set_attr "type" "simd_fclass")
   (set_attr "mode" "<MODE>")])

(define_mode_attr fint
  [(V4SF "v4si")
   (V2DF "v2di")])

(define_mode_attr FINTCNV
  [(V4SF "I2S")
   (V2DF "I2D")])

(define_mode_attr FINTCNV_2
  [(V4SF "S2I")
   (V2DF "D2I")])

(define_insn "float<fint><FLSX:mode>2"
  [(set (match_operand:FLSX 0 "register_operand" "=f")
	(float:FLSX (match_operand:<VIMODE> 1 "register_operand" "f")))]
  "ISA_HAS_LSX"
  "vffint.<flsxfmt>.<ilsxfmt>\t%w0,%w1"
  [(set_attr "type" "simd_fcvt")
   (set_attr "cnv_mode" "<FINTCNV>")
   (set_attr "mode" "<MODE>")])

(define_insn "floatuns<fint><FLSX:mode>2"
  [(set (match_operand:FLSX 0 "register_operand" "=f")
	(unsigned_float:FLSX
	  (match_operand:<VIMODE> 1 "register_operand" "f")))]
  "ISA_HAS_LSX"
  "vffint.<flsxfmt>.<ilsxfmt_u>\t%w0,%w1"
  [(set_attr "type" "simd_fcvt")
   (set_attr "cnv_mode" "<FINTCNV>")
   (set_attr "mode" "<MODE>")])

(define_mode_attr FFQ
  [(V4SF "V8HI")
   (V2DF "V4SI")])

(define_insn "lsx_vreplgr2vr_<lsxfmt_f>"
  [(set (match_operand:ILSX 0 "register_operand" "=f,f")
	(vec_duplicate:ILSX
	  (match_operand:<UNITMODE> 1 "reg_or_0_operand" "r,J")))]
  "ISA_HAS_LSX"
{
  if (which_alternative == 1)
    return "vrepli.b\t%w0,0";

  return "vreplgr2vr.<lsxfmt>\t%w0,%z1";
}
  [(set_attr "type" "simd_fill")
   (set_attr "mode" "<MODE>")])

(define_insn "logb<mode>2"
  [(set (match_operand:FLSX 0 "register_operand" "=f")
	(unspec:FLSX [(match_operand:FLSX 1 "register_operand" "f")]
		     UNSPEC_LSX_VFLOGB))]
  "ISA_HAS_LSX"
  "vflogb.<flsxfmt>\t%w0,%w1"
  [(set_attr "type" "simd_flog2")
   (set_attr "mode" "<MODE>")])

;; Only for loongarch_expand_vector_init in loongarch.cc.
;; Merge two scalar floating-point op1 and op2 into a LSX op0.
(define_insn "lsx_vilvl_<lsxfmt_f>_internal"
  [(set (match_operand:FLSX 0 "register_operand" "=f")
	(unspec:FLSX [(match_operand:<UNITMODE> 1 "register_operand" "f")
		      (match_operand:<UNITMODE> 2 "register_operand" "f")]
		     UNSPEC_LSX_VILVL_INTERNAL))]
  "ISA_HAS_LSX"
  "vilvl.<lsxfmt>\t%w0,%w2,%w1"
  [(set_attr "type" "simd_permute")
   (set_attr "mode" "<MODE>")])

(define_insn "smax<mode>3"
  [(set (match_operand:FLSX 0 "register_operand" "=f")
	(smax:FLSX (match_operand:FLSX 1 "register_operand" "f")
		   (match_operand:FLSX 2 "register_operand" "f")))]
  "ISA_HAS_LSX"
  "vfmax.<flsxfmt>\t%w0,%w1,%w2"
  [(set_attr "type" "simd_fminmax")
   (set_attr "mode" "<MODE>")])

(define_insn "lsx_vfmaxa_<flsxfmt>"
  [(set (match_operand:FLSX 0 "register_operand" "=f")
	(if_then_else:FLSX
	   (gt (abs:FLSX (match_operand:FLSX 1 "register_operand" "f"))
	       (abs:FLSX (match_operand:FLSX 2 "register_operand" "f")))
	   (match_dup 1)
	   (match_dup 2)))]
  "ISA_HAS_LSX"
  "vfmaxa.<flsxfmt>\t%w0,%w1,%w2"
  [(set_attr "type" "simd_fminmax")
   (set_attr "mode" "<MODE>")])

(define_insn "smin<mode>3"
  [(set (match_operand:FLSX 0 "register_operand" "=f")
	(smin:FLSX (match_operand:FLSX 1 "register_operand" "f")
		   (match_operand:FLSX 2 "register_operand" "f")))]
  "ISA_HAS_LSX"
  "vfmin.<flsxfmt>\t%w0,%w1,%w2"
  [(set_attr "type" "simd_fminmax")
   (set_attr "mode" "<MODE>")])

(define_insn "lsx_vfmina_<flsxfmt>"
  [(set (match_operand:FLSX 0 "register_operand" "=f")
	(if_then_else:FLSX
	   (lt (abs:FLSX (match_operand:FLSX 1 "register_operand" "f"))
	       (abs:FLSX (match_operand:FLSX 2 "register_operand" "f")))
	   (match_dup 1)
	   (match_dup 2)))]
  "ISA_HAS_LSX"
  "vfmina.<flsxfmt>\t%w0,%w1,%w2"
  [(set_attr "type" "simd_fminmax")
   (set_attr "mode" "<MODE>")])

(define_insn "recip<mode>3"
  [(set (match_operand:FLSX 0 "register_operand" "=f")
       (div:FLSX (match_operand:FLSX 1 "const_vector_1_operand" "")
		 (match_operand:FLSX 2 "register_operand" "f")))]
  "ISA_HAS_LSX"
  "vfrecip.<flsxfmt>\t%w0,%w2"
  [(set_attr "type" "simd_fdiv")
   (set_attr "mode" "<MODE>")])

;; Approximate Reciprocal Instructions.

(define_insn "lsx_vfrecipe_<flsxfmt>"
  [(set (match_operand:FLSX 0 "register_operand" "=f")
    (unspec:FLSX [(match_operand:FLSX 1 "register_operand" "f")]
		 UNSPEC_LSX_VFRECIPE))]
  "ISA_HAS_LSX && ISA_HAS_FRECIPE"
  "vfrecipe.<flsxfmt>\t%w0,%w1"
  [(set_attr "type" "simd_fdiv")
   (set_attr "mode" "<MODE>")])

(define_expand "rsqrt<mode>2"
  [(set (match_operand:FLSX 0 "register_operand" "=f")
    (unspec:FLSX [(match_operand:FLSX 1 "register_operand" "f")]
	     UNSPEC_LSX_VFRSQRT))]
 "ISA_HAS_LSX"
{
 if (<MODE>mode == V4SFmode && TARGET_RECIP_VEC_RSQRT)
   {
     loongarch_emit_swrsqrtsf (operands[0], operands[1], V4SFmode, 1);
     DONE;
   }
})

(define_insn "*rsqrt<mode>2"
  [(set (match_operand:FLSX 0 "register_operand" "=f")
    (unspec:FLSX [(match_operand:FLSX 1 "register_operand" "f")]
		 UNSPEC_LSX_VFRSQRT))]
  "ISA_HAS_LSX"
  "vfrsqrt.<flsxfmt>\t%w0,%w1"
  [(set_attr "type" "simd_fdiv")
   (set_attr "mode" "<MODE>")])

;; Approximate Reciprocal Square Root Instructions.

(define_insn "lsx_vfrsqrte_<flsxfmt>"
  [(set (match_operand:FLSX 0 "register_operand" "=f")
    (unspec:FLSX [(match_operand:FLSX 1 "register_operand" "f")]
		 UNSPEC_LSX_VFRSQRTE))]
  "ISA_HAS_LSX && ISA_HAS_FRECIPE"
  "vfrsqrte.<flsxfmt>\t%w0,%w1"
  [(set_attr "type" "simd_fdiv")
   (set_attr "mode" "<MODE>")])

(define_insn "lsx_vftint_u_<ilsxfmt_u>_<flsxfmt>"
  [(set (match_operand:<VIMODE> 0 "register_operand" "=f")
	(unspec:<VIMODE> [(match_operand:FLSX 1 "register_operand" "f")]
			 UNSPEC_LSX_VFTINT_U))]
  "ISA_HAS_LSX"
  "vftint.<ilsxfmt_u>.<flsxfmt>\t%w0,%w1"
  [(set_attr "type" "simd_fcvt")
   (set_attr "cnv_mode" "<FINTCNV_2>")
   (set_attr "mode" "<MODE>")])

(define_insn "fixuns_trunc<FLSX:mode><mode_i>2"
  [(set (match_operand:<VIMODE> 0 "register_operand" "=f")
	(unsigned_fix:<VIMODE> (match_operand:FLSX 1 "register_operand" "f")))]
  "ISA_HAS_LSX"
  "vftintrz.<ilsxfmt_u>.<flsxfmt>\t%w0,%w1"
  [(set_attr "type" "simd_fcvt")
   (set_attr "cnv_mode" "<FINTCNV_2>")
   (set_attr "mode" "<MODE>")])

(define_insn "lsx_vh<optab>w_h<u>_b<u>"
  [(set (match_operand:V8HI 0 "register_operand" "=f")
	(addsub:V8HI
	  (any_extend:V8HI
	    (vec_select:V8QI
	      (match_operand:V16QI 1 "register_operand" "f")
	      (parallel [(const_int 1) (const_int 3)
			 (const_int 5) (const_int 7)
			 (const_int 9) (const_int 11)
			 (const_int 13) (const_int 15)])))
	  (any_extend:V8HI
	    (vec_select:V8QI
	      (match_operand:V16QI 2 "register_operand" "f")
	      (parallel [(const_int 0) (const_int 2)
			 (const_int 4) (const_int 6)
			 (const_int 8) (const_int 10)
			 (const_int 12) (const_int 14)])))))]
  "ISA_HAS_LSX"
  "vh<optab>w.h<u>.b<u>\t%w0,%w1,%w2"
  [(set_attr "type" "simd_int_arith")
   (set_attr "mode" "V8HI")])

(define_insn "lsx_vh<optab>w_w<u>_h<u>"
  [(set (match_operand:V4SI 0 "register_operand" "=f")
	(addsub:V4SI
	  (any_extend:V4SI
	    (vec_select:V4HI
	      (match_operand:V8HI 1 "register_operand" "f")
	      (parallel [(const_int 1) (const_int 3)
			 (const_int 5) (const_int 7)])))
	  (any_extend:V4SI
	    (vec_select:V4HI
	      (match_operand:V8HI 2 "register_operand" "f")
	      (parallel [(const_int 0) (const_int 2)
			 (const_int 4) (const_int 6)])))))]
  "ISA_HAS_LSX"
  "vh<optab>w.w<u>.h<u>\t%w0,%w1,%w2"
  [(set_attr "type" "simd_int_arith")
   (set_attr "mode" "V4SI")])

(define_insn "lsx_vh<optab>w_d<u>_w<u>"
  [(set (match_operand:V2DI 0 "register_operand" "=f")
	(addsub:V2DI
	  (any_extend:V2DI
	    (vec_select:V2SI
	      (match_operand:V4SI 1 "register_operand" "f")
	      (parallel [(const_int 1) (const_int 3)])))
	  (any_extend:V2DI
	    (vec_select:V2SI
	      (match_operand:V4SI 2 "register_operand" "f")
	      (parallel [(const_int 0) (const_int 2)])))))]
  "ISA_HAS_LSX"
  "vh<optab>w.d<u>.w<u>\t%w0,%w1,%w2"
  [(set_attr "type" "simd_int_arith")
   (set_attr "mode" "V2DI")])

(define_insn "lsx_vpackev_b"
  [(set (match_operand:V16QI 0 "register_operand" "=f")
	(vec_select:V16QI
	  (vec_concat:V32QI
	    (match_operand:V16QI 1 "register_operand" "f")
	    (match_operand:V16QI 2 "register_operand" "f"))
	  (parallel [(const_int 0)  (const_int 16)
		     (const_int 2)  (const_int 18)
		     (const_int 4)  (const_int 20)
		     (const_int 6)  (const_int 22)
		     (const_int 8)  (const_int 24)
		     (const_int 10) (const_int 26)
		     (const_int 12) (const_int 28)
		     (const_int 14) (const_int 30)])))]
  "ISA_HAS_LSX"
  "vpackev.b\t%w0,%w2,%w1"
  [(set_attr "type" "simd_permute")
   (set_attr "mode" "V16QI")])

(define_insn "lsx_vpackev_h"
  [(set (match_operand:V8HI 0 "register_operand" "=f")
	(vec_select:V8HI
	  (vec_concat:V16HI
	    (match_operand:V8HI 1 "register_operand" "f")
	    (match_operand:V8HI 2 "register_operand" "f"))
	  (parallel [(const_int 0) (const_int 8)
		     (const_int 2) (const_int 10)
		     (const_int 4) (const_int 12)
		     (const_int 6) (const_int 14)])))]
  "ISA_HAS_LSX"
  "vpackev.h\t%w0,%w2,%w1"
  [(set_attr "type" "simd_permute")
   (set_attr "mode" "V8HI")])

(define_insn "lsx_vpackev_w"
  [(set (match_operand:V4SI 0 "register_operand" "=f")
	(vec_select:V4SI
	  (vec_concat:V8SI
	    (match_operand:V4SI 1 "register_operand" "f")
	    (match_operand:V4SI 2 "register_operand" "f"))
	  (parallel [(const_int 0) (const_int 4)
		     (const_int 2) (const_int 6)])))]
  "ISA_HAS_LSX"
  "vpackev.w\t%w0,%w2,%w1"
  [(set_attr "type" "simd_permute")
   (set_attr "mode" "V4SI")])

(define_insn "lsx_vpackev_w_f"
  [(set (match_operand:V4SF 0 "register_operand" "=f")
	(vec_select:V4SF
	  (vec_concat:V8SF
	    (match_operand:V4SF 1 "register_operand" "f")
	    (match_operand:V4SF 2 "register_operand" "f"))
	  (parallel [(const_int 0) (const_int 4)
		     (const_int 2) (const_int 6)])))]
  "ISA_HAS_LSX"
  "vpackev.w\t%w0,%w2,%w1"
  [(set_attr "type" "simd_permute")
   (set_attr "mode" "V4SF")])

(define_insn "lsx_vilvh_b"
  [(set (match_operand:V16QI 0 "register_operand" "=f")
	(vec_select:V16QI
	  (vec_concat:V32QI
	    (match_operand:V16QI 1 "register_operand" "f")
	    (match_operand:V16QI 2 "register_operand" "f"))
	  (parallel [(const_int 8)  (const_int 24)
		     (const_int 9)  (const_int 25)
		     (const_int 10) (const_int 26)
		     (const_int 11) (const_int 27)
		     (const_int 12) (const_int 28)
		     (const_int 13) (const_int 29)
		     (const_int 14) (const_int 30)
		     (const_int 15) (const_int 31)])))]
  "ISA_HAS_LSX"
  "vilvh.b\t%w0,%w2,%w1"
  [(set_attr "type" "simd_permute")
   (set_attr "mode" "V16QI")])

(define_insn "lsx_vilvh_h"
  [(set (match_operand:V8HI 0 "register_operand" "=f")
	(vec_select:V8HI
	  (vec_concat:V16HI
	    (match_operand:V8HI 1 "register_operand" "f")
	    (match_operand:V8HI 2 "register_operand" "f"))
	  (parallel [(const_int 4) (const_int 12)
		     (const_int 5) (const_int 13)
		     (const_int 6) (const_int 14)
		     (const_int 7) (const_int 15)])))]
  "ISA_HAS_LSX"
  "vilvh.h\t%w0,%w2,%w1"
  [(set_attr "type" "simd_permute")
   (set_attr "mode" "V8HI")])

(define_mode_attr vilvh_suffix
  [(V4SI "") (V4SF "_f")
   (V2DI "") (V2DF "_f")])

(define_insn "lsx_vilvh_w<vilvh_suffix>"
  [(set (match_operand:LSX_W 0 "register_operand" "=f")
	(vec_select:LSX_W
	  (vec_concat:<VEMODE>
	    (match_operand:LSX_W 1 "register_operand" "f")
	    (match_operand:LSX_W 2 "register_operand" "f"))
	  (parallel [(const_int 2) (const_int 6)
		     (const_int 3) (const_int 7)])))]
  "ISA_HAS_LSX"
  "vilvh.w\t%w0,%w2,%w1"
  [(set_attr "type" "simd_permute")
   (set_attr "mode" "<MODE>")])

(define_insn "lsx_vilvh_d<vilvh_suffix>"
  [(set (match_operand:LSX_D 0 "register_operand" "=f")
	(vec_select:LSX_D
	  (vec_concat:<VEMODE>
	    (match_operand:LSX_D 1 "register_operand" "f")
	    (match_operand:LSX_D 2 "register_operand" "f"))
	  (parallel [(const_int 1) (const_int 3)])))]
  "ISA_HAS_LSX"
  "vilvh.d\t%w0,%w2,%w1"
  [(set_attr "type" "simd_permute")
   (set_attr "mode" "<MODE>")])

(define_insn "lsx_vpackod_b"
  [(set (match_operand:V16QI 0 "register_operand" "=f")
	(vec_select:V16QI
	  (vec_concat:V32QI
	    (match_operand:V16QI 1 "register_operand" "f")
	    (match_operand:V16QI 2 "register_operand" "f"))
	  (parallel [(const_int 1)  (const_int 17)
		     (const_int 3)  (const_int 19)
		     (const_int 5)  (const_int 21)
		     (const_int 7)  (const_int 23)
		     (const_int 9)  (const_int 25)
		     (const_int 11) (const_int 27)
		     (const_int 13) (const_int 29)
		     (const_int 15) (const_int 31)])))]
  "ISA_HAS_LSX"
  "vpackod.b\t%w0,%w2,%w1"
  [(set_attr "type" "simd_permute")
   (set_attr "mode" "V16QI")])

(define_insn "lsx_vpackod_h"
  [(set (match_operand:V8HI 0 "register_operand" "=f")
	(vec_select:V8HI
	  (vec_concat:V16HI
	    (match_operand:V8HI 1 "register_operand" "f")
	    (match_operand:V8HI 2 "register_operand" "f"))
	  (parallel [(const_int 1) (const_int 9)
		     (const_int 3) (const_int 11)
		     (const_int 5) (const_int 13)
		     (const_int 7) (const_int 15)])))]
  "ISA_HAS_LSX"
  "vpackod.h\t%w0,%w2,%w1"
  [(set_attr "type" "simd_permute")
   (set_attr "mode" "V8HI")])

(define_insn "lsx_vpackod_w"
  [(set (match_operand:V4SI 0 "register_operand" "=f")
	(vec_select:V4SI
	  (vec_concat:V8SI
	    (match_operand:V4SI 1 "register_operand" "f")
	    (match_operand:V4SI 2 "register_operand" "f"))
	  (parallel [(const_int 1) (const_int 5)
		     (const_int 3) (const_int 7)])))]
  "ISA_HAS_LSX"
  "vpackod.w\t%w0,%w2,%w1"
  [(set_attr "type" "simd_permute")
   (set_attr "mode" "V4SI")])

(define_insn "lsx_vpackod_w_f"
  [(set (match_operand:V4SF 0 "register_operand" "=f")
	(vec_select:V4SF
	  (vec_concat:V8SF
	    (match_operand:V4SF 1 "register_operand" "f")
	    (match_operand:V4SF 2 "register_operand" "f"))
	  (parallel [(const_int 1) (const_int 5)
		     (const_int 3) (const_int 7)])))]
  "ISA_HAS_LSX"
  "vpackod.w\t%w0,%w2,%w1"
  [(set_attr "type" "simd_permute")
   (set_attr "mode" "V4SF")])

(define_insn "lsx_vilvl_b"
  [(set (match_operand:V16QI 0 "register_operand" "=f")
	(vec_select:V16QI
	  (vec_concat:V32QI
	    (match_operand:V16QI 1 "register_operand" "f")
	    (match_operand:V16QI 2 "register_operand" "f"))
	  (parallel [(const_int 0) (const_int 16)
		     (const_int 1) (const_int 17)
		     (const_int 2) (const_int 18)
		     (const_int 3) (const_int 19)
		     (const_int 4) (const_int 20)
		     (const_int 5) (const_int 21)
		     (const_int 6) (const_int 22)
		     (const_int 7) (const_int 23)])))]
  "ISA_HAS_LSX"
  "vilvl.b\t%w0,%w2,%w1"
  [(set_attr "type" "simd_permute")
   (set_attr "mode" "V16QI")])

(define_insn "lsx_vilvl_h"
  [(set (match_operand:V8HI 0 "register_operand" "=f")
	(vec_select:V8HI
	  (vec_concat:V16HI
	    (match_operand:V8HI 1 "register_operand" "f")
	    (match_operand:V8HI 2 "register_operand" "f"))
	  (parallel [(const_int 0) (const_int 8)
		     (const_int 1) (const_int 9)
		     (const_int 2) (const_int 10)
		     (const_int 3) (const_int 11)])))]
  "ISA_HAS_LSX"
  "vilvl.h\t%w0,%w2,%w1"
  [(set_attr "type" "simd_permute")
   (set_attr "mode" "V8HI")])

(define_insn "lsx_vilvl_w"
  [(set (match_operand:V4SI 0 "register_operand" "=f")
	(vec_select:V4SI
	  (vec_concat:V8SI
	    (match_operand:V4SI 1 "register_operand" "f")
	    (match_operand:V4SI 2 "register_operand" "f"))
	  (parallel [(const_int 0) (const_int 4)
		     (const_int 1) (const_int 5)])))]
  "ISA_HAS_LSX"
  "vilvl.w\t%w0,%w2,%w1"
  [(set_attr "type" "simd_permute")
   (set_attr "mode" "V4SI")])

(define_insn "lsx_vilvl_w_f"
  [(set (match_operand:V4SF 0 "register_operand" "=f")
	(vec_select:V4SF
	  (vec_concat:V8SF
	    (match_operand:V4SF 1 "register_operand" "f")
	    (match_operand:V4SF 2 "register_operand" "f"))
	  (parallel [(const_int 0) (const_int 4)
		     (const_int 1) (const_int 5)])))]
  "ISA_HAS_LSX"
  "vilvl.w\t%w0,%w2,%w1"
  [(set_attr "type" "simd_permute")
   (set_attr "mode" "V4SF")])

(define_insn "lsx_vilvl_d"
  [(set (match_operand:V2DI 0 "register_operand" "=f")
	(vec_select:V2DI
	  (vec_concat:V4DI
	    (match_operand:V2DI 1 "register_operand" "f")
	    (match_operand:V2DI 2 "register_operand" "f"))
	  (parallel [(const_int 0) (const_int 2)])))]
  "ISA_HAS_LSX"
  "vilvl.d\t%w0,%w2,%w1"
  [(set_attr "type" "simd_permute")
   (set_attr "mode" "V2DI")])

(define_insn "lsx_vilvl_d_f"
  [(set (match_operand:V2DF 0 "register_operand" "=f")
	(vec_select:V2DF
	  (vec_concat:V4DF
	    (match_operand:V2DF 1 "register_operand" "f")
	    (match_operand:V2DF 2 "register_operand" "f"))
	  (parallel [(const_int 0) (const_int 2)])))]
  "ISA_HAS_LSX"
  "vilvl.d\t%w0,%w2,%w1"
  [(set_attr "type" "simd_permute")
   (set_attr "mode" "V2DF")])

(define_insn "smax<mode>3"
  [(set (match_operand:ILSX 0 "register_operand" "=f,f")
	(smax:ILSX (match_operand:ILSX 1 "register_operand" "f,f")
		   (match_operand:ILSX 2 "reg_or_vector_same_simm5_operand" "f,Usv5")))]
  "ISA_HAS_LSX"
  "@
   vmax.<lsxfmt>\t%w0,%w1,%w2
   vmaxi.<lsxfmt>\t%w0,%w1,%E2"
  [(set_attr "type" "simd_int_arith")
   (set_attr "mode" "<MODE>")])

(define_insn "umax<mode>3"
  [(set (match_operand:ILSX 0 "register_operand" "=f,f")
	(umax:ILSX (match_operand:ILSX 1 "register_operand" "f,f")
		   (match_operand:ILSX 2 "reg_or_vector_same_uimm5_operand" "f,Uuv5")))]
  "ISA_HAS_LSX"
  "@
   vmax.<lsxfmt_u>\t%w0,%w1,%w2
   vmaxi.<lsxfmt_u>\t%w0,%w1,%B2"
  [(set_attr "type" "simd_int_arith")
   (set_attr "mode" "<MODE>")])

(define_insn "smin<mode>3"
  [(set (match_operand:ILSX 0 "register_operand" "=f,f")
	(smin:ILSX (match_operand:ILSX 1 "register_operand" "f,f")
		   (match_operand:ILSX 2 "reg_or_vector_same_simm5_operand" "f,Usv5")))]
  "ISA_HAS_LSX"
  "@
   vmin.<lsxfmt>\t%w0,%w1,%w2
   vmini.<lsxfmt>\t%w0,%w1,%E2"
  [(set_attr "type" "simd_int_arith")
   (set_attr "mode" "<MODE>")])

(define_insn "umin<mode>3"
  [(set (match_operand:ILSX 0 "register_operand" "=f,f")
	(umin:ILSX (match_operand:ILSX 1 "register_operand" "f,f")
		   (match_operand:ILSX 2 "reg_or_vector_same_uimm5_operand" "f,Uuv5")))]
  "ISA_HAS_LSX"
  "@
   vmin.<lsxfmt_u>\t%w0,%w1,%w2
   vmini.<lsxfmt_u>\t%w0,%w1,%B2"
  [(set_attr "type" "simd_int_arith")
   (set_attr "mode" "<MODE>")])

(define_insn "lsx_vclo_<lsxfmt>"
  [(set (match_operand:ILSX 0 "register_operand" "=f")
	(clz:ILSX (not:ILSX (match_operand:ILSX 1 "register_operand" "f"))))]
  "ISA_HAS_LSX"
  "vclo.<lsxfmt>\t%w0,%w1"
  [(set_attr "type" "simd_bit")
   (set_attr "mode" "<MODE>")])

(define_insn "clz<mode>2"
  [(set (match_operand:ILSX 0 "register_operand" "=f")
	(clz:ILSX (match_operand:ILSX 1 "register_operand" "f")))]
  "ISA_HAS_LSX"
  "vclz.<lsxfmt>\t%w0,%w1"
  [(set_attr "type" "simd_bit")
   (set_attr "mode" "<MODE>")])

(define_insn "lsx_nor_<lsxfmt>"
  [(set (match_operand:ILSX 0 "register_operand" "=f,f")
	(and:ILSX (not:ILSX (match_operand:ILSX 1 "register_operand" "f,f"))
		  (not:ILSX (match_operand:ILSX 2 "reg_or_vector_same_val_operand" "f,Urv8"))))]
  "ISA_HAS_LSX"
  "@
   vnor.v\t%w0,%w1,%w2
   vnori.b\t%w0,%w1,%B2"
  [(set_attr "type" "simd_logic")
   (set_attr "mode" "<MODE>")])

(define_insn "lsx_vpickev_b"
[(set (match_operand:V16QI 0 "register_operand" "=f")
      (vec_select:V16QI
	(vec_concat:V32QI
	  (match_operand:V16QI 1 "register_operand" "f")
	  (match_operand:V16QI 2 "register_operand" "f"))
	(parallel [(const_int 0) (const_int 2)
		   (const_int 4) (const_int 6)
		   (const_int 8) (const_int 10)
		   (const_int 12) (const_int 14)
		   (const_int 16) (const_int 18)
		   (const_int 20) (const_int 22)
		   (const_int 24) (const_int 26)
		   (const_int 28) (const_int 30)])))]
  "ISA_HAS_LSX"
  "vpickev.b\t%w0,%w2,%w1"
  [(set_attr "type" "simd_permute")
   (set_attr "mode" "V16QI")])

(define_insn "lsx_vpickev_h"
[(set (match_operand:V8HI 0 "register_operand" "=f")
      (vec_select:V8HI
	(vec_concat:V16HI
	  (match_operand:V8HI 1 "register_operand" "f")
	  (match_operand:V8HI 2 "register_operand" "f"))
	(parallel [(const_int 0) (const_int 2)
		   (const_int 4) (const_int 6)
		   (const_int 8) (const_int 10)
		   (const_int 12) (const_int 14)])))]
  "ISA_HAS_LSX"
  "vpickev.h\t%w0,%w2,%w1"
  [(set_attr "type" "simd_permute")
   (set_attr "mode" "V8HI")])

(define_insn "lsx_vpickev_w"
[(set (match_operand:V4SI 0 "register_operand" "=f")
      (vec_select:V4SI
	(vec_concat:V8SI
	  (match_operand:V4SI 1 "register_operand" "f")
	  (match_operand:V4SI 2 "register_operand" "f"))
	(parallel [(const_int 0) (const_int 2)
		   (const_int 4) (const_int 6)])))]
  "ISA_HAS_LSX"
  "vpickev.w\t%w0,%w2,%w1"
  [(set_attr "type" "simd_permute")
   (set_attr "mode" "V4SI")])

(define_insn "lsx_vpickev_w_f"
[(set (match_operand:V4SF 0 "register_operand" "=f")
      (vec_select:V4SF
	(vec_concat:V8SF
	  (match_operand:V4SF 1 "register_operand" "f")
	  (match_operand:V4SF 2 "register_operand" "f"))
	(parallel [(const_int 0) (const_int 2)
		   (const_int 4) (const_int 6)])))]
  "ISA_HAS_LSX"
  "vpickev.w\t%w0,%w2,%w1"
  [(set_attr "type" "simd_permute")
   (set_attr "mode" "V4SF")])

(define_insn "lsx_vpickod_b"
[(set (match_operand:V16QI 0 "register_operand" "=f")
      (vec_select:V16QI
	(vec_concat:V32QI
	  (match_operand:V16QI 1 "register_operand" "f")
	  (match_operand:V16QI 2 "register_operand" "f"))
	(parallel [(const_int 1) (const_int 3)
		   (const_int 5) (const_int 7)
		   (const_int 9) (const_int 11)
		   (const_int 13) (const_int 15)
		   (const_int 17) (const_int 19)
		   (const_int 21) (const_int 23)
		   (const_int 25) (const_int 27)
		   (const_int 29) (const_int 31)])))]
  "ISA_HAS_LSX"
  "vpickod.b\t%w0,%w2,%w1"
  [(set_attr "type" "simd_permute")
   (set_attr "mode" "V16QI")])

(define_insn "lsx_vpickod_h"
[(set (match_operand:V8HI 0 "register_operand" "=f")
      (vec_select:V8HI
	(vec_concat:V16HI
	  (match_operand:V8HI 1 "register_operand" "f")
	  (match_operand:V8HI 2 "register_operand" "f"))
	(parallel [(const_int 1) (const_int 3)
		   (const_int 5) (const_int 7)
		   (const_int 9) (const_int 11)
		   (const_int 13) (const_int 15)])))]
  "ISA_HAS_LSX"
  "vpickod.h\t%w0,%w2,%w1"
  [(set_attr "type" "simd_permute")
   (set_attr "mode" "V8HI")])

(define_insn "lsx_vpickod_w"
[(set (match_operand:V4SI 0 "register_operand" "=f")
      (vec_select:V4SI
	(vec_concat:V8SI
	  (match_operand:V4SI 1 "register_operand" "f")
	  (match_operand:V4SI 2 "register_operand" "f"))
	(parallel [(const_int 1) (const_int 3)
		   (const_int 5) (const_int 7)])))]
  "ISA_HAS_LSX"
  "vpickod.w\t%w0,%w2,%w1"
  [(set_attr "type" "simd_permute")
   (set_attr "mode" "V4SI")])

(define_insn "lsx_vpickod_w_f"
[(set (match_operand:V4SF 0 "register_operand" "=f")
      (vec_select:V4SF
	(vec_concat:V8SF
	  (match_operand:V4SF 1 "register_operand" "f")
	  (match_operand:V4SF 2 "register_operand" "f"))
	(parallel [(const_int 1) (const_int 3)
		   (const_int 5) (const_int 7)])))]
  "ISA_HAS_LSX"
  "vpickod.w\t%w0,%w2,%w1"
  [(set_attr "type" "simd_permute")
   (set_attr "mode" "V4SF")])

(define_insn "popcount<mode>2"
  [(set (match_operand:ILSX 0 "register_operand" "=f")
	(popcount:ILSX (match_operand:ILSX 1 "register_operand" "f")))]
  "ISA_HAS_LSX"
  "vpcnt.<lsxfmt>\t%w0,%w1"
  [(set_attr "type" "simd_pcnt")
   (set_attr "mode" "<MODE>")])

(define_insn "lsx_vsat_s_<lsxfmt>"
  [(set (match_operand:ILSX 0 "register_operand" "=f")
	(unspec:ILSX [(match_operand:ILSX 1 "register_operand" "f")
		      (match_operand 2 "const_<bitimm>_operand" "")]
		     UNSPEC_LSX_VSAT_S))]
  "ISA_HAS_LSX"
  "vsat.<lsxfmt>\t%w0,%w1,%2"
  [(set_attr "type" "simd_sat")
   (set_attr "mode" "<MODE>")])

(define_insn "lsx_vsat_u_<lsxfmt_u>"
  [(set (match_operand:ILSX 0 "register_operand" "=f")
	(unspec:ILSX [(match_operand:ILSX 1 "register_operand" "f")
		      (match_operand 2 "const_<bitimm>_operand" "")]
		     UNSPEC_LSX_VSAT_U))]
  "ISA_HAS_LSX"
  "vsat.<lsxfmt_u>\t%w0,%w1,%2"
  [(set_attr "type" "simd_sat")
   (set_attr "mode" "<MODE>")])

(define_insn "lsx_vshuf4i_<lsxfmt_f>"
  [(set (match_operand:LSX_WHB_W 0 "register_operand" "=f")
	(vec_select:LSX_WHB_W
	  (match_operand:LSX_WHB_W 1 "register_operand" "f")
	  (match_operand 2 "par_const_vector_shf_set_operand" "")))]
  "ISA_HAS_LSX"
{
  HOST_WIDE_INT val = 0;
  unsigned int i;

  /* We convert the selection to an immediate.  */
  for (i = 0; i < 4; i++)
    val |= INTVAL (XVECEXP (operands[2], 0, i)) << (2 * i);

  operands[2] = GEN_INT (val);
  return "vshuf4i.<lsxfmt>\t%w0,%w1,%X2";
}
  [(set_attr "type" "simd_shf")
   (set_attr "mode" "<MODE>")])

(define_insn "lsx_vsrar_<lsxfmt>"
  [(set (match_operand:ILSX 0 "register_operand" "=f")
	(unspec:ILSX [(match_operand:ILSX 1 "register_operand" "f")
		      (match_operand:ILSX 2 "register_operand" "f")]
		     UNSPEC_LSX_VSRAR))]
  "ISA_HAS_LSX"
  "vsrar.<lsxfmt>\t%w0,%w1,%w2"
  [(set_attr "type" "simd_shift")
   (set_attr "mode" "<MODE>")])

(define_insn "lsx_vsrari_<lsxfmt>"
  [(set (match_operand:ILSX 0 "register_operand" "=f")
	(unspec:ILSX [(match_operand:ILSX 1 "register_operand" "f")
		      (match_operand 2 "const_<bitimm>_operand" "")]
		     UNSPEC_LSX_VSRARI))]
  "ISA_HAS_LSX"
  "vsrari.<lsxfmt>\t%w0,%w1,%2"
  [(set_attr "type" "simd_shift")
   (set_attr "mode" "<MODE>")])

(define_insn "lsx_vsrlr_<lsxfmt>"
  [(set (match_operand:ILSX 0 "register_operand" "=f")
	(unspec:ILSX [(match_operand:ILSX 1 "register_operand" "f")
		      (match_operand:ILSX 2 "register_operand" "f")]
		     UNSPEC_LSX_VSRLR))]
  "ISA_HAS_LSX"
  "vsrlr.<lsxfmt>\t%w0,%w1,%w2"
  [(set_attr "type" "simd_shift")
   (set_attr "mode" "<MODE>")])

(define_insn "lsx_vsrlri_<lsxfmt>"
  [(set (match_operand:ILSX 0 "register_operand" "=f")
	(unspec:ILSX [(match_operand:ILSX 1 "register_operand" "f")
		      (match_operand 2 "const_<bitimm>_operand" "")]
		     UNSPEC_LSX_VSRLRI))]
  "ISA_HAS_LSX"
  "vsrlri.<lsxfmt>\t%w0,%w1,%2"
  [(set_attr "type" "simd_shift")
   (set_attr "mode" "<MODE>")])

(define_insn "lsx_vssub_s_<lsxfmt>"
  [(set (match_operand:ILSX 0 "register_operand" "=f")
	(ss_minus:ILSX (match_operand:ILSX 1 "register_operand" "f")
		      (match_operand:ILSX 2 "register_operand" "f")))]
  "ISA_HAS_LSX"
  "vssub.<lsxfmt>\t%w0,%w1,%w2"
  [(set_attr "type" "simd_int_arith")
   (set_attr "mode" "<MODE>")])

(define_insn "lsx_vssub_u_<lsxfmt_u>"
  [(set (match_operand:ILSX 0 "register_operand" "=f")
	(us_minus:ILSX (match_operand:ILSX 1 "register_operand" "f")
		      (match_operand:ILSX 2 "register_operand" "f")))]
  "ISA_HAS_LSX"
  "vssub.<lsxfmt_u>\t%w0,%w1,%w2"
  [(set_attr "type" "simd_int_arith")
   (set_attr "mode" "<MODE>")])

(define_insn "lsx_vreplve_<lsxfmt_f>"
  [(set (match_operand:LSX 0 "register_operand" "=f")
	(vec_duplicate:LSX
	  (vec_select:<UNITMODE>
	    (match_operand:LSX 1 "register_operand" "f")
	    (parallel [(match_operand:SI 2 "register_operand" "r")]))))]
  "ISA_HAS_LSX"
  "vreplve.<lsxfmt>\t%w0,%w1,%z2"
  [(set_attr "type" "simd_splat")
   (set_attr "mode" "<MODE>")])

(define_insn "lsx_vreplvei_mirror_<lsxfmt_f>"
  [(set (match_operand:LSX 0 "register_operand" "=f")
	(unspec: LSX [(match_operand:LSX 1 "register_operand" "f")
				(match_operand 2 "const_<indeximm>_operand" "")]
				UNSPEC_LSX_VREPLVEI_MIRROR))]
  "ISA_HAS_LSX"
  "vreplvei.d\t%w0,%w1,%2"
  [(set_attr "type" "simd_splat")
   (set_attr "mode" "<MODE>")])

(define_insn "lsx_vreplvei_<lsxfmt_f>"
  [(set (match_operand:LSX 0 "register_operand" "=f")
	(vec_duplicate:LSX
	  (vec_select:<UNITMODE>
	    (match_operand:LSX 1 "register_operand" "f")
	    (parallel [(match_operand 2 "const_<indeximm>_operand" "")]))))]
  "ISA_HAS_LSX"
  "vreplvei.<lsxfmt>\t%w0,%w1,%2"
  [(set_attr "type" "simd_splat")
   (set_attr "mode" "<MODE>")])

(define_insn "lsx_vreplvei_<lsxfmt_f>_scalar"
  [(set (match_operand:LSX 0 "register_operand" "=f")
      (vec_duplicate:LSX
	(match_operand:<UNITMODE> 1 "register_operand" "f")))]
  "ISA_HAS_LSX"
  "vreplvei.<lsxfmt>\t%w0,%w1,0"
  [(set_attr "type" "simd_splat")
   (set_attr "mode" "<MODE>")])

(define_insn "lsx_vfcvt_h_s"
  [(set (match_operand:V8HI 0 "register_operand" "=f")
	(unspec:V8HI [(match_operand:V4SF 1 "register_operand" "f")
		      (match_operand:V4SF 2 "register_operand" "f")]
		     UNSPEC_LSX_VFCVT))]
  "ISA_HAS_LSX"
  "vfcvt.h.s\t%w0,%w1,%w2"
  [(set_attr "type" "simd_fcvt")
   (set_attr "mode" "V8HI")])

(define_insn "lsx_vfcvt_s_d"
  [(set (match_operand:V4SF 0 "register_operand" "=f")
	(unspec:V4SF [(match_operand:V2DF 1 "register_operand" "f")
		      (match_operand:V2DF 2 "register_operand" "f")]
		     UNSPEC_LSX_VFCVT))]
  "ISA_HAS_LSX"
  "vfcvt.s.d\t%w0,%w1,%w2"
  [(set_attr "type" "simd_fcvt")
   (set_attr "mode" "V4SF")])

(define_insn "vec_pack_trunc_v2df"
  [(set (match_operand:V4SF 0 "register_operand" "=f")
	(vec_concat:V4SF
	  (float_truncate:V2SF (match_operand:V2DF 1 "register_operand" "f"))
	  (float_truncate:V2SF (match_operand:V2DF 2 "register_operand" "f"))))]
  "ISA_HAS_LSX"
  "vfcvt.s.d\t%w0,%w2,%w1"
  [(set_attr "type" "simd_fcvt")
   (set_attr "mode" "V4SF")])

(define_insn "lsx_vfcvth_s_h"
  [(set (match_operand:V4SF 0 "register_operand" "=f")
	(unspec:V4SF [(match_operand:V8HI 1 "register_operand" "f")]
		     UNSPEC_LSX_VFCVTH))]
  "ISA_HAS_LSX"
  "vfcvth.s.h\t%w0,%w1"
  [(set_attr "type" "simd_fcvt")
   (set_attr "mode" "V4SF")])

(define_insn "lsx_vfcvth_d_s"
  [(set (match_operand:V2DF 0 "register_operand" "=f")
	(float_extend:V2DF
	(vec_select:V2SF
	  (match_operand:V4SF 1 "register_operand" "f")
	  (parallel [(const_int 2) (const_int 3)]))))]
  "ISA_HAS_LSX"
  "vfcvth.d.s\t%w0,%w1"
  [(set_attr "type" "simd_fcvt")
   (set_attr "mode" "V2DF")])

(define_insn "lsx_vfcvtl_s_h"
  [(set (match_operand:V4SF 0 "register_operand" "=f")
	(unspec:V4SF [(match_operand:V8HI 1 "register_operand" "f")]
		     UNSPEC_LSX_VFCVTL))]
  "ISA_HAS_LSX"
  "vfcvtl.s.h\t%w0,%w1"
  [(set_attr "type" "simd_fcvt")
   (set_attr "mode" "V4SF")])

(define_insn "lsx_vfcvtl_d_s"
  [(set (match_operand:V2DF 0 "register_operand" "=f")
	(float_extend:V2DF
	(vec_select:V2SF
	  (match_operand:V4SF 1 "register_operand" "f")
	  (parallel [(const_int 0) (const_int 1)]))))]
  "ISA_HAS_LSX"
  "vfcvtl.d.s\t%w0,%w1"
  [(set_attr "type" "simd_fcvt")
   (set_attr "mode" "V2DF")])

(define_code_attr lsxbr
  [(eq "bz")
   (ne "bnz")])

(define_code_attr lsxeq_v
  [(eq "eqz")
   (ne "nez")])

(define_code_attr lsxne_v
  [(eq "nez")
   (ne "eqz")])

(define_code_attr lsxeq
  [(eq "anyeqz")
   (ne "allnez")])

(define_code_attr lsxne
  [(eq "allnez")
   (ne "anyeqz")])

(define_insn "lsx_<lsxbr>_<lsxfmt_f>"
 [(set (pc) (if_then_else
	      (equality_op
		(unspec:SI [(match_operand:LSX 1 "register_operand" "f")]
			    UNSPEC_LSX_BRANCH)
		  (match_operand:SI 2 "const_0_operand"))
		  (label_ref (match_operand 0))
		  (pc)))
      (clobber (match_scratch:FCC 3 "=z"))]
 "ISA_HAS_LSX"
{
  return loongarch_output_conditional_branch (insn, operands,
					 "vset<lsxeq>.<lsxfmt>\t%Z3%w1\n\tbcnez\t%Z3%0",
					 "vset<lsxne>.<lsxfmt>\t%Z3%w1\n\tbcnez\t%Z3%0");
}
 [(set_attr "type" "simd_branch")
  (set_attr "mode" "<MODE>")])

(define_insn "lsx_<lsxbr>_v_<lsxfmt_f>"
 [(set (pc) (if_then_else
	      (equality_op
		(unspec:SI [(match_operand:LSX 1 "register_operand" "f")]
			    UNSPEC_LSX_BRANCH_V)
		  (match_operand:SI 2 "const_0_operand"))
		  (label_ref (match_operand 0))
		  (pc)))
      (clobber (match_scratch:FCC 3 "=z"))]
 "ISA_HAS_LSX"
{
  return loongarch_output_conditional_branch (insn, operands,
					 "vset<lsxeq_v>.v\t%Z3%w1\n\tbcnez\t%Z3%0",
					 "vset<lsxne_v>.v\t%Z3%w1\n\tbcnez\t%Z3%0");
}
 [(set_attr "type" "simd_branch")
  (set_attr "mode" "TI")])

;; vec_concate
(define_expand "vec_concatv2di"
  [(set (match_operand:V2DI 0 "register_operand")
	(vec_concat:V2DI
	  (match_operand:DI 1 "register_operand")
	  (match_operand:DI 2 "register_operand")))]
  "ISA_HAS_LSX"
{
  emit_insn (gen_lsx_vinsgr2vr_d (operands[0], operands[1],
				  operands[0], GEN_INT (0)));
  emit_insn (gen_lsx_vinsgr2vr_d (operands[0], operands[2],
				  operands[0], GEN_INT (1)));
  DONE;
})

;; Implement vec_concatv2df by vilvl.d.
(define_insn_and_split "vec_concatv2df"
  [(set (match_operand:V2DF 0 "register_operand" "=f")
	(vec_concat:V2DF
	  (match_operand:DF 1 "register_operand" "f")
	  (match_operand:DF 2 "register_operand" "f")))]
  "ISA_HAS_LSX"
  ""
  "&& reload_completed"
  [(const_int 0)]
{
  emit_insn (gen_lsx_vilvl_d_f (operands[0],
				gen_rtx_REG (V2DFmode, REGNO (operands[1])),
				gen_rtx_REG (V2DFmode, REGNO (operands[2]))));
  DONE;
}
  [(set_attr "mode" "V2DF")])

;; Implement vec_concatv4sf.
;; Optimize based on hardware register allocation of operands.
(define_insn_and_split "vec_concatv4sf"
  [(set (match_operand:V4SF 0 "register_operand" "=f")
	(vec_concat:V4SF
	  (vec_concat:V2SF
	    (match_operand:SF 1 "register_operand" "f")
	    (match_operand:SF 2 "register_operand" "f"))
	  (vec_concat:V2SF
	    (match_operand:SF 3 "register_operand" "f")
	    (match_operand:SF 4 "register_operand" "f"))))]
  "ISA_HAS_LSX"
  ""
  "&& reload_completed"
  [(const_int 0)]
{
  operands[5] = GEN_INT (1);
  operands[6] = GEN_INT (2);
  operands[7] = GEN_INT (4);
  operands[8] = GEN_INT (8);

  /* If all input are same, use vreplvei.w to broadcast.  */
  if (REGNO (operands[1]) == REGNO (operands[2])
      && REGNO (operands[1]) == REGNO (operands[3])
      && REGNO (operands[1]) == REGNO (operands[4]))
    {
      emit_insn (gen_lsx_vreplvei_w_f_scalar (operands[0], operands[1]));
    }
  /* If op0 is equal to op3, use vreplvei.w to set each element of op0 as op3.
     If other input is different from op3, use vextrins.w to insert.  */
  else if (REGNO (operands[0]) == REGNO (operands[3]))
    {
      emit_insn (gen_lsx_vreplvei_w_f_scalar (operands[0], operands[3]));
      if (REGNO (operands[1]) != REGNO (operands[3]))
	emit_insn (gen_lsx_vextrins_w_f_scalar (operands[0], operands[1],
						operands[0], operands[5]));
      if (REGNO (operands[2]) != REGNO (operands[3]))
	emit_insn (gen_lsx_vextrins_w_f_scalar (operands[0], operands[2],
						operands[0], operands[6]));
      if (REGNO (operands[4]) != REGNO (operands[3]))
	emit_insn (gen_lsx_vextrins_w_f_scalar (operands[0], operands[4],
						operands[0], operands[8]));
    }
  /* If op0 is equal to op4, use vreplvei.w to set each element of op0 as op4.
     If other input is different from op4, use vextrins.w to insert.  */
  else if (REGNO (operands[0]) == REGNO (operands[4]))
    {
      emit_insn (gen_lsx_vreplvei_w_f_scalar (operands[0], operands[4]));
      if (REGNO (operands[1]) != REGNO (operands[4]))
	emit_insn (gen_lsx_vextrins_w_f_scalar (operands[0], operands[1],
						operands[0], operands[5]));
      if (REGNO (operands[2]) != REGNO (operands[4]))
	emit_insn (gen_lsx_vextrins_w_f_scalar (operands[0], operands[2],
						operands[0], operands[6]));
      if (REGNO (operands[3]) != REGNO (operands[4]))
	emit_insn (gen_lsx_vextrins_w_f_scalar (operands[0], operands[3],
						operands[0], operands[7]));
    }
  /* Otherwise, use vilvl.w to merge op1 and op2 first.
     If op3 is different from op1, use vextrins.w to insert.
     If op4 is different from op2, use vextrins.w to insert.  */
  else
    {
      emit_insn (
	gen_lsx_vilvl_w_f (operands[0],
			   gen_rtx_REG (V4SFmode, REGNO (operands[1])),
			   gen_rtx_REG (V4SFmode, REGNO (operands[2]))));
      emit_insn (gen_lsx_vextrins_w_f_scalar (operands[0], operands[3],
					      operands[0], operands[7]));
      emit_insn (gen_lsx_vextrins_w_f_scalar (operands[0], operands[4],
					      operands[0], operands[8]));
    }
  DONE;
}
  [(set_attr "mode" "V4SF")])

(define_insn "andn<mode>3"
  [(set (match_operand:LSX 0 "register_operand" "=f")
	(and:LSX (not:LSX (match_operand:LSX 2 "register_operand" "f"))
		 (match_operand:LSX 1 "register_operand" "f")))]
  "ISA_HAS_LSX"
  "vandn.v\t%w0,%w2,%w1"
  [(set_attr "type" "simd_logic")
   (set_attr "mode" "<MODE>")])

(define_insn "abs<mode>2"
  [(set (match_operand:ILSX 0 "register_operand" "=f")
	(abs:ILSX (match_operand:ILSX 1 "register_operand" "f")))]
  "ISA_HAS_LSX"
  "vsigncov.<lsxfmt>\t%w0,%w1,%w1"
  [(set_attr "type" "simd_logic")
   (set_attr "mode" "<MODE>")])

(define_insn "vneg<mode>2"
  [(set (match_operand:ILSX 0 "register_operand" "=f")
	(neg:ILSX (match_operand:ILSX 1 "register_operand" "f")))]
  "ISA_HAS_LSX"
  "vneg.<lsxfmt>\t%w0,%w1"
  [(set_attr "type" "simd_logic")
   (set_attr "mode" "<MODE>")])

(define_insn "lsx_vextw_s_d"
  [(set (match_operand:V2DI 0 "register_operand" "=f")
	(unspec:V2DI [(match_operand:V4SI 1 "register_operand" "f")]
		     UNSPEC_LSX_VEXTW_S))]
  "ISA_HAS_LSX"
  "vextw_s.d\t%w0,%w1"
  [(set_attr "type" "simd_fcvt")
   (set_attr "mode" "V4SI")])

(define_insn "lsx_vextw_u_d"
  [(set (match_operand:V2DI 0 "register_operand" "=f")
	(unspec:V2DI [(match_operand:V4SI 1 "register_operand" "f")]
		     UNSPEC_LSX_VEXTW_U))]
  "ISA_HAS_LSX"
  "vextw_u.d\t%w0,%w1"
  [(set_attr "type" "simd_fcvt")
   (set_attr "mode" "V4SI")])

(define_insn "lsx_vsllwil_s_<dlsxfmt>_<lsxfmt>"
  [(set (match_operand:<VDMODE> 0 "register_operand" "=f")
	(unspec:<VDMODE> [(match_operand:ILSX_WHB 1 "register_operand" "f")
			  (match_operand 2 "const_<bitimm>_operand" "")]
			 UNSPEC_LSX_VSLLWIL_S))]
  "ISA_HAS_LSX"
  "vsllwil.<dlsxfmt>.<lsxfmt>\t%w0,%w1,%2"
  [(set_attr "type" "simd_shift")
   (set_attr "mode" "<MODE>")])

(define_insn "lsx_vsllwil_u_<dlsxfmt_u>_<lsxfmt_u>"
  [(set (match_operand:<VDMODE> 0 "register_operand" "=f")
	(unspec:<VDMODE> [(match_operand:ILSX_WHB 1 "register_operand" "f")
			  (match_operand 2 "const_<bitimm>_operand" "")]
			 UNSPEC_LSX_VSLLWIL_U))]
  "ISA_HAS_LSX"
  "vsllwil.<dlsxfmt_u>.<lsxfmt_u>\t%w0,%w1,%2"
  [(set_attr "type" "simd_shift")
   (set_attr "mode" "<MODE>")])

(define_insn "lsx_vsran_<hlsxfmt>_<lsxfmt>"
  [(set (match_operand:<VHMODE> 0 "register_operand" "=f")
	(unspec:<VHMODE> [(match_operand:ILSX_DWH 1 "register_operand" "f")
			  (match_operand:ILSX_DWH 2 "register_operand" "f")]
			 UNSPEC_LSX_VSRAN))]
  "ISA_HAS_LSX"
  "vsran.<hlsxfmt>.<lsxfmt>\t%w0,%w1,%w2"
  [(set_attr "type" "simd_int_arith")
   (set_attr "mode" "<MODE>")])

(define_insn "lsx_vssran_s_<hlsxfmt>_<lsxfmt>"
  [(set (match_operand:<VHMODE> 0 "register_operand" "=f")
	(unspec:<VHMODE> [(match_operand:ILSX_DWH 1 "register_operand" "f")
			  (match_operand:ILSX_DWH 2 "register_operand" "f")]
			 UNSPEC_LSX_VSSRAN_S))]
  "ISA_HAS_LSX"
  "vssran.<hlsxfmt>.<lsxfmt>\t%w0,%w1,%w2"
  [(set_attr "type" "simd_int_arith")
   (set_attr "mode" "<MODE>")])

(define_insn "lsx_vssran_u_<hlsxfmt_u>_<lsxfmt>"
  [(set (match_operand:<VHMODE> 0 "register_operand" "=f")
	(unspec:<VHMODE> [(match_operand:ILSX_DWH 1 "register_operand" "f")
			  (match_operand:ILSX_DWH 2 "register_operand" "f")]
			 UNSPEC_LSX_VSSRAN_U))]
  "ISA_HAS_LSX"
  "vssran.<hlsxfmt_u>.<lsxfmt>\t%w0,%w1,%w2"
  [(set_attr "type" "simd_int_arith")
   (set_attr "mode" "<MODE>")])

(define_insn "lsx_vsrain_<hlsxfmt>"
  [(set (match_operand:<VHMODE> 0 "register_operand" "=f")
	(unspec:<VHMODE> [(match_operand:ILSX_DWH 1 "register_operand" "f")
			  (match_operand 2 "const_<bitimm>_operand" "")]
			 UNSPEC_LSX_VSRAIN))]
  "ISA_HAS_LSX"
  "vsrain.<hlsxfmt>\t%w0,%w1,%2"
  [(set_attr "type" "simd_shift")
   (set_attr "mode" "<MODE>")])

;; FIXME: bitimm
(define_insn "lsx_vsrains_s_<hlsxfmt>"
  [(set (match_operand:<VHMODE> 0 "register_operand" "=f")
	(unspec:<VHMODE> [(match_operand:ILSX_DWH 1 "register_operand" "f")
			  (match_operand 2 "const_<bitimm>_operand" "")]
			 UNSPEC_LSX_VSRAINS_S))]
  "ISA_HAS_LSX"
  "vsrains_s.<hlsxfmt>\t%w0,%w1,%2"
  [(set_attr "type" "simd_shift")
   (set_attr "mode" "<MODE>")])

;; FIXME: bitimm
(define_insn "lsx_vsrains_u_<hlsxfmt>"
  [(set (match_operand:<VHMODE> 0 "register_operand" "=f")
	(unspec:<VHMODE> [(match_operand:ILSX_DWH 1 "register_operand" "f")
			  (match_operand 2 "const_<bitimm>_operand" "")]
			 UNSPEC_LSX_VSRAINS_U))]
  "ISA_HAS_LSX"
  "vsrains_u.<hlsxfmt>\t%w0,%w1,%2"
  [(set_attr "type" "simd_shift")
   (set_attr "mode" "<MODE>")])

(define_insn "lsx_vsrarn_<hlsxfmt>_<lsxfmt>"
  [(set (match_operand:<VHMODE> 0 "register_operand" "=f")
	(unspec:<VHMODE> [(match_operand:ILSX_DWH 1 "register_operand" "f")
			  (match_operand:ILSX_DWH 2 "register_operand" "f")]
			 UNSPEC_LSX_VSRARN))]
  "ISA_HAS_LSX"
  "vsrarn.<hlsxfmt>.<lsxfmt>\t%w0,%w1,%w2"
  [(set_attr "type" "simd_int_arith")
   (set_attr "mode" "<MODE>")])

(define_insn "lsx_vssrarn_s_<hlsxfmt>_<lsxfmt>"
  [(set (match_operand:<VHMODE> 0 "register_operand" "=f")
	(unspec:<VHMODE> [(match_operand:ILSX_DWH 1 "register_operand" "f")
			  (match_operand:ILSX_DWH 2 "register_operand" "f")]
			 UNSPEC_LSX_VSSRARN_S))]
  "ISA_HAS_LSX"
  "vssrarn.<hlsxfmt>.<lsxfmt>\t%w0,%w1,%w2"
  [(set_attr "type" "simd_int_arith")
   (set_attr "mode" "<MODE>")])

(define_insn "lsx_vssrarn_u_<hlsxfmt_u>_<lsxfmt>"
  [(set (match_operand:<VHMODE> 0 "register_operand" "=f")
	(unspec:<VHMODE> [(match_operand:ILSX_DWH 1 "register_operand" "f")
			  (match_operand:ILSX_DWH 2 "register_operand" "f")]
			 UNSPEC_LSX_VSSRARN_U))]
  "ISA_HAS_LSX"
  "vssrarn.<hlsxfmt_u>.<lsxfmt>\t%w0,%w1,%w2"
  [(set_attr "type" "simd_int_arith")
   (set_attr "mode" "<MODE>")])

(define_insn "lsx_vsrln_<hlsxfmt>_<lsxfmt>"
  [(set (match_operand:<VHMODE> 0 "register_operand" "=f")
	(unspec:<VHMODE> [(match_operand:ILSX_DWH 1 "register_operand" "f")
			  (match_operand:ILSX_DWH 2 "register_operand" "f")]
			 UNSPEC_LSX_VSRLN))]
  "ISA_HAS_LSX"
  "vsrln.<hlsxfmt>.<lsxfmt>\t%w0,%w1,%w2"
  [(set_attr "type" "simd_int_arith")
   (set_attr "mode" "<MODE>")])

(define_insn "lsx_vssrln_u_<hlsxfmt_u>_<lsxfmt>"
  [(set (match_operand:<VHMODE> 0 "register_operand" "=f")
	(unspec:<VHMODE> [(match_operand:ILSX_DWH 1 "register_operand" "f")
			  (match_operand:ILSX_DWH 2 "register_operand" "f")]
			 UNSPEC_LSX_VSSRLN_U))]
  "ISA_HAS_LSX"
  "vssrln.<hlsxfmt_u>.<lsxfmt>\t%w0,%w1,%w2"
  [(set_attr "type" "simd_int_arith")
   (set_attr "mode" "<MODE>")])

(define_insn "lsx_vsrlrn_<hlsxfmt>_<lsxfmt>"
  [(set (match_operand:<VHMODE> 0 "register_operand" "=f")
	(unspec:<VHMODE> [(match_operand:ILSX_DWH 1 "register_operand" "f")
			  (match_operand:ILSX_DWH 2 "register_operand" "f")]
			 UNSPEC_LSX_VSRLRN))]
  "ISA_HAS_LSX"
  "vsrlrn.<hlsxfmt>.<lsxfmt>\t%w0,%w1,%w2"
  [(set_attr "type" "simd_int_arith")
   (set_attr "mode" "<MODE>")])

(define_insn "lsx_vssrlrn_u_<hlsxfmt_u>_<lsxfmt>"
  [(set (match_operand:<VHMODE> 0 "register_operand" "=f")
	(unspec:<VHMODE> [(match_operand:ILSX_DWH 1 "register_operand" "f")
			  (match_operand:ILSX_DWH 2 "register_operand" "f")]
			 UNSPEC_LSX_VSSRLRN_U))]
  "ISA_HAS_LSX"
  "vssrlrn.<hlsxfmt_u>.<lsxfmt>\t%w0,%w1,%w2"
  [(set_attr "type" "simd_int_arith")
   (set_attr "mode" "<MODE>")])

(define_insn "lsx_vfrstpi_<lsxfmt>"
  [(set (match_operand:ILSX_HB 0 "register_operand" "=f")
	(unspec:ILSX_HB [(match_operand:ILSX_HB 1 "register_operand" "0")
			 (match_operand:ILSX_HB 2 "register_operand" "f")
			 (match_operand 3 "const_uimm5_operand" "")]
			UNSPEC_LSX_VFRSTPI))]
  "ISA_HAS_LSX"
  "vfrstpi.<lsxfmt>\t%w0,%w2,%3"
  [(set_attr "type" "simd_shift")
   (set_attr "mode" "<MODE>")])

(define_insn "lsx_vfrstp_<lsxfmt>"
  [(set (match_operand:ILSX_HB 0 "register_operand" "=f")
	(unspec:ILSX_HB [(match_operand:ILSX_HB 1 "register_operand" "0")
			 (match_operand:ILSX_HB 2 "register_operand" "f")
			 (match_operand:ILSX_HB 3 "register_operand" "f")]
			UNSPEC_LSX_VFRSTP))]
  "ISA_HAS_LSX"
  "vfrstp.<lsxfmt>\t%w0,%w2,%w3"
  [(set_attr "type" "simd_int_arith")
   (set_attr "mode" "<MODE>")])

(define_insn "lsx_vshuf4i_d"
  [(set (match_operand:V2DI 0 "register_operand" "=f")
	(unspec:V2DI [(match_operand:V2DI 1 "register_operand" "0")
		      (match_operand:V2DI 2 "register_operand" "f")
		      (match_operand 3 "const_uimm8_operand")]
		     UNSPEC_LSX_VSHUF4I))]
  "ISA_HAS_LSX"
  "vshuf4i.d\t%w0,%w2,%3"
  [(set_attr "type" "simd_sld")
   (set_attr "mode" "V2DI")])

(define_insn "lsx_vbsrl_<lsxfmt_f>"
  [(set (match_operand:LSX 0 "register_operand" "=f")
	(unspec:LSX [(match_operand:LSX 1 "register_operand" "f")
		     (match_operand 2 "const_uimm5_operand" "")]
		    UNSPEC_LSX_VBSRL_V))]
  "ISA_HAS_LSX"
  "vbsrl.v\t%w0,%w1,%2"
  [(set_attr "type" "simd_shift")
   (set_attr "mode" "<MODE>")])

(define_insn "lsx_vbsll_<lsxfmt>"
  [(set (match_operand:ILSX 0 "register_operand" "=f")
	(unspec:ILSX [(match_operand:ILSX 1 "register_operand" "f")
		      (match_operand 2 "const_uimm5_operand" "")]
		     UNSPEC_LSX_VBSLL_V))]
  "ISA_HAS_LSX"
  "vbsll.v\t%w0,%w1,%2"
  [(set_attr "type" "simd_shift")
   (set_attr "mode" "<MODE>")])

(define_insn "lsx_vextrins_<lsxfmt>"
  [(set (match_operand:ILSX 0 "register_operand" "=f")
	(unspec:ILSX [(match_operand:ILSX 1 "register_operand" "0")
		      (match_operand:ILSX 2 "register_operand" "f")
		      (match_operand 3 "const_uimm8_operand" "")]
		     UNSPEC_LSX_VEXTRINS))]
  "ISA_HAS_LSX"
  "vextrins.<lsxfmt>\t%w0,%w2,%3"
  [(set_attr "type" "simd_shift")
   (set_attr "mode" "<MODE>")])

(define_insn "lsx_vmskltz_<lsxfmt>"
  [(set (match_operand:ILSX 0 "register_operand" "=f")
	(unspec:ILSX [(match_operand:ILSX 1 "register_operand" "f")]
		     UNSPEC_LSX_VMSKLTZ))]
  "ISA_HAS_LSX"
  "vmskltz.<lsxfmt>\t%w0,%w1"
  [(set_attr "type" "simd_shift")
   (set_attr "mode" "<MODE>")])

(define_insn "lsx_vsigncov_<lsxfmt>"
  [(set (match_operand:ILSX 0 "register_operand" "=f")
	(unspec:ILSX [(match_operand:ILSX 1 "register_operand" "f")
		      (match_operand:ILSX 2 "register_operand" "f")]
		     UNSPEC_LSX_VSIGNCOV))]
  "ISA_HAS_LSX"
  "vsigncov.<lsxfmt>\t%w0,%w1,%w2"
  [(set_attr "type" "simd_int_arith")
   (set_attr "mode" "<MODE>")])

(define_expand "copysign<mode>3"
  [(set (match_dup 4)
	(and:FLSX
	  (not:FLSX (match_dup 3))
	  (match_operand:FLSX 1 "register_operand")))
   (set (match_dup 5)
	(and:FLSX (match_dup 3)
		  (match_operand:FLSX 2 "reg_or_vector_same_val_operand")))
   (set (match_operand:FLSX 0 "register_operand")
	(ior:FLSX (match_dup 4) (match_dup 5)))]
  "ISA_HAS_LSX"
{
  /* copysign (x, -1) should instead be expanded as setting the sign
     bit.  */
  if (!REG_P (operands[2]))
    {
      rtx op2_elt = unwrap_const_vec_duplicate (operands[2]);
      if (GET_CODE (op2_elt) == CONST_DOUBLE
	  && real_isneg (CONST_DOUBLE_REAL_VALUE (op2_elt)))
	{
	  rtx n = GEN_INT (8 * GET_MODE_SIZE (<UNITMODE>mode) - 1);
	  operands[0] = lowpart_subreg (<VIMODE>mode, operands[0],
					<MODE>mode);
	  operands[1] = lowpart_subreg (<VIMODE>mode, operands[1],
					<MODE>mode);
	  emit_insn (gen_lsx_vbitseti_<lsxfmt> (operands[0], operands[1],
						n));
	  DONE;
	}
    }

  operands[2] = force_reg (<MODE>mode, operands[2]);
  operands[3] = loongarch_build_signbit_mask (<MODE>mode, 1, 0);

  operands[4] = gen_reg_rtx (<MODE>mode);
  operands[5] = gen_reg_rtx (<MODE>mode);
})

(define_expand "@xorsign<mode>3"
  [(set (match_dup 4)
    (and:FLSX (match_dup 3)
        (match_operand:FLSX 2 "register_operand")))
   (set (match_operand:FLSX 0 "register_operand")
    (xor:FLSX (match_dup 4)
         (match_operand:FLSX 1 "register_operand")))]
  "ISA_HAS_LSX"
{
  operands[3] = loongarch_build_signbit_mask (<MODE>mode, 1, 0);

  operands[4] = gen_reg_rtx (<MODE>mode);
})


(define_insn "absv2df2"
  [(set (match_operand:V2DF 0 "register_operand" "=f")
	(abs:V2DF (match_operand:V2DF 1 "register_operand" "f")))]
  "ISA_HAS_LSX"
  "vbitclri.d\t%w0,%w1,63"
  [(set_attr "type" "simd_logic")
   (set_attr "mode" "V2DF")])

(define_insn "absv4sf2"
  [(set (match_operand:V4SF 0 "register_operand" "=f")
	(abs:V4SF (match_operand:V4SF 1 "register_operand" "f")))]
  "ISA_HAS_LSX"
  "vbitclri.w\t%w0,%w1,31"
  [(set_attr "type" "simd_logic")
   (set_attr "mode" "V4SF")])

(define_insn "vfmadd<mode>4"
  [(set (match_operand:FLSX 0 "register_operand" "=f")
	(fma:FLSX (match_operand:FLSX 1 "register_operand" "f")
		  (match_operand:FLSX 2 "register_operand" "f")
		  (match_operand:FLSX 3 "register_operand" "f")))]
  "ISA_HAS_LSX"
  "vfmadd.<flsxfmt>\t%w0,%w1,$w2,%w3"
  [(set_attr "type" "simd_fmadd")
   (set_attr "mode" "<MODE>")])

(define_insn "fms<mode>4"
  [(set (match_operand:FLSX 0 "register_operand" "=f")
	(fma:FLSX (match_operand:FLSX 1 "register_operand" "f")
		  (match_operand:FLSX 2 "register_operand" "f")
		  (neg:FLSX (match_operand:FLSX 3 "register_operand" "f"))))]
  "ISA_HAS_LSX"
  "vfmsub.<flsxfmt>\t%w0,%w1,%w2,%w3"
  [(set_attr "type" "simd_fmadd")
   (set_attr "mode" "<MODE>")])

(define_insn "vfnmsub<mode>4_nmsub4"
  [(set (match_operand:FLSX 0 "register_operand" "=f")
	(neg:FLSX
	  (fma:FLSX
	    (match_operand:FLSX 1 "register_operand" "f")
	    (match_operand:FLSX 2 "register_operand" "f")
	    (neg:FLSX (match_operand:FLSX 3 "register_operand" "f")))))]
  "ISA_HAS_LSX"
  "vfnmsub.<flsxfmt>\t%w0,%w1,%w2,%w3"
  [(set_attr "type" "simd_fmadd")
   (set_attr "mode" "<MODE>")])


(define_insn "vfnmadd<mode>4_nmadd4"
  [(set (match_operand:FLSX 0 "register_operand" "=f")
	(neg:FLSX
	  (fma:FLSX
	    (match_operand:FLSX 1 "register_operand" "f")
	    (match_operand:FLSX 2 "register_operand" "f")
	    (match_operand:FLSX 3 "register_operand" "f"))))]
  "ISA_HAS_LSX"
  "vfnmadd.<flsxfmt>\t%w0,%w1,%w2,%w3"
  [(set_attr "type" "simd_fmadd")
   (set_attr "mode" "<MODE>")])

(define_insn "lsx_vftint_w_d"
  [(set (match_operand:V4SI 0 "register_operand" "=f")
	(unspec:V4SI [(match_operand:V2DF 1 "register_operand" "f")
		      (match_operand:V2DF 2 "register_operand" "f")]
		     UNSPEC_LSX_VFTINT_W_D))]
  "ISA_HAS_LSX"
  "vftint.w.d\t%w0,%w1,%w2"
  [(set_attr "type" "simd_int_arith")
   (set_attr "mode" "V2DF")])

(define_insn "lsx_vffint_s_l"
  [(set (match_operand:V4SF 0 "register_operand" "=f")
	(unspec:V4SF [(match_operand:V2DI 1 "register_operand" "f")
		      (match_operand:V2DI 2 "register_operand" "f")]
		     UNSPEC_LSX_VFFINT_S_L))]
  "ISA_HAS_LSX"
  "vffint.s.l\t%w0,%w1,%w2"
  [(set_attr "type" "simd_int_arith")
   (set_attr "mode" "V2DI")])

(define_insn "lsx_vftintrz_w_d"
  [(set (match_operand:V4SI 0 "register_operand" "=f")
	(unspec:V4SI [(match_operand:V2DF 1 "register_operand" "f")
		      (match_operand:V2DF 2 "register_operand" "f")]
		     UNSPEC_LSX_VFTINTRZ_W_D))]
  "ISA_HAS_LSX"
  "vftintrz.w.d\t%w0,%w1,%w2"
  [(set_attr "type" "simd_int_arith")
   (set_attr "mode" "V2DF")])

(define_insn "lsx_vftintrp_w_d"
  [(set (match_operand:V4SI 0 "register_operand" "=f")
	(unspec:V4SI [(match_operand:V2DF 1 "register_operand" "f")
		      (match_operand:V2DF 2 "register_operand" "f")]
		     UNSPEC_LSX_VFTINTRP_W_D))]
  "ISA_HAS_LSX"
  "vftintrp.w.d\t%w0,%w1,%w2"
  [(set_attr "type" "simd_int_arith")
   (set_attr "mode" "V2DF")])

(define_insn "lsx_vftintrm_w_d"
  [(set (match_operand:V4SI 0 "register_operand" "=f")
	(unspec:V4SI [(match_operand:V2DF 1 "register_operand" "f")
		      (match_operand:V2DF 2 "register_operand" "f")]
		     UNSPEC_LSX_VFTINTRM_W_D))]
  "ISA_HAS_LSX"
  "vftintrm.w.d\t%w0,%w1,%w2"
  [(set_attr "type" "simd_int_arith")
   (set_attr "mode" "V2DF")])

(define_insn "lsx_vftintrne_w_d"
  [(set (match_operand:V4SI 0 "register_operand" "=f")
	(unspec:V4SI [(match_operand:V2DF 1 "register_operand" "f")
		      (match_operand:V2DF 2 "register_operand" "f")]
		     UNSPEC_LSX_VFTINTRNE_W_D))]
  "ISA_HAS_LSX"
  "vftintrne.w.d\t%w0,%w1,%w2"
  [(set_attr "type" "simd_int_arith")
   (set_attr "mode" "V2DF")])

(define_insn "lsx_vftinth_l_s"
  [(set (match_operand:V2DI 0 "register_operand" "=f")
	(unspec:V2DI [(match_operand:V4SF 1 "register_operand" "f")]
		     UNSPEC_LSX_VFTINTH_L_H))]
  "ISA_HAS_LSX"
  "vftinth.l.s\t%w0,%w1"
  [(set_attr "type" "simd_shift")
   (set_attr "mode" "V4SF")])

(define_insn "lsx_vftintl_l_s"
  [(set (match_operand:V2DI 0 "register_operand" "=f")
	(unspec:V2DI [(match_operand:V4SF 1 "register_operand" "f")]
		     UNSPEC_LSX_VFTINTL_L_S))]
  "ISA_HAS_LSX"
  "vftintl.l.s\t%w0,%w1"
  [(set_attr "type" "simd_shift")
   (set_attr "mode" "V4SF")])

(define_insn "lsx_vffinth_d_w"
  [(set (match_operand:V2DF 0 "register_operand" "=f")
	(unspec:V2DF [(match_operand:V4SI 1 "register_operand" "f")]
		     UNSPEC_LSX_VFFINTH_D_W))]
  "ISA_HAS_LSX"
  "vffinth.d.w\t%w0,%w1"
  [(set_attr "type" "simd_shift")
   (set_attr "mode" "V4SI")])

(define_insn "lsx_vffintl_d_w"
  [(set (match_operand:V2DF 0 "register_operand" "=f")
	(unspec:V2DF [(match_operand:V4SI 1 "register_operand" "f")]
		     UNSPEC_LSX_VFFINTL_D_W))]
  "ISA_HAS_LSX"
  "vffintl.d.w\t%w0,%w1"
  [(set_attr "type" "simd_shift")
   (set_attr "mode" "V4SI")])

(define_insn "lsx_vftintrzh_l_s"
  [(set (match_operand:V2DI 0 "register_operand" "=f")
	(unspec:V2DI [(match_operand:V4SF 1 "register_operand" "f")]
		     UNSPEC_LSX_VFTINTRZH_L_S))]
  "ISA_HAS_LSX"
  "vftintrzh.l.s\t%w0,%w1"
  [(set_attr "type" "simd_shift")
   (set_attr "mode" "V4SF")])

(define_insn "lsx_vftintrzl_l_s"
  [(set (match_operand:V2DI 0 "register_operand" "=f")
	(unspec:V2DI [(match_operand:V4SF 1 "register_operand" "f")]
		     UNSPEC_LSX_VFTINTRZL_L_S))]
  "ISA_HAS_LSX"
  "vftintrzl.l.s\t%w0,%w1"
  [(set_attr "type" "simd_shift")
   (set_attr "mode" "V4SF")])

(define_insn "lsx_vftintrph_l_s"
  [(set (match_operand:V2DI 0 "register_operand" "=f")
	(unspec:V2DI [(match_operand:V4SF 1 "register_operand" "f")]
		     UNSPEC_LSX_VFTINTRPH_L_S))]
  "ISA_HAS_LSX"
  "vftintrph.l.s\t%w0,%w1"
  [(set_attr "type" "simd_shift")
   (set_attr "mode" "V4SF")])

(define_insn "lsx_vftintrpl_l_s"
  [(set (match_operand:V2DI 0 "register_operand" "=f")
	(unspec:V2DI [(match_operand:V4SF 1 "register_operand" "f")]
		     UNSPEC_LSX_VFTINTRPL_L_S))]
  "ISA_HAS_LSX"
  "vftintrpl.l.s\t%w0,%w1"
  [(set_attr "type" "simd_shift")
   (set_attr "mode" "V4SF")])

(define_insn "lsx_vftintrmh_l_s"
  [(set (match_operand:V2DI 0 "register_operand" "=f")
	(unspec:V2DI [(match_operand:V4SF 1 "register_operand" "f")]
		     UNSPEC_LSX_VFTINTRMH_L_S))]
  "ISA_HAS_LSX"
  "vftintrmh.l.s\t%w0,%w1"
  [(set_attr "type" "simd_shift")
   (set_attr "mode" "V4SF")])

(define_insn "lsx_vftintrml_l_s"
  [(set (match_operand:V2DI 0 "register_operand" "=f")
	(unspec:V2DI [(match_operand:V4SF 1 "register_operand" "f")]
		     UNSPEC_LSX_VFTINTRML_L_S))]
  "ISA_HAS_LSX"
  "vftintrml.l.s\t%w0,%w1"
  [(set_attr "type" "simd_shift")
   (set_attr "mode" "V4SF")])

(define_insn "lsx_vftintrneh_l_s"
  [(set (match_operand:V2DI 0 "register_operand" "=f")
	(unspec:V2DI [(match_operand:V4SF 1 "register_operand" "f")]
		     UNSPEC_LSX_VFTINTRNEH_L_S))]
  "ISA_HAS_LSX"
  "vftintrneh.l.s\t%w0,%w1"
  [(set_attr "type" "simd_shift")
   (set_attr "mode" "V4SF")])

(define_insn "lsx_vftintrnel_l_s"
  [(set (match_operand:V2DI 0 "register_operand" "=f")
	(unspec:V2DI [(match_operand:V4SF 1 "register_operand" "f")]
		     UNSPEC_LSX_VFTINTRNEL_L_S))]
  "ISA_HAS_LSX"
  "vftintrnel.l.s\t%w0,%w1"
  [(set_attr "type" "simd_shift")
   (set_attr "mode" "V4SF")])

;; Offset load and broadcast
(define_expand "lsx_vldrepl_<lsxfmt_f>"
  [(match_operand:LSX 0 "register_operand")
   (match_operand 1 "pmode_register_operand")
   (match_operand 2 "aq12<lsxfmt>_operand")]
  "ISA_HAS_LSX"
{
  emit_insn (gen_lsx_vldrepl_<lsxfmt_f>_insn
	     (operands[0], operands[1], operands[2]));
  DONE;
})

(define_insn "lsx_vldrepl_<lsxfmt_f>_insn"
  [(set (match_operand:LSX 0 "register_operand" "=f")
	(vec_duplicate:LSX
	  (mem:<UNITMODE> (plus:DI (match_operand:DI 1 "register_operand" "r")
				   (match_operand 2 "aq12<lsxfmt>_operand")))))]
  "ISA_HAS_LSX"
{
    return "vldrepl.<lsxfmt>\t%w0,%1,%2";
}
  [(set_attr "type" "simd_load")
   (set_attr "mode" "<MODE>")
   (set_attr "length" "4")])

(define_insn "lsx_vldrepl_<lsxfmt_f>_insn_0"
  [(set (match_operand:LSX 0 "register_operand" "=f")
    (vec_duplicate:LSX
      (mem:<UNITMODE> (match_operand:DI 1 "register_operand" "r"))))]
  "ISA_HAS_LSX"
{
    return "vldrepl.<lsxfmt>\t%w0,%1,0";
}
  [(set_attr "type" "simd_load")
   (set_attr "mode" "<MODE>")
   (set_attr "length" "4")])

;; Offset store by sel
(define_expand "lsx_vstelm_<lsxfmt_f>"
  [(match_operand:LSX 0 "register_operand")
   (match_operand 3 "const_<indeximm>_operand")
   (match_operand 2 "aq8<lsxfmt>_operand")
   (match_operand 1 "pmode_register_operand")]
  "ISA_HAS_LSX"
{
  emit_insn (gen_lsx_vstelm_<lsxfmt_f>_insn
	     (operands[1], operands[2], operands[0], operands[3]));
  DONE;
})

(define_insn "lsx_vstelm_<lsxfmt_f>_insn"
  [(set (mem:<UNITMODE> (plus:DI (match_operand:DI 0 "register_operand" "r")
				 (match_operand 1 "aq8<lsxfmt>_operand")))
	(vec_select:<UNITMODE>
	  (match_operand:LSX 2 "register_operand" "f")
	  (parallel [(match_operand 3 "const_<indeximm>_operand" "")])))]

  "ISA_HAS_LSX"
{
  return "vstelm.<lsxfmt>\t%w2,%0,%1,%3";
}
  [(set_attr "type" "simd_store")
   (set_attr "mode" "<MODE>")
   (set_attr "length" "4")])

;; Offset is "0"
(define_insn "lsx_vstelm_<lsxfmt_f>_insn_0"
  [(set (mem:<UNITMODE> (match_operand:DI 0 "register_operand" "r"))
    (vec_select:<UNITMODE>
      (match_operand:LSX 1 "register_operand" "f")
      (parallel [(match_operand:SI 2 "const_<indeximm>_operand")])))]
  "ISA_HAS_LSX"
{
    return "vstelm.<lsxfmt>\t%w1,%0,0,%2";
}
  [(set_attr "type" "simd_store")
   (set_attr "mode" "<MODE>")
   (set_attr "length" "4")])

(define_expand "lsx_vld"
  [(match_operand:V16QI 0 "register_operand")
   (match_operand 1 "pmode_register_operand")
   (match_operand 2 "aq12b_operand")]
  "ISA_HAS_LSX"
{
  rtx addr = plus_constant (GET_MODE (operands[1]), operands[1],
			    INTVAL (operands[2]));
  loongarch_emit_move (operands[0], gen_rtx_MEM (V16QImode, addr));
  DONE;
})

(define_expand "lsx_vst"
  [(match_operand:V16QI 0 "register_operand")
   (match_operand 1 "pmode_register_operand")
   (match_operand 2 "aq12b_operand")]
  "ISA_HAS_LSX"
{
  rtx addr = plus_constant (GET_MODE (operands[1]), operands[1],
			    INTVAL (operands[2]));
  loongarch_emit_move (gen_rtx_MEM (V16QImode, addr), operands[0]);
  DONE;
})

(define_insn "lsx_vssrln_<hlsxfmt>_<lsxfmt>"
  [(set (match_operand:<VHMODE> 0 "register_operand" "=f")
	(unspec:<VHMODE> [(match_operand:ILSX_DWH 1 "register_operand" "f")
			  (match_operand:ILSX_DWH 2 "register_operand" "f")]
			 UNSPEC_LSX_VSSRLN))]
  "ISA_HAS_LSX"
  "vssrln.<hlsxfmt>.<lsxfmt>\t%w0,%w1,%w2"
  [(set_attr "type" "simd_int_arith")
   (set_attr "mode" "<MODE>")])


(define_insn "lsx_vssrlrn_<hlsxfmt>_<lsxfmt>"
  [(set (match_operand:<VHMODE> 0 "register_operand" "=f")
	(unspec:<VHMODE> [(match_operand:ILSX_DWH 1 "register_operand" "f")
			  (match_operand:ILSX_DWH 2 "register_operand" "f")]
			 UNSPEC_LSX_VSSRLRN))]
  "ISA_HAS_LSX"
  "vssrlrn.<hlsxfmt>.<lsxfmt>\t%w0,%w1,%w2"
  [(set_attr "type" "simd_int_arith")
   (set_attr "mode" "<MODE>")])

(define_insn "iorn<mode>3"
  [(set (match_operand:ILSX 0 "register_operand" "=f")
	(ior:ILSX (not:ILSX (match_operand:ILSX 2 "register_operand" "f"))
		  (match_operand:ILSX 1 "register_operand" "f")))]
  "ISA_HAS_LSX"
  "vorn.v\t%w0,%w1,%w2"
  [(set_attr "type" "simd_logic")
   (set_attr "mode" "<MODE>")])

(define_insn "lsx_vldi"
  [(set (match_operand:V2DI 0 "register_operand" "=f")
	(unspec:V2DI [(match_operand 1 "const_imm13_operand")]
		    UNSPEC_LSX_VLDI))]
  "ISA_HAS_LSX"
{
  HOST_WIDE_INT val = INTVAL (operands[1]);
  if (val < 0)
  {
    HOST_WIDE_INT modeVal = (val & 0xf00) >> 8;
    if (modeVal < 13)
      return  "vldi\t%w0,%1";
    else
      sorry ("imm13 only support 0000 ~ 1100 in bits 9 ~ 12 when bit '13' is 1");
    return "#";
  }
  else
    return "vldi\t%w0,%1";
}
  [(set_attr "type" "simd_load")
   (set_attr "mode" "V2DI")])

(define_insn "lsx_vshuf_b"
  [(set (match_operand:V16QI 0 "register_operand" "=f")
	(unspec:V16QI [(match_operand:V16QI 1 "register_operand" "f")
		       (match_operand:V16QI 2 "register_operand" "f")
		       (match_operand:V16QI 3 "register_operand" "f")]
		      UNSPEC_LSX_VSHUF_B))]
  "ISA_HAS_LSX"
  "vshuf.b\t%w0,%w1,%w2,%w3"
  [(set_attr "type" "simd_shf")
   (set_attr "mode" "V16QI")])

(define_insn "lsx_vldx"
  [(set (match_operand:V16QI 0 "register_operand" "=f")
	(unspec:V16QI [(match_operand:DI 1 "register_operand" "r")
		       (match_operand:DI 2 "reg_or_0_operand" "rJ")]
		      UNSPEC_LSX_VLDX))]
  "ISA_HAS_LSX"
{
  return "vldx\t%w0,%1,%z2";
}
  [(set_attr "type" "simd_load")
   (set_attr "mode" "V16QI")])

(define_insn "lsx_vstx"
  [(set (mem:V16QI (plus:DI (match_operand:DI 1 "register_operand" "r")
			    (match_operand:DI 2 "reg_or_0_operand" "rJ")))
	(unspec: V16QI [(match_operand:V16QI 0 "register_operand" "f")]
		      UNSPEC_LSX_VSTX))]

  "ISA_HAS_LSX"
{
  return "vstx\t%w0,%1,%z2";
}
  [(set_attr "type" "simd_store")
   (set_attr "mode" "DI")])

(define_insn "lsx_vextl_qu_du"
  [(set (match_operand:V2DI 0 "register_operand" "=f")
	(unspec:V2DI [(match_operand:V2DI 1 "register_operand" "f")]
		     UNSPEC_LSX_VEXTL_QU_DU))]
  "ISA_HAS_LSX"
  "vextl.qu.du\t%w0,%w1"
  [(set_attr "type" "simd_bit")
   (set_attr "mode" "V2DI")])

(define_insn "lsx_vseteqz_v"
  [(set (match_operand:FCC 0 "register_operand" "=z")
	(eq:FCC
	  (unspec:SI [(match_operand:V16QI 1 "register_operand" "f")]
		     UNSPEC_LSX_VSETEQZ_V)
	  (match_operand:SI 2 "const_0_operand")))]
  "ISA_HAS_LSX"
{
  return "vseteqz.v\t%0,%1";
}
  [(set_attr "type" "simd_fcmp")
   (set_attr "mode" "FCC")])

;; Vector reduction operation
(define_expand "reduc_plus_scal_v2di"
  [(match_operand:DI 0 "register_operand")
   (match_operand:V2DI 1 "register_operand")]
  "ISA_HAS_LSX"
{
  rtx tmp = gen_reg_rtx (V2DImode);
  emit_insn (gen_lsx_vhaddw_q_d (tmp, operands[1], operands[1]));
  emit_insn (gen_vec_extractv2didi (operands[0], tmp, const0_rtx));
  DONE;
})

(define_expand "reduc_plus_scal_v4si"
  [(match_operand:SI 0 "register_operand")
   (match_operand:V4SI 1 "register_operand")]
  "ISA_HAS_LSX"
{
  rtx tmp = gen_reg_rtx (V2DImode);
  rtx tmp1 = gen_reg_rtx (V2DImode);
  emit_insn (gen_lsx_vhaddw_d_w (tmp, operands[1], operands[1]));
  emit_insn (gen_lsx_vhaddw_q_d (tmp1, tmp, tmp));
  emit_insn (gen_vec_extractv4sisi (operands[0], gen_lowpart (V4SImode,tmp1),
				    const0_rtx));
  DONE;
})

(define_expand "reduc_plus_scal_<mode>"
  [(match_operand:<UNITMODE> 0 "register_operand")
   (match_operand:FLSX 1 "register_operand")]
  "ISA_HAS_LSX"
{
  rtx tmp = gen_reg_rtx (<MODE>mode);
  loongarch_expand_vector_reduc (gen_add<mode>3, tmp, operands[1]);
  emit_insn (gen_vec_extract<mode><unitmode> (operands[0], tmp,
					      const0_rtx));
  DONE;
})

(define_expand "reduc_<optab>_scal_<mode>"
  [(any_bitwise:<UNITMODE>
      (match_operand:<UNITMODE> 0 "register_operand")
      (match_operand:ILSX 1 "register_operand"))]
  "ISA_HAS_LSX"
{
  rtx tmp = gen_reg_rtx (<MODE>mode);
  loongarch_expand_vector_reduc (gen_<optab><mode>3, tmp, operands[1]);
  emit_insn (gen_vec_extract<mode><unitmode> (operands[0], tmp,
					      const0_rtx));
  DONE;
})

(define_expand "reduc_smax_scal_<mode>"
  [(match_operand:<UNITMODE> 0 "register_operand")
   (match_operand:LSX 1 "register_operand")]
  "ISA_HAS_LSX"
{
  rtx tmp = gen_reg_rtx (<MODE>mode);
  loongarch_expand_vector_reduc (gen_smax<mode>3, tmp, operands[1]);
  emit_insn (gen_vec_extract<mode><unitmode> (operands[0], tmp,
					      const0_rtx));
  DONE;
})

(define_expand "reduc_smin_scal_<mode>"
  [(match_operand:<UNITMODE> 0 "register_operand")
   (match_operand:LSX 1 "register_operand")]
  "ISA_HAS_LSX"
{
  rtx tmp = gen_reg_rtx (<MODE>mode);
  loongarch_expand_vector_reduc (gen_smin<mode>3, tmp, operands[1]);
  emit_insn (gen_vec_extract<mode><unitmode> (operands[0], tmp,
					      const0_rtx));
  DONE;
})

(define_expand "reduc_umax_scal_<mode>"
  [(match_operand:<UNITMODE> 0 "register_operand")
   (match_operand:ILSX 1 "register_operand")]
  "ISA_HAS_LSX"
{
  rtx tmp = gen_reg_rtx (<MODE>mode);
  loongarch_expand_vector_reduc (gen_umax<mode>3, tmp, operands[1]);
  emit_insn (gen_vec_extract<mode><unitmode> (operands[0], tmp,
					      const0_rtx));
  DONE;
})

(define_expand "reduc_umin_scal_<mode>"
  [(match_operand:<UNITMODE> 0 "register_operand")
   (match_operand:ILSX 1 "register_operand")]
  "ISA_HAS_LSX"
{
  rtx tmp = gen_reg_rtx (<MODE>mode);
  loongarch_expand_vector_reduc (gen_umin<mode>3, tmp, operands[1]);
  emit_insn (gen_vec_extract<mode><unitmode> (operands[0], tmp,
					      const0_rtx));
  DONE;
})

(define_expand "avg<mode>3_ceil"
  [(match_operand:ILSX_WHB 0 "register_operand")
   (match_operand:ILSX_WHB 1 "register_operand")
   (match_operand:ILSX_WHB 2 "register_operand")]
  "ISA_HAS_LSX"
{
  emit_insn (gen_lsx_vavgr_s_<lsxfmt> (operands[0],
	operands[1], operands[2]));
  DONE;
})

(define_expand "uavg<mode>3_ceil"
  [(match_operand:ILSX_WHB 0 "register_operand")
   (match_operand:ILSX_WHB 1 "register_operand")
   (match_operand:ILSX_WHB 2 "register_operand")]
  "ISA_HAS_LSX"
{
  emit_insn (gen_lsx_vavgr_u_<lsxfmt_u> (operands[0],
	operands[1], operands[2]));
  DONE;
})

(define_expand "avg<mode>3_floor"
  [(match_operand:ILSX_WHB 0 "register_operand")
   (match_operand:ILSX_WHB 1 "register_operand")
   (match_operand:ILSX_WHB 2 "register_operand")]
  "ISA_HAS_LSX"
{
  emit_insn (gen_lsx_vavg_s_<lsxfmt> (operands[0],
	operands[1], operands[2]));
  DONE;
})

(define_expand "uavg<mode>3_floor"
  [(match_operand:ILSX_WHB 0 "register_operand")
   (match_operand:ILSX_WHB 1 "register_operand")
   (match_operand:ILSX_WHB 2 "register_operand")]
  "ISA_HAS_LSX"
{
  emit_insn (gen_lsx_vavg_u_<lsxfmt_u> (operands[0],
	operands[1], operands[2]));
  DONE;
})

(define_expand "usadv16qi"
  [(match_operand:V4SI 0 "register_operand")
   (match_operand:V16QI 1 "register_operand")
   (match_operand:V16QI 2 "register_operand")
   (match_operand:V4SI 3 "register_operand")]
  "ISA_HAS_LSX"
{
  rtx t1 = gen_reg_rtx (V16QImode);
  rtx t2 = gen_reg_rtx (V8HImode);
  rtx t3 = gen_reg_rtx (V4SImode);
  emit_insn (gen_lsx_vabsd_u_bu (t1, operands[1], operands[2]));
  emit_insn (gen_lsx_vhaddw_hu_bu (t2, t1, t1));
  emit_insn (gen_lsx_vhaddw_wu_hu (t3, t2, t2));
  emit_insn (gen_addv4si3 (operands[0], t3, operands[3]));
  DONE;
})

(define_expand "ssadv16qi"
  [(match_operand:V4SI 0 "register_operand")
   (match_operand:V16QI 1 "register_operand")
   (match_operand:V16QI 2 "register_operand")
   (match_operand:V4SI 3 "register_operand")]
  "ISA_HAS_LSX"
{
  rtx t1 = gen_reg_rtx (V16QImode);
  rtx t2 = gen_reg_rtx (V8HImode);
  rtx t3 = gen_reg_rtx (V4SImode);
  emit_insn (gen_lsx_vabsd_s_b (t1, operands[1], operands[2]));
  emit_insn (gen_lsx_vhaddw_hu_bu (t2, t1, t1));
  emit_insn (gen_lsx_vhaddw_wu_hu (t3, t2, t2));
  emit_insn (gen_addv4si3 (operands[0], t3, operands[3]));
  DONE;
})

(define_insn "lsx_v<optab>wev_d_w<u>"
  [(set (match_operand:V2DI 0 "register_operand" "=f")
	(addsubmul:V2DI
	  (any_extend:V2DI
	    (vec_select:V2SI
	      (match_operand:V4SI 1 "register_operand" "%f")
	      (parallel [(const_int 0) (const_int 2)])))
	  (any_extend:V2DI
	    (vec_select:V2SI
	      (match_operand:V4SI 2 "register_operand" "f")
	      (parallel [(const_int 0) (const_int 2)])))))]
  "ISA_HAS_LSX"
  "v<optab>wev.d.w<u>\t%w0,%w1,%w2"
  [(set_attr "type" "simd_int_arith")
   (set_attr "mode" "V2DI")])

(define_insn "lsx_v<optab>wev_w_h<u>"
  [(set (match_operand:V4SI 0 "register_operand" "=f")
	(addsubmul:V4SI
	  (any_extend:V4SI
	    (vec_select:V4HI
	      (match_operand:V8HI 1 "register_operand" "%f")
	      (parallel [(const_int 0) (const_int 2)
			 (const_int 4) (const_int 6)])))
	  (any_extend:V4SI
	    (vec_select:V4HI
	      (match_operand:V8HI 2 "register_operand" "f")
	      (parallel [(const_int 0) (const_int 2)
			 (const_int 4) (const_int 6)])))))]
  "ISA_HAS_LSX"
  "v<optab>wev.w.h<u>\t%w0,%w1,%w2"
  [(set_attr "type" "simd_int_arith")
   (set_attr "mode" "V4SI")])

(define_insn "lsx_v<optab>wev_h_b<u>"
  [(set (match_operand:V8HI 0 "register_operand" "=f")
	(addsubmul:V8HI
	  (any_extend:V8HI
	    (vec_select:V8QI
	      (match_operand:V16QI 1 "register_operand" "%f")
	      (parallel [(const_int 0) (const_int 2)
			 (const_int 4) (const_int 6)
			 (const_int 8) (const_int 10)
			 (const_int 12) (const_int 14)])))
	  (any_extend:V8HI
	    (vec_select:V8QI
	      (match_operand:V16QI 2 "register_operand" "f")
	      (parallel [(const_int 0) (const_int 2)
			 (const_int 4) (const_int 6)
			 (const_int 8) (const_int 10)
			 (const_int 12) (const_int 14)])))))]
  "ISA_HAS_LSX"
  "v<optab>wev.h.b<u>\t%w0,%w1,%w2"
  [(set_attr "type" "simd_int_arith")
   (set_attr "mode" "V8HI")])

(define_insn "lsx_v<optab>wod_d_w<u>"
  [(set (match_operand:V2DI 0 "register_operand" "=f")
	(addsubmul:V2DI
	  (any_extend:V2DI
	    (vec_select:V2SI
	      (match_operand:V4SI 1 "register_operand" "%f")
	      (parallel [(const_int 1) (const_int 3)])))
	  (any_extend:V2DI
	    (vec_select:V2SI
	      (match_operand:V4SI 2 "register_operand" "f")
	      (parallel [(const_int 1) (const_int 3)])))))]
  "ISA_HAS_LSX"
  "v<optab>wod.d.w<u>\t%w0,%w1,%w2"
  [(set_attr "type" "simd_int_arith")
   (set_attr "mode" "V2DI")])

(define_insn "lsx_v<optab>wod_w_h<u>"
  [(set (match_operand:V4SI 0 "register_operand" "=f")
	(addsubmul:V4SI
	  (any_extend:V4SI
	    (vec_select:V4HI
	      (match_operand:V8HI 1 "register_operand" "%f")
	      (parallel [(const_int 1) (const_int 3)
			 (const_int 5) (const_int 7)])))
	  (any_extend:V4SI
	    (vec_select:V4HI
	      (match_operand:V8HI 2 "register_operand" "f")
	      (parallel [(const_int 1) (const_int 3)
			 (const_int 5) (const_int 7)])))))]
  "ISA_HAS_LSX"
  "v<optab>wod.w.h<u>\t%w0,%w1,%w2"
  [(set_attr "type" "simd_int_arith")
   (set_attr "mode" "V4SI")])

(define_insn "lsx_v<optab>wod_h_b<u>"
  [(set (match_operand:V8HI 0 "register_operand" "=f")
	(addsubmul:V8HI
	  (any_extend:V8HI
	    (vec_select:V8QI
	      (match_operand:V16QI 1 "register_operand" "%f")
	      (parallel [(const_int 1) (const_int 3)
			 (const_int 5) (const_int 7)
			 (const_int 9) (const_int 11)
			 (const_int 13) (const_int 15)])))
	  (any_extend:V8HI
	    (vec_select:V8QI
	      (match_operand:V16QI 2 "register_operand" "f")
	      (parallel [(const_int 1) (const_int 3)
			 (const_int 5) (const_int 7)
			 (const_int 9) (const_int 11)
			 (const_int 13) (const_int 15)])))))]
  "ISA_HAS_LSX"
  "v<optab>wod.h.b<u>\t%w0,%w1,%w2"
  [(set_attr "type" "simd_int_arith")
   (set_attr "mode" "V8HI")])

(define_insn "lsx_v<optab>wev_d_wu_w"
  [(set (match_operand:V2DI 0 "register_operand" "=f")
	(addmul:V2DI
	  (zero_extend:V2DI
	    (vec_select:V2SI
	      (match_operand:V4SI 1 "register_operand" "%f")
	      (parallel [(const_int 0) (const_int 2)])))
	  (sign_extend:V2DI
	    (vec_select:V2SI
	      (match_operand:V4SI 2 "register_operand" "f")
	      (parallel [(const_int 0) (const_int 2)])))))]
  "ISA_HAS_LSX"
  "v<optab>wev.d.wu.w\t%w0,%w1,%w2"
  [(set_attr "type" "simd_int_arith")
   (set_attr "mode" "V2DI")])

(define_insn "lsx_v<optab>wev_w_hu_h"
  [(set (match_operand:V4SI 0 "register_operand" "=f")
	(addmul:V4SI
	  (zero_extend:V4SI
	    (vec_select:V4HI
	      (match_operand:V8HI 1 "register_operand" "%f")
	      (parallel [(const_int 0) (const_int 2)
			 (const_int 4) (const_int 6)])))
	  (sign_extend:V4SI
	    (vec_select:V4HI
	      (match_operand:V8HI 2 "register_operand" "f")
	      (parallel [(const_int 0) (const_int 2)
			 (const_int 4) (const_int 6)])))))]
  "ISA_HAS_LSX"
  "v<optab>wev.w.hu.h\t%w0,%w1,%w2"
  [(set_attr "type" "simd_int_arith")
   (set_attr "mode" "V4SI")])

(define_insn "lsx_v<optab>wev_h_bu_b"
  [(set (match_operand:V8HI 0 "register_operand" "=f")
	(addmul:V8HI
	  (zero_extend:V8HI
	    (vec_select:V8QI
	      (match_operand:V16QI 1 "register_operand" "%f")
	      (parallel [(const_int 0) (const_int 2)
			 (const_int 4) (const_int 6)
			 (const_int 8) (const_int 10)
			 (const_int 12) (const_int 14)])))
	  (sign_extend:V8HI
	    (vec_select:V8QI
	      (match_operand:V16QI 2 "register_operand" "f")
	      (parallel [(const_int 0) (const_int 2)
			 (const_int 4) (const_int 6)
			 (const_int 8) (const_int 10)
			 (const_int 12) (const_int 14)])))))]
  "ISA_HAS_LSX"
  "v<optab>wev.h.bu.b\t%w0,%w1,%w2"
  [(set_attr "type" "simd_int_arith")
   (set_attr "mode" "V8HI")])

(define_insn "lsx_v<optab>wod_d_wu_w"
  [(set (match_operand:V2DI 0 "register_operand" "=f")
	(addmul:V2DI
	  (zero_extend:V2DI
	    (vec_select:V2SI
	      (match_operand:V4SI 1 "register_operand" "%f")
	      (parallel [(const_int 1) (const_int 3)])))
	  (sign_extend:V2DI
	    (vec_select:V2SI
	      (match_operand:V4SI 2 "register_operand" "f")
	      (parallel [(const_int 1) (const_int 3)])))))]
  "ISA_HAS_LSX"
  "v<optab>wod.d.wu.w\t%w0,%w1,%w2"
  [(set_attr "type" "simd_int_arith")
   (set_attr "mode" "V2DI")])

(define_insn "lsx_v<optab>wod_w_hu_h"
  [(set (match_operand:V4SI 0 "register_operand" "=f")
	(addmul:V4SI
	  (zero_extend:V4SI
	    (vec_select:V4HI
	      (match_operand:V8HI 1 "register_operand" "%f")
	      (parallel [(const_int 1) (const_int 3)
			 (const_int 5) (const_int 7)])))
	  (sign_extend:V4SI
	    (vec_select:V4HI
	      (match_operand:V8HI 2 "register_operand" "f")
	      (parallel [(const_int 1) (const_int 3)
			 (const_int 5) (const_int 7)])))))]
  "ISA_HAS_LSX"
  "v<optab>wod.w.hu.h\t%w0,%w1,%w2"
  [(set_attr "type" "simd_int_arith")
   (set_attr "mode" "V4SI")])

(define_insn "lsx_v<optab>wod_h_bu_b"
  [(set (match_operand:V8HI 0 "register_operand" "=f")
	(addmul:V8HI
	  (zero_extend:V8HI
	    (vec_select:V8QI
	      (match_operand:V16QI 1 "register_operand" "%f")
	      (parallel [(const_int 1) (const_int 3)
			 (const_int 5) (const_int 7)
			 (const_int 9) (const_int 11)
			 (const_int 13) (const_int 15)])))
	  (sign_extend:V8HI
	    (vec_select:V8QI
	      (match_operand:V16QI 2 "register_operand" "f")
	      (parallel [(const_int 1) (const_int 3)
			 (const_int 5) (const_int 7)
			 (const_int 9) (const_int 11)
			 (const_int 13) (const_int 15)])))))]
  "ISA_HAS_LSX"
  "v<optab>wod.h.bu.b\t%w0,%w1,%w2"
  [(set_attr "type" "simd_int_arith")
   (set_attr "mode" "V8HI")])

(define_insn "lsx_vaddwev_q_d"
  [(set (match_operand:V2DI 0 "register_operand" "=f")
	(unspec:V2DI [(match_operand:V2DI 1 "register_operand" "f")
		      (match_operand:V2DI 2 "register_operand" "f")]
		     UNSPEC_LSX_VADDWEV))]
  "ISA_HAS_LSX"
  "vaddwev.q.d\t%w0,%w1,%w2"
  [(set_attr "type" "simd_int_arith")
   (set_attr "mode" "V2DI")])

(define_insn "lsx_vaddwev_q_du"
  [(set (match_operand:V2DI 0 "register_operand" "=f")
	(unspec:V2DI [(match_operand:V2DI 1 "register_operand" "f")
		      (match_operand:V2DI 2 "register_operand" "f")]
		     UNSPEC_LSX_VADDWEV2))]
  "ISA_HAS_LSX"
  "vaddwev.q.du\t%w0,%w1,%w2"
  [(set_attr "type" "simd_int_arith")
   (set_attr "mode" "V2DI")])

(define_insn "lsx_vaddwod_q_d"
  [(set (match_operand:V2DI 0 "register_operand" "=f")
	(unspec:V2DI [(match_operand:V2DI 1 "register_operand" "f")
		      (match_operand:V2DI 2 "register_operand" "f")]
		     UNSPEC_LSX_VADDWOD))]
  "ISA_HAS_LSX"
  "vaddwod.q.d\t%w0,%w1,%w2"
  [(set_attr "type" "simd_int_arith")
   (set_attr "mode" "V2DI")])

(define_insn "lsx_vaddwod_q_du"
  [(set (match_operand:V2DI 0 "register_operand" "=f")
	(unspec:V2DI [(match_operand:V2DI 1 "register_operand" "f")
		      (match_operand:V2DI 2 "register_operand" "f")]
		     UNSPEC_LSX_VADDWOD2))]
  "ISA_HAS_LSX"
  "vaddwod.q.du\t%w0,%w1,%w2"
  [(set_attr "type" "simd_int_arith")
   (set_attr "mode" "V2DI")])

(define_insn "lsx_vsubwev_q_d"
  [(set (match_operand:V2DI 0 "register_operand" "=f")
	(unspec:V2DI [(match_operand:V2DI 1 "register_operand" "f")
		      (match_operand:V2DI 2 "register_operand" "f")]
		     UNSPEC_LSX_VSUBWEV))]
  "ISA_HAS_LSX"
  "vsubwev.q.d\t%w0,%w1,%w2"
  [(set_attr "type" "simd_int_arith")
   (set_attr "mode" "V2DI")])

(define_insn "lsx_vsubwev_q_du"
  [(set (match_operand:V2DI 0 "register_operand" "=f")
	(unspec:V2DI [(match_operand:V2DI 1 "register_operand" "f")
		      (match_operand:V2DI 2 "register_operand" "f")]
		     UNSPEC_LSX_VSUBWEV2))]
  "ISA_HAS_LSX"
  "vsubwev.q.du\t%w0,%w1,%w2"
  [(set_attr "type" "simd_int_arith")
   (set_attr "mode" "V2DI")])

(define_insn "lsx_vsubwod_q_d"
  [(set (match_operand:V2DI 0 "register_operand" "=f")
	(unspec:V2DI [(match_operand:V2DI 1 "register_operand" "f")
		      (match_operand:V2DI 2 "register_operand" "f")]
		     UNSPEC_LSX_VSUBWOD))]
  "ISA_HAS_LSX"
  "vsubwod.q.d\t%w0,%w1,%w2"
  [(set_attr "type" "simd_int_arith")
   (set_attr "mode" "V2DI")])

(define_insn "lsx_vsubwod_q_du"
  [(set (match_operand:V2DI 0 "register_operand" "=f")
	(unspec:V2DI [(match_operand:V2DI 1 "register_operand" "f")
		      (match_operand:V2DI 2 "register_operand" "f")]
		     UNSPEC_LSX_VSUBWOD2))]
  "ISA_HAS_LSX"
  "vsubwod.q.du\t%w0,%w1,%w2"
  [(set_attr "type" "simd_int_arith")
   (set_attr "mode" "V2DI")])

(define_insn "lsx_vaddwev_q_du_d"
  [(set (match_operand:V2DI 0 "register_operand" "=f")
	(unspec:V2DI [(match_operand:V2DI 1 "register_operand" "f")
		      (match_operand:V2DI 2 "register_operand" "f")]
		     UNSPEC_LSX_VADDWEV3))]
  "ISA_HAS_LSX"
  "vaddwev.q.du.d\t%w0,%w1,%w2"
  [(set_attr "type" "simd_int_arith")
   (set_attr "mode" "V2DI")])

(define_insn "lsx_vaddwod_q_du_d"
  [(set (match_operand:V2DI 0 "register_operand" "=f")
	(unspec:V2DI [(match_operand:V2DI 1 "register_operand" "f")
		      (match_operand:V2DI 2 "register_operand" "f")]
		     UNSPEC_LSX_VADDWOD3))]
  "ISA_HAS_LSX"
  "vaddwod.q.du.d\t%w0,%w1,%w2"
  [(set_attr "type" "simd_int_arith")
   (set_attr "mode" "V2DI")])

(define_insn "lsx_vmulwev_q_du_d"
  [(set (match_operand:V2DI 0 "register_operand" "=f")
	(unspec:V2DI [(match_operand:V2DI 1 "register_operand" "f")
		      (match_operand:V2DI 2 "register_operand" "f")]
		     UNSPEC_LSX_VMULWEV3))]
  "ISA_HAS_LSX"
  "vmulwev.q.du.d\t%w0,%w1,%w2"
  [(set_attr "type" "simd_int_arith")
   (set_attr "mode" "V2DI")])

(define_insn "lsx_vmulwod_q_du_d"
  [(set (match_operand:V2DI 0 "register_operand" "=f")
	(unspec:V2DI [(match_operand:V2DI 1 "register_operand" "f")
		      (match_operand:V2DI 2 "register_operand" "f")]
		     UNSPEC_LSX_VMULWOD3))]
  "ISA_HAS_LSX"
  "vmulwod.q.du.d\t%w0,%w1,%w2"
  [(set_attr "type" "simd_int_arith")
   (set_attr "mode" "V2DI")])

(define_insn "lsx_vmulwev_q_d"
  [(set (match_operand:V2DI 0 "register_operand" "=f")
	(unspec:V2DI [(match_operand:V2DI 1 "register_operand" "f")
		      (match_operand:V2DI 2 "register_operand" "f")]
		     UNSPEC_LSX_VMULWEV))]
  "ISA_HAS_LSX"
  "vmulwev.q.d\t%w0,%w1,%w2"
  [(set_attr "type" "simd_int_arith")
   (set_attr "mode" "V2DI")])

(define_insn "lsx_vmulwev_q_du"
  [(set (match_operand:V2DI 0 "register_operand" "=f")
	(unspec:V2DI [(match_operand:V2DI 1 "register_operand" "f")
		      (match_operand:V2DI 2 "register_operand" "f")]
		     UNSPEC_LSX_VMULWEV2))]
  "ISA_HAS_LSX"
  "vmulwev.q.du\t%w0,%w1,%w2"
  [(set_attr "type" "simd_int_arith")
   (set_attr "mode" "V2DI")])

(define_insn "lsx_vmulwod_q_d"
  [(set (match_operand:V2DI 0 "register_operand" "=f")
	(unspec:V2DI [(match_operand:V2DI 1 "register_operand" "f")
		      (match_operand:V2DI 2 "register_operand" "f")]
		     UNSPEC_LSX_VMULWOD))]
  "ISA_HAS_LSX"
  "vmulwod.q.d\t%w0,%w1,%w2"
  [(set_attr "type" "simd_int_arith")
   (set_attr "mode" "V2DI")])

(define_insn "lsx_vmulwod_q_du"
  [(set (match_operand:V2DI 0 "register_operand" "=f")
	(unspec:V2DI [(match_operand:V2DI 1 "register_operand" "f")
		      (match_operand:V2DI 2 "register_operand" "f")]
		     UNSPEC_LSX_VMULWOD2))]
  "ISA_HAS_LSX"
  "vmulwod.q.du\t%w0,%w1,%w2"
  [(set_attr "type" "simd_int_arith")
   (set_attr "mode" "V2DI")])

(define_insn "lsx_vhaddw_q_d"
  [(set (match_operand:V2DI 0 "register_operand" "=f")
	(unspec:V2DI [(match_operand:V2DI 1 "register_operand" "f")
		      (match_operand:V2DI 2 "register_operand" "f")]
		     UNSPEC_LSX_VHADDW_Q_D))]
  "ISA_HAS_LSX"
  "vhaddw.q.d\t%w0,%w1,%w2"
  [(set_attr "type" "simd_int_arith")
   (set_attr "mode" "V2DI")])

(define_insn "lsx_vhaddw_qu_du"
  [(set (match_operand:V2DI 0 "register_operand" "=f")
	(unspec:V2DI [(match_operand:V2DI 1 "register_operand" "f")
		      (match_operand:V2DI 2 "register_operand" "f")]
		     UNSPEC_LSX_VHADDW_QU_DU))]
  "ISA_HAS_LSX"
  "vhaddw.qu.du\t%w0,%w1,%w2"
  [(set_attr "type" "simd_int_arith")
   (set_attr "mode" "V2DI")])

(define_insn "lsx_vhsubw_q_d"
  [(set (match_operand:V2DI 0 "register_operand" "=f")
	(unspec:V2DI [(match_operand:V2DI 1 "register_operand" "f")
		      (match_operand:V2DI 2 "register_operand" "f")]
		     UNSPEC_LSX_VHSUBW_Q_D))]
  "ISA_HAS_LSX"
  "vhsubw.q.d\t%w0,%w1,%w2"
  [(set_attr "type" "simd_int_arith")
   (set_attr "mode" "V2DI")])

(define_insn "lsx_vhsubw_qu_du"
  [(set (match_operand:V2DI 0 "register_operand" "=f")
	(unspec:V2DI [(match_operand:V2DI 1 "register_operand" "f")
		      (match_operand:V2DI 2 "register_operand" "f")]
		     UNSPEC_LSX_VHSUBW_QU_DU))]
  "ISA_HAS_LSX"
  "vhsubw.qu.du\t%w0,%w1,%w2"
  [(set_attr "type" "simd_int_arith")
   (set_attr "mode" "V2DI")])

(define_insn "lsx_vmaddwev_d_w<u>"
  [(set (match_operand:V2DI 0 "register_operand" "=f")
	(plus:V2DI
	  (match_operand:V2DI 1 "register_operand" "0")
	  (mult:V2DI
	    (any_extend:V2DI
	      (vec_select:V2SI
		(match_operand:V4SI 2 "register_operand" "%f")
		(parallel [(const_int 0) (const_int 2)])))
	    (any_extend:V2DI
	      (vec_select:V2SI
		(match_operand:V4SI 3 "register_operand" "f")
		(parallel [(const_int 0) (const_int 2)]))))))]
  "ISA_HAS_LSX"
  "vmaddwev.d.w<u>\t%w0,%w2,%w3"
  [(set_attr "type" "simd_fmadd")
   (set_attr "mode" "V2DI")])

(define_insn "lsx_vmaddwev_w_h<u>"
  [(set (match_operand:V4SI 0 "register_operand" "=f")
	(plus:V4SI
	  (match_operand:V4SI 1 "register_operand" "0")
	  (mult:V4SI
	    (any_extend:V4SI
	      (vec_select:V4HI
		(match_operand:V8HI 2 "register_operand" "%f")
		(parallel [(const_int 0) (const_int 2)
			   (const_int 4) (const_int 6)])))
	    (any_extend:V4SI
	      (vec_select:V4HI
		(match_operand:V8HI 3 "register_operand" "f")
		(parallel [(const_int 0) (const_int 2)
			   (const_int 4) (const_int 6)]))))))]
  "ISA_HAS_LSX"
  "vmaddwev.w.h<u>\t%w0,%w2,%w3"
  [(set_attr "type" "simd_fmadd")
   (set_attr "mode" "V4SI")])

(define_insn "lsx_vmaddwev_h_b<u>"
  [(set (match_operand:V8HI 0 "register_operand" "=f")
	(plus:V8HI
	  (match_operand:V8HI 1 "register_operand" "0")
	  (mult:V8HI
	    (any_extend:V8HI
	      (vec_select:V8QI
		(match_operand:V16QI 2 "register_operand" "%f")
		(parallel [(const_int 0) (const_int 2)
			   (const_int 4) (const_int 6)
			   (const_int 8) (const_int 10)
			   (const_int 12) (const_int 14)])))
	    (any_extend:V8HI
	      (vec_select:V8QI
		(match_operand:V16QI 3 "register_operand" "f")
		(parallel [(const_int 0) (const_int 2)
			   (const_int 4) (const_int 6)
			   (const_int 8) (const_int 10)
			   (const_int 12) (const_int 14)]))))))]
  "ISA_HAS_LSX"
  "vmaddwev.h.b<u>\t%w0,%w2,%w3"
  [(set_attr "type" "simd_fmadd")
   (set_attr "mode" "V8HI")])

(define_insn "lsx_vmaddwod_d_w<u>"
  [(set (match_operand:V2DI 0 "register_operand" "=f")
	(plus:V2DI
	  (match_operand:V2DI 1 "register_operand" "0")
	  (mult:V2DI
	    (any_extend:V2DI
	      (vec_select:V2SI
		(match_operand:V4SI 2 "register_operand" "%f")
		(parallel [(const_int 1) (const_int 3)])))
	    (any_extend:V2DI
	      (vec_select:V2SI
		(match_operand:V4SI 3 "register_operand" "f")
		(parallel [(const_int 1) (const_int 3)]))))))]
  "ISA_HAS_LSX"
  "vmaddwod.d.w<u>\t%w0,%w2,%w3"
  [(set_attr "type" "simd_fmadd")
   (set_attr "mode" "V2DI")])

(define_insn "lsx_vmaddwod_w_h<u>"
  [(set (match_operand:V4SI 0 "register_operand" "=f")
	(plus:V4SI
	  (match_operand:V4SI 1 "register_operand" "0")
	  (mult:V4SI
	    (any_extend:V4SI
	      (vec_select:V4HI
		(match_operand:V8HI 2 "register_operand" "%f")
		(parallel [(const_int 1) (const_int 3)
			   (const_int 5) (const_int 7)])))
	    (any_extend:V4SI
	      (vec_select:V4HI
		(match_operand:V8HI 3 "register_operand" "f")
		(parallel [(const_int 1) (const_int 3)
			   (const_int 5) (const_int 7)]))))))]
  "ISA_HAS_LSX"
  "vmaddwod.w.h<u>\t%w0,%w2,%w3"
  [(set_attr "type" "simd_fmadd")
   (set_attr "mode" "V4SI")])

(define_insn "lsx_vmaddwod_h_b<u>"
  [(set (match_operand:V8HI 0 "register_operand" "=f")
	(plus:V8HI
	  (match_operand:V8HI 1 "register_operand" "0")
	  (mult:V8HI
	    (any_extend:V8HI
	      (vec_select:V8QI
		(match_operand:V16QI 2 "register_operand" "%f")
		(parallel [(const_int 1) (const_int 3)
			   (const_int 5) (const_int 7)
			   (const_int 9) (const_int 11)
			   (const_int 13) (const_int 15)])))
	    (any_extend:V8HI
	      (vec_select:V8QI
		(match_operand:V16QI 3 "register_operand" "f")
		(parallel [(const_int 1) (const_int 3)
			   (const_int 5) (const_int 7)
			   (const_int 9) (const_int 11)
			   (const_int 13) (const_int 15)]))))))]
  "ISA_HAS_LSX"
  "vmaddwod.h.b<u>\t%w0,%w2,%w3"
  [(set_attr "type" "simd_fmadd")
   (set_attr "mode" "V8HI")])

(define_insn "lsx_vmaddwev_d_wu_w"
  [(set (match_operand:V2DI 0 "register_operand" "=f")
	(plus:V2DI
	  (match_operand:V2DI 1 "register_operand" "0")
	  (mult:V2DI
	    (zero_extend:V2DI
	      (vec_select:V2SI
		(match_operand:V4SI 2 "register_operand" "%f")
		(parallel [(const_int 0) (const_int 2)])))
	    (sign_extend:V2DI
	      (vec_select:V2SI
		(match_operand:V4SI 3 "register_operand" "f")
		(parallel [(const_int 0) (const_int 2)]))))))]
  "ISA_HAS_LSX"
  "vmaddwev.d.wu.w\t%w0,%w2,%w3"
  [(set_attr "type" "simd_fmadd")
   (set_attr "mode" "V2DI")])

(define_insn "lsx_vmaddwev_w_hu_h"
  [(set (match_operand:V4SI 0 "register_operand" "=f")
	(plus:V4SI
	  (match_operand:V4SI 1 "register_operand" "0")
	  (mult:V4SI
	    (zero_extend:V4SI
	      (vec_select:V4HI
		(match_operand:V8HI 2 "register_operand" "%f")
		(parallel [(const_int 0) (const_int 2)
			   (const_int 4) (const_int 6)])))
	    (sign_extend:V4SI
	      (vec_select:V4HI
		(match_operand:V8HI 3 "register_operand" "f")
		(parallel [(const_int 0) (const_int 2)
			   (const_int 4) (const_int 6)]))))))]
  "ISA_HAS_LSX"
  "vmaddwev.w.hu.h\t%w0,%w2,%w3"
  [(set_attr "type" "simd_fmadd")
   (set_attr "mode" "V4SI")])

(define_insn "lsx_vmaddwev_h_bu_b"
  [(set (match_operand:V8HI 0 "register_operand" "=f")
	(plus:V8HI
	  (match_operand:V8HI 1 "register_operand" "0")
	  (mult:V8HI
	    (zero_extend:V8HI
	      (vec_select:V8QI
		(match_operand:V16QI 2 "register_operand" "%f")
		(parallel [(const_int 0) (const_int 2)
			   (const_int 4) (const_int 6)
			   (const_int 8) (const_int 10)
			   (const_int 12) (const_int 14)])))
	    (sign_extend:V8HI
	      (vec_select:V8QI
		(match_operand:V16QI 3 "register_operand" "f")
		(parallel [(const_int 0) (const_int 2)
			   (const_int 4) (const_int 6)
			   (const_int 8) (const_int 10)
			   (const_int 12) (const_int 14)]))))))]
  "ISA_HAS_LSX"
  "vmaddwev.h.bu.b\t%w0,%w2,%w3"
  [(set_attr "type" "simd_fmadd")
   (set_attr "mode" "V8HI")])

(define_insn "lsx_vmaddwod_d_wu_w"
  [(set (match_operand:V2DI 0 "register_operand" "=f")
	(plus:V2DI
	  (match_operand:V2DI 1 "register_operand" "0")
	  (mult:V2DI
	    (zero_extend:V2DI
	      (vec_select:V2SI
		(match_operand:V4SI 2 "register_operand" "%f")
		(parallel [(const_int 1) (const_int 3)])))
	    (sign_extend:V2DI
	      (vec_select:V2SI
		(match_operand:V4SI 3 "register_operand" "f")
		(parallel [(const_int 1) (const_int 3)]))))))]
  "ISA_HAS_LSX"
  "vmaddwod.d.wu.w\t%w0,%w2,%w3"
  [(set_attr "type" "simd_fmadd")
   (set_attr "mode" "V2DI")])

(define_insn "lsx_vmaddwod_w_hu_h"
  [(set (match_operand:V4SI 0 "register_operand" "=f")
	(plus:V4SI
	  (match_operand:V4SI 1 "register_operand" "0")
	  (mult:V4SI
	    (zero_extend:V4SI
	      (vec_select:V4HI
		(match_operand:V8HI 2 "register_operand" "%f")
		(parallel [(const_int 1) (const_int 3)
			   (const_int 5) (const_int 7)])))
	    (sign_extend:V4SI
	      (vec_select:V4HI
		(match_operand:V8HI 3 "register_operand" "f")
		(parallel [(const_int 1) (const_int 3)
			   (const_int 5) (const_int 7)]))))))]
  "ISA_HAS_LSX"
  "vmaddwod.w.hu.h\t%w0,%w2,%w3"
  [(set_attr "type" "simd_fmadd")
   (set_attr "mode" "V4SI")])

(define_insn "lsx_vmaddwod_h_bu_b"
  [(set (match_operand:V8HI 0 "register_operand" "=f")
	(plus:V8HI
	  (match_operand:V8HI 1 "register_operand" "0")
	  (mult:V8HI
	    (zero_extend:V8HI
	      (vec_select:V8QI
		(match_operand:V16QI 2 "register_operand" "%f")
		(parallel [(const_int 1) (const_int 3)
			   (const_int 5) (const_int 7)
			   (const_int 9) (const_int 11)
			   (const_int 13) (const_int 15)])))
	    (sign_extend:V8HI
	      (vec_select:V8QI
		(match_operand:V16QI 3 "register_operand" "f")
		(parallel [(const_int 1) (const_int 3)
			   (const_int 5) (const_int 7)
			   (const_int 9) (const_int 11)
			   (const_int 13) (const_int 15)]))))))]
  "ISA_HAS_LSX"
  "vmaddwod.h.bu.b\t%w0,%w2,%w3"
  [(set_attr "type" "simd_fmadd")
   (set_attr "mode" "V8HI")])

(define_insn "lsx_vmaddwev_q_d"
  [(set (match_operand:V2DI 0 "register_operand" "=f")
	(unspec:V2DI [(match_operand:V2DI 1 "register_operand" "0")
		      (match_operand:V2DI 2 "register_operand" "f")
		      (match_operand:V2DI 3 "register_operand" "f")]
		     UNSPEC_LSX_VMADDWEV))]
  "ISA_HAS_LSX"
  "vmaddwev.q.d\t%w0,%w2,%w3"
  [(set_attr "type" "simd_int_arith")
   (set_attr "mode" "V2DI")])

(define_insn "lsx_vmaddwod_q_d"
  [(set (match_operand:V2DI 0 "register_operand" "=f")
	(unspec:V2DI [(match_operand:V2DI 1 "register_operand" "0")
		      (match_operand:V2DI 2 "register_operand" "f")
		      (match_operand:V2DI 3 "register_operand" "f")]
		     UNSPEC_LSX_VMADDWOD))]
  "ISA_HAS_LSX"
  "vmaddwod.q.d\t%w0,%w2,%w3"
  [(set_attr "type" "simd_int_arith")
   (set_attr "mode" "V2DI")])

(define_insn "lsx_vmaddwev_q_du"
  [(set (match_operand:V2DI 0 "register_operand" "=f")
	(unspec:V2DI [(match_operand:V2DI 1 "register_operand" "0")
		      (match_operand:V2DI 2 "register_operand" "f")
		      (match_operand:V2DI 3 "register_operand" "f")]
		     UNSPEC_LSX_VMADDWEV2))]
  "ISA_HAS_LSX"
  "vmaddwev.q.du\t%w0,%w2,%w3"
  [(set_attr "type" "simd_int_arith")
   (set_attr "mode" "V2DI")])

(define_insn "lsx_vmaddwod_q_du"
  [(set (match_operand:V2DI 0 "register_operand" "=f")
	(unspec:V2DI [(match_operand:V2DI 1 "register_operand" "0")
		      (match_operand:V2DI 2 "register_operand" "f")
		      (match_operand:V2DI 3 "register_operand" "f")]
		     UNSPEC_LSX_VMADDWOD2))]
  "ISA_HAS_LSX"
  "vmaddwod.q.du\t%w0,%w2,%w3"
  [(set_attr "type" "simd_int_arith")
   (set_attr "mode" "V2DI")])

(define_insn "lsx_vmaddwev_q_du_d"
  [(set (match_operand:V2DI 0 "register_operand" "=f")
	(unspec:V2DI [(match_operand:V2DI 1 "register_operand" "0")
		      (match_operand:V2DI 2 "register_operand" "f")
		      (match_operand:V2DI 3 "register_operand" "f")]
		     UNSPEC_LSX_VMADDWEV3))]
  "ISA_HAS_LSX"
  "vmaddwev.q.du.d\t%w0,%w2,%w3"
  [(set_attr "type" "simd_int_arith")
   (set_attr "mode" "V2DI")])

(define_insn "lsx_vmaddwod_q_du_d"
  [(set (match_operand:V2DI 0 "register_operand" "=f")
	(unspec:V2DI [(match_operand:V2DI 1 "register_operand" "0")
		      (match_operand:V2DI 2 "register_operand" "f")
		      (match_operand:V2DI 3 "register_operand" "f")]
		     UNSPEC_LSX_VMADDWOD3))]
  "ISA_HAS_LSX"
  "vmaddwod.q.du.d\t%w0,%w2,%w3"
  [(set_attr "type" "simd_int_arith")
   (set_attr "mode" "V2DI")])

(define_insn "lsx_vadd_q"
  [(set (match_operand:V2DI 0 "register_operand" "=f")
	(unspec:V2DI [(match_operand:V2DI 1 "register_operand" "f")
		      (match_operand:V2DI 2 "register_operand" "f")]
		     UNSPEC_LSX_VADD_Q))]
  "ISA_HAS_LSX"
  "vadd.q\t%w0,%w1,%w2"
  [(set_attr "type" "simd_int_arith")
   (set_attr "mode" "V2DI")])

(define_insn "lsx_vsub_q"
  [(set (match_operand:V2DI 0 "register_operand" "=f")
	(unspec:V2DI [(match_operand:V2DI 1 "register_operand" "f")
		      (match_operand:V2DI 2 "register_operand" "f")]
		     UNSPEC_LSX_VSUB_Q))]
  "ISA_HAS_LSX"
  "vsub.q\t%w0,%w1,%w2"
  [(set_attr "type" "simd_int_arith")
   (set_attr "mode" "V2DI")])

(define_insn "lsx_vmskgez_b"
  [(set (match_operand:V16QI 0 "register_operand" "=f")
	(unspec:V16QI [(match_operand:V16QI 1 "register_operand" "f")]
		      UNSPEC_LSX_VMSKGEZ))]
  "ISA_HAS_LSX"
  "vmskgez.b\t%w0,%w1"
  [(set_attr "type" "simd_bit")
   (set_attr "mode" "V16QI")])

(define_insn "lsx_vmsknz_b"
  [(set (match_operand:V16QI 0 "register_operand" "=f")
	(unspec:V16QI [(match_operand:V16QI 1 "register_operand" "f")]
		      UNSPEC_LSX_VMSKNZ))]
  "ISA_HAS_LSX"
  "vmsknz.b\t%w0,%w1"
  [(set_attr "type" "simd_bit")
   (set_attr "mode" "V16QI")])

(define_insn "lsx_vexth_h<u>_b<u>"
  [(set (match_operand:V8HI 0 "register_operand" "=f")
	(any_extend:V8HI
	  (vec_select:V8QI
	    (match_operand:V16QI 1 "register_operand" "f")
	    (parallel [(const_int 8) (const_int 9)
		       (const_int 10) (const_int 11)
		       (const_int 12) (const_int 13)
		       (const_int 14) (const_int 15)]))))]
  "ISA_HAS_LSX"
  "vexth.h<u>.b<u>\t%w0,%w1"
  [(set_attr "type" "simd_fcvt")
   (set_attr "mode" "V8HI")])

(define_insn "lsx_vexth_w<u>_h<u>"
  [(set (match_operand:V4SI 0 "register_operand" "=f")
	(any_extend:V4SI
	  (vec_select:V4HI
	    (match_operand:V8HI 1 "register_operand" "f")
	    (parallel [(const_int 4) (const_int 5)
		       (const_int 6) (const_int 7)]))))]
  "ISA_HAS_LSX"
  "vexth.w<u>.h<u>\t%w0,%w1"
  [(set_attr "type" "simd_fcvt")
   (set_attr "mode" "V4SI")])

(define_insn "lsx_vexth_d<u>_w<u>"
  [(set (match_operand:V2DI 0 "register_operand" "=f")
	(any_extend:V2DI
	  (vec_select:V2SI
	    (match_operand:V4SI 1 "register_operand" "f")
	    (parallel [(const_int 2) (const_int 3)]))))]
  "ISA_HAS_LSX"
  "vexth.d<u>.w<u>\t%w0,%w1"
  [(set_attr "type" "simd_fcvt")
   (set_attr "mode" "V2DI")])

(define_insn "lsx_vexth_q_d"
  [(set (match_operand:V2DI 0 "register_operand" "=f")
	(unspec:V2DI [(match_operand:V2DI 1 "register_operand" "f")]
		     UNSPEC_LSX_VEXTH_Q_D))]
  "ISA_HAS_LSX"
  "vexth.q.d\t%w0,%w1"
  [(set_attr "type" "simd_fcvt")
   (set_attr "mode" "V2DI")])

(define_insn "lsx_vexth_qu_du"
  [(set (match_operand:V2DI 0 "register_operand" "=f")
	(unspec:V2DI [(match_operand:V2DI 1 "register_operand" "f")]
		     UNSPEC_LSX_VEXTH_QU_DU))]
  "ISA_HAS_LSX"
  "vexth.qu.du\t%w0,%w1"
  [(set_attr "type" "simd_fcvt")
   (set_attr "mode" "V2DI")])

(define_insn "lsx_vextl_q_d"
  [(set (match_operand:V2DI 0 "register_operand" "=f")
	(unspec:V2DI [(match_operand:V2DI 1 "register_operand" "f")]
		     UNSPEC_LSX_VEXTL_Q_D))]
  "ISA_HAS_LSX"
  "vextl.q.d\t%w0,%w1"
  [(set_attr "type" "simd_fcvt")
   (set_attr "mode" "V2DI")])

(define_insn "lsx_vsrlni_<lsxfmt>_<dlsxfmt>"
  [(set (match_operand:ILSX 0 "register_operand" "=f")
	(unspec:ILSX [(match_operand:ILSX 1 "register_operand" "0")
		      (match_operand:ILSX 2 "register_operand" "f")
		      (match_operand 3 "const_uimm8_operand" "")]
		     UNSPEC_LSX_VSRLNI))]
  "ISA_HAS_LSX"
  "vsrlni.<lsxfmt>.<dlsxfmt>\t%w0,%w2,%3"
  [(set_attr "type" "simd_shift")
   (set_attr "mode" "<MODE>")])

(define_insn "lsx_vsrlrni_<lsxfmt>_<dlsxfmt>"
  [(set (match_operand:ILSX 0 "register_operand" "=f")
	(unspec:ILSX [(match_operand:ILSX 1 "register_operand" "0")
		      (match_operand:ILSX 2 "register_operand" "f")
		      (match_operand 3 "const_uimm8_operand" "")]
		     UNSPEC_LSX_VSRLRNI))]
  "ISA_HAS_LSX"
  "vsrlrni.<lsxfmt>.<dlsxfmt>\t%w0,%w2,%3"
  [(set_attr "type" "simd_shift")
   (set_attr "mode" "<MODE>")])

(define_insn "lsx_vssrlni_<lsxfmt>_<dlsxfmt>"
  [(set (match_operand:ILSX 0 "register_operand" "=f")
	(unspec:ILSX [(match_operand:ILSX 1 "register_operand" "0")
		      (match_operand:ILSX 2 "register_operand" "f")
		      (match_operand 3 "const_uimm8_operand" "")]
		     UNSPEC_LSX_VSSRLNI))]
  "ISA_HAS_LSX"
  "vssrlni.<lsxfmt>.<dlsxfmt>\t%w0,%w2,%3"
  [(set_attr "type" "simd_shift")
   (set_attr "mode" "<MODE>")])

(define_insn "lsx_vssrlni_<lsxfmt_u>_<dlsxfmt>"
  [(set (match_operand:ILSX 0 "register_operand" "=f")
	(unspec:ILSX [(match_operand:ILSX 1 "register_operand" "0")
		      (match_operand:ILSX 2 "register_operand" "f")
		      (match_operand 3 "const_uimm8_operand" "")]
		     UNSPEC_LSX_VSSRLNI2))]
  "ISA_HAS_LSX"
  "vssrlni.<lsxfmt_u>.<dlsxfmt>\t%w0,%w2,%3"
  [(set_attr "type" "simd_shift")
   (set_attr "mode" "<MODE>")])

(define_insn "lsx_vssrlrni_<lsxfmt>_<dlsxfmt>"
  [(set (match_operand:ILSX 0 "register_operand" "=f")
	(unspec:ILSX [(match_operand:ILSX 1 "register_operand" "0")
		      (match_operand:ILSX 2 "register_operand" "f")
		      (match_operand 3 "const_uimm8_operand" "")]
		     UNSPEC_LSX_VSSRLRNI))]
  "ISA_HAS_LSX"
  "vssrlrni.<lsxfmt>.<dlsxfmt>\t%w0,%w2,%3"
  [(set_attr "type" "simd_shift")
   (set_attr "mode" "<MODE>")])

(define_insn "lsx_vssrlrni_<lsxfmt_u>_<dlsxfmt>"
  [(set (match_operand:ILSX 0 "register_operand" "=f")
	(unspec:ILSX [(match_operand:ILSX 1 "register_operand" "0")
		      (match_operand:ILSX 2 "register_operand" "f")
		      (match_operand 3 "const_uimm8_operand" "")]
		     UNSPEC_LSX_VSSRLRNI2))]
  "ISA_HAS_LSX"
  "vssrlrni.<lsxfmt_u>.<dlsxfmt>\t%w0,%w2,%3"
  [(set_attr "type" "simd_shift")
   (set_attr "mode" "<MODE>")])

(define_insn "lsx_vsrani_<lsxfmt>_<dlsxfmt>"
  [(set (match_operand:ILSX 0 "register_operand" "=f")
	(unspec:ILSX [(match_operand:ILSX 1 "register_operand" "0")
		      (match_operand:ILSX 2 "register_operand" "f")
		      (match_operand 3 "const_uimm8_operand" "")]
		     UNSPEC_LSX_VSRANI))]
  "ISA_HAS_LSX"
  "vsrani.<lsxfmt>.<dlsxfmt>\t%w0,%w2,%3"
  [(set_attr "type" "simd_shift")
   (set_attr "mode" "<MODE>")])

(define_insn "lsx_vsrarni_<lsxfmt>_<dlsxfmt>"
  [(set (match_operand:ILSX 0 "register_operand" "=f")
	(unspec:ILSX [(match_operand:ILSX 1 "register_operand" "0")
		      (match_operand:ILSX 2 "register_operand" "f")
		      (match_operand 3 "const_uimm8_operand" "")]
		     UNSPEC_LSX_VSRARNI))]
  "ISA_HAS_LSX"
  "vsrarni.<lsxfmt>.<dlsxfmt>\t%w0,%w2,%3"
  [(set_attr "type" "simd_shift")
   (set_attr "mode" "<MODE>")])

(define_insn "lsx_vssrani_<lsxfmt>_<dlsxfmt>"
  [(set (match_operand:ILSX 0 "register_operand" "=f")
	(unspec:ILSX [(match_operand:ILSX 1 "register_operand" "0")
		      (match_operand:ILSX 2 "register_operand" "f")
		      (match_operand 3 "const_uimm8_operand" "")]
		    UNSPEC_LSX_VSSRANI))]
  "ISA_HAS_LSX"
  "vssrani.<lsxfmt>.<dlsxfmt>\t%w0,%w2,%3"
  [(set_attr "type" "simd_shift")
   (set_attr "mode" "<MODE>")])

(define_insn "lsx_vssrani_<lsxfmt_u>_<dlsxfmt>"
  [(set (match_operand:ILSX 0 "register_operand" "=f")
	(unspec:ILSX [(match_operand:ILSX 1 "register_operand" "0")
		      (match_operand:ILSX 2 "register_operand" "f")
		      (match_operand 3 "const_uimm8_operand" "")]
		     UNSPEC_LSX_VSSRANI2))]
  "ISA_HAS_LSX"
  "vssrani.<lsxfmt_u>.<dlsxfmt>\t%w0,%w2,%3"
  [(set_attr "type" "simd_shift")
   (set_attr "mode" "<MODE>")])

(define_insn "lsx_vssrarni_<lsxfmt>_<dlsxfmt>"
  [(set (match_operand:ILSX 0 "register_operand" "=f")
	(unspec:ILSX [(match_operand:ILSX 1 "register_operand" "0")
		      (match_operand:ILSX 2 "register_operand" "f")
		      (match_operand 3 "const_uimm8_operand" "")]
		     UNSPEC_LSX_VSSRARNI))]
  "ISA_HAS_LSX"
  "vssrarni.<lsxfmt>.<dlsxfmt>\t%w0,%w2,%3"
  [(set_attr "type" "simd_shift")
   (set_attr "mode" "<MODE>")])

(define_insn "lsx_vssrarni_<lsxfmt_u>_<dlsxfmt>"
  [(set (match_operand:ILSX 0 "register_operand" "=f")
	(unspec:ILSX [(match_operand:ILSX 1 "register_operand" "0")
		      (match_operand:ILSX 2 "register_operand" "f")
		      (match_operand 3 "const_uimm8_operand" "")]
		     UNSPEC_LSX_VSSRARNI2))]
  "ISA_HAS_LSX"
  "vssrarni.<lsxfmt_u>.<dlsxfmt>\t%w0,%w2,%3"
  [(set_attr "type" "simd_shift")
   (set_attr "mode" "<MODE>")])

(define_insn "lsx_vpermi_w"
  [(set (match_operand:V4SI 0 "register_operand" "=f")
	(unspec:V4SI [(match_operand:V4SI 1 "register_operand" "0")
		      (match_operand:V4SI 2 "register_operand" "f")
		      (match_operand 3 "const_uimm8_operand" "")]
		     UNSPEC_LSX_VPERMI))]
  "ISA_HAS_LSX"
  "vpermi.w\t%w0,%w2,%3"
  [(set_attr "type" "simd_bit")
   (set_attr "mode" "V4SI")])

;; Delete one of two instructions that exactly play the same role.
(define_peephole2
  [(set (match_operand:V2DI 0 "register_operand")
	(vec_duplicate:V2DI (match_operand:DI 1 "register_operand")))
   (set (match_operand:V2DI 2 "register_operand")
	(vec_merge:V2DI
	  (vec_duplicate:V2DI (match_operand:DI 3 "register_operand"))
	  (match_operand:V2DI 4 "register_operand")
	  (match_operand 5 "const_int_operand")))]
  "operands[0] == operands[2] &&
   operands[1] == operands[3] &&
   operands[2] == operands[4] &&
   INTVAL (operands[5]) == 2"
  [(set (match_dup 0)
	(vec_duplicate:V2DI (match_dup 1)))]
  "")
