;; Machine Description for LARCH Loongson ASX ASE
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
  UNSPEC_LASX_XVABSD_S
  UNSPEC_LASX_XVABSD_U
  UNSPEC_LASX_XVAVG_S
  UNSPEC_LASX_XVAVG_U
  UNSPEC_LASX_XVAVGR_S
  UNSPEC_LASX_XVAVGR_U
  UNSPEC_LASX_XVBITCLR
  UNSPEC_LASX_XVBITCLRI
  UNSPEC_LASX_XVBITREV
  UNSPEC_LASX_XVBITREVI
  UNSPEC_LASX_XVBITSET
  UNSPEC_LASX_XVBITSETI
  UNSPEC_LASX_XVFCLASS
  UNSPEC_LASX_XVFCVT
  UNSPEC_LASX_XVFCVTH
  UNSPEC_LASX_XVFCVTL
  UNSPEC_LASX_XVFLOGB
  UNSPEC_LASX_XVFRECIP
  UNSPEC_LASX_XVFRECIPE
  UNSPEC_LASX_XVFRINT
  UNSPEC_LASX_XVFRSQRT
  UNSPEC_LASX_XVFRSQRTE
  UNSPEC_LASX_XVFTINT_U
  UNSPEC_LASX_XVCLO
  UNSPEC_LASX_XVSAT_S
  UNSPEC_LASX_XVSAT_U
  UNSPEC_LASX_XVREPLVE0
  UNSPEC_LASX_XVREPL128VEI
  UNSPEC_LASX_XVSRAR
  UNSPEC_LASX_XVSRARI
  UNSPEC_LASX_XVSRLR
  UNSPEC_LASX_XVSRLRI
  UNSPEC_LASX_XVSHUF
  UNSPEC_LASX_XVSHUF_B
  UNSPEC_LASX_BRANCH
  UNSPEC_LASX_BRANCH_V

  UNSPEC_LASX_MXVEXTW_U
  UNSPEC_LASX_XVSLLWIL_S
  UNSPEC_LASX_XVSLLWIL_U
  UNSPEC_LASX_XVSRAN
  UNSPEC_LASX_XVSSRAN_S
  UNSPEC_LASX_XVSSRAN_U
  UNSPEC_LASX_XVSRARN
  UNSPEC_LASX_XVSSRARN_S
  UNSPEC_LASX_XVSSRARN_U
  UNSPEC_LASX_XVSRLN
  UNSPEC_LASX_XVSSRLN_U
  UNSPEC_LASX_XVSRLRN
  UNSPEC_LASX_XVSSRLRN_U
  UNSPEC_LASX_XVFRSTPI
  UNSPEC_LASX_XVFRSTP
  UNSPEC_LASX_XVSHUF4I
  UNSPEC_LASX_XVBSRL_V
  UNSPEC_LASX_XVBSLL_V
  UNSPEC_LASX_XVEXTRINS
  UNSPEC_LASX_XVMSKLTZ
  UNSPEC_LASX_XVSIGNCOV
  UNSPEC_LASX_XVFTINT_W_D
  UNSPEC_LASX_XVFFINT_S_L
  UNSPEC_LASX_XVFTINTRZ_W_D
  UNSPEC_LASX_XVFTINTRP_W_D
  UNSPEC_LASX_XVFTINTRM_W_D
  UNSPEC_LASX_XVFTINTRNE_W_D
  UNSPEC_LASX_XVFTINTH_L_S
  UNSPEC_LASX_XVFTINTL_L_S
  UNSPEC_LASX_XVFFINTH_D_W
  UNSPEC_LASX_XVFFINTL_D_W
  UNSPEC_LASX_XVFTINTRZH_L_S
  UNSPEC_LASX_XVFTINTRZL_L_S
  UNSPEC_LASX_XVFTINTRPH_L_S
  UNSPEC_LASX_XVFTINTRPL_L_S
  UNSPEC_LASX_XVFTINTRMH_L_S
  UNSPEC_LASX_XVFTINTRML_L_S
  UNSPEC_LASX_XVFTINTRNEL_L_S
  UNSPEC_LASX_XVFTINTRNEH_L_S
  UNSPEC_LASX_XVREPLVE0_Q
  UNSPEC_LASX_XVPERM_W
  UNSPEC_LASX_XVPERMI_Q
  UNSPEC_LASX_XVPERMI_D

  UNSPEC_LASX_XVADDWEV
  UNSPEC_LASX_XVADDWEV2
  UNSPEC_LASX_XVADDWEV3
  UNSPEC_LASX_XVSUBWEV
  UNSPEC_LASX_XVSUBWEV2
  UNSPEC_LASX_XVMULWEV
  UNSPEC_LASX_XVMULWEV2
  UNSPEC_LASX_XVMULWEV3
  UNSPEC_LASX_XVADDWOD
  UNSPEC_LASX_XVADDWOD2
  UNSPEC_LASX_XVADDWOD3
  UNSPEC_LASX_XVSUBWOD
  UNSPEC_LASX_XVSUBWOD2
  UNSPEC_LASX_XVMULWOD
  UNSPEC_LASX_XVMULWOD2
  UNSPEC_LASX_XVMULWOD3
  UNSPEC_LASX_XVMADDWEV
  UNSPEC_LASX_XVMADDWEV2
  UNSPEC_LASX_XVMADDWEV3
  UNSPEC_LASX_XVMADDWOD
  UNSPEC_LASX_XVMADDWOD2
  UNSPEC_LASX_XVMADDWOD3
  UNSPEC_LASX_XVHADDW_Q_D
  UNSPEC_LASX_XVHSUBW_Q_D
  UNSPEC_LASX_XVHADDW_QU_DU
  UNSPEC_LASX_XVHSUBW_QU_DU
  UNSPEC_LASX_XVADD_Q
  UNSPEC_LASX_XVSUB_Q
  UNSPEC_LASX_XVREPLVE
  UNSPEC_LASX_XVSHUF4
  UNSPEC_LASX_XVMSKGEZ
  UNSPEC_LASX_XVMSKNZ
  UNSPEC_LASX_XVEXTH_Q_D
  UNSPEC_LASX_XVEXTH_QU_DU
  UNSPEC_LASX_XVEXTL_Q_D
  UNSPEC_LASX_XVSRLNI
  UNSPEC_LASX_XVSRLRNI
  UNSPEC_LASX_XVSSRLNI
  UNSPEC_LASX_XVSSRLNI2
  UNSPEC_LASX_XVSSRLRNI
  UNSPEC_LASX_XVSSRLRNI2
  UNSPEC_LASX_XVSRANI
  UNSPEC_LASX_XVSRARNI
  UNSPEC_LASX_XVSSRANI
  UNSPEC_LASX_XVSSRANI2
  UNSPEC_LASX_XVSSRARNI
  UNSPEC_LASX_XVSSRARNI2
  UNSPEC_LASX_XVPERMI
  UNSPEC_LASX_XVINSVE0
  UNSPEC_LASX_XVPICKVE
  UNSPEC_LASX_XVSSRLN
  UNSPEC_LASX_XVSSRLRN
  UNSPEC_LASX_XVEXTL_QU_DU
  UNSPEC_LASX_XVLDI
  UNSPEC_LASX_XVLDX
  UNSPEC_LASX_XVSTX
  UNSPEC_LASX_VECINIT_MERGE
  UNSPEC_LASX_VEC_SET_INTERNAL
  UNSPEC_LASX_XVILVL_INTERNAL
])

;; All vector modes with 256 bits.
(define_mode_iterator LASX [V4DF V8SF V4DI V8SI V16HI V32QI])

;; Only used for splitting insert_d and copy_{u,s}.d.
(define_mode_iterator LASX_D [V4DI V4DF])

;; Only used for splitting insert_d and copy_{u,s}.d.
(define_mode_iterator LASX_WD [V4DI V4DF V8SI V8SF])

;; Only used for copy256_{u,s}.w.
(define_mode_iterator LASX_W    [V8SI V8SF])

;; As ILASX but excludes V32QI.
(define_mode_iterator ILASX_DWH [V4DI V8SI V16HI])

;; As LASX but excludes V32QI.
(define_mode_iterator LASX_DWH [V4DF V8SF V4DI V8SI V16HI])

;; As ILASX but excludes V4DI.
(define_mode_iterator ILASX_WHB [V8SI V16HI V32QI])

;; Only integer modes equal or larger than a word.
(define_mode_iterator ILASX_DW  [V4DI V8SI])

;; Only integer modes smaller than a word.
(define_mode_iterator ILASX_HB  [V16HI V32QI])

;; Only used for immediate set shuffle elements instruction.
(define_mode_iterator LASX_WHB_W [V8SI V16HI V32QI V8SF])

;; The attribute gives the integer vector mode with same size in Loongson ASX.
(define_mode_attr VIMODE256
  [(V4DF "V4DI")
   (V8SF "V8SI")
   (V4DI "V4DI")
   (V8SI "V8SI")
   (V16HI "V16HI")
   (V32QI "V32QI")])

;;attribute gives half modes for vector modes.
;;attribute gives half modes (Same Size) for vector modes.
(define_mode_attr VHSMODE256
  [(V16HI "V32QI")
   (V8SI "V16HI")
   (V4DI "V8SI")])

;;attribute gives half modes  for vector modes.
(define_mode_attr VHMODE256
  [(V32QI "V16QI")
   (V16HI "V8HI")
   (V8SI "V4SI")
   (V4DI "V2DI")])

;;attribute gives half float modes for vector modes.
(define_mode_attr VFHMODE256
   [(V8SF "V4SF")
   (V4DF "V2DF")])

;; The attribute gives half int/float modes for vector modes.
(define_mode_attr VHMODE256_ALL
  [(V32QI "V16QI")
   (V16HI "V8HI")
   (V8SI "V4SI")
   (V4DI "V2DI")
   (V8SF "V4SF")
   (V4DF "V2DF")])

;; The attribute gives double modes for vector modes in LASX.
(define_mode_attr VDMODE256
  [(V8SI "V4DI")
   (V16HI "V8SI")
   (V32QI "V16HI")])

;; extended from VDMODE256
(define_mode_attr VDMODEEXD256
  [(V4DI "V4DI")
   (V8SI "V4DI")
   (V16HI "V8SI")
   (V32QI "V16HI")])

;; The attribute gives half modes with same number of elements for vector modes.
(define_mode_attr VTRUNCMODE256
  [(V16HI "V16QI")
   (V8SI "V8HI")
   (V4DI "V4SI")])

;; Double-sized Vector MODE with same elemet type. "Vector, Enlarged-MODE"
(define_mode_attr VEMODE256
  [(V8SF "V16SF")
   (V8SI "V16SI")
   (V4DF "V8DF")
   (V4DI "V8DI")])

;; This attribute gives the mode of the result for "copy_s_b, copy_u_b" etc.
(define_mode_attr VRES256
  [(V4DF "DF")
   (V8SF "SF")
   (V4DI "DI")
   (V8SI "SI")
   (V16HI "SI")
   (V32QI "SI")])

;; Only used with LASX_D iterator.
(define_mode_attr lasx_d
  [(V4DI "reg_or_0")
   (V4DF "register")])

;; This attribute gives the 256 bit integer vector mode with same size.
(define_mode_attr mode256_i
  [(V4DF "v4di")
   (V8SF "v8si")
   (V4DI "v4di")
   (V8SI "v8si")
   (V16HI "v16hi")
   (V32QI "v32qi")])


;; This attribute gives the 256 bit float vector mode with same size.
(define_mode_attr mode256_f
  [(V4DF "v4df")
   (V8SF "v8sf")
   (V4DI "v4df")
   (V8SI "v8sf")])

;; This attribute gives V32QI mode and V16HI mode with half size.
(define_mode_attr mode256_i_half
  [(V32QI "v16qi")
   (V16HI "v8hi")])

 ;; This attribute gives suffix for LASX instructions.  HOW?
(define_mode_attr lasxfmt
  [(V4DF "d")
   (V8SF "w")
   (V4DI "d")
   (V8SI "w")
   (V16HI "h")
   (V32QI "b")])

(define_mode_attr flasxfmt
  [(V4DF "d")
   (V8SF "s")])

(define_mode_attr lasxfmt_u
  [(V4DF "du")
   (V8SF "wu")
   (V4DI "du")
   (V8SI "wu")
   (V16HI "hu")
   (V32QI "bu")])

(define_mode_attr ilasxfmt
  [(V4DF "l")
   (V8SF "w")])

(define_mode_attr ilasxfmt_u
  [(V4DF "lu")
   (V8SF "wu")])

;; This attribute gives suffix for integers in VHMODE256.
(define_mode_attr hlasxfmt
  [(V4DI "w")
   (V8SI "h")
   (V16HI "b")])

(define_mode_attr hlasxfmt_u
  [(V4DI "wu")
   (V8SI "hu")
   (V16HI "bu")])

;; This attribute gives suffix for integers in VHSMODE256.
(define_mode_attr hslasxfmt
  [(V4DI "w")
   (V8SI "h")
   (V16HI "b")])

;; This attribute gives define_insn suffix for LASX instructions that need
;; distinction between integer and floating point.
(define_mode_attr lasxfmt_f
  [(V4DF "d_f")
   (V8SF "w_f")
   (V4DI "d")
   (V8SI "w")
   (V16HI "h")
   (V32QI "b")])

(define_mode_attr flasxfmt_f
  [(V4DF "d_f")
   (V8SF "s_f")
   (V4DI "d")
   (V8SI "w")
   (V16HI "h")
   (V32QI "b")])

;; This attribute gives define_insn suffix for LASX instructions that need
;; distinction between integer and floating point.
(define_mode_attr lasxfmt_f_wd
  [(V4DF "d_f")
   (V8SF "w_f")
   (V4DI "d")
   (V8SI "w")])

;; This attribute gives suffix for integers in VHMODE256.
(define_mode_attr dlasxfmt
  [(V8SI "d")
   (V16HI "w")
   (V32QI "h")])

(define_mode_attr dlasxfmt_u
  [(V8SI "du")
   (V16HI "wu")
   (V32QI "hu")])

;; for VDMODEEXD256
(define_mode_attr dlasxqfmt
  [(V4DI "q")
   (V8SI "d")
   (V16HI "w")
   (V32QI "h")])

;; This is used to form an immediate operand constraint using
;; "const_<indeximm256>_operand".
(define_mode_attr indeximm256
  [(V4DF "0_to_3")
   (V8SF "0_to_7")
   (V4DI "0_to_3")
   (V8SI "0_to_7")
   (V16HI "uimm4")
   (V32QI "uimm5")])

;; This is used to form an immediate operand constraint using to ref high half
;; "const_<indeximm_hi>_operand".
(define_mode_attr indeximm_hi
  [(V4DF "2_or_3")
   (V8SF "4_to_7")
   (V4DI "2_or_3")
   (V8SI "4_to_7")
   (V16HI "8_to_15")
   (V32QI "16_to_31")])

;; This is used to form an immediate operand constraint using to ref low half
;; "const_<indeximm_lo>_operand".
(define_mode_attr indeximm_lo
  [(V4DF "0_or_1")
   (V8SF "0_to_3")
   (V4DI "0_or_1")
   (V8SI "0_to_3")
   (V16HI "uimm3")
   (V32QI "uimm4")])

;; This attribute represents bitmask needed for vec_merge using in lasx
;; "const_<bitmask256>_operand".
(define_mode_attr bitmask256
  [(V4DF "exp_4")
   (V8SF "exp_8")
   (V4DI "exp_4")
   (V8SI "exp_8")
   (V16HI "exp_16")
   (V32QI "exp_32")])

;; This attribute represents bitmask needed for vec_merge using to ref low half
;; "const_<bitmask_lo>_operand".
(define_mode_attr bitmask_lo
  [(V4DF "exp_2")
   (V8SF "exp_4")
   (V4DI "exp_2")
   (V8SI "exp_4")
   (V16HI "exp_8")
   (V32QI "exp_16")])


;; This attribute is used to form an immediate operand constraint using
;; "const_<bitimm256>_operand".
(define_mode_attr bitimm256
  [(V32QI "uimm3")
   (V16HI  "uimm4")
   (V8SI  "uimm5")
   (V4DI  "uimm6")])


(define_mode_attr d2lasxfmt
  [(V8SI "q")
   (V16HI "d")
   (V32QI "w")])

(define_mode_attr d2lasxfmt_u
  [(V8SI "qu")
   (V16HI "du")
   (V32QI "wu")])

(define_mode_attr VD2MODE256
  [(V8SI "V4DI")
   (V16HI "V4DI")
   (V32QI "V8SI")])

(define_mode_attr lasxfmt_wd
  [(V4DI "d")
   (V8SI "w")
   (V16HI "w")
   (V32QI "w")])

;; Half modes of all LASX vector modes, in lower-case.
(define_mode_attr lasxhalf [(V32QI "v16qi")  (V16HI "v8hi")
             (V8SI "v4si")  (V4DI  "v2di")
             (V8SF  "v4sf") (V4DF  "v2df")])

(define_expand "vec_init<mode><unitmode>"
  [(match_operand:LASX 0 "register_operand")
   (match_operand:LASX 1 "")]
  "ISA_HAS_LASX"
{
  loongarch_expand_vector_init (operands[0], operands[1]);
  DONE;
})

(define_expand "vec_init<mode><lasxhalf>"
 [(match_operand:LASX 0 "register_operand")
  (match_operand:<VHMODE256_ALL> 1 "")]
  "ISA_HAS_LASX"
{
  loongarch_expand_vector_group_init (operands[0], operands[1]);
  DONE;
})

;; FIXME: Delete.
(define_insn "vec_pack_trunc_<mode>"
  [(set (match_operand:<VHSMODE256> 0 "register_operand" "=f")
	(vec_concat:<VHSMODE256>
	  (truncate:<VTRUNCMODE256>
	    (match_operand:ILASX_DWH 1 "register_operand" "f"))
	  (truncate:<VTRUNCMODE256>
	    (match_operand:ILASX_DWH 2 "register_operand" "f"))))]
  "ISA_HAS_LASX"
  "xvpickev.<hslasxfmt>\t%u0,%u2,%u1\n\txvpermi.d\t%u0,%u0,0xd8"
  [(set_attr "type" "simd_permute")
   (set_attr "mode" "<MODE>")
   (set_attr "length" "8")])

(define_expand "vec_unpacks_hi_v8sf"
  [(set (match_operand:V4DF 0 "register_operand" "=f")
	(float_extend:V4DF
	  (vec_select:V4SF
	    (match_operand:V8SF 1 "register_operand" "f")
	    (match_dup 2))))]
  "ISA_HAS_LASX"
{
  operands[2] = loongarch_lsx_vec_parallel_const_half (V8SFmode,
						       true/*high_p*/);
})

(define_expand "vec_unpacks_lo_v8sf"
  [(set (match_operand:V4DF 0 "register_operand" "=f")
	(float_extend:V4DF
	  (vec_select:V4SF
	    (match_operand:V8SF 1 "register_operand" "f")
	    (match_dup 2))))]
  "ISA_HAS_LASX"
{
  operands[2] = loongarch_lsx_vec_parallel_const_half (V8SFmode,
						       false/*high_p*/);
})

(define_expand "vec_unpacks_hi_<mode>"
  [(match_operand:<VDMODE256> 0 "register_operand")
   (match_operand:ILASX_WHB 1 "register_operand")]
  "ISA_HAS_LASX"
{
  loongarch_expand_vec_unpack (operands, false/*unsigned_p*/,
			       true/*high_p*/);
  DONE;
})

(define_expand "vec_unpacks_lo_<mode>"
  [(match_operand:<VDMODE256> 0 "register_operand")
   (match_operand:ILASX_WHB 1 "register_operand")]
  "ISA_HAS_LASX"
{
  loongarch_expand_vec_unpack (operands, false/*unsigned_p*/, false/*high_p*/);
  DONE;
})

(define_expand "vec_unpacku_hi_<mode>"
  [(match_operand:<VDMODE256> 0 "register_operand")
   (match_operand:ILASX_WHB 1 "register_operand")]
  "ISA_HAS_LASX"
{
  loongarch_expand_vec_unpack (operands, true/*unsigned_p*/, true/*high_p*/);
  DONE;
})

(define_expand "vec_unpacku_lo_<mode>"
  [(match_operand:<VDMODE256> 0 "register_operand")
   (match_operand:ILASX_WHB 1 "register_operand")]
  "ISA_HAS_LASX"
{
  loongarch_expand_vec_unpack (operands, true/*unsigned_p*/, false/*high_p*/);
  DONE;
})

(define_insn "lasx_xvinsgr2vr_<lasxfmt_f_wd>"
  [(set (match_operand:ILASX_DW 0 "register_operand" "=f")
	(vec_merge:ILASX_DW
	  (vec_duplicate:ILASX_DW
	    (match_operand:<UNITMODE> 1 "reg_or_0_operand" "rJ"))
	  (match_operand:ILASX_DW 2 "register_operand" "0")
	  (match_operand 3 "const_<bitmask256>_operand" "")))]
  "ISA_HAS_LASX"
{
  return "xvinsgr2vr.<lasxfmt>\t%u0,%z1,%y3";
}
  [(set_attr "type" "simd_insert")
   (set_attr "mode" "<MODE>")])

(define_insn "vec_concat<mode>"
  [(set (match_operand:LASX 0 "register_operand" "=f")
	(vec_concat:LASX
	  (match_operand:<VHMODE256_ALL> 1 "register_operand" "0")
	  (match_operand:<VHMODE256_ALL> 2 "register_operand" "f")))]
  "ISA_HAS_LASX"
{
  return "xvpermi.q\t%u0,%u2,0x02";
}
  [(set_attr "type" "simd_splat")
   (set_attr "mode" "<MODE>")])

;; xshuf.w
(define_insn "lasx_xvperm_<lasxfmt_f_wd>"
  [(set (match_operand:LASX_W 0 "register_operand" "=f")
	(unspec:LASX_W
	  [(match_operand:LASX_W 1 "nonimmediate_operand" "f")
	   (match_operand:V8SI 2 "register_operand" "f")]
	  UNSPEC_LASX_XVPERM_W))]
  "ISA_HAS_LASX"
  "xvperm.w\t%u0,%u1,%u2"
  [(set_attr "type" "simd_splat")
   (set_attr "mode" "<MODE>")])

;; xvpermi.d
(define_insn "lasx_xvpermi_d_<LASX:mode>"
  [(set (match_operand:LASX 0 "register_operand" "=f")
	  (unspec:LASX
	    [(match_operand:LASX 1 "register_operand" "f")
	     (match_operand:SI     2 "const_uimm8_operand")]
	    UNSPEC_LASX_XVPERMI_D))]
  "ISA_HAS_LASX"
  "xvpermi.d\t%u0,%u1,%2"
  [(set_attr "type" "simd_splat")
   (set_attr "mode" "<MODE>")])

(define_insn "lasx_xvpermi_d_<mode>_1"
  [(set (match_operand:LASX_D 0 "register_operand" "=f")
	(vec_select:LASX_D
	 (match_operand:LASX_D 1 "register_operand" "f")
	 (parallel [(match_operand 2 "const_0_to_3_operand")
		    (match_operand 3 "const_0_to_3_operand")
		    (match_operand 4 "const_0_to_3_operand")
		    (match_operand 5 "const_0_to_3_operand")])))]
  "ISA_HAS_LASX"
{
  int mask = 0;
  mask |= INTVAL (operands[2]) << 0;
  mask |= INTVAL (operands[3]) << 2;
  mask |= INTVAL (operands[4]) << 4;
  mask |= INTVAL (operands[5]) << 6;
  operands[2] = GEN_INT (mask);
  return "xvpermi.d\t%u0,%u1,%2";
}
  [(set_attr "type" "simd_splat")
   (set_attr "mode" "<MODE>")])

;; xvpermi.q
(define_insn "lasx_xvpermi_q_<LASX:mode>"
  [(set (match_operand:LASX 0 "register_operand" "=f")
	(unspec:LASX
	  [(match_operand:LASX 1 "register_operand" "0")
	   (match_operand:LASX 2 "register_operand" "f")
	   (match_operand     3 "const_uimm8_operand")]
	  UNSPEC_LASX_XVPERMI_Q))]
  "ISA_HAS_LASX"
{
  return "xvpermi.q\t%u0,%u2,%3";
}
  [(set_attr "type" "simd_splat")
   (set_attr "mode" "<MODE>")])

;; Only for loongarch_expand_vector_init in loongarch.cc.
;; Support a LSX-mode input op2.
(define_insn "lasx_vecinit_merge_<LASX:mode>"
  [(set (match_operand:LASX 0 "register_operand" "=f")
	(unspec:LASX
	  [(match_operand:LASX 1 "register_operand" "0")
	   (match_operand:<VHMODE256_ALL> 2 "register_operand" "f")
	   (match_operand     3 "const_uimm8_operand")]
	   UNSPEC_LASX_VECINIT_MERGE))]
  "ISA_HAS_LASX"
  "xvpermi.q\t%u0,%u2,%3"
  [(set_attr "type" "simd_splat")
   (set_attr "mode" "<MODE>")])

(define_insn "lasx_xvpickve2gr_d<u>"
  [(set (match_operand:DI 0 "register_operand" "=r")
	(any_extend:DI
	  (vec_select:DI
	    (match_operand:V4DI 1 "register_operand" "f")
	    (parallel [(match_operand 2 "const_0_to_3_operand" "")]))))]
  "ISA_HAS_LASX"
  "xvpickve2gr.d<u>\t%0,%u1,%2"
  [(set_attr "type" "simd_copy")
   (set_attr "mode" "V4DI")])

(define_expand "vec_set<mode>"
  [(match_operand:ILASX_DW 0 "register_operand")
   (match_operand:<UNITMODE> 1 "reg_or_0_operand")
   (match_operand 2 "const_<indeximm256>_operand")]
  "ISA_HAS_LASX"
{
  rtx index = GEN_INT (1 << INTVAL (operands[2]));
  emit_insn (gen_lasx_xvinsgr2vr_<lasxfmt_f_wd> (operands[0], operands[1],
                      operands[0], index));
  DONE;
})

;; Only for loongarch_expand_vector_init in loongarch.cc.
;; Simulate missing instructions xvinsgr2vr.b and xvinsgr2vr.h.
(define_expand "vec_set<mode>_internal"
  [(match_operand:ILASX_HB 0 "register_operand")
   (match_operand:<UNITMODE> 1 "reg_or_0_operand")
   (match_operand 2 "const_<indeximm256>_operand")]
  "ISA_HAS_LASX"
{
  rtx index = GEN_INT (1 << INTVAL (operands[2]));
  emit_insn (gen_lasx_xvinsgr2vr_<mode256_i_half>_internal
	     (operands[0], operands[1], operands[0], index));
  DONE;
})

(define_insn "lasx_xvinsgr2vr_<mode256_i_half>_internal"
  [(set (match_operand:ILASX_HB 0 "register_operand" "=f")
	(unspec:ILASX_HB [(match_operand:<UNITMODE> 1 "reg_or_0_operand" "rJ")
			  (match_operand:ILASX_HB 2 "register_operand" "0")
			  (match_operand 3 "const_<bitmask256>_operand" "")]
			 UNSPEC_LASX_VEC_SET_INTERNAL))]
  "ISA_HAS_LASX"
{
  return "vinsgr2vr.<lasxfmt>\t%w0,%z1,%y3";
}
  [(set_attr "type" "simd_insert")
   (set_attr "mode" "<MODE>")])

(define_expand "vec_set<mode>"
  [(match_operand:FLASX 0 "register_operand")
   (match_operand:<UNITMODE> 1 "reg_or_0_operand")
   (match_operand 2 "const_<indeximm256>_operand")]
  "ISA_HAS_LASX"
{
  rtx index = GEN_INT (1 << INTVAL (operands[2]));
  emit_insn (gen_lasx_xvinsve0_<lasxfmt_f>_scalar (operands[0], operands[1],
                      operands[0], index));
  DONE;
})

(define_expand "vec_extract<mode><unitmode>"
  [(match_operand:<UNITMODE> 0 "register_operand")
   (match_operand:LASX 1 "register_operand")
   (match_operand 2 "const_<indeximm256>_operand")]
  "ISA_HAS_LASX"
{
  loongarch_expand_vector_extract (operands[0], operands[1],
      INTVAL (operands[2]));
  DONE;
})

(define_insn_and_split "vec_extract<mode>_0"
  [(set (match_operand:<UNITMODE> 0 "register_operand" "=f")
        (vec_select:<UNITMODE>
          (match_operand:FLASX 1 "register_operand" "f")
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

(define_expand "vec_perm<mode>"
 [(match_operand:LASX 0 "register_operand")
  (match_operand:LASX 1 "register_operand")
  (match_operand:LASX 2 "register_operand")
  (match_operand:<VIMODE256> 3 "register_operand")]
  "ISA_HAS_LASX"
{
   loongarch_expand_vec_perm_1 (operands);
   DONE;
})

;; Same as vcond_
(define_expand "vcond_mask_<mode><mode256_i>"
  [(match_operand:LASX 0 "register_operand")
   (match_operand:LASX 1 "reg_or_m1_operand")
   (match_operand:LASX 2 "reg_or_0_operand")
   (match_operand:<VIMODE256> 3 "register_operand")]
  "ISA_HAS_LASX"
{
  loongarch_expand_vec_cond_mask_expr (<MODE>mode,
				       <VIMODE256>mode, operands);
  DONE;
})

(define_expand "lasx_xvrepli<mode>"
  [(match_operand:ILASX 0 "register_operand")
   (match_operand 1 "const_imm10_operand")]
  "ISA_HAS_LASX"
{
  if (<MODE>mode == V32QImode)
    operands[1] = GEN_INT (trunc_int_for_mode (INTVAL (operands[1]),
					       <UNITMODE>mode));
  emit_move_insn (operands[0],
  loongarch_gen_const_int_vector (<MODE>mode, INTVAL (operands[1])));
  DONE;
})

(define_expand "mov<mode>"
  [(set (match_operand:LASX 0)
	(match_operand:LASX 1))]
  "ISA_HAS_LASX"
{
  if (loongarch_legitimize_move (<MODE>mode, operands[0], operands[1]))
    DONE;
})


(define_expand "movmisalign<mode>"
  [(set (match_operand:LASX 0)
	(match_operand:LASX 1))]
  "ISA_HAS_LASX"
{
  if (loongarch_legitimize_move (<MODE>mode, operands[0], operands[1]))
    DONE;
})

;; 256-bit LASX modes can only exist in LASX registers or memory.
(define_insn "mov<mode>_lasx"
  [(set (match_operand:LASX 0 "nonimmediate_operand" "=f,f,R,*r,*f")
	(match_operand:LASX 1 "move_operand" "fYGYI,R,f,*f,*r"))]
  "ISA_HAS_LASX"
  { return loongarch_output_move (operands[0], operands[1]); }
  [(set_attr "type" "simd_move,simd_load,simd_store,simd_copy,simd_insert")
   (set_attr "mode" "<MODE>")
   (set_attr "length" "8,4,4,4,4")])


(define_split
  [(set (match_operand:LASX 0 "nonimmediate_operand")
	(match_operand:LASX 1 "move_operand"))]
  "reload_completed && ISA_HAS_LASX
   && loongarch_split_move_p (operands[0], operands[1])"
  [(const_int 0)]
{
  loongarch_split_move (operands[0], operands[1]);
  DONE;
})

;; LASX
(define_insn "add<mode>3"
  [(set (match_operand:ILASX 0 "register_operand" "=f,f,f")
	(plus:ILASX
	  (match_operand:ILASX 1 "register_operand" "f,f,f")
	  (match_operand:ILASX 2 "reg_or_vector_same_ximm5_operand" "f,Unv5,Uuv5")))]
  "ISA_HAS_LASX"
{
  switch (which_alternative)
    {
    case 0:
      return "xvadd.<lasxfmt>\t%u0,%u1,%u2";
    case 1:
      {
	HOST_WIDE_INT val = INTVAL (CONST_VECTOR_ELT (operands[2], 0));

	operands[2] = GEN_INT (-val);
	return "xvsubi.<lasxfmt_u>\t%u0,%u1,%d2";
      }
    case 2:
      return "xvaddi.<lasxfmt_u>\t%u0,%u1,%E2";
    default:
      gcc_unreachable ();
    }
}
  [(set_attr "alu_type" "simd_add")
   (set_attr "type" "simd_int_arith")
   (set_attr "mode" "<MODE>")])

(define_insn "sub<mode>3"
  [(set (match_operand:ILASX 0 "register_operand" "=f,f")
	(minus:ILASX
	  (match_operand:ILASX 1 "register_operand" "f,f")
	  (match_operand:ILASX 2 "reg_or_vector_same_uimm5_operand" "f,Uuv5")))]
  "ISA_HAS_LASX"
  "@
   xvsub.<lasxfmt>\t%u0,%u1,%u2
   xvsubi.<lasxfmt_u>\t%u0,%u1,%E2"
  [(set_attr "alu_type" "simd_add")
   (set_attr "type" "simd_int_arith")
   (set_attr "mode" "<MODE>")])

(define_insn "mul<mode>3"
  [(set (match_operand:ILASX 0 "register_operand" "=f")
	(mult:ILASX (match_operand:ILASX 1 "register_operand" "f")
		    (match_operand:ILASX 2 "register_operand" "f")))]
  "ISA_HAS_LASX"
  "xvmul.<lasxfmt>\t%u0,%u1,%u2"
  [(set_attr "type" "simd_mul")
   (set_attr "mode" "<MODE>")])

(define_insn "lasx_xvmadd_<lasxfmt>"
  [(set (match_operand:ILASX 0 "register_operand" "=f")
	(plus:ILASX (mult:ILASX (match_operand:ILASX 2 "register_operand" "f")
				(match_operand:ILASX 3 "register_operand" "f"))
		    (match_operand:ILASX 1 "register_operand" "0")))]
  "ISA_HAS_LASX"
  "xvmadd.<lasxfmt>\t%u0,%u2,%u3"
  [(set_attr "type" "simd_mul")
   (set_attr "mode" "<MODE>")])



(define_insn "lasx_xvmsub_<lasxfmt>"
  [(set (match_operand:ILASX 0 "register_operand" "=f")
	(minus:ILASX (match_operand:ILASX 1 "register_operand" "0")
		     (mult:ILASX (match_operand:ILASX 2 "register_operand" "f")
				 (match_operand:ILASX 3 "register_operand" "f"))))]
  "ISA_HAS_LASX"
  "xvmsub.<lasxfmt>\t%u0,%u2,%u3"
  [(set_attr "type" "simd_mul")
   (set_attr "mode" "<MODE>")])

(define_insn "div<mode>3"
  [(set (match_operand:ILASX 0 "register_operand" "=f")
	(div:ILASX (match_operand:ILASX 1 "register_operand" "f")
		   (match_operand:ILASX 2 "register_operand" "f")))]
  "ISA_HAS_LASX"
{
  return loongarch_lsx_output_division ("xvdiv.<lasxfmt>\t%u0,%u1,%u2",
					operands);
}
  [(set_attr "type" "simd_div")
   (set_attr "mode" "<MODE>")])

(define_insn "udiv<mode>3"
  [(set (match_operand:ILASX 0 "register_operand" "=f")
	(udiv:ILASX (match_operand:ILASX 1 "register_operand" "f")
		    (match_operand:ILASX 2 "register_operand" "f")))]
  "ISA_HAS_LASX"
{
  return loongarch_lsx_output_division ("xvdiv.<lasxfmt_u>\t%u0,%u1,%u2",
					operands);
}
  [(set_attr "type" "simd_div")
   (set_attr "mode" "<MODE>")])

(define_insn "mod<mode>3"
  [(set (match_operand:ILASX 0 "register_operand" "=f")
	(mod:ILASX (match_operand:ILASX 1 "register_operand" "f")
		   (match_operand:ILASX 2 "register_operand" "f")))]
  "ISA_HAS_LASX"
{
  return loongarch_lsx_output_division ("xvmod.<lasxfmt>\t%u0,%u1,%u2",
					operands);
}
  [(set_attr "type" "simd_div")
   (set_attr "mode" "<MODE>")])

(define_insn "umod<mode>3"
  [(set (match_operand:ILASX 0 "register_operand" "=f")
	(umod:ILASX (match_operand:ILASX 1 "register_operand" "f")
		    (match_operand:ILASX 2 "register_operand" "f")))]
  "ISA_HAS_LASX"
{
  return loongarch_lsx_output_division ("xvmod.<lasxfmt_u>\t%u0,%u1,%u2",
					operands);
}
  [(set_attr "type" "simd_div")
   (set_attr "mode" "<MODE>")])

(define_insn "xor<mode>3"
  [(set (match_operand:LASX 0 "register_operand" "=f,f,f")
	(xor:LASX
	  (match_operand:LASX 1 "register_operand" "f,f,f")
	  (match_operand:LASX 2 "reg_or_vector_same_val_operand" "f,YC,Urv8")))]
  "ISA_HAS_LASX"
  "@
   xvxor.v\t%u0,%u1,%u2
   xvbitrevi.%v0\t%u0,%u1,%V2
   xvxori.b\t%u0,%u1,%B2"
  [(set_attr "type" "simd_logic,simd_bit,simd_logic")
   (set_attr "mode" "<MODE>")])

(define_insn "ior<mode>3"
  [(set (match_operand:LASX 0 "register_operand" "=f,f,f")
	(ior:LASX
	  (match_operand:LASX 1 "register_operand" "f,f,f")
	  (match_operand:LASX 2 "reg_or_vector_same_val_operand" "f,YC,Urv8")))]
  "ISA_HAS_LASX"
  "@
   xvor.v\t%u0,%u1,%u2
   xvbitseti.%v0\t%u0,%u1,%V2
   xvori.b\t%u0,%u1,%B2"
  [(set_attr "type" "simd_logic,simd_bit,simd_logic")
   (set_attr "mode" "<MODE>")])

(define_insn "and<mode>3"
  [(set (match_operand:LASX 0 "register_operand" "=f,f,f")
	(and:LASX
	  (match_operand:LASX 1 "register_operand" "f,f,f")
	  (match_operand:LASX 2 "reg_or_vector_same_val_operand" "f,YZ,Urv8")))]
  "ISA_HAS_LASX"
{
  switch (which_alternative)
    {
    case 0:
      return "xvand.v\t%u0,%u1,%u2";
    case 1:
      {
	rtx elt0 = CONST_VECTOR_ELT (operands[2], 0);
	unsigned HOST_WIDE_INT val = ~UINTVAL (elt0);
	operands[2] = loongarch_gen_const_int_vector (<MODE>mode, val & (-val));
	return "xvbitclri.%v0\t%u0,%u1,%V2";
      }
    case 2:
      return "xvandi.b\t%u0,%u1,%B2";
    default:
      gcc_unreachable ();
    }
}
  [(set_attr "type" "simd_logic,simd_bit,simd_logic")
   (set_attr "mode" "<MODE>")])

(define_insn "one_cmpl<mode>2"
  [(set (match_operand:ILASX 0 "register_operand" "=f")
	(not:ILASX (match_operand:ILASX 1 "register_operand" "f")))]
  "ISA_HAS_LASX"
  "xvnor.v\t%u0,%u1,%u1"
  [(set_attr "type" "simd_logic")
   (set_attr "mode" "V32QI")])

;; LASX
(define_insn "vlshr<mode>3"
  [(set (match_operand:ILASX 0 "register_operand" "=f,f")
	(lshiftrt:ILASX
	  (match_operand:ILASX 1 "register_operand" "f,f")
	  (match_operand:ILASX 2 "reg_or_vector_same_uimm6_operand" "f,Uuv6")))]
  "ISA_HAS_LASX"
  "@
   xvsrl.<lasxfmt>\t%u0,%u1,%u2
   xvsrli.<lasxfmt>\t%u0,%u1,%E2"
  [(set_attr "type" "simd_shift")
   (set_attr "mode" "<MODE>")])

;; LASX ">>"
(define_insn "vashr<mode>3"
  [(set (match_operand:ILASX 0 "register_operand" "=f,f")
	(ashiftrt:ILASX
	  (match_operand:ILASX 1 "register_operand" "f,f")
	  (match_operand:ILASX 2 "reg_or_vector_same_uimm6_operand" "f,Uuv6")))]
  "ISA_HAS_LASX"
  "@
   xvsra.<lasxfmt>\t%u0,%u1,%u2
   xvsrai.<lasxfmt>\t%u0,%u1,%E2"
  [(set_attr "type" "simd_shift")
   (set_attr "mode" "<MODE>")])

;; LASX "<<"
(define_insn "vashl<mode>3"
  [(set (match_operand:ILASX 0 "register_operand" "=f,f")
	(ashift:ILASX
	  (match_operand:ILASX 1 "register_operand" "f,f")
	  (match_operand:ILASX 2 "reg_or_vector_same_uimm6_operand" "f,Uuv6")))]
  "ISA_HAS_LASX"
  "@
   xvsll.<lasxfmt>\t%u0,%u1,%u2
   xvslli.<lasxfmt>\t%u0,%u1,%E2"
  [(set_attr "type" "simd_shift")
   (set_attr "mode" "<MODE>")])


(define_insn "add<mode>3"
  [(set (match_operand:FLASX 0 "register_operand" "=f")
	(plus:FLASX (match_operand:FLASX 1 "register_operand" "f")
		    (match_operand:FLASX 2 "register_operand" "f")))]
  "ISA_HAS_LASX"
  "xvfadd.<flasxfmt>\t%u0,%u1,%u2"
  [(set_attr "type" "simd_fadd")
   (set_attr "mode" "<MODE>")])

(define_insn "sub<mode>3"
  [(set (match_operand:FLASX 0 "register_operand" "=f")
	(minus:FLASX (match_operand:FLASX 1 "register_operand" "f")
		     (match_operand:FLASX 2 "register_operand" "f")))]
  "ISA_HAS_LASX"
  "xvfsub.<flasxfmt>\t%u0,%u1,%u2"
  [(set_attr "type" "simd_fadd")
   (set_attr "mode" "<MODE>")])

(define_insn "mul<mode>3"
  [(set (match_operand:FLASX 0 "register_operand" "=f")
	(mult:FLASX (match_operand:FLASX 1 "register_operand" "f")
		    (match_operand:FLASX 2 "register_operand" "f")))]
  "ISA_HAS_LASX"
  "xvfmul.<flasxfmt>\t%u0,%u1,%u2"
  [(set_attr "type" "simd_fmul")
   (set_attr "mode" "<MODE>")])

(define_expand "div<mode>3"
  [(set (match_operand:FLASX 0 "register_operand")
    (div:FLASX (match_operand:FLASX 1 "reg_or_vecotr_1_operand")
	       (match_operand:FLASX 2 "register_operand")))]
  "ISA_HAS_LASX"
{
  if (<MODE>mode == V8SFmode
    && TARGET_RECIP_VEC_DIV
    && optimize_insn_for_speed_p ()
    && flag_finite_math_only && !flag_trapping_math
    && flag_unsafe_math_optimizations)
  {
    loongarch_emit_swdivsf (operands[0], operands[1],
	operands[2], V8SFmode);
    DONE;
  }
})

(define_insn "*div<mode>3"
  [(set (match_operand:FLASX 0 "register_operand" "=f")
	(div:FLASX (match_operand:FLASX 1 "register_operand" "f")
		   (match_operand:FLASX 2 "register_operand" "f")))]
  "ISA_HAS_LASX"
  "xvfdiv.<flasxfmt>\t%u0,%u1,%u2"
  [(set_attr "type" "simd_fdiv")
   (set_attr "mode" "<MODE>")])

(define_insn "fma<mode>4"
  [(set (match_operand:FLASX 0 "register_operand" "=f")
	(fma:FLASX (match_operand:FLASX 1 "register_operand" "f")
		   (match_operand:FLASX 2 "register_operand" "f")
		   (match_operand:FLASX 3 "register_operand" "f")))]
  "ISA_HAS_LASX"
  "xvfmadd.<flasxfmt>\t%u0,%u1,%u2,%u3"
  [(set_attr "type" "simd_fmadd")
   (set_attr "mode" "<MODE>")])

(define_insn "fnma<mode>4"
  [(set (match_operand:FLASX 0 "register_operand" "=f")
	(fma:FLASX (neg:FLASX (match_operand:FLASX 1 "register_operand" "f"))
		   (match_operand:FLASX 2 "register_operand" "f")
		   (match_operand:FLASX 3 "register_operand" "0")))]
  "ISA_HAS_LASX"
  "xvfnmsub.<flasxfmt>\t%u0,%u1,%u2,%u0"
  [(set_attr "type" "simd_fmadd")
   (set_attr "mode" "<MODE>")])

(define_expand "sqrt<mode>2"
  [(set (match_operand:FLASX 0 "register_operand")
    (sqrt:FLASX (match_operand:FLASX 1 "register_operand")))]
  "ISA_HAS_LASX"
{
  if (<MODE>mode == V8SFmode
      && TARGET_RECIP_VEC_SQRT
      && flag_unsafe_math_optimizations
      && optimize_insn_for_speed_p ()
      && flag_finite_math_only && !flag_trapping_math)
    {
      loongarch_emit_swrsqrtsf (operands[0], operands[1], V8SFmode, 0);
      DONE;
    }
})

(define_insn "*sqrt<mode>2"
  [(set (match_operand:FLASX 0 "register_operand" "=f")
	(sqrt:FLASX (match_operand:FLASX 1 "register_operand" "f")))]
  "ISA_HAS_LASX"
  "xvfsqrt.<flasxfmt>\t%u0,%u1"
  [(set_attr "type" "simd_fdiv")
   (set_attr "mode" "<MODE>")])

(define_insn "lasx_xvadda_<lasxfmt>"
  [(set (match_operand:ILASX 0 "register_operand" "=f")
	(plus:ILASX (abs:ILASX (match_operand:ILASX 1 "register_operand" "f"))
		    (abs:ILASX (match_operand:ILASX 2 "register_operand" "f"))))]
  "ISA_HAS_LASX"
  "xvadda.<lasxfmt>\t%u0,%u1,%u2"
  [(set_attr "type" "simd_int_arith")
   (set_attr "mode" "<MODE>")])

(define_insn "ssadd<mode>3"
  [(set (match_operand:ILASX 0 "register_operand" "=f")
	(ss_plus:ILASX (match_operand:ILASX 1 "register_operand" "f")
		       (match_operand:ILASX 2 "register_operand" "f")))]
  "ISA_HAS_LASX"
  "xvsadd.<lasxfmt>\t%u0,%u1,%u2"
  [(set_attr "type" "simd_int_arith")
   (set_attr "mode" "<MODE>")])

(define_insn "usadd<mode>3"
  [(set (match_operand:ILASX 0 "register_operand" "=f")
	(us_plus:ILASX (match_operand:ILASX 1 "register_operand" "f")
		       (match_operand:ILASX 2 "register_operand" "f")))]
  "ISA_HAS_LASX"
  "xvsadd.<lasxfmt_u>\t%u0,%u1,%u2"
  [(set_attr "type" "simd_int_arith")
   (set_attr "mode" "<MODE>")])

(define_insn "lasx_xvabsd_s_<lasxfmt>"
  [(set (match_operand:ILASX 0 "register_operand" "=f")
	(unspec:ILASX [(match_operand:ILASX 1 "register_operand" "f")
		       (match_operand:ILASX 2 "register_operand" "f")]
		      UNSPEC_LASX_XVABSD_S))]
  "ISA_HAS_LASX"
  "xvabsd.<lasxfmt>\t%u0,%u1,%u2"
  [(set_attr "type" "simd_int_arith")
   (set_attr "mode" "<MODE>")])

(define_insn "lasx_xvabsd_u_<lasxfmt_u>"
  [(set (match_operand:ILASX 0 "register_operand" "=f")
	(unspec:ILASX [(match_operand:ILASX 1 "register_operand" "f")
		       (match_operand:ILASX 2 "register_operand" "f")]
		      UNSPEC_LASX_XVABSD_U))]
  "ISA_HAS_LASX"
  "xvabsd.<lasxfmt_u>\t%u0,%u1,%u2"
  [(set_attr "type" "simd_int_arith")
   (set_attr "mode" "<MODE>")])

(define_insn "lasx_xvavg_s_<lasxfmt>"
  [(set (match_operand:ILASX 0 "register_operand" "=f")
	(unspec:ILASX [(match_operand:ILASX 1 "register_operand" "f")
		       (match_operand:ILASX 2 "register_operand" "f")]
		      UNSPEC_LASX_XVAVG_S))]
  "ISA_HAS_LASX"
  "xvavg.<lasxfmt>\t%u0,%u1,%u2"
  [(set_attr "type" "simd_int_arith")
   (set_attr "mode" "<MODE>")])

(define_insn "lasx_xvavg_u_<lasxfmt_u>"
  [(set (match_operand:ILASX 0 "register_operand" "=f")
	(unspec:ILASX [(match_operand:ILASX 1 "register_operand" "f")
		       (match_operand:ILASX 2 "register_operand" "f")]
		      UNSPEC_LASX_XVAVG_U))]
  "ISA_HAS_LASX"
  "xvavg.<lasxfmt_u>\t%u0,%u1,%u2"
  [(set_attr "type" "simd_int_arith")
   (set_attr "mode" "<MODE>")])

(define_insn "lasx_xvavgr_s_<lasxfmt>"
  [(set (match_operand:ILASX 0 "register_operand" "=f")
	(unspec:ILASX [(match_operand:ILASX 1 "register_operand" "f")
		       (match_operand:ILASX 2 "register_operand" "f")]
		      UNSPEC_LASX_XVAVGR_S))]
  "ISA_HAS_LASX"
  "xvavgr.<lasxfmt>\t%u0,%u1,%u2"
  [(set_attr "type" "simd_int_arith")
   (set_attr "mode" "<MODE>")])

(define_insn "lasx_xvavgr_u_<lasxfmt_u>"
  [(set (match_operand:ILASX 0 "register_operand" "=f")
	(unspec:ILASX [(match_operand:ILASX 1 "register_operand" "f")
		       (match_operand:ILASX 2 "register_operand" "f")]
		      UNSPEC_LASX_XVAVGR_U))]
  "ISA_HAS_LASX"
  "xvavgr.<lasxfmt_u>\t%u0,%u1,%u2"
  [(set_attr "type" "simd_int_arith")
   (set_attr "mode" "<MODE>")])

(define_insn "lasx_xvbitclr_<lasxfmt>"
  [(set (match_operand:ILASX 0 "register_operand" "=f")
	(unspec:ILASX [(match_operand:ILASX 1 "register_operand" "f")
		       (match_operand:ILASX 2 "register_operand" "f")]
		      UNSPEC_LASX_XVBITCLR))]
  "ISA_HAS_LASX"
  "xvbitclr.<lasxfmt>\t%u0,%u1,%u2"
  [(set_attr "type" "simd_bit")
   (set_attr "mode" "<MODE>")])

(define_insn "lasx_xvbitclri_<lasxfmt>"
  [(set (match_operand:ILASX 0 "register_operand" "=f")
	(unspec:ILASX [(match_operand:ILASX 1 "register_operand" "f")
		       (match_operand 2 "const_<bitimm256>_operand" "")]
		      UNSPEC_LASX_XVBITCLRI))]
  "ISA_HAS_LASX"
  "xvbitclri.<lasxfmt>\t%u0,%u1,%2"
  [(set_attr "type" "simd_bit")
   (set_attr "mode" "<MODE>")])

(define_insn "lasx_xvbitrev_<lasxfmt>"
  [(set (match_operand:ILASX 0 "register_operand" "=f")
	(unspec:ILASX [(match_operand:ILASX 1 "register_operand" "f")
		       (match_operand:ILASX 2 "register_operand" "f")]
		      UNSPEC_LASX_XVBITREV))]
  "ISA_HAS_LASX"
  "xvbitrev.<lasxfmt>\t%u0,%u1,%u2"
  [(set_attr "type" "simd_bit")
   (set_attr "mode" "<MODE>")])

(define_insn "lasx_xvbitrevi_<lasxfmt>"
  [(set (match_operand:ILASX 0 "register_operand" "=f")
	(unspec:ILASX [(match_operand:ILASX 1 "register_operand" "f")
		       (match_operand 2 "const_<bitimm256>_operand" "")]
		     UNSPEC_LASX_XVBITREVI))]
  "ISA_HAS_LASX"
  "xvbitrevi.<lasxfmt>\t%u0,%u1,%2"
  [(set_attr "type" "simd_bit")
   (set_attr "mode" "<MODE>")])

(define_insn "lasx_xvbitsel_<lasxfmt_f>"
  [(set (match_operand:LASX 0 "register_operand" "=f")
	(ior:LASX (and:LASX (not:LASX
			      (match_operand:LASX 3 "register_operand" "f"))
			      (match_operand:LASX 1 "register_operand" "f"))
		  (and:LASX (match_dup 3)
			    (match_operand:LASX 2 "register_operand" "f"))))]
  "ISA_HAS_LASX"
  "xvbitsel.v\t%u0,%u1,%u2,%u3"
  [(set_attr "type" "simd_bitmov")
   (set_attr "mode" "<MODE>")])

(define_insn "lasx_xvbitseli_b"
  [(set (match_operand:V32QI 0 "register_operand" "=f")
	(ior:V32QI (and:V32QI (not:V32QI
				(match_operand:V32QI 1 "register_operand" "0"))
			      (match_operand:V32QI 2 "register_operand" "f"))
		   (and:V32QI (match_dup 1)
			      (match_operand:V32QI 3 "const_vector_same_val_operand" "Urv8"))))]
  "ISA_HAS_LASX"
  "xvbitseli.b\t%u0,%u2,%B3"
  [(set_attr "type" "simd_bitmov")
   (set_attr "mode" "V32QI")])

(define_insn "lasx_xvbitset_<lasxfmt>"
  [(set (match_operand:ILASX 0 "register_operand" "=f")
	(unspec:ILASX [(match_operand:ILASX 1 "register_operand" "f")
		       (match_operand:ILASX 2 "register_operand" "f")]
		      UNSPEC_LASX_XVBITSET))]
  "ISA_HAS_LASX"
  "xvbitset.<lasxfmt>\t%u0,%u1,%u2"
  [(set_attr "type" "simd_bit")
   (set_attr "mode" "<MODE>")])

(define_insn "lasx_xvbitseti_<lasxfmt>"
  [(set (match_operand:ILASX 0 "register_operand" "=f")
	(unspec:ILASX [(match_operand:ILASX 1 "register_operand" "f")
		       (match_operand 2 "const_<bitimm256>_operand" "")]
		      UNSPEC_LASX_XVBITSETI))]
  "ISA_HAS_LASX"
  "xvbitseti.<lasxfmt>\t%u0,%u1,%2"
  [(set_attr "type" "simd_bit")
   (set_attr "mode" "<MODE>")])

(define_insn "lasx_xvs<ICC:icc>_<ILASX:lasxfmt><cmpi_1>"
  [(set (match_operand:ILASX 0 "register_operand" "=f,f")
	(ICC:ILASX
	  (match_operand:ILASX 1 "register_operand" "f,f")
	  (match_operand:ILASX 2 "reg_or_vector_same_<ICC:cmpi>imm5_operand" "f,U<ICC:cmpi>v5")))]
  "ISA_HAS_LASX"
  "@
   xvs<ICC:icc>.<ILASX:lasxfmt><cmpi_1>\t%u0,%u1,%u2
   xvs<ICC:icci>.<ILASX:lasxfmt><cmpi_1>\t%u0,%u1,%E2"
  [(set_attr "type" "simd_int_arith")
   (set_attr "mode" "<MODE>")])

(define_expand "vec_cmp<mode><mode256_i>"
  [(set (match_operand:<VIMODE256> 0 "register_operand")
	(match_operator 1 ""
	  [(match_operand:LASX 2 "register_operand")
	   (match_operand:LASX 3 "register_operand")]))]
  "ISA_HAS_LASX"
{
  loongarch_expand_vec_cmp (operands);
  DONE;
})

(define_expand "vec_cmpu<ILASX:mode><mode256_i>"
  [(set (match_operand:<VIMODE256> 0 "register_operand")
	(match_operator 1 ""
	  [(match_operand:ILASX 2 "register_operand")
	   (match_operand:ILASX 3 "register_operand")]))]
  "ISA_HAS_LASX"
{
  loongarch_expand_vec_cmp (operands);
  DONE;
})

(define_insn "lasx_xvfclass_<flasxfmt>"
  [(set (match_operand:<VIMODE256> 0 "register_operand" "=f")
	(unspec:<VIMODE256> [(match_operand:FLASX 1 "register_operand" "f")]
			    UNSPEC_LASX_XVFCLASS))]
  "ISA_HAS_LASX"
  "xvfclass.<flasxfmt>\t%u0,%u1"
  [(set_attr "type" "simd_fclass")
   (set_attr "mode" "<MODE>")])

(define_mode_attr fint256
  [(V8SF "v8si")
   (V4DF "v4di")])

(define_mode_attr FINTCNV256
  [(V8SF "I2S")
   (V4DF "I2D")])

(define_mode_attr FINTCNV256_2
  [(V8SF "S2I")
   (V4DF "D2I")])

(define_insn "float<fint256><FLASX:mode>2"
  [(set (match_operand:FLASX 0 "register_operand" "=f")
	(float:FLASX (match_operand:<VIMODE256> 1 "register_operand" "f")))]
  "ISA_HAS_LASX"
  "xvffint.<flasxfmt>.<ilasxfmt>\t%u0,%u1"
  [(set_attr "type" "simd_fcvt")
   (set_attr "cnv_mode" "<FINTCNV256>")
   (set_attr "mode" "<MODE>")])

(define_insn "floatuns<fint256><FLASX:mode>2"
  [(set (match_operand:FLASX 0 "register_operand" "=f")
	(unsigned_float:FLASX
	  (match_operand:<VIMODE256> 1 "register_operand" "f")))]
  "ISA_HAS_LASX"
  "xvffint.<flasxfmt>.<ilasxfmt_u>\t%u0,%u1"
  [(set_attr "type" "simd_fcvt")
   (set_attr "cnv_mode" "<FINTCNV256>")
   (set_attr "mode" "<MODE>")])

(define_mode_attr FFQ256
  [(V4SF "V16HI")
   (V2DF "V8SI")])

(define_insn "lasx_xvreplgr2vr_<lasxfmt_f>"
  [(set (match_operand:ILASX 0 "register_operand" "=f,f")
	(vec_duplicate:ILASX
	  (match_operand:<UNITMODE> 1 "reg_or_0_operand" "r,J")))]
  "ISA_HAS_LASX"
{
  if (which_alternative == 1)
    return "xvrepli.b\t%u0,0";

  return "xvreplgr2vr.<lasxfmt>\t%u0,%z1";
}
  [(set_attr "type" "simd_fill")
   (set_attr "mode" "<MODE>")
   (set_attr "length" "8")])

(define_insn "logb<mode>2"
  [(set (match_operand:FLASX 0 "register_operand" "=f")
	(unspec:FLASX [(match_operand:FLASX 1 "register_operand" "f")]
		      UNSPEC_LASX_XVFLOGB))]
  "ISA_HAS_LASX"
  "xvflogb.<flasxfmt>\t%u0,%u1"
  [(set_attr "type" "simd_flog2")
   (set_attr "mode" "<MODE>")])

;; Only for loongarch_expand_vector_init in loongarch.cc.
;; Merge two scalar floating-point op1 and op2 into a LASX op0.
(define_insn "lasx_xvilvl_<lasxfmt_f>_internal"
  [(set (match_operand:FLASX 0 "register_operand" "=f")
	(unspec:FLASX [(match_operand:<UNITMODE> 1 "register_operand" "f")
		       (match_operand:<UNITMODE> 2 "register_operand" "f")]
		      UNSPEC_LASX_XVILVL_INTERNAL))]
  "ISA_HAS_LASX"
  "xvilvl.<lasxfmt>\t%u0,%u2,%u1"
  [(set_attr "type" "simd_permute")
   (set_attr "mode" "<MODE>")])

(define_insn "smax<mode>3"
  [(set (match_operand:FLASX 0 "register_operand" "=f")
	(smax:FLASX (match_operand:FLASX 1 "register_operand" "f")
		    (match_operand:FLASX 2 "register_operand" "f")))]
  "ISA_HAS_LASX"
  "xvfmax.<flasxfmt>\t%u0,%u1,%u2"
  [(set_attr "type" "simd_fminmax")
   (set_attr "mode" "<MODE>")])

(define_insn "lasx_xvfmaxa_<flasxfmt>"
  [(set (match_operand:FLASX 0 "register_operand" "=f")
	(if_then_else:FLASX
	   (gt (abs:FLASX (match_operand:FLASX 1 "register_operand" "f"))
	       (abs:FLASX (match_operand:FLASX 2 "register_operand" "f")))
	   (match_dup 1)
	   (match_dup 2)))]
  "ISA_HAS_LASX"
  "xvfmaxa.<flasxfmt>\t%u0,%u1,%u2"
  [(set_attr "type" "simd_fminmax")
   (set_attr "mode" "<MODE>")])

(define_insn "smin<mode>3"
  [(set (match_operand:FLASX 0 "register_operand" "=f")
	(smin:FLASX (match_operand:FLASX 1 "register_operand" "f")
		    (match_operand:FLASX 2 "register_operand" "f")))]
  "ISA_HAS_LASX"
  "xvfmin.<flasxfmt>\t%u0,%u1,%u2"
  [(set_attr "type" "simd_fminmax")
   (set_attr "mode" "<MODE>")])

(define_insn "lasx_xvfmina_<flasxfmt>"
  [(set (match_operand:FLASX 0 "register_operand" "=f")
	(if_then_else:FLASX
	   (lt (abs:FLASX (match_operand:FLASX 1 "register_operand" "f"))
	       (abs:FLASX (match_operand:FLASX 2 "register_operand" "f")))
	   (match_dup 1)
	   (match_dup 2)))]
  "ISA_HAS_LASX"
  "xvfmina.<flasxfmt>\t%u0,%u1,%u2"
  [(set_attr "type" "simd_fminmax")
   (set_attr "mode" "<MODE>")])

(define_insn "recip<mode>3"
  [(set (match_operand:FLASX 0 "register_operand" "=f")
       (div:FLASX (match_operand:FLASX 1 "const_vector_1_operand" "")
		  (match_operand:FLASX 2 "register_operand" "f")))]
  "ISA_HAS_LASX"
  "xvfrecip.<flasxfmt>\t%u0,%u2"
  [(set_attr "type" "simd_fdiv")
   (set_attr "mode" "<MODE>")])

;; Approximate Reciprocal Instructions.

(define_insn "lasx_xvfrecipe_<flasxfmt>"
  [(set (match_operand:FLASX 0 "register_operand" "=f")
    (unspec:FLASX [(match_operand:FLASX 1 "register_operand" "f")]
		  UNSPEC_LASX_XVFRECIPE))]
  "ISA_HAS_LASX && ISA_HAS_FRECIPE"
  "xvfrecipe.<flasxfmt>\t%u0,%u1"
  [(set_attr "type" "simd_fdiv")
   (set_attr "mode" "<MODE>")])

(define_expand "rsqrt<mode>2"
  [(set (match_operand:FLASX 0 "register_operand" "=f")
    (unspec:FLASX [(match_operand:FLASX 1 "register_operand" "f")]
	     UNSPEC_LASX_XVFRSQRT))]
  "ISA_HAS_LASX"
 {
   if (<MODE>mode == V8SFmode && TARGET_RECIP_VEC_RSQRT)
     {
       loongarch_emit_swrsqrtsf (operands[0], operands[1], V8SFmode, 1);
       DONE;
     }
})

(define_insn "*rsqrt<mode>2"
  [(set (match_operand:FLASX 0 "register_operand" "=f")
    (unspec:FLASX [(match_operand:FLASX 1 "register_operand" "f")]
		  UNSPEC_LASX_XVFRSQRT))]
  "ISA_HAS_LASX"
  "xvfrsqrt.<flasxfmt>\t%u0,%u1"
  [(set_attr "type" "simd_fdiv")
   (set_attr "mode" "<MODE>")])

;; Approximate Reciprocal Square Root Instructions.

(define_insn "lasx_xvfrsqrte_<flasxfmt>"
  [(set (match_operand:FLASX 0 "register_operand" "=f")
    (unspec:FLASX [(match_operand:FLASX 1 "register_operand" "f")]
		  UNSPEC_LASX_XVFRSQRTE))]
  "ISA_HAS_LASX && ISA_HAS_FRECIPE"
  "xvfrsqrte.<flasxfmt>\t%u0,%u1"
  [(set_attr "type" "simd_fdiv")
   (set_attr "mode" "<MODE>")])

(define_insn "lasx_xvftint_u_<ilasxfmt_u>_<flasxfmt>"
  [(set (match_operand:<VIMODE256> 0 "register_operand" "=f")
	(unspec:<VIMODE256> [(match_operand:FLASX 1 "register_operand" "f")]
			    UNSPEC_LASX_XVFTINT_U))]
  "ISA_HAS_LASX"
  "xvftint.<ilasxfmt_u>.<flasxfmt>\t%u0,%u1"
  [(set_attr "type" "simd_fcvt")
   (set_attr "cnv_mode" "<FINTCNV256_2>")
   (set_attr "mode" "<MODE>")])

(define_insn "fixuns_trunc<FLASX:mode><mode256_i>2"
  [(set (match_operand:<VIMODE256> 0 "register_operand" "=f")
	(unsigned_fix:<VIMODE256> (match_operand:FLASX 1 "register_operand" "f")))]
  "ISA_HAS_LASX"
  "xvftintrz.<ilasxfmt_u>.<flasxfmt>\t%u0,%u1"
  [(set_attr "type" "simd_fcvt")
   (set_attr "cnv_mode" "<FINTCNV256_2>")
   (set_attr "mode" "<MODE>")])

(define_insn "lasx_xvh<optab>w_h<u>_b<u>"
  [(set (match_operand:V16HI 0 "register_operand" "=f")
	(addsub:V16HI
	  (any_extend:V16HI
	    (vec_select:V16QI
	      (match_operand:V32QI 1 "register_operand" "f")
	      (parallel [(const_int 1) (const_int 3)
			 (const_int 5) (const_int 7)
			 (const_int 9) (const_int 11)
			 (const_int 13) (const_int 15)
			 (const_int 17) (const_int 19)
			 (const_int 21) (const_int 23)
			 (const_int 25) (const_int 27)
			 (const_int 29) (const_int 31)])))
	  (any_extend:V16HI
	    (vec_select:V16QI
	      (match_operand:V32QI 2 "register_operand" "f")
	      (parallel [(const_int 0) (const_int 2)
			 (const_int 4) (const_int 6)
			 (const_int 8) (const_int 10)
			 (const_int 12) (const_int 14)
			 (const_int 16) (const_int 18)
			 (const_int 20) (const_int 22)
			 (const_int 24) (const_int 26)
			 (const_int 28) (const_int 30)])))))]
  "ISA_HAS_LASX"
  "xvh<optab>w.h<u>.b<u>\t%u0,%u1,%u2"
  [(set_attr "type" "simd_int_arith")
   (set_attr "mode" "V16HI")])

(define_insn "lasx_xvh<optab>w_w<u>_h<u>"
  [(set (match_operand:V8SI 0 "register_operand" "=f")
	(addsub:V8SI
	  (any_extend:V8SI
	    (vec_select:V8HI
	      (match_operand:V16HI 1 "register_operand" "f")
	      (parallel [(const_int 1) (const_int 3)
			 (const_int 5) (const_int 7)
			 (const_int 9) (const_int 11)
			 (const_int 13) (const_int 15)])))
	  (any_extend:V8SI
	    (vec_select:V8HI
	      (match_operand:V16HI 2 "register_operand" "f")
	      (parallel [(const_int 0) (const_int 2)
			 (const_int 4) (const_int 6)
			 (const_int 8) (const_int 10)
			 (const_int 12) (const_int 14)])))))]
  "ISA_HAS_LASX"
  "xvh<optab>w.w<u>.h<u>\t%u0,%u1,%u2"
  [(set_attr "type" "simd_int_arith")
   (set_attr "mode" "V8SI")])

(define_insn "lasx_xvh<optab>w_d<u>_w<u>"
  [(set (match_operand:V4DI 0 "register_operand" "=f")
	(addsub:V4DI
	  (any_extend:V4DI
	    (vec_select:V4SI
	      (match_operand:V8SI 1 "register_operand" "f")
	      (parallel [(const_int 1) (const_int 3)
			 (const_int 5) (const_int 7)])))
	  (any_extend:V4DI
	    (vec_select:V4SI
	      (match_operand:V8SI 2 "register_operand" "f")
	      (parallel [(const_int 0) (const_int 2)
			 (const_int 4) (const_int 6)])))))]
  "ISA_HAS_LASX"
  "xvh<optab>w.d<u>.w<u>\t%u0,%u1,%u2"
  [(set_attr "type" "simd_int_arith")
   (set_attr "mode" "V4DI")])

(define_insn "lasx_xvpackev_b"
  [(set (match_operand:V32QI 0 "register_operand" "=f")
	(vec_select:V32QI
	  (vec_concat:V64QI
	    (match_operand:V32QI 1 "register_operand" "f")
	    (match_operand:V32QI 2 "register_operand" "f"))
	  (parallel [(const_int 0)  (const_int 32)
		     (const_int 2)  (const_int 34)
		     (const_int 4)  (const_int 36)
		     (const_int 6)  (const_int 38)
		     (const_int 8)  (const_int 40)
		     (const_int 10)  (const_int 42)
		     (const_int 12)  (const_int 44)
		     (const_int 14)  (const_int 46)
		     (const_int 16)  (const_int 48)
		     (const_int 18)  (const_int 50)
		     (const_int 20)  (const_int 52)
		     (const_int 22)  (const_int 54)
		     (const_int 24)  (const_int 56)
		     (const_int 26)  (const_int 58)
		     (const_int 28)  (const_int 60)
		     (const_int 30)  (const_int 62)])))]
  "ISA_HAS_LASX"
  "xvpackev.b\t%u0,%u2,%u1"
  [(set_attr "type" "simd_permute")
   (set_attr "mode" "V32QI")])


(define_insn "lasx_xvpackev_h"
  [(set (match_operand:V16HI 0 "register_operand" "=f")
	(vec_select:V16HI
	  (vec_concat:V32HI
	    (match_operand:V16HI 1 "register_operand" "f")
	    (match_operand:V16HI 2 "register_operand" "f"))
	  (parallel [(const_int 0)  (const_int 16)
		     (const_int 2)  (const_int 18)
		     (const_int 4)  (const_int 20)
		     (const_int 6)  (const_int 22)
		     (const_int 8)  (const_int 24)
		     (const_int 10) (const_int 26)
		     (const_int 12) (const_int 28)
		     (const_int 14) (const_int 30)])))]
  "ISA_HAS_LASX"
  "xvpackev.h\t%u0,%u2,%u1"
  [(set_attr "type" "simd_permute")
   (set_attr "mode" "V16HI")])

(define_insn "lasx_xvpackev_w"
  [(set (match_operand:V8SI 0 "register_operand" "=f")
	(vec_select:V8SI
	  (vec_concat:V16SI
	    (match_operand:V8SI 1 "register_operand" "f")
	    (match_operand:V8SI 2 "register_operand" "f"))
	  (parallel [(const_int 0) (const_int 8)
		     (const_int 2) (const_int 10)
		     (const_int 4) (const_int 12)
		     (const_int 6) (const_int 14)])))]
  "ISA_HAS_LASX"
  "xvpackev.w\t%u0,%u2,%u1"
  [(set_attr "type" "simd_permute")
   (set_attr "mode" "V8SI")])

(define_insn "lasx_xvpackev_w_f"
  [(set (match_operand:V8SF 0 "register_operand" "=f")
	(vec_select:V8SF
	  (vec_concat:V16SF
	    (match_operand:V8SF 1 "register_operand" "f")
	    (match_operand:V8SF 2 "register_operand" "f"))
	  (parallel [(const_int 0) (const_int 8)
		     (const_int 2) (const_int 10)
		     (const_int 4) (const_int 12)
		     (const_int 6) (const_int 14)])))]
  "ISA_HAS_LASX"
  "xvpackev.w\t%u0,%u2,%u1"
  [(set_attr "type" "simd_permute")
   (set_attr "mode" "V8SF")])

(define_insn "lasx_xvilvh_b"
  [(set (match_operand:V32QI 0 "register_operand" "=f")
	(vec_select:V32QI
	  (vec_concat:V64QI
	    (match_operand:V32QI 1 "register_operand" "f")
	    (match_operand:V32QI 2 "register_operand" "f"))
	  (parallel [(const_int 8) (const_int 40)
		     (const_int 9) (const_int 41)
		     (const_int 10) (const_int 42)
		     (const_int 11) (const_int 43)
		     (const_int 12) (const_int 44)
		     (const_int 13) (const_int 45)
		     (const_int 14) (const_int 46)
		     (const_int 15) (const_int 47)
		     (const_int 24) (const_int 56)
		     (const_int 25) (const_int 57)
		     (const_int 26) (const_int 58)
		     (const_int 27) (const_int 59)
		     (const_int 28) (const_int 60)
		     (const_int 29) (const_int 61)
		     (const_int 30) (const_int 62)
		     (const_int 31) (const_int 63)])))]
  "ISA_HAS_LASX"
  "xvilvh.b\t%u0,%u2,%u1"
  [(set_attr "type" "simd_permute")
   (set_attr "mode" "V32QI")])

(define_insn "lasx_xvilvh_h"
  [(set (match_operand:V16HI 0 "register_operand" "=f")
	(vec_select:V16HI
	  (vec_concat:V32HI
	    (match_operand:V16HI 1 "register_operand" "f")
	    (match_operand:V16HI 2 "register_operand" "f"))
	  (parallel [(const_int 4) (const_int 20)
		     (const_int 5) (const_int 21)
		     (const_int 6) (const_int 22)
		     (const_int 7) (const_int 23)
		     (const_int 12) (const_int 28)
		     (const_int 13) (const_int 29)
		     (const_int 14) (const_int 30)
		     (const_int 15) (const_int 31)])))]
  "ISA_HAS_LASX"
  "xvilvh.h\t%u0,%u2,%u1"
  [(set_attr "type" "simd_permute")
   (set_attr "mode" "V16HI")])

(define_mode_attr xvilvh_suffix
  [(V8SI "") (V8SF "_f")
   (V4DI "") (V4DF "_f")])

(define_insn "lasx_xvilvh_w<xvilvh_suffix>"
  [(set (match_operand:LASX_W 0 "register_operand" "=f")
	(vec_select:LASX_W
	  (vec_concat:<VEMODE256>
	    (match_operand:LASX_W 1 "register_operand" "f")
	    (match_operand:LASX_W 2 "register_operand" "f"))
	  (parallel [(const_int 2) (const_int 10)
		     (const_int 3) (const_int 11)
		     (const_int 6) (const_int 14)
		     (const_int 7) (const_int 15)])))]
  "ISA_HAS_LASX"
  "xvilvh.w\t%u0,%u2,%u1"
  [(set_attr "type" "simd_permute")
   (set_attr "mode" "<MODE>")])

(define_insn "lasx_xvilvh_d<xvilvh_suffix>"
  [(set (match_operand:LASX_D 0 "register_operand" "=f")
	(vec_select:LASX_D
	  (vec_concat:<VEMODE256>
	    (match_operand:LASX_D 1 "register_operand" "f")
	    (match_operand:LASX_D 2 "register_operand" "f"))
	  (parallel [(const_int 1) (const_int 5)
		     (const_int 3) (const_int 7)])))]
  "ISA_HAS_LASX"
  "xvilvh.d\t%u0,%u2,%u1"
  [(set_attr "type" "simd_permute")
   (set_attr "mode" "<MODE>")])

(define_insn "lasx_xvpackod_b"
  [(set (match_operand:V32QI 0 "register_operand" "=f")
	(vec_select:V32QI
	  (vec_concat:V64QI
	    (match_operand:V32QI 1 "register_operand" "f")
	    (match_operand:V32QI 2 "register_operand" "f"))
	  (parallel [(const_int 1)  (const_int 33)
		     (const_int 3)  (const_int 35)
		     (const_int 5)  (const_int 37)
		     (const_int 7)  (const_int 39)
		     (const_int 9)  (const_int 41)
		     (const_int 11)  (const_int 43)
		     (const_int 13)  (const_int 45)
		     (const_int 15)  (const_int 47)
		     (const_int 17)  (const_int 49)
		     (const_int 19)  (const_int 51)
		     (const_int 21)  (const_int 53)
		     (const_int 23)  (const_int 55)
		     (const_int 25)  (const_int 57)
		     (const_int 27)  (const_int 59)
		     (const_int 29)  (const_int 61)
		     (const_int 31)  (const_int 63)])))]
  "ISA_HAS_LASX"
  "xvpackod.b\t%u0,%u2,%u1"
  [(set_attr "type" "simd_permute")
   (set_attr "mode" "V32QI")])


(define_insn "lasx_xvpackod_h"
  [(set (match_operand:V16HI 0 "register_operand" "=f")
	(vec_select:V16HI
	  (vec_concat:V32HI
	    (match_operand:V16HI 1 "register_operand" "f")
	    (match_operand:V16HI 2 "register_operand" "f"))
	  (parallel [(const_int 1) (const_int 17)
		     (const_int 3) (const_int 19)
		     (const_int 5) (const_int 21)
		     (const_int 7) (const_int 23)
		     (const_int 9) (const_int 25)
		     (const_int 11) (const_int 27)
		     (const_int 13) (const_int 29)
		     (const_int 15) (const_int 31)])))]
  "ISA_HAS_LASX"
  "xvpackod.h\t%u0,%u2,%u1"
  [(set_attr "type" "simd_permute")
   (set_attr "mode" "V16HI")])


(define_insn "lasx_xvpackod_w"
  [(set (match_operand:V8SI 0 "register_operand" "=f")
	(vec_select:V8SI
	  (vec_concat:V16SI
	    (match_operand:V8SI 1 "register_operand" "f")
	    (match_operand:V8SI 2 "register_operand" "f"))
	  (parallel [(const_int 1) (const_int 9)
		     (const_int 3) (const_int 11)
		     (const_int 5) (const_int 13)
		     (const_int 7) (const_int 15)])))]
  "ISA_HAS_LASX"
  "xvpackod.w\t%u0,%u2,%u1"
  [(set_attr "type" "simd_permute")
   (set_attr "mode" "V8SI")])


(define_insn "lasx_xvpackod_w_f"
  [(set (match_operand:V8SF 0 "register_operand" "=f")
	(vec_select:V8SF
	  (vec_concat:V16SF
	    (match_operand:V8SF 1 "register_operand" "f")
	    (match_operand:V8SF 2 "register_operand" "f"))
	  (parallel [(const_int 1) (const_int 9)
		     (const_int 3) (const_int 11)
		     (const_int 5) (const_int 13)
		     (const_int 7) (const_int 15)])))]
  "ISA_HAS_LASX"
  "xvpackod.w\t%u0,%u2,%u1"
  [(set_attr "type" "simd_permute")
   (set_attr "mode" "V8SF")])

(define_insn "lasx_xvilvl_b"
  [(set (match_operand:V32QI 0 "register_operand" "=f")
	(vec_select:V32QI
	  (vec_concat:V64QI
	    (match_operand:V32QI 1 "register_operand" "f")
	    (match_operand:V32QI 2 "register_operand" "f"))
	  (parallel [(const_int 0) (const_int 32)
		     (const_int 1) (const_int 33)
		     (const_int 2) (const_int 34)
		     (const_int 3) (const_int 35)
		     (const_int 4) (const_int 36)
		     (const_int 5) (const_int 37)
		     (const_int 6) (const_int 38)
		     (const_int 7) (const_int 39)
		     (const_int 16) (const_int 48)
		     (const_int 17) (const_int 49)
		     (const_int 18) (const_int 50)
		     (const_int 19) (const_int 51)
		     (const_int 20) (const_int 52)
		     (const_int 21) (const_int 53)
		     (const_int 22) (const_int 54)
		     (const_int 23) (const_int 55)])))]
  "ISA_HAS_LASX"
  "xvilvl.b\t%u0,%u2,%u1"
  [(set_attr "type" "simd_permute")
   (set_attr "mode" "V32QI")])

(define_insn "lasx_xvilvl_h"
  [(set (match_operand:V16HI 0 "register_operand" "=f")
	(vec_select:V16HI
	  (vec_concat:V32HI
	    (match_operand:V16HI 1 "register_operand" "f")
	    (match_operand:V16HI 2 "register_operand" "f"))
	  (parallel [(const_int 0) (const_int 16)
		     (const_int 1) (const_int 17)
		     (const_int 2) (const_int 18)
		     (const_int 3) (const_int 19)
		     (const_int 8) (const_int 24)
		     (const_int 9) (const_int 25)
		     (const_int 10) (const_int 26)
		     (const_int 11) (const_int 27)])))]
  "ISA_HAS_LASX"
  "xvilvl.h\t%u0,%u2,%u1"
  [(set_attr "type" "simd_permute")
   (set_attr "mode" "V16HI")])

(define_insn "lasx_xvilvl_w"
  [(set (match_operand:V8SI 0 "register_operand" "=f")
	(vec_select:V8SI
	  (vec_concat:V16SI
	    (match_operand:V8SI 1 "register_operand" "f")
	    (match_operand:V8SI 2 "register_operand" "f"))
	  (parallel [(const_int 0) (const_int 8)
		     (const_int 1) (const_int 9)
		     (const_int 4) (const_int 12)
		     (const_int 5) (const_int 13)])))]
  "ISA_HAS_LASX"
  "xvilvl.w\t%u0,%u2,%u1"
  [(set_attr "type" "simd_permute")
   (set_attr "mode" "V8SI")])

(define_insn "lasx_xvilvl_w_f"
  [(set (match_operand:V8SF 0 "register_operand" "=f")
	(vec_select:V8SF
	  (vec_concat:V16SF
	    (match_operand:V8SF 1 "register_operand" "f")
	    (match_operand:V8SF 2 "register_operand" "f"))
	  (parallel [(const_int 0) (const_int 8)
		     (const_int 1) (const_int 9)
		     (const_int 4) (const_int 12)
		     (const_int 5) (const_int 13)])))]
  "ISA_HAS_LASX"
  "xvilvl.w\t%u0,%u2,%u1"
  [(set_attr "type" "simd_permute")
   (set_attr "mode" "V8SF")])

(define_insn "lasx_xvilvl_d"
  [(set (match_operand:V4DI 0 "register_operand" "=f")
	(vec_select:V4DI
	  (vec_concat:V8DI
	    (match_operand:V4DI 1 "register_operand" "f")
	    (match_operand:V4DI 2 "register_operand" "f"))
	  (parallel [(const_int 0) (const_int 4)
		     (const_int 2) (const_int 6)])))]
  "ISA_HAS_LASX"
  "xvilvl.d\t%u0,%u2,%u1"
  [(set_attr "type" "simd_permute")
   (set_attr "mode" "V4DI")])

(define_insn "lasx_xvilvl_d_f"
  [(set (match_operand:V4DF 0 "register_operand" "=f")
	(vec_select:V4DF
	  (vec_concat:V8DF
	    (match_operand:V4DF 1 "register_operand" "f")
	    (match_operand:V4DF 2 "register_operand" "f"))
	  (parallel [(const_int 0) (const_int 4)
		     (const_int 2) (const_int 6)])))]
  "ISA_HAS_LASX"
  "xvilvl.d\t%u0,%u2,%u1"
  [(set_attr "type" "simd_permute")
   (set_attr "mode" "V4DF")])

(define_insn "smax<mode>3"
  [(set (match_operand:ILASX 0 "register_operand" "=f,f")
	(smax:ILASX (match_operand:ILASX 1 "register_operand" "f,f")
		    (match_operand:ILASX 2 "reg_or_vector_same_simm5_operand" "f,Usv5")))]
  "ISA_HAS_LASX"
  "@
   xvmax.<lasxfmt>\t%u0,%u1,%u2
   xvmaxi.<lasxfmt>\t%u0,%u1,%E2"
  [(set_attr "type" "simd_int_arith")
   (set_attr "mode" "<MODE>")])

(define_insn "umax<mode>3"
  [(set (match_operand:ILASX 0 "register_operand" "=f,f")
	(umax:ILASX (match_operand:ILASX 1 "register_operand" "f,f")
		    (match_operand:ILASX 2 "reg_or_vector_same_uimm5_operand" "f,Uuv5")))]
  "ISA_HAS_LASX"
  "@
   xvmax.<lasxfmt_u>\t%u0,%u1,%u2
   xvmaxi.<lasxfmt_u>\t%u0,%u1,%B2"
  [(set_attr "type" "simd_int_arith")
   (set_attr "mode" "<MODE>")])

(define_insn "smin<mode>3"
  [(set (match_operand:ILASX 0 "register_operand" "=f,f")
	(smin:ILASX (match_operand:ILASX 1 "register_operand" "f,f")
		    (match_operand:ILASX 2 "reg_or_vector_same_simm5_operand" "f,Usv5")))]
  "ISA_HAS_LASX"
  "@
   xvmin.<lasxfmt>\t%u0,%u1,%u2
   xvmini.<lasxfmt>\t%u0,%u1,%E2"
  [(set_attr "type" "simd_int_arith")
   (set_attr "mode" "<MODE>")])

(define_insn "umin<mode>3"
  [(set (match_operand:ILASX 0 "register_operand" "=f,f")
	(umin:ILASX (match_operand:ILASX 1 "register_operand" "f,f")
		    (match_operand:ILASX 2 "reg_or_vector_same_uimm5_operand" "f,Uuv5")))]
  "ISA_HAS_LASX"
  "@
   xvmin.<lasxfmt_u>\t%u0,%u1,%u2
   xvmini.<lasxfmt_u>\t%u0,%u1,%B2"
  [(set_attr "type" "simd_int_arith")
   (set_attr "mode" "<MODE>")])

(define_insn "lasx_xvclo_<lasxfmt>"
  [(set (match_operand:ILASX 0 "register_operand" "=f")
	(clz:ILASX (not:ILASX (match_operand:ILASX 1 "register_operand" "f"))))]
  "ISA_HAS_LASX"
  "xvclo.<lasxfmt>\t%u0,%u1"
  [(set_attr "type" "simd_bit")
   (set_attr "mode" "<MODE>")])

(define_insn "clz<mode>2"
  [(set (match_operand:ILASX 0 "register_operand" "=f")
	(clz:ILASX (match_operand:ILASX 1 "register_operand" "f")))]
  "ISA_HAS_LASX"
  "xvclz.<lasxfmt>\t%u0,%u1"
  [(set_attr "type" "simd_bit")
   (set_attr "mode" "<MODE>")])

(define_insn "lasx_xvnor_<lasxfmt>"
  [(set (match_operand:ILASX 0 "register_operand" "=f,f")
	(and:ILASX (not:ILASX (match_operand:ILASX 1 "register_operand" "f,f"))
		   (not:ILASX (match_operand:ILASX 2 "reg_or_vector_same_val_operand" "f,Urv8"))))]
  "ISA_HAS_LASX"
  "@
   xvnor.v\t%u0,%u1,%u2
   xvnori.b\t%u0,%u1,%B2"
  [(set_attr "type" "simd_logic")
   (set_attr "mode" "<MODE>")])

(define_insn "lasx_xvpickev_b"
  [(set (match_operand:V32QI 0 "register_operand" "=f")
	(vec_select:V32QI
	  (vec_concat:V64QI
	    (match_operand:V32QI 1 "register_operand" "f")
	    (match_operand:V32QI 2 "register_operand" "f"))
	  (parallel [(const_int 0) (const_int 2)
		     (const_int 4) (const_int 6)
		     (const_int 8) (const_int 10)
		     (const_int 12) (const_int 14)
		     (const_int 32) (const_int 34)
		     (const_int 36) (const_int 38)
		     (const_int 40) (const_int 42)
		     (const_int 44) (const_int 46)
		     (const_int 16) (const_int 18)
		     (const_int 20) (const_int 22)
		     (const_int 24) (const_int 26)
		     (const_int 28) (const_int 30)
		     (const_int 48) (const_int 50)
		     (const_int 52) (const_int 54)
		     (const_int 56) (const_int 58)
		     (const_int 60) (const_int 62)])))]
  "ISA_HAS_LASX"
  "xvpickev.b\t%u0,%u2,%u1"
  [(set_attr "type" "simd_permute")
   (set_attr "mode" "V32QI")])

(define_insn "lasx_xvpickev_h"
  [(set (match_operand:V16HI 0 "register_operand" "=f")
	(vec_select:V16HI
	  (vec_concat:V32HI
	    (match_operand:V16HI 1 "register_operand" "f")
	    (match_operand:V16HI 2 "register_operand" "f"))
	  (parallel [(const_int 0) (const_int 2)
		     (const_int 4) (const_int 6)
		     (const_int 16) (const_int 18)
		     (const_int 20) (const_int 22)
		     (const_int 8) (const_int 10)
		     (const_int 12) (const_int 14)
		     (const_int 24) (const_int 26)
		     (const_int 28) (const_int 30)])))]
  "ISA_HAS_LASX"
  "xvpickev.h\t%u0,%u2,%u1"
  [(set_attr "type" "simd_permute")
   (set_attr "mode" "V16HI")])

(define_insn "lasx_xvpickev_w"
  [(set (match_operand:V8SI 0 "register_operand" "=f")
	(vec_select:V8SI
	  (vec_concat:V16SI
	    (match_operand:V8SI 1 "register_operand" "f")
	    (match_operand:V8SI 2 "register_operand" "f"))
	  (parallel [(const_int 0) (const_int 2)
		     (const_int 8) (const_int 10)
		     (const_int 4) (const_int 6)
		     (const_int 12) (const_int 14)])))]
  "ISA_HAS_LASX"
  "xvpickev.w\t%u0,%u2,%u1"
  [(set_attr "type" "simd_permute")
   (set_attr "mode" "V8SI")])

(define_insn "lasx_xvpickev_w_f"
  [(set (match_operand:V8SF 0 "register_operand" "=f")
	(vec_select:V8SF
	  (vec_concat:V16SF
	    (match_operand:V8SF 1 "register_operand" "f")
	    (match_operand:V8SF 2 "register_operand" "f"))
	  (parallel [(const_int 0) (const_int 2)
		     (const_int 8) (const_int 10)
		     (const_int 4) (const_int 6)
		     (const_int 12) (const_int 14)])))]
  "ISA_HAS_LASX"
  "xvpickev.w\t%u0,%u2,%u1"
  [(set_attr "type" "simd_permute")
   (set_attr "mode" "V8SF")])

(define_insn "lasx_xvpickod_b"
  [(set (match_operand:V32QI 0 "register_operand" "=f")
	(vec_select:V32QI
	  (vec_concat:V64QI
	    (match_operand:V32QI 1 "register_operand" "f")
	    (match_operand:V32QI 2 "register_operand" "f"))
	  (parallel [(const_int 1) (const_int 3)
		     (const_int 5) (const_int 7)
		     (const_int 9) (const_int 11)
		     (const_int 13) (const_int 15)
		     (const_int 33) (const_int 35)
		     (const_int 37) (const_int 39)
		     (const_int 41) (const_int 43)
		     (const_int 45) (const_int 47)
		     (const_int 17) (const_int 19)
		     (const_int 21) (const_int 23)
		     (const_int 25) (const_int 27)
		     (const_int 29) (const_int 31)
		     (const_int 49) (const_int 51)
		     (const_int 53) (const_int 55)
		     (const_int 57) (const_int 59)
		     (const_int 61) (const_int 63)])))]
  "ISA_HAS_LASX"
  "xvpickod.b\t%u0,%u2,%u1"
  [(set_attr "type" "simd_permute")
   (set_attr "mode" "V32QI")])

(define_insn "lasx_xvpickod_h"
  [(set (match_operand:V16HI 0 "register_operand" "=f")
	(vec_select:V16HI
	  (vec_concat:V32HI
	    (match_operand:V16HI 1 "register_operand" "f")
	    (match_operand:V16HI 2 "register_operand" "f"))
	  (parallel [(const_int 1) (const_int 3)
		     (const_int 5) (const_int 7)
		     (const_int 17) (const_int 19)
		     (const_int 21) (const_int 23)
		     (const_int 9) (const_int 11)
		     (const_int 13) (const_int 15)
		     (const_int 25) (const_int 27)
		     (const_int 29) (const_int 31)])))]
  "ISA_HAS_LASX"
  "xvpickod.h\t%u0,%u2,%u1"
  [(set_attr "type" "simd_permute")
   (set_attr "mode" "V16HI")])

(define_insn "lasx_xvpickod_w"
  [(set (match_operand:V8SI 0 "register_operand" "=f")
	(vec_select:V8SI
	  (vec_concat:V16SI
	    (match_operand:V8SI 1 "register_operand" "f")
	    (match_operand:V8SI 2 "register_operand" "f"))
	  (parallel [(const_int 1) (const_int 3)
		     (const_int 9) (const_int 11)
		     (const_int 5) (const_int 7)
		     (const_int 13) (const_int 15)])))]
  "ISA_HAS_LASX"
  "xvpickod.w\t%u0,%u2,%u1"
  [(set_attr "type" "simd_permute")
   (set_attr "mode" "V8SI")])

(define_insn "lasx_xvpickod_w_f"
  [(set (match_operand:V8SF 0 "register_operand" "=f")
	(vec_select:V8SF
	  (vec_concat:V16SF
	    (match_operand:V8SF 1 "register_operand" "f")
	    (match_operand:V8SF 2 "register_operand" "f"))
	  (parallel [(const_int 1) (const_int 3)
		     (const_int 9) (const_int 11)
		     (const_int 5) (const_int 7)
		     (const_int 13) (const_int 15)])))]
  "ISA_HAS_LASX"
  "xvpickod.w\t%u0,%u2,%u1"
  [(set_attr "type" "simd_permute")
   (set_attr "mode" "V8SF")])

(define_insn "popcount<mode>2"
  [(set (match_operand:ILASX 0 "register_operand" "=f")
	(popcount:ILASX (match_operand:ILASX 1 "register_operand" "f")))]
  "ISA_HAS_LASX"
  "xvpcnt.<lasxfmt>\t%u0,%u1"
  [(set_attr "type" "simd_pcnt")
   (set_attr "mode" "<MODE>")])


(define_insn "lasx_xvsat_s_<lasxfmt>"
  [(set (match_operand:ILASX 0 "register_operand" "=f")
	(unspec:ILASX [(match_operand:ILASX 1 "register_operand" "f")
		      (match_operand 2 "const_<bitimm256>_operand" "")]
		     UNSPEC_LASX_XVSAT_S))]
  "ISA_HAS_LASX"
  "xvsat.<lasxfmt>\t%u0,%u1,%2"
  [(set_attr "type" "simd_sat")
   (set_attr "mode" "<MODE>")])

(define_insn "lasx_xvsat_u_<lasxfmt_u>"
  [(set (match_operand:ILASX 0 "register_operand" "=f")
	(unspec:ILASX [(match_operand:ILASX 1 "register_operand" "f")
		       (match_operand 2 "const_<bitimm256>_operand" "")]
		      UNSPEC_LASX_XVSAT_U))]
  "ISA_HAS_LASX"
  "xvsat.<lasxfmt_u>\t%u0,%u1,%2"
  [(set_attr "type" "simd_sat")
   (set_attr "mode" "<MODE>")])

(define_insn "lasx_xvshuf4i_<lasxfmt_f>"
  [(set (match_operand:LASX_WHB_W 0 "register_operand" "=f")
	(unspec:LASX_WHB_W [(match_operand:LASX_WHB_W 1 "register_operand" "f")
			    (match_operand 2 "const_uimm8_operand")]
			   UNSPEC_LASX_XVSHUF4I))]
  "ISA_HAS_LASX"
  "xvshuf4i.<lasxfmt>\t%u0,%u1,%2"
  [(set_attr "type" "simd_shf")
   (set_attr "mode" "<MODE>")])

(define_insn "lasx_xvshuf4i_<lasxfmt_f>_1"
  [(set (match_operand:LASX_W 0 "register_operand" "=f")
    (vec_select:LASX_W
      (match_operand:LASX_W 1 "nonimmediate_operand" "f")
      (parallel [(match_operand 2 "const_0_to_3_operand")
             (match_operand 3 "const_0_to_3_operand")
             (match_operand 4 "const_0_to_3_operand")
             (match_operand 5 "const_0_to_3_operand")
             (match_operand 6 "const_4_to_7_operand")
             (match_operand 7 "const_4_to_7_operand")
             (match_operand 8 "const_4_to_7_operand")
             (match_operand 9 "const_4_to_7_operand")])))]
  "ISA_HAS_LASX
   && INTVAL (operands[2]) + 4 == INTVAL (operands[6])
   && INTVAL (operands[3]) + 4 == INTVAL (operands[7])
   && INTVAL (operands[4]) + 4 == INTVAL (operands[8])
   && INTVAL (operands[5]) + 4 == INTVAL (operands[9])"
{
  int mask = 0;
  mask |= INTVAL (operands[2]) << 0;
  mask |= INTVAL (operands[3]) << 2;
  mask |= INTVAL (operands[4]) << 4;
  mask |= INTVAL (operands[5]) << 6;
  operands[2] = GEN_INT (mask);

  return "xvshuf4i.w\t%u0,%u1,%2";
}
  [(set_attr "type" "simd_shf")
   (set_attr "mode" "<MODE>")])

(define_insn "lasx_xvsrar_<lasxfmt>"
  [(set (match_operand:ILASX 0 "register_operand" "=f")
	(unspec:ILASX [(match_operand:ILASX 1 "register_operand" "f")
		       (match_operand:ILASX 2 "register_operand" "f")]
		      UNSPEC_LASX_XVSRAR))]
  "ISA_HAS_LASX"
  "xvsrar.<lasxfmt>\t%u0,%u1,%u2"
  [(set_attr "type" "simd_shift")
   (set_attr "mode" "<MODE>")])

(define_insn "lasx_xvsrari_<lasxfmt>"
  [(set (match_operand:ILASX 0 "register_operand" "=f")
	(unspec:ILASX [(match_operand:ILASX 1 "register_operand" "f")
		       (match_operand 2 "const_<bitimm256>_operand" "")]
		      UNSPEC_LASX_XVSRARI))]
  "ISA_HAS_LASX"
  "xvsrari.<lasxfmt>\t%u0,%u1,%2"
  [(set_attr "type" "simd_shift")
   (set_attr "mode" "<MODE>")])

(define_insn "lasx_xvsrlr_<lasxfmt>"
  [(set (match_operand:ILASX 0 "register_operand" "=f")
	(unspec:ILASX [(match_operand:ILASX 1 "register_operand" "f")
		       (match_operand:ILASX 2 "register_operand" "f")]
		      UNSPEC_LASX_XVSRLR))]
  "ISA_HAS_LASX"
  "xvsrlr.<lasxfmt>\t%u0,%u1,%u2"
  [(set_attr "type" "simd_shift")
   (set_attr "mode" "<MODE>")])

(define_insn "lasx_xvsrlri_<lasxfmt>"
  [(set (match_operand:ILASX 0 "register_operand" "=f")
	(unspec:ILASX [(match_operand:ILASX 1 "register_operand" "f")
		       (match_operand 2 "const_<bitimm256>_operand" "")]
		      UNSPEC_LASX_XVSRLRI))]
  "ISA_HAS_LASX"
  "xvsrlri.<lasxfmt>\t%u0,%u1,%2"
  [(set_attr "type" "simd_shift")
   (set_attr "mode" "<MODE>")])

(define_insn "lasx_xvssub_s_<lasxfmt>"
  [(set (match_operand:ILASX 0 "register_operand" "=f")
	(ss_minus:ILASX (match_operand:ILASX 1 "register_operand" "f")
		       (match_operand:ILASX 2 "register_operand" "f")))]
  "ISA_HAS_LASX"
  "xvssub.<lasxfmt>\t%u0,%u1,%u2"
  [(set_attr "type" "simd_int_arith")
   (set_attr "mode" "<MODE>")])

(define_insn "lasx_xvssub_u_<lasxfmt_u>"
  [(set (match_operand:ILASX 0 "register_operand" "=f")
	(us_minus:ILASX (match_operand:ILASX 1 "register_operand" "f")
		       (match_operand:ILASX 2 "register_operand" "f")))]
  "ISA_HAS_LASX"
  "xvssub.<lasxfmt_u>\t%u0,%u1,%u2"
  [(set_attr "type" "simd_int_arith")
   (set_attr "mode" "<MODE>")])

(define_insn "lasx_xvshuf_<lasxfmt_f>"
  [(set (match_operand:LASX_DWH 0 "register_operand" "=f")
	(unspec:LASX_DWH [(match_operand:LASX_DWH 1 "register_operand" "0")
			  (match_operand:LASX_DWH 2 "register_operand" "f")
			  (match_operand:LASX_DWH 3 "register_operand" "f")]
			UNSPEC_LASX_XVSHUF))]
  "ISA_HAS_LASX"
  "xvshuf.<lasxfmt>\t%u0,%u2,%u3"
  [(set_attr "type" "simd_sld")
   (set_attr "mode" "<MODE>")])

(define_insn "lasx_xvshuf_b"
  [(set (match_operand:V32QI 0 "register_operand" "=f")
	(unspec:V32QI [(match_operand:V32QI 1 "register_operand" "f")
		       (match_operand:V32QI 2 "register_operand" "f")
		       (match_operand:V32QI 3 "register_operand" "f")]
		      UNSPEC_LASX_XVSHUF_B))]
  "ISA_HAS_LASX"
  "xvshuf.b\t%u0,%u1,%u2,%u3"
  [(set_attr "type" "simd_sld")
   (set_attr "mode" "V32QI")])

(define_insn "lasx_xvreplve0_<lasxfmt_f>"
  [(set (match_operand:LASX 0 "register_operand" "=f")
	(vec_duplicate:LASX
	  (vec_select:<UNITMODE>
	    (match_operand:LASX 1 "register_operand" "f")
	    (parallel [(const_int 0)]))))]
  "ISA_HAS_LASX"
  "xvreplve0.<lasxfmt>\t%u0,%u1"
  [(set_attr "type" "simd_splat")
   (set_attr "mode" "<MODE>")])

(define_insn "lasx_xvrepl128vei_b_internal"
  [(set (match_operand:V32QI 0 "register_operand" "=f")
	(vec_duplicate:V32QI
	  (vec_select:V32QI
	    (match_operand:V32QI 1 "register_operand" "f")
	    (parallel [(match_operand 2 "const_uimm4_operand" "")
		       (match_dup 2) (match_dup 2) (match_dup 2)
		       (match_dup 2) (match_dup 2) (match_dup 2)
		       (match_dup 2) (match_dup 2) (match_dup 2)
		       (match_dup 2) (match_dup 2) (match_dup 2)
		       (match_dup 2) (match_dup 2) (match_dup 2)
		       (match_operand 3 "const_16_to_31_operand" "")
		       (match_dup 3) (match_dup 3) (match_dup 3)
		       (match_dup 3) (match_dup 3) (match_dup 3)
		       (match_dup 3) (match_dup 3) (match_dup 3)
		       (match_dup 3) (match_dup 3) (match_dup 3)
		       (match_dup 3) (match_dup 3) (match_dup 3)]))))]
  "ISA_HAS_LASX && ((INTVAL (operands[3]) - INTVAL (operands[2])) == 16)"
  "xvrepl128vei.b\t%u0,%u1,%2"
  [(set_attr "type" "simd_splat")
   (set_attr "mode" "V32QI")])

(define_insn "lasx_xvrepl128vei_h_internal"
  [(set (match_operand:V16HI 0 "register_operand" "=f")
	(vec_duplicate:V16HI
	  (vec_select:V16HI
	    (match_operand:V16HI 1 "register_operand" "f")
	    (parallel [(match_operand 2 "const_uimm3_operand" "")
		       (match_dup 2) (match_dup 2) (match_dup 2)
		       (match_dup 2) (match_dup 2) (match_dup 2)
		       (match_dup 2)
		       (match_operand 3 "const_8_to_15_operand" "")
		       (match_dup 3) (match_dup 3) (match_dup 3)
		       (match_dup 3) (match_dup 3) (match_dup 3)
		       (match_dup 3)]))))]
  "ISA_HAS_LASX && ((INTVAL (operands[3]) - INTVAL (operands[2])) == 8)"
  "xvrepl128vei.h\t%u0,%u1,%2"
  [(set_attr "type" "simd_splat")
   (set_attr "mode" "V16HI")])

(define_insn "lasx_xvrepl128vei_w_internal"
  [(set (match_operand:V8SI 0 "register_operand" "=f")
	(vec_duplicate:V8SI
	  (vec_select:V8SI
	    (match_operand:V8SI 1 "register_operand" "f")
	    (parallel [(match_operand 2 "const_0_to_3_operand" "")
		       (match_dup 2) (match_dup 2) (match_dup 2)
		       (match_operand 3 "const_4_to_7_operand" "")
		       (match_dup 3) (match_dup 3) (match_dup 3)]))))]
  "ISA_HAS_LASX && ((INTVAL (operands[3]) - INTVAL (operands[2])) == 4)"
  "xvrepl128vei.w\t%u0,%u1,%2"
  [(set_attr "type" "simd_splat")
   (set_attr "mode" "V8SI")])

(define_insn "lasx_xvrepl128vei_d_internal"
  [(set (match_operand:V4DI 0 "register_operand" "=f")
	(vec_duplicate:V4DI
	  (vec_select:V4DI
	    (match_operand:V4DI 1 "register_operand" "f")
	    (parallel [(match_operand 2 "const_0_or_1_operand" "")
		       (match_dup 2)
		       (match_operand 3 "const_2_or_3_operand" "")
		       (match_dup 3)]))))]
  "ISA_HAS_LASX && ((INTVAL (operands[3]) - INTVAL (operands[2])) == 2)"
  "xvrepl128vei.d\t%u0,%u1,%2"
  [(set_attr "type" "simd_splat")
   (set_attr "mode" "V4DI")])

(define_insn "lasx_xvrepl128vei_<lasxfmt_f>"
  [(set (match_operand:LASX 0 "register_operand" "=f")
	(unspec:LASX [(match_operand:LASX 1 "register_operand" "f")
		      (match_operand 2 "const_<indeximm_lo>_operand" "")]
		     UNSPEC_LASX_XVREPL128VEI))]
  "ISA_HAS_LASX"
  "xvrepl128vei.<lasxfmt>\t%u0,%u1,%2"
  [(set_attr "type" "simd_splat")
   (set_attr "mode" "<MODE>")])

(define_insn "lasx_xvreplve0_<lasxfmt_f>_scalar"
  [(set (match_operand:FLASX 0 "register_operand" "=f")
    (vec_duplicate:FLASX
      (match_operand:<UNITMODE> 1 "register_operand" "f")))]
  "ISA_HAS_LASX"
  "xvreplve0.<lasxfmt>\t%u0,%u1"
  [(set_attr "type" "simd_splat")
   (set_attr "mode" "<MODE>")])

(define_insn "lasx_xvreplve0_q"
  [(set (match_operand:V32QI 0 "register_operand" "=f")
	(unspec:V32QI [(match_operand:V32QI 1 "register_operand" "f")]
		      UNSPEC_LASX_XVREPLVE0_Q))]
  "ISA_HAS_LASX"
  "xvreplve0.q\t%u0,%u1"
  [(set_attr "type" "simd_splat")
   (set_attr "mode" "V32QI")])

(define_insn "lasx_xvfcvt_h_s"
  [(set (match_operand:V16HI 0 "register_operand" "=f")
	(unspec:V16HI [(match_operand:V8SF 1 "register_operand" "f")
		       (match_operand:V8SF 2 "register_operand" "f")]
		      UNSPEC_LASX_XVFCVT))]
  "ISA_HAS_LASX"
  "xvfcvt.h.s\t%u0,%u1,%u2"
  [(set_attr "type" "simd_fcvt")
   (set_attr "mode" "V16HI")])

(define_insn "lasx_xvfcvt_s_d"
  [(set (match_operand:V8SF 0 "register_operand" "=f")
	(unspec:V8SF [(match_operand:V4DF 1 "register_operand" "f")
		      (match_operand:V4DF 2 "register_operand" "f")]
		     UNSPEC_LASX_XVFCVT))]
  "ISA_HAS_LASX"
  "xvfcvt.s.d\t%u0,%u1,%u2"
  [(set_attr "type" "simd_fcvt")
   (set_attr "mode" "V8SF")])

(define_insn "vec_pack_trunc_v4df"
  [(set (match_operand:V8SF 0 "register_operand" "=f")
	(vec_concat:V8SF
	  (float_truncate:V4SF (match_operand:V4DF 1 "register_operand" "f"))
	  (float_truncate:V4SF (match_operand:V4DF 2 "register_operand" "f"))))]
  "ISA_HAS_LASX"
  "xvfcvt.s.d\t%u0,%u2,%u1\n\txvpermi.d\t%u0,%u0,0xd8"
  [(set_attr "type" "simd_fcvt")
   (set_attr "mode" "V8SF")
   (set_attr "length" "8")])

;; Define for builtin function.
(define_insn "lasx_xvfcvth_s_h"
  [(set (match_operand:V8SF 0 "register_operand" "=f")
	(unspec:V8SF [(match_operand:V16HI 1 "register_operand" "f")]
		     UNSPEC_LASX_XVFCVTH))]
  "ISA_HAS_LASX"
  "xvfcvth.s.h\t%u0,%u1"
  [(set_attr "type" "simd_fcvt")
   (set_attr "mode" "V8SF")])

;; Define for builtin function.
(define_insn "lasx_xvfcvth_d_s"
  [(set (match_operand:V4DF 0 "register_operand" "=f")
	(float_extend:V4DF
	 (vec_select:V4SF
	  (match_operand:V8SF 1 "register_operand" "f")
	  (parallel [(const_int 2) (const_int 3)
		      (const_int 6) (const_int 7)]))))]
  "ISA_HAS_LASX"
  "xvfcvth.d.s\t%u0,%u1"
  [(set_attr "type" "simd_fcvt")
   (set_attr "mode" "V4DF")
   (set_attr "length" "12")])

;; Define for gen insn.
(define_insn "lasx_xvfcvth_d_insn"
  [(set (match_operand:V4DF 0 "register_operand" "=f")
	(float_extend:V4DF
	(vec_select:V4SF
	  (match_operand:V8SF 1 "register_operand" "f")
	  (parallel [(const_int 4) (const_int 5)
		     (const_int 6) (const_int 7)]))))]
  "ISA_HAS_LASX"
  "xvpermi.d\t%u0,%u1,0xfa\n\txvfcvtl.d.s\t%u0,%u0"
  [(set_attr "type" "simd_fcvt")
   (set_attr "mode" "V4DF")
   (set_attr "length" "12")])

;; Define for builtin function.
(define_insn "lasx_xvfcvtl_s_h"
  [(set (match_operand:V8SF 0 "register_operand" "=f")
	(unspec:V8SF [(match_operand:V16HI 1 "register_operand" "f")]
		     UNSPEC_LASX_XVFCVTL))]
  "ISA_HAS_LASX"
  "xvfcvtl.s.h\t%u0,%u1"
  [(set_attr "type" "simd_fcvt")
   (set_attr "mode" "V8SF")])

;; Define for builtin function.
(define_insn "lasx_xvfcvtl_d_s"
  [(set (match_operand:V4DF 0 "register_operand" "=f")
	(float_extend:V4DF
	(vec_select:V4SF
	  (match_operand:V8SF 1 "register_operand" "f")
	  (parallel [(const_int 0) (const_int 1)
		     (const_int 4) (const_int 5)]))))]
  "ISA_HAS_LASX"
  "xvfcvtl.d.s\t%u0,%u1"
  [(set_attr "type" "simd_fcvt")
   (set_attr "mode" "V4DF")
   (set_attr "length" "8")])

;; Define for gen insn.
(define_insn "lasx_xvfcvtl_d_insn"
  [(set (match_operand:V4DF 0 "register_operand" "=f")
	(float_extend:V4DF
	(vec_select:V4SF
	  (match_operand:V8SF 1 "register_operand" "f")
	  (parallel [(const_int 0) (const_int 1)
		     (const_int 2) (const_int 3)]))))]
  "ISA_HAS_LASX"
  "xvpermi.d\t%u0,%u1,0x50\n\txvfcvtl.d.s\t%u0,%u0"
  [(set_attr "type" "simd_fcvt")
   (set_attr "mode" "V4DF")
   (set_attr "length" "8")])

(define_code_attr lasxbr
  [(eq "xbz")
   (ne "xbnz")])

(define_code_attr lasxeq_v
  [(eq "eqz")
   (ne "nez")])

(define_code_attr lasxne_v
  [(eq "nez")
   (ne "eqz")])

(define_code_attr lasxeq
  [(eq "anyeqz")
   (ne "allnez")])

(define_code_attr lasxne
  [(eq "allnez")
   (ne "anyeqz")])

(define_insn "lasx_<lasxbr>_<lasxfmt_f>"
  [(set (pc)
	(if_then_else
	  (equality_op
	    (unspec:SI [(match_operand:LASX 1 "register_operand" "f")]
		       UNSPEC_LASX_BRANCH)
	    (match_operand:SI 2 "const_0_operand"))
	  (label_ref (match_operand 0))
	  (pc)))
   (clobber (match_scratch:FCC 3 "=z"))]
  "ISA_HAS_LASX"
{
  return loongarch_output_conditional_branch (insn, operands,
					 "xvset<lasxeq>.<lasxfmt>\t%Z3%u1\n\tbcnez\t%Z3%0",
					 "xvset<lasxne>.<lasxfmt>\t%z3%u1\n\tbcnez\t%Z3%0");
}
  [(set_attr "type" "simd_branch")
   (set_attr "mode" "<MODE>")])

(define_insn "lasx_<lasxbr>_v_<lasxfmt_f>"
  [(set (pc)
	(if_then_else
	  (equality_op
	    (unspec:SI [(match_operand:LASX 1 "register_operand" "f")]
		       UNSPEC_LASX_BRANCH_V)
	    (match_operand:SI 2 "const_0_operand"))
	  (label_ref (match_operand 0))
	  (pc)))
   (clobber (match_scratch:FCC 3 "=z"))]
  "ISA_HAS_LASX"
{
  return loongarch_output_conditional_branch (insn, operands,
					 "xvset<lasxeq_v>.v\t%Z3%u1\n\tbcnez\t%Z3%0",
					 "xvset<lasxne_v>.v\t%Z3%u1\n\tbcnez\t%Z3%0");
}
  [(set_attr "type" "simd_branch")
   (set_attr "mode" "<MODE>")])

;; loongson-asx.
(define_insn "lasx_vext2xv_h<u>_b<u>"
  [(set (match_operand:V16HI 0 "register_operand" "=f")
	(any_extend:V16HI
	  (vec_select:V16QI
	    (match_operand:V32QI 1 "register_operand" "f")
	    (parallel [(const_int 0) (const_int 1)
		       (const_int 2) (const_int 3)
		       (const_int 4) (const_int 5)
		       (const_int 6) (const_int 7)
		       (const_int 8) (const_int 9)
		       (const_int 10) (const_int 11)
		       (const_int 12) (const_int 13)
		       (const_int 14) (const_int 15)]))))]
  "ISA_HAS_LASX"
  "vext2xv.h<u>.b<u>\t%u0,%u1"
  [(set_attr "type" "simd_shift")
   (set_attr "mode" "V16HI")])

(define_insn "lasx_vext2xv_w<u>_h<u>"
  [(set (match_operand:V8SI 0 "register_operand" "=f")
	(any_extend:V8SI
	  (vec_select:V8HI
	    (match_operand:V16HI 1 "register_operand" "f")
	    (parallel [(const_int 0) (const_int 1)
		       (const_int 2) (const_int 3)
		       (const_int 4) (const_int 5)
		       (const_int 6) (const_int 7)]))))]
  "ISA_HAS_LASX"
  "vext2xv.w<u>.h<u>\t%u0,%u1"
  [(set_attr "type" "simd_shift")
   (set_attr "mode" "V8SI")])

(define_insn "lasx_vext2xv_d<u>_w<u>"
  [(set (match_operand:V4DI 0 "register_operand" "=f")
	(any_extend:V4DI
	  (vec_select:V4SI
	    (match_operand:V8SI 1 "register_operand" "f")
	    (parallel [(const_int 0) (const_int 1)
		       (const_int 2) (const_int 3)]))))]
  "ISA_HAS_LASX"
  "vext2xv.d<u>.w<u>\t%u0,%u1"
  [(set_attr "type" "simd_shift")
   (set_attr "mode" "V4DI")])

(define_insn "lasx_vext2xv_w<u>_b<u>"
  [(set (match_operand:V8SI 0 "register_operand" "=f")
	(any_extend:V8SI
	  (vec_select:V8QI
	   (match_operand:V32QI 1 "register_operand" "f")
	    (parallel [(const_int 0) (const_int 1)
		       (const_int 2) (const_int 3)
		       (const_int 4) (const_int 5)
		       (const_int 6) (const_int 7)]))))]
  "ISA_HAS_LASX"
  "vext2xv.w<u>.b<u>\t%u0,%u1"
  [(set_attr "type" "simd_shift")
   (set_attr "mode" "V8SI")])

(define_insn "lasx_vext2xv_d<u>_h<u>"
  [(set (match_operand:V4DI 0 "register_operand" "=f")
	(any_extend:V4DI
	  (vec_select:V4HI
	    (match_operand:V16HI 1 "register_operand" "f")
	    (parallel [(const_int 0) (const_int 1)
		       (const_int 2) (const_int 3)]))))]
  "ISA_HAS_LASX"
  "vext2xv.d<u>.h<u>\t%u0,%u1"
  [(set_attr "type" "simd_shift")
   (set_attr "mode" "V4DI")])

(define_insn "lasx_vext2xv_d<u>_b<u>"
  [(set (match_operand:V4DI 0 "register_operand" "=f")
	(any_extend:V4DI
	  (vec_select:V4QI
	    (match_operand:V32QI 1 "register_operand" "f")
	    (parallel [(const_int 0) (const_int 1)
		       (const_int 2) (const_int 3)]))))]
  "ISA_HAS_LASX"
  "vext2xv.d<u>.b<u>\t%u0,%u1"
  [(set_attr "type" "simd_shift")
   (set_attr "mode" "V4DI")])

;; Extend loongson-sx to loongson-asx.
(define_insn "andn<mode>3"
  [(set (match_operand:LASX 0 "register_operand" "=f")
	(and:LASX (not:LASX (match_operand:LASX 2 "register_operand" "f"))
			    (match_operand:LASX 1 "register_operand" "f")))]
  "ISA_HAS_LASX"
  "xvandn.v\t%u0,%u2,%u1"
  [(set_attr "type" "simd_logic")
   (set_attr "mode" "<MODE>")])

(define_insn "abs<mode>2"
  [(set (match_operand:ILASX 0 "register_operand" "=f")
	(abs:ILASX (match_operand:ILASX 1 "register_operand" "f")))]
  "ISA_HAS_LASX"
  "xvsigncov.<lasxfmt>\t%u0,%u1,%u1"
  [(set_attr "type" "simd_logic")
   (set_attr "mode" "<MODE>")])

(define_insn "neg<mode>2"
  [(set (match_operand:ILASX 0 "register_operand" "=f")
	(neg:ILASX (match_operand:ILASX 1 "register_operand" "f")))]
  "ISA_HAS_LASX"
  "xvneg.<lasxfmt>\t%u0,%u1"
  [(set_attr "type" "simd_logic")
   (set_attr "mode" "<MODE>")])

(define_insn "lasx_xvsllwil_s_<dlasxfmt>_<lasxfmt>"
  [(set (match_operand:<VDMODE256> 0 "register_operand" "=f")
	(unspec:<VDMODE256> [(match_operand:ILASX_WHB 1 "register_operand" "f")
			     (match_operand 2 "const_<bitimm256>_operand" "")]
			    UNSPEC_LASX_XVSLLWIL_S))]
  "ISA_HAS_LASX"
  "xvsllwil.<dlasxfmt>.<lasxfmt>\t%u0,%u1,%2"
  [(set_attr "type" "simd_shift")
   (set_attr "mode" "<MODE>")])

(define_insn "lasx_xvsllwil_u_<dlasxfmt_u>_<lasxfmt_u>"
  [(set (match_operand:<VDMODE256> 0 "register_operand" "=f")
	(unspec:<VDMODE256> [(match_operand:ILASX_WHB 1 "register_operand" "f")
			     (match_operand 2 "const_<bitimm256>_operand" "")]
			    UNSPEC_LASX_XVSLLWIL_U))]
  "ISA_HAS_LASX"
  "xvsllwil.<dlasxfmt_u>.<lasxfmt_u>\t%u0,%u1,%2"
  [(set_attr "type" "simd_shift")
   (set_attr "mode" "<MODE>")])

(define_insn "lasx_xvsran_<hlasxfmt>_<lasxfmt>"
  [(set (match_operand:<VHSMODE256> 0 "register_operand" "=f")
	(unspec:<VHSMODE256> [(match_operand:ILASX_DWH 1 "register_operand" "f")
			      (match_operand:ILASX_DWH 2 "register_operand" "f")]
			     UNSPEC_LASX_XVSRAN))]
  "ISA_HAS_LASX"
  "xvsran.<hlasxfmt>.<lasxfmt>\t%u0,%u1,%u2"
  [(set_attr "type" "simd_int_arith")
   (set_attr "mode" "<MODE>")])

(define_insn "lasx_xvssran_s_<hlasxfmt>_<lasxfmt>"
  [(set (match_operand:<VHSMODE256> 0 "register_operand" "=f")
	(unspec:<VHSMODE256> [(match_operand:ILASX_DWH 1 "register_operand" "f")
			      (match_operand:ILASX_DWH 2 "register_operand" "f")]
			     UNSPEC_LASX_XVSSRAN_S))]
  "ISA_HAS_LASX"
  "xvssran.<hlasxfmt>.<lasxfmt>\t%u0,%u1,%u2"
  [(set_attr "type" "simd_int_arith")
   (set_attr "mode" "<MODE>")])

(define_insn "lasx_xvssran_u_<hlasxfmt_u>_<lasxfmt>"
  [(set (match_operand:<VHSMODE256> 0 "register_operand" "=f")
	(unspec:<VHSMODE256> [(match_operand:ILASX_DWH 1 "register_operand" "f")
			      (match_operand:ILASX_DWH 2 "register_operand" "f")]
			     UNSPEC_LASX_XVSSRAN_U))]
  "ISA_HAS_LASX"
  "xvssran.<hlasxfmt_u>.<lasxfmt>\t%u0,%u1,%u2"
  [(set_attr "type" "simd_int_arith")
   (set_attr "mode" "<MODE>")])

(define_insn "lasx_xvsrarn_<hlasxfmt>_<lasxfmt>"
  [(set (match_operand:<VHSMODE256> 0 "register_operand" "=f")
	(unspec:<VHSMODE256> [(match_operand:ILASX_DWH 1 "register_operand" "f")
			      (match_operand:ILASX_DWH 2 "register_operand" "f")]
			     UNSPEC_LASX_XVSRARN))]
  "ISA_HAS_LASX"
  "xvsrarn.<hlasxfmt>.<lasxfmt>\t%u0,%u1,%u2"
  [(set_attr "type" "simd_int_arith")
   (set_attr "mode" "<MODE>")])

(define_insn "lasx_xvssrarn_s_<hlasxfmt>_<lasxfmt>"
  [(set (match_operand:<VHSMODE256> 0 "register_operand" "=f")
	(unspec:<VHSMODE256> [(match_operand:ILASX_DWH 1 "register_operand" "f")
			      (match_operand:ILASX_DWH 2 "register_operand" "f")]
			     UNSPEC_LASX_XVSSRARN_S))]
  "ISA_HAS_LASX"
  "xvssrarn.<hlasxfmt>.<lasxfmt>\t%u0,%u1,%u2"
  [(set_attr "type" "simd_int_arith")
   (set_attr "mode" "<MODE>")])

(define_insn "lasx_xvssrarn_u_<hlasxfmt_u>_<lasxfmt>"
  [(set (match_operand:<VHSMODE256> 0 "register_operand" "=f")
	(unspec:<VHSMODE256> [(match_operand:ILASX_DWH 1 "register_operand" "f")
			      (match_operand:ILASX_DWH 2 "register_operand" "f")]
			     UNSPEC_LASX_XVSSRARN_U))]
  "ISA_HAS_LASX"
  "xvssrarn.<hlasxfmt_u>.<lasxfmt>\t%u0,%u1,%u2"
  [(set_attr "type" "simd_int_arith")
   (set_attr "mode" "<MODE>")])

(define_insn "lasx_xvsrln_<hlasxfmt>_<lasxfmt>"
  [(set (match_operand:<VHSMODE256> 0 "register_operand" "=f")
	(unspec:<VHSMODE256> [(match_operand:ILASX_DWH 1 "register_operand" "f")
			      (match_operand:ILASX_DWH 2 "register_operand" "f")]
			     UNSPEC_LASX_XVSRLN))]
  "ISA_HAS_LASX"
  "xvsrln.<hlasxfmt>.<lasxfmt>\t%u0,%u1,%u2"
  [(set_attr "type" "simd_int_arith")
   (set_attr "mode" "<MODE>")])

(define_insn "lasx_xvssrln_u_<hlasxfmt_u>_<lasxfmt>"
  [(set (match_operand:<VHSMODE256> 0 "register_operand" "=f")
	(unspec:<VHSMODE256> [(match_operand:ILASX_DWH 1 "register_operand" "f")
			      (match_operand:ILASX_DWH 2 "register_operand" "f")]
			     UNSPEC_LASX_XVSSRLN_U))]
  "ISA_HAS_LASX"
  "xvssrln.<hlasxfmt_u>.<lasxfmt>\t%u0,%u1,%u2"
  [(set_attr "type" "simd_int_arith")
   (set_attr "mode" "<MODE>")])

(define_insn "lasx_xvsrlrn_<hlasxfmt>_<lasxfmt>"
  [(set (match_operand:<VHSMODE256> 0 "register_operand" "=f")
	(unspec:<VHSMODE256> [(match_operand:ILASX_DWH 1 "register_operand" "f")
			      (match_operand:ILASX_DWH 2 "register_operand" "f")]
			     UNSPEC_LASX_XVSRLRN))]
  "ISA_HAS_LASX"
  "xvsrlrn.<hlasxfmt>.<lasxfmt>\t%u0,%u1,%u2"
  [(set_attr "type" "simd_int_arith")
   (set_attr "mode" "<MODE>")])

(define_insn "lasx_xvssrlrn_u_<hlasxfmt_u>_<lasxfmt>"
  [(set (match_operand:<VHSMODE256> 0 "register_operand" "=f")
	(unspec:<VHSMODE256> [(match_operand:ILASX_DWH 1 "register_operand" "f")
			      (match_operand:ILASX_DWH 2 "register_operand" "f")]
			     UNSPEC_LASX_XVSSRLRN_U))]
  "ISA_HAS_LASX"
  "xvssrlrn.<hlasxfmt_u>.<lasxfmt>\t%u0,%u1,%u2"
  [(set_attr "type" "simd_int_arith")
   (set_attr "mode" "<MODE>")])

(define_insn "lasx_xvfrstpi_<lasxfmt>"
  [(set (match_operand:ILASX_HB 0 "register_operand" "=f")
	(unspec:ILASX_HB [(match_operand:ILASX_HB 1 "register_operand" "0")
			  (match_operand:ILASX_HB 2 "register_operand" "f")
			  (match_operand 3 "const_uimm5_operand" "")]
			 UNSPEC_LASX_XVFRSTPI))]
  "ISA_HAS_LASX"
  "xvfrstpi.<lasxfmt>\t%u0,%u2,%3"
  [(set_attr "type" "simd_shift")
   (set_attr "mode" "<MODE>")])

(define_insn "lasx_xvfrstp_<lasxfmt>"
  [(set (match_operand:ILASX_HB 0 "register_operand" "=f")
	(unspec:ILASX_HB [(match_operand:ILASX_HB 1 "register_operand" "0")
			  (match_operand:ILASX_HB 2 "register_operand" "f")
			  (match_operand:ILASX_HB 3 "register_operand" "f")]
			 UNSPEC_LASX_XVFRSTP))]
  "ISA_HAS_LASX"
  "xvfrstp.<lasxfmt>\t%u0,%u2,%u3"
  [(set_attr "type" "simd_int_arith")
   (set_attr "mode" "<MODE>")])

(define_insn "lasx_xvshuf4i_d"
  [(set (match_operand:V4DI 0 "register_operand" "=f")
	(unspec:V4DI [(match_operand:V4DI 1 "register_operand" "0")
		      (match_operand:V4DI 2 "register_operand" "f")
		      (match_operand 3 "const_uimm8_operand")]
		     UNSPEC_LASX_XVSHUF4I))]
  "ISA_HAS_LASX"
  "xvshuf4i.d\t%u0,%u2,%3"
  [(set_attr "type" "simd_sld")
   (set_attr "mode" "V4DI")])

(define_insn "lasx_xvbsrl_<lasxfmt>"
  [(set (match_operand:ILASX 0 "register_operand" "=f")
	(unspec:ILASX [(match_operand:ILASX 1 "register_operand" "f")
		       (match_operand 2 "const_uimm5_operand" "")]
		      UNSPEC_LASX_XVBSRL_V))]
  "ISA_HAS_LASX"
  "xvbsrl.v\t%u0,%u1,%2"
  [(set_attr "type" "simd_shift")
   (set_attr "mode" "<MODE>")])

(define_insn "lasx_xvbsll_<lasxfmt>"
  [(set (match_operand:ILASX 0 "register_operand" "=f")
	(unspec:ILASX [(match_operand:ILASX 1 "register_operand" "f")
		       (match_operand 2 "const_uimm5_operand" "")]
		      UNSPEC_LASX_XVBSLL_V))]
  "ISA_HAS_LASX"
  "xvbsll.v\t%u0,%u1,%2"
  [(set_attr "type" "simd_shift")
   (set_attr "mode" "<MODE>")])

(define_insn "lasx_xvextrins_<lasxfmt>"
  [(set (match_operand:ILASX 0 "register_operand" "=f")
	(unspec:ILASX [(match_operand:ILASX 1 "register_operand" "0")
		       (match_operand:ILASX 2 "register_operand" "f")
		       (match_operand 3 "const_uimm8_operand" "")]
		      UNSPEC_LASX_XVEXTRINS))]
  "ISA_HAS_LASX"
  "xvextrins.<lasxfmt>\t%u0,%u2,%3"
  [(set_attr "type" "simd_shift")
   (set_attr "mode" "<MODE>")])

(define_insn "lasx_xvmskltz_<lasxfmt>"
  [(set (match_operand:ILASX 0 "register_operand" "=f")
	(unspec:ILASX [(match_operand:ILASX 1 "register_operand" "f")]
		      UNSPEC_LASX_XVMSKLTZ))]
  "ISA_HAS_LASX"
  "xvmskltz.<lasxfmt>\t%u0,%u1"
  [(set_attr "type" "simd_shift")
   (set_attr "mode" "<MODE>")])

(define_insn "lasx_xvsigncov_<lasxfmt>"
  [(set (match_operand:ILASX 0 "register_operand" "=f")
	(unspec:ILASX [(match_operand:ILASX 1 "register_operand" "f")
		       (match_operand:ILASX 2 "register_operand" "f")]
		      UNSPEC_LASX_XVSIGNCOV))]
  "ISA_HAS_LASX"
  "xvsigncov.<lasxfmt>\t%u0,%u1,%u2"
  [(set_attr "type" "simd_int_arith")
   (set_attr "mode" "<MODE>")])

(define_expand "copysign<mode>3"
  [(set (match_dup 4)
	(and:FLASX
	  (not:FLASX (match_dup 3))
	  (match_operand:FLASX 1 "register_operand")))
   (set (match_dup 5)
	(and:FLASX (match_dup 3)
		   (match_operand:FLASX 2 "reg_or_vector_same_val_operand")))
   (set (match_operand:FLASX 0 "register_operand")
	(ior:FLASX (match_dup 4) (match_dup 5)))]
  "ISA_HAS_LASX"
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
	  operands[0] = lowpart_subreg (<VIMODE256>mode, operands[0],
					<MODE>mode);
	  operands[1] = lowpart_subreg (<VIMODE256>mode, operands[1],
					<MODE>mode);
	  emit_insn (gen_lasx_xvbitseti_<lasxfmt> (operands[0],
						   operands[1], n));
	  DONE;
	}
    }

  operands[2] = force_reg (<MODE>mode, operands[2]);
  operands[3] = loongarch_build_signbit_mask (<MODE>mode, 1, 0);

  operands[4] = gen_reg_rtx (<MODE>mode);
  operands[5] = gen_reg_rtx (<MODE>mode);
})

(define_expand "xorsign<mode>3"
  [(set (match_dup 4)
    (and:FLASX (match_dup 3)
        (match_operand:FLASX 2 "register_operand")))
   (set (match_operand:FLASX 0 "register_operand")
    (xor:FLASX (match_dup 4)
         (match_operand:FLASX 1 "register_operand")))]
  "ISA_HAS_LASX"
{
  operands[3] = loongarch_build_signbit_mask (<MODE>mode, 1, 0);

  operands[4] = gen_reg_rtx (<MODE>mode);
})


(define_insn "absv4df2"
  [(set (match_operand:V4DF 0 "register_operand" "=f")
	(abs:V4DF (match_operand:V4DF 1 "register_operand" "f")))]
  "ISA_HAS_LASX"
  "xvbitclri.d\t%u0,%u1,63"
  [(set_attr "type" "simd_logic")
   (set_attr "mode" "V4DF")])

(define_insn "absv8sf2"
  [(set (match_operand:V8SF 0 "register_operand" "=f")
	(abs:V8SF (match_operand:V8SF 1 "register_operand" "f")))]
  "ISA_HAS_LASX"
  "xvbitclri.w\t%u0,%u1,31"
  [(set_attr "type" "simd_logic")
   (set_attr "mode" "V8SF")])

(define_insn "xvfmadd<mode>4"
  [(set (match_operand:FLASX 0 "register_operand" "=f")
	(fma:FLASX (match_operand:FLASX 1 "register_operand" "f")
		   (match_operand:FLASX 2 "register_operand" "f")
		   (match_operand:FLASX 3 "register_operand" "f")))]
  "ISA_HAS_LASX"
  "xvfmadd.<flasxfmt>\t%u0,%u1,$u2,%u3"
  [(set_attr "type" "simd_fmadd")
   (set_attr "mode" "<MODE>")])

(define_insn "fms<mode>4"
  [(set (match_operand:FLASX 0 "register_operand" "=f")
	(fma:FLASX (match_operand:FLASX 1 "register_operand" "f")
		   (match_operand:FLASX 2 "register_operand" "f")
		   (neg:FLASX (match_operand:FLASX 3 "register_operand" "f"))))]
  "ISA_HAS_LASX"
  "xvfmsub.<flasxfmt>\t%u0,%u1,%u2,%u3"
  [(set_attr "type" "simd_fmadd")
   (set_attr "mode" "<MODE>")])

(define_insn "xvfnmsub<mode>4_nmsub4"
  [(set (match_operand:FLASX 0 "register_operand" "=f")
	(neg:FLASX
	  (fma:FLASX
	    (match_operand:FLASX 1 "register_operand" "f")
	    (match_operand:FLASX 2 "register_operand" "f")
	    (neg:FLASX (match_operand:FLASX 3 "register_operand" "f")))))]
  "ISA_HAS_LASX"
  "xvfnmsub.<flasxfmt>\t%u0,%u1,%u2,%u3"
  [(set_attr "type" "simd_fmadd")
   (set_attr "mode" "<MODE>")])


(define_insn "xvfnmadd<mode>4_nmadd4"
  [(set (match_operand:FLASX 0 "register_operand" "=f")
	(neg:FLASX
	  (fma:FLASX
	    (match_operand:FLASX 1 "register_operand" "f")
	    (match_operand:FLASX 2 "register_operand" "f")
	    (match_operand:FLASX 3 "register_operand" "f"))))]
  "ISA_HAS_LASX"
  "xvfnmadd.<flasxfmt>\t%u0,%u1,%u2,%u3"
  [(set_attr "type" "simd_fmadd")
   (set_attr "mode" "<MODE>")])

(define_insn "lasx_xvftint_w_d"
  [(set (match_operand:V8SI 0 "register_operand" "=f")
	(unspec:V8SI [(match_operand:V4DF 1 "register_operand" "f")
		      (match_operand:V4DF 2 "register_operand" "f")]
		     UNSPEC_LASX_XVFTINT_W_D))]
  "ISA_HAS_LASX"
  "xvftint.w.d\t%u0,%u1,%u2"
  [(set_attr "type" "simd_int_arith")
   (set_attr "mode" "V4DF")])

(define_insn "lasx_xvffint_s_l"
  [(set (match_operand:V8SF 0 "register_operand" "=f")
	(unspec:V8SF [(match_operand:V4DI 1 "register_operand" "f")
		      (match_operand:V4DI 2 "register_operand" "f")]
		     UNSPEC_LASX_XVFFINT_S_L))]
  "ISA_HAS_LASX"
  "xvffint.s.l\t%u0,%u1,%u2"
  [(set_attr "type" "simd_int_arith")
   (set_attr "mode" "V4DI")])

(define_insn "lasx_xvftintrz_w_d"
  [(set (match_operand:V8SI 0 "register_operand" "=f")
	(unspec:V8SI [(match_operand:V4DF 1 "register_operand" "f")
		      (match_operand:V4DF 2 "register_operand" "f")]
		     UNSPEC_LASX_XVFTINTRZ_W_D))]
  "ISA_HAS_LASX"
  "xvftintrz.w.d\t%u0,%u1,%u2"
  [(set_attr "type" "simd_int_arith")
   (set_attr "mode" "V4DF")])

(define_insn "lasx_xvftintrp_w_d"
  [(set (match_operand:V8SI 0 "register_operand" "=f")
	(unspec:V8SI [(match_operand:V4DF 1 "register_operand" "f")
		      (match_operand:V4DF 2 "register_operand" "f")]
		     UNSPEC_LASX_XVFTINTRP_W_D))]
  "ISA_HAS_LASX"
  "xvftintrp.w.d\t%u0,%u1,%u2"
  [(set_attr "type" "simd_int_arith")
   (set_attr "mode" "V4DF")])

(define_insn "lasx_xvftintrm_w_d"
  [(set (match_operand:V8SI 0 "register_operand" "=f")
	(unspec:V8SI [(match_operand:V4DF 1 "register_operand" "f")
		      (match_operand:V4DF 2 "register_operand" "f")]
		     UNSPEC_LASX_XVFTINTRM_W_D))]
  "ISA_HAS_LASX"
  "xvftintrm.w.d\t%u0,%u1,%u2"
  [(set_attr "type" "simd_int_arith")
   (set_attr "mode" "V4DF")])

(define_insn "lasx_xvftintrne_w_d"
  [(set (match_operand:V8SI 0 "register_operand" "=f")
	(unspec:V8SI [(match_operand:V4DF 1 "register_operand" "f")
		      (match_operand:V4DF 2 "register_operand" "f")]
		     UNSPEC_LASX_XVFTINTRNE_W_D))]
  "ISA_HAS_LASX"
  "xvftintrne.w.d\t%u0,%u1,%u2"
  [(set_attr "type" "simd_int_arith")
   (set_attr "mode" "V4DF")])

(define_insn "lasx_xvftinth_l_s"
  [(set (match_operand:V4DI 0 "register_operand" "=f")
	(unspec:V4DI [(match_operand:V8SF 1 "register_operand" "f")]
		     UNSPEC_LASX_XVFTINTH_L_S))]
  "ISA_HAS_LASX"
  "xvftinth.l.s\t%u0,%u1"
  [(set_attr "type" "simd_shift")
   (set_attr "mode" "V8SF")])

(define_insn "lasx_xvftintl_l_s"
  [(set (match_operand:V4DI 0 "register_operand" "=f")
	(unspec:V4DI [(match_operand:V8SF 1 "register_operand" "f")]
		     UNSPEC_LASX_XVFTINTL_L_S))]
  "ISA_HAS_LASX"
  "xvftintl.l.s\t%u0,%u1"
  [(set_attr "type" "simd_shift")
   (set_attr "mode" "V8SF")])

(define_insn "lasx_xvffinth_d_w"
  [(set (match_operand:V4DF 0 "register_operand" "=f")
	(unspec:V4DF [(match_operand:V8SI 1 "register_operand" "f")]
		     UNSPEC_LASX_XVFFINTH_D_W))]
  "ISA_HAS_LASX"
  "xvffinth.d.w\t%u0,%u1"
  [(set_attr "type" "simd_shift")
   (set_attr "mode" "V8SI")])

(define_insn "lasx_xvffintl_d_w"
  [(set (match_operand:V4DF 0 "register_operand" "=f")
	(unspec:V4DF [(match_operand:V8SI 1 "register_operand" "f")]
		     UNSPEC_LASX_XVFFINTL_D_W))]
  "ISA_HAS_LASX"
  "xvffintl.d.w\t%u0,%u1"
  [(set_attr "type" "simd_shift")
   (set_attr "mode" "V8SI")])

(define_insn "lasx_xvftintrzh_l_s"
  [(set (match_operand:V4DI 0 "register_operand" "=f")
	(unspec:V4DI [(match_operand:V8SF 1 "register_operand" "f")]
		     UNSPEC_LASX_XVFTINTRZH_L_S))]
  "ISA_HAS_LASX"
  "xvftintrzh.l.s\t%u0,%u1"
  [(set_attr "type" "simd_shift")
   (set_attr "mode" "V8SF")])

(define_insn "lasx_xvftintrzl_l_s"
  [(set (match_operand:V4DI 0 "register_operand" "=f")
	(unspec:V4DI [(match_operand:V8SF 1 "register_operand" "f")]
		     UNSPEC_LASX_XVFTINTRZL_L_S))]
  "ISA_HAS_LASX"
  "xvftintrzl.l.s\t%u0,%u1"
  [(set_attr "type" "simd_shift")
   (set_attr "mode" "V4SF")])

(define_insn "lasx_xvftintrph_l_s"
  [(set (match_operand:V4DI 0 "register_operand" "=f")
	(unspec:V4DI [(match_operand:V8SF 1 "register_operand" "f")]
		     UNSPEC_LASX_XVFTINTRPH_L_S))]
  "ISA_HAS_LASX"
  "xvftintrph.l.s\t%u0,%u1"
  [(set_attr "type" "simd_shift")
   (set_attr "mode" "V4SF")])

(define_insn "lasx_xvftintrpl_l_s"
  [(set (match_operand:V4DI 0 "register_operand" "=f")
	(unspec:V4DI [(match_operand:V8SF 1 "register_operand" "f")]
		     UNSPEC_LASX_XVFTINTRPL_L_S))]
  "ISA_HAS_LASX"
  "xvftintrpl.l.s\t%u0,%u1"
  [(set_attr "type" "simd_shift")
   (set_attr "mode" "V8SF")])

(define_insn "lasx_xvftintrmh_l_s"
  [(set (match_operand:V4DI 0 "register_operand" "=f")
	(unspec:V4DI [(match_operand:V8SF 1 "register_operand" "f")]
		     UNSPEC_LASX_XVFTINTRMH_L_S))]
  "ISA_HAS_LASX"
  "xvftintrmh.l.s\t%u0,%u1"
  [(set_attr "type" "simd_shift")
   (set_attr "mode" "V8SF")])

(define_insn "lasx_xvftintrml_l_s"
  [(set (match_operand:V4DI 0 "register_operand" "=f")
	(unspec:V4DI [(match_operand:V8SF 1 "register_operand" "f")]
		     UNSPEC_LASX_XVFTINTRML_L_S))]
  "ISA_HAS_LASX"
  "xvftintrml.l.s\t%u0,%u1"
  [(set_attr "type" "simd_shift")
   (set_attr "mode" "V8SF")])

(define_insn "lasx_xvftintrneh_l_s"
  [(set (match_operand:V4DI 0 "register_operand" "=f")
	(unspec:V4DI [(match_operand:V8SF 1 "register_operand" "f")]
		     UNSPEC_LASX_XVFTINTRNEH_L_S))]
  "ISA_HAS_LASX"
  "xvftintrneh.l.s\t%u0,%u1"
  [(set_attr "type" "simd_shift")
   (set_attr "mode" "V8SF")])

(define_insn "lasx_xvftintrnel_l_s"
  [(set (match_operand:V4DI 0 "register_operand" "=f")
	(unspec:V4DI [(match_operand:V8SF 1 "register_operand" "f")]
		     UNSPEC_LASX_XVFTINTRNEL_L_S))]
  "ISA_HAS_LASX"
  "xvftintrnel.l.s\t%u0,%u1"
  [(set_attr "type" "simd_shift")
   (set_attr "mode" "V8SF")])

;; Offset load and broadcast
(define_expand "lasx_xvldrepl_<lasxfmt_f>"
  [(match_operand:LASX 0 "register_operand")
   (match_operand 2 "aq12<lasxfmt>_operand")
   (match_operand 1 "pmode_register_operand")]
  "ISA_HAS_LASX"
{
  emit_insn (gen_lasx_xvldrepl_<lasxfmt_f>_insn
	     (operands[0], operands[1], operands[2]));
  DONE;
})

(define_insn "lasx_xvldrepl_<lasxfmt_f>_insn"
  [(set (match_operand:LASX 0 "register_operand" "=f")
	(vec_duplicate:LASX
	  (mem:<UNITMODE> (plus:DI (match_operand:DI 1 "register_operand" "r")
				   (match_operand 2 "aq12<lasxfmt>_operand")))))]
  "ISA_HAS_LASX"
{
  return "xvldrepl.<lasxfmt>\t%u0,%1,%2";
}
  [(set_attr "type" "simd_load")
   (set_attr "mode" "<MODE>")
   (set_attr "length" "4")])

;; Offset is "0"
(define_insn "lasx_xvldrepl_<lasxfmt_f>_insn_0"
  [(set (match_operand:LASX 0 "register_operand" "=f")
    (vec_duplicate:LASX
      (mem:<UNITMODE> (match_operand:DI 1 "register_operand" "r"))))]
  "ISA_HAS_LASX"
{
    return "xvldrepl.<lasxfmt>\t%u0,%1,0";
}
  [(set_attr "type" "simd_load")
   (set_attr "mode" "<MODE>")
   (set_attr "length" "4")])

;;XVADDWEV.H.B   XVSUBWEV.H.B   XVMULWEV.H.B
;;XVADDWEV.H.BU  XVSUBWEV.H.BU  XVMULWEV.H.BU
(define_insn "lasx_xv<optab>wev_h_b<u>"
  [(set (match_operand:V16HI 0 "register_operand" "=f")
	(addsubmul:V16HI
	  (any_extend:V16HI
	    (vec_select:V16QI
	      (match_operand:V32QI 1 "register_operand" "%f")
	      (parallel [(const_int 0) (const_int 2)
			 (const_int 4) (const_int 6)
			 (const_int 8) (const_int 10)
			 (const_int 12) (const_int 14)
			 (const_int 16) (const_int 18)
			 (const_int 20) (const_int 22)
			 (const_int 24) (const_int 26)
			 (const_int 28) (const_int 30)])))
	  (any_extend:V16HI
	    (vec_select:V16QI
	      (match_operand:V32QI 2 "register_operand" "f")
	      (parallel [(const_int 0) (const_int 2)
			 (const_int 4) (const_int 6)
			 (const_int 8) (const_int 10)
			 (const_int 12) (const_int 14)
			 (const_int 16) (const_int 18)
			 (const_int 20) (const_int 22)
			 (const_int 24) (const_int 26)
			 (const_int 28) (const_int 30)])))))]
  "ISA_HAS_LASX"
  "xv<optab>wev.h.b<u>\t%u0,%u1,%u2"
  [(set_attr "type" "simd_int_arith")
   (set_attr "mode" "V16HI")])

;;XVADDWEV.W.H   XVSUBWEV.W.H   XVMULWEV.W.H
;;XVADDWEV.W.HU  XVSUBWEV.W.HU  XVMULWEV.W.HU
(define_insn "lasx_xv<optab>wev_w_h<u>"
  [(set (match_operand:V8SI 0 "register_operand" "=f")
	(addsubmul:V8SI
	  (any_extend:V8SI
	    (vec_select:V8HI
	      (match_operand:V16HI 1 "register_operand" "%f")
	      (parallel [(const_int 0) (const_int 2)
			 (const_int 4) (const_int 6)
			 (const_int 8) (const_int 10)
			 (const_int 12) (const_int 14)])))
	  (any_extend:V8SI
	    (vec_select:V8HI
	      (match_operand:V16HI 2 "register_operand" "f")
	      (parallel [(const_int 0) (const_int 2)
			 (const_int 4) (const_int 6)
			 (const_int 8) (const_int 10)
			 (const_int 12) (const_int 14)])))))]
  "ISA_HAS_LASX"
  "xv<optab>wev.w.h<u>\t%u0,%u1,%u2"
  [(set_attr "type" "simd_int_arith")
   (set_attr "mode" "V8SI")])

;;XVADDWEV.D.W   XVSUBWEV.D.W   XVMULWEV.D.W
;;XVADDWEV.D.WU  XVSUBWEV.D.WU  XVMULWEV.D.WU
(define_insn "lasx_xv<optab>wev_d_w<u>"
  [(set (match_operand:V4DI 0 "register_operand" "=f")
	(addsubmul:V4DI
	  (any_extend:V4DI
	    (vec_select:V4SI
	      (match_operand:V8SI 1 "register_operand" "%f")
	      (parallel [(const_int 0) (const_int 2)
			 (const_int 4) (const_int 6)])))
	  (any_extend:V4DI
	    (vec_select:V4SI
	      (match_operand:V8SI 2 "register_operand" "f")
	      (parallel [(const_int 0) (const_int 2)
			 (const_int 4) (const_int 6)])))))]
  "ISA_HAS_LASX"
  "xv<optab>wev.d.w<u>\t%u0,%u1,%u2"
  [(set_attr "type" "simd_int_arith")
   (set_attr "mode" "V4DI")])

;;XVADDWEV.Q.D
;;TODO2
(define_insn "lasx_xvaddwev_q_d"
  [(set (match_operand:V4DI 0 "register_operand" "=f")
	(unspec:V4DI [(match_operand:V4DI 1 "register_operand" "f")
		      (match_operand:V4DI 2 "register_operand" "f")]
		     UNSPEC_LASX_XVADDWEV))]
  "ISA_HAS_LASX"
  "xvaddwev.q.d\t%u0,%u1,%u2"
  [(set_attr "type" "simd_int_arith")
   (set_attr "mode" "V4DI")])

;;XVSUBWEV.Q.D
;;TODO2
(define_insn "lasx_xvsubwev_q_d"
  [(set (match_operand:V4DI 0 "register_operand" "=f")
	(unspec:V4DI [(match_operand:V4DI 1 "register_operand" "f")
		      (match_operand:V4DI 2 "register_operand" "f")]
		     UNSPEC_LASX_XVSUBWEV))]
  "ISA_HAS_LASX"
  "xvsubwev.q.d\t%u0,%u1,%u2"
  [(set_attr "type" "simd_int_arith")
   (set_attr "mode" "V4DI")])

;;XVMULWEV.Q.D
;;TODO2
(define_insn "lasx_xvmulwev_q_d"
  [(set (match_operand:V4DI 0 "register_operand" "=f")
	(unspec:V4DI [(match_operand:V4DI 1 "register_operand" "f")
		      (match_operand:V4DI 2 "register_operand" "f")]
		     UNSPEC_LASX_XVMULWEV))]
  "ISA_HAS_LASX"
  "xvmulwev.q.d\t%u0,%u1,%u2"
  [(set_attr "type" "simd_int_arith")
   (set_attr "mode" "V4DI")])


;;XVADDWOD.H.B   XVSUBWOD.H.B   XVMULWOD.H.B
;;XVADDWOD.H.BU  XVSUBWOD.H.BU  XVMULWOD.H.BU
(define_insn "lasx_xv<optab>wod_h_b<u>"
  [(set (match_operand:V16HI 0 "register_operand" "=f")
	(addsubmul:V16HI
	  (any_extend:V16HI
	    (vec_select:V16QI
	      (match_operand:V32QI 1 "register_operand" "%f")
	      (parallel [(const_int 1) (const_int 3)
			 (const_int 5) (const_int 7)
			 (const_int 9) (const_int 11)
			 (const_int 13) (const_int 15)
			 (const_int 17) (const_int 19)
			 (const_int 21) (const_int 23)
			 (const_int 25) (const_int 27)
			 (const_int 29) (const_int 31)])))
	  (any_extend:V16HI
	    (vec_select:V16QI
	      (match_operand:V32QI 2 "register_operand" "f")
	      (parallel [(const_int 1) (const_int 3)
			 (const_int 5) (const_int 7)
			 (const_int 9) (const_int 11)
			 (const_int 13) (const_int 15)
			 (const_int 17) (const_int 19)
			 (const_int 21) (const_int 23)
			 (const_int 25) (const_int 27)
			 (const_int 29) (const_int 31)])))))]
  "ISA_HAS_LASX"
  "xv<optab>wod.h.b<u>\t%u0,%u1,%u2"
  [(set_attr "type" "simd_int_arith")
   (set_attr "mode" "V16HI")])

;;XVADDWOD.W.H   XVSUBWOD.W.H   XVMULWOD.W.H
;;XVADDWOD.W.HU  XVSUBWOD.W.HU  XVMULWOD.W.HU
(define_insn "lasx_xv<optab>wod_w_h<u>"
  [(set (match_operand:V8SI 0 "register_operand" "=f")
	(addsubmul:V8SI
	  (any_extend:V8SI
	    (vec_select:V8HI
	      (match_operand:V16HI 1 "register_operand" "%f")
	      (parallel [(const_int 1) (const_int 3)
			 (const_int 5) (const_int 7)
			 (const_int 9) (const_int 11)
			 (const_int 13) (const_int 15)])))
	  (any_extend:V8SI
	    (vec_select:V8HI
	      (match_operand:V16HI 2 "register_operand" "f")
	      (parallel [(const_int 1) (const_int 3)
			 (const_int 5) (const_int 7)
			 (const_int 9) (const_int 11)
			 (const_int 13) (const_int 15)])))))]
  "ISA_HAS_LASX"
  "xv<optab>wod.w.h<u>\t%u0,%u1,%u2"
  [(set_attr "type" "simd_int_arith")
   (set_attr "mode" "V8SI")])


;;XVADDWOD.D.W   XVSUBWOD.D.W   XVMULWOD.D.W
;;XVADDWOD.D.WU  XVSUBWOD.D.WU  XVMULWOD.D.WU
(define_insn "lasx_xv<optab>wod_d_w<u>"
  [(set (match_operand:V4DI 0 "register_operand" "=f")
	(addsubmul:V4DI
	  (any_extend:V4DI
	    (vec_select:V4SI
	      (match_operand:V8SI 1 "register_operand" "%f")
	      (parallel [(const_int 1) (const_int 3)
			 (const_int 5) (const_int 7)])))
	  (any_extend:V4DI
	    (vec_select:V4SI
	      (match_operand:V8SI 2 "register_operand" "f")
	      (parallel [(const_int 1) (const_int 3)
			 (const_int 5) (const_int 7)])))))]
  "ISA_HAS_LASX"
  "xv<optab>wod.d.w<u>\t%u0,%u1,%u2"
  [(set_attr "type" "simd_int_arith")
   (set_attr "mode" "V4DI")])

;;XVADDWOD.Q.D
;;TODO2
(define_insn "lasx_xvaddwod_q_d"
  [(set (match_operand:V4DI 0 "register_operand" "=f")
	(unspec:V4DI [(match_operand:V4DI 1 "register_operand" "f")
		      (match_operand:V4DI 2 "register_operand" "f")]
		     UNSPEC_LASX_XVADDWOD))]
  "ISA_HAS_LASX"
  "xvaddwod.q.d\t%u0,%u1,%u2"
  [(set_attr "type" "simd_int_arith")
   (set_attr "mode" "V4DI")])

;;XVSUBWOD.Q.D
;;TODO2
(define_insn "lasx_xvsubwod_q_d"
  [(set (match_operand:V4DI 0 "register_operand" "=f")
	(unspec:V4DI [(match_operand:V4DI 1 "register_operand" "f")
		      (match_operand:V4DI 2 "register_operand" "f")]
		     UNSPEC_LASX_XVSUBWOD))]
  "ISA_HAS_LASX"
  "xvsubwod.q.d\t%u0,%u1,%u2"
  [(set_attr "type" "simd_int_arith")
   (set_attr "mode" "V4DI")])

;;XVMULWOD.Q.D
;;TODO2
(define_insn "lasx_xvmulwod_q_d"
  [(set (match_operand:V4DI 0 "register_operand" "=f")
	(unspec:V4DI [(match_operand:V4DI 1 "register_operand" "f")
		      (match_operand:V4DI 2 "register_operand" "f")]
		     UNSPEC_LASX_XVMULWOD))]
  "ISA_HAS_LASX"
  "xvmulwod.q.d\t%u0,%u1,%u2"
  [(set_attr "type" "simd_int_arith")
   (set_attr "mode" "V4DI")])

;;XVADDWEV.Q.DU
;;TODO2
(define_insn "lasx_xvaddwev_q_du"
  [(set (match_operand:V4DI 0 "register_operand" "=f")
	(unspec:V4DI [(match_operand:V4DI 1 "register_operand" "f")
		      (match_operand:V4DI 2 "register_operand" "f")]
		     UNSPEC_LASX_XVADDWEV2))]
  "ISA_HAS_LASX"
  "xvaddwev.q.du\t%u0,%u1,%u2"
  [(set_attr "type" "simd_int_arith")
   (set_attr "mode" "V4DI")])

;;XVSUBWEV.Q.DU
;;TODO2
(define_insn "lasx_xvsubwev_q_du"
  [(set (match_operand:V4DI 0 "register_operand" "=f")
	(unspec:V4DI [(match_operand:V4DI 1 "register_operand" "f")
		      (match_operand:V4DI 2 "register_operand" "f")]
		     UNSPEC_LASX_XVSUBWEV2))]
  "ISA_HAS_LASX"
  "xvsubwev.q.du\t%u0,%u1,%u2"
  [(set_attr "type" "simd_int_arith")
   (set_attr "mode" "V4DI")])

;;XVMULWEV.Q.DU
;;TODO2
(define_insn "lasx_xvmulwev_q_du"
  [(set (match_operand:V4DI 0 "register_operand" "=f")
	(unspec:V4DI [(match_operand:V4DI 1 "register_operand" "f")
		      (match_operand:V4DI 2 "register_operand" "f")]
		     UNSPEC_LASX_XVMULWEV2))]
  "ISA_HAS_LASX"
  "xvmulwev.q.du\t%u0,%u1,%u2"
  [(set_attr "type" "simd_int_arith")
   (set_attr "mode" "V4DI")])

;;XVADDWOD.Q.DU
;;TODO2
(define_insn "lasx_xvaddwod_q_du"
  [(set (match_operand:V4DI 0 "register_operand" "=f")
	(unspec:V4DI [(match_operand:V4DI 1 "register_operand" "f")
		      (match_operand:V4DI 2 "register_operand" "f")]
		     UNSPEC_LASX_XVADDWOD2))]
  "ISA_HAS_LASX"
  "xvaddwod.q.du\t%u0,%u1,%u2"
  [(set_attr "type" "simd_int_arith")
   (set_attr "mode" "V4DI")])

;;XVSUBWOD.Q.DU
;;TODO2
(define_insn "lasx_xvsubwod_q_du"
  [(set (match_operand:V4DI 0 "register_operand" "=f")
	(unspec:V4DI [(match_operand:V4DI 1 "register_operand" "f")
		      (match_operand:V4DI 2 "register_operand" "f")]
		     UNSPEC_LASX_XVSUBWOD2))]
  "ISA_HAS_LASX"
  "xvsubwod.q.du\t%u0,%u1,%u2"
  [(set_attr "type" "simd_int_arith")
   (set_attr "mode" "V4DI")])

;;XVMULWOD.Q.DU
;;TODO2
(define_insn "lasx_xvmulwod_q_du"
  [(set (match_operand:V4DI 0 "register_operand" "=f")
	(unspec:V4DI [(match_operand:V4DI 1 "register_operand" "f")
		      (match_operand:V4DI 2 "register_operand" "f")]
		     UNSPEC_LASX_XVMULWOD2))]
  "ISA_HAS_LASX"
  "xvmulwod.q.du\t%u0,%u1,%u2"
  [(set_attr "type" "simd_int_arith")
   (set_attr "mode" "V4DI")])

;;XVADDWEV.H.BU.B   XVMULWEV.H.BU.B
(define_insn "lasx_xv<optab>wev_h_bu_b"
  [(set (match_operand:V16HI 0 "register_operand" "=f")
	(addmul:V16HI
	  (zero_extend:V16HI
	    (vec_select:V16QI
	      (match_operand:V32QI 1 "register_operand" "%f")
	      (parallel [(const_int 0) (const_int 2)
			 (const_int 4) (const_int 6)
			 (const_int 8) (const_int 10)
			 (const_int 12) (const_int 14)
			 (const_int 16) (const_int 18)
			 (const_int 20) (const_int 22)
			 (const_int 24) (const_int 26)
			 (const_int 28) (const_int 30)])))
	  (sign_extend:V16HI
	    (vec_select:V16QI
	      (match_operand:V32QI 2 "register_operand" "f")
	      (parallel [(const_int 0) (const_int 2)
			 (const_int 4) (const_int 6)
			 (const_int 8) (const_int 10)
			 (const_int 12) (const_int 14)
			 (const_int 16) (const_int 18)
			 (const_int 20) (const_int 22)
			 (const_int 24) (const_int 26)
			 (const_int 28) (const_int 30)])))))]
  "ISA_HAS_LASX"
  "xv<optab>wev.h.bu.b\t%u0,%u1,%u2"
  [(set_attr "type" "simd_int_arith")
   (set_attr "mode" "V16HI")])

;;XVADDWEV.W.HU.H   XVMULWEV.W.HU.H
(define_insn "lasx_xv<optab>wev_w_hu_h"
  [(set (match_operand:V8SI 0 "register_operand" "=f")
	(addmul:V8SI
	  (zero_extend:V8SI
	    (vec_select:V8HI
	      (match_operand:V16HI 1 "register_operand" "%f")
	      (parallel [(const_int 0) (const_int 2)
			 (const_int 4) (const_int 6)
			 (const_int 8) (const_int 10)
			 (const_int 12) (const_int 14)])))
	  (sign_extend:V8SI
	    (vec_select:V8HI
	      (match_operand:V16HI 2 "register_operand" "f")
	      (parallel [(const_int 0) (const_int 2)
			 (const_int 4) (const_int 6)
			 (const_int 8) (const_int 10)
			 (const_int 12) (const_int 14)])))))]
  "ISA_HAS_LASX"
  "xv<optab>wev.w.hu.h\t%u0,%u1,%u2"
  [(set_attr "type" "simd_int_arith")
   (set_attr "mode" "V8SI")])

;;XVADDWEV.D.WU.W   XVMULWEV.D.WU.W
(define_insn "lasx_xv<optab>wev_d_wu_w"
  [(set (match_operand:V4DI 0 "register_operand" "=f")
	(addmul:V4DI
	  (zero_extend:V4DI
	    (vec_select:V4SI
	      (match_operand:V8SI 1 "register_operand" "%f")
	      (parallel [(const_int 0) (const_int 2)
			 (const_int 4) (const_int 6)])))
	  (sign_extend:V4DI
	    (vec_select:V4SI
	      (match_operand:V8SI 2 "register_operand" "f")
	      (parallel [(const_int 0) (const_int 2)
			 (const_int 4) (const_int 6)])))))]
  "ISA_HAS_LASX"
  "xv<optab>wev.d.wu.w\t%u0,%u1,%u2"
  [(set_attr "type" "simd_int_arith")
   (set_attr "mode" "V4DI")])

;;XVADDWOD.H.BU.B   XVMULWOD.H.BU.B
(define_insn "lasx_xv<optab>wod_h_bu_b"
  [(set (match_operand:V16HI 0 "register_operand" "=f")
	(addmul:V16HI
	  (zero_extend:V16HI
	    (vec_select:V16QI
	      (match_operand:V32QI 1 "register_operand" "%f")
	      (parallel [(const_int 1) (const_int 3)
			 (const_int 5) (const_int 7)
			 (const_int 9) (const_int 11)
			 (const_int 13) (const_int 15)
			 (const_int 17) (const_int 19)
			 (const_int 21) (const_int 23)
			 (const_int 25) (const_int 27)
			 (const_int 29) (const_int 31)])))
	  (sign_extend:V16HI
	    (vec_select:V16QI
	      (match_operand:V32QI 2 "register_operand" "f")
	      (parallel [(const_int 1) (const_int 3)
			 (const_int 5) (const_int 7)
			 (const_int 9) (const_int 11)
			 (const_int 13) (const_int 15)
			 (const_int 17) (const_int 19)
			 (const_int 21) (const_int 23)
			 (const_int 25) (const_int 27)
			 (const_int 29) (const_int 31)])))))]
  "ISA_HAS_LASX"
  "xv<optab>wod.h.bu.b\t%u0,%u1,%u2"
  [(set_attr "type" "simd_int_arith")
   (set_attr "mode" "V16HI")])

;;XVADDWOD.W.HU.H   XVMULWOD.W.HU.H
(define_insn "lasx_xv<optab>wod_w_hu_h"
  [(set (match_operand:V8SI 0 "register_operand" "=f")
	(addmul:V8SI
	  (zero_extend:V8SI
	    (vec_select:V8HI
	      (match_operand:V16HI 1 "register_operand" "%f")
	      (parallel [(const_int 1) (const_int 3)
			 (const_int 5) (const_int 7)
			 (const_int 9) (const_int 11)
			 (const_int 13) (const_int 15)])))
	  (sign_extend:V8SI
	    (vec_select:V8HI
	      (match_operand:V16HI 2 "register_operand" "f")
	      (parallel [(const_int 1) (const_int 3)
			 (const_int 5) (const_int 7)
			 (const_int 9) (const_int 11)
			 (const_int 13) (const_int 15)])))))]
  "ISA_HAS_LASX"
  "xv<optab>wod.w.hu.h\t%u0,%u1,%u2"
  [(set_attr "type" "simd_int_arith")
   (set_attr "mode" "V8SI")])

;;XVADDWOD.D.WU.W   XVMULWOD.D.WU.W
(define_insn "lasx_xv<optab>wod_d_wu_w"
  [(set (match_operand:V4DI 0 "register_operand" "=f")
	(addmul:V4DI
	  (zero_extend:V4DI
	    (vec_select:V4SI
	      (match_operand:V8SI 1 "register_operand" "%f")
	      (parallel [(const_int 1) (const_int 3)
			 (const_int 5) (const_int 7)])))
	  (sign_extend:V4DI
	    (vec_select:V4SI
	      (match_operand:V8SI 2 "register_operand" "f")
	      (parallel [(const_int 1) (const_int 3)
			 (const_int 5) (const_int 7)])))))]
  "ISA_HAS_LASX"
  "xv<optab>wod.d.wu.w\t%u0,%u1,%u2"
  [(set_attr "type" "simd_int_arith")
   (set_attr "mode" "V4DI")])

;;XVMADDWEV.H.B   XVMADDWEV.H.BU
(define_insn "lasx_xvmaddwev_h_b<u>"
  [(set (match_operand:V16HI 0 "register_operand" "=f")
	(plus:V16HI
	  (match_operand:V16HI 1 "register_operand" "0")
	  (mult:V16HI
	    (any_extend:V16HI
	      (vec_select:V16QI
		(match_operand:V32QI 2 "register_operand" "%f")
		(parallel [(const_int 0) (const_int 2)
			   (const_int 4) (const_int 6)
			   (const_int 8) (const_int 10)
			   (const_int 12) (const_int 14)
			   (const_int 16) (const_int 18)
			   (const_int 20) (const_int 22)
			   (const_int 24) (const_int 26)
			   (const_int 28) (const_int 30)])))
	    (any_extend:V16HI
	      (vec_select:V16QI
		(match_operand:V32QI 3 "register_operand" "f")
		(parallel [(const_int 0) (const_int 2)
			   (const_int 4) (const_int 6)
			   (const_int 8) (const_int 10)
			   (const_int 12) (const_int 14)
			   (const_int 16) (const_int 18)
			   (const_int 20) (const_int 22)
			   (const_int 24) (const_int 26)
			   (const_int 28) (const_int 30)]))))))]
  "ISA_HAS_LASX"
  "xvmaddwev.h.b<u>\t%u0,%u2,%u3"
  [(set_attr "type" "simd_fmadd")
   (set_attr "mode" "V16HI")])

;;XVMADDWEV.W.H   XVMADDWEV.W.HU
(define_insn "lasx_xvmaddwev_w_h<u>"
  [(set (match_operand:V8SI 0 "register_operand" "=f")
	(plus:V8SI
	  (match_operand:V8SI 1 "register_operand" "0")
	  (mult:V8SI
	    (any_extend:V8SI
	      (vec_select:V8HI
		(match_operand:V16HI 2 "register_operand" "%f")
		(parallel [(const_int 0) (const_int 2)
			   (const_int 4) (const_int 6)
			   (const_int 8) (const_int 10)
			   (const_int 12) (const_int 14)])))
	    (any_extend:V8SI
	      (vec_select:V8HI
		(match_operand:V16HI 3 "register_operand" "f")
		(parallel [(const_int 0) (const_int 2)
			   (const_int 4) (const_int 6)
			   (const_int 8) (const_int 10)
			   (const_int 12) (const_int 14)]))))))]
  "ISA_HAS_LASX"
  "xvmaddwev.w.h<u>\t%u0,%u2,%u3"
  [(set_attr "type" "simd_fmadd")
   (set_attr "mode" "V8SI")])

;;XVMADDWEV.D.W   XVMADDWEV.D.WU
(define_insn "lasx_xvmaddwev_d_w<u>"
  [(set (match_operand:V4DI 0 "register_operand" "=f")
	(plus:V4DI
	  (match_operand:V4DI 1 "register_operand" "0")
	  (mult:V4DI
	    (any_extend:V4DI
	      (vec_select:V4SI
		(match_operand:V8SI 2 "register_operand" "%f")
		(parallel [(const_int 0) (const_int 2)
			   (const_int 4) (const_int 6)])))
	    (any_extend:V4DI
	      (vec_select:V4SI
		(match_operand:V8SI 3 "register_operand" "f")
		(parallel [(const_int 0) (const_int 2)
			   (const_int 4) (const_int 6)]))))))]
  "ISA_HAS_LASX"
  "xvmaddwev.d.w<u>\t%u0,%u2,%u3"
  [(set_attr "type" "simd_fmadd")
   (set_attr "mode" "V4DI")])

;;XVMADDWEV.Q.D
;;TODO2
(define_insn "lasx_xvmaddwev_q_d"
  [(set (match_operand:V4DI 0 "register_operand" "=f")
	(unspec:V4DI [(match_operand:V4DI 1 "register_operand" "0")
		      (match_operand:V4DI 2 "register_operand" "f")
		      (match_operand:V4DI 3 "register_operand" "f")]
		     UNSPEC_LASX_XVMADDWEV))]
  "ISA_HAS_LASX"
  "xvmaddwev.q.d\t%u0,%u2,%u3"
  [(set_attr "type" "simd_int_arith")
   (set_attr "mode" "V4DI")])

;;XVMADDWOD.H.B   XVMADDWOD.H.BU
(define_insn "lasx_xvmaddwod_h_b<u>"
  [(set (match_operand:V16HI 0 "register_operand" "=f")
	(plus:V16HI
	  (match_operand:V16HI 1 "register_operand" "0")
	  (mult:V16HI
	    (any_extend:V16HI
	      (vec_select:V16QI
		(match_operand:V32QI 2 "register_operand" "%f")
		(parallel [(const_int 1) (const_int 3)
			   (const_int 5) (const_int 7)
			   (const_int 9) (const_int 11)
			   (const_int 13) (const_int 15)
			   (const_int 17) (const_int 19)
			   (const_int 21) (const_int 23)
			   (const_int 25) (const_int 27)
			   (const_int 29) (const_int 31)])))
	    (any_extend:V16HI
	      (vec_select:V16QI
		(match_operand:V32QI 3 "register_operand" "f")
		(parallel [(const_int 1) (const_int 3)
			   (const_int 5) (const_int 7)
			   (const_int 9) (const_int 11)
			   (const_int 13) (const_int 15)
			   (const_int 17) (const_int 19)
			   (const_int 21) (const_int 23)
			   (const_int 25) (const_int 27)
			   (const_int 29) (const_int 31)]))))))]
  "ISA_HAS_LASX"
  "xvmaddwod.h.b<u>\t%u0,%u2,%u3"
  [(set_attr "type" "simd_fmadd")
   (set_attr "mode" "V16HI")])

;;XVMADDWOD.W.H   XVMADDWOD.W.HU
(define_insn "lasx_xvmaddwod_w_h<u>"
  [(set (match_operand:V8SI 0 "register_operand" "=f")
	(plus:V8SI
	  (match_operand:V8SI 1 "register_operand" "0")
	  (mult:V8SI
	    (any_extend:V8SI
	      (vec_select:V8HI
		(match_operand:V16HI 2 "register_operand" "%f")
		(parallel [(const_int 1) (const_int 3)
			   (const_int 5) (const_int 7)
			   (const_int 9) (const_int 11)
			   (const_int 13) (const_int 15)])))
	    (any_extend:V8SI
	      (vec_select:V8HI
		(match_operand:V16HI 3 "register_operand" "f")
		(parallel [(const_int 1) (const_int 3)
			   (const_int 5) (const_int 7)
			   (const_int 9) (const_int 11)
			   (const_int 13) (const_int 15)]))))))]
  "ISA_HAS_LASX"
  "xvmaddwod.w.h<u>\t%u0,%u2,%u3"
  [(set_attr "type" "simd_fmadd")
   (set_attr "mode" "V8SI")])

;;XVMADDWOD.D.W   XVMADDWOD.D.WU
(define_insn "lasx_xvmaddwod_d_w<u>"
  [(set (match_operand:V4DI 0 "register_operand" "=f")
	(plus:V4DI
	  (match_operand:V4DI 1 "register_operand" "0")
	  (mult:V4DI
	    (any_extend:V4DI
	      (vec_select:V4SI
		(match_operand:V8SI 2 "register_operand" "%f")
		(parallel [(const_int 1) (const_int 3)
			   (const_int 5) (const_int 7)])))
	    (any_extend:V4DI
	      (vec_select:V4SI
		(match_operand:V8SI 3 "register_operand" "f")
		(parallel [(const_int 1) (const_int 3)
			   (const_int 5) (const_int 7)]))))))]
  "ISA_HAS_LASX"
  "xvmaddwod.d.w<u>\t%u0,%u2,%u3"
  [(set_attr "type" "simd_fmadd")
   (set_attr "mode" "V4DI")])

;;XVMADDWOD.Q.D
;;TODO2
(define_insn "lasx_xvmaddwod_q_d"
  [(set (match_operand:V4DI 0 "register_operand" "=f")
	(unspec:V4DI [(match_operand:V4DI 1 "register_operand" "0")
		      (match_operand:V4DI 2 "register_operand" "f")
		      (match_operand:V4DI 3 "register_operand" "f")]
		     UNSPEC_LASX_XVMADDWOD))]
  "ISA_HAS_LASX"
  "xvmaddwod.q.d\t%u0,%u2,%u3"
  [(set_attr "type" "simd_int_arith")
   (set_attr "mode" "V4DI")])

;;XVMADDWEV.Q.DU
;;TODO2
(define_insn "lasx_xvmaddwev_q_du"
  [(set (match_operand:V4DI 0 "register_operand" "=f")
	(unspec:V4DI [(match_operand:V4DI 1 "register_operand" "0")
		      (match_operand:V4DI 2 "register_operand" "f")
		      (match_operand:V4DI 3 "register_operand" "f")]
		     UNSPEC_LASX_XVMADDWEV2))]
  "ISA_HAS_LASX"
  "xvmaddwev.q.du\t%u0,%u2,%u3"
  [(set_attr "type" "simd_int_arith")
   (set_attr "mode" "V4DI")])

;;XVMADDWOD.Q.DU
;;TODO2
(define_insn "lasx_xvmaddwod_q_du"
  [(set (match_operand:V4DI 0 "register_operand" "=f")
	(unspec:V4DI [(match_operand:V4DI 1 "register_operand" "0")
		      (match_operand:V4DI 2 "register_operand" "f")
		      (match_operand:V4DI 3 "register_operand" "f")]
		     UNSPEC_LASX_XVMADDWOD2))]
  "ISA_HAS_LASX"
  "xvmaddwod.q.du\t%u0,%u2,%u3"
  [(set_attr "type" "simd_int_arith")
   (set_attr "mode" "V4DI")])

;;XVMADDWEV.H.BU.B
(define_insn "lasx_xvmaddwev_h_bu_b"
  [(set (match_operand:V16HI 0 "register_operand" "=f")
	(plus:V16HI
	  (match_operand:V16HI 1 "register_operand" "0")
	  (mult:V16HI
	    (zero_extend:V16HI
	      (vec_select:V16QI
		(match_operand:V32QI 2 "register_operand" "%f")
		(parallel [(const_int 0) (const_int 2)
			   (const_int 4) (const_int 6)
			   (const_int 8) (const_int 10)
			   (const_int 12) (const_int 14)
			   (const_int 16) (const_int 18)
			   (const_int 20) (const_int 22)
			   (const_int 24) (const_int 26)
			   (const_int 28) (const_int 30)])))
	    (sign_extend:V16HI
	      (vec_select:V16QI
		(match_operand:V32QI 3 "register_operand" "f")
		(parallel [(const_int 0) (const_int 2)
			   (const_int 4) (const_int 6)
			   (const_int 8) (const_int 10)
			   (const_int 12) (const_int 14)
			   (const_int 16) (const_int 18)
			   (const_int 20) (const_int 22)
			   (const_int 24) (const_int 26)
			   (const_int 28) (const_int 30)]))))))]
  "ISA_HAS_LASX"
  "xvmaddwev.h.bu.b\t%u0,%u2,%u3"
  [(set_attr "type" "simd_fmadd")
   (set_attr "mode" "V16HI")])

;;XVMADDWEV.W.HU.H
(define_insn "lasx_xvmaddwev_w_hu_h"
  [(set (match_operand:V8SI 0 "register_operand" "=f")
	(plus:V8SI
	  (match_operand:V8SI 1 "register_operand" "0")
	  (mult:V8SI
	    (zero_extend:V8SI
	      (vec_select:V8HI
		(match_operand:V16HI 2 "register_operand" "%f")
		(parallel [(const_int 0) (const_int 2)
			   (const_int 4) (const_int 6)
			   (const_int 8) (const_int 10)
			   (const_int 12) (const_int 14)])))
	    (sign_extend:V8SI
	      (vec_select:V8HI
		(match_operand:V16HI 3 "register_operand" "f")
		(parallel [(const_int 0) (const_int 2)
			   (const_int 4) (const_int 6)
			   (const_int 8) (const_int 10)
			   (const_int 12) (const_int 14)]))))))]
  "ISA_HAS_LASX"
  "xvmaddwev.w.hu.h\t%u0,%u2,%u3"
  [(set_attr "type" "simd_fmadd")
   (set_attr "mode" "V8SI")])

;;XVMADDWEV.D.WU.W
(define_insn "lasx_xvmaddwev_d_wu_w"
  [(set (match_operand:V4DI 0 "register_operand" "=f")
	(plus:V4DI
	  (match_operand:V4DI 1 "register_operand" "0")
	  (mult:V4DI
	    (zero_extend:V4DI
	      (vec_select:V4SI
		(match_operand:V8SI 2 "register_operand" "%f")
		(parallel [(const_int 0) (const_int 2)
			   (const_int 4) (const_int 6)])))
	    (sign_extend:V4DI
	      (vec_select:V4SI
		(match_operand:V8SI 3 "register_operand" "f")
		(parallel [(const_int 0) (const_int 2)
			   (const_int 4) (const_int 6)]))))))]
  "ISA_HAS_LASX"
  "xvmaddwev.d.wu.w\t%u0,%u2,%u3"
  [(set_attr "type" "simd_fmadd")
   (set_attr "mode" "V4DI")])

;;XVMADDWEV.Q.DU.D
;;TODO2
(define_insn "lasx_xvmaddwev_q_du_d"
  [(set (match_operand:V4DI 0 "register_operand" "=f")
	(unspec:V4DI [(match_operand:V4DI 1 "register_operand" "0")
		      (match_operand:V4DI 2 "register_operand" "f")
		      (match_operand:V4DI 3 "register_operand" "f")]
		     UNSPEC_LASX_XVMADDWEV3))]
  "ISA_HAS_LASX"
  "xvmaddwev.q.du.d\t%u0,%u2,%u3"
  [(set_attr "type" "simd_int_arith")
   (set_attr "mode" "V4DI")])

;;XVMADDWOD.H.BU.B
(define_insn "lasx_xvmaddwod_h_bu_b"
  [(set (match_operand:V16HI 0 "register_operand" "=f")
	(plus:V16HI
	  (match_operand:V16HI 1 "register_operand" "0")
	  (mult:V16HI
	    (zero_extend:V16HI
	      (vec_select:V16QI
		(match_operand:V32QI 2 "register_operand" "%f")
		(parallel [(const_int 1) (const_int 3)
			   (const_int 5) (const_int 7)
			   (const_int 9) (const_int 11)
			   (const_int 13) (const_int 15)
			   (const_int 17) (const_int 19)
			   (const_int 21) (const_int 23)
			   (const_int 25) (const_int 27)
			   (const_int 29) (const_int 31)])))
	    (sign_extend:V16HI
	      (vec_select:V16QI
		(match_operand:V32QI 3 "register_operand" "f")
		(parallel [(const_int 1) (const_int 3)
			   (const_int 5) (const_int 7)
			   (const_int 9) (const_int 11)
			   (const_int 13) (const_int 15)
			   (const_int 17) (const_int 19)
			   (const_int 21) (const_int 23)
			   (const_int 25) (const_int 27)
			   (const_int 29) (const_int 31)]))))))]
  "ISA_HAS_LASX"
  "xvmaddwod.h.bu.b\t%u0,%u2,%u3"
  [(set_attr "type" "simd_fmadd")
   (set_attr "mode" "V16HI")])

;;XVMADDWOD.W.HU.H
(define_insn "lasx_xvmaddwod_w_hu_h"
  [(set (match_operand:V8SI 0 "register_operand" "=f")
	(plus:V8SI
	  (match_operand:V8SI 1 "register_operand" "0")
	  (mult:V8SI
	    (zero_extend:V8SI
	      (vec_select:V8HI
		(match_operand:V16HI 2 "register_operand" "%f")
		(parallel [(const_int 1) (const_int 3)
			   (const_int 5) (const_int 7)
			   (const_int 9) (const_int 11)
			   (const_int 13) (const_int 15)])))
	    (sign_extend:V8SI
	      (vec_select:V8HI
		(match_operand:V16HI 3 "register_operand" "f")
		(parallel [(const_int 1) (const_int 3)
			   (const_int 5) (const_int 7)
			   (const_int 9) (const_int 11)
			   (const_int 13) (const_int 15)]))))))]
  "ISA_HAS_LASX"
  "xvmaddwod.w.hu.h\t%u0,%u2,%u3"
  [(set_attr "type" "simd_fmadd")
   (set_attr "mode" "V8SI")])

;;XVMADDWOD.D.WU.W
(define_insn "lasx_xvmaddwod_d_wu_w"
  [(set (match_operand:V4DI 0 "register_operand" "=f")
	(plus:V4DI
	  (match_operand:V4DI 1 "register_operand" "0")
	  (mult:V4DI
	    (zero_extend:V4DI
	      (vec_select:V4SI
		(match_operand:V8SI 2 "register_operand" "%f")
		(parallel [(const_int 1) (const_int 3)
			   (const_int 5) (const_int 7)])))
	    (sign_extend:V4DI
	      (vec_select:V4SI
		(match_operand:V8SI 3 "register_operand" "f")
		(parallel [(const_int 1) (const_int 3)
			   (const_int 5) (const_int 7)]))))))]
  "ISA_HAS_LASX"
  "xvmaddwod.d.wu.w\t%u0,%u2,%u3"
  [(set_attr "type" "simd_fmadd")
   (set_attr "mode" "V4DI")])

;;XVMADDWOD.Q.DU.D
;;TODO2
(define_insn "lasx_xvmaddwod_q_du_d"
  [(set (match_operand:V4DI 0 "register_operand" "=f")
	(unspec:V4DI [(match_operand:V4DI 1 "register_operand" "0")
		      (match_operand:V4DI 2 "register_operand" "f")
		      (match_operand:V4DI 3 "register_operand" "f")]
		     UNSPEC_LASX_XVMADDWOD3))]
  "ISA_HAS_LASX"
  "xvmaddwod.q.du.d\t%u0,%u2,%u3"
  [(set_attr "type" "simd_int_arith")
   (set_attr "mode" "V4DI")])

;;XVHADDW.Q.D
;;TODO2
(define_insn "lasx_xvhaddw_q_d"
  [(set (match_operand:V4DI 0 "register_operand" "=f")
	(unspec:V4DI [(match_operand:V4DI 1 "register_operand" "f")
		      (match_operand:V4DI 2 "register_operand" "f")]
		     UNSPEC_LASX_XVHADDW_Q_D))]
  "ISA_HAS_LASX"
  "xvhaddw.q.d\t%u0,%u1,%u2"
  [(set_attr "type" "simd_int_arith")
   (set_attr "mode" "V4DI")])

;;XVHSUBW.Q.D
;;TODO2
(define_insn "lasx_xvhsubw_q_d"
  [(set (match_operand:V4DI 0 "register_operand" "=f")
	(unspec:V4DI [(match_operand:V4DI 1 "register_operand" "f")
		      (match_operand:V4DI 2 "register_operand" "f")]
		     UNSPEC_LASX_XVHSUBW_Q_D))]
  "ISA_HAS_LASX"
  "xvhsubw.q.d\t%u0,%u1,%u2"
  [(set_attr "type" "simd_int_arith")
   (set_attr "mode" "V4DI")])

;;XVHADDW.QU.DU
;;TODO2
(define_insn "lasx_xvhaddw_qu_du"
  [(set (match_operand:V4DI 0 "register_operand" "=f")
	(unspec:V4DI [(match_operand:V4DI 1 "register_operand" "f")
		      (match_operand:V4DI 2 "register_operand" "f")]
		     UNSPEC_LASX_XVHADDW_QU_DU))]
  "ISA_HAS_LASX"
  "xvhaddw.qu.du\t%u0,%u1,%u2"
  [(set_attr "type" "simd_int_arith")
   (set_attr "mode" "V4DI")])

;;XVHSUBW.QU.DU
;;TODO2
(define_insn "lasx_xvhsubw_qu_du"
  [(set (match_operand:V4DI 0 "register_operand" "=f")
	(unspec:V4DI [(match_operand:V4DI 1 "register_operand" "f")
		      (match_operand:V4DI 2 "register_operand" "f")]
		     UNSPEC_LASX_XVHSUBW_QU_DU))]
  "ISA_HAS_LASX"
  "xvhsubw.qu.du\t%u0,%u1,%u2"
  [(set_attr "type" "simd_int_arith")
   (set_attr "mode" "V4DI")])

;;XVADD.Q
;;TODO2
(define_insn "lasx_xvadd_q"
  [(set (match_operand:V4DI 0 "register_operand" "=f")
	(unspec:V4DI [(match_operand:V4DI 1 "register_operand" "f")
		      (match_operand:V4DI 2 "register_operand" "f")]
		     UNSPEC_LASX_XVADD_Q))]
  "ISA_HAS_LASX"
  "xvadd.q\t%u0,%u1,%u2"
  [(set_attr "type" "simd_int_arith")
   (set_attr "mode" "V4DI")])

;;XVSUB.Q
;;TODO2
(define_insn "lasx_xvsub_q"
  [(set (match_operand:V4DI 0 "register_operand" "=f")
	(unspec:V4DI [(match_operand:V4DI 1 "register_operand" "f")
		      (match_operand:V4DI 2 "register_operand" "f")]
		     UNSPEC_LASX_XVSUB_Q))]
  "ISA_HAS_LASX"
  "xvsub.q\t%u0,%u1,%u2"
  [(set_attr "type" "simd_int_arith")
   (set_attr "mode" "V4DI")])

;;XVSSRLN.B.H   XVSSRLN.H.W   XVSSRLN.W.D
(define_insn "lasx_xvssrln_<hlasxfmt>_<lasxfmt>"
  [(set (match_operand:<VHSMODE256> 0 "register_operand" "=f")
	(unspec:<VHSMODE256> [(match_operand:ILASX_DWH 1 "register_operand" "f")
			      (match_operand:ILASX_DWH 2 "register_operand" "f")]
			     UNSPEC_LASX_XVSSRLN))]
  "ISA_HAS_LASX"
  "xvssrln.<hlasxfmt>.<lasxfmt>\t%u0,%u1,%u2"
  [(set_attr "type" "simd_int_arith")
   (set_attr "mode" "<MODE>")])

;;XVREPLVE.B   XVREPLVE.H   XVREPLVE.W   XVREPLVE.D
(define_insn "lasx_xvreplve_<lasxfmt_f>"
  [(set (match_operand:LASX 0 "register_operand" "=f")
	(unspec:LASX [(match_operand:LASX 1 "register_operand" "f")
		      (match_operand:SI 2 "register_operand" "r")]
		     UNSPEC_LASX_XVREPLVE))]
  "ISA_HAS_LASX"
  "xvreplve.<lasxfmt>\t%u0,%u1,%z2"
  [(set_attr "type" "simd_splat")
   (set_attr "mode" "<MODE>")])

;;XVADDWEV.Q.DU.D
(define_insn "lasx_xvaddwev_q_du_d"
  [(set (match_operand:V4DI 0 "register_operand" "=f")
	(unspec:V4DI [(match_operand:V4DI 1 "register_operand" "f")
		      (match_operand:V4DI 2 "register_operand" "f")]
		     UNSPEC_LASX_XVADDWEV3))]
  "ISA_HAS_LASX"
  "xvaddwev.q.du.d\t%u0,%u1,%u2"
  [(set_attr "type" "simd_int_arith")
   (set_attr "mode" "V4DI")])

;;XVADDWOD.Q.DU.D
(define_insn "lasx_xvaddwod_q_du_d"
  [(set (match_operand:V4DI 0 "register_operand" "=f")
	(unspec:V4DI [(match_operand:V4DI 1 "register_operand" "f")
		      (match_operand:V4DI 2 "register_operand" "f")]
		     UNSPEC_LASX_XVADDWOD3))]
  "ISA_HAS_LASX"
  "xvaddwod.q.du.d\t%u0,%u1,%u2"
  [(set_attr "type" "simd_int_arith")
   (set_attr "mode" "V4DI")])

;;XVMULWEV.Q.DU.D
(define_insn "lasx_xvmulwev_q_du_d"
  [(set (match_operand:V4DI 0 "register_operand" "=f")
	(unspec:V4DI [(match_operand:V4DI 1 "register_operand" "f")
		      (match_operand:V4DI 2 "register_operand" "f")]
		     UNSPEC_LASX_XVMULWEV3))]
  "ISA_HAS_LASX"
  "xvmulwev.q.du.d\t%u0,%u1,%u2"
  [(set_attr "type" "simd_int_arith")
   (set_attr "mode" "V4DI")])

;;XVMULWOD.Q.DU.D
(define_insn "lasx_xvmulwod_q_du_d"
  [(set (match_operand:V4DI 0 "register_operand" "=f")
	(unspec:V4DI [(match_operand:V4DI 1 "register_operand" "f")
		      (match_operand:V4DI 2 "register_operand" "f")]
		     UNSPEC_LASX_XVMULWOD3))]
  "ISA_HAS_LASX"
  "xvmulwod.q.du.d\t%u0,%u1,%u2"
  [(set_attr "type" "simd_int_arith")
   (set_attr "mode" "V4DI")])

(define_insn "lasx_xvpickve2gr_w<u>"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(any_extend:SI
	  (vec_select:SI
	    (match_operand:V8SI 1 "register_operand" "f")
	    (parallel [(match_operand 2 "const_0_to_7_operand" "")]))))]
  "ISA_HAS_LASX"
  "xvpickve2gr.w<u>\t%0,%u1,%2"
  [(set_attr "type" "simd_copy")
   (set_attr "mode" "V8SI")])


(define_insn "lasx_xvmskgez_b"
  [(set (match_operand:V32QI 0 "register_operand" "=f")
	(unspec:V32QI [(match_operand:V32QI 1 "register_operand" "f")]
		      UNSPEC_LASX_XVMSKGEZ))]
  "ISA_HAS_LASX"
  "xvmskgez.b\t%u0,%u1"
  [(set_attr "type" "simd_bit")
   (set_attr "mode" "V32QI")])

(define_insn "lasx_xvmsknz_b"
  [(set (match_operand:V32QI 0 "register_operand" "=f")
	(unspec:V32QI [(match_operand:V32QI 1 "register_operand" "f")]
		      UNSPEC_LASX_XVMSKNZ))]
  "ISA_HAS_LASX"
  "xvmsknz.b\t%u0,%u1"
  [(set_attr "type" "simd_bit")
   (set_attr "mode" "V32QI")])

(define_insn "lasx_xvexth_h<u>_b<u>"
  [(set (match_operand:V16HI 0 "register_operand" "=f")
	(any_extend:V16HI
	  (vec_select:V16QI
	    (match_operand:V32QI 1 "register_operand" "f")
	      (parallel [(const_int 16) (const_int 17)
			 (const_int 18) (const_int 19)
			 (const_int 20) (const_int 21)
			 (const_int 22) (const_int 23)
			 (const_int 24) (const_int 25)
			 (const_int 26) (const_int 27)
			 (const_int 28) (const_int 29)
			 (const_int 30) (const_int 31)]))))]
  "ISA_HAS_LASX"
  "xvexth.h<u>.b<u>\t%u0,%u1"
  [(set_attr "type" "simd_fcvt")
   (set_attr "mode" "V16HI")])

(define_insn "lasx_xvexth_w<u>_h<u>"
  [(set (match_operand:V8SI 0 "register_operand" "=f")
	(any_extend:V8SI
	  (vec_select:V8HI
	    (match_operand:V16HI 1 "register_operand" "f")
	    (parallel [(const_int 8) (const_int 9)
		       (const_int 10) (const_int 11)
		       (const_int 12) (const_int 13)
		       (const_int 14) (const_int 15)]))))]
  "ISA_HAS_LASX"
  "xvexth.w<u>.h<u>\t%u0,%u1"
  [(set_attr "type" "simd_fcvt")
   (set_attr "mode" "V8SI")])

(define_insn "lasx_xvexth_d<u>_w<u>"
  [(set (match_operand:V4DI 0 "register_operand" "=f")
	(any_extend:V4DI
	  (vec_select:V4SI
	    (match_operand:V8SI 1 "register_operand" "f")
	    (parallel [(const_int 4) (const_int 5)
		       (const_int 6) (const_int 7)]))))]
  "ISA_HAS_LASX"
  "xvexth.d<u>.w<u>\t%u0,%u1"
  [(set_attr "type" "simd_fcvt")
   (set_attr "mode" "V4DI")])

(define_insn "lasx_xvexth_q_d"
  [(set (match_operand:V4DI 0 "register_operand" "=f")
	(unspec:V4DI [(match_operand:V4DI 1 "register_operand" "f")]
		     UNSPEC_LASX_XVEXTH_Q_D))]
  "ISA_HAS_LASX"
  "xvexth.q.d\t%u0,%u1"
  [(set_attr "type" "simd_fcvt")
   (set_attr "mode" "V4DI")])

(define_insn "lasx_xvexth_qu_du"
  [(set (match_operand:V4DI 0 "register_operand" "=f")
	(unspec:V4DI [(match_operand:V4DI 1 "register_operand" "f")]
		     UNSPEC_LASX_XVEXTH_QU_DU))]
  "ISA_HAS_LASX"
  "xvexth.qu.du\t%u0,%u1"
  [(set_attr "type" "simd_fcvt")
   (set_attr "mode" "V4DI")])

(define_insn "lasx_xvextl_q_d"
  [(set (match_operand:V4DI 0 "register_operand" "=f")
	(unspec:V4DI [(match_operand:V4DI 1 "register_operand" "f")]
		     UNSPEC_LASX_XVEXTL_Q_D))]
  "ISA_HAS_LASX"
  "xvextl.q.d\t%u0,%u1"
  [(set_attr "type" "simd_fcvt")
   (set_attr "mode" "V4DI")])

(define_insn "lasx_xvsrlni_<lasxfmt>_<dlasxqfmt>"
  [(set (match_operand:ILASX 0 "register_operand" "=f")
	(unspec:ILASX [(match_operand:ILASX 1 "register_operand" "0")
		       (match_operand:ILASX 2 "register_operand" "f")
		       (match_operand 3 "const_uimm8_operand" "")]
		      UNSPEC_LASX_XVSRLNI))]
  "ISA_HAS_LASX"
  "xvsrlni.<lasxfmt>.<dlasxqfmt>\t%u0,%u2,%3"
  [(set_attr "type" "simd_shift")
   (set_attr "mode" "<MODE>")])

(define_insn "lasx_xvsrlrni_<lasxfmt>_<dlasxqfmt>"
  [(set (match_operand:ILASX 0 "register_operand" "=f")
	(unspec:ILASX [(match_operand:ILASX 1 "register_operand" "0")
		       (match_operand:ILASX 2 "register_operand" "f")
		       (match_operand 3 "const_uimm8_operand" "")]
		      UNSPEC_LASX_XVSRLRNI))]
  "ISA_HAS_LASX"
  "xvsrlrni.<lasxfmt>.<dlasxqfmt>\t%u0,%u2,%3"
  [(set_attr "type" "simd_shift")
   (set_attr "mode" "<MODE>")])

(define_insn "lasx_xvssrlni_<lasxfmt>_<dlasxqfmt>"
  [(set (match_operand:ILASX 0 "register_operand" "=f")
	(unspec:ILASX [(match_operand:ILASX 1 "register_operand" "0")
		       (match_operand:ILASX 2 "register_operand" "f")
		       (match_operand 3 "const_uimm8_operand" "")]
		      UNSPEC_LASX_XVSSRLNI))]
  "ISA_HAS_LASX"
  "xvssrlni.<lasxfmt>.<dlasxqfmt>\t%u0,%u2,%3"
  [(set_attr "type" "simd_shift")
   (set_attr "mode" "<MODE>")])

(define_insn "lasx_xvssrlni_<lasxfmt_u>_<dlasxqfmt>"
  [(set (match_operand:ILASX 0 "register_operand" "=f")
	(unspec:ILASX [(match_operand:ILASX 1 "register_operand" "0")
		       (match_operand:ILASX 2 "register_operand" "f")
		       (match_operand 3 "const_uimm8_operand" "")]
		      UNSPEC_LASX_XVSSRLNI2))]
  "ISA_HAS_LASX"
  "xvssrlni.<lasxfmt_u>.<dlasxqfmt>\t%u0,%u2,%3"
  [(set_attr "type" "simd_shift")
   (set_attr "mode" "<MODE>")])

(define_insn "lasx_xvssrlrni_<lasxfmt>_<dlasxqfmt>"
  [(set (match_operand:ILASX 0 "register_operand" "=f")
	(unspec:ILASX [(match_operand:ILASX 1 "register_operand" "0")
		       (match_operand:ILASX 2 "register_operand" "f")
		       (match_operand 3 "const_uimm8_operand" "")]
		      UNSPEC_LASX_XVSSRLRNI))]
  "ISA_HAS_LASX"
  "xvssrlrni.<lasxfmt>.<dlasxqfmt>\t%u0,%u2,%3"
  [(set_attr "type" "simd_shift")
   (set_attr "mode" "<MODE>")])

(define_insn "lasx_xvssrlrni_<lasxfmt_u>_<dlasxqfmt>"
  [(set (match_operand:ILASX 0 "register_operand" "=f")
	(unspec:ILASX [(match_operand:ILASX 1 "register_operand" "0")
		       (match_operand:ILASX 2 "register_operand" "f")
		       (match_operand 3 "const_uimm8_operand" "")]
		      UNSPEC_LASX_XVSSRLRNI2))]
  "ISA_HAS_LASX"
  "xvssrlrni.<lasxfmt_u>.<dlasxqfmt>\t%u0,%u2,%3"
  [(set_attr "type" "simd_shift")
   (set_attr "mode" "<MODE>")])

(define_insn "lasx_xvsrani_<lasxfmt>_<dlasxqfmt>"
  [(set (match_operand:ILASX 0 "register_operand" "=f")
	(unspec:ILASX [(match_operand:ILASX 1 "register_operand" "0")
		       (match_operand:ILASX 2 "register_operand" "f")
		       (match_operand 3 "const_uimm8_operand" "")]
		      UNSPEC_LASX_XVSRANI))]
  "ISA_HAS_LASX"
  "xvsrani.<lasxfmt>.<dlasxqfmt>\t%u0,%u2,%3"
  [(set_attr "type" "simd_shift")
   (set_attr "mode" "<MODE>")])

(define_insn "lasx_xvsrarni_<lasxfmt>_<dlasxqfmt>"
  [(set (match_operand:ILASX 0 "register_operand" "=f")
	(unspec:ILASX [(match_operand:ILASX 1 "register_operand" "0")
		       (match_operand:ILASX 2 "register_operand" "f")
		       (match_operand 3 "const_uimm8_operand" "")]
		      UNSPEC_LASX_XVSRARNI))]
  "ISA_HAS_LASX"
  "xvsrarni.<lasxfmt>.<dlasxqfmt>\t%u0,%u2,%3"
  [(set_attr "type" "simd_shift")
   (set_attr "mode" "<MODE>")])

(define_insn "lasx_xvssrani_<lasxfmt>_<dlasxqfmt>"
  [(set (match_operand:ILASX 0 "register_operand" "=f")
	(unspec:ILASX [(match_operand:ILASX 1 "register_operand" "0")
		       (match_operand:ILASX 2 "register_operand" "f")
		       (match_operand 3 "const_uimm8_operand" "")]
		      UNSPEC_LASX_XVSSRANI))]
  "ISA_HAS_LASX"
  "xvssrani.<lasxfmt>.<dlasxqfmt>\t%u0,%u2,%3"
  [(set_attr "type" "simd_shift")
   (set_attr "mode" "<MODE>")])

(define_insn "lasx_xvssrani_<lasxfmt_u>_<dlasxqfmt>"
  [(set (match_operand:ILASX 0 "register_operand" "=f")
	(unspec:ILASX [(match_operand:ILASX 1 "register_operand" "0")
		       (match_operand:ILASX 2 "register_operand" "f")
		       (match_operand 3 "const_uimm8_operand" "")]
		      UNSPEC_LASX_XVSSRANI2))]
  "ISA_HAS_LASX"
  "xvssrani.<lasxfmt_u>.<dlasxqfmt>\t%u0,%u2,%3"
  [(set_attr "type" "simd_shift")
   (set_attr "mode" "<MODE>")])

(define_insn "lasx_xvssrarni_<lasxfmt>_<dlasxqfmt>"
  [(set (match_operand:ILASX 0 "register_operand" "=f")
	(unspec:ILASX [(match_operand:ILASX 1 "register_operand" "0")
		       (match_operand:ILASX 2 "register_operand" "f")
		       (match_operand 3 "const_uimm8_operand" "")]
		      UNSPEC_LASX_XVSSRARNI))]
  "ISA_HAS_LASX"
  "xvssrarni.<lasxfmt>.<dlasxqfmt>\t%u0,%u2,%3"
  [(set_attr "type" "simd_shift")
   (set_attr "mode" "<MODE>")])

(define_insn "lasx_xvssrarni_<lasxfmt_u>_<dlasxqfmt>"
  [(set (match_operand:ILASX 0 "register_operand" "=f")
	(unspec:ILASX [(match_operand:ILASX 1 "register_operand" "0")
		       (match_operand:ILASX 2 "register_operand" "f")
		       (match_operand 3 "const_uimm8_operand" "")]
		      UNSPEC_LASX_XVSSRARNI2))]
  "ISA_HAS_LASX"
  "xvssrarni.<lasxfmt_u>.<dlasxqfmt>\t%u0,%u2,%3"
  [(set_attr "type" "simd_shift")
   (set_attr "mode" "<MODE>")])

(define_mode_attr VDOUBLEMODEW256
  [(V8SI "V16SI")
   (V8SF "V16SF")])

(define_insn "lasx_xvpermi_<lasxfmt_f_wd>"
  [(set (match_operand:LASX_W 0 "register_operand" "=f")
    (unspec:LASX_W [(match_operand:LASX_W 1 "register_operand" "0")
               (match_operand:LASX_W 2 "register_operand" "f")
                   (match_operand 3 "const_uimm8_operand" "")]
             UNSPEC_LASX_XVPERMI))]
  "ISA_HAS_LASX"
  "xvpermi.w\t%u0,%u2,%3"
  [(set_attr "type" "simd_bit")
   (set_attr "mode" "<MODE>")])

(define_insn "lasx_xvpermi_<lasxfmt_f_wd>_1"
  [(set (match_operand:LASX_W 0 "register_operand" "=f")
     (vec_select:LASX_W
       (vec_concat:<VDOUBLEMODEW256>
         (match_operand:LASX_W 1 "register_operand" "f")
         (match_operand:LASX_W 2 "register_operand" "0"))
       (parallel [(match_operand 3  "const_0_to_3_operand")
              (match_operand 4  "const_0_to_3_operand"  )
              (match_operand 5  "const_8_to_11_operand" )
              (match_operand 6  "const_8_to_11_operand" )
              (match_operand 7  "const_4_to_7_operand"  )
              (match_operand 8  "const_4_to_7_operand"  )
              (match_operand 9  "const_12_to_15_operand")
              (match_operand 10 "const_12_to_15_operand")])))]
  "ISA_HAS_LASX
  && INTVAL (operands[3]) + 4 == INTVAL (operands[7])
  && INTVAL (operands[4]) + 4 == INTVAL (operands[8])
  && INTVAL (operands[5]) + 4 == INTVAL (operands[9])
  && INTVAL (operands[6]) + 4 == INTVAL (operands[10])"
{
  int mask = 0;
  mask |= INTVAL (operands[3]) << 0;
  mask |= INTVAL (operands[4]) << 2;
  mask |= (INTVAL (operands[5]) - 8) << 4;
  mask |= (INTVAL (operands[6]) - 8) << 6;
  operands[3] = GEN_INT (mask);

  return "xvpermi.w\t%u0,%u1,%3";
}
  [(set_attr "type" "simd_bit")
   (set_attr "mode" "<MODE>")])

(define_expand "lasx_xvld"
  [(match_operand:V32QI 0 "register_operand")
   (match_operand 1 "pmode_register_operand")
   (match_operand 2 "aq12b_operand")]
  "ISA_HAS_LASX"
{
  rtx addr = plus_constant (GET_MODE (operands[1]), operands[1],
			    INTVAL (operands[2]));
  loongarch_emit_move (operands[0], gen_rtx_MEM (V32QImode, addr));
  DONE;
})

(define_expand "lasx_xvst"
  [(match_operand:V32QI 0 "register_operand")
   (match_operand 1 "pmode_register_operand")
   (match_operand 2 "aq12b_operand")]
  "ISA_HAS_LASX"
{
  rtx addr = plus_constant (GET_MODE (operands[1]), operands[1],
			    INTVAL (operands[2]));
  loongarch_emit_move (gen_rtx_MEM (V32QImode, addr), operands[0]);
  DONE;
})

(define_expand "lasx_xvstelm_<lasxfmt_f>"
  [(match_operand:LASX 0 "register_operand")
   (match_operand 3 "const_<indeximm256>_operand")
   (match_operand 2 "aq8<lasxfmt>_operand")
   (match_operand 1 "pmode_register_operand")]
  "ISA_HAS_LASX"
{
  emit_insn (gen_lasx_xvstelm_<lasxfmt_f>_insn
	     (operands[1], operands[2], operands[0], operands[3]));
  DONE;
})

(define_insn "lasx_xvstelm_<lasxfmt_f>_insn"
  [(set (mem:<UNITMODE> (plus:DI (match_operand:DI 0 "register_operand" "r")
				 (match_operand 1 "aq8<lasxfmt>_operand")))
	(vec_select:<UNITMODE>
	  (match_operand:LASX 2 "register_operand" "f")
	  (parallel [(match_operand 3 "const_<indeximm256>_operand" "")])))]
  "ISA_HAS_LASX"
{
  return "xvstelm.<lasxfmt>\t%u2,%0,%1,%3";
}
  [(set_attr "type" "simd_store")
   (set_attr "mode" "<MODE>")
   (set_attr "length" "4")])

;; Offset is "0"
(define_insn "lasx_xvstelm_<lasxfmt_f>_insn_0"
  [(set (mem:<UNITMODE> (match_operand:DI 0 "register_operand" "r"))
    (vec_select:<UNITMODE>
      (match_operand:LASX_WD 1 "register_operand" "f")
      (parallel [(match_operand:SI 2 "const_<indeximm256>_operand")])))]
  "ISA_HAS_LASX"
{
    return "xvstelm.<lasxfmt>\t%u1,%0,0,%2";
}
  [(set_attr "type" "simd_store")
   (set_attr "mode" "<MODE>")
   (set_attr "length" "4")])

(define_insn "lasx_xvinsve0_<lasxfmt_f>"
  [(set (match_operand:LASX_WD 0 "register_operand" "=f")
	(unspec:LASX_WD [(match_operand:LASX_WD 1 "register_operand" "0")
			 (match_operand:LASX_WD 2 "register_operand" "f")
			 (match_operand 3 "const_<indeximm256>_operand" "")]
			UNSPEC_LASX_XVINSVE0))]
  "ISA_HAS_LASX"
  "xvinsve0.<lasxfmt>\t%u0,%u2,%3"
  [(set_attr "type" "simd_shf")
   (set_attr "mode" "<MODE>")])

(define_insn "lasx_xvinsve0_<lasxfmt_f>_scalar"
  [(set (match_operand:FLASX 0 "register_operand" "=f")
    (vec_merge:FLASX
      (vec_duplicate:FLASX
        (match_operand:<UNITMODE> 1 "register_operand" "f"))
      (match_operand:FLASX 2 "register_operand" "0")
      (match_operand 3 "const_<bitmask256>_operand" "")))]
  "ISA_HAS_LASX"
  "xvinsve0.<lasxfmt>\t%u0,%u1,%y3"
  [(set_attr "type" "simd_insert")
   (set_attr "mode" "<MODE>")])

(define_insn "lasx_xvpickve_<lasxfmt_f>"
  [(set (match_operand:LASX_WD 0 "register_operand" "=f")
	(unspec:LASX_WD [(match_operand:LASX_WD 1 "register_operand" "f")
			 (match_operand 2 "const_<indeximm256>_operand" "")]
			UNSPEC_LASX_XVPICKVE))]
  "ISA_HAS_LASX"
  "xvpickve.<lasxfmt>\t%u0,%u1,%2"
  [(set_attr "type" "simd_shf")
   (set_attr "mode" "<MODE>")])

(define_insn "lasx_xvpickve_<lasxfmt_f>_scalar"
  [(set (match_operand:<UNITMODE> 0 "register_operand" "=f")
	(vec_select:<UNITMODE>
	 (match_operand:FLASX 1 "register_operand" "f")
	 (parallel [(match_operand 2 "const_<indeximm256>_operand" "")])))]
  "ISA_HAS_LASX"
  "xvpickve.<lasxfmt>\t%u0,%u1,%2"
  [(set_attr "type" "simd_shf")
   (set_attr "mode" "<MODE>")])

(define_insn "lasx_xvssrlrn_<hlasxfmt>_<lasxfmt>"
  [(set (match_operand:<VHSMODE256> 0 "register_operand" "=f")
	(unspec:<VHSMODE256> [(match_operand:ILASX_DWH 1 "register_operand" "f")
			      (match_operand:ILASX_DWH 2 "register_operand" "f")]
			     UNSPEC_LASX_XVSSRLRN))]
  "ISA_HAS_LASX"
  "xvssrlrn.<hlasxfmt>.<lasxfmt>\t%u0,%u1,%u2"
  [(set_attr "type" "simd_int_arith")
   (set_attr "mode" "<MODE>")])

(define_insn "iorn<mode>3"
  [(set (match_operand:ILASX 0 "register_operand" "=f")
	(ior:ILASX (not:ILASX (match_operand:ILASX 2 "register_operand" "f"))
		   (match_operand:ILASX 1 "register_operand" "f")))]
  "ISA_HAS_LASX"
  "xvorn.v\t%u0,%u1,%u2"
  [(set_attr "type" "simd_logic")
   (set_attr "mode" "<MODE>")])

(define_insn "lasx_xvextl_qu_du"
  [(set (match_operand:V4DI 0 "register_operand" "=f")
	(unspec:V4DI [(match_operand:V4DI 1 "register_operand" "f")]
		     UNSPEC_LASX_XVEXTL_QU_DU))]
  "ISA_HAS_LASX"
  "xvextl.qu.du\t%u0,%u1"
  [(set_attr "type" "simd_bit")
   (set_attr "mode" "V4DI")])

(define_insn "lasx_xvldi"
  [(set (match_operand:V4DI 0 "register_operand" "=f")
	(unspec:V4DI[(match_operand 1 "const_imm13_operand")]
		    UNSPEC_LASX_XVLDI))]
  "ISA_HAS_LASX"
{
  HOST_WIDE_INT val = INTVAL (operands[1]);
  if (val < 0)
    {
      HOST_WIDE_INT modeVal = (val & 0xf00) >> 8;
      if (modeVal < 13)
	return  "xvldi\t%u0,%1";
      else
	{
	  sorry ("imm13 only support 0000 ~ 1100 in bits '12 ~ 9' when bit '13' is 1");
	  return "#";
	}
    }
  else
    return "xvldi\t%u0,%1";
}
  [(set_attr "type" "simd_load")
   (set_attr "mode" "V4DI")])

(define_insn "lasx_xvldx"
  [(set (match_operand:V32QI 0 "register_operand" "=f")
	(unspec:V32QI [(match_operand:DI 1 "register_operand" "r")
		       (match_operand:DI 2 "reg_or_0_operand" "rJ")]
		      UNSPEC_LASX_XVLDX))]
  "ISA_HAS_LASX"
{
  return "xvldx\t%u0,%1,%z2";
}
  [(set_attr "type" "simd_load")
   (set_attr "mode" "V32QI")])

(define_insn "lasx_xvstx"
  [(set (mem:V32QI (plus:DI (match_operand:DI 1 "register_operand" "r")
			    (match_operand:DI 2 "reg_or_0_operand" "rJ")))
	(unspec: V32QI[(match_operand:V32QI 0 "register_operand" "f")]
		      UNSPEC_LASX_XVSTX))]

  "ISA_HAS_LASX"
{
  return "xvstx\t%u0,%1,%z2";
}
  [(set_attr "type" "simd_store")
   (set_attr "mode" "DI")])

(define_expand "vec_widen_<su>add_hi_<mode>"
  [(match_operand:<VDMODE256> 0 "register_operand")
   (any_extend:<VDMODE256> (match_operand:ILASX_HB 1 "register_operand"))
   (any_extend:<VDMODE256> (match_operand:ILASX_HB 2 "register_operand"))]
  "ISA_HAS_LASX"
{
  loongarch_expand_vec_widen_hilo (operands[0], operands[1], operands[2],
                        <u_bool>, true, "add");
  DONE;
})

(define_expand "vec_widen_<su>add_lo_<mode>"
  [(match_operand:<VDMODE256> 0 "register_operand")
   (any_extend:<VDMODE256> (match_operand:ILASX_HB 1 "register_operand"))
   (any_extend:<VDMODE256> (match_operand:ILASX_HB 2 "register_operand"))]
  "ISA_HAS_LASX"
{
  loongarch_expand_vec_widen_hilo (operands[0], operands[1], operands[2],
                        <u_bool>, false, "add");
  DONE;
})

(define_expand "vec_widen_<su>sub_hi_<mode>"
  [(match_operand:<VDMODE256> 0 "register_operand")
   (any_extend:<VDMODE256> (match_operand:ILASX_HB 1 "register_operand"))
   (any_extend:<VDMODE256> (match_operand:ILASX_HB 2 "register_operand"))]
  "ISA_HAS_LASX"
{
  loongarch_expand_vec_widen_hilo (operands[0], operands[1], operands[2],
                        <u_bool>, true, "sub");
  DONE;
})

(define_expand "vec_widen_<su>sub_lo_<mode>"
  [(match_operand:<VDMODE256> 0 "register_operand")
   (any_extend:<VDMODE256> (match_operand:ILASX_HB 1 "register_operand"))
   (any_extend:<VDMODE256> (match_operand:ILASX_HB 2 "register_operand"))]
  "ISA_HAS_LASX"
{
  loongarch_expand_vec_widen_hilo (operands[0], operands[1], operands[2],
                        <u_bool>, false, "sub");
  DONE;
})

(define_expand "vec_widen_<su>mult_hi_<mode>"
  [(match_operand:<VDMODE256> 0 "register_operand")
   (any_extend:<VDMODE256> (match_operand:ILASX_HB 1 "register_operand"))
   (any_extend:<VDMODE256> (match_operand:ILASX_HB 2 "register_operand"))]
  "ISA_HAS_LASX"
{
  loongarch_expand_vec_widen_hilo (operands[0], operands[1], operands[2],
                        <u_bool>, true, "mult");
  DONE;
})

(define_expand "vec_widen_<su>mult_lo_<mode>"
  [(match_operand:<VDMODE256> 0 "register_operand")
   (any_extend:<VDMODE256> (match_operand:ILASX_HB 1 "register_operand"))
   (any_extend:<VDMODE256> (match_operand:ILASX_HB 2 "register_operand"))]
  "ISA_HAS_LASX"
{
  loongarch_expand_vec_widen_hilo (operands[0], operands[1], operands[2],
                        <u_bool>, false, "mult");
  DONE;
})

;; Vector reduction operation
(define_expand "reduc_plus_scal_v4di"
  [(match_operand:DI 0 "register_operand")
   (match_operand:V4DI 1 "register_operand")]
  "ISA_HAS_LASX"
{
  rtx tmp = gen_reg_rtx (V4DImode);
  rtx tmp1 = gen_reg_rtx (V4DImode);
  rtx vec_res = gen_reg_rtx (V4DImode);
  emit_insn (gen_lasx_xvhaddw_q_d (tmp, operands[1], operands[1]));
  emit_insn (gen_lasx_xvpermi_d_v4di (tmp1, tmp, GEN_INT (2)));
  emit_insn (gen_addv4di3 (vec_res, tmp, tmp1));
  emit_insn (gen_vec_extractv4didi (operands[0], vec_res, const0_rtx));
  DONE;
})

(define_expand "reduc_plus_scal_v8si"
  [(match_operand:SI 0 "register_operand")
   (match_operand:V8SI 1 "register_operand")]
  "ISA_HAS_LASX"
{
  rtx tmp = gen_reg_rtx (V4DImode);
  rtx tmp1 = gen_reg_rtx (V4DImode);
  rtx vec_res = gen_reg_rtx (V4DImode);
  emit_insn (gen_lasx_xvhaddw_d_w (tmp, operands[1], operands[1]));
  emit_insn (gen_lasx_xvhaddw_q_d (tmp1, tmp, tmp));
  emit_insn (gen_lasx_xvpermi_d_v4di (tmp, tmp1, GEN_INT (2)));
  emit_insn (gen_addv4di3 (vec_res, tmp, tmp1));
  emit_insn (gen_vec_extractv8sisi (operands[0], gen_lowpart (V8SImode,vec_res),
				    const0_rtx));
  DONE;
})

(define_expand "reduc_plus_scal_<mode>"
  [(match_operand:<UNITMODE> 0 "register_operand")
   (match_operand:FLASX 1 "register_operand")]
  "ISA_HAS_LASX"
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
     (match_operand:ILASX 1 "register_operand"))]
  "ISA_HAS_LASX"
{
  rtx tmp = gen_reg_rtx (<MODE>mode);
  loongarch_expand_vector_reduc (gen_<optab><mode>3, tmp, operands[1]);
  emit_insn (gen_vec_extract<mode><unitmode> (operands[0], tmp,
					      const0_rtx));
  DONE;
})

(define_expand "reduc_smax_scal_<mode>"
  [(match_operand:<UNITMODE> 0 "register_operand")
   (match_operand:LASX 1 "register_operand")]
  "ISA_HAS_LASX"
{
  rtx tmp = gen_reg_rtx (<MODE>mode);
  loongarch_expand_vector_reduc (gen_smax<mode>3, tmp, operands[1]);
  emit_insn (gen_vec_extract<mode><unitmode> (operands[0], tmp,
					      const0_rtx));
  DONE;
})

(define_expand "reduc_smin_scal_<mode>"
  [(match_operand:<UNITMODE> 0 "register_operand")
   (match_operand:LASX 1 "register_operand")]
  "ISA_HAS_LASX"
{
  rtx tmp = gen_reg_rtx (<MODE>mode);
  loongarch_expand_vector_reduc (gen_smin<mode>3, tmp, operands[1]);
  emit_insn (gen_vec_extract<mode><unitmode> (operands[0], tmp,
					      const0_rtx));
  DONE;
})

(define_expand "reduc_umax_scal_<mode>"
  [(match_operand:<UNITMODE> 0 "register_operand")
   (match_operand:ILASX 1 "register_operand")]
  "ISA_HAS_LASX"
{
  rtx tmp = gen_reg_rtx (<MODE>mode);
  loongarch_expand_vector_reduc (gen_umax<mode>3, tmp, operands[1]);
  emit_insn (gen_vec_extract<mode><unitmode> (operands[0], tmp,
					      const0_rtx));
  DONE;
})

(define_expand "reduc_umin_scal_<mode>"
  [(match_operand:<UNITMODE> 0 "register_operand")
   (match_operand:ILASX 1 "register_operand")]
  "ISA_HAS_LASX"
{
  rtx tmp = gen_reg_rtx (<MODE>mode);
  loongarch_expand_vector_reduc (gen_umin<mode>3, tmp, operands[1]);
  emit_insn (gen_vec_extract<mode><unitmode> (operands[0], tmp,
					      const0_rtx));
  DONE;
})

(define_expand "avg<mode>3_ceil"
  [(match_operand:ILASX_WHB 0 "register_operand")
   (match_operand:ILASX_WHB 1 "register_operand")
   (match_operand:ILASX_WHB 2 "register_operand")]
  "ISA_HAS_LASX"
{
  emit_insn (gen_lasx_xvavgr_s_<lasxfmt> (operands[0],
	operands[1], operands[2]));
  DONE;
})

(define_expand "uavg<mode>3_ceil"
  [(match_operand:ILASX_WHB 0 "register_operand")
   (match_operand:ILASX_WHB 1 "register_operand")
   (match_operand:ILASX_WHB 2 "register_operand")]
  "ISA_HAS_LASX"
{
  emit_insn (gen_lasx_xvavgr_u_<lasxfmt_u> (operands[0],
	operands[1], operands[2]));
  DONE;
})

(define_expand "avg<mode>3_floor"
  [(match_operand:ILASX_WHB 0 "register_operand")
   (match_operand:ILASX_WHB 1 "register_operand")
   (match_operand:ILASX_WHB 2 "register_operand")]
  "ISA_HAS_LASX"
{
  emit_insn (gen_lasx_xvavg_s_<lasxfmt> (operands[0],
	operands[1], operands[2]));
  DONE;
})

(define_expand "uavg<mode>3_floor"
  [(match_operand:ILASX_WHB 0 "register_operand")
   (match_operand:ILASX_WHB 1 "register_operand")
   (match_operand:ILASX_WHB 2 "register_operand")]
  "ISA_HAS_LASX"
{
  emit_insn (gen_lasx_xvavg_u_<lasxfmt_u> (operands[0],
	operands[1], operands[2]));
  DONE;
})

(define_expand "usadv32qi"
  [(match_operand:V8SI 0 "register_operand")
   (match_operand:V32QI 1 "register_operand")
   (match_operand:V32QI 2 "register_operand")
   (match_operand:V8SI 3 "register_operand")]
  "ISA_HAS_LASX"
{
  rtx t1 = gen_reg_rtx (V32QImode);
  rtx t2 = gen_reg_rtx (V16HImode);
  rtx t3 = gen_reg_rtx (V8SImode);
  emit_insn (gen_lasx_xvabsd_u_bu (t1, operands[1], operands[2]));
  emit_insn (gen_lasx_xvhaddw_hu_bu (t2, t1, t1));
  emit_insn (gen_lasx_xvhaddw_wu_hu (t3, t2, t2));
  emit_insn (gen_addv8si3 (operands[0], t3, operands[3]));
  DONE;
})

(define_expand "ssadv32qi"
  [(match_operand:V8SI 0 "register_operand")
   (match_operand:V32QI 1 "register_operand")
   (match_operand:V32QI 2 "register_operand")
   (match_operand:V8SI 3 "register_operand")]
  "ISA_HAS_LASX"
{
  rtx t1 = gen_reg_rtx (V32QImode);
  rtx t2 = gen_reg_rtx (V16HImode);
  rtx t3 = gen_reg_rtx (V8SImode);
  emit_insn (gen_lasx_xvabsd_s_b (t1, operands[1], operands[2]));
  emit_insn (gen_lasx_xvhaddw_hu_bu (t2, t1, t1));
  emit_insn (gen_lasx_xvhaddw_wu_hu (t3, t2, t2));
  emit_insn (gen_addv8si3 (operands[0], t3, operands[3]));
  DONE;
})
