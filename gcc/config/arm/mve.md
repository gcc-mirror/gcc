;; Arm M-profile Vector Extension Machine Description
;; Copyright (C) 2019-2020 Free Software Foundation, Inc.
;;
;; This file is part of GCC.
;;
;; GCC is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; GCC is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GCC; see the file COPYING3.  If not see
;; <http://www.gnu.org/licenses/>.

(define_mode_attr V_sz_elem2 [(V16QI "s8") (V8HI "u16") (V4SI "u32")
			      (V2DI "u64")])
(define_mode_iterator MVE_types [V16QI V8HI V4SI V2DI TI V8HF V4SF V2DF])
(define_mode_iterator MVE_VLD_ST [V16QI V8HI V4SI V8HF V4SF])
(define_mode_iterator MVE_0 [V8HF V4SF])
(define_mode_iterator MVE_1 [V16QI V8HI V4SI V2DI])
(define_mode_iterator MVE_3 [V16QI V8HI])
(define_mode_iterator MVE_2 [V16QI V8HI V4SI])
(define_mode_iterator MVE_5 [V8HI V4SI])

(define_c_enum "unspec" [VST4Q VRNDXQ_F VRNDQ_F VRNDPQ_F VRNDNQ_F VRNDMQ_F
			 VRNDAQ_F VREV64Q_F VNEGQ_F VDUPQ_N_F VABSQ_F VREV32Q_F
			 VCVTTQ_F32_F16 VCVTBQ_F32_F16 VCVTQ_TO_F_S VQNEGQ_S
			 VCVTQ_TO_F_U VREV16Q_S VREV16Q_U VADDLVQ_S VMVNQ_N_S
			 VMVNQ_N_U VCVTAQ_S VCVTAQ_U VREV64Q_S VREV64Q_U
			 VQABSQ_S VNEGQ_S VMVNQ_S VMVNQ_U VDUPQ_N_U VDUPQ_N_S
			 VCLZQ_U VCLZQ_S VCLSQ_S VADDVQ_S VADDVQ_U VABSQ_S
			 VREV32Q_U VREV32Q_S VMOVLTQ_U VMOVLTQ_S VMOVLBQ_S
			 VMOVLBQ_U VCVTQ_FROM_F_S VCVTQ_FROM_F_U VCVTPQ_S
			 VCVTPQ_U VCVTNQ_S VCVTNQ_U VCVTMQ_S VCVTMQ_U
			 VADDLVQ_U VCTP8Q VCTP16Q VCTP32Q VCTP64Q VPNOT
			 VCREATEQ_F VCVTQ_N_TO_F_S VCVTQ_N_TO_F_U VBRSRQ_N_F
			 VSUBQ_N_F VCREATEQ_U VCREATEQ_S VSHRQ_N_S VSHRQ_N_U
			 VCVTQ_N_FROM_F_S VCVTQ_N_FROM_F_U VADDLVQ_P_S
			 VADDLVQ_P_U VCMPNEQ_U VCMPNEQ_S VSHLQ_S VSHLQ_U VABDQ_S
			 VADDQ_N_S VADDVAQ_S VADDVQ_P_S VANDQ_S VBICQ_S
			 VBRSRQ_N_S VCADDQ_ROT270_S VCADDQ_ROT90_S VCMPEQQ_S
			 VCMPEQQ_N_S VCMPNEQ_N_S VEORQ_S VHADDQ_S VHADDQ_N_S
			 VHSUBQ_S VHSUBQ_N_S VMAXQ_S VMAXVQ_S VMINQ_S VMINVQ_S
			 VMLADAVQ_S VMULHQ_S VMULLBQ_INT_S VMULLTQ_INT_S VMULQ_S
			 VMULQ_N_S VORNQ_S VORRQ_S VQADDQ_S VQADDQ_N_S VQRSHLQ_S
			 VQRSHLQ_N_S VQSHLQ_S VQSHLQ_N_S VQSHLQ_R_S VQSUBQ_S
			 VQSUBQ_N_S VRHADDQ_S VRMULHQ_S VRSHLQ_S VRSHLQ_N_S
			 VRSHRQ_N_S VSHLQ_N_S VSHLQ_R_S VSUBQ_S VSUBQ_N_S
			 VABDQ_U VADDQ_N_U VADDVAQ_U VADDVQ_P_U VANDQ_U VBICQ_U
			 VBRSRQ_N_U VCADDQ_ROT270_U VCADDQ_ROT90_U VCMPEQQ_U
			 VCMPEQQ_N_U VCMPNEQ_N_U VEORQ_U VHADDQ_U VHADDQ_N_U
			 VHSUBQ_U VHSUBQ_N_U VMAXQ_U VMAXVQ_U VMINQ_U VMINVQ_U
			 VMLADAVQ_U VMULHQ_U VMULLBQ_INT_U VMULLTQ_INT_U VMULQ_U
			 VMULQ_N_U VORNQ_U VORRQ_U VQADDQ_U VQADDQ_N_U VQRSHLQ_U
			 VQRSHLQ_N_U VQSHLQ_U VQSHLQ_N_U VQSHLQ_R_U VQSUBQ_U
			 VQSUBQ_N_U VRHADDQ_U VRMULHQ_U VRSHLQ_U VRSHLQ_N_U
			 VRSHRQ_N_U VSHLQ_N_U VSHLQ_R_U VSUBQ_U VSUBQ_N_U
			 VCMPGEQ_N_S VCMPGEQ_S VCMPGTQ_N_S VCMPGTQ_S VCMPLEQ_N_S
			 VCMPLEQ_S VCMPLTQ_N_S VCMPLTQ_S VHCADDQ_ROT270_S
			 VHCADDQ_ROT90_S VMAXAQ_S VMAXAVQ_S VMINAQ_S VMINAVQ_S
			 VMLADAVXQ_S VMLSDAVQ_S VMLSDAVXQ_S VQDMULHQ_N_S
			 VQDMULHQ_S VQRDMULHQ_N_S VQRDMULHQ_S VQSHLUQ_N_S
			 VCMPCSQ_N_U VCMPCSQ_U VCMPHIQ_N_U VCMPHIQ_U VABDQ_M_S
			 VABDQ_M_U VABDQ_F VADDQ_N_F VANDQ_F VBICQ_F
			 VCADDQ_ROT270_F VCADDQ_ROT90_F VCMPEQQ_F VCMPEQQ_N_F
			 VCMPGEQ_F VCMPGEQ_N_F VCMPGTQ_F VCMPGTQ_N_F VCMPLEQ_F
			 VCMPLEQ_N_F VCMPLTQ_F VCMPLTQ_N_F VCMPNEQ_F VCMPNEQ_N_F
			 VCMULQ_F VCMULQ_ROT180_F VCMULQ_ROT270_F VCMULQ_ROT90_F
			 VEORQ_F VMAXNMAQ_F VMAXNMAVQ_F VMAXNMQ_F VMAXNMVQ_F
			 VMINNMAQ_F VMINNMAVQ_F VMINNMQ_F VMINNMVQ_F VMULQ_F
			 VMULQ_N_F VORNQ_F VORRQ_F VSUBQ_F VADDLVAQ_U
			 VADDLVAQ_S VBICQ_N_U VBICQ_N_S VCTP8Q_M VCTP16Q_M
			 VCTP32Q_M VCTP64Q_M VCVTBQ_F16_F32 VCVTTQ_F16_F32
			 VMLALDAVQ_U VMLALDAVXQ_U VMLALDAVXQ_S VMLALDAVQ_S
			 VMLSLDAVQ_S VMLSLDAVXQ_S VMOVNBQ_U VMOVNBQ_S
			 VMOVNTQ_U VMOVNTQ_S VORRQ_N_S VORRQ_N_U VQDMULLBQ_N_S
			 VQDMULLBQ_S VQDMULLTQ_N_S VQDMULLTQ_S VQMOVNBQ_U
			 VQMOVNBQ_S VQMOVUNBQ_S VQMOVUNTQ_S VRMLALDAVHXQ_S
			 VRMLSLDAVHQ_S VRMLSLDAVHXQ_S VSHLLBQ_S
			 VSHLLBQ_U VSHLLTQ_U VSHLLTQ_S VQMOVNTQ_U VQMOVNTQ_S
			 VSHLLBQ_N_S VSHLLBQ_N_U VSHLLTQ_N_U VSHLLTQ_N_S
			 VRMLALDAVHQ_U VRMLALDAVHQ_S VMULLTQ_POLY_P
			 VMULLBQ_POLY_P VBICQ_M_N_S VBICQ_M_N_U VCMPEQQ_M_F
			 VCVTAQ_M_S VCVTAQ_M_U VCVTQ_M_TO_F_S VCVTQ_M_TO_F_U
			 VQRSHRNBQ_N_U VQRSHRNBQ_N_S VQRSHRUNBQ_N_S
			 VRMLALDAVHAQ_S VABAVQ_S VABAVQ_U VSHLCQ_S VSHLCQ_U
			 VRMLALDAVHAQ_U])

(define_mode_attr MVE_CNVT [(V8HI "V8HF") (V4SI "V4SF")
			    (V8HF "V8HI") (V4SF "V4SI")])

(define_int_attr supf [(VCVTQ_TO_F_S "s") (VCVTQ_TO_F_U "u") (VREV16Q_S "s")
		       (VREV16Q_U "u") (VMVNQ_N_S "s") (VMVNQ_N_U "u")
		       (VCVTAQ_U "u") (VCVTAQ_S "s") (VREV64Q_S "s")
		       (VREV64Q_U "u") (VMVNQ_S "s") (VMVNQ_U "u")
		       (VDUPQ_N_U "u") (VDUPQ_N_S"s") (VADDVQ_S "s")
		       (VADDVQ_U "u") (VADDVQ_S "s") (VADDVQ_U "u")
		       (VMOVLTQ_U "u") (VMOVLTQ_S "s") (VMOVLBQ_S "s")
		       (VMOVLBQ_U "u") (VCVTQ_FROM_F_S "s") (VCVTQ_FROM_F_U "u")
		       (VCVTPQ_S "s") (VCVTPQ_U "u") (VCVTNQ_S "s")
		       (VCVTNQ_U "u") (VCVTMQ_S "s") (VCVTMQ_U "u")
		       (VCLZQ_U "u") (VCLZQ_S "s") (VREV32Q_U "u")
		       (VREV32Q_S "s") (VADDLVQ_U "u") (VADDLVQ_S "s")
		       (VCVTQ_N_TO_F_S "s") (VCVTQ_N_TO_F_U "u")
		       (VCREATEQ_U "u") (VCREATEQ_S "s") (VSHRQ_N_S "s")
		       (VSHRQ_N_U "u") (VCVTQ_N_FROM_F_S "s") (VSHLQ_U "u")
		       (VCVTQ_N_FROM_F_U "u") (VADDLVQ_P_S "s") (VSHLQ_S "s")
		       (VADDLVQ_P_U "u") (VCMPNEQ_U "u") (VCMPNEQ_S "s")
		       (VABDQ_M_S "s") (VABDQ_M_U "u") (VABDQ_S "s")
		       (VABDQ_U "u") (VADDQ_N_S "s") (VADDQ_N_U "u")
		       (VADDVQ_P_S "s")	(VADDVQ_P_U "u") (VANDQ_S "s")
		       (VANDQ_U "u") (VBICQ_S "s") (VBICQ_U "u")
		       (VBRSRQ_N_S "s") (VBRSRQ_N_U "u") (VCADDQ_ROT270_S "s")
		       (VCADDQ_ROT270_U "u") (VCADDQ_ROT90_S "s")
		       (VCMPEQQ_S "s") (VCMPEQQ_U "u") (VCADDQ_ROT90_U "u")
		       (VCMPEQQ_N_S "s") (VCMPEQQ_N_U "u") (VCMPNEQ_N_S "s")
		       (VCMPNEQ_N_U "u") (VEORQ_S "s") (VEORQ_U "u")
		       (VHADDQ_N_S "s") (VHADDQ_N_U "u") (VHADDQ_S "s")
		       (VHADDQ_U "u") (VHSUBQ_N_S "s")	(VHSUBQ_N_U "u")
		       (VHSUBQ_S "s") (VMAXQ_S "s") (VMAXQ_U "u") (VHSUBQ_U "u")
		       (VMAXVQ_S "s") (VMAXVQ_U "u") (VMINQ_S "s") (VMINQ_U "u")
		       (VMINVQ_S "s") (VMINVQ_U "u") (VMLADAVQ_S "s")
		       (VMLADAVQ_U "u") (VMULHQ_S "s") (VMULHQ_U "u")
		       (VMULLBQ_INT_S "s") (VMULLBQ_INT_U "u") (VQADDQ_S "s")
		       (VMULLTQ_INT_S "s") (VMULLTQ_INT_U "u") (VQADDQ_U "u")
		       (VMULQ_N_S "s") (VMULQ_N_U "u") (VMULQ_S "s")
		       (VMULQ_U "u") (VORNQ_S "s") (VORNQ_U "u") (VORRQ_S "s")
		       (VORRQ_U "u") (VQADDQ_N_S "s") (VQADDQ_N_U "u")
		       (VQRSHLQ_N_S "s") (VQRSHLQ_N_U "u") (VQRSHLQ_S "s")
		       (VQRSHLQ_U "u") (VQSHLQ_N_S "s")	(VQSHLQ_N_U "u")
		       (VQSHLQ_R_S "s") (VQSHLQ_R_U "u") (VQSHLQ_S "s")
		       (VQSHLQ_U "u") (VQSUBQ_N_S "s") (VQSUBQ_N_U "u")
		       (VQSUBQ_S "s") (VQSUBQ_U "u") (VRHADDQ_S "s")
		       (VRHADDQ_U "u") (VRMULHQ_S "s") (VRMULHQ_U "u")
		       (VRSHLQ_N_S "s") (VRSHLQ_N_U "u") (VRSHLQ_S "s")
		       (VRSHLQ_U "u") (VRSHRQ_N_S "s") (VRSHRQ_N_U "u")
		       (VSHLQ_N_S "s") (VSHLQ_N_U "u") (VSHLQ_R_S "s")
		       (VSHLQ_R_U "u") (VSUBQ_N_S "s") (VSUBQ_N_U "u")
		       (VSUBQ_S "s") (VSUBQ_U "u") (VADDVAQ_S "s")
		       (VADDVAQ_U "u") (VADDLVAQ_S "s") (VADDLVAQ_U "u")
		       (VBICQ_N_S "s") (VBICQ_N_U "u") (VMLALDAVQ_U "u")
		       (VMLALDAVQ_S "s") (VMLALDAVXQ_U "u") (VMLALDAVXQ_S "s")
		       (VMOVNBQ_U "u") (VMOVNBQ_S "s") (VMOVNTQ_U "u")
		       (VMOVNTQ_S "s") (VORRQ_N_S "s") (VORRQ_N_U "u")
		       (VQMOVNBQ_U "u") (VQMOVNBQ_S "s") (VQMOVNTQ_S "s")
		       (VQMOVNTQ_U "u") (VSHLLBQ_N_U "u") (VSHLLBQ_N_S "s")
		       (VSHLLTQ_N_U "u") (VSHLLTQ_N_S "s") (VRMLALDAVHQ_U "u")
		       (VRMLALDAVHQ_S "s") (VBICQ_M_N_S "s") (VBICQ_M_N_U "u")
		       (VCVTAQ_M_S "s") (VCVTAQ_M_U "u") (VCVTQ_M_TO_F_S "s")
		       (VCVTQ_M_TO_F_U "u") (VQRSHRNBQ_N_S "s")
		       (VQRSHRNBQ_N_U "u") (VABAVQ_S "s") (VABAVQ_U "u")
		       (VRMLALDAVHAQ_U "u") (VRMLALDAVHAQ_S "s") (VSHLCQ_S "s")
		       (VSHLCQ_U "u")])

(define_int_attr mode1 [(VCTP8Q "8") (VCTP16Q "16") (VCTP32Q "32")
			(VCTP64Q "64") (VCTP8Q_M "8") (VCTP16Q_M "16")
			(VCTP32Q_M "32") (VCTP64Q_M "64")])
(define_mode_attr MVE_pred2 [(V16QI "mve_imm_8") (V8HI "mve_imm_16")
			     (V4SI "mve_imm_32")])
(define_mode_attr MVE_constraint2 [(V16QI "Rb") (V8HI "Rd") (V4SI "Rf")])
(define_mode_attr MVE_LANES [(V16QI "16") (V8HI "8") (V4SI "4")])

(define_int_iterator VCVTQ_TO_F [VCVTQ_TO_F_S VCVTQ_TO_F_U])
(define_int_iterator VMVNQ_N [VMVNQ_N_U VMVNQ_N_S])
(define_int_iterator VREV64Q [VREV64Q_S VREV64Q_U])
(define_int_iterator VCVTQ_FROM_F [VCVTQ_FROM_F_S VCVTQ_FROM_F_U])
(define_int_iterator VREV16Q [VREV16Q_U VREV16Q_S])
(define_int_iterator VCVTAQ [VCVTAQ_U VCVTAQ_S])
(define_int_iterator VMVNQ [VMVNQ_U VMVNQ_S])
(define_int_iterator VDUPQ_N [VDUPQ_N_U VDUPQ_N_S])
(define_int_iterator VCLZQ [VCLZQ_U VCLZQ_S])
(define_int_iterator VADDVQ [VADDVQ_U VADDVQ_S])
(define_int_iterator VREV32Q [VREV32Q_U VREV32Q_S])
(define_int_iterator VMOVLBQ [VMOVLBQ_S VMOVLBQ_U])
(define_int_iterator VMOVLTQ [VMOVLTQ_U VMOVLTQ_S])
(define_int_iterator VCVTPQ [VCVTPQ_S VCVTPQ_U])
(define_int_iterator VCVTNQ [VCVTNQ_S VCVTNQ_U])
(define_int_iterator VCVTMQ [VCVTMQ_S VCVTMQ_U])
(define_int_iterator VADDLVQ [VADDLVQ_U VADDLVQ_S])
(define_int_iterator VCTPQ [VCTP8Q VCTP16Q VCTP32Q VCTP64Q])
(define_int_iterator VCTPQ_M [VCTP8Q_M VCTP16Q_M VCTP32Q_M VCTP64Q_M])
(define_int_iterator VCVTQ_N_TO_F [VCVTQ_N_TO_F_S VCVTQ_N_TO_F_U])
(define_int_iterator VCREATEQ [VCREATEQ_U VCREATEQ_S])
(define_int_iterator VSHRQ_N [VSHRQ_N_S VSHRQ_N_U])
(define_int_iterator VCVTQ_N_FROM_F [VCVTQ_N_FROM_F_S VCVTQ_N_FROM_F_U])
(define_int_iterator VADDLVQ_P [VADDLVQ_P_S VADDLVQ_P_U])
(define_int_iterator VCMPNEQ [VCMPNEQ_U VCMPNEQ_S])
(define_int_iterator VSHLQ [VSHLQ_S VSHLQ_U])
(define_int_iterator VABDQ [VABDQ_S VABDQ_U])
(define_int_iterator VADDQ_N [VADDQ_N_S VADDQ_N_U])
(define_int_iterator VADDVAQ [VADDVAQ_S VADDVAQ_U])
(define_int_iterator VADDVQ_P [VADDVQ_P_U VADDVQ_P_S])
(define_int_iterator VANDQ [VANDQ_U VANDQ_S])
(define_int_iterator VBICQ [VBICQ_S VBICQ_U])
(define_int_iterator VBRSRQ_N [VBRSRQ_N_U VBRSRQ_N_S])
(define_int_iterator VCADDQ_ROT270 [VCADDQ_ROT270_S VCADDQ_ROT270_U])
(define_int_iterator VCADDQ_ROT90 [VCADDQ_ROT90_U VCADDQ_ROT90_S])
(define_int_iterator VCMPEQQ [VCMPEQQ_U VCMPEQQ_S])
(define_int_iterator VCMPEQQ_N [VCMPEQQ_N_S VCMPEQQ_N_U])
(define_int_iterator VCMPNEQ_N [VCMPNEQ_N_U VCMPNEQ_N_S])
(define_int_iterator VEORQ [VEORQ_U VEORQ_S])
(define_int_iterator VHADDQ [VHADDQ_S VHADDQ_U])
(define_int_iterator VHADDQ_N [VHADDQ_N_U VHADDQ_N_S])
(define_int_iterator VHSUBQ [VHSUBQ_S VHSUBQ_U])
(define_int_iterator VHSUBQ_N [VHSUBQ_N_U VHSUBQ_N_S])
(define_int_iterator VMAXQ [VMAXQ_U VMAXQ_S])
(define_int_iterator VMAXVQ [VMAXVQ_U VMAXVQ_S])
(define_int_iterator VMINQ [VMINQ_S VMINQ_U])
(define_int_iterator VMINVQ [VMINVQ_U VMINVQ_S])
(define_int_iterator VMLADAVQ [VMLADAVQ_U VMLADAVQ_S])
(define_int_iterator VMULHQ [VMULHQ_S VMULHQ_U])
(define_int_iterator VMULLBQ_INT [VMULLBQ_INT_U VMULLBQ_INT_S])
(define_int_iterator VMULLTQ_INT [VMULLTQ_INT_U VMULLTQ_INT_S])
(define_int_iterator VMULQ [VMULQ_U VMULQ_S])
(define_int_iterator VMULQ_N [VMULQ_N_U VMULQ_N_S])
(define_int_iterator VORNQ [VORNQ_U VORNQ_S])
(define_int_iterator VORRQ [VORRQ_S VORRQ_U])
(define_int_iterator VQADDQ [VQADDQ_U VQADDQ_S])
(define_int_iterator VQADDQ_N [VQADDQ_N_S VQADDQ_N_U])
(define_int_iterator VQRSHLQ [VQRSHLQ_S VQRSHLQ_U])
(define_int_iterator VQRSHLQ_N [VQRSHLQ_N_S VQRSHLQ_N_U])
(define_int_iterator VQSHLQ [VQSHLQ_S VQSHLQ_U])
(define_int_iterator VQSHLQ_N [VQSHLQ_N_S VQSHLQ_N_U])
(define_int_iterator VQSHLQ_R [VQSHLQ_R_U VQSHLQ_R_S])
(define_int_iterator VQSUBQ [VQSUBQ_U VQSUBQ_S])
(define_int_iterator VQSUBQ_N [VQSUBQ_N_S VQSUBQ_N_U])
(define_int_iterator VRHADDQ [VRHADDQ_S VRHADDQ_U])
(define_int_iterator VRMULHQ [VRMULHQ_S VRMULHQ_U])
(define_int_iterator VRSHLQ [VRSHLQ_S VRSHLQ_U])
(define_int_iterator VRSHLQ_N [VRSHLQ_N_U VRSHLQ_N_S])
(define_int_iterator VRSHRQ_N [VRSHRQ_N_S VRSHRQ_N_U])
(define_int_iterator VSHLQ_N [VSHLQ_N_U VSHLQ_N_S])
(define_int_iterator VSHLQ_R [VSHLQ_R_S VSHLQ_R_U])
(define_int_iterator VSUBQ [VSUBQ_S VSUBQ_U])
(define_int_iterator VSUBQ_N [VSUBQ_N_S VSUBQ_N_U])
(define_int_iterator VADDLVAQ [VADDLVAQ_S VADDLVAQ_U])
(define_int_iterator VBICQ_N [VBICQ_N_S VBICQ_N_U])
(define_int_iterator VMLALDAVQ [VMLALDAVQ_U VMLALDAVQ_S])
(define_int_iterator VMLALDAVXQ [VMLALDAVXQ_U VMLALDAVXQ_S])
(define_int_iterator VMOVNBQ [VMOVNBQ_U VMOVNBQ_S])
(define_int_iterator VMOVNTQ [VMOVNTQ_S VMOVNTQ_U])
(define_int_iterator VORRQ_N [VORRQ_N_U VORRQ_N_S])
(define_int_iterator VQMOVNBQ [VQMOVNBQ_U VQMOVNBQ_S])
(define_int_iterator VQMOVNTQ [VQMOVNTQ_U VQMOVNTQ_S])
(define_int_iterator VSHLLBQ_N [VSHLLBQ_N_S VSHLLBQ_N_U])
(define_int_iterator VSHLLTQ_N [VSHLLTQ_N_U VSHLLTQ_N_S])
(define_int_iterator VRMLALDAVHQ [VRMLALDAVHQ_U VRMLALDAVHQ_S])
(define_int_iterator VBICQ_M_N [VBICQ_M_N_S VBICQ_M_N_U])
(define_int_iterator VCVTAQ_M [VCVTAQ_M_S VCVTAQ_M_U])
(define_int_iterator VCVTQ_M_TO_F [VCVTQ_M_TO_F_S VCVTQ_M_TO_F_U])
(define_int_iterator VQRSHRNBQ_N [VQRSHRNBQ_N_U VQRSHRNBQ_N_S])
(define_int_iterator VABAVQ [VABAVQ_S VABAVQ_U])
(define_int_iterator VSHLCQ [VSHLCQ_S VSHLCQ_U])
(define_int_iterator VRMLALDAVHAQ [VRMLALDAVHAQ_S VRMLALDAVHAQ_U])

(define_insn "*mve_mov<mode>"
  [(set (match_operand:MVE_types 0 "nonimmediate_operand" "=w,w,r,w,w,r,w,Us")
	(match_operand:MVE_types 1 "general_operand" "w,r,w,Dn,Usi,r,Dm,w"))]
  "TARGET_HAVE_MVE || TARGET_HAVE_MVE_FLOAT"
{
  if (which_alternative == 3 || which_alternative == 6)
    {
      int width, is_valid;
      static char templ[40];

      is_valid = simd_immediate_valid_for_move (operands[1], <MODE>mode,
	&operands[1], &width);

      gcc_assert (is_valid != 0);

      if (width == 0)
	return "vmov.f32\t%q0, %1  @ <mode>";
      else
	sprintf (templ, "vmov.i%d\t%%q0, %%x1  @ <mode>", width);
      return templ;
    }
  switch (which_alternative)
    {
    case 0:
      return "vmov\t%q0, %q1";
    case 1:
      return "vmov\t%e0, %Q1, %R1  @ <mode>\;vmov\t%f0, %J1, %K1";
    case 2:
      return "vmov\t%Q0, %R0, %e1  @ <mode>\;vmov\t%J0, %K0, %f1";
    case 4:
      if ((TARGET_HAVE_MVE_FLOAT && VALID_MVE_SF_MODE (<MODE>mode))
	  || (MEM_P (operands[1])
	      && GET_CODE (XEXP (operands[1], 0)) == LABEL_REF))
	return output_move_neon (operands);
      else
	return "vldrb.8 %q0, %E1";
    case 5:
      return output_move_neon (operands);
    case 7:
      return "vstrb.8 %q1, %E0";
    default:
      gcc_unreachable ();
      return "";
    }
}
  [(set_attr "type" "mve_move,mve_move,mve_move,mve_move,mve_load,mve_move,mve_move,mve_store")
   (set_attr "length" "4,8,8,4,8,8,4,4")
   (set_attr "thumb2_pool_range" "*,*,*,*,1018,*,*,*")
   (set_attr "neg_pool_range" "*,*,*,*,996,*,*,*")])

(define_insn "*mve_mov<mode>"
  [(set (match_operand:MVE_types 0 "s_register_operand" "=w,w")
	(vec_duplicate:MVE_types
	  (match_operand:SI 1 "nonmemory_operand" "r,i")))]
  "TARGET_HAVE_MVE || TARGET_HAVE_MVE_FLOAT"
{
  if (which_alternative == 0)
    return "vdup.<V_sz_elem>\t%q0, %1";
  return "vmov.<V_sz_elem>\t%q0, %1";
}
  [(set_attr "length" "4,4")
   (set_attr "type" "mve_move,mve_move")])

;;
;; [vst4q])
;;
(define_insn "mve_vst4q<mode>"
  [(set (match_operand:XI 0 "neon_struct_operand" "=Um")
	(unspec:XI [(match_operand:XI 1 "s_register_operand" "w")
		    (unspec:MVE_VLD_ST [(const_int 0)] UNSPEC_VSTRUCTDUMMY)]
	 VST4Q))
  ]
  "TARGET_HAVE_MVE"
{
   rtx ops[6];
   int regno = REGNO (operands[1]);
   ops[0] = gen_rtx_REG (TImode, regno);
   ops[1] = gen_rtx_REG (TImode, regno+4);
   ops[2] = gen_rtx_REG (TImode, regno+8);
   ops[3] = gen_rtx_REG (TImode, regno+12);
   rtx reg  = operands[0];
   while (reg && !REG_P (reg))
    reg = XEXP (reg, 0);
   gcc_assert (REG_P (reg));
   ops[4] = reg;
   ops[5] = operands[0];
   /* Here in first three instructions data is stored to ops[4]'s location but
      in the fourth instruction data is stored to operands[0], this is to
      support the writeback.  */
   output_asm_insn ("vst40.<V_sz_elem>\t{%q0, %q1, %q2, %q3}, [%4]\n\t"
		    "vst41.<V_sz_elem>\t{%q0, %q1, %q2, %q3}, [%4]\n\t"
		    "vst42.<V_sz_elem>\t{%q0, %q1, %q2, %q3}, [%4]\n\t"
		    "vst43.<V_sz_elem>\t{%q0, %q1, %q2, %q3}, %5", ops);
   return "";
}
  [(set_attr "length" "16")])

;;
;; [vrndxq_f])
;;
(define_insn "mve_vrndxq_f<mode>"
  [
   (set (match_operand:MVE_0 0 "s_register_operand" "=w")
	(unspec:MVE_0 [(match_operand:MVE_0 1 "s_register_operand" "w")]
	 VRNDXQ_F))
  ]
  "TARGET_HAVE_MVE && TARGET_HAVE_MVE_FLOAT"
  "vrintx.f%#<V_sz_elem>	%q0, %q1"
  [(set_attr "type" "mve_move")
])

;;
;; [vrndq_f])
;;
(define_insn "mve_vrndq_f<mode>"
  [
   (set (match_operand:MVE_0 0 "s_register_operand" "=w")
	(unspec:MVE_0 [(match_operand:MVE_0 1 "s_register_operand" "w")]
	 VRNDQ_F))
  ]
  "TARGET_HAVE_MVE && TARGET_HAVE_MVE_FLOAT"
  "vrintz.f%#<V_sz_elem>	%q0, %q1"
  [(set_attr "type" "mve_move")
])

;;
;; [vrndpq_f])
;;
(define_insn "mve_vrndpq_f<mode>"
  [
   (set (match_operand:MVE_0 0 "s_register_operand" "=w")
	(unspec:MVE_0 [(match_operand:MVE_0 1 "s_register_operand" "w")]
	 VRNDPQ_F))
  ]
  "TARGET_HAVE_MVE && TARGET_HAVE_MVE_FLOAT"
  "vrintp.f%#<V_sz_elem>	%q0, %q1"
  [(set_attr "type" "mve_move")
])

;;
;; [vrndnq_f])
;;
(define_insn "mve_vrndnq_f<mode>"
  [
   (set (match_operand:MVE_0 0 "s_register_operand" "=w")
	(unspec:MVE_0 [(match_operand:MVE_0 1 "s_register_operand" "w")]
	 VRNDNQ_F))
  ]
  "TARGET_HAVE_MVE && TARGET_HAVE_MVE_FLOAT"
  "vrintn.f%#<V_sz_elem>	%q0, %q1"
  [(set_attr "type" "mve_move")
])

;;
;; [vrndmq_f])
;;
(define_insn "mve_vrndmq_f<mode>"
  [
   (set (match_operand:MVE_0 0 "s_register_operand" "=w")
	(unspec:MVE_0 [(match_operand:MVE_0 1 "s_register_operand" "w")]
	 VRNDMQ_F))
  ]
  "TARGET_HAVE_MVE && TARGET_HAVE_MVE_FLOAT"
  "vrintm.f%#<V_sz_elem>	%q0, %q1"
  [(set_attr "type" "mve_move")
])

;;
;; [vrndaq_f])
;;
(define_insn "mve_vrndaq_f<mode>"
  [
   (set (match_operand:MVE_0 0 "s_register_operand" "=w")
	(unspec:MVE_0 [(match_operand:MVE_0 1 "s_register_operand" "w")]
	 VRNDAQ_F))
  ]
  "TARGET_HAVE_MVE && TARGET_HAVE_MVE_FLOAT"
  "vrinta.f%#<V_sz_elem>	%q0, %q1"
  [(set_attr "type" "mve_move")
])

;;
;; [vrev64q_f])
;;
(define_insn "mve_vrev64q_f<mode>"
  [
   (set (match_operand:MVE_0 0 "s_register_operand" "=w")
	(unspec:MVE_0 [(match_operand:MVE_0 1 "s_register_operand" "w")]
	 VREV64Q_F))
  ]
  "TARGET_HAVE_MVE && TARGET_HAVE_MVE_FLOAT"
  "vrev64.%#<V_sz_elem> %q0, %q1"
  [(set_attr "type" "mve_move")
])

;;
;; [vnegq_f])
;;
(define_insn "mve_vnegq_f<mode>"
  [
   (set (match_operand:MVE_0 0 "s_register_operand" "=w")
	(unspec:MVE_0 [(match_operand:MVE_0 1 "s_register_operand" "w")]
	 VNEGQ_F))
  ]
  "TARGET_HAVE_MVE && TARGET_HAVE_MVE_FLOAT"
  "vneg.f%#<V_sz_elem>  %q0, %q1"
  [(set_attr "type" "mve_move")
])

;;
;; [vdupq_n_f])
;;
(define_insn "mve_vdupq_n_f<mode>"
  [
   (set (match_operand:MVE_0 0 "s_register_operand" "=w")
	(unspec:MVE_0 [(match_operand:<V_elem> 1 "s_register_operand" "r")]
	 VDUPQ_N_F))
  ]
  "TARGET_HAVE_MVE && TARGET_HAVE_MVE_FLOAT"
  "vdup.%#<V_sz_elem>   %q0, %1"
  [(set_attr "type" "mve_move")
])

;;
;; [vabsq_f])
;;
(define_insn "mve_vabsq_f<mode>"
  [
   (set (match_operand:MVE_0 0 "s_register_operand" "=w")
	(unspec:MVE_0 [(match_operand:MVE_0 1 "s_register_operand" "w")]
	 VABSQ_F))
  ]
  "TARGET_HAVE_MVE && TARGET_HAVE_MVE_FLOAT"
  "vabs.f%#<V_sz_elem>  %q0, %q1"
  [(set_attr "type" "mve_move")
])

;;
;; [vrev32q_f])
;;
(define_insn "mve_vrev32q_fv8hf"
  [
   (set (match_operand:V8HF 0 "s_register_operand" "=w")
	(unspec:V8HF [(match_operand:V8HF 1 "s_register_operand" "w")]
	 VREV32Q_F))
  ]
  "TARGET_HAVE_MVE && TARGET_HAVE_MVE_FLOAT"
  "vrev32.16 %q0, %q1"
  [(set_attr "type" "mve_move")
])
;;
;; [vcvttq_f32_f16])
;;
(define_insn "mve_vcvttq_f32_f16v4sf"
  [
   (set (match_operand:V4SF 0 "s_register_operand" "=w")
	(unspec:V4SF [(match_operand:V8HF 1 "s_register_operand" "w")]
	 VCVTTQ_F32_F16))
  ]
  "TARGET_HAVE_MVE && TARGET_HAVE_MVE_FLOAT"
  "vcvtt.f32.f16 %q0, %q1"
  [(set_attr "type" "mve_move")
])

;;
;; [vcvtbq_f32_f16])
;;
(define_insn "mve_vcvtbq_f32_f16v4sf"
  [
   (set (match_operand:V4SF 0 "s_register_operand" "=w")
	(unspec:V4SF [(match_operand:V8HF 1 "s_register_operand" "w")]
	 VCVTBQ_F32_F16))
  ]
  "TARGET_HAVE_MVE && TARGET_HAVE_MVE_FLOAT"
  "vcvtb.f32.f16 %q0, %q1"
  [(set_attr "type" "mve_move")
])

;;
;; [vcvtq_to_f_s, vcvtq_to_f_u])
;;
(define_insn "mve_vcvtq_to_f_<supf><mode>"
  [
   (set (match_operand:MVE_0 0 "s_register_operand" "=w")
	(unspec:MVE_0 [(match_operand:<MVE_CNVT> 1 "s_register_operand" "w")]
	 VCVTQ_TO_F))
  ]
  "TARGET_HAVE_MVE && TARGET_HAVE_MVE_FLOAT"
  "vcvt.f%#<V_sz_elem>.<supf>%#<V_sz_elem>       %q0, %q1"
  [(set_attr "type" "mve_move")
])

;;
;; [vrev64q_u, vrev64q_s])
;;
(define_insn "mve_vrev64q_<supf><mode>"
  [
   (set (match_operand:MVE_2 0 "s_register_operand" "=w")
	(unspec:MVE_2 [(match_operand:MVE_2 1 "s_register_operand" "w")]
	 VREV64Q))
  ]
  "TARGET_HAVE_MVE"
  "vrev64.%#<V_sz_elem> %q0, %q1"
  [(set_attr "type" "mve_move")
])

;;
;; [vcvtq_from_f_s, vcvtq_from_f_u])
;;
(define_insn "mve_vcvtq_from_f_<supf><mode>"
  [
   (set (match_operand:MVE_5 0 "s_register_operand" "=w")
	(unspec:MVE_5 [(match_operand:<MVE_CNVT> 1 "s_register_operand" "w")]
	 VCVTQ_FROM_F))
  ]
  "TARGET_HAVE_MVE && TARGET_HAVE_MVE_FLOAT"
  "vcvt.<supf>%#<V_sz_elem>.f%#<V_sz_elem>       %q0, %q1"
  [(set_attr "type" "mve_move")
])
;; [vqnegq_s])
;;
(define_insn "mve_vqnegq_s<mode>"
  [
   (set (match_operand:MVE_2 0 "s_register_operand" "=w")
	(unspec:MVE_2 [(match_operand:MVE_2 1 "s_register_operand" "w")]
	 VQNEGQ_S))
  ]
  "TARGET_HAVE_MVE"
  "vqneg.s%#<V_sz_elem> %q0, %q1"
  [(set_attr "type" "mve_move")
])

;;
;; [vqabsq_s])
;;
(define_insn "mve_vqabsq_s<mode>"
  [
   (set (match_operand:MVE_2 0 "s_register_operand" "=w")
	(unspec:MVE_2 [(match_operand:MVE_2 1 "s_register_operand" "w")]
	 VQABSQ_S))
  ]
  "TARGET_HAVE_MVE"
  "vqabs.s%#<V_sz_elem> %q0, %q1"
  [(set_attr "type" "mve_move")
])

;;
;; [vnegq_s])
;;
(define_insn "mve_vnegq_s<mode>"
  [
   (set (match_operand:MVE_2 0 "s_register_operand" "=w")
	(unspec:MVE_2 [(match_operand:MVE_2 1 "s_register_operand" "w")]
	 VNEGQ_S))
  ]
  "TARGET_HAVE_MVE"
  "vneg.s%#<V_sz_elem>  %q0, %q1"
  [(set_attr "type" "mve_move")
])

;;
;; [vmvnq_u, vmvnq_s])
;;
(define_insn "mve_vmvnq_<supf><mode>"
  [
   (set (match_operand:MVE_2 0 "s_register_operand" "=w")
	(unspec:MVE_2 [(match_operand:MVE_2 1 "s_register_operand" "w")]
	 VMVNQ))
  ]
  "TARGET_HAVE_MVE"
  "vmvn %q0, %q1"
  [(set_attr "type" "mve_move")
])

;;
;; [vdupq_n_u, vdupq_n_s])
;;
(define_insn "mve_vdupq_n_<supf><mode>"
  [
   (set (match_operand:MVE_2 0 "s_register_operand" "=w")
	(unspec:MVE_2 [(match_operand:<V_elem> 1 "s_register_operand" "r")]
	 VDUPQ_N))
  ]
  "TARGET_HAVE_MVE"
  "vdup.%#<V_sz_elem>   %q0, %1"
  [(set_attr "type" "mve_move")
])

;;
;; [vclzq_u, vclzq_s])
;;
(define_insn "mve_vclzq_<supf><mode>"
  [
   (set (match_operand:MVE_2 0 "s_register_operand" "=w")
	(unspec:MVE_2 [(match_operand:MVE_2 1 "s_register_operand" "w")]
	 VCLZQ))
  ]
  "TARGET_HAVE_MVE"
  "vclz.i%#<V_sz_elem>  %q0, %q1"
  [(set_attr "type" "mve_move")
])

;;
;; [vclsq_s])
;;
(define_insn "mve_vclsq_s<mode>"
  [
   (set (match_operand:MVE_2 0 "s_register_operand" "=w")
	(unspec:MVE_2 [(match_operand:MVE_2 1 "s_register_operand" "w")]
	 VCLSQ_S))
  ]
  "TARGET_HAVE_MVE"
  "vcls.s%#<V_sz_elem>  %q0, %q1"
  [(set_attr "type" "mve_move")
])

;;
;; [vaddvq_s, vaddvq_u])
;;
(define_insn "mve_vaddvq_<supf><mode>"
  [
   (set (match_operand:SI 0 "s_register_operand" "=e")
	(unspec:SI [(match_operand:MVE_2 1 "s_register_operand" "w")]
	 VADDVQ))
  ]
  "TARGET_HAVE_MVE"
  "vaddv.<supf>%#<V_sz_elem>\t%0, %q1"
  [(set_attr "type" "mve_move")
])

;;
;; [vabsq_s])
;;
(define_insn "mve_vabsq_s<mode>"
  [
   (set (match_operand:MVE_2 0 "s_register_operand" "=w")
	(unspec:MVE_2 [(match_operand:MVE_2 1 "s_register_operand" "w")]
	 VABSQ_S))
  ]
  "TARGET_HAVE_MVE"
  "vabs.s%#<V_sz_elem>\t%q0, %q1"
  [(set_attr "type" "mve_move")
])

;;
;; [vrev32q_u, vrev32q_s])
;;
(define_insn "mve_vrev32q_<supf><mode>"
  [
   (set (match_operand:MVE_3 0 "s_register_operand" "=w")
	(unspec:MVE_3 [(match_operand:MVE_3 1 "s_register_operand" "w")]
	 VREV32Q))
  ]
  "TARGET_HAVE_MVE"
  "vrev32.%#<V_sz_elem>\t%q0, %q1"
  [(set_attr "type" "mve_move")
])

;;
;; [vmovltq_u, vmovltq_s])
;;
(define_insn "mve_vmovltq_<supf><mode>"
  [
   (set (match_operand:<V_double_width> 0 "s_register_operand" "=w")
	(unspec:<V_double_width> [(match_operand:MVE_3 1 "s_register_operand" "w")]
	 VMOVLTQ))
  ]
  "TARGET_HAVE_MVE"
  "vmovlt.<supf>%#<V_sz_elem>   %q0, %q1"
  [(set_attr "type" "mve_move")
])

;;
;; [vmovlbq_s, vmovlbq_u])
;;
(define_insn "mve_vmovlbq_<supf><mode>"
  [
   (set (match_operand:<V_double_width> 0 "s_register_operand" "=w")
	(unspec:<V_double_width> [(match_operand:MVE_3 1 "s_register_operand" "w")]
	 VMOVLBQ))
  ]
  "TARGET_HAVE_MVE"
  "vmovlb.<supf>%#<V_sz_elem>   %q0, %q1"
  [(set_attr "type" "mve_move")
])

;;
;; [vcvtpq_s, vcvtpq_u])
;;
(define_insn "mve_vcvtpq_<supf><mode>"
  [
   (set (match_operand:MVE_5 0 "s_register_operand" "=w")
	(unspec:MVE_5 [(match_operand:<MVE_CNVT> 1 "s_register_operand" "w")]
	 VCVTPQ))
  ]
  "TARGET_HAVE_MVE && TARGET_HAVE_MVE_FLOAT"
  "vcvtp.<supf>%#<V_sz_elem>.f%#<V_sz_elem>      %q0, %q1"
  [(set_attr "type" "mve_move")
])

;;
;; [vcvtnq_s, vcvtnq_u])
;;
(define_insn "mve_vcvtnq_<supf><mode>"
  [
   (set (match_operand:MVE_5 0 "s_register_operand" "=w")
	(unspec:MVE_5 [(match_operand:<MVE_CNVT> 1 "s_register_operand" "w")]
	 VCVTNQ))
  ]
  "TARGET_HAVE_MVE && TARGET_HAVE_MVE_FLOAT"
  "vcvtn.<supf>%#<V_sz_elem>.f%#<V_sz_elem>      %q0, %q1"
  [(set_attr "type" "mve_move")
])

;;
;; [vcvtmq_s, vcvtmq_u])
;;
(define_insn "mve_vcvtmq_<supf><mode>"
  [
   (set (match_operand:MVE_5 0 "s_register_operand" "=w")
	(unspec:MVE_5 [(match_operand:<MVE_CNVT> 1 "s_register_operand" "w")]
	 VCVTMQ))
  ]
  "TARGET_HAVE_MVE && TARGET_HAVE_MVE_FLOAT"
  "vcvtm.<supf>%#<V_sz_elem>.f%#<V_sz_elem>      %q0, %q1"
  [(set_attr "type" "mve_move")
])

;;
;; [vcvtaq_u, vcvtaq_s])
;;
(define_insn "mve_vcvtaq_<supf><mode>"
  [
   (set (match_operand:MVE_5 0 "s_register_operand" "=w")
	(unspec:MVE_5 [(match_operand:<MVE_CNVT> 1 "s_register_operand" "w")]
	 VCVTAQ))
  ]
  "TARGET_HAVE_MVE && TARGET_HAVE_MVE_FLOAT"
  "vcvta.<supf>%#<V_sz_elem>.f%#<V_sz_elem>      %q0, %q1"
  [(set_attr "type" "mve_move")
])

;;
;; [vmvnq_n_u, vmvnq_n_s])
;;
(define_insn "mve_vmvnq_n_<supf><mode>"
  [
   (set (match_operand:MVE_5 0 "s_register_operand" "=w")
	(unspec:MVE_5 [(match_operand:HI 1 "immediate_operand" "i")]
	 VMVNQ_N))
  ]
  "TARGET_HAVE_MVE"
  "vmvn.i%#<V_sz_elem>  %q0, %1"
  [(set_attr "type" "mve_move")
])

;;
;; [vrev16q_u, vrev16q_s])
;;
(define_insn "mve_vrev16q_<supf>v16qi"
  [
   (set (match_operand:V16QI 0 "s_register_operand" "=w")
	(unspec:V16QI [(match_operand:V16QI 1 "s_register_operand" "w")]
	 VREV16Q))
  ]
  "TARGET_HAVE_MVE"
  "vrev16.8 %q0, %q1"
  [(set_attr "type" "mve_move")
])

;;
;; [vaddlvq_s vaddlvq_u])
;;
(define_insn "mve_vaddlvq_<supf>v4si"
  [
   (set (match_operand:DI 0 "s_register_operand" "=r")
	(unspec:DI [(match_operand:V4SI 1 "s_register_operand" "w")]
	 VADDLVQ))
  ]
  "TARGET_HAVE_MVE"
  "vaddlv.<supf>32 %Q0, %R0, %q1"
  [(set_attr "type" "mve_move")
])

;;
;; [vctp8q vctp16q vctp32q vctp64q])
;;
(define_insn "mve_vctp<mode1>qhi"
  [
   (set (match_operand:HI 0 "vpr_register_operand" "=Up")
	(unspec:HI [(match_operand:SI 1 "s_register_operand" "r")]
	VCTPQ))
  ]
  "TARGET_HAVE_MVE"
  "vctp.<mode1> %1"
  [(set_attr "type" "mve_move")
])

;;
;; [vpnot])
;;
(define_insn "mve_vpnothi"
  [
   (set (match_operand:HI 0 "vpr_register_operand" "=Up")
	(unspec:HI [(match_operand:HI 1 "vpr_register_operand" "0")]
	 VPNOT))
  ]
  "TARGET_HAVE_MVE"
  "vpnot"
  [(set_attr "type" "mve_move")
])

;;
;; [vsubq_n_f])
;;
(define_insn "mve_vsubq_n_f<mode>"
  [
   (set (match_operand:MVE_0 0 "s_register_operand" "=w")
	(unspec:MVE_0 [(match_operand:MVE_0 1 "s_register_operand" "w")
		       (match_operand:<V_elem> 2 "s_register_operand" "r")]
	 VSUBQ_N_F))
  ]
  "TARGET_HAVE_MVE && TARGET_HAVE_MVE_FLOAT"
  "vsub.f<V_sz_elem>  %q0, %q1, %2"
  [(set_attr "type" "mve_move")
])

;;
;; [vbrsrq_n_f])
;;
(define_insn "mve_vbrsrq_n_f<mode>"
  [
   (set (match_operand:MVE_0 0 "s_register_operand" "=w")
	(unspec:MVE_0 [(match_operand:MVE_0 1 "s_register_operand" "w")
		       (match_operand:SI 2 "s_register_operand" "r")]
	 VBRSRQ_N_F))
  ]
  "TARGET_HAVE_MVE && TARGET_HAVE_MVE_FLOAT"
  "vbrsr.<V_sz_elem>  %q0, %q1, %2"
  [(set_attr "type" "mve_move")
])

;;
;; [vcvtq_n_to_f_s, vcvtq_n_to_f_u])
;;
(define_insn "mve_vcvtq_n_to_f_<supf><mode>"
  [
   (set (match_operand:MVE_0 0 "s_register_operand" "=w")
	(unspec:MVE_0 [(match_operand:<MVE_CNVT> 1 "s_register_operand" "w")
		       (match_operand:SI 2 "mve_imm_16" "Rd")]
	 VCVTQ_N_TO_F))
  ]
  "TARGET_HAVE_MVE && TARGET_HAVE_MVE_FLOAT"
  "vcvt.f<V_sz_elem>.<supf><V_sz_elem>\t%q0, %q1, %2"
  [(set_attr "type" "mve_move")
])

;; [vcreateq_f])
;;
(define_insn "mve_vcreateq_f<mode>"
  [
   (set (match_operand:MVE_0 0 "s_register_operand" "=w")
	(unspec:MVE_0 [(match_operand:DI 1 "s_register_operand" "r")
		       (match_operand:DI 2 "s_register_operand" "r")]
	 VCREATEQ_F))
  ]
  "TARGET_HAVE_MVE && TARGET_HAVE_MVE_FLOAT"
  "vmov %q0[2], %q0[0], %Q2, %Q1\;vmov %q0[3], %q0[1], %R2, %R1"
  [(set_attr "type" "mve_move")
   (set_attr "length""8")])

;;
;; [vcreateq_u, vcreateq_s])
;;
(define_insn "mve_vcreateq_<supf><mode>"
  [
   (set (match_operand:MVE_1 0 "s_register_operand" "=w")
	(unspec:MVE_1 [(match_operand:DI 1 "s_register_operand" "r")
		       (match_operand:DI 2 "s_register_operand" "r")]
	 VCREATEQ))
  ]
  "TARGET_HAVE_MVE"
  "vmov %q0[2], %q0[0], %Q2, %Q1\;vmov %q0[3], %q0[1], %R2, %R1"
  [(set_attr "type" "mve_move")
   (set_attr "length""8")])

;;
;; [vshrq_n_s, vshrq_n_u])
;;
(define_insn "mve_vshrq_n_<supf><mode>"
  [
   (set (match_operand:MVE_2 0 "s_register_operand" "=w")
	(unspec:MVE_2 [(match_operand:MVE_2 1 "s_register_operand" "w")
		       (match_operand:SI 2 "<MVE_pred2>" "<MVE_constraint2>")]
	 VSHRQ_N))
  ]
  "TARGET_HAVE_MVE"
  "vshr.<supf><V_sz_elem>\t%q0, %q1, %2"
  [(set_attr "type" "mve_move")
])

;;
;; [vcvtq_n_from_f_s, vcvtq_n_from_f_u])
;;
(define_insn "mve_vcvtq_n_from_f_<supf><mode>"
  [
   (set (match_operand:MVE_5 0 "s_register_operand" "=w")
	(unspec:MVE_5 [(match_operand:<MVE_CNVT> 1 "s_register_operand" "w")
		       (match_operand:SI 2 "mve_imm_16" "Rd")]
	 VCVTQ_N_FROM_F))
  ]
  "TARGET_HAVE_MVE && TARGET_HAVE_MVE_FLOAT"
  "vcvt.<supf><V_sz_elem>.f<V_sz_elem>\t%q0, %q1, %2"
  [(set_attr "type" "mve_move")
])

;;
;; [vaddlvq_p_s])
;;
(define_insn "mve_vaddlvq_p_<supf>v4si"
  [
   (set (match_operand:DI 0 "s_register_operand" "=r")
	(unspec:DI [(match_operand:V4SI 1 "s_register_operand" "w")
		    (match_operand:HI 2 "vpr_register_operand" "Up")]
	 VADDLVQ_P))
  ]
  "TARGET_HAVE_MVE"
  "vpst\;vaddlvt.<supf>32 %Q0, %R0, %q1"
  [(set_attr "type" "mve_move")
   (set_attr "length""8")])

;;
;; [vcmpneq_u, vcmpneq_s])
;;
(define_insn "mve_vcmpneq_<supf><mode>"
  [
   (set (match_operand:HI 0 "vpr_register_operand" "=Up")
	(unspec:HI [(match_operand:MVE_2 1 "s_register_operand" "w")
		    (match_operand:MVE_2 2 "s_register_operand" "w")]
	 VCMPNEQ))
  ]
  "TARGET_HAVE_MVE"
  "vcmp.i%#<V_sz_elem>  ne, %q1, %q2"
  [(set_attr "type" "mve_move")
])

;;
;; [vshlq_s, vshlq_u])
;;
(define_insn "mve_vshlq_<supf><mode>"
  [
   (set (match_operand:MVE_2 0 "s_register_operand" "=w")
	(unspec:MVE_2 [(match_operand:MVE_2 1 "s_register_operand" "w")
		       (match_operand:MVE_2 2 "s_register_operand" "w")]
	 VSHLQ))
  ]
  "TARGET_HAVE_MVE"
  "vshl.<supf>%#<V_sz_elem>\t%q0, %q1, %q2"
  [(set_attr "type" "mve_move")
])

;;
;; [vabdq_s, vabdq_u])
;;
(define_insn "mve_vabdq_<supf><mode>"
  [
   (set (match_operand:MVE_2 0 "s_register_operand" "=w")
	(unspec:MVE_2 [(match_operand:MVE_2 1 "s_register_operand" "w")
		       (match_operand:MVE_2 2 "s_register_operand" "w")]
	 VABDQ))
  ]
  "TARGET_HAVE_MVE"
  "vabd.<supf>%#<V_sz_elem>	%q0, %q1, %q2"
  [(set_attr "type" "mve_move")
])

;;
;; [vaddq_n_s, vaddq_n_u])
;;
(define_insn "mve_vaddq_n_<supf><mode>"
  [
   (set (match_operand:MVE_2 0 "s_register_operand" "=w")
	(unspec:MVE_2 [(match_operand:MVE_2 1 "s_register_operand" "w")
		       (match_operand:<V_elem> 2 "s_register_operand" "r")]
	 VADDQ_N))
  ]
  "TARGET_HAVE_MVE"
  "vadd.i%#<V_sz_elem>	%q0, %q1, %2"
  [(set_attr "type" "mve_move")
])

;;
;; [vaddvaq_s, vaddvaq_u])
;;
(define_insn "mve_vaddvaq_<supf><mode>"
  [
   (set (match_operand:SI 0 "s_register_operand" "=e")
	(unspec:SI [(match_operand:SI 1 "s_register_operand" "0")
		    (match_operand:MVE_2 2 "s_register_operand" "w")]
	 VADDVAQ))
  ]
  "TARGET_HAVE_MVE"
  "vaddva.<supf>%#<V_sz_elem>	%0, %q2"
  [(set_attr "type" "mve_move")
])

;;
;; [vaddvq_p_u, vaddvq_p_s])
;;
(define_insn "mve_vaddvq_p_<supf><mode>"
  [
   (set (match_operand:SI 0 "s_register_operand" "=e")
	(unspec:SI [(match_operand:MVE_2 1 "s_register_operand" "w")
		    (match_operand:HI 2 "vpr_register_operand" "Up")]
	 VADDVQ_P))
  ]
  "TARGET_HAVE_MVE"
  "vpst\;vaddvt.<supf>%#<V_sz_elem>	%0, %q1"
  [(set_attr "type" "mve_move")
   (set_attr "length""8")])

;;
;; [vandq_u, vandq_s])
;;
(define_insn "mve_vandq_<supf><mode>"
  [
   (set (match_operand:MVE_2 0 "s_register_operand" "=w")
	(unspec:MVE_2 [(match_operand:MVE_2 1 "s_register_operand" "w")
		       (match_operand:MVE_2 2 "s_register_operand" "w")]
	 VANDQ))
  ]
  "TARGET_HAVE_MVE"
  "vand %q0, %q1, %q2"
  [(set_attr "type" "mve_move")
])

;;
;; [vbicq_s, vbicq_u])
;;
(define_insn "mve_vbicq_<supf><mode>"
  [
   (set (match_operand:MVE_2 0 "s_register_operand" "=w")
	(unspec:MVE_2 [(match_operand:MVE_2 1 "s_register_operand" "w")
		       (match_operand:MVE_2 2 "s_register_operand" "w")]
	 VBICQ))
  ]
  "TARGET_HAVE_MVE"
  "vbic %q0, %q1, %q2"
  [(set_attr "type" "mve_move")
])

;;
;; [vbrsrq_n_u, vbrsrq_n_s])
;;
(define_insn "mve_vbrsrq_n_<supf><mode>"
  [
   (set (match_operand:MVE_2 0 "s_register_operand" "=w")
	(unspec:MVE_2 [(match_operand:MVE_2 1 "s_register_operand" "w")
		       (match_operand:SI 2 "s_register_operand" "r")]
	 VBRSRQ_N))
  ]
  "TARGET_HAVE_MVE"
  "vbrsr.%#<V_sz_elem>	%q0, %q1, %2"
  [(set_attr "type" "mve_move")
])

;;
;; [vcaddq_rot270_s, vcaddq_rot270_u])
;;
(define_insn "mve_vcaddq_rot270_<supf><mode>"
  [
   (set (match_operand:MVE_2 0 "s_register_operand" "=w")
	(unspec:MVE_2 [(match_operand:MVE_2 1 "s_register_operand" "w")
		       (match_operand:MVE_2 2 "s_register_operand" "w")]
	 VCADDQ_ROT270))
  ]
  "TARGET_HAVE_MVE"
  "vcadd.i%#<V_sz_elem>	%q0, %q1, %q2, #270"
  [(set_attr "type" "mve_move")
])

;;
;; [vcaddq_rot90_u, vcaddq_rot90_s])
;;
(define_insn "mve_vcaddq_rot90_<supf><mode>"
  [
   (set (match_operand:MVE_2 0 "s_register_operand" "=w")
	(unspec:MVE_2 [(match_operand:MVE_2 1 "s_register_operand" "w")
		       (match_operand:MVE_2 2 "s_register_operand" "w")]
	 VCADDQ_ROT90))
  ]
  "TARGET_HAVE_MVE"
  "vcadd.i%#<V_sz_elem>	%q0, %q1, %q2, #90"
  [(set_attr "type" "mve_move")
])

;;
;; [vcmpcsq_n_u])
;;
(define_insn "mve_vcmpcsq_n_u<mode>"
  [
   (set (match_operand:HI 0 "vpr_register_operand" "=Up")
	(unspec:HI [(match_operand:MVE_2 1 "s_register_operand" "w")
		    (match_operand:<V_elem> 2 "s_register_operand" "r")]
	 VCMPCSQ_N_U))
  ]
  "TARGET_HAVE_MVE"
  "vcmp.u%#<V_sz_elem>	cs, %q1, %2"
  [(set_attr "type" "mve_move")
])

;;
;; [vcmpcsq_u])
;;
(define_insn "mve_vcmpcsq_u<mode>"
  [
   (set (match_operand:HI 0 "vpr_register_operand" "=Up")
	(unspec:HI [(match_operand:MVE_2 1 "s_register_operand" "w")
		    (match_operand:MVE_2 2 "s_register_operand" "w")]
	 VCMPCSQ_U))
  ]
  "TARGET_HAVE_MVE"
  "vcmp.u%#<V_sz_elem>	cs, %q1, %q2"
  [(set_attr "type" "mve_move")
])

;;
;; [vcmpeqq_n_s, vcmpeqq_n_u])
;;
(define_insn "mve_vcmpeqq_n_<supf><mode>"
  [
   (set (match_operand:HI 0 "vpr_register_operand" "=Up")
	(unspec:HI [(match_operand:MVE_2 1 "s_register_operand" "w")
		    (match_operand:<V_elem> 2 "s_register_operand" "r")]
	 VCMPEQQ_N))
  ]
  "TARGET_HAVE_MVE"
  "vcmp.i%#<V_sz_elem>	eq, %q1, %2"
  [(set_attr "type" "mve_move")
])

;;
;; [vcmpeqq_u, vcmpeqq_s])
;;
(define_insn "mve_vcmpeqq_<supf><mode>"
  [
   (set (match_operand:HI 0 "vpr_register_operand" "=Up")
	(unspec:HI [(match_operand:MVE_2 1 "s_register_operand" "w")
		    (match_operand:MVE_2 2 "s_register_operand" "w")]
	 VCMPEQQ))
  ]
  "TARGET_HAVE_MVE"
  "vcmp.i%#<V_sz_elem>	eq, %q1, %q2"
  [(set_attr "type" "mve_move")
])

;;
;; [vcmpgeq_n_s])
;;
(define_insn "mve_vcmpgeq_n_s<mode>"
  [
   (set (match_operand:HI 0 "vpr_register_operand" "=Up")
	(unspec:HI [(match_operand:MVE_2 1 "s_register_operand" "w")
		    (match_operand:<V_elem> 2 "s_register_operand" "r")]
	 VCMPGEQ_N_S))
  ]
  "TARGET_HAVE_MVE"
  "vcmp.s%#<V_sz_elem>	ge, %q1, %2"
  [(set_attr "type" "mve_move")
])

;;
;; [vcmpgeq_s])
;;
(define_insn "mve_vcmpgeq_s<mode>"
  [
   (set (match_operand:HI 0 "vpr_register_operand" "=Up")
	(unspec:HI [(match_operand:MVE_2 1 "s_register_operand" "w")
		    (match_operand:MVE_2 2 "s_register_operand" "w")]
	 VCMPGEQ_S))
  ]
  "TARGET_HAVE_MVE"
  "vcmp.s%#<V_sz_elem>	ge, %q1, %q2"
  [(set_attr "type" "mve_move")
])

;;
;; [vcmpgtq_n_s])
;;
(define_insn "mve_vcmpgtq_n_s<mode>"
  [
   (set (match_operand:HI 0 "vpr_register_operand" "=Up")
	(unspec:HI [(match_operand:MVE_2 1 "s_register_operand" "w")
		    (match_operand:<V_elem> 2 "s_register_operand" "r")]
	 VCMPGTQ_N_S))
  ]
  "TARGET_HAVE_MVE"
  "vcmp.s%#<V_sz_elem>	gt, %q1, %2"
  [(set_attr "type" "mve_move")
])

;;
;; [vcmpgtq_s])
;;
(define_insn "mve_vcmpgtq_s<mode>"
  [
   (set (match_operand:HI 0 "vpr_register_operand" "=Up")
	(unspec:HI [(match_operand:MVE_2 1 "s_register_operand" "w")
		    (match_operand:MVE_2 2 "s_register_operand" "w")]
	 VCMPGTQ_S))
  ]
  "TARGET_HAVE_MVE"
  "vcmp.s%#<V_sz_elem>	gt, %q1, %q2"
  [(set_attr "type" "mve_move")
])

;;
;; [vcmphiq_n_u])
;;
(define_insn "mve_vcmphiq_n_u<mode>"
  [
   (set (match_operand:HI 0 "vpr_register_operand" "=Up")
	(unspec:HI [(match_operand:MVE_2 1 "s_register_operand" "w")
		    (match_operand:<V_elem> 2 "s_register_operand" "r")]
	 VCMPHIQ_N_U))
  ]
  "TARGET_HAVE_MVE"
  "vcmp.u%#<V_sz_elem>	hi, %q1, %2"
  [(set_attr "type" "mve_move")
])

;;
;; [vcmphiq_u])
;;
(define_insn "mve_vcmphiq_u<mode>"
  [
   (set (match_operand:HI 0 "vpr_register_operand" "=Up")
	(unspec:HI [(match_operand:MVE_2 1 "s_register_operand" "w")
		    (match_operand:MVE_2 2 "s_register_operand" "w")]
	 VCMPHIQ_U))
  ]
  "TARGET_HAVE_MVE"
  "vcmp.u%#<V_sz_elem>	hi, %q1, %q2"
  [(set_attr "type" "mve_move")
])

;;
;; [vcmpleq_n_s])
;;
(define_insn "mve_vcmpleq_n_s<mode>"
  [
   (set (match_operand:HI 0 "vpr_register_operand" "=Up")
	(unspec:HI [(match_operand:MVE_2 1 "s_register_operand" "w")
		    (match_operand:<V_elem> 2 "s_register_operand" "r")]
	 VCMPLEQ_N_S))
  ]
  "TARGET_HAVE_MVE"
  "vcmp.s%#<V_sz_elem>	le, %q1, %2"
  [(set_attr "type" "mve_move")
])

;;
;; [vcmpleq_s])
;;
(define_insn "mve_vcmpleq_s<mode>"
  [
   (set (match_operand:HI 0 "vpr_register_operand" "=Up")
	(unspec:HI [(match_operand:MVE_2 1 "s_register_operand" "w")
		    (match_operand:MVE_2 2 "s_register_operand" "w")]
	 VCMPLEQ_S))
  ]
  "TARGET_HAVE_MVE"
  "vcmp.s%#<V_sz_elem>	le, %q1, %q2"
  [(set_attr "type" "mve_move")
])

;;
;; [vcmpltq_n_s])
;;
(define_insn "mve_vcmpltq_n_s<mode>"
  [
   (set (match_operand:HI 0 "vpr_register_operand" "=Up")
	(unspec:HI [(match_operand:MVE_2 1 "s_register_operand" "w")
		    (match_operand:<V_elem> 2 "s_register_operand" "r")]
	 VCMPLTQ_N_S))
  ]
  "TARGET_HAVE_MVE"
  "vcmp.s%#<V_sz_elem>	lt, %q1, %2"
  [(set_attr "type" "mve_move")
])

;;
;; [vcmpltq_s])
;;
(define_insn "mve_vcmpltq_s<mode>"
  [
   (set (match_operand:HI 0 "vpr_register_operand" "=Up")
	(unspec:HI [(match_operand:MVE_2 1 "s_register_operand" "w")
		    (match_operand:MVE_2 2 "s_register_operand" "w")]
	 VCMPLTQ_S))
  ]
  "TARGET_HAVE_MVE"
  "vcmp.s%#<V_sz_elem>	lt, %q1, %q2"
  [(set_attr "type" "mve_move")
])

;;
;; [vcmpneq_n_u, vcmpneq_n_s])
;;
(define_insn "mve_vcmpneq_n_<supf><mode>"
  [
   (set (match_operand:HI 0 "vpr_register_operand" "=Up")
	(unspec:HI [(match_operand:MVE_2 1 "s_register_operand" "w")
		    (match_operand:<V_elem> 2 "s_register_operand" "r")]
	 VCMPNEQ_N))
  ]
  "TARGET_HAVE_MVE"
  "vcmp.i%#<V_sz_elem>	ne, %q1, %2"
  [(set_attr "type" "mve_move")
])

;;
;; [veorq_u, veorq_s])
;;
(define_insn "mve_veorq_<supf><mode>"
  [
   (set (match_operand:MVE_2 0 "s_register_operand" "=w")
	(unspec:MVE_2 [(match_operand:MVE_2 1 "s_register_operand" "w")
		       (match_operand:MVE_2 2 "s_register_operand" "w")]
	 VEORQ))
  ]
  "TARGET_HAVE_MVE"
  "veor %q0, %q1, %q2"
  [(set_attr "type" "mve_move")
])

;;
;; [vhaddq_n_u, vhaddq_n_s])
;;
(define_insn "mve_vhaddq_n_<supf><mode>"
  [
   (set (match_operand:MVE_2 0 "s_register_operand" "=w")
	(unspec:MVE_2 [(match_operand:MVE_2 1 "s_register_operand" "w")
		       (match_operand:<V_elem> 2 "s_register_operand" "r")]
	 VHADDQ_N))
  ]
  "TARGET_HAVE_MVE"
  "vhadd.<supf>%#<V_sz_elem>\t%q0, %q1, %2"
  [(set_attr "type" "mve_move")
])

;;
;; [vhaddq_s, vhaddq_u])
;;
(define_insn "mve_vhaddq_<supf><mode>"
  [
   (set (match_operand:MVE_2 0 "s_register_operand" "=w")
	(unspec:MVE_2 [(match_operand:MVE_2 1 "s_register_operand" "w")
		       (match_operand:MVE_2 2 "s_register_operand" "w")]
	 VHADDQ))
  ]
  "TARGET_HAVE_MVE"
  "vhadd.<supf>%#<V_sz_elem>\t%q0, %q1, %q2"
  [(set_attr "type" "mve_move")
])

;;
;; [vhcaddq_rot270_s])
;;
(define_insn "mve_vhcaddq_rot270_s<mode>"
  [
   (set (match_operand:MVE_2 0 "s_register_operand" "=w")
	(unspec:MVE_2 [(match_operand:MVE_2 1 "s_register_operand" "w")
		       (match_operand:MVE_2 2 "s_register_operand" "w")]
	 VHCADDQ_ROT270_S))
  ]
  "TARGET_HAVE_MVE"
  "vhcadd.s%#<V_sz_elem>\t%q0, %q1, %q2, #270"
  [(set_attr "type" "mve_move")
])

;;
;; [vhcaddq_rot90_s])
;;
(define_insn "mve_vhcaddq_rot90_s<mode>"
  [
   (set (match_operand:MVE_2 0 "s_register_operand" "=w")
	(unspec:MVE_2 [(match_operand:MVE_2 1 "s_register_operand" "w")
		       (match_operand:MVE_2 2 "s_register_operand" "w")]
	 VHCADDQ_ROT90_S))
  ]
  "TARGET_HAVE_MVE"
  "vhcadd.s%#<V_sz_elem>\t%q0, %q1, %q2, #90"
  [(set_attr "type" "mve_move")
])

;;
;; [vhsubq_n_u, vhsubq_n_s])
;;
(define_insn "mve_vhsubq_n_<supf><mode>"
  [
   (set (match_operand:MVE_2 0 "s_register_operand" "=w")
	(unspec:MVE_2 [(match_operand:MVE_2 1 "s_register_operand" "w")
		       (match_operand:<V_elem> 2 "s_register_operand" "r")]
	 VHSUBQ_N))
  ]
  "TARGET_HAVE_MVE"
  "vhsub.<supf>%#<V_sz_elem>\t%q0, %q1, %2"
  [(set_attr "type" "mve_move")
])

;;
;; [vhsubq_s, vhsubq_u])
;;
(define_insn "mve_vhsubq_<supf><mode>"
  [
   (set (match_operand:MVE_2 0 "s_register_operand" "=w")
	(unspec:MVE_2 [(match_operand:MVE_2 1 "s_register_operand" "w")
		       (match_operand:MVE_2 2 "s_register_operand" "w")]
	 VHSUBQ))
  ]
  "TARGET_HAVE_MVE"
  "vhsub.<supf>%#<V_sz_elem>\t%q0, %q1, %q2"
  [(set_attr "type" "mve_move")
])

;;
;; [vmaxaq_s])
;;
(define_insn "mve_vmaxaq_s<mode>"
  [
   (set (match_operand:MVE_2 0 "s_register_operand" "=w")
	(unspec:MVE_2 [(match_operand:MVE_2 1 "s_register_operand" "0")
		       (match_operand:MVE_2 2 "s_register_operand" "w")]
	 VMAXAQ_S))
  ]
  "TARGET_HAVE_MVE"
  "vmaxa.s%#<V_sz_elem>	%q0, %q2"
  [(set_attr "type" "mve_move")
])

;;
;; [vmaxavq_s])
;;
(define_insn "mve_vmaxavq_s<mode>"
  [
   (set (match_operand:<V_elem> 0 "s_register_operand" "=r")
	(unspec:<V_elem> [(match_operand:<V_elem> 1 "s_register_operand" "0")
			  (match_operand:MVE_2 2 "s_register_operand" "w")]
	 VMAXAVQ_S))
  ]
  "TARGET_HAVE_MVE"
  "vmaxav.s%#<V_sz_elem>\t%0, %q2"
  [(set_attr "type" "mve_move")
])

;;
;; [vmaxq_u, vmaxq_s])
;;
(define_insn "mve_vmaxq_<supf><mode>"
  [
   (set (match_operand:MVE_2 0 "s_register_operand" "=w")
	(unspec:MVE_2 [(match_operand:MVE_2 1 "s_register_operand" "w")
		       (match_operand:MVE_2 2 "s_register_operand" "w")]
	 VMAXQ))
  ]
  "TARGET_HAVE_MVE"
  "vmax.<supf>%#<V_sz_elem>\t%q0, %q1, %q2"
  [(set_attr "type" "mve_move")
])

;;
;; [vmaxvq_u, vmaxvq_s])
;;
(define_insn "mve_vmaxvq_<supf><mode>"
  [
   (set (match_operand:<V_elem> 0 "s_register_operand" "=r")
	(unspec:<V_elem> [(match_operand:<V_elem> 1 "s_register_operand" "0")
			  (match_operand:MVE_2 2 "s_register_operand" "w")]
	 VMAXVQ))
  ]
  "TARGET_HAVE_MVE"
  "vmaxv.<supf>%#<V_sz_elem>\t%0, %q2"
  [(set_attr "type" "mve_move")
])

;;
;; [vminaq_s])
;;
(define_insn "mve_vminaq_s<mode>"
  [
   (set (match_operand:MVE_2 0 "s_register_operand" "=w")
	(unspec:MVE_2 [(match_operand:MVE_2 1 "s_register_operand" "0")
		       (match_operand:MVE_2 2 "s_register_operand" "w")]
	 VMINAQ_S))
  ]
  "TARGET_HAVE_MVE"
  "vmina.s%#<V_sz_elem>\t%q0, %q2"
  [(set_attr "type" "mve_move")
])

;;
;; [vminavq_s])
;;
(define_insn "mve_vminavq_s<mode>"
  [
   (set (match_operand:<V_elem> 0 "s_register_operand" "=r")
	(unspec:<V_elem> [(match_operand:<V_elem> 1 "s_register_operand" "0")
			  (match_operand:MVE_2 2 "s_register_operand" "w")]
	 VMINAVQ_S))
  ]
  "TARGET_HAVE_MVE"
  "vminav.s%#<V_sz_elem>\t%0, %q2"
  [(set_attr "type" "mve_move")
])

;;
;; [vminq_s, vminq_u])
;;
(define_insn "mve_vminq_<supf><mode>"
  [
   (set (match_operand:MVE_2 0 "s_register_operand" "=w")
	(unspec:MVE_2 [(match_operand:MVE_2 1 "s_register_operand" "w")
		       (match_operand:MVE_2 2 "s_register_operand" "w")]
	 VMINQ))
  ]
  "TARGET_HAVE_MVE"
  "vmin.<supf>%#<V_sz_elem>\t%q0, %q1, %q2"
  [(set_attr "type" "mve_move")
])

;;
;; [vminvq_u, vminvq_s])
;;
(define_insn "mve_vminvq_<supf><mode>"
  [
   (set (match_operand:<V_elem> 0 "s_register_operand" "=r")
	(unspec:<V_elem> [(match_operand:<V_elem> 1 "s_register_operand" "0")
			  (match_operand:MVE_2 2 "s_register_operand" "w")]
	 VMINVQ))
  ]
  "TARGET_HAVE_MVE"
  "vminv.<supf>%#<V_sz_elem>\t%0, %q2"
  [(set_attr "type" "mve_move")
])

;;
;; [vmladavq_u, vmladavq_s])
;;
(define_insn "mve_vmladavq_<supf><mode>"
  [
   (set (match_operand:SI 0 "s_register_operand" "=e")
	(unspec:SI [(match_operand:MVE_2 1 "s_register_operand" "w")
		    (match_operand:MVE_2 2 "s_register_operand" "w")]
	 VMLADAVQ))
  ]
  "TARGET_HAVE_MVE"
  "vmladav.<supf>%#<V_sz_elem>\t%0, %q1, %q2"
  [(set_attr "type" "mve_move")
])

;;
;; [vmladavxq_s])
;;
(define_insn "mve_vmladavxq_s<mode>"
  [
   (set (match_operand:SI 0 "s_register_operand" "=e")
	(unspec:SI [(match_operand:MVE_2 1 "s_register_operand" "w")
		    (match_operand:MVE_2 2 "s_register_operand" "w")]
	 VMLADAVXQ_S))
  ]
  "TARGET_HAVE_MVE"
  "vmladavx.s%#<V_sz_elem>\t%0, %q1, %q2"
  [(set_attr "type" "mve_move")
])

;;
;; [vmlsdavq_s])
;;
(define_insn "mve_vmlsdavq_s<mode>"
  [
   (set (match_operand:SI 0 "s_register_operand" "=e")
	(unspec:SI [(match_operand:MVE_2 1 "s_register_operand" "w")
		    (match_operand:MVE_2 2 "s_register_operand" "w")]
	 VMLSDAVQ_S))
  ]
  "TARGET_HAVE_MVE"
  "vmlsdav.s%#<V_sz_elem>\t%0, %q1, %q2"
  [(set_attr "type" "mve_move")
])

;;
;; [vmlsdavxq_s])
;;
(define_insn "mve_vmlsdavxq_s<mode>"
  [
   (set (match_operand:SI 0 "s_register_operand" "=e")
	(unspec:SI [(match_operand:MVE_2 1 "s_register_operand" "w")
		    (match_operand:MVE_2 2 "s_register_operand" "w")]
	 VMLSDAVXQ_S))
  ]
  "TARGET_HAVE_MVE"
  "vmlsdavx.s%#<V_sz_elem>\t%0, %q1, %q2"
  [(set_attr "type" "mve_move")
])

;;
;; [vmulhq_s, vmulhq_u])
;;
(define_insn "mve_vmulhq_<supf><mode>"
  [
   (set (match_operand:MVE_2 0 "s_register_operand" "=w")
	(unspec:MVE_2 [(match_operand:MVE_2 1 "s_register_operand" "w")
		       (match_operand:MVE_2 2 "s_register_operand" "w")]
	 VMULHQ))
  ]
  "TARGET_HAVE_MVE"
  "vmulh.<supf>%#<V_sz_elem>\t%q0, %q1, %q2"
  [(set_attr "type" "mve_move")
])

;;
;; [vmullbq_int_u, vmullbq_int_s])
;;
(define_insn "mve_vmullbq_int_<supf><mode>"
  [
   (set (match_operand:<V_double_width> 0 "s_register_operand" "=w")
	(unspec:<V_double_width> [(match_operand:MVE_2 1 "s_register_operand" "w")
				  (match_operand:MVE_2 2 "s_register_operand" "w")]
	 VMULLBQ_INT))
  ]
  "TARGET_HAVE_MVE"
  "vmullb.<supf>%#<V_sz_elem>\t%q0, %q1, %q2"
  [(set_attr "type" "mve_move")
])

;;
;; [vmulltq_int_u, vmulltq_int_s])
;;
(define_insn "mve_vmulltq_int_<supf><mode>"
  [
   (set (match_operand:<V_double_width> 0 "s_register_operand" "=w")
	(unspec:<V_double_width> [(match_operand:MVE_2 1 "s_register_operand" "w")
				  (match_operand:MVE_2 2 "s_register_operand" "w")]
	 VMULLTQ_INT))
  ]
  "TARGET_HAVE_MVE"
  "vmullt.<supf>%#<V_sz_elem>\t%q0, %q1, %q2"
  [(set_attr "type" "mve_move")
])

;;
;; [vmulq_n_u, vmulq_n_s])
;;
(define_insn "mve_vmulq_n_<supf><mode>"
  [
   (set (match_operand:MVE_2 0 "s_register_operand" "=w")
	(unspec:MVE_2 [(match_operand:MVE_2 1 "s_register_operand" "w")
		       (match_operand:<V_elem> 2 "s_register_operand" "r")]
	 VMULQ_N))
  ]
  "TARGET_HAVE_MVE"
  "vmul.i%#<V_sz_elem>\t%q0, %q1, %2"
  [(set_attr "type" "mve_move")
])

;;
;; [vmulq_u, vmulq_s])
;;
(define_insn "mve_vmulq_<supf><mode>"
  [
   (set (match_operand:MVE_2 0 "s_register_operand" "=w")
	(unspec:MVE_2 [(match_operand:MVE_2 1 "s_register_operand" "w")
		       (match_operand:MVE_2 2 "s_register_operand" "w")]
	 VMULQ))
  ]
  "TARGET_HAVE_MVE"
  "vmul.i%#<V_sz_elem>\t%q0, %q1, %q2"
  [(set_attr "type" "mve_move")
])

;;
;; [vornq_u, vornq_s])
;;
(define_insn "mve_vornq_<supf><mode>"
  [
   (set (match_operand:MVE_2 0 "s_register_operand" "=w")
	(unspec:MVE_2 [(match_operand:MVE_2 1 "s_register_operand" "w")
		       (match_operand:MVE_2 2 "s_register_operand" "w")]
	 VORNQ))
  ]
  "TARGET_HAVE_MVE"
  "vorn %q0, %q1, %q2"
  [(set_attr "type" "mve_move")
])

;;
;; [vorrq_s, vorrq_u])
;;
(define_insn "mve_vorrq_<supf><mode>"
  [
   (set (match_operand:MVE_2 0 "s_register_operand" "=w")
	(unspec:MVE_2 [(match_operand:MVE_2 1 "s_register_operand" "w")
		       (match_operand:MVE_2 2 "s_register_operand" "w")]
	 VORRQ))
  ]
  "TARGET_HAVE_MVE"
  "vorr %q0, %q1, %q2"
  [(set_attr "type" "mve_move")
])

;;
;; [vqaddq_n_s, vqaddq_n_u])
;;
(define_insn "mve_vqaddq_n_<supf><mode>"
  [
   (set (match_operand:MVE_2 0 "s_register_operand" "=w")
	(unspec:MVE_2 [(match_operand:MVE_2 1 "s_register_operand" "w")
		       (match_operand:<V_elem> 2 "s_register_operand" "r")]
	 VQADDQ_N))
  ]
  "TARGET_HAVE_MVE"
  "vqadd.<supf>%#<V_sz_elem>\t%q0, %q1, %2"
  [(set_attr "type" "mve_move")
])

;;
;; [vqaddq_u, vqaddq_s])
;;
(define_insn "mve_vqaddq_<supf><mode>"
  [
   (set (match_operand:MVE_2 0 "s_register_operand" "=w")
	(unspec:MVE_2 [(match_operand:MVE_2 1 "s_register_operand" "w")
		       (match_operand:MVE_2 2 "s_register_operand" "w")]
	 VQADDQ))
  ]
  "TARGET_HAVE_MVE"
  "vqadd.<supf>%#<V_sz_elem>\t%q0, %q1, %q2"
  [(set_attr "type" "mve_move")
])

;;
;; [vqdmulhq_n_s])
;;
(define_insn "mve_vqdmulhq_n_s<mode>"
  [
   (set (match_operand:MVE_2 0 "s_register_operand" "=w")
	(unspec:MVE_2 [(match_operand:MVE_2 1 "s_register_operand" "w")
		       (match_operand:<V_elem> 2 "s_register_operand" "r")]
	 VQDMULHQ_N_S))
  ]
  "TARGET_HAVE_MVE"
  "vqdmulh.s%#<V_sz_elem>\t%q0, %q1, %2"
  [(set_attr "type" "mve_move")
])

;;
;; [vqdmulhq_s])
;;
(define_insn "mve_vqdmulhq_s<mode>"
  [
   (set (match_operand:MVE_2 0 "s_register_operand" "=w")
	(unspec:MVE_2 [(match_operand:MVE_2 1 "s_register_operand" "w")
		       (match_operand:MVE_2 2 "s_register_operand" "w")]
	 VQDMULHQ_S))
  ]
  "TARGET_HAVE_MVE"
  "vqdmulh.s%#<V_sz_elem>\t%q0, %q1, %q2"
  [(set_attr "type" "mve_move")
])

;;
;; [vqrdmulhq_n_s])
;;
(define_insn "mve_vqrdmulhq_n_s<mode>"
  [
   (set (match_operand:MVE_2 0 "s_register_operand" "=w")
	(unspec:MVE_2 [(match_operand:MVE_2 1 "s_register_operand" "w")
		       (match_operand:<V_elem> 2 "s_register_operand" "r")]
	 VQRDMULHQ_N_S))
  ]
  "TARGET_HAVE_MVE"
  "vqrdmulh.s%#<V_sz_elem>\t%q0, %q1, %2"
  [(set_attr "type" "mve_move")
])

;;
;; [vqrdmulhq_s])
;;
(define_insn "mve_vqrdmulhq_s<mode>"
  [
   (set (match_operand:MVE_2 0 "s_register_operand" "=w")
	(unspec:MVE_2 [(match_operand:MVE_2 1 "s_register_operand" "w")
		       (match_operand:MVE_2 2 "s_register_operand" "w")]
	 VQRDMULHQ_S))
  ]
  "TARGET_HAVE_MVE"
  "vqrdmulh.s%#<V_sz_elem>\t%q0, %q1, %q2"
  [(set_attr "type" "mve_move")
])

;;
;; [vqrshlq_n_s, vqrshlq_n_u])
;;
(define_insn "mve_vqrshlq_n_<supf><mode>"
  [
   (set (match_operand:MVE_2 0 "s_register_operand" "=w")
	(unspec:MVE_2 [(match_operand:MVE_2 1 "s_register_operand" "0")
		       (match_operand:SI 2 "s_register_operand" "r")]
	 VQRSHLQ_N))
  ]
  "TARGET_HAVE_MVE"
  "vqrshl.<supf>%#<V_sz_elem>\t%q0, %2"
  [(set_attr "type" "mve_move")
])

;;
;; [vqrshlq_s, vqrshlq_u])
;;
(define_insn "mve_vqrshlq_<supf><mode>"
  [
   (set (match_operand:MVE_2 0 "s_register_operand" "=w")
	(unspec:MVE_2 [(match_operand:MVE_2 1 "s_register_operand" "w")
		       (match_operand:MVE_2 2 "s_register_operand" "w")]
	 VQRSHLQ))
  ]
  "TARGET_HAVE_MVE"
  "vqrshl.<supf>%#<V_sz_elem>\t%q0, %q1, %q2"
  [(set_attr "type" "mve_move")
])

;;
;; [vqshlq_n_s, vqshlq_n_u])
;;
(define_insn "mve_vqshlq_n_<supf><mode>"
  [
   (set (match_operand:MVE_2 0 "s_register_operand" "=w")
	(unspec:MVE_2 [(match_operand:MVE_2 1 "s_register_operand" "w")
		       (match_operand:SI 2 "immediate_operand" "i")]
	 VQSHLQ_N))
  ]
  "TARGET_HAVE_MVE"
  "vqshl.<supf>%#<V_sz_elem>\t%q0, %q1, %2"
  [(set_attr "type" "mve_move")
])

;;
;; [vqshlq_r_u, vqshlq_r_s])
;;
(define_insn "mve_vqshlq_r_<supf><mode>"
  [
   (set (match_operand:MVE_2 0 "s_register_operand" "=w")
	(unspec:MVE_2 [(match_operand:MVE_2 1 "s_register_operand" "0")
		       (match_operand:SI 2 "s_register_operand" "r")]
	 VQSHLQ_R))
  ]
  "TARGET_HAVE_MVE"
  "vqshl.<supf>%#<V_sz_elem>\t%q0, %2"
  [(set_attr "type" "mve_move")
])

;;
;; [vqshlq_s, vqshlq_u])
;;
(define_insn "mve_vqshlq_<supf><mode>"
  [
   (set (match_operand:MVE_2 0 "s_register_operand" "=w")
	(unspec:MVE_2 [(match_operand:MVE_2 1 "s_register_operand" "w")
		       (match_operand:MVE_2 2 "s_register_operand" "w")]
	 VQSHLQ))
  ]
  "TARGET_HAVE_MVE"
  "vqshl.<supf>%#<V_sz_elem>\t%q0, %q1, %q2"
  [(set_attr "type" "mve_move")
])

;;
;; [vqshluq_n_s])
;;
(define_insn "mve_vqshluq_n_s<mode>"
  [
   (set (match_operand:MVE_2 0 "s_register_operand" "=w")
	(unspec:MVE_2 [(match_operand:MVE_2 1 "s_register_operand" "w")
		       (match_operand:SI 2 "mve_imm_7" "Ra")]
	 VQSHLUQ_N_S))
  ]
  "TARGET_HAVE_MVE"
  "vqshlu.s%#<V_sz_elem>\t%q0, %q1, %2"
  [(set_attr "type" "mve_move")
])

;;
;; [vqsubq_n_s, vqsubq_n_u])
;;
(define_insn "mve_vqsubq_n_<supf><mode>"
  [
   (set (match_operand:MVE_2 0 "s_register_operand" "=w")
	(unspec:MVE_2 [(match_operand:MVE_2 1 "s_register_operand" "w")
		       (match_operand:<V_elem> 2 "s_register_operand" "r")]
	 VQSUBQ_N))
  ]
  "TARGET_HAVE_MVE"
  "vqsub.<supf>%#<V_sz_elem>\t%q0, %q1, %2"
  [(set_attr "type" "mve_move")
])

;;
;; [vqsubq_u, vqsubq_s])
;;
(define_insn "mve_vqsubq_<supf><mode>"
  [
   (set (match_operand:MVE_2 0 "s_register_operand" "=w")
	(unspec:MVE_2 [(match_operand:MVE_2 1 "s_register_operand" "w")
		       (match_operand:MVE_2 2 "s_register_operand" "w")]
	 VQSUBQ))
  ]
  "TARGET_HAVE_MVE"
  "vqsub.<supf>%#<V_sz_elem>\t%q0, %q1, %q2"
  [(set_attr "type" "mve_move")
])

;;
;; [vrhaddq_s, vrhaddq_u])
;;
(define_insn "mve_vrhaddq_<supf><mode>"
  [
   (set (match_operand:MVE_2 0 "s_register_operand" "=w")
	(unspec:MVE_2 [(match_operand:MVE_2 1 "s_register_operand" "w")
		       (match_operand:MVE_2 2 "s_register_operand" "w")]
	 VRHADDQ))
  ]
  "TARGET_HAVE_MVE"
  "vrhadd.<supf>%#<V_sz_elem>\t%q0, %q1, %q2"
  [(set_attr "type" "mve_move")
])

;;
;; [vrmulhq_s, vrmulhq_u])
;;
(define_insn "mve_vrmulhq_<supf><mode>"
  [
   (set (match_operand:MVE_2 0 "s_register_operand" "=w")
	(unspec:MVE_2 [(match_operand:MVE_2 1 "s_register_operand" "w")
		       (match_operand:MVE_2 2 "s_register_operand" "w")]
	 VRMULHQ))
  ]
  "TARGET_HAVE_MVE"
  "vrmulh.<supf>%#<V_sz_elem>\t%q0, %q1, %q2"
  [(set_attr "type" "mve_move")
])

;;
;; [vrshlq_n_u, vrshlq_n_s])
;;
(define_insn "mve_vrshlq_n_<supf><mode>"
  [
   (set (match_operand:MVE_2 0 "s_register_operand" "=w")
	(unspec:MVE_2 [(match_operand:MVE_2 1 "s_register_operand" "0")
		       (match_operand:SI 2 "s_register_operand" "r")]
	 VRSHLQ_N))
  ]
  "TARGET_HAVE_MVE"
  "vrshl.<supf>%#<V_sz_elem>\t%q0, %2"
  [(set_attr "type" "mve_move")
])

;;
;; [vrshlq_s, vrshlq_u])
;;
(define_insn "mve_vrshlq_<supf><mode>"
  [
   (set (match_operand:MVE_2 0 "s_register_operand" "=w")
	(unspec:MVE_2 [(match_operand:MVE_2 1 "s_register_operand" "w")
		       (match_operand:MVE_2 2 "s_register_operand" "w")]
	 VRSHLQ))
  ]
  "TARGET_HAVE_MVE"
  "vrshl.<supf>%#<V_sz_elem>\t%q0, %q1, %q2"
  [(set_attr "type" "mve_move")
])

;;
;; [vrshrq_n_s, vrshrq_n_u])
;;
(define_insn "mve_vrshrq_n_<supf><mode>"
  [
   (set (match_operand:MVE_2 0 "s_register_operand" "=w")
	(unspec:MVE_2 [(match_operand:MVE_2 1 "s_register_operand" "w")
		       (match_operand:SI 2 "<MVE_pred2>" "<MVE_constraint2>")]
	 VRSHRQ_N))
  ]
  "TARGET_HAVE_MVE"
  "vrshr.<supf>%#<V_sz_elem>\t%q0, %q1, %2"
  [(set_attr "type" "mve_move")
])

;;
;; [vshlq_n_u, vshlq_n_s])
;;
(define_insn "mve_vshlq_n_<supf><mode>"
  [
   (set (match_operand:MVE_2 0 "s_register_operand" "=w")
	(unspec:MVE_2 [(match_operand:MVE_2 1 "s_register_operand" "w")
		       (match_operand:SI 2 "immediate_operand" "i")]
	 VSHLQ_N))
  ]
  "TARGET_HAVE_MVE"
  "vshl.<supf>%#<V_sz_elem>\t%q0, %q1, %2"
  [(set_attr "type" "mve_move")
])

;;
;; [vshlq_r_s, vshlq_r_u])
;;
(define_insn "mve_vshlq_r_<supf><mode>"
  [
   (set (match_operand:MVE_2 0 "s_register_operand" "=w")
	(unspec:MVE_2 [(match_operand:MVE_2 1 "s_register_operand" "0")
		       (match_operand:SI 2 "s_register_operand" "r")]
	 VSHLQ_R))
  ]
  "TARGET_HAVE_MVE"
  "vshl.<supf>%#<V_sz_elem>\t%q0, %2"
  [(set_attr "type" "mve_move")
])

;;
;; [vsubq_n_s, vsubq_n_u])
;;
(define_insn "mve_vsubq_n_<supf><mode>"
  [
   (set (match_operand:MVE_2 0 "s_register_operand" "=w")
	(unspec:MVE_2 [(match_operand:MVE_2 1 "s_register_operand" "w")
		       (match_operand:<V_elem> 2 "s_register_operand" "r")]
	 VSUBQ_N))
  ]
  "TARGET_HAVE_MVE"
  "vsub.i%#<V_sz_elem>\t%q0, %q1, %2"
  [(set_attr "type" "mve_move")
])

;;
;; [vsubq_s, vsubq_u])
;;
(define_insn "mve_vsubq_<supf><mode>"
  [
   (set (match_operand:MVE_2 0 "s_register_operand" "=w")
	(unspec:MVE_2 [(match_operand:MVE_2 1 "s_register_operand" "w")
		       (match_operand:MVE_2 2 "s_register_operand" "w")]
	 VSUBQ))
  ]
  "TARGET_HAVE_MVE"
  "vsub.i%#<V_sz_elem>\t%q0, %q1, %q2"
  [(set_attr "type" "mve_move")
])

;;
;; [vabdq_f])
;;
(define_insn "mve_vabdq_f<mode>"
  [
   (set (match_operand:MVE_0 0 "s_register_operand" "=w")
	(unspec:MVE_0 [(match_operand:MVE_0 1 "s_register_operand" "w")
		       (match_operand:MVE_0 2 "s_register_operand" "w")]
	 VABDQ_F))
  ]
  "TARGET_HAVE_MVE && TARGET_HAVE_MVE_FLOAT"
  "vabd.f%#<V_sz_elem>	%q0, %q1, %q2"
  [(set_attr "type" "mve_move")
])

;;
;; [vaddlvaq_s vaddlvaq_u])
;;
(define_insn "mve_vaddlvaq_<supf>v4si"
  [
   (set (match_operand:DI 0 "s_register_operand" "=r")
	(unspec:DI [(match_operand:DI 1 "s_register_operand" "0")
		    (match_operand:V4SI 2 "s_register_operand" "w")]
	 VADDLVAQ))
  ]
  "TARGET_HAVE_MVE"
  "vaddlva.<supf>32 %Q0, %R0, %q2"
  [(set_attr "type" "mve_move")
])

;;
;; [vaddq_n_f])
;;
(define_insn "mve_vaddq_n_f<mode>"
  [
   (set (match_operand:MVE_0 0 "s_register_operand" "=w")
	(unspec:MVE_0 [(match_operand:MVE_0 1 "s_register_operand" "w")
		       (match_operand:<V_elem> 2 "s_register_operand" "r")]
	 VADDQ_N_F))
  ]
  "TARGET_HAVE_MVE && TARGET_HAVE_MVE_FLOAT"
  "vadd.f%#<V_sz_elem>	%q0, %q1, %2"
  [(set_attr "type" "mve_move")
])

;;
;; [vandq_f])
;;
(define_insn "mve_vandq_f<mode>"
  [
   (set (match_operand:MVE_0 0 "s_register_operand" "=w")
	(unspec:MVE_0 [(match_operand:MVE_0 1 "s_register_operand" "w")
		       (match_operand:MVE_0 2 "s_register_operand" "w")]
	 VANDQ_F))
  ]
  "TARGET_HAVE_MVE && TARGET_HAVE_MVE_FLOAT"
  "vand %q0, %q1, %q2"
  [(set_attr "type" "mve_move")
])

;;
;; [vbicq_f])
;;
(define_insn "mve_vbicq_f<mode>"
  [
   (set (match_operand:MVE_0 0 "s_register_operand" "=w")
	(unspec:MVE_0 [(match_operand:MVE_0 1 "s_register_operand" "w")
		       (match_operand:MVE_0 2 "s_register_operand" "w")]
	 VBICQ_F))
  ]
  "TARGET_HAVE_MVE && TARGET_HAVE_MVE_FLOAT"
  "vbic %q0, %q1, %q2"
  [(set_attr "type" "mve_move")
])

;;
;; [vbicq_n_s, vbicq_n_u])
;;
(define_insn "mve_vbicq_n_<supf><mode>"
  [
   (set (match_operand:MVE_5 0 "s_register_operand" "=w")
	(unspec:MVE_5 [(match_operand:MVE_5 1 "s_register_operand" "0")
		       (match_operand:SI 2 "immediate_operand" "i")]
	 VBICQ_N))
  ]
  "TARGET_HAVE_MVE"
  "vbic.i%#<V_sz_elem>	%q0, %2"
  [(set_attr "type" "mve_move")
])

;;
;; [vcaddq_rot270_f])
;;
(define_insn "mve_vcaddq_rot270_f<mode>"
  [
   (set (match_operand:MVE_0 0 "s_register_operand" "=w")
	(unspec:MVE_0 [(match_operand:MVE_0 1 "s_register_operand" "w")
		       (match_operand:MVE_0 2 "s_register_operand" "w")]
	 VCADDQ_ROT270_F))
  ]
  "TARGET_HAVE_MVE && TARGET_HAVE_MVE_FLOAT"
  "vcadd.f%#<V_sz_elem>	%q0, %q1, %q2, #270"
  [(set_attr "type" "mve_move")
])

;;
;; [vcaddq_rot90_f])
;;
(define_insn "mve_vcaddq_rot90_f<mode>"
  [
   (set (match_operand:MVE_0 0 "s_register_operand" "=w")
	(unspec:MVE_0 [(match_operand:MVE_0 1 "s_register_operand" "w")
		       (match_operand:MVE_0 2 "s_register_operand" "w")]
	 VCADDQ_ROT90_F))
  ]
  "TARGET_HAVE_MVE && TARGET_HAVE_MVE_FLOAT"
  "vcadd.f%#<V_sz_elem>	%q0, %q1, %q2, #90"
  [(set_attr "type" "mve_move")
])

;;
;; [vcmpeqq_f])
;;
(define_insn "mve_vcmpeqq_f<mode>"
  [
   (set (match_operand:HI 0 "vpr_register_operand" "=Up")
	(unspec:HI [(match_operand:MVE_0 1 "s_register_operand" "w")
		    (match_operand:MVE_0 2 "s_register_operand" "w")]
	 VCMPEQQ_F))
  ]
  "TARGET_HAVE_MVE && TARGET_HAVE_MVE_FLOAT"
  "vcmp.f%#<V_sz_elem>	eq, %q1, %q2"
  [(set_attr "type" "mve_move")
])

;;
;; [vcmpeqq_n_f])
;;
(define_insn "mve_vcmpeqq_n_f<mode>"
  [
   (set (match_operand:HI 0 "vpr_register_operand" "=Up")
	(unspec:HI [(match_operand:MVE_0 1 "s_register_operand" "w")
		    (match_operand:<V_elem> 2 "s_register_operand" "r")]
	 VCMPEQQ_N_F))
  ]
  "TARGET_HAVE_MVE && TARGET_HAVE_MVE_FLOAT"
  "vcmp.f%#<V_sz_elem>	eq, %q1, %2"
  [(set_attr "type" "mve_move")
])

;;
;; [vcmpgeq_f])
;;
(define_insn "mve_vcmpgeq_f<mode>"
  [
   (set (match_operand:HI 0 "vpr_register_operand" "=Up")
	(unspec:HI [(match_operand:MVE_0 1 "s_register_operand" "w")
		    (match_operand:MVE_0 2 "s_register_operand" "w")]
	 VCMPGEQ_F))
  ]
  "TARGET_HAVE_MVE && TARGET_HAVE_MVE_FLOAT"
  "vcmp.f%#<V_sz_elem>	ge, %q1, %q2"
  [(set_attr "type" "mve_move")
])

;;
;; [vcmpgeq_n_f])
;;
(define_insn "mve_vcmpgeq_n_f<mode>"
  [
   (set (match_operand:HI 0 "vpr_register_operand" "=Up")
	(unspec:HI [(match_operand:MVE_0 1 "s_register_operand" "w")
		    (match_operand:<V_elem> 2 "s_register_operand" "r")]
	 VCMPGEQ_N_F))
  ]
  "TARGET_HAVE_MVE && TARGET_HAVE_MVE_FLOAT"
  "vcmp.f%#<V_sz_elem>	ge, %q1, %2"
  [(set_attr "type" "mve_move")
])

;;
;; [vcmpgtq_f])
;;
(define_insn "mve_vcmpgtq_f<mode>"
  [
   (set (match_operand:HI 0 "vpr_register_operand" "=Up")
	(unspec:HI [(match_operand:MVE_0 1 "s_register_operand" "w")
		    (match_operand:MVE_0 2 "s_register_operand" "w")]
	 VCMPGTQ_F))
  ]
  "TARGET_HAVE_MVE && TARGET_HAVE_MVE_FLOAT"
  "vcmp.f%#<V_sz_elem>	gt, %q1, %q2"
  [(set_attr "type" "mve_move")
])

;;
;; [vcmpgtq_n_f])
;;
(define_insn "mve_vcmpgtq_n_f<mode>"
  [
   (set (match_operand:HI 0 "vpr_register_operand" "=Up")
	(unspec:HI [(match_operand:MVE_0 1 "s_register_operand" "w")
		    (match_operand:<V_elem> 2 "s_register_operand" "r")]
	 VCMPGTQ_N_F))
  ]
  "TARGET_HAVE_MVE && TARGET_HAVE_MVE_FLOAT"
  "vcmp.f%#<V_sz_elem>	gt, %q1, %2"
  [(set_attr "type" "mve_move")
])

;;
;; [vcmpleq_f])
;;
(define_insn "mve_vcmpleq_f<mode>"
  [
   (set (match_operand:HI 0 "vpr_register_operand" "=Up")
	(unspec:HI [(match_operand:MVE_0 1 "s_register_operand" "w")
		    (match_operand:MVE_0 2 "s_register_operand" "w")]
	 VCMPLEQ_F))
  ]
  "TARGET_HAVE_MVE && TARGET_HAVE_MVE_FLOAT"
  "vcmp.f%#<V_sz_elem>	le, %q1, %q2"
  [(set_attr "type" "mve_move")
])

;;
;; [vcmpleq_n_f])
;;
(define_insn "mve_vcmpleq_n_f<mode>"
  [
   (set (match_operand:HI 0 "vpr_register_operand" "=Up")
	(unspec:HI [(match_operand:MVE_0 1 "s_register_operand" "w")
		    (match_operand:<V_elem> 2 "s_register_operand" "r")]
	 VCMPLEQ_N_F))
  ]
  "TARGET_HAVE_MVE && TARGET_HAVE_MVE_FLOAT"
  "vcmp.f%#<V_sz_elem>	le, %q1, %2"
  [(set_attr "type" "mve_move")
])

;;
;; [vcmpltq_f])
;;
(define_insn "mve_vcmpltq_f<mode>"
  [
   (set (match_operand:HI 0 "vpr_register_operand" "=Up")
	(unspec:HI [(match_operand:MVE_0 1 "s_register_operand" "w")
		    (match_operand:MVE_0 2 "s_register_operand" "w")]
	 VCMPLTQ_F))
  ]
  "TARGET_HAVE_MVE && TARGET_HAVE_MVE_FLOAT"
  "vcmp.f%#<V_sz_elem>	lt, %q1, %q2"
  [(set_attr "type" "mve_move")
])

;;
;; [vcmpltq_n_f])
;;
(define_insn "mve_vcmpltq_n_f<mode>"
  [
   (set (match_operand:HI 0 "vpr_register_operand" "=Up")
	(unspec:HI [(match_operand:MVE_0 1 "s_register_operand" "w")
		    (match_operand:<V_elem> 2 "s_register_operand" "r")]
	 VCMPLTQ_N_F))
  ]
  "TARGET_HAVE_MVE && TARGET_HAVE_MVE_FLOAT"
  "vcmp.f%#<V_sz_elem>	lt, %q1, %2"
  [(set_attr "type" "mve_move")
])

;;
;; [vcmpneq_f])
;;
(define_insn "mve_vcmpneq_f<mode>"
  [
   (set (match_operand:HI 0 "vpr_register_operand" "=Up")
	(unspec:HI [(match_operand:MVE_0 1 "s_register_operand" "w")
		    (match_operand:MVE_0 2 "s_register_operand" "w")]
	 VCMPNEQ_F))
  ]
  "TARGET_HAVE_MVE && TARGET_HAVE_MVE_FLOAT"
  "vcmp.f%#<V_sz_elem>	ne, %q1, %q2"
  [(set_attr "type" "mve_move")
])

;;
;; [vcmpneq_n_f])
;;
(define_insn "mve_vcmpneq_n_f<mode>"
  [
   (set (match_operand:HI 0 "vpr_register_operand" "=Up")
	(unspec:HI [(match_operand:MVE_0 1 "s_register_operand" "w")
		    (match_operand:<V_elem> 2 "s_register_operand" "r")]
	 VCMPNEQ_N_F))
  ]
  "TARGET_HAVE_MVE && TARGET_HAVE_MVE_FLOAT"
  "vcmp.f%#<V_sz_elem>	ne, %q1, %2"
  [(set_attr "type" "mve_move")
])

;;
;; [vcmulq_f])
;;
(define_insn "mve_vcmulq_f<mode>"
  [
   (set (match_operand:MVE_0 0 "s_register_operand" "=w")
	(unspec:MVE_0 [(match_operand:MVE_0 1 "s_register_operand" "w")
		       (match_operand:MVE_0 2 "s_register_operand" "w")]
	 VCMULQ_F))
  ]
  "TARGET_HAVE_MVE && TARGET_HAVE_MVE_FLOAT"
  "vcmul.f%#<V_sz_elem>	%q0, %q1, %q2, #0"
  [(set_attr "type" "mve_move")
])

;;
;; [vcmulq_rot180_f])
;;
(define_insn "mve_vcmulq_rot180_f<mode>"
  [
   (set (match_operand:MVE_0 0 "s_register_operand" "=w")
	(unspec:MVE_0 [(match_operand:MVE_0 1 "s_register_operand" "w")
		       (match_operand:MVE_0 2 "s_register_operand" "w")]
	 VCMULQ_ROT180_F))
  ]
  "TARGET_HAVE_MVE && TARGET_HAVE_MVE_FLOAT"
  "vcmul.f%#<V_sz_elem>	%q0, %q1, %q2, #180"
  [(set_attr "type" "mve_move")
])

;;
;; [vcmulq_rot270_f])
;;
(define_insn "mve_vcmulq_rot270_f<mode>"
  [
   (set (match_operand:MVE_0 0 "s_register_operand" "=w")
	(unspec:MVE_0 [(match_operand:MVE_0 1 "s_register_operand" "w")
		       (match_operand:MVE_0 2 "s_register_operand" "w")]
	 VCMULQ_ROT270_F))
  ]
  "TARGET_HAVE_MVE && TARGET_HAVE_MVE_FLOAT"
  "vcmul.f%#<V_sz_elem>	%q0, %q1, %q2, #270"
  [(set_attr "type" "mve_move")
])

;;
;; [vcmulq_rot90_f])
;;
(define_insn "mve_vcmulq_rot90_f<mode>"
  [
   (set (match_operand:MVE_0 0 "s_register_operand" "=w")
	(unspec:MVE_0 [(match_operand:MVE_0 1 "s_register_operand" "w")
		       (match_operand:MVE_0 2 "s_register_operand" "w")]
	 VCMULQ_ROT90_F))
  ]
  "TARGET_HAVE_MVE && TARGET_HAVE_MVE_FLOAT"
  "vcmul.f%#<V_sz_elem>	%q0, %q1, %q2, #90"
  [(set_attr "type" "mve_move")
])

;;
;; [vctp8q_m vctp16q_m vctp32q_m vctp64q_m])
;;
(define_insn "mve_vctp<mode1>q_mhi"
  [
   (set (match_operand:HI 0 "vpr_register_operand" "=Up")
	(unspec:HI [(match_operand:SI 1 "s_register_operand" "r")
		    (match_operand:HI 2 "vpr_register_operand" "Up")]
	 VCTPQ_M))
  ]
  "TARGET_HAVE_MVE"
  "vpst\;vctpt.<mode1> %1"
  [(set_attr "type" "mve_move")
   (set_attr "length""8")])

;;
;; [vcvtbq_f16_f32])
;;
(define_insn "mve_vcvtbq_f16_f32v8hf"
  [
   (set (match_operand:V8HF 0 "s_register_operand" "=w")
	(unspec:V8HF [(match_operand:V8HF 1 "s_register_operand" "0")
		      (match_operand:V4SF 2 "s_register_operand" "w")]
	 VCVTBQ_F16_F32))
  ]
  "TARGET_HAVE_MVE && TARGET_HAVE_MVE_FLOAT"
  "vcvtb.f16.f32 %q0, %q2"
  [(set_attr "type" "mve_move")
])

;;
;; [vcvttq_f16_f32])
;;
(define_insn "mve_vcvttq_f16_f32v8hf"
  [
   (set (match_operand:V8HF 0 "s_register_operand" "=w")
	(unspec:V8HF [(match_operand:V8HF 1 "s_register_operand" "0")
		      (match_operand:V4SF 2 "s_register_operand" "w")]
	 VCVTTQ_F16_F32))
  ]
  "TARGET_HAVE_MVE && TARGET_HAVE_MVE_FLOAT"
  "vcvtt.f16.f32 %q0, %q2"
  [(set_attr "type" "mve_move")
])

;;
;; [veorq_f])
;;
(define_insn "mve_veorq_f<mode>"
  [
   (set (match_operand:MVE_0 0 "s_register_operand" "=w")
	(unspec:MVE_0 [(match_operand:MVE_0 1 "s_register_operand" "w")
		       (match_operand:MVE_0 2 "s_register_operand" "w")]
	 VEORQ_F))
  ]
  "TARGET_HAVE_MVE && TARGET_HAVE_MVE_FLOAT"
  "veor %q0, %q1, %q2"
  [(set_attr "type" "mve_move")
])

;;
;; [vmaxnmaq_f])
;;
(define_insn "mve_vmaxnmaq_f<mode>"
  [
   (set (match_operand:MVE_0 0 "s_register_operand" "=w")
	(unspec:MVE_0 [(match_operand:MVE_0 1 "s_register_operand" "0")
		       (match_operand:MVE_0 2 "s_register_operand" "w")]
	 VMAXNMAQ_F))
  ]
  "TARGET_HAVE_MVE && TARGET_HAVE_MVE_FLOAT"
  "vmaxnma.f%#<V_sz_elem>	%q0, %q2"
  [(set_attr "type" "mve_move")
])

;;
;; [vmaxnmavq_f])
;;
(define_insn "mve_vmaxnmavq_f<mode>"
  [
   (set (match_operand:<V_elem> 0 "s_register_operand" "=r")
	(unspec:<V_elem> [(match_operand:<V_elem> 1 "s_register_operand" "0")
			  (match_operand:MVE_0 2 "s_register_operand" "w")]
	 VMAXNMAVQ_F))
  ]
  "TARGET_HAVE_MVE && TARGET_HAVE_MVE_FLOAT"
  "vmaxnmav.f%#<V_sz_elem>	%0, %q2"
  [(set_attr "type" "mve_move")
])

;;
;; [vmaxnmq_f])
;;
(define_insn "mve_vmaxnmq_f<mode>"
  [
   (set (match_operand:MVE_0 0 "s_register_operand" "=w")
	(unspec:MVE_0 [(match_operand:MVE_0 1 "s_register_operand" "w")
		       (match_operand:MVE_0 2 "s_register_operand" "w")]
	 VMAXNMQ_F))
  ]
  "TARGET_HAVE_MVE && TARGET_HAVE_MVE_FLOAT"
  "vmaxnm.f%#<V_sz_elem>	%q0, %q1, %q2"
  [(set_attr "type" "mve_move")
])

;;
;; [vmaxnmvq_f])
;;
(define_insn "mve_vmaxnmvq_f<mode>"
  [
   (set (match_operand:<V_elem> 0 "s_register_operand" "=r")
	(unspec:<V_elem> [(match_operand:<V_elem> 1 "s_register_operand" "0")
			  (match_operand:MVE_0 2 "s_register_operand" "w")]
	 VMAXNMVQ_F))
  ]
  "TARGET_HAVE_MVE && TARGET_HAVE_MVE_FLOAT"
  "vmaxnmv.f%#<V_sz_elem>	%0, %q2"
  [(set_attr "type" "mve_move")
])

;;
;; [vminnmaq_f])
;;
(define_insn "mve_vminnmaq_f<mode>"
  [
   (set (match_operand:MVE_0 0 "s_register_operand" "=w")
	(unspec:MVE_0 [(match_operand:MVE_0 1 "s_register_operand" "0")
		       (match_operand:MVE_0 2 "s_register_operand" "w")]
	 VMINNMAQ_F))
  ]
  "TARGET_HAVE_MVE && TARGET_HAVE_MVE_FLOAT"
  "vminnma.f%#<V_sz_elem>	%q0, %q2"
  [(set_attr "type" "mve_move")
])

;;
;; [vminnmavq_f])
;;
(define_insn "mve_vminnmavq_f<mode>"
  [
   (set (match_operand:<V_elem> 0 "s_register_operand" "=r")
	(unspec:<V_elem> [(match_operand:<V_elem> 1 "s_register_operand" "0")
			  (match_operand:MVE_0 2 "s_register_operand" "w")]
	 VMINNMAVQ_F))
  ]
  "TARGET_HAVE_MVE && TARGET_HAVE_MVE_FLOAT"
  "vminnmav.f%#<V_sz_elem>	%0, %q2"
  [(set_attr "type" "mve_move")
])

;;
;; [vminnmq_f])
;;
(define_insn "mve_vminnmq_f<mode>"
  [
   (set (match_operand:MVE_0 0 "s_register_operand" "=w")
	(unspec:MVE_0 [(match_operand:MVE_0 1 "s_register_operand" "w")
		       (match_operand:MVE_0 2 "s_register_operand" "w")]
	 VMINNMQ_F))
  ]
  "TARGET_HAVE_MVE && TARGET_HAVE_MVE_FLOAT"
  "vminnm.f%#<V_sz_elem>	%q0, %q1, %q2"
  [(set_attr "type" "mve_move")
])

;;
;; [vminnmvq_f])
;;
(define_insn "mve_vminnmvq_f<mode>"
  [
   (set (match_operand:<V_elem> 0 "s_register_operand" "=r")
	(unspec:<V_elem> [(match_operand:<V_elem> 1 "s_register_operand" "0")
			  (match_operand:MVE_0 2 "s_register_operand" "w")]
	 VMINNMVQ_F))
  ]
  "TARGET_HAVE_MVE && TARGET_HAVE_MVE_FLOAT"
  "vminnmv.f%#<V_sz_elem>	%0, %q2"
  [(set_attr "type" "mve_move")
])

;;
;; [vmlaldavq_u, vmlaldavq_s])
;;
(define_insn "mve_vmlaldavq_<supf><mode>"
  [
   (set (match_operand:DI 0 "s_register_operand" "=r")
	(unspec:DI [(match_operand:MVE_5 1 "s_register_operand" "w")
		    (match_operand:MVE_5 2 "s_register_operand" "w")]
	 VMLALDAVQ))
  ]
  "TARGET_HAVE_MVE"
  "vmlaldav.<supf>%#<V_sz_elem>	%Q0, %R0, %q1, %q2"
  [(set_attr "type" "mve_move")
])

;;
;; [vmlaldavxq_s])
;;
(define_insn "mve_vmlaldavxq_s<mode>"
  [
   (set (match_operand:DI 0 "s_register_operand" "=r")
	(unspec:DI [(match_operand:MVE_5 1 "s_register_operand" "w")
		    (match_operand:MVE_5 2 "s_register_operand" "w")]
	 VMLALDAVXQ_S))
  ]
  "TARGET_HAVE_MVE"
  "vmlaldavx.s%#<V_sz_elem> %Q0, %R0, %q1, %q2"
  [(set_attr "type" "mve_move")
])

;;
;; [vmlsldavq_s])
;;
(define_insn "mve_vmlsldavq_s<mode>"
  [
   (set (match_operand:DI 0 "s_register_operand" "=r")
	(unspec:DI [(match_operand:MVE_5 1 "s_register_operand" "w")
		    (match_operand:MVE_5 2 "s_register_operand" "w")]
	 VMLSLDAVQ_S))
  ]
  "TARGET_HAVE_MVE"
  "vmlsldav.s%#<V_sz_elem> %Q0, %R0, %q1, %q2"
  [(set_attr "type" "mve_move")
])

;;
;; [vmlsldavxq_s])
;;
(define_insn "mve_vmlsldavxq_s<mode>"
  [
   (set (match_operand:DI 0 "s_register_operand" "=r")
	(unspec:DI [(match_operand:MVE_5 1 "s_register_operand" "w")
		    (match_operand:MVE_5 2 "s_register_operand" "w")]
	 VMLSLDAVXQ_S))
  ]
  "TARGET_HAVE_MVE"
  "vmlsldavx.s%#<V_sz_elem> %Q0, %R0, %q1, %q2"
  [(set_attr "type" "mve_move")
])

;;
;; [vmovnbq_u, vmovnbq_s])
;;
(define_insn "mve_vmovnbq_<supf><mode>"
  [
   (set (match_operand:<V_narrow_pack> 0 "s_register_operand" "=w")
	(unspec:<V_narrow_pack> [(match_operand:<V_narrow_pack> 1 "s_register_operand" "0")
				 (match_operand:MVE_5 2 "s_register_operand" "w")]
	 VMOVNBQ))
  ]
  "TARGET_HAVE_MVE"
  "vmovnb.i%#<V_sz_elem>	%q0, %q2"
  [(set_attr "type" "mve_move")
])

;;
;; [vmovntq_s, vmovntq_u])
;;
(define_insn "mve_vmovntq_<supf><mode>"
  [
   (set (match_operand:<V_narrow_pack> 0 "s_register_operand" "=w")
	(unspec:<V_narrow_pack> [(match_operand:<V_narrow_pack> 1 "s_register_operand" "0")
				 (match_operand:MVE_5 2 "s_register_operand" "w")]
	 VMOVNTQ))
  ]
  "TARGET_HAVE_MVE"
  "vmovnt.i%#<V_sz_elem>	%q0, %q2"
  [(set_attr "type" "mve_move")
])

;;
;; [vmulq_f])
;;
(define_insn "mve_vmulq_f<mode>"
  [
   (set (match_operand:MVE_0 0 "s_register_operand" "=w")
	(unspec:MVE_0 [(match_operand:MVE_0 1 "s_register_operand" "w")
		       (match_operand:MVE_0 2 "s_register_operand" "w")]
	 VMULQ_F))
  ]
  "TARGET_HAVE_MVE && TARGET_HAVE_MVE_FLOAT"
  "vmul.f%#<V_sz_elem>	%q0, %q1, %q2"
  [(set_attr "type" "mve_move")
])

;;
;; [vmulq_n_f])
;;
(define_insn "mve_vmulq_n_f<mode>"
  [
   (set (match_operand:MVE_0 0 "s_register_operand" "=w")
	(unspec:MVE_0 [(match_operand:MVE_0 1 "s_register_operand" "w")
		       (match_operand:<V_elem> 2 "s_register_operand" "r")]
	 VMULQ_N_F))
  ]
  "TARGET_HAVE_MVE && TARGET_HAVE_MVE_FLOAT"
  "vmul.f%#<V_sz_elem>	%q0, %q1, %2"
  [(set_attr "type" "mve_move")
])

;;
;; [vornq_f])
;;
(define_insn "mve_vornq_f<mode>"
  [
   (set (match_operand:MVE_0 0 "s_register_operand" "=w")
	(unspec:MVE_0 [(match_operand:MVE_0 1 "s_register_operand" "w")
		       (match_operand:MVE_0 2 "s_register_operand" "w")]
	 VORNQ_F))
  ]
  "TARGET_HAVE_MVE && TARGET_HAVE_MVE_FLOAT"
  "vorn %q0, %q1, %q2"
  [(set_attr "type" "mve_move")
])

;;
;; [vorrq_f])
;;
(define_insn "mve_vorrq_f<mode>"
  [
   (set (match_operand:MVE_0 0 "s_register_operand" "=w")
	(unspec:MVE_0 [(match_operand:MVE_0 1 "s_register_operand" "w")
		       (match_operand:MVE_0 2 "s_register_operand" "w")]
	 VORRQ_F))
  ]
  "TARGET_HAVE_MVE && TARGET_HAVE_MVE_FLOAT"
  "vorr %q0, %q1, %q2"
  [(set_attr "type" "mve_move")
])

;;
;; [vorrq_n_u, vorrq_n_s])
;;
(define_insn "mve_vorrq_n_<supf><mode>"
  [
   (set (match_operand:MVE_5 0 "s_register_operand" "=w")
	(unspec:MVE_5 [(match_operand:MVE_5 1 "s_register_operand" "0")
		       (match_operand:SI 2 "immediate_operand" "i")]
	 VORRQ_N))
  ]
  "TARGET_HAVE_MVE"
  "vorr.i%#<V_sz_elem>	%q0, %2"
  [(set_attr "type" "mve_move")
])

;;
;; [vqdmullbq_n_s])
;;
(define_insn "mve_vqdmullbq_n_s<mode>"
  [
   (set (match_operand:<V_double_width> 0 "s_register_operand" "=w")
	(unspec:<V_double_width> [(match_operand:MVE_5 1 "s_register_operand" "w")
				  (match_operand:<V_elem> 2 "s_register_operand" "r")]
	 VQDMULLBQ_N_S))
  ]
  "TARGET_HAVE_MVE"
  "vqdmullb.s%#<V_sz_elem>	%q0, %q1, %2"
  [(set_attr "type" "mve_move")
])

;;
;; [vqdmullbq_s])
;;
(define_insn "mve_vqdmullbq_s<mode>"
  [
   (set (match_operand:<V_double_width> 0 "s_register_operand" "=w")
	(unspec:<V_double_width> [(match_operand:MVE_5 1 "s_register_operand" "w")
				  (match_operand:MVE_5 2 "s_register_operand" "w")]
	 VQDMULLBQ_S))
  ]
  "TARGET_HAVE_MVE"
  "vqdmullb.s%#<V_sz_elem>	%q0, %q1, %q2"
  [(set_attr "type" "mve_move")
])

;;
;; [vqdmulltq_n_s])
;;
(define_insn "mve_vqdmulltq_n_s<mode>"
  [
   (set (match_operand:<V_double_width> 0 "s_register_operand" "=w")
	(unspec:<V_double_width> [(match_operand:MVE_5 1 "s_register_operand" "w")
				  (match_operand:<V_elem> 2 "s_register_operand" "r")]
	 VQDMULLTQ_N_S))
  ]
  "TARGET_HAVE_MVE"
  "vqdmullt.s%#<V_sz_elem>	%q0, %q1, %2"
  [(set_attr "type" "mve_move")
])

;;
;; [vqdmulltq_s])
;;
(define_insn "mve_vqdmulltq_s<mode>"
  [
   (set (match_operand:<V_double_width> 0 "s_register_operand" "=w")
	(unspec:<V_double_width> [(match_operand:MVE_5 1 "s_register_operand" "w")
				  (match_operand:MVE_5 2 "s_register_operand" "w")]
	 VQDMULLTQ_S))
  ]
  "TARGET_HAVE_MVE"
  "vqdmullt.s%#<V_sz_elem>	%q0, %q1, %q2"
  [(set_attr "type" "mve_move")
])

;;
;; [vqmovnbq_u, vqmovnbq_s])
;;
(define_insn "mve_vqmovnbq_<supf><mode>"
  [
   (set (match_operand:<V_narrow_pack> 0 "s_register_operand" "=w")
	(unspec:<V_narrow_pack> [(match_operand:<V_narrow_pack> 1 "s_register_operand" "0")
				 (match_operand:MVE_5 2 "s_register_operand" "w")]
	 VQMOVNBQ))
  ]
  "TARGET_HAVE_MVE"
  "vqmovnb.<supf>%#<V_sz_elem>	%q0, %q2"
  [(set_attr "type" "mve_move")
])

;;
;; [vqmovntq_u, vqmovntq_s])
;;
(define_insn "mve_vqmovntq_<supf><mode>"
  [
   (set (match_operand:<V_narrow_pack> 0 "s_register_operand" "=w")
	(unspec:<V_narrow_pack> [(match_operand:<V_narrow_pack> 1 "s_register_operand" "0")
				 (match_operand:MVE_5 2 "s_register_operand" "w")]
	 VQMOVNTQ))
  ]
  "TARGET_HAVE_MVE"
  "vqmovnt.<supf>%#<V_sz_elem>	%q0, %q2"
  [(set_attr "type" "mve_move")
])

;;
;; [vqmovunbq_s])
;;
(define_insn "mve_vqmovunbq_s<mode>"
  [
   (set (match_operand:<V_narrow_pack> 0 "s_register_operand" "=w")
	(unspec:<V_narrow_pack> [(match_operand:<V_narrow_pack> 1 "s_register_operand" "0")
				 (match_operand:MVE_5 2 "s_register_operand" "w")]
	 VQMOVUNBQ_S))
  ]
  "TARGET_HAVE_MVE"
  "vqmovunb.s%#<V_sz_elem>	%q0, %q2"
  [(set_attr "type" "mve_move")
])

;;
;; [vqmovuntq_s])
;;
(define_insn "mve_vqmovuntq_s<mode>"
  [
   (set (match_operand:<V_narrow_pack> 0 "s_register_operand" "=w")
	(unspec:<V_narrow_pack> [(match_operand:<V_narrow_pack> 1 "s_register_operand" "0")
				 (match_operand:MVE_5 2 "s_register_operand" "w")]
	 VQMOVUNTQ_S))
  ]
  "TARGET_HAVE_MVE"
  "vqmovunt.s%#<V_sz_elem>	%q0, %q2"
  [(set_attr "type" "mve_move")
])

;;
;; [vrmlaldavhxq_s])
;;
(define_insn "mve_vrmlaldavhxq_sv4si"
  [
   (set (match_operand:DI 0 "s_register_operand" "=r")
	(unspec:DI [(match_operand:V4SI 1 "s_register_operand" "w")
		    (match_operand:V4SI 2 "s_register_operand" "w")]
	 VRMLALDAVHXQ_S))
  ]
  "TARGET_HAVE_MVE"
  "vrmlaldavhx.s32 %Q0, %R0, %q1, %q2"
  [(set_attr "type" "mve_move")
])

;;
;; [vrmlsldavhq_s])
;;
(define_insn "mve_vrmlsldavhq_sv4si"
  [
   (set (match_operand:DI 0 "s_register_operand" "=r")
	(unspec:DI [(match_operand:V4SI 1 "s_register_operand" "w")
		    (match_operand:V4SI 2 "s_register_operand" "w")]
	 VRMLSLDAVHQ_S))
  ]
  "TARGET_HAVE_MVE"
  "vrmlsldavh.s32\t%Q0, %R0, %q1, %q2"
  [(set_attr "type" "mve_move")
])

;;
;; [vrmlsldavhxq_s])
;;
(define_insn "mve_vrmlsldavhxq_sv4si"
  [
   (set (match_operand:DI 0 "s_register_operand" "=r")
	(unspec:DI [(match_operand:V4SI 1 "s_register_operand" "w")
		    (match_operand:V4SI 2 "s_register_operand" "w")]
	 VRMLSLDAVHXQ_S))
  ]
  "TARGET_HAVE_MVE"
  "vrmlsldavhx.s32\t%Q0, %R0, %q1, %q2"
  [(set_attr "type" "mve_move")
])

;;
;; [vshllbq_n_s, vshllbq_n_u])
;;
(define_insn "mve_vshllbq_n_<supf><mode>"
  [
   (set (match_operand:<V_double_width> 0 "s_register_operand" "=w")
	(unspec:<V_double_width> [(match_operand:MVE_3 1 "s_register_operand" "w")
				  (match_operand:SI 2 "immediate_operand" "i")]
	 VSHLLBQ_N))
  ]
  "TARGET_HAVE_MVE"
  "vshllb.<supf>%#<V_sz_elem>\t%q0, %q1, %2"
  [(set_attr "type" "mve_move")
])

;;
;; [vshlltq_n_u, vshlltq_n_s])
;;
(define_insn "mve_vshlltq_n_<supf><mode>"
  [
   (set (match_operand:<V_double_width> 0 "s_register_operand" "=w")
	(unspec:<V_double_width> [(match_operand:MVE_3 1 "s_register_operand" "w")
				  (match_operand:SI 2 "immediate_operand" "i")]
	 VSHLLTQ_N))
  ]
  "TARGET_HAVE_MVE"
  "vshllt.<supf>%#<V_sz_elem>\t%q0, %q1, %2"
  [(set_attr "type" "mve_move")
])

;;
;; [vsubq_f])
;;
(define_insn "mve_vsubq_f<mode>"
  [
   (set (match_operand:MVE_0 0 "s_register_operand" "=w")
	(unspec:MVE_0 [(match_operand:MVE_0 1 "s_register_operand" "w")
		       (match_operand:MVE_0 2 "s_register_operand" "w")]
	 VSUBQ_F))
  ]
  "TARGET_HAVE_MVE && TARGET_HAVE_MVE_FLOAT"
  "vsub.f%#<V_sz_elem>\t%q0, %q1, %q2"
  [(set_attr "type" "mve_move")
])

;;
;; [vmulltq_poly_p])
;;
(define_insn "mve_vmulltq_poly_p<mode>"
  [
   (set (match_operand:<V_double_width> 0 "s_register_operand" "=w")
	(unspec:<V_double_width> [(match_operand:MVE_3 1 "s_register_operand" "w")
				  (match_operand:MVE_3 2 "s_register_operand" "w")]
	 VMULLTQ_POLY_P))
  ]
  "TARGET_HAVE_MVE"
  "vmullt.p%#<V_sz_elem>\t%q0, %q1, %q2"
  [(set_attr "type" "mve_move")
])

;;
;; [vmullbq_poly_p])
;;
(define_insn "mve_vmullbq_poly_p<mode>"
  [
   (set (match_operand:<V_double_width> 0 "s_register_operand" "=w")
	(unspec:<V_double_width> [(match_operand:MVE_3 1 "s_register_operand" "w")
				  (match_operand:MVE_3 2 "s_register_operand" "w")]
	 VMULLBQ_POLY_P))
  ]
  "TARGET_HAVE_MVE"
  "vmullb.p%#<V_sz_elem>\t%q0, %q1, %q2"
  [(set_attr "type" "mve_move")
])

;;
;; [vrmlaldavhq_u vrmlaldavhq_s])
;;
(define_insn "mve_vrmlaldavhq_<supf>v4si"
  [
   (set (match_operand:DI 0 "s_register_operand" "=r")
	(unspec:DI [(match_operand:V4SI 1 "s_register_operand" "w")
		    (match_operand:V4SI 2 "s_register_operand" "w")]
	 VRMLALDAVHQ))
  ]
  "TARGET_HAVE_MVE"
  "vrmlaldavh.<supf>32 %Q0, %R0, %q1, %q2"
  [(set_attr "type" "mve_move")
])

;;
;; [vbicq_m_n_s, vbicq_m_n_u])
;;
(define_insn "mve_vbicq_m_n_<supf><mode>"
  [
   (set (match_operand:MVE_5 0 "s_register_operand" "=w")
	(unspec:MVE_5 [(match_operand:MVE_5 1 "s_register_operand" "0")
		       (match_operand:SI 2 "immediate_operand" "i")
		       (match_operand:HI 3 "vpr_register_operand" "Up")]
	 VBICQ_M_N))
  ]
  "TARGET_HAVE_MVE"
  "vpst\;vbict.i%#<V_sz_elem>	%q0, %2"
  [(set_attr "type" "mve_move")
   (set_attr "length""8")])
;;
;; [vcmpeqq_m_f])
;;
(define_insn "mve_vcmpeqq_m_f<mode>"
  [
   (set (match_operand:HI 0 "vpr_register_operand" "=Up")
	(unspec:HI [(match_operand:MVE_0 1 "s_register_operand" "w")
		    (match_operand:MVE_0 2 "s_register_operand" "w")
		    (match_operand:HI 3 "vpr_register_operand" "Up")]
	 VCMPEQQ_M_F))
  ]
  "TARGET_HAVE_MVE && TARGET_HAVE_MVE_FLOAT"
  "vpst\;vcmpt.f%#<V_sz_elem>	eq, %q1, %q2"
  [(set_attr "type" "mve_move")
   (set_attr "length""8")])
;;
;; [vcvtaq_m_u, vcvtaq_m_s])
;;
(define_insn "mve_vcvtaq_m_<supf><mode>"
  [
   (set (match_operand:MVE_5 0 "s_register_operand" "=w")
	(unspec:MVE_5 [(match_operand:MVE_5 1 "s_register_operand" "0")
		       (match_operand:<MVE_CNVT> 2 "s_register_operand" "w")
		       (match_operand:HI 3 "vpr_register_operand" "Up")]
	 VCVTAQ_M))
  ]
  "TARGET_HAVE_MVE && TARGET_HAVE_MVE_FLOAT"
  "vpst\;vcvtat.<supf>%#<V_sz_elem>.f%#<V_sz_elem>\t%q0, %q2"
  [(set_attr "type" "mve_move")
   (set_attr "length""8")])
;;
;; [vcvtq_m_to_f_s, vcvtq_m_to_f_u])
;;
(define_insn "mve_vcvtq_m_to_f_<supf><mode>"
  [
   (set (match_operand:MVE_0 0 "s_register_operand" "=w")
	(unspec:MVE_0 [(match_operand:MVE_0 1 "s_register_operand" "0")
		       (match_operand:<MVE_CNVT> 2 "s_register_operand" "w")
		       (match_operand:HI 3 "vpr_register_operand" "Up")]
	 VCVTQ_M_TO_F))
  ]
  "TARGET_HAVE_MVE && TARGET_HAVE_MVE_FLOAT"
  "vpst\;vcvtt.f%#<V_sz_elem>.<supf>%#<V_sz_elem>	 %q0, %q2"
  [(set_attr "type" "mve_move")
   (set_attr "length""8")])
;;
;; [vqrshrnbq_n_u, vqrshrnbq_n_s])
;;
(define_insn "mve_vqrshrnbq_n_<supf><mode>"
  [
   (set (match_operand:<V_narrow_pack> 0 "s_register_operand" "=w")
	(unspec:<V_narrow_pack> [(match_operand:<V_narrow_pack> 1 "s_register_operand" "0")
				 (match_operand:MVE_5 2 "s_register_operand" "w")
				 (match_operand:SI 3 "mve_imm_8" "Rb")]
	 VQRSHRNBQ_N))
  ]
  "TARGET_HAVE_MVE"
  "vqrshrnb.<supf>%#<V_sz_elem>	%q0, %q2, %3"
  [(set_attr "type" "mve_move")
])
;;
;; [vqrshrunbq_n_s])
;;
(define_insn "mve_vqrshrunbq_n_s<mode>"
  [
   (set (match_operand:<V_narrow_pack> 0 "s_register_operand" "=w")
	(unspec:<V_narrow_pack> [(match_operand:<V_narrow_pack> 1 "s_register_operand" "0")
				 (match_operand:MVE_5 2 "s_register_operand" "w")
				 (match_operand:SI 3 "mve_imm_8" "Rb")]
	 VQRSHRUNBQ_N_S))
  ]
  "TARGET_HAVE_MVE"
  "vqrshrunb.s%#<V_sz_elem>\t%q0, %q2, %3"
  [(set_attr "type" "mve_move")
])
;;
;; [vrmlaldavhaq_s vrmlaldavhaq_u])
;;
(define_insn "mve_vrmlaldavhaq_<supf>v4si"
  [
   (set (match_operand:DI 0 "s_register_operand" "=r")
	(unspec:DI [(match_operand:DI 1 "s_register_operand" "0")
		    (match_operand:V4SI 2 "s_register_operand" "w")
		    (match_operand:V4SI 3 "s_register_operand" "w")]
	 VRMLALDAVHAQ))
  ]
  "TARGET_HAVE_MVE"
  "vrmlaldavha.<supf>32 %Q0, %R0, %q2, %q3"
  [(set_attr "type" "mve_move")
])

;;
;; [vabavq_s, vabavq_u])
;;
(define_insn "mve_vabavq_<supf><mode>"
  [
   (set (match_operand:SI 0 "s_register_operand" "=r")
	(unspec:SI [(match_operand:SI 1 "s_register_operand" "0")
		    (match_operand:MVE_2 2 "s_register_operand" "w")
		    (match_operand:MVE_2 3 "s_register_operand" "w")]
	 VABAVQ))
  ]
  "TARGET_HAVE_MVE"
  "vabav.<supf>%#<V_sz_elem>\t%0, %q2, %q3"
  [(set_attr "type" "mve_move")
])

;;
;; [vshlcq_u vshlcq_s]
;;
(define_expand "mve_vshlcq_vec_<supf><mode>"
 [(match_operand:MVE_2 0 "s_register_operand")
  (match_operand:MVE_2 1 "s_register_operand")
  (match_operand:SI 2 "s_register_operand")
  (match_operand:SI 3 "mve_imm_32")
  (unspec:MVE_2 [(const_int 0)] VSHLCQ)]
 "TARGET_HAVE_MVE"
{
  rtx ignore_wb = gen_reg_rtx (SImode);
  emit_insn(gen_mve_vshlcq_<supf><mode>(operands[0], ignore_wb, operands[1],
                                      operands[2], operands[3]));
  DONE;
})

(define_expand "mve_vshlcq_carry_<supf><mode>"
 [(match_operand:SI 0 "s_register_operand")
  (match_operand:MVE_2 1 "s_register_operand")
  (match_operand:SI 2 "s_register_operand")
  (match_operand:SI 3 "mve_imm_32")
  (unspec:MVE_2 [(const_int 0)] VSHLCQ)]
 "TARGET_HAVE_MVE"
{
  rtx ignore_vec = gen_reg_rtx (<MODE>mode);
  emit_insn(gen_mve_vshlcq_<supf><mode>(ignore_vec, operands[0], operands[1],
				      operands[2], operands[3]));
  DONE;
})

(define_insn "mve_vshlcq_<supf><mode>"
 [(set (match_operand:MVE_2 0 "s_register_operand" "=w")
       (unspec:MVE_2 [(match_operand:MVE_2 2 "s_register_operand" "0")
		      (match_operand:SI 3 "s_register_operand" "1")
		      (match_operand:SI 4 "mve_imm_32" "Rf")]
	VSHLCQ))
  (set (match_operand:SI  1 "s_register_operand" "=r")
       (unspec:SI [(match_dup 2)
		   (match_dup 3)
		   (match_dup 4)]
	VSHLCQ))]
 "TARGET_HAVE_MVE"
 "vshlc %q0, %1, %4")
