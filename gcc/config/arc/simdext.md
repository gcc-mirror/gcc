;; Machine description of the Synopsys DesignWare ARC cpu for GNU C compiler
;; Copyright (C) 2007-2014 Free Software Foundation, Inc.

;; This file is part of GCC.

;; GCC is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; GCC is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GCC; see the file COPYING3.  If not see
;; <http://www.gnu.org/licenses/>.

(define_constants
  [
  ;; Va, Vb, Vc builtins
  (UNSPEC_ARC_SIMD_VADDAW     1000)
  (UNSPEC_ARC_SIMD_VADDW      1001)
  (UNSPEC_ARC_SIMD_VAVB       1002)
  (UNSPEC_ARC_SIMD_VAVRB      1003)
  (UNSPEC_ARC_SIMD_VDIFAW     1004)
  (UNSPEC_ARC_SIMD_VDIFW      1005)
  (UNSPEC_ARC_SIMD_VMAXAW     1006)
  (UNSPEC_ARC_SIMD_VMAXW      1007)
  (UNSPEC_ARC_SIMD_VMINAW     1008)
  (UNSPEC_ARC_SIMD_VMINW      1009)
  (UNSPEC_ARC_SIMD_VMULAW     1010)
  (UNSPEC_ARC_SIMD_VMULFAW    1011)
  (UNSPEC_ARC_SIMD_VMULFW     1012)
  (UNSPEC_ARC_SIMD_VMULW      1013)
  (UNSPEC_ARC_SIMD_VSUBAW     1014)
  (UNSPEC_ARC_SIMD_VSUBW      1015)
  (UNSPEC_ARC_SIMD_VSUMMW     1016)
  (UNSPEC_ARC_SIMD_VAND       1017)
  (UNSPEC_ARC_SIMD_VANDAW     1018)
  (UNSPEC_ARC_SIMD_VBIC       1019)
  (UNSPEC_ARC_SIMD_VBICAW     1020)
  (UNSPEC_ARC_SIMD_VOR        1021)
  (UNSPEC_ARC_SIMD_VXOR       1022)
  (UNSPEC_ARC_SIMD_VXORAW     1023)
  (UNSPEC_ARC_SIMD_VEQW       1024)
  (UNSPEC_ARC_SIMD_VLEW       1025)
  (UNSPEC_ARC_SIMD_VLTW       1026)
  (UNSPEC_ARC_SIMD_VNEW       1027)
  (UNSPEC_ARC_SIMD_VMR1AW     1028)
  (UNSPEC_ARC_SIMD_VMR1W      1029)
  (UNSPEC_ARC_SIMD_VMR2AW     1030)
  (UNSPEC_ARC_SIMD_VMR2W      1031)
  (UNSPEC_ARC_SIMD_VMR3AW     1032)
  (UNSPEC_ARC_SIMD_VMR3W      1033)
  (UNSPEC_ARC_SIMD_VMR4AW     1034)
  (UNSPEC_ARC_SIMD_VMR4W      1035)
  (UNSPEC_ARC_SIMD_VMR5AW     1036)
  (UNSPEC_ARC_SIMD_VMR5W      1037)
  (UNSPEC_ARC_SIMD_VMR6AW     1038)
  (UNSPEC_ARC_SIMD_VMR6W      1039)
  (UNSPEC_ARC_SIMD_VMR7AW     1040)
  (UNSPEC_ARC_SIMD_VMR7W      1041)
  (UNSPEC_ARC_SIMD_VMRB       1042)
  (UNSPEC_ARC_SIMD_VH264F     1043)
  (UNSPEC_ARC_SIMD_VH264FT    1044)
  (UNSPEC_ARC_SIMD_VH264FW    1045)
  (UNSPEC_ARC_SIMD_VVC1F      1046)
  (UNSPEC_ARC_SIMD_VVC1FT     1047)
  ;; Va, Vb, rc/limm builtins
  (UNSPEC_ARC_SIMD_VBADDW     1050)
  (UNSPEC_ARC_SIMD_VBMAXW     1051)
  (UNSPEC_ARC_SIMD_VBMINW     1052)
  (UNSPEC_ARC_SIMD_VBMULAW    1053)
  (UNSPEC_ARC_SIMD_VBMULFW    1054)
  (UNSPEC_ARC_SIMD_VBMULW     1055)
  (UNSPEC_ARC_SIMD_VBRSUBW    1056)
  (UNSPEC_ARC_SIMD_VBSUBW     1057)

  ;; Va, Vb, Ic builtins
  (UNSPEC_ARC_SIMD_VASRW      1060)
  (UNSPEC_ARC_SIMD_VSR8       1061)
  (UNSPEC_ARC_SIMD_VSR8AW     1062)

  ;; Va, Vb, Ic builtins
  (UNSPEC_ARC_SIMD_VASRRWi    1065)
  (UNSPEC_ARC_SIMD_VASRSRWi   1066)
  (UNSPEC_ARC_SIMD_VASRWi     1067)
  (UNSPEC_ARC_SIMD_VASRPWBi   1068)
  (UNSPEC_ARC_SIMD_VASRRPWBi  1069)
  (UNSPEC_ARC_SIMD_VSR8AWi    1070)
  (UNSPEC_ARC_SIMD_VSR8i      1071)

  ;; Va, Vb, u8 (simm) builtins
  (UNSPEC_ARC_SIMD_VMVAW      1075)
  (UNSPEC_ARC_SIMD_VMVW       1076)
  (UNSPEC_ARC_SIMD_VMVZW      1077)
  (UNSPEC_ARC_SIMD_VD6TAPF    1078)

  ;; Va, rlimm, u8 (simm) builtins
  (UNSPEC_ARC_SIMD_VMOVAW     1080)
  (UNSPEC_ARC_SIMD_VMOVW      1081)
  (UNSPEC_ARC_SIMD_VMOVZW     1082)

  ;; Va, Vb builtins
  (UNSPEC_ARC_SIMD_VABSAW     1085)
  (UNSPEC_ARC_SIMD_VABSW      1086)
  (UNSPEC_ARC_SIMD_VADDSUW    1087)
  (UNSPEC_ARC_SIMD_VSIGNW     1088)
  (UNSPEC_ARC_SIMD_VEXCH1     1089)
  (UNSPEC_ARC_SIMD_VEXCH2     1090)
  (UNSPEC_ARC_SIMD_VEXCH4     1091)
  (UNSPEC_ARC_SIMD_VUPBAW     1092)
  (UNSPEC_ARC_SIMD_VUPBW      1093)
  (UNSPEC_ARC_SIMD_VUPSBAW    1094)
  (UNSPEC_ARC_SIMD_VUPSBW     1095)

  (UNSPEC_ARC_SIMD_VDIRUN     1100)
  (UNSPEC_ARC_SIMD_VDORUN     1101)
  (UNSPEC_ARC_SIMD_VDIWR      1102)
  (UNSPEC_ARC_SIMD_VDOWR      1103)

  (UNSPEC_ARC_SIMD_VREC      1105)
  (UNSPEC_ARC_SIMD_VRUN      1106)
  (UNSPEC_ARC_SIMD_VRECRUN   1107)
  (UNSPEC_ARC_SIMD_VENDREC   1108)

  (UNSPEC_ARC_SIMD_VCAST     1200)
  (UNSPEC_ARC_SIMD_VINTI     1201)
   ]
)

;; Scheduler descriptions for the simd instructions
(define_insn_reservation "simd_lat_0_insn" 1
  (eq_attr "type" "simd_dma, simd_vstore, simd_vcontrol")
  "issue+simd_unit")

(define_insn_reservation "simd_lat_1_insn" 2
       (eq_attr "type" "simd_vcompare, simd_vlogic,
                        simd_vmove_else_zero, simd_varith_1cycle")
  "issue+simd_unit, nothing")

(define_insn_reservation "simd_lat_2_insn" 3
       (eq_attr "type" "simd_valign, simd_vpermute,
                        simd_vpack, simd_varith_2cycle")
  "issue+simd_unit, nothing*2")

(define_insn_reservation "simd_lat_3_insn" 4
       (eq_attr "type" "simd_valign_with_acc, simd_vpack_with_acc,
                        simd_vlogic_with_acc, simd_vload128,
                        simd_vmove_with_acc, simd_vspecial_3cycle,
                        simd_varith_with_acc")
  "issue+simd_unit, nothing*3")

(define_insn_reservation "simd_lat_4_insn" 5
       (eq_attr "type" "simd_vload, simd_vmove, simd_vspecial_4cycle")
  "issue+simd_unit, nothing*4")

(define_expand "movv8hi"
  [(set (match_operand:V8HI 0 "general_operand" "")
	(match_operand:V8HI 1 "general_operand" ""))]
  ""
  "
{
  /* Everything except mem = const or mem = mem can be done easily.  */

  if (GET_CODE (operands[0]) == MEM && GET_CODE(operands[1]) == MEM)
    operands[1] = force_reg (V8HImode, operands[1]);
}")

;; This pattern should appear before the movv8hi_insn pattern
(define_insn "vld128_insn"
  [(set (match_operand:V8HI 0 "vector_register_operand" "=v")
	(mem:V8HI (plus:SI (zero_extend:SI (vec_select:HI (match_operand:V8HI 1 "vector_register_operand"  "v")
							  (parallel [(match_operand:SI 2 "immediate_operand" "L")])))
			   (match_operand:SI 3 "immediate_operand" "P"))))]
 "TARGET_SIMD_SET"
 "vld128 %0, [i%2, %3]"
 [(set_attr "type" "simd_vload128")
  (set_attr "length" "4")
  (set_attr "cond" "nocond")]
)

(define_insn "vst128_insn"
  [(set	(mem:V8HI (plus:SI (zero_extend:SI (vec_select:HI (match_operand:V8HI 0 "vector_register_operand"  "v")
							  (parallel [(match_operand:SI 1 "immediate_operand" "L")])))
			   (match_operand:SI 2 "immediate_operand" "P")))
	(match_operand:V8HI 3 "vector_register_operand" "=v"))]
 "TARGET_SIMD_SET"
 "vst128 %3, [i%1, %2]"
 [(set_attr "type" "simd_vstore")
  (set_attr "length" "4")
  (set_attr "cond" "nocond")]
)

(define_insn "vst64_insn"
  [(set	(mem:V4HI (plus:SI (zero_extend:SI (vec_select:HI (match_operand:V8HI 0 "vector_register_operand"  "v")
							  (parallel [(match_operand:SI 1 "immediate_operand" "L")])))
			   (match_operand:SI 2 "immediate_operand" "P")))
	(vec_select:V4HI (match_operand:V8HI 3 "vector_register_operand" "=v")
			 (parallel [(const_int 0)])))]
 "TARGET_SIMD_SET"
 "vst64 %3, [i%1, %2]"
 [(set_attr "type" "simd_vstore")
  (set_attr "length" "4")
  (set_attr "cond" "nocond")]
)

(define_insn "movv8hi_insn"
  [(set (match_operand:V8HI 0 "vector_register_or_memory_operand" "=v,m,v")
	(match_operand:V8HI 1 "vector_register_or_memory_operand" "m,v,v"))]
  "TARGET_SIMD_SET && !(GET_CODE (operands[0]) == MEM && GET_CODE(operands[1]) == MEM)"
  "@
    vld128r %0, %1
    vst128r %1, %0
    vmvzw %0,%1,0xffff"
  [(set_attr "type" "simd_vload128,simd_vstore,simd_vmove_else_zero")
   (set_attr "length" "8,8,4")
   (set_attr "cond" "nocond, nocond, nocond")])

(define_insn "movti_insn"
  [(set (match_operand:TI 0 "vector_register_or_memory_operand" "=v,m,v")
	(match_operand:TI 1 "vector_register_or_memory_operand" "m,v,v"))]
  ""
  "@
    vld128r %0, %1
    vst128r %1, %0
    vmvzw %0,%1,0xffff"
  [(set_attr "type" "simd_vload128,simd_vstore,simd_vmove_else_zero")
   (set_attr "length" "8,8,4")
   (set_attr "cond" "nocond, nocond, nocond")])

;; (define_insn "*movv8hi_insn_rr"
;;   [(set (match_operand:V8HI 0 "vector_register_operand" "=v")
;; 	(match_operand:V8HI 1 "vector_register_operand" "v"))]
;;   ""
;;   "mov reg,reg"
;;   [(set_attr "length" "8")
;;   (set_attr "type" "move")])

;; (define_insn "*movv8_out"
;;   [(set (match_operand:V8HI 0 "memory_operand" "=m")
;; 	(match_operand:V8HI 1 "vector_register_operand" "v"))]
;;   ""
;;   "mov out"
;;   [(set_attr "length" "8")
;;   (set_attr "type" "move")])


;; (define_insn "addv8hi3"
;;   [(set (match_operand:V8HI 0 "vector_register_operand"           "=v")
;; 	(plus:V8HI (match_operand:V8HI 1 "vector_register_operand"  "v")
;; 		   (match_operand:V8HI 2 "vector_register_operand" "v")))]
;;   "TARGET_SIMD_SET"
;;   "vaddw %0, %1, %2"
;;   [(set_attr "length" "8")
;;    (set_attr "cond" "nocond")])

;; (define_insn "vaddw_insn"
;;   [(set (match_operand:V8HI 0 "vector_register_operand"           "=v")
;; 	(unspec [(match_operand:V8HI 1 "vector_register_operand"  "v")
;; 			 (match_operand:V8HI 2 "vector_register_operand" "v")] UNSPEC_ARC_SIMD_VADDW))]
;;   "TARGET_SIMD_SET"
;;   "vaddw %0, %1, %2"
;;   [(set_attr "length" "8")
;;    (set_attr "cond" "nocond")])

;; V V V Insns
(define_insn "vaddaw_insn"
  [(set (match_operand:V8HI 0 "vector_register_operand"           "=v")
	(unspec:V8HI [(match_operand:V8HI 1 "vector_register_operand"  "v")
			 (match_operand:V8HI 2 "vector_register_operand" "v")] UNSPEC_ARC_SIMD_VADDAW))]
  "TARGET_SIMD_SET"
  "vaddaw %0, %1, %2"
  [(set_attr "type" "simd_varith_with_acc")
   (set_attr "length" "4")
   (set_attr "cond" "nocond")])

(define_insn "vaddw_insn"
  [(set (match_operand:V8HI 0 "vector_register_operand"           "=v")
	(unspec:V8HI [(match_operand:V8HI 1 "vector_register_operand"  "v")
			 (match_operand:V8HI 2 "vector_register_operand" "v")] UNSPEC_ARC_SIMD_VADDW))]
  "TARGET_SIMD_SET"
  "vaddw %0, %1, %2"
  [(set_attr "type" "simd_varith_1cycle")
   (set_attr "length" "4")
   (set_attr "cond" "nocond")])

(define_insn "vavb_insn"
  [(set (match_operand:V8HI 0 "vector_register_operand"           "=v")
	(unspec:V8HI [(match_operand:V8HI 1 "vector_register_operand"  "v")
			 (match_operand:V8HI 2 "vector_register_operand" "v")] UNSPEC_ARC_SIMD_VAVB))]
  "TARGET_SIMD_SET"
  "vavb %0, %1, %2"
  [(set_attr "type" "simd_varith_1cycle")
   (set_attr "length" "4")
   (set_attr "cond" "nocond")])

(define_insn "vavrb_insn"
  [(set (match_operand:V8HI 0 "vector_register_operand"           "=v")
	(unspec:V8HI [(match_operand:V8HI 1 "vector_register_operand"  "v")
			 (match_operand:V8HI 2 "vector_register_operand" "v")] UNSPEC_ARC_SIMD_VAVRB))]
  "TARGET_SIMD_SET"
  "vavrb %0, %1, %2"
  [(set_attr "type" "simd_varith_1cycle")
   (set_attr "length" "4")
   (set_attr "cond" "nocond")])

(define_insn "vdifaw_insn"
  [(set (match_operand:V8HI 0 "vector_register_operand"           "=v")
	(unspec:V8HI [(match_operand:V8HI 1 "vector_register_operand"  "v")
			 (match_operand:V8HI 2 "vector_register_operand" "v")] UNSPEC_ARC_SIMD_VDIFAW))]
  "TARGET_SIMD_SET"
  "vdifaw %0, %1, %2"
  [(set_attr "type" "simd_varith_with_acc")
   (set_attr "length" "4")
   (set_attr "cond" "nocond")])

(define_insn "vdifw_insn"
  [(set (match_operand:V8HI 0 "vector_register_operand"           "=v")
	(unspec:V8HI [(match_operand:V8HI 1 "vector_register_operand"  "v")
			 (match_operand:V8HI 2 "vector_register_operand" "v")] UNSPEC_ARC_SIMD_VDIFW))]
  "TARGET_SIMD_SET"
  "vdifw %0, %1, %2"
  [(set_attr "type" "simd_varith_1cycle")
   (set_attr "length" "4")
   (set_attr "cond" "nocond")])

(define_insn "vmaxaw_insn"
  [(set (match_operand:V8HI 0 "vector_register_operand"           "=v")
	(unspec:V8HI [(match_operand:V8HI 1 "vector_register_operand"  "v")
			 (match_operand:V8HI 2 "vector_register_operand" "v")] UNSPEC_ARC_SIMD_VMAXAW))]
  "TARGET_SIMD_SET"
  "vmaxaw %0, %1, %2"
  [(set_attr "type" "simd_varith_with_acc")
   (set_attr "length" "4")
   (set_attr "cond" "nocond")])

(define_insn "vmaxw_insn"
  [(set (match_operand:V8HI 0 "vector_register_operand"           "=v")
	(unspec:V8HI [(match_operand:V8HI 1 "vector_register_operand"  "v")
			 (match_operand:V8HI 2 "vector_register_operand" "v")] UNSPEC_ARC_SIMD_VMAXW))]
  "TARGET_SIMD_SET"
  "vmaxw %0, %1, %2"
  [(set_attr "type" "simd_varith_1cycle")
   (set_attr "length" "4")
   (set_attr "cond" "nocond")])

(define_insn "vminaw_insn"
  [(set (match_operand:V8HI 0 "vector_register_operand"           "=v")
	(unspec:V8HI [(match_operand:V8HI 1 "vector_register_operand"  "v")
			 (match_operand:V8HI 2 "vector_register_operand" "v")] UNSPEC_ARC_SIMD_VMINAW))]
  "TARGET_SIMD_SET"
  "vminaw %0, %1, %2"
  [(set_attr "type" "simd_varith_with_acc")
   (set_attr "length" "4")
   (set_attr "cond" "nocond")])

(define_insn "vminw_insn"
  [(set (match_operand:V8HI 0 "vector_register_operand"           "=v")
	(unspec:V8HI [(match_operand:V8HI 1 "vector_register_operand"  "v")
			 (match_operand:V8HI 2 "vector_register_operand" "v")] UNSPEC_ARC_SIMD_VMINW))]
  "TARGET_SIMD_SET"
  "vminw %0, %1, %2"
  [(set_attr "type" "simd_varith_1cycle")
   (set_attr "length" "4")
   (set_attr "cond" "nocond")])

(define_insn "vmulaw_insn"
  [(set (match_operand:V8HI 0 "vector_register_operand"           "=v")
	(unspec:V8HI [(match_operand:V8HI 1 "vector_register_operand"  "v")
			 (match_operand:V8HI 2 "vector_register_operand" "v")] UNSPEC_ARC_SIMD_VMULAW))]
  "TARGET_SIMD_SET"
  "vmulaw %0, %1, %2"
  [(set_attr "type" "simd_varith_with_acc")
   (set_attr "length" "4")
   (set_attr "cond" "nocond")])

(define_insn "vmulfaw_insn"
  [(set (match_operand:V8HI 0 "vector_register_operand"           "=v")
	(unspec:V8HI [(match_operand:V8HI 1 "vector_register_operand"  "v")
			 (match_operand:V8HI 2 "vector_register_operand" "v")] UNSPEC_ARC_SIMD_VMULFAW))]
  "TARGET_SIMD_SET"
  "vmulfaw %0, %1, %2"
  [(set_attr "type" "simd_varith_with_acc")
   (set_attr "length" "4")
   (set_attr "cond" "nocond")])

(define_insn "vmulfw_insn"
  [(set (match_operand:V8HI 0 "vector_register_operand"           "=v")
	(unspec:V8HI [(match_operand:V8HI 1 "vector_register_operand"  "v")
			 (match_operand:V8HI 2 "vector_register_operand" "v")] UNSPEC_ARC_SIMD_VMULFW))]
  "TARGET_SIMD_SET"
  "vmulfw %0, %1, %2"
  [(set_attr "type" "simd_varith_2cycle")
   (set_attr "length" "4")
   (set_attr "cond" "nocond")])

(define_insn "vmulw_insn"
  [(set (match_operand:V8HI 0 "vector_register_operand"           "=v")
	(unspec:V8HI [(match_operand:V8HI 1 "vector_register_operand"  "v")
			 (match_operand:V8HI 2 "vector_register_operand" "v")] UNSPEC_ARC_SIMD_VMULW))]
  "TARGET_SIMD_SET"
  "vmulw %0, %1, %2"
  [(set_attr "type" "simd_varith_2cycle")
   (set_attr "length" "4")
   (set_attr "cond" "nocond")])

(define_insn "vsubaw_insn"
  [(set (match_operand:V8HI 0 "vector_register_operand"           "=v")
	(unspec:V8HI [(match_operand:V8HI 1 "vector_register_operand"  "v")
			 (match_operand:V8HI 2 "vector_register_operand" "v")] UNSPEC_ARC_SIMD_VSUBAW))]
  "TARGET_SIMD_SET"
  "vsubaw %0, %1, %2"
  [(set_attr "type" "simd_varith_with_acc")
   (set_attr "length" "4")
   (set_attr "cond" "nocond")])

(define_insn "vsubw_insn"
  [(set (match_operand:V8HI 0 "vector_register_operand"           "=v")
	(unspec:V8HI [(match_operand:V8HI 1 "vector_register_operand"  "v")
			 (match_operand:V8HI 2 "vector_register_operand" "v")] UNSPEC_ARC_SIMD_VSUBW))]
  "TARGET_SIMD_SET"
  "vsubw %0, %1, %2"
  [(set_attr "type" "simd_varith_1cycle")
   (set_attr "length" "4")
   (set_attr "cond" "nocond")])

(define_insn "vsummw_insn"
  [(set (match_operand:V8HI 0 "vector_register_operand"           "=v")
	(unspec:V8HI [(match_operand:V8HI 1 "vector_register_operand"  "v")
			 (match_operand:V8HI 2 "vector_register_operand" "v")] UNSPEC_ARC_SIMD_VSUMMW))]
  "TARGET_SIMD_SET"
  "vsummw %0, %1, %2"
  [(set_attr "type" "simd_varith_2cycle")
   (set_attr "length" "4")
   (set_attr "cond" "nocond")])

(define_insn "vand_insn"
  [(set (match_operand:V8HI 0 "vector_register_operand"           "=v")
	(unspec:V8HI [(match_operand:V8HI 1 "vector_register_operand"  "v")
			 (match_operand:V8HI 2 "vector_register_operand" "v")] UNSPEC_ARC_SIMD_VAND))]
  "TARGET_SIMD_SET"
  "vand %0, %1, %2"
  [(set_attr "type" "simd_vlogic")
   (set_attr "length" "4")
   (set_attr "cond" "nocond")])

(define_insn "vandaw_insn"
  [(set (match_operand:V8HI 0 "vector_register_operand"           "=v")
	(unspec:V8HI [(match_operand:V8HI 1 "vector_register_operand"  "v")
			 (match_operand:V8HI 2 "vector_register_operand" "v")] UNSPEC_ARC_SIMD_VANDAW))]
  "TARGET_SIMD_SET"
  "vandaw %0, %1, %2"
  [(set_attr "type" "simd_vlogic_with_acc")
   (set_attr "length" "4")
   (set_attr "cond" "nocond")])

(define_insn "vbic_insn"
  [(set (match_operand:V8HI 0 "vector_register_operand"           "=v")
	(unspec:V8HI [(match_operand:V8HI 1 "vector_register_operand"  "v")
			 (match_operand:V8HI 2 "vector_register_operand" "v")] UNSPEC_ARC_SIMD_VBIC))]
  "TARGET_SIMD_SET"
  "vbic %0, %1, %2"
  [(set_attr "type" "simd_vlogic")
   (set_attr "length" "4")
   (set_attr "cond" "nocond")])

(define_insn "vbicaw_insn"
  [(set (match_operand:V8HI 0 "vector_register_operand"           "=v")
	(unspec:V8HI [(match_operand:V8HI 1 "vector_register_operand"  "v")
			 (match_operand:V8HI 2 "vector_register_operand" "v")] UNSPEC_ARC_SIMD_VBICAW))]
  "TARGET_SIMD_SET"
  "vbicaw %0, %1, %2"
  [(set_attr "type" "simd_vlogic_with_acc")
   (set_attr "length" "4")
   (set_attr "cond" "nocond")])

(define_insn "vor_insn"
  [(set (match_operand:V8HI 0 "vector_register_operand"           "=v")
	(unspec:V8HI [(match_operand:V8HI 1 "vector_register_operand"  "v")
			 (match_operand:V8HI 2 "vector_register_operand" "v")] UNSPEC_ARC_SIMD_VOR))]
  "TARGET_SIMD_SET"
  "vor %0, %1, %2"
  [(set_attr "type" "simd_vlogic")
   (set_attr "length" "4")
   (set_attr "cond" "nocond")])

(define_insn "vxor_insn"
  [(set (match_operand:V8HI 0 "vector_register_operand"           "=v")
	(unspec:V8HI [(match_operand:V8HI 1 "vector_register_operand"  "v")
			 (match_operand:V8HI 2 "vector_register_operand" "v")] UNSPEC_ARC_SIMD_VXOR))]
  "TARGET_SIMD_SET"
  "vxor %0, %1, %2"
  [(set_attr "type" "simd_vlogic")
   (set_attr "length" "4")
   (set_attr "cond" "nocond")])

(define_insn "vxoraw_insn"
  [(set (match_operand:V8HI 0 "vector_register_operand"           "=v")
	(unspec:V8HI [(match_operand:V8HI 1 "vector_register_operand"  "v")
			 (match_operand:V8HI 2 "vector_register_operand" "v")] UNSPEC_ARC_SIMD_VXORAW))]
  "TARGET_SIMD_SET"
  "vxoraw %0, %1, %2"
  [(set_attr "type" "simd_vlogic_with_acc")
   (set_attr "length" "4")
   (set_attr "cond" "nocond")])

(define_insn "veqw_insn"
  [(set (match_operand:V8HI 0 "vector_register_operand"           "=v")
	(unspec:V8HI [(match_operand:V8HI 1 "vector_register_operand"  "v")
			 (match_operand:V8HI 2 "vector_register_operand" "v")] UNSPEC_ARC_SIMD_VEQW))]
  "TARGET_SIMD_SET"
  "veqw %0, %1, %2"
  [(set_attr "type" "simd_vcompare")
   (set_attr "length" "4")
   (set_attr "cond" "nocond")])

(define_insn "vlew_insn"
  [(set (match_operand:V8HI 0 "vector_register_operand"           "=v")
	(unspec:V8HI [(match_operand:V8HI 1 "vector_register_operand"  "v")
			 (match_operand:V8HI 2 "vector_register_operand" "v")] UNSPEC_ARC_SIMD_VLEW))]
  "TARGET_SIMD_SET"
  "vlew %0, %1, %2"
  [(set_attr "type" "simd_vcompare")
   (set_attr "length" "4")
   (set_attr "cond" "nocond")])

(define_insn "vltw_insn"
  [(set (match_operand:V8HI 0 "vector_register_operand"           "=v")
	(unspec:V8HI [(match_operand:V8HI 1 "vector_register_operand"  "v")
			 (match_operand:V8HI 2 "vector_register_operand" "v")] UNSPEC_ARC_SIMD_VLTW))]
  "TARGET_SIMD_SET"
  "vltw %0, %1, %2"
  [(set_attr "type" "simd_vcompare")
   (set_attr "length" "4")
   (set_attr "cond" "nocond")])

(define_insn "vnew_insn"
  [(set (match_operand:V8HI 0 "vector_register_operand"           "=v")
	(unspec:V8HI [(match_operand:V8HI 1 "vector_register_operand"  "v")
			 (match_operand:V8HI 2 "vector_register_operand" "v")] UNSPEC_ARC_SIMD_VNEW))]
  "TARGET_SIMD_SET"
  "vnew %0, %1, %2"
  [(set_attr "type" "simd_vcompare")
   (set_attr "length" "4")
   (set_attr "cond" "nocond")])

(define_insn "vmr1aw_insn"
  [(set (match_operand:V8HI 0 "vector_register_operand"           "=v")
	(unspec:V8HI [(match_operand:V8HI 1 "vector_register_operand"  "v")
			 (match_operand:V8HI 2 "vector_register_operand" "v")] UNSPEC_ARC_SIMD_VMR1AW))]
  "TARGET_SIMD_SET"
  "vmr1aw %0, %1, %2"
  [(set_attr "type" "simd_valign_with_acc")
   (set_attr "length" "4")
   (set_attr "cond" "nocond")])

(define_insn "vmr1w_insn"
  [(set (match_operand:V8HI 0 "vector_register_operand"           "=v")
	(unspec:V8HI [(match_operand:V8HI 1 "vector_register_operand"  "v")
			 (match_operand:V8HI 2 "vector_register_operand" "v")] UNSPEC_ARC_SIMD_VMR1W))]
  "TARGET_SIMD_SET"
  "vmr1w %0, %1, %2"
  [(set_attr "type" "simd_valign")
   (set_attr "length" "4")
   (set_attr "cond" "nocond")])

(define_insn "vmr2aw_insn"
  [(set (match_operand:V8HI 0 "vector_register_operand"           "=v")
	(unspec:V8HI [(match_operand:V8HI 1 "vector_register_operand"  "v")
			 (match_operand:V8HI 2 "vector_register_operand" "v")] UNSPEC_ARC_SIMD_VMR2AW))]
  "TARGET_SIMD_SET"
  "vmr2aw %0, %1, %2"
  [(set_attr "type" "simd_valign_with_acc")
   (set_attr "length" "4")
   (set_attr "cond" "nocond")])

(define_insn "vmr2w_insn"
  [(set (match_operand:V8HI 0 "vector_register_operand"           "=v")
	(unspec:V8HI [(match_operand:V8HI 1 "vector_register_operand"  "v")
			 (match_operand:V8HI 2 "vector_register_operand" "v")] UNSPEC_ARC_SIMD_VMR2W))]
  "TARGET_SIMD_SET"
  "vmr2w %0, %1, %2"
  [(set_attr "type" "simd_valign")
   (set_attr "length" "4")
   (set_attr "cond" "nocond")])

(define_insn "vmr3aw_insn"
  [(set (match_operand:V8HI 0 "vector_register_operand"           "=v")
	(unspec:V8HI [(match_operand:V8HI 1 "vector_register_operand"  "v")
			 (match_operand:V8HI 2 "vector_register_operand" "v")] UNSPEC_ARC_SIMD_VMR3AW))]
  "TARGET_SIMD_SET"
  "vmr3aw %0, %1, %2"
  [(set_attr "type" "simd_valign_with_acc")
   (set_attr "length" "4")
   (set_attr "cond" "nocond")])

(define_insn "vmr3w_insn"
  [(set (match_operand:V8HI 0 "vector_register_operand"           "=v")
	(unspec:V8HI [(match_operand:V8HI 1 "vector_register_operand"  "v")
			 (match_operand:V8HI 2 "vector_register_operand" "v")] UNSPEC_ARC_SIMD_VMR3W))]
  "TARGET_SIMD_SET"
  "vmr3w %0, %1, %2"
  [(set_attr "type" "simd_valign")
   (set_attr "length" "4")
   (set_attr "cond" "nocond")])

(define_insn "vmr4aw_insn"
  [(set (match_operand:V8HI 0 "vector_register_operand"           "=v")
	(unspec:V8HI [(match_operand:V8HI 1 "vector_register_operand"  "v")
			 (match_operand:V8HI 2 "vector_register_operand" "v")] UNSPEC_ARC_SIMD_VMR4AW))]
  "TARGET_SIMD_SET"
  "vmr4aw %0, %1, %2"
  [(set_attr "type" "simd_valign_with_acc")
   (set_attr "length" "4")
   (set_attr "cond" "nocond")])

(define_insn "vmr4w_insn"
  [(set (match_operand:V8HI 0 "vector_register_operand"           "=v")
	(unspec:V8HI [(match_operand:V8HI 1 "vector_register_operand"  "v")
			 (match_operand:V8HI 2 "vector_register_operand" "v")] UNSPEC_ARC_SIMD_VMR4W))]
  "TARGET_SIMD_SET"
  "vmr4w %0, %1, %2"
  [(set_attr "type" "simd_valign")
   (set_attr "length" "4")
   (set_attr "cond" "nocond")])

(define_insn "vmr5aw_insn"
  [(set (match_operand:V8HI 0 "vector_register_operand"           "=v")
	(unspec:V8HI [(match_operand:V8HI 1 "vector_register_operand"  "v")
			 (match_operand:V8HI 2 "vector_register_operand" "v")] UNSPEC_ARC_SIMD_VMR5AW))]
  "TARGET_SIMD_SET"
  "vmr5aw %0, %1, %2"
  [(set_attr "type" "simd_valign_with_acc")
   (set_attr "length" "4")
   (set_attr "cond" "nocond")])

(define_insn "vmr5w_insn"
  [(set (match_operand:V8HI 0 "vector_register_operand"           "=v")
	(unspec:V8HI [(match_operand:V8HI 1 "vector_register_operand"  "v")
			 (match_operand:V8HI 2 "vector_register_operand" "v")] UNSPEC_ARC_SIMD_VMR5W))]
  "TARGET_SIMD_SET"
  "vmr5w %0, %1, %2"
  [(set_attr "type" "simd_valign")
   (set_attr "length" "4")
   (set_attr "cond" "nocond")])

(define_insn "vmr6aw_insn"
  [(set (match_operand:V8HI 0 "vector_register_operand"           "=v")
	(unspec:V8HI [(match_operand:V8HI 1 "vector_register_operand"  "v")
			 (match_operand:V8HI 2 "vector_register_operand" "v")] UNSPEC_ARC_SIMD_VMR6AW))]
  "TARGET_SIMD_SET"
  "vmr6aw %0, %1, %2"
  [(set_attr "type" "simd_valign_with_acc")
   (set_attr "length" "4")
   (set_attr "cond" "nocond")])

(define_insn "vmr6w_insn"
  [(set (match_operand:V8HI 0 "vector_register_operand"           "=v")
	(unspec:V8HI [(match_operand:V8HI 1 "vector_register_operand"  "v")
			 (match_operand:V8HI 2 "vector_register_operand" "v")] UNSPEC_ARC_SIMD_VMR6W))]
  "TARGET_SIMD_SET"
  "vmr6w %0, %1, %2"
  [(set_attr "type" "simd_valign")
   (set_attr "length" "4")
   (set_attr "cond" "nocond")])

(define_insn "vmr7aw_insn"
  [(set (match_operand:V8HI 0 "vector_register_operand"           "=v")
	(unspec:V8HI [(match_operand:V8HI 1 "vector_register_operand"  "v")
			 (match_operand:V8HI 2 "vector_register_operand" "v")] UNSPEC_ARC_SIMD_VMR7AW))]
  "TARGET_SIMD_SET"
  "vmr7aw %0, %1, %2"
  [(set_attr "type" "simd_valign_with_acc")
   (set_attr "length" "4")
   (set_attr "cond" "nocond")])

(define_insn "vmr7w_insn"
  [(set (match_operand:V8HI 0 "vector_register_operand"           "=v")
	(unspec:V8HI [(match_operand:V8HI 1 "vector_register_operand"  "v")
			 (match_operand:V8HI 2 "vector_register_operand" "v")] UNSPEC_ARC_SIMD_VMR7W))]
  "TARGET_SIMD_SET"
  "vmr7w %0, %1, %2"
  [(set_attr "type" "simd_valign")
   (set_attr "length" "4")
   (set_attr "cond" "nocond")])

(define_insn "vmrb_insn"
  [(set (match_operand:V8HI 0 "vector_register_operand"           "=v")
	(unspec:V8HI [(match_operand:V8HI 1 "vector_register_operand"  "v")
			 (match_operand:V8HI 2 "vector_register_operand" "v")] UNSPEC_ARC_SIMD_VMRB))]
  "TARGET_SIMD_SET"
  "vmrb %0, %1, %2"
  [(set_attr "type" "simd_valign")
   (set_attr "length" "4")
   (set_attr "cond" "nocond")])

(define_insn "vh264f_insn"
  [(set (match_operand:V8HI 0 "vector_register_operand"           "=v")
	(unspec:V8HI [(match_operand:V8HI 1 "vector_register_operand"  "v")
			 (match_operand:V8HI 2 "vector_register_operand" "v")] UNSPEC_ARC_SIMD_VH264F))]
  "TARGET_SIMD_SET"
  "vh264f %0, %1, %2"
  [(set_attr "type" "simd_vspecial_3cycle")
   (set_attr "length" "4")
   (set_attr "cond" "nocond")])

(define_insn "vh264ft_insn"
  [(set (match_operand:V8HI 0 "vector_register_operand"           "=v")
	(unspec:V8HI [(match_operand:V8HI 1 "vector_register_operand"  "v")
			 (match_operand:V8HI 2 "vector_register_operand" "v")] UNSPEC_ARC_SIMD_VH264FT))]
  "TARGET_SIMD_SET"
  "vh264ft %0, %1, %2"
  [(set_attr "type" "simd_vspecial_3cycle")
   (set_attr "length" "4")
   (set_attr "cond" "nocond")])

(define_insn "vh264fw_insn"
  [(set (match_operand:V8HI 0 "vector_register_operand"           "=v")
	(unspec:V8HI [(match_operand:V8HI 1 "vector_register_operand"  "v")
			 (match_operand:V8HI 2 "vector_register_operand" "v")] UNSPEC_ARC_SIMD_VH264FW))]
  "TARGET_SIMD_SET"
  "vh264fw %0, %1, %2"
  [(set_attr "type" "simd_vspecial_3cycle")
   (set_attr "length" "4")
   (set_attr "cond" "nocond")])

(define_insn "vvc1f_insn"
  [(set (match_operand:V8HI 0 "vector_register_operand"           "=v")
	(unspec:V8HI [(match_operand:V8HI 1 "vector_register_operand"  "v")
			 (match_operand:V8HI 2 "vector_register_operand" "v")] UNSPEC_ARC_SIMD_VVC1F))]
  "TARGET_SIMD_SET"
  "vvc1f %0, %1, %2"
  [(set_attr "type" "simd_vspecial_3cycle")
   (set_attr "length" "4")
   (set_attr "cond" "nocond")])

(define_insn "vvc1ft_insn"
  [(set (match_operand:V8HI 0 "vector_register_operand"           "=v")
	(unspec:V8HI [(match_operand:V8HI 1 "vector_register_operand"  "v")
			 (match_operand:V8HI 2 "vector_register_operand" "v")] UNSPEC_ARC_SIMD_VVC1FT))]
  "TARGET_SIMD_SET"
  "vvc1ft %0, %1, %2"
  [(set_attr "type" "simd_vspecial_3cycle")
   (set_attr "length" "4")
   (set_attr "cond" "nocond")])



;;---
;; V V r/limm Insns

;; (define_insn "vbaddw_insn"
;;   [(set (match_operand:V8HI 0 "vector_register_operand"           "=v")
;; 	(unspec:V8HI [(match_operand:V8HI 1 "vector_register_operand"  "v")
;; 			      (match_operand:SI 2 "nonmemory_operand" "rCal")] UNSPEC_ARC_SIMD_VBADDW))]
;;   "TARGET_SIMD_SET"
;;   "vbaddw %0, %1, %2"
;;   [(set_attr "length" "4")
;;    (set_attr "cond" "nocond")])

(define_insn "vbaddw_insn"
  [(set (match_operand:V8HI 0 "vector_register_operand"           "=v")
	(unspec:V8HI [(match_operand:V8HI 1 "vector_register_operand"  "v")
		      (match_operand:SI 2 "nonmemory_operand" "r")] UNSPEC_ARC_SIMD_VBADDW))]
  "TARGET_SIMD_SET"
  "vbaddw %0, %1, %2"
  [(set_attr "type" "simd_varith_1cycle")
   (set_attr "length" "4")
   (set_attr "cond" "nocond")])

(define_insn "vbmaxw_insn"
  [(set (match_operand:V8HI 0 "vector_register_operand"           "=v")
	(unspec:V8HI [(match_operand:V8HI 1 "vector_register_operand"  "v")
		      (match_operand:SI 2 "nonmemory_operand" "r")] UNSPEC_ARC_SIMD_VBMAXW))]
  "TARGET_SIMD_SET"
  "vbmaxw %0, %1, %2"
  [(set_attr "type" "simd_varith_1cycle")
   (set_attr "length" "4")
   (set_attr "cond" "nocond")])

(define_insn "vbminw_insn"
  [(set (match_operand:V8HI 0 "vector_register_operand"           "=v")
	(unspec:V8HI [(match_operand:V8HI 1 "vector_register_operand"  "v")
		      (match_operand:SI 2 "nonmemory_operand" "r")] UNSPEC_ARC_SIMD_VBMINW))]
  "TARGET_SIMD_SET"
  "vbminw %0, %1, %2"
  [(set_attr "type" "simd_varith_1cycle")
   (set_attr "length" "4")
   (set_attr "cond" "nocond")])

(define_insn "vbmulaw_insn"
  [(set (match_operand:V8HI 0 "vector_register_operand"           "=v")
	(unspec:V8HI [(match_operand:V8HI 1 "vector_register_operand"  "v")
		      (match_operand:SI 2 "nonmemory_operand" "r")] UNSPEC_ARC_SIMD_VBMULAW))]
  "TARGET_SIMD_SET"
  "vbmulaw %0, %1, %2"
  [(set_attr "type" "simd_varith_with_acc")
   (set_attr "length" "4")
   (set_attr "cond" "nocond")])

(define_insn "vbmulfw_insn"
  [(set (match_operand:V8HI 0 "vector_register_operand"           "=v")
	(unspec:V8HI [(match_operand:V8HI 1 "vector_register_operand"  "v")
		      (match_operand:SI 2 "nonmemory_operand" "r")] UNSPEC_ARC_SIMD_VBMULFW))]
  "TARGET_SIMD_SET"
  "vbmulfw %0, %1, %2"
  [(set_attr "type" "simd_varith_2cycle")
   (set_attr "length" "4")
   (set_attr "cond" "nocond")])

(define_insn "vbmulw_insn"
  [(set (match_operand:V8HI 0 "vector_register_operand"           "=v")
	(unspec:V8HI [(match_operand:V8HI 1 "vector_register_operand"  "v")
		      (match_operand:SI 2 "nonmemory_operand" "r")] UNSPEC_ARC_SIMD_VBMULW))]
  "TARGET_SIMD_SET"
  "vbmulw %0, %1, %2"
  [(set_attr "type" "simd_varith_2cycle")
   (set_attr "length" "4")
   (set_attr "cond" "nocond")])

(define_insn "vbrsubw_insn"
  [(set (match_operand:V8HI 0 "vector_register_operand"           "=v")
	(unspec:V8HI [(match_operand:V8HI 1 "vector_register_operand"  "v")
		     (match_operand:SI 2 "nonmemory_operand" "r")] UNSPEC_ARC_SIMD_VBRSUBW))]
  "TARGET_SIMD_SET"
  "vbrsubw %0, %1, %2"
  [(set_attr "type" "simd_varith_1cycle")
   (set_attr "length" "4")
   (set_attr "cond" "nocond")])

(define_insn "vbsubw_insn"
  [(set (match_operand:V8HI 0 "vector_register_operand"           "=v")
	(unspec:V8HI [(match_operand:V8HI 1 "vector_register_operand"  "v")
		      (match_operand:SI 2 "nonmemory_operand" "r")] UNSPEC_ARC_SIMD_VBSUBW))]
  "TARGET_SIMD_SET"
  "vbsubw %0, %1, %2"
  [(set_attr "type" "simd_varith_1cycle")
   (set_attr "length" "4")
   (set_attr "cond" "nocond")])
; Va, Vb, Ic instructions

; Va, Vb, u6 instructions
(define_insn "vasrrwi_insn"
  [(set (match_operand:V8HI 0 "vector_register_operand"           "=v")
	(unspec:V8HI [(match_operand:V8HI 1 "vector_register_operand"  "v")
		      (match_operand:SI 2 "immediate_operand" "L")] UNSPEC_ARC_SIMD_VASRRWi))]
  "TARGET_SIMD_SET"
  "vasrrwi %0, %1, %2"
  [(set_attr "type" "simd_varith_2cycle")
   (set_attr "length" "4")
   (set_attr "cond" "nocond")])

(define_insn "vasrsrwi_insn"
  [(set (match_operand:V8HI 0 "vector_register_operand"           "=v")
	(unspec:V8HI [(match_operand:V8HI 1 "vector_register_operand"  "v")
		     (match_operand:SI 2 "immediate_operand" "L")] UNSPEC_ARC_SIMD_VASRSRWi))]
  "TARGET_SIMD_SET"
  "vasrsrwi %0, %1, %2"
  [(set_attr "type" "simd_varith_2cycle")
   (set_attr "length" "4")
   (set_attr "cond" "nocond")])

(define_insn "vasrwi_insn"
  [(set (match_operand:V8HI 0 "vector_register_operand"           "=v")
	(unspec:V8HI [(match_operand:V8HI 1 "vector_register_operand"  "v")
		      (match_operand:SI 2 "immediate_operand" "L")] UNSPEC_ARC_SIMD_VASRWi))]
  "TARGET_SIMD_SET"
  "vasrwi %0, %1, %2"
  [(set_attr "type" "simd_varith_1cycle")
   (set_attr "length" "4")
   (set_attr "cond" "nocond")])

(define_insn "vasrpwbi_insn"
  [(set (match_operand:V8HI 0 "vector_register_operand"           "=v")
	(unspec:V8HI [(match_operand:V8HI 1 "vector_register_operand"  "v")
		      (match_operand:SI 2 "immediate_operand" "L")] UNSPEC_ARC_SIMD_VASRPWBi))]
  "TARGET_SIMD_SET"
  "vasrpwbi %0, %1, %2"
  [(set_attr "type" "simd_vpack")
   (set_attr "length" "4")
   (set_attr "cond" "nocond")])

(define_insn "vasrrpwbi_insn"
  [(set (match_operand:V8HI 0 "vector_register_operand"           "=v")
	(unspec:V8HI [(match_operand:V8HI 1 "vector_register_operand"  "v")
		      (match_operand:SI 2 "immediate_operand" "L")] UNSPEC_ARC_SIMD_VASRRPWBi))]
  "TARGET_SIMD_SET"
  "vasrrpwbi %0, %1, %2"
  [(set_attr "type" "simd_vpack")
   (set_attr "length" "4")
   (set_attr "cond" "nocond")])

(define_insn "vsr8awi_insn"
  [(set (match_operand:V8HI 0 "vector_register_operand"           "=v")
	(unspec:V8HI [(match_operand:V8HI 1 "vector_register_operand"  "v")
		      (match_operand:SI 2 "immediate_operand" "L")] UNSPEC_ARC_SIMD_VSR8AWi))]
  "TARGET_SIMD_SET"
  "vsr8awi %0, %1, %2"
  [(set_attr "type" "simd_valign_with_acc")
   (set_attr "length" "4")
   (set_attr "cond" "nocond")])

(define_insn "vsr8i_insn"
  [(set (match_operand:V8HI 0 "vector_register_operand"           "=v")
	(unspec:V8HI [(match_operand:V8HI 1 "vector_register_operand"  "v")
		      (match_operand:SI 2 "immediate_operand" "L")] UNSPEC_ARC_SIMD_VSR8i))]
  "TARGET_SIMD_SET"
  "vsr8i %0, %1, %2"
  [(set_attr "type" "simd_valign")
   (set_attr "length" "4")
   (set_attr "cond" "nocond")])

;; Va, Vb, u8 (simm) insns

(define_insn "vmvaw_insn"
  [(set (match_operand:V8HI 0 "vector_register_operand"           "=v")
        (unspec:V8HI [(match_operand:V8HI 1 "vector_register_operand"  "v")
		      (match_operand:SI 2 "immediate_operand" "P")] UNSPEC_ARC_SIMD_VMVAW))]
  "TARGET_SIMD_SET"
  "vmvaw %0, %1, %2"
  [(set_attr "type" "simd_vmove_with_acc")
   (set_attr "length" "4")
   (set_attr "cond" "nocond")])

(define_insn "vmvw_insn"
  [(set (match_operand:V8HI 0 "vector_register_operand"           "=v")
        (unspec:V8HI [(match_operand:V8HI 1 "vector_register_operand"  "v")
		      (match_operand:SI 2 "immediate_operand" "P")] UNSPEC_ARC_SIMD_VMVW))]
  "TARGET_SIMD_SET"
  "vmvw %0, %1, %2"
  [(set_attr "type" "simd_vmove")
   (set_attr "length" "4")
   (set_attr "cond" "nocond")])

(define_insn "vmvzw_insn"
  [(set (match_operand:V8HI 0 "vector_register_operand"           "=v")
        (unspec:V8HI [(match_operand:V8HI 1 "vector_register_operand"  "v")
		      (match_operand:SI 2 "immediate_operand" "P")] UNSPEC_ARC_SIMD_VMVZW))]
  "TARGET_SIMD_SET"
  "vmvzw %0, %1, %2"
  [(set_attr "type" "simd_vmove_else_zero")
   (set_attr "length" "4")
   (set_attr "cond" "nocond")])

(define_insn "vd6tapf_insn"
  [(set (match_operand:V8HI 0 "vector_register_operand"           "=v")
        (unspec:V8HI [(match_operand:V8HI 1 "vector_register_operand"  "v")
		      (match_operand:SI 2 "immediate_operand" "P")] UNSPEC_ARC_SIMD_VD6TAPF))]
  "TARGET_SIMD_SET"
  "vd6tapf %0, %1, %2"
  [(set_attr "type" "simd_vspecial_4cycle")
   (set_attr "length" "4")
   (set_attr "cond" "nocond")])

;; Va, rlimm, u8 (simm) insns
(define_insn "vmovaw_insn"
  [(set (match_operand:V8HI 0 "vector_register_operand"           "=v")
        (unspec:V8HI [(match_operand:SI 1 "nonmemory_operand"  "r")
		      (match_operand:SI 2 "immediate_operand" "P")] UNSPEC_ARC_SIMD_VMOVAW))]
  "TARGET_SIMD_SET"
  "vmovaw %0, %1, %2"
  [(set_attr "type" "simd_vmove_with_acc")
   (set_attr "length" "4")
   (set_attr "cond" "nocond")])

(define_insn "vmovw_insn"
  [(set (match_operand:V8HI 0 "vector_register_operand"           "=v")
        (unspec:V8HI [(match_operand:SI 1 "nonmemory_operand"  "r")
		      (match_operand:SI 2 "immediate_operand" "P")] UNSPEC_ARC_SIMD_VMOVW))]
  "TARGET_SIMD_SET"
  "vmovw %0, %1, %2"
  [(set_attr "type" "simd_vmove")
   (set_attr "length" "4")
   (set_attr "cond" "nocond")])

(define_insn "vmovzw_insn"
  [(set (match_operand:V8HI 0 "vector_register_operand"           "=v")
        (unspec:V8HI [(match_operand:SI 1 "nonmemory_operand"  "r")
		      (match_operand:SI 2 "immediate_operand" "P")] UNSPEC_ARC_SIMD_VMOVZW))]
  "TARGET_SIMD_SET"
  "vmovzw %0, %1, %2"
  [(set_attr "type" "simd_vmove_else_zero")
   (set_attr "length" "4")
   (set_attr "cond" "nocond")])

;; Va, rlimm, Ic insns
(define_insn "vsr8_insn"
  [(set (match_operand:V8HI 0 "vector_register_operand"           "=v")
	(unspec:V8HI [(match_operand:V8HI 1 "vector_register_operand"  "v")
		      (match_operand:SI 2 "immediate_operand" "K")
		      (match_operand:V8HI 3 "vector_register_operand"  "v")] UNSPEC_ARC_SIMD_VSR8))]
  "TARGET_SIMD_SET"
  "vsr8 %0, %1, i%2"
  [(set_attr "type" "simd_valign")
   (set_attr "length" "4")
   (set_attr "cond" "nocond")])

(define_insn "vasrw_insn"
  [(set (match_operand:V8HI 0 "vector_register_operand"           "=v")
	(unspec:V8HI [(match_operand:V8HI 1 "vector_register_operand"  "v")
		      (match_operand:SI 2 "immediate_operand" "K")
		      (match_operand:V8HI 3 "vector_register_operand"  "v")] UNSPEC_ARC_SIMD_VASRW))]
  "TARGET_SIMD_SET"
  "vasrw %0, %1, i%2"
  [(set_attr "type" "simd_varith_1cycle")
   (set_attr "length" "4")
   (set_attr "cond" "nocond")])

(define_insn "vsr8aw_insn"
  [(set (match_operand:V8HI 0 "vector_register_operand"           "=v")
	(unspec:V8HI [(match_operand:V8HI 1 "vector_register_operand"  "v")
		      (match_operand:SI 2 "immediate_operand" "K")
		      (match_operand:V8HI 3 "vector_register_operand"  "v")] UNSPEC_ARC_SIMD_VSR8AW))]
  "TARGET_SIMD_SET"
  "vsr8aw %0, %1, i%2"
  [(set_attr "type" "simd_valign_with_acc")
   (set_attr "length" "4")
   (set_attr "cond" "nocond")])

;; Va, Vb insns
(define_insn "vabsaw_insn"
  [(set (match_operand:V8HI 0 "vector_register_operand"  "=v")
	(unspec:V8HI [(match_operand:V8HI 1 "vector_register_operand"  "v")] UNSPEC_ARC_SIMD_VABSAW))]
  "TARGET_SIMD_SET"
  "vabsaw %0, %1"
  [(set_attr "type" "simd_varith_with_acc")
   (set_attr "length" "4")
   (set_attr "cond" "nocond")])

(define_insn "vabsw_insn"
  [(set (match_operand:V8HI 0 "vector_register_operand"  "=v")
	(unspec:V8HI [(match_operand:V8HI 1 "vector_register_operand"  "v")] UNSPEC_ARC_SIMD_VABSW))]
  "TARGET_SIMD_SET"
  "vabsw %0, %1"
  [(set_attr "type" "simd_varith_1cycle")
   (set_attr "length" "4")
   (set_attr "cond" "nocond")])

(define_insn "vaddsuw_insn"
  [(set (match_operand:V8HI 0 "vector_register_operand"  "=v")
	(unspec:V8HI [(match_operand:V8HI 1 "vector_register_operand"  "v")] UNSPEC_ARC_SIMD_VADDSUW))]
  "TARGET_SIMD_SET"
  "vaddsuw %0, %1"
  [(set_attr "type" "simd_varith_1cycle")
   (set_attr "length" "4")
   (set_attr "cond" "nocond")])

(define_insn "vsignw_insn"
  [(set (match_operand:V8HI 0 "vector_register_operand"  "=v")
	(unspec:V8HI [(match_operand:V8HI 1 "vector_register_operand"  "v")] UNSPEC_ARC_SIMD_VSIGNW))]
  "TARGET_SIMD_SET"
  "vsignw %0, %1"
  [(set_attr "type" "simd_varith_1cycle")
   (set_attr "length" "4")
   (set_attr "cond" "nocond")])

(define_insn "vexch1_insn"
  [(set (match_operand:V8HI 0 "vector_register_operand"  "=v")
	(unspec:V8HI [(match_operand:V8HI 1 "vector_register_operand"  "v")] UNSPEC_ARC_SIMD_VEXCH1))]
  "TARGET_SIMD_SET"
  "vexch1 %0, %1"
  [(set_attr "type" "simd_vpermute")
   (set_attr "length" "4")
   (set_attr "cond" "nocond")])

(define_insn "vexch2_insn"
  [(set (match_operand:V8HI 0 "vector_register_operand"  "=v")
	(unspec:V8HI [(match_operand:V8HI 1 "vector_register_operand"  "v")] UNSPEC_ARC_SIMD_VEXCH2))]
  "TARGET_SIMD_SET"
  "vexch2 %0, %1"
  [(set_attr "type" "simd_vpermute")
   (set_attr "length" "4")
   (set_attr "cond" "nocond")])

(define_insn "vexch4_insn"
  [(set (match_operand:V8HI 0 "vector_register_operand"  "=v")
	(unspec:V8HI [(match_operand:V8HI 1 "vector_register_operand"  "v")] UNSPEC_ARC_SIMD_VEXCH4))]
  "TARGET_SIMD_SET"
  "vexch4 %0, %1"
  [(set_attr "type" "simd_vpermute")
   (set_attr "length" "4")
   (set_attr "cond" "nocond")])

(define_insn "vupbaw_insn"
  [(set (match_operand:V8HI 0 "vector_register_operand"  "=v")
	(unspec:V8HI [(match_operand:V8HI 1 "vector_register_operand"  "v")] UNSPEC_ARC_SIMD_VUPBAW))]
  "TARGET_SIMD_SET"
  "vupbaw %0, %1"
  [(set_attr "type" "simd_vpack_with_acc")
   (set_attr "length" "4")
   (set_attr "cond" "nocond")])

(define_insn "vupbw_insn"
  [(set (match_operand:V8HI 0 "vector_register_operand"  "=v")
	(unspec:V8HI [(match_operand:V8HI 1 "vector_register_operand"  "v")] UNSPEC_ARC_SIMD_VUPBW))]
  "TARGET_SIMD_SET"
  "vupbw %0, %1"
  [(set_attr "type" "simd_vpack")
   (set_attr "length" "4")
   (set_attr "cond" "nocond")])

(define_insn "vupsbaw_insn"
  [(set (match_operand:V8HI 0 "vector_register_operand"  "=v")
	(unspec:V8HI [(match_operand:V8HI 1 "vector_register_operand"  "v")] UNSPEC_ARC_SIMD_VUPSBAW))]
  "TARGET_SIMD_SET"
  "vupsbaw %0, %1"
  [(set_attr "type" "simd_vpack_with_acc")
   (set_attr "length" "4")
   (set_attr "cond" "nocond")])

(define_insn "vupsbw_insn"
  [(set (match_operand:V8HI 0 "vector_register_operand"  "=v")
	(unspec:V8HI [(match_operand:V8HI 1 "vector_register_operand"  "v")] UNSPEC_ARC_SIMD_VUPSBW))]
  "TARGET_SIMD_SET"
  "vupsbw %0, %1"
  [(set_attr "type" "simd_vpack")
   (set_attr "length" "4")
   (set_attr "cond" "nocond")])

; DMA setup instructions
(define_insn "vdirun_insn"
  [(set (match_operand:SI 0 "arc_simd_dma_register_operand"           "=d")
        (unspec_volatile:SI [(match_operand:SI 1 "nonmemory_operand"  "r")
			     (match_operand:SI 2 "nonmemory_operand" "r")] UNSPEC_ARC_SIMD_VDIRUN))]
  "TARGET_SIMD_SET"
  "vdirun %1, %2"
  [(set_attr "type" "simd_dma")
   (set_attr "length" "4")
   (set_attr "cond" "nocond")])

(define_insn "vdorun_insn"
  [(set (match_operand:SI 0 "arc_simd_dma_register_operand"              "=d")
        (unspec_volatile:SI [(match_operand:SI 1 "nonmemory_operand"     "r")
			     (match_operand:SI 2 "nonmemory_operand"     "r")] UNSPEC_ARC_SIMD_VDORUN))]
  "TARGET_SIMD_SET"
  "vdorun %1, %2"
  [(set_attr "type" "simd_dma")
   (set_attr "length" "4")
   (set_attr "cond" "nocond")])

(define_insn "vdiwr_insn"
  [(set (match_operand:SI 0 "arc_simd_dma_register_operand"           "=d,d")
        (unspec_volatile:SI [(match_operand:SI 1 "nonmemory_operand"  "r,Cal")] UNSPEC_ARC_SIMD_VDIWR))]
  "TARGET_SIMD_SET"
  "vdiwr %0, %1"
  [(set_attr "type" "simd_dma")
   (set_attr "length" "4,8")
   (set_attr "cond" "nocond,nocond")])

(define_insn "vdowr_insn"
  [(set (match_operand:SI 0 "arc_simd_dma_register_operand"           "=d,d")
        (unspec_volatile:SI [(match_operand:SI 1 "nonmemory_operand"  "r,Cal")] UNSPEC_ARC_SIMD_VDOWR))]
  "TARGET_SIMD_SET"
  "vdowr %0, %1"
  [(set_attr "type" "simd_dma")
   (set_attr "length" "4,8")
   (set_attr "cond" "nocond,nocond")])

;; vector record and run instructions
(define_insn "vrec_insn"
  [(unspec_volatile [(match_operand:SI 0 "nonmemory_operand"  "r")] UNSPEC_ARC_SIMD_VREC)]
  "TARGET_SIMD_SET"
  "vrec %0"
  [(set_attr "type" "simd_vcontrol")
   (set_attr "length" "4")
   (set_attr "cond" "nocond")])

(define_insn "vrun_insn"
  [(unspec_volatile [(match_operand:SI 0 "nonmemory_operand"  "r")] UNSPEC_ARC_SIMD_VRUN)]
  "TARGET_SIMD_SET"
  "vrun %0"
  [(set_attr "type" "simd_vcontrol")
   (set_attr "length" "4")
   (set_attr "cond" "nocond")])

(define_insn "vrecrun_insn"
  [(unspec_volatile [(match_operand:SI 0 "nonmemory_operand"  "r")] UNSPEC_ARC_SIMD_VRECRUN)]
  "TARGET_SIMD_SET"
  "vrecrun %0"
  [(set_attr "type" "simd_vcontrol")
   (set_attr "length" "4")
   (set_attr "cond" "nocond")])

(define_insn "vendrec_insn"
  [(unspec_volatile [(match_operand:SI 0 "nonmemory_operand"  "r")] UNSPEC_ARC_SIMD_VENDREC)]
  "TARGET_SIMD_SET"
  "vendrec %S0"
  [(set_attr "type" "simd_vcontrol")
   (set_attr "length" "4")
   (set_attr "cond" "nocond")])

(define_insn "vld32wh_insn"
  [(set (match_operand:V8HI 0 "vector_register_operand"           "=v")
	(vec_concat:V8HI (zero_extend:V4HI (mem:V4QI (plus:SI (match_operand:SI 1 "immediate_operand" "P")
							      (zero_extend: SI (vec_select:HI (match_operand:V8HI 2 "vector_register_operand"  "v")
											      (parallel [(match_operand:SI 3 "immediate_operand" "L")]))))))
			 (vec_select:V4HI (match_dup 0)
					  (parallel [(const_int 0)]))))]
  "TARGET_SIMD_SET"
  "vld32wh %0, [i%3,%1]"
  [(set_attr "type" "simd_vload")
   (set_attr "length" "4")
   (set_attr "cond" "nocond")])

(define_insn "vld32wl_insn"
  [(set (match_operand:V8HI 0 "vector_register_operand"           "=v")
	(vec_concat:V8HI (vec_select:V4HI (match_dup 0)
					  (parallel [(const_int 1)]))
			 (zero_extend:V4HI (mem:V4QI (plus:SI (match_operand:SI 1 "immediate_operand" "P")
							      (zero_extend: SI (vec_select:HI (match_operand:V8HI 2 "vector_register_operand"  "v")
											      (parallel [(match_operand:SI 3 "immediate_operand" "L")])))))) ))]
  "TARGET_SIMD_SET"
  "vld32wl %0, [i%3,%1]"
  [(set_attr "type" "simd_vload")
   (set_attr "length" "4")
   (set_attr "cond" "nocond")])

(define_insn "vld64w_insn"
  [(set (match_operand:V8HI 0 "vector_register_operand" "=v")
	(zero_extend:V8HI (mem:V4HI (plus:SI (zero_extend:SI (vec_select:HI (match_operand:V8HI 1 "vector_register_operand"  "v")
									    (parallel [(match_operand:SI 2 "immediate_operand" "L")])))
					     (match_operand:SI 3 "immediate_operand" "P")))))]
 "TARGET_SIMD_SET"
 "vld64w %0, [i%2, %3]"
 [(set_attr "type" "simd_vload")
  (set_attr "length" "4")
  (set_attr "cond" "nocond")]
)

(define_insn "vld64_insn"
  [(set (match_operand:V8HI 0 "vector_register_operand"           "=v")
	(vec_concat:V8HI (vec_select:V4HI (match_dup 0)
					  (parallel [(const_int 1)]))
			 (mem:V4HI (plus:SI (match_operand:SI 1 "immediate_operand" "P")
					    (zero_extend: SI (vec_select:HI (match_operand:V8HI 2 "vector_register_operand"  "v")
									    (parallel [(match_operand:SI 3 "immediate_operand" "L")]))))) ))]
  "TARGET_SIMD_SET"
  "vld64 %0, [i%3,%1]"
  [(set_attr "type" "simd_vload")
   (set_attr "length" "4")
   (set_attr "cond" "nocond")])

(define_insn "vld32_insn"
  [(set (match_operand:V8HI 0 "vector_register_operand"           "=v")
	(vec_concat:V8HI (vec_select:V4HI (match_dup 0)
					  (parallel [(const_int 1)]))
			 (vec_concat:V4HI  (vec_select:V2HI (match_dup 0)
							    (parallel [(const_int 1)]))
					   (mem:V2HI (plus:SI (match_operand:SI 1 "immediate_operand" "P")
							      (zero_extend: SI (vec_select:HI (match_operand:V8HI 2 "vector_register_operand"  "v")
											      (parallel [(match_operand:SI 3 "immediate_operand" "L")])))))) ))]
  "TARGET_SIMD_SET"
  "vld32 %0, [i%3,%1]"
  [(set_attr "type" "simd_vload")
   (set_attr "length" "4")
   (set_attr "cond" "nocond")])

(define_insn "vst16_n_insn"
  [(set  (mem:HI (plus:SI (match_operand:SI 0 "immediate_operand" "P")
			  (zero_extend: SI (vec_select:HI (match_operand:V8HI 1 "vector_register_operand"  "v")
							  (parallel [(match_operand:SI 2 "immediate_operand" "L")])))))
	 (vec_select:HI (match_operand:V8HI 3 "vector_register_operand" "v")
			(parallel [(match_operand:SI 4 "immediate_operand" "L")])))]
 "TARGET_SIMD_SET"
 "vst16_%4 %3,[i%2, %0]"
 [(set_attr "type" "simd_vstore")
  (set_attr "length" "4")
  (set_attr "cond" "nocond")])

(define_insn "vst32_n_insn"
  [(set  (mem:SI (plus:SI (match_operand:SI 0 "immediate_operand" "P")
			  (zero_extend: SI (vec_select:HI (match_operand:V8HI 1 "vector_register_operand"  "v")
							  (parallel [(match_operand:SI 2 "immediate_operand" "L")])))))
	 (vec_select:SI (unspec:V4SI [(match_operand:V8HI 3 "vector_register_operand" "v")] UNSPEC_ARC_SIMD_VCAST)
			(parallel [(match_operand:SI 4 "immediate_operand" "L")])))]
 "TARGET_SIMD_SET"
 "vst32_%4 %3,[i%2, %0]"
 [(set_attr "type" "simd_vstore")
  (set_attr "length" "4")
  (set_attr "cond" "nocond")])

;; SIMD unit interrupt
(define_insn "vinti_insn"
  [(unspec_volatile [(match_operand:SI 0 "nonmemory_operand"  "L")] UNSPEC_ARC_SIMD_VINTI)]
  "TARGET_SIMD_SET"
  "vinti %0"
  [(set_attr "type" "simd_vcontrol")
   (set_attr "length" "4")
   (set_attr "cond" "nocond")])
