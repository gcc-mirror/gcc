;;- Instruction patterns for the System z vector facility
;;  Copyright (C) 2015-2025 Free Software Foundation, Inc.
;;  Contributed by Andreas Krebbel (Andreas.Krebbel@de.ibm.com)

;; This file is part of GCC.

;; GCC is free software; you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free
;; Software Foundation; either version 3, or (at your option) any later
;; version.

;; GCC is distributed in the hope that it will be useful, but WITHOUT ANY
;; WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
;; for more details.

;; You should have received a copy of the GNU General Public License
;; along with GCC; see the file COPYING3.  If not see
;; <http://www.gnu.org/licenses/>.

; All vector modes supported in a vector register
(define_mode_iterator V
  [V1QI V2QI V4QI V8QI V16QI V1HI V2HI V4HI V8HI V1SI V2SI V4SI V1DI V2DI V1SF
   V2SF V4SF V1DF V2DF])
(define_mode_iterator VT
  [V1QI V2QI V4QI V8QI V16QI V1HI V2HI V4HI V8HI V1SI V2SI V4SI V1DI V2DI V1SF
   V2SF V4SF V1DF V2DF V1TF V1TI TI])
(define_mode_iterator VT_VXE3
  [V1QI V2QI V4QI V8QI V16QI V1HI V2HI V4HI V8HI V1SI V2SI V4SI V1DI V2DI V1SF
   V2SF V4SF V1DF V2DF V1TF (V1TI "TARGET_VXE3") (TI "TARGET_VXE3")])

; All modes directly supported by the hardware having full vector reg size
(define_mode_iterator V_HW  [V16QI V8HI V4SI V2DI V1TI TI V2DF
			     (V4SF "TARGET_VXE") (V1TF "TARGET_VXE")
			     (TF "TARGET_VXE")])
(define_mode_iterator V_HW1 [V16QI V8HI V4SI V2DI V1TI V2DF
			     (V4SF "TARGET_VXE") (V1TF "TARGET_VXE")
			     (TF "TARGET_VXE")])
(define_mode_iterator V_HW2 [V16QI V8HI V4SI V2DI V2DF (V4SF "TARGET_VXE")
			     (V1TF "TARGET_VXE") (TF "TARGET_VXE")])
(define_mode_iterator V_HW3 [V16QI V8HI V4SI V2DI V1TI TI V4SF V2DF V1TF TF])

(define_mode_iterator VT_HW_HSDT [V8HI V4SI V4SF V2DI V2DF V1TI V1TF TI TF])
(define_mode_iterator V_HW_HSD [V8HI V4SI (V4SF "TARGET_VXE") V2DI V2DF])

; Including TI for instructions that support it (va, vn, ...)
(define_mode_iterator VT_HW [V16QI V8HI V4SI V2DI V2DF V1TI TI (V4SF "TARGET_VXE") (V1TF "TARGET_VXE")])

; All full size integer vector modes supported in a vector register + TImode
(define_mode_iterator VIT_HW    [V16QI V8HI V4SI V2DI V1TI TI])
(define_mode_iterator VIT_HW_VXE3_T [V16QI V8HI V4SI V2DI (V1TI "TARGET_VXE3") (TI "TARGET_VXE3")])
(define_mode_iterator VIT_HW_VXE3_DT [V16QI V8HI V4SI (V2DI "TARGET_VXE3") (V1TI "TARGET_VXE3") (TI "TARGET_VXE3")])
(define_mode_iterator VI_HW     [V16QI V8HI V4SI V2DI])
(define_mode_iterator VI_HW_VXE3 [V16QI V8HI V4SI (V2DI "TARGET_VXE3")])
(define_mode_iterator VI_HW_QHS [V16QI V8HI V4SI])
(define_mode_iterator VI_HW_SDT [V4SI  V2DI V1TI TI])
(define_mode_iterator VI_HW_HSD [V8HI  V4SI V2DI])
(define_mode_iterator VI_HW_HSDT [V8HI V4SI V2DI V1TI TI])
(define_mode_iterator VI_HW_HS  [V8HI  V4SI])
(define_mode_iterator VI_HW_QH  [V16QI V8HI])
(define_mode_iterator VI_HW_T   [V1TI TI])

; Directly supported vector modes with a certain number of elements
(define_mode_iterator V_HW_2   [V2DI V2DF])
(define_mode_iterator V_HW_4   [V4SI V4SF])

; All integer vector modes supported in a vector register + TImode
(define_mode_iterator VIT [V1QI V2QI V4QI V8QI V16QI V1HI V2HI V4HI V8HI V1SI V2SI V4SI V1DI V2DI V1TI TI])
(define_mode_iterator VIT_VXE3 [V1QI V2QI V4QI V8QI V16QI V1HI V2HI V4HI V8HI V1SI V2SI V4SI V1DI V2DI (V1TI "TARGET_VXE3") (TI "TARGET_VXE3")])
(define_mode_iterator VI  [V1QI V2QI V4QI V8QI V16QI V1HI V2HI V4HI V8HI V1SI V2SI V4SI V1DI V2DI])
(define_mode_iterator VI_VXE3 [V1QI V2QI V4QI V8QI V16QI V1HI V2HI V4HI V8HI V1SI V2SI V4SI V1DI (V2DI "TARGET_VXE3")])
(define_mode_iterator VI_QHS [V1QI V2QI V4QI V8QI V16QI V1HI V2HI V4HI V8HI V1SI V2SI V4SI])

(define_mode_iterator VFT [(V1SF "TARGET_VXE") (V2SF "TARGET_VXE") (V4SF "TARGET_VXE")
			   V1DF V2DF
			   (V1TF "TARGET_VXE") (TF "TARGET_VXE")])

; All modes present in V_HW1 and VFT.
(define_mode_iterator V_HW1_FT [V16QI V8HI V4SI V2DI V1TI V1DF
			       V2DF (V1SF "TARGET_VXE") (V2SF "TARGET_VXE")
			       (V4SF "TARGET_VXE") (V1TF "TARGET_VXE")
			       (TF "TARGET_VXE")])

; FP vector modes directly supported by the HW.  This does not include
; vector modes using only part of a vector register and should be used
; for instructions which might trigger IEEE exceptions.
(define_mode_iterator VF_HW [(V4SF "TARGET_VXE") V2DF (V1TF "TARGET_VXE")
			     (TF "TARGET_VXE")])

(define_mode_iterator V_8   [V1QI])
(define_mode_iterator V_16  [V2QI  V1HI])
(define_mode_iterator V_32  [V4QI  V2HI V1SI V1SF])
(define_mode_iterator V_64  [V8QI  V4HI V2SI V2SF V1DI V1DF])
(define_mode_iterator V_128 [V16QI V8HI V4SI V4SF V2DI V2DF V1TI V1TF
			     (TF "TARGET_VXE")])
(define_mode_iterator V_128_NOSINGLE [V16QI V8HI V4SI V4SF V2DI V2DF])

; 32 bit int<->fp vector conversion instructions are available since VXE2 (z15).
(define_mode_iterator VX_VEC_CONV_BFP [V2DF (V4SF "TARGET_VXE2")])

(define_mode_iterator VI_EXTEND [V2QI V2HI V2SI V4QI V4HI])

; Empty string for all but TImode.  This is used to hide the TImode
; expander name in case it is defined already.  See addti3 for an
; example.
(define_mode_attr ti* [(V1QI "")  (V2QI "") (V4QI "") (V8QI "") (V16QI "")
		       (V1HI "")  (V2HI "") (V4HI "") (V8HI "")
		       (V1SI "")  (V2SI "") (V4SI "")
		       (V1DI "")  (V2DI "")
		       (V1TI "")  (TI "*")
		       (V1SF "")  (V2SF "") (V4SF "")
		       (V1DF "")  (V2DF "")
		       (V1TF "")  (TF "")])

;; Facilitate dispatching TFmode expanders on z14+.
(define_mode_attr tf_vr [(TF "_vr") (V4SF "") (V2DF "") (V1TF "") (V1SF "")
			 (V2SF "") (V1DF "") (V16QI "") (V8HI "") (V4SI "")
			 (V2DI "") (V1TI "")])

; The element type of the vector.
(define_mode_attr non_vec[(V1QI "QI") (V2QI "QI") (V4QI "QI") (V8QI "QI") (V16QI "QI")
			  (V1HI "HI") (V2HI "HI") (V4HI "HI") (V8HI "HI")
			  (V1SI "SI") (V2SI "SI") (V4SI "SI")
			  (V1DI "DI") (V2DI "DI")
			  (V1TI "TI") (TI "TI")
			  (V1SF "SF") (V2SF "SF") (V4SF "SF")
			  (V1DF "DF") (V2DF "DF")
			  (V1TF "TF") (TF "TF")])

; Like above, but in lower case.
(define_mode_attr non_vec_l[(V1QI "qi") (V2QI "qi") (V4QI "qi") (V8QI "qi")
			    (V16QI "qi")
			    (V1HI "hi") (V2HI "hi") (V4HI "hi") (V8HI "hi")
			    (V1SI "si") (V2SI "si") (V4SI "si")
			    (V1DI "di") (V2DI "di")
			    (V1TI "ti") (TI "ti")
			    (V1SF "sf") (V2SF "sf") (V4SF "sf")
			    (V1DF "df") (V2DF "df")
			    (V1TF "tf") (TF "tf")])

; The instruction suffix for integer instructions and instructions
; which do not care about whether it is floating point or integer.
(define_mode_attr bhfgq[(V1QI "b") (V2QI "b") (V4QI "b") (V8QI "b") (V16QI "b")
			(V1HI "h") (V2HI "h") (V4HI "h") (V8HI "h")
			(V1SI "f") (V2SI "f") (V4SI "f")
			(V1DI "g") (V2DI "g")
			(V1TI "q") (TI "q")
			(V1SF "f") (V2SF "f") (V4SF "f")
			(V1DF "g") (V2DF "g")
			(V1TF "q") (TF "q")])

; This is for vmalhw. It gets an 'w' attached to avoid confusion with
; multiply and add logical high vmalh.
(define_mode_attr w [(V1QI "")  (V2QI "")  (V4QI "")  (V8QI "") (V16QI "")
		     (V1HI "w") (V2HI "w") (V4HI "w") (V8HI "w")
		     (V1SI "")  (V2SI "")  (V4SI "")
		     (V1DI "")  (V2DI "")
		     (V1TI "")  (TI "")])

; Resulting mode of a vector comparison.  For floating point modes an
; integer vector mode with the same element size is picked.
(define_mode_attr TOINTVEC [(V1QI "V1QI") (V2QI "V2QI") (V4QI "V4QI") (V8QI "V8QI") (V16QI "V16QI")
			    (V1HI "V1HI") (V2HI "V2HI") (V4HI "V4HI") (V8HI "V8HI")
			    (V1SI "V1SI") (V2SI "V2SI") (V4SI "V4SI")
			    (V1DI "V1DI") (V2DI "V2DI")
			    (V1TI "V1TI") (TI "TI")
			    (V1SF "V1SI") (V2SF "V2SI") (V4SF "V4SI")
			    (V1DF "V1DI") (V2DF "V2DI")
			    (V1TF "V1TI") (TF "TI")])

(define_mode_attr tointvec [(V1QI "v1qi") (V2QI "v2qi") (V4QI "v4qi") (V8QI "v8qi") (V16QI "v16qi")
			    (V1HI "v1hi") (V2HI "v2hi") (V4HI "v4hi") (V8HI "v8hi")
			    (V1SI "v1si") (V2SI "v2si") (V4SI "v4si")
			    (V1DI "v1di") (V2DI "v2di")
			    (V1TI "v1ti") (TI "ti")
			    (V1SF "v1si") (V2SF "v2si") (V4SF "v4si")
			    (V1DF "v1di") (V2DF "v2di")
			    (V1TF "v1ti") (TF   "ti")])

(define_mode_attr vw [(SF "w") (V1SF "w") (V2SF "v") (V4SF "v")
		      (DF "w") (V1DF "w") (V2DF "v")
		      (TF "w") (V1TF "w")])

(define_mode_attr sdx [(SF "s") (V1SF "s") (V2SF "s") (V4SF "s")
		       (DF "d") (V1DF "d") (V2DF "d")
		       (TF "x") (V1TF "x")])

; Vector with widened element size but half the number of elements.
(define_mode_attr vec_double [(V1QI "V1HI") (V2QI "V1HI") (V4QI "V2HI") (V8QI "V4HI") (V16QI "V8HI")
			      (V1HI "V1SI") (V2HI "V1SI") (V4HI "V2SI") (V8HI "V4SI")
			      (V1SI "V1DI") (V2SI "V1DI") (V4SI "V2DI")
			      (V1DI "V1TI") (V2DI "TI")
			      (V1SF "V1DF") (V2SF "V1DF") (V4SF "V2DF")])

; Vector with shrinked element size but twice the number of elements.
(define_mode_attr vec_half [(V1HI "V2QI") (V2HI "V4QI") (V4HI "V8QI") (V8HI "V16QI")
			    (V1SI "V2HI") (V2SI "V4HI") (V4SI "V8HI")
			    (V1DI "V2SI") (V2DI "V4SI")
			    (V1TI "V2DI")
			    (V1DF "V2SF") (V2DF "V4SF")
			    (V1TF "V1DF")])

; Vector with twice the number of elements but same element size.
(define_mode_attr vec_2x_nelts [(V1QI "V2QI") (V2QI "V4QI") (V4QI "V8QI") (V8QI "V16QI") (V16QI "V32QI")
				(V1HI "V2HI") (V2HI "V4HI") (V4HI "V8HI") (V8HI "V16HI")
				(V1SI "V2SI") (V2SI "V4SI") (V4SI "V8SI")
				(V1DI "V2DI") (V2DI "V4DI")
				(V1SF "V2SF") (V2SF "V4SF") (V4SF "V8SF")
				(V1DF "V2DF") (V2DF "V4DF")])

; Vector with widened element size and the same number of elements.
(define_mode_attr VEC_2X_WIDE [(V1QI "V1HI") (V2QI "V2HI") (V4QI "V4HI") (V8QI "V8HI") (V16QI "V16HI")
			       (V1HI "V1SI") (V2HI "V2SI") (V4HI "V4SI") (V8HI "V8SI")
			       (V1SI "V1DI") (V2SI "V2DI") (V4SI "V4DI")
			       (V1DI "V1TI") (V2DI "V2TI")
			       (V1SF "V1DF") (V2SF "V2DF") (V4SF "V4DF")
			       (V1DF "V1TF") (V2DF "V2TF")])

(define_mode_attr vec_2x_wide [(V1QI "v1hi") (V2QI "v2hi") (V4QI "v4hi") (V8QI "v8hi") (V16QI "v16hi")
			       (V1HI "v1si") (V2HI "v2si") (V4HI "v4si") (V8HI "v8si")
			       (V1SI "v1di") (V2SI "v2di") (V4SI "v4di")
			       (V1DI "v1ti") (V2DI "v2ti")
			       (V1SF "v1df") (V2SF "v2df") (V4SF "v4df")
			       (V1DF "v1tf") (V2DF "v2tf")])

; Vector with half the element size AND half the number of elements.
(define_mode_attr vec_halfhalf
  [(V2HI "V2QI") (V4HI "V4QI") (V8HI "V8QI")
   (V2SI "V2HI") (V4SI "V4HI")
   (V2DI "V2SI")
   (V2DF "V2SF")])

(define_mode_attr vec_halfnumelts
  [(V4SF "V2SF") (V4SI "V2SI")])



; Comparison operators on int and fp compares which are directly
; supported by the HW.
(define_code_iterator VICMP_HW_OP [eq gt gtu])
(define_code_iterator VICMP_HW_OP2 [gt gtu])
; For int insn_cmp_op can be used in the insn name as well as in the asm output.
(define_code_attr insn_cmp_op [(eq "eq") (gt "h") (gtu "hl") (ge "he")])

; Flags for vector string instructions (vfae all 4, vfee only ZS and CS, vstrc all 4)
(define_constants
  [(VSTRING_FLAG_IN         8)   ; invert result
   (VSTRING_FLAG_RT         4)   ; result type
   (VSTRING_FLAG_ZS         2)   ; zero search
   (VSTRING_FLAG_CS         1)]) ; condition code set

(include "vx-builtins.md")

; Full HW vector size moves

; We don't use lm/stm for 128 bit moves since these are slower than
; splitting it into separate moves.

; FIXME: More constants are possible by enabling jxx, jyy constraints
; for TImode (use double-int for the calculations)

; vgmb, vgmh, vgmf, vgmg, vrepib, vrepih, vrepif, vrepig
(define_insn "mov<mode><tf_vr>"
  [(set (match_operand:V_128 0 "nonimmediate_operand" "=v,v,R,  v,  v,  v,  v,  v,v,*d,*d,?o")
	(match_operand:V_128 1 "general_operand"      " v,R,v,j00,jm1,jyy,jxx,jzz,d, v,dT,*d"))]
  ""
  "@
   vlr\t%v0,%v1
   vl\t%v0,%1%A1
   vst\t%v1,%0%A0
   vzero\t%v0
   vone\t%v0
   vgbm\t%v0,%r1
   vgm\t%v0,%q1
   vrepi\t%v0,%p1
   vlvgp\t%v0,%1,%N1
   #
   #
   #"
  [(set_attr "cpu_facility" "vx,vx,vx,vx,vx,vx,vx,vx,vx,vx,*,*")
   (set_attr "op_type"      "VRR,VRX,VRX,VRI,VRI,VRI,VRI,VRI,VRR,*,*,*")])

(define_expand "movtf"
  [(match_operand:TF 0 "nonimmediate_operand" "")
   (match_operand:TF 1 "general_operand"      "")]
  ""
  { EXPAND_MOVTF(movtf); })

; VR -> GPR, no instruction so split it into 64 element sets.
(define_split
  [(set (match_operand:V_128 0 "register_operand" "")
	(match_operand:V_128 1 "register_operand" ""))]
  "TARGET_VX && GENERAL_REG_P (operands[0]) && VECTOR_REG_P (operands[1])"
  [(set (match_dup 2)
       (vec_select:DI
         (subreg:V2DI (match_dup 1) 0)
           (parallel [(const_int 0)])))
   (set (match_dup 3)
       (vec_select:DI
         (subreg:V2DI (match_dup 1) 0)
           (parallel [(const_int 1)])))]
{
  operands[2] = operand_subword (operands[0], 0, 0, <MODE>mode);
  operands[3] = operand_subword (operands[0], 1, 0, <MODE>mode);
})

; Split the 128 bit GPR move into two word mode moves
; s390_split_ok_p decides which part needs to be moved first.

(define_split
  [(set (match_operand:V_128 0 "nonimmediate_operand" "")
        (match_operand:V_128 1 "general_operand" ""))]
  "reload_completed
   && s390_split_ok_p (operands[0], operands[1], <MODE>mode, 0)"
  [(set (match_dup 2) (match_dup 4))
   (set (match_dup 3) (match_dup 5))]
{
  operands[2] = operand_subword (operands[0], 0, 0, <MODE>mode);
  operands[3] = operand_subword (operands[0], 1, 0, <MODE>mode);
  operands[4] = operand_subword (operands[1], 0, 0, <MODE>mode);
  operands[5] = operand_subword (operands[1], 1, 0, <MODE>mode);
})

(define_split
  [(set (match_operand:V_128 0 "nonimmediate_operand" "")
        (match_operand:V_128 1 "general_operand" ""))]
  "reload_completed
   && s390_split_ok_p (operands[0], operands[1], <MODE>mode, 1)"
  [(set (match_dup 2) (match_dup 4))
   (set (match_dup 3) (match_dup 5))]
{
  operands[2] = operand_subword (operands[0], 1, 0, <MODE>mode);
  operands[3] = operand_subword (operands[0], 0, 0, <MODE>mode);
  operands[4] = operand_subword (operands[1], 1, 0, <MODE>mode);
  operands[5] = operand_subword (operands[1], 0, 0, <MODE>mode);
})

; This is the vector equivalent to the TImode splitter in s390.md.  It
; is required if both target GPRs occur in the source address operand.

; For non-s_operands at least one of the target GPRs does not conflict
; with the address operand and one of the splitters above will take
; over.
(define_split
  [(set (match_operand:V_128 0 "register_operand" "")
        (match_operand:V_128 1 "memory_operand" ""))]
  "TARGET_ZARCH && reload_completed
   && !VECTOR_REG_P (operands[0])
   && !s_operand (operands[1], VOIDmode)"
  [(set (match_dup 0) (match_dup 1))]
{
  rtx addr = operand_subword (operands[0], 1, 0, <MODE>mode);
  addr = gen_lowpart (Pmode, addr);
  s390_load_address (addr, XEXP (operands[1], 0));
  operands[1] = replace_equiv_address (operands[1], addr);
})

; Moves for smaller vector modes.

; In these patterns only the vlr, vone, and vzero instructions write
; VR bytes outside the mode.  This should be ok since we disallow
; formerly bigger modes being accessed with smaller modes via
; subreg. Note: The vone, vzero instructions could easily be replaced
; with vlei which would only access the bytes belonging to the mode.
; However, this would probably be slower.

(define_insn "mov<mode>"
  [(set (match_operand:V_8 0 "nonimmediate_operand" "=v,v,d,v,R,  v,  v,  v,  v,  v,d,  Q,  S,  Q,  S,  d,  d,d,R,T")
        (match_operand:V_8 1 "general_operand"      " v,d,v,R,v,j00,jm1,jyy,jxx,jzz,d,j00,j00,jm1,jm1,j00,jm1,T,d,d"))]
  "TARGET_VX"
  "@
   vlr\t%v0,%v1
   vlvgb\t%v0,%1,0
   vlgvb\t%0,%v1,0
   vleb\t%v0,%1,0
   vsteb\t%v1,%0,0
   vzero\t%v0
   vone\t%v0
   vgbm\t%v0,%r1
   vgm\t%v0,%q1
   vrepi\t%v0,%p1
   lr\t%0,%1
   mvi\t%0,0
   mviy\t%0,0
   mvi\t%0,255
   mviy\t%0,255
   lhi\t%0,0
   lhi\t%0,-1
   llc\t%0,%1
   stc\t%1,%0
   stcy\t%1,%0"
  [(set_attr "op_type"      "VRR,VRS,VRS,VRX,VRX,VRI,VRI,VRI,VRI,VRI,RR,SI,SIY,SI,SIY,RI,RI,RXY,RX,RXY")])

(define_insn "mov<mode>"
  [(set (match_operand:V_16 0 "nonimmediate_operand" "=v,v,d,v,R,  v,  v,  v,  v,  v,d,  Q,  Q,  d,  d,d,d,d,R,T,b")
        (match_operand:V_16 1 "general_operand"      " v,d,v,R,v,j00,jm1,jyy,jxx,jzz,d,j00,jm1,j00,jm1,R,T,b,d,d,d"))]
  ""
  "@
   vlr\t%v0,%v1
   vlvgh\t%v0,%1,0
   vlgvh\t%0,%v1,0
   vleh\t%v0,%1,0
   vsteh\t%v1,%0,0
   vzero\t%v0
   vone\t%v0
   vgbm\t%v0,%r1
   vgm\t%v0,%q1
   vrepi\t%v0,%p1
   lr\t%0,%1
   mvhhi\t%0,0
   mvhhi\t%0,-1
   lhi\t%0,0
   lhi\t%0,-1
   lh\t%0,%1
   lhy\t%0,%1
   lhrl\t%0,%1
   sth\t%1,%0
   sthy\t%1,%0
   sthrl\t%1,%0"
  [(set_attr "op_type"      "VRR,VRS,VRS,VRX,VRX,VRI,VRI,VRI,VRI,VRI,RR,SIL,SIL,RI,RI,RX,RXY,RIL,RX,RXY,RIL")])

(define_insn "mov<mode>"
  [(set (match_operand:V_32 0 "nonimmediate_operand" "=f,f,f,R,T,v,v,d,v,R,  f,  v,  v,  v,  v,  v,  Q,  Q,  d,  d,d,d,d,d,R,T,b")
	(match_operand:V_32 1 "general_operand"      " f,R,T,f,f,v,d,v,R,v,j00,j00,jm1,jyy,jxx,jzz,j00,jm1,j00,jm1,b,d,R,T,d,d,d"))]
  "TARGET_VX"
  "@
   ldr\t%v0,%v1
   lde\t%0,%1
   ley\t%0,%1
   ste\t%1,%0
   stey\t%1,%0
   vlr\t%v0,%v1
   vlvgf\t%v0,%1,0
   vlgvf\t%0,%v1,0
   vlef\t%v0,%1,0
   vstef\t%1,%0,0
   lzer\t%v0
   vzero\t%v0
   vone\t%v0
   vgbm\t%v0,%r1
   vgm\t%v0,%q1
   vrepi\t%v0,%p1
   mvhi\t%0,0
   mvhi\t%0,-1
   lhi\t%0,0
   lhi\t%0,-1
   lrl\t%0,%1
   lr\t%0,%1
   l\t%0,%1
   ly\t%0,%1
   st\t%1,%0
   sty\t%1,%0
   strl\t%1,%0"
  [(set_attr "op_type" "RR,RXE,RXY,RX,RXY,VRR,VRS,VRS,VRX,VRX,RRE,VRI,VRI,VRI,VRI,VRI,SIL,SIL,RI,RI,
                        RIL,RR,RX,RXY,RX,RXY,RIL")])

(define_insn "mov<mode>"
  [(set (match_operand:V_64 0 "nonimmediate_operand"
         "=f,f,f,R,T,v,v,d,v,R,  f,  v,  v,  v,  v,  v,  Q,  Q,  d,  d,f,d,d,d,d,T,b")
        (match_operand:V_64 1 "general_operand"
         " f,R,T,f,f,v,d,v,R,v,j00,j00,jm1,jyy,jxx,jzz,j00,jm1,j00,jm1,d,f,b,d,T,d,d"))]
  "TARGET_ZARCH"
  "@
   ldr\t%0,%1
   ld\t%0,%1
   ldy\t%0,%1
   std\t%1,%0
   stdy\t%1,%0
   vlr\t%v0,%v1
   vlvgg\t%v0,%1,0
   vlgvg\t%0,%v1,0
   vleg\t%v0,%1,0
   vsteg\t%v1,%0,0
   lzdr\t%0
   vzero\t%v0
   vone\t%v0
   vgbm\t%v0,%r1
   vgm\t%v0,%q1
   vrepi\t%v0,%p1
   mvghi\t%0,0
   mvghi\t%0,-1
   lghi\t%0,0
   lghi\t%0,-1
   ldgr\t%0,%1
   lgdr\t%0,%1
   lgrl\t%0,%1
   lgr\t%0,%1
   lg\t%0,%1
   stg\t%1,%0
   stgrl\t%1,%0"
  [(set_attr "op_type" "RRE,RX,RXY,RX,RXY,VRR,VRS,VRS,VRX,VRX,RRE,VRI,VRI,VRI,VRI,VRI,
                        SIL,SIL,RI,RI,RRE,RRE,RIL,RR,RXY,RXY,RIL")])


; vec_load_lanes?

; vec_store_lanes?

; vec_set is supposed to *modify* an existing vector so operand 0 is
; duplicated as input operand.
(define_expand "vec_set<mode>"
  [(set (match_operand:V                    0 "register_operand"  "")
	(unspec:V [(match_operand:<non_vec> 1 "general_operand"   "")
		   (match_operand:SI        2 "nonmemory_operand" "")
		   (match_dup 0)]
		   UNSPEC_VEC_SET))]
  "TARGET_VX")

; FIXME: Support also vector mode operands for 1
; FIXME: A target memory operand seems to be useful otherwise we end
; up with vl vlvgg vst.  Shouldn't the middle-end be able to handle
; that itself?
; vlvgb, vlvgh, vlvgf, vlvgg, vleb, vleh, vlef, vleg, vleib, vleih, vleif, vleig
(define_insn "*vec_set<mode>"
  [(set (match_operand:V                    0 "register_operand"  "=v,v,v")
	(unspec:V [(match_operand:<non_vec> 1 "general_operand"    "d,R,K")
		   (match_operand:SI        2 "nonmemory_operand" "an,I,I")
		   (match_operand:V         3 "register_operand"   "0,0,0")]
		  UNSPEC_VEC_SET))]
  "TARGET_VX
   && (!CONST_INT_P (operands[2])
       || UINTVAL (operands[2]) < GET_MODE_NUNITS (<V:MODE>mode))"
  "@
   vlvg<bhfgq>\t%v0,%1,%Y2
   vle<bhfgq>\t%v0,%1,%2
   vlei<bhfgq>\t%v0,%1,%2"
  [(set_attr "op_type" "VRS,VRX,VRI")])

; vlvgb, vlvgh, vlvgf, vlvgg
(define_insn "*vec_set<mode>_plus"
  [(set (match_operand:V                      0 "register_operand" "=v")
	(unspec:V [(match_operand:<non_vec>   1 "general_operand"   "d")
		   (plus:SI (match_operand:SI 2 "register_operand"  "a")
			    (match_operand:SI 4 "const_int_operand" "n"))
		   (match_operand:V           3 "register_operand"  "0")]
		  UNSPEC_VEC_SET))]
  "TARGET_VX"
  "vlvg<bhfgq>\t%v0,%1,%Y4(%2)"
  [(set_attr "op_type" "VRS")])


;; FIXME: Support also vector mode operands for 0
;; This is used via RTL standard name as well as for expanding the builtin
(define_expand "vec_extract<mode><non_vec_l>"
  [(set (match_operand:<non_vec>    0 "nonimmediate_operand" "")
       (vec_select:<non_vec>
         (match_operand:V           1 "register_operand" "")
         (parallel
          [(match_operand:SI        2 "nonmemory_operand" "")])))]
  "TARGET_VX"
)

; vlgvb, vlgvh, vlgvf, vlgvg, vsteb, vsteh, vstef, vsteg
(define_insn "*vec_extract<mode>"
  [(set (match_operand:<non_vec> 0 "nonimmediate_operand" "=d,R")
       (vec_select:<non_vec>
         (match_operand:V        1 "nonmemory_operand"  "v,v")
         (parallel
          [(match_operand:SI     2 "nonmemory_operand" "an,I")])))]
  "TARGET_VX"
  {
    if (CONST_INT_P (operands[2]))
	  operands[2] = GEN_INT (UINTVAL (operands[2]) & (GET_MODE_NUNITS (<V:MODE>mode) - 1));
    if (which_alternative == 0)
      return "vlgv<bhfgq>\t%0,%v1,%Y2";
	return "vste<bhfgq>\t%v1,%0,%2";
  }
  [(set_attr "op_type" "VRS,VRX")])

; vlgvb, vlgvh, vlgvf, vlgvg
(define_insn "*vec_extract<mode>_plus"
  [(set (match_operand:<non_vec>       0 "nonimmediate_operand" "=d")
	(vec_select:<non_vec>
	 (match_operand:V              1 "register_operand"      "v")
	 (plus:SI (match_operand:SI    2 "nonmemory_operand"     "a")
	  (parallel [(match_operand:SI 3 "const_int_operand"     "n")]))))]
  "TARGET_VX"
  "vlgv<bhfgq>\t%0,%v1,%Y3(%2)"
  [(set_attr "op_type" "VRS")])

(define_expand "vec_init<mode><non_vec_l>"
  [(match_operand:V_128 0 "register_operand" "")
   (match_operand:V_128 1 "nonmemory_operand" "")]
  "TARGET_VX"
{
  s390_expand_vec_init (operands[0], operands[1]);
  DONE;
})

(define_insn "*vec_vllezlf<mode>"
  [(set (match_operand:V_HW_4              0 "register_operand" "=v")
	(vec_concat:V_HW_4
	 (vec_concat:<vec_halfnumelts>
	  (match_operand:<non_vec> 1 "memory_operand"    "R")
	  (const_int 0))
	 (vec_concat:<vec_halfnumelts>
	  (const_int 0)
	  (const_int 0))))]
  "TARGET_VXE"
  "vllezlf\t%v0,%1"
  [(set_attr "op_type" "VRX")])

; Replicate from vector element
; vrepb, vreph, vrepf, vrepg
(define_insn "*vec_splat<mode>"
  [(set (match_operand:V_128_NOSINGLE   0 "register_operand" "=v")
	(vec_duplicate:V_128_NOSINGLE
	 (vec_select:<non_vec>
	  (match_operand:V_128_NOSINGLE 1 "register_operand"  "v")
	  (parallel
	   [(match_operand:QI 2 "const_mask_operand" "C")]))))]
  "TARGET_VX && UINTVAL (operands[2]) < GET_MODE_NUNITS (<MODE>mode)"
  "vrep<bhfgq>\t%v0,%v1,%2"
  [(set_attr "op_type" "VRI")])

; vlrepb, vlreph, vlrepf, vlrepg, vrepib, vrepih, vrepif, vrepig, vrepb, vreph, vrepf, vrepg
(define_insn "*vec_splats<mode>"
  [(set (match_operand:V_128_NOSINGLE                          0 "register_operand" "=v,v,v,v")
	(vec_duplicate:V_128_NOSINGLE (match_operand:<non_vec> 1 "general_operand"  " R,K,v,d")))]
  "TARGET_VX"
  "@
   vlrep<bhfgq>\t%v0,%1
   vrepi<bhfgq>\t%v0,%h1
   vrep<bhfgq>\t%v0,%v1,0
   #"
  [(set_attr "op_type" "VRX,VRI,VRI,*")])

; vlbrreph, vlbrrepf, vlbrrepg
(define_insn "*vec_splats_bswap_vec<mode>"
  [(set (match_operand:V_HW_HSD                           0 "register_operand"        "=v")
	(bswap:V_HW_HSD
	 (vec_duplicate:V_HW_HSD (match_operand:<non_vec> 1 "memory_operand"           "R"))))
   (use (match_operand:V16QI                              2 "permute_pattern_operand"  "X"))]
  "TARGET_VXE2"
  "vlbrrep<bhfgq>\t%v0,%1"
  [(set_attr "op_type" "VRX")])

; Why do we need both? Shouldn't there be a canonical form?
; vlbrreph, vlbrrepf, vlbrrepg
(define_insn "*vec_splats_bswap_elem<mode>"
  [(set (match_operand:V_HW_HSD                    0 "register_operand" "=v")
	(vec_duplicate:V_HW_HSD
	 (bswap:<non_vec> (match_operand:<non_vec> 1 "memory_operand"    "R"))))]
  "TARGET_VXE2"
  "vlbrrep<bhfgq>\t%v0,%1"
  [(set_attr "op_type" "VRX")])

; A TFmode operand resides in FPR register pairs while V1TF is in a
; single vector register.
(define_insn "*vec_tf_to_v1tf_fpr"
  [(set (match_operand:V1TF                   0 "nonimmediate_operand" "=v,v,R,v,v")
	(vec_duplicate:V1TF (match_operand:TF 1 "general_operand"       "f,R,f,G,d")))]
  "TARGET_VX && !TARGET_VXE"
  "@
   vmrhg\t%v0,%1,%N1
   vl\t%v0,%1%A1
   vst\t%v1,%0%A0
   vzero\t%v0
   vlvgp\t%v0,%1,%N1"
  [(set_attr "op_type" "VRR,VRX,VRX,VRI,VRR")])

; Both TFmode and V1TFmode operands reside in vector registers.
(define_insn "*vec_tf_to_v1tf_vr"
  [(set (match_operand:V1TF                   0 "nonimmediate_operand" "=v,v,R,v,v")
	(vec_duplicate:V1TF (match_operand:TF 1 "general_operand"       "v,R,v,G,d")))]
  "TARGET_VXE"
  "@
   vlr\t%v0,%1
   vl\t%v0,%1%A1
   vst\t%v1,%0%A0
   vzero\t%v0
   vlvgp\t%v0,%1,%N1"
  [(set_attr "op_type" "VRR,VRX,VRX,VRI,VRR")])

(define_insn_and_split "fprx2_to_tf"
  [(set (match_operand:TF               0 "nonimmediate_operand" "=v,AR")
	(subreg:TF (match_operand:FPRX2 1 "general_operand"       "f,f") 0))]
  "TARGET_VXE"
  "@
   vmrhg\t%v0,%1,%N1
   #"
  "!(MEM_P (operands[0]) && MEM_VOLATILE_P (operands[0]))"
  [(set (match_dup 2) (match_dup 3))
   (set (match_dup 4) (match_dup 5))]
{
  operands[2] = simplify_gen_subreg (DFmode, operands[0], TFmode, 0);
  operands[3] = simplify_gen_subreg (DFmode, operands[1], FPRX2mode, 0);
  operands[4] = simplify_gen_subreg (DFmode, operands[0], TFmode, 8);
  operands[5] = simplify_gen_subreg (DFmode, operands[1], FPRX2mode, 8);
}
  [(set_attr "op_type" "VRR,*")])

(define_insn "*vec_ti_to_v1ti"
  [(set (match_operand:V1TI                   0 "nonimmediate_operand" "=v,v,R,  v,  v,v")
	(vec_duplicate:V1TI (match_operand:TI 1 "general_operand"       "v,R,v,j00,jm1,d")))]
  "TARGET_VX"
  "@
   vlr\t%v0,%v1
   vl\t%v0,%1%A1
   vst\t%v1,%0%A0
   vzero\t%v0
   vone\t%v0
   vlvgp\t%v0,%1,%N1"
  [(set_attr "op_type" "VRR,VRX,VRX,VRI,VRI,VRR")])

; vec_splats is supposed to replicate op1 into all elements of op0
; This splitter first sets the rightmost element of op0 to op1 and
; then does a vec_splat to replicate that element into all other
; elements.
(define_split
  [(set (match_operand:V_128_NOSINGLE                          0 "register_operand" "")
	(vec_duplicate:V_128_NOSINGLE (match_operand:<non_vec> 1 "register_operand" "")))]
  "TARGET_VX && GENERAL_REG_P (operands[1])"
  [(set (match_dup 0)
	(unspec:V_128_NOSINGLE [(match_dup 1) (match_dup 2) (match_dup 0)] UNSPEC_VEC_SET))
   (set (match_dup 0)
	(vec_duplicate:V_128_NOSINGLE
	 (vec_select:<non_vec>
	  (match_dup 0) (parallel [(match_dup 2)]))))]
{
  operands[2] = GEN_INT (GET_MODE_NUNITS (<MODE>mode) - 1);
})

(define_predicate "vcond_comparison_operator"
  (match_operand 0 "comparison_operator")
{
  if (!HONOR_NANS (GET_MODE (XEXP (op, 0)))
      && !HONOR_NANS (GET_MODE (XEXP (op, 1))))
    return true;
  switch (GET_CODE (op))
    {
    case LE:
    case LT:
    case GE:
    case GT:
    case LTGT:
      /* Signaling vector comparisons are supported only on z14+.  */
      return TARGET_VXE || TARGET_NONSIGNALING_VECTOR_COMPARE_OK;
    default:
      return true;
    }
})

(define_expand "vcond_mask_<mode><tointvec>"
  [(set (match_operand:VT 0 "register_operand" "")
	(if_then_else:VT
	 (eq (match_operand:<TOINTVEC> 3 "register_operand" "")
	     (match_dup 4))
	 (match_operand:VT 2 "register_operand" "")
	 (match_operand:VT 1 "register_operand" "")))]
  "TARGET_VX"
  "operands[4] = CONST0_RTX (<TOINTVEC>mode);")


; We only have HW support for byte vectors.  The middle-end is
; supposed to lower the mode if required.
(define_insn "vec_permv16qi"
  [(set (match_operand:V16QI 0 "register_operand"               "=v")
	(unspec:V16QI [(match_operand:V16QI 1 "register_operand" "v")
		       (match_operand:V16QI 2 "register_operand" "v")
		       (match_operand:V16QI 3 "register_operand" "v")]
		      UNSPEC_VEC_PERM))]
  "TARGET_VX"
  "vperm\t%v0,%v1,%v2,%v3"
  [(set_attr "op_type" "VRR")])

(define_insn "*vec_perm<mode>"
  [(set (match_operand:VT_HW                                            0 "register_operand" "=v")
	(subreg:VT_HW (unspec:V16QI [(subreg:V16QI (match_operand:VT_HW 1 "register_operand"  "v") 0)
				     (subreg:V16QI (match_operand:VT_HW 2 "register_operand"  "v") 0)
				     (match_operand:V16QI               3 "register_operand"  "v")]
				    UNSPEC_VEC_PERM) 0))]
  "TARGET_VX"
  "vperm\t%v0,%v1,%v2,%v3"
  [(set_attr "op_type" "VRR")])


; First DW of op1 and second DW of op2
(define_insn "@vpdi1<mode>"
  [(set (match_operand:V_HW_2   0 "register_operand" "=v")
	(vec_select:V_HW_2
	 (vec_concat:<vec_2x_nelts>
	  (match_operand:V_HW_2 1 "register_operand"  "v")
	  (match_operand:V_HW_2 2 "register_operand"  "v"))
	 (parallel [(const_int 0) (const_int 3)])))]
  "TARGET_VX"
  "vpdi\t%v0,%v1,%v2,1"
  [(set_attr "op_type" "VRR")])

; Second DW of op1 and first of op2
(define_insn "@vpdi4<mode>"
  [(set (match_operand:V_HW_2   0 "register_operand" "=v")
	(vec_select:V_HW_2
	 (vec_concat:<vec_2x_nelts>
	  (match_operand:V_HW_2 1 "register_operand"  "v")
	  (match_operand:V_HW_2 2 "register_operand"  "v"))
	 (parallel [(const_int 1) (const_int 2)])))]
  "TARGET_VX"
  "vpdi\t%v0,%v1,%v2,4"
  [(set_attr "op_type" "VRR")])

; Second DW of op1 and first DW of op2 (when interpreted as 2-element vector).
(define_insn "@vpdi4_2<mode>"
  [(set (match_operand:V_HW_4   0 "register_operand" "=v")
	(vec_select:V_HW_4
	 (vec_concat:<vec_2x_nelts>
	  (match_operand:V_HW_4 1 "register_operand"  "v")
	  (match_operand:V_HW_4 2 "register_operand"  "v"))
	 (parallel [(const_int 2) (const_int 3) (const_int 4) (const_int 5)])))]
  "TARGET_VX"
  "vpdi\t%v0,%v1,%v2,4"
  [(set_attr "op_type" "VRR")])

(define_insn "*vmrhb"
  [(set (match_operand:V16QI                     0 "register_operand" "=v")
        (vec_select:V16QI
	  (vec_concat:V32QI (match_operand:V16QI 1 "register_operand"  "v")
			    (match_operand:V16QI 2 "register_operand"  "v"))
	  (parallel [(const_int 0) (const_int 16)
		     (const_int 1) (const_int 17)
		     (const_int 2) (const_int 18)
		     (const_int 3) (const_int 19)
		     (const_int 4) (const_int 20)
		     (const_int 5) (const_int 21)
		     (const_int 6) (const_int 22)
		     (const_int 7) (const_int 23)])))]
  "TARGET_VX"
  "vmrhb\t%0,%1,%2";
  [(set_attr "op_type" "VRR")])

(define_insn "*vmrlb"
  [(set (match_operand:V16QI                     0 "register_operand" "=v")
        (vec_select:V16QI
	  (vec_concat:V32QI (match_operand:V16QI 1 "register_operand"  "v")
			    (match_operand:V16QI 2 "register_operand"  "v"))
	  (parallel [(const_int  8) (const_int 24)
		     (const_int  9) (const_int 25)
		     (const_int 10) (const_int 26)
		     (const_int 11) (const_int 27)
		     (const_int 12) (const_int 28)
		     (const_int 13) (const_int 29)
		     (const_int 14) (const_int 30)
		     (const_int 15) (const_int 31)])))]
  "TARGET_VX"
  "vmrlb\t%0,%1,%2";
  [(set_attr "op_type" "VRR")])

(define_insn "*vmrhh"
  [(set (match_operand:V8HI                     0 "register_operand" "=v")
        (vec_select:V8HI
	  (vec_concat:V16HI (match_operand:V8HI 1 "register_operand"  "v")
			    (match_operand:V8HI 2 "register_operand"  "v"))
	  (parallel [(const_int 0) (const_int 8)
		     (const_int 1) (const_int 9)
		     (const_int 2) (const_int 10)
		     (const_int 3) (const_int 11)])))]
  "TARGET_VX"
  "vmrhh\t%0,%1,%2";
  [(set_attr "op_type" "VRR")])

(define_insn "*vmrlh"
  [(set (match_operand:V8HI                     0 "register_operand" "=v")
        (vec_select:V8HI
	  (vec_concat:V16HI (match_operand:V8HI 1 "register_operand"  "v")
			    (match_operand:V8HI 2 "register_operand"  "v"))
	  (parallel [(const_int 4) (const_int 12)
		     (const_int 5) (const_int 13)
		     (const_int 6) (const_int 14)
		     (const_int 7) (const_int 15)])))]
  "TARGET_VX"
  "vmrlh\t%0,%1,%2";
  [(set_attr "op_type" "VRR")])

(define_insn "*vmrhf"
  [(set (match_operand:V_HW_4                              0 "register_operand" "=v")
        (vec_select:V_HW_4
	  (vec_concat:<vec_2x_nelts> (match_operand:V_HW_4 1 "register_operand"  "v")
				     (match_operand:V_HW_4 2 "register_operand"  "v"))
	  (parallel [(const_int 0) (const_int 4)
		     (const_int 1) (const_int 5)])))]
  "TARGET_VX"
  "vmrhf\t%0,%1,%2";
  [(set_attr "op_type" "VRR")])

(define_insn "*vmrhf_half<mode>"
  [(set (match_operand:V_HW_4                                0 "register_operand" "=v")
	(vec_select:V_HW_4
	 (vec_concat:V_HW_4 (match_operand:<vec_halfnumelts> 1 "register_operand"  "v")
			    (match_operand:<vec_halfnumelts> 2 "register_operand"  "v"))
	 (parallel [(const_int 0) (const_int 2)
		    (const_int 1) (const_int 3)])))]
  "TARGET_VX"
  "vmrhf\t%0,%1,%2";
  [(set_attr "op_type" "VRR")])

(define_insn "*vmrlf"
  [(set (match_operand:V_HW_4                              0 "register_operand" "=v")
        (vec_select:V_HW_4
	  (vec_concat:<vec_2x_nelts> (match_operand:V_HW_4 1 "register_operand"  "v")
				     (match_operand:V_HW_4 2 "register_operand"  "v"))
	  (parallel [(const_int 2) (const_int 6)
		     (const_int 3) (const_int 7)])))]
  "TARGET_VX"
  "vmrlf\t%0,%1,%2";
  [(set_attr "op_type" "VRR")])

(define_insn "*vmrhg"
  [(set (match_operand:V_HW_2                              0 "register_operand" "=v")
        (vec_select:V_HW_2
	  (vec_concat:<vec_2x_nelts> (match_operand:V_HW_2 1 "register_operand"  "v")
				     (match_operand:V_HW_2 2 "register_operand"  "v"))
	  (parallel [(const_int 0) (const_int 2)])))]
  "TARGET_VX"
  "vmrhg\t%0,%1,%2";
  [(set_attr "op_type" "VRR")])

(define_insn "*vmrlg"
  [(set (match_operand:V_HW_2                              0 "register_operand" "=v")
        (vec_select:V_HW_2
	  (vec_concat:<vec_2x_nelts> (match_operand:V_HW_2 1 "register_operand"  "v")
				     (match_operand:V_HW_2 2 "register_operand"  "v"))
	  (parallel [(const_int 1) (const_int 3)])))]
  "TARGET_VX"
  "vmrlg\t%0,%1,%2";
  [(set_attr "op_type" "VRR")])

(define_insn "tf_to_fprx2"
  [(set (match_operand:FPRX2             0 "register_operand" "=f,f ,f")
	(unspec:FPRX2 [(match_operand:TF 1 "general_operand"   "v,AR,AT")]
		      UNSPEC_TF_TO_FPRX2))]
  "TARGET_VXE"
{
  char buf[64];
  const char *reg_pair = reg_names[REGNO (operands[0]) + 1];
  switch (which_alternative)
    {
    case 0:
      if (REGNO (operands[0]) == REGNO (operands[1]))
	{
	  reg_pair += 2;  // get rid of prefix %f
	  snprintf (buf, sizeof (buf), "vpdi\t%%%%v%s,%%v1,%%%%v%s,5", reg_pair, reg_pair);
	  output_asm_insn (buf, operands);
	  return "";
	}
      else
	{
	  reg_pair += 2;  // get rid of prefix %f
	  snprintf (buf, sizeof (buf), "ldr\t%%f0,%%f1;vpdi\t%%%%v%s,%%v1,%%%%v%s,5", reg_pair, reg_pair);
	  output_asm_insn (buf, operands);
	  return "";
	}
    case 1:
      {
	snprintf (buf, sizeof (buf), "ld\t%%f0,%%1;ld\t%%%s,8+%%1", reg_pair);
	output_asm_insn (buf, operands);
	return "";
      }
    case 2:
      {
	snprintf (buf, sizeof (buf), "ldy\t%%f0,%%1;ldy\t%%%s,8+%%1", reg_pair);
	output_asm_insn (buf, operands);
	return "";
      }
    default: gcc_unreachable ();
    }
})

;; VECTOR REVERSE ELEMENTS V16QI

(define_expand "eltswapv16qi"
  [(parallel
    [(set (match_operand:V16QI  0 "nonimmediate_operand")
	  (vec_select:V16QI
	   (match_operand:V16QI 1 "nonimmediate_operand")
	   (match_dup 2)))
     (use (match_dup 3))])]
  "TARGET_VX"
{
  rtvec vec = rtvec_alloc (16);
  for (int i = 0; i < 16; ++i)
    RTVEC_ELT (vec, i) = GEN_INT (15 - i);
  operands[2] = gen_rtx_PARALLEL (VOIDmode, vec);
  operands[3] = gen_rtx_CONST_VECTOR (V16QImode, vec);
})

(define_insn_and_split "*eltswapv16qi"
  [(set (match_operand:V16QI  0 "nonimmediate_operand" "=v,^R,^v")
	(vec_select:V16QI
	 (match_operand:V16QI 1 "nonimmediate_operand"  "v,^v,^R")
	 (parallel [(const_int 15)
		    (const_int 14)
		    (const_int 13)
		    (const_int 12)
		    (const_int 11)
		    (const_int 10)
		    (const_int 9)
		    (const_int 8)
		    (const_int 7)
		    (const_int 6)
		    (const_int 5)
		    (const_int 4)
		    (const_int 3)
		    (const_int 2)
		    (const_int 1)
		    (const_int 0)])))
   (use (match_operand:V16QI 2 "permute_pattern_operand" "v,X,X"))]
  "TARGET_VX"
  "@
   #
   vstbrq\t%v1,%0
   vlbrq\t%v0,%1"
  "&& reload_completed && REG_P (operands[0]) && REG_P (operands[1])"
  [(set (match_dup 0)
	(unspec:V16QI [(match_dup 1)
		       (match_dup 1)
		       (match_dup 2)]
		      UNSPEC_VEC_PERM))]
  ""
  [(set_attr "cpu_facility" "*,vxe2,vxe2")
   (set_attr "op_type" "*,VRX,VRX")])

;; VECTOR REVERSE ELEMENTS V8HI

(define_insn_and_split "eltswapv8hi"
  [(set (match_operand:V8HI  0 "nonimmediate_operand" "=v,R,v")
	(vec_select:V8HI
	 (match_operand:V8HI 1 "nonimmediate_operand"  "v,v,R")
	 (parallel [(const_int 7)
		    (const_int 6)
		    (const_int 5)
		    (const_int 4)
		    (const_int 3)
		    (const_int 2)
		    (const_int 1)
		    (const_int 0)])))
   (clobber (match_scratch:V2DI 2 "=&v,X,X"))
   (clobber (match_scratch:V4SI 3 "=&v,X,X"))]
  "TARGET_VX"
  "@
   #
   vsterh\t%v1,%0
   vlerh\t%v0,%1"
  "&& reload_completed && REG_P (operands[0]) && REG_P (operands[1])"
  [(set (match_dup 2)
	(subreg:V2DI (match_dup 1) 0))
   (set (match_dup 2)
	(vec_select:V2DI
	 (match_dup 2)
	 (parallel [(const_int 1) (const_int 0)])))
   (set (match_dup 2)
	(rotate:V2DI
	 (match_dup 2)
	 (const_int 32)))
   (set (match_dup 3)
	(subreg:V4SI (match_dup 2) 0))
   (set (match_dup 3)
	(rotate:V4SI
	 (match_dup 3)
	 (const_int 16)))
   (set (match_dup 0)
	(subreg:V8HI (match_dup 3) 0))]
  ""
  [(set_attr "cpu_facility" "*,vxe2,vxe2")
   (set_attr "op_type" "*,VRX,VRX")])

;; VECTOR REVERSE ELEMENTS V4SI / V4SF

(define_insn_and_split "eltswap<mode>"
  [(set (match_operand:V_HW_4  0 "nonimmediate_operand" "=v,R,v")
	(vec_select:V_HW_4
	 (match_operand:V_HW_4 1 "nonimmediate_operand"  "v,v,R")
	 (parallel [(const_int 3)
		    (const_int 2)
		    (const_int 1)
		    (const_int 0)])))
   (clobber (match_scratch:V2DI 2 "=&v,X,X"))]
  "TARGET_VX"
  "@
   #
   vsterf\t%v1,%0
   vlerf\t%v0,%1"
  "&& reload_completed && REG_P (operands[0]) && REG_P (operands[1])"
  [(set (match_dup 2)
	(subreg:V2DI (match_dup 1) 0))
   (set (match_dup 2)
	(vec_select:V2DI
	 (match_dup 2)
	 (parallel [(const_int 1) (const_int 0)])))
   (set (match_dup 2)
	(rotate:V2DI
	 (match_dup 2)
	 (const_int 32)))
   (set (match_dup 0)
	(subreg:V_HW_4 (match_dup 2) 0))]
  ""
  [(set_attr "cpu_facility" "*,vxe2,vxe2")
   (set_attr "op_type" "*,VRX,VRX")])

;; VECTOR REVERSE ELEMENTS V2DI / V2DF

(define_insn "eltswap<mode>"
  [(set (match_operand:V_HW_2  0 "nonimmediate_operand" "=v,R,v")
	(vec_select:V_HW_2
	 (match_operand:V_HW_2 1 "nonimmediate_operand"  "v,v,R")
	 (parallel [(const_int 1)
		    (const_int 0)])))]
  "TARGET_VX"
  "@
   vpdi\t%v0,%v1,%v1,4
   vsterg\t%v1,%0
   vlerg\t%v0,%1"
  [(set_attr "cpu_facility" "vx,vxe2,vxe2")
   (set_attr "op_type" "VRR,VRX,VRX")])

;;
;; Vector integer arithmetic instructions
;;

; vab, vah, vaf, vag, vaq

; We use nonimmediate_operand instead of register_operand since it is
; better to have the reloads into VRs instead of splitting the
; operation into two DImode ADDs.
(define_insn "<ti*>add<mode>3"
  [(set (match_operand:VIT           0 "nonimmediate_operand" "=v")
	(plus:VIT (match_operand:VIT 1 "nonimmediate_operand"  "v")
		  (match_operand:VIT 2 "general_operand"       "v")))]
  "TARGET_VX"
  "va<bhfgq>\t%v0,%v1,%v2"
  [(set_attr "op_type" "VRR")])

; vsb, vsh, vsf, vsg, vsq
(define_insn "<ti*>sub<mode>3"
  [(set (match_operand:VIT            0 "nonimmediate_operand" "=v")
	(minus:VIT (match_operand:VIT 1 "nonimmediate_operand"  "v")
		   (match_operand:VIT 2 "general_operand"       "v")))]
  "TARGET_VX"
  "vs<bhfgq>\t%v0,%v1,%v2"
  [(set_attr "op_type" "VRR")])

; vmlb, vmlhw, vmlf, vmlg, vmlq
(define_insn "mul<mode>3"
  [(set (match_operand:VIT_HW_VXE3_DT                      0 "register_operand" "=v")
	(mult:VIT_HW_VXE3_DT (match_operand:VIT_HW_VXE3_DT 1 "register_operand"  "v")
			     (match_operand:VIT_HW_VXE3_DT 2 "register_operand"  "v")))]
  "TARGET_VX"
  "vml<bhfgq><w>\t%v0,%v1,%v2"
  [(set_attr "op_type" "VRR")])

; vdf, vdg, vdq
(define_insn "div<mode>3"
  [(set (match_operand:VI_HW_SDT                0 "register_operand" "=v")
	(div:VI_HW_SDT (match_operand:VI_HW_SDT 1 "register_operand"  "v")
		       (match_operand:VI_HW_SDT 2 "register_operand"  "v")))]
  "TARGET_VXE3"
  "vd<bhfgq>\t%v0,%v1,%v2,0"
  [(set_attr "op_type" "VRR")])

; vdlf, vdlg, vdlq
(define_insn "udiv<mode>3"
  [(set (match_operand:VI_HW_SDT                 0 "register_operand" "=v")
	(udiv:VI_HW_SDT (match_operand:VI_HW_SDT 1 "register_operand"  "v")
			(match_operand:VI_HW_SDT 2 "register_operand"  "v")))]
  "TARGET_VXE3"
  "vdl<bhfgq>\t%v0,%v1,%v2,0"
  [(set_attr "op_type" "VRR")])

; vrf, vrg, vrq
(define_insn "mod<mode>3"
  [(set (match_operand:VI_HW_SDT                0 "register_operand" "=v")
	(mod:VI_HW_SDT (match_operand:VI_HW_SDT 1 "register_operand"  "v")
		       (match_operand:VI_HW_SDT 2 "register_operand"  "v")))]
  "TARGET_VXE3"
  "vr<bhfgq>\t%v0,%v1,%v2,0"
  [(set_attr "op_type" "VRR")])

; vrlf, vrlg, vrlq
(define_insn "umod<mode>3"
  [(set (match_operand:VI_HW_SDT                 0 "register_operand" "=v")
	(umod:VI_HW_SDT (match_operand:VI_HW_SDT 1 "register_operand"  "v")
			(match_operand:VI_HW_SDT 2 "register_operand"  "v")))]
  "TARGET_VXE3"
  "vrl<bhfgq>\t%v0,%v1,%v2,0"
  [(set_attr "op_type" "VRR")])

; vlcb, vlch, vlcf, vlcg
(define_insn "neg<mode>2"
  [(set (match_operand:VI         0 "register_operand" "=v")
	(neg:VI (match_operand:VI 1 "register_operand"  "v")))]
  "TARGET_VX"
  "vlc<bhfgq>\t%v0,%v1"
  [(set_attr "op_type" "VRR")])

(define_expand "abs<mode>2"
  [(set (match_operand:VIT          0 "register_operand" "=v")
	(abs:VIT (match_operand:VIT 1 "register_operand"  "v")))]
  "TARGET_VX"
{
  // Emulate via vec_sel (op1, -op1, op1 < 0)
  if ((<MODE>mode == V1TImode || <MODE>mode == TImode) && !TARGET_VXE3)
    {
      rtx zero = gen_reg_rtx (<MODE>mode);
      rtx neg_op1 = gen_reg_rtx (<MODE>mode);
      rtx lt = gen_reg_rtx (<MODE>mode);
      emit_move_insn (zero, GEN_INT (0));
      emit_move_insn (neg_op1, gen_rtx_MINUS (<MODE>mode, zero, operands[1]));
      s390_expand_vec_compare (lt, LT, operands[1], zero);
      emit_insn (gen_vec_sel0<mode> (operands[0], operands[1], neg_op1, lt, GEN_INT (0)));
      DONE;
    }
})

; vlpb, vlph, vlpf, vlpg, vlpq
(define_insn "*abs<mode>2"
  [(set (match_operand:VIT_VXE3               0 "register_operand" "=v")
	(abs:VIT_VXE3 (match_operand:VIT_VXE3 1 "register_operand"  "v")))]
  "TARGET_VX"
  "vlp<bhfgq>\t%v0,%v1"
  [(set_attr "op_type" "VRR")])


; Vector sum across

; Sum across DImode parts of the 1st operand and add the rightmost
; element of 2nd operand
; vsumgh, vsumgf
(define_insn "*vec_sum2<mode>"
  [(set (match_operand:V2DI 0 "register_operand" "=v")
	(unspec:V2DI [(match_operand:VI_HW_HS 1 "register_operand" "v")
		      (match_operand:VI_HW_HS 2 "register_operand" "v")]
		     UNSPEC_VEC_VSUMG))]
  "TARGET_VX"
  "vsumg<bhfgq>\t%v0,%v1,%v2"
  [(set_attr "op_type" "VRR")])

; vsumb, vsumh
(define_insn "*vec_sum4<mode>"
  [(set (match_operand:V4SI 0 "register_operand" "=v")
	(unspec:V4SI [(match_operand:VI_HW_QH 1 "register_operand" "v")
		      (match_operand:VI_HW_QH 2 "register_operand" "v")]
		     UNSPEC_VEC_VSUM))]
  "TARGET_VX"
  "vsum<bhfgq>\t%v0,%v1,%v2"
  [(set_attr "op_type" "VRR")])

;;
;; Vector bit instructions (int + fp)
;;

; Vector and

(define_insn "and<mode>3"
  [(set (match_operand:VT         0 "register_operand" "=v")
	(and:VT (match_operand:VT 1 "register_operand"  "v")
		(match_operand:VT 2 "register_operand"  "v")))]
  "TARGET_VX"
  "vn\t%v0,%v1,%v2"
  [(set_attr "op_type" "VRR")])

; Vector not and

(define_insn "notand<mode>3"
  [(set (match_operand:VT                 0 "register_operand" "=v")
	(ior:VT (not:VT (match_operand:VT 1 "register_operand"  "v"))
		(not:VT	(match_operand:VT 2 "register_operand"  "v"))))]
  "TARGET_VXE"
  "vnn\t%v0,%v1,%v2"
  [(set_attr "op_type" "VRR")])

; Vector or

(define_insn "ior<mode>3"
  [(set (match_operand:VT         0 "register_operand" "=v")
	(ior:VT (match_operand:VT 1 "register_operand"  "v")
		(match_operand:VT 2 "register_operand"  "v")))]
  "TARGET_VX"
  "vo\t%v0,%v1,%v2"
  [(set_attr "op_type" "VRR")])

; Vector or with complement

(define_insn "ior_not<mode>3"
  [(set (match_operand:VT                 0 "register_operand" "=v")
	(ior:VT (not:VT (match_operand:VT 2 "register_operand"  "v"))
		(match_operand:VT         1 "register_operand"  "v")))]
  "TARGET_VXE"
  "voc\t%v0,%v1,%v2"
  [(set_attr "op_type" "VRR")])

; Vector xor

(define_insn "xor<mode>3"
  [(set (match_operand:VT         0 "register_operand" "=v")
	(xor:VT (match_operand:VT 1 "register_operand"  "v")
		(match_operand:VT 2 "register_operand"  "v")))]
  "TARGET_VX"
  "vx\t%v0,%v1,%v2"
  [(set_attr "op_type" "VRR")])

; Vector not xor

(define_insn "notxor<mode>3"
  [(set (match_operand:VT                 0 "register_operand" "=v")
	(not:VT (xor:VT (match_operand:VT 1 "register_operand"  "v")
			(match_operand:VT 2 "register_operand"  "v"))))]
  "TARGET_VXE"
  "vnx\t%v0,%v1,%v2"
  [(set_attr "op_type" "VRR")])

; Bitwise inversion of a vector
(define_insn "one_cmpl<mode>2"
  [(set (match_operand:VT         0 "register_operand" "=v")
	(not:VT (match_operand:VT 1 "register_operand"  "v")))]
  "TARGET_VX"
  "vnot\t%v0,%v1"
  [(set_attr "op_type" "VRR")])

; Vector population count

(define_expand "popcount<mode>2"
  [(set (match_operand:VI_HW                0 "register_operand" "=v")
	(unspec:VI_HW [(match_operand:VI_HW 1 "register_operand"  "v")]
		      UNSPEC_POPCNT))]
  "TARGET_VX"
{
  if (TARGET_VXE)
    emit_insn (gen_popcount<mode>2_vxe (operands[0], operands[1]));
  else
    emit_insn (gen_popcount<mode>2_vx (operands[0], operands[1]));
  DONE;
})

; vpopctb, vpopcth, vpopctf, vpopctg
(define_insn "popcount<mode>2_vxe"
  [(set (match_operand:VI_HW                0 "register_operand" "=v")
	(unspec:VI_HW [(match_operand:VI_HW 1 "register_operand"  "v")]
		      UNSPEC_POPCNT))]
  "TARGET_VXE"
  "vpopct<bhfgq>\t%v0,%v1"
  [(set_attr "op_type" "VRR")])

(define_insn "popcountv16qi2_vx"
  [(set (match_operand:V16QI                0 "register_operand" "=v")
	(unspec:V16QI [(match_operand:V16QI 1 "register_operand"  "v")]
		      UNSPEC_POPCNT))]
  "TARGET_VX && !TARGET_VXE"
  "vpopct\t%v0,%v1,0"
  [(set_attr "op_type" "VRR")])

; vpopct only counts bits in byte elements.  Bigger element sizes need
; to be emulated.  Word and doubleword elements can use the sum across
; instructions.  For halfword sized elements we do a shift of a copy
; of the result, add it to the result and extend it to halfword
; element size (unpack).

(define_expand "popcountv8hi2_vx"
  [(set (match_dup 2)
	(unspec:V16QI [(match_operand:V8HI 1 "register_operand" "v")]
		      UNSPEC_POPCNT))
   ; Make a copy of the result
   (set (match_dup 3) (match_dup 2))
   ; Generate the shift count operand in a VR (8->byte 7)
   (set (match_dup 4) (match_dup 5))
   (set (match_dup 4) (unspec:V16QI [(const_int 8)
				     (const_int 7)
				     (match_dup 4)] UNSPEC_VEC_SET))
   ; Vector shift right logical by one byte
   (set (match_dup 3)
	(unspec:V16QI [(match_dup 3) (match_dup 4)] UNSPEC_VEC_SRLB))
   ; Add the shifted and the original result
   (set (match_dup 2)
	(plus:V16QI (match_dup 2) (match_dup 3)))
   ; Generate mask for the odd numbered byte elements
   (set (match_dup 3)
	(const_vector:V16QI [(const_int 0) (const_int -1)
			     (const_int 0) (const_int -1)
			     (const_int 0) (const_int -1)
			     (const_int 0) (const_int -1)
			     (const_int 0) (const_int -1)
			     (const_int 0) (const_int -1)
			     (const_int 0) (const_int -1)
			     (const_int 0) (const_int -1)]))
   ; Zero out the even indexed bytes
   (set (match_operand:V8HI 0 "register_operand" "=v")
	(and:V8HI (subreg:V8HI (match_dup 2) 0)
		  (subreg:V8HI (match_dup 3) 0)))
]
  "TARGET_VX && !TARGET_VXE"
{
  operands[1] = simplify_gen_subreg (V16QImode, operands[1],
				     V8HImode, 0);
  operands[2] = gen_reg_rtx (V16QImode);
  operands[3] = gen_reg_rtx (V16QImode);
  operands[4] = gen_reg_rtx (V16QImode);
  operands[5] = CONST0_RTX (V16QImode);
})

(define_expand "popcountv4si2_vx"
  [(set (match_dup 2)
	(unspec:V16QI [(match_operand:V4SI 1 "register_operand" "v")]
		      UNSPEC_POPCNT))
   (set (match_operand:V4SI 0 "register_operand" "=v")
	(unspec:V4SI [(match_dup 2) (match_dup 3)]
		     UNSPEC_VEC_VSUM))]
  "TARGET_VX && !TARGET_VXE"
{
  operands[1] = simplify_gen_subreg (V16QImode, operands[1], V4SImode, 0);
  operands[2] = gen_reg_rtx (V16QImode);
  operands[3] = force_reg (V16QImode, CONST0_RTX (V16QImode));
})

(define_expand "popcountv2di2_vx"
  [(set (match_dup 2)
	(unspec:V16QI [(match_operand:V2DI 1 "register_operand" "v")]
		      UNSPEC_POPCNT))
   (set (match_dup 3)
	(unspec:V4SI [(match_dup 2) (match_dup 4)]
		     UNSPEC_VEC_VSUM))
   (set (match_operand:V2DI 0 "register_operand" "=v")
	(unspec:V2DI [(match_dup 3) (match_dup 5)]
		     UNSPEC_VEC_VSUMG))]
  "TARGET_VX && !TARGET_VXE"
{
  operands[1] = simplify_gen_subreg (V16QImode, operands[1], V2DImode, 0);
  operands[2] = gen_reg_rtx (V16QImode);
  operands[3] = gen_reg_rtx (V4SImode);
  operands[4] = force_reg (V16QImode, CONST0_RTX (V16QImode));
  operands[5] = force_reg (V4SImode, CONST0_RTX (V4SImode));
})

; Count leading zeros
; vclzb, vclzh, vclzf, vclzg, vclzq
(define_insn "clz<mode>2"
  [(set (match_operand:VT_VXE3              0 "register_operand" "=v")
	(clz:VT_VXE3 (match_operand:VT_VXE3 1 "register_operand"  "v")))]
  "TARGET_VX"
  "vclz<bhfgq>\t%v0,%v1"
  [(set_attr "op_type" "VRR")])

; Count trailing zeros
; vctzb, vctzh, vctzf, vctzg, vctzq
(define_insn "ctz<mode>2"
  [(set (match_operand:VT_VXE3              0 "register_operand" "=v")
	(ctz:VT_VXE3 (match_operand:VT_VXE3 1 "register_operand"  "v")))]
  "TARGET_VX"
  "vctz<bhfgq>\t%v0,%v1"
  [(set_attr "op_type" "VRR")])



; Each vector element rotated by the corresponding vector element
; verllvb, verllvh, verllvf, verllvg
(define_insn "vrotl<mode>3"
  [(set (match_operand:VI            0 "register_operand" "=v")
	(rotate:VI (match_operand:VI 1 "register_operand"  "v")
		   (match_operand:VI 2 "register_operand"  "v")))]
  "TARGET_VX"
  "verllv<bhfgq>\t%v0,%v1,%v2"
  [(set_attr "op_type" "VRR")])


; Vector rotate and shift by scalar instructions

(define_code_iterator VEC_SHIFTS [ashift ashiftrt lshiftrt rotate])
(define_code_attr vec_shifts_name [(ashift "ashl")    (ashiftrt "ashr")
				   (lshiftrt "lshr")  (rotate "rotl")])
(define_code_attr vec_shifts_mnem [(ashift "vesl")    (ashiftrt "vesra")
				   (lshiftrt "vesrl") (rotate "verll")])

; Each vector element rotated by a scalar
(define_expand "<vec_shifts_name><mode>3"
  [(set (match_operand:VIT 0 "register_operand" "")
	(VEC_SHIFTS:VIT (match_operand:VIT 1 "register_operand" "")
			(match_operand:QI  2 "shift_count_operand" "")))]
  "TARGET_VX && ((<MODE>mode != V1TImode && <MODE>mode != TImode) || <CODE> != ROTATE)"
{
  if (<MODE>mode == V1TImode || <MODE>mode == TImode)
    {
      rtx shift_count = gen_reg_rtx (V16QImode);
      emit_insn (gen_vec_splatsv16qi (shift_count, operands[2]));

      if (!CONST_INT_P (operands[2]) || UINTVAL (operands[2]) > 7)
	switch (<CODE>)
	  {
	  case ASHIFT: emit_insn (gen_vec_slb (<MODE>mode, operands[0], operands[1], shift_count)); break;
	  case ASHIFTRT: emit_insn (gen_vec_srab (<MODE>mode, operands[0], operands[1], shift_count)); break;
	  case LSHIFTRT: emit_insn (gen_vec_srb (<MODE>mode, operands[0], operands[1], shift_count)); break;
	  default: gcc_unreachable ();
	  }
      else
	emit_move_insn (operands[0], operands[1]);

      if (!CONST_INT_P (operands[2]) || (UINTVAL (operands[2]) & 7) != 0)
	{
	  switch (<CODE>)
	    {
	    case ASHIFT: emit_insn (gen_vec_sll (<MODE>mode, V16QImode, operands[0], operands[0], shift_count)); break;
	    case ASHIFTRT: emit_insn (gen_vec_sral (<MODE>mode, V16QImode, operands[0], operands[0], shift_count)); break;
	    case LSHIFTRT: emit_insn (gen_vec_srl (<MODE>mode, V16QImode, operands[0], operands[0], shift_count)); break;
	    default: gcc_unreachable ();
	    }
	}

      DONE;
    }
})

; verllb, verllh, verllf, verllg
; veslb,  veslh,  veslf,  veslg
; vesrab, vesrah, vesraf, vesrag
; vesrlb, vesrlh, vesrlf, vesrlg
(define_insn "*<vec_shifts_name><mode>3"
  [(set (match_operand:VI                0 "register_operand"  "=v")
	(VEC_SHIFTS:VI (match_operand:VI 1 "register_operand"   "v")
		       (match_operand:QI 2 "shift_count_operand_vec" "jsc")))]
  "TARGET_VX
  && s390_valid_shift_count (operands[2],
    GET_MODE_BITSIZE (GET_MODE_INNER (<MODE>mode)) - 1)
  "
  "<vec_shifts_mnem><bhfgq>\t%v0,%v1,%Y2"
  [(set_attr "op_type" "VRS")])

; verllg for V4SI/V4SF.  This swaps the first and the second two
; elements of a vector and is only valid in that context.
(define_expand "rotl<mode>3_di"
 [
 (set (match_dup 2)
  (subreg:V2DI (match_operand:V_HW_4 1) 0))
 (set (match_dup 3)
  (rotate:V2DI
   (match_dup 2)
   (const_int 32)))
 (set (match_operand:V_HW_4 0)
  (subreg:V_HW_4 (match_dup 3) 0))]
 "TARGET_VX"
 {
  operands[2] = gen_reg_rtx (V2DImode);
  operands[3] = gen_reg_rtx (V2DImode);
 })

; Shift each element by corresponding vector element

; veslvb, veslvh, veslvf, veslvg
(define_insn "vashl<mode>3"
  [(set (match_operand:VI            0 "register_operand" "=v")
	(ashift:VI (match_operand:VI 1 "register_operand"  "v")
		   (match_operand:VI 2 "register_operand"  "v")))]
  "TARGET_VX"
  "veslv<bhfgq>\t%v0,%v1,%v2"
  [(set_attr "op_type" "VRR")])

; vesravb, vesravh, vesravf, vesravg
(define_insn "vashr<mode>3"
  [(set (match_operand:VI              0 "register_operand" "=v")
	(ashiftrt:VI (match_operand:VI 1 "register_operand"  "v")
		     (match_operand:VI 2 "register_operand"  "v")))]
  "TARGET_VX"
  "vesrav<bhfgq>\t%v0,%v1,%v2"
  [(set_attr "op_type" "VRR")])

; vesrlvb, vesrlvh, vesrlvf, vesrlvg
(define_insn "vlshr<mode>3"
  [(set (match_operand:VI              0 "register_operand" "=v")
	(lshiftrt:VI (match_operand:VI 1 "register_operand"  "v")
		     (match_operand:VI 2 "register_operand"  "v")))]
  "TARGET_VX"
  "vesrlv<bhfgq>\t%v0,%v1,%v2"
  [(set_attr "op_type" "VRR")])

; Vector shift right logical by byte

; Pattern used by e.g. popcount
(define_insn "*vec_srb<mode>"
  [(set (match_operand:V_HW3                0 "register_operand" "=v")
	(unspec:V_HW3 [(match_operand:V_HW3 1 "register_operand"  "v")
		       (match_operand:V16QI 2 "register_operand"  "v")]
		   UNSPEC_VEC_SRLB))]
  "TARGET_VX"
  "vsrlb\t%v0,%v1,%v2"
  [(set_attr "op_type" "VRR")])


; Vector shift left by byte

(define_insn "*vec_slb<mode>"
  [(set (match_operand:V_HW3                0 "register_operand" "=v")
	(unspec:V_HW3 [(match_operand:V_HW3 1 "register_operand"  "v")
		    (match_operand:V16QI    2 "register_operand"  "v")]
		   UNSPEC_VEC_SLB))]
  "TARGET_VX"
  "vslb\t%v0,%v1,%v2"
  [(set_attr "op_type" "VRR")])

; vec_shr is defined as shift towards element 0
; this means it is a left shift on BE targets!
(define_expand "vec_shr_<mode>"
  [(set (match_dup 3)
	(unspec:V16QI [(match_operand:SI 2 "const_shift_by_byte_operand" "")
		   (const_int 7)
		   (match_dup 3)]
		   UNSPEC_VEC_SET))
   (set (match_operand:V_128 0 "register_operand" "")
	(unspec:V_128 [(match_operand:V_128 1 "register_operand" "")
		    (match_dup 3)]
		   UNSPEC_VEC_SLB))]
  "TARGET_VX"
 {
   operands[3] = gen_reg_rtx(V16QImode);
 })

(define_expand "smin<mode>3"
  [(set (match_operand:VIT           0 "register_operand" "=v")
	(smin:VIT (match_operand:VIT 1 "register_operand"  "v")
		  (match_operand:VIT 2 "register_operand"  "v")))]
  "TARGET_VX"
{
  // Emulate via vec_sel (op1, op2, op2 < op1)
  if ((<MODE>mode == V1TImode || <MODE>mode == TImode) && !TARGET_VXE3)
    {
      rtx lt = gen_reg_rtx (<MODE>mode);
      s390_expand_vec_compare (lt, LT, operands[2], operands[1]);
      emit_insn (gen_vec_sel0<mode> (operands[0], operands[1], operands[2], lt, GEN_INT (0)));
      DONE;
    }
})

; vmnb, vmnh, vmnf, vmng, vmnq
(define_insn "*smin<mode>3"
  [(set (match_operand:VIT_VXE3                0 "register_operand" "=v")
	(smin:VIT_VXE3 (match_operand:VIT_VXE3 1 "register_operand"  "v")
		       (match_operand:VIT_VXE3 2 "register_operand"  "v")))]
  "TARGET_VX"
  "vmn<bhfgq>\t%v0,%v1,%v2"
  [(set_attr "op_type" "VRR")])

(define_expand "smax<mode>3"
  [(set (match_operand:VIT           0 "register_operand" "=v")
	(smax:VIT (match_operand:VIT 1 "register_operand"  "v")
		  (match_operand:VIT 2 "register_operand"  "v")))]
  "TARGET_VX"
{
  // Emulate via vec_sel (op1, op2, op1 < op2)
  if ((<MODE>mode == V1TImode || <MODE>mode == TImode) && !TARGET_VXE3)
    {
      rtx lt = gen_reg_rtx (<MODE>mode);
      s390_expand_vec_compare (lt, LT, operands[1], operands[2]);
      emit_insn (gen_vec_sel0<mode> (operands[0], operands[1], operands[2], lt, GEN_INT (0)));
      DONE;
    }
})

; vmxb, vmxh, vmxf, vmxg, vmxq
(define_insn "*smax<mode>3"
  [(set (match_operand:VIT_VXE3                0 "register_operand" "=v")
	(smax:VIT_VXE3 (match_operand:VIT_VXE3 1 "register_operand"  "v")
		       (match_operand:VIT_VXE3 2 "register_operand"  "v")))]
  "TARGET_VX"
  "vmx<bhfgq>\t%v0,%v1,%v2"
  [(set_attr "op_type" "VRR")])

(define_expand "umin<mode>3"
  [(set (match_operand:VIT           0 "register_operand" "=v")
	(umin:VIT (match_operand:VIT 1 "register_operand"  "v")
		  (match_operand:VIT 2 "register_operand"  "v")))]
  "TARGET_VX"
{
  // Emulate via vec_sel (op1, op2, op2 < op1)
  if ((<MODE>mode == V1TImode || <MODE>mode == TImode) && !TARGET_VXE3)
    {
      rtx ltu = gen_reg_rtx (<MODE>mode);
      s390_expand_vec_compare (ltu, LTU, operands[2], operands[1]);
      emit_insn (gen_vec_sel0<mode> (operands[0], operands[1], operands[2], ltu, GEN_INT (0)));
      DONE;
    }
})

; vmnlb, vmnlh, vmnlf, vmnlg, vmnlq
(define_insn "*umin<mode>3"
  [(set (match_operand:VIT_VXE3                0 "register_operand" "=v")
	(umin:VIT_VXE3 (match_operand:VIT_VXE3 1 "register_operand"  "v")
		       (match_operand:VIT_VXE3 2 "register_operand"  "v")))]
  "TARGET_VX"
  "vmnl<bhfgq>\t%v0,%v1,%v2"
  [(set_attr "op_type" "VRR")])

(define_expand "umax<mode>3"
  [(set (match_operand:VIT           0 "register_operand" "=v")
	(umax:VIT (match_operand:VIT 1 "register_operand"  "v")
		  (match_operand:VIT 2 "register_operand"  "v")))]
  "TARGET_VX"
{
  // Emulate via vec_sel (op1, op2, op1 < op2)
  if ((<MODE>mode == V1TImode || <MODE>mode == TImode) && !TARGET_VXE3)
    {
      rtx ltu = gen_reg_rtx (<MODE>mode);
      s390_expand_vec_compare (ltu, LTU, operands[1], operands[2]);
      emit_insn (gen_vec_sel0<mode> (operands[0], operands[1], operands[2], ltu, GEN_INT (0)));
      DONE;
    }
})

; vmxlb, vmxlh, vmxlf, vmxlg, vmxlq
(define_insn "*umax<mode>3"
  [(set (match_operand:VIT_VXE3                0 "register_operand" "=v")
	(umax:VIT_VXE3 (match_operand:VIT_VXE3 1 "register_operand"  "v")
		       (match_operand:VIT_VXE3 2 "register_operand"  "v")))]
  "TARGET_VX"
  "vmxl<bhfgq>\t%v0,%v1,%v2"
  [(set_attr "op_type" "VRR")])

; vmeb, vmeh, vmef, vmeg
(define_insn "vec_widen_smult_even_<mode>"
  [(set (match_operand:<vec_double>                  0 "register_operand" "=v")
	(unspec:<vec_double> [(match_operand:VI_VXE3 1 "register_operand"  "v")
			      (match_operand:VI_VXE3 2 "register_operand"  "v")]
			     UNSPEC_VEC_SMULT_EVEN))]
  "TARGET_VX"
  "vme<bhfgq>\t%v0,%v1,%v2"
  [(set_attr "op_type" "VRR")])

; vmleb, vmleh, vmlef, vmleg
(define_insn "vec_widen_umult_even_<mode>"
  [(set (match_operand:<vec_double>                  0 "register_operand" "=v")
	(unspec:<vec_double> [(match_operand:VI_VXE3 1 "register_operand"  "v")
			      (match_operand:VI_VXE3 2 "register_operand"  "v")]
			     UNSPEC_VEC_UMULT_EVEN))]
  "TARGET_VX"
  "vmle<bhfgq>\t%v0,%v1,%v2"
  [(set_attr "op_type" "VRR")])

; vmob, vmoh, vmof, vmog
(define_insn "vec_widen_smult_odd_<mode>"
  [(set (match_operand:<vec_double>                  0 "register_operand" "=v")
	(unspec:<vec_double> [(match_operand:VI_VXE3 1 "register_operand"  "v")
			      (match_operand:VI_VXE3 2 "register_operand"  "v")]
			     UNSPEC_VEC_SMULT_ODD))]
  "TARGET_VX"
  "vmo<bhfgq>\t%v0,%v1,%v2"
  [(set_attr "op_type" "VRR")])

; vmlob, vmloh, vmlof, vmlog
(define_insn "vec_widen_umult_odd_<mode>"
  [(set (match_operand:<vec_double>                  0 "register_operand" "=v")
	(unspec:<vec_double> [(match_operand:VI_VXE3 1 "register_operand"  "v")
			      (match_operand:VI_VXE3 2 "register_operand"  "v")]
			     UNSPEC_VEC_UMULT_ODD))]
  "TARGET_VX"
  "vmlo<bhfgq>\t%v0,%v1,%v2"
  [(set_attr "op_type" "VRR")])


; Widening hi/lo multiplications

; The S/390 instructions vml and vmh return the low or high parts of
; the double sized result elements in the corresponding elements of
; the target register.  That's NOT what the vec_widen_umult_lo/hi
; patterns are expected to do.

; We emulate the widening lo/hi multiplies with the even/odd versions
; followed by a vector merge


(define_expand "vec_widen_umult_lo_<mode>"
  [(set (match_dup 3)
	(unspec:<vec_double> [(match_operand:VI_QHS 1 "register_operand" "")
			      (match_operand:VI_QHS 2 "register_operand" "")]
			     UNSPEC_VEC_UMULT_EVEN))
   (set (match_dup 4)
	(unspec:<vec_double> [(match_dup 1) (match_dup 2)]
			     UNSPEC_VEC_UMULT_ODD))
   (set (match_operand:<vec_double>                 0 "register_operand" "")
        (vec_select:<vec_double>
	 (vec_concat:<VEC_2X_WIDE> (match_dup 3) (match_dup 4))
	 (match_dup 5)))]
  "TARGET_VX"
 {
   operands[3] = gen_reg_rtx (<vec_double>mode);
   operands[4] = gen_reg_rtx (<vec_double>mode);
   operands[5] = s390_expand_merge_perm_const (<vec_double>mode, false);
 })

(define_expand "vec_widen_umult_hi_<mode>"
  [(set (match_dup 3)
	(unspec:<vec_double> [(match_operand:VI_QHS 1 "register_operand" "")
			      (match_operand:VI_QHS 2 "register_operand" "")]
			     UNSPEC_VEC_UMULT_EVEN))
   (set (match_dup 4)
	(unspec:<vec_double> [(match_dup 1) (match_dup 2)]
			     UNSPEC_VEC_UMULT_ODD))
   (set (match_operand:<vec_double>                 0 "register_operand" "")
        (vec_select:<vec_double>
	 (vec_concat:<VEC_2X_WIDE> (match_dup 3) (match_dup 4))
	 (match_dup 5)))]
  "TARGET_VX"
 {
   operands[3] = gen_reg_rtx (<vec_double>mode);
   operands[4] = gen_reg_rtx (<vec_double>mode);
   operands[5] = s390_expand_merge_perm_const (<vec_double>mode, true);
 })

(define_expand "vec_widen_smult_lo_<mode>"
  [(set (match_dup 3)
	(unspec:<vec_double> [(match_operand:VI_QHS 1 "register_operand" "")
			      (match_operand:VI_QHS 2 "register_operand" "")]
			     UNSPEC_VEC_SMULT_EVEN))
   (set (match_dup 4)
	(unspec:<vec_double> [(match_dup 1) (match_dup 2)]
			     UNSPEC_VEC_SMULT_ODD))
   (set (match_operand:<vec_double>                 0 "register_operand" "")
        (vec_select:<vec_double>
	 (vec_concat:<VEC_2X_WIDE> (match_dup 3) (match_dup 4))
	 (match_dup 5)))]
  "TARGET_VX"
 {
   operands[3] = gen_reg_rtx (<vec_double>mode);
   operands[4] = gen_reg_rtx (<vec_double>mode);
   operands[5] = s390_expand_merge_perm_const (<vec_double>mode, false);
 })

(define_expand "vec_widen_smult_hi_<mode>"
  [(set (match_dup 3)
	(unspec:<vec_double> [(match_operand:VI_QHS 1 "register_operand" "")
			      (match_operand:VI_QHS 2 "register_operand" "")]
			     UNSPEC_VEC_SMULT_EVEN))
   (set (match_dup 4)
	(unspec:<vec_double> [(match_dup 1) (match_dup 2)]
			     UNSPEC_VEC_SMULT_ODD))
   (set (match_operand:<vec_double>                 0 "register_operand" "")
        (vec_select:<vec_double>
	 (vec_concat:<VEC_2X_WIDE> (match_dup 3) (match_dup 4))
	 (match_dup 5)))]
  "TARGET_VX"
 {
   operands[3] = gen_reg_rtx (<vec_double>mode);
   operands[4] = gen_reg_rtx (<vec_double>mode);
   operands[5] = s390_expand_merge_perm_const (<vec_double>mode, true);
 })

; vec_widen_ushiftl_hi
; vec_widen_ushiftl_lo
; vec_widen_sshiftl_hi
; vec_widen_sshiftl_lo

;;
;; Vector floating point arithmetic instructions
;;

; vfasb, vfadb, wfasb, wfadb, wfaxb
(define_insn "add<mode>3<tf_vr>"
  [(set (match_operand:VF_HW             0 "register_operand" "=v")
	(plus:VF_HW (match_operand:VF_HW 1 "register_operand"  "v")
		    (match_operand:VF_HW 2 "register_operand"  "v")))]
  "TARGET_VX"
  "<vw>fa<sdx>b\t%v0,%v1,%v2"
  [(set_attr "op_type" "VRR")])

(define_expand "addtf3"
  [(match_operand:TF 0 "register_operand"     "")
   (match_operand:TF 1 "nonimmediate_operand" "")
   (match_operand:TF 2 "general_operand"      "")]
  "HAVE_TF (addtf3)"
  { EXPAND_TF (addtf3, 3); })

; vfssb, vfsdb, wfssb, wfsdb, wfsxb
(define_insn "sub<mode>3<tf_vr>"
  [(set (match_operand:VF_HW              0 "register_operand" "=v")
	(minus:VF_HW (match_operand:VF_HW 1 "register_operand"  "v")
		     (match_operand:VF_HW 2 "register_operand"  "v")))]
  "TARGET_VX"
  "<vw>fs<sdx>b\t%v0,%v1,%v2"
  [(set_attr "op_type" "VRR")])

(define_expand "subtf3"
  [(match_operand:TF 0 "register_operand" "")
   (match_operand:TF 1 "register_operand" "")
   (match_operand:TF 2 "general_operand"  "")]
  "HAVE_TF (subtf3)"
  { EXPAND_TF (subtf3, 3); })

; vfmsb, vfmdb, wfmsb, wfmdb, wfmxb
(define_insn "mul<mode>3<tf_vr>"
  [(set (match_operand:VF_HW             0 "register_operand" "=v")
	(mult:VF_HW (match_operand:VF_HW 1 "register_operand"  "v")
		    (match_operand:VF_HW 2 "register_operand"  "v")))]
  "TARGET_VX"
  "<vw>fm<sdx>b\t%v0,%v1,%v2"
  [(set_attr "op_type" "VRR")])

(define_expand "multf3"
  [(match_operand:TF 0 "register_operand"     "")
   (match_operand:TF 1 "nonimmediate_operand" "")
   (match_operand:TF 2 "general_operand"      "")]
  "HAVE_TF (multf3)"
  { EXPAND_TF (multf3, 3); })

; vfdsb, vfddb, wfdsb, wfddb, wfdxb
(define_insn "div<mode>3<tf_vr>"
  [(set (match_operand:VF_HW            0 "register_operand" "=v")
	(div:VF_HW (match_operand:VF_HW 1 "register_operand"  "v")
		   (match_operand:VF_HW 2 "register_operand"  "v")))]
  "TARGET_VX"
  "<vw>fd<sdx>b\t%v0,%v1,%v2"
  [(set_attr "op_type" "VRR")])

(define_expand "divtf3"
  [(match_operand:TF 0 "register_operand" "")
   (match_operand:TF 1 "register_operand" "")
   (match_operand:TF 2 "general_operand"  "")]
  "HAVE_TF (divtf3)"
  { EXPAND_TF (divtf3, 3); })

; vfsqsb, vfsqdb, wfsqsb, wfsqdb, wfsqxb
(define_insn "sqrt<mode>2<tf_vr>"
  [(set (match_operand:VF_HW             0 "register_operand" "=v")
	(sqrt:VF_HW (match_operand:VF_HW 1 "register_operand"  "v")))]
  "TARGET_VX"
  "<vw>fsq<sdx>b\t%v0,%v1"
  [(set_attr "op_type" "VRR")])

(define_expand "sqrttf2"
  [(match_operand:TF 0 "register_operand" "")
   (match_operand:TF 1 "general_operand"  "")]
  "HAVE_TF (sqrttf2)"
  { EXPAND_TF (sqrttf2, 2); })

; vfmasb, vfmadb, wfmasb, wfmadb, wfmaxb
(define_insn "fma<mode>4"
  [(set (match_operand:VF_HW            0 "register_operand" "=v")
	(fma:VF_HW (match_operand:VF_HW 1 "register_operand"  "v")
		   (match_operand:VF_HW 2 "register_operand"  "v")
		   (match_operand:VF_HW 3 "register_operand"  "v")))]
  "TARGET_VX && s390_fma_allowed_p (<MODE>mode)"
  "<vw>fma<sdx>b\t%v0,%v1,%v2,%v3"
  [(set_attr "op_type" "VRR")])

; vfmssb, vfmsdb, wfmssb, wfmsdb, wfmsxb
(define_insn "fms<mode>4"
  [(set (match_operand:VF_HW                     0 "register_operand" "=v")
	(fma:VF_HW (match_operand:VF_HW          1 "register_operand"  "v")
		   (match_operand:VF_HW          2 "register_operand"  "v")
		 (neg:VF_HW (match_operand:VF_HW 3 "register_operand"  "v"))))]
  "TARGET_VX && s390_fma_allowed_p (<MODE>mode)"
  "<vw>fms<sdx>b\t%v0,%v1,%v2,%v3"
  [(set_attr "op_type" "VRR")])

; vfnmasb, vfnmadb, wfnmasb, wfnmadb, wfnmaxb
(define_insn "neg_fma<mode>4"
  [(set (match_operand:VF_HW             0 "register_operand" "=v")
	(neg:VF_HW
	 (fma:VF_HW (match_operand:VF_HW 1 "register_operand"  "v")
		    (match_operand:VF_HW 2 "register_operand"  "v")
		    (match_operand:VF_HW 3 "register_operand"  "v"))))]
  "TARGET_VXE && s390_fma_allowed_p (<MODE>mode)"
  "<vw>fnma<sdx>b\t%v0,%v1,%v2,%v3"
  [(set_attr "op_type" "VRR")])

; vfnmssb, vfnmsdb, wfnmssb, wfnmsdb, wfnmsxb
(define_insn "neg_fms<mode>4"
  [(set (match_operand:VF_HW                      0 "register_operand" "=v")
	(neg:VF_HW
	 (fma:VF_HW (match_operand:VF_HW          1 "register_operand"  "v")
		    (match_operand:VF_HW          2 "register_operand"  "v")
		  (neg:VF_HW (match_operand:VF_HW 3 "register_operand"  "v")))))]
  "TARGET_VXE && s390_fma_allowed_p (<MODE>mode)"
  "<vw>fnms<sdx>b\t%v0,%v1,%v2,%v3"
  [(set_attr "op_type" "VRR")])

; vflcsb, vflcdb, wflcsb, wflcdb, wflcxb
(define_insn "neg<mode>2<tf_vr>"
  [(set (match_operand:VFT          0 "register_operand" "=v")
	(neg:VFT (match_operand:VFT 1 "register_operand"  "v")))]
  "TARGET_VX"
  "<vw>flc<sdx>b\t%v0,%v1"
  [(set_attr "op_type" "VRR")])

(define_expand "negtf2"
  [(match_operand:TF 0 "register_operand" "")
   (match_operand:TF 1 "register_operand" "")]
  "HAVE_TF (negtf2)"
  { EXPAND_TF (negtf2, 2); })

; vflpsb, vflpdb, wflpsb, wflpdb, wflpxb
(define_insn "abs<mode>2<tf_vr>"
  [(set (match_operand:VFT          0 "register_operand" "=v")
	(abs:VFT (match_operand:VFT 1 "register_operand"  "v")))]
  "TARGET_VX"
  "<vw>flp<sdx>b\t%v0,%v1"
  [(set_attr "op_type" "VRR")])

(define_expand "abstf2"
  [(match_operand:TF 0 "register_operand" "")
   (match_operand:TF 1 "register_operand" "")]
  "HAVE_TF (abstf2)"
  { EXPAND_TF (abstf2, 2); })

; vflnsb, vflndb, wflnsb, wflndb, wflnxb
(define_insn "negabs<mode>2"
  [(set (match_operand:VFT                   0 "register_operand" "=v")
	(neg:VFT (abs:VFT (match_operand:VFT 1 "register_operand"  "v"))))]
  "TARGET_VX"
  "<vw>fln<sdx>b\t%v0,%v1"
  [(set_attr "op_type" "VRR")])

(define_expand "smax<mode>3"
  [(set (match_operand:VF_HW             0 "register_operand")
	(smax:VF_HW (match_operand:VF_HW 1 "register_operand")
		    (match_operand:VF_HW 2 "register_operand")))]
  "TARGET_VX")

; vfmaxsb, vfmaxdb, wfmaxsb, wfmaxdb, wfmaxxb
(define_insn "*smax<mode>3_vxe"
  [(set (match_operand:VF_HW             0 "register_operand" "=v")
	(smax:VF_HW (match_operand:VF_HW 1 "register_operand"  "v")
		    (match_operand:VF_HW 2 "register_operand"  "v")))]
  "TARGET_VXE"
  "<vw>fmax<sdx>b\t%v0,%v1,%v2,4"
  [(set_attr "op_type" "VRR")])

; Emulate with compare + select
(define_insn_and_split "*smaxv2df3_vx"
  [(set (match_operand:V2DF            0 "register_operand" "=v")
	(smax:V2DF (match_operand:V2DF 1 "register_operand"  "v")
		   (match_operand:V2DF 2 "register_operand"  "v")))]
  "TARGET_VX && !TARGET_VXE"
  "#"
  "&& 1"
  [(set (match_dup 3)
	(not:V2DI
	 (unge:V2DI (match_dup 2) (match_dup 1))))
   (set (match_dup 0)
	(if_then_else:V2DF
	 (eq (match_dup 3) (match_dup 4))
	 (match_dup 2)
	 (match_dup 1)))]
{
  operands[3] = gen_reg_rtx (V2DImode);
  operands[4] = CONST0_RTX (V2DImode);
})

(define_expand "smin<mode>3"
  [(set (match_operand:VF_HW             0 "register_operand")
	(smin:VF_HW (match_operand:VF_HW 1 "register_operand")
		    (match_operand:VF_HW 2 "register_operand")))]
  "TARGET_VX")

; vfminsb, vfmindb, wfminsb, wfmindb, wfminxb
(define_insn "*smin<mode>3_vxe"
  [(set (match_operand:VF_HW             0 "register_operand" "=v")
	(smin:VF_HW (match_operand:VF_HW 1 "register_operand"  "v")
		    (match_operand:VF_HW 2 "register_operand"  "v")))]
  "TARGET_VXE"
  "<vw>fmin<sdx>b\t%v0,%v1,%v2,4"
  [(set_attr "op_type" "VRR")])

; Emulate with compare + select
(define_insn_and_split "*sminv2df3_vx"
  [(set (match_operand:V2DF            0 "register_operand" "=v")
	(smin:V2DF (match_operand:V2DF 1 "register_operand"  "v")
		   (match_operand:V2DF 2 "register_operand"  "v")))]
  "TARGET_VX && !TARGET_VXE"
  "#"
  "&& 1"
  [(set (match_dup 3)
	(not:V2DI
	 (unge:V2DI (match_dup 2) (match_dup 1))))
   (set (match_dup 0)
	(if_then_else:V2DF
	 (eq (match_dup 3) (match_dup 4))
	 (match_dup 1)
	 (match_dup 2)))]
{
  operands[3] = gen_reg_rtx (V2DImode);
  operands[4] = CONST0_RTX (V2DImode);
})

; Vector copysign, implement using vector select
(define_expand "copysign<mode>3"
  [(set (match_operand:VFT            0 "register_operand" "")
	(ior:VFT
	 (and:VFT (match_operand:VFT  2 "register_operand" "")
		  (match_dup 3))
	 (and:VFT (not:VFT (match_dup 3))
		  (match_operand:VFT  1 "register_operand" ""))))]
  "TARGET_VX"
{
  rtx mask = s390_build_signbit_mask (<MODE>mode);
  operands[3] = force_reg (<MODE>mode, mask);
})

;;
;; Compares
;;

(define_expand "vec_cmp<mode><tointvec>"
  [(set (match_operand:<TOINTVEC>  0 "register_operand" "")
	(match_operator:<TOINTVEC> 1 "vcond_comparison_operator"
	  [(match_operand:V_HW1    2 "register_operand" "")
	   (match_operand:V_HW1    3 "nonmemory_operand" "")]))]
  "TARGET_VX"
{
  s390_expand_vec_compare (operands[0], GET_CODE(operands[1]), operands[2], operands[3]);
  DONE;
})

(define_expand "vec_cmpu<VIT_HW:mode><VIT_HW:mode>"
  [(set (match_operand:VIT_HW    0 "register_operand" "")
	(match_operator:VIT_HW   1 ""
	  [(match_operand:VIT_HW 2 "register_operand" "")
	   (match_operand:VIT_HW 3 "register_operand" "")]))]
  "TARGET_VX"
{
  s390_expand_vec_compare (operands[0], GET_CODE(operands[1]), operands[2], operands[3]);
  DONE;
})

(define_insn "*vec_cmp<VICMP_HW_OP:code><VIT_VXE3:mode><VIT_VXE3:mode>_nocc"
  [(set (match_operand:VIT_VXE3                       2 "register_operand" "=v")
	(VICMP_HW_OP:VIT_VXE3 (match_operand:VIT_VXE3 0 "register_operand"  "v")
			      (match_operand:VIT_VXE3 1 "register_operand"  "v")))]
  "TARGET_VX"
  "vc<VICMP_HW_OP:insn_cmp_op><VIT_VXE3:bhfgq>\t%v2,%v0,%v1"
  [(set_attr "op_type" "VRR")])

(define_insn_and_split "*vec_cmpeq<mode><mode>_nocc_emu"
  [(set (match_operand:VI_HW_T             0 "register_operand" "=v")
	(eq:VI_HW_T (match_operand:VI_HW_T 1 "register_operand"  "v")
		    (match_operand:VI_HW_T 2 "register_operand"  "v")))]
  "TARGET_VX && !TARGET_VXE3"
  "#"
  "&& can_create_pseudo_p ()"
  [(set (match_dup 3)
	(eq:V2DI (match_dup 1) (match_dup 2)))
   (set (match_dup 4)
	(vec_select:V2DI (match_dup 3) (parallel [(const_int 1) (const_int 0)])))
   (set (match_dup 3)
	(and:V2DI (match_dup 3) (match_dup 4)))
   (set (match_dup 0)
	(subreg:<MODE> (match_dup 3) 0))]
{
  operands[1] = simplify_gen_subreg (V2DImode, operands[1], <MODE>mode, 0);
  operands[2] = simplify_gen_subreg (V2DImode, operands[2], <MODE>mode, 0);
  operands[3] = gen_reg_rtx (V2DImode);
  operands[4] = gen_reg_rtx (V2DImode);
})

(define_insn_and_split "*vec_cmpgt<mode><mode>_nocc_emu"
  [(set (match_operand:VI_HW_T             0 "register_operand" "=v")
	(gt:VI_HW_T (match_operand:VI_HW_T 1 "register_operand"  "v")
		    (match_operand:VI_HW_T 2 "register_operand"  "v")))]
  "TARGET_VX && !TARGET_VXE3"
  "#"
  "&& can_create_pseudo_p ()"
  [(set (match_dup 3)
	(gt:V2DI (match_dup 1) (match_dup 2)))
   (set (match_dup 4)
	(eq:V2DI (match_dup 1) (match_dup 2)))
   (set (match_dup 5)
	(gtu:V2DI (match_dup 1) (match_dup 2)))
   (set (match_dup 5)
	(vec_select:V2DI (match_dup 5) (parallel [(const_int 1) (const_int 0)])))
   (set (match_dup 4)
	(and:V2DI (match_dup 4) (match_dup 5)))
   (set (match_dup 4)
	(ior:V2DI (match_dup 3) (match_dup 4)))
   (set (match_dup 4)
	(vec_duplicate:V2DI
	 (vec_select:DI
	  (match_dup 4)
	  (parallel [(const_int 1)]))))
   (set (match_dup 0)
	(subreg:<MODE> (match_dup 4) 0))]
{
  operands[1] = simplify_gen_subreg (V2DImode, operands[1], <MODE>mode, 0);
  operands[2] = simplify_gen_subreg (V2DImode, operands[2], <MODE>mode, 0);
  operands[3] = gen_reg_rtx (V2DImode);
  operands[4] = gen_reg_rtx (V2DImode);
  operands[5] = gen_reg_rtx (V2DImode);
})

(define_insn_and_split "*vec_cmpgtu<mode><mode>_nocc_emu"
  [(set (match_operand:VI_HW_T              0 "register_operand" "=v")
	(gtu:VI_HW_T (match_operand:VI_HW_T 1 "register_operand"  "v")
		     (match_operand:VI_HW_T 2 "register_operand"  "v")))]
  "TARGET_VX"
  "#"
  "&& can_create_pseudo_p ()"
  [(set (match_dup 3)
	(gtu:V2DI (match_dup 1) (match_dup 2)))
   (set (match_dup 4)
	(eq:V2DI (match_dup 1) (match_dup 2)))
   (set (match_dup 5)
	(vec_select:V2DI (match_dup 3) (parallel [(const_int 1) (const_int 0)])))
   (set (match_dup 4)
	(and:V2DI (match_dup 4) (match_dup 5)))
   (set (match_dup 4)
	(ior:V2DI (match_dup 3) (match_dup 4)))
   (set (match_dup 4)
	(vec_duplicate:V2DI
	 (vec_select:DI
	  (match_dup 4)
	  (parallel [(const_int 1)]))))
   (set (match_dup 0)
	(subreg:<MODE> (match_dup 4) 0))]
{
  operands[1] = simplify_gen_subreg (V2DImode, operands[1], <MODE>mode, 0);
  operands[2] = simplify_gen_subreg (V2DImode, operands[2], <MODE>mode, 0);
  operands[3] = gen_reg_rtx (V2DImode);
  operands[4] = gen_reg_rtx (V2DImode);
  operands[5] = gen_reg_rtx (V2DImode);
})


;;
;; Floating point compares
;;

; vfcesb, vfcedb, wfcexb: non-signaling "==" comparison (a == b)
(define_insn "*vec_cmpeq<mode>_quiet_nocc"
  [(set (match_operand:<TOINTVEC>         0 "register_operand" "=v")
	(eq:<TOINTVEC> (match_operand:VFT 1 "register_operand" "v")
		       (match_operand:VFT 2 "register_operand" "v")))]
  "TARGET_VX"
  "<vw>fce<sdx>b\t%v0,%v1,%v2"
  [(set_attr "op_type" "VRR")])

; vfchsb, vfchdb, wfchxb: non-signaling > comparison (!(b u>= a))
(define_insn "vec_cmpgt<mode>_quiet_nocc"
  [(set (match_operand:<TOINTVEC>            0 "register_operand" "=v")
	(not:<TOINTVEC>
	 (unge:<TOINTVEC> (match_operand:VFT 2 "register_operand" "v")
			  (match_operand:VFT 1 "register_operand" "v"))))]
  "TARGET_VX"
  "<vw>fch<sdx>b\t%v0,%v1,%v2"
  [(set_attr "op_type" "VRR")])

(define_expand "vec_cmplt<mode>_quiet_nocc"
  [(set (match_operand:<TOINTVEC>            0 "register_operand" "=v")
	(not:<TOINTVEC>
	 (unge:<TOINTVEC> (match_operand:VFT 1 "register_operand" "v")
			  (match_operand:VFT 2 "register_operand" "v"))))]
  "TARGET_VX")

; vfchesb, vfchedb, wfchexb: non-signaling >= comparison (!(a u< b))
(define_insn "vec_cmpge<mode>_quiet_nocc"
  [(set (match_operand:<TOINTVEC>            0 "register_operand" "=v")
	(not:<TOINTVEC>
	 (unlt:<TOINTVEC> (match_operand:VFT 1 "register_operand" "v")
			  (match_operand:VFT 2 "register_operand" "v"))))]
  "TARGET_VX"
  "<vw>fche<sdx>b\t%v0,%v1,%v2"
  [(set_attr "op_type" "VRR")])

(define_expand "vec_cmple<mode>_quiet_nocc"
  [(set (match_operand:<TOINTVEC>            0 "register_operand" "=v")
	(not:<TOINTVEC>
	 (unlt:<TOINTVEC> (match_operand:VFT 2 "register_operand" "v")
			  (match_operand:VFT 1 "register_operand" "v"))))]
  "TARGET_VX")

; vfkesb, vfkedb, wfkexb: signaling == comparison ((a >= b) & (b >= a))
(define_insn "*vec_cmpeq<mode>_signaling_nocc"
  [(set (match_operand:<TOINTVEC>          0 "register_operand" "=v")
	(and:<TOINTVEC>
	 (ge:<TOINTVEC> (match_operand:VFT 1 "register_operand" "v")
			(match_operand:VFT 2 "register_operand" "v"))
	 (ge:<TOINTVEC> (match_dup         2)
			(match_dup         1))))]
  "TARGET_VXE"
  "<vw>fke<sdx>b\t%v0,%v1,%v2"
  [(set_attr "op_type" "VRR")])

; vfkhsb, vfkhdb, wfkhxb: signaling > comparison (a > b)
(define_insn "*vec_cmpgt<mode>_signaling_nocc"
  [(set (match_operand:<TOINTVEC>         0 "register_operand" "=v")
	(gt:<TOINTVEC> (match_operand:VFT 1 "register_operand" "v")
		       (match_operand:VFT 2 "register_operand" "v")))]
  "TARGET_VXE"
  "<vw>fkh<sdx>b\t%v0,%v1,%v2"
  [(set_attr "op_type" "VRR")])

(define_insn "*vec_cmpgt<mode>_signaling_finite_nocc"
  [(set (match_operand:<TOINTVEC>         0 "register_operand" "=v")
	(gt:<TOINTVEC> (match_operand:VFT 1 "register_operand" "v")
		       (match_operand:VFT 2 "register_operand" "v")))]
  "TARGET_NONSIGNALING_VECTOR_COMPARE_OK"
  "<vw>fch<sdx>b\t%v0,%v1,%v2"
  [(set_attr "op_type" "VRR")])

; vfkhesb, vfkhedb, wfkhexb: signaling >= comparison (a >= b)
(define_insn "*vec_cmpge<mode>_signaling_nocc"
  [(set (match_operand:<TOINTVEC>         0 "register_operand" "=v")
	(ge:<TOINTVEC> (match_operand:VFT 1 "register_operand" "v")
		       (match_operand:VFT 2 "register_operand" "v")))]
  "TARGET_VXE"
  "<vw>fkhe<sdx>b\t%v0,%v1,%v2"
  [(set_attr "op_type" "VRR")])

(define_insn "*vec_cmpge<mode>_signaling_finite_nocc"
  [(set (match_operand:<TOINTVEC>         0 "register_operand" "=v")
	(ge:<TOINTVEC> (match_operand:VFT 1 "register_operand" "v")
		       (match_operand:VFT 2 "register_operand" "v")))]
  "TARGET_NONSIGNALING_VECTOR_COMPARE_OK"
  "<vw>fche<sdx>b\t%v0,%v1,%v2"
  [(set_attr "op_type" "VRR")])

; Expanders for not directly supported comparisons
; Signaling comparisons must be expressed via signaling rtxes only,
; and quiet comparisons must be expressed via quiet rtxes only.

; UNGT a u> b -> !!(b u< a)
(define_expand "vec_cmpungt<mode>"
  [(set (match_operand:<TOINTVEC>            0 "register_operand" "=v")
	(not:<TOINTVEC>
	 (unlt:<TOINTVEC> (match_operand:VFT 2 "register_operand" "v")
			  (match_operand:VFT 1 "register_operand" "v"))))
   (set (match_dup                           0)
	(not:<TOINTVEC> (match_dup           0)))]
  "TARGET_VX")

; UNGE a u>= b -> !!(a u>= b)
(define_expand "vec_cmpunge<mode>"
  [(set (match_operand:<TOINTVEC>            0 "register_operand" "=v")
	(not:<TOINTVEC>
	 (unge:<TOINTVEC> (match_operand:VFT 1 "register_operand" "v")
			  (match_operand:VFT 2 "register_operand" "v"))))
   (set (match_dup                           0)
	(not:<TOINTVEC> (match_dup           0)))]
  "TARGET_VX")

; UNEQ a u== b -> !(!(a u>= b) | !(b u>= a))
(define_expand "vec_cmpuneq<mode>"
  [(set (match_operand:<TOINTVEC>            0 "register_operand" "=v")
	(not:<TOINTVEC>
	 (unge:<TOINTVEC> (match_operand:VFT 1 "register_operand"  "v")
		          (match_operand:VFT 2 "register_operand"  "v"))))
   (set (match_dup                           3)
	(not:<TOINTVEC>
	 (unge:<TOINTVEC> (match_dup         2)
	                  (match_dup         1))))
   (set (match_dup                           0)
	(ior:<TOINTVEC> (match_dup           0)
			(match_dup           3)))
   (set (match_dup                           0)
	(not:<TOINTVEC> (match_dup           0)))]
  "TARGET_VX"
{
  operands[3] = gen_reg_rtx (<TOINTVEC>mode);
})

; LTGT a <> b -> a > b | b > a
(define_expand "vec_cmpltgt<mode>"
  [(set (match_operand:<TOINTVEC>         0 "register_operand" "=v")
	(gt:<TOINTVEC> (match_operand:VFT 1 "register_operand"  "v")
		    (match_operand:VFT 2 "register_operand"  "v")))
   (set (match_dup 3) (gt:<TOINTVEC> (match_dup 2) (match_dup 1)))
   (set (match_dup 0) (ior:<TOINTVEC> (match_dup 0) (match_dup 3)))]
  "TARGET_VXE"
{
  operands[3] = gen_reg_rtx (<TOINTVEC>mode);
})

; ORDERED (a, b): !(a u< b) | !(a u>= b)
(define_expand "vec_cmpordered<mode>"
  [(set (match_operand:<TOINTVEC>            0 "register_operand" "=v")
	(not:<TOINTVEC>
	 (unlt:<TOINTVEC> (match_operand:VFT 1 "register_operand" "v")
		          (match_operand:VFT 2 "register_operand" "v"))))
   (set (match_dup                           3)
	(not:<TOINTVEC>
	 (unge:<TOINTVEC> (match_dup         1)
			  (match_dup         2))))
   (set (match_dup                           0)
	(ior:<TOINTVEC> (match_dup           0)
			(match_dup           3)))]
  "TARGET_VX"
{
  operands[3] = gen_reg_rtx (<TOINTVEC>mode);
})

; UNORDERED (a, b): !ORDERED (a, b)
(define_expand "vec_cmpunordered<mode>"
  [(match_operand:<TOINTVEC> 0 "register_operand" "=v")
   (match_operand:VFT        1 "register_operand" "v")
   (match_operand:VFT        2 "register_operand" "v")]
  "TARGET_VX"
{
  emit_insn (gen_vec_cmpordered<mode> (operands[0], operands[1], operands[2]));
  emit_insn (gen_rtx_SET (operands[0],
	     gen_rtx_NOT (<TOINTVEC>mode, operands[0])));
  DONE;
})

(define_code_iterator VEC_CMP_EXPAND
  [ungt unge uneq ltgt ordered unordered])

(define_expand "vec_cmp<code>"
  [(match_operand 0 "register_operand" "")
   (VEC_CMP_EXPAND (match_operand 1 "register_operand" "")
                   (match_operand 2 "register_operand" ""))]
  "TARGET_VX"
{
  if (GET_MODE (operands[1]) == V4SFmode)
    emit_insn (gen_vec_cmp<code>v4sf (operands[0], operands[1], operands[2]));
  else if (GET_MODE (operands[1]) == V2DFmode)
    emit_insn (gen_vec_cmp<code>v2df (operands[0], operands[1], operands[2]));
  else
    gcc_unreachable ();

  DONE;
})

(define_insn "*vec_load_pair<mode>"
  [(set (match_operand:V_HW_2                       0 "register_operand" "=v,v")
	(vec_concat:V_HW_2 (match_operand:<non_vec> 1 "register_operand"  "d,v")
			   (match_operand:<non_vec> 2 "register_operand"  "d,v")))]
  "TARGET_VX"
  "@
   vlvgp\t%v0,%1,%2
   vmrhg\t%v0,%v1,%v2"
  [(set_attr "op_type" "VRR,VRR")])

(define_insn "vllv16qi"
  [(set (match_operand:V16QI              0 "register_operand" "=v")
	(unspec:V16QI [(match_operand:SI  1 "register_operand"  "d")
		       (match_operand:BLK 2 "memory_operand"    "Q")]
		      UNSPEC_VEC_LOAD_LEN))]
  "TARGET_VX"
  "vll\t%v0,%1,%2"
  [(set_attr "op_type" "VRS")])

; vfeebs, vfeehs, vfeefs
; vfeezbs, vfeezhs, vfeezfs
(define_insn "@vec_vfees<mode>"
  [(set (match_operand:VI_HW_QHS 0 "register_operand" "=v")
	(unspec:VI_HW_QHS [(match_operand:VI_HW_QHS 1 "register_operand" "v")
			   (match_operand:VI_HW_QHS 2 "register_operand" "v")
			   (match_operand:QI 3 "const_mask_operand" "C")]
			  UNSPEC_VEC_VFEE))
   (set (reg:CCRAW CC_REGNUM)
	(unspec:CCRAW [(match_dup 1)
		       (match_dup 2)
		       (match_dup 3)]
		      UNSPEC_VEC_VFEECC))]
  "TARGET_VX"
{
  unsigned HOST_WIDE_INT flags = UINTVAL (operands[3]);

  gcc_assert (!(flags & ~(VSTRING_FLAG_ZS | VSTRING_FLAG_CS)));
  flags &= ~VSTRING_FLAG_CS;

  if (flags == VSTRING_FLAG_ZS)
    return "vfeez<bhfgq>s\t%v0,%v1,%v2";
  return "vfee<bhfgq>s\t%v0,%v1,%v2";
}
  [(set_attr "op_type" "VRR")])

; vfenebs, vfenehs, vfenefs
; vfenezbs, vfenezhs, vfenezfs
(define_insn "vec_vfenes<mode>"
  [(set (match_operand:VI_HW_QHS 0 "register_operand" "=v")
	(unspec:VI_HW_QHS [(match_operand:VI_HW_QHS 1 "register_operand" "v")
			   (match_operand:VI_HW_QHS 2 "register_operand" "v")
			   (match_operand:QI 3 "const_mask_operand" "C")]
			  UNSPEC_VEC_VFENE))
   (set (reg:CCRAW CC_REGNUM)
	(unspec:CCRAW [(match_dup 1)
		       (match_dup 2)
		       (match_dup 3)]
		      UNSPEC_VEC_VFENECC))]
  "TARGET_VX"
{
  unsigned HOST_WIDE_INT flags = UINTVAL (operands[3]);

  gcc_assert (!(flags & ~(VSTRING_FLAG_ZS | VSTRING_FLAG_CS)));
  flags &= ~VSTRING_FLAG_CS;

  if (flags == VSTRING_FLAG_ZS)
    return "vfenez<bhfgq>s\t%v0,%v1,%v2";
  return "vfene<bhfgq>s\t%v0,%v1,%v2";
}
  [(set_attr "op_type" "VRR")])


; Vector select

; The following splitters simplify vec_sel for constant 0 or -1
; selection sources.  This is required to generate efficient code for
; vcond.

; a = b == c;
(define_split
  [(set (match_operand:V 0 "register_operand" "")
	(if_then_else:V
	 (eq (match_operand:<TOINTVEC> 3 "register_operand" "")
	     (match_operand:V 4 "const0_operand" ""))
	 (match_operand:V 1 "const0_operand" "")
	 (match_operand:V 2 "all_ones_operand" "")))]
  "TARGET_VX"
  [(set (match_dup 0) (match_dup 3))]
{
  PUT_MODE (operands[3], <V:MODE>mode);
})

; a = ~(b == c)
(define_split
  [(set (match_operand:V 0 "register_operand" "")
	(if_then_else:V
	 (eq (match_operand:<TOINTVEC> 3 "register_operand" "")
	     (match_operand:V 4 "const0_operand" ""))
	 (match_operand:V 1 "all_ones_operand" "")
	 (match_operand:V 2 "const0_operand" "")))]
  "TARGET_VX"
  [(set (match_dup 0) (not:V (match_dup 3)))]
{
  PUT_MODE (operands[3], <V:MODE>mode);
})

; a = b != c
(define_split
  [(set (match_operand:V 0 "register_operand" "")
	(if_then_else:V
	 (ne (match_operand:<TOINTVEC> 3 "register_operand" "")
	     (match_operand:V 4 "const0_operand" ""))
	 (match_operand:V 1 "all_ones_operand" "")
	 (match_operand:V 2 "const0_operand" "")))]
  "TARGET_VX"
  [(set (match_dup 0) (match_dup 3))]
{
  PUT_MODE (operands[3], <V:MODE>mode);
})

; a = ~(b != c)
(define_split
  [(set (match_operand:V 0 "register_operand" "")
	(if_then_else:V
	 (ne (match_operand:<TOINTVEC> 3 "register_operand" "")
	     (match_operand:V 4 "const0_operand" ""))
	 (match_operand:V 1 "const0_operand" "")
	 (match_operand:V 2 "all_ones_operand" "")))]
  "TARGET_VX"
  [(set (match_dup 0) (not:V (match_dup 3)))]
{
  PUT_MODE (operands[3], <V:MODE>mode);
})

; op0 = op3 == 0 ? op1 : op2
(define_insn "vec_sel0<mode>"
  [(set (match_operand:VT 0 "register_operand" "=v")
	(if_then_else:VT
	 (eq (match_operand:<TOINTVEC> 3 "register_operand" "v")
	     (match_operand:<TOINTVEC> 4 "const0_operand" ""))
	 (match_operand:VT 1 "register_operand" "v")
	 (match_operand:VT 2 "register_operand" "v")))]
  "TARGET_VX"
  "vsel\t%v0,%2,%1,%3"
  [(set_attr "op_type" "VRR")])

; op0 = !op3 == 0 ? op1 : op2
(define_insn "*vec_sel0<mode>"
  [(set (match_operand:V 0 "register_operand" "=v")
	(if_then_else:V
	 (eq (not:<TOINTVEC> (match_operand:<TOINTVEC> 3 "register_operand" "v"))
	     (match_operand:<TOINTVEC> 4 "const0_operand" ""))
	 (match_operand:V 1 "register_operand" "v")
	 (match_operand:V 2 "register_operand" "v")))]
  "TARGET_VX"
  "vsel\t%v0,%1,%2,%3"
  [(set_attr "op_type" "VRR")])

; op0 = op3 == -1 ? op1 : op2
(define_insn "*vec_sel1<mode>"
  [(set (match_operand:V 0 "register_operand" "=v")
	(if_then_else:V
	 (eq (match_operand:<TOINTVEC> 3 "register_operand" "v")
	     (match_operand:<TOINTVEC> 4 "all_ones_operand" ""))
	 (match_operand:V 1 "register_operand" "v")
	 (match_operand:V 2 "register_operand" "v")))]
  "TARGET_VX"
  "vsel\t%v0,%1,%2,%3"
  [(set_attr "op_type" "VRR")])

; op0 = !op3 == -1 ? op1 : op2
(define_insn "*vec_sel1<mode>"
  [(set (match_operand:V 0 "register_operand" "=v")
	(if_then_else:V
	 (eq (not:<TOINTVEC> (match_operand:<TOINTVEC> 3 "register_operand" "v"))
	     (match_operand:<TOINTVEC> 4 "all_ones_operand" ""))
	 (match_operand:V 1 "register_operand" "v")
	 (match_operand:V 2 "register_operand" "v")))]
  "TARGET_VX"
  "vsel\t%v0,%2,%1,%3"
  [(set_attr "op_type" "VRR")])

; vec_pack_trunc

; vpkh, vpkf, vpkg
(define_insn "vec_pack_trunc_<mode>"
  [(set (match_operand:<vec_half> 0 "register_operand" "=v")
	(vec_concat:<vec_half>
	 (truncate:<vec_halfhalf>
	  (match_operand:VI_HW_HSD 1 "register_operand" "v"))
	 (truncate:<vec_halfhalf>
	  (match_operand:VI_HW_HSD 2 "register_operand" "v"))))]
  "TARGET_VX"
  "vpk<bhfgq>\t%0,%1,%2"
  [(set_attr "op_type" "VRR")])

; vpksh, vpksf, vpksg
(define_insn "vec_pack_ssat_<mode>"
  [(set (match_operand:<vec_half> 0 "register_operand" "=v")
	(vec_concat:<vec_half>
	 (ss_truncate:<vec_halfhalf>
	  (match_operand:VI_HW_HSD 1 "register_operand" "v"))
	 (ss_truncate:<vec_halfhalf>
	  (match_operand:VI_HW_HSD 2 "register_operand" "v"))))]
  "TARGET_VX"
  "vpks<bhfgq>\t%0,%1,%2"
  [(set_attr "op_type" "VRR")])

; vpklsh, vpklsf, vpklsg
(define_insn "vec_pack_usat_<mode>"
  [(set (match_operand:<vec_half> 0 "register_operand" "=v")
	(vec_concat:<vec_half>
	 (us_truncate:<vec_halfhalf>
	  (match_operand:VI_HW_HSD 1 "register_operand" "v"))
	 (us_truncate:<vec_halfhalf>
	  (match_operand:VI_HW_HSD 2 "register_operand" "v"))))]
  "TARGET_VX"
  "vpkls<bhfgq>\t%0,%1,%2"
  [(set_attr "op_type" "VRR")])

;; vector unpack / extend

(define_insn "<extend_insn><VI_EXTEND:mode><vec_2x_wide>2"
  [(set (match_operand:<VEC_2X_WIDE> 0 "register_operand" "=v")
	(any_extend:<VEC_2X_WIDE>
	 (match_operand:VI_EXTEND 1 "register_operand" "v")))]
  "TARGET_VX"
  "vup<zero_extend>h<bhfgq>\t%0,%1"
  [(set_attr "op_type" "VRR")])

(define_expand "extendv2sfv2df2"
  [(set (match_dup 2)
	(vec_select:V4SF
	 (vec_concat:V4SF (match_operand:V2SF 1 "register_operand")
			  (match_dup 1))
	 (parallel [(const_int 0) (const_int 2)
		    (const_int 1) (const_int 3)])))
   (set (match_operand:V2DF 0 "register_operand")
	(float_extend:V2DF
	 (vec_select:V2SF
	  (match_dup 2)
	  (parallel [(const_int 0) (const_int 2)]))))]
  "TARGET_VX"
{
  operands[2] = gen_reg_rtx (V4SFmode);
})

;; vector unpack v16qi

; signed

(define_insn "vec_unpacks_hi_v16qi"
  [(set (match_operand:V8HI 0 "register_operand" "=v")
	(sign_extend:V8HI
	 (vec_select:V8QI
	  (match_operand:V16QI 1 "register_operand" "v")
	  (parallel [(const_int 0)(const_int 1)(const_int 2)(const_int 3)
		     (const_int 4)(const_int 5)(const_int 6)(const_int 7)]))))]
  "TARGET_VX"
  "vuphb\t%0,%1"
  [(set_attr "op_type" "VRR")])

(define_insn "vec_unpacks_lo_v16qi"
  [(set (match_operand:V8HI 0 "register_operand" "=v")
	(sign_extend:V8HI
	 (vec_select:V8QI
	  (match_operand:V16QI 1 "register_operand" "v")
	  (parallel [(const_int 8) (const_int 9) (const_int 10)(const_int 11)
		     (const_int 12)(const_int 13)(const_int 14)(const_int 15)]))))]
  "TARGET_VX"
  "vuplb\t%0,%1"
  [(set_attr "op_type" "VRR")])

; unsigned

(define_insn "vec_unpacku_hi_v16qi"
  [(set (match_operand:V8HI 0 "register_operand" "=v")
	(zero_extend:V8HI
	 (vec_select:V8QI
	  (match_operand:V16QI 1 "register_operand" "v")
	  (parallel [(const_int 0)(const_int 1)(const_int 2)(const_int 3)
		     (const_int 4)(const_int 5)(const_int 6)(const_int 7)]))))]
  "TARGET_VX"
  "vuplhb\t%0,%1"
  [(set_attr "op_type" "VRR")])

(define_insn "vec_unpacku_lo_v16qi"
  [(set (match_operand:V8HI 0 "register_operand" "=v")
	(zero_extend:V8HI
	 (vec_select:V8QI
	  (match_operand:V16QI 1 "register_operand" "v")
	  (parallel [(const_int 8) (const_int 9) (const_int 10)(const_int 11)
		     (const_int 12)(const_int 13)(const_int 14)(const_int 15)]))))]
  "TARGET_VX"
  "vupllb\t%0,%1"
  [(set_attr "op_type" "VRR")])

;; vector unpack v8hi

; signed

(define_insn "vec_unpacks_hi_v8hi"
  [(set (match_operand:V4SI 0 "register_operand" "=v")
	(sign_extend:V4SI
	 (vec_select:V4HI
	  (match_operand:V8HI 1 "register_operand" "v")
	  (parallel [(const_int 0)(const_int 1)(const_int 2)(const_int 3)]))))]
  "TARGET_VX"
  "vuphh\t%0,%1"
  [(set_attr "op_type" "VRR")])

(define_insn "vec_unpacks_lo_v8hi"
  [(set (match_operand:V4SI 0 "register_operand" "=v")
	(sign_extend:V4SI
	 (vec_select:V4HI
	  (match_operand:V8HI 1 "register_operand" "v")
	  (parallel [(const_int 4)(const_int 5)(const_int 6)(const_int 7)]))))]
  "TARGET_VX"
  "vuplhw\t%0,%1"
  [(set_attr "op_type" "VRR")])

; unsigned

(define_insn "vec_unpacku_hi_v8hi"
  [(set (match_operand:V4SI 0 "register_operand" "=v")
	(zero_extend:V4SI
	 (vec_select:V4HI
	  (match_operand:V8HI 1 "register_operand" "v")
	  (parallel [(const_int 0)(const_int 1)(const_int 2)(const_int 3)]))))]
  "TARGET_VX"
  "vuplhh\t%0,%1"
  [(set_attr "op_type" "VRR")])

(define_insn "vec_unpacku_lo_v8hi"
  [(set (match_operand:V4SI 0 "register_operand" "=v")
	(zero_extend:V4SI
	 (vec_select:V4HI
	  (match_operand:V8HI 1 "register_operand" "v")
	  (parallel [(const_int 4)(const_int 5)(const_int 6)(const_int 7)]))))]
  "TARGET_VX"
  "vupllh\t%0,%1"
  [(set_attr "op_type" "VRR")])

;; vector unpack v4si

; signed

(define_insn "vec_unpacks_hi_v4si"
  [(set (match_operand:V2DI 0 "register_operand" "=v")
	(sign_extend:V2DI
	 (vec_select:V2SI
	  (match_operand:V4SI 1 "register_operand" "v")
	  (parallel [(const_int 0)(const_int 1)]))))]
  "TARGET_VX"
  "vuphf\t%0,%1"
  [(set_attr "op_type" "VRR")])

(define_insn "vec_unpacks_lo_v4si"
  [(set (match_operand:V2DI 0 "register_operand" "=v")
	(sign_extend:V2DI
	 (vec_select:V2SI
	  (match_operand:V4SI 1 "register_operand" "v")
	  (parallel [(const_int 2)(const_int 3)]))))]
  "TARGET_VX"
  "vuplf\t%0,%1"
  [(set_attr "op_type" "VRR")])

; unsigned

(define_insn "vec_unpacku_hi_v4si"
  [(set (match_operand:V2DI 0 "register_operand" "=v")
	(zero_extend:V2DI
	 (vec_select:V2SI
	  (match_operand:V4SI 1 "register_operand" "v")
	  (parallel [(const_int 0)(const_int 1)]))))]
  "TARGET_VX"
  "vuplhf\t%0,%1"
  [(set_attr "op_type" "VRR")])

(define_insn "vec_unpacku_lo_v4si"
  [(set (match_operand:V2DI 0 "register_operand" "=v")
	(zero_extend:V2DI
	 (vec_select:V2SI
	  (match_operand:V4SI 1 "register_operand" "v")
	  (parallel [(const_int 2)(const_int 3)]))))]
  "TARGET_VX"
  "vupllf\t%0,%1"
  [(set_attr "op_type" "VRR")])

;; vector load lengthened

; vflls float -> double
(define_insn "*vec_extendv4sf"
  [(set (match_operand:V2DF 0 "register_operand" "=v")
	(float_extend:V2DF
	 (vec_select:V2SF
	  (match_operand:V4SF 1 "register_operand" "v")
	  (parallel [(const_int 0) (const_int 2)]))))]
  "TARGET_VX"
  "vldeb\t%v0,%v1"
  [(set_attr "op_type" "VRR")])

(define_expand "vec_unpacks_lo_v4sf"
  [(set (match_dup 2)
        (vec_select:V4SF
	 (vec_concat:V8SF (match_operand:V4SF 1 "register_operand" "") (match_dup 1))
	 (match_dup 3)))
   (set (match_operand:V2DF                   0 "register_operand" "")
	(float_extend:V2DF
	 (vec_select:V2SF
	  (match_dup 2)
	  (parallel [(const_int 0) (const_int 2)]))))]
  "TARGET_VX"
{
  operands[2] = gen_reg_rtx(V4SFmode);
  operands[3] = s390_expand_merge_perm_const (V4SFmode, false);
})

(define_expand "vec_unpacks_hi_v4sf"
  [(set (match_dup 2)
        (vec_select:V4SF
	 (vec_concat:V8SF (match_operand:V4SF 1 "register_operand" "") (match_dup 1))
	 (match_dup 3)))
   (set (match_operand:V2DF                   0 "register_operand" "")
	(float_extend:V2DF
	 (vec_select:V2SF
	  (match_dup 2)
	  (parallel [(const_int 0) (const_int 2)]))))]
  "TARGET_VX"
{
  operands[2] = gen_reg_rtx(V4SFmode);
  operands[3] = s390_expand_merge_perm_const (V4SFmode, true);
})


; double -> long double
(define_insn "*vec_extendv2df"
  [(set (match_operand:V1TF 0 "register_operand" "=v")
	(float_extend:V1TF
	 (vec_select:V1DF
	  (match_operand:V2DF 1 "register_operand" "v")
	  (parallel [(const_int 0)]))))]
  "TARGET_VXE"
  "wflld\t%v0,%v1"
  [(set_attr "op_type" "VRR")])

(define_expand "vec_unpacks_lo_v2df"
  [(set (match_dup 2)
        (vec_select:V2DF
	 (vec_concat:V4DF (match_operand:V2DF 1 "register_operand" "") (match_dup 1))
	 (match_dup 3)))
   (set (match_operand:V1TF                   0 "register_operand" "")
	(float_extend:V1TF
	 (vec_select:V1DF
	  (match_dup 2)
	  (parallel [(const_int 0)]))))]
  "TARGET_VXE"
{
  operands[2] = gen_reg_rtx (V2DFmode);
  operands[3] = s390_expand_merge_perm_const (V2DFmode, false);
})

(define_expand "vec_unpacks_hi_v2df"
  [(set (match_dup 2)
        (vec_select:V2DF
	 (vec_concat:V4DF (match_operand:V2DF 1 "register_operand" "") (match_dup 1))
	 (match_dup 3)))
   (set (match_operand:V1TF                   0 "register_operand" "")
	(float_extend:V1TF
	 (vec_select:V1DF
	  (match_dup 2)
	  (parallel [(const_int 0)]))))]
  "TARGET_VXE"
{
  operands[2] = gen_reg_rtx (V2DFmode);
  operands[3] = s390_expand_merge_perm_const (V2DFmode, true);
})


; 2 x v2df -> 1 x v4sf
(define_expand "vec_pack_trunc_v2df"
  [(set (match_dup 3)
	(unspec:V4SF [(match_operand:V2DF 1 "register_operand" "")
		      (const_int VEC_INEXACT)
		      (const_int VEC_RND_CURRENT)]
		     UNSPEC_VEC_VFLR))
   (set (match_dup 4)
	(unspec:V4SF [(match_operand:V2DF 2 "register_operand" "")
		      (const_int VEC_INEXACT)
		      (const_int VEC_RND_CURRENT)]
		     UNSPEC_VEC_VFLR))
   (set (match_dup 6)
	(unspec:V16QI [(subreg:V16QI (match_dup 3) 0)
		       (subreg:V16QI (match_dup 4) 0)
		       (match_dup 5)]
		      UNSPEC_VEC_PERM))
   (set (match_operand:V4SF 0 "register_operand" "")
	(subreg:V4SF (match_dup 6) 0))]
  "TARGET_VX"
{
  rtx constv, perm[16];
  int i;

  for (i = 0; i < 4; ++i)
    {
      perm[i] = GEN_INT (i);
      perm[i + 4] = GEN_INT (i + 8);
      perm[i + 8] = GEN_INT (i + 16);
      perm[i + 12] = GEN_INT (i + 24);
    }
  constv = gen_rtx_CONST_VECTOR (V16QImode, gen_rtvec_v (16, perm));

  operands[3] = gen_reg_rtx (V4SFmode);
  operands[4] = gen_reg_rtx (V4SFmode);
  operands[5] = force_reg (V16QImode, constv);
  operands[6] = gen_reg_rtx (V16QImode);
})

;
; BFP <-> integer conversions
;

; signed integer to floating point

; op2: inexact exception not suppressed (IEEE 754 2008)
; op3: according to current rounding mode
; vcdgb, vcefb
(define_insn "float<tointvec><mode>2"
  [(set (match_operand:VX_VEC_CONV_BFP                   0 "register_operand" "=v")
	(float:VX_VEC_CONV_BFP (match_operand:<TOINTVEC> 1 "register_operand"  "v")))]
  "TARGET_VX"
  "vc<xde><bhfgq>b\t%v0,%v1,0,0"
  [(set_attr "op_type" "VRR")])

; There is no instruction for loading a signed integer into an extended BFP
; operand in a VR, therefore we need to load it into a FPR pair first.
(define_expand "float<mode>tf2_vr"
  [(set (match_dup 2)
	(float:FPRX2 (match_operand:DSI 1 "register_operand" "")))
   (set (match_operand:TF               0 "register_operand" "")
	(subreg:TF (match_dup 2) 0))]
  "TARGET_VXE"
{
  operands[2] = gen_reg_rtx (FPRX2mode);
})

(define_expand "float<mode>tf2"
  [(match_operand:TF  0 "register_operand" "")
   (match_operand:DSI 1 "register_operand" "")]
  "HAVE_TF (float<mode>tf2)"
  { EXPAND_TF (float<mode>tf2, 2); })

; unsigned integer to floating point

; op2: inexact exception not suppressed (IEEE 754 2008)
; op3: according to current rounding mode
; vcdlgb, vcelfb
(define_insn "floatuns<tointvec><mode>2"
  [(set (match_operand:VX_VEC_CONV_BFP                            0 "register_operand" "=v")
	(unsigned_float:VX_VEC_CONV_BFP (match_operand:<TOINTVEC> 1 "register_operand"  "v")))]
  "TARGET_VX"
  "vc<xde>l<bhfgq>b\t%v0,%v1,0,0"
  [(set_attr "op_type" "VRR")])

; There is no instruction for loading an unsigned integer into an extended BFP
; operand in a VR, therefore load it into a FPR pair first.
(define_expand "floatuns<mode>tf2_vr"
  [(set (match_dup 2)
	(unsigned_float:FPRX2 (match_operand:GPR 1 "register_operand" "")))
   (set (match_operand:TF                        0 "register_operand" "")
	(subreg:TF (match_dup 2) 0))]
  "TARGET_VXE"
{
  operands[2] = gen_reg_rtx (FPRX2mode);
})

(define_expand "floatuns<mode>tf2"
  [(match_operand:TF  0 "register_operand" "")
   (match_operand:GPR 1 "register_operand" "")]
  "HAVE_TF (floatuns<mode>tf2)"
  { EXPAND_TF (floatuns<mode>tf2, 2); })

; floating point to signed integer

; op2: inexact exception not suppressed (IEEE 754 2008)
; op3: rounding mode 5 (round towards 0 C11 6.3.1.4)
; vcgdb, vcfeb
(define_insn "fix_trunc<mode><tointvec>2"
  [(set (match_operand:<TOINTVEC>                      0 "register_operand" "=v")
	(fix:<TOINTVEC> (match_operand:VX_VEC_CONV_BFP 1 "register_operand"  "v")))]
  "TARGET_VX"
  "vc<bhfgq><xde>b\t%v0,%v1,0,5"
  [(set_attr "op_type" "VRR")])

; There is no instruction for rounding an extended BFP operand in a VR into
; a signed integer, therefore copy it into a FPR pair first.
(define_expand "fix_trunctf<mode>2_vr"
  [(set (match_dup 2)
	(unspec:FPRX2 [(match_operand:TF 1 "register_operand")] UNSPEC_TF_TO_FPRX2))
   (parallel [(set (match_operand:GPR 0 "register_operand" "")
		   (fix:GPR (match_dup 2)))
	      (unspec:GPR [(const_int BFP_RND_TOWARD_0)] UNSPEC_ROUND)
	      (clobber (reg:CC CC_REGNUM))])]
  "TARGET_VXE"
{
  operands[2] = gen_reg_rtx (FPRX2mode);
})

(define_expand "fix_trunctf<mode>2"
  [(match_operand:GPR 0 "register_operand" "")
   (match_operand:TF  1 "register_operand" "")]
  "HAVE_TF (fix_trunctf<mode>2)"
  { EXPAND_TF (fix_trunctf<mode>2, 2); })

; floating point to unsigned integer

; op2: inexact exception not suppressed (IEEE 754 2008)
; op3: rounding mode 5 (round towards 0 C11 6.3.1.4)
; vclgdb, vclfeb
(define_insn "fixuns_trunc<VX_VEC_CONV_BFP:mode><tointvec>2"
  [(set (match_operand:<TOINTVEC>                               0 "register_operand" "=v")
	(unsigned_fix:<TOINTVEC> (match_operand:VX_VEC_CONV_BFP 1 "register_operand"  "v")))]
  "TARGET_VX"
  "vcl<bhfgq><xde>b\t%v0,%v1,0,5"
  [(set_attr "op_type" "VRR")])

; There is no instruction for rounding an extended BFP operand in a VR into
; an unsigned integer, therefore copy it into a FPR pair first.
(define_expand "fixuns_trunctf<mode>2_vr"
  [(set (match_dup 2)
	(unspec:FPRX2 [(match_operand:TF 1 "register_operand")] UNSPEC_TF_TO_FPRX2))
   (parallel [(set (match_operand:GPR 0 "register_operand" "")
		   (unsigned_fix:GPR (match_dup 2)))
	      (unspec:GPR [(const_int BFP_RND_TOWARD_0)] UNSPEC_ROUND)
	      (clobber (reg:CC CC_REGNUM))])]
  "TARGET_VXE"
{
  operands[2] = gen_reg_rtx (FPRX2mode);
})

(define_expand "fixuns_trunctf<mode>2"
  [(match_operand:GPR 0 "register_operand" "")
   (match_operand:TF  1 "register_operand" "")]
  "HAVE_TF (fixuns_trunctf<mode>2)"
  { EXPAND_TF (fixuns_trunctf<mode>2, 2); })

; load fp integer

; vfisb, wfisb, vfidb, wfidb, wfixb; suppress inexact exceptions
(define_insn "<FPINT:fpint_name><VF_HW:mode>2<VF_HW:tf_vr>"
  [(set (match_operand:VF_HW                0 "register_operand" "=v")
	(unspec:VF_HW [(match_operand:VF_HW 1 "register_operand"  "v")]
		      FPINT))]
  "TARGET_VX"
  "<vw>fi<VF_HW:sdx>b\t%v0,%v1,4,<FPINT:fpint_roundingmode>"
  [(set_attr "op_type" "VRR")])

(define_expand "<FPINT:fpint_name>tf2"
  [(match_operand:TF 0 "register_operand" "")
   (match_operand:TF 1 "register_operand" "")
   ; recognize FPINT as an iterator
   (unspec:TF [(match_dup 1)] FPINT)]
  "HAVE_TF (<FPINT:fpint_name>tf2)"
  { EXPAND_TF (<FPINT:fpint_name>tf2, 2); })

; vfisb, wfisb, vfidb, wfidb, wfixb; raise inexact exceptions
(define_insn "rint<mode>2<tf_vr>"
  [(set (match_operand:VF_HW                0 "register_operand" "=v")
	(unspec:VF_HW [(match_operand:VF_HW 1 "register_operand"  "v")]
		      UNSPEC_FPINT_RINT))]
  "TARGET_VX"
  "<vw>fi<sdx>b\t%v0,%v1,0,0"
  [(set_attr "op_type" "VRR")])

(define_expand "rinttf2"
  [(match_operand:TF 0 "register_operand" "")
   (match_operand:TF 1 "register_operand" "")]
  "HAVE_TF (rinttf2)"
  { EXPAND_TF (rinttf2, 2); })

; load rounded

; wflrx
(define_insn "*trunctfdf2_vr"
  [(set (match_operand:DF                    0 "register_operand" "=f")
	(float_truncate:DF (match_operand:TF 1 "register_operand"  "v")))
   (unspec:DF [(match_operand                2 "const_int_operand" "")]
	       UNSPEC_ROUND)]
  "TARGET_VXE"
  "wflrx\t%v0,%v1,0,%2"
  [(set_attr "op_type" "VRR")])

(define_expand "trunctfdf2_vr"
  [(parallel [
     (set (match_operand:DF                    0 "register_operand" "")
	  (float_truncate:DF (match_operand:TF 1 "register_operand" "")))
     (unspec:DF [(const_int BFP_RND_CURRENT)] UNSPEC_ROUND)])]
  "TARGET_VXE")

(define_expand "trunctfdf2"
  [(match_operand:DF 0 "register_operand" "")
   (match_operand:TF 1 "register_operand" "")]
  "HAVE_TF (trunctfdf2)"
  { EXPAND_TF (trunctfdf2, 2); })

; wflrx + (ledbr|wledb)
(define_expand "trunctfsf2_vr"
  [(parallel [
     (set (match_dup 2)
	  (float_truncate:DF (match_operand:TF 1 "register_operand" "")))
     (unspec:DF [(const_int BFP_RND_PREP_FOR_SHORT_PREC)] UNSPEC_ROUND)])
   (set (match_operand:SF                    0 "register_operand" "")
	(float_truncate:SF (match_dup 2)))]
  "TARGET_VXE"
{
  operands[2] = gen_reg_rtx(DFmode);
})

(define_expand "trunctfsf2"
  [(match_operand:SF 0 "register_operand" "")
   (match_operand:TF 1 "register_operand" "")]
  "HAVE_TF (trunctfsf2)"
  { EXPAND_TF (trunctfsf2, 2); })

(define_expand "trunctf<DFP_ALL:mode>2_vr"
  [(match_operand:DFP_ALL 0 "nonimmediate_operand" "")
   (match_operand:TF 1 "nonimmediate_operand" "")]
  "TARGET_HARD_DFP
   && GET_MODE_SIZE (TFmode) > GET_MODE_SIZE (<DFP_ALL:MODE>mode)
   && TARGET_VXE"
{
  rtx fprx2 = gen_reg_rtx (FPRX2mode);
  emit_insn (gen_tf_to_fprx2 (fprx2, operands[1]));
  emit_insn (gen_truncfprx2<DFP_ALL:mode>2 (operands[0], fprx2));
  DONE;
})

(define_expand "trunctf<DFP_ALL:mode>2"
  [(match_operand:DFP_ALL 0 "nonimmediate_operand" "")
   (match_operand:TF 1 "nonimmediate_operand" "")]
  "HAVE_TF (trunctf<DFP_ALL:mode>2)"
  { EXPAND_TF (trunctf<DFP_ALL:mode>2, 2); })

(define_expand "trunctdtf2_vr"
  [(match_operand:TF 0 "nonimmediate_operand" "")
   (match_operand:TD 1 "nonimmediate_operand" "")]
  "TARGET_HARD_DFP && TARGET_VXE"
{
  rtx fprx2 = gen_reg_rtx (FPRX2mode);
  emit_insn (gen_trunctdfprx22 (fprx2, operands[1]));
  emit_insn (gen_fprx2_to_tf (operands[0], fprx2));
  DONE;
})

(define_expand "trunctdtf2"
  [(match_operand:TF 0 "nonimmediate_operand" "")
   (match_operand:TD 1 "nonimmediate_operand" "")]
  "HAVE_TF (trunctdtf2)"
  { EXPAND_TF (trunctdtf2, 2); })

; load lengthened

(define_insn "extenddftf2_vr"
  [(set (match_operand:TF                  0 "register_operand" "=v")
	(float_extend:TF (match_operand:DF 1 "register_operand"  "f")))]
  "TARGET_VXE"
  "wflld\t%v0,%v1"
  [(set_attr "op_type" "VRR")])

(define_expand "extenddftf2"
  [(match_operand:TF 0 "register_operand" "")
   (match_operand:DF 1 "nonimmediate_operand" "")]
  "HAVE_TF (extenddftf2)"
  { EXPAND_TF (extenddftf2, 2); })

(define_expand "extendsftf2_vr"
  [(set (match_dup 2)
	(float_extend:DF (match_operand:SF 1 "nonimmediate_operand" "")))
   (set (match_operand:TF                  0 "register_operand"     "")
	(float_extend:TF (match_dup 2)))]
  "TARGET_VXE"
{
  operands[2] = gen_reg_rtx(DFmode);
})

(define_expand "extendsftf2"
  [(match_operand:TF 0 "register_operand" "")
   (match_operand:SF 1 "nonimmediate_operand" "")]
  "HAVE_TF (extendsftf2)"
  { EXPAND_TF (extendsftf2, 2); })

(define_expand "extend<DFP_ALL:mode>tf2_vr"
  [(match_operand:TF 0 "nonimmediate_operand" "")
   (match_operand:DFP_ALL 1 "nonimmediate_operand" "")]
  "TARGET_HARD_DFP
   && GET_MODE_SIZE (<DFP_ALL:MODE>mode) < GET_MODE_SIZE (TFmode)
   && TARGET_VXE"
{
  rtx fprx2 = gen_reg_rtx (FPRX2mode);
  emit_insn (gen_extend<DFP_ALL:mode>fprx22 (fprx2, operands[1]));
  emit_insn (gen_fprx2_to_tf (operands[0], fprx2));
  DONE;
})

(define_expand "extend<DFP_ALL:mode>tf2"
  [(match_operand:TF 0 "nonimmediate_operand" "")
   (match_operand:DFP_ALL 1 "nonimmediate_operand" "")]
  "HAVE_TF (extend<DFP_ALL:mode>tf2)"
  { EXPAND_TF (extend<DFP_ALL:mode>tf2, 2); })

(define_expand "extendtftd2_vr"
  [(match_operand:TD 0 "nonimmediate_operand" "")
   (match_operand:TF 1 "nonimmediate_operand" "")]
  "TARGET_HARD_DFP && TARGET_VXE"
{
  rtx fprx2 = gen_reg_rtx (FPRX2mode);
  emit_insn (gen_tf_to_fprx2 (fprx2, operands[1]));
  emit_insn (gen_extendfprx2td2 (operands[0], fprx2));
  DONE;
})

(define_expand "extendtftd2"
  [(match_operand:TD 0 "nonimmediate_operand" "")
   (match_operand:TF 1 "nonimmediate_operand" "")]
  "HAVE_TF (extendtftd2)"
  { EXPAND_TF (extendtftd2, 2); })

; test data class

(define_expand "signbittf2_vr"
  [(parallel
    [(set (reg:CCRAW CC_REGNUM)
	  (unspec:CCRAW [(match_operand:TF 1 "register_operand" "")
			 (match_dup        2)]
			UNSPEC_VEC_VFTCICC))
     (clobber (scratch:TI))])
   (set (match_operand:SI                  0 "register_operand" "")
	(const_int 0))
   (set (match_dup                         0)
	(if_then_else:SI (eq (reg:CCRAW CC_REGNUM) (const_int 8))
			 (const_int 1)
			 (match_dup        0)))]
  "TARGET_VXE"
{
  operands[2] = GEN_INT (S390_TDC_SIGNBIT_SET);
})

(define_expand "signbittf2"
  [(match_operand:SI 0 "register_operand" "")
   (match_operand:TF 1 "register_operand" "")]
  "HAVE_TF (signbittf2)"
  { EXPAND_TF (signbittf2, 2); })

(define_expand "isinftf2_vr"
  [(parallel
    [(set (reg:CCRAW CC_REGNUM)
	  (unspec:CCRAW [(match_operand:TF 1 "register_operand" "")
			 (match_dup        2)]
			UNSPEC_VEC_VFTCICC))
     (clobber (scratch:TI))])
   (set (match_operand:SI                  0 "register_operand" "")
	(const_int 0))
   (set (match_dup                         0)
	(if_then_else:SI (eq (reg:CCRAW CC_REGNUM) (const_int 8))
			 (const_int 1)
			 (match_dup        0)))]
  "TARGET_VXE"
{
  operands[2] = GEN_INT (S390_TDC_INFINITY);
})

(define_expand "isinftf2"
  [(match_operand:SI 0 "register_operand" "")
   (match_operand:TF 1 "register_operand" "")]
  "HAVE_TF (isinftf2)"
  { EXPAND_TF (isinftf2, 2); })

;
; Vector byte swap patterns
;

; FIXME: The bswap rtl standard name currently does not appear to be
; used for vector modes.
(define_expand "bswap<mode>"
  [(parallel
    [(set (match_operand:VT_HW_HSDT                   0 "nonimmediate_operand" "")
	  (bswap:VT_HW_HSDT (match_operand:VT_HW_HSDT 1 "nonimmediate_operand" "")))
     (use (match_dup 2))])]
  "TARGET_VX"
{
  static const char p[4][16] =
    { { 1,  0,  3,  2,  5,  4,  7, 6, 9,  8,  11, 10, 13, 12, 15, 14 },   /* H */
      { 3,  2,  1,  0,  7,  6,  5, 4, 11, 10, 9,  8,  15, 14, 13, 12 },   /* S */
      { 7,  6,  5,  4,  3,  2,  1, 0, 15, 14, 13, 12, 11, 10, 9,  8  },   /* D */
      { 15, 14, 13, 12, 11, 10, 9, 8, 7,  6,  5,  4,  3,  2,  1,  0  } }; /* T */
  const char *perm;
  rtx perm_rtx[16];

  switch (GET_MODE_SIZE (GET_MODE_INNER (<MODE>mode)))
    {
    case 2: perm = p[0]; break;
    case 4: perm = p[1]; break;
    case 8: perm = p[2]; break;
    case 16: perm = p[3]; break;
    default: gcc_unreachable ();
    }
  for (int i = 0; i < 16; i++)
    perm_rtx[i] = GEN_INT (perm[i]);

  operands[2] = gen_rtx_CONST_VECTOR (V16QImode, gen_rtvec_v (16, perm_rtx));

  /* Without vxe2 we do not have byte swap instructions dealing
     directly with memory operands.  So instead of waiting until
     reload to fix that up switch over to vector permute right
     now.  */
  if (!TARGET_VXE2)
    {
      rtx in = force_reg (V16QImode, simplify_gen_subreg (V16QImode, operands[1], <MODE>mode, 0));
      rtx permute = force_reg (V16QImode, force_const_mem (V16QImode, operands[2]));
      rtx out = gen_reg_rtx (V16QImode);

      emit_insn (gen_vec_permv16qi (out, in, in, permute));
      emit_move_insn (operands[0], simplify_gen_subreg (<MODE>mode, out, V16QImode, 0));
      DONE;
    }
})

; Switching late to the reg-reg variant requires the vector permute
; pattern to be pushed into literal pool and allocating a vector
; register to load it into.  We rely on both being provided by LRA
; when fixing up the v constraint for operand 2.

; permute_pattern_operand: general_operand would reject the permute
; pattern constants since these are not accepted by
; s390_legimitate_constant_p

; ^R: Prevent these alternatives from being chosen if it would require
; pushing the operand into memory first

; vlbrh, vlbrf, vlbrg, vlbrq, vstbrh, vstbrf, vstbrg, vstbrq
(define_insn_and_split "*bswap<mode>"
  [(set (match_operand:VT_HW_HSDT                   0 "nonimmediate_operand"    "=v, v,^R")
	(bswap:VT_HW_HSDT (match_operand:VT_HW_HSDT 1 "nonimmediate_operand"     "v,^R, v")))
   (use (match_operand:V16QI                        2 "permute_pattern_operand"  "v, X, X"))]
  "TARGET_VXE2"
  "@
   #
   vlbr<bhfgq>\t%v0,%1
   vstbr<bhfgq>\t%v1,%0"
  "&& reload_completed
   && !memory_operand (operands[0], <MODE>mode)
   && !memory_operand (operands[1], <MODE>mode)"
  [(set (match_dup 0)
	(subreg:VT_HW_HSDT
	 (unspec:V16QI [(subreg:V16QI (match_dup 1) 0)
			(subreg:V16QI (match_dup 1) 0)
			(match_dup 2)]
		       UNSPEC_VEC_PERM) 0))]
  ""
  [(set_attr "op_type"      "*,VRX,VRX")])

(define_insn "*vstbr<mode>"
  [(set (match_operand:VI_HW_HSDT                   0 "memory_operand"  "=R")
	(bswap:VI_HW_HSDT (match_operand:VI_HW_HSDT 1 "register_operand" "v")))]
  "TARGET_VXE2"
  "vstbr<bhfgq>\t%v1,%0"
  [(set_attr "op_type" "VRX")])

;
; Implement len_load/len_store optabs with vll/vstl.
(define_expand "len_load_v16qi"
  [(match_operand:V16QI 0 "register_operand")
   (match_operand:V16QI 1 "memory_operand")
   (match_operand:QI 2 "register_operand")
   (match_operand:QI 3 "vll_bias_operand")
  ]
  "TARGET_VX && TARGET_64BIT"
{
  rtx mem = adjust_address (operands[1], BLKmode, 0);

  rtx len = gen_reg_rtx (SImode);
  emit_move_insn (len, gen_rtx_ZERO_EXTEND (SImode, operands[2]));
  emit_insn (gen_vllv16qi (operands[0], len, mem));
  DONE;
})

(define_expand "len_store_v16qi"
  [(match_operand:V16QI 0 "memory_operand")
   (match_operand:V16QI 1 "register_operand")
   (match_operand:QI 2 "register_operand")
   (match_operand:QI 3 "vll_bias_operand")
  ]
  "TARGET_VX && TARGET_64BIT"
{
  rtx mem = adjust_address (operands[0], BLKmode, 0);

  rtx len = gen_reg_rtx (SImode);
  emit_move_insn (len, gen_rtx_ZERO_EXTEND (SImode, operands[2]));
  emit_insn (gen_vstlv16qi (operands[1], len, mem));
  DONE;
});;

(define_code_iterator LOGIC_OP1 [and ior xor])
(define_code_iterator LOGIC_OP2 [and ior xor])
(define_code_attr logic_op [(and "&") (ior "|") (xor "^")])
(define_code_attr logic_op_stringify [(and "and") (ior "ior") (xor "xor")])

(define_insn_and_split "*veval<mode>_<LOGIC_OP1:logic_op_stringify><LOGIC_OP2:logic_op_stringify>"
  [(set (match_operand:VIT   0 "register_operand" "=v")
        (LOGIC_OP1:VIT
	 (LOGIC_OP2:VIT
	  (match_operand:VIT 1 "register_operand"  "v")
	  (match_operand:VIT 2 "register_operand"  "v"))
	 (match_operand:VIT  3 "register_operand"  "v")))]
  "TARGET_VXE3"
  "#"
  "&& true"
  [(set (match_dup 0)
	(unspec:VIT [(match_dup 3)
		     (match_dup 1)
		     (match_dup 2)
		     (match_dup 4)]
		    UNSPEC_VEC_VEVAL))]
{
  int op = 15 <LOGIC_OP1:logic_op> (23 <LOGIC_OP2:logic_op> 113);
  operands[4] = GEN_INT (op);
})

(define_insn "veval<mode>"
  [(set (match_operand:VIT              0 "register_operand" "=v")
	(unspec:VIT [(match_operand:VIT 1 "register_operand"  "v")
		     (match_operand:VIT 2 "register_operand"  "v")
		     (match_operand:VIT 3 "register_operand"  "v")
		     (match_operand:QI  4 "const_int_operand")]
		    UNSPEC_VEC_VEVAL))]
  "TARGET_VXE3"
  "veval\t%v0,%v1,%v2,%v3,%b4"
  [(set_attr "op_type" "VRI")])

; reduc_smin
; reduc_smax
; reduc_umin
; reduc_umax

; vec_pack_sfix_trunc: convert + pack ?
; vec_pack_ufix_trunc
; vec_unpacks_float_hi
; vec_unpacks_float_lo
; vec_unpacku_float_hi
; vec_unpacku_float_lo
