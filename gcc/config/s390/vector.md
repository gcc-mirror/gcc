;;- Instruction patterns for the System z vector facility
;;  Copyright (C) 2015-2016 Free Software Foundation, Inc.
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

; All vector modes directly supported by the hardware having full vector reg size
; V_HW2 is duplicate of V_HW for having two iterators expanding
; independently e.g. vcond
(define_mode_iterator V_HW  [V16QI V8HI V4SI V2DI V2DF])
(define_mode_iterator V_HW2 [V16QI V8HI V4SI V2DI V2DF])
; Including TI for instructions that support it (va, vn, ...)
(define_mode_iterator VT_HW [V16QI V8HI V4SI V2DI V2DF V1TI TI])

; All full size integer vector modes supported in a vector register + TImode
(define_mode_iterator VIT_HW    [V16QI V8HI V4SI V2DI V1TI TI])
(define_mode_iterator VI_HW     [V16QI V8HI V4SI V2DI])
(define_mode_iterator VI_HW_QHS [V16QI V8HI V4SI])
(define_mode_iterator VI_HW_HS  [V8HI V4SI])
(define_mode_iterator VI_HW_QH  [V16QI V8HI])

; All integer vector modes supported in a vector register + TImode
(define_mode_iterator VIT [V1QI V2QI V4QI V8QI V16QI V1HI V2HI V4HI V8HI V1SI V2SI V4SI V1DI V2DI V1TI TI])
(define_mode_iterator VI  [V1QI V2QI V4QI V8QI V16QI V1HI V2HI V4HI V8HI V1SI V2SI V4SI V1DI V2DI])
(define_mode_iterator VI_QHS [V1QI V2QI V4QI V8QI V16QI V1HI V2HI V4HI V8HI V1SI V2SI V4SI])

(define_mode_iterator V_8   [V1QI])
(define_mode_iterator V_16  [V2QI  V1HI])
(define_mode_iterator V_32  [V4QI  V2HI V1SI V1SF])
(define_mode_iterator V_64  [V8QI  V4HI V2SI V2SF V1DI V1DF])
(define_mode_iterator V_128 [V16QI V8HI V4SI V4SF V2DI V2DF V1TI V1TF])

; A blank for vector modes and a * for TImode.  This is used to hide
; the TImode expander name in case it is defined already.  See addti3
; for an example.
(define_mode_attr ti* [(V1QI "") (V2QI "") (V4QI "") (V8QI "") (V16QI "")
		       (V1HI "") (V2HI "") (V4HI "") (V8HI "")
		       (V1SI "") (V2SI "") (V4SI "")
		       (V1DI "") (V2DI "")
		       (V1TI "*") (TI "*")])

; The element type of the vector.
(define_mode_attr non_vec[(V1QI "QI") (V2QI "QI") (V4QI "QI") (V8QI "QI") (V16QI "QI")
			  (V1HI "HI") (V2HI "HI") (V4HI "HI") (V8HI "HI")
			  (V1SI "SI") (V2SI "SI") (V4SI "SI")
			  (V1DI "DI") (V2DI "DI")
			  (V1TI "TI")
			  (V1SF "SF") (V2SF "SF") (V4SF "SF")
			  (V1DF "DF") (V2DF "DF")
			  (V1TF "TF")])

; The instruction suffix
(define_mode_attr bhfgq[(V1QI "b") (V2QI "b") (V4QI "b") (V8QI "b") (V16QI "b")
			(V1HI "h") (V2HI "h") (V4HI "h") (V8HI "h")
			(V1SI "f") (V2SI "f") (V4SI "f")
			(V1DI "g") (V2DI "g")
			(V1TI "q") (TI "q")
			(V1SF "f") (V2SF "f") (V4SF "f")
			(V1DF "g") (V2DF "g")
			(V1TF "q")])

; This is for vmalhw. It gets an 'w' attached to avoid confusion with
; multiply and add logical high vmalh.
(define_mode_attr w [(V1QI "")  (V2QI "")  (V4QI "")  (V8QI "") (V16QI "")
		     (V1HI "w") (V2HI "w") (V4HI "w") (V8HI "w")
		     (V1SI "")  (V2SI "")  (V4SI "")
		     (V1DI "")  (V2DI "")])

; Resulting mode of a vector comparison.  For floating point modes an
; integer vector mode with the same element size is picked.
(define_mode_attr tointvec [(V1QI "V1QI") (V2QI "V2QI") (V4QI "V4QI") (V8QI "V8QI") (V16QI "V16QI")
			    (V1HI "V1HI") (V2HI "V2HI") (V4HI "V4HI") (V8HI "V8HI")
			    (V1SI "V1SI") (V2SI "V2SI") (V4SI "V4SI")
			    (V1DI "V1DI") (V2DI "V2DI")
			    (V1TI "V1TI")
			    (V1SF "V1SI") (V2SF "V2SI") (V4SF "V4SI")
			    (V1DF "V1DI") (V2DF "V2DI")
			    (V1TF "V1TI")])

; Vector with doubled element size.
(define_mode_attr vec_double [(V1QI "V1HI") (V2QI "V1HI") (V4QI "V2HI") (V8QI "V4HI") (V16QI "V8HI")
			      (V1HI "V1SI") (V2HI "V1SI") (V4HI "V2SI") (V8HI "V4SI")
			      (V1SI "V1DI") (V2SI "V1DI") (V4SI "V2DI")
			      (V1DI "V1TI") (V2DI "V1TI")
			      (V1SF "V1DF") (V2SF "V1DF") (V4SF "V2DF")])

; Vector with half the element size.
(define_mode_attr vec_half [(V1HI "V2QI") (V2HI "V4QI") (V4HI "V8QI") (V8HI "V16QI")
			    (V1SI "V2HI") (V2SI "V4HI") (V4SI "V8HI")
			    (V1DI "V2SI") (V2DI "V4SI")
			    (V1TI "V2DI")
			    (V1DF "V2SF") (V2DF "V4SF")
			    (V1TF "V1DF")])

; The comparisons not setting CC iterate over the rtx code.
(define_code_iterator VFCMP_HW_OP [eq gt ge])
(define_code_attr asm_fcmp_op [(eq "e") (gt "h") (ge "he")])



; Comparison operators on int and fp compares which are directly
; supported by the HW.
(define_code_iterator VICMP_HW_OP [eq gt gtu])
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
(define_insn "mov<mode>"
  [(set (match_operand:V_128 0 "nonimmediate_operand" "=v,v,R,  v,  v,  v,  v,  v,v,d")
	(match_operand:V_128 1 "general_operand"      " v,R,v,j00,jm1,jyy,jxx,jKK,d,v"))]
  "TARGET_VX"
  "@
   vlr\t%v0,%v1
   vl\t%v0,%1
   vst\t%v1,%0
   vzero\t%v0
   vone\t%v0
   vgbm\t%v0,%t1
   vgm<bhfgq>\t%v0,%s1,%e1
   vrepi<bhfgq>\t%v0,%h1
   vlvgp\t%v0,%1,%N1
   #"
  [(set_attr "op_type" "VRR,VRX,VRX,VRI,VRI,VRI,VRI,VRI,VRR,*")])

(define_split
  [(set (match_operand:V_128 0 "register_operand" "")
	(match_operand:V_128 1 "register_operand" ""))]
  "TARGET_VX && GENERAL_REG_P (operands[0]) && VECTOR_REG_P (operands[1])"
  [(set (match_dup 2)
	(unspec:DI [(subreg:V2DI (match_dup 1) 0)
		    (const_int 0)] UNSPEC_VEC_EXTRACT))
   (set (match_dup 3)
	(unspec:DI [(subreg:V2DI (match_dup 1) 0)
		    (const_int 1)] UNSPEC_VEC_EXTRACT))]
{
  operands[2] = operand_subword (operands[0], 0, 0, <MODE>mode);
  operands[3] = operand_subword (operands[0], 1, 0, <MODE>mode);
})

; Moves for smaller vector modes.

; In these patterns only the vlr, vone, and vzero instructions write
; VR bytes outside the mode.  This should be ok since we disallow
; formerly bigger modes being accessed with smaller modes via
; subreg. Note: The vone, vzero instructions could easily be replaced
; with vlei which would only access the bytes belonging to the mode.
; However, this would probably be slower.

(define_insn "mov<mode>"
  [(set (match_operand:V_8 0 "nonimmediate_operand" "=v,v,d,v,R,  v,  v,  v,  v,d,  Q,  S,  Q,  S,  d,  d,d,d,d,R,T")
        (match_operand:V_8 1 "general_operand"      " v,d,v,R,v,j00,jm1,jyy,jxx,d,j00,j00,jm1,jm1,j00,jm1,R,T,b,d,d"))]
  ""
  "@
   vlr\t%v0,%v1
   vlvgb\t%v0,%1,0
   vlgvb\t%0,%v1,0
   vleb\t%v0,%1,0
   vsteb\t%v1,%0,0
   vzero\t%v0
   vone\t%v0
   vgbm\t%v0,%t1
   vgm\t%v0,%s1,%e1
   lr\t%0,%1
   mvi\t%0,0
   mviy\t%0,0
   mvi\t%0,-1
   mviy\t%0,-1
   lhi\t%0,0
   lhi\t%0,-1
   lh\t%0,%1
   lhy\t%0,%1
   lhrl\t%0,%1
   stc\t%1,%0
   stcy\t%1,%0"
  [(set_attr "op_type"      "VRR,VRS,VRS,VRX,VRX,VRI,VRI,VRI,VRI,RR,SI,SIY,SI,SIY,RI,RI,RX,RXY,RIL,RX,RXY")])

(define_insn "mov<mode>"
  [(set (match_operand:V_16 0 "nonimmediate_operand" "=v,v,d,v,R,  v,  v,  v,  v,d,  Q,  Q,  d,  d,d,d,d,R,T,b")
        (match_operand:V_16 1 "general_operand"      " v,d,v,R,v,j00,jm1,jyy,jxx,d,j00,jm1,j00,jm1,R,T,b,d,d,d"))]
  ""
  "@
   vlr\t%v0,%v1
   vlvgh\t%v0,%1,0
   vlgvh\t%0,%v1,0
   vleh\t%v0,%1,0
   vsteh\t%v1,%0,0
   vzero\t%v0
   vone\t%v0
   vgbm\t%v0,%t1
   vgm\t%v0,%s1,%e1
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
  [(set_attr "op_type"      "VRR,VRS,VRS,VRX,VRX,VRI,VRI,VRI,VRI,RR,SIL,SIL,RI,RI,RX,RXY,RIL,RX,RXY,RIL")])

(define_insn "mov<mode>"
  [(set (match_operand:V_32 0 "nonimmediate_operand" "=f,f,f,R,T,v,v,d,v,R,  f,  v,  v,  v,  v,  Q,  Q,  d,  d,d,d,d,d,R,T,b")
	(match_operand:V_32 1 "general_operand"      " f,R,T,f,f,v,d,v,R,v,j00,j00,jm1,jyy,jxx,j00,jm1,j00,jm1,b,d,R,T,d,d,d"))]
  "TARGET_VX"
  "@
   lder\t%v0,%v1
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
   vgbm\t%v0,%t1
   vgm\t%v0,%s1,%e1
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
  [(set_attr "op_type" "RRE,RXE,RXY,RX,RXY,VRR,VRS,VRS,VRX,VRX,RRE,VRI,VRI,VRI,VRI,SIL,SIL,RI,RI,
                        RIL,RR,RX,RXY,RX,RXY,RIL")])

(define_insn "mov<mode>"
  [(set (match_operand:V_64 0 "nonimmediate_operand"
         "=f,f,f,R,T,v,v,d,v,R,  f,  v,  v,  v,  v,  Q,  Q,  d,  d,f,d,d,d,d,T,b")
        (match_operand:V_64 1 "general_operand"
         " f,R,T,f,f,v,d,v,R,v,j00,j00,jm1,jyy,jxx,j00,jm1,j00,jm1,d,f,b,d,T,d,d"))]
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
   vgbm\t%v0,%t1
   vgm\t%v0,%s1,%e1
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
  [(set_attr "op_type" "RRE,RX,RXY,RX,RXY,VRR,VRS,VRS,VRX,VRX,RRE,VRI,VRI,VRI,VRI,
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


; FIXME: Support also vector mode operands for 0
; FIXME: This should be (vec_select ..) or something but it does only allow constant selectors :(
; This is used via RTL standard name as well as for expanding the builtin
(define_expand "vec_extract<mode>"
  [(set (match_operand:<non_vec> 0 "nonimmediate_operand" "")
	(unspec:<non_vec> [(match_operand:V  1 "register_operand" "")
			   (match_operand:SI 2 "nonmemory_operand" "")]
			  UNSPEC_VEC_EXTRACT))]
  "TARGET_VX")

(define_insn "*vec_extract<mode>"
  [(set (match_operand:<non_vec> 0 "nonimmediate_operand"          "=d,R")
	(unspec:<non_vec> [(match_operand:V  1 "register_operand"   "v,v")
			   (match_operand:SI 2 "nonmemory_operand" "an,I")]
			  UNSPEC_VEC_EXTRACT))]
  "TARGET_VX
   && (!CONST_INT_P (operands[2])
       || UINTVAL (operands[2]) < GET_MODE_NUNITS (<V:MODE>mode))"
  "@
   vlgv<bhfgq>\t%0,%v1,%Y2
   vste<bhfgq>\t%v1,%0,%2"
  [(set_attr "op_type" "VRS,VRX")])

(define_insn "*vec_extract<mode>_plus"
  [(set (match_operand:<non_vec>                      0 "nonimmediate_operand" "=d")
	(unspec:<non_vec> [(match_operand:V           1 "register_operand"      "v")
			   (plus:SI (match_operand:SI 2 "nonmemory_operand"     "a")
				    (match_operand:SI 3 "const_int_operand"     "n"))]
			   UNSPEC_VEC_EXTRACT))]
  "TARGET_VX"
  "vlgv<bhfgq>\t%0,%v1,%Y3(%2)"
  [(set_attr "op_type" "VRS")])

(define_expand "vec_init<V_HW:mode>"
  [(match_operand:V_HW 0 "register_operand" "")
   (match_operand:V_HW 1 "nonmemory_operand" "")]
  "TARGET_VX"
{
  s390_expand_vec_init (operands[0], operands[1]);
  DONE;
})

; Replicate from vector element
(define_insn "*vec_splat<mode>"
  [(set (match_operand:V_HW   0 "register_operand" "=v")
	(vec_duplicate:V_HW
	 (vec_select:<non_vec>
	  (match_operand:V_HW 1 "register_operand"  "v")
	  (parallel
	   [(match_operand:QI 2 "const_mask_operand" "C")]))))]
  "TARGET_VX && UINTVAL (operands[2]) < GET_MODE_NUNITS (<V_HW:MODE>mode)"
  "vrep<bhfgq>\t%v0,%v1,%2"
  [(set_attr "op_type" "VRI")])

(define_insn "*vec_splats<mode>"
  [(set (match_operand:V_HW                          0 "register_operand" "=v,v,v,v")
	(vec_duplicate:V_HW (match_operand:<non_vec> 1 "general_operand"  " R,K,v,d")))]
  "TARGET_VX"
  "@
   vlrep<bhfgq>\t%v0,%1
   vrepi<bhfgq>\t%v0,%h1
   vrep<bhfgq>\t%v0,%v1,0
   #"
  [(set_attr "op_type" "VRX,VRI,VRI,*")])

; vec_splats is supposed to replicate op1 into all elements of op0
; This splitter first sets the rightmost element of op0 to op1 and
; then does a vec_splat to replicate that element into all other
; elements.
(define_split
  [(set (match_operand:V_HW                          0 "register_operand" "")
	(vec_duplicate:V_HW (match_operand:<non_vec> 1 "register_operand" "")))]
  "TARGET_VX && GENERAL_REG_P (operands[1])"
  [(set (match_dup 0)
	(unspec:V_HW [(match_dup 1) (match_dup 2) (match_dup 0)] UNSPEC_VEC_SET))
   (set (match_dup 0)
	(vec_duplicate:V_HW
	 (vec_select:<non_vec>
	  (match_dup 0) (parallel [(match_dup 2)]))))]
{
  operands[2] = GEN_INT (GET_MODE_NUNITS (<MODE>mode) - 1);
})

(define_expand "vcond<V_HW:mode><V_HW2:mode>"
  [(set (match_operand:V_HW 0 "register_operand" "")
	(if_then_else:V_HW
	 (match_operator 3 "comparison_operator"
			 [(match_operand:V_HW2 4 "register_operand" "")
			  (match_operand:V_HW2 5 "nonmemory_operand" "")])
	 (match_operand:V_HW 1 "nonmemory_operand" "")
	 (match_operand:V_HW 2 "nonmemory_operand" "")))]
  "TARGET_VX && GET_MODE_NUNITS (<V_HW:MODE>mode) == GET_MODE_NUNITS (<V_HW2:MODE>mode)"
{
  s390_expand_vcond (operands[0], operands[1], operands[2],
		     GET_CODE (operands[3]), operands[4], operands[5]);
  DONE;
})

(define_expand "vcondu<V_HW:mode><V_HW2:mode>"
  [(set (match_operand:V_HW 0 "register_operand" "")
	(if_then_else:V_HW
	 (match_operator 3 "comparison_operator"
			 [(match_operand:V_HW2 4 "register_operand" "")
			  (match_operand:V_HW2 5 "nonmemory_operand" "")])
	 (match_operand:V_HW 1 "nonmemory_operand" "")
	 (match_operand:V_HW 2 "nonmemory_operand" "")))]
  "TARGET_VX && GET_MODE_NUNITS (<V_HW:MODE>mode) == GET_MODE_NUNITS (<V_HW2:MODE>mode)"
{
  s390_expand_vcond (operands[0], operands[1], operands[2],
		     GET_CODE (operands[3]), operands[4], operands[5]);
  DONE;
})

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

; vec_perm_const for V2DI using vpdi?

;;
;; Vector integer arithmetic instructions
;;

; vab, vah, vaf, vag, vaq

; We use nonimmediate_operand instead of register_operand since it is
; better to have the reloads into VRs instead of splitting the
; operation into two DImode ADDs.
(define_insn "<ti*>add<mode>3"
  [(set (match_operand:VIT           0 "nonimmediate_operand" "=v")
	(plus:VIT (match_operand:VIT 1 "nonimmediate_operand" "%v")
		  (match_operand:VIT 2 "general_operand"       "v")))]
  "TARGET_VX"
  "va<bhfgq>\t%v0,%v1,%v2"
  [(set_attr "op_type" "VRR")])

; vsb, vsh, vsf, vsg, vsq
(define_insn "<ti*>sub<mode>3"
  [(set (match_operand:VIT            0 "nonimmediate_operand" "=v")
	(minus:VIT (match_operand:VIT 1 "nonimmediate_operand"  "v")
		   (match_operand:VIT 2 "general_operand"  "v")))]
  "TARGET_VX"
  "vs<bhfgq>\t%v0,%v1,%v2"
  [(set_attr "op_type" "VRR")])

; vmlb, vmlhw, vmlf
(define_insn "mul<mode>3"
  [(set (match_operand:VI_QHS              0 "register_operand" "=v")
	(mult:VI_QHS (match_operand:VI_QHS 1 "register_operand" "%v")
		     (match_operand:VI_QHS 2 "register_operand"  "v")))]
  "TARGET_VX"
  "vml<bhfgq><w>\t%v0,%v1,%v2"
  [(set_attr "op_type" "VRR")])

; vlcb, vlch, vlcf, vlcg
(define_insn "neg<mode>2"
  [(set (match_operand:VI         0 "register_operand" "=v")
	(neg:VI (match_operand:VI 1 "register_operand"  "v")))]
  "TARGET_VX"
  "vlc<bhfgq>\t%v0,%v1"
  [(set_attr "op_type" "VRR")])

; vlpb, vlph, vlpf, vlpg
(define_insn "abs<mode>2"
  [(set (match_operand:VI         0 "register_operand" "=v")
	(abs:VI (match_operand:VI 1 "register_operand"  "v")))]
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
	(and:VT (match_operand:VT 1 "register_operand" "%v")
		(match_operand:VT 2 "register_operand"  "v")))]
  "TARGET_VX"
  "vn\t%v0,%v1,%v2"
  [(set_attr "op_type" "VRR")])


; Vector or

(define_insn "ior<mode>3"
  [(set (match_operand:VT         0 "register_operand" "=v")
	(ior:VT (match_operand:VT 1 "register_operand" "%v")
		(match_operand:VT 2 "register_operand"  "v")))]
  "TARGET_VX"
  "vo\t%v0,%v1,%v2"
  [(set_attr "op_type" "VRR")])


; Vector xor

(define_insn "xor<mode>3"
  [(set (match_operand:VT         0 "register_operand" "=v")
	(xor:VT (match_operand:VT 1 "register_operand" "%v")
		(match_operand:VT 2 "register_operand"  "v")))]
  "TARGET_VX"
  "vx\t%v0,%v1,%v2"
  [(set_attr "op_type" "VRR")])


; Bitwise inversion of a vector - used for vec_cmpne
(define_insn "*not<mode>"
  [(set (match_operand:VT         0 "register_operand" "=v")
	(not:VT (match_operand:VT 1 "register_operand"  "v")))]
  "TARGET_VX"
  "vnot\t%v0,%v1"
  [(set_attr "op_type" "VRR")])

; Vector population count

(define_insn "popcountv16qi2"
  [(set (match_operand:V16QI                0 "register_operand" "=v")
	(unspec:V16QI [(match_operand:V16QI 1 "register_operand"  "v")]
		      UNSPEC_POPCNT))]
  "TARGET_VX"
  "vpopct\t%v0,%v1,0"
  [(set_attr "op_type" "VRR")])

; vpopct only counts bits in byte elements.  Bigger element sizes need
; to be emulated.  Word and doubleword elements can use the sum across
; instructions.  For halfword sized elements we do a shift of a copy
; of the result, add it to the result and extend it to halfword
; element size (unpack).

(define_expand "popcountv8hi2"
  [(set (match_dup 2)
	(unspec:V16QI [(subreg:V16QI (match_operand:V8HI 1 "register_operand" "v") 0)]
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
	(const_vector:V16QI [(const_int 0) (const_int 255)
			     (const_int 0) (const_int 255)
			     (const_int 0) (const_int 255)
			     (const_int 0) (const_int 255)
			     (const_int 0) (const_int 255)
			     (const_int 0) (const_int 255)
			     (const_int 0) (const_int 255)
			     (const_int 0) (const_int 255)]))
   ; Zero out the even indexed bytes
   (set (match_operand:V8HI 0 "register_operand" "=v")
	(and:V8HI (subreg:V8HI (match_dup 2) 0)
		  (subreg:V8HI (match_dup 3) 0)))
]
  "TARGET_VX"
{
  operands[2] = gen_reg_rtx (V16QImode);
  operands[3] = gen_reg_rtx (V16QImode);
  operands[4] = gen_reg_rtx (V16QImode);
  operands[5] = CONST0_RTX (V16QImode);
})

(define_expand "popcountv4si2"
  [(set (match_dup 2)
	(unspec:V16QI [(subreg:V16QI (match_operand:V4SI 1 "register_operand" "v") 0)]
		      UNSPEC_POPCNT))
   (set (match_operand:V4SI 0 "register_operand" "=v")
	(unspec:V4SI [(match_dup 2) (match_dup 3)]
		     UNSPEC_VEC_VSUM))]
  "TARGET_VX"
{
  operands[2] = gen_reg_rtx (V16QImode);
  operands[3] = force_reg (V16QImode, CONST0_RTX (V16QImode));
})

(define_expand "popcountv2di2"
  [(set (match_dup 2)
	(unspec:V16QI [(subreg:V16QI (match_operand:V2DI 1 "register_operand" "v") 0)]
		      UNSPEC_POPCNT))
   (set (match_dup 3)
	(unspec:V4SI [(match_dup 2) (match_dup 4)]
		     UNSPEC_VEC_VSUM))
   (set (match_operand:V2DI 0 "register_operand" "=v")
	(unspec:V2DI [(match_dup 3) (match_dup 5)]
		     UNSPEC_VEC_VSUMG))]
  "TARGET_VX"
{
  operands[2] = gen_reg_rtx (V16QImode);
  operands[3] = gen_reg_rtx (V4SImode);
  operands[4] = force_reg (V16QImode, CONST0_RTX (V16QImode));
  operands[5] = force_reg (V4SImode, CONST0_RTX (V4SImode));
})

; Count leading zeros
(define_insn "clz<mode>2"
  [(set (match_operand:V        0 "register_operand" "=v")
	(clz:V (match_operand:V 1 "register_operand"  "v")))]
  "TARGET_VX"
  "vclz<bhfgq>\t%v0,%v1"
  [(set_attr "op_type" "VRR")])

; Count trailing zeros
(define_insn "ctz<mode>2"
  [(set (match_operand:V        0 "register_operand" "=v")
	(ctz:V (match_operand:V 1 "register_operand"  "v")))]
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
  [(set (match_operand:VI 0 "register_operand" "")
	(VEC_SHIFTS:VI (match_operand:VI 1 "register_operand" "")
		       (match_operand:SI 2 "nonmemory_operand" "")))]
  "TARGET_VX")

; verllb, verllh, verllf, verllg
; veslb,  veslh,  veslf,  veslg
; vesrab, vesrah, vesraf, vesrag
; vesrlb, vesrlh, vesrlf, vesrlg
(define_insn "*<vec_shifts_name><mode>3<addr_style_op>"
  [(set (match_operand:VI                0 "register_operand"  "=v")
	(VEC_SHIFTS:VI (match_operand:VI 1 "register_operand"   "v")
		       (match_operand:SI 2 "nonmemory_operand" "an")))]
  "TARGET_VX"
  "<vec_shifts_mnem><bhfgq>\t%v0,%v1,%Y2"
  [(set_attr "op_type" "VRS")])

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
  [(set (match_operand:V_HW 0 "register_operand"                    "=v")
	(unspec:V_HW [(match_operand:V_HW 1 "register_operand"       "v")
		      (match_operand:<tointvec> 2 "register_operand" "v")]
		     UNSPEC_VEC_SRLB))]
  "TARGET_VX"
  "vsrlb\t%v0,%v1,%v2"
  [(set_attr "op_type" "VRR")])


; vmnb, vmnh, vmnf, vmng
(define_insn "smin<mode>3"
  [(set (match_operand:VI          0 "register_operand" "=v")
	(smin:VI (match_operand:VI 1 "register_operand" "%v")
		 (match_operand:VI 2 "register_operand"  "v")))]
  "TARGET_VX"
  "vmn<bhfgq>\t%v0,%v1,%v2"
  [(set_attr "op_type" "VRR")])

; vmxb, vmxh, vmxf, vmxg
(define_insn "smax<mode>3"
  [(set (match_operand:VI          0 "register_operand" "=v")
	(smax:VI (match_operand:VI 1 "register_operand" "%v")
		 (match_operand:VI 2 "register_operand"  "v")))]
  "TARGET_VX"
  "vmx<bhfgq>\t%v0,%v1,%v2"
  [(set_attr "op_type" "VRR")])

; vmnlb, vmnlh, vmnlf, vmnlg
(define_insn "umin<mode>3"
  [(set (match_operand:VI          0 "register_operand" "=v")
	(umin:VI (match_operand:VI 1 "register_operand" "%v")
		 (match_operand:VI 2 "register_operand"  "v")))]
  "TARGET_VX"
  "vmnl<bhfgq>\t%v0,%v1,%v2"
  [(set_attr "op_type" "VRR")])

; vmxlb, vmxlh, vmxlf, vmxlg
(define_insn "umax<mode>3"
  [(set (match_operand:VI          0 "register_operand" "=v")
	(umax:VI (match_operand:VI 1 "register_operand" "%v")
		 (match_operand:VI 2 "register_operand"  "v")))]
  "TARGET_VX"
  "vmxl<bhfgq>\t%v0,%v1,%v2"
  [(set_attr "op_type" "VRR")])

; vmeb, vmeh, vmef
(define_insn "vec_widen_smult_even_<mode>"
  [(set (match_operand:<vec_double>                 0 "register_operand" "=v")
	(unspec:<vec_double> [(match_operand:VI_QHS 1 "register_operand" "%v")
			      (match_operand:VI_QHS 2 "register_operand"  "v")]
			     UNSPEC_VEC_SMULT_EVEN))]
  "TARGET_VX"
  "vme<bhfgq>\t%v0,%v1,%v2"
  [(set_attr "op_type" "VRR")])

; vmleb, vmleh, vmlef
(define_insn "vec_widen_umult_even_<mode>"
  [(set (match_operand:<vec_double>                 0 "register_operand" "=v")
	(unspec:<vec_double> [(match_operand:VI_QHS 1 "register_operand" "%v")
			      (match_operand:VI_QHS 2 "register_operand"  "v")]
			     UNSPEC_VEC_UMULT_EVEN))]
  "TARGET_VX"
  "vmle<bhfgq>\t%v0,%v1,%v2"
  [(set_attr "op_type" "VRR")])

; vmob, vmoh, vmof
(define_insn "vec_widen_smult_odd_<mode>"
  [(set (match_operand:<vec_double>                 0 "register_operand" "=v")
	(unspec:<vec_double> [(match_operand:VI_QHS 1 "register_operand" "%v")
			      (match_operand:VI_QHS 2 "register_operand"  "v")]
			     UNSPEC_VEC_SMULT_ODD))]
  "TARGET_VX"
  "vmo<bhfgq>\t%v0,%v1,%v2"
  [(set_attr "op_type" "VRR")])

; vmlob, vmloh, vmlof
(define_insn "vec_widen_umult_odd_<mode>"
  [(set (match_operand:<vec_double>                 0 "register_operand" "=v")
	(unspec:<vec_double> [(match_operand:VI_QHS 1 "register_operand" "%v")
			      (match_operand:VI_QHS 2 "register_operand"  "v")]
			     UNSPEC_VEC_UMULT_ODD))]
  "TARGET_VX"
  "vmlo<bhfgq>\t%v0,%v1,%v2"
  [(set_attr "op_type" "VRR")])

; vec_widen_umult_hi
; vec_widen_umult_lo
; vec_widen_smult_hi
; vec_widen_smult_lo

; vec_widen_ushiftl_hi
; vec_widen_ushiftl_lo
; vec_widen_sshiftl_hi
; vec_widen_sshiftl_lo

;;
;; Vector floating point arithmetic instructions
;;

(define_insn "addv2df3"
  [(set (match_operand:V2DF            0 "register_operand" "=v")
	(plus:V2DF (match_operand:V2DF 1 "register_operand" "%v")
		   (match_operand:V2DF 2 "register_operand"  "v")))]
  "TARGET_VX"
  "vfadb\t%v0,%v1,%v2"
  [(set_attr "op_type" "VRR")])

(define_insn "subv2df3"
  [(set (match_operand:V2DF             0 "register_operand" "=v")
	(minus:V2DF (match_operand:V2DF 1 "register_operand" "%v")
		    (match_operand:V2DF 2 "register_operand"  "v")))]
  "TARGET_VX"
  "vfsdb\t%v0,%v1,%v2"
  [(set_attr "op_type" "VRR")])

(define_insn "mulv2df3"
  [(set (match_operand:V2DF            0 "register_operand" "=v")
	(mult:V2DF (match_operand:V2DF 1 "register_operand" "%v")
		   (match_operand:V2DF 2 "register_operand"  "v")))]
  "TARGET_VX"
  "vfmdb\t%v0,%v1,%v2"
  [(set_attr "op_type" "VRR")])

(define_insn "divv2df3"
  [(set (match_operand:V2DF           0 "register_operand" "=v")
	(div:V2DF (match_operand:V2DF 1 "register_operand"  "v")
		  (match_operand:V2DF 2 "register_operand"  "v")))]
  "TARGET_VX"
  "vfddb\t%v0,%v1,%v2"
  [(set_attr "op_type" "VRR")])

(define_insn "sqrtv2df2"
  [(set (match_operand:V2DF            0 "register_operand" "=v")
	(sqrt:V2DF (match_operand:V2DF 1 "register_operand"  "v")))]
  "TARGET_VX"
  "vfsqdb\t%v0,%v1"
  [(set_attr "op_type" "VRR")])

(define_insn "fmav2df4"
  [(set (match_operand:V2DF           0 "register_operand" "=v")
	(fma:V2DF (match_operand:V2DF 1 "register_operand" "%v")
		  (match_operand:V2DF 2 "register_operand"  "v")
		  (match_operand:V2DF 3 "register_operand"  "v")))]
  "TARGET_VX"
  "vfmadb\t%v0,%v1,%v2,%v3"
  [(set_attr "op_type" "VRR")])

(define_insn "fmsv2df4"
  [(set (match_operand:V2DF                     0 "register_operand" "=v")
	(fma:V2DF (match_operand:V2DF           1 "register_operand" "%v")
		  (match_operand:V2DF           2 "register_operand"  "v")
		  (neg:V2DF (match_operand:V2DF 3 "register_operand"  "v"))))]
  "TARGET_VX"
  "vfmsdb\t%v0,%v1,%v2,%v3"
  [(set_attr "op_type" "VRR")])

(define_insn "negv2df2"
  [(set (match_operand:V2DF           0 "register_operand" "=v")
	(neg:V2DF (match_operand:V2DF 1 "register_operand"  "v")))]
  "TARGET_VX"
  "vflcdb\t%v0,%v1"
  [(set_attr "op_type" "VRR")])

(define_insn "absv2df2"
  [(set (match_operand:V2DF           0 "register_operand" "=v")
	(abs:V2DF (match_operand:V2DF 1 "register_operand"  "v")))]
  "TARGET_VX"
  "vflpdb\t%v0,%v1"
  [(set_attr "op_type" "VRR")])

(define_insn "*negabsv2df2"
  [(set (match_operand:V2DF                     0 "register_operand" "=v")
	(neg:V2DF (abs:V2DF (match_operand:V2DF 1 "register_operand"  "v"))))]
  "TARGET_VX"
  "vflndb\t%v0,%v1"
  [(set_attr "op_type" "VRR")])

; Emulate with compare + select
(define_insn_and_split "smaxv2df3"
  [(set (match_operand:V2DF            0 "register_operand" "=v")
	(smax:V2DF (match_operand:V2DF 1 "register_operand" "%v")
		   (match_operand:V2DF 2 "register_operand"  "v")))]
  "TARGET_VX"
  "#"
  ""
  [(set (match_dup 3)
	(gt:V2DI (match_dup 1) (match_dup 2)))
   (set (match_dup 0)
	(if_then_else:V2DF
	 (eq (match_dup 3) (match_dup 4))
	 (match_dup 2)
	 (match_dup 1)))]
{
  operands[3] = gen_reg_rtx (V2DImode);
  operands[4] = CONST0_RTX (V2DImode);
})

; Emulate with compare + select
(define_insn_and_split "sminv2df3"
  [(set (match_operand:V2DF            0 "register_operand" "=v")
	(smin:V2DF (match_operand:V2DF 1 "register_operand" "%v")
		   (match_operand:V2DF 2 "register_operand"  "v")))]
  "TARGET_VX"
  "#"
  ""
  [(set (match_dup 3)
	(gt:V2DI (match_dup 1) (match_dup 2)))
   (set (match_dup 0)
	(if_then_else:V2DF
	 (eq (match_dup 3) (match_dup 4))
	 (match_dup 1)
	 (match_dup 2)))]
{
  operands[3] = gen_reg_rtx (V2DImode);
  operands[4] = CONST0_RTX (V2DImode);
})


;;
;; Integer compares
;;

(define_insn "*vec_cmp<VICMP_HW_OP:code><VI:mode>_nocc"
  [(set (match_operand:VI                 2 "register_operand" "=v")
	(VICMP_HW_OP:VI (match_operand:VI 0 "register_operand"  "v")
			(match_operand:VI 1 "register_operand"  "v")))]
  "TARGET_VX"
  "vc<VICMP_HW_OP:insn_cmp_op><VI:bhfgq>\t%v2,%v0,%v1"
  [(set_attr "op_type" "VRR")])


;;
;; Floating point compares
;;

; EQ, GT, GE
(define_insn "*vec_cmp<VFCMP_HW_OP:code>v2df_nocc"
  [(set (match_operand:V2DI                   0 "register_operand" "=v")
	(VFCMP_HW_OP:V2DI (match_operand:V2DF 1 "register_operand"  "v")
			  (match_operand:V2DF 2 "register_operand"  "v")))]
   "TARGET_VX"
   "vfc<VFCMP_HW_OP:asm_fcmp_op>db\t%v0,%v1,%v2"
  [(set_attr "op_type" "VRR")])

; Expanders for not directly supported comparisons

; UNEQ a u== b -> !(a > b | b > a)
(define_expand "vec_cmpuneqv2df"
  [(set (match_operand:V2DI          0 "register_operand" "=v")
	(gt:V2DI (match_operand:V2DF 1 "register_operand"  "v")
		 (match_operand:V2DF 2 "register_operand"  "v")))
   (set (match_dup 3)
	(gt:V2DI (match_dup 2) (match_dup 1)))
   (set (match_dup 0) (ior:V2DI (match_dup 0) (match_dup 3)))
   (set (match_dup 0) (not:V2DI (match_dup 0)))]
  "TARGET_VX"
{
  operands[3] = gen_reg_rtx (V2DImode);
})

; LTGT a <> b -> a > b | b > a
(define_expand "vec_cmpltgtv2df"
  [(set (match_operand:V2DI          0 "register_operand" "=v")
	(gt:V2DI (match_operand:V2DF 1 "register_operand"  "v")
		 (match_operand:V2DF 2 "register_operand"  "v")))
   (set (match_dup 3) (gt:V2DI (match_dup 2) (match_dup 1)))
   (set (match_dup 0) (ior:V2DI (match_dup 0) (match_dup 3)))]
  "TARGET_VX"
{
  operands[3] = gen_reg_rtx (V2DImode);
})

; ORDERED (a, b): a >= b | b > a
(define_expand "vec_orderedv2df"
  [(set (match_operand:V2DI          0 "register_operand" "=v")
	(ge:V2DI (match_operand:V2DF 1 "register_operand"  "v")
		 (match_operand:V2DF 2 "register_operand"  "v")))
   (set (match_dup 3) (gt:V2DI (match_dup 2) (match_dup 1)))
   (set (match_dup 0) (ior:V2DI (match_dup 0) (match_dup 3)))]
  "TARGET_VX"
{
  operands[3] = gen_reg_rtx (V2DImode);
})

; UNORDERED (a, b): !ORDERED (a, b)
(define_expand "vec_unorderedv2df"
  [(set (match_operand:V2DI          0 "register_operand" "=v")
	(ge:V2DI (match_operand:V2DF 1 "register_operand"  "v")
		 (match_operand:V2DF 2 "register_operand"  "v")))
   (set (match_dup 3) (gt:V2DI (match_dup 2) (match_dup 1)))
   (set (match_dup 0) (ior:V2DI (match_dup 0) (match_dup 3)))
   (set (match_dup 0) (not:V2DI (match_dup 0)))]
  "TARGET_VX"
{
  operands[3] = gen_reg_rtx (V2DImode);
})

(define_insn "*vec_load_pairv2di"
  [(set (match_operand:V2DI                0 "register_operand" "=v")
	(vec_concat:V2DI (match_operand:DI 1 "register_operand"  "d")
			 (match_operand:DI 2 "register_operand"  "d")))]
  "TARGET_VX"
  "vlvgp\t%v0,%1,%2"
  [(set_attr "op_type" "VRR")])

(define_insn "vllv16qi"
  [(set (match_operand:V16QI              0 "register_operand" "=v")
	(unspec:V16QI [(match_operand:SI  1 "register_operand"  "d")
		       (match_operand:BLK 2 "memory_operand"    "Q")]
		      UNSPEC_VEC_LOAD_LEN))]
  "TARGET_VX"
  "vll\t%v0,%1,%2"
  [(set_attr "op_type" "VRS")])

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
  unsigned HOST_WIDE_INT flags = INTVAL (operands[3]);

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
	 (eq (match_operand:<tointvec> 3 "register_operand" "")
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
	 (eq (match_operand:<tointvec> 3 "register_operand" "")
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
	 (ne (match_operand:<tointvec> 3 "register_operand" "")
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
	 (ne (match_operand:<tointvec> 3 "register_operand" "")
	     (match_operand:V 4 "const0_operand" ""))
	 (match_operand:V 1 "const0_operand" "")
	 (match_operand:V 2 "all_ones_operand" "")))]
  "TARGET_VX"
  [(set (match_dup 0) (not:V (match_dup 3)))]
{
  PUT_MODE (operands[3], <V:MODE>mode);
})

; op0 = op3 == 0 ? op1 : op2
(define_insn "*vec_sel0<mode>"
  [(set (match_operand:V 0 "register_operand" "=v")
	(if_then_else:V
	 (eq (match_operand:<tointvec> 3 "register_operand" "v")
	     (match_operand:<tointvec> 4 "const0_operand" ""))
	 (match_operand:V 1 "register_operand" "v")
	 (match_operand:V 2 "register_operand" "v")))]
  "TARGET_VX"
  "vsel\t%v0,%2,%1,%3"
  [(set_attr "op_type" "VRR")])

; op0 = !op3 == 0 ? op1 : op2
(define_insn "*vec_sel0<mode>"
  [(set (match_operand:V 0 "register_operand" "=v")
	(if_then_else:V
	 (eq (not:<tointvec> (match_operand:<tointvec> 3 "register_operand" "v"))
	     (match_operand:<tointvec> 4 "const0_operand" ""))
	 (match_operand:V 1 "register_operand" "v")
	 (match_operand:V 2 "register_operand" "v")))]
  "TARGET_VX"
  "vsel\t%v0,%1,%2,%3"
  [(set_attr "op_type" "VRR")])

; op0 = op3 == -1 ? op1 : op2
(define_insn "*vec_sel1<mode>"
  [(set (match_operand:V 0 "register_operand" "=v")
	(if_then_else:V
	 (eq (match_operand:<tointvec> 3 "register_operand" "v")
	     (match_operand:<tointvec> 4 "all_ones_operand" ""))
	 (match_operand:V 1 "register_operand" "v")
	 (match_operand:V 2 "register_operand" "v")))]
  "TARGET_VX"
  "vsel\t%v0,%1,%2,%3"
  [(set_attr "op_type" "VRR")])

; op0 = !op3 == -1 ? op1 : op2
(define_insn "*vec_sel1<mode>"
  [(set (match_operand:V 0 "register_operand" "=v")
	(if_then_else:V
	 (eq (not:<tointvec> (match_operand:<tointvec> 3 "register_operand" "v"))
	     (match_operand:<tointvec> 4 "all_ones_operand" ""))
	 (match_operand:V 1 "register_operand" "v")
	 (match_operand:V 2 "register_operand" "v")))]
  "TARGET_VX"
  "vsel\t%v0,%2,%1,%3"
  [(set_attr "op_type" "VRR")])



; reduc_smin
; reduc_smax
; reduc_umin
; reduc_umax

; vec_shl vrep + vsl
; vec_shr

; vec_pack_trunc
; vec_pack_ssat
; vec_pack_usat
; vec_pack_sfix_trunc
; vec_pack_ufix_trunc
; vec_unpacks_hi
; vec_unpacks_low
; vec_unpacku_hi
; vec_unpacku_low
; vec_unpacks_float_hi
; vec_unpacks_float_lo
; vec_unpacku_float_hi
; vec_unpacku_float_lo
