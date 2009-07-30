;; AltiVec patterns.
;; Copyright (C) 2002, 2003, 2004, 2005, 2006, 2007, 2008
;; Free Software Foundation, Inc.
;; Contributed by Aldy Hernandez (aldy@quesejoda.com)

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

(define_constants
   ;; 51-62 deleted
  [(UNSPEC_VCMPBFP       64)
   (UNSPEC_VMSUMU        65)
   (UNSPEC_VMSUMM        66)
   (UNSPEC_VMSUMSHM      68)
   (UNSPEC_VMSUMUHS      69)
   (UNSPEC_VMSUMSHS      70)
   (UNSPEC_VMHADDSHS     71)
   (UNSPEC_VMHRADDSHS    72)
   (UNSPEC_VMLADDUHM     73)
   (UNSPEC_VADDCUW       75)
   (UNSPEC_VADDU         76)
   (UNSPEC_VADDS         77)
   (UNSPEC_VAVGU         80)
   (UNSPEC_VAVGS         81)
   (UNSPEC_VMULEUB       83)
   (UNSPEC_VMULESB       84)
   (UNSPEC_VMULEUH       85)
   (UNSPEC_VMULESH       86)
   (UNSPEC_VMULOUB       87)
   (UNSPEC_VMULOSB       88)
   (UNSPEC_VMULOUH       89)
   (UNSPEC_VMULOSH       90)
   (UNSPEC_VPKUHUM       93)
   (UNSPEC_VPKUWUM       94)
   (UNSPEC_VPKPX         95)
   (UNSPEC_VPKSHSS       97)
   (UNSPEC_VPKSWSS       99)
   (UNSPEC_VPKUHUS      100)
   (UNSPEC_VPKSHUS      101)
   (UNSPEC_VPKUWUS      102)
   (UNSPEC_VPKSWUS      103)
   ;; 104 deleted
   (UNSPEC_VSLV4SI      110)
   (UNSPEC_VSLO         111)
   (UNSPEC_VSR          118)
   (UNSPEC_VSRO         119)
   (UNSPEC_VSUBCUW      124)
   (UNSPEC_VSUBU        125)
   (UNSPEC_VSUBS        126)
   (UNSPEC_VSUM4UBS     131)
   (UNSPEC_VSUM4S       132)
   (UNSPEC_VSUM2SWS     134)
   (UNSPEC_VSUMSWS      135)
   (UNSPEC_VPERM        144)
   (UNSPEC_VPERM_UNS    145)
   ;; 148 deleted
   (UNSPEC_VRFIN        149)
   ;; 150 deleted
   (UNSPEC_VCFUX        151)
   (UNSPEC_VCFSX        152)
   (UNSPEC_VCTUXS       153)
   (UNSPEC_VCTSXS       154)
   (UNSPEC_VLOGEFP      155)
   (UNSPEC_VEXPTEFP     156)
   (UNSPEC_VRSQRTEFP    157)
   (UNSPEC_VREFP        158)
   ;; 159-162 deleted
   (UNSPEC_VLSDOI       163)
   (UNSPEC_VUPKHSB      167)
   (UNSPEC_VUPKHPX      168)
   (UNSPEC_VUPKHSH      169)
   (UNSPEC_VUPKLSB      170)
   (UNSPEC_VUPKLPX      171)
   (UNSPEC_VUPKLSH      172)
   ;; 173 deleted
   (UNSPEC_DST          190)
   (UNSPEC_DSTT         191)
   (UNSPEC_DSTST        192)
   (UNSPEC_DSTSTT       193)
   (UNSPEC_LVSL         194)
   (UNSPEC_LVSR         195)
   (UNSPEC_LVE          196)
   (UNSPEC_STVX         201)
   (UNSPEC_STVXL        202)
   (UNSPEC_STVE         203)
   (UNSPEC_SET_VSCR     213)
   (UNSPEC_GET_VRSAVE   214)
   ;; 215 deleted
   (UNSPEC_REDUC_PLUS   217)
   (UNSPEC_VECSH        219)
   (UNSPEC_EXTEVEN_V4SI 220)
   (UNSPEC_EXTEVEN_V8HI 221)
   (UNSPEC_EXTEVEN_V16QI 222)
   (UNSPEC_EXTEVEN_V4SF 223)
   (UNSPEC_EXTODD_V4SI  224)
   (UNSPEC_EXTODD_V8HI  225)
   (UNSPEC_EXTODD_V16QI 226)
   (UNSPEC_EXTODD_V4SF  227)
   (UNSPEC_INTERHI_V4SI 228)
   (UNSPEC_INTERHI_V8HI 229)
   (UNSPEC_INTERHI_V16QI 230)
   ;; delete 231
   (UNSPEC_INTERLO_V4SI 232)
   (UNSPEC_INTERLO_V8HI 233)
   (UNSPEC_INTERLO_V16QI 234)
   ;; delete 235
   (UNSPEC_LVLX         236)
   (UNSPEC_LVLXL        237)
   (UNSPEC_LVRX         238)
   (UNSPEC_LVRXL        239)
   (UNSPEC_STVLX        240)
   (UNSPEC_STVLXL       241)
   (UNSPEC_STVRX        242)
   (UNSPEC_STVRXL       243)
   (UNSPEC_VMULWHUB     308)
   (UNSPEC_VMULWLUB     309)
   (UNSPEC_VMULWHSB     310)
   (UNSPEC_VMULWLSB     311)
   (UNSPEC_VMULWHUH     312)
   (UNSPEC_VMULWLUH     313)
   (UNSPEC_VMULWHSH     314)
   (UNSPEC_VMULWLSH     315)
   (UNSPEC_VUPKHUB      316)
   (UNSPEC_VUPKHUH      317)
   (UNSPEC_VUPKLUB      318)
   (UNSPEC_VUPKLUH      319)
   (UNSPEC_VPERMSI	320)
   (UNSPEC_VPERMHI	321)
   (UNSPEC_INTERHI      322)
   (UNSPEC_INTERLO      323)
   (UNSPEC_VUPKHS_V4SF   324)
   (UNSPEC_VUPKLS_V4SF   325)
   (UNSPEC_VUPKHU_V4SF   326)
   (UNSPEC_VUPKLU_V4SF   327)
])

(define_constants
  [(UNSPECV_SET_VRSAVE   30)
   (UNSPECV_MTVSCR      186)
   (UNSPECV_MFVSCR      187)
   (UNSPECV_DSSALL      188)
   (UNSPECV_DSS         189)
  ])

;; Vec int modes
(define_mode_iterator VI [V4SI V8HI V16QI])
;; Short vec in modes
(define_mode_iterator VIshort [V8HI V16QI])
;; Vec float modes
(define_mode_iterator VF [V4SF])
;; Vec modes, pity mode iterators are not composable
(define_mode_iterator V [V4SI V8HI V16QI V4SF])
;; Vec modes for move/logical/permute ops, include vector types for move not
;; otherwise handled by altivec (v2df, v2di, ti)
(define_mode_iterator VM [V4SI V8HI V16QI V4SF V2DF V2DI TI])

;; Like VM, except don't do TImode
(define_mode_iterator VM2 [V4SI V8HI V16QI V4SF V2DF V2DI])

(define_mode_attr VI_char [(V4SI "w") (V8HI "h") (V16QI "b")])

;; Vector move instructions.
(define_insn "*altivec_mov<mode>"
  [(set (match_operand:VM2 0 "nonimmediate_operand" "=Z,v,v,*o,*r,*r,v,v")
	(match_operand:VM2 1 "input_operand" "v,Z,v,r,o,r,j,W"))]
  "VECTOR_MEM_ALTIVEC_P (<MODE>mode)
   && (register_operand (operands[0], <MODE>mode) 
       || register_operand (operands[1], <MODE>mode))"
{
  switch (which_alternative)
    {
    case 0: return "stvx %1,%y0";
    case 1: return "lvx %0,%y1";
    case 2: return "vor %0,%1,%1";
    case 3: return "#";
    case 4: return "#";
    case 5: return "#";
    case 6: return "vxor %0,%0,%0";
    case 7: return output_vec_const_move (operands);
    default: gcc_unreachable ();
    }
}
  [(set_attr "type" "vecstore,vecload,vecsimple,store,load,*,vecsimple,*")])

;; Unlike other altivec moves, allow the GPRs, since a normal use of TImode
;; is for unions.  However for plain data movement, slightly favor the vector
;; loads
(define_insn "*altivec_movti"
  [(set (match_operand:TI 0 "nonimmediate_operand" "=Z,v,v,?o,?r,?r,v,v")
	(match_operand:TI 1 "input_operand" "v,Z,v,r,o,r,j,W"))]
  "VECTOR_MEM_ALTIVEC_P (TImode)
   && (register_operand (operands[0], TImode) 
       || register_operand (operands[1], TImode))"
{
  switch (which_alternative)
    {
    case 0: return "stvx %1,%y0";
    case 1: return "lvx %0,%y1";
    case 2: return "vor %0,%1,%1";
    case 3: return "#";
    case 4: return "#";
    case 5: return "#";
    case 6: return "vxor %0,%0,%0";
    case 7: return output_vec_const_move (operands);
    default: gcc_unreachable ();
    }
}
  [(set_attr "type" "vecstore,vecload,vecsimple,store,load,*,vecsimple,*")])

;; Load up a vector with the most significant bit set by loading up -1 and
;; doing a shift left
(define_split
  [(set (match_operand:VM 0 "altivec_register_operand" "")
	(match_operand:VM 1 "easy_vector_constant_msb" ""))]
  "VECTOR_UNIT_ALTIVEC_P (<MODE>mode) && reload_completed"
  [(const_int 0)]
{
  rtx dest = operands[0];
  enum machine_mode mode = GET_MODE (operands[0]);
  rtvec v;
  int i, num_elements;

  if (mode == V4SFmode)
    {
      mode = V4SImode;
      dest = gen_lowpart (V4SImode, dest);
    }

  num_elements = GET_MODE_NUNITS (mode);
  v = rtvec_alloc (num_elements);
  for (i = 0; i < num_elements; i++)
    RTVEC_ELT (v, i) = constm1_rtx;

  emit_insn (gen_vec_initv4si (dest, gen_rtx_PARALLEL (mode, v)));
  emit_insn (gen_rtx_SET (VOIDmode, dest, gen_rtx_ASHIFT (mode, dest, dest)));
  DONE;
})

(define_split
  [(set (match_operand:VM 0 "altivec_register_operand" "")
	(match_operand:VM 1 "easy_vector_constant_add_self" ""))]
  "VECTOR_UNIT_ALTIVEC_P (<MODE>mode) && reload_completed"
  [(set (match_dup 0) (match_dup 3))
   (set (match_dup 0) (match_dup 4))]
{
  rtx dup = gen_easy_altivec_constant (operands[1]);
  rtx const_vec;
  enum machine_mode op_mode = <MODE>mode;

  /* Divide the operand of the resulting VEC_DUPLICATE, and use
     simplify_rtx to make a CONST_VECTOR.  */
  XEXP (dup, 0) = simplify_const_binary_operation (ASHIFTRT, QImode,
						   XEXP (dup, 0), const1_rtx);
  const_vec = simplify_rtx (dup);

  if (op_mode == V4SFmode)
    {
      op_mode = V4SImode;
      operands[0] = gen_lowpart (op_mode, operands[0]);
    }
  if (GET_MODE (const_vec) == op_mode)
    operands[3] = const_vec;
  else
    operands[3] = gen_lowpart (op_mode, const_vec);
  operands[4] = gen_rtx_PLUS (op_mode, operands[0], operands[0]);
})

(define_insn "get_vrsave_internal"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(unspec:SI [(reg:SI 109)] UNSPEC_GET_VRSAVE))]
  "TARGET_ALTIVEC"
{
  if (TARGET_MACHO)
     return "mfspr %0,256";
  else
     return "mfvrsave %0";
}
  [(set_attr "type" "*")])

(define_insn "*set_vrsave_internal"
  [(match_parallel 0 "vrsave_operation"
     [(set (reg:SI 109)
	   (unspec_volatile:SI [(match_operand:SI 1 "register_operand" "r")
				(reg:SI 109)] UNSPECV_SET_VRSAVE))])]
  "TARGET_ALTIVEC"
{
  if (TARGET_MACHO)
    return "mtspr 256,%1";
  else
    return "mtvrsave %1";
}
  [(set_attr "type" "*")])

(define_insn "*save_world"
 [(match_parallel 0 "save_world_operation"
                  [(clobber (reg:SI 65))
                   (use (match_operand:SI 1 "call_operand" "s"))])]
 "TARGET_MACHO && (DEFAULT_ABI == ABI_DARWIN) && TARGET_32BIT"         
 "bl %z1"
  [(set_attr "type" "branch")
   (set_attr "length" "4")])

(define_insn "*restore_world"
 [(match_parallel 0 "restore_world_operation"
                  [(return)
		   (use (reg:SI 65))
                   (use (match_operand:SI 1 "call_operand" "s"))
                   (clobber (match_operand:SI 2 "gpc_reg_operand" "=r"))])]
 "TARGET_MACHO && (DEFAULT_ABI == ABI_DARWIN) && TARGET_32BIT"
 "b %z1")

;; Simple binary operations.

;; add
(define_insn "add<mode>3"
  [(set (match_operand:VI 0 "register_operand" "=v")
        (plus:VI (match_operand:VI 1 "register_operand" "v")
                 (match_operand:VI 2 "register_operand" "v")))]
  "TARGET_ALTIVEC"
  "vaddu<VI_char>m %0,%1,%2"
  [(set_attr "type" "vecsimple")])

(define_insn "*altivec_addv4sf3"
  [(set (match_operand:V4SF 0 "register_operand" "=v")
        (plus:V4SF (match_operand:V4SF 1 "register_operand" "v")
	 	   (match_operand:V4SF 2 "register_operand" "v")))]
  "VECTOR_UNIT_ALTIVEC_P (V4SFmode)"
  "vaddfp %0,%1,%2"
  [(set_attr "type" "vecfloat")])

(define_insn "altivec_vaddcuw"
  [(set (match_operand:V4SI 0 "register_operand" "=v")
        (unspec:V4SI [(match_operand:V4SI 1 "register_operand" "v")
                      (match_operand:V4SI 2 "register_operand" "v")]
		     UNSPEC_VADDCUW))]
  "TARGET_ALTIVEC"
  "vaddcuw %0,%1,%2"
  [(set_attr "type" "vecsimple")])

(define_insn "altivec_vaddu<VI_char>s"
  [(set (match_operand:VI 0 "register_operand" "=v")
        (unspec:VI [(match_operand:VI 1 "register_operand" "v")
                    (match_operand:VI 2 "register_operand" "v")]
		   UNSPEC_VADDU))
   (set (reg:SI 110) (unspec:SI [(const_int 0)] UNSPEC_SET_VSCR))]
  "TARGET_ALTIVEC"
  "vaddu<VI_char>s %0,%1,%2"
  [(set_attr "type" "vecsimple")])

(define_insn "altivec_vadds<VI_char>s"
  [(set (match_operand:VI 0 "register_operand" "=v")
        (unspec:VI [(match_operand:VI 1 "register_operand" "v")
                    (match_operand:VI 2 "register_operand" "v")]
		   UNSPEC_VADDS))
   (set (reg:SI 110) (unspec:SI [(const_int 0)] UNSPEC_SET_VSCR))]
  "TARGET_ALTIVEC"
  "vadds<VI_char>s %0,%1,%2"
  [(set_attr "type" "vecsimple")])

;; sub
(define_insn "sub<mode>3"
  [(set (match_operand:VI 0 "register_operand" "=v")
        (minus:VI (match_operand:VI 1 "register_operand" "v")
                  (match_operand:VI 2 "register_operand" "v")))]
  "TARGET_ALTIVEC"
  "vsubu<VI_char>m %0,%1,%2"
  [(set_attr "type" "vecsimple")])

(define_insn "*altivec_subv4sf3"
  [(set (match_operand:V4SF 0 "register_operand" "=v")
        (minus:V4SF (match_operand:V4SF 1 "register_operand" "v")
                    (match_operand:V4SF 2 "register_operand" "v")))]
  "VECTOR_UNIT_ALTIVEC_P (V4SFmode)"
  "vsubfp %0,%1,%2"
  [(set_attr "type" "vecfloat")])

(define_insn "altivec_vsubcuw"
  [(set (match_operand:V4SI 0 "register_operand" "=v")
        (unspec:V4SI [(match_operand:V4SI 1 "register_operand" "v")
                      (match_operand:V4SI 2 "register_operand" "v")]
		     UNSPEC_VSUBCUW))]
  "TARGET_ALTIVEC"
  "vsubcuw %0,%1,%2"
  [(set_attr "type" "vecsimple")])

(define_insn "altivec_vsubu<VI_char>s"
  [(set (match_operand:VI 0 "register_operand" "=v")
        (unspec:VI [(match_operand:VI 1 "register_operand" "v")
                    (match_operand:VI 2 "register_operand" "v")]
		   UNSPEC_VSUBU))
   (set (reg:SI 110) (unspec:SI [(const_int 0)] UNSPEC_SET_VSCR))]
  "TARGET_ALTIVEC"
  "vsubu<VI_char>s %0,%1,%2"
  [(set_attr "type" "vecsimple")])

(define_insn "altivec_vsubs<VI_char>s"
  [(set (match_operand:VI 0 "register_operand" "=v")
        (unspec:VI [(match_operand:VI 1 "register_operand" "v")
                    (match_operand:VI 2 "register_operand" "v")]
		   UNSPEC_VSUBS))
   (set (reg:SI 110) (unspec:SI [(const_int 0)] UNSPEC_SET_VSCR))]
  "TARGET_ALTIVEC"
  "vsubs<VI_char>s %0,%1,%2"
  [(set_attr "type" "vecsimple")])

;;
(define_insn "altivec_vavgu<VI_char>"
  [(set (match_operand:VI 0 "register_operand" "=v")
        (unspec:VI [(match_operand:VI 1 "register_operand" "v")
                    (match_operand:VI 2 "register_operand" "v")]
		   UNSPEC_VAVGU))]
  "TARGET_ALTIVEC"
  "vavgu<VI_char> %0,%1,%2"
  [(set_attr "type" "vecsimple")])

(define_insn "altivec_vavgs<VI_char>"
  [(set (match_operand:VI 0 "register_operand" "=v")
        (unspec:VI [(match_operand:VI 1 "register_operand" "v")
                    (match_operand:VI 2 "register_operand" "v")]
		   UNSPEC_VAVGS))]
  "TARGET_ALTIVEC"
  "vavgs<VI_char> %0,%1,%2"
  [(set_attr "type" "vecsimple")])

(define_insn "altivec_vcmpbfp"
  [(set (match_operand:V4SI 0 "register_operand" "=v")
        (unspec:V4SI [(match_operand:V4SF 1 "register_operand" "v")
                      (match_operand:V4SF 2 "register_operand" "v")] 
                      UNSPEC_VCMPBFP))]
  "TARGET_ALTIVEC"
  "vcmpbfp %0,%1,%2"
  [(set_attr "type" "veccmp")])

(define_insn "*altivec_eq<mode>"
  [(set (match_operand:VI 0 "altivec_register_operand" "=v")
	(eq:VI (match_operand:VI 1 "altivec_register_operand" "v")
	       (match_operand:VI 2 "altivec_register_operand" "v")))]
  "TARGET_ALTIVEC"
  "vcmpequ<VI_char> %0,%1,%2"
  [(set_attr "type" "veccmp")])

(define_insn "*altivec_gt<mode>"
  [(set (match_operand:VI 0 "altivec_register_operand" "=v")
	(gt:VI (match_operand:VI 1 "altivec_register_operand" "v")
	       (match_operand:VI 2 "altivec_register_operand" "v")))]
  "TARGET_ALTIVEC"
  "vcmpgts<VI_char> %0,%1,%2"
  [(set_attr "type" "veccmp")])

(define_insn "*altivec_gtu<mode>"
  [(set (match_operand:VI 0 "altivec_register_operand" "=v")
	(gtu:VI (match_operand:VI 1 "altivec_register_operand" "v")
		(match_operand:VI 2 "altivec_register_operand" "v")))]
  "TARGET_ALTIVEC"
  "vcmpgtu<VI_char> %0,%1,%2"
  [(set_attr "type" "veccmp")])

(define_insn "*altivec_eqv4sf"
  [(set (match_operand:V4SF 0 "altivec_register_operand" "=v")
	(eq:V4SF (match_operand:V4SF 1 "altivec_register_operand" "v")
		 (match_operand:V4SF 2 "altivec_register_operand" "v")))]
  "VECTOR_UNIT_ALTIVEC_P (V4SFmode)"
  "vcmpeqfp %0,%1,%2"
  [(set_attr "type" "veccmp")])

(define_insn "*altivec_gtv4sf"
  [(set (match_operand:V4SF 0 "altivec_register_operand" "=v")
	(gt:V4SF (match_operand:V4SF 1 "altivec_register_operand" "v")
		 (match_operand:V4SF 2 "altivec_register_operand" "v")))]
  "VECTOR_UNIT_ALTIVEC_P (V4SFmode)"
  "vcmpgtfp %0,%1,%2"
  [(set_attr "type" "veccmp")])

(define_insn "*altivec_gev4sf"
  [(set (match_operand:V4SF 0 "altivec_register_operand" "=v")
	(ge:V4SF (match_operand:V4SF 1 "altivec_register_operand" "v")
		 (match_operand:V4SF 2 "altivec_register_operand" "v")))]
  "VECTOR_UNIT_ALTIVEC_P (V4SFmode)"
  "vcmpgefp %0,%1,%2"
  [(set_attr "type" "veccmp")])

(define_insn "*altivec_vsel<mode>"
  [(set (match_operand:VM 0 "altivec_register_operand" "=v")
	(if_then_else:VM
	 (ne:CC (match_operand:VM 1 "altivec_register_operand" "v")
		(const_int 0))
	 (match_operand:VM 2 "altivec_register_operand" "v")
	 (match_operand:VM 3 "altivec_register_operand" "v")))]
  "VECTOR_MEM_ALTIVEC_P (<MODE>mode)"
  "vsel %0,%3,%2,%1"
  [(set_attr "type" "vecperm")])

(define_insn "*altivec_vsel<mode>_uns"
  [(set (match_operand:VM 0 "altivec_register_operand" "=v")
	(if_then_else:VM
	 (ne:CCUNS (match_operand:VM 1 "altivec_register_operand" "v")
		   (const_int 0))
	 (match_operand:VM 2 "altivec_register_operand" "v")
	 (match_operand:VM 3 "altivec_register_operand" "v")))]
  "VECTOR_MEM_ALTIVEC_P (<MODE>mode)"
  "vsel %0,%3,%2,%1"
  [(set_attr "type" "vecperm")])

;; Fused multiply add
(define_insn "altivec_vmaddfp"
  [(set (match_operand:V4SF 0 "register_operand" "=v")
	(plus:V4SF (mult:V4SF (match_operand:V4SF 1 "register_operand" "v")
			      (match_operand:V4SF 2 "register_operand" "v"))
	  	   (match_operand:V4SF 3 "register_operand" "v")))]
  "VECTOR_UNIT_ALTIVEC_P (V4SFmode)"
  "vmaddfp %0,%1,%2,%3"
  [(set_attr "type" "vecfloat")])

;; We do multiply as a fused multiply-add with an add of a -0.0 vector.

(define_expand "altivec_mulv4sf3"
  [(use (match_operand:V4SF 0 "register_operand" ""))
   (use (match_operand:V4SF 1 "register_operand" ""))
   (use (match_operand:V4SF 2 "register_operand" ""))]
  "VECTOR_UNIT_ALTIVEC_P (V4SFmode) && TARGET_FUSED_MADD"
  "
{
  rtx neg0;

  /* Generate [-0.0, -0.0, -0.0, -0.0].  */
  neg0 = gen_reg_rtx (V4SImode);
  emit_insn (gen_altivec_vspltisw (neg0, constm1_rtx));
  emit_insn (gen_vashlv4si3 (neg0, neg0, neg0));

  /* Use the multiply-add.  */
  emit_insn (gen_altivec_vmaddfp (operands[0], operands[1], operands[2],
				  gen_lowpart (V4SFmode, neg0)));
  DONE;
}")

;; 32-bit integer multiplication
;; A_high = Operand_0 & 0xFFFF0000 >> 16
;; A_low = Operand_0 & 0xFFFF
;; B_high = Operand_1 & 0xFFFF0000 >> 16
;; B_low = Operand_1 & 0xFFFF
;; result = A_low * B_low + (A_high * B_low + B_high * A_low) << 16

;; (define_insn "mulv4si3"
;;   [(set (match_operand:V4SI 0 "register_operand" "=v")
;;         (mult:V4SI (match_operand:V4SI 1 "register_operand" "v")
;;                    (match_operand:V4SI 2 "register_operand" "v")))]
(define_expand "mulv4si3"
  [(use (match_operand:V4SI 0 "register_operand" ""))
   (use (match_operand:V4SI 1 "register_operand" ""))
   (use (match_operand:V4SI 2 "register_operand" ""))]
   "TARGET_ALTIVEC"
   "
 {
   rtx zero;
   rtx swap;
   rtx small_swap;
   rtx sixteen;
   rtx one;
   rtx two;
   rtx low_product;
   rtx high_product;
       
   zero = gen_reg_rtx (V4SImode);
   emit_insn (gen_altivec_vspltisw (zero, const0_rtx));
 
   sixteen = gen_reg_rtx (V4SImode);   
   emit_insn (gen_altivec_vspltisw (sixteen,  gen_rtx_CONST_INT (V4SImode, -16)));
 
   swap = gen_reg_rtx (V4SImode);
   emit_insn (gen_vrotlv4si3 (swap, operands[2], sixteen));
 
   one = gen_reg_rtx (V8HImode);
   convert_move (one, operands[1], 0);
 
   two = gen_reg_rtx (V8HImode);
   convert_move (two, operands[2], 0);
 
   small_swap = gen_reg_rtx (V8HImode);
   convert_move (small_swap, swap, 0);
 
   low_product = gen_reg_rtx (V4SImode);
   emit_insn (gen_altivec_vmulouh (low_product, one, two));
 
   high_product = gen_reg_rtx (V4SImode);
   emit_insn (gen_altivec_vmsumuhm (high_product, one, small_swap, zero));
 
   emit_insn (gen_vashlv4si3 (high_product, high_product, sixteen));
 
   emit_insn (gen_addv4si3 (operands[0], high_product, low_product));
   
   DONE;
 }")
 
(define_expand "mulv8hi3"
  [(use (match_operand:V8HI 0 "register_operand" ""))
   (use (match_operand:V8HI 1 "register_operand" ""))
   (use (match_operand:V8HI 2 "register_operand" ""))]
   "TARGET_ALTIVEC"
   "
{
   rtx odd = gen_reg_rtx (V4SImode);
   rtx even = gen_reg_rtx (V4SImode);
   rtx high = gen_reg_rtx (V4SImode);
   rtx low = gen_reg_rtx (V4SImode);

   emit_insn (gen_altivec_vmulesh (even, operands[1], operands[2]));
   emit_insn (gen_altivec_vmulosh (odd, operands[1], operands[2]));

   emit_insn (gen_altivec_vmrghw (high, even, odd));
   emit_insn (gen_altivec_vmrglw (low, even, odd));

   emit_insn (gen_altivec_vpkuwum (operands[0], high, low));

   DONE;
}")

;; Fused multiply subtract 
(define_insn "altivec_vnmsubfp"
  [(set (match_operand:V4SF 0 "register_operand" "=v")
	(neg:V4SF (minus:V4SF (mult:V4SF (match_operand:V4SF 1 "register_operand" "v")
			       (match_operand:V4SF 2 "register_operand" "v"))
	  	    (match_operand:V4SF 3 "register_operand" "v"))))]
  "VECTOR_UNIT_ALTIVEC_P (V4SFmode)"
  "vnmsubfp %0,%1,%2,%3"
  [(set_attr "type" "vecfloat")])

(define_insn "altivec_vmsumu<VI_char>m"
  [(set (match_operand:V4SI 0 "register_operand" "=v")
        (unspec:V4SI [(match_operand:VIshort 1 "register_operand" "v")
		      (match_operand:VIshort 2 "register_operand" "v")
                      (match_operand:V4SI 3 "register_operand" "v")]
		     UNSPEC_VMSUMU))]
  "TARGET_ALTIVEC"
  "vmsumu<VI_char>m %0,%1,%2,%3"
  [(set_attr "type" "veccomplex")])

(define_insn "altivec_vmsumm<VI_char>m"
  [(set (match_operand:V4SI 0 "register_operand" "=v")
        (unspec:V4SI [(match_operand:VIshort 1 "register_operand" "v")
		      (match_operand:VIshort 2 "register_operand" "v")
                      (match_operand:V4SI 3 "register_operand" "v")]
		     UNSPEC_VMSUMM))]
  "TARGET_ALTIVEC"
  "vmsumm<VI_char>m %0,%1,%2,%3"
  [(set_attr "type" "veccomplex")])

(define_insn "altivec_vmsumshm"
  [(set (match_operand:V4SI 0 "register_operand" "=v")
        (unspec:V4SI [(match_operand:V8HI 1 "register_operand" "v")
		      (match_operand:V8HI 2 "register_operand" "v")
                      (match_operand:V4SI 3 "register_operand" "v")]
		     UNSPEC_VMSUMSHM))]
  "TARGET_ALTIVEC"
  "vmsumshm %0,%1,%2,%3"
  [(set_attr "type" "veccomplex")])

(define_insn "altivec_vmsumuhs"
  [(set (match_operand:V4SI 0 "register_operand" "=v")
        (unspec:V4SI [(match_operand:V8HI 1 "register_operand" "v")
		      (match_operand:V8HI 2 "register_operand" "v")
                      (match_operand:V4SI 3 "register_operand" "v")]
		     UNSPEC_VMSUMUHS))
   (set (reg:SI 110) (unspec:SI [(const_int 0)] UNSPEC_SET_VSCR))]
  "TARGET_ALTIVEC"
  "vmsumuhs %0,%1,%2,%3"
  [(set_attr "type" "veccomplex")])

(define_insn "altivec_vmsumshs"
  [(set (match_operand:V4SI 0 "register_operand" "=v")
        (unspec:V4SI [(match_operand:V8HI 1 "register_operand" "v")
		      (match_operand:V8HI 2 "register_operand" "v")
                      (match_operand:V4SI 3 "register_operand" "v")]
		     UNSPEC_VMSUMSHS))
   (set (reg:SI 110) (unspec:SI [(const_int 0)] UNSPEC_SET_VSCR))]
  "TARGET_ALTIVEC"
  "vmsumshs %0,%1,%2,%3"
  [(set_attr "type" "veccomplex")])

;; max

(define_insn "umax<mode>3"
  [(set (match_operand:VI 0 "register_operand" "=v")
        (umax:VI (match_operand:VI 1 "register_operand" "v")
                 (match_operand:VI 2 "register_operand" "v")))]
  "TARGET_ALTIVEC"
  "vmaxu<VI_char> %0,%1,%2"
  [(set_attr "type" "vecsimple")])

(define_insn "smax<mode>3"
  [(set (match_operand:VI 0 "register_operand" "=v")
        (smax:VI (match_operand:VI 1 "register_operand" "v")
                 (match_operand:VI 2 "register_operand" "v")))]
  "TARGET_ALTIVEC"
  "vmaxs<VI_char> %0,%1,%2"
  [(set_attr "type" "vecsimple")])

(define_insn "*altivec_smaxv4sf3"
  [(set (match_operand:V4SF 0 "register_operand" "=v")
        (smax:V4SF (match_operand:V4SF 1 "register_operand" "v")
                   (match_operand:V4SF 2 "register_operand" "v")))]
  "VECTOR_UNIT_ALTIVEC_P (V4SFmode)"
  "vmaxfp %0,%1,%2"
  [(set_attr "type" "veccmp")])

(define_insn "umin<mode>3"
  [(set (match_operand:VI 0 "register_operand" "=v")
        (umin:VI (match_operand:VI 1 "register_operand" "v")
                 (match_operand:VI 2 "register_operand" "v")))]
  "TARGET_ALTIVEC"
  "vminu<VI_char> %0,%1,%2"
  [(set_attr "type" "vecsimple")])

(define_insn "smin<mode>3"
  [(set (match_operand:VI 0 "register_operand" "=v")
        (smin:VI (match_operand:VI 1 "register_operand" "v")
                 (match_operand:VI 2 "register_operand" "v")))]
  "TARGET_ALTIVEC"
  "vmins<VI_char> %0,%1,%2"
  [(set_attr "type" "vecsimple")])

(define_insn "*altivec_sminv4sf3"
  [(set (match_operand:V4SF 0 "register_operand" "=v")
        (smin:V4SF (match_operand:V4SF 1 "register_operand" "v")
                   (match_operand:V4SF 2 "register_operand" "v")))]
  "VECTOR_UNIT_ALTIVEC_P (V4SFmode)"
  "vminfp %0,%1,%2"
  [(set_attr "type" "veccmp")])

(define_insn "altivec_vmhaddshs"
  [(set (match_operand:V8HI 0 "register_operand" "=v")
        (unspec:V8HI [(match_operand:V8HI 1 "register_operand" "v")
		      (match_operand:V8HI 2 "register_operand" "v")
                      (match_operand:V8HI 3 "register_operand" "v")]
		     UNSPEC_VMHADDSHS))
   (set (reg:SI 110) (unspec:SI [(const_int 0)] UNSPEC_SET_VSCR))]
  "TARGET_ALTIVEC"
  "vmhaddshs %0,%1,%2,%3"
  [(set_attr "type" "veccomplex")])

(define_insn "altivec_vmhraddshs"
  [(set (match_operand:V8HI 0 "register_operand" "=v")
        (unspec:V8HI [(match_operand:V8HI 1 "register_operand" "v")
		      (match_operand:V8HI 2 "register_operand" "v")
                      (match_operand:V8HI 3 "register_operand" "v")]
		     UNSPEC_VMHRADDSHS))
   (set (reg:SI 110) (unspec:SI [(const_int 0)] UNSPEC_SET_VSCR))]
  "TARGET_ALTIVEC"
  "vmhraddshs %0,%1,%2,%3"
  [(set_attr "type" "veccomplex")])

(define_insn "altivec_vmladduhm"
  [(set (match_operand:V8HI 0 "register_operand" "=v")
        (unspec:V8HI [(match_operand:V8HI 1 "register_operand" "v")
		      (match_operand:V8HI 2 "register_operand" "v")
                      (match_operand:V8HI 3 "register_operand" "v")]
		     UNSPEC_VMLADDUHM))]
  "TARGET_ALTIVEC"
  "vmladduhm %0,%1,%2,%3"
  [(set_attr "type" "veccomplex")])

(define_insn "altivec_vmrghb"
  [(set (match_operand:V16QI 0 "register_operand" "=v")
        (vec_merge:V16QI (vec_select:V16QI (match_operand:V16QI 1 "register_operand" "v")
					   (parallel [(const_int 0)
					   	      (const_int 8)
					   	      (const_int 1)
					   	      (const_int 9)
					   	      (const_int 2)
					   	      (const_int 10)
						      (const_int 3)
						      (const_int 11)
					   	      (const_int 4)
					   	      (const_int 12)
					   	      (const_int 5)
					   	      (const_int 13)
					   	      (const_int 6)
					   	      (const_int 14)
					   	      (const_int 7)
						      (const_int 15)]))
                        (vec_select:V16QI (match_operand:V16QI 2 "register_operand" "v")
					   (parallel [(const_int 8)
					   	      (const_int 0)
					   	      (const_int 9)
					   	      (const_int 1)
					   	      (const_int 10)
					   	      (const_int 2)
						      (const_int 11)
						      (const_int 3)
					   	      (const_int 12)
					   	      (const_int 4)
					   	      (const_int 13)
					   	      (const_int 5)
					   	      (const_int 14)
					   	      (const_int 6)
					   	      (const_int 15)
						      (const_int 7)]))
		      (const_int 21845)))]
  "TARGET_ALTIVEC"
  "vmrghb %0,%1,%2"
  [(set_attr "type" "vecperm")])

(define_insn "altivec_vmrghh"
  [(set (match_operand:V8HI 0 "register_operand" "=v")
        (vec_merge:V8HI (vec_select:V8HI (match_operand:V8HI 1 "register_operand" "v")
					   (parallel [(const_int 0)
					   	      (const_int 4)
					   	      (const_int 1)
					   	      (const_int 5)
					   	      (const_int 2)
					   	      (const_int 6)
					   	      (const_int 3)
					   	      (const_int 7)]))
                        (vec_select:V8HI (match_operand:V8HI 2 "register_operand" "v")
					   (parallel [(const_int 4)
					   	      (const_int 0)
					   	      (const_int 5)
					   	      (const_int 1)
					   	      (const_int 6)
					   	      (const_int 2)
					   	      (const_int 7)
					   	      (const_int 3)]))
		      (const_int 85)))]
  "TARGET_ALTIVEC"
  "vmrghh %0,%1,%2"
  [(set_attr "type" "vecperm")])

(define_insn "altivec_vmrghw"
  [(set (match_operand:V4SI 0 "register_operand" "=v")
        (vec_merge:V4SI (vec_select:V4SI (match_operand:V4SI 1 "register_operand" "v")
					 (parallel [(const_int 0)
					 	    (const_int 2)
						    (const_int 1)
						    (const_int 3)]))
                        (vec_select:V4SI (match_operand:V4SI 2 "register_operand" "v")
					 (parallel [(const_int 2)
					 	    (const_int 0)
						    (const_int 3)
						    (const_int 1)]))
		      (const_int 5)))]
  "VECTOR_MEM_ALTIVEC_P (V4SImode)"
  "vmrghw %0,%1,%2"
  [(set_attr "type" "vecperm")])

(define_insn "*altivec_vmrghsf"
  [(set (match_operand:V4SF 0 "register_operand" "=v")
        (vec_merge:V4SF (vec_select:V4SF (match_operand:V4SF 1 "register_operand" "v")
                                         (parallel [(const_int 0)
                                                    (const_int 2)
                                                    (const_int 1)
                                                    (const_int 3)]))
                        (vec_select:V4SF (match_operand:V4SF 2 "register_operand" "v")
                                         (parallel [(const_int 2)
                                                    (const_int 0)
                                                    (const_int 3)
                                                    (const_int 1)]))
                      (const_int 5)))]
  "VECTOR_MEM_ALTIVEC_P (V4SFmode)"
  "vmrghw %0,%1,%2"
  [(set_attr "type" "vecperm")])

(define_insn "altivec_vmrglb"
  [(set (match_operand:V16QI 0 "register_operand" "=v")
        (vec_merge:V16QI (vec_select:V16QI (match_operand:V16QI 1 "register_operand" "v")
					   (parallel [(const_int 8)
					   	      (const_int 0)
					   	      (const_int 9)
					   	      (const_int 1)
					   	      (const_int 10)
					   	      (const_int 2)
						      (const_int 11)
						      (const_int 3)
					   	      (const_int 12)
					   	      (const_int 4)
					   	      (const_int 13)
					   	      (const_int 5)
					   	      (const_int 14)
					   	      (const_int 6)
					   	      (const_int 15)
						      (const_int 7)]))
                      (vec_select:V16QI (match_operand:V16QI 2 "register_operand" "v")
					   (parallel [(const_int 0)
					   	      (const_int 8)
					   	      (const_int 1)
					   	      (const_int 9)
					   	      (const_int 2)
					   	      (const_int 10)
						      (const_int 3)
						      (const_int 11)
					   	      (const_int 4)
					   	      (const_int 12)
					   	      (const_int 5)
					   	      (const_int 13)
					   	      (const_int 6)
					   	      (const_int 14)
					   	      (const_int 7)
						      (const_int 15)]))
		      (const_int 21845)))]
  "TARGET_ALTIVEC"
  "vmrglb %0,%1,%2"
  [(set_attr "type" "vecperm")])

(define_insn "altivec_vmrglh"
  [(set (match_operand:V8HI 0 "register_operand" "=v")
        (vec_merge:V8HI (vec_select:V8HI (match_operand:V8HI 1 "register_operand" "v")
					   (parallel [(const_int 4)
					   	      (const_int 0)
					   	      (const_int 5)
					   	      (const_int 1)
					   	      (const_int 6)
					   	      (const_int 2)
					   	      (const_int 7)
					   	      (const_int 3)]))
                        (vec_select:V8HI (match_operand:V8HI 2 "register_operand" "v")
					   (parallel [(const_int 0)
					   	      (const_int 4)
					   	      (const_int 1)
					   	      (const_int 5)
					   	      (const_int 2)
					   	      (const_int 6)
					   	      (const_int 3)
					   	      (const_int 7)]))
		      (const_int 85)))]
  "TARGET_ALTIVEC"
  "vmrglh %0,%1,%2"
  [(set_attr "type" "vecperm")])

(define_insn "altivec_vmrglw"
  [(set (match_operand:V4SI 0 "register_operand" "=v")
        (vec_merge:V4SI
	 (vec_select:V4SI (match_operand:V4SI 1 "register_operand" "v")
			  (parallel [(const_int 2)
				     (const_int 0)
				     (const_int 3)
				     (const_int 1)]))
	 (vec_select:V4SI (match_operand:V4SI 2 "register_operand" "v")
			  (parallel [(const_int 0)
				     (const_int 2)
				     (const_int 1)
				     (const_int 3)]))
	 (const_int 5)))]
  "VECTOR_MEM_ALTIVEC_P (V4SImode)"
  "vmrglw %0,%1,%2"
  [(set_attr "type" "vecperm")])

(define_insn "*altivec_vmrglsf"
  [(set (match_operand:V4SF 0 "register_operand" "=v")
        (vec_merge:V4SF
	 (vec_select:V4SF (match_operand:V4SF 1 "register_operand" "v")
			  (parallel [(const_int 2)
				     (const_int 0)
				     (const_int 3)
				     (const_int 1)]))
	 (vec_select:V4SF (match_operand:V4SF 2 "register_operand" "v")
			  (parallel [(const_int 0)
				     (const_int 2)
				     (const_int 1)
				     (const_int 3)]))
	 (const_int 5)))]
  "VECTOR_MEM_ALTIVEC_P (V4SFmode)"
  "vmrglw %0,%1,%2"
  [(set_attr "type" "vecperm")])

(define_insn "altivec_vmuleub"
  [(set (match_operand:V8HI 0 "register_operand" "=v")
        (unspec:V8HI [(match_operand:V16QI 1 "register_operand" "v")
                      (match_operand:V16QI 2 "register_operand" "v")]
		     UNSPEC_VMULEUB))]
  "TARGET_ALTIVEC"
  "vmuleub %0,%1,%2"
  [(set_attr "type" "veccomplex")])

(define_insn "altivec_vmulesb"
  [(set (match_operand:V8HI 0 "register_operand" "=v")
        (unspec:V8HI [(match_operand:V16QI 1 "register_operand" "v")
                      (match_operand:V16QI 2 "register_operand" "v")]
		     UNSPEC_VMULESB))]
  "TARGET_ALTIVEC"
  "vmulesb %0,%1,%2"
  [(set_attr "type" "veccomplex")])

(define_insn "altivec_vmuleuh"
  [(set (match_operand:V4SI 0 "register_operand" "=v")
        (unspec:V4SI [(match_operand:V8HI 1 "register_operand" "v")
                      (match_operand:V8HI 2 "register_operand" "v")]
		     UNSPEC_VMULEUH))]
  "TARGET_ALTIVEC"
  "vmuleuh %0,%1,%2"
  [(set_attr "type" "veccomplex")])

(define_insn "altivec_vmulesh"
  [(set (match_operand:V4SI 0 "register_operand" "=v")
        (unspec:V4SI [(match_operand:V8HI 1 "register_operand" "v")
                      (match_operand:V8HI 2 "register_operand" "v")]
		     UNSPEC_VMULESH))]
  "TARGET_ALTIVEC"
  "vmulesh %0,%1,%2"
  [(set_attr "type" "veccomplex")])

(define_insn "altivec_vmuloub"
  [(set (match_operand:V8HI 0 "register_operand" "=v")
        (unspec:V8HI [(match_operand:V16QI 1 "register_operand" "v")
                      (match_operand:V16QI 2 "register_operand" "v")]
		     UNSPEC_VMULOUB))]
  "TARGET_ALTIVEC"
  "vmuloub %0,%1,%2"
  [(set_attr "type" "veccomplex")])

(define_insn "altivec_vmulosb"
  [(set (match_operand:V8HI 0 "register_operand" "=v")
        (unspec:V8HI [(match_operand:V16QI 1 "register_operand" "v")
                      (match_operand:V16QI 2 "register_operand" "v")]
		     UNSPEC_VMULOSB))]
  "TARGET_ALTIVEC"
  "vmulosb %0,%1,%2"
  [(set_attr "type" "veccomplex")])

(define_insn "altivec_vmulouh"
  [(set (match_operand:V4SI 0 "register_operand" "=v")
        (unspec:V4SI [(match_operand:V8HI 1 "register_operand" "v")
                      (match_operand:V8HI 2 "register_operand" "v")]
		     UNSPEC_VMULOUH))]
  "TARGET_ALTIVEC"
  "vmulouh %0,%1,%2"
  [(set_attr "type" "veccomplex")])

(define_insn "altivec_vmulosh"
  [(set (match_operand:V4SI 0 "register_operand" "=v")
        (unspec:V4SI [(match_operand:V8HI 1 "register_operand" "v")
                      (match_operand:V8HI 2 "register_operand" "v")]
		     UNSPEC_VMULOSH))]
  "TARGET_ALTIVEC"
  "vmulosh %0,%1,%2"
  [(set_attr "type" "veccomplex")])


;; logical ops.  Have the logical ops follow the memory ops in
;; terms of whether to prefer VSX or Altivec

(define_insn "*altivec_and<mode>3"
  [(set (match_operand:VM 0 "register_operand" "=v")
        (and:VM (match_operand:VM 1 "register_operand" "v")
		(match_operand:VM 2 "register_operand" "v")))]
  "VECTOR_MEM_ALTIVEC_P (<MODE>mode)"
  "vand %0,%1,%2"
  [(set_attr "type" "vecsimple")])

(define_insn "*altivec_ior<mode>3"
  [(set (match_operand:VM 0 "register_operand" "=v")
        (ior:VM (match_operand:VM 1 "register_operand" "v")
		(match_operand:VM 2 "register_operand" "v")))]
  "VECTOR_MEM_ALTIVEC_P (<MODE>mode)"
  "vor %0,%1,%2"
  [(set_attr "type" "vecsimple")])

(define_insn "*altivec_xor<mode>3"
  [(set (match_operand:VM 0 "register_operand" "=v")
        (xor:VM (match_operand:VM 1 "register_operand" "v")
		(match_operand:VM 2 "register_operand" "v")))]
  "VECTOR_MEM_ALTIVEC_P (<MODE>mode)"
  "vxor %0,%1,%2"
  [(set_attr "type" "vecsimple")])

(define_insn "*altivec_one_cmpl<mode>2"
  [(set (match_operand:VM 0 "register_operand" "=v")
        (not:VM (match_operand:VM 1 "register_operand" "v")))]
  "VECTOR_MEM_ALTIVEC_P (<MODE>mode)"
  "vnor %0,%1,%1"
  [(set_attr "type" "vecsimple")])
  
(define_insn "*altivec_nor<mode>3"
  [(set (match_operand:VM 0 "register_operand" "=v")
        (not:VM (ior:VM (match_operand:VM 1 "register_operand" "v")
			(match_operand:VM 2 "register_operand" "v"))))]
  "VECTOR_MEM_ALTIVEC_P (<MODE>mode)"
  "vnor %0,%1,%2"
  [(set_attr "type" "vecsimple")])

(define_insn "*altivec_andc<mode>3"
  [(set (match_operand:VM 0 "register_operand" "=v")
        (and:VM (not:VM (match_operand:VM 2 "register_operand" "v"))
		(match_operand:VM 1 "register_operand" "v")))]
  "VECTOR_MEM_ALTIVEC_P (<MODE>mode)"
  "vandc %0,%1,%2"
  [(set_attr "type" "vecsimple")])

(define_insn "altivec_vpkuhum"
  [(set (match_operand:V16QI 0 "register_operand" "=v")
        (unspec:V16QI [(match_operand:V8HI 1 "register_operand" "v")
                       (match_operand:V8HI 2 "register_operand" "v")]
		      UNSPEC_VPKUHUM))]
  "TARGET_ALTIVEC"
  "vpkuhum %0,%1,%2"
  [(set_attr "type" "vecperm")])

(define_insn "altivec_vpkuwum"
  [(set (match_operand:V8HI 0 "register_operand" "=v")
        (unspec:V8HI [(match_operand:V4SI 1 "register_operand" "v")
                      (match_operand:V4SI 2 "register_operand" "v")]
		     UNSPEC_VPKUWUM))]
  "TARGET_ALTIVEC"
  "vpkuwum %0,%1,%2"
  [(set_attr "type" "vecperm")])

(define_insn "altivec_vpkpx"
  [(set (match_operand:V8HI 0 "register_operand" "=v")
        (unspec:V8HI [(match_operand:V4SI 1 "register_operand" "v")
                      (match_operand:V4SI 2 "register_operand" "v")]
		     UNSPEC_VPKPX))]
  "TARGET_ALTIVEC"
  "vpkpx %0,%1,%2"
  [(set_attr "type" "vecperm")])

(define_insn "altivec_vpkshss"
  [(set (match_operand:V16QI 0 "register_operand" "=v")
        (unspec:V16QI [(match_operand:V8HI 1 "register_operand" "v")
                       (match_operand:V8HI 2 "register_operand" "v")]
		      UNSPEC_VPKSHSS))
   (set (reg:SI 110) (unspec:SI [(const_int 0)] UNSPEC_SET_VSCR))]
  "TARGET_ALTIVEC"
  "vpkshss %0,%1,%2"
  [(set_attr "type" "vecperm")])

(define_insn "altivec_vpkswss"
  [(set (match_operand:V8HI 0 "register_operand" "=v")
        (unspec:V8HI [(match_operand:V4SI 1 "register_operand" "v")
                      (match_operand:V4SI 2 "register_operand" "v")]
		     UNSPEC_VPKSWSS))
   (set (reg:SI 110) (unspec:SI [(const_int 0)] UNSPEC_SET_VSCR))]
  "TARGET_ALTIVEC"
  "vpkswss %0,%1,%2"
  [(set_attr "type" "vecperm")])

(define_insn "altivec_vpkuhus"
  [(set (match_operand:V16QI 0 "register_operand" "=v")
        (unspec:V16QI [(match_operand:V8HI 1 "register_operand" "v")
                       (match_operand:V8HI 2 "register_operand" "v")]
		      UNSPEC_VPKUHUS))
   (set (reg:SI 110) (unspec:SI [(const_int 0)] UNSPEC_SET_VSCR))]
  "TARGET_ALTIVEC"
  "vpkuhus %0,%1,%2"
  [(set_attr "type" "vecperm")])

(define_insn "altivec_vpkshus"
  [(set (match_operand:V16QI 0 "register_operand" "=v")
        (unspec:V16QI [(match_operand:V8HI 1 "register_operand" "v")
                       (match_operand:V8HI 2 "register_operand" "v")]
		      UNSPEC_VPKSHUS))
   (set (reg:SI 110) (unspec:SI [(const_int 0)] UNSPEC_SET_VSCR))]
  "TARGET_ALTIVEC"
  "vpkshus %0,%1,%2"
  [(set_attr "type" "vecperm")])

(define_insn "altivec_vpkuwus"
  [(set (match_operand:V8HI 0 "register_operand" "=v")
        (unspec:V8HI [(match_operand:V4SI 1 "register_operand" "v")
                      (match_operand:V4SI 2 "register_operand" "v")]
		     UNSPEC_VPKUWUS))
   (set (reg:SI 110) (unspec:SI [(const_int 0)] UNSPEC_SET_VSCR))]
  "TARGET_ALTIVEC"
  "vpkuwus %0,%1,%2"
  [(set_attr "type" "vecperm")])

(define_insn "altivec_vpkswus"
  [(set (match_operand:V8HI 0 "register_operand" "=v")
        (unspec:V8HI [(match_operand:V4SI 1 "register_operand" "v")
                      (match_operand:V4SI 2 "register_operand" "v")]
		     UNSPEC_VPKSWUS))
   (set (reg:SI 110) (unspec:SI [(const_int 0)] UNSPEC_SET_VSCR))]
  "TARGET_ALTIVEC"
  "vpkswus %0,%1,%2"
  [(set_attr "type" "vecperm")])

(define_insn "*altivec_vrl<VI_char>"
  [(set (match_operand:VI 0 "register_operand" "=v")
        (rotate:VI (match_operand:VI 1 "register_operand" "v")
		   (match_operand:VI 2 "register_operand" "v")))]
  "TARGET_ALTIVEC"
  "vrl<VI_char> %0,%1,%2"
  [(set_attr "type" "vecsimple")])

(define_insn "altivec_vsl"
  [(set (match_operand:V4SI 0 "register_operand" "=v")
        (unspec:V4SI [(match_operand:V4SI 1 "register_operand" "v")
                      (match_operand:V4SI 2 "register_operand" "v")]
		     UNSPEC_VSLV4SI))]
  "TARGET_ALTIVEC"
  "vsl %0,%1,%2"
  [(set_attr "type" "vecperm")])

(define_insn "altivec_vslo"
  [(set (match_operand:V4SI 0 "register_operand" "=v")
        (unspec:V4SI [(match_operand:V4SI 1 "register_operand" "v")
                      (match_operand:V4SI 2 "register_operand" "v")]
		     UNSPEC_VSLO))]
  "TARGET_ALTIVEC"
  "vslo %0,%1,%2"
  [(set_attr "type" "vecperm")])

(define_insn "*altivec_vsl<VI_char>"
  [(set (match_operand:VI 0 "register_operand" "=v")
        (ashift:VI (match_operand:VI 1 "register_operand" "v")
		   (match_operand:VI 2 "register_operand" "v")))]
  "TARGET_ALTIVEC"
  "vsl<VI_char> %0,%1,%2"
  [(set_attr "type" "vecsimple")])

(define_insn "*altivec_vsr<VI_char>"
  [(set (match_operand:VI 0 "register_operand" "=v")
        (lshiftrt:VI (match_operand:VI 1 "register_operand" "v")
		     (match_operand:VI 2 "register_operand" "v")))]
  "TARGET_ALTIVEC"
  "vsr<VI_char> %0,%1,%2"
  [(set_attr "type" "vecsimple")])

(define_insn "*altivec_vsra<VI_char>"
  [(set (match_operand:VI 0 "register_operand" "=v")
        (ashiftrt:VI (match_operand:VI 1 "register_operand" "v")
		     (match_operand:VI 2 "register_operand" "v")))]
  "TARGET_ALTIVEC"
  "vsra<VI_char> %0,%1,%2"
  [(set_attr "type" "vecsimple")])

(define_insn "altivec_vsr"
  [(set (match_operand:V4SI 0 "register_operand" "=v")
        (unspec:V4SI [(match_operand:V4SI 1 "register_operand" "v")
                      (match_operand:V4SI 2 "register_operand" "v")]
		     UNSPEC_VSR))]
  "TARGET_ALTIVEC"
  "vsr %0,%1,%2"
  [(set_attr "type" "vecperm")])

(define_insn "altivec_vsro"
  [(set (match_operand:V4SI 0 "register_operand" "=v")
        (unspec:V4SI [(match_operand:V4SI 1 "register_operand" "v")
                      (match_operand:V4SI 2 "register_operand" "v")]
		     UNSPEC_VSRO))]
  "TARGET_ALTIVEC"
  "vsro %0,%1,%2"
  [(set_attr "type" "vecperm")])

(define_insn "altivec_vsum4ubs"
  [(set (match_operand:V4SI 0 "register_operand" "=v")
        (unspec:V4SI [(match_operand:V16QI 1 "register_operand" "v")
                      (match_operand:V4SI 2 "register_operand" "v")]
		     UNSPEC_VSUM4UBS))
   (set (reg:SI 110) (unspec:SI [(const_int 0)] UNSPEC_SET_VSCR))]
  "TARGET_ALTIVEC"
  "vsum4ubs %0,%1,%2"
  [(set_attr "type" "veccomplex")])

(define_insn "altivec_vsum4s<VI_char>s"
  [(set (match_operand:V4SI 0 "register_operand" "=v")
        (unspec:V4SI [(match_operand:VIshort 1 "register_operand" "v")
                      (match_operand:V4SI 2 "register_operand" "v")]
		     UNSPEC_VSUM4S))
   (set (reg:SI 110) (unspec:SI [(const_int 0)] UNSPEC_SET_VSCR))]
  "TARGET_ALTIVEC"
  "vsum4s<VI_char>s %0,%1,%2"
  [(set_attr "type" "veccomplex")])

(define_insn "altivec_vsum2sws"
  [(set (match_operand:V4SI 0 "register_operand" "=v")
        (unspec:V4SI [(match_operand:V4SI 1 "register_operand" "v")
                      (match_operand:V4SI 2 "register_operand" "v")]
		     UNSPEC_VSUM2SWS))
   (set (reg:SI 110) (unspec:SI [(const_int 0)] UNSPEC_SET_VSCR))]
  "TARGET_ALTIVEC"
  "vsum2sws %0,%1,%2"
  [(set_attr "type" "veccomplex")])

(define_insn "altivec_vsumsws"
  [(set (match_operand:V4SI 0 "register_operand" "=v")
        (unspec:V4SI [(match_operand:V4SI 1 "register_operand" "v")
                      (match_operand:V4SI 2 "register_operand" "v")]
		     UNSPEC_VSUMSWS))
   (set (reg:SI 110) (unspec:SI [(const_int 0)] UNSPEC_SET_VSCR))]
  "TARGET_ALTIVEC"
  "vsumsws %0,%1,%2"
  [(set_attr "type" "veccomplex")])

(define_insn "altivec_vspltb"
  [(set (match_operand:V16QI 0 "register_operand" "=v")
        (vec_duplicate:V16QI
	 (vec_select:QI (match_operand:V16QI 1 "register_operand" "v")
			(parallel
			 [(match_operand:QI 2 "u5bit_cint_operand" "")]))))]
  "TARGET_ALTIVEC"
  "vspltb %0,%1,%2"
  [(set_attr "type" "vecperm")])

(define_insn "altivec_vsplth"
  [(set (match_operand:V8HI 0 "register_operand" "=v")
	(vec_duplicate:V8HI
	 (vec_select:HI (match_operand:V8HI 1 "register_operand" "v")
			(parallel
			 [(match_operand:QI 2 "u5bit_cint_operand" "")]))))]
  "TARGET_ALTIVEC"
  "vsplth %0,%1,%2"
  [(set_attr "type" "vecperm")])

(define_insn "altivec_vspltw"
  [(set (match_operand:V4SI 0 "register_operand" "=v")
	(vec_duplicate:V4SI
	 (vec_select:SI (match_operand:V4SI 1 "register_operand" "v")
			(parallel
			 [(match_operand:QI 2 "u5bit_cint_operand" "i")]))))]
  "TARGET_ALTIVEC"
  "vspltw %0,%1,%2"
  [(set_attr "type" "vecperm")])

(define_insn "altivec_vspltsf"
  [(set (match_operand:V4SF 0 "register_operand" "=v")
	(vec_duplicate:V4SF
	 (vec_select:SF (match_operand:V4SF 1 "register_operand" "v")
			(parallel
			 [(match_operand:QI 2 "u5bit_cint_operand" "i")]))))]
  "VECTOR_UNIT_ALTIVEC_P (V4SFmode)"
  "vspltw %0,%1,%2"
  [(set_attr "type" "vecperm")])

(define_insn "altivec_vspltis<VI_char>"
  [(set (match_operand:VI 0 "register_operand" "=v")
	(vec_duplicate:VI
	 (match_operand:QI 1 "s5bit_cint_operand" "i")))]
  "TARGET_ALTIVEC"
  "vspltis<VI_char> %0,%1"
  [(set_attr "type" "vecperm")])

(define_insn "*altivec_vrfiz"
  [(set (match_operand:V4SF 0 "register_operand" "=v")
  	(fix:V4SF (match_operand:V4SF 1 "register_operand" "v")))]
  "VECTOR_UNIT_ALTIVEC_P (V4SFmode)"
  "vrfiz %0,%1"
  [(set_attr "type" "vecfloat")])

(define_insn "altivec_vperm_<mode>"
  [(set (match_operand:VM 0 "register_operand" "=v")
	(unspec:VM [(match_operand:VM 1 "register_operand" "v")
		    (match_operand:VM 2 "register_operand" "v")
		    (match_operand:V16QI 3 "register_operand" "v")]
		   UNSPEC_VPERM))]
  "TARGET_ALTIVEC"
  "vperm %0,%1,%2,%3"
  [(set_attr "type" "vecperm")])

(define_insn "altivec_vperm_<mode>_uns"
  [(set (match_operand:VM 0 "register_operand" "=v")
	(unspec:VM [(match_operand:VM 1 "register_operand" "v")
		    (match_operand:VM 2 "register_operand" "v")
		    (match_operand:V16QI 3 "register_operand" "v")]
		   UNSPEC_VPERM_UNS))]
  "TARGET_ALTIVEC"
  "vperm %0,%1,%2,%3"
  [(set_attr "type" "vecperm")])

(define_insn "altivec_vrfip"		; ceil
  [(set (match_operand:V4SF 0 "register_operand" "=v")
        (unspec:V4SF [(match_operand:V4SF 1 "register_operand" "v")]
		     UNSPEC_FRIP))]
  "TARGET_ALTIVEC"
  "vrfip %0,%1"
  [(set_attr "type" "vecfloat")])

(define_insn "altivec_vrfin"
  [(set (match_operand:V4SF 0 "register_operand" "=v")
        (unspec:V4SF [(match_operand:V4SF 1 "register_operand" "v")]
		     UNSPEC_VRFIN))]
  "TARGET_ALTIVEC"
  "vrfin %0,%1"
  [(set_attr "type" "vecfloat")])

(define_insn "*altivec_vrfim"		; floor
  [(set (match_operand:V4SF 0 "register_operand" "=v")
        (unspec:V4SF [(match_operand:V4SF 1 "register_operand" "v")]
		     UNSPEC_FRIM))]
  "TARGET_ALTIVEC"
  "vrfim %0,%1"
  [(set_attr "type" "vecfloat")])

(define_insn "altivec_vcfux"
  [(set (match_operand:V4SF 0 "register_operand" "=v")
        (unspec:V4SF [(match_operand:V4SI 1 "register_operand" "v")
	              (match_operand:QI 2 "immediate_operand" "i")]
		     UNSPEC_VCFUX))]
  "TARGET_ALTIVEC"
  "vcfux %0,%1,%2"
  [(set_attr "type" "vecfloat")])

(define_insn "altivec_vcfsx"
  [(set (match_operand:V4SF 0 "register_operand" "=v")
        (unspec:V4SF [(match_operand:V4SI 1 "register_operand" "v")
	              (match_operand:QI 2 "immediate_operand" "i")]
		     UNSPEC_VCFSX))]
  "TARGET_ALTIVEC"
  "vcfsx %0,%1,%2"
  [(set_attr "type" "vecfloat")])

(define_insn "altivec_vctuxs"
  [(set (match_operand:V4SI 0 "register_operand" "=v")
        (unspec:V4SI [(match_operand:V4SF 1 "register_operand" "v")
                      (match_operand:QI 2 "immediate_operand" "i")]
		     UNSPEC_VCTUXS))
   (set (reg:SI 110) (unspec:SI [(const_int 0)] UNSPEC_SET_VSCR))]
  "TARGET_ALTIVEC"
  "vctuxs %0,%1,%2"
  [(set_attr "type" "vecfloat")])

(define_insn "altivec_vctsxs"
  [(set (match_operand:V4SI 0 "register_operand" "=v")
        (unspec:V4SI [(match_operand:V4SF 1 "register_operand" "v")
                      (match_operand:QI 2 "immediate_operand" "i")]
		     UNSPEC_VCTSXS))
   (set (reg:SI 110) (unspec:SI [(const_int 0)] UNSPEC_SET_VSCR))]
  "TARGET_ALTIVEC"
  "vctsxs %0,%1,%2"
  [(set_attr "type" "vecfloat")])

(define_insn "altivec_vlogefp"
  [(set (match_operand:V4SF 0 "register_operand" "=v")
        (unspec:V4SF [(match_operand:V4SF 1 "register_operand" "v")]
		     UNSPEC_VLOGEFP))]
  "TARGET_ALTIVEC"
  "vlogefp %0,%1"
  [(set_attr "type" "vecfloat")])

(define_insn "altivec_vexptefp"
  [(set (match_operand:V4SF 0 "register_operand" "=v")
        (unspec:V4SF [(match_operand:V4SF 1 "register_operand" "v")]
		     UNSPEC_VEXPTEFP))]
  "TARGET_ALTIVEC"
  "vexptefp %0,%1"
  [(set_attr "type" "vecfloat")])

(define_insn "altivec_vrsqrtefp"
  [(set (match_operand:V4SF 0 "register_operand" "=v")
        (unspec:V4SF [(match_operand:V4SF 1 "register_operand" "v")]
		     UNSPEC_VRSQRTEFP))]
  "TARGET_ALTIVEC"
  "vrsqrtefp %0,%1"
  [(set_attr "type" "vecfloat")])

(define_insn "altivec_vrefp"
  [(set (match_operand:V4SF 0 "register_operand" "=v")
        (unspec:V4SF [(match_operand:V4SF 1 "register_operand" "v")]
		     UNSPEC_VREFP))]
  "TARGET_ALTIVEC"
  "vrefp %0,%1"
  [(set_attr "type" "vecfloat")])

(define_expand "altivec_copysign_v4sf3"
  [(use (match_operand:V4SF 0 "register_operand" ""))
   (use (match_operand:V4SF 1 "register_operand" ""))
   (use (match_operand:V4SF 2 "register_operand" ""))]
  "VECTOR_UNIT_ALTIVEC_P (V4SFmode)"
  "
{
  rtx mask = gen_reg_rtx (V4SImode);
  rtvec v = rtvec_alloc (4);
  unsigned HOST_WIDE_INT mask_val = ((unsigned HOST_WIDE_INT)1) << 31;

  RTVEC_ELT (v, 0) = GEN_INT (mask_val);
  RTVEC_ELT (v, 1) = GEN_INT (mask_val);
  RTVEC_ELT (v, 2) = GEN_INT (mask_val);
  RTVEC_ELT (v, 3) = GEN_INT (mask_val);

  emit_insn (gen_vec_initv4si (mask, gen_rtx_PARALLEL (V4SImode, v)));
  emit_insn (gen_vector_select_v4sf (operands[0], operands[1], operands[2],
				     gen_lowpart (V4SFmode, mask)));
  DONE;
}")

(define_insn "altivec_vsldoi_<mode>"
  [(set (match_operand:VM 0 "register_operand" "=v")
        (unspec:VM [(match_operand:VM 1 "register_operand" "v")
		    (match_operand:VM 2 "register_operand" "v")
		    (match_operand:QI 3 "immediate_operand" "i")]
		  UNSPEC_VLSDOI))]
  "TARGET_ALTIVEC"
  "vsldoi %0,%1,%2,%3"
  [(set_attr "type" "vecperm")])

(define_insn "altivec_vupkhsb"
  [(set (match_operand:V8HI 0 "register_operand" "=v")
  	(unspec:V8HI [(match_operand:V16QI 1 "register_operand" "v")]
		     UNSPEC_VUPKHSB))]
  "TARGET_ALTIVEC"
  "vupkhsb %0,%1"
  [(set_attr "type" "vecperm")])

(define_insn "altivec_vupkhpx"
  [(set (match_operand:V4SI 0 "register_operand" "=v")
  	(unspec:V4SI [(match_operand:V8HI 1 "register_operand" "v")]
		     UNSPEC_VUPKHPX))]
  "TARGET_ALTIVEC"
  "vupkhpx %0,%1"
  [(set_attr "type" "vecperm")])

(define_insn "altivec_vupkhsh"
  [(set (match_operand:V4SI 0 "register_operand" "=v")
  	(unspec:V4SI [(match_operand:V8HI 1 "register_operand" "v")]
		     UNSPEC_VUPKHSH))]
  "TARGET_ALTIVEC"
  "vupkhsh %0,%1"
  [(set_attr "type" "vecperm")])

(define_insn "altivec_vupklsb"
  [(set (match_operand:V8HI 0 "register_operand" "=v")
  	(unspec:V8HI [(match_operand:V16QI 1 "register_operand" "v")]
		     UNSPEC_VUPKLSB))]
  "TARGET_ALTIVEC"
  "vupklsb %0,%1"
  [(set_attr "type" "vecperm")])

(define_insn "altivec_vupklpx"
  [(set (match_operand:V4SI 0 "register_operand" "=v")
  	(unspec:V4SI [(match_operand:V8HI 1 "register_operand" "v")]
		     UNSPEC_VUPKLPX))]
  "TARGET_ALTIVEC"
  "vupklpx %0,%1"
  [(set_attr "type" "vecperm")])

(define_insn "altivec_vupklsh"
  [(set (match_operand:V4SI 0 "register_operand" "=v")
  	(unspec:V4SI [(match_operand:V8HI 1 "register_operand" "v")]
		     UNSPEC_VUPKLSH))]
  "TARGET_ALTIVEC"
  "vupklsh %0,%1"
  [(set_attr "type" "vecperm")])

;; Compare vectors producing a vector result and a predicate, setting CR6 to
;; indicate a combined status
(define_insn "*altivec_vcmpequ<VI_char>_p"
  [(set (reg:CC 74)
	(unspec:CC [(eq:CC (match_operand:VI 1 "register_operand" "v")
			   (match_operand:VI 2 "register_operand" "v"))]
		   UNSPEC_PREDICATE))
   (set (match_operand:VI 0 "register_operand" "=v")
	(eq:VI (match_dup 1)
	       (match_dup 2)))]
  "VECTOR_UNIT_ALTIVEC_P (<MODE>mode)"
  "vcmpequ<VI_char>. %0,%1,%2"
  [(set_attr "type" "veccmp")])

(define_insn "*altivec_vcmpgts<VI_char>_p"
  [(set (reg:CC 74)
	(unspec:CC [(gt:CC (match_operand:VI 1 "register_operand" "v")
			   (match_operand:VI 2 "register_operand" "v"))]
		   UNSPEC_PREDICATE))
   (set (match_operand:VI 0 "register_operand" "=v")
	(gt:VI (match_dup 1)
	       (match_dup 2)))]
  "VECTOR_UNIT_ALTIVEC_P (<MODE>mode)"
  "vcmpgts<VI_char>. %0,%1,%2"
  [(set_attr "type" "veccmp")])

(define_insn "*altivec_vcmpgtu<VI_char>_p"
  [(set (reg:CC 74)
	(unspec:CC [(gtu:CC (match_operand:VI 1 "register_operand" "v")
			    (match_operand:VI 2 "register_operand" "v"))]
		   UNSPEC_PREDICATE))
   (set (match_operand:VI 0 "register_operand" "=v")
	(gtu:VI (match_dup 1)
		(match_dup 2)))]
  "VECTOR_UNIT_ALTIVEC_P (<MODE>mode)"
  "vcmpgtu<VI_char>. %0,%1,%2"
  [(set_attr "type" "veccmp")])

(define_insn "*altivec_vcmpeqfp_p"
  [(set (reg:CC 74)
	(unspec:CC [(eq:CC (match_operand:V4SF 1 "register_operand" "v")
			   (match_operand:V4SF 2 "register_operand" "v"))]
		   UNSPEC_PREDICATE))
   (set (match_operand:V4SF 0 "register_operand" "=v")
	(eq:V4SF (match_dup 1)
		 (match_dup 2)))]
  "VECTOR_UNIT_ALTIVEC_P (V4SFmode)"
  "vcmpeqfp. %0,%1,%2"
  [(set_attr "type" "veccmp")])

(define_insn "*altivec_vcmpgtfp_p"
  [(set (reg:CC 74)
	(unspec:CC [(gt:CC (match_operand:V4SF 1 "register_operand" "v")
			   (match_operand:V4SF 2 "register_operand" "v"))]
		   UNSPEC_PREDICATE))
   (set (match_operand:V4SF 0 "register_operand" "=v")
	(gt:V4SF (match_dup 1)
		 (match_dup 2)))]
  "VECTOR_UNIT_ALTIVEC_P (V4SFmode)"
  "vcmpgtfp. %0,%1,%2"
  [(set_attr "type" "veccmp")])

(define_insn "*altivec_vcmpgefp_p"
  [(set (reg:CC 74)
	(unspec:CC [(ge:CC (match_operand:V4SF 1 "register_operand" "v")
			   (match_operand:V4SF 2 "register_operand" "v"))]
		   UNSPEC_PREDICATE))
   (set (match_operand:V4SF 0 "register_operand" "=v")
	(ge:V4SF (match_dup 1)
		 (match_dup 2)))]
  "VECTOR_UNIT_ALTIVEC_P (V4SFmode)"
  "vcmpgefp. %0,%1,%2"
  [(set_attr "type" "veccmp")])

(define_insn "altivec_vcmpbfp_p"
  [(set (reg:CC 74)
	(unspec:CC [(match_operand:V4SF 1 "register_operand" "v")
		    (match_operand:V4SF 2 "register_operand" "v")]
		   UNSPEC_VCMPBFP))
   (set (match_operand:V4SF 0 "register_operand" "=v")
        (unspec:V4SF [(match_dup 1)
                      (match_dup 2)] 
                      UNSPEC_VCMPBFP))]
  "VECTOR_UNIT_ALTIVEC_OR_VSX_P (V4SFmode)"
  "vcmpbfp. %0,%1,%2"
  [(set_attr "type" "veccmp")])

(define_insn "altivec_mtvscr"
  [(set (reg:SI 110)
	(unspec_volatile:SI
	 [(match_operand:V4SI 0 "register_operand" "v")] UNSPECV_MTVSCR))]
  "TARGET_ALTIVEC"
  "mtvscr %0"
  [(set_attr "type" "vecsimple")])

(define_insn "altivec_mfvscr"
  [(set (match_operand:V8HI 0 "register_operand" "=v")
	(unspec_volatile:V8HI [(reg:SI 110)] UNSPECV_MFVSCR))]
  "TARGET_ALTIVEC"
  "mfvscr %0"
  [(set_attr "type" "vecsimple")])

(define_insn "altivec_dssall"
  [(unspec_volatile [(const_int 0)] UNSPECV_DSSALL)]
  "TARGET_ALTIVEC"
  "dssall"
  [(set_attr "type" "vecsimple")])

(define_insn "altivec_dss"
  [(unspec_volatile [(match_operand:QI 0 "immediate_operand" "i")]
		    UNSPECV_DSS)]
  "TARGET_ALTIVEC"
  "dss %0"
  [(set_attr "type" "vecsimple")])

(define_insn "altivec_dst"
  [(unspec [(match_operand 0 "register_operand" "b")
	    (match_operand:SI 1 "register_operand" "r")
	    (match_operand:QI 2 "immediate_operand" "i")] UNSPEC_DST)]
  "TARGET_ALTIVEC && GET_MODE (operands[0]) == Pmode"
  "dst %0,%1,%2"
  [(set_attr "type" "vecsimple")])

(define_insn "altivec_dstt"
  [(unspec [(match_operand 0 "register_operand" "b")
	    (match_operand:SI 1 "register_operand" "r")
	    (match_operand:QI 2 "immediate_operand" "i")] UNSPEC_DSTT)]
  "TARGET_ALTIVEC && GET_MODE (operands[0]) == Pmode"
  "dstt %0,%1,%2"
  [(set_attr "type" "vecsimple")])

(define_insn "altivec_dstst"
  [(unspec [(match_operand 0 "register_operand" "b")
	    (match_operand:SI 1 "register_operand" "r")
	    (match_operand:QI 2 "immediate_operand" "i")] UNSPEC_DSTST)]
  "TARGET_ALTIVEC && GET_MODE (operands[0]) == Pmode"
  "dstst %0,%1,%2"
  [(set_attr "type" "vecsimple")])

(define_insn "altivec_dststt"
  [(unspec [(match_operand 0 "register_operand" "b")
	    (match_operand:SI 1 "register_operand" "r")
	    (match_operand:QI 2 "immediate_operand" "i")] UNSPEC_DSTSTT)]
  "TARGET_ALTIVEC && GET_MODE (operands[0]) == Pmode"
  "dststt %0,%1,%2"
  [(set_attr "type" "vecsimple")])

(define_insn "altivec_lvsl"
  [(set (match_operand:V16QI 0 "register_operand" "=v")
	(unspec:V16QI [(match_operand 1 "memory_operand" "Z")] UNSPEC_LVSL))]
  "TARGET_ALTIVEC"
  "lvsl %0,%y1"
  [(set_attr "type" "vecload")])

(define_insn "altivec_lvsr"
  [(set (match_operand:V16QI 0 "register_operand" "=v")
	(unspec:V16QI [(match_operand 1 "memory_operand" "Z")] UNSPEC_LVSR))]
  "TARGET_ALTIVEC"
  "lvsr %0,%y1"
  [(set_attr "type" "vecload")])

(define_expand "build_vector_mask_for_load"
  [(set (match_operand:V16QI 0 "register_operand" "")
	(unspec:V16QI [(match_operand 1 "memory_operand" "")] UNSPEC_LVSR))]
  "TARGET_ALTIVEC"
  "
{ 
  rtx addr;
  rtx temp;

  gcc_assert (GET_CODE (operands[1]) == MEM);

  addr = XEXP (operands[1], 0);
  temp = gen_reg_rtx (GET_MODE (addr));
  emit_insn (gen_rtx_SET (VOIDmode, temp, 
			  gen_rtx_NEG (GET_MODE (addr), addr)));
  emit_insn (gen_altivec_lvsr (operands[0], 
			       replace_equiv_address (operands[1], temp)));
  DONE;
}")

;; Parallel some of the LVE* and STV*'s with unspecs because some have
;; identical rtl but different instructions-- and gcc gets confused.

(define_insn "altivec_lve<VI_char>x"
  [(parallel
    [(set (match_operand:VI 0 "register_operand" "=v")
	  (match_operand:VI 1 "memory_operand" "Z"))
     (unspec [(const_int 0)] UNSPEC_LVE)])]
  "TARGET_ALTIVEC"
  "lve<VI_char>x %0,%y1"
  [(set_attr "type" "vecload")])

(define_insn "*altivec_lvesfx"
  [(parallel
    [(set (match_operand:V4SF 0 "register_operand" "=v")
	  (match_operand:V4SF 1 "memory_operand" "Z"))
     (unspec [(const_int 0)] UNSPEC_LVE)])]
  "TARGET_ALTIVEC"
  "lvewx %0,%y1"
  [(set_attr "type" "vecload")])

(define_insn "altivec_lvxl"
  [(parallel
    [(set (match_operand:V4SI 0 "register_operand" "=v")
	  (match_operand:V4SI 1 "memory_operand" "Z"))
     (unspec [(const_int 0)] UNSPEC_SET_VSCR)])]
  "TARGET_ALTIVEC"
  "lvxl %0,%y1"
  [(set_attr "type" "vecload")])

(define_insn "altivec_lvx"
  [(set (match_operand:V4SI 0 "register_operand" "=v")
	(match_operand:V4SI 1 "memory_operand" "Z"))]
  "TARGET_ALTIVEC"
  "lvx %0,%y1"
  [(set_attr "type" "vecload")])

(define_insn "altivec_stvx"
  [(parallel
    [(set (match_operand:V4SI 0 "memory_operand" "=Z")
	  (match_operand:V4SI 1 "register_operand" "v"))
     (unspec [(const_int 0)] UNSPEC_STVX)])]
  "TARGET_ALTIVEC"
  "stvx %1,%y0"
  [(set_attr "type" "vecstore")])

(define_insn "altivec_stvxl"
  [(parallel
    [(set (match_operand:V4SI 0 "memory_operand" "=Z")
	  (match_operand:V4SI 1 "register_operand" "v"))
     (unspec [(const_int 0)] UNSPEC_STVXL)])]
  "TARGET_ALTIVEC"
  "stvxl %1,%y0"
  [(set_attr "type" "vecstore")])

(define_insn "altivec_stve<VI_char>x"
  [(parallel
    [(set (match_operand:VI 0 "memory_operand" "=Z")
	  (match_operand:VI 1 "register_operand" "v"))
     (unspec [(const_int 0)] UNSPEC_STVE)])]
  "TARGET_ALTIVEC"
  "stve<VI_char>x %1,%y0"
  [(set_attr "type" "vecstore")])

(define_insn "*altivec_stvesfx"
  [(parallel
    [(set (match_operand:V4SF 0 "memory_operand" "=Z")
	  (match_operand:V4SF 1 "register_operand" "v"))
     (unspec [(const_int 0)] UNSPEC_STVE)])]
  "TARGET_ALTIVEC"
  "stvewx %1,%y0"
  [(set_attr "type" "vecstore")])

;; Generate
;;    vspltis? SCRATCH0,0
;;    vsubu?m SCRATCH2,SCRATCH1,%1
;;    vmaxs? %0,%1,SCRATCH2"
(define_expand "abs<mode>2"
  [(set (match_dup 2) (vec_duplicate:VI (const_int 0)))
   (set (match_dup 3)
        (minus:VI (match_dup 2)
                  (match_operand:VI 1 "register_operand" "v")))
   (set (match_operand:VI 0 "register_operand" "=v")
        (smax:VI (match_dup 1) (match_dup 3)))]
  "TARGET_ALTIVEC"
{
  operands[2] = gen_reg_rtx (GET_MODE (operands[0]));
  operands[3] = gen_reg_rtx (GET_MODE (operands[0]));
})

;; Generate
;;    vspltisw SCRATCH1,-1
;;    vslw SCRATCH2,SCRATCH1,SCRATCH1
;;    vandc %0,%1,SCRATCH2
(define_expand "altivec_absv4sf2"
  [(set (match_dup 2)
	(vec_duplicate:V4SI (const_int -1)))
   (set (match_dup 3)
        (ashift:V4SI (match_dup 2) (match_dup 2)))
   (set (match_operand:V4SF 0 "register_operand" "=v")
        (and:V4SF (not:V4SF (subreg:V4SF (match_dup 3) 0))
                  (match_operand:V4SF 1 "register_operand" "v")))]
  "TARGET_ALTIVEC"
{
  operands[2] = gen_reg_rtx (V4SImode);
  operands[3] = gen_reg_rtx (V4SImode);
})

;; Generate
;;    vspltis? SCRATCH0,0
;;    vsubs?s SCRATCH2,SCRATCH1,%1
;;    vmaxs? %0,%1,SCRATCH2"
(define_expand "altivec_abss_<mode>"
  [(set (match_dup 2) (vec_duplicate:VI (const_int 0)))
   (parallel [(set (match_dup 3)
		   (unspec:VI [(match_dup 2)
			       (match_operand:VI 1 "register_operand" "v")]
			      UNSPEC_VSUBS))
              (set (reg:SI 110) (unspec:SI [(const_int 0)] UNSPEC_SET_VSCR))])
   (set (match_operand:VI 0 "register_operand" "=v")
        (smax:VI (match_dup 1) (match_dup 3)))]
  "TARGET_ALTIVEC"
{
  operands[2] = gen_reg_rtx (GET_MODE (operands[0]));
  operands[3] = gen_reg_rtx (GET_MODE (operands[0]));
})

(define_insn "altivec_vsumsws_nomode"
  [(set (match_operand 0 "register_operand" "=v")
        (unspec:V4SI [(match_operand:V4SI 1 "register_operand" "v")
                      (match_operand:V4SI 2 "register_operand" "v")]
		     UNSPEC_VSUMSWS))
   (set (reg:SI 110) (unspec:SI [(const_int 0)] UNSPEC_SET_VSCR))]
  "TARGET_ALTIVEC"
  "vsumsws %0,%1,%2"
  [(set_attr "type" "veccomplex")])

(define_expand "reduc_splus_<mode>"
  [(set (match_operand:VIshort 0 "register_operand" "=v")
        (unspec:VIshort [(match_operand:VIshort 1 "register_operand" "v")]
			UNSPEC_REDUC_PLUS))]
  "TARGET_ALTIVEC"
  "
{ 
  rtx vzero = gen_reg_rtx (V4SImode);
  rtx vtmp1 = gen_reg_rtx (V4SImode);

  emit_insn (gen_altivec_vspltisw (vzero, const0_rtx));
  emit_insn (gen_altivec_vsum4s<VI_char>s (vtmp1, operands[1], vzero));
  emit_insn (gen_altivec_vsumsws_nomode (operands[0], vtmp1, vzero));
  DONE;
}")

(define_expand "reduc_uplus_v16qi"
  [(set (match_operand:V16QI 0 "register_operand" "=v")
        (unspec:V16QI [(match_operand:V16QI 1 "register_operand" "v")]
		      UNSPEC_REDUC_PLUS))]
  "TARGET_ALTIVEC"
  "
{
  rtx vzero = gen_reg_rtx (V4SImode);
  rtx vtmp1 = gen_reg_rtx (V4SImode);

  emit_insn (gen_altivec_vspltisw (vzero, const0_rtx));
  emit_insn (gen_altivec_vsum4ubs (vtmp1, operands[1], vzero));
  emit_insn (gen_altivec_vsumsws_nomode (operands[0], vtmp1, vzero));
  DONE;
}")

(define_expand "neg<mode>2"
  [(use (match_operand:VI 0 "register_operand" ""))
   (use (match_operand:VI 1 "register_operand" ""))]
  "TARGET_ALTIVEC"
  "
{
  rtx vzero;

  vzero = gen_reg_rtx (GET_MODE (operands[0]));
  emit_insn (gen_altivec_vspltis<VI_char> (vzero, const0_rtx));
  emit_insn (gen_sub<mode>3 (operands[0], vzero, operands[1])); 
  
  DONE;
}")

(define_expand "udot_prod<mode>"
  [(set (match_operand:V4SI 0 "register_operand" "=v")
        (plus:V4SI (match_operand:V4SI 3 "register_operand" "v")
                   (unspec:V4SI [(match_operand:VIshort 1 "register_operand" "v")  
                                 (match_operand:VIshort 2 "register_operand" "v")] 
                                UNSPEC_VMSUMU)))]
  "TARGET_ALTIVEC"
  "
{  
  emit_insn (gen_altivec_vmsumu<VI_char>m (operands[0], operands[1], operands[2], operands[3]));
  DONE;
}")
   
(define_expand "sdot_prodv8hi"
  [(set (match_operand:V4SI 0 "register_operand" "=v")
        (plus:V4SI (match_operand:V4SI 3 "register_operand" "v")
                   (unspec:V4SI [(match_operand:V8HI 1 "register_operand" "v")
                                 (match_operand:V8HI 2 "register_operand" "v")]
                                UNSPEC_VMSUMSHM)))]
  "TARGET_ALTIVEC"
  "
{
  emit_insn (gen_altivec_vmsumshm (operands[0], operands[1], operands[2], operands[3]));
  DONE;
}")

(define_expand "widen_usum<mode>3"
  [(set (match_operand:V4SI 0 "register_operand" "=v")
        (plus:V4SI (match_operand:V4SI 2 "register_operand" "v")
                   (unspec:V4SI [(match_operand:VIshort 1 "register_operand" "v")]
                                UNSPEC_VMSUMU)))]
  "TARGET_ALTIVEC"
  "
{
  rtx vones = gen_reg_rtx (GET_MODE (operands[1]));

  emit_insn (gen_altivec_vspltis<VI_char> (vones, const1_rtx));
  emit_insn (gen_altivec_vmsumu<VI_char>m (operands[0], operands[1], vones, operands[2]));
  DONE;
}")

(define_expand "widen_ssumv16qi3"
  [(set (match_operand:V4SI 0 "register_operand" "=v")
        (plus:V4SI (match_operand:V4SI 2 "register_operand" "v")
                   (unspec:V4SI [(match_operand:V16QI 1 "register_operand" "v")]
                                UNSPEC_VMSUMM)))]
  "TARGET_ALTIVEC"
  "
{
  rtx vones = gen_reg_rtx (V16QImode);

  emit_insn (gen_altivec_vspltisb (vones, const1_rtx));
  emit_insn (gen_altivec_vmsummbm (operands[0], operands[1], vones, operands[2]));
  DONE;
}")

(define_expand "widen_ssumv8hi3"
  [(set (match_operand:V4SI 0 "register_operand" "=v")
        (plus:V4SI (match_operand:V4SI 2 "register_operand" "v")
                   (unspec:V4SI [(match_operand:V8HI 1 "register_operand" "v")]
                                UNSPEC_VMSUMSHM)))]
  "TARGET_ALTIVEC"
  "
{
  rtx vones = gen_reg_rtx (V8HImode);

  emit_insn (gen_altivec_vspltish (vones, const1_rtx));
  emit_insn (gen_altivec_vmsumshm (operands[0], operands[1], vones, operands[2]));
  DONE;
}")

(define_expand "vec_unpacks_hi_v16qi"
  [(set (match_operand:V8HI 0 "register_operand" "=v")
        (unspec:V8HI [(match_operand:V16QI 1 "register_operand" "v")]
                     UNSPEC_VUPKHSB))]
  "TARGET_ALTIVEC"
  "
{
  emit_insn (gen_altivec_vupkhsb (operands[0], operands[1]));
  DONE;
}")

(define_expand "vec_unpacks_hi_v8hi"
  [(set (match_operand:V4SI 0 "register_operand" "=v")
        (unspec:V4SI [(match_operand:V8HI 1 "register_operand" "v")]
                     UNSPEC_VUPKHSH))]
  "TARGET_ALTIVEC"
  "
{
  emit_insn (gen_altivec_vupkhsh (operands[0], operands[1]));
  DONE;
}")

(define_expand "vec_unpacks_lo_v16qi"
  [(set (match_operand:V8HI 0 "register_operand" "=v")
        (unspec:V8HI [(match_operand:V16QI 1 "register_operand" "v")]
                     UNSPEC_VUPKLSB))]
  "TARGET_ALTIVEC"
  "
{
  emit_insn (gen_altivec_vupklsb (operands[0], operands[1]));
  DONE;
}")

(define_expand "vec_unpacks_lo_v8hi"
  [(set (match_operand:V4SI 0 "register_operand" "=v")
        (unspec:V4SI [(match_operand:V8HI 1 "register_operand" "v")]
                     UNSPEC_VUPKLSH))]
  "TARGET_ALTIVEC"
  "
{
  emit_insn (gen_altivec_vupklsh (operands[0], operands[1]));
  DONE;
}")

(define_insn "vperm_v8hiv4si"
  [(set (match_operand:V4SI 0 "register_operand" "=v")
        (unspec:V4SI [(match_operand:V8HI 1 "register_operand" "v")
                   (match_operand:V4SI 2 "register_operand" "v")
                   (match_operand:V16QI 3 "register_operand" "v")]
                  UNSPEC_VPERMSI))]
  "TARGET_ALTIVEC"
  "vperm %0,%1,%2,%3"
  [(set_attr "type" "vecperm")])

(define_insn "vperm_v16qiv8hi"
  [(set (match_operand:V8HI 0 "register_operand" "=v")
        (unspec:V8HI [(match_operand:V16QI 1 "register_operand" "v")
                   (match_operand:V8HI 2 "register_operand" "v")
                   (match_operand:V16QI 3 "register_operand" "v")]
                  UNSPEC_VPERMHI))]
  "TARGET_ALTIVEC"
  "vperm %0,%1,%2,%3"
  [(set_attr "type" "vecperm")])


(define_expand "vec_unpacku_hi_v16qi"
  [(set (match_operand:V8HI 0 "register_operand" "=v")
        (unspec:V8HI [(match_operand:V16QI 1 "register_operand" "v")]
                     UNSPEC_VUPKHUB))]
  "TARGET_ALTIVEC"      
  "
{  
  rtx vzero = gen_reg_rtx (V8HImode);
  rtx mask = gen_reg_rtx (V16QImode);
  rtvec v = rtvec_alloc (16);
   
  emit_insn (gen_altivec_vspltish (vzero, const0_rtx));
   
  RTVEC_ELT (v, 0) = gen_rtx_CONST_INT (QImode, 16);
  RTVEC_ELT (v, 1) = gen_rtx_CONST_INT (QImode, 0);
  RTVEC_ELT (v, 2) = gen_rtx_CONST_INT (QImode, 16);
  RTVEC_ELT (v, 3) = gen_rtx_CONST_INT (QImode, 1);
  RTVEC_ELT (v, 4) = gen_rtx_CONST_INT (QImode, 16);
  RTVEC_ELT (v, 5) = gen_rtx_CONST_INT (QImode, 2);
  RTVEC_ELT (v, 6) = gen_rtx_CONST_INT (QImode, 16);
  RTVEC_ELT (v, 7) = gen_rtx_CONST_INT (QImode, 3);
  RTVEC_ELT (v, 8) = gen_rtx_CONST_INT (QImode, 16);
  RTVEC_ELT (v, 9) = gen_rtx_CONST_INT (QImode, 4);
  RTVEC_ELT (v, 10) = gen_rtx_CONST_INT (QImode, 16);
  RTVEC_ELT (v, 11) = gen_rtx_CONST_INT (QImode, 5);
  RTVEC_ELT (v, 12) = gen_rtx_CONST_INT (QImode, 16);
  RTVEC_ELT (v, 13) = gen_rtx_CONST_INT (QImode, 6);
  RTVEC_ELT (v, 14) = gen_rtx_CONST_INT (QImode, 16);
  RTVEC_ELT (v, 15) = gen_rtx_CONST_INT (QImode, 7);

  emit_insn (gen_vec_initv16qi (mask, gen_rtx_PARALLEL (V16QImode, v)));
  emit_insn (gen_vperm_v16qiv8hi (operands[0], operands[1], vzero, mask));
  DONE;
}")

(define_expand "vec_unpacku_hi_v8hi"
  [(set (match_operand:V4SI 0 "register_operand" "=v")
        (unspec:V4SI [(match_operand:V8HI 1 "register_operand" "v")]
                     UNSPEC_VUPKHUH))]
  "TARGET_ALTIVEC"
  "
{
  rtx vzero = gen_reg_rtx (V4SImode);
  rtx mask = gen_reg_rtx (V16QImode);
  rtvec v = rtvec_alloc (16);

  emit_insn (gen_altivec_vspltisw (vzero, const0_rtx));
 
  RTVEC_ELT (v, 0) = gen_rtx_CONST_INT (QImode, 16);
  RTVEC_ELT (v, 1) = gen_rtx_CONST_INT (QImode, 17);
  RTVEC_ELT (v, 2) = gen_rtx_CONST_INT (QImode, 0);
  RTVEC_ELT (v, 3) = gen_rtx_CONST_INT (QImode, 1);
  RTVEC_ELT (v, 4) = gen_rtx_CONST_INT (QImode, 16);
  RTVEC_ELT (v, 5) = gen_rtx_CONST_INT (QImode, 17);
  RTVEC_ELT (v, 6) = gen_rtx_CONST_INT (QImode, 2);
  RTVEC_ELT (v, 7) = gen_rtx_CONST_INT (QImode, 3);
  RTVEC_ELT (v, 8) = gen_rtx_CONST_INT (QImode, 16);
  RTVEC_ELT (v, 9) = gen_rtx_CONST_INT (QImode, 17);
  RTVEC_ELT (v, 10) = gen_rtx_CONST_INT (QImode, 4);
  RTVEC_ELT (v, 11) = gen_rtx_CONST_INT (QImode, 5);
  RTVEC_ELT (v, 12) = gen_rtx_CONST_INT (QImode, 16);
  RTVEC_ELT (v, 13) = gen_rtx_CONST_INT (QImode, 17);
  RTVEC_ELT (v, 14) = gen_rtx_CONST_INT (QImode, 6);
  RTVEC_ELT (v, 15) = gen_rtx_CONST_INT (QImode, 7);

  emit_insn (gen_vec_initv16qi (mask, gen_rtx_PARALLEL (V16QImode, v)));
  emit_insn (gen_vperm_v8hiv4si (operands[0], operands[1], vzero, mask));
  DONE;
}")

(define_expand "vec_unpacku_lo_v16qi"
  [(set (match_operand:V8HI 0 "register_operand" "=v")
        (unspec:V8HI [(match_operand:V16QI 1 "register_operand" "v")]
                     UNSPEC_VUPKLUB))]
  "TARGET_ALTIVEC"
  "
{
  rtx vzero = gen_reg_rtx (V8HImode);
  rtx mask = gen_reg_rtx (V16QImode);
  rtvec v = rtvec_alloc (16);

  emit_insn (gen_altivec_vspltish (vzero, const0_rtx));

  RTVEC_ELT (v, 0) = gen_rtx_CONST_INT (QImode, 16);
  RTVEC_ELT (v, 1) = gen_rtx_CONST_INT (QImode, 8);
  RTVEC_ELT (v, 2) = gen_rtx_CONST_INT (QImode, 16);
  RTVEC_ELT (v, 3) = gen_rtx_CONST_INT (QImode, 9);
  RTVEC_ELT (v, 4) = gen_rtx_CONST_INT (QImode, 16);
  RTVEC_ELT (v, 5) = gen_rtx_CONST_INT (QImode, 10);
  RTVEC_ELT (v, 6) = gen_rtx_CONST_INT (QImode, 16);
  RTVEC_ELT (v, 7) = gen_rtx_CONST_INT (QImode, 11);
  RTVEC_ELT (v, 8) = gen_rtx_CONST_INT (QImode, 16);
  RTVEC_ELT (v, 9) = gen_rtx_CONST_INT (QImode, 12);
  RTVEC_ELT (v, 10) = gen_rtx_CONST_INT (QImode, 16);
  RTVEC_ELT (v, 11) = gen_rtx_CONST_INT (QImode, 13);
  RTVEC_ELT (v, 12) = gen_rtx_CONST_INT (QImode, 16);
  RTVEC_ELT (v, 13) = gen_rtx_CONST_INT (QImode, 14);
  RTVEC_ELT (v, 14) = gen_rtx_CONST_INT (QImode, 16);
  RTVEC_ELT (v, 15) = gen_rtx_CONST_INT (QImode, 15);

  emit_insn (gen_vec_initv16qi (mask, gen_rtx_PARALLEL (V16QImode, v)));
  emit_insn (gen_vperm_v16qiv8hi (operands[0], operands[1], vzero, mask));
  DONE;
}")

(define_expand "vec_unpacku_lo_v8hi"
  [(set (match_operand:V4SI 0 "register_operand" "=v")
        (unspec:V4SI [(match_operand:V8HI 1 "register_operand" "v")]
                     UNSPEC_VUPKLUH))]
  "TARGET_ALTIVEC"
  "
{
  rtx vzero = gen_reg_rtx (V4SImode);
  rtx mask = gen_reg_rtx (V16QImode);
  rtvec v = rtvec_alloc (16);

  emit_insn (gen_altivec_vspltisw (vzero, const0_rtx));
 
  RTVEC_ELT (v, 0) = gen_rtx_CONST_INT (QImode, 16);
  RTVEC_ELT (v, 1) = gen_rtx_CONST_INT (QImode, 17);
  RTVEC_ELT (v, 2) = gen_rtx_CONST_INT (QImode, 8);
  RTVEC_ELT (v, 3) = gen_rtx_CONST_INT (QImode, 9);
  RTVEC_ELT (v, 4) = gen_rtx_CONST_INT (QImode, 16);
  RTVEC_ELT (v, 5) = gen_rtx_CONST_INT (QImode, 17);
  RTVEC_ELT (v, 6) = gen_rtx_CONST_INT (QImode, 10);
  RTVEC_ELT (v, 7) = gen_rtx_CONST_INT (QImode, 11);
  RTVEC_ELT (v, 8) = gen_rtx_CONST_INT (QImode, 16);
  RTVEC_ELT (v, 9) = gen_rtx_CONST_INT (QImode, 17);
  RTVEC_ELT (v, 10) = gen_rtx_CONST_INT (QImode, 12);
  RTVEC_ELT (v, 11) = gen_rtx_CONST_INT (QImode, 13);
  RTVEC_ELT (v, 12) = gen_rtx_CONST_INT (QImode, 16);
  RTVEC_ELT (v, 13) = gen_rtx_CONST_INT (QImode, 17);
  RTVEC_ELT (v, 14) = gen_rtx_CONST_INT (QImode, 14);
  RTVEC_ELT (v, 15) = gen_rtx_CONST_INT (QImode, 15);

  emit_insn (gen_vec_initv16qi (mask, gen_rtx_PARALLEL (V16QImode, v)));
  emit_insn (gen_vperm_v8hiv4si (operands[0], operands[1], vzero, mask));
  DONE;
}")

(define_expand "vec_widen_umult_hi_v16qi"
  [(set (match_operand:V8HI 0 "register_operand" "=v")
        (unspec:V8HI [(match_operand:V16QI 1 "register_operand" "v")
                      (match_operand:V16QI 2 "register_operand" "v")]
                     UNSPEC_VMULWHUB))]
  "TARGET_ALTIVEC"
  "
{
  rtx ve = gen_reg_rtx (V8HImode);
  rtx vo = gen_reg_rtx (V8HImode);
  
  emit_insn (gen_altivec_vmuleub (ve, operands[1], operands[2]));
  emit_insn (gen_altivec_vmuloub (vo, operands[1], operands[2]));
  emit_insn (gen_altivec_vmrghh (operands[0], ve, vo));
  DONE;
}")

(define_expand "vec_widen_umult_lo_v16qi"
  [(set (match_operand:V8HI 0 "register_operand" "=v")
        (unspec:V8HI [(match_operand:V16QI 1 "register_operand" "v")
                      (match_operand:V16QI 2 "register_operand" "v")]
                     UNSPEC_VMULWLUB))]
  "TARGET_ALTIVEC"
  "
{
  rtx ve = gen_reg_rtx (V8HImode);
  rtx vo = gen_reg_rtx (V8HImode);
  
  emit_insn (gen_altivec_vmuleub (ve, operands[1], operands[2]));
  emit_insn (gen_altivec_vmuloub (vo, operands[1], operands[2]));
  emit_insn (gen_altivec_vmrglh (operands[0], ve, vo));
  DONE;
}")

(define_expand "vec_widen_smult_hi_v16qi"
  [(set (match_operand:V8HI 0 "register_operand" "=v")
        (unspec:V8HI [(match_operand:V16QI 1 "register_operand" "v")
                      (match_operand:V16QI 2 "register_operand" "v")]
                     UNSPEC_VMULWHSB))]
  "TARGET_ALTIVEC"
  "
{
  rtx ve = gen_reg_rtx (V8HImode);
  rtx vo = gen_reg_rtx (V8HImode);
  
  emit_insn (gen_altivec_vmulesb (ve, operands[1], operands[2]));
  emit_insn (gen_altivec_vmulosb (vo, operands[1], operands[2]));
  emit_insn (gen_altivec_vmrghh (operands[0], ve, vo));
  DONE;
}")

(define_expand "vec_widen_smult_lo_v16qi"
  [(set (match_operand:V8HI 0 "register_operand" "=v")
        (unspec:V8HI [(match_operand:V16QI 1 "register_operand" "v")
                      (match_operand:V16QI 2 "register_operand" "v")]
                     UNSPEC_VMULWLSB))]
  "TARGET_ALTIVEC"
  "
{
  rtx ve = gen_reg_rtx (V8HImode);
  rtx vo = gen_reg_rtx (V8HImode);
  
  emit_insn (gen_altivec_vmulesb (ve, operands[1], operands[2]));
  emit_insn (gen_altivec_vmulosb (vo, operands[1], operands[2]));
  emit_insn (gen_altivec_vmrglh (operands[0], ve, vo));
  DONE;
}")

(define_expand "vec_widen_umult_hi_v8hi"
  [(set (match_operand:V4SI 0 "register_operand" "=v")
        (unspec:V4SI [(match_operand:V8HI 1 "register_operand" "v")
                      (match_operand:V8HI 2 "register_operand" "v")]
                     UNSPEC_VMULWHUH))]
  "TARGET_ALTIVEC"
  "
{ 
  rtx ve = gen_reg_rtx (V4SImode);
  rtx vo = gen_reg_rtx (V4SImode);
  
  emit_insn (gen_altivec_vmuleuh (ve, operands[1], operands[2]));
  emit_insn (gen_altivec_vmulouh (vo, operands[1], operands[2]));
  emit_insn (gen_altivec_vmrghw (operands[0], ve, vo));
  DONE;
}")

(define_expand "vec_widen_umult_lo_v8hi"
  [(set (match_operand:V4SI 0 "register_operand" "=v")
        (unspec:V4SI [(match_operand:V8HI 1 "register_operand" "v")
                      (match_operand:V8HI 2 "register_operand" "v")]
                     UNSPEC_VMULWLUH))]
  "TARGET_ALTIVEC"
  "
{ 
  rtx ve = gen_reg_rtx (V4SImode);
  rtx vo = gen_reg_rtx (V4SImode);
  
  emit_insn (gen_altivec_vmuleuh (ve, operands[1], operands[2]));
  emit_insn (gen_altivec_vmulouh (vo, operands[1], operands[2]));
  emit_insn (gen_altivec_vmrglw (operands[0], ve, vo));
  DONE;
}")

(define_expand "vec_widen_smult_hi_v8hi"
  [(set (match_operand:V4SI 0 "register_operand" "=v")
        (unspec:V4SI [(match_operand:V8HI 1 "register_operand" "v")
                      (match_operand:V8HI 2 "register_operand" "v")]
                     UNSPEC_VMULWHSH))]
  "TARGET_ALTIVEC"
  "
{ 
  rtx ve = gen_reg_rtx (V4SImode);
  rtx vo = gen_reg_rtx (V4SImode);
  
  emit_insn (gen_altivec_vmulesh (ve, operands[1], operands[2]));
  emit_insn (gen_altivec_vmulosh (vo, operands[1], operands[2]));
  emit_insn (gen_altivec_vmrghw (operands[0], ve, vo));
  DONE;
}")

(define_expand "vec_widen_smult_lo_v8hi"
  [(set (match_operand:V4SI 0 "register_operand" "=v")
        (unspec:V4SI [(match_operand:V8HI 1 "register_operand" "v")
                      (match_operand:V8HI 2 "register_operand" "v")]
                     UNSPEC_VMULWLSH))]
  "TARGET_ALTIVEC"
  "
{ 
  rtx ve = gen_reg_rtx (V4SImode);
  rtx vo = gen_reg_rtx (V4SImode);
  
  emit_insn (gen_altivec_vmulesh (ve, operands[1], operands[2]));
  emit_insn (gen_altivec_vmulosh (vo, operands[1], operands[2]));
  emit_insn (gen_altivec_vmrglw (operands[0], ve, vo));
  DONE;
}")

(define_expand "vec_pack_trunc_v8hi"
  [(set (match_operand:V16QI 0 "register_operand" "=v")
        (unspec:V16QI [(match_operand:V8HI 1 "register_operand" "v")
                       (match_operand:V8HI 2 "register_operand" "v")]
                      UNSPEC_VPKUHUM))]
  "TARGET_ALTIVEC"
  "
{
  emit_insn (gen_altivec_vpkuhum (operands[0], operands[1], operands[2]));
  DONE;
}")
                                                                                
(define_expand "vec_pack_trunc_v4si"
  [(set (match_operand:V8HI 0 "register_operand" "=v")
        (unspec:V8HI [(match_operand:V4SI 1 "register_operand" "v")
                      (match_operand:V4SI 2 "register_operand" "v")]
                     UNSPEC_VPKUWUM))]
  "TARGET_ALTIVEC"
  "
{
  emit_insn (gen_altivec_vpkuwum (operands[0], operands[1], operands[2]));
  DONE;
}")

(define_expand "altivec_negv4sf2"
  [(use (match_operand:V4SF 0 "register_operand" ""))
   (use (match_operand:V4SF 1 "register_operand" ""))]
  "TARGET_ALTIVEC"
  "
{
  rtx neg0;

  /* Generate [-0.0, -0.0, -0.0, -0.0].  */
  neg0 = gen_reg_rtx (V4SImode);
  emit_insn (gen_altivec_vspltisw (neg0, constm1_rtx));
  emit_insn (gen_vashlv4si3 (neg0, neg0, neg0));

  /* XOR */
  emit_insn (gen_xorv4sf3 (operands[0],
			   gen_lowpart (V4SFmode, neg0), operands[1])); 
    
  DONE;
}")

;; Vector SIMD PEM v2.06c defines LVLX, LVLXL, LVRX, LVRXL,
;; STVLX, STVLXL, STVVRX, STVRXL are available only on Cell.
(define_insn "altivec_lvlx"
  [(set (match_operand:V16QI 0 "register_operand" "=v")
        (unspec:V16QI [(match_operand 1 "memory_operand" "Z")] 
		      UNSPEC_LVLX))]
  "TARGET_ALTIVEC && rs6000_cpu == PROCESSOR_CELL"
  "lvlx %0,%y1"
  [(set_attr "type" "vecload")])

(define_insn "altivec_lvlxl"
  [(set (match_operand:V16QI 0 "register_operand" "=v")
        (unspec:V16QI [(match_operand 1 "memory_operand" "Z")] 
		      UNSPEC_LVLXL))]
  "TARGET_ALTIVEC && rs6000_cpu == PROCESSOR_CELL"
  "lvlxl %0,%y1"
  [(set_attr "type" "vecload")])

(define_insn "altivec_lvrx"
  [(set (match_operand:V16QI 0 "register_operand" "=v")
        (unspec:V16QI [(match_operand 1 "memory_operand" "Z")] 
		      UNSPEC_LVRX))]
  "TARGET_ALTIVEC && rs6000_cpu == PROCESSOR_CELL"
  "lvrx %0,%y1"
  [(set_attr "type" "vecload")])

(define_insn "altivec_lvrxl"
  [(set (match_operand:V16QI 0 "register_operand" "=v")
        (unspec:V16QI [(match_operand 1 "memory_operand" "Z")] 
		      UNSPEC_LVRXL))]
  "TARGET_ALTIVEC && rs6000_cpu == PROCESSOR_CELL"
  "lvrxl %0,%y1"
  [(set_attr "type" "vecload")])

(define_insn "altivec_stvlx"
  [(parallel
    [(set (match_operand:V4SI 0 "memory_operand" "=Z")
	  (match_operand:V4SI 1 "register_operand" "v"))
     (unspec [(const_int 0)] UNSPEC_STVLX)])]
  "TARGET_ALTIVEC && rs6000_cpu == PROCESSOR_CELL"
  "stvlx %1,%y0"
  [(set_attr "type" "vecstore")])

(define_insn "altivec_stvlxl"
  [(parallel
    [(set (match_operand:V4SI 0 "memory_operand" "=Z")
	  (match_operand:V4SI 1 "register_operand" "v"))
     (unspec [(const_int 0)] UNSPEC_STVLXL)])]
  "TARGET_ALTIVEC && rs6000_cpu == PROCESSOR_CELL"
  "stvlxl %1,%y0"
  [(set_attr "type" "vecstore")])

(define_insn "altivec_stvrx"
  [(parallel
    [(set (match_operand:V4SI 0 "memory_operand" "=Z")
	  (match_operand:V4SI 1 "register_operand" "v"))
     (unspec [(const_int 0)] UNSPEC_STVRX)])]
  "TARGET_ALTIVEC && rs6000_cpu == PROCESSOR_CELL"
  "stvrx %1,%y0"
  [(set_attr "type" "vecstore")])

(define_insn "altivec_stvrxl"
  [(parallel
    [(set (match_operand:V4SI 0 "memory_operand" "=Z")
	  (match_operand:V4SI 1 "register_operand" "v"))
     (unspec [(const_int 0)] UNSPEC_STVRXL)])]
  "TARGET_ALTIVEC && rs6000_cpu == PROCESSOR_CELL"
  "stvrxl %1,%y0"
  [(set_attr "type" "vecstore")])

(define_expand "vec_extract_evenv4si"
 [(set (match_operand:V4SI 0 "register_operand" "")
        (unspec:V8HI [(match_operand:V4SI 1 "register_operand" "")
                      (match_operand:V4SI 2 "register_operand" "")]
		      UNSPEC_EXTEVEN_V4SI))]
  "TARGET_ALTIVEC"
  "
{
  rtx mask = gen_reg_rtx (V16QImode);
  rtvec v = rtvec_alloc (16);

  RTVEC_ELT (v, 0) = gen_rtx_CONST_INT (QImode, 0);
  RTVEC_ELT (v, 1) = gen_rtx_CONST_INT (QImode, 1);
  RTVEC_ELT (v, 2) = gen_rtx_CONST_INT (QImode, 2);
  RTVEC_ELT (v, 3) = gen_rtx_CONST_INT (QImode, 3);
  RTVEC_ELT (v, 4) = gen_rtx_CONST_INT (QImode, 8);
  RTVEC_ELT (v, 5) = gen_rtx_CONST_INT (QImode, 9);
  RTVEC_ELT (v, 6) = gen_rtx_CONST_INT (QImode, 10);
  RTVEC_ELT (v, 7) = gen_rtx_CONST_INT (QImode, 11);
  RTVEC_ELT (v, 8) = gen_rtx_CONST_INT (QImode, 16);
  RTVEC_ELT (v, 9) = gen_rtx_CONST_INT (QImode, 17);
  RTVEC_ELT (v, 10) = gen_rtx_CONST_INT (QImode, 18);
  RTVEC_ELT (v, 11) = gen_rtx_CONST_INT (QImode, 19);
  RTVEC_ELT (v, 12) = gen_rtx_CONST_INT (QImode, 24);
  RTVEC_ELT (v, 13) = gen_rtx_CONST_INT (QImode, 25);
  RTVEC_ELT (v, 14) = gen_rtx_CONST_INT (QImode, 26);
  RTVEC_ELT (v, 15) = gen_rtx_CONST_INT (QImode, 27);
  emit_insn (gen_vec_initv16qi (mask, gen_rtx_PARALLEL (V16QImode, v)));
  emit_insn (gen_altivec_vperm_v4si (operands[0], operands[1], operands[2], mask));
  
  DONE;
}")

(define_expand "vec_extract_evenv4sf"
 [(set (match_operand:V4SF 0 "register_operand" "")
        (unspec:V8HI [(match_operand:V4SF 1 "register_operand" "")
                      (match_operand:V4SF 2 "register_operand" "")]
                      UNSPEC_EXTEVEN_V4SF))]
  "TARGET_ALTIVEC"
  "
{ 
  rtx mask = gen_reg_rtx (V16QImode);
  rtvec v = rtvec_alloc (16);
  
  RTVEC_ELT (v, 0) = gen_rtx_CONST_INT (QImode, 0);
  RTVEC_ELT (v, 1) = gen_rtx_CONST_INT (QImode, 1);
  RTVEC_ELT (v, 2) = gen_rtx_CONST_INT (QImode, 2);
  RTVEC_ELT (v, 3) = gen_rtx_CONST_INT (QImode, 3);
  RTVEC_ELT (v, 4) = gen_rtx_CONST_INT (QImode, 8);
  RTVEC_ELT (v, 5) = gen_rtx_CONST_INT (QImode, 9);
  RTVEC_ELT (v, 6) = gen_rtx_CONST_INT (QImode, 10);
  RTVEC_ELT (v, 7) = gen_rtx_CONST_INT (QImode, 11);
  RTVEC_ELT (v, 8) = gen_rtx_CONST_INT (QImode, 16);
  RTVEC_ELT (v, 9) = gen_rtx_CONST_INT (QImode, 17);
  RTVEC_ELT (v, 10) = gen_rtx_CONST_INT (QImode, 18);
  RTVEC_ELT (v, 11) = gen_rtx_CONST_INT (QImode, 19);
  RTVEC_ELT (v, 12) = gen_rtx_CONST_INT (QImode, 24);
  RTVEC_ELT (v, 13) = gen_rtx_CONST_INT (QImode, 25);
  RTVEC_ELT (v, 14) = gen_rtx_CONST_INT (QImode, 26);
  RTVEC_ELT (v, 15) = gen_rtx_CONST_INT (QImode, 27);
  emit_insn (gen_vec_initv16qi (mask, gen_rtx_PARALLEL (V16QImode, v)));
  emit_insn (gen_altivec_vperm_v4sf (operands[0], operands[1], operands[2], mask));
  
  DONE;
}")

(define_expand "vec_extract_evenv8hi"
 [(set (match_operand:V4SI 0 "register_operand" "")
        (unspec:V8HI [(match_operand:V8HI 1 "register_operand" "")
                      (match_operand:V8HI 2 "register_operand" "")]
                      UNSPEC_EXTEVEN_V8HI))]
  "TARGET_ALTIVEC"
  "
{ 
  rtx mask = gen_reg_rtx (V16QImode);
  rtvec v = rtvec_alloc (16);
  
  RTVEC_ELT (v, 0) = gen_rtx_CONST_INT (QImode, 0);
  RTVEC_ELT (v, 1) = gen_rtx_CONST_INT (QImode, 1);
  RTVEC_ELT (v, 2) = gen_rtx_CONST_INT (QImode, 4);
  RTVEC_ELT (v, 3) = gen_rtx_CONST_INT (QImode, 5);
  RTVEC_ELT (v, 4) = gen_rtx_CONST_INT (QImode, 8);
  RTVEC_ELT (v, 5) = gen_rtx_CONST_INT (QImode, 9);
  RTVEC_ELT (v, 6) = gen_rtx_CONST_INT (QImode, 12);
  RTVEC_ELT (v, 7) = gen_rtx_CONST_INT (QImode, 13);
  RTVEC_ELT (v, 8) = gen_rtx_CONST_INT (QImode, 16);
  RTVEC_ELT (v, 9) = gen_rtx_CONST_INT (QImode, 17);
  RTVEC_ELT (v, 10) = gen_rtx_CONST_INT (QImode, 20);
  RTVEC_ELT (v, 11) = gen_rtx_CONST_INT (QImode, 21);
  RTVEC_ELT (v, 12) = gen_rtx_CONST_INT (QImode, 24);
  RTVEC_ELT (v, 13) = gen_rtx_CONST_INT (QImode, 25);
  RTVEC_ELT (v, 14) = gen_rtx_CONST_INT (QImode, 28);
  RTVEC_ELT (v, 15) = gen_rtx_CONST_INT (QImode, 29);
  emit_insn (gen_vec_initv16qi (mask, gen_rtx_PARALLEL (V16QImode, v)));
  emit_insn (gen_altivec_vperm_v8hi (operands[0], operands[1], operands[2], mask));
  
  DONE;
}")

(define_expand "vec_extract_evenv16qi"
 [(set (match_operand:V4SI 0 "register_operand" "")
        (unspec:V8HI [(match_operand:V16QI 1 "register_operand" "")
                      (match_operand:V16QI 2 "register_operand" "")]
                      UNSPEC_EXTEVEN_V16QI))]
  "TARGET_ALTIVEC"
  "
{ 
  rtx mask = gen_reg_rtx (V16QImode);
  rtvec v = rtvec_alloc (16);
  
  RTVEC_ELT (v, 0) = gen_rtx_CONST_INT (QImode, 0);
  RTVEC_ELT (v, 1) = gen_rtx_CONST_INT (QImode, 2);
  RTVEC_ELT (v, 2) = gen_rtx_CONST_INT (QImode, 4);
  RTVEC_ELT (v, 3) = gen_rtx_CONST_INT (QImode, 6);
  RTVEC_ELT (v, 4) = gen_rtx_CONST_INT (QImode, 8);
  RTVEC_ELT (v, 5) = gen_rtx_CONST_INT (QImode, 10);
  RTVEC_ELT (v, 6) = gen_rtx_CONST_INT (QImode, 12);
  RTVEC_ELT (v, 7) = gen_rtx_CONST_INT (QImode, 14);
  RTVEC_ELT (v, 8) = gen_rtx_CONST_INT (QImode, 16);
  RTVEC_ELT (v, 9) = gen_rtx_CONST_INT (QImode, 18);
  RTVEC_ELT (v, 10) = gen_rtx_CONST_INT (QImode, 20);
  RTVEC_ELT (v, 11) = gen_rtx_CONST_INT (QImode, 22);
  RTVEC_ELT (v, 12) = gen_rtx_CONST_INT (QImode, 24);
  RTVEC_ELT (v, 13) = gen_rtx_CONST_INT (QImode, 26);
  RTVEC_ELT (v, 14) = gen_rtx_CONST_INT (QImode, 28);
  RTVEC_ELT (v, 15) = gen_rtx_CONST_INT (QImode, 30);
  emit_insn (gen_vec_initv16qi (mask, gen_rtx_PARALLEL (V16QImode, v)));
  emit_insn (gen_altivec_vperm_v16qi (operands[0], operands[1], operands[2], mask));
  
  DONE;
}")

(define_expand "vec_extract_oddv4si"
 [(set (match_operand:V4SI 0 "register_operand" "")
        (unspec:V8HI [(match_operand:V4SI 1 "register_operand" "")
                      (match_operand:V4SI 2 "register_operand" "")]
                      UNSPEC_EXTODD_V4SI))]
  "TARGET_ALTIVEC"
  "
{
  rtx mask = gen_reg_rtx (V16QImode);
  rtvec v = rtvec_alloc (16);

  RTVEC_ELT (v, 0) = gen_rtx_CONST_INT (QImode, 4);
  RTVEC_ELT (v, 1) = gen_rtx_CONST_INT (QImode, 5);
  RTVEC_ELT (v, 2) = gen_rtx_CONST_INT (QImode, 6);
  RTVEC_ELT (v, 3) = gen_rtx_CONST_INT (QImode, 7);
  RTVEC_ELT (v, 4) = gen_rtx_CONST_INT (QImode, 12);
  RTVEC_ELT (v, 5) = gen_rtx_CONST_INT (QImode, 13);
  RTVEC_ELT (v, 6) = gen_rtx_CONST_INT (QImode, 14);
  RTVEC_ELT (v, 7) = gen_rtx_CONST_INT (QImode, 15);
  RTVEC_ELT (v, 8) = gen_rtx_CONST_INT (QImode, 20);
  RTVEC_ELT (v, 9) = gen_rtx_CONST_INT (QImode, 21);
  RTVEC_ELT (v, 10) = gen_rtx_CONST_INT (QImode, 22);
  RTVEC_ELT (v, 11) = gen_rtx_CONST_INT (QImode, 23);
  RTVEC_ELT (v, 12) = gen_rtx_CONST_INT (QImode, 28);
  RTVEC_ELT (v, 13) = gen_rtx_CONST_INT (QImode, 29);
  RTVEC_ELT (v, 14) = gen_rtx_CONST_INT (QImode, 30);
  RTVEC_ELT (v, 15) = gen_rtx_CONST_INT (QImode, 31);
  emit_insn (gen_vec_initv16qi (mask, gen_rtx_PARALLEL (V16QImode, v)));
  emit_insn (gen_altivec_vperm_v4si (operands[0], operands[1], operands[2], mask));
  
  DONE;
}")

(define_expand "vec_extract_oddv4sf"
 [(set (match_operand:V4SF 0 "register_operand" "")
        (unspec:V8HI [(match_operand:V4SF 1 "register_operand" "")
                      (match_operand:V4SF 2 "register_operand" "")]
                      UNSPEC_EXTODD_V4SF))]
  "TARGET_ALTIVEC"
  "
{
  rtx mask = gen_reg_rtx (V16QImode);
  rtvec v = rtvec_alloc (16);

  RTVEC_ELT (v, 0) = gen_rtx_CONST_INT (QImode, 4);
  RTVEC_ELT (v, 1) = gen_rtx_CONST_INT (QImode, 5);
  RTVEC_ELT (v, 2) = gen_rtx_CONST_INT (QImode, 6);
  RTVEC_ELT (v, 3) = gen_rtx_CONST_INT (QImode, 7);
  RTVEC_ELT (v, 4) = gen_rtx_CONST_INT (QImode, 12);
  RTVEC_ELT (v, 5) = gen_rtx_CONST_INT (QImode, 13);
  RTVEC_ELT (v, 6) = gen_rtx_CONST_INT (QImode, 14);
  RTVEC_ELT (v, 7) = gen_rtx_CONST_INT (QImode, 15);
  RTVEC_ELT (v, 8) = gen_rtx_CONST_INT (QImode, 20);
  RTVEC_ELT (v, 9) = gen_rtx_CONST_INT (QImode, 21);
  RTVEC_ELT (v, 10) = gen_rtx_CONST_INT (QImode, 22);
  RTVEC_ELT (v, 11) = gen_rtx_CONST_INT (QImode, 23);
  RTVEC_ELT (v, 12) = gen_rtx_CONST_INT (QImode, 28);
  RTVEC_ELT (v, 13) = gen_rtx_CONST_INT (QImode, 29);
  RTVEC_ELT (v, 14) = gen_rtx_CONST_INT (QImode, 30);
  RTVEC_ELT (v, 15) = gen_rtx_CONST_INT (QImode, 31);
  emit_insn (gen_vec_initv16qi (mask, gen_rtx_PARALLEL (V16QImode, v)));
  emit_insn (gen_altivec_vperm_v4sf (operands[0], operands[1], operands[2], mask));

  DONE;
}")

(define_insn "vpkuhum_nomode"
  [(set (match_operand:V16QI 0 "register_operand" "=v")
        (unspec:V16QI [(match_operand 1 "register_operand" "v")
                       (match_operand 2 "register_operand" "v")]
                      UNSPEC_VPKUHUM))] 
  "TARGET_ALTIVEC"
  "vpkuhum %0,%1,%2"
  [(set_attr "type" "vecperm")])

(define_insn "vpkuwum_nomode"
  [(set (match_operand:V8HI 0 "register_operand" "=v")
        (unspec:V8HI [(match_operand 1 "register_operand" "v")
                      (match_operand 2 "register_operand" "v")]
                     UNSPEC_VPKUWUM))]
  "TARGET_ALTIVEC"
  "vpkuwum %0,%1,%2"
  [(set_attr "type" "vecperm")])

(define_expand "vec_extract_oddv8hi"
 [(set (match_operand:V8HI 0 "register_operand" "")
        (unspec:V8HI [(match_operand:V8HI 1 "register_operand" "")
                      (match_operand:V8HI 2 "register_operand" "")]
                      UNSPEC_EXTODD_V8HI))]
  "TARGET_ALTIVEC"
  "
{
  emit_insn (gen_vpkuwum_nomode (operands[0], operands[1], operands[2]));
  DONE;
}")

(define_expand "vec_extract_oddv16qi"
 [(set (match_operand:V16QI 0 "register_operand" "")
        (unspec:V16QI [(match_operand:V16QI 1 "register_operand" "")
                      (match_operand:V16QI 2 "register_operand" "")]
                      UNSPEC_EXTODD_V16QI))]
  "TARGET_ALTIVEC"
  "
{
  emit_insn (gen_vpkuhum_nomode (operands[0], operands[1], operands[2]));
  DONE;
}")

(define_expand "vec_interleave_high<mode>"
 [(set (match_operand:VI 0 "register_operand" "")
        (unspec:VI [(match_operand:VI 1 "register_operand" "")
                    (match_operand:VI 2 "register_operand" "")]
                     UNSPEC_INTERHI))]
  "TARGET_ALTIVEC"
  "
{
  emit_insn (gen_altivec_vmrgh<VI_char> (operands[0], operands[1], operands[2]));
  DONE;
}")

(define_expand "vec_interleave_low<mode>"
 [(set (match_operand:VI 0 "register_operand" "")
        (unspec:VI [(match_operand:VI 1 "register_operand" "")
                    (match_operand:VI 2 "register_operand" "")]
                     UNSPEC_INTERLO))]
  "TARGET_ALTIVEC"
  "
{
  emit_insn (gen_altivec_vmrgl<VI_char> (operands[0], operands[1], operands[2]));
  DONE;
}")

(define_expand "vec_unpacks_float_hi_v8hi"
 [(set (match_operand:V4SF 0 "register_operand" "")
        (unspec:V4SF [(match_operand:V8HI 1 "register_operand" "")]
                     UNSPEC_VUPKHS_V4SF))]
  "TARGET_ALTIVEC"
  "
{
  rtx tmp = gen_reg_rtx (V4SImode);

  emit_insn (gen_vec_unpacks_hi_v8hi (tmp, operands[1]));
  emit_insn (gen_altivec_vcfsx (operands[0], tmp, const0_rtx));
  DONE;
}")

(define_expand "vec_unpacks_float_lo_v8hi"
 [(set (match_operand:V4SF 0 "register_operand" "")
        (unspec:V4SF [(match_operand:V8HI 1 "register_operand" "")]
                     UNSPEC_VUPKLS_V4SF))]
  "TARGET_ALTIVEC"
  "
{
  rtx tmp = gen_reg_rtx (V4SImode);

  emit_insn (gen_vec_unpacks_lo_v8hi (tmp, operands[1]));
  emit_insn (gen_altivec_vcfsx (operands[0], tmp, const0_rtx));
  DONE;
}")

(define_expand "vec_unpacku_float_hi_v8hi"
 [(set (match_operand:V4SF 0 "register_operand" "")
        (unspec:V4SF [(match_operand:V8HI 1 "register_operand" "")]
                     UNSPEC_VUPKHU_V4SF))]
  "TARGET_ALTIVEC"
  "
{
  rtx tmp = gen_reg_rtx (V4SImode);

  emit_insn (gen_vec_unpacku_hi_v8hi (tmp, operands[1]));
  emit_insn (gen_altivec_vcfux (operands[0], tmp, const0_rtx));
  DONE;
}")

(define_expand "vec_unpacku_float_lo_v8hi"
 [(set (match_operand:V4SF 0 "register_operand" "")
        (unspec:V4SF [(match_operand:V8HI 1 "register_operand" "")]
                     UNSPEC_VUPKLU_V4SF))]
  "TARGET_ALTIVEC"
  "
{
  rtx tmp = gen_reg_rtx (V4SImode);

  emit_insn (gen_vec_unpacku_lo_v8hi (tmp, operands[1]));
  emit_insn (gen_altivec_vcfux (operands[0], tmp, const0_rtx));
  DONE;
}")
