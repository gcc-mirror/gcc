;; AltiVec patterns.
;; Copyright (C) 2002-2016 Free Software Foundation, Inc.
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

(define_c_enum "unspec"
  [UNSPEC_VCMPBFP
   UNSPEC_VMSUMU
   UNSPEC_VMSUMM
   UNSPEC_VMSUMSHM
   UNSPEC_VMSUMUHS
   UNSPEC_VMSUMSHS
   UNSPEC_VMHADDSHS
   UNSPEC_VMHRADDSHS
   UNSPEC_VADDCUW
   UNSPEC_VADDU
   UNSPEC_VADDS
   UNSPEC_VAVGU
   UNSPEC_VAVGS
   UNSPEC_VMULEUB
   UNSPEC_VMULESB
   UNSPEC_VMULEUH
   UNSPEC_VMULESH
   UNSPEC_VMULOUB
   UNSPEC_VMULOSB
   UNSPEC_VMULOUH
   UNSPEC_VMULOSH
   UNSPEC_VPKPX
   UNSPEC_VPACK_SIGN_SIGN_SAT
   UNSPEC_VPACK_SIGN_UNS_SAT
   UNSPEC_VPACK_UNS_UNS_SAT
   UNSPEC_VPACK_UNS_UNS_MOD
   UNSPEC_VPACK_UNS_UNS_MOD_DIRECT
   UNSPEC_VSLV4SI
   UNSPEC_VSLO
   UNSPEC_VSR
   UNSPEC_VSRO
   UNSPEC_VSUBCUW
   UNSPEC_VSUBU
   UNSPEC_VSUBS
   UNSPEC_VSUM4UBS
   UNSPEC_VSUM4S
   UNSPEC_VSUM2SWS
   UNSPEC_VSUMSWS
   UNSPEC_VPERM
   UNSPEC_VPERMR
   UNSPEC_VPERM_UNS
   UNSPEC_VRFIN
   UNSPEC_VCFUX
   UNSPEC_VCFSX
   UNSPEC_VCTUXS
   UNSPEC_VCTSXS
   UNSPEC_VLOGEFP
   UNSPEC_VEXPTEFP
   UNSPEC_VSLDOI
   UNSPEC_VUNPACK_HI_SIGN
   UNSPEC_VUNPACK_LO_SIGN
   UNSPEC_VUNPACK_HI_SIGN_DIRECT
   UNSPEC_VUNPACK_LO_SIGN_DIRECT
   UNSPEC_VUPKHPX
   UNSPEC_VUPKLPX
   UNSPEC_DARN
   UNSPEC_DARN_32
   UNSPEC_DARN_RAW
   UNSPEC_DST
   UNSPEC_DSTT
   UNSPEC_DSTST
   UNSPEC_DSTSTT
   UNSPEC_LVSL
   UNSPEC_LVSR
   UNSPEC_LVE
   UNSPEC_STVX
   UNSPEC_STVXL
   UNSPEC_STVE
   UNSPEC_SET_VSCR
   UNSPEC_GET_VRSAVE
   UNSPEC_LVX
   UNSPEC_REDUC_PLUS
   UNSPEC_VECSH
   UNSPEC_EXTEVEN_V4SI
   UNSPEC_EXTEVEN_V8HI
   UNSPEC_EXTEVEN_V16QI
   UNSPEC_EXTEVEN_V4SF
   UNSPEC_EXTODD_V4SI
   UNSPEC_EXTODD_V8HI
   UNSPEC_EXTODD_V16QI
   UNSPEC_EXTODD_V4SF
   UNSPEC_INTERHI_V4SI
   UNSPEC_INTERHI_V8HI
   UNSPEC_INTERHI_V16QI
   UNSPEC_INTERLO_V4SI
   UNSPEC_INTERLO_V8HI
   UNSPEC_INTERLO_V16QI
   UNSPEC_LVLX
   UNSPEC_LVLXL
   UNSPEC_LVRX
   UNSPEC_LVRXL
   UNSPEC_STVLX
   UNSPEC_STVLXL
   UNSPEC_STVRX
   UNSPEC_STVRXL
   UNSPEC_VMULWHUB
   UNSPEC_VMULWLUB
   UNSPEC_VMULWHSB
   UNSPEC_VMULWLSB
   UNSPEC_VMULWHUH
   UNSPEC_VMULWLUH
   UNSPEC_VMULWHSH
   UNSPEC_VMULWLSH
   UNSPEC_VUPKHUB
   UNSPEC_VUPKHUH
   UNSPEC_VUPKLUB
   UNSPEC_VUPKLUH
   UNSPEC_VPERMSI
   UNSPEC_VPERMHI
   UNSPEC_INTERHI
   UNSPEC_INTERLO
   UNSPEC_VUPKHS_V4SF
   UNSPEC_VUPKLS_V4SF
   UNSPEC_VUPKHU_V4SF
   UNSPEC_VUPKLU_V4SF
   UNSPEC_VGBBD
   UNSPEC_VMRGH_DIRECT
   UNSPEC_VMRGL_DIRECT
   UNSPEC_VSPLT_DIRECT
   UNSPEC_VSUMSWS_DIRECT
   UNSPEC_VADDCUQ
   UNSPEC_VADDEUQM
   UNSPEC_VADDECUQ
   UNSPEC_VSUBCUQ
   UNSPEC_VSUBEUQM
   UNSPEC_VSUBECUQ
   UNSPEC_VBPERMQ
   UNSPEC_BCDADD
   UNSPEC_BCDSUB
   UNSPEC_BCD_OVERFLOW
])

(define_c_enum "unspecv"
  [UNSPECV_SET_VRSAVE
   UNSPECV_MTVSCR
   UNSPECV_MFVSCR
   UNSPECV_DSSALL
   UNSPECV_DSS
  ])

;; Vec int modes
(define_mode_iterator VI [V4SI V8HI V16QI])
;; Like VI, but add ISA 2.07 integer vector ops
(define_mode_iterator VI2 [V4SI V8HI V16QI V2DI])
;; Short vec in modes
(define_mode_iterator VIshort [V8HI V16QI])
;; Vec float modes
(define_mode_iterator VF [V4SF])
;; Vec modes, pity mode iterators are not composable
(define_mode_iterator V [V4SI V8HI V16QI V4SF])
;; Vec modes for move/logical/permute ops, include vector types for move not
;; otherwise handled by altivec (v2df, v2di, ti)
(define_mode_iterator VM [V4SI
			  V8HI
			  V16QI
			  V4SF
			  V2DF
			  V2DI
			  V1TI
			  TI
			  (KF "FLOAT128_VECTOR_P (KFmode)")
			  (TF "FLOAT128_VECTOR_P (TFmode)")])

;; Like VM, except don't do TImode
(define_mode_iterator VM2 [V4SI
			   V8HI
			   V16QI
			   V4SF
			   V2DF
			   V2DI
			   V1TI
			   (KF "FLOAT128_VECTOR_P (KFmode)")
			   (TF "FLOAT128_VECTOR_P (TFmode)")])

;; Specific iterator for parity which does not have a byte/half-word form, but
;; does have a quad word form
(define_mode_iterator VParity [V4SI
			       V2DI
			       V1TI
			       (TI "TARGET_VSX_TIMODE")])

(define_mode_attr VI_char [(V2DI "d") (V4SI "w") (V8HI "h") (V16QI "b")])
(define_mode_attr VI_scalar [(V2DI "DI") (V4SI "SI") (V8HI "HI") (V16QI "QI")])
(define_mode_attr VI_unit [(V16QI "VECTOR_UNIT_ALTIVEC_P (V16QImode)")
			   (V8HI "VECTOR_UNIT_ALTIVEC_P (V8HImode)")
			   (V4SI "VECTOR_UNIT_ALTIVEC_P (V4SImode)")
			   (V2DI "VECTOR_UNIT_P8_VECTOR_P (V2DImode)")
			   (V1TI "VECTOR_UNIT_ALTIVEC_P (V1TImode)")])

;; Vector pack/unpack
(define_mode_iterator VP [V2DI V4SI V8HI])
(define_mode_attr VP_small [(V2DI "V4SI") (V4SI "V8HI") (V8HI "V16QI")])
(define_mode_attr VP_small_lc [(V2DI "v4si") (V4SI "v8hi") (V8HI "v16qi")])
(define_mode_attr VU_char [(V2DI "w") (V4SI "h") (V8HI "b")])

;; Vector move instructions.
(define_insn "*altivec_mov<mode>"
  [(set (match_operand:VM2 0 "nonimmediate_operand" "=Z,v,v,*Y,*r,*r,v,v,*r")
	(match_operand:VM2 1 "input_operand" "v,Z,v,r,Y,r,j,W,W"))]
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
    case 8: return "#";
    default: gcc_unreachable ();
    }
}
  [(set_attr "type" "vecstore,vecload,vecsimple,store,load,*,vecsimple,*,*")
   (set_attr "length" "4,4,4,20,20,20,4,8,32")])

;; Unlike other altivec moves, allow the GPRs, since a normal use of TImode
;; is for unions.  However for plain data movement, slightly favor the vector
;; loads
(define_insn "*altivec_movti"
  [(set (match_operand:TI 0 "nonimmediate_operand" "=Z,v,v,?Y,?r,?r,v,v")
	(match_operand:TI 1 "input_operand" "v,Z,v,r,Y,r,j,W"))]
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
  "VECTOR_UNIT_ALTIVEC_OR_VSX_P (<MODE>mode) && reload_completed"
  [(const_int 0)]
{
  rtx dest = operands[0];
  machine_mode mode = GET_MODE (operands[0]);
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
  emit_insn (gen_rtx_SET (dest, gen_rtx_ASHIFT (mode, dest, dest)));
  DONE;
})

(define_split
  [(set (match_operand:VM 0 "altivec_register_operand" "")
	(match_operand:VM 1 "easy_vector_constant_add_self" ""))]
  "VECTOR_UNIT_ALTIVEC_OR_VSX_P (<MODE>mode) && reload_completed"
  [(set (match_dup 0) (match_dup 3))
   (set (match_dup 0) (match_dup 4))]
{
  rtx dup = gen_easy_altivec_constant (operands[1]);
  rtx const_vec;
  machine_mode op_mode = <MODE>mode;

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

(define_split
  [(set (match_operand:VM 0 "altivec_register_operand" "")
	(match_operand:VM 1 "easy_vector_constant_vsldoi" ""))]
  "VECTOR_UNIT_ALTIVEC_OR_VSX_P (<MODE>mode) && can_create_pseudo_p ()"
  [(set (match_dup 2) (match_dup 3))
   (set (match_dup 4) (match_dup 5))
   (set (match_dup 0)
        (unspec:VM [(match_dup 2)
		    (match_dup 4)
		    (match_dup 6)]
		   UNSPEC_VSLDOI))]
{
  rtx op1 = operands[1];
  int elt = (BYTES_BIG_ENDIAN) ? 0 : GET_MODE_NUNITS (<MODE>mode) - 1;
  HOST_WIDE_INT val = const_vector_elt_as_int (op1, elt);
  rtx rtx_val = GEN_INT (val);
  int shift = vspltis_shifted (op1);
  int nunits = GET_MODE_NUNITS (<MODE>mode);
  int i;

  gcc_assert (shift != 0);
  operands[2] = gen_reg_rtx (<MODE>mode);
  operands[3] = gen_rtx_CONST_VECTOR (<MODE>mode, rtvec_alloc (nunits));
  operands[4] = gen_reg_rtx (<MODE>mode);

  if (shift < 0)
    {
      operands[5] = CONSTM1_RTX (<MODE>mode);
      operands[6] = GEN_INT (-shift);
    }
  else
    {
      operands[5] = CONST0_RTX (<MODE>mode);
      operands[6] = GEN_INT (shift);
    }

  /* Populate the constant vectors.  */
  for (i = 0; i < nunits; i++)
    XVECEXP (operands[3], 0, i) = rtx_val;
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

;; The save_vregs and restore_vregs patterns don't use memory_operand
;; because (plus (reg) (const_int)) is not a valid vector address.
;; This way is more compact than describing exactly what happens in
;; the out-of-line functions, ie. loading the constant into r11/r12
;; then using indexed addressing, and requires less editing of rtl
;; to describe the operation to dwarf2out_frame_debug_expr.
(define_insn "*save_vregs_<mode>_r11"
  [(match_parallel 0 "any_parallel_operand"
     [(clobber (reg:P 65))
      (use (match_operand:P 1 "symbol_ref_operand" "s"))
      (clobber (reg:P 11))
      (use (reg:P 0))
      (set (mem:V4SI (plus:P (match_operand:P 2 "gpc_reg_operand" "b")
			     (match_operand:P 3 "short_cint_operand" "I")))
	   (match_operand:V4SI 4 "altivec_register_operand" "v"))])]
  "TARGET_ALTIVEC"
  "bl %1"
  [(set_attr "type" "branch")
   (set_attr "length" "4")])

(define_insn "*save_vregs_<mode>_r12"
  [(match_parallel 0 "any_parallel_operand"
     [(clobber (reg:P 65))
      (use (match_operand:P 1 "symbol_ref_operand" "s"))
      (clobber (reg:P 12))
      (use (reg:P 0))
      (set (mem:V4SI (plus:P (match_operand:P 2 "gpc_reg_operand" "b")
			     (match_operand:P 3 "short_cint_operand" "I")))
	   (match_operand:V4SI 4 "altivec_register_operand" "v"))])]
  "TARGET_ALTIVEC"
  "bl %1"
  [(set_attr "type" "branch")
   (set_attr "length" "4")])

(define_insn "*restore_vregs_<mode>_r11"
  [(match_parallel 0 "any_parallel_operand"
     [(clobber (reg:P 65))
      (use (match_operand:P 1 "symbol_ref_operand" "s"))
      (clobber (reg:P 11))
      (use (reg:P 0))
      (set (match_operand:V4SI 2 "altivec_register_operand" "=v")
	   (mem:V4SI (plus:P (match_operand:P 3 "gpc_reg_operand" "b")
			     (match_operand:P 4 "short_cint_operand" "I"))))])]
  "TARGET_ALTIVEC"
  "bl %1"
  [(set_attr "type" "branch")
   (set_attr "length" "4")])

(define_insn "*restore_vregs_<mode>_r12"
  [(match_parallel 0 "any_parallel_operand"
     [(clobber (reg:P 65))
      (use (match_operand:P 1 "symbol_ref_operand" "s"))
      (clobber (reg:P 12))
      (use (reg:P 0))
      (set (match_operand:V4SI 2 "altivec_register_operand" "=v")
	   (mem:V4SI (plus:P (match_operand:P 3 "gpc_reg_operand" "b")
			     (match_operand:P 4 "short_cint_operand" "I"))))])]
  "TARGET_ALTIVEC"
  "bl %1"
  [(set_attr "type" "branch")
   (set_attr "length" "4")])

;; Simple binary operations.

;; add
(define_insn "add<mode>3"
  [(set (match_operand:VI2 0 "register_operand" "=v")
        (plus:VI2 (match_operand:VI2 1 "register_operand" "v")
		  (match_operand:VI2 2 "register_operand" "v")))]
  "<VI_unit>"
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
  "VECTOR_UNIT_ALTIVEC_P (V4SImode)"
  "vaddcuw %0,%1,%2"
  [(set_attr "type" "vecsimple")])

(define_insn "altivec_vaddu<VI_char>s"
  [(set (match_operand:VI 0 "register_operand" "=v")
        (unspec:VI [(match_operand:VI 1 "register_operand" "v")
		    (match_operand:VI 2 "register_operand" "v")]
		   UNSPEC_VADDU))
   (set (reg:SI 110) (unspec:SI [(const_int 0)] UNSPEC_SET_VSCR))]
  "<VI_unit>"
  "vaddu<VI_char>s %0,%1,%2"
  [(set_attr "type" "vecsimple")])

(define_insn "altivec_vadds<VI_char>s"
  [(set (match_operand:VI 0 "register_operand" "=v")
        (unspec:VI [(match_operand:VI 1 "register_operand" "v")
                    (match_operand:VI 2 "register_operand" "v")]
		   UNSPEC_VADDS))
   (set (reg:SI 110) (unspec:SI [(const_int 0)] UNSPEC_SET_VSCR))]
  "VECTOR_UNIT_ALTIVEC_P (<MODE>mode)"
  "vadds<VI_char>s %0,%1,%2"
  [(set_attr "type" "vecsimple")])

;; sub
(define_insn "sub<mode>3"
  [(set (match_operand:VI2 0 "register_operand" "=v")
        (minus:VI2 (match_operand:VI2 1 "register_operand" "v")
		   (match_operand:VI2 2 "register_operand" "v")))]
  "<VI_unit>"
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
  "VECTOR_UNIT_ALTIVEC_P (V4SImode)"
  "vsubcuw %0,%1,%2"
  [(set_attr "type" "vecsimple")])

(define_insn "altivec_vsubu<VI_char>s"
  [(set (match_operand:VI 0 "register_operand" "=v")
        (unspec:VI [(match_operand:VI 1 "register_operand" "v")
                    (match_operand:VI 2 "register_operand" "v")]
		   UNSPEC_VSUBU))
   (set (reg:SI 110) (unspec:SI [(const_int 0)] UNSPEC_SET_VSCR))]
  "VECTOR_UNIT_ALTIVEC_P (<MODE>mode)"
  "vsubu<VI_char>s %0,%1,%2"
  [(set_attr "type" "vecsimple")])

(define_insn "altivec_vsubs<VI_char>s"
  [(set (match_operand:VI 0 "register_operand" "=v")
        (unspec:VI [(match_operand:VI 1 "register_operand" "v")
                    (match_operand:VI 2 "register_operand" "v")]
		   UNSPEC_VSUBS))
   (set (reg:SI 110) (unspec:SI [(const_int 0)] UNSPEC_SET_VSCR))]
  "VECTOR_UNIT_ALTIVEC_P (<MODE>mode)"
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
  "VECTOR_UNIT_ALTIVEC_P (<MODE>mode)"
  "vavgs<VI_char> %0,%1,%2"
  [(set_attr "type" "vecsimple")])

(define_insn "altivec_vcmpbfp"
  [(set (match_operand:V4SI 0 "register_operand" "=v")
        (unspec:V4SI [(match_operand:V4SF 1 "register_operand" "v")
                      (match_operand:V4SF 2 "register_operand" "v")] 
                      UNSPEC_VCMPBFP))]
  "VECTOR_UNIT_ALTIVEC_P (V4SImode)"
  "vcmpbfp %0,%1,%2"
  [(set_attr "type" "veccmp")])

(define_insn "*altivec_eq<mode>"
  [(set (match_operand:VI2 0 "altivec_register_operand" "=v")
	(eq:VI2 (match_operand:VI2 1 "altivec_register_operand" "v")
		(match_operand:VI2 2 "altivec_register_operand" "v")))]
  "<VI_unit>"
  "vcmpequ<VI_char> %0,%1,%2"
  [(set_attr "type" "veccmp")])

(define_insn "*altivec_gt<mode>"
  [(set (match_operand:VI2 0 "altivec_register_operand" "=v")
	(gt:VI2 (match_operand:VI2 1 "altivec_register_operand" "v")
		(match_operand:VI2 2 "altivec_register_operand" "v")))]
  "<VI_unit>"
  "vcmpgts<VI_char> %0,%1,%2"
  [(set_attr "type" "veccmp")])

(define_insn "*altivec_gtu<mode>"
  [(set (match_operand:VI2 0 "altivec_register_operand" "=v")
	(gtu:VI2 (match_operand:VI2 1 "altivec_register_operand" "v")
		 (match_operand:VI2 2 "altivec_register_operand" "v")))]
  "<VI_unit>"
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
		(match_operand:VM 4 "zero_constant" ""))
	 (match_operand:VM 2 "altivec_register_operand" "v")
	 (match_operand:VM 3 "altivec_register_operand" "v")))]
  "VECTOR_MEM_ALTIVEC_P (<MODE>mode)"
  "vsel %0,%3,%2,%1"
  [(set_attr "type" "vecperm")])

(define_insn "*altivec_vsel<mode>_uns"
  [(set (match_operand:VM 0 "altivec_register_operand" "=v")
	(if_then_else:VM
	 (ne:CCUNS (match_operand:VM 1 "altivec_register_operand" "v")
		   (match_operand:VM 4 "zero_constant" ""))
	 (match_operand:VM 2 "altivec_register_operand" "v")
	 (match_operand:VM 3 "altivec_register_operand" "v")))]
  "VECTOR_MEM_ALTIVEC_P (<MODE>mode)"
  "vsel %0,%3,%2,%1"
  [(set_attr "type" "vecperm")])

;; Fused multiply add.

(define_insn "*altivec_fmav4sf4"
  [(set (match_operand:V4SF 0 "register_operand" "=v")
	(fma:V4SF (match_operand:V4SF 1 "register_operand" "v")
		  (match_operand:V4SF 2 "register_operand" "v")
		  (match_operand:V4SF 3 "register_operand" "v")))]
  "VECTOR_UNIT_ALTIVEC_P (V4SFmode)"
  "vmaddfp %0,%1,%2,%3"
  [(set_attr "type" "vecfloat")])

;; We do multiply as a fused multiply-add with an add of a -0.0 vector.

(define_expand "altivec_mulv4sf3"
  [(set (match_operand:V4SF 0 "register_operand" "")
	(fma:V4SF (match_operand:V4SF 1 "register_operand" "")
		  (match_operand:V4SF 2 "register_operand" "")
		  (match_dup 3)))]
  "VECTOR_UNIT_ALTIVEC_P (V4SFmode)"
{
  rtx neg0;

  /* Generate [-0.0, -0.0, -0.0, -0.0].  */
  neg0 = gen_reg_rtx (V4SImode);
  emit_insn (gen_altivec_vspltisw (neg0, constm1_rtx));
  emit_insn (gen_vashlv4si3 (neg0, neg0, neg0));

  operands[3] = gen_lowpart (V4SFmode, neg0);
})

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
(define_insn "mulv4si3_p8"
  [(set (match_operand:V4SI 0 "register_operand" "=v")
        (mult:V4SI (match_operand:V4SI 1 "register_operand" "v")
                   (match_operand:V4SI 2 "register_operand" "v")))]
  "TARGET_P8_VECTOR"
  "vmuluwm %0,%1,%2"
  [(set_attr "type" "veccomplex")])

(define_expand "mulv4si3"
  [(use (match_operand:V4SI 0 "register_operand" ""))
   (use (match_operand:V4SI 1 "register_operand" ""))
   (use (match_operand:V4SI 2 "register_operand" ""))]
   "TARGET_ALTIVEC"
{
  rtx zero;
  rtx swap;
  rtx small_swap;
  rtx sixteen;
  rtx one;
  rtx two;
  rtx low_product;
  rtx high_product;
       
  if (TARGET_P8_VECTOR)
    {
      emit_insn (gen_mulv4si3_p8 (operands[0], operands[1], operands[2]));
      DONE;
    }

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
})
 
(define_expand "mulv8hi3"
  [(use (match_operand:V8HI 0 "register_operand" ""))
   (use (match_operand:V8HI 1 "register_operand" ""))
   (use (match_operand:V8HI 2 "register_operand" ""))]
   "TARGET_ALTIVEC"
{
  rtx zero = gen_reg_rtx (V8HImode);

  emit_insn (gen_altivec_vspltish (zero, const0_rtx));
  emit_insn (gen_altivec_vmladduhm(operands[0], operands[1], operands[2], zero));

  DONE;
})

;; Fused multiply subtract 
(define_insn "*altivec_vnmsubfp"
  [(set (match_operand:V4SF 0 "register_operand" "=v")
	(neg:V4SF
	 (fma:V4SF (match_operand:V4SF 1 "register_operand" "v")
		   (match_operand:V4SF 2 "register_operand" "v")
		   (neg:V4SF
		    (match_operand:V4SF 3 "register_operand" "v")))))]
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
  [(set (match_operand:VI2 0 "register_operand" "=v")
        (umax:VI2 (match_operand:VI2 1 "register_operand" "v")
		  (match_operand:VI2 2 "register_operand" "v")))]
  "<VI_unit>"
  "vmaxu<VI_char> %0,%1,%2"
  [(set_attr "type" "vecsimple")])

(define_insn "smax<mode>3"
  [(set (match_operand:VI2 0 "register_operand" "=v")
        (smax:VI2 (match_operand:VI2 1 "register_operand" "v")
		  (match_operand:VI2 2 "register_operand" "v")))]
  "<VI_unit>"
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
  [(set (match_operand:VI2 0 "register_operand" "=v")
        (umin:VI2 (match_operand:VI2 1 "register_operand" "v")
		  (match_operand:VI2 2 "register_operand" "v")))]
  "<VI_unit>"
  "vminu<VI_char> %0,%1,%2"
  [(set_attr "type" "vecsimple")])

(define_insn "smin<mode>3"
  [(set (match_operand:VI2 0 "register_operand" "=v")
        (smin:VI2 (match_operand:VI2 1 "register_operand" "v")
		  (match_operand:VI2 2 "register_operand" "v")))]
  "<VI_unit>"
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
        (plus:V8HI (mult:V8HI (match_operand:V8HI 1 "register_operand" "v")
		   	      (match_operand:V8HI 2 "register_operand" "v"))
		   (match_operand:V8HI 3 "register_operand" "v")))]
  "TARGET_ALTIVEC"
  "vmladduhm %0,%1,%2,%3"
  [(set_attr "type" "veccomplex")])

(define_expand "altivec_vmrghb"
  [(use (match_operand:V16QI 0 "register_operand" ""))
   (use (match_operand:V16QI 1 "register_operand" ""))
   (use (match_operand:V16QI 2 "register_operand" ""))]
  "TARGET_ALTIVEC"
{
  rtvec v;
  rtx x;

  /* Special handling for LE with -maltivec=be.  */
  if (!BYTES_BIG_ENDIAN && VECTOR_ELT_ORDER_BIG)
    {
      v = gen_rtvec (16, GEN_INT (8), GEN_INT (24), GEN_INT (9), GEN_INT (25),
                     GEN_INT (10), GEN_INT (26), GEN_INT (11), GEN_INT (27),
		     GEN_INT (12), GEN_INT (28), GEN_INT (13), GEN_INT (29),
		     GEN_INT (14), GEN_INT (30), GEN_INT (15), GEN_INT (31));
      x = gen_rtx_VEC_CONCAT (V32QImode, operands[2], operands[1]);
    }
  else
    {
      v = gen_rtvec (16, GEN_INT (0), GEN_INT (16), GEN_INT (1), GEN_INT (17),
                     GEN_INT (2), GEN_INT (18), GEN_INT (3), GEN_INT (19),
		     GEN_INT (4), GEN_INT (20), GEN_INT (5), GEN_INT (21),
		     GEN_INT (6), GEN_INT (22), GEN_INT (7), GEN_INT (23));
      x = gen_rtx_VEC_CONCAT (V32QImode, operands[1], operands[2]);
    }

  x = gen_rtx_VEC_SELECT (V16QImode, x, gen_rtx_PARALLEL (VOIDmode, v));
  emit_insn (gen_rtx_SET (operands[0], x));
  DONE;
})

(define_insn "*altivec_vmrghb_internal"
  [(set (match_operand:V16QI 0 "register_operand" "=v")
        (vec_select:V16QI
	  (vec_concat:V32QI
	    (match_operand:V16QI 1 "register_operand" "v")
	    (match_operand:V16QI 2 "register_operand" "v"))
	  (parallel [(const_int 0) (const_int 16)
		     (const_int 1) (const_int 17)
		     (const_int 2) (const_int 18)
		     (const_int 3) (const_int 19)
		     (const_int 4) (const_int 20)
		     (const_int 5) (const_int 21)
		     (const_int 6) (const_int 22)
		     (const_int 7) (const_int 23)])))]
  "TARGET_ALTIVEC"
{
  if (BYTES_BIG_ENDIAN)
    return "vmrghb %0,%1,%2";
  else
    return "vmrglb %0,%2,%1";
}
  [(set_attr "type" "vecperm")])

(define_insn "altivec_vmrghb_direct"
  [(set (match_operand:V16QI 0 "register_operand" "=v")
        (unspec:V16QI [(match_operand:V16QI 1 "register_operand" "v")
                       (match_operand:V16QI 2 "register_operand" "v")]
		      UNSPEC_VMRGH_DIRECT))]
  "TARGET_ALTIVEC"
  "vmrghb %0,%1,%2"
  [(set_attr "type" "vecperm")])

(define_expand "altivec_vmrghh"
  [(use (match_operand:V8HI 0 "register_operand" ""))
   (use (match_operand:V8HI 1 "register_operand" ""))
   (use (match_operand:V8HI 2 "register_operand" ""))]
  "TARGET_ALTIVEC"
{
  rtvec v;
  rtx x;

  /* Special handling for LE with -maltivec=be.  */
  if (!BYTES_BIG_ENDIAN && VECTOR_ELT_ORDER_BIG)
    {
      v = gen_rtvec (8, GEN_INT (4), GEN_INT (12), GEN_INT (5), GEN_INT (13),
                     GEN_INT (6), GEN_INT (14), GEN_INT (7), GEN_INT (15));
      x = gen_rtx_VEC_CONCAT (V16HImode, operands[2], operands[1]);
    }
  else
    {
      v = gen_rtvec (8, GEN_INT (0), GEN_INT (8), GEN_INT (1), GEN_INT (9),
                     GEN_INT (2), GEN_INT (10), GEN_INT (3), GEN_INT (11));
      x = gen_rtx_VEC_CONCAT (V16HImode, operands[1], operands[2]);
    }

  x = gen_rtx_VEC_SELECT (V8HImode, x, gen_rtx_PARALLEL (VOIDmode, v));
  emit_insn (gen_rtx_SET (operands[0], x));
  DONE;
})

(define_insn "*altivec_vmrghh_internal"
  [(set (match_operand:V8HI 0 "register_operand" "=v")
        (vec_select:V8HI
	  (vec_concat:V16HI
	    (match_operand:V8HI 1 "register_operand" "v")
	    (match_operand:V8HI 2 "register_operand" "v"))
	  (parallel [(const_int 0) (const_int 8)
		     (const_int 1) (const_int 9)
		     (const_int 2) (const_int 10)
		     (const_int 3) (const_int 11)])))]
  "TARGET_ALTIVEC"
{
  if (BYTES_BIG_ENDIAN)
    return "vmrghh %0,%1,%2";
  else
    return "vmrglh %0,%2,%1";
}
  [(set_attr "type" "vecperm")])

(define_insn "altivec_vmrghh_direct"
  [(set (match_operand:V8HI 0 "register_operand" "=v")
        (unspec:V8HI [(match_operand:V8HI 1 "register_operand" "v")
                      (match_operand:V8HI 2 "register_operand" "v")]
                     UNSPEC_VMRGH_DIRECT))]
  "TARGET_ALTIVEC"
  "vmrghh %0,%1,%2"
  [(set_attr "type" "vecperm")])

(define_expand "altivec_vmrghw"
  [(use (match_operand:V4SI 0 "register_operand" ""))
   (use (match_operand:V4SI 1 "register_operand" ""))
   (use (match_operand:V4SI 2 "register_operand" ""))]
  "VECTOR_MEM_ALTIVEC_P (V4SImode)"
{
  rtvec v;
  rtx x;

  /* Special handling for LE with -maltivec=be.  */
  if (!BYTES_BIG_ENDIAN && VECTOR_ELT_ORDER_BIG)
    {
      v = gen_rtvec (4, GEN_INT (2), GEN_INT (6), GEN_INT (3), GEN_INT (7));
      x = gen_rtx_VEC_CONCAT (V8SImode, operands[2], operands[1]);
    }
  else
    {
      v = gen_rtvec (4, GEN_INT (0), GEN_INT (4), GEN_INT (1), GEN_INT (5));
      x = gen_rtx_VEC_CONCAT (V8SImode, operands[1], operands[2]);
    }

  x = gen_rtx_VEC_SELECT (V4SImode, x, gen_rtx_PARALLEL (VOIDmode, v));
  emit_insn (gen_rtx_SET (operands[0], x));
  DONE;
})

(define_insn "*altivec_vmrghw_internal"
  [(set (match_operand:V4SI 0 "register_operand" "=v")
        (vec_select:V4SI
	  (vec_concat:V8SI
	    (match_operand:V4SI 1 "register_operand" "v")
	    (match_operand:V4SI 2 "register_operand" "v"))
	  (parallel [(const_int 0) (const_int 4)
		     (const_int 1) (const_int 5)])))]
  "VECTOR_MEM_ALTIVEC_P (V4SImode)"
{
  if (BYTES_BIG_ENDIAN)
    return "vmrghw %0,%1,%2";
  else
    return "vmrglw %0,%2,%1";
}
  [(set_attr "type" "vecperm")])

(define_insn "altivec_vmrghw_direct"
  [(set (match_operand:V4SI 0 "register_operand" "=v")
        (unspec:V4SI [(match_operand:V4SI 1 "register_operand" "v")
                      (match_operand:V4SI 2 "register_operand" "v")]
                     UNSPEC_VMRGH_DIRECT))]
  "TARGET_ALTIVEC"
  "vmrghw %0,%1,%2"
  [(set_attr "type" "vecperm")])

(define_insn "*altivec_vmrghsf"
  [(set (match_operand:V4SF 0 "register_operand" "=v")
        (vec_select:V4SF
	  (vec_concat:V8SF
	    (match_operand:V4SF 1 "register_operand" "v")
	    (match_operand:V4SF 2 "register_operand" "v"))
	  (parallel [(const_int 0) (const_int 4)
		     (const_int 1) (const_int 5)])))]
  "VECTOR_MEM_ALTIVEC_P (V4SFmode)"
{
  if (BYTES_BIG_ENDIAN)
    return "vmrghw %0,%1,%2";
  else
    return "vmrglw %0,%2,%1";
}
  [(set_attr "type" "vecperm")])

(define_expand "altivec_vmrglb"
  [(use (match_operand:V16QI 0 "register_operand" ""))
   (use (match_operand:V16QI 1 "register_operand" ""))
   (use (match_operand:V16QI 2 "register_operand" ""))]
  "TARGET_ALTIVEC"
{
  rtvec v;
  rtx x;

  /* Special handling for LE with -maltivec=be.  */
  if (!BYTES_BIG_ENDIAN && VECTOR_ELT_ORDER_BIG)
    {
      v = gen_rtvec (16, GEN_INT (0), GEN_INT (16), GEN_INT (1), GEN_INT (17),
                     GEN_INT (2), GEN_INT (18), GEN_INT (3), GEN_INT (19),
		     GEN_INT (4), GEN_INT (20), GEN_INT (5), GEN_INT (21),
		     GEN_INT (6), GEN_INT (22), GEN_INT (7), GEN_INT (23));
      x = gen_rtx_VEC_CONCAT (V32QImode, operands[2], operands[1]);
    }
  else
    {
      v = gen_rtvec (16, GEN_INT (8), GEN_INT (24), GEN_INT (9), GEN_INT (25),
                     GEN_INT (10), GEN_INT (26), GEN_INT (11), GEN_INT (27),
		     GEN_INT (12), GEN_INT (28), GEN_INT (13), GEN_INT (29),
		     GEN_INT (14), GEN_INT (30), GEN_INT (15), GEN_INT (31));
      x = gen_rtx_VEC_CONCAT (V32QImode, operands[1], operands[2]);
    }

  x = gen_rtx_VEC_SELECT (V16QImode, x, gen_rtx_PARALLEL (VOIDmode, v));
  emit_insn (gen_rtx_SET (operands[0], x));
  DONE;
})

(define_insn "*altivec_vmrglb_internal"
  [(set (match_operand:V16QI 0 "register_operand" "=v")
        (vec_select:V16QI
	  (vec_concat:V32QI
	    (match_operand:V16QI 1 "register_operand" "v")
	    (match_operand:V16QI 2 "register_operand" "v"))
	  (parallel [(const_int  8) (const_int 24)
		     (const_int  9) (const_int 25)
		     (const_int 10) (const_int 26)
		     (const_int 11) (const_int 27)
		     (const_int 12) (const_int 28)
		     (const_int 13) (const_int 29)
		     (const_int 14) (const_int 30)
		     (const_int 15) (const_int 31)])))]
  "TARGET_ALTIVEC"
{
  if (BYTES_BIG_ENDIAN)
    return "vmrglb %0,%1,%2";
  else
    return "vmrghb %0,%2,%1";
}
  [(set_attr "type" "vecperm")])

(define_insn "altivec_vmrglb_direct"
  [(set (match_operand:V16QI 0 "register_operand" "=v")
        (unspec:V16QI [(match_operand:V16QI 1 "register_operand" "v")
    		       (match_operand:V16QI 2 "register_operand" "v")]
                      UNSPEC_VMRGL_DIRECT))]
  "TARGET_ALTIVEC"
  "vmrglb %0,%1,%2"
  [(set_attr "type" "vecperm")])

(define_expand "altivec_vmrglh"
  [(use (match_operand:V8HI 0 "register_operand" ""))
   (use (match_operand:V8HI 1 "register_operand" ""))
   (use (match_operand:V8HI 2 "register_operand" ""))]
  "TARGET_ALTIVEC"
{
  rtvec v;
  rtx x;

  /* Special handling for LE with -maltivec=be.  */
  if (!BYTES_BIG_ENDIAN && VECTOR_ELT_ORDER_BIG)
    {
      v = gen_rtvec (8, GEN_INT (0), GEN_INT (8), GEN_INT (1), GEN_INT (9),
                     GEN_INT (2), GEN_INT (10), GEN_INT (3), GEN_INT (11));
      x = gen_rtx_VEC_CONCAT (V16HImode, operands[2], operands[1]);
    }
  else
    {
      v = gen_rtvec (8, GEN_INT (4), GEN_INT (12), GEN_INT (5), GEN_INT (13),
                     GEN_INT (6), GEN_INT (14), GEN_INT (7), GEN_INT (15));
      x = gen_rtx_VEC_CONCAT (V16HImode, operands[1], operands[2]);
    }

  x = gen_rtx_VEC_SELECT (V8HImode, x, gen_rtx_PARALLEL (VOIDmode, v));
  emit_insn (gen_rtx_SET (operands[0], x));
  DONE;
})

(define_insn "*altivec_vmrglh_internal"
  [(set (match_operand:V8HI 0 "register_operand" "=v")
        (vec_select:V8HI
	  (vec_concat:V16HI
	    (match_operand:V8HI 1 "register_operand" "v")
	    (match_operand:V8HI 2 "register_operand" "v"))
	  (parallel [(const_int 4) (const_int 12)
		     (const_int 5) (const_int 13)
		     (const_int 6) (const_int 14)
		     (const_int 7) (const_int 15)])))]
  "TARGET_ALTIVEC"
{
  if (BYTES_BIG_ENDIAN)
    return "vmrglh %0,%1,%2";
  else
    return "vmrghh %0,%2,%1";
}
  [(set_attr "type" "vecperm")])

(define_insn "altivec_vmrglh_direct"
  [(set (match_operand:V8HI 0 "register_operand" "=v")
        (unspec:V8HI [(match_operand:V8HI 1 "register_operand" "v")
		      (match_operand:V8HI 2 "register_operand" "v")]
                     UNSPEC_VMRGL_DIRECT))]
  "TARGET_ALTIVEC"
  "vmrglh %0,%1,%2"
  [(set_attr "type" "vecperm")])

(define_expand "altivec_vmrglw"
  [(use (match_operand:V4SI 0 "register_operand" ""))
   (use (match_operand:V4SI 1 "register_operand" ""))
   (use (match_operand:V4SI 2 "register_operand" ""))]
  "VECTOR_MEM_ALTIVEC_P (V4SImode)"
{
  rtvec v;
  rtx x;

  /* Special handling for LE with -maltivec=be.  */
  if (!BYTES_BIG_ENDIAN && VECTOR_ELT_ORDER_BIG)
    {
      v = gen_rtvec (4, GEN_INT (0), GEN_INT (4), GEN_INT (1), GEN_INT (5));
      x = gen_rtx_VEC_CONCAT (V8SImode, operands[2], operands[1]);
    }
  else
    {
      v = gen_rtvec (4, GEN_INT (2), GEN_INT (6), GEN_INT (3), GEN_INT (7));
      x = gen_rtx_VEC_CONCAT (V8SImode, operands[1], operands[2]);
    }

  x = gen_rtx_VEC_SELECT (V4SImode, x, gen_rtx_PARALLEL (VOIDmode, v));
  emit_insn (gen_rtx_SET (operands[0], x));
  DONE;
})

(define_insn "*altivec_vmrglw_internal"
  [(set (match_operand:V4SI 0 "register_operand" "=v")
        (vec_select:V4SI
	  (vec_concat:V8SI
	    (match_operand:V4SI 1 "register_operand" "v")
	    (match_operand:V4SI 2 "register_operand" "v"))
	  (parallel [(const_int 2) (const_int 6)
		     (const_int 3) (const_int 7)])))]
  "VECTOR_MEM_ALTIVEC_P (V4SImode)"
{
  if (BYTES_BIG_ENDIAN)
    return "vmrglw %0,%1,%2";
  else
    return "vmrghw %0,%2,%1";
}
  [(set_attr "type" "vecperm")])

(define_insn "altivec_vmrglw_direct"
  [(set (match_operand:V4SI 0 "register_operand" "=v")
        (unspec:V4SI [(match_operand:V4SI 1 "register_operand" "v")
	              (match_operand:V4SI 2 "register_operand" "v")]
                     UNSPEC_VMRGL_DIRECT))]
  "TARGET_ALTIVEC"
  "vmrglw %0,%1,%2"
  [(set_attr "type" "vecperm")])

(define_insn "*altivec_vmrglsf"
  [(set (match_operand:V4SF 0 "register_operand" "=v")
        (vec_select:V4SF
	 (vec_concat:V8SF
	   (match_operand:V4SF 1 "register_operand" "v")
	   (match_operand:V4SF 2 "register_operand" "v"))
	 (parallel [(const_int 2) (const_int 6)
		    (const_int 3) (const_int 7)])))]
  "VECTOR_MEM_ALTIVEC_P (V4SFmode)"
{
  if (BYTES_BIG_ENDIAN)
    return "vmrglw %0,%1,%2";
  else
    return "vmrghw %0,%2,%1";
}
  [(set_attr "type" "vecperm")])

;; Power8 vector merge even/odd
(define_insn "p8_vmrgew"
  [(set (match_operand:V4SI 0 "register_operand" "=v")
	(vec_select:V4SI
	  (vec_concat:V8SI
	    (match_operand:V4SI 1 "register_operand" "v")
	    (match_operand:V4SI 2 "register_operand" "v"))
	  (parallel [(const_int 0) (const_int 4)
		     (const_int 2) (const_int 6)])))]
  "TARGET_P8_VECTOR"
{
  if (BYTES_BIG_ENDIAN)
    return "vmrgew %0,%1,%2";
  else
    return "vmrgow %0,%2,%1";
}
  [(set_attr "type" "vecperm")])

(define_insn "p8_vmrgow"
  [(set (match_operand:V4SI 0 "register_operand" "=v")
	(vec_select:V4SI
	  (vec_concat:V8SI
	    (match_operand:V4SI 1 "register_operand" "v")
	    (match_operand:V4SI 2 "register_operand" "v"))
	  (parallel [(const_int 1) (const_int 5)
		     (const_int 3) (const_int 7)])))]
  "TARGET_P8_VECTOR"
{
  if (BYTES_BIG_ENDIAN)
    return "vmrgow %0,%1,%2";
  else
    return "vmrgew %0,%2,%1";
}
  [(set_attr "type" "vecperm")])

(define_expand "vec_widen_umult_even_v16qi"
  [(use (match_operand:V8HI 0 "register_operand" ""))
   (use (match_operand:V16QI 1 "register_operand" ""))
   (use (match_operand:V16QI 2 "register_operand" ""))]
  "TARGET_ALTIVEC"
{
  if (VECTOR_ELT_ORDER_BIG)
    emit_insn (gen_altivec_vmuleub (operands[0], operands[1], operands[2]));
  else
    emit_insn (gen_altivec_vmuloub (operands[0], operands[1], operands[2]));
  DONE;
})

(define_expand "vec_widen_smult_even_v16qi"
  [(use (match_operand:V8HI 0 "register_operand" ""))
   (use (match_operand:V16QI 1 "register_operand" ""))
   (use (match_operand:V16QI 2 "register_operand" ""))]
  "TARGET_ALTIVEC"
{
  if (VECTOR_ELT_ORDER_BIG)
    emit_insn (gen_altivec_vmulesb (operands[0], operands[1], operands[2]));
  else
    emit_insn (gen_altivec_vmulosb (operands[0], operands[1], operands[2]));
  DONE;
})

(define_expand "vec_widen_umult_even_v8hi"
  [(use (match_operand:V4SI 0 "register_operand" ""))
   (use (match_operand:V8HI 1 "register_operand" ""))
   (use (match_operand:V8HI 2 "register_operand" ""))]
  "TARGET_ALTIVEC"
{
  if (VECTOR_ELT_ORDER_BIG)
    emit_insn (gen_altivec_vmuleuh (operands[0], operands[1], operands[2]));
  else
    emit_insn (gen_altivec_vmulouh (operands[0], operands[1], operands[2]));
  DONE;
})

(define_expand "vec_widen_smult_even_v8hi"
  [(use (match_operand:V4SI 0 "register_operand" ""))
   (use (match_operand:V8HI 1 "register_operand" ""))
   (use (match_operand:V8HI 2 "register_operand" ""))]
  "TARGET_ALTIVEC"
{
  if (VECTOR_ELT_ORDER_BIG)
    emit_insn (gen_altivec_vmulesh (operands[0], operands[1], operands[2]));
  else
    emit_insn (gen_altivec_vmulosh (operands[0], operands[1], operands[2]));
  DONE;
})

(define_expand "vec_widen_umult_odd_v16qi"
  [(use (match_operand:V8HI 0 "register_operand" ""))
   (use (match_operand:V16QI 1 "register_operand" ""))
   (use (match_operand:V16QI 2 "register_operand" ""))]
  "TARGET_ALTIVEC"
{
  if (VECTOR_ELT_ORDER_BIG)
    emit_insn (gen_altivec_vmuloub (operands[0], operands[1], operands[2]));
  else
    emit_insn (gen_altivec_vmuleub (operands[0], operands[1], operands[2]));
  DONE;
})

(define_expand "vec_widen_smult_odd_v16qi"
  [(use (match_operand:V8HI 0 "register_operand" ""))
   (use (match_operand:V16QI 1 "register_operand" ""))
   (use (match_operand:V16QI 2 "register_operand" ""))]
  "TARGET_ALTIVEC"
{
  if (VECTOR_ELT_ORDER_BIG)
    emit_insn (gen_altivec_vmulosb (operands[0], operands[1], operands[2]));
  else
    emit_insn (gen_altivec_vmulesb (operands[0], operands[1], operands[2]));
  DONE;
})

(define_expand "vec_widen_umult_odd_v8hi"
  [(use (match_operand:V4SI 0 "register_operand" ""))
   (use (match_operand:V8HI 1 "register_operand" ""))
   (use (match_operand:V8HI 2 "register_operand" ""))]
  "TARGET_ALTIVEC"
{
  if (VECTOR_ELT_ORDER_BIG)
    emit_insn (gen_altivec_vmulouh (operands[0], operands[1], operands[2]));
  else
    emit_insn (gen_altivec_vmuleuh (operands[0], operands[1], operands[2]));
  DONE;
})

(define_expand "vec_widen_smult_odd_v8hi"
  [(use (match_operand:V4SI 0 "register_operand" ""))
   (use (match_operand:V8HI 1 "register_operand" ""))
   (use (match_operand:V8HI 2 "register_operand" ""))]
  "TARGET_ALTIVEC"
{
  if (VECTOR_ELT_ORDER_BIG)
    emit_insn (gen_altivec_vmulosh (operands[0], operands[1], operands[2]));
  else
    emit_insn (gen_altivec_vmulesh (operands[0], operands[1], operands[2]));
  DONE;
})

(define_insn "altivec_vmuleub"
  [(set (match_operand:V8HI 0 "register_operand" "=v")
        (unspec:V8HI [(match_operand:V16QI 1 "register_operand" "v")
                      (match_operand:V16QI 2 "register_operand" "v")]
		     UNSPEC_VMULEUB))]
  "TARGET_ALTIVEC"
  "vmuleub %0,%1,%2"
  [(set_attr "type" "veccomplex")])

(define_insn "altivec_vmuloub"
  [(set (match_operand:V8HI 0 "register_operand" "=v")
        (unspec:V8HI [(match_operand:V16QI 1 "register_operand" "v")
                      (match_operand:V16QI 2 "register_operand" "v")]
		     UNSPEC_VMULOUB))]
  "TARGET_ALTIVEC"
  "vmuloub %0,%1,%2"
  [(set_attr "type" "veccomplex")])

(define_insn "altivec_vmulesb"
  [(set (match_operand:V8HI 0 "register_operand" "=v")
        (unspec:V8HI [(match_operand:V16QI 1 "register_operand" "v")
                      (match_operand:V16QI 2 "register_operand" "v")]
		     UNSPEC_VMULESB))]
  "TARGET_ALTIVEC"
  "vmulesb %0,%1,%2"
  [(set_attr "type" "veccomplex")])

(define_insn "altivec_vmulosb"
  [(set (match_operand:V8HI 0 "register_operand" "=v")
        (unspec:V8HI [(match_operand:V16QI 1 "register_operand" "v")
                      (match_operand:V16QI 2 "register_operand" "v")]
		     UNSPEC_VMULOSB))]
  "TARGET_ALTIVEC"
  "vmulosb %0,%1,%2"
  [(set_attr "type" "veccomplex")])

(define_insn "altivec_vmuleuh"
  [(set (match_operand:V4SI 0 "register_operand" "=v")
        (unspec:V4SI [(match_operand:V8HI 1 "register_operand" "v")
                      (match_operand:V8HI 2 "register_operand" "v")]
		     UNSPEC_VMULEUH))]
  "TARGET_ALTIVEC"
  "vmuleuh %0,%1,%2"
  [(set_attr "type" "veccomplex")])

(define_insn "altivec_vmulouh"
  [(set (match_operand:V4SI 0 "register_operand" "=v")
        (unspec:V4SI [(match_operand:V8HI 1 "register_operand" "v")
                      (match_operand:V8HI 2 "register_operand" "v")]
		     UNSPEC_VMULOUH))]
  "TARGET_ALTIVEC"
  "vmulouh %0,%1,%2"
  [(set_attr "type" "veccomplex")])

(define_insn "altivec_vmulesh"
  [(set (match_operand:V4SI 0 "register_operand" "=v")
        (unspec:V4SI [(match_operand:V8HI 1 "register_operand" "v")
                      (match_operand:V8HI 2 "register_operand" "v")]
		     UNSPEC_VMULESH))]
  "TARGET_ALTIVEC"
  "vmulesh %0,%1,%2"
  [(set_attr "type" "veccomplex")])

(define_insn "altivec_vmulosh"
  [(set (match_operand:V4SI 0 "register_operand" "=v")
        (unspec:V4SI [(match_operand:V8HI 1 "register_operand" "v")
                      (match_operand:V8HI 2 "register_operand" "v")]
		     UNSPEC_VMULOSH))]
  "TARGET_ALTIVEC"
  "vmulosh %0,%1,%2"
  [(set_attr "type" "veccomplex")])


;; Vector pack/unpack
(define_insn "altivec_vpkpx"
  [(set (match_operand:V8HI 0 "register_operand" "=v")
        (unspec:V8HI [(match_operand:V4SI 1 "register_operand" "v")
                      (match_operand:V4SI 2 "register_operand" "v")]
		     UNSPEC_VPKPX))]
  "TARGET_ALTIVEC"
  "*
  {
    if (VECTOR_ELT_ORDER_BIG)
      return \"vpkpx %0,%1,%2\";
    else
      return \"vpkpx %0,%2,%1\";
  }"
  [(set_attr "type" "vecperm")])

(define_insn "altivec_vpks<VI_char>ss"
  [(set (match_operand:<VP_small> 0 "register_operand" "=v")
	(unspec:<VP_small> [(match_operand:VP 1 "register_operand" "v")
			    (match_operand:VP 2 "register_operand" "v")]
			   UNSPEC_VPACK_SIGN_SIGN_SAT))]
  "<VI_unit>"
  "*
  {
    if (VECTOR_ELT_ORDER_BIG)
      return \"vpks<VI_char>ss %0,%1,%2\";
    else
      return \"vpks<VI_char>ss %0,%2,%1\";
  }"
  [(set_attr "type" "vecperm")])

(define_insn "altivec_vpks<VI_char>us"
  [(set (match_operand:<VP_small> 0 "register_operand" "=v")
	(unspec:<VP_small> [(match_operand:VP 1 "register_operand" "v")
			    (match_operand:VP 2 "register_operand" "v")]
			   UNSPEC_VPACK_SIGN_UNS_SAT))]
  "<VI_unit>"
  "*
  {
    if (VECTOR_ELT_ORDER_BIG)
      return \"vpks<VI_char>us %0,%1,%2\";
    else
      return \"vpks<VI_char>us %0,%2,%1\";
  }"
  [(set_attr "type" "vecperm")])

(define_insn "altivec_vpku<VI_char>us"
  [(set (match_operand:<VP_small> 0 "register_operand" "=v")
	(unspec:<VP_small> [(match_operand:VP 1 "register_operand" "v")
			    (match_operand:VP 2 "register_operand" "v")]
			   UNSPEC_VPACK_UNS_UNS_SAT))]
  "<VI_unit>"
  "*
  {
    if (VECTOR_ELT_ORDER_BIG)
      return \"vpku<VI_char>us %0,%1,%2\";
    else
      return \"vpku<VI_char>us %0,%2,%1\";
  }"
  [(set_attr "type" "vecperm")])

(define_insn "altivec_vpku<VI_char>um"
  [(set (match_operand:<VP_small> 0 "register_operand" "=v")
	(unspec:<VP_small> [(match_operand:VP 1 "register_operand" "v")
			    (match_operand:VP 2 "register_operand" "v")]
			   UNSPEC_VPACK_UNS_UNS_MOD))]
  "<VI_unit>"
  "*
  {
    if (VECTOR_ELT_ORDER_BIG)
      return \"vpku<VI_char>um %0,%1,%2\";
    else
      return \"vpku<VI_char>um %0,%2,%1\";
  }"
  [(set_attr "type" "vecperm")])

(define_insn "altivec_vpku<VI_char>um_direct"
  [(set (match_operand:<VP_small> 0 "register_operand" "=v")
	(unspec:<VP_small> [(match_operand:VP 1 "register_operand" "v")
			    (match_operand:VP 2 "register_operand" "v")]
			   UNSPEC_VPACK_UNS_UNS_MOD_DIRECT))]
  "<VI_unit>"
  "*
  {
    if (BYTES_BIG_ENDIAN)
      return \"vpku<VI_char>um %0,%1,%2\";
    else
      return \"vpku<VI_char>um %0,%2,%1\";
  }"
  [(set_attr "type" "vecperm")])

(define_insn "*altivec_vrl<VI_char>"
  [(set (match_operand:VI2 0 "register_operand" "=v")
        (rotate:VI2 (match_operand:VI2 1 "register_operand" "v")
		    (match_operand:VI2 2 "register_operand" "v")))]
  "<VI_unit>"
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
  [(set (match_operand:VI2 0 "register_operand" "=v")
        (ashift:VI2 (match_operand:VI2 1 "register_operand" "v")
		    (match_operand:VI2 2 "register_operand" "v")))]
  "<VI_unit>"
  "vsl<VI_char> %0,%1,%2"
  [(set_attr "type" "vecsimple")])

(define_insn "*altivec_vsr<VI_char>"
  [(set (match_operand:VI2 0 "register_operand" "=v")
        (lshiftrt:VI2 (match_operand:VI2 1 "register_operand" "v")
		      (match_operand:VI2 2 "register_operand" "v")))]
  "<VI_unit>"
  "vsr<VI_char> %0,%1,%2"
  [(set_attr "type" "vecsimple")])

(define_insn "*altivec_vsra<VI_char>"
  [(set (match_operand:VI2 0 "register_operand" "=v")
        (ashiftrt:VI2 (match_operand:VI2 1 "register_operand" "v")
		      (match_operand:VI2 2 "register_operand" "v")))]
  "<VI_unit>"
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

;; FIXME: For the following two patterns, the scratch should only be
;; allocated for !VECTOR_ELT_ORDER_BIG, and the instructions should
;; be emitted separately.
(define_insn "altivec_vsum2sws"
  [(set (match_operand:V4SI 0 "register_operand" "=v")
        (unspec:V4SI [(match_operand:V4SI 1 "register_operand" "v")
                      (match_operand:V4SI 2 "register_operand" "v")]
		     UNSPEC_VSUM2SWS))
   (set (reg:SI 110) (unspec:SI [(const_int 0)] UNSPEC_SET_VSCR))
   (clobber (match_scratch:V4SI 3 "=v"))]
  "TARGET_ALTIVEC"
{
  if (VECTOR_ELT_ORDER_BIG)
    return "vsum2sws %0,%1,%2";
  else
    return "vsldoi %3,%2,%2,12\n\tvsum2sws %3,%1,%3\n\tvsldoi %0,%3,%3,4";
}
  [(set_attr "type" "veccomplex")
   (set (attr "length")
     (if_then_else
       (match_test "VECTOR_ELT_ORDER_BIG")
       (const_string "4")
       (const_string "12")))])

(define_insn "altivec_vsumsws"
  [(set (match_operand:V4SI 0 "register_operand" "=v")
        (unspec:V4SI [(match_operand:V4SI 1 "register_operand" "v")
                      (match_operand:V4SI 2 "register_operand" "v")]
		     UNSPEC_VSUMSWS))
   (set (reg:SI 110) (unspec:SI [(const_int 0)] UNSPEC_SET_VSCR))
   (clobber (match_scratch:V4SI 3 "=v"))]
  "TARGET_ALTIVEC"
{
  if (VECTOR_ELT_ORDER_BIG)
    return "vsumsws %0,%1,%2";
  else
    return "vspltw %3,%2,0\n\tvsumsws %3,%1,%3\n\tvsldoi %0,%3,%3,12";
}
  [(set_attr "type" "veccomplex")
   (set (attr "length")
     (if_then_else
       (match_test "(VECTOR_ELT_ORDER_BIG)")
       (const_string "4")
       (const_string "12")))])

(define_insn "altivec_vsumsws_direct"
  [(set (match_operand:V4SI 0 "register_operand" "=v")
        (unspec:V4SI [(match_operand:V4SI 1 "register_operand" "v")
                      (match_operand:V4SI 2 "register_operand" "v")]
		     UNSPEC_VSUMSWS_DIRECT))
   (set (reg:SI 110) (unspec:SI [(const_int 0)] UNSPEC_SET_VSCR))]
  "TARGET_ALTIVEC"
  "vsumsws %0,%1,%2"
  [(set_attr "type" "veccomplex")])

(define_expand "altivec_vspltb"
  [(use (match_operand:V16QI 0 "register_operand" ""))
   (use (match_operand:V16QI 1 "register_operand" ""))
   (use (match_operand:QI 2 "u5bit_cint_operand" ""))]
  "TARGET_ALTIVEC"
{
  rtvec v;
  rtx x;

  /* Special handling for LE with -maltivec=be.  We have to reflect
     the actual selected index for the splat in the RTL.  */
  if (!BYTES_BIG_ENDIAN && VECTOR_ELT_ORDER_BIG)
    operands[2] = GEN_INT (15 - INTVAL (operands[2]));

  v = gen_rtvec (1, operands[2]);
  x = gen_rtx_VEC_SELECT (QImode, operands[1], gen_rtx_PARALLEL (VOIDmode, v));
  x = gen_rtx_VEC_DUPLICATE (V16QImode, x);
  emit_insn (gen_rtx_SET (operands[0], x));
  DONE;
})

(define_insn "*altivec_vspltb_internal"
  [(set (match_operand:V16QI 0 "register_operand" "=v")
        (vec_duplicate:V16QI
	 (vec_select:QI (match_operand:V16QI 1 "register_operand" "v")
			(parallel
			 [(match_operand:QI 2 "u5bit_cint_operand" "")]))))]
  "TARGET_ALTIVEC"
{
  /* For true LE, this adjusts the selected index.  For LE with 
     -maltivec=be, this reverses what was done in the define_expand
     because the instruction already has big-endian bias.  */
  if (!BYTES_BIG_ENDIAN)
    operands[2] = GEN_INT (15 - INTVAL (operands[2]));

  return "vspltb %0,%1,%2";
}
  [(set_attr "type" "vecperm")])

(define_insn "altivec_vspltb_direct"
  [(set (match_operand:V16QI 0 "register_operand" "=v")
        (unspec:V16QI [(match_operand:V16QI 1 "register_operand" "v")
	               (match_operand:QI 2 "u5bit_cint_operand" "i")]
                      UNSPEC_VSPLT_DIRECT))]
  "TARGET_ALTIVEC"
  "vspltb %0,%1,%2"
  [(set_attr "type" "vecperm")])

(define_expand "altivec_vsplth"
  [(use (match_operand:V8HI 0 "register_operand" ""))
   (use (match_operand:V8HI 1 "register_operand" ""))
   (use (match_operand:QI 2 "u5bit_cint_operand" ""))]
  "TARGET_ALTIVEC"
{
  rtvec v;
  rtx x;

  /* Special handling for LE with -maltivec=be.  We have to reflect
     the actual selected index for the splat in the RTL.  */
  if (!BYTES_BIG_ENDIAN && VECTOR_ELT_ORDER_BIG)
    operands[2] = GEN_INT (7 - INTVAL (operands[2]));

  v = gen_rtvec (1, operands[2]);
  x = gen_rtx_VEC_SELECT (HImode, operands[1], gen_rtx_PARALLEL (VOIDmode, v));
  x = gen_rtx_VEC_DUPLICATE (V8HImode, x);
  emit_insn (gen_rtx_SET (operands[0], x));
  DONE;
})

(define_insn "*altivec_vsplth_internal"
  [(set (match_operand:V8HI 0 "register_operand" "=v")
	(vec_duplicate:V8HI
	 (vec_select:HI (match_operand:V8HI 1 "register_operand" "v")
			(parallel
			 [(match_operand:QI 2 "u5bit_cint_operand" "")]))))]
  "TARGET_ALTIVEC"
{
  /* For true LE, this adjusts the selected index.  For LE with 
     -maltivec=be, this reverses what was done in the define_expand
     because the instruction already has big-endian bias.  */
  if (!BYTES_BIG_ENDIAN)
    operands[2] = GEN_INT (7 - INTVAL (operands[2]));

  return "vsplth %0,%1,%2";
}
  [(set_attr "type" "vecperm")])

(define_insn "altivec_vsplth_direct"
  [(set (match_operand:V8HI 0 "register_operand" "=v")
        (unspec:V8HI [(match_operand:V8HI 1 "register_operand" "v")
                      (match_operand:QI 2 "u5bit_cint_operand" "i")]
                     UNSPEC_VSPLT_DIRECT))]
  "TARGET_ALTIVEC"
  "vsplth %0,%1,%2"
  [(set_attr "type" "vecperm")])

(define_expand "altivec_vspltw"
  [(use (match_operand:V4SI 0 "register_operand" ""))
   (use (match_operand:V4SI 1 "register_operand" ""))
   (use (match_operand:QI 2 "u5bit_cint_operand" ""))]
  "TARGET_ALTIVEC"
{
  rtvec v;
  rtx x;

  /* Special handling for LE with -maltivec=be.  We have to reflect
     the actual selected index for the splat in the RTL.  */
  if (!BYTES_BIG_ENDIAN && VECTOR_ELT_ORDER_BIG)
    operands[2] = GEN_INT (3 - INTVAL (operands[2]));

  v = gen_rtvec (1, operands[2]);
  x = gen_rtx_VEC_SELECT (SImode, operands[1], gen_rtx_PARALLEL (VOIDmode, v));
  x = gen_rtx_VEC_DUPLICATE (V4SImode, x);
  emit_insn (gen_rtx_SET (operands[0], x));
  DONE;
})

(define_insn "*altivec_vspltw_internal"
  [(set (match_operand:V4SI 0 "register_operand" "=v")
	(vec_duplicate:V4SI
	 (vec_select:SI (match_operand:V4SI 1 "register_operand" "v")
			(parallel
			 [(match_operand:QI 2 "u5bit_cint_operand" "i")]))))]
  "TARGET_ALTIVEC"
{
  /* For true LE, this adjusts the selected index.  For LE with 
     -maltivec=be, this reverses what was done in the define_expand
     because the instruction already has big-endian bias.  */
  if (!BYTES_BIG_ENDIAN)
    operands[2] = GEN_INT (3 - INTVAL (operands[2]));

  return "vspltw %0,%1,%2";
}
  [(set_attr "type" "vecperm")])

(define_insn "altivec_vspltw_direct"
  [(set (match_operand:V4SI 0 "register_operand" "=v")
        (unspec:V4SI [(match_operand:V4SI 1 "register_operand" "v")
                      (match_operand:QI 2 "u5bit_cint_operand" "i")]
                     UNSPEC_VSPLT_DIRECT))]
  "TARGET_ALTIVEC"
  "vspltw %0,%1,%2"
  [(set_attr "type" "vecperm")])

(define_expand "altivec_vspltsf"
  [(use (match_operand:V4SF 0 "register_operand" ""))
   (use (match_operand:V4SF 1 "register_operand" ""))
   (use (match_operand:QI 2 "u5bit_cint_operand" ""))]
  "TARGET_ALTIVEC"
{
  rtvec v;
  rtx x;

  /* Special handling for LE with -maltivec=be.  We have to reflect
     the actual selected index for the splat in the RTL.  */
  if (!BYTES_BIG_ENDIAN && VECTOR_ELT_ORDER_BIG)
    operands[2] = GEN_INT (3 - INTVAL (operands[2]));

  v = gen_rtvec (1, operands[2]);
  x = gen_rtx_VEC_SELECT (SFmode, operands[1], gen_rtx_PARALLEL (VOIDmode, v));
  x = gen_rtx_VEC_DUPLICATE (V4SFmode, x);
  emit_insn (gen_rtx_SET (operands[0], x));
  DONE;
})

(define_insn "*altivec_vspltsf_internal"
  [(set (match_operand:V4SF 0 "register_operand" "=v")
	(vec_duplicate:V4SF
	 (vec_select:SF (match_operand:V4SF 1 "register_operand" "v")
			(parallel
			 [(match_operand:QI 2 "u5bit_cint_operand" "i")]))))]
  "VECTOR_UNIT_ALTIVEC_P (V4SFmode)"
{
  /* For true LE, this adjusts the selected index.  For LE with 
     -maltivec=be, this reverses what was done in the define_expand
     because the instruction already has big-endian bias.  */
  if (!BYTES_BIG_ENDIAN)
    operands[2] = GEN_INT (3 - INTVAL (operands[2]));

  return "vspltw %0,%1,%2";
}
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

(define_expand "altivec_vperm_<mode>"
  [(set (match_operand:VM 0 "register_operand" "")
	(unspec:VM [(match_operand:VM 1 "register_operand" "")
		    (match_operand:VM 2 "register_operand" "")
		    (match_operand:V16QI 3 "register_operand" "")]
		   UNSPEC_VPERM))]
  "TARGET_ALTIVEC"
{
  if (!VECTOR_ELT_ORDER_BIG)
    {
      altivec_expand_vec_perm_le (operands);
      DONE;
    }
})

;; Slightly prefer vperm, since the target does not overlap the source
(define_insn "*altivec_vperm_<mode>_internal"
  [(set (match_operand:VM 0 "register_operand" "=v,?wo")
	(unspec:VM [(match_operand:VM 1 "register_operand" "v,0")
		    (match_operand:VM 2 "register_operand" "v,wo")
		    (match_operand:V16QI 3 "register_operand" "v,wo")]
		   UNSPEC_VPERM))]
  "TARGET_ALTIVEC"
  "@
   vperm %0,%1,%2,%3
   xxperm %x0,%x2,%x3"
  [(set_attr "type" "vecperm")
   (set_attr "length" "4")])

(define_insn "altivec_vperm_v8hiv16qi"
  [(set (match_operand:V16QI 0 "register_operand" "=v,?wo")
	(unspec:V16QI [(match_operand:V8HI 1 "register_operand" "v,0")
   	               (match_operand:V8HI 2 "register_operand" "v,wo")
		       (match_operand:V16QI 3 "register_operand" "v,wo")]
		   UNSPEC_VPERM))]
  "TARGET_ALTIVEC"
  "@
   vperm %0,%1,%2,%3
   xxperm %x0,%x2,%x3"
  [(set_attr "type" "vecperm")
   (set_attr "length" "4")])

(define_expand "altivec_vperm_<mode>_uns"
  [(set (match_operand:VM 0 "register_operand" "")
	(unspec:VM [(match_operand:VM 1 "register_operand" "")
		    (match_operand:VM 2 "register_operand" "")
		    (match_operand:V16QI 3 "register_operand" "")]
		   UNSPEC_VPERM_UNS))]
  "TARGET_ALTIVEC"
{
  if (!VECTOR_ELT_ORDER_BIG)
    {
      altivec_expand_vec_perm_le (operands);
      DONE;
    }
})

(define_insn "*altivec_vperm_<mode>_uns_internal"
  [(set (match_operand:VM 0 "register_operand" "=v,?wo")
	(unspec:VM [(match_operand:VM 1 "register_operand" "v,0")
		    (match_operand:VM 2 "register_operand" "v,wo")
		    (match_operand:V16QI 3 "register_operand" "v,wo")]
		   UNSPEC_VPERM_UNS))]
  "TARGET_ALTIVEC"
  "@
   vperm %0,%1,%2,%3
   xxperm %x0,%x2,%x3"
  [(set_attr "type" "vecperm")
   (set_attr "length" "4")])

(define_expand "vec_permv16qi"
  [(set (match_operand:V16QI 0 "register_operand" "")
	(unspec:V16QI [(match_operand:V16QI 1 "register_operand" "")
		       (match_operand:V16QI 2 "register_operand" "")
		       (match_operand:V16QI 3 "register_operand" "")]
		      UNSPEC_VPERM))]
  "TARGET_ALTIVEC"
{
  if (!BYTES_BIG_ENDIAN) {
    altivec_expand_vec_perm_le (operands);
    DONE;
  }
})

(define_expand "vec_perm_constv16qi"
  [(match_operand:V16QI 0 "register_operand" "")
   (match_operand:V16QI 1 "register_operand" "")
   (match_operand:V16QI 2 "register_operand" "")
   (match_operand:V16QI 3 "" "")]
  "TARGET_ALTIVEC"
{
  if (altivec_expand_vec_perm_const (operands))
    DONE;
  else
    FAIL;
})

(define_insn "*altivec_vpermr_<mode>_internal"
  [(set (match_operand:VM 0 "register_operand" "=v,?wo")
	(unspec:VM [(match_operand:VM 1 "register_operand" "v,0")
		    (match_operand:VM 2 "register_operand" "v,wo")
		    (match_operand:V16QI 3 "register_operand" "v,wo")]
		   UNSPEC_VPERMR))]
  "TARGET_P9_VECTOR"
  "@
   vpermr %0,%1,%2,%3
   xxpermr %x0,%x2,%x3"
  [(set_attr "type" "vecperm")
   (set_attr "length" "4")])

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

(define_insn "*altivec_vrsqrtefp"
  [(set (match_operand:V4SF 0 "register_operand" "=v")
        (unspec:V4SF [(match_operand:V4SF 1 "register_operand" "v")]
		     UNSPEC_RSQRT))]
  "VECTOR_UNIT_ALTIVEC_P (V4SFmode)"
  "vrsqrtefp %0,%1"
  [(set_attr "type" "vecfloat")])

(define_insn "altivec_vrefp"
  [(set (match_operand:V4SF 0 "register_operand" "=v")
        (unspec:V4SF [(match_operand:V4SF 1 "register_operand" "v")]
		     UNSPEC_FRES))]
  "VECTOR_UNIT_ALTIVEC_P (V4SFmode)"
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
		  UNSPEC_VSLDOI))]
  "TARGET_ALTIVEC"
  "vsldoi %0,%1,%2,%3"
  [(set_attr "type" "vecperm")])

(define_insn "altivec_vupkhs<VU_char>"
  [(set (match_operand:VP 0 "register_operand" "=v")
	(unspec:VP [(match_operand:<VP_small> 1 "register_operand" "v")]
		     UNSPEC_VUNPACK_HI_SIGN))]
  "<VI_unit>"
{
  if (VECTOR_ELT_ORDER_BIG)
    return "vupkhs<VU_char> %0,%1";
  else
    return "vupkls<VU_char> %0,%1";
}
  [(set_attr "type" "vecperm")])

(define_insn "*altivec_vupkhs<VU_char>_direct"
  [(set (match_operand:VP 0 "register_operand" "=v")
	(unspec:VP [(match_operand:<VP_small> 1 "register_operand" "v")]
		     UNSPEC_VUNPACK_HI_SIGN_DIRECT))]
  "<VI_unit>"
  "vupkhs<VU_char> %0,%1"
  [(set_attr "type" "vecperm")])

(define_insn "altivec_vupkls<VU_char>"
  [(set (match_operand:VP 0 "register_operand" "=v")
	(unspec:VP [(match_operand:<VP_small> 1 "register_operand" "v")]
		     UNSPEC_VUNPACK_LO_SIGN))]
  "<VI_unit>"
{
  if (VECTOR_ELT_ORDER_BIG)
    return "vupkls<VU_char> %0,%1";
  else
    return "vupkhs<VU_char> %0,%1";
}
  [(set_attr "type" "vecperm")])

(define_insn "*altivec_vupkls<VU_char>_direct"
  [(set (match_operand:VP 0 "register_operand" "=v")
	(unspec:VP [(match_operand:<VP_small> 1 "register_operand" "v")]
		     UNSPEC_VUNPACK_LO_SIGN_DIRECT))]
  "<VI_unit>"
  "vupkls<VU_char> %0,%1"
  [(set_attr "type" "vecperm")])

(define_insn "altivec_vupkhpx"
  [(set (match_operand:V4SI 0 "register_operand" "=v")
	(unspec:V4SI [(match_operand:V8HI 1 "register_operand" "v")]
		     UNSPEC_VUPKHPX))]
  "TARGET_ALTIVEC"
{
  if (VECTOR_ELT_ORDER_BIG)
    return "vupkhpx %0,%1";
  else
    return "vupklpx %0,%1";
}
  [(set_attr "type" "vecperm")])

(define_insn "altivec_vupklpx"
  [(set (match_operand:V4SI 0 "register_operand" "=v")
	(unspec:V4SI [(match_operand:V8HI 1 "register_operand" "v")]
		     UNSPEC_VUPKLPX))]
  "TARGET_ALTIVEC"
{
  if (VECTOR_ELT_ORDER_BIG)
    return "vupklpx %0,%1";
  else
    return "vupkhpx %0,%1";
}
  [(set_attr "type" "vecperm")])

;; Compare vectors producing a vector result and a predicate, setting CR6 to
;; indicate a combined status
(define_insn "*altivec_vcmpequ<VI_char>_p"
  [(set (reg:CC 74)
	(unspec:CC [(eq:CC (match_operand:VI2 1 "register_operand" "v")
			   (match_operand:VI2 2 "register_operand" "v"))]
		   UNSPEC_PREDICATE))
   (set (match_operand:VI2 0 "register_operand" "=v")
	(eq:VI2 (match_dup 1)
		(match_dup 2)))]
  "<VI_unit>"
  "vcmpequ<VI_char>. %0,%1,%2"
  [(set_attr "type" "veccmp")])

(define_insn "*altivec_vcmpgts<VI_char>_p"
  [(set (reg:CC 74)
	(unspec:CC [(gt:CC (match_operand:VI2 1 "register_operand" "v")
			   (match_operand:VI2 2 "register_operand" "v"))]
		   UNSPEC_PREDICATE))
   (set (match_operand:VI2 0 "register_operand" "=v")
	(gt:VI2 (match_dup 1)
		(match_dup 2)))]
  "<VI_unit>"
  "vcmpgts<VI_char>. %0,%1,%2"
  [(set_attr "type" "veccmp")])

(define_insn "*altivec_vcmpgtu<VI_char>_p"
  [(set (reg:CC 74)
	(unspec:CC [(gtu:CC (match_operand:VI2 1 "register_operand" "v")
			    (match_operand:VI2 2 "register_operand" "v"))]
		   UNSPEC_PREDICATE))
   (set (match_operand:VI2 0 "register_operand" "=v")
	(gtu:VI2 (match_dup 1)
		 (match_dup 2)))]
  "<VI_unit>"
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

(define_expand "altivec_lvsl"
  [(use (match_operand:V16QI 0 "register_operand" ""))
   (use (match_operand:V16QI 1 "memory_operand" ""))]
  "TARGET_ALTIVEC"
{
  if (VECTOR_ELT_ORDER_BIG)
    emit_insn (gen_altivec_lvsl_direct (operands[0], operands[1]));
  else
    {
      int i;
      rtx mask, perm[16], constv, vperm;
      mask = gen_reg_rtx (V16QImode);
      emit_insn (gen_altivec_lvsl_direct (mask, operands[1]));
      for (i = 0; i < 16; ++i)
        perm[i] = GEN_INT (i);
      constv = gen_rtx_CONST_VECTOR (V16QImode, gen_rtvec_v (16, perm));
      constv = force_reg (V16QImode, constv);
      vperm = gen_rtx_UNSPEC (V16QImode, gen_rtvec (3, mask, mask, constv),
                              UNSPEC_VPERM);
      emit_insn (gen_rtx_SET (operands[0], vperm));
    }
  DONE;
})

(define_insn "altivec_lvsl_direct"
  [(set (match_operand:V16QI 0 "register_operand" "=v")
	(unspec:V16QI [(match_operand:V16QI 1 "memory_operand" "Z")]
		      UNSPEC_LVSL))]
  "TARGET_ALTIVEC"
  "lvsl %0,%y1"
  [(set_attr "type" "vecload")])

(define_expand "altivec_lvsr"
  [(use (match_operand:V16QI 0 "register_operand" ""))
   (use (match_operand:V16QI 1 "memory_operand" ""))]
  "TARGET_ALTIVEC"
{
  if (VECTOR_ELT_ORDER_BIG)
    emit_insn (gen_altivec_lvsr_direct (operands[0], operands[1]));
  else
    {
      int i;
      rtx mask, perm[16], constv, vperm;
      mask = gen_reg_rtx (V16QImode);
      emit_insn (gen_altivec_lvsr_direct (mask, operands[1]));
      for (i = 0; i < 16; ++i)
        perm[i] = GEN_INT (i);
      constv = gen_rtx_CONST_VECTOR (V16QImode, gen_rtvec_v (16, perm));
      constv = force_reg (V16QImode, constv);
      vperm = gen_rtx_UNSPEC (V16QImode, gen_rtvec (3, mask, mask, constv),
                              UNSPEC_VPERM);
      emit_insn (gen_rtx_SET (operands[0], vperm));
    }
  DONE;
})

(define_insn "altivec_lvsr_direct"
  [(set (match_operand:V16QI 0 "register_operand" "=v")
	(unspec:V16QI [(match_operand:V16QI 1 "memory_operand" "Z")]
		      UNSPEC_LVSR))]
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
  emit_insn (gen_rtx_SET (temp, gen_rtx_NEG (GET_MODE (addr), addr)));
  emit_insn (gen_altivec_lvsr (operands[0], 
			       replace_equiv_address (operands[1], temp)));
  DONE;
}")

;; Parallel some of the LVE* and STV*'s with unspecs because some have
;; identical rtl but different instructions-- and gcc gets confused.

(define_expand "altivec_lve<VI_char>x"
  [(parallel
    [(set (match_operand:VI 0 "register_operand" "=v")
	  (match_operand:VI 1 "memory_operand" "Z"))
     (unspec [(const_int 0)] UNSPEC_LVE)])]
  "TARGET_ALTIVEC"
{
  if (!BYTES_BIG_ENDIAN && VECTOR_ELT_ORDER_BIG)
    {
      altivec_expand_lvx_be (operands[0], operands[1], <MODE>mode, UNSPEC_LVE);
      DONE;
    }
})

(define_insn "*altivec_lve<VI_char>x_internal"
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

(define_expand "altivec_lvxl_<mode>"
  [(parallel
    [(set (match_operand:VM2 0 "register_operand" "=v")
	  (match_operand:VM2 1 "memory_operand" "Z"))
     (unspec [(const_int 0)] UNSPEC_SET_VSCR)])]
  "TARGET_ALTIVEC"
{
  if (!BYTES_BIG_ENDIAN && VECTOR_ELT_ORDER_BIG)
    {
      altivec_expand_lvx_be (operands[0], operands[1], <MODE>mode, UNSPEC_SET_VSCR);
      DONE;
    }
})

(define_insn "*altivec_lvxl_<mode>_internal"
  [(parallel
    [(set (match_operand:VM2 0 "register_operand" "=v")
	  (match_operand:VM2 1 "memory_operand" "Z"))
     (unspec [(const_int 0)] UNSPEC_SET_VSCR)])]
  "TARGET_ALTIVEC"
  "lvxl %0,%y1"
  [(set_attr "type" "vecload")])

; This version of lvx is used only in cases where we need to force an lvx
; over any other load, and we don't care about losing CSE opportunities.
; Its primary use is for prologue register saves.
(define_insn "altivec_lvx_<mode>_internal"
  [(parallel
    [(set (match_operand:VM2 0 "register_operand" "=v")
	  (match_operand:VM2 1 "memory_operand" "Z"))
     (unspec [(const_int 0)] UNSPEC_LVX)])]
  "TARGET_ALTIVEC"
  "lvx %0,%y1"
  [(set_attr "type" "vecload")])

; The next two patterns embody what lvx should usually look like.
(define_insn "altivec_lvx_<mode>_2op"
  [(set (match_operand:VM2 0 "register_operand" "=v")
        (mem:VM2 (and:DI (plus:DI (match_operand:DI 1 "register_operand" "b")
                                  (match_operand:DI 2 "register_operand" "r"))
		         (const_int -16))))]
  "TARGET_ALTIVEC && TARGET_64BIT"
  "lvx %0,%1,%2"
  [(set_attr "type" "vecload")])

(define_insn "altivec_lvx_<mode>_1op"
  [(set (match_operand:VM2 0 "register_operand" "=v")
        (mem:VM2 (and:DI (match_operand:DI 1 "register_operand" "r")
			 (const_int -16))))]
  "TARGET_ALTIVEC && TARGET_64BIT"
  "lvx %0,0,%1"
  [(set_attr "type" "vecload")])

; 32-bit versions of the above.
(define_insn "altivec_lvx_<mode>_2op_si"
  [(set (match_operand:VM2 0 "register_operand" "=v")
        (mem:VM2 (and:SI (plus:SI (match_operand:SI 1 "register_operand" "b")
                                  (match_operand:SI 2 "register_operand" "r"))
		         (const_int -16))))]
  "TARGET_ALTIVEC && TARGET_32BIT"
  "lvx %0,%1,%2"
  [(set_attr "type" "vecload")])

(define_insn "altivec_lvx_<mode>_1op_si"
  [(set (match_operand:VM2 0 "register_operand" "=v")
        (mem:VM2 (and:SI (match_operand:SI 1 "register_operand" "r")
			 (const_int -16))))]
  "TARGET_ALTIVEC && TARGET_32BIT"
  "lvx %0,0,%1"
  [(set_attr "type" "vecload")])

; This version of stvx is used only in cases where we need to force an stvx
; over any other store, and we don't care about losing CSE opportunities.
; Its primary use is for epilogue register restores.
(define_insn "altivec_stvx_<mode>_internal"
  [(parallel
    [(set (match_operand:VM2 0 "memory_operand" "=Z")
	  (match_operand:VM2 1 "register_operand" "v"))
     (unspec [(const_int 0)] UNSPEC_STVX)])]
  "TARGET_ALTIVEC"
  "stvx %1,%y0"
  [(set_attr "type" "vecstore")])

; The next two patterns embody what stvx should usually look like.
(define_insn "altivec_stvx_<mode>_2op"
  [(set (mem:VM2 (and:DI (plus:DI (match_operand:DI 1 "register_operand" "b")
  	                          (match_operand:DI 2 "register_operand" "r"))
	                 (const_int -16)))
        (match_operand:VM2 0 "register_operand" "v"))]
  "TARGET_ALTIVEC && TARGET_64BIT"
  "stvx %0,%1,%2"
  [(set_attr "type" "vecstore")])

(define_insn "altivec_stvx_<mode>_1op"
  [(set (mem:VM2 (and:DI (match_operand:DI 1 "register_operand" "r")
	                 (const_int -16)))
        (match_operand:VM2 0 "register_operand" "v"))]
  "TARGET_ALTIVEC && TARGET_64BIT"
  "stvx %0,0,%1"
  [(set_attr "type" "vecstore")])

; 32-bit versions of the above.
(define_insn "altivec_stvx_<mode>_2op_si"
  [(set (mem:VM2 (and:SI (plus:SI (match_operand:SI 1 "register_operand" "b")
  	                          (match_operand:SI 2 "register_operand" "r"))
	                 (const_int -16)))
        (match_operand:VM2 0 "register_operand" "v"))]
  "TARGET_ALTIVEC && TARGET_32BIT"
  "stvx %0,%1,%2"
  [(set_attr "type" "vecstore")])

(define_insn "altivec_stvx_<mode>_1op_si"
  [(set (mem:VM2 (and:SI (match_operand:SI 1 "register_operand" "r")
	                 (const_int -16)))
        (match_operand:VM2 0 "register_operand" "v"))]
  "TARGET_ALTIVEC && TARGET_32BIT"
  "stvx %0,0,%1"
  [(set_attr "type" "vecstore")])

(define_expand "altivec_stvxl_<mode>"
  [(parallel
    [(set (match_operand:VM2 0 "memory_operand" "=Z")
	  (match_operand:VM2 1 "register_operand" "v"))
     (unspec [(const_int 0)] UNSPEC_STVXL)])]
  "TARGET_ALTIVEC"
{
  if (!BYTES_BIG_ENDIAN && VECTOR_ELT_ORDER_BIG)
    {
      altivec_expand_stvx_be (operands[0], operands[1], <MODE>mode, UNSPEC_STVXL);
      DONE;
    }
})

(define_insn "*altivec_stvxl_<mode>_internal"
  [(parallel
    [(set (match_operand:VM2 0 "memory_operand" "=Z")
	  (match_operand:VM2 1 "register_operand" "v"))
     (unspec [(const_int 0)] UNSPEC_STVXL)])]
  "TARGET_ALTIVEC"
  "stvxl %1,%y0"
  [(set_attr "type" "vecstore")])

(define_expand "altivec_stve<VI_char>x"
  [(set (match_operand:<VI_scalar> 0 "memory_operand" "=Z")
	(unspec:<VI_scalar> [(match_operand:VI 1 "register_operand" "v")] UNSPEC_STVE))]
  "TARGET_ALTIVEC"
{
  if (!BYTES_BIG_ENDIAN && VECTOR_ELT_ORDER_BIG)
    {
      altivec_expand_stvex_be (operands[0], operands[1], <MODE>mode, UNSPEC_STVE);
      DONE;
    }
})

(define_insn "*altivec_stve<VI_char>x_internal"
  [(set (match_operand:<VI_scalar> 0 "memory_operand" "=Z")
	(unspec:<VI_scalar> [(match_operand:VI 1 "register_operand" "v")] UNSPEC_STVE))]
  "TARGET_ALTIVEC"
  "stve<VI_char>x %1,%y0"
  [(set_attr "type" "vecstore")])

(define_insn "*altivec_stvesfx"
  [(set (match_operand:SF 0 "memory_operand" "=Z")
	(unspec:SF [(match_operand:V4SF 1 "register_operand" "v")] UNSPEC_STVE))]
  "TARGET_ALTIVEC"
  "stvewx %1,%y0"
  [(set_attr "type" "vecstore")])

;; Generate
;;    xxlxor/vxor SCRATCH0,SCRATCH0,SCRATCH0
;;    vsubu?m SCRATCH2,SCRATCH1,%1
;;    vmaxs? %0,%1,SCRATCH2"
(define_expand "abs<mode>2"
  [(set (match_dup 2) (match_dup 3))
   (set (match_dup 4)
        (minus:VI2 (match_dup 2)
		   (match_operand:VI2 1 "register_operand" "v")))
   (set (match_operand:VI2 0 "register_operand" "=v")
        (smax:VI2 (match_dup 1) (match_dup 4)))]
  "<VI_unit>"
{
  int i, n_elt = GET_MODE_NUNITS (<MODE>mode);
  rtvec v = rtvec_alloc (n_elt);

  /* Create an all 0 constant.  */
  for (i = 0; i < n_elt; ++i)
    RTVEC_ELT (v, i) = const0_rtx;

  operands[2] = gen_reg_rtx (<MODE>mode);
  operands[3] = gen_rtx_CONST_VECTOR (<MODE>mode, v);
  operands[4] = gen_reg_rtx (<MODE>mode);
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

(define_expand "reduc_plus_scal_<mode>"
  [(set (match_operand:<VI_scalar> 0 "register_operand" "=v")
        (unspec:VIshort [(match_operand:VIshort 1 "register_operand" "v")]
			UNSPEC_REDUC_PLUS))]
  "TARGET_ALTIVEC"
{
  rtx vzero = gen_reg_rtx (V4SImode);
  rtx vtmp1 = gen_reg_rtx (V4SImode);
  rtx vtmp2 = gen_reg_rtx (<MODE>mode);
  rtx dest = gen_lowpart (V4SImode, vtmp2);
  int elt = BYTES_BIG_ENDIAN ? GET_MODE_NUNITS (<MODE>mode) - 1 : 0;

  emit_insn (gen_altivec_vspltisw (vzero, const0_rtx));
  emit_insn (gen_altivec_vsum4s<VI_char>s (vtmp1, operands[1], vzero));
  emit_insn (gen_altivec_vsumsws_direct (dest, vtmp1, vzero));
  rs6000_expand_vector_extract (operands[0], vtmp2, elt);
  DONE;
})

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

(define_expand "vec_unpacks_hi_<VP_small_lc>"
  [(set (match_operand:VP 0 "register_operand" "=v")
        (unspec:VP [(match_operand:<VP_small> 1 "register_operand" "v")]
		   UNSPEC_VUNPACK_HI_SIGN_DIRECT))]
  "<VI_unit>"
  "")

(define_expand "vec_unpacks_lo_<VP_small_lc>"
  [(set (match_operand:VP 0 "register_operand" "=v")
        (unspec:VP [(match_operand:<VP_small> 1 "register_operand" "v")]
		   UNSPEC_VUNPACK_LO_SIGN_DIRECT))]
  "<VI_unit>"
  "")

(define_insn "vperm_v8hiv4si"
  [(set (match_operand:V4SI 0 "register_operand" "=v,?wo")
        (unspec:V4SI [(match_operand:V8HI 1 "register_operand" "v,0")
		      (match_operand:V4SI 2 "register_operand" "v,wo")
		      (match_operand:V16QI 3 "register_operand" "v,wo")]
                  UNSPEC_VPERMSI))]
  "TARGET_ALTIVEC"
  "@
   vperm %0,%1,%2,%3
   xxperm %x0,%x2,%x3"
  [(set_attr "type" "vecperm")
   (set_attr "length" "4")])

(define_insn "vperm_v16qiv8hi"
  [(set (match_operand:V8HI 0 "register_operand" "=v,?wo")
        (unspec:V8HI [(match_operand:V16QI 1 "register_operand" "v,0")
		      (match_operand:V8HI 2 "register_operand" "v,wo")
		      (match_operand:V16QI 3 "register_operand" "v,wo")]
                  UNSPEC_VPERMHI))]
  "TARGET_ALTIVEC"
  "@
   vperm %0,%1,%2,%3
   xxperm %x0,%x2,%x3"
  [(set_attr "type" "vecperm")
   (set_attr "length" "4")])


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
  bool be = BYTES_BIG_ENDIAN;
   
  emit_insn (gen_altivec_vspltish (vzero, const0_rtx));
   
  RTVEC_ELT (v,  0) = gen_rtx_CONST_INT (QImode, be ? 16 :  7);
  RTVEC_ELT (v,  1) = gen_rtx_CONST_INT (QImode, be ?  0 : 16);
  RTVEC_ELT (v,  2) = gen_rtx_CONST_INT (QImode, be ? 16 :  6);
  RTVEC_ELT (v,  3) = gen_rtx_CONST_INT (QImode, be ?  1 : 16);
  RTVEC_ELT (v,  4) = gen_rtx_CONST_INT (QImode, be ? 16 :  5);
  RTVEC_ELT (v,  5) = gen_rtx_CONST_INT (QImode, be ?  2 : 16);
  RTVEC_ELT (v,  6) = gen_rtx_CONST_INT (QImode, be ? 16 :  4);
  RTVEC_ELT (v,  7) = gen_rtx_CONST_INT (QImode, be ?  3 : 16);
  RTVEC_ELT (v,  8) = gen_rtx_CONST_INT (QImode, be ? 16 :  3);
  RTVEC_ELT (v,  9) = gen_rtx_CONST_INT (QImode, be ?  4 : 16);
  RTVEC_ELT (v, 10) = gen_rtx_CONST_INT (QImode, be ? 16 :  2);
  RTVEC_ELT (v, 11) = gen_rtx_CONST_INT (QImode, be ?  5 : 16);
  RTVEC_ELT (v, 12) = gen_rtx_CONST_INT (QImode, be ? 16 :  1);
  RTVEC_ELT (v, 13) = gen_rtx_CONST_INT (QImode, be ?  6 : 16);
  RTVEC_ELT (v, 14) = gen_rtx_CONST_INT (QImode, be ? 16 :  0);
  RTVEC_ELT (v, 15) = gen_rtx_CONST_INT (QImode, be ?  7 : 16);

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
  bool be = BYTES_BIG_ENDIAN;

  emit_insn (gen_altivec_vspltisw (vzero, const0_rtx));
 
  RTVEC_ELT (v,  0) = gen_rtx_CONST_INT (QImode, be ? 16 :  7);
  RTVEC_ELT (v,  1) = gen_rtx_CONST_INT (QImode, be ? 17 :  6);
  RTVEC_ELT (v,  2) = gen_rtx_CONST_INT (QImode, be ?  0 : 17);
  RTVEC_ELT (v,  3) = gen_rtx_CONST_INT (QImode, be ?  1 : 16);
  RTVEC_ELT (v,  4) = gen_rtx_CONST_INT (QImode, be ? 16 :  5);
  RTVEC_ELT (v,  5) = gen_rtx_CONST_INT (QImode, be ? 17 :  4);
  RTVEC_ELT (v,  6) = gen_rtx_CONST_INT (QImode, be ?  2 : 17);
  RTVEC_ELT (v,  7) = gen_rtx_CONST_INT (QImode, be ?  3 : 16);
  RTVEC_ELT (v,  8) = gen_rtx_CONST_INT (QImode, be ? 16 :  3);
  RTVEC_ELT (v,  9) = gen_rtx_CONST_INT (QImode, be ? 17 :  2);
  RTVEC_ELT (v, 10) = gen_rtx_CONST_INT (QImode, be ?  4 : 17);
  RTVEC_ELT (v, 11) = gen_rtx_CONST_INT (QImode, be ?  5 : 16);
  RTVEC_ELT (v, 12) = gen_rtx_CONST_INT (QImode, be ? 16 :  1);
  RTVEC_ELT (v, 13) = gen_rtx_CONST_INT (QImode, be ? 17 :  0);
  RTVEC_ELT (v, 14) = gen_rtx_CONST_INT (QImode, be ?  6 : 17);
  RTVEC_ELT (v, 15) = gen_rtx_CONST_INT (QImode, be ?  7 : 16);

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
  bool be = BYTES_BIG_ENDIAN;

  emit_insn (gen_altivec_vspltish (vzero, const0_rtx));

  RTVEC_ELT (v,  0) = gen_rtx_CONST_INT (QImode, be ? 16 : 15);
  RTVEC_ELT (v,  1) = gen_rtx_CONST_INT (QImode, be ?  8 : 16);
  RTVEC_ELT (v,  2) = gen_rtx_CONST_INT (QImode, be ? 16 : 14);
  RTVEC_ELT (v,  3) = gen_rtx_CONST_INT (QImode, be ?  9 : 16);
  RTVEC_ELT (v,  4) = gen_rtx_CONST_INT (QImode, be ? 16 : 13);
  RTVEC_ELT (v,  5) = gen_rtx_CONST_INT (QImode, be ? 10 : 16);
  RTVEC_ELT (v,  6) = gen_rtx_CONST_INT (QImode, be ? 16 : 12);
  RTVEC_ELT (v,  7) = gen_rtx_CONST_INT (QImode, be ? 11 : 16);
  RTVEC_ELT (v,  8) = gen_rtx_CONST_INT (QImode, be ? 16 : 11);
  RTVEC_ELT (v,  9) = gen_rtx_CONST_INT (QImode, be ? 12 : 16);
  RTVEC_ELT (v, 10) = gen_rtx_CONST_INT (QImode, be ? 16 : 10);
  RTVEC_ELT (v, 11) = gen_rtx_CONST_INT (QImode, be ? 13 : 16);
  RTVEC_ELT (v, 12) = gen_rtx_CONST_INT (QImode, be ? 16 :  9);
  RTVEC_ELT (v, 13) = gen_rtx_CONST_INT (QImode, be ? 14 : 16);
  RTVEC_ELT (v, 14) = gen_rtx_CONST_INT (QImode, be ? 16 :  8);
  RTVEC_ELT (v, 15) = gen_rtx_CONST_INT (QImode, be ? 15 : 16);

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
  bool be = BYTES_BIG_ENDIAN;

  emit_insn (gen_altivec_vspltisw (vzero, const0_rtx));
 
  RTVEC_ELT (v,  0) = gen_rtx_CONST_INT (QImode, be ? 16 : 15);
  RTVEC_ELT (v,  1) = gen_rtx_CONST_INT (QImode, be ? 17 : 14);
  RTVEC_ELT (v,  2) = gen_rtx_CONST_INT (QImode, be ?  8 : 17);
  RTVEC_ELT (v,  3) = gen_rtx_CONST_INT (QImode, be ?  9 : 16);
  RTVEC_ELT (v,  4) = gen_rtx_CONST_INT (QImode, be ? 16 : 13);
  RTVEC_ELT (v,  5) = gen_rtx_CONST_INT (QImode, be ? 17 : 12);
  RTVEC_ELT (v,  6) = gen_rtx_CONST_INT (QImode, be ? 10 : 17);
  RTVEC_ELT (v,  7) = gen_rtx_CONST_INT (QImode, be ? 11 : 16);
  RTVEC_ELT (v,  8) = gen_rtx_CONST_INT (QImode, be ? 16 : 11);
  RTVEC_ELT (v,  9) = gen_rtx_CONST_INT (QImode, be ? 17 : 10);
  RTVEC_ELT (v, 10) = gen_rtx_CONST_INT (QImode, be ? 12 : 17);
  RTVEC_ELT (v, 11) = gen_rtx_CONST_INT (QImode, be ? 13 : 16);
  RTVEC_ELT (v, 12) = gen_rtx_CONST_INT (QImode, be ? 16 :  9);
  RTVEC_ELT (v, 13) = gen_rtx_CONST_INT (QImode, be ? 17 :  8);
  RTVEC_ELT (v, 14) = gen_rtx_CONST_INT (QImode, be ? 14 : 17);
  RTVEC_ELT (v, 15) = gen_rtx_CONST_INT (QImode, be ? 15 : 16);

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
  
  if (BYTES_BIG_ENDIAN)
    {
      emit_insn (gen_altivec_vmuleub (ve, operands[1], operands[2]));
      emit_insn (gen_altivec_vmuloub (vo, operands[1], operands[2]));
      emit_insn (gen_altivec_vmrghh_direct (operands[0], ve, vo));
    }
  else
    {
      emit_insn (gen_altivec_vmuloub (ve, operands[1], operands[2]));
      emit_insn (gen_altivec_vmuleub (vo, operands[1], operands[2]));
      emit_insn (gen_altivec_vmrghh_direct (operands[0], vo, ve));
    }
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
  
  if (BYTES_BIG_ENDIAN)
    {
      emit_insn (gen_altivec_vmuleub (ve, operands[1], operands[2]));
      emit_insn (gen_altivec_vmuloub (vo, operands[1], operands[2]));
      emit_insn (gen_altivec_vmrglh_direct (operands[0], ve, vo));
    }
  else
    {
      emit_insn (gen_altivec_vmuloub (ve, operands[1], operands[2]));
      emit_insn (gen_altivec_vmuleub (vo, operands[1], operands[2]));
      emit_insn (gen_altivec_vmrglh_direct (operands[0], vo, ve));
    }
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
  
  if (BYTES_BIG_ENDIAN)
    {
      emit_insn (gen_altivec_vmulesb (ve, operands[1], operands[2]));
      emit_insn (gen_altivec_vmulosb (vo, operands[1], operands[2]));
      emit_insn (gen_altivec_vmrghh_direct (operands[0], ve, vo));
    }
  else
    {
      emit_insn (gen_altivec_vmulosb (ve, operands[1], operands[2]));
      emit_insn (gen_altivec_vmulesb (vo, operands[1], operands[2]));
      emit_insn (gen_altivec_vmrghh_direct (operands[0], vo, ve));
    }
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
  
  if (BYTES_BIG_ENDIAN)
    {
      emit_insn (gen_altivec_vmulesb (ve, operands[1], operands[2]));
      emit_insn (gen_altivec_vmulosb (vo, operands[1], operands[2]));
      emit_insn (gen_altivec_vmrglh_direct (operands[0], ve, vo));
    }
  else
    {
      emit_insn (gen_altivec_vmulosb (ve, operands[1], operands[2]));
      emit_insn (gen_altivec_vmulesb (vo, operands[1], operands[2]));
      emit_insn (gen_altivec_vmrglh_direct (operands[0], vo, ve));
    }
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
  
  if (BYTES_BIG_ENDIAN)
    {
      emit_insn (gen_altivec_vmuleuh (ve, operands[1], operands[2]));
      emit_insn (gen_altivec_vmulouh (vo, operands[1], operands[2]));
      emit_insn (gen_altivec_vmrghw_direct (operands[0], ve, vo));
    }
  else
    {
      emit_insn (gen_altivec_vmulouh (ve, operands[1], operands[2]));
      emit_insn (gen_altivec_vmuleuh (vo, operands[1], operands[2]));
      emit_insn (gen_altivec_vmrghw_direct (operands[0], vo, ve));
    }
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
  
  if (BYTES_BIG_ENDIAN)
    {
      emit_insn (gen_altivec_vmuleuh (ve, operands[1], operands[2]));
      emit_insn (gen_altivec_vmulouh (vo, operands[1], operands[2]));
      emit_insn (gen_altivec_vmrglw_direct (operands[0], ve, vo));
    }
  else
    {
      emit_insn (gen_altivec_vmulouh (ve, operands[1], operands[2]));
      emit_insn (gen_altivec_vmuleuh (vo, operands[1], operands[2]));
      emit_insn (gen_altivec_vmrglw_direct (operands[0], vo, ve));
    }
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
  
  if (BYTES_BIG_ENDIAN)
    {
      emit_insn (gen_altivec_vmulesh (ve, operands[1], operands[2]));
      emit_insn (gen_altivec_vmulosh (vo, operands[1], operands[2]));
      emit_insn (gen_altivec_vmrghw_direct (operands[0], ve, vo));
    }
  else
    {
      emit_insn (gen_altivec_vmulosh (ve, operands[1], operands[2]));
      emit_insn (gen_altivec_vmulesh (vo, operands[1], operands[2]));
      emit_insn (gen_altivec_vmrghw_direct (operands[0], vo, ve));
    }
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
  
  if (BYTES_BIG_ENDIAN)
    {
      emit_insn (gen_altivec_vmulesh (ve, operands[1], operands[2]));
      emit_insn (gen_altivec_vmulosh (vo, operands[1], operands[2]));
      emit_insn (gen_altivec_vmrglw_direct (operands[0], ve, vo));
    }
  else
    {
      emit_insn (gen_altivec_vmulosh (ve, operands[1], operands[2]));
      emit_insn (gen_altivec_vmulesh (vo, operands[1], operands[2]));
      emit_insn (gen_altivec_vmrglw_direct (operands[0], vo, ve));
    }
  DONE;
}")

(define_expand "vec_pack_trunc_<mode>"
  [(set (match_operand:<VP_small> 0 "register_operand" "=v")
        (unspec:<VP_small> [(match_operand:VP 1 "register_operand" "v")
			    (match_operand:VP 2 "register_operand" "v")]
                      UNSPEC_VPACK_UNS_UNS_MOD))]
  "<VI_unit>"
  "")

(define_expand "mulv16qi3"
  [(set (match_operand:V16QI 0 "register_operand" "=v")
        (mult:V16QI (match_operand:V16QI 1 "register_operand" "v")
                    (match_operand:V16QI 2 "register_operand" "v")))]
  "TARGET_ALTIVEC"
  "
{
  rtx even = gen_reg_rtx (V8HImode);
  rtx odd = gen_reg_rtx (V8HImode);
  rtx mask = gen_reg_rtx (V16QImode);
  rtvec v = rtvec_alloc (16);
  int i;

  for (i = 0; i < 8; ++i) {
    RTVEC_ELT (v, 2 * i)
     = gen_rtx_CONST_INT (QImode, BYTES_BIG_ENDIAN ? 2 * i + 1 : 31 - 2 * i);
    RTVEC_ELT (v, 2 * i + 1)
     = gen_rtx_CONST_INT (QImode, BYTES_BIG_ENDIAN ? 2 * i + 17 : 15 - 2 * i);
  }

  emit_insn (gen_vec_initv16qi (mask, gen_rtx_PARALLEL (V16QImode, v)));
  emit_insn (gen_altivec_vmulesb (even, operands[1], operands[2]));
  emit_insn (gen_altivec_vmulosb (odd, operands[1], operands[2]));
  emit_insn (gen_altivec_vperm_v8hiv16qi (operands[0], even, odd, mask));
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
        (unspec:V16QI [(match_operand:BLK 1 "memory_operand" "Z")]
		      UNSPEC_LVLX))]
  "TARGET_ALTIVEC && rs6000_cpu == PROCESSOR_CELL"
  "lvlx %0,%y1"
  [(set_attr "type" "vecload")])

(define_insn "altivec_lvlxl"
  [(set (match_operand:V16QI 0 "register_operand" "=v")
        (unspec:V16QI [(match_operand:BLK 1 "memory_operand" "Z")]
		      UNSPEC_LVLXL))]
  "TARGET_ALTIVEC && rs6000_cpu == PROCESSOR_CELL"
  "lvlxl %0,%y1"
  [(set_attr "type" "vecload")])

(define_insn "altivec_lvrx"
  [(set (match_operand:V16QI 0 "register_operand" "=v")
        (unspec:V16QI [(match_operand:BLK 1 "memory_operand" "Z")]
		      UNSPEC_LVRX))]
  "TARGET_ALTIVEC && rs6000_cpu == PROCESSOR_CELL"
  "lvrx %0,%y1"
  [(set_attr "type" "vecload")])

(define_insn "altivec_lvrxl"
  [(set (match_operand:V16QI 0 "register_operand" "=v")
        (unspec:V16QI [(match_operand:BLK 1 "memory_operand" "Z")]
		      UNSPEC_LVRXL))]
  "TARGET_ALTIVEC && rs6000_cpu == PROCESSOR_CELL"
  "lvrxl %0,%y1"
  [(set_attr "type" "vecload")])

(define_insn "altivec_stvlx"
  [(parallel
    [(set (match_operand:V16QI 0 "memory_operand" "=Z")
	  (match_operand:V16QI 1 "register_operand" "v"))
     (unspec [(const_int 0)] UNSPEC_STVLX)])]
  "TARGET_ALTIVEC && rs6000_cpu == PROCESSOR_CELL"
  "stvlx %1,%y0"
  [(set_attr "type" "vecstore")])

(define_insn "altivec_stvlxl"
  [(parallel
    [(set (match_operand:V16QI 0 "memory_operand" "=Z")
	  (match_operand:V16QI 1 "register_operand" "v"))
     (unspec [(const_int 0)] UNSPEC_STVLXL)])]
  "TARGET_ALTIVEC && rs6000_cpu == PROCESSOR_CELL"
  "stvlxl %1,%y0"
  [(set_attr "type" "vecstore")])

(define_insn "altivec_stvrx"
  [(parallel
    [(set (match_operand:V16QI 0 "memory_operand" "=Z")
	  (match_operand:V16QI 1 "register_operand" "v"))
     (unspec [(const_int 0)] UNSPEC_STVRX)])]
  "TARGET_ALTIVEC && rs6000_cpu == PROCESSOR_CELL"
  "stvrx %1,%y0"
  [(set_attr "type" "vecstore")])

(define_insn "altivec_stvrxl"
  [(parallel
    [(set (match_operand:V16QI 0 "memory_operand" "=Z")
	  (match_operand:V16QI 1 "register_operand" "v"))
     (unspec [(const_int 0)] UNSPEC_STVRXL)])]
  "TARGET_ALTIVEC && rs6000_cpu == PROCESSOR_CELL"
  "stvrxl %1,%y0"
  [(set_attr "type" "vecstore")])

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


;; Power8/power9 vector instructions encoded as Altivec instructions

;; Vector count leading zeros
(define_insn "*p8v_clz<mode>2"
  [(set (match_operand:VI2 0 "register_operand" "=v")
	(clz:VI2 (match_operand:VI2 1 "register_operand" "v")))]
  "TARGET_P8_VECTOR"
  "vclz<wd> %0,%1"
  [(set_attr "length" "4")
   (set_attr "type" "vecsimple")])

;; Vector count trailing zeros
(define_insn "*p9v_ctz<mode>2"
  [(set (match_operand:VI2 0 "register_operand" "=v")
	(ctz:VI2 (match_operand:VI2 1 "register_operand" "v")))]
  "TARGET_P9_VECTOR"
  "vctz<wd> %0,%1"
  [(set_attr "length" "4")
   (set_attr "type" "vecsimple")])

;; Vector population count
(define_insn "*p8v_popcount<mode>2"
  [(set (match_operand:VI2 0 "register_operand" "=v")
        (popcount:VI2 (match_operand:VI2 1 "register_operand" "v")))]
  "TARGET_P8_VECTOR"
  "vpopcnt<wd> %0,%1"
  [(set_attr "length" "4")
   (set_attr "type" "vecsimple")])

;; Vector parity
(define_insn "*p9v_parity<mode>2"
  [(set (match_operand:VParity 0 "register_operand" "=v")
        (parity:VParity (match_operand:VParity 1 "register_operand" "v")))]
  "TARGET_P9_VECTOR"
  "vprtyb<wd> %0,%1"
  [(set_attr "length" "4")
   (set_attr "type" "vecsimple")])

;; Vector Gather Bits by Bytes by Doubleword
(define_insn "p8v_vgbbd"
  [(set (match_operand:V16QI 0 "register_operand" "=v")
	(unspec:V16QI [(match_operand:V16QI 1 "register_operand" "v")]
		      UNSPEC_VGBBD))]
  "TARGET_P8_VECTOR"
  "vgbbd %0,%1"
  [(set_attr "length" "4")
   (set_attr "type" "vecsimple")])


;; 128-bit binary integer arithmetic
;; We have a special container type (V1TImode) to allow operations using the
;; ISA 2.07 128-bit binary support to target the VMX/altivec registers without
;; having to worry about the register allocator deciding GPRs are better.

(define_insn "altivec_vadduqm"
  [(set (match_operand:V1TI 0 "register_operand" "=v")
	(plus:V1TI (match_operand:V1TI 1 "register_operand" "v")
		   (match_operand:V1TI 2 "register_operand" "v")))]
  "TARGET_VADDUQM"
  "vadduqm %0,%1,%2"
  [(set_attr "length" "4")
   (set_attr "type" "vecsimple")])

(define_insn "altivec_vaddcuq"
  [(set (match_operand:V1TI 0 "register_operand" "=v")
	(unspec:V1TI [(match_operand:V1TI 1 "register_operand" "v")
		      (match_operand:V1TI 2 "register_operand" "v")]
		     UNSPEC_VADDCUQ))]
  "TARGET_VADDUQM"
  "vaddcuq %0,%1,%2"
  [(set_attr "length" "4")
   (set_attr "type" "vecsimple")])

(define_insn "altivec_vsubuqm"
  [(set (match_operand:V1TI 0 "register_operand" "=v")
	(minus:V1TI (match_operand:V1TI 1 "register_operand" "v")
		    (match_operand:V1TI 2 "register_operand" "v")))]
  "TARGET_VADDUQM"
  "vsubuqm %0,%1,%2"
  [(set_attr "length" "4")
   (set_attr "type" "vecsimple")])

(define_insn "altivec_vsubcuq"
  [(set (match_operand:V1TI 0 "register_operand" "=v")
	(unspec:V1TI [(match_operand:V1TI 1 "register_operand" "v")
		      (match_operand:V1TI 2 "register_operand" "v")]
		     UNSPEC_VSUBCUQ))]
  "TARGET_VADDUQM"
  "vsubcuq %0,%1,%2"
  [(set_attr "length" "4")
   (set_attr "type" "vecsimple")])

(define_insn "altivec_vaddeuqm"
  [(set (match_operand:V1TI 0 "register_operand" "=v")
	(unspec:V1TI [(match_operand:V1TI 1 "register_operand" "v")
		      (match_operand:V1TI 2 "register_operand" "v")
		      (match_operand:V1TI 3 "register_operand" "v")]
		     UNSPEC_VADDEUQM))]
  "TARGET_VADDUQM"
  "vaddeuqm %0,%1,%2,%3"
  [(set_attr "length" "4")
   (set_attr "type" "vecsimple")])

(define_insn "altivec_vaddecuq"
  [(set (match_operand:V1TI 0 "register_operand" "=v")
	(unspec:V1TI [(match_operand:V1TI 1 "register_operand" "v")
		      (match_operand:V1TI 2 "register_operand" "v")
		      (match_operand:V1TI 3 "register_operand" "v")]
		     UNSPEC_VADDECUQ))]
  "TARGET_VADDUQM"
  "vaddecuq %0,%1,%2,%3"
  [(set_attr "length" "4")
   (set_attr "type" "vecsimple")])

(define_insn "altivec_vsubeuqm"
  [(set (match_operand:V1TI 0 "register_operand" "=v")
	(unspec:V1TI [(match_operand:V1TI 1 "register_operand" "v")
		      (match_operand:V1TI 2 "register_operand" "v")
		      (match_operand:V1TI 3 "register_operand" "v")]
		   UNSPEC_VSUBEUQM))]
  "TARGET_VADDUQM"
  "vsubeuqm %0,%1,%2,%3"
  [(set_attr "length" "4")
   (set_attr "type" "vecsimple")])

(define_insn "altivec_vsubecuq"
  [(set (match_operand:V1TI 0 "register_operand" "=v")
	(unspec:V1TI [(match_operand:V1TI 1 "register_operand" "v")
		      (match_operand:V1TI 2 "register_operand" "v")
		      (match_operand:V1TI 3 "register_operand" "v")]
		     UNSPEC_VSUBECUQ))]
  "TARGET_VADDUQM"
  "vsubecuq %0,%1,%2,%3"
  [(set_attr "length" "4")
   (set_attr "type" "vecsimple")])

;; We use V2DI as the output type to simplify converting the permute
;; bits into an integer
(define_insn "altivec_vbpermq"
  [(set (match_operand:V2DI 0 "register_operand" "=v")
	(unspec:V2DI [(match_operand:V16QI 1 "register_operand" "v")
		      (match_operand:V16QI 2 "register_operand" "v")]
		     UNSPEC_VBPERMQ))]
  "TARGET_P8_VECTOR"
  "vbpermq %0,%1,%2"
  [(set_attr "length" "4")
   (set_attr "type" "vecsimple")])

;; Decimal Integer operations
(define_int_iterator UNSPEC_BCD_ADD_SUB [UNSPEC_BCDADD UNSPEC_BCDSUB])

(define_int_attr bcd_add_sub [(UNSPEC_BCDADD "add")
			      (UNSPEC_BCDSUB "sub")])

(define_code_iterator BCD_TEST [eq lt gt unordered])

(define_insn "bcd<bcd_add_sub>"
  [(set (match_operand:V1TI 0 "register_operand" "")
	(unspec:V1TI [(match_operand:V1TI 1 "register_operand" "")
		      (match_operand:V1TI 2 "register_operand" "")
		      (match_operand:QI 3 "const_0_to_1_operand" "")]
		     UNSPEC_BCD_ADD_SUB))
   (clobber (reg:CCFP 74))]
  "TARGET_P8_VECTOR"
  "bcd<bcd_add_sub>. %0,%1,%2,%3"
  [(set_attr "length" "4")
   (set_attr "type" "vecsimple")])

;; Use a floating point type (V2DFmode) for the compare to set CR6 so that we
;; can use the unordered test for BCD nans and add/subtracts that overflow.  An
;; UNORDERED test on an integer type (like V1TImode) is not defined.  The type
;; probably should be one that can go in the VMX (Altivec) registers, so we
;; can't use DDmode or DFmode.
(define_insn "*bcd<bcd_add_sub>_test"
  [(set (reg:CCFP 74)
	(compare:CCFP
	 (unspec:V2DF [(match_operand:V1TI 1 "register_operand" "v")
		       (match_operand:V1TI 2 "register_operand" "v")
		       (match_operand:QI 3 "const_0_to_1_operand" "i")]
		      UNSPEC_BCD_ADD_SUB)
	 (match_operand:V2DF 4 "zero_constant" "j")))
   (clobber (match_scratch:V1TI 0 "=v"))]
  "TARGET_P8_VECTOR"
  "bcd<bcd_add_sub>. %0,%1,%2,%3"
  [(set_attr "length" "4")
   (set_attr "type" "vecsimple")])

(define_insn "*bcd<bcd_add_sub>_test2"
  [(set (match_operand:V1TI 0 "register_operand" "=v")
	(unspec:V1TI [(match_operand:V1TI 1 "register_operand" "v")
		      (match_operand:V1TI 2 "register_operand" "v")
		      (match_operand:QI 3 "const_0_to_1_operand" "i")]
		     UNSPEC_BCD_ADD_SUB))
   (set (reg:CCFP 74)
	(compare:CCFP
	 (unspec:V2DF [(match_dup 1)
		       (match_dup 2)
		       (match_dup 3)]
		      UNSPEC_BCD_ADD_SUB)
	 (match_operand:V2DF 4 "zero_constant" "j")))]
  "TARGET_P8_VECTOR"
  "bcd<bcd_add_sub>. %0,%1,%2,%3"
  [(set_attr "length" "4")
   (set_attr "type" "vecsimple")])

(define_insn "darn_32"
  [(set (match_operand:SI 0 "register_operand" "=r")
        (unspec:SI [(const_int 0)] UNSPEC_DARN_32))]
  "TARGET_MODULO"
  "darn %0,0"
  [(set_attr "type" "integer")])

(define_insn "darn_raw"
  [(set (match_operand:DI 0 "register_operand" "=r")
        (unspec:DI [(const_int 0)] UNSPEC_DARN_RAW))]
  "TARGET_MODULO && TARGET_64BIT"
  "darn %0,2"
  [(set_attr "type" "integer")])

(define_insn "darn"
  [(set (match_operand:DI 0 "register_operand" "=r")
        (unspec:DI [(const_int 0)] UNSPEC_DARN))]
  "TARGET_MODULO && TARGET_64BIT"
  "darn %0,1"
  [(set_attr "type" "integer")])

(define_expand "bcd<bcd_add_sub>_<code>"
  [(parallel [(set (reg:CCFP 74)
		   (compare:CCFP
		    (unspec:V2DF [(match_operand:V1TI 1 "register_operand" "")
				  (match_operand:V1TI 2 "register_operand" "")
				  (match_operand:QI 3 "const_0_to_1_operand" "")]
				 UNSPEC_BCD_ADD_SUB)
		    (match_dup 4)))
	      (clobber (match_scratch:V1TI 5 ""))])
   (set (match_operand:SI 0 "register_operand" "")
	(BCD_TEST:SI (reg:CCFP 74)
		     (const_int 0)))]
  "TARGET_P8_VECTOR"
{
  operands[4] = CONST0_RTX (V2DFmode);
})

;; Peephole2 pattern to combine a bcdadd/bcdsub that calculates the value and
;; the bcdadd/bcdsub that tests the value.  The combiner won't work since
;; CR6 is a hard coded register.  Unfortunately, all of the Altivec predicate
;; support is hard coded to use the fixed register CR6 instead of creating
;; a register class for CR6.

(define_peephole2
  [(parallel [(set (match_operand:V1TI 0 "register_operand" "")
		   (unspec:V1TI [(match_operand:V1TI 1 "register_operand" "")
				 (match_operand:V1TI 2 "register_operand" "")
				 (match_operand:QI 3 "const_0_to_1_operand" "")]
				UNSPEC_BCD_ADD_SUB))
	      (clobber (reg:CCFP 74))])
   (parallel [(set (reg:CCFP 74)
		   (compare:CCFP
		    (unspec:V2DF [(match_dup 1)
				  (match_dup 2)
				  (match_dup 3)]
				 UNSPEC_BCD_ADD_SUB)
		    (match_operand:V2DF 4 "zero_constant" "")))
	      (clobber (match_operand:V1TI 5 "register_operand" ""))])]
  "TARGET_P8_VECTOR"
  [(parallel [(set (match_dup 0)
		   (unspec:V1TI [(match_dup 1)
				 (match_dup 2)
				 (match_dup 3)]
				UNSPEC_BCD_ADD_SUB))
	      (set (reg:CCFP 74)
		   (compare:CCFP
		    (unspec:V2DF [(match_dup 1)
				  (match_dup 2)
				  (match_dup 3)]
				 UNSPEC_BCD_ADD_SUB)
		    (match_dup 4)))])])
