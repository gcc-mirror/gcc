;; Machine description for RISC-V vector dot-product extensions.
;; Copyright (C) 2026 Free Software Foundation, Inc.
;; Contributed by Jiawei Chen (jiawei@iscas.ac.cn), ISCAS.

;; This file is part of GCC.

;; GCC is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; GCC is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.	 See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GCC; see the file COPYING3.  If not see
;; <http://www.gnu.org/licenses/>.

;; Earlyclobber prevents overlap with either source group or the mask.
;; The accumulator is tied to the result.
;; Distinct UNSPECs select each signedness or FP8 format combination, making
;; the altfmt requirement an instruction property.
;; Like reductions, these operations are opaque: VL counts source terms,
;; not active accumulator elements.  Zvdota masks source terms; Zvbdota
;; masks the eight accumulator elements selected by ci.

;; Zvdota dot-product instructions.  The accumulator always has EMUL=1,
;; independently of the source LMUL.

(define_insn "@pred_<dot_name>_vv_zvdota<mode>"
  [(set (match_operand:<V_QUAD_EXT_LMUL1> 0 "register_operand"	       "=&vr")
	(unspec:<V_QUAD_EXT_LMUL1>
	  [(unspec:<VM>
	    [(match_operand:<VM> 1 "vector_mask_operand"	       "vmWc1")
	     (match_operand 5 "vector_length_operand"		       "rvl")
	     (match_operand 6 "const_int_operand"		       "i")
	     (match_operand 7 "const_int_operand"		       "i")
	     (match_operand 8 "const_int_operand"		       "i")
	     (reg:SI VL_REGNUM)
	     (reg:SI VTYPE_REGNUM)] UNSPEC_VPREDICATE)
	   (unspec:<V_QUAD_EXT_LMUL1>
	     [(match_operand:VI_ZVQWDOTA 3 "register_operand"	       "vr")
	      (match_operand:VI_ZVQWDOTA 4 "register_operand"	       "vr")
	      (match_operand:<V_QUAD_EXT_LMUL1> 2 "register_operand"   "0")]
	     ZVQWDOT)
	   (match_dup 2)] UNSPEC_REDUC))]
  "TARGET_VECTOR"
  "<dot_asm>.vv\t%0,%3,%4%p1"
  [(set_attr "type" "vidota")
   (set_attr "mode" "<MODE>")
   (set (attr "altfmt") (symbol_ref "riscv_vector::<dot_altfmt>"))])

(define_insn "@pred_vfwdota_vv_zvdota<mode>"
  [(set (match_operand:<V_EXT_LMUL1> 0 "register_operand"	       "=&vr")
	(unspec:<V_EXT_LMUL1>
	  [(unspec:<VM>
	    [(match_operand:<VM> 1 "vector_mask_operand"	       "vmWc1")
	     (match_operand 5 "vector_length_operand"		       "rvl")
	     (match_operand 6 "const_int_operand"		       "i")
	     (match_operand 7 "const_int_operand"		       "i")
	     (match_operand 8 "const_int_operand"		       "i")
	     (reg:SI VL_REGNUM)
	     (reg:SI VTYPE_REGNUM)] UNSPEC_VPREDICATE)
	   (unspec:<V_EXT_LMUL1>
	     [(match_operand:VF_ZVFBF16 3 "register_operand"	       "vr")
	      (match_operand:VF_ZVFBF16 4 "register_operand"	       "vr")
	      (match_operand:<V_EXT_LMUL1> 2 "register_operand"	       "0")]
	     UNSPEC_VFWDOTA)
	   (match_dup 2)] UNSPEC_REDUC))]
  "TARGET_ZVFWDOTA16BF && TARGET_VECTOR_ELEN_FP_32"
  "vfwdota.vv\t%0,%3,%4%p1"
  [(set_attr "type" "vfdota")
   (set_attr "mode" "<MODE>")
   (set (attr "altfmt") (symbol_ref "riscv_vector::ALTFMT_ALT"))])

(define_insn "@pred_<dot_name>_vv_zvdota<mode>"
  [(set (match_operand:RVVM1SF 0 "register_operand"		       "=&vr")
	(unspec:RVVM1SF
	  [(unspec:<VM>
	    [(match_operand:<VM> 1 "vector_mask_operand"	       "vmWc1")
	     (match_operand 5 "vector_length_operand"		       "rvl")
	     (match_operand 6 "const_int_operand"		       "i")
	     (match_operand 7 "const_int_operand"		       "i")
	     (match_operand 8 "const_int_operand"		       "i")
	     (reg:SI VL_REGNUM)
	     (reg:SI VTYPE_REGNUM)] UNSPEC_VPREDICATE)
	   (unspec:RVVM1SF
	     [(match_operand:VDOT_QI 3 "register_operand"	       "vr")
	      (match_operand:VDOT_QI 4 "register_operand"	       "vr")
	      (match_operand:RVVM1SF 2 "register_operand"	       "0")]
	     ZVFQWDOT)
	   (match_dup 2)] UNSPEC_REDUC))]
  "TARGET_ZVFQWDOTA8F && TARGET_VECTOR_ELEN_FP_32"
  "<dot_asm>.vv\t%0,%3,%4%p1"
  [(set_attr "type" "vfdota")
   (set_attr "mode" "<MODE>")
   (set (attr "altfmt") (symbol_ref "riscv_vector::<dot_altfmt>"))])

;; Zvbdota batched dot-product instructions.  VTYPE uses LMUL=1; vs2 has
;; EMUL=8 and vs1 has EMUL=1.  Eight results are accumulated at offset ci
;; in vd.  The intrinsic checker requires capacity for at least eight results.

(define_insn "@pred_<dot_name>_vv<mode>"
  [(set (match_operand:VI_ZVQWBDOTA 0 "register_operand"	       "=&vr")
	(unspec:VI_ZVQWBDOTA
	  [(unspec:<VM>
	    [(match_operand:<VM> 1 "vector_mask_operand"	       "vmWc1")
	     (match_operand 6 "vector_length_operand"		       "rvl")
	     (match_operand 7 "const_int_operand"		       "i")
	     (match_operand 8 "const_int_operand"		       "i")
	     (match_operand 9 "const_int_operand"		       "i")
	     (reg:SI VL_REGNUM)
	     (reg:SI VTYPE_REGNUM)] UNSPEC_VPREDICATE)
	   (unspec:VI_ZVQWBDOTA
	     [(match_operand:<V_QUAD_TRUNC_LMUL8> 3 "register_operand" "vr")
	      (match_operand:<V_QUAD_TRUNC_LMUL1> 4 "register_operand" "vr")
	      (match_operand 5 "zvbdota_ci_operand"		       "i")
	      (match_operand:VI_ZVQWBDOTA 2 "register_operand"	       "0")]
	     ZVQWBDOT)
	   (match_dup 2)] UNSPEC_REDUC))]
  "TARGET_VECTOR"
  "<dot_asm>.vv\t%0,%3,%4,%5%p1"
  [(set_attr "type" "vibdota")
   (set_attr "mode" "<V_QUAD_TRUNC_LMUL1>")
   (set (attr "altfmt") (symbol_ref "riscv_vector::<dot_altfmt>"))])

(define_insn "@pred_vfwbdota_vv<mode>"
  [(set (match_operand:VF_S_NOFRAC 0 "register_operand"		       "=&vr")
	(unspec:VF_S_NOFRAC
	  [(unspec:<VM>
	    [(match_operand:<VM> 1 "vector_mask_operand"	       "vmWc1")
	     (match_operand 6 "vector_length_operand"		       "rvl")
	     (match_operand 7 "const_int_operand"		       "i")
	     (match_operand 8 "const_int_operand"		       "i")
	     (match_operand 9 "const_int_operand"		       "i")
	     (reg:SI VL_REGNUM)
	     (reg:SI VTYPE_REGNUM)] UNSPEC_VPREDICATE)
	   (unspec:VF_S_NOFRAC
	     [(match_operand:RVVM8BF 3 "register_operand"	       "vr")
	      (match_operand:RVVM1BF 4 "register_operand"	       "vr")
	      (match_operand 5 "zvbdota_ci_operand"		       "i")
	      (match_operand:VF_S_NOFRAC 2 "register_operand"	       "0")]
	     UNSPEC_VFWBDOTA)
	   (match_dup 2)] UNSPEC_REDUC))]
  "TARGET_ZVFWBDOTA16BF && TARGET_VECTOR_ELEN_BF_16"
  "vfwbdota.vv\t%0,%3,%4,%5%p1"
  [(set_attr "type" "vfbdota")
   (set_attr "mode" "RVVM1BF")
   (set (attr "altfmt") (symbol_ref "riscv_vector::ALTFMT_ALT"))])

(define_insn "@pred_<dot_name>_vv<mode>"
  [(set (match_operand:VF_S_NOFRAC 0 "register_operand"		       "=&vr")
	(unspec:VF_S_NOFRAC
	  [(unspec:<VM>
	    [(match_operand:<VM> 1 "vector_mask_operand"	       "vmWc1")
	     (match_operand 6 "vector_length_operand"		       "rvl")
	     (match_operand 7 "const_int_operand"		       "i")
	     (match_operand 8 "const_int_operand"		       "i")
	     (match_operand 9 "const_int_operand"		       "i")
	     (reg:SI VL_REGNUM)
	     (reg:SI VTYPE_REGNUM)] UNSPEC_VPREDICATE)
	   (unspec:VF_S_NOFRAC
	     [(match_operand:RVVM8QI 3 "register_operand"	       "vr")
	      (match_operand:RVVM1QI 4 "register_operand"	       "vr")
	      (match_operand 5 "zvbdota_ci_operand"		       "i")
	      (match_operand:VF_S_NOFRAC 2 "register_operand"	       "0")]
	     ZVFQWBDOT)
	   (match_dup 2)] UNSPEC_REDUC))]
  "TARGET_ZVFQWBDOTA8F"
  "<dot_asm>.vv\t%0,%3,%4,%5%p1"
  [(set_attr "type" "vfbdota")
   (set_attr "mode" "RVVM1QI")
   (set (attr "altfmt") (symbol_ref "riscv_vector::<dot_altfmt>"))])

(define_insn "@pred_vfbdota_vv<mode>"
  [(set (match_operand:VF_S_NOFRAC 0 "register_operand"		       "=&vr")
	(unspec:VF_S_NOFRAC
	  [(unspec:<VM>
	    [(match_operand:<VM> 1 "vector_mask_operand"	       "vmWc1")
	     (match_operand 6 "vector_length_operand"		       "rvl")
	     (match_operand 7 "const_int_operand"		       "i")
	     (match_operand 8 "const_int_operand"		       "i")
	     (match_operand 9 "const_int_operand"		       "i")
	     (match_operand 10 "const_int_operand"		       "i")
	     (reg:SI VL_REGNUM)
	     (reg:SI VTYPE_REGNUM)
	     (reg:SI FRM_REGNUM)] UNSPEC_VPREDICATE)
	   (unspec:VF_S_NOFRAC
	     [(match_operand:RVVM8SF 3 "register_operand"	       "vr")
	      (match_operand:RVVM1SF 4 "register_operand"	       "vr")
	      (match_operand 5 "zvbdota_ci_operand"		       "i")
	      (match_operand:VF_S_NOFRAC 2 "register_operand"	       "0")]
	     UNSPEC_VFBDOTA)
	   (match_dup 2)] UNSPEC_REDUC))]
  "TARGET_ZVFBDOTA32F"
  "vfbdota.vv\t%0,%3,%4,%5%p1"
  [(set_attr "type" "vfbdota")
   (set_attr "mode" "RVVM1SF")
   (set (attr "frm_mode")
	(symbol_ref "riscv_vector::get_frm_mode (operands[10])"))])
