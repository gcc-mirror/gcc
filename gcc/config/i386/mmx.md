;; GCC machine description for MMX and 3dNOW! instructions
;; Copyright (C) 2005, 2007, 2008, 2009, 2010, 2011
;; Free Software Foundation, Inc.
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

;; The MMX and 3dNOW! patterns are in the same file because they use
;; the same register file, and 3dNOW! adds a number of extensions to
;; the base integer MMX isa.

;; Note!  Except for the basic move instructions, *all* of these
;; patterns are outside the normal optabs namespace.  This is because
;; use of these registers requires the insertion of emms or femms
;; instructions to return to normal fpu mode.  The compiler doesn't
;; know how to do that itself, which means it's up to the user.  Which
;; means that we should never use any of these patterns except at the
;; direction of the user via a builtin.

(define_c_enum "unspec" [
  UNSPEC_MOVNTQ
  UNSPEC_PFRCP
  UNSPEC_PFRCPIT1
  UNSPEC_PFRCPIT2
  UNSPEC_PFRSQRT
  UNSPEC_PFRSQIT1
])

(define_c_enum "unspecv" [
  UNSPECV_EMMS
  UNSPECV_FEMMS
])

;; 8 byte integral modes handled by MMX (and by extension, SSE)
(define_mode_iterator MMXMODEI [V8QI V4HI V2SI])
(define_mode_iterator MMXMODEI8 [V8QI V4HI V2SI V1DI])

;; All 8-byte vector modes handled by MMX
(define_mode_iterator MMXMODE [V8QI V4HI V2SI V1DI V2SF])

;; Mix-n-match
(define_mode_iterator MMXMODE12 [V8QI V4HI])
(define_mode_iterator MMXMODE24 [V4HI V2SI])
(define_mode_iterator MMXMODE248 [V4HI V2SI V1DI])

;; Mapping from integer vector mode to mnemonic suffix
(define_mode_attr mmxvecsize [(V8QI "b") (V4HI "w") (V2SI "d") (V1DI "q")])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Move patterns
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; All of these patterns are enabled for MMX as well as 3dNOW.
;; This is essential for maintaining stable calling conventions.

(define_expand "mov<mode>"
  [(set (match_operand:MMXMODEI8 0 "nonimmediate_operand")
	(match_operand:MMXMODEI8 1 "nonimmediate_operand"))]
  "TARGET_MMX"
{
  ix86_expand_vector_move (<MODE>mode, operands);
  DONE;
})

;; movd instead of movq is required to handle broken assemblers.
(define_insn "*mov<mode>_internal_rex64"
  [(set (match_operand:MMXMODEI8 0 "nonimmediate_operand"
	 "=rm,r,!?y,!y,!?y,m  ,!y ,*x,x,x ,m,r ,Yi")
	(match_operand:MMXMODEI8 1 "vector_move_operand"
	 "Cr ,m,C  ,!y,m  ,!?y,*x,!y ,C,xm,x,Yi,r"))]
  "TARGET_64BIT && TARGET_MMX
   && !(MEM_P (operands[0]) && MEM_P (operands[1]))"
  "@
    mov{q}\t{%1, %0|%0, %1}
    mov{q}\t{%1, %0|%0, %1}
    pxor\t%0, %0
    movq\t{%1, %0|%0, %1}
    movq\t{%1, %0|%0, %1}
    movq\t{%1, %0|%0, %1}
    movdq2q\t{%1, %0|%0, %1}
    movq2dq\t{%1, %0|%0, %1}
    %vpxor\t%0, %d0
    %vmovq\t{%1, %0|%0, %1}
    %vmovq\t{%1, %0|%0, %1}
    %vmovd\t{%1, %0|%0, %1}
    %vmovd\t{%1, %0|%0, %1}"
  [(set (attr "type")
     (cond [(eq_attr "alternative" "0,1")
	      (const_string "imov")
	    (eq_attr "alternative" "2")
	      (const_string "mmx")
	    (eq_attr "alternative" "3,4,5")
	      (const_string "mmxmov")
	    (eq_attr "alternative" "6,7")
	      (const_string "ssecvt")
	    (eq_attr "alternative" "8")
	      (const_string "sselog1")
	   ]
	   (const_string "ssemov")))
   (set (attr "unit")
     (if_then_else (eq_attr "alternative" "6,7")
       (const_string "mmx")
       (const_string "*")))
   (set (attr "prefix_rep")
     (if_then_else (eq_attr "alternative" "6,7,9")
       (const_string "1")
       (const_string "*")))
   (set (attr "prefix_data16")
     (if_then_else (eq_attr "alternative" "10,11,12")
       (const_string "1")
       (const_string "*")))
   (set (attr "prefix_rex")
     (if_then_else (eq_attr "alternative" "9,10")
       (symbol_ref "x86_extended_reg_mentioned_p (insn)")
       (const_string "*")))
   (set (attr "prefix")
     (if_then_else (eq_attr "alternative" "8,9,10,11,12")
       (const_string "maybe_vex")
       (const_string "orig")))
   (set_attr "mode" "DI")])

(define_insn "*mov<mode>_internal"
  [(set (match_operand:MMXMODEI8 0 "nonimmediate_operand"
	 "=!?y,!y,!?y,m  ,!y,*x,*x,*x ,m ,*x,*x,*x,m ,r  ,m")
	(match_operand:MMXMODEI8 1 "vector_move_operand"
	 "C   ,!y,m  ,!?y,*x,!y,C ,*xm,*x,C ,*x,m ,*x,irm,r"))]
  "!TARGET_64BIT && TARGET_MMX
   && !(MEM_P (operands[0]) && MEM_P (operands[1]))"
  "@
    pxor\t%0, %0
    movq\t{%1, %0|%0, %1}
    movq\t{%1, %0|%0, %1}
    movq\t{%1, %0|%0, %1}
    movdq2q\t{%1, %0|%0, %1}
    movq2dq\t{%1, %0|%0, %1}
    %vpxor\t%0, %d0
    %vmovq\t{%1, %0|%0, %1}
    %vmovq\t{%1, %0|%0, %1}
    xorps\t%0, %0
    movaps\t{%1, %0|%0, %1}
    movlps\t{%1, %0|%0, %1}
    movlps\t{%1, %0|%0, %1}
    #
    #"
  [(set (attr "isa")
     (cond [(eq_attr "alternative" "4,5,6,7,8")
	      (const_string "sse2")
	    (eq_attr "alternative" "9,10,11,12")
	      (const_string "noavx")
	   ]
           (const_string "*")))
   (set (attr "type")
     (cond [(eq_attr "alternative" "0")
	      (const_string "mmx")
	    (eq_attr "alternative" "1,2,3")
	      (const_string "mmxmov")
	    (eq_attr "alternative" "4,5")
	      (const_string "ssecvt")
	    (eq_attr "alternative" "6,9")
	      (const_string "sselog1")
	    (eq_attr "alternative" "13,14")
	      (const_string "multi")
	   ]
	   (const_string "ssemov")))
   (set (attr "unit")
     (if_then_else (eq_attr "alternative" "4,5")
       (const_string "mmx")
       (const_string "*")))
   (set (attr "prefix_rep")
     (if_then_else
       (ior (eq_attr "alternative" "4,5")
	    (and (eq_attr "alternative" "7")
		 (not (match_test "TARGET_AVX"))))
       (const_string "1")
       (const_string "*")))
   (set (attr "prefix_data16")
     (if_then_else
       (and (eq_attr "alternative" "8")
	    (not (match_test "TARGET_AVX")))
       (const_string "1")
       (const_string "*")))
   (set (attr "prefix")
     (if_then_else (eq_attr "alternative" "6,7,8")
       (const_string "maybe_vex")
       (const_string "orig")))
   (set_attr "mode" "DI,DI,DI,DI,DI,DI,TI,DI,DI,V4SF,V4SF,V2SF,V2SF,DI,DI")])

(define_expand "movv2sf"
  [(set (match_operand:V2SF 0 "nonimmediate_operand")
	(match_operand:V2SF 1 "nonimmediate_operand"))]
  "TARGET_MMX"
{
  ix86_expand_vector_move (V2SFmode, operands);
  DONE;
})

;; movd instead of movq is required to handle broken assemblers.
(define_insn "*movv2sf_internal_rex64"
  [(set (match_operand:V2SF 0 "nonimmediate_operand"
	 "=rm,r,!?y,!y,!?y,m  ,!y,*x,x,x,x,m,r ,Yi")
        (match_operand:V2SF 1 "vector_move_operand"
	 "Cr ,m,C  ,!y,m  ,!?y,*x,!y,C,x,m,x,Yi,r"))]
  "TARGET_64BIT && TARGET_MMX
   && !(MEM_P (operands[0]) && MEM_P (operands[1]))"
  "@
    mov{q}\t{%1, %0|%0, %1}
    mov{q}\t{%1, %0|%0, %1}
    pxor\t%0, %0
    movq\t{%1, %0|%0, %1}
    movq\t{%1, %0|%0, %1}
    movq\t{%1, %0|%0, %1}
    movdq2q\t{%1, %0|%0, %1}
    movq2dq\t{%1, %0|%0, %1}
    %vxorps\t%0, %d0
    %vmovaps\t{%1, %0|%0, %1}
    %vmovlps\t{%1, %d0|%d0, %1}
    %vmovlps\t{%1, %0|%0, %1}
    %vmovd\t{%1, %0|%0, %1}
    %vmovd\t{%1, %0|%0, %1}"
  [(set (attr "type")
     (cond [(eq_attr "alternative" "0,1")
	      (const_string "imov")
	    (eq_attr "alternative" "2")
	      (const_string "mmx")
	    (eq_attr "alternative" "3,4,5")
	      (const_string "mmxmov")
	    (eq_attr "alternative" "6,7")
	      (const_string "ssecvt")
	    (eq_attr "alternative" "9")
	      (const_string "sselog1")
	   ]
	   (const_string "ssemov")))
   (set (attr "unit")
     (if_then_else (eq_attr "alternative" "6,7")
       (const_string "mmx")
       (const_string "*")))
   (set (attr "prefix_rep")
     (if_then_else (eq_attr "alternative" "6,7")
       (const_string "1")
       (const_string "*")))
   (set (attr "length_vex")
     (if_then_else
       (and (eq_attr "alternative" "12,13")
	    (match_test "TARGET_AVX"))
       (const_string "4")
       (const_string "*")))
   (set (attr "prefix")
     (if_then_else (eq_attr "alternative" "8,9,10,11,12,13")
       (const_string "maybe_vex")
       (const_string "orig")))
   (set_attr "mode" "DI,DI,DI,DI,DI,DI,DI,DI,V4SF,V4SF,V2SF,V2SF,DI,DI")])

(define_insn "*movv2sf_internal"
  [(set (match_operand:V2SF 0 "nonimmediate_operand"
	 "=!?y,!y,!?y,m  ,!y,*x,*x,*x,*x,m ,r  ,m")
        (match_operand:V2SF 1 "vector_move_operand"
	 "C   ,!y,m  ,!?y,*x,!y,C ,*x,m ,*x,irm,r"))]
  "!TARGET_64BIT && TARGET_MMX
   && !(MEM_P (operands[0]) && MEM_P (operands[1]))"
  "@
    pxor\t%0, %0
    movq\t{%1, %0|%0, %1}
    movq\t{%1, %0|%0, %1}
    movq\t{%1, %0|%0, %1}
    movdq2q\t{%1, %0|%0, %1}
    movq2dq\t{%1, %0|%0, %1}
    %vxorps\t%0, %d0
    %vmovaps\t{%1, %0|%0, %1}
    %vmovlps\t{%1, %d0|%d0, %1}
    %vmovlps\t{%1, %0|%0, %1}
    #
    #"
  [(set (attr "isa")
     (if_then_else (eq_attr "alternative" "4,5")
       (const_string "sse2")
       (const_string "*")))
   (set (attr "type")
     (cond [(eq_attr "alternative" "0")
	      (const_string "mmx")
	    (eq_attr "alternative" "1,2,3")
	      (const_string "mmxmov")
	    (eq_attr "alternative" "4,5")
	      (const_string "ssecvt")
	    (eq_attr "alternative" "6")
	      (const_string "sselog1")
	    (eq_attr "alternative" "10,11")
	      (const_string "multi")
	   ]
	   (const_string "ssemov")))
   (set (attr "unit")
     (if_then_else (eq_attr "alternative" "4,5")
       (const_string "mmx")
       (const_string "*")))
   (set (attr "prefix_rep")
     (if_then_else (eq_attr "alternative" "4,5")
       (const_string "1")
       (const_string "*")))
   (set (attr "prefix")
     (if_then_else (eq_attr "alternative" "6,7,8,9")
       (const_string "maybe_vex")
       (const_string "orig")))
   (set_attr "mode" "DI,DI,DI,DI,DI,DI,V4SF,V4SF,V2SF,V2SF,DI,DI")])

;; %%% This multiword shite has got to go.
(define_split
  [(set (match_operand:MMXMODE 0 "nonimmediate_operand")
        (match_operand:MMXMODE 1 "general_operand"))]
  "!TARGET_64BIT && reload_completed
   && !(MMX_REG_P (operands[0]) || SSE_REG_P (operands[0])
	|| MMX_REG_P (operands[1]) || SSE_REG_P (operands[1]))"
  [(const_int 0)]
  "ix86_split_long_move (operands); DONE;")

(define_expand "push<mode>1"
  [(match_operand:MMXMODE 0 "register_operand")]
  "TARGET_MMX"
{
  ix86_expand_push (<MODE>mode, operands[0]);
  DONE;
})

(define_expand "movmisalign<mode>"
  [(set (match_operand:MMXMODE 0 "nonimmediate_operand")
	(match_operand:MMXMODE 1 "nonimmediate_operand"))]
  "TARGET_MMX"
{
  ix86_expand_vector_move (<MODE>mode, operands);
  DONE;
})

(define_insn "sse_movntq"
  [(set (match_operand:DI 0 "memory_operand" "=m")
	(unspec:DI [(match_operand:DI 1 "register_operand" "y")]
		   UNSPEC_MOVNTQ))]
  "TARGET_SSE || TARGET_3DNOW_A"
  "movntq\t{%1, %0|%0, %1}"
  [(set_attr "type" "mmxmov")
   (set_attr "mode" "DI")])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Parallel single-precision floating point arithmetic
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define_expand "mmx_addv2sf3"
  [(set (match_operand:V2SF 0 "register_operand")
	(plus:V2SF
	  (match_operand:V2SF 1 "nonimmediate_operand")
	  (match_operand:V2SF 2 "nonimmediate_operand")))]
  "TARGET_3DNOW"
  "ix86_fixup_binary_operands_no_copy (PLUS, V2SFmode, operands);")

(define_insn "*mmx_addv2sf3"
  [(set (match_operand:V2SF 0 "register_operand" "=y")
	(plus:V2SF (match_operand:V2SF 1 "nonimmediate_operand" "%0")
		   (match_operand:V2SF 2 "nonimmediate_operand" "ym")))]
  "TARGET_3DNOW && ix86_binary_operator_ok (PLUS, V2SFmode, operands)"
  "pfadd\t{%2, %0|%0, %2}"
  [(set_attr "type" "mmxadd")
   (set_attr "prefix_extra" "1")
   (set_attr "mode" "V2SF")])

(define_expand "mmx_subv2sf3"
  [(set (match_operand:V2SF 0 "register_operand")
        (minus:V2SF (match_operand:V2SF 1 "register_operand")
		    (match_operand:V2SF 2 "nonimmediate_operand")))]
  "TARGET_3DNOW")

(define_expand "mmx_subrv2sf3"
  [(set (match_operand:V2SF 0 "register_operand")
        (minus:V2SF (match_operand:V2SF 2 "register_operand")
		    (match_operand:V2SF 1 "nonimmediate_operand")))]
  "TARGET_3DNOW")

(define_insn "*mmx_subv2sf3"
  [(set (match_operand:V2SF 0 "register_operand" "=y,y")
        (minus:V2SF (match_operand:V2SF 1 "nonimmediate_operand" "0,ym")
		    (match_operand:V2SF 2 "nonimmediate_operand" "ym,0")))]
  "TARGET_3DNOW && !(MEM_P (operands[0]) && MEM_P (operands[1]))"
  "@
   pfsub\t{%2, %0|%0, %2}
   pfsubr\t{%1, %0|%0, %1}"
  [(set_attr "type" "mmxadd")
   (set_attr "prefix_extra" "1")
   (set_attr "mode" "V2SF")])

(define_expand "mmx_mulv2sf3"
  [(set (match_operand:V2SF 0 "register_operand")
	(mult:V2SF (match_operand:V2SF 1 "nonimmediate_operand")
		   (match_operand:V2SF 2 "nonimmediate_operand")))]
  "TARGET_3DNOW"
  "ix86_fixup_binary_operands_no_copy (MULT, V2SFmode, operands);")

(define_insn "*mmx_mulv2sf3"
  [(set (match_operand:V2SF 0 "register_operand" "=y")
	(mult:V2SF (match_operand:V2SF 1 "nonimmediate_operand" "%0")
		   (match_operand:V2SF 2 "nonimmediate_operand" "ym")))]
  "TARGET_3DNOW && ix86_binary_operator_ok (MULT, V2SFmode, operands)"
  "pfmul\t{%2, %0|%0, %2}"
  [(set_attr "type" "mmxmul")
   (set_attr "prefix_extra" "1")
   (set_attr "mode" "V2SF")])

;; ??? For !flag_finite_math_only, the representation with SMIN/SMAX
;; isn't really correct, as those rtl operators aren't defined when
;; applied to NaNs.  Hopefully the optimizers won't get too smart on us.

(define_expand "mmx_<code>v2sf3"
  [(set (match_operand:V2SF 0 "register_operand")
        (smaxmin:V2SF
	  (match_operand:V2SF 1 "nonimmediate_operand")
	  (match_operand:V2SF 2 "nonimmediate_operand")))]
  "TARGET_3DNOW"
{
  if (!flag_finite_math_only)
    operands[1] = force_reg (V2SFmode, operands[1]);
  ix86_fixup_binary_operands_no_copy (<CODE>, V2SFmode, operands);
})

(define_insn "*mmx_<code>v2sf3_finite"
  [(set (match_operand:V2SF 0 "register_operand" "=y")
        (smaxmin:V2SF
	  (match_operand:V2SF 1 "nonimmediate_operand" "%0")
	  (match_operand:V2SF 2 "nonimmediate_operand" "ym")))]
  "TARGET_3DNOW && flag_finite_math_only
   && ix86_binary_operator_ok (<CODE>, V2SFmode, operands)"
  "pf<maxmin_float>\t{%2, %0|%0, %2}"
  [(set_attr "type" "mmxadd")
   (set_attr "prefix_extra" "1")
   (set_attr "mode" "V2SF")])

(define_insn "*mmx_<code>v2sf3"
  [(set (match_operand:V2SF 0 "register_operand" "=y")
        (smaxmin:V2SF
	  (match_operand:V2SF 1 "register_operand" "0")
	  (match_operand:V2SF 2 "nonimmediate_operand" "ym")))]
  "TARGET_3DNOW"
  "pf<maxmin_float>\t{%2, %0|%0, %2}"
  [(set_attr "type" "mmxadd")
   (set_attr "prefix_extra" "1")
   (set_attr "mode" "V2SF")])

(define_insn "mmx_rcpv2sf2"
  [(set (match_operand:V2SF 0 "register_operand" "=y")
        (unspec:V2SF [(match_operand:V2SF 1 "nonimmediate_operand" "ym")]
		     UNSPEC_PFRCP))]
  "TARGET_3DNOW"
  "pfrcp\t{%1, %0|%0, %1}"
  [(set_attr "type" "mmx")
   (set_attr "prefix_extra" "1")
   (set_attr "mode" "V2SF")])

(define_insn "mmx_rcpit1v2sf3"
  [(set (match_operand:V2SF 0 "register_operand" "=y")
	(unspec:V2SF [(match_operand:V2SF 1 "register_operand" "0")
		      (match_operand:V2SF 2 "nonimmediate_operand" "ym")]
		     UNSPEC_PFRCPIT1))]
  "TARGET_3DNOW"
  "pfrcpit1\t{%2, %0|%0, %2}"
  [(set_attr "type" "mmx")
   (set_attr "prefix_extra" "1")
   (set_attr "mode" "V2SF")])

(define_insn "mmx_rcpit2v2sf3"
  [(set (match_operand:V2SF 0 "register_operand" "=y")
	(unspec:V2SF [(match_operand:V2SF 1 "register_operand" "0")
		      (match_operand:V2SF 2 "nonimmediate_operand" "ym")]
		     UNSPEC_PFRCPIT2))]
  "TARGET_3DNOW"
  "pfrcpit2\t{%2, %0|%0, %2}"
  [(set_attr "type" "mmx")
   (set_attr "prefix_extra" "1")
   (set_attr "mode" "V2SF")])

(define_insn "mmx_rsqrtv2sf2"
  [(set (match_operand:V2SF 0 "register_operand" "=y")
	(unspec:V2SF [(match_operand:V2SF 1 "nonimmediate_operand" "ym")]
		     UNSPEC_PFRSQRT))]
  "TARGET_3DNOW"
  "pfrsqrt\t{%1, %0|%0, %1}"
  [(set_attr "type" "mmx")
   (set_attr "prefix_extra" "1")
   (set_attr "mode" "V2SF")])

(define_insn "mmx_rsqit1v2sf3"
  [(set (match_operand:V2SF 0 "register_operand" "=y")
	(unspec:V2SF [(match_operand:V2SF 1 "register_operand" "0")
		      (match_operand:V2SF 2 "nonimmediate_operand" "ym")]
		     UNSPEC_PFRSQIT1))]
  "TARGET_3DNOW"
  "pfrsqit1\t{%2, %0|%0, %2}"
  [(set_attr "type" "mmx")
   (set_attr "prefix_extra" "1")
   (set_attr "mode" "V2SF")])

(define_insn "mmx_haddv2sf3"
  [(set (match_operand:V2SF 0 "register_operand" "=y")
	(vec_concat:V2SF
	  (plus:SF
	    (vec_select:SF
	      (match_operand:V2SF 1 "register_operand" "0")
	      (parallel [(const_int  0)]))
	    (vec_select:SF (match_dup 1) (parallel [(const_int 1)])))
	  (plus:SF
            (vec_select:SF
	      (match_operand:V2SF 2 "nonimmediate_operand" "ym")
	      (parallel [(const_int  0)]))
	    (vec_select:SF (match_dup 2) (parallel [(const_int 1)])))))]
  "TARGET_3DNOW"
  "pfacc\t{%2, %0|%0, %2}"
  [(set_attr "type" "mmxadd")
   (set_attr "prefix_extra" "1")
   (set_attr "mode" "V2SF")])

(define_insn "mmx_hsubv2sf3"
  [(set (match_operand:V2SF 0 "register_operand" "=y")
	(vec_concat:V2SF
	  (minus:SF
	    (vec_select:SF
	      (match_operand:V2SF 1 "register_operand" "0")
	      (parallel [(const_int  0)]))
	    (vec_select:SF (match_dup 1) (parallel [(const_int 1)])))
	  (minus:SF
            (vec_select:SF
	      (match_operand:V2SF 2 "nonimmediate_operand" "ym")
	      (parallel [(const_int  0)]))
	    (vec_select:SF (match_dup 2) (parallel [(const_int 1)])))))]
  "TARGET_3DNOW_A"
  "pfnacc\t{%2, %0|%0, %2}"
  [(set_attr "type" "mmxadd")
   (set_attr "prefix_extra" "1")
   (set_attr "mode" "V2SF")])

(define_insn "mmx_addsubv2sf3"
  [(set (match_operand:V2SF 0 "register_operand" "=y")
        (vec_merge:V2SF
          (plus:V2SF
            (match_operand:V2SF 1 "register_operand" "0")
            (match_operand:V2SF 2 "nonimmediate_operand" "ym"))
          (minus:V2SF (match_dup 1) (match_dup 2))
          (const_int 1)))]
  "TARGET_3DNOW_A"
  "pfpnacc\t{%2, %0|%0, %2}"
  [(set_attr "type" "mmxadd")
   (set_attr "prefix_extra" "1")
   (set_attr "mode" "V2SF")])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Parallel single-precision floating point comparisons
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define_expand "mmx_eqv2sf3"
  [(set (match_operand:V2SI 0 "register_operand")
	(eq:V2SI (match_operand:V2SF 1 "nonimmediate_operand")
		 (match_operand:V2SF 2 "nonimmediate_operand")))]
  "TARGET_3DNOW"
  "ix86_fixup_binary_operands_no_copy (EQ, V2SFmode, operands);")

(define_insn "*mmx_eqv2sf3"
  [(set (match_operand:V2SI 0 "register_operand" "=y")
	(eq:V2SI (match_operand:V2SF 1 "nonimmediate_operand" "%0")
		 (match_operand:V2SF 2 "nonimmediate_operand" "ym")))]
  "TARGET_3DNOW && ix86_binary_operator_ok (EQ, V2SFmode, operands)"
  "pfcmpeq\t{%2, %0|%0, %2}"
  [(set_attr "type" "mmxcmp")
   (set_attr "prefix_extra" "1")
   (set_attr "mode" "V2SF")])

(define_insn "mmx_gtv2sf3"
  [(set (match_operand:V2SI 0 "register_operand" "=y")
	(gt:V2SI (match_operand:V2SF 1 "register_operand" "0")
		 (match_operand:V2SF 2 "nonimmediate_operand" "ym")))]
  "TARGET_3DNOW"
  "pfcmpgt\t{%2, %0|%0, %2}"
  [(set_attr "type" "mmxcmp")
   (set_attr "prefix_extra" "1")
   (set_attr "mode" "V2SF")])

(define_insn "mmx_gev2sf3"
  [(set (match_operand:V2SI 0 "register_operand" "=y")
	(ge:V2SI (match_operand:V2SF 1 "register_operand" "0")
		 (match_operand:V2SF 2 "nonimmediate_operand" "ym")))]
  "TARGET_3DNOW"
  "pfcmpge\t{%2, %0|%0, %2}"
  [(set_attr "type" "mmxcmp")
   (set_attr "prefix_extra" "1")
   (set_attr "mode" "V2SF")])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Parallel single-precision floating point conversion operations
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define_insn "mmx_pf2id"
  [(set (match_operand:V2SI 0 "register_operand" "=y")
	(fix:V2SI (match_operand:V2SF 1 "nonimmediate_operand" "ym")))]
  "TARGET_3DNOW"
  "pf2id\t{%1, %0|%0, %1}"
  [(set_attr "type" "mmxcvt")
   (set_attr "prefix_extra" "1")
   (set_attr "mode" "V2SF")])

(define_insn "mmx_pf2iw"
  [(set (match_operand:V2SI 0 "register_operand" "=y")
	(sign_extend:V2SI
	  (ss_truncate:V2HI
	    (fix:V2SI
	      (match_operand:V2SF 1 "nonimmediate_operand" "ym")))))]
  "TARGET_3DNOW_A"
  "pf2iw\t{%1, %0|%0, %1}"
  [(set_attr "type" "mmxcvt")
   (set_attr "prefix_extra" "1")
   (set_attr "mode" "V2SF")])

(define_insn "mmx_pi2fw"
  [(set (match_operand:V2SF 0 "register_operand" "=y")
	(float:V2SF
	  (sign_extend:V2SI
	    (truncate:V2HI
	      (match_operand:V2SI 1 "nonimmediate_operand" "ym")))))]
  "TARGET_3DNOW_A"
  "pi2fw\t{%1, %0|%0, %1}"
  [(set_attr "type" "mmxcvt")
   (set_attr "prefix_extra" "1")
   (set_attr "mode" "V2SF")])

(define_insn "mmx_floatv2si2"
  [(set (match_operand:V2SF 0 "register_operand" "=y")
	(float:V2SF (match_operand:V2SI 1 "nonimmediate_operand" "ym")))]
  "TARGET_3DNOW"
  "pi2fd\t{%1, %0|%0, %1}"
  [(set_attr "type" "mmxcvt")
   (set_attr "prefix_extra" "1")
   (set_attr "mode" "V2SF")])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Parallel single-precision floating point element swizzling
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define_insn "mmx_pswapdv2sf2"
  [(set (match_operand:V2SF 0 "register_operand" "=y")
	(vec_select:V2SF (match_operand:V2SF 1 "nonimmediate_operand" "ym")
			 (parallel [(const_int 1) (const_int 0)])))]
  "TARGET_3DNOW_A"
  "pswapd\t{%1, %0|%0, %1}"
  [(set_attr "type" "mmxcvt")
   (set_attr "prefix_extra" "1")
   (set_attr "mode" "V2SF")])

(define_insn "*vec_dupv2sf"
  [(set (match_operand:V2SF 0 "register_operand" "=y")
	(vec_duplicate:V2SF
	  (match_operand:SF 1 "register_operand" "0")))]
  "TARGET_MMX"
  "punpckldq\t%0, %0"
  [(set_attr "type" "mmxcvt")
   (set_attr "mode" "DI")])

(define_insn "*mmx_concatv2sf"
  [(set (match_operand:V2SF 0 "register_operand"     "=y,y")
	(vec_concat:V2SF
	  (match_operand:SF 1 "nonimmediate_operand" " 0,rm")
	  (match_operand:SF 2 "vector_move_operand"  "ym,C")))]
  "TARGET_MMX && !TARGET_SSE"
  "@
   punpckldq\t{%2, %0|%0, %2}
   movd\t{%1, %0|%0, %1}"
  [(set_attr "type" "mmxcvt,mmxmov")
   (set_attr "mode" "DI")])

(define_expand "vec_setv2sf"
  [(match_operand:V2SF 0 "register_operand")
   (match_operand:SF 1 "register_operand")
   (match_operand 2 "const_int_operand")]
  "TARGET_MMX"
{
  ix86_expand_vector_set (false, operands[0], operands[1],
			  INTVAL (operands[2]));
  DONE;
})

;; Avoid combining registers from different units in a single alternative,
;; see comment above inline_secondary_memory_needed function in i386.c
(define_insn_and_split "*vec_extractv2sf_0"
  [(set (match_operand:SF 0 "nonimmediate_operand"     "=x, m,y ,m,f,r")
	(vec_select:SF
	  (match_operand:V2SF 1 "nonimmediate_operand" " xm,x,ym,y,m,m")
	  (parallel [(const_int 0)])))]
  "TARGET_MMX && !(MEM_P (operands[0]) && MEM_P (operands[1]))"
  "#"
  "&& reload_completed"
  [(const_int 0)]
{
  rtx op1 = operands[1];
  if (REG_P (op1))
    op1 = gen_rtx_REG (SFmode, REGNO (op1));
  else
    op1 = gen_lowpart (SFmode, op1);
  emit_move_insn (operands[0], op1);
  DONE;
})

;; Avoid combining registers from different units in a single alternative,
;; see comment above inline_secondary_memory_needed function in i386.c
(define_insn "*vec_extractv2sf_1"
  [(set (match_operand:SF 0 "nonimmediate_operand"     "=y,x,y,x,f,r")
	(vec_select:SF
	  (match_operand:V2SF 1 "nonimmediate_operand" " 0,0,o,o,o,o")
	  (parallel [(const_int 1)])))]
  "TARGET_MMX && !(MEM_P (operands[0]) && MEM_P (operands[1]))"
  "@
   punpckhdq\t%0, %0
   unpckhps\t%0, %0
   #
   #
   #
   #"
  [(set_attr "type" "mmxcvt,sselog1,mmxmov,ssemov,fmov,imov")
   (set_attr "mode" "DI,V4SF,SF,SF,SF,SF")])

(define_split
  [(set (match_operand:SF 0 "register_operand")
	(vec_select:SF
	  (match_operand:V2SF 1 "memory_operand")
	  (parallel [(const_int 1)])))]
  "TARGET_MMX && reload_completed"
  [(const_int 0)]
{
  operands[1] = adjust_address (operands[1], SFmode, 4);
  emit_move_insn (operands[0], operands[1]);
  DONE;
})

(define_expand "vec_extractv2sf"
  [(match_operand:SF 0 "register_operand")
   (match_operand:V2SF 1 "register_operand")
   (match_operand 2 "const_int_operand")]
  "TARGET_MMX"
{
  ix86_expand_vector_extract (false, operands[0], operands[1],
			      INTVAL (operands[2]));
  DONE;
})

(define_expand "vec_initv2sf"
  [(match_operand:V2SF 0 "register_operand")
   (match_operand 1)]
  "TARGET_SSE"
{
  ix86_expand_vector_init (false, operands[0], operands[1]);
  DONE;
})

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Parallel integral arithmetic
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define_expand "mmx_<plusminus_insn><mode>3"
  [(set (match_operand:MMXMODEI8 0 "register_operand")
	(plusminus:MMXMODEI8
	  (match_operand:MMXMODEI8 1 "nonimmediate_operand")
	  (match_operand:MMXMODEI8 2 "nonimmediate_operand")))]
  "TARGET_MMX || (TARGET_SSE2 && <MODE>mode == V1DImode)"
  "ix86_fixup_binary_operands_no_copy (<CODE>, <MODE>mode, operands);")

(define_insn "*mmx_<plusminus_insn><mode>3"
  [(set (match_operand:MMXMODEI8 0 "register_operand" "=y")
        (plusminus:MMXMODEI8
	  (match_operand:MMXMODEI8 1 "nonimmediate_operand" "<comm>0")
	  (match_operand:MMXMODEI8 2 "nonimmediate_operand" "ym")))]
  "(TARGET_MMX || (TARGET_SSE2 && <MODE>mode == V1DImode))
   && ix86_binary_operator_ok (<CODE>, <MODE>mode, operands)"
  "p<plusminus_mnemonic><mmxvecsize>\t{%2, %0|%0, %2}"
  [(set_attr "type" "mmxadd")
   (set_attr "mode" "DI")])

(define_expand "mmx_<plusminus_insn><mode>3"
  [(set (match_operand:MMXMODE12 0 "register_operand")
	(sat_plusminus:MMXMODE12
	  (match_operand:MMXMODE12 1 "nonimmediate_operand")
	  (match_operand:MMXMODE12 2 "nonimmediate_operand")))]
  "TARGET_MMX"
  "ix86_fixup_binary_operands_no_copy (<CODE>, <MODE>mode, operands);")

(define_insn "*mmx_<plusminus_insn><mode>3"
  [(set (match_operand:MMXMODE12 0 "register_operand" "=y")
        (sat_plusminus:MMXMODE12
	  (match_operand:MMXMODE12 1 "nonimmediate_operand" "<comm>0")
	  (match_operand:MMXMODE12 2 "nonimmediate_operand" "ym")))]
  "TARGET_MMX && ix86_binary_operator_ok (<CODE>, <MODE>mode, operands)"
  "p<plusminus_mnemonic><mmxvecsize>\t{%2, %0|%0, %2}"
  [(set_attr "type" "mmxadd")
   (set_attr "mode" "DI")])

(define_expand "mmx_mulv4hi3"
  [(set (match_operand:V4HI 0 "register_operand")
        (mult:V4HI (match_operand:V4HI 1 "nonimmediate_operand")
		   (match_operand:V4HI 2 "nonimmediate_operand")))]
  "TARGET_MMX"
  "ix86_fixup_binary_operands_no_copy (MULT, V4HImode, operands);")

(define_insn "*mmx_mulv4hi3"
  [(set (match_operand:V4HI 0 "register_operand" "=y")
        (mult:V4HI (match_operand:V4HI 1 "nonimmediate_operand" "%0")
		   (match_operand:V4HI 2 "nonimmediate_operand" "ym")))]
  "TARGET_MMX && ix86_binary_operator_ok (MULT, V4HImode, operands)"
  "pmullw\t{%2, %0|%0, %2}"
  [(set_attr "type" "mmxmul")
   (set_attr "mode" "DI")])

(define_expand "mmx_smulv4hi3_highpart"
  [(set (match_operand:V4HI 0 "register_operand")
	(truncate:V4HI
	  (lshiftrt:V4SI
	    (mult:V4SI
	      (sign_extend:V4SI
		(match_operand:V4HI 1 "nonimmediate_operand"))
	      (sign_extend:V4SI
		(match_operand:V4HI 2 "nonimmediate_operand")))
	    (const_int 16))))]
  "TARGET_MMX"
  "ix86_fixup_binary_operands_no_copy (MULT, V4HImode, operands);")

(define_insn "*mmx_smulv4hi3_highpart"
  [(set (match_operand:V4HI 0 "register_operand" "=y")
	(truncate:V4HI
	  (lshiftrt:V4SI
	    (mult:V4SI
	      (sign_extend:V4SI
		(match_operand:V4HI 1 "nonimmediate_operand" "%0"))
	      (sign_extend:V4SI
		(match_operand:V4HI 2 "nonimmediate_operand" "ym")))
	    (const_int 16))))]
  "TARGET_MMX && ix86_binary_operator_ok (MULT, V4HImode, operands)"
  "pmulhw\t{%2, %0|%0, %2}"
  [(set_attr "type" "mmxmul")
   (set_attr "mode" "DI")])

(define_expand "mmx_umulv4hi3_highpart"
  [(set (match_operand:V4HI 0 "register_operand")
	(truncate:V4HI
	  (lshiftrt:V4SI
	    (mult:V4SI
	      (zero_extend:V4SI
		(match_operand:V4HI 1 "nonimmediate_operand"))
	      (zero_extend:V4SI
		(match_operand:V4HI 2 "nonimmediate_operand")))
	    (const_int 16))))]
  "TARGET_SSE || TARGET_3DNOW_A"
  "ix86_fixup_binary_operands_no_copy (MULT, V4HImode, operands);")

(define_insn "*mmx_umulv4hi3_highpart"
  [(set (match_operand:V4HI 0 "register_operand" "=y")
	(truncate:V4HI
	  (lshiftrt:V4SI
	    (mult:V4SI
	      (zero_extend:V4SI
		(match_operand:V4HI 1 "nonimmediate_operand" "%0"))
	      (zero_extend:V4SI
		(match_operand:V4HI 2 "nonimmediate_operand" "ym")))
	  (const_int 16))))]
  "(TARGET_SSE || TARGET_3DNOW_A)
   && ix86_binary_operator_ok (MULT, V4HImode, operands)"
  "pmulhuw\t{%2, %0|%0, %2}"
  [(set_attr "type" "mmxmul")
   (set_attr "mode" "DI")])

(define_expand "mmx_pmaddwd"
  [(set (match_operand:V2SI 0 "register_operand")
        (plus:V2SI
	  (mult:V2SI
	    (sign_extend:V2SI
	      (vec_select:V2HI
		(match_operand:V4HI 1 "nonimmediate_operand")
		(parallel [(const_int 0) (const_int 2)])))
	    (sign_extend:V2SI
	      (vec_select:V2HI
		(match_operand:V4HI 2 "nonimmediate_operand")
		(parallel [(const_int 0) (const_int 2)]))))
	  (mult:V2SI
	    (sign_extend:V2SI
	      (vec_select:V2HI (match_dup 1)
		(parallel [(const_int 1) (const_int 3)])))
	    (sign_extend:V2SI
	      (vec_select:V2HI (match_dup 2)
		(parallel [(const_int 1) (const_int 3)]))))))]
  "TARGET_MMX"
  "ix86_fixup_binary_operands_no_copy (MULT, V4HImode, operands);")

(define_insn "*mmx_pmaddwd"
  [(set (match_operand:V2SI 0 "register_operand" "=y")
        (plus:V2SI
	  (mult:V2SI
	    (sign_extend:V2SI
	      (vec_select:V2HI
		(match_operand:V4HI 1 "nonimmediate_operand" "%0")
		(parallel [(const_int 0) (const_int 2)])))
	    (sign_extend:V2SI
	      (vec_select:V2HI
		(match_operand:V4HI 2 "nonimmediate_operand" "ym")
		(parallel [(const_int 0) (const_int 2)]))))
	  (mult:V2SI
	    (sign_extend:V2SI
	      (vec_select:V2HI (match_dup 1)
		(parallel [(const_int 1) (const_int 3)])))
	    (sign_extend:V2SI
	      (vec_select:V2HI (match_dup 2)
		(parallel [(const_int 1) (const_int 3)]))))))]
  "TARGET_MMX && ix86_binary_operator_ok (MULT, V4HImode, operands)"
  "pmaddwd\t{%2, %0|%0, %2}"
  [(set_attr "type" "mmxmul")
   (set_attr "mode" "DI")])

(define_expand "mmx_pmulhrwv4hi3"
  [(set (match_operand:V4HI 0 "register_operand")
	(truncate:V4HI
	  (lshiftrt:V4SI
	    (plus:V4SI
	      (mult:V4SI
	        (sign_extend:V4SI
		  (match_operand:V4HI 1 "nonimmediate_operand"))
	        (sign_extend:V4SI
		  (match_operand:V4HI 2 "nonimmediate_operand")))
	      (const_vector:V4SI [(const_int 32768) (const_int 32768)
				  (const_int 32768) (const_int 32768)]))
	    (const_int 16))))]
  "TARGET_3DNOW"
  "ix86_fixup_binary_operands_no_copy (MULT, V4HImode, operands);")

(define_insn "*mmx_pmulhrwv4hi3"
  [(set (match_operand:V4HI 0 "register_operand" "=y")
	(truncate:V4HI
	  (lshiftrt:V4SI
	    (plus:V4SI
	      (mult:V4SI
	        (sign_extend:V4SI
		  (match_operand:V4HI 1 "nonimmediate_operand" "%0"))
	        (sign_extend:V4SI
		  (match_operand:V4HI 2 "nonimmediate_operand" "ym")))
	      (const_vector:V4SI [(const_int 32768) (const_int 32768)
				  (const_int 32768) (const_int 32768)]))
	    (const_int 16))))]
  "TARGET_3DNOW && ix86_binary_operator_ok (MULT, V4HImode, operands)"
  "pmulhrw\t{%2, %0|%0, %2}"
  [(set_attr "type" "mmxmul")
   (set_attr "prefix_extra" "1")
   (set_attr "mode" "DI")])

(define_expand "sse2_umulv1siv1di3"
  [(set (match_operand:V1DI 0 "register_operand")
        (mult:V1DI
	  (zero_extend:V1DI
	    (vec_select:V1SI
	      (match_operand:V2SI 1 "nonimmediate_operand")
	      (parallel [(const_int 0)])))
	  (zero_extend:V1DI
	    (vec_select:V1SI
	      (match_operand:V2SI 2 "nonimmediate_operand")
	      (parallel [(const_int 0)])))))]
  "TARGET_SSE2"
  "ix86_fixup_binary_operands_no_copy (MULT, V2SImode, operands);")

(define_insn "*sse2_umulv1siv1di3"
  [(set (match_operand:V1DI 0 "register_operand" "=y")
        (mult:V1DI
	  (zero_extend:V1DI
	    (vec_select:V1SI
	      (match_operand:V2SI 1 "nonimmediate_operand" "%0")
	      (parallel [(const_int 0)])))
	  (zero_extend:V1DI
	    (vec_select:V1SI
	      (match_operand:V2SI 2 "nonimmediate_operand" "ym")
	      (parallel [(const_int 0)])))))]
  "TARGET_SSE2 && ix86_binary_operator_ok (MULT, V2SImode, operands)"
  "pmuludq\t{%2, %0|%0, %2}"
  [(set_attr "type" "mmxmul")
   (set_attr "mode" "DI")])

(define_expand "mmx_<code>v4hi3"
  [(set (match_operand:V4HI 0 "register_operand")
        (smaxmin:V4HI
	  (match_operand:V4HI 1 "nonimmediate_operand")
	  (match_operand:V4HI 2 "nonimmediate_operand")))]
  "TARGET_SSE || TARGET_3DNOW_A"
  "ix86_fixup_binary_operands_no_copy (<CODE>, V4HImode, operands);")

(define_insn "*mmx_<code>v4hi3"
  [(set (match_operand:V4HI 0 "register_operand" "=y")
        (smaxmin:V4HI
	  (match_operand:V4HI 1 "nonimmediate_operand" "%0")
	  (match_operand:V4HI 2 "nonimmediate_operand" "ym")))]
  "(TARGET_SSE || TARGET_3DNOW_A)
   && ix86_binary_operator_ok (<CODE>, V4HImode, operands)"
  "p<maxmin_int>w\t{%2, %0|%0, %2}"
  [(set_attr "type" "mmxadd")
   (set_attr "mode" "DI")])

(define_expand "mmx_<code>v8qi3"
  [(set (match_operand:V8QI 0 "register_operand")
        (umaxmin:V8QI
	  (match_operand:V8QI 1 "nonimmediate_operand")
	  (match_operand:V8QI 2 "nonimmediate_operand")))]
  "TARGET_SSE || TARGET_3DNOW_A"
  "ix86_fixup_binary_operands_no_copy (<CODE>, V8QImode, operands);")

(define_insn "*mmx_<code>v8qi3"
  [(set (match_operand:V8QI 0 "register_operand" "=y")
        (umaxmin:V8QI
	  (match_operand:V8QI 1 "nonimmediate_operand" "%0")
	  (match_operand:V8QI 2 "nonimmediate_operand" "ym")))]
  "(TARGET_SSE || TARGET_3DNOW_A)
   && ix86_binary_operator_ok (<CODE>, V8QImode, operands)"
  "p<maxmin_int>b\t{%2, %0|%0, %2}"
  [(set_attr "type" "mmxadd")
   (set_attr "mode" "DI")])

(define_insn "mmx_ashr<mode>3"
  [(set (match_operand:MMXMODE24 0 "register_operand" "=y")
        (ashiftrt:MMXMODE24
	  (match_operand:MMXMODE24 1 "register_operand" "0")
	  (match_operand:SI 2 "nonmemory_operand" "yN")))]
  "TARGET_MMX"
  "psra<mmxvecsize>\t{%2, %0|%0, %2}"
  [(set_attr "type" "mmxshft")
   (set (attr "length_immediate")
     (if_then_else (match_operand 2 "const_int_operand")
       (const_string "1")
       (const_string "0")))
   (set_attr "mode" "DI")])

(define_insn "mmx_<shift_insn><mode>3"
  [(set (match_operand:MMXMODE248 0 "register_operand" "=y")
        (any_lshift:MMXMODE248
	  (match_operand:MMXMODE248 1 "register_operand" "0")
	  (match_operand:SI 2 "nonmemory_operand" "yN")))]
  "TARGET_MMX"
  "p<vshift><mmxvecsize>\t{%2, %0|%0, %2}"
  [(set_attr "type" "mmxshft")
   (set (attr "length_immediate")
     (if_then_else (match_operand 2 "const_int_operand")
       (const_string "1")
       (const_string "0")))
   (set_attr "mode" "DI")])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Parallel integral comparisons
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define_expand "mmx_eq<mode>3"
  [(set (match_operand:MMXMODEI 0 "register_operand")
        (eq:MMXMODEI
	  (match_operand:MMXMODEI 1 "nonimmediate_operand")
	  (match_operand:MMXMODEI 2 "nonimmediate_operand")))]
  "TARGET_MMX"
  "ix86_fixup_binary_operands_no_copy (EQ, <MODE>mode, operands);")

(define_insn "*mmx_eq<mode>3"
  [(set (match_operand:MMXMODEI 0 "register_operand" "=y")
        (eq:MMXMODEI
	  (match_operand:MMXMODEI 1 "nonimmediate_operand" "%0")
	  (match_operand:MMXMODEI 2 "nonimmediate_operand" "ym")))]
  "TARGET_MMX && ix86_binary_operator_ok (EQ, <MODE>mode, operands)"
  "pcmpeq<mmxvecsize>\t{%2, %0|%0, %2}"
  [(set_attr "type" "mmxcmp")
   (set_attr "mode" "DI")])

(define_insn "mmx_gt<mode>3"
  [(set (match_operand:MMXMODEI 0 "register_operand" "=y")
        (gt:MMXMODEI
	  (match_operand:MMXMODEI 1 "register_operand" "0")
	  (match_operand:MMXMODEI 2 "nonimmediate_operand" "ym")))]
  "TARGET_MMX"
  "pcmpgt<mmxvecsize>\t{%2, %0|%0, %2}"
  [(set_attr "type" "mmxcmp")
   (set_attr "mode" "DI")])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Parallel integral logical operations
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define_insn "mmx_andnot<mode>3"
  [(set (match_operand:MMXMODEI 0 "register_operand" "=y")
	(and:MMXMODEI
	  (not:MMXMODEI (match_operand:MMXMODEI 1 "register_operand" "0"))
	  (match_operand:MMXMODEI 2 "nonimmediate_operand" "ym")))]
  "TARGET_MMX"
  "pandn\t{%2, %0|%0, %2}"
  [(set_attr "type" "mmxadd")
   (set_attr "mode" "DI")])

(define_expand "mmx_<code><mode>3"
  [(set (match_operand:MMXMODEI 0 "register_operand")
	(any_logic:MMXMODEI
	  (match_operand:MMXMODEI 1 "nonimmediate_operand")
	  (match_operand:MMXMODEI 2 "nonimmediate_operand")))]
  "TARGET_MMX"
  "ix86_fixup_binary_operands_no_copy (<CODE>, <MODE>mode, operands);")

(define_insn "*mmx_<code><mode>3"
  [(set (match_operand:MMXMODEI 0 "register_operand" "=y")
        (any_logic:MMXMODEI
	  (match_operand:MMXMODEI 1 "nonimmediate_operand" "%0")
	  (match_operand:MMXMODEI 2 "nonimmediate_operand" "ym")))]
  "TARGET_MMX && ix86_binary_operator_ok (<CODE>, <MODE>mode, operands)"
  "p<logic>\t{%2, %0|%0, %2}"
  [(set_attr "type" "mmxadd")
   (set_attr "mode" "DI")])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Parallel integral element swizzling
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define_insn "mmx_packsswb"
  [(set (match_operand:V8QI 0 "register_operand" "=y")
	(vec_concat:V8QI
	  (ss_truncate:V4QI
	    (match_operand:V4HI 1 "register_operand" "0"))
	  (ss_truncate:V4QI
	    (match_operand:V4HI 2 "nonimmediate_operand" "ym"))))]
  "TARGET_MMX"
  "packsswb\t{%2, %0|%0, %2}"
  [(set_attr "type" "mmxshft")
   (set_attr "mode" "DI")])

(define_insn "mmx_packssdw"
  [(set (match_operand:V4HI 0 "register_operand" "=y")
	(vec_concat:V4HI
	  (ss_truncate:V2HI
	    (match_operand:V2SI 1 "register_operand" "0"))
	  (ss_truncate:V2HI
	    (match_operand:V2SI 2 "nonimmediate_operand" "ym"))))]
  "TARGET_MMX"
  "packssdw\t{%2, %0|%0, %2}"
  [(set_attr "type" "mmxshft")
   (set_attr "mode" "DI")])

(define_insn "mmx_packuswb"
  [(set (match_operand:V8QI 0 "register_operand" "=y")
	(vec_concat:V8QI
	  (us_truncate:V4QI
	    (match_operand:V4HI 1 "register_operand" "0"))
	  (us_truncate:V4QI
	    (match_operand:V4HI 2 "nonimmediate_operand" "ym"))))]
  "TARGET_MMX"
  "packuswb\t{%2, %0|%0, %2}"
  [(set_attr "type" "mmxshft")
   (set_attr "mode" "DI")])

(define_insn "mmx_punpckhbw"
  [(set (match_operand:V8QI 0 "register_operand" "=y")
	(vec_select:V8QI
	  (vec_concat:V16QI
	    (match_operand:V8QI 1 "register_operand" "0")
	    (match_operand:V8QI 2 "nonimmediate_operand" "ym"))
          (parallel [(const_int 4) (const_int 12)
                     (const_int 5) (const_int 13)
                     (const_int 6) (const_int 14)
                     (const_int 7) (const_int 15)])))]
  "TARGET_MMX"
  "punpckhbw\t{%2, %0|%0, %2}"
  [(set_attr "type" "mmxcvt")
   (set_attr "mode" "DI")])

(define_insn "mmx_punpcklbw"
  [(set (match_operand:V8QI 0 "register_operand" "=y")
	(vec_select:V8QI
	  (vec_concat:V16QI
	    (match_operand:V8QI 1 "register_operand" "0")
	    (match_operand:V8QI 2 "nonimmediate_operand" "ym"))
          (parallel [(const_int 0) (const_int 8)
                     (const_int 1) (const_int 9)
                     (const_int 2) (const_int 10)
                     (const_int 3) (const_int 11)])))]
  "TARGET_MMX"
  "punpcklbw\t{%2, %0|%0, %2}"
  [(set_attr "type" "mmxcvt")
   (set_attr "mode" "DI")])

(define_insn "mmx_punpckhwd"
  [(set (match_operand:V4HI 0 "register_operand" "=y")
	(vec_select:V4HI
	  (vec_concat:V8HI
	    (match_operand:V4HI 1 "register_operand" "0")
	    (match_operand:V4HI 2 "nonimmediate_operand" "ym"))
          (parallel [(const_int 2) (const_int 6)
                     (const_int 3) (const_int 7)])))]
  "TARGET_MMX"
  "punpckhwd\t{%2, %0|%0, %2}"
  [(set_attr "type" "mmxcvt")
   (set_attr "mode" "DI")])

(define_insn "mmx_punpcklwd"
  [(set (match_operand:V4HI 0 "register_operand" "=y")
	(vec_select:V4HI
	  (vec_concat:V8HI
	    (match_operand:V4HI 1 "register_operand" "0")
	    (match_operand:V4HI 2 "nonimmediate_operand" "ym"))
          (parallel [(const_int 0) (const_int 4)
                     (const_int 1) (const_int 5)])))]
  "TARGET_MMX"
  "punpcklwd\t{%2, %0|%0, %2}"
  [(set_attr "type" "mmxcvt")
   (set_attr "mode" "DI")])

(define_insn "mmx_punpckhdq"
  [(set (match_operand:V2SI 0 "register_operand" "=y")
	(vec_select:V2SI
	  (vec_concat:V4SI
	    (match_operand:V2SI 1 "register_operand" "0")
	    (match_operand:V2SI 2 "nonimmediate_operand" "ym"))
	  (parallel [(const_int 1)
		     (const_int 3)])))]
  "TARGET_MMX"
  "punpckhdq\t{%2, %0|%0, %2}"
  [(set_attr "type" "mmxcvt")
   (set_attr "mode" "DI")])

(define_insn "mmx_punpckldq"
  [(set (match_operand:V2SI 0 "register_operand" "=y")
	(vec_select:V2SI
	  (vec_concat:V4SI
	    (match_operand:V2SI 1 "register_operand" "0")
	    (match_operand:V2SI 2 "nonimmediate_operand" "ym"))
	  (parallel [(const_int 0)
		     (const_int 2)])))]
  "TARGET_MMX"
  "punpckldq\t{%2, %0|%0, %2}"
  [(set_attr "type" "mmxcvt")
   (set_attr "mode" "DI")])

(define_expand "mmx_pinsrw"
  [(set (match_operand:V4HI 0 "register_operand")
        (vec_merge:V4HI
          (vec_duplicate:V4HI
            (match_operand:SI 2 "nonimmediate_operand"))
	  (match_operand:V4HI 1 "register_operand")
          (match_operand:SI 3 "const_0_to_3_operand")))]
  "TARGET_SSE || TARGET_3DNOW_A"
{
  operands[2] = gen_lowpart (HImode, operands[2]);
  operands[3] = GEN_INT (1 << INTVAL (operands[3]));
})

(define_insn "*mmx_pinsrw"
  [(set (match_operand:V4HI 0 "register_operand" "=y")
        (vec_merge:V4HI
          (vec_duplicate:V4HI
            (match_operand:HI 2 "nonimmediate_operand" "rm"))
	  (match_operand:V4HI 1 "register_operand" "0")
          (match_operand:SI 3 "const_int_operand")))]
  "(TARGET_SSE || TARGET_3DNOW_A)
   && ((unsigned) exact_log2 (INTVAL (operands[3]))
       < GET_MODE_NUNITS (V4HImode))"
{
  operands[3] = GEN_INT (exact_log2 (INTVAL (operands[3])));
  if (MEM_P (operands[2]))
    return "pinsrw\t{%3, %2, %0|%0, %2, %3}";
  else
    return "pinsrw\t{%3, %k2, %0|%0, %k2, %3}";
}
  [(set_attr "type" "mmxcvt")
   (set_attr "length_immediate" "1")
   (set_attr "mode" "DI")])

(define_insn "mmx_pextrw"
  [(set (match_operand:SI 0 "register_operand" "=r")
        (zero_extend:SI
	  (vec_select:HI
	    (match_operand:V4HI 1 "register_operand" "y")
	    (parallel [(match_operand:SI 2 "const_0_to_3_operand" "n")]))))]
  "TARGET_SSE || TARGET_3DNOW_A"
  "pextrw\t{%2, %1, %0|%0, %1, %2}"
  [(set_attr "type" "mmxcvt")
   (set_attr "length_immediate" "1")
   (set_attr "mode" "DI")])

(define_expand "mmx_pshufw"
  [(match_operand:V4HI 0 "register_operand")
   (match_operand:V4HI 1 "nonimmediate_operand")
   (match_operand:SI 2 "const_int_operand")]
  "TARGET_SSE || TARGET_3DNOW_A"
{
  int mask = INTVAL (operands[2]);
  emit_insn (gen_mmx_pshufw_1 (operands[0], operands[1],
                               GEN_INT ((mask >> 0) & 3),
                               GEN_INT ((mask >> 2) & 3),
                               GEN_INT ((mask >> 4) & 3),
                               GEN_INT ((mask >> 6) & 3)));
  DONE;
})

(define_insn "mmx_pshufw_1"
  [(set (match_operand:V4HI 0 "register_operand" "=y")
        (vec_select:V4HI
          (match_operand:V4HI 1 "nonimmediate_operand" "ym")
          (parallel [(match_operand 2 "const_0_to_3_operand")
                     (match_operand 3 "const_0_to_3_operand")
                     (match_operand 4 "const_0_to_3_operand")
                     (match_operand 5 "const_0_to_3_operand")])))]
  "TARGET_SSE || TARGET_3DNOW_A"
{
  int mask = 0;
  mask |= INTVAL (operands[2]) << 0;
  mask |= INTVAL (operands[3]) << 2;
  mask |= INTVAL (operands[4]) << 4;
  mask |= INTVAL (operands[5]) << 6;
  operands[2] = GEN_INT (mask);

  return "pshufw\t{%2, %1, %0|%0, %1, %2}";
}
  [(set_attr "type" "mmxcvt")
   (set_attr "length_immediate" "1")
   (set_attr "mode" "DI")])

(define_insn "mmx_pswapdv2si2"
  [(set (match_operand:V2SI 0 "register_operand" "=y")
	(vec_select:V2SI
	  (match_operand:V2SI 1 "nonimmediate_operand" "ym")
	  (parallel [(const_int 1) (const_int 0)])))]
  "TARGET_3DNOW_A"
  "pswapd\t{%1, %0|%0, %1}"
  [(set_attr "type" "mmxcvt")
   (set_attr "prefix_extra" "1")
   (set_attr "mode" "DI")])

(define_insn "*vec_dupv4hi"
  [(set (match_operand:V4HI 0 "register_operand" "=y")
	(vec_duplicate:V4HI
	  (truncate:HI
	    (match_operand:SI 1 "register_operand" "0"))))]
  "TARGET_SSE || TARGET_3DNOW_A"
  "pshufw\t{$0, %0, %0|%0, %0, 0}"
  [(set_attr "type" "mmxcvt")
   (set_attr "length_immediate" "1")
   (set_attr "mode" "DI")])

(define_insn "*vec_dupv2si"
  [(set (match_operand:V2SI 0 "register_operand" "=y")
	(vec_duplicate:V2SI
	  (match_operand:SI 1 "register_operand" "0")))]
  "TARGET_MMX"
  "punpckldq\t%0, %0"
  [(set_attr "type" "mmxcvt")
   (set_attr "mode" "DI")])

(define_insn "*mmx_concatv2si"
  [(set (match_operand:V2SI 0 "register_operand"     "=y,y")
	(vec_concat:V2SI
	  (match_operand:SI 1 "nonimmediate_operand" " 0,rm")
	  (match_operand:SI 2 "vector_move_operand"  "ym,C")))]
  "TARGET_MMX && !TARGET_SSE"
  "@
   punpckldq\t{%2, %0|%0, %2}
   movd\t{%1, %0|%0, %1}"
  [(set_attr "type" "mmxcvt,mmxmov")
   (set_attr "mode" "DI")])

(define_expand "vec_setv2si"
  [(match_operand:V2SI 0 "register_operand")
   (match_operand:SI 1 "register_operand")
   (match_operand 2 "const_int_operand")]
  "TARGET_MMX"
{
  ix86_expand_vector_set (false, operands[0], operands[1],
			  INTVAL (operands[2]));
  DONE;
})

;; Avoid combining registers from different units in a single alternative,
;; see comment above inline_secondary_memory_needed function in i386.c
(define_insn_and_split "*vec_extractv2si_0"
  [(set (match_operand:SI 0 "nonimmediate_operand"     "=x,m,y, m,r")
	(vec_select:SI
	  (match_operand:V2SI 1 "nonimmediate_operand" "xm,x,ym,y,m")
	  (parallel [(const_int 0)])))]
  "TARGET_MMX && !(MEM_P (operands[0]) && MEM_P (operands[1]))"
  "#"
  "&& reload_completed"
  [(const_int 0)]
{
  rtx op1 = operands[1];
  if (REG_P (op1))
    op1 = gen_rtx_REG (SImode, REGNO (op1));
  else
    op1 = gen_lowpart (SImode, op1);
  emit_move_insn (operands[0], op1);
  DONE;
})

;; Avoid combining registers from different units in a single alternative,
;; see comment above inline_secondary_memory_needed function in i386.c
(define_insn "*vec_extractv2si_1"
  [(set (match_operand:SI 0 "nonimmediate_operand"     "=y,x,x,x,y,x,r")
	(vec_select:SI
	  (match_operand:V2SI 1 "nonimmediate_operand" " 0,0,x,0,o,o,o")
	  (parallel [(const_int 1)])))]
  "TARGET_MMX && !(MEM_P (operands[0]) && MEM_P (operands[1]))"
  "@
   punpckhdq\t%0, %0
   punpckhdq\t%0, %0
   pshufd\t{$85, %1, %0|%0, %1, 85}
   unpckhps\t%0, %0
   #
   #
   #"
  [(set (attr "isa")
     (if_then_else (eq_attr "alternative" "1,2")
       (const_string "sse2")
       (const_string "*")))
   (set_attr "type" "mmxcvt,sselog1,sselog1,sselog1,mmxmov,ssemov,imov")
   (set_attr "length_immediate" "*,*,1,*,*,*,*")
   (set_attr "mode" "DI,TI,TI,V4SF,SI,SI,SI")])

(define_split
  [(set (match_operand:SI 0 "register_operand")
	(vec_select:SI
	  (match_operand:V2SI 1 "memory_operand")
	  (parallel [(const_int 1)])))]
  "TARGET_MMX && reload_completed"
  [(const_int 0)]
{
  operands[1] = adjust_address (operands[1], SImode, 4);
  emit_move_insn (operands[0], operands[1]);
  DONE;
})

(define_expand "vec_extractv2si"
  [(match_operand:SI 0 "register_operand")
   (match_operand:V2SI 1 "register_operand")
   (match_operand 2 "const_int_operand")]
  "TARGET_MMX"
{
  ix86_expand_vector_extract (false, operands[0], operands[1],
			      INTVAL (operands[2]));
  DONE;
})

(define_expand "vec_initv2si"
  [(match_operand:V2SI 0 "register_operand")
   (match_operand 1)]
  "TARGET_SSE"
{
  ix86_expand_vector_init (false, operands[0], operands[1]);
  DONE;
})

(define_expand "vec_setv4hi"
  [(match_operand:V4HI 0 "register_operand")
   (match_operand:HI 1 "register_operand")
   (match_operand 2 "const_int_operand")]
  "TARGET_MMX"
{
  ix86_expand_vector_set (false, operands[0], operands[1],
			  INTVAL (operands[2]));
  DONE;
})

(define_expand "vec_extractv4hi"
  [(match_operand:HI 0 "register_operand")
   (match_operand:V4HI 1 "register_operand")
   (match_operand 2 "const_int_operand")]
  "TARGET_MMX"
{
  ix86_expand_vector_extract (false, operands[0], operands[1],
			      INTVAL (operands[2]));
  DONE;
})

(define_expand "vec_initv4hi"
  [(match_operand:V4HI 0 "register_operand")
   (match_operand 1)]
  "TARGET_SSE"
{
  ix86_expand_vector_init (false, operands[0], operands[1]);
  DONE;
})

(define_expand "vec_setv8qi"
  [(match_operand:V8QI 0 "register_operand")
   (match_operand:QI 1 "register_operand")
   (match_operand 2 "const_int_operand")]
  "TARGET_MMX"
{
  ix86_expand_vector_set (false, operands[0], operands[1],
			  INTVAL (operands[2]));
  DONE;
})

(define_expand "vec_extractv8qi"
  [(match_operand:QI 0 "register_operand")
   (match_operand:V8QI 1 "register_operand")
   (match_operand 2 "const_int_operand")]
  "TARGET_MMX"
{
  ix86_expand_vector_extract (false, operands[0], operands[1],
			      INTVAL (operands[2]));
  DONE;
})

(define_expand "vec_initv8qi"
  [(match_operand:V8QI 0 "register_operand")
   (match_operand 1)]
  "TARGET_SSE"
{
  ix86_expand_vector_init (false, operands[0], operands[1]);
  DONE;
})

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Miscellaneous
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define_expand "mmx_uavgv8qi3"
  [(set (match_operand:V8QI 0 "register_operand")
	(truncate:V8QI
	  (lshiftrt:V8HI
	    (plus:V8HI
	      (plus:V8HI
		(zero_extend:V8HI
		  (match_operand:V8QI 1 "nonimmediate_operand"))
		(zero_extend:V8HI
		  (match_operand:V8QI 2 "nonimmediate_operand")))
	      (const_vector:V8HI [(const_int 1) (const_int 1)
				  (const_int 1) (const_int 1)
				  (const_int 1) (const_int 1)
				  (const_int 1) (const_int 1)]))
	    (const_int 1))))]
  "TARGET_SSE || TARGET_3DNOW"
  "ix86_fixup_binary_operands_no_copy (PLUS, V8QImode, operands);")

(define_insn "*mmx_uavgv8qi3"
  [(set (match_operand:V8QI 0 "register_operand" "=y")
	(truncate:V8QI
	  (lshiftrt:V8HI
	    (plus:V8HI
	      (plus:V8HI
		(zero_extend:V8HI
		  (match_operand:V8QI 1 "nonimmediate_operand" "%0"))
		(zero_extend:V8HI
		  (match_operand:V8QI 2 "nonimmediate_operand" "ym")))
	      (const_vector:V8HI [(const_int 1) (const_int 1)
				  (const_int 1) (const_int 1)
				  (const_int 1) (const_int 1)
				  (const_int 1) (const_int 1)]))
	    (const_int 1))))]
  "(TARGET_SSE || TARGET_3DNOW)
   && ix86_binary_operator_ok (PLUS, V8QImode, operands)"
{
  /* These two instructions have the same operation, but their encoding
     is different.  Prefer the one that is de facto standard.  */
  if (TARGET_SSE || TARGET_3DNOW_A)
    return "pavgb\t{%2, %0|%0, %2}";
  else
    return "pavgusb\t{%2, %0|%0, %2}";
}
  [(set_attr "type" "mmxshft")
   (set (attr "prefix_extra")
     (if_then_else
       (not (ior (match_test "TARGET_SSE")
		 (match_test "TARGET_3DNOW_A")))
       (const_string "1")
       (const_string "*")))
   (set_attr "mode" "DI")])

(define_expand "mmx_uavgv4hi3"
  [(set (match_operand:V4HI 0 "register_operand")
	(truncate:V4HI
	  (lshiftrt:V4SI
	    (plus:V4SI
	      (plus:V4SI
		(zero_extend:V4SI
		  (match_operand:V4HI 1 "nonimmediate_operand"))
		(zero_extend:V4SI
		  (match_operand:V4HI 2 "nonimmediate_operand")))
	      (const_vector:V4SI [(const_int 1) (const_int 1)
				  (const_int 1) (const_int 1)]))
	    (const_int 1))))]
  "TARGET_SSE || TARGET_3DNOW_A"
  "ix86_fixup_binary_operands_no_copy (PLUS, V4HImode, operands);")

(define_insn "*mmx_uavgv4hi3"
  [(set (match_operand:V4HI 0 "register_operand" "=y")
	(truncate:V4HI
	  (lshiftrt:V4SI
	    (plus:V4SI
	      (plus:V4SI
		(zero_extend:V4SI
		  (match_operand:V4HI 1 "nonimmediate_operand" "%0"))
		(zero_extend:V4SI
		  (match_operand:V4HI 2 "nonimmediate_operand" "ym")))
	      (const_vector:V4SI [(const_int 1) (const_int 1)
				  (const_int 1) (const_int 1)]))
	    (const_int 1))))]
  "(TARGET_SSE || TARGET_3DNOW_A)
   && ix86_binary_operator_ok (PLUS, V4HImode, operands)"
  "pavgw\t{%2, %0|%0, %2}"
  [(set_attr "type" "mmxshft")
   (set_attr "mode" "DI")])

(define_insn "mmx_psadbw"
  [(set (match_operand:V1DI 0 "register_operand" "=y")
        (unspec:V1DI [(match_operand:V8QI 1 "register_operand" "0")
		      (match_operand:V8QI 2 "nonimmediate_operand" "ym")]
		     UNSPEC_PSADBW))]
  "TARGET_SSE || TARGET_3DNOW_A"
  "psadbw\t{%2, %0|%0, %2}"
  [(set_attr "type" "mmxshft")
   (set_attr "mode" "DI")])

(define_insn "mmx_pmovmskb"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(unspec:SI [(match_operand:V8QI 1 "register_operand" "y")]
		   UNSPEC_MOVMSK))]
  "TARGET_SSE || TARGET_3DNOW_A"
  "pmovmskb\t{%1, %0|%0, %1}"
  [(set_attr "type" "mmxcvt")
   (set_attr "mode" "DI")])

(define_expand "mmx_maskmovq"
  [(set (match_operand:V8QI 0 "memory_operand")
	(unspec:V8QI [(match_operand:V8QI 1 "register_operand")
		      (match_operand:V8QI 2 "register_operand")
		      (match_dup 0)]
		     UNSPEC_MASKMOV))]
  "TARGET_SSE || TARGET_3DNOW_A")

(define_insn "*mmx_maskmovq"
  [(set (mem:V8QI (match_operand:P 0 "register_operand" "D"))
	(unspec:V8QI [(match_operand:V8QI 1 "register_operand" "y")
		      (match_operand:V8QI 2 "register_operand" "y")
		      (mem:V8QI (match_dup 0))]
		     UNSPEC_MASKMOV))]
  "TARGET_SSE || TARGET_3DNOW_A"
  ;; @@@ check ordering of operands in intel/nonintel syntax
  "maskmovq\t{%2, %1|%1, %2}"
  [(set_attr "type" "mmxcvt")
   (set_attr "mode" "DI")])

(define_expand "mmx_emms"
  [(match_par_dup 0 [(const_int 0)])]
  "TARGET_MMX"
{
  int regno;

  operands[0] = gen_rtx_PARALLEL (VOIDmode, rtvec_alloc (17));

  XVECEXP (operands[0], 0, 0)
    = gen_rtx_UNSPEC_VOLATILE (VOIDmode, gen_rtvec (1, const0_rtx),
			       UNSPECV_EMMS);

  for (regno = 0; regno < 8; regno++)
    {
      XVECEXP (operands[0], 0, regno + 1)
	= gen_rtx_CLOBBER (VOIDmode,
			   gen_rtx_REG (XFmode, FIRST_STACK_REG + regno));

      XVECEXP (operands[0], 0, regno + 9)
	= gen_rtx_CLOBBER (VOIDmode,
			   gen_rtx_REG (DImode, FIRST_MMX_REG + regno));
    }
})

(define_insn "*mmx_emms"
  [(match_parallel 0 "emms_operation"
    [(unspec_volatile [(const_int 0)] UNSPECV_EMMS)])]
  "TARGET_MMX"
  "emms"
  [(set_attr "type" "mmx")
   (set_attr "modrm" "0")
   (set_attr "memory" "none")])

(define_expand "mmx_femms"
  [(match_par_dup 0 [(const_int 0)])]
  "TARGET_3DNOW"
{
  int regno;

  operands[0] = gen_rtx_PARALLEL (VOIDmode, rtvec_alloc (17));

  XVECEXP (operands[0], 0, 0)
    = gen_rtx_UNSPEC_VOLATILE (VOIDmode, gen_rtvec (1, const0_rtx),
			       UNSPECV_FEMMS);

  for (regno = 0; regno < 8; regno++)
    {
      XVECEXP (operands[0], 0, regno + 1)
	= gen_rtx_CLOBBER (VOIDmode,
			   gen_rtx_REG (XFmode, FIRST_STACK_REG + regno));

      XVECEXP (operands[0], 0, regno + 9)
	= gen_rtx_CLOBBER (VOIDmode,
			   gen_rtx_REG (DImode, FIRST_MMX_REG + regno));
    }
})

(define_insn "*mmx_femms"
  [(match_parallel 0 "emms_operation"
    [(unspec_volatile [(const_int 0)] UNSPECV_FEMMS)])]
  "TARGET_3DNOW"
  "femms"
  [(set_attr "type" "mmx")
   (set_attr "modrm" "0")
   (set_attr "memory" "none")])
