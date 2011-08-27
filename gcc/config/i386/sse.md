;; GCC machine description for SSE instructions
;; Copyright (C) 2005, 2006, 2007, 2008, 2009, 2010, 2011
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


;; Instruction suffix for sign and zero extensions.
(define_code_attr extsuffix [(sign_extend "sx") (zero_extend "zx")])

;; 16 byte integral modes handled by SSE
(define_mode_iterator SSEMODEI [V16QI V8HI V4SI V2DI])

;; All 16-byte vector modes handled by SSE
(define_mode_iterator SSEMODE [V16QI V8HI V4SI V2DI V4SF V2DF])
(define_mode_iterator SSEMODE16 [V16QI V8HI V4SI V2DI V1TI V4SF V2DF])

;; 32 byte integral vector modes handled by AVX
(define_mode_iterator AVX256MODEI [V32QI V16HI V8SI V4DI])

;; All 32-byte vector modes handled by AVX
(define_mode_iterator AVX256MODE [V32QI V16HI V8SI V4DI V8SF V4DF])

;; All QI vector modes handled by AVX
(define_mode_iterator AVXMODEQI [V32QI V16QI])

;; All DI vector modes handled by AVX
(define_mode_iterator AVXMODEDI [V4DI V2DI])

;; All vector modes handled by AVX
(define_mode_iterator AVXMODE
  [V16QI V8HI V4SI V2DI V4SF V2DF V32QI V16HI V8SI V4DI V8SF V4DF])
(define_mode_iterator AVXMODE16
  [V16QI V8HI V4SI V2DI V1TI V4SF V2DF V32QI V16HI V8SI V4DI V8SF V4DF])

;; Mix-n-match
(define_mode_iterator SSEMODE12 [V16QI V8HI])
(define_mode_iterator SSEMODE24 [V8HI V4SI])
(define_mode_iterator SSEMODE14 [V16QI V4SI])
(define_mode_iterator SSEMODE124 [V16QI V8HI V4SI])
(define_mode_iterator SSEMODE248 [V8HI V4SI V2DI])
(define_mode_iterator SSEMODE1248 [V16QI V8HI V4SI V2DI])
(define_mode_iterator SSEMODEF4 [SF DF V4SF V2DF])
(define_mode_iterator SSEMODEF2P [V4SF V2DF])

(define_mode_iterator AVX256MODEF2P [V8SF V4DF])
(define_mode_iterator AVX256MODE2P [V8SI V8SF V4DF])
(define_mode_iterator AVX256MODE24P [V8SI V8SF V4DI V4DF])
(define_mode_iterator AVX256MODE4P [V4DI V4DF])
(define_mode_iterator AVX256MODE8P [V8SI V8SF])
(define_mode_iterator AVXMODEF2P [V4SF V2DF V8SF V4DF])
(define_mode_iterator AVXMODEF4P [V4SF V4DF])
(define_mode_iterator AVXMODEFDP [V2DF V4DF])
(define_mode_iterator AVXMODEFSP [V4SF V8SF])
(define_mode_iterator AVXMODEDCVTDQ2PS [V4SF V8SF])
(define_mode_iterator AVXMODEDCVTPS2DQ [V4SI V8SI])

(define_mode_iterator FMAMODE [SF DF V4SF V2DF V8SF V4DF])

;; Int-float size matches
(define_mode_iterator SSEMODE4S [V4SF V4SI])
(define_mode_iterator SSEMODE2D [V2DF V2DI])

;; Modes handled by integer vcond pattern
(define_mode_iterator SSEMODE124C8 [V16QI V8HI V4SI
				    (V2DI "TARGET_SSE4_2")])

;; Modes handled by vec_extract_even/odd pattern.
(define_mode_iterator SSEMODE_EO
  [(V4SF "TARGET_SSE")
   (V2DF "TARGET_SSE2")
   (V2DI "TARGET_SSE2") (V4SI "TARGET_SSE2")
   (V8HI "TARGET_SSE2") (V16QI "TARGET_SSE2")
   (V4DF "TARGET_AVX") (V8SF "TARGET_AVX")])

;; Modes handled by storent patterns.
(define_mode_iterator STORENT_MODE
  [(SF "TARGET_SSE4A") (DF "TARGET_SSE4A")
   (SI "TARGET_SSE2") (V2DI "TARGET_SSE2") (V2DF "TARGET_SSE2")
   (V4SF "TARGET_SSE")
   (V4DF "TARGET_AVX") (V8SF "TARGET_AVX")])

;; Modes handled by vector float patterns.
(define_mode_iterator VEC_FLOAT_MODE
  [(V2DF "TARGET_SSE2") (V4SF "TARGET_SSE")
   (V4DF "TARGET_AVX") (V8SF "TARGET_AVX")])

;; Modes handled by vector extract patterns.
(define_mode_iterator VEC_EXTRACT_MODE
  [(V2DI "TARGET_SSE") (V4SI "TARGET_SSE")
   (V8HI "TARGET_SSE") (V16QI "TARGET_SSE")
   (V2DF "TARGET_SSE") (V4SF "TARGET_SSE")
   (V4DF "TARGET_AVX") (V8SF "TARGET_AVX")])

;; Mapping from float mode to required SSE level
(define_mode_attr sse [(SF "sse") (DF "sse2") (V4SF "sse") (V2DF "sse2")])

;; Mapping from integer vector mode to mnemonic suffix
(define_mode_attr ssevecsize [(V16QI "b") (V8HI "w") (V4SI "d") (V2DI "q")])

;; Mapping of the insn mnemonic suffix
(define_mode_attr ssemodesuffix
  [(SF "ss") (DF "sd") (V4SF "ps") (V2DF "pd") (V8SF "ps") (V4DF "pd")
   (V8SI "ps") (V4DI "pd")])
(define_mode_attr ssescalarmodesuffix 
  [(SF "ss") (DF "sd") (V4SF "ss") (V2DF "sd") (V8SF "ss") (V8SI "ss")
   (V4DF "sd") (V4SI "d") (V4DI "sd")])

;; Mapping of the max integer size for xop rotate immediate constraint
(define_mode_attr sserotatemax [(V16QI "7") (V8HI "15") (V4SI "31") (V2DI "63")])

;; Mapping of vector modes back to the scalar modes
(define_mode_attr ssescalarmode [(V4SF "SF") (V2DF "DF")
				 (V16QI "QI") (V8HI "HI")
				 (V4SI "SI") (V2DI "DI")])

;; Mapping of vector modes to a vector mode of double size
(define_mode_attr ssedoublesizemode
  [(V2DF "V4DF") (V2DI "V4DI") (V4SF "V8SF") (V4SI "V8SI")
   (V8HI "V16HI") (V16QI "V32QI")
   (V4DF "V8DF") (V8SF "V16SF")
   (V4DI "V8DI") (V8SI "V16SI") (V16HI "V32HI") (V32QI "V64QI")])

;; Number of scalar elements in each vector type
(define_mode_attr ssescalarnum
  [(V4SF "4") (V2DF "2") (V16QI "16") (V8HI "8") (V4SI "4") (V2DI "2")
   (V8SF "8") (V4DF "4") (V32QI "32") (V16HI "16") (V8SI "8") (V4DI "4")])

;; Mapping for AVX
(define_mode_attr avxvecmode
  [(V16QI "TI") (V8HI "TI") (V4SI "TI") (V2DI "TI") (V1TI "TI")
   (V4SF "V4SF") (V8SF "V8SF") (V2DF "V2DF") (V4DF "V4DF")
   (V32QI "OI") (V16HI "OI") (V8SI "OI") (V4DI "OI")])
(define_mode_attr avxvecpsmode
  [(V16QI "V4SF") (V8HI "V4SF") (V4SI "V4SF") (V2DI "V4SF")
   (V32QI "V8SF") (V16HI "V8SF") (V8SI "V8SF") (V4DI "V8SF")])
(define_mode_attr avxhalfvecmode
  [(V32QI "V16QI") (V16HI "V8HI") (V8SI "V4SI") (V4DI "V2DI")
   (V8SF "V4SF") (V4DF "V2DF")
   (V16QI  "V8QI") (V8HI  "V4HI") (V4SI "V2SI") (V4SF "V2SF")])
(define_mode_attr avxscalarmode
  [(V16QI "QI") (V8HI  "HI") (V4SI "SI") (V2DI "DI") (V4SF "SF") (V2DF "DF")
   (V32QI "QI") (V16HI "HI") (V8SI "SI") (V4DI "DI") (V8SF "SF") (V4DF "DF")])
(define_mode_attr avxcvtvecmode
  [(V4SF "V4SI") (V8SF "V8SI") (V4SI "V4SF") (V8SI "V8SF")])
(define_mode_attr avxpermvecmode
  [(V2DF "V2DI") (V4SF "V4SI") (V4DF "V4DI") (V8SF "V8SI")])
(define_mode_attr avxmodesuffixp
 [(V2DF "pd") (V4SI "si") (V4SF "ps") (V8SF "ps") (V8SI "si")
  (V4DF "pd")])
(define_mode_attr avxmodesuffix
  [(V16QI "") (V32QI "256") (V4SI "") (V4SF "") (V2DF "")
   (V8SI "256") (V8SF "256") (V4DF "256")])

;; Mapping of immediate bits for blend instructions
(define_mode_attr blendbits
  [(V8SF "255") (V4SF "15") (V4DF "15") (V2DF "3")])

;; Mapping of immediate bits for pinsr instructions
(define_mode_attr pinsrbits [(V16QI "32768") (V8HI "128") (V4SI "8")])

;; Patterns whose name begins with "sse{,2,3}_" are invoked by intrinsics.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Move patterns
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define_expand "mov<mode>"
  [(set (match_operand:AVX256MODE 0 "nonimmediate_operand" "")
	(match_operand:AVX256MODE 1 "nonimmediate_operand" ""))]
  "TARGET_AVX"
{
  ix86_expand_vector_move (<MODE>mode, operands);
  DONE;
})

(define_insn "*avx_mov<mode>_internal"
  [(set (match_operand:AVXMODE16 0 "nonimmediate_operand" "=x,x ,m")
	(match_operand:AVXMODE16 1 "nonimmediate_or_sse_const_operand"  "C ,xm,x"))]
  "TARGET_AVX
   && (register_operand (operands[0], <MODE>mode)
       || register_operand (operands[1], <MODE>mode))"
{
  switch (which_alternative)
    {
    case 0:
      return standard_sse_constant_opcode (insn, operands[1]);
    case 1:
    case 2:
      switch (get_attr_mode (insn))
        {
	case MODE_V8SF:
	case MODE_V4SF:
	  if (misaligned_operand (operands[0], <MODE>mode)
	      || misaligned_operand (operands[1], <MODE>mode))
	    return "vmovups\t{%1, %0|%0, %1}";
	  else
	    return "vmovaps\t{%1, %0|%0, %1}";
	case MODE_V4DF:
	case MODE_V2DF:
	  if (misaligned_operand (operands[0], <MODE>mode)
	      || misaligned_operand (operands[1], <MODE>mode))
	    return "vmovupd\t{%1, %0|%0, %1}";
	  else if (TARGET_SSE_PACKED_SINGLE_INSN_OPTIMAL)
	    return "vmovaps\t{%1, %0|%0, %1}";
	  else
	    return "vmovapd\t{%1, %0|%0, %1}";
	default:
	  if (misaligned_operand (operands[0], <MODE>mode)
	      || misaligned_operand (operands[1], <MODE>mode))
	    return "vmovdqu\t{%1, %0|%0, %1}";
	  else if (TARGET_SSE_PACKED_SINGLE_INSN_OPTIMAL)
	    return "vmovaps\t{%1, %0|%0, %1}";
	  else
	    return "vmovdqa\t{%1, %0|%0, %1}";
	}
    default:
      gcc_unreachable ();
    }
}
  [(set_attr "type" "sselog1,ssemov,ssemov")
   (set_attr "prefix" "vex")
   (set_attr "mode" "<avxvecmode>")])

;; All of these patterns are enabled for SSE1 as well as SSE2.
;; This is essential for maintaining stable calling conventions.

(define_expand "mov<mode>"
  [(set (match_operand:SSEMODE16 0 "nonimmediate_operand" "")
	(match_operand:SSEMODE16 1 "nonimmediate_operand" ""))]
  "TARGET_SSE"
{
  ix86_expand_vector_move (<MODE>mode, operands);
  DONE;
})

(define_insn "*mov<mode>_internal"
  [(set (match_operand:SSEMODE16 0 "nonimmediate_operand" "=x,x ,m")
	(match_operand:SSEMODE16 1 "nonimmediate_or_sse_const_operand"  "C ,xm,x"))]
  "TARGET_SSE
   && (register_operand (operands[0], <MODE>mode)
       || register_operand (operands[1], <MODE>mode))"
{
  switch (which_alternative)
    {
    case 0:
      return standard_sse_constant_opcode (insn, operands[1]);
    case 1:
    case 2:
      switch (get_attr_mode (insn))
	{
	case MODE_V4SF:
	  return "movaps\t{%1, %0|%0, %1}";
	case MODE_V2DF:
	  if (TARGET_SSE_PACKED_SINGLE_INSN_OPTIMAL)
	    return "movaps\t{%1, %0|%0, %1}";
	  else
	    return "movapd\t{%1, %0|%0, %1}";
	default:
	  if (TARGET_SSE_PACKED_SINGLE_INSN_OPTIMAL)
	    return "movaps\t{%1, %0|%0, %1}";
	  else
	    return "movdqa\t{%1, %0|%0, %1}";
	}
    default:
      gcc_unreachable ();
    }
}
  [(set_attr "type" "sselog1,ssemov,ssemov")
   (set (attr "mode")
	(cond [(ior (ior (ne (symbol_ref "optimize_function_for_size_p (cfun)") (const_int 0))
			 (eq (symbol_ref "TARGET_SSE2") (const_int 0)))
		    (and (eq_attr "alternative" "2")
			 (ne (symbol_ref "TARGET_SSE_TYPELESS_STORES")
			     (const_int 0))))
		 (const_string "V4SF")
	       (eq (const_string "<MODE>mode") (const_string "V4SFmode"))
		 (const_string "V4SF")
	       (eq (const_string "<MODE>mode") (const_string "V2DFmode"))
		 (const_string "V2DF")
	      ]
	  (const_string "TI")))])

;; Move a DI from a 32-bit register pair (e.g. %edx:%eax) to an xmm.
;; We'd rather avoid this entirely; if the 32-bit reg pair was loaded
;; from memory, we'd prefer to load the memory directly into the %xmm
;; register.  To facilitate this happy circumstance, this pattern won't
;; split until after register allocation.  If the 64-bit value didn't
;; come from memory, this is the best we can do.  This is much better
;; than storing %edx:%eax into a stack temporary and loading an %xmm
;; from there.

(define_insn_and_split "movdi_to_sse"
  [(parallel
    [(set (match_operand:V4SI 0 "register_operand" "=?x,x")
	  (subreg:V4SI (match_operand:DI 1 "nonimmediate_operand" "r,m") 0))
     (clobber (match_scratch:V4SI 2 "=&x,X"))])]
  "!TARGET_64BIT && TARGET_SSE2 && TARGET_INTER_UNIT_MOVES"
  "#"
  "&& reload_completed"
  [(const_int 0)]
{
 if (register_operand (operands[1], DImode))
   {
      /* The DImode arrived in a pair of integral registers (e.g. %edx:%eax).
	 Assemble the 64-bit DImode value in an xmm register.  */
      emit_insn (gen_sse2_loadld (operands[0], CONST0_RTX (V4SImode),
      				  gen_rtx_SUBREG (SImode, operands[1], 0)));
      emit_insn (gen_sse2_loadld (operands[2], CONST0_RTX (V4SImode),
				  gen_rtx_SUBREG (SImode, operands[1], 4)));
      emit_insn (gen_vec_interleave_lowv4si (operands[0], operands[0],
      					     operands[2]));
    }
 else if (memory_operand (operands[1], DImode))
   emit_insn (gen_vec_concatv2di (gen_lowpart (V2DImode, operands[0]),
   	     			  operands[1], const0_rtx));
 else
   gcc_unreachable ();
})

(define_split
  [(set (match_operand:V4SF 0 "register_operand" "")
	(match_operand:V4SF 1 "zero_extended_scalar_load_operand" ""))]
  "TARGET_SSE && reload_completed"
  [(set (match_dup 0)
	(vec_merge:V4SF
	  (vec_duplicate:V4SF (match_dup 1))
	  (match_dup 2)
	  (const_int 1)))]
{
  operands[1] = simplify_gen_subreg (SFmode, operands[1], V4SFmode, 0);
  operands[2] = CONST0_RTX (V4SFmode);
})

(define_split
  [(set (match_operand:V2DF 0 "register_operand" "")
	(match_operand:V2DF 1 "zero_extended_scalar_load_operand" ""))]
  "TARGET_SSE2 && reload_completed"
  [(set (match_dup 0) (vec_concat:V2DF (match_dup 1) (match_dup 2)))]
{
  operands[1] = simplify_gen_subreg (DFmode, operands[1], V2DFmode, 0);
  operands[2] = CONST0_RTX (DFmode);
})

(define_expand "push<mode>1"
  [(match_operand:AVX256MODE 0 "register_operand" "")]
  "TARGET_AVX"
{
  ix86_expand_push (<MODE>mode, operands[0]);
  DONE;
})

(define_expand "push<mode>1"
  [(match_operand:SSEMODE16 0 "register_operand" "")]
  "TARGET_SSE"
{
  ix86_expand_push (<MODE>mode, operands[0]);
  DONE;
})

(define_expand "movmisalign<mode>"
  [(set (match_operand:AVX256MODE 0 "nonimmediate_operand" "")
	(match_operand:AVX256MODE 1 "nonimmediate_operand" ""))]
  "TARGET_AVX"
{
  ix86_expand_vector_move_misalign (<MODE>mode, operands);
  DONE;
})

(define_expand "movmisalign<mode>"
  [(set (match_operand:SSEMODE16 0 "nonimmediate_operand" "")
	(match_operand:SSEMODE16 1 "nonimmediate_operand" ""))]
  "TARGET_SSE"
{
  ix86_expand_vector_move_misalign (<MODE>mode, operands);
  DONE;
})

(define_expand "avx_movu<ssemodesuffix><avxmodesuffix>"
  [(set (match_operand:AVXMODEF2P 0 "nonimmediate_operand" "")
	(unspec:AVXMODEF2P
	  [(match_operand:AVXMODEF2P 1 "nonimmediate_operand" "")]
	  UNSPEC_MOVU))]
  "AVX_VEC_FLOAT_MODE_P (<MODE>mode)"
{
  if (MEM_P (operands[0]) && MEM_P (operands[1]))
    operands[1] = force_reg (<MODE>mode, operands[1]);
})

(define_insn "*avx_movu<ssemodesuffix><avxmodesuffix>"
  [(set (match_operand:AVXMODEF2P 0 "nonimmediate_operand" "=x,m")
	(unspec:AVXMODEF2P
	  [(match_operand:AVXMODEF2P 1 "nonimmediate_operand" "xm,x")]
	  UNSPEC_MOVU))]
  "AVX_VEC_FLOAT_MODE_P (<MODE>mode)
   && !(MEM_P (operands[0]) && MEM_P (operands[1]))"
  "vmovu<ssemodesuffix>\t{%1, %0|%0, %1}"
  [(set_attr "type" "ssemov")
   (set_attr "movu" "1")
   (set_attr "prefix" "vex")
   (set_attr "mode" "<MODE>")])

(define_insn "sse2_movq128"
  [(set (match_operand:V2DI 0 "register_operand" "=x")
	(vec_concat:V2DI
	  (vec_select:DI
	    (match_operand:V2DI 1 "nonimmediate_operand" "xm")
	    (parallel [(const_int 0)]))
	  (const_int 0)))]
  "TARGET_SSE2"
  "%vmovq\t{%1, %0|%0, %1}"
  [(set_attr "type" "ssemov")
   (set_attr "prefix" "maybe_vex")
   (set_attr "mode" "TI")])

(define_expand "<sse>_movu<ssemodesuffix>"
  [(set (match_operand:SSEMODEF2P 0 "nonimmediate_operand" "")
	(unspec:SSEMODEF2P
	  [(match_operand:SSEMODEF2P 1 "nonimmediate_operand" "")]
	  UNSPEC_MOVU))]
  "SSE_VEC_FLOAT_MODE_P (<MODE>mode)"
{
  if (MEM_P (operands[0]) && MEM_P (operands[1]))
    operands[1] = force_reg (<MODE>mode, operands[1]);
})

(define_insn "*<sse>_movu<ssemodesuffix>"
  [(set (match_operand:SSEMODEF2P 0 "nonimmediate_operand" "=x,m")
	(unspec:SSEMODEF2P
	  [(match_operand:SSEMODEF2P 1 "nonimmediate_operand" "xm,x")]
	  UNSPEC_MOVU))]
  "SSE_VEC_FLOAT_MODE_P (<MODE>mode)
   && !(MEM_P (operands[0]) && MEM_P (operands[1]))"
  "movu<ssemodesuffix>\t{%1, %0|%0, %1}"
  [(set_attr "type" "ssemov")
   (set_attr "movu" "1")
   (set_attr "mode" "<MODE>")])

(define_expand "avx_movdqu<avxmodesuffix>"
  [(set (match_operand:AVXMODEQI 0 "nonimmediate_operand" "")
	(unspec:AVXMODEQI
	  [(match_operand:AVXMODEQI 1 "nonimmediate_operand" "")]
	  UNSPEC_MOVU))]
  "TARGET_AVX"
{
  if (MEM_P (operands[0]) && MEM_P (operands[1]))
    operands[1] = force_reg (<MODE>mode, operands[1]);
})

(define_insn "*avx_movdqu<avxmodesuffix>"
  [(set (match_operand:AVXMODEQI 0 "nonimmediate_operand" "=x,m")
	(unspec:AVXMODEQI
	  [(match_operand:AVXMODEQI 1 "nonimmediate_operand" "xm,x")]
	  UNSPEC_MOVU))]
  "TARGET_AVX && !(MEM_P (operands[0]) && MEM_P (operands[1]))"
  "vmovdqu\t{%1, %0|%0, %1}"
  [(set_attr "type" "ssemov")
   (set_attr "movu" "1")
   (set_attr "prefix" "vex")
   (set_attr "mode" "<avxvecmode>")])

(define_expand "sse2_movdqu"
  [(set (match_operand:V16QI 0 "nonimmediate_operand" "")
	(unspec:V16QI [(match_operand:V16QI 1 "nonimmediate_operand" "")]
		      UNSPEC_MOVU))]
  "TARGET_SSE2"
{
  if (MEM_P (operands[0]) && MEM_P (operands[1]))
    operands[1] = force_reg (V16QImode, operands[1]);
})

(define_insn "*sse2_movdqu"
  [(set (match_operand:V16QI 0 "nonimmediate_operand" "=x,m")
	(unspec:V16QI [(match_operand:V16QI 1 "nonimmediate_operand" "xm,x")]
		      UNSPEC_MOVU))]
  "TARGET_SSE2 && !(MEM_P (operands[0]) && MEM_P (operands[1]))"
  "movdqu\t{%1, %0|%0, %1}"
  [(set_attr "type" "ssemov")
   (set_attr "movu" "1")
   (set_attr "prefix_data16" "1")
   (set_attr "mode" "TI")])

(define_insn "avx_movnt<mode>"
  [(set (match_operand:AVXMODEF2P 0 "memory_operand" "=m")
	(unspec:AVXMODEF2P
	  [(match_operand:AVXMODEF2P 1 "register_operand" "x")]
	  UNSPEC_MOVNT))]
  "AVX_VEC_FLOAT_MODE_P (<MODE>mode)"
  "vmovnt<ssemodesuffix>\t{%1, %0|%0, %1}"
  [(set_attr "type" "ssemov")
   (set_attr "prefix" "vex")
   (set_attr "mode" "<MODE>")])

(define_insn "<sse>_movnt<mode>"
  [(set (match_operand:SSEMODEF2P 0 "memory_operand" "=m")
	(unspec:SSEMODEF2P
	  [(match_operand:SSEMODEF2P 1 "register_operand" "x")]
	  UNSPEC_MOVNT))]
  "SSE_VEC_FLOAT_MODE_P (<MODE>mode)"
  "movnt<ssemodesuffix>\t{%1, %0|%0, %1}"
  [(set_attr "type" "ssemov")
   (set_attr "mode" "<MODE>")])

(define_insn "avx_movnt<mode>"
  [(set (match_operand:AVXMODEDI 0 "memory_operand" "=m")
	(unspec:AVXMODEDI
	  [(match_operand:AVXMODEDI 1 "register_operand" "x")]
	  UNSPEC_MOVNT))]
  "TARGET_AVX"
  "vmovntdq\t{%1, %0|%0, %1}"
  [(set_attr "type" "ssecvt")
   (set_attr "prefix" "vex")
   (set_attr "mode" "<avxvecmode>")])

(define_insn "sse2_movntv2di"
  [(set (match_operand:V2DI 0 "memory_operand" "=m")
	(unspec:V2DI [(match_operand:V2DI 1 "register_operand" "x")]
		     UNSPEC_MOVNT))]
  "TARGET_SSE2"
  "movntdq\t{%1, %0|%0, %1}"
  [(set_attr "type" "ssemov")
   (set_attr "prefix_data16" "1")
   (set_attr "mode" "TI")])

(define_insn "sse2_movntsi"
  [(set (match_operand:SI 0 "memory_operand" "=m")
	(unspec:SI [(match_operand:SI 1 "register_operand" "r")]
		   UNSPEC_MOVNT))]
  "TARGET_SSE2"
  "movnti\t{%1, %0|%0, %1}"
  [(set_attr "type" "ssemov")
   (set_attr "prefix_data16" "0")
   (set_attr "mode" "V2DF")])

(define_insn "avx_lddqu<avxmodesuffix>"
  [(set (match_operand:AVXMODEQI 0 "register_operand" "=x")
	(unspec:AVXMODEQI
	  [(match_operand:AVXMODEQI 1 "memory_operand" "m")]
	  UNSPEC_LDDQU))]
  "TARGET_AVX"
  "vlddqu\t{%1, %0|%0, %1}"
  [(set_attr "type" "ssecvt")
   (set_attr "movu" "1")
   (set_attr "prefix" "vex")
   (set_attr "mode" "<avxvecmode>")])

(define_insn "sse3_lddqu"
  [(set (match_operand:V16QI 0 "register_operand" "=x")
	(unspec:V16QI [(match_operand:V16QI 1 "memory_operand" "m")]
		      UNSPEC_LDDQU))]
  "TARGET_SSE3"
  "lddqu\t{%1, %0|%0, %1}"
  [(set_attr "type" "ssemov")
   (set_attr "movu" "1")
   (set_attr "prefix_data16" "0")
   (set_attr "prefix_rep" "1")
   (set_attr "mode" "TI")])

; Expand patterns for non-temporal stores.  At the moment, only those
; that directly map to insns are defined; it would be possible to
; define patterns for other modes that would expand to several insns.

(define_expand "storent<mode>"
  [(set (match_operand:STORENT_MODE 0 "memory_operand" "")
	(unspec:STORENT_MODE
	  [(match_operand:STORENT_MODE 1 "register_operand" "")]
	  UNSPEC_MOVNT))])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Parallel floating point arithmetic
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define_expand "<code><mode>2"
  [(set (match_operand:VEC_FLOAT_MODE 0 "register_operand" "")
	(absneg:VEC_FLOAT_MODE
	  (match_operand:VEC_FLOAT_MODE 1 "register_operand" "")))]
  ""
  "ix86_expand_fp_absneg_operator (<CODE>, <MODE>mode, operands); DONE;")

(define_insn_and_split "*avx_absneg<mode>2"
  [(set (match_operand:AVXMODEF2P 0 "register_operand" "=x,x")
	(match_operator:AVXMODEF2P 3 "absneg_operator"
	  [(match_operand:AVXMODEF2P 1 "nonimmediate_operand" "x,m")]))
   (use (match_operand:AVXMODEF2P 2 "nonimmediate_operand" "xm,x"))]
  "AVX_VEC_FLOAT_MODE_P (<MODE>mode)"
  "#"
  "&& reload_completed"
  [(const_int 0)]
{
  rtx t;

  if (MEM_P (operands[1]))
    t = gen_rtx_fmt_ee (GET_CODE (operands[3]) == NEG ? XOR : AND,
			<MODE>mode, operands[2], operands[1]);
  else
    t = gen_rtx_fmt_ee (GET_CODE (operands[3]) == NEG ? XOR : AND,
			<MODE>mode, operands[1], operands[2]);
  t = gen_rtx_SET (VOIDmode, operands[0], t);
  emit_insn (t);
  DONE;
})

(define_insn_and_split "*sse_absneg<mode>2"
  [(set (match_operand:SSEMODEF2P 0 "register_operand" "=x,x")
	(match_operator:SSEMODEF2P 3 "absneg_operator"
	  [(match_operand:SSEMODEF2P 1 "nonimmediate_operand" "0,xm")]))
   (use (match_operand:SSEMODEF2P 2 "nonimmediate_operand" "xm,0"))]
  "SSE_VEC_FLOAT_MODE_P (<MODE>mode)"
  "#"
  "&& reload_completed"
  [(const_int 0)]
{
  rtx t;

  t = operands[rtx_equal_p (operands[0], operands[1]) ? 2 : 1];
  t = gen_rtx_fmt_ee (GET_CODE (operands[3]) == NEG ? XOR : AND,
		      <MODE>mode, operands[0], t);
  t = gen_rtx_SET (VOIDmode, operands[0], t);
  emit_insn (t);
  DONE;
})

(define_expand "<plusminus_insn><mode>3"
  [(set (match_operand:AVX256MODEF2P 0 "register_operand" "")
	(plusminus:AVX256MODEF2P
	  (match_operand:AVX256MODEF2P 1 "nonimmediate_operand" "")
	  (match_operand:AVX256MODEF2P 2 "nonimmediate_operand" "")))]
  "AVX256_VEC_FLOAT_MODE_P (<MODE>mode)"
  "ix86_fixup_binary_operands_no_copy (<CODE>, <MODE>mode, operands);")

(define_insn "*avx_<plusminus_insn><mode>3"
  [(set (match_operand:AVXMODEF2P 0 "register_operand" "=x")
	(plusminus:AVXMODEF2P
	  (match_operand:AVXMODEF2P 1 "nonimmediate_operand" "<comm>x")
	  (match_operand:AVXMODEF2P 2 "nonimmediate_operand" "xm")))]
  "AVX_VEC_FLOAT_MODE_P (<MODE>mode)
   && ix86_binary_operator_ok (<CODE>, <MODE>mode, operands)"
  "v<plusminus_mnemonic><ssemodesuffix>\t{%2, %1, %0|%0, %1, %2}"
  [(set_attr "type" "sseadd")
   (set_attr "prefix" "vex")
   (set_attr "mode" "<avxvecmode>")])

(define_expand "<plusminus_insn><mode>3"
  [(set (match_operand:SSEMODEF2P 0 "register_operand" "")
	(plusminus:SSEMODEF2P
	  (match_operand:SSEMODEF2P 1 "nonimmediate_operand" "")
	  (match_operand:SSEMODEF2P 2 "nonimmediate_operand" "")))]
  "SSE_VEC_FLOAT_MODE_P (<MODE>mode)"
  "ix86_fixup_binary_operands_no_copy (<CODE>, <MODE>mode, operands);")

(define_insn "*<plusminus_insn><mode>3"
  [(set (match_operand:SSEMODEF2P 0 "register_operand" "=x")
	(plusminus:SSEMODEF2P
	  (match_operand:SSEMODEF2P 1 "nonimmediate_operand" "<comm>0")
	  (match_operand:SSEMODEF2P 2 "nonimmediate_operand" "xm")))]
  "SSE_VEC_FLOAT_MODE_P (<MODE>mode)
   && ix86_binary_operator_ok (<CODE>, <MODE>mode, operands)"
  "<plusminus_mnemonic><ssemodesuffix>\t{%2, %0|%0, %2}"
  [(set_attr "type" "sseadd")
   (set_attr "mode" "<MODE>")])

(define_insn "*avx_vm<plusminus_insn><mode>3"
  [(set (match_operand:SSEMODEF2P 0 "register_operand" "=x")
	(vec_merge:SSEMODEF2P
	  (plusminus:SSEMODEF2P
	    (match_operand:SSEMODEF2P 1 "register_operand" "x")
	    (match_operand:SSEMODEF2P 2 "nonimmediate_operand" "xm"))
	  (match_dup 1)
	  (const_int 1)))]
  "AVX128_VEC_FLOAT_MODE_P (<MODE>mode)"
  "v<plusminus_mnemonic><ssescalarmodesuffix>\t{%2, %1, %0|%0, %1, %2}"
  [(set_attr "type" "sseadd")
   (set_attr "prefix" "vex")
   (set_attr "mode" "<ssescalarmode>")])

(define_insn "<sse>_vm<plusminus_insn><mode>3"
  [(set (match_operand:SSEMODEF2P 0 "register_operand" "=x")
	(vec_merge:SSEMODEF2P
	  (plusminus:SSEMODEF2P
	    (match_operand:SSEMODEF2P 1 "register_operand" "0")
	    (match_operand:SSEMODEF2P 2 "nonimmediate_operand" "xm"))
	  (match_dup 1)
	  (const_int 1)))]
  "SSE_VEC_FLOAT_MODE_P (<MODE>mode)"
  "<plusminus_mnemonic><ssescalarmodesuffix>\t{%2, %0|%0, %2}"
  [(set_attr "type" "sseadd")
   (set_attr "mode" "<ssescalarmode>")])

(define_expand "mul<mode>3"
  [(set (match_operand:AVX256MODEF2P 0 "register_operand" "")
	(mult:AVX256MODEF2P
	  (match_operand:AVX256MODEF2P 1 "nonimmediate_operand" "")
	  (match_operand:AVX256MODEF2P 2 "nonimmediate_operand" "")))]
  "AVX256_VEC_FLOAT_MODE_P (<MODE>mode)"
  "ix86_fixup_binary_operands_no_copy (MULT, <MODE>mode, operands);")

(define_insn "*avx_mul<mode>3"
  [(set (match_operand:AVXMODEF2P 0 "register_operand" "=x")
	(mult:AVXMODEF2P
	  (match_operand:AVXMODEF2P 1 "nonimmediate_operand" "%x")
	  (match_operand:AVXMODEF2P 2 "nonimmediate_operand" "xm")))]
  "AVX_VEC_FLOAT_MODE_P (<MODE>mode)
   && ix86_binary_operator_ok (MULT, <MODE>mode, operands)"
  "vmul<ssemodesuffix>\t{%2, %1, %0|%0, %1, %2}"
  [(set_attr "type" "ssemul")
   (set_attr "prefix" "vex")
   (set_attr "mode" "<avxvecmode>")])

(define_expand "mul<mode>3"
  [(set (match_operand:SSEMODEF2P 0 "register_operand" "")
	(mult:SSEMODEF2P
	  (match_operand:SSEMODEF2P 1 "nonimmediate_operand" "")
	  (match_operand:SSEMODEF2P 2 "nonimmediate_operand" "")))]
  "SSE_VEC_FLOAT_MODE_P (<MODE>mode)"
  "ix86_fixup_binary_operands_no_copy (MULT, <MODE>mode, operands);")

(define_insn "*mul<mode>3"
  [(set (match_operand:SSEMODEF2P 0 "register_operand" "=x")
	(mult:SSEMODEF2P
	  (match_operand:SSEMODEF2P 1 "nonimmediate_operand" "%0")
	  (match_operand:SSEMODEF2P 2 "nonimmediate_operand" "xm")))]
  "SSE_VEC_FLOAT_MODE_P (<MODE>mode)
   && ix86_binary_operator_ok (MULT, <MODE>mode, operands)"
  "mul<ssemodesuffix>\t{%2, %0|%0, %2}"
  [(set_attr "type" "ssemul")
   (set_attr "mode" "<MODE>")])

(define_insn "*avx_vmmul<mode>3"
  [(set (match_operand:SSEMODEF2P 0 "register_operand" "=x")
	(vec_merge:SSEMODEF2P
	  (mult:SSEMODEF2P
	    (match_operand:SSEMODEF2P 1 "register_operand" "x")
	    (match_operand:SSEMODEF2P 2 "nonimmediate_operand" "xm"))
	  (match_dup 1)
	  (const_int 1)))]
  "AVX_VEC_FLOAT_MODE_P (<MODE>mode)"
  "vmul<ssescalarmodesuffix>\t{%2, %1, %0|%0, %1, %2}"
  [(set_attr "type" "ssemul")
   (set_attr "prefix" "vex")
   (set_attr "mode" "<ssescalarmode>")])

(define_insn "<sse>_vmmul<mode>3"
  [(set (match_operand:SSEMODEF2P 0 "register_operand" "=x")
	(vec_merge:SSEMODEF2P
	  (mult:SSEMODEF2P
	    (match_operand:SSEMODEF2P 1 "register_operand" "0")
	    (match_operand:SSEMODEF2P 2 "nonimmediate_operand" "xm"))
	  (match_dup 1)
	  (const_int 1)))]
  "SSE_VEC_FLOAT_MODE_P (<MODE>mode)"
  "mul<ssescalarmodesuffix>\t{%2, %0|%0, %2}"
  [(set_attr "type" "ssemul")
   (set_attr "mode" "<ssescalarmode>")])

(define_expand "divv8sf3"
  [(set (match_operand:V8SF 0 "register_operand" "")
	(div:V8SF (match_operand:V8SF 1 "register_operand" "")
		  (match_operand:V8SF 2 "nonimmediate_operand" "")))]
  "TARGET_AVX"
{
  ix86_fixup_binary_operands_no_copy (DIV, V8SFmode, operands);

  if (TARGET_SSE_MATH && TARGET_RECIP && !optimize_insn_for_size_p ()
      && flag_finite_math_only && !flag_trapping_math
      && flag_unsafe_math_optimizations)
    {
      ix86_emit_swdivsf (operands[0], operands[1],
			 operands[2], V8SFmode);
      DONE;
    }
})

(define_expand "divv4df3"
  [(set (match_operand:V4DF 0 "register_operand" "")
	(div:V4DF (match_operand:V4DF 1 "register_operand" "")
		  (match_operand:V4DF 2 "nonimmediate_operand" "")))]
  "TARGET_AVX"
  "ix86_fixup_binary_operands_no_copy (DIV, V4DFmode, operands);")

(define_insn "avx_div<mode>3"
  [(set (match_operand:AVXMODEF2P 0 "register_operand" "=x")
	(div:AVXMODEF2P
	  (match_operand:AVXMODEF2P 1 "register_operand" "x")
	  (match_operand:AVXMODEF2P 2 "nonimmediate_operand" "xm")))]
  "AVX_VEC_FLOAT_MODE_P (<MODE>mode)"
  "vdiv<ssemodesuffix>\t{%2, %1, %0|%0, %1, %2}"
  [(set_attr "type" "ssediv")
   (set_attr "prefix" "vex")
   (set_attr "mode" "<MODE>")])

(define_expand "divv4sf3"
  [(set (match_operand:V4SF 0 "register_operand" "")
	(div:V4SF (match_operand:V4SF 1 "register_operand" "")
		  (match_operand:V4SF 2 "nonimmediate_operand" "")))]
  "TARGET_SSE"
{
  if (TARGET_SSE_MATH && TARGET_RECIP && optimize_insn_for_speed_p ()
      && flag_finite_math_only && !flag_trapping_math
      && flag_unsafe_math_optimizations)
    {
      ix86_emit_swdivsf (operands[0], operands[1],
			 operands[2], V4SFmode);
      DONE;
    }
})

(define_expand "divv2df3"
  [(set (match_operand:V2DF 0 "register_operand" "")
	(div:V2DF (match_operand:V2DF 1 "register_operand" "")
		  (match_operand:V2DF 2 "nonimmediate_operand" "")))]
  "TARGET_SSE2")

(define_insn "*avx_div<mode>3"
  [(set (match_operand:SSEMODEF2P 0 "register_operand" "=x")
	(div:SSEMODEF2P
	  (match_operand:SSEMODEF2P 1 "register_operand" "x")
	  (match_operand:SSEMODEF2P 2 "nonimmediate_operand" "xm")))]
  "AVX128_VEC_FLOAT_MODE_P (<MODE>mode)"
  "vdiv<ssemodesuffix>\t{%2, %1, %0|%0, %1, %2}"
  [(set_attr "type" "ssediv")
   (set_attr "prefix" "vex")
   (set_attr "mode" "<MODE>")])

(define_insn "<sse>_div<mode>3"
  [(set (match_operand:SSEMODEF2P 0 "register_operand" "=x")
	(div:SSEMODEF2P
	  (match_operand:SSEMODEF2P 1 "register_operand" "0")
	  (match_operand:SSEMODEF2P 2 "nonimmediate_operand" "xm")))]
  "SSE_VEC_FLOAT_MODE_P (<MODE>mode)"
  "div<ssemodesuffix>\t{%2, %0|%0, %2}"
  [(set_attr "type" "ssediv")
   (set_attr "mode" "<MODE>")])

(define_insn "*avx_vmdiv<mode>3"
  [(set (match_operand:SSEMODEF2P 0 "register_operand" "=x")
	(vec_merge:SSEMODEF2P
	  (div:SSEMODEF2P
	    (match_operand:SSEMODEF2P 1 "register_operand" "x")
	    (match_operand:SSEMODEF2P 2 "nonimmediate_operand" "xm"))
	  (match_dup 1)
	  (const_int 1)))]
  "AVX128_VEC_FLOAT_MODE_P (<MODE>mode)"
  "vdiv<ssescalarmodesuffix>\t{%2, %1, %0|%0, %1, %2}"
  [(set_attr "type" "ssediv")
   (set_attr "prefix" "vex")
   (set_attr "mode" "<ssescalarmode>")])

(define_insn "<sse>_vmdiv<mode>3"
  [(set (match_operand:SSEMODEF2P 0 "register_operand" "=x")
	(vec_merge:SSEMODEF2P
	  (div:SSEMODEF2P
	    (match_operand:SSEMODEF2P 1 "register_operand" "0")
	    (match_operand:SSEMODEF2P 2 "nonimmediate_operand" "xm"))
	  (match_dup 1)
	  (const_int 1)))]
  "SSE_VEC_FLOAT_MODE_P (<MODE>mode)"
  "div<ssescalarmodesuffix>\t{%2, %0|%0, %2}"
  [(set_attr "type" "ssediv")
   (set_attr "mode" "<ssescalarmode>")])

(define_insn "avx_rcpv8sf2"
  [(set (match_operand:V8SF 0 "register_operand" "=x")
	(unspec:V8SF
	  [(match_operand:V8SF 1 "nonimmediate_operand" "xm")] UNSPEC_RCP))]
  "TARGET_AVX"
  "vrcpps\t{%1, %0|%0, %1}"
  [(set_attr "type" "sse")
   (set_attr "prefix" "vex")
   (set_attr "mode" "V8SF")])

(define_insn "sse_rcpv4sf2"
  [(set (match_operand:V4SF 0 "register_operand" "=x")
	(unspec:V4SF
	  [(match_operand:V4SF 1 "nonimmediate_operand" "xm")] UNSPEC_RCP))]
  "TARGET_SSE"
  "%vrcpps\t{%1, %0|%0, %1}"
  [(set_attr "type" "sse")
   (set_attr "atom_sse_attr" "rcp")
   (set_attr "prefix" "maybe_vex")
   (set_attr "mode" "V4SF")])

(define_insn "*avx_vmrcpv4sf2"
  [(set (match_operand:V4SF 0 "register_operand" "=x")
	(vec_merge:V4SF
	  (unspec:V4SF [(match_operand:V4SF 1 "nonimmediate_operand" "xm")]
		       UNSPEC_RCP)
	  (match_operand:V4SF 2 "register_operand" "x")
	  (const_int 1)))]
  "TARGET_AVX"
  "vrcpss\t{%1, %2, %0|%0, %2, %1}"
  [(set_attr "type" "sse")
   (set_attr "prefix" "vex")
   (set_attr "mode" "SF")])

(define_insn "sse_vmrcpv4sf2"
  [(set (match_operand:V4SF 0 "register_operand" "=x")
	(vec_merge:V4SF
	  (unspec:V4SF [(match_operand:V4SF 1 "nonimmediate_operand" "xm")]
		       UNSPEC_RCP)
	  (match_operand:V4SF 2 "register_operand" "0")
	  (const_int 1)))]
  "TARGET_SSE"
  "rcpss\t{%1, %0|%0, %1}"
  [(set_attr "type" "sse")
   (set_attr "atom_sse_attr" "rcp")
   (set_attr "mode" "SF")])

(define_expand "sqrtv8sf2"
  [(set (match_operand:V8SF 0 "register_operand" "")
	(sqrt:V8SF (match_operand:V8SF 1 "nonimmediate_operand" "")))]
  "TARGET_AVX"
{
  if (TARGET_SSE_MATH && TARGET_RECIP && !optimize_insn_for_size_p ()
      && flag_finite_math_only && !flag_trapping_math
      && flag_unsafe_math_optimizations)
    {
      ix86_emit_swsqrtsf (operands[0], operands[1], V8SFmode, 0);
      DONE;
    }
})

(define_insn "avx_sqrtv8sf2"
  [(set (match_operand:V8SF 0 "register_operand" "=x")
	(sqrt:V8SF (match_operand:V8SF 1 "nonimmediate_operand" "xm")))]
  "TARGET_AVX"
  "vsqrtps\t{%1, %0|%0, %1}"
  [(set_attr "type" "sse")
   (set_attr "prefix" "vex")
   (set_attr "mode" "V8SF")])

(define_expand "sqrtv4sf2"
  [(set (match_operand:V4SF 0 "register_operand" "")
	(sqrt:V4SF (match_operand:V4SF 1 "nonimmediate_operand" "")))]
  "TARGET_SSE"
{
  if (TARGET_SSE_MATH && TARGET_RECIP && optimize_insn_for_speed_p ()
      && flag_finite_math_only && !flag_trapping_math
      && flag_unsafe_math_optimizations)
    {
      ix86_emit_swsqrtsf (operands[0], operands[1], V4SFmode, 0);
      DONE;
    }
})

(define_insn "sse_sqrtv4sf2"
  [(set (match_operand:V4SF 0 "register_operand" "=x")
	(sqrt:V4SF (match_operand:V4SF 1 "nonimmediate_operand" "xm")))]
  "TARGET_SSE"
  "%vsqrtps\t{%1, %0|%0, %1}"
  [(set_attr "type" "sse")
   (set_attr "atom_sse_attr" "sqrt")
   (set_attr "prefix" "maybe_vex")
   (set_attr "mode" "V4SF")])

(define_insn "sqrtv4df2"
  [(set (match_operand:V4DF 0 "register_operand" "=x")
	(sqrt:V4DF (match_operand:V4DF 1 "nonimmediate_operand" "xm")))]
  "TARGET_AVX"
  "vsqrtpd\t{%1, %0|%0, %1}"
  [(set_attr "type" "sse")
   (set_attr "prefix" "vex")
   (set_attr "mode" "V4DF")])

(define_insn "sqrtv2df2"
  [(set (match_operand:V2DF 0 "register_operand" "=x")
	(sqrt:V2DF (match_operand:V2DF 1 "nonimmediate_operand" "xm")))]
  "TARGET_SSE2"
  "%vsqrtpd\t{%1, %0|%0, %1}"
  [(set_attr "type" "sse")
   (set_attr "prefix" "maybe_vex")
   (set_attr "mode" "V2DF")])

(define_insn "*avx_vmsqrt<mode>2"
  [(set (match_operand:SSEMODEF2P 0 "register_operand" "=x")
	(vec_merge:SSEMODEF2P
	  (sqrt:SSEMODEF2P
	    (match_operand:SSEMODEF2P 1 "nonimmediate_operand" "xm"))
	  (match_operand:SSEMODEF2P 2 "register_operand" "x")
	  (const_int 1)))]
  "AVX_VEC_FLOAT_MODE_P (<MODE>mode)"
  "vsqrt<ssescalarmodesuffix>\t{%1, %2, %0|%0, %2, %1}"
  [(set_attr "type" "sse")
   (set_attr "prefix" "vex")
   (set_attr "mode" "<ssescalarmode>")])

(define_insn "<sse>_vmsqrt<mode>2"
  [(set (match_operand:SSEMODEF2P 0 "register_operand" "=x")
	(vec_merge:SSEMODEF2P
	  (sqrt:SSEMODEF2P
	    (match_operand:SSEMODEF2P 1 "nonimmediate_operand" "xm"))
	  (match_operand:SSEMODEF2P 2 "register_operand" "0")
	  (const_int 1)))]
  "SSE_VEC_FLOAT_MODE_P (<MODE>mode)"
  "sqrt<ssescalarmodesuffix>\t{%1, %0|%0, %1}"
  [(set_attr "type" "sse")
   (set_attr "atom_sse_attr" "sqrt")
   (set_attr "mode" "<ssescalarmode>")])

(define_expand "rsqrtv8sf2"
  [(set (match_operand:V8SF 0 "register_operand" "")
	(unspec:V8SF
	  [(match_operand:V8SF 1 "nonimmediate_operand" "")] UNSPEC_RSQRT))]
  "TARGET_AVX && TARGET_SSE_MATH"
{
  ix86_emit_swsqrtsf (operands[0], operands[1], V8SFmode, 1);
  DONE;
})

(define_insn "avx_rsqrtv8sf2"
  [(set (match_operand:V8SF 0 "register_operand" "=x")
	(unspec:V8SF
	  [(match_operand:V8SF 1 "nonimmediate_operand" "xm")] UNSPEC_RSQRT))]
  "TARGET_AVX"
  "vrsqrtps\t{%1, %0|%0, %1}"
  [(set_attr "type" "sse")
   (set_attr "prefix" "vex")
   (set_attr "mode" "V8SF")])

(define_expand "rsqrtv4sf2"
  [(set (match_operand:V4SF 0 "register_operand" "")
	(unspec:V4SF
	  [(match_operand:V4SF 1 "nonimmediate_operand" "")] UNSPEC_RSQRT))]
  "TARGET_SSE_MATH"
{
  ix86_emit_swsqrtsf (operands[0], operands[1], V4SFmode, 1);
  DONE;
})

(define_insn "sse_rsqrtv4sf2"
  [(set (match_operand:V4SF 0 "register_operand" "=x")
	(unspec:V4SF
	  [(match_operand:V4SF 1 "nonimmediate_operand" "xm")] UNSPEC_RSQRT))]
  "TARGET_SSE"
  "%vrsqrtps\t{%1, %0|%0, %1}"
  [(set_attr "type" "sse")
   (set_attr "prefix" "maybe_vex")
   (set_attr "mode" "V4SF")])

(define_insn "*avx_vmrsqrtv4sf2"
  [(set (match_operand:V4SF 0 "register_operand" "=x")
	(vec_merge:V4SF
	  (unspec:V4SF [(match_operand:V4SF 1 "nonimmediate_operand" "xm")]
		       UNSPEC_RSQRT)
	  (match_operand:V4SF 2 "register_operand" "x")
	  (const_int 1)))]
  "TARGET_AVX"
  "vrsqrtss\t{%1, %2, %0|%0, %2, %1}"
  [(set_attr "type" "sse")
   (set_attr "prefix" "vex")
   (set_attr "mode" "SF")])

(define_insn "sse_vmrsqrtv4sf2"
  [(set (match_operand:V4SF 0 "register_operand" "=x")
	(vec_merge:V4SF
	  (unspec:V4SF [(match_operand:V4SF 1 "nonimmediate_operand" "xm")]
		       UNSPEC_RSQRT)
	  (match_operand:V4SF 2 "register_operand" "0")
	  (const_int 1)))]
  "TARGET_SSE"
  "rsqrtss\t{%1, %0|%0, %1}"
  [(set_attr "type" "sse")
   (set_attr "mode" "SF")])

;; ??? For !flag_finite_math_only, the representation with SMIN/SMAX
;; isn't really correct, as those rtl operators aren't defined when
;; applied to NaNs.  Hopefully the optimizers won't get too smart on us.

(define_expand "<code><mode>3"
  [(set (match_operand:AVX256MODEF2P 0 "register_operand" "")
	(smaxmin:AVX256MODEF2P
	  (match_operand:AVX256MODEF2P 1 "nonimmediate_operand" "")
	  (match_operand:AVX256MODEF2P 2 "nonimmediate_operand" "")))]
  "AVX256_VEC_FLOAT_MODE_P (<MODE>mode)"
{
  if (!flag_finite_math_only)
    operands[1] = force_reg (<MODE>mode, operands[1]);
  ix86_fixup_binary_operands_no_copy (<CODE>, <MODE>mode, operands);
})

(define_expand "<code><mode>3"
  [(set (match_operand:SSEMODEF2P 0 "register_operand" "")
	(smaxmin:SSEMODEF2P
	  (match_operand:SSEMODEF2P 1 "nonimmediate_operand" "")
	  (match_operand:SSEMODEF2P 2 "nonimmediate_operand" "")))]
  "SSE_VEC_FLOAT_MODE_P (<MODE>mode)"
{
  if (!flag_finite_math_only)
    operands[1] = force_reg (<MODE>mode, operands[1]);
  ix86_fixup_binary_operands_no_copy (<CODE>, <MODE>mode, operands);
})

(define_insn "*avx_<code><mode>3_finite"
  [(set (match_operand:AVXMODEF2P 0 "register_operand" "=x")
	(smaxmin:AVXMODEF2P
	  (match_operand:AVXMODEF2P 1 "nonimmediate_operand" "%x")
	  (match_operand:AVXMODEF2P 2 "nonimmediate_operand" "xm")))]
  "AVX_VEC_FLOAT_MODE_P (<MODE>mode) && flag_finite_math_only
   && ix86_binary_operator_ok (<CODE>, <MODE>mode, operands)"
  "v<maxmin_float><ssemodesuffix>\t{%2, %1, %0|%0, %1, %2}"
  [(set_attr "type" "sseadd")
   (set_attr "prefix" "vex")
   (set_attr "mode" "<MODE>")])

(define_insn "*<code><mode>3_finite"
  [(set (match_operand:SSEMODEF2P 0 "register_operand" "=x")
	(smaxmin:SSEMODEF2P
	  (match_operand:SSEMODEF2P 1 "nonimmediate_operand" "%0")
	  (match_operand:SSEMODEF2P 2 "nonimmediate_operand" "xm")))]
  "SSE_VEC_FLOAT_MODE_P (<MODE>mode) && flag_finite_math_only
   && ix86_binary_operator_ok (<CODE>, <MODE>mode, operands)"
  "<maxmin_float><ssemodesuffix>\t{%2, %0|%0, %2}"
  [(set_attr "type" "sseadd")
   (set_attr "mode" "<MODE>")])

(define_insn "*avx_<code><mode>3"
  [(set (match_operand:AVXMODEF2P 0 "register_operand" "=x")
	(smaxmin:AVXMODEF2P
	  (match_operand:AVXMODEF2P 1 "nonimmediate_operand" "%x")
	  (match_operand:AVXMODEF2P 2 "nonimmediate_operand" "xm")))]
  "AVX_VEC_FLOAT_MODE_P (<MODE>mode)"
  "v<maxmin_float><ssemodesuffix>\t{%2, %1, %0|%0, %1, %2}"
  [(set_attr "type" "sseadd")
   (set_attr "prefix" "vex")
   (set_attr "mode" "<avxvecmode>")])

(define_insn "*<code><mode>3"
  [(set (match_operand:SSEMODEF2P 0 "register_operand" "=x")
	(smaxmin:SSEMODEF2P
	  (match_operand:SSEMODEF2P 1 "register_operand" "0")
	  (match_operand:SSEMODEF2P 2 "nonimmediate_operand" "xm")))]
  "SSE_VEC_FLOAT_MODE_P (<MODE>mode)"
  "<maxmin_float><ssemodesuffix>\t{%2, %0|%0, %2}"
  [(set_attr "type" "sseadd")
   (set_attr "mode" "<MODE>")])

(define_insn "*avx_vm<code><mode>3"
  [(set (match_operand:SSEMODEF2P 0 "register_operand" "=x")
	(vec_merge:SSEMODEF2P
	  (smaxmin:SSEMODEF2P
	    (match_operand:SSEMODEF2P 1 "register_operand" "x")
	    (match_operand:SSEMODEF2P 2 "nonimmediate_operand" "xm"))
	 (match_dup 1)
	 (const_int 1)))]
  "AVX128_VEC_FLOAT_MODE_P (<MODE>mode)"
  "v<maxmin_float><ssescalarmodesuffix>\t{%2, %1, %0|%0, %1, %2}"
  [(set_attr "type" "sse")
   (set_attr "prefix" "vex")
   (set_attr "mode" "<ssescalarmode>")])

(define_insn "<sse>_vm<code><mode>3"
  [(set (match_operand:SSEMODEF2P 0 "register_operand" "=x")
	(vec_merge:SSEMODEF2P
	  (smaxmin:SSEMODEF2P
	    (match_operand:SSEMODEF2P 1 "register_operand" "0")
	    (match_operand:SSEMODEF2P 2 "nonimmediate_operand" "xm"))
	 (match_dup 1)
	 (const_int 1)))]
  "SSE_VEC_FLOAT_MODE_P (<MODE>mode)"
  "<maxmin_float><ssescalarmodesuffix>\t{%2, %0|%0, %2}"
  [(set_attr "type" "sseadd")
   (set_attr "mode" "<ssescalarmode>")])

;; These versions of the min/max patterns implement exactly the operations
;;   min = (op1 < op2 ? op1 : op2)
;;   max = (!(op1 < op2) ? op1 : op2)
;; Their operands are not commutative, and thus they may be used in the
;; presence of -0.0 and NaN.

(define_insn "*avx_ieee_smin<mode>3"
  [(set (match_operand:AVXMODEF2P 0 "register_operand" "=x")
	(unspec:AVXMODEF2P
	  [(match_operand:AVXMODEF2P 1 "register_operand" "x")
	   (match_operand:AVXMODEF2P 2 "nonimmediate_operand" "xm")]
	 UNSPEC_IEEE_MIN))]
  "AVX_VEC_FLOAT_MODE_P (<MODE>mode)"
  "vmin<ssemodesuffix>\t{%2, %1, %0|%0, %1, %2}"
  [(set_attr "type" "sseadd")
   (set_attr "prefix" "vex")
   (set_attr "mode" "<avxvecmode>")])

(define_insn "*avx_ieee_smax<mode>3"
  [(set (match_operand:AVXMODEF2P 0 "register_operand" "=x")
	(unspec:AVXMODEF2P
	  [(match_operand:AVXMODEF2P 1 "register_operand" "x")
	   (match_operand:AVXMODEF2P 2 "nonimmediate_operand" "xm")]
	 UNSPEC_IEEE_MAX))]
  "AVX_VEC_FLOAT_MODE_P (<MODE>mode)"
  "vmax<ssemodesuffix>\t{%2, %1, %0|%0, %1, %2}"
  [(set_attr "type" "sseadd")
   (set_attr "prefix" "vex")
   (set_attr "mode" "<avxvecmode>")])

(define_insn "*ieee_smin<mode>3"
  [(set (match_operand:SSEMODEF2P 0 "register_operand" "=x")
	(unspec:SSEMODEF2P
	  [(match_operand:SSEMODEF2P 1 "register_operand" "0")
	   (match_operand:SSEMODEF2P 2 "nonimmediate_operand" "xm")]
	 UNSPEC_IEEE_MIN))]
  "SSE_VEC_FLOAT_MODE_P (<MODE>mode)"
  "min<ssemodesuffix>\t{%2, %0|%0, %2}"
  [(set_attr "type" "sseadd")
   (set_attr "mode" "<MODE>")])

(define_insn "*ieee_smax<mode>3"
  [(set (match_operand:SSEMODEF2P 0 "register_operand" "=x")
	(unspec:SSEMODEF2P
	  [(match_operand:SSEMODEF2P 1 "register_operand" "0")
	   (match_operand:SSEMODEF2P 2 "nonimmediate_operand" "xm")]
	 UNSPEC_IEEE_MAX))]
  "SSE_VEC_FLOAT_MODE_P (<MODE>mode)"
  "max<ssemodesuffix>\t{%2, %0|%0, %2}"
  [(set_attr "type" "sseadd")
   (set_attr "mode" "<MODE>")])

(define_insn "avx_addsubv8sf3"
  [(set (match_operand:V8SF 0 "register_operand" "=x")
	(vec_merge:V8SF
	  (plus:V8SF
	    (match_operand:V8SF 1 "register_operand" "x")
	    (match_operand:V8SF 2 "nonimmediate_operand" "xm"))
	  (minus:V8SF (match_dup 1) (match_dup 2))
	  (const_int 170)))]
  "TARGET_AVX"
  "vaddsubps\t{%2, %1, %0|%0, %1, %2}"
  [(set_attr "type" "sseadd")
   (set_attr "prefix" "vex")
   (set_attr "mode" "V8SF")])

(define_insn "avx_addsubv4df3"
  [(set (match_operand:V4DF 0 "register_operand" "=x")
	(vec_merge:V4DF
	  (plus:V4DF
	    (match_operand:V4DF 1 "register_operand" "x")
	    (match_operand:V4DF 2 "nonimmediate_operand" "xm"))
	  (minus:V4DF (match_dup 1) (match_dup 2))
	  (const_int 10)))]
  "TARGET_AVX"
  "vaddsubpd\t{%2, %1, %0|%0, %1, %2}"
  [(set_attr "type" "sseadd")
   (set_attr "prefix" "vex")
   (set_attr "mode" "V4DF")])

(define_insn "*avx_addsubv4sf3"
  [(set (match_operand:V4SF 0 "register_operand" "=x")
	(vec_merge:V4SF
	  (plus:V4SF
	    (match_operand:V4SF 1 "register_operand" "x")
	    (match_operand:V4SF 2 "nonimmediate_operand" "xm"))
	  (minus:V4SF (match_dup 1) (match_dup 2))
	  (const_int 10)))]
  "TARGET_AVX"
  "vaddsubps\t{%2, %1, %0|%0, %1, %2}"
  [(set_attr "type" "sseadd")
   (set_attr "prefix" "vex")
   (set_attr "mode" "V4SF")])

(define_insn "sse3_addsubv4sf3"
  [(set (match_operand:V4SF 0 "register_operand" "=x")
	(vec_merge:V4SF
	  (plus:V4SF
	    (match_operand:V4SF 1 "register_operand" "0")
	    (match_operand:V4SF 2 "nonimmediate_operand" "xm"))
	  (minus:V4SF (match_dup 1) (match_dup 2))
	  (const_int 10)))]
  "TARGET_SSE3"
  "addsubps\t{%2, %0|%0, %2}"
  [(set_attr "type" "sseadd")
   (set_attr "prefix_rep" "1")
   (set_attr "mode" "V4SF")])

(define_insn "*avx_addsubv2df3"
  [(set (match_operand:V2DF 0 "register_operand" "=x")
	(vec_merge:V2DF
	  (plus:V2DF
	    (match_operand:V2DF 1 "register_operand" "x")
	    (match_operand:V2DF 2 "nonimmediate_operand" "xm"))
	  (minus:V2DF (match_dup 1) (match_dup 2))
	  (const_int 2)))]
  "TARGET_AVX"
  "vaddsubpd\t{%2, %1, %0|%0, %1, %2}"
  [(set_attr "type" "sseadd")
   (set_attr "prefix" "vex")
   (set_attr "mode" "V2DF")])

(define_insn "sse3_addsubv2df3"
  [(set (match_operand:V2DF 0 "register_operand" "=x")
	(vec_merge:V2DF
	  (plus:V2DF
	    (match_operand:V2DF 1 "register_operand" "0")
	    (match_operand:V2DF 2 "nonimmediate_operand" "xm"))
	  (minus:V2DF (match_dup 1) (match_dup 2))
	  (const_int 2)))]
  "TARGET_SSE3"
  "addsubpd\t{%2, %0|%0, %2}"
  [(set_attr "type" "sseadd")
   (set_attr "atom_unit" "complex")
   (set_attr "mode" "V2DF")])

(define_insn "avx_h<plusminus_insn>v4df3"
  [(set (match_operand:V4DF 0 "register_operand" "=x")
	(vec_concat:V4DF
	  (vec_concat:V2DF
	    (plusminus:DF
	      (vec_select:DF
		(match_operand:V4DF 1 "register_operand" "x")
		(parallel [(const_int 0)]))
	      (vec_select:DF (match_dup 1) (parallel [(const_int 1)])))
	    (plusminus:DF
	      (vec_select:DF (match_dup 1) (parallel [(const_int 2)]))
	      (vec_select:DF (match_dup 1) (parallel [(const_int 3)]))))
	  (vec_concat:V2DF
	    (plusminus:DF
	      (vec_select:DF
		(match_operand:V4DF 2 "nonimmediate_operand" "xm")
		(parallel [(const_int 0)]))
	      (vec_select:DF (match_dup 2) (parallel [(const_int 1)])))
	    (plusminus:DF
	      (vec_select:DF (match_dup 2) (parallel [(const_int 2)]))
	      (vec_select:DF (match_dup 2) (parallel [(const_int 3)]))))))]
  "TARGET_AVX"
  "vh<plusminus_mnemonic>pd\t{%2, %1, %0|%0, %1, %2}"
  [(set_attr "type" "sseadd")
   (set_attr "prefix" "vex")
   (set_attr "mode" "V4DF")])

(define_insn "avx_h<plusminus_insn>v8sf3"
  [(set (match_operand:V8SF 0 "register_operand" "=x")
	(vec_concat:V8SF
	  (vec_concat:V4SF
	    (vec_concat:V2SF
	      (plusminus:SF
		(vec_select:SF
		  (match_operand:V8SF 1 "register_operand" "x")
		  (parallel [(const_int 0)]))
		(vec_select:SF (match_dup 1) (parallel [(const_int 1)])))
	      (plusminus:SF
		(vec_select:SF (match_dup 1) (parallel [(const_int 2)]))
		(vec_select:SF (match_dup 1) (parallel [(const_int 3)]))))
	    (vec_concat:V2SF
	      (plusminus:SF
		(vec_select:SF
		  (match_operand:V8SF 2 "nonimmediate_operand" "xm")
		  (parallel [(const_int 0)]))
		(vec_select:SF (match_dup 2) (parallel [(const_int 1)])))
	      (plusminus:SF
		(vec_select:SF (match_dup 2) (parallel [(const_int 2)]))
		(vec_select:SF (match_dup 2) (parallel [(const_int 3)])))))
	  (vec_concat:V4SF
	    (vec_concat:V2SF
	      (plusminus:SF
		(vec_select:SF (match_dup 1) (parallel [(const_int 4)]))
		(vec_select:SF (match_dup 1) (parallel [(const_int 5)])))
	      (plusminus:SF
		(vec_select:SF (match_dup 1) (parallel [(const_int 6)]))
		(vec_select:SF (match_dup 1) (parallel [(const_int 7)]))))
	    (vec_concat:V2SF
	      (plusminus:SF
		(vec_select:SF (match_dup 2) (parallel [(const_int 4)]))
		(vec_select:SF (match_dup 2) (parallel [(const_int 5)])))
	      (plusminus:SF
		(vec_select:SF (match_dup 2) (parallel [(const_int 6)]))
		(vec_select:SF (match_dup 2) (parallel [(const_int 7)])))))))]
  "TARGET_AVX"
  "vh<plusminus_mnemonic>ps\t{%2, %1, %0|%0, %1, %2}"
  [(set_attr "type" "sseadd")
   (set_attr "prefix" "vex")
   (set_attr "mode" "V8SF")])

(define_insn "*avx_h<plusminus_insn>v4sf3"
  [(set (match_operand:V4SF 0 "register_operand" "=x")
	(vec_concat:V4SF
	  (vec_concat:V2SF
	    (plusminus:SF
	      (vec_select:SF
		(match_operand:V4SF 1 "register_operand" "x")
		(parallel [(const_int 0)]))
	      (vec_select:SF (match_dup 1) (parallel [(const_int 1)])))
	    (plusminus:SF
	      (vec_select:SF (match_dup 1) (parallel [(const_int 2)]))
	      (vec_select:SF (match_dup 1) (parallel [(const_int 3)]))))
	  (vec_concat:V2SF
	    (plusminus:SF
	      (vec_select:SF
		(match_operand:V4SF 2 "nonimmediate_operand" "xm")
		(parallel [(const_int 0)]))
	      (vec_select:SF (match_dup 2) (parallel [(const_int 1)])))
	    (plusminus:SF
	      (vec_select:SF (match_dup 2) (parallel [(const_int 2)]))
	      (vec_select:SF (match_dup 2) (parallel [(const_int 3)]))))))]
  "TARGET_AVX"
  "vh<plusminus_mnemonic>ps\t{%2, %1, %0|%0, %1, %2}"
  [(set_attr "type" "sseadd")
   (set_attr "prefix" "vex")
   (set_attr "mode" "V4SF")])

(define_insn "sse3_h<plusminus_insn>v4sf3"
  [(set (match_operand:V4SF 0 "register_operand" "=x")
	(vec_concat:V4SF
	  (vec_concat:V2SF
	    (plusminus:SF
	      (vec_select:SF
		(match_operand:V4SF 1 "register_operand" "0")
		(parallel [(const_int 0)]))
	      (vec_select:SF (match_dup 1) (parallel [(const_int 1)])))
	    (plusminus:SF
	      (vec_select:SF (match_dup 1) (parallel [(const_int 2)]))
	      (vec_select:SF (match_dup 1) (parallel [(const_int 3)]))))
	  (vec_concat:V2SF
	    (plusminus:SF
	      (vec_select:SF
		(match_operand:V4SF 2 "nonimmediate_operand" "xm")
		(parallel [(const_int 0)]))
	      (vec_select:SF (match_dup 2) (parallel [(const_int 1)])))
	    (plusminus:SF
	      (vec_select:SF (match_dup 2) (parallel [(const_int 2)]))
	      (vec_select:SF (match_dup 2) (parallel [(const_int 3)]))))))]
  "TARGET_SSE3"
  "h<plusminus_mnemonic>ps\t{%2, %0|%0, %2}"
  [(set_attr "type" "sseadd")
   (set_attr "atom_unit" "complex")
   (set_attr "prefix_rep" "1")
   (set_attr "mode" "V4SF")])

(define_insn "*avx_h<plusminus_insn>v2df3"
  [(set (match_operand:V2DF 0 "register_operand" "=x")
	(vec_concat:V2DF
	  (plusminus:DF
	    (vec_select:DF
	      (match_operand:V2DF 1 "register_operand" "x")
	      (parallel [(const_int 0)]))
	    (vec_select:DF (match_dup 1) (parallel [(const_int 1)])))
	  (plusminus:DF
	    (vec_select:DF
	      (match_operand:V2DF 2 "nonimmediate_operand" "xm")
	      (parallel [(const_int 0)]))
	    (vec_select:DF (match_dup 2) (parallel [(const_int 1)])))))]
  "TARGET_AVX"
  "vh<plusminus_mnemonic>pd\t{%2, %1, %0|%0, %1, %2}"
  [(set_attr "type" "sseadd")
   (set_attr "prefix" "vex")
   (set_attr "mode" "V2DF")])

(define_insn "sse3_h<plusminus_insn>v2df3"
  [(set (match_operand:V2DF 0 "register_operand" "=x")
	(vec_concat:V2DF
	  (plusminus:DF
	    (vec_select:DF
	      (match_operand:V2DF 1 "register_operand" "0")
	      (parallel [(const_int 0)]))
	    (vec_select:DF (match_dup 1) (parallel [(const_int 1)])))
	  (plusminus:DF
	    (vec_select:DF
	      (match_operand:V2DF 2 "nonimmediate_operand" "xm")
	      (parallel [(const_int 0)]))
	    (vec_select:DF (match_dup 2) (parallel [(const_int 1)])))))]
  "TARGET_SSE3"
  "h<plusminus_mnemonic>pd\t{%2, %0|%0, %2}"
  [(set_attr "type" "sseadd")
   (set_attr "mode" "V2DF")])

(define_expand "reduc_splus_v8sf"
  [(match_operand:V8SF 0 "register_operand" "")
   (match_operand:V8SF 1 "register_operand" "")]
  "TARGET_AVX"
{
  rtx tmp = gen_reg_rtx (V8SFmode);
  rtx tmp2 = gen_reg_rtx (V8SFmode);
  emit_insn (gen_avx_haddv8sf3 (tmp, operands[1], operands[1]));
  emit_insn (gen_avx_haddv8sf3 (tmp2, tmp, tmp));
  emit_insn (gen_avx_vperm2f128v8sf3 (tmp, tmp2, tmp2, GEN_INT (1)));
  emit_insn (gen_addv8sf3 (operands[0], tmp, tmp2));
  DONE;
})

(define_expand "reduc_splus_v4sf"
  [(match_operand:V4SF 0 "register_operand" "")
   (match_operand:V4SF 1 "register_operand" "")]
  "TARGET_SSE"
{
  if (TARGET_SSE3)
    {
      rtx tmp = gen_reg_rtx (V4SFmode);
      emit_insn (gen_sse3_haddv4sf3 (tmp, operands[1], operands[1]));
      emit_insn (gen_sse3_haddv4sf3 (operands[0], tmp, tmp));
    }
  else
    ix86_expand_reduc_v4sf (gen_addv4sf3, operands[0], operands[1]);
  DONE;
})

(define_expand "reduc_splus_v4df"
  [(match_operand:V4DF 0 "register_operand" "")
   (match_operand:V4DF 1 "register_operand" "")]
  "TARGET_AVX"
{
  rtx tmp = gen_reg_rtx (V4DFmode);
  rtx tmp2 = gen_reg_rtx (V4DFmode);
  emit_insn (gen_avx_haddv4df3 (tmp, operands[1], operands[1]));
  emit_insn (gen_avx_vperm2f128v4df3 (tmp2, tmp, tmp, GEN_INT (1)));
  emit_insn (gen_addv4df3 (operands[0], tmp, tmp2));
  DONE;
})

(define_expand "reduc_splus_v2df"
  [(match_operand:V2DF 0 "register_operand" "")
   (match_operand:V2DF 1 "register_operand" "")]
  "TARGET_SSE3"
{
  emit_insn (gen_sse3_haddv2df3 (operands[0], operands[1], operands[1]));
  DONE;
})

(define_expand "reduc_smax_v4sf"
  [(match_operand:V4SF 0 "register_operand" "")
   (match_operand:V4SF 1 "register_operand" "")]
  "TARGET_SSE"
{
  ix86_expand_reduc_v4sf (gen_smaxv4sf3, operands[0], operands[1]);
  DONE;
})

(define_expand "reduc_smin_v4sf"
  [(match_operand:V4SF 0 "register_operand" "")
   (match_operand:V4SF 1 "register_operand" "")]
  "TARGET_SSE"
{
  ix86_expand_reduc_v4sf (gen_sminv4sf3, operands[0], operands[1]);
  DONE;
})

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Parallel floating point comparisons
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define_insn "avx_cmp<ssemodesuffix><mode>3"
  [(set (match_operand:AVXMODEF2P 0 "register_operand" "=x")
	(unspec:AVXMODEF2P
	  [(match_operand:AVXMODEF2P 1 "register_operand" "x")
	   (match_operand:AVXMODEF2P 2 "nonimmediate_operand" "xm")
	   (match_operand:SI 3 "const_0_to_31_operand" "n")]
	  UNSPEC_PCMP))]
  "TARGET_AVX"
  "vcmp<ssemodesuffix>\t{%3, %2, %1, %0|%0, %1, %2, %3}"
  [(set_attr "type" "ssecmp")
   (set_attr "length_immediate" "1")
   (set_attr "prefix" "vex")
   (set_attr "mode" "<MODE>")])

(define_insn "avx_cmp<ssescalarmodesuffix><mode>3"
  [(set (match_operand:SSEMODEF2P 0 "register_operand" "=x")
	(vec_merge:SSEMODEF2P
	  (unspec:SSEMODEF2P
	    [(match_operand:SSEMODEF2P 1 "register_operand" "x")
	     (match_operand:SSEMODEF2P 2 "nonimmediate_operand" "xm")
	     (match_operand:SI 3 "const_0_to_31_operand" "n")]
	    UNSPEC_PCMP)
	 (match_dup 1)
	 (const_int 1)))]
  "TARGET_AVX"
  "vcmp<ssescalarmodesuffix>\t{%3, %2, %1, %0|%0, %1, %2, %3}"
  [(set_attr "type" "ssecmp")
   (set_attr "length_immediate" "1")
   (set_attr "prefix" "vex")
   (set_attr "mode" "<ssescalarmode>")])

;; We don't promote 128bit vector compare intrinsics. But vectorizer
;; may generate 256bit vector compare instructions.
(define_insn "*avx_maskcmp<mode>3"
  [(set (match_operand:AVXMODEF2P 0 "register_operand" "=x")
	(match_operator:AVXMODEF2P 3 "avx_comparison_float_operator"
		[(match_operand:AVXMODEF2P 1 "register_operand" "x")
		 (match_operand:AVXMODEF2P 2 "nonimmediate_operand" "xm")]))]
  "AVX_VEC_FLOAT_MODE_P (<MODE>mode)"
  "vcmp%D3<ssemodesuffix>\t{%2, %1, %0|%0, %1, %2}"
  [(set_attr "type" "ssecmp")
   (set_attr "prefix" "vex")
   (set_attr "length_immediate" "1")
   (set_attr "mode" "<avxvecmode>")])

(define_insn "<sse>_maskcmp<mode>3"
  [(set (match_operand:SSEMODEF4 0 "register_operand" "=x")
	(match_operator:SSEMODEF4 3 "sse_comparison_operator"
		[(match_operand:SSEMODEF4 1 "register_operand" "0")
		 (match_operand:SSEMODEF4 2 "nonimmediate_operand" "xm")]))]
  "!TARGET_XOP 
  && (SSE_FLOAT_MODE_P (<MODE>mode) || SSE_VEC_FLOAT_MODE_P (<MODE>mode))"
  "cmp%D3<ssemodesuffix>\t{%2, %0|%0, %2}"
  [(set_attr "type" "ssecmp")
   (set_attr "length_immediate" "1")
   (set_attr "mode" "<MODE>")])

(define_insn "*avx_vmmaskcmp<mode>3"
  [(set (match_operand:SSEMODEF2P 0 "register_operand" "=x")
	(vec_merge:SSEMODEF2P
	 (match_operator:SSEMODEF2P 3 "sse_comparison_operator"
		[(match_operand:SSEMODEF2P 1 "register_operand" "x")
		 (match_operand:SSEMODEF2P 2 "nonimmediate_operand" "xm")])
	 (match_dup 1)
	 (const_int 1)))]
  "AVX_VEC_FLOAT_MODE_P (<MODE>mode)"
  "vcmp%D3<ssescalarmodesuffix>\t{%2, %1, %0|%0, %1, %2}"
  [(set_attr "type" "ssecmp")
   (set_attr "prefix" "vex")
   (set_attr "mode" "<ssescalarmode>")])

(define_insn "<sse>_vmmaskcmp<mode>3"
  [(set (match_operand:SSEMODEF2P 0 "register_operand" "=x")
	(vec_merge:SSEMODEF2P
	 (match_operator:SSEMODEF2P 3 "sse_comparison_operator"
		[(match_operand:SSEMODEF2P 1 "register_operand" "0")
		 (match_operand:SSEMODEF2P 2 "nonimmediate_operand" "xm")])
	 (match_dup 1)
	 (const_int 1)))]
  "SSE_VEC_FLOAT_MODE_P (<MODE>mode)"
  "cmp%D3<ssescalarmodesuffix>\t{%2, %0|%0, %2}"
  [(set_attr "type" "ssecmp")
   (set_attr "length_immediate" "1")
   (set_attr "mode" "<ssescalarmode>")])

(define_insn "<sse>_comi"
  [(set (reg:CCFP FLAGS_REG)
	(compare:CCFP
	  (vec_select:MODEF
	    (match_operand:<ssevecmode> 0 "register_operand" "x")
	    (parallel [(const_int 0)]))
	  (vec_select:MODEF
	    (match_operand:<ssevecmode> 1 "nonimmediate_operand" "xm")
	    (parallel [(const_int 0)]))))]
  "SSE_FLOAT_MODE_P (<MODE>mode)"
  "%vcomis<ssemodefsuffix>\t{%1, %0|%0, %1}"
  [(set_attr "type" "ssecomi")
   (set_attr "prefix" "maybe_vex")
   (set_attr "prefix_rep" "0")
   (set (attr "prefix_data16")
	(if_then_else (eq_attr "mode" "DF")
		      (const_string "1")
		      (const_string "0")))
   (set_attr "mode" "<MODE>")])

(define_insn "<sse>_ucomi"
  [(set (reg:CCFPU FLAGS_REG)
	(compare:CCFPU
	  (vec_select:MODEF
	    (match_operand:<ssevecmode> 0 "register_operand" "x")
	    (parallel [(const_int 0)]))
	  (vec_select:MODEF
	    (match_operand:<ssevecmode> 1 "nonimmediate_operand" "xm")
	    (parallel [(const_int 0)]))))]
  "SSE_FLOAT_MODE_P (<MODE>mode)"
  "%vucomis<ssemodefsuffix>\t{%1, %0|%0, %1}"
  [(set_attr "type" "ssecomi")
   (set_attr "prefix" "maybe_vex")
   (set_attr "prefix_rep" "0")
   (set (attr "prefix_data16")
	(if_then_else (eq_attr "mode" "DF")
		      (const_string "1")
		      (const_string "0")))
   (set_attr "mode" "<MODE>")])

(define_expand "vcond<mode>"
  [(set (match_operand:AVXMODEF2P 0 "register_operand" "")
        (if_then_else:AVXMODEF2P
          (match_operator 3 ""
            [(match_operand:AVXMODEF2P 4 "nonimmediate_operand" "")
             (match_operand:AVXMODEF2P 5 "nonimmediate_operand" "")])
          (match_operand:AVXMODEF2P 1 "general_operand" "")
          (match_operand:AVXMODEF2P 2 "general_operand" "")))]
  "(SSE_VEC_FLOAT_MODE_P (<MODE>mode)
    || AVX_VEC_FLOAT_MODE_P (<MODE>mode))"
{
  bool ok = ix86_expand_fp_vcond (operands);
  gcc_assert (ok);
  DONE;
})

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Parallel floating point logical operations
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define_insn "avx_andnot<mode>3"
  [(set (match_operand:AVXMODEF2P 0 "register_operand" "=x")
	(and:AVXMODEF2P
	  (not:AVXMODEF2P
	    (match_operand:AVXMODEF2P 1 "register_operand" "x"))
	  (match_operand:AVXMODEF2P 2 "nonimmediate_operand" "xm")))]
  "AVX_VEC_FLOAT_MODE_P (<MODE>mode)"
  "vandn<ssemodesuffix>\t{%2, %1, %0|%0, %1, %2}"
  [(set_attr "type" "sselog")
   (set_attr "prefix" "vex")
   (set_attr "mode" "<avxvecmode>")])

(define_insn "<sse>_andnot<mode>3"
  [(set (match_operand:SSEMODEF2P 0 "register_operand" "=x")
	(and:SSEMODEF2P
	  (not:SSEMODEF2P
	    (match_operand:SSEMODEF2P 1 "register_operand" "0"))
	  (match_operand:SSEMODEF2P 2 "nonimmediate_operand" "xm")))]
  "SSE_VEC_FLOAT_MODE_P (<MODE>mode)"
  "andn<ssemodesuffix>\t{%2, %0|%0, %2}"
  [(set_attr "type" "sselog")
   (set_attr "mode" "<MODE>")])

(define_expand "<code><mode>3"
  [(set (match_operand:AVX256MODEF2P 0 "register_operand" "")
	(any_logic:AVX256MODEF2P
	  (match_operand:AVX256MODEF2P 1 "nonimmediate_operand" "")
	  (match_operand:AVX256MODEF2P 2 "nonimmediate_operand" "")))]
  "AVX256_VEC_FLOAT_MODE_P (<MODE>mode)"
  "ix86_fixup_binary_operands_no_copy (<CODE>, <MODE>mode, operands);")

(define_insn "*avx_<code><mode>3"
  [(set (match_operand:AVXMODEF2P 0 "register_operand" "=x")
	(any_logic:AVXMODEF2P
	  (match_operand:AVXMODEF2P 1 "nonimmediate_operand" "%x")
	  (match_operand:AVXMODEF2P 2 "nonimmediate_operand" "xm")))]
  "AVX_VEC_FLOAT_MODE_P (<MODE>mode)
   && ix86_binary_operator_ok (<CODE>, <MODE>mode, operands)"
{
  if (TARGET_SSE_PACKED_SINGLE_INSN_OPTIMAL)
    return "v<logic>ps\t{%2, %1, %0|%0, %1, %2}";
  else
    return "v<logic><ssemodesuffix>\t{%2, %1, %0|%0, %1, %2}";
}
  [(set_attr "type" "sselog")
   (set_attr "prefix" "vex")
   (set_attr "mode" "<avxvecmode>")])

(define_expand "<code><mode>3"
  [(set (match_operand:SSEMODEF2P 0 "register_operand" "")
	(any_logic:SSEMODEF2P
	  (match_operand:SSEMODEF2P 1 "nonimmediate_operand" "")
	  (match_operand:SSEMODEF2P 2 "nonimmediate_operand" "")))]
  "SSE_VEC_FLOAT_MODE_P (<MODE>mode)"
  "ix86_fixup_binary_operands_no_copy (<CODE>, <MODE>mode, operands);")

(define_insn "*<code><mode>3"
  [(set (match_operand:SSEMODEF2P 0 "register_operand" "=x")
	(any_logic:SSEMODEF2P
	  (match_operand:SSEMODEF2P 1 "nonimmediate_operand" "%0")
	  (match_operand:SSEMODEF2P 2 "nonimmediate_operand" "xm")))]
  "SSE_VEC_FLOAT_MODE_P (<MODE>mode)
   && ix86_binary_operator_ok (<CODE>, <MODE>mode, operands)"
{
  if (TARGET_SSE_PACKED_SINGLE_INSN_OPTIMAL)
    return "<logic>ps\t{%2, %0|%0, %2}";
  else
    return "<logic><ssemodesuffix>\t{%2, %0|%0, %2}";
}
  [(set_attr "type" "sselog")
   (set_attr "mode" "<MODE>")])

(define_expand "copysign<mode>3"
  [(set (match_dup 4)
	(and:VEC_FLOAT_MODE
	  (not:VEC_FLOAT_MODE (match_dup 3))
	  (match_operand:VEC_FLOAT_MODE 1 "nonimmediate_operand" "")))
   (set (match_dup 5)
	(and:VEC_FLOAT_MODE (match_dup 3)
			    (match_operand:VEC_FLOAT_MODE 2 "nonimmediate_operand" "")))
   (set (match_operand:VEC_FLOAT_MODE 0 "register_operand" "")
	(ior:VEC_FLOAT_MODE (match_dup 4) (match_dup 5)))]
  ""
{
  operands[3] = ix86_build_signbit_mask (<MODE>mode, 1, 0);

  operands[4] = gen_reg_rtx (<MODE>mode);
  operands[5] = gen_reg_rtx (<MODE>mode);
})

;; Also define scalar versions.  These are used for abs, neg, and
;; conditional move.  Using subregs into vector modes causes register
;; allocation lossage.  These patterns do not allow memory operands
;; because the native instructions read the full 128-bits.

(define_insn "*avx_andnot<mode>3"
  [(set (match_operand:MODEF 0 "register_operand" "=x")
	(and:MODEF
	  (not:MODEF
	    (match_operand:MODEF 1 "register_operand" "x"))
	    (match_operand:MODEF 2 "register_operand" "x")))]
  "AVX_FLOAT_MODE_P (<MODE>mode)"
  "vandnp<ssemodefsuffix>\t{%2, %1, %0|%0, %1, %2}"
  [(set_attr "type" "sselog")
   (set_attr "prefix" "vex")
   (set_attr "mode" "<ssevecmode>")])

(define_insn "*andnot<mode>3"
  [(set (match_operand:MODEF 0 "register_operand" "=x")
	(and:MODEF
	  (not:MODEF
	    (match_operand:MODEF 1 "register_operand" "0"))
	    (match_operand:MODEF 2 "register_operand" "x")))]
  "SSE_FLOAT_MODE_P (<MODE>mode)"
  "andnp<ssemodefsuffix>\t{%2, %0|%0, %2}"
  [(set_attr "type" "sselog")
   (set_attr "mode" "<ssevecmode>")])

(define_insn "*avx_<code><mode>3"
  [(set (match_operand:MODEF 0 "register_operand" "=x")
	(any_logic:MODEF
	  (match_operand:MODEF 1 "register_operand" "x")
	  (match_operand:MODEF 2 "register_operand" "x")))]
  "AVX_FLOAT_MODE_P (<MODE>mode)"
{
  if (TARGET_SSE_PACKED_SINGLE_INSN_OPTIMAL)
    return "v<logic>ps\t{%2, %1, %0|%0, %1, %2}";
  else
    return "v<logic>p<ssemodefsuffix>\t{%2, %1, %0|%0, %1, %2}";
}
  [(set_attr "type" "sselog")
   (set_attr "prefix" "vex")
   (set_attr "mode" "<ssevecmode>")])

(define_insn "*<code><mode>3"
  [(set (match_operand:MODEF 0 "register_operand" "=x")
	(any_logic:MODEF
	  (match_operand:MODEF 1 "register_operand" "0")
	  (match_operand:MODEF 2 "register_operand" "x")))]
  "SSE_FLOAT_MODE_P (<MODE>mode)"
{
  if (TARGET_SSE_PACKED_SINGLE_INSN_OPTIMAL)
    return "<logic>ps\t{%2, %0|%0, %2}";
  else
    return "<logic>p<ssemodefsuffix>\t{%2, %0|%0, %2}";
}
  [(set_attr "type" "sselog")
   (set_attr "mode" "<ssevecmode>")])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; FMA4 floating point multiply/accumulate instructions.  This
;; includes the scalar version of the instructions as well as the
;; vector.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; In order to match (*a * *b) + *c, particularly when vectorizing, allow
;; combine to generate a multiply/add with two memory references.  We then
;; split this insn, into loading up the destination register with one of the
;; memory operations.  If we don't manage to split the insn, reload will
;; generate the appropriate moves.  The reason this is needed, is that combine
;; has already folded one of the memory references into both the multiply and
;; add insns, and it can't generate a new pseudo.  I.e.:
;;	(set (reg1) (mem (addr1)))
;;	(set (reg2) (mult (reg1) (mem (addr2))))
;;	(set (reg3) (plus (reg2) (mem (addr3))))
;;
;; ??? This is historic, pre-dating the gimple fma transformation.
;; We could now properly represent that only one memory operand is
;; allowed and not be penalized during optimization.

;; Intrinsic FMA operations.

;; The standard names for fma is only available with SSE math enabled.
(define_expand "fma<mode>4"
  [(set (match_operand:FMAMODE 0 "register_operand")
	(fma:FMAMODE
	  (match_operand:FMAMODE 1 "nonimmediate_operand")
	  (match_operand:FMAMODE 2 "nonimmediate_operand")
	  (match_operand:FMAMODE 3 "nonimmediate_operand")))]
  "(TARGET_FMA || TARGET_FMA4) && TARGET_SSE_MATH"
  "")

(define_expand "fms<mode>4"
  [(set (match_operand:FMAMODE 0 "register_operand")
	(fma:FMAMODE
	  (match_operand:FMAMODE 1 "nonimmediate_operand")
	  (match_operand:FMAMODE 2 "nonimmediate_operand")
	  (neg:FMAMODE (match_operand:FMAMODE 3 "nonimmediate_operand"))))]
  "(TARGET_FMA || TARGET_FMA4) && TARGET_SSE_MATH"
  "")

(define_expand "fnma<mode>4"
  [(set (match_operand:FMAMODE 0 "register_operand")
	(fma:FMAMODE
	  (neg:FMAMODE (match_operand:FMAMODE 1 "nonimmediate_operand"))
	  (match_operand:FMAMODE 2 "nonimmediate_operand")
	  (match_operand:FMAMODE 3 "nonimmediate_operand")))]
  "(TARGET_FMA || TARGET_FMA4) && TARGET_SSE_MATH"
  "")

(define_expand "fnms<mode>4"
  [(set (match_operand:FMAMODE 0 "register_operand")
	(fma:FMAMODE
	  (neg:FMAMODE (match_operand:FMAMODE 1 "nonimmediate_operand"))
	  (match_operand:FMAMODE 2 "nonimmediate_operand")
	  (neg:FMAMODE (match_operand:FMAMODE 3 "nonimmediate_operand"))))]
  "(TARGET_FMA || TARGET_FMA4) && TARGET_SSE_MATH"
  "")

;; The builtin for fma4intrin.h is not constrained by SSE math enabled.
(define_expand "fma4i_fmadd_<mode>"
  [(set (match_operand:FMAMODE 0 "register_operand")
	(fma:FMAMODE
	  (match_operand:FMAMODE 1 "nonimmediate_operand")
	  (match_operand:FMAMODE 2 "nonimmediate_operand")
	  (match_operand:FMAMODE 3 "nonimmediate_operand")))]
  "TARGET_FMA || TARGET_FMA4"
  "")

(define_insn "*fma4i_fmadd_<mode>"
  [(set (match_operand:FMAMODE 0 "register_operand" "=x,x")
	(fma:FMAMODE
	  (match_operand:FMAMODE 1 "nonimmediate_operand" "%x,x")
	  (match_operand:FMAMODE 2 "nonimmediate_operand" " x,m")
	  (match_operand:FMAMODE 3 "nonimmediate_operand" "xm,x")))]
  "TARGET_FMA4"
  "vfmadd<ssemodesuffix>\t{%3, %2, %1, %0|%0, %1, %2, %3}"
  [(set_attr "type" "ssemuladd")
   (set_attr "mode" "<MODE>")])

(define_insn "*fma4i_fmsub_<mode>"
  [(set (match_operand:FMAMODE 0 "register_operand" "=x,x")
	(fma:FMAMODE
	  (match_operand:FMAMODE 1 "nonimmediate_operand" "%x,x")
	  (match_operand:FMAMODE 2 "nonimmediate_operand" " x,m")
	  (neg:FMAMODE
	    (match_operand:FMAMODE 3 "nonimmediate_operand" "xm,x"))))]
  "TARGET_FMA4"
  "vfmsub<ssemodesuffix>\t{%3, %2, %1, %0|%0, %1, %2, %3}"
  [(set_attr "type" "ssemuladd")
   (set_attr "mode" "<MODE>")])

(define_insn "*fma4i_fnmadd_<mode>"
  [(set (match_operand:FMAMODE 0 "register_operand" "=x,x")
	(fma:FMAMODE
	  (neg:FMAMODE
	    (match_operand:FMAMODE 1 "nonimmediate_operand" "%x,x"))
	  (match_operand:FMAMODE   2 "nonimmediate_operand" " x,m")
	  (match_operand:FMAMODE   3 "nonimmediate_operand" "xm,x")))]
  "TARGET_FMA4"
  "vfnmadd<ssemodesuffix>\t{%3, %2, %1, %0|%0, %1, %2, %3}"
  [(set_attr "type" "ssemuladd")
   (set_attr "mode" "<MODE>")])

(define_insn "*fma4i_fnmsub_<mode>"
  [(set (match_operand:FMAMODE 0 "register_operand" "=x,x")
	(fma:FMAMODE
	  (neg:FMAMODE
	    (match_operand:FMAMODE 1 "nonimmediate_operand" "%x,x"))
	  (match_operand:FMAMODE   2 "nonimmediate_operand" " x,m")
	  (neg:FMAMODE
	    (match_operand:FMAMODE 3 "nonimmediate_operand" "xm,x"))))]
  "TARGET_FMA4"
  "vfnmsub<ssemodesuffix>\t{%3, %2, %1, %0|%0, %1, %2, %3}"
  [(set_attr "type" "ssemuladd")
   (set_attr "mode" "<MODE>")])

;; Scalar versions of the above.  Unlike ADDSS et al, these write the
;; entire destination register, with the high-order elements zeroed.

(define_expand "fma4i_vmfmadd_<mode>"
  [(set (match_operand:SSEMODEF2P 0 "register_operand")
	(vec_merge:SSEMODEF2P
	  (fma:SSEMODEF2P
	    (match_operand:SSEMODEF2P 1 "nonimmediate_operand")
	    (match_operand:SSEMODEF2P 2 "nonimmediate_operand")
	    (match_operand:SSEMODEF2P 3 "nonimmediate_operand"))
	  (match_dup 4)
	  (const_int 1)))]
  "TARGET_FMA4"
{
  operands[4] = CONST0_RTX (<MODE>mode);
})

(define_insn "*fma4i_vmfmadd_<mode>"
  [(set (match_operand:SSEMODEF2P 0 "register_operand" "=x,x")
	(vec_merge:SSEMODEF2P
	  (fma:SSEMODEF2P
	    (match_operand:SSEMODEF2P 1 "nonimmediate_operand" "%x,x")
	    (match_operand:SSEMODEF2P 2 "nonimmediate_operand" " x,m")
	    (match_operand:SSEMODEF2P 3 "nonimmediate_operand" "xm,x"))
	  (match_operand:SSEMODEF2P 4 "const0_operand" "")
	  (const_int 1)))]
  "TARGET_FMA4"
  "vfmadd<ssescalarmodesuffix>\t{%3, %2, %1, %0|%0, %1, %2, %3}"
  [(set_attr "type" "ssemuladd")
   (set_attr "mode" "<MODE>")])

(define_insn "*fma4i_vmfmsub_<mode>"
  [(set (match_operand:SSEMODEF2P 0 "register_operand" "=x,x")
	(vec_merge:SSEMODEF2P
	  (fma:SSEMODEF2P
	    (match_operand:SSEMODEF2P 1 "nonimmediate_operand" "%x,x")
	    (match_operand:SSEMODEF2P 2 "nonimmediate_operand" " x,m")
	    (neg:SSEMODEF2P
	      (match_operand:SSEMODEF2P 3 "nonimmediate_operand" "xm,x")))
	  (match_operand:SSEMODEF2P 4 "const0_operand" "")
	  (const_int 1)))]
  "TARGET_FMA4"
  "vfmsub<ssescalarmodesuffix>\t{%3, %2, %1, %0|%0, %1, %2, %3}"
  [(set_attr "type" "ssemuladd")
   (set_attr "mode" "<MODE>")])

(define_insn "*fma4i_vmfnmadd_<mode>"
  [(set (match_operand:SSEMODEF2P 0 "register_operand" "=x,x")
	(vec_merge:SSEMODEF2P
	  (fma:SSEMODEF2P
	    (neg:SSEMODEF2P
	      (match_operand:SSEMODEF2P 1 "nonimmediate_operand" "%x,x"))
	    (match_operand:SSEMODEF2P   2 "nonimmediate_operand" " x,m")
	    (match_operand:SSEMODEF2P   3 "nonimmediate_operand" "xm,x"))
	  (match_operand:SSEMODEF2P 4 "const0_operand" "")
	  (const_int 1)))]
  "TARGET_FMA4"
  "vfnmadd<ssescalarmodesuffix>\t{%3, %2, %1, %0|%0, %1, %2, %3}"
  [(set_attr "type" "ssemuladd")
   (set_attr "mode" "<MODE>")])

(define_insn "*fma4i_vmfnmsub_<mode>"
  [(set (match_operand:SSEMODEF2P 0 "register_operand" "=x,x")
	(vec_merge:SSEMODEF2P
	  (fma:SSEMODEF2P
	    (neg:SSEMODEF2P
	      (match_operand:SSEMODEF2P 1 "nonimmediate_operand" "%x,x"))
	    (match_operand:SSEMODEF2P   2 "nonimmediate_operand" " x,m")
	    (neg:SSEMODEF2P
	      (match_operand:SSEMODEF2P   3 "nonimmediate_operand" "xm,x")))
	  (match_operand:SSEMODEF2P 4 "const0_operand" "")
	  (const_int 1)))]
  "TARGET_FMA4"
  "vfnmsub<ssescalarmodesuffix>\t{%3, %2, %1, %0|%0, %1, %2, %3}"
  [(set_attr "type" "ssemuladd")
   (set_attr "mode" "<MODE>")])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; FMA4 Parallel floating point multiply addsub and subadd operations.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; It would be possible to represent these without the UNSPEC as
;;
;; (vec_merge
;;   (fma op1 op2 op3)
;;   (fma op1 op2 (neg op3))
;;   (merge-const))
;;
;; But this doesn't seem useful in practice.

(define_expand "fmaddsub_<mode>"
  [(set (match_operand:AVXMODEF2P 0 "register_operand")
	(unspec:AVXMODEF2P
	  [(match_operand:AVXMODEF2P 1 "nonimmediate_operand")
	   (match_operand:AVXMODEF2P 2 "nonimmediate_operand")
	   (match_operand:AVXMODEF2P 3 "nonimmediate_operand")]
	  UNSPEC_FMADDSUB))]
  "TARGET_FMA || TARGET_FMA4"
  "")

(define_insn "*fma4_fmaddsub_<mode>"
  [(set (match_operand:AVXMODEF2P 0 "register_operand" "=x,x")
	(unspec:AVXMODEF2P
	  [(match_operand:AVXMODEF2P 1 "nonimmediate_operand" "%x,x")
	   (match_operand:AVXMODEF2P 2 "nonimmediate_operand" " x,m")
	   (match_operand:AVXMODEF2P 3 "nonimmediate_operand" "xm,x")]
	  UNSPEC_FMADDSUB))]
  "TARGET_FMA4"
  "vfmaddsub<ssemodesuffix>\t{%3, %2, %1, %0|%0, %1, %2, %3}"
  [(set_attr "type" "ssemuladd")
   (set_attr "mode" "<MODE>")])

(define_insn "*fma4_fmsubadd_<mode>"
  [(set (match_operand:AVXMODEF2P 0 "register_operand" "=x,x")
	(unspec:AVXMODEF2P
	  [(match_operand:AVXMODEF2P 1 "nonimmediate_operand" "%x,x")
	   (match_operand:AVXMODEF2P 2 "nonimmediate_operand" " x,m")
	   (neg:AVXMODEF2P
	     (match_operand:AVXMODEF2P 3 "nonimmediate_operand" "xm,x"))]
	  UNSPEC_FMADDSUB))]
  "TARGET_FMA4"
  "vfmsubadd<ssemodesuffix>\t{%3, %2, %1, %0|%0, %1, %2, %3}"
  [(set_attr "type" "ssemuladd")
   (set_attr "mode" "<MODE>")])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; FMA3 floating point multiply/accumulate instructions.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define_insn "*fma_fmadd_<mode>"
  [(set (match_operand:FMAMODE 0 "register_operand" "=x,x,x")
	(fma:FMAMODE
	  (match_operand:FMAMODE 1 "nonimmediate_operand" "%0, 0,x")
	  (match_operand:FMAMODE 2 "nonimmediate_operand" "xm, x,xm")
	  (match_operand:FMAMODE 3 "nonimmediate_operand" " x,xm,0")))]
  "TARGET_FMA"
  "@
   vfmadd132<ssemodesuffix>\t{%2, %3, %0|%0, %3, %2}
   vfmadd312<ssemodesuffix>\t{%3, %2, %0|%0, %2, %3}
   vfmadd231<ssemodesuffix>\t{%2, %1, %0|%0, %1, %2}"
  [(set_attr "type" "ssemuladd")
   (set_attr "mode" "<MODE>")])

(define_insn "*fma_fmsub_<mode>"
  [(set (match_operand:FMAMODE 0 "register_operand" "=x,x,x")
	(fma:FMAMODE
	  (match_operand:FMAMODE   1 "nonimmediate_operand" "%0, 0,x")
	  (match_operand:FMAMODE   2 "nonimmediate_operand" "xm, x,xm")
	  (neg:FMAMODE
	    (match_operand:FMAMODE 3 "nonimmediate_operand" " x,xm,0"))))]
  "TARGET_FMA"
  "@
   vfmsub132<ssemodesuffix>\t{%2, %3, %0|%0, %3, %2}
   vfmsub312<ssemodesuffix>\t{%3, %2, %0|%0, %2, %3}
   vfmsub231<ssemodesuffix>\t{%2, %1, %0|%0, %1, %2}"
  [(set_attr "type" "ssemuladd")
   (set_attr "mode" "<MODE>")])

(define_insn "*fma_fnmadd_<mode>"
  [(set (match_operand:FMAMODE 0 "register_operand" "=x,x,x")
	(fma:FMAMODE
	  (neg:FMAMODE
	    (match_operand:FMAMODE 1 "nonimmediate_operand" "%0, 0,x"))
	  (match_operand:FMAMODE   2 "nonimmediate_operand" "xm, x,xm")
	  (match_operand:FMAMODE   3 "nonimmediate_operand" " x,xm,0")))]
  "TARGET_FMA"
  "@
   vfnmadd132<ssemodesuffix>\t{%2, %3, %0|%0, %3, %2}
   vfnmadd312<ssemodesuffix>\t{%3, %2, %0|%0, %2, %3}
   vfnmadd231<ssemodesuffix>\t{%2, %1, %0|%0, %1, %2}"
  [(set_attr "type" "ssemuladd")
   (set_attr "mode" "<MODE>")])

(define_insn "*fma_fnmsub_<mode>"
  [(set (match_operand:FMAMODE 0 "register_operand" "=x,x,x")
	(fma:FMAMODE
	  (neg:FMAMODE
	    (match_operand:FMAMODE 1 "nonimmediate_operand" "%0, 0,x"))
	  (match_operand:FMAMODE   2 "nonimmediate_operand" "xm, x,xm")
	  (neg:FMAMODE
	    (match_operand:FMAMODE 3 "nonimmediate_operand" " x,xm,0"))))]
  "TARGET_FMA"
  "@
   vfnmsub132<ssemodesuffix>\t{%2, %3, %0|%0, %3, %2}
   vfnmsub312<ssemodesuffix>\t{%3, %2, %0|%0, %2, %3}
   vfnmsub231<ssemodesuffix>\t{%2, %1, %0|%0, %1, %2}"
  [(set_attr "type" "ssemuladd")
   (set_attr "mode" "<MODE>")])

(define_insn "*fma_fmaddsub_<mode>"
  [(set (match_operand:AVXMODEF2P 0 "register_operand" "=x,x,x")
	(unspec:AVXMODEF2P
	  [(match_operand:AVXMODEF2P 1 "nonimmediate_operand" "%0, 0,x")
	   (match_operand:AVXMODEF2P 2 "nonimmediate_operand" "xm, x,xm")
	   (match_operand:AVXMODEF2P 3 "nonimmediate_operand" " x,xm,0")]
	  UNSPEC_FMADDSUB))]
  "TARGET_FMA"
  "@
   vfmaddsub132<ssemodesuffix>\t{%2, %3, %0|%0, %3, %2}
   vfmaddsub213<ssemodesuffix>\t{%3, %2, %0|%0, %2, %3}
   vfmaddsub231<ssemodesuffix>\t{%2, %1, %0|%0, %1, %2}"
  [(set_attr "type" "ssemuladd")
   (set_attr "mode" "<MODE>")])

(define_insn "*fma_fmsubadd_<mode>"
  [(set (match_operand:AVXMODEF2P 0 "register_operand" "=x,x,x")
	(unspec:AVXMODEF2P
	  [(match_operand:AVXMODEF2P   1 "nonimmediate_operand" "%0, 0,x")
	   (match_operand:AVXMODEF2P   2 "nonimmediate_operand" "xm, x,xm")
	   (neg:AVXMODEF2P
	     (match_operand:AVXMODEF2P 3 "nonimmediate_operand" " x,xm,0"))]
	  UNSPEC_FMADDSUB))]
  "TARGET_FMA"
  "@
   vfmsubadd132<ssemodesuffix>\t{%2, %3, %0|%0, %3, %2}
   vfmsubadd213<ssemodesuffix>\t{%3, %2, %0|%0, %2, %3}
   vfmsubadd231<ssemodesuffix>\t{%2, %1, %0|%0, %1, %2}"
  [(set_attr "type" "ssemuladd")
   (set_attr "mode" "<MODE>")])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Parallel single-precision floating point conversion operations
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define_insn "sse_cvtpi2ps"
  [(set (match_operand:V4SF 0 "register_operand" "=x")
	(vec_merge:V4SF
	  (vec_duplicate:V4SF
	    (float:V2SF (match_operand:V2SI 2 "nonimmediate_operand" "ym")))
	  (match_operand:V4SF 1 "register_operand" "0")
	  (const_int 3)))]
  "TARGET_SSE"
  "cvtpi2ps\t{%2, %0|%0, %2}"
  [(set_attr "type" "ssecvt")
   (set_attr "mode" "V4SF")])

(define_insn "sse_cvtps2pi"
  [(set (match_operand:V2SI 0 "register_operand" "=y")
	(vec_select:V2SI
	  (unspec:V4SI [(match_operand:V4SF 1 "nonimmediate_operand" "xm")]
		       UNSPEC_FIX_NOTRUNC)
	  (parallel [(const_int 0) (const_int 1)])))]
  "TARGET_SSE"
  "cvtps2pi\t{%1, %0|%0, %1}"
  [(set_attr "type" "ssecvt")
   (set_attr "unit" "mmx")
   (set_attr "mode" "DI")])

(define_insn "sse_cvttps2pi"
  [(set (match_operand:V2SI 0 "register_operand" "=y")
	(vec_select:V2SI
	  (fix:V4SI (match_operand:V4SF 1 "nonimmediate_operand" "xm"))
	  (parallel [(const_int 0) (const_int 1)])))]
  "TARGET_SSE"
  "cvttps2pi\t{%1, %0|%0, %1}"
  [(set_attr "type" "ssecvt")
   (set_attr "unit" "mmx")
   (set_attr "prefix_rep" "0")
   (set_attr "mode" "SF")])

(define_insn "*avx_cvtsi2ss"
  [(set (match_operand:V4SF 0 "register_operand" "=x")
	(vec_merge:V4SF
	  (vec_duplicate:V4SF
	    (float:SF (match_operand:SI 2 "nonimmediate_operand" "rm")))
	  (match_operand:V4SF 1 "register_operand" "x")
	  (const_int 1)))]
  "TARGET_AVX"
  "vcvtsi2ss\t{%2, %1, %0|%0, %1, %2}"
  [(set_attr "type" "sseicvt")
   (set_attr "prefix" "vex")
   (set_attr "mode" "SF")])

(define_insn "sse_cvtsi2ss"
  [(set (match_operand:V4SF 0 "register_operand" "=x,x")
	(vec_merge:V4SF
	  (vec_duplicate:V4SF
	    (float:SF (match_operand:SI 2 "nonimmediate_operand" "r,m")))
	  (match_operand:V4SF 1 "register_operand" "0,0")
	  (const_int 1)))]
  "TARGET_SSE"
  "cvtsi2ss\t{%2, %0|%0, %2}"
  [(set_attr "type" "sseicvt")
   (set_attr "athlon_decode" "vector,double")
   (set_attr "amdfam10_decode" "vector,double")
   (set_attr "bdver1_decode" "double,direct")
   (set_attr "mode" "SF")])

(define_insn "*avx_cvtsi2ssq"
  [(set (match_operand:V4SF 0 "register_operand" "=x")
	(vec_merge:V4SF
	  (vec_duplicate:V4SF
	    (float:SF (match_operand:DI 2 "nonimmediate_operand" "rm")))
	  (match_operand:V4SF 1 "register_operand" "x")
	  (const_int 1)))]
  "TARGET_AVX && TARGET_64BIT"
  "vcvtsi2ssq\t{%2, %1, %0|%0, %1, %2}"
  [(set_attr "type" "sseicvt")
   (set_attr "length_vex" "4")
   (set_attr "prefix" "vex")
   (set_attr "mode" "SF")])

(define_insn "sse_cvtsi2ssq"
  [(set (match_operand:V4SF 0 "register_operand" "=x,x")
	(vec_merge:V4SF
	  (vec_duplicate:V4SF
	    (float:SF (match_operand:DI 2 "nonimmediate_operand" "r,rm")))
	  (match_operand:V4SF 1 "register_operand" "0,0")
	  (const_int 1)))]
  "TARGET_SSE && TARGET_64BIT"
  "cvtsi2ssq\t{%2, %0|%0, %2}"
  [(set_attr "type" "sseicvt")
   (set_attr "prefix_rex" "1")
   (set_attr "athlon_decode" "vector,double")
   (set_attr "amdfam10_decode" "vector,double")
   (set_attr "bdver1_decode" "double,direct")
   (set_attr "mode" "SF")])

(define_insn "sse_cvtss2si"
  [(set (match_operand:SI 0 "register_operand" "=r,r")
	(unspec:SI
	  [(vec_select:SF
	     (match_operand:V4SF 1 "nonimmediate_operand" "x,m")
	     (parallel [(const_int 0)]))]
	  UNSPEC_FIX_NOTRUNC))]
  "TARGET_SSE"
  "%vcvtss2si\t{%1, %0|%0, %1}"
  [(set_attr "type" "sseicvt")
   (set_attr "athlon_decode" "double,vector")
   (set_attr "bdver1_decode" "double,double")
   (set_attr "prefix_rep" "1")
   (set_attr "prefix" "maybe_vex")
   (set_attr "mode" "SI")])

(define_insn "sse_cvtss2si_2"
  [(set (match_operand:SI 0 "register_operand" "=r,r")
	(unspec:SI [(match_operand:SF 1 "nonimmediate_operand" "x,m")]
		   UNSPEC_FIX_NOTRUNC))]
  "TARGET_SSE"
  "%vcvtss2si\t{%1, %0|%0, %1}"
  [(set_attr "type" "sseicvt")
   (set_attr "athlon_decode" "double,vector")
   (set_attr "amdfam10_decode" "double,double")
   (set_attr "bdver1_decode" "double,double")
   (set_attr "prefix_rep" "1")
   (set_attr "prefix" "maybe_vex")
   (set_attr "mode" "SI")])

(define_insn "sse_cvtss2siq"
  [(set (match_operand:DI 0 "register_operand" "=r,r")
	(unspec:DI
	  [(vec_select:SF
	     (match_operand:V4SF 1 "nonimmediate_operand" "x,m")
	     (parallel [(const_int 0)]))]
	  UNSPEC_FIX_NOTRUNC))]
  "TARGET_SSE && TARGET_64BIT"
  "%vcvtss2si{q}\t{%1, %0|%0, %1}"
  [(set_attr "type" "sseicvt")
   (set_attr "athlon_decode" "double,vector")
   (set_attr "bdver1_decode" "double,double")
   (set_attr "prefix_rep" "1")
   (set_attr "prefix" "maybe_vex")
   (set_attr "mode" "DI")])

(define_insn "sse_cvtss2siq_2"
  [(set (match_operand:DI 0 "register_operand" "=r,r")
	(unspec:DI [(match_operand:SF 1 "nonimmediate_operand" "x,m")]
		   UNSPEC_FIX_NOTRUNC))]
  "TARGET_SSE && TARGET_64BIT"
  "%vcvtss2si{q}\t{%1, %0|%0, %1}"
  [(set_attr "type" "sseicvt")
   (set_attr "athlon_decode" "double,vector")
   (set_attr "amdfam10_decode" "double,double")
   (set_attr "bdver1_decode" "double,double")
   (set_attr "prefix_rep" "1")
   (set_attr "prefix" "maybe_vex")
   (set_attr "mode" "DI")])

(define_insn "sse_cvttss2si"
  [(set (match_operand:SI 0 "register_operand" "=r,r")
	(fix:SI
	  (vec_select:SF
	    (match_operand:V4SF 1 "nonimmediate_operand" "x,m")
	    (parallel [(const_int 0)]))))]
  "TARGET_SSE"
  "%vcvttss2si\t{%1, %0|%0, %1}"
  [(set_attr "type" "sseicvt")
   (set_attr "athlon_decode" "double,vector")
   (set_attr "amdfam10_decode" "double,double")
   (set_attr "bdver1_decode" "double,double")
   (set_attr "prefix_rep" "1")
   (set_attr "prefix" "maybe_vex")
   (set_attr "mode" "SI")])

(define_insn "sse_cvttss2siq"
  [(set (match_operand:DI 0 "register_operand" "=r,r")
	(fix:DI
	  (vec_select:SF
	    (match_operand:V4SF 1 "nonimmediate_operand" "x,m")
	    (parallel [(const_int 0)]))))]
  "TARGET_SSE && TARGET_64BIT"
  "%vcvttss2si{q}\t{%1, %0|%0, %1}"
  [(set_attr "type" "sseicvt")
   (set_attr "athlon_decode" "double,vector")
   (set_attr "amdfam10_decode" "double,double")
   (set_attr "bdver1_decode" "double,double")
   (set_attr "prefix_rep" "1")
   (set_attr "prefix" "maybe_vex")
   (set_attr "mode" "DI")])

(define_insn "avx_cvtdq2ps<avxmodesuffix>"
  [(set (match_operand:AVXMODEDCVTDQ2PS 0 "register_operand" "=x")
	(float:AVXMODEDCVTDQ2PS
	  (match_operand:<avxcvtvecmode> 1 "nonimmediate_operand" "xm")))]
  "TARGET_AVX"
  "vcvtdq2ps\t{%1, %0|%0, %1}"
  [(set_attr "type" "ssecvt")
   (set_attr "prefix" "vex")
   (set_attr "mode" "<avxvecmode>")])

(define_insn "sse2_cvtdq2ps"
  [(set (match_operand:V4SF 0 "register_operand" "=x")
	(float:V4SF (match_operand:V4SI 1 "nonimmediate_operand" "xm")))]
  "TARGET_SSE2"
  "cvtdq2ps\t{%1, %0|%0, %1}"
  [(set_attr "type" "ssecvt")
   (set_attr "mode" "V4SF")])

(define_expand "sse2_cvtudq2ps"
  [(set (match_dup 5)
	(float:V4SF (match_operand:V4SI 1 "nonimmediate_operand" "")))
   (set (match_dup 6)
	(lt:V4SF (match_dup 5) (match_dup 3)))
   (set (match_dup 7)
	(and:V4SF (match_dup 6) (match_dup 4)))
   (set (match_operand:V4SF 0 "register_operand" "")
	(plus:V4SF (match_dup 5) (match_dup 7)))]
  "TARGET_SSE2"
{
  REAL_VALUE_TYPE TWO32r;
  rtx x;
  int i;

  real_ldexp (&TWO32r, &dconst1, 32);
  x = const_double_from_real_value (TWO32r, SFmode);

  operands[3] = force_reg (V4SFmode, CONST0_RTX (V4SFmode));
  operands[4] = force_reg (V4SFmode,
			   ix86_build_const_vector (V4SFmode, 1, x));

  for (i = 5; i < 8; i++)
    operands[i] = gen_reg_rtx (V4SFmode);
})

(define_insn "avx_cvtps2dq<avxmodesuffix>"
  [(set (match_operand:AVXMODEDCVTPS2DQ 0 "register_operand" "=x")
	(unspec:AVXMODEDCVTPS2DQ
	  [(match_operand:<avxcvtvecmode> 1 "nonimmediate_operand" "xm")]
	  UNSPEC_FIX_NOTRUNC))]
  "TARGET_AVX"
  "vcvtps2dq\t{%1, %0|%0, %1}"
  [(set_attr "type" "ssecvt")
   (set_attr "prefix" "vex")
   (set_attr "mode" "<avxvecmode>")])

(define_insn "sse2_cvtps2dq"
  [(set (match_operand:V4SI 0 "register_operand" "=x")
	(unspec:V4SI [(match_operand:V4SF 1 "nonimmediate_operand" "xm")]
		     UNSPEC_FIX_NOTRUNC))]
  "TARGET_SSE2"
  "cvtps2dq\t{%1, %0|%0, %1}"
  [(set_attr "type" "ssecvt")
   (set_attr "prefix_data16" "1")
   (set_attr "mode" "TI")])

(define_insn "avx_cvttps2dq<avxmodesuffix>"
  [(set (match_operand:AVXMODEDCVTPS2DQ 0 "register_operand" "=x")
	(fix:AVXMODEDCVTPS2DQ
	  (match_operand:<avxcvtvecmode> 1 "nonimmediate_operand" "xm")))]
  "TARGET_AVX"
  "vcvttps2dq\t{%1, %0|%0, %1}"
  [(set_attr "type" "ssecvt")
   (set_attr "prefix" "vex")
   (set_attr "mode" "<avxvecmode>")])

(define_insn "sse2_cvttps2dq"
  [(set (match_operand:V4SI 0 "register_operand" "=x")
	(fix:V4SI (match_operand:V4SF 1 "nonimmediate_operand" "xm")))]
  "TARGET_SSE2"
  "cvttps2dq\t{%1, %0|%0, %1}"
  [(set_attr "type" "ssecvt")
   (set_attr "prefix_rep" "1")
   (set_attr "prefix_data16" "0")
   (set_attr "mode" "TI")])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Parallel double-precision floating point conversion operations
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define_insn "sse2_cvtpi2pd"
  [(set (match_operand:V2DF 0 "register_operand" "=x,x")
	(float:V2DF (match_operand:V2SI 1 "nonimmediate_operand" "y,m")))]
  "TARGET_SSE2"
  "cvtpi2pd\t{%1, %0|%0, %1}"
  [(set_attr "type" "ssecvt")
   (set_attr "unit" "mmx,*")
   (set_attr "prefix_data16" "1,*")
   (set_attr "mode" "V2DF")])

(define_insn "sse2_cvtpd2pi"
  [(set (match_operand:V2SI 0 "register_operand" "=y")
	(unspec:V2SI [(match_operand:V2DF 1 "nonimmediate_operand" "xm")]
		     UNSPEC_FIX_NOTRUNC))]
  "TARGET_SSE2"
  "cvtpd2pi\t{%1, %0|%0, %1}"
  [(set_attr "type" "ssecvt")
   (set_attr "unit" "mmx")
   (set_attr "prefix_data16" "1")
   (set_attr "mode" "DI")
   (set_attr "bdver1_decode" "double")])

(define_insn "sse2_cvttpd2pi"
  [(set (match_operand:V2SI 0 "register_operand" "=y")
	(fix:V2SI (match_operand:V2DF 1 "nonimmediate_operand" "xm")))]
  "TARGET_SSE2"
  "cvttpd2pi\t{%1, %0|%0, %1}"
  [(set_attr "type" "ssecvt")
   (set_attr "unit" "mmx")
   (set_attr "prefix_data16" "1")
   (set_attr "mode" "TI")
   (set_attr "bdver1_decode" "double")])

(define_insn "*avx_cvtsi2sd"
  [(set (match_operand:V2DF 0 "register_operand" "=x")
	(vec_merge:V2DF
	  (vec_duplicate:V2DF
	    (float:DF (match_operand:SI 2 "nonimmediate_operand" "rm")))
	  (match_operand:V2DF 1 "register_operand" "x")
	  (const_int 1)))]
  "TARGET_AVX"
  "vcvtsi2sd\t{%2, %1, %0|%0, %1, %2}"
  [(set_attr "type" "sseicvt")
   (set_attr "prefix" "vex")
   (set_attr "mode" "DF")])

(define_insn "sse2_cvtsi2sd"
  [(set (match_operand:V2DF 0 "register_operand" "=x,x")
	(vec_merge:V2DF
	  (vec_duplicate:V2DF
	    (float:DF (match_operand:SI 2 "nonimmediate_operand" "r,m")))
	  (match_operand:V2DF 1 "register_operand" "0,0")
	  (const_int 1)))]
  "TARGET_SSE2"
  "cvtsi2sd\t{%2, %0|%0, %2}"
  [(set_attr "type" "sseicvt")
   (set_attr "mode" "DF")
   (set_attr "athlon_decode" "double,direct")
   (set_attr "amdfam10_decode" "vector,double")
   (set_attr "bdver1_decode" "double,direct")])

(define_insn "*avx_cvtsi2sdq"
  [(set (match_operand:V2DF 0 "register_operand" "=x")
	(vec_merge:V2DF
	  (vec_duplicate:V2DF
	    (float:DF (match_operand:DI 2 "nonimmediate_operand" "rm")))
	  (match_operand:V2DF 1 "register_operand" "x")
	  (const_int 1)))]
  "TARGET_AVX && TARGET_64BIT"
  "vcvtsi2sdq\t{%2, %1, %0|%0, %1, %2}"
  [(set_attr "type" "sseicvt")
   (set_attr "length_vex" "4")
   (set_attr "prefix" "vex")
   (set_attr "mode" "DF")])

(define_insn "sse2_cvtsi2sdq"
  [(set (match_operand:V2DF 0 "register_operand" "=x,x")
	(vec_merge:V2DF
	  (vec_duplicate:V2DF
	    (float:DF (match_operand:DI 2 "nonimmediate_operand" "r,m")))
	  (match_operand:V2DF 1 "register_operand" "0,0")
	  (const_int 1)))]
  "TARGET_SSE2 && TARGET_64BIT"
  "cvtsi2sdq\t{%2, %0|%0, %2}"
  [(set_attr "type" "sseicvt")
   (set_attr "prefix_rex" "1")
   (set_attr "mode" "DF")
   (set_attr "athlon_decode" "double,direct")
   (set_attr "amdfam10_decode" "vector,double")
   (set_attr "bdver1_decode" "double,direct")])

(define_insn "sse2_cvtsd2si"
  [(set (match_operand:SI 0 "register_operand" "=r,r")
	(unspec:SI
	  [(vec_select:DF
	     (match_operand:V2DF 1 "nonimmediate_operand" "x,m")
	     (parallel [(const_int 0)]))]
	  UNSPEC_FIX_NOTRUNC))]
  "TARGET_SSE2"
  "%vcvtsd2si\t{%1, %0|%0, %1}"
  [(set_attr "type" "sseicvt")
   (set_attr "athlon_decode" "double,vector")
   (set_attr "bdver1_decode" "double,double")
   (set_attr "prefix_rep" "1")
   (set_attr "prefix" "maybe_vex")
   (set_attr "mode" "SI")])

(define_insn "sse2_cvtsd2si_2"
  [(set (match_operand:SI 0 "register_operand" "=r,r")
	(unspec:SI [(match_operand:DF 1 "nonimmediate_operand" "x,m")]
		   UNSPEC_FIX_NOTRUNC))]
  "TARGET_SSE2"
  "%vcvtsd2si\t{%1, %0|%0, %1}"
  [(set_attr "type" "sseicvt")
   (set_attr "athlon_decode" "double,vector")
   (set_attr "amdfam10_decode" "double,double")
   (set_attr "bdver1_decode" "double,double")
   (set_attr "prefix_rep" "1")
   (set_attr "prefix" "maybe_vex")
   (set_attr "mode" "SI")])

(define_insn "sse2_cvtsd2siq"
  [(set (match_operand:DI 0 "register_operand" "=r,r")
	(unspec:DI
	  [(vec_select:DF
	     (match_operand:V2DF 1 "nonimmediate_operand" "x,m")
	     (parallel [(const_int 0)]))]
	  UNSPEC_FIX_NOTRUNC))]
  "TARGET_SSE2 && TARGET_64BIT"
  "%vcvtsd2siq\t{%1, %0|%0, %1}"
  [(set_attr "type" "sseicvt")
   (set_attr "athlon_decode" "double,vector")
   (set_attr "bdver1_decode" "double,double")
   (set_attr "prefix_rep" "1")
   (set_attr "prefix" "maybe_vex")
   (set_attr "mode" "DI")])

(define_insn "sse2_cvtsd2siq_2"
  [(set (match_operand:DI 0 "register_operand" "=r,r")
	(unspec:DI [(match_operand:DF 1 "nonimmediate_operand" "x,m")]
		   UNSPEC_FIX_NOTRUNC))]
  "TARGET_SSE2 && TARGET_64BIT"
  "%vcvtsd2siq\t{%1, %0|%0, %1}"
  [(set_attr "type" "sseicvt")
   (set_attr "athlon_decode" "double,vector")
   (set_attr "amdfam10_decode" "double,double")
   (set_attr "bdver1_decode" "double,double")
   (set_attr "prefix_rep" "1")
   (set_attr "prefix" "maybe_vex")
   (set_attr "mode" "DI")])

(define_insn "sse2_cvttsd2si"
  [(set (match_operand:SI 0 "register_operand" "=r,r")
	(fix:SI
	  (vec_select:DF
	    (match_operand:V2DF 1 "nonimmediate_operand" "x,m")
	    (parallel [(const_int 0)]))))]
  "TARGET_SSE2"
  "%vcvttsd2si\t{%1, %0|%0, %1}"
  [(set_attr "type" "sseicvt")
   (set_attr "prefix_rep" "1")
   (set_attr "prefix" "maybe_vex")
   (set_attr "mode" "SI")
   (set_attr "athlon_decode" "double,vector")
   (set_attr "amdfam10_decode" "double,double")
   (set_attr "bdver1_decode" "double,double")])

(define_insn "sse2_cvttsd2siq"
  [(set (match_operand:DI 0 "register_operand" "=r,r")
	(fix:DI
	  (vec_select:DF
	    (match_operand:V2DF 1 "nonimmediate_operand" "x,m")
	    (parallel [(const_int 0)]))))]
  "TARGET_SSE2 && TARGET_64BIT"
  "%vcvttsd2siq\t{%1, %0|%0, %1}"
  [(set_attr "type" "sseicvt")
   (set_attr "prefix_rep" "1")
   (set_attr "prefix" "maybe_vex")
   (set_attr "mode" "DI")
   (set_attr "athlon_decode" "double,vector")
   (set_attr "amdfam10_decode" "double,double")
   (set_attr "bdver1_decode" "double,double")])

(define_insn "avx_cvtdq2pd256"
  [(set (match_operand:V4DF 0 "register_operand" "=x")
	(float:V4DF (match_operand:V4SI 1 "nonimmediate_operand" "xm")))]
  "TARGET_AVX"
  "vcvtdq2pd\t{%1, %0|%0, %1}"
  [(set_attr "type" "ssecvt")
   (set_attr "prefix" "vex")
   (set_attr "mode" "V4DF")])

(define_insn "*avx_cvtdq2pd256_2"
  [(set (match_operand:V4DF 0 "register_operand" "=x")
	(float:V4DF
	  (vec_select:V4SI
	    (match_operand:V8SI 1 "nonimmediate_operand" "xm")
	    (parallel [(const_int 0) (const_int 1) (const_int 2) (const_int 3)]))))]
  "TARGET_AVX"
  "vcvtdq2pd\t{%x1, %0|%0, %x1}"
  [(set_attr "type" "ssecvt")
   (set_attr "prefix" "vex")
   (set_attr "mode" "V4DF")])

(define_insn "sse2_cvtdq2pd"
  [(set (match_operand:V2DF 0 "register_operand" "=x")
	(float:V2DF
	  (vec_select:V2SI
	    (match_operand:V4SI 1 "nonimmediate_operand" "xm")
	    (parallel [(const_int 0) (const_int 1)]))))]
  "TARGET_SSE2"
  "%vcvtdq2pd\t{%1, %0|%0, %1}"
  [(set_attr "type" "ssecvt")
   (set_attr "prefix" "maybe_vex")
   (set_attr "mode" "V2DF")])

(define_insn "avx_cvtpd2dq256"
  [(set (match_operand:V4SI 0 "register_operand" "=x")
	(unspec:V4SI [(match_operand:V4DF 1 "nonimmediate_operand" "xm")]
		     UNSPEC_FIX_NOTRUNC))]
  "TARGET_AVX"
  "vcvtpd2dq{y}\t{%1, %0|%0, %1}"
  [(set_attr "type" "ssecvt")
   (set_attr "prefix" "vex")
   (set_attr "mode" "OI")])

(define_expand "sse2_cvtpd2dq"
  [(set (match_operand:V4SI 0 "register_operand" "")
	(vec_concat:V4SI
	  (unspec:V2SI [(match_operand:V2DF 1 "nonimmediate_operand" "")]
		       UNSPEC_FIX_NOTRUNC)
	  (match_dup 2)))]
  "TARGET_SSE2"
  "operands[2] = CONST0_RTX (V2SImode);")

(define_insn "*sse2_cvtpd2dq"
  [(set (match_operand:V4SI 0 "register_operand" "=x")
	(vec_concat:V4SI
	  (unspec:V2SI [(match_operand:V2DF 1 "nonimmediate_operand" "xm")]
		       UNSPEC_FIX_NOTRUNC)
	  (match_operand:V2SI 2 "const0_operand" "")))]
  "TARGET_SSE2"
  "* return TARGET_AVX ? \"vcvtpd2dq{x}\t{%1, %0|%0, %1}\"
		       : \"cvtpd2dq\t{%1, %0|%0, %1}\";"
  [(set_attr "type" "ssecvt")
   (set_attr "prefix_rep" "1")
   (set_attr "prefix_data16" "0")
   (set_attr "prefix" "maybe_vex")
   (set_attr "mode" "TI")
   (set_attr "amdfam10_decode" "double")
   (set_attr "athlon_decode" "vector")
   (set_attr "bdver1_decode" "double")])

(define_insn "avx_cvttpd2dq256"
  [(set (match_operand:V4SI 0 "register_operand" "=x")
	(fix:V4SI (match_operand:V4DF 1 "nonimmediate_operand" "xm")))]
  "TARGET_AVX"
  "vcvttpd2dq{y}\t{%1, %0|%0, %1}"
  [(set_attr "type" "ssecvt")
   (set_attr "prefix" "vex")
   (set_attr "mode" "OI")])

(define_expand "sse2_cvttpd2dq"
  [(set (match_operand:V4SI 0 "register_operand" "")
	(vec_concat:V4SI
	  (fix:V2SI (match_operand:V2DF 1 "nonimmediate_operand" ""))
	  (match_dup 2)))]
  "TARGET_SSE2"
  "operands[2] = CONST0_RTX (V2SImode);")

(define_insn "*sse2_cvttpd2dq"
  [(set (match_operand:V4SI 0 "register_operand" "=x")
	(vec_concat:V4SI
	  (fix:V2SI (match_operand:V2DF 1 "nonimmediate_operand" "xm"))
	  (match_operand:V2SI 2 "const0_operand" "")))]
  "TARGET_SSE2"
  "* return TARGET_AVX ? \"vcvttpd2dq{x}\t{%1, %0|%0, %1}\"
		       : \"cvttpd2dq\t{%1, %0|%0, %1}\";"
  [(set_attr "type" "ssecvt")
   (set_attr "prefix" "maybe_vex")
   (set_attr "mode" "TI")
   (set_attr "amdfam10_decode" "double")
   (set_attr "athlon_decode" "vector")
   (set_attr "bdver1_decode" "double")])

(define_insn "*avx_cvtsd2ss"
  [(set (match_operand:V4SF 0 "register_operand" "=x")
	(vec_merge:V4SF
	  (vec_duplicate:V4SF
	    (float_truncate:V2SF
	      (match_operand:V2DF 2 "nonimmediate_operand" "xm")))
	  (match_operand:V4SF 1 "register_operand" "x")
	  (const_int 1)))]
  "TARGET_AVX"
  "vcvtsd2ss\t{%2, %1, %0|%0, %1, %2}"
  [(set_attr "type" "ssecvt")
   (set_attr "prefix" "vex")
   (set_attr "mode" "SF")])

(define_insn "sse2_cvtsd2ss"
  [(set (match_operand:V4SF 0 "register_operand" "=x,x")
	(vec_merge:V4SF
	  (vec_duplicate:V4SF
	    (float_truncate:V2SF
	      (match_operand:V2DF 2 "nonimmediate_operand" "x,m")))
	  (match_operand:V4SF 1 "register_operand" "0,0")
	  (const_int 1)))]
  "TARGET_SSE2"
  "cvtsd2ss\t{%2, %0|%0, %2}"
  [(set_attr "type" "ssecvt")
   (set_attr "athlon_decode" "vector,double")
   (set_attr "amdfam10_decode" "vector,double")
   (set_attr "bdver1_decode" "direct,direct")
   (set_attr "mode" "SF")])

(define_insn "*avx_cvtss2sd"
  [(set (match_operand:V2DF 0 "register_operand" "=x")
	(vec_merge:V2DF
	  (float_extend:V2DF
	    (vec_select:V2SF
	      (match_operand:V4SF 2 "nonimmediate_operand" "xm")
	      (parallel [(const_int 0) (const_int 1)])))
	  (match_operand:V2DF 1 "register_operand" "x")
	  (const_int 1)))]
  "TARGET_AVX"
  "vcvtss2sd\t{%2, %1, %0|%0, %1, %2}"
  [(set_attr "type" "ssecvt")
   (set_attr "prefix" "vex")
   (set_attr "mode" "DF")])

(define_insn "sse2_cvtss2sd"
  [(set (match_operand:V2DF 0 "register_operand" "=x,x")
	(vec_merge:V2DF
	  (float_extend:V2DF
	    (vec_select:V2SF
	      (match_operand:V4SF 2 "nonimmediate_operand" "x,m")
	      (parallel [(const_int 0) (const_int 1)])))
	  (match_operand:V2DF 1 "register_operand" "0,0")
	  (const_int 1)))]
  "TARGET_SSE2"
  "cvtss2sd\t{%2, %0|%0, %2}"
  [(set_attr "type" "ssecvt")
   (set_attr "amdfam10_decode" "vector,double")
   (set_attr "athlon_decode" "direct,direct")
   (set_attr "bdver1_decode" "direct,direct")
   (set_attr "mode" "DF")])

(define_insn "avx_cvtpd2ps256"
  [(set (match_operand:V4SF 0 "register_operand" "=x")
	(float_truncate:V4SF
	  (match_operand:V4DF 1 "nonimmediate_operand" "xm")))]
  "TARGET_AVX"
  "vcvtpd2ps{y}\t{%1, %0|%0, %1}"
  [(set_attr "type" "ssecvt")
   (set_attr "prefix" "vex")
   (set_attr "mode" "V4SF")])

(define_expand "sse2_cvtpd2ps"
  [(set (match_operand:V4SF 0 "register_operand" "")
	(vec_concat:V4SF
	  (float_truncate:V2SF
	    (match_operand:V2DF 1 "nonimmediate_operand" ""))
	  (match_dup 2)))]
  "TARGET_SSE2"
  "operands[2] = CONST0_RTX (V2SFmode);")

(define_insn "*sse2_cvtpd2ps"
  [(set (match_operand:V4SF 0 "register_operand" "=x")
	(vec_concat:V4SF
	  (float_truncate:V2SF
	    (match_operand:V2DF 1 "nonimmediate_operand" "xm"))
	  (match_operand:V2SF 2 "const0_operand" "")))]
  "TARGET_SSE2"
  "* return TARGET_AVX ? \"vcvtpd2ps{x}\t{%1, %0|%0, %1}\"
		       : \"cvtpd2ps\t{%1, %0|%0, %1}\";"
  [(set_attr "type" "ssecvt")
   (set_attr "prefix_data16" "1")
   (set_attr "prefix" "maybe_vex")
   (set_attr "mode" "V4SF")
   (set_attr "amdfam10_decode" "double")
   (set_attr "athlon_decode" "vector")
   (set_attr "bdver1_decode" "double")])

(define_insn "avx_cvtps2pd256"
  [(set (match_operand:V4DF 0 "register_operand" "=x")
	(float_extend:V4DF
	  (match_operand:V4SF 1 "nonimmediate_operand" "xm")))]
  "TARGET_AVX"
  "vcvtps2pd\t{%1, %0|%0, %1}"
  [(set_attr "type" "ssecvt")
   (set_attr "prefix" "vex")
   (set_attr "mode" "V4DF")])

(define_insn "*avx_cvtps2pd256_2"
  [(set (match_operand:V4DF 0 "register_operand" "=x")
	(float_extend:V4DF
	  (vec_select:V4SF
	    (match_operand:V8SF 1 "nonimmediate_operand" "xm")
	    (parallel [(const_int 0) (const_int 1) (const_int 2) (const_int 3)]))))]
  "TARGET_AVX"
  "vcvtps2pd\t{%x1, %0|%0, %x1}"
  [(set_attr "type" "ssecvt")
   (set_attr "prefix" "vex")
   (set_attr "mode" "V4DF")])

(define_insn "sse2_cvtps2pd"
  [(set (match_operand:V2DF 0 "register_operand" "=x")
	(float_extend:V2DF
	  (vec_select:V2SF
	    (match_operand:V4SF 1 "nonimmediate_operand" "xm")
	    (parallel [(const_int 0) (const_int 1)]))))]
  "TARGET_SSE2"
  "%vcvtps2pd\t{%1, %0|%0, %1}"
  [(set_attr "type" "ssecvt")
   (set_attr "prefix" "maybe_vex")
   (set_attr "mode" "V2DF")
   (set_attr "prefix_data16" "0")
   (set_attr "amdfam10_decode" "direct")
   (set_attr "athlon_decode" "double")
   (set_attr "bdver1_decode" "double")])

(define_expand "vec_unpacks_hi_v4sf"
  [(set (match_dup 2)
   (vec_select:V4SF
     (vec_concat:V8SF
       (match_dup 2)
       (match_operand:V4SF 1 "nonimmediate_operand" ""))
     (parallel [(const_int 6)
		(const_int 7)
		(const_int 2)
		(const_int 3)])))
  (set (match_operand:V2DF 0 "register_operand" "")
   (float_extend:V2DF
     (vec_select:V2SF
       (match_dup 2)
       (parallel [(const_int 0) (const_int 1)]))))]
  "TARGET_SSE2"
  "operands[2] = gen_reg_rtx (V4SFmode);")

(define_expand "vec_unpacks_hi_v8sf"
  [(set (match_dup 2)
	(vec_select:V4SF
	  (match_operand:V8SF 1 "nonimmediate_operand" "")
	  (parallel [(const_int 4)
		     (const_int 5)
		     (const_int 6)
		     (const_int 7)])))
   (set (match_operand:V4DF 0 "register_operand" "")
	(float_extend:V4DF
	  (match_dup 2)))]
  "TARGET_AVX"
{
  operands[2] = gen_reg_rtx (V4SFmode);
})

(define_expand "vec_unpacks_lo_v4sf"
  [(set (match_operand:V2DF 0 "register_operand" "")
	(float_extend:V2DF
	  (vec_select:V2SF
	    (match_operand:V4SF 1 "nonimmediate_operand" "")
	    (parallel [(const_int 0) (const_int 1)]))))]
  "TARGET_SSE2")

(define_expand "vec_unpacks_lo_v8sf"
  [(set (match_operand:V4DF 0 "register_operand" "")
	(float_extend:V4DF
	  (vec_select:V4SF
	    (match_operand:V8SF 1 "nonimmediate_operand" "")
	    (parallel [(const_int 0) (const_int 1) (const_int 2) (const_int 3)]))))]
  "TARGET_AVX")

(define_expand "vec_unpacks_float_hi_v8hi"
  [(match_operand:V4SF 0 "register_operand" "")
   (match_operand:V8HI 1 "register_operand" "")]
  "TARGET_SSE2"
{
  rtx tmp = gen_reg_rtx (V4SImode);

  emit_insn (gen_vec_unpacks_hi_v8hi (tmp, operands[1]));
  emit_insn (gen_sse2_cvtdq2ps (operands[0], tmp));
  DONE;
})

(define_expand "vec_unpacks_float_lo_v8hi"
  [(match_operand:V4SF 0 "register_operand" "")
   (match_operand:V8HI 1 "register_operand" "")]
  "TARGET_SSE2"
{
  rtx tmp = gen_reg_rtx (V4SImode);

  emit_insn (gen_vec_unpacks_lo_v8hi (tmp, operands[1]));
  emit_insn (gen_sse2_cvtdq2ps (operands[0], tmp));
  DONE;
})

(define_expand "vec_unpacku_float_hi_v8hi"
  [(match_operand:V4SF 0 "register_operand" "")
   (match_operand:V8HI 1 "register_operand" "")]
  "TARGET_SSE2"
{
  rtx tmp = gen_reg_rtx (V4SImode);

  emit_insn (gen_vec_unpacku_hi_v8hi (tmp, operands[1]));
  emit_insn (gen_sse2_cvtdq2ps (operands[0], tmp));
  DONE;
})

(define_expand "vec_unpacku_float_lo_v8hi"
  [(match_operand:V4SF 0 "register_operand" "")
   (match_operand:V8HI 1 "register_operand" "")]
  "TARGET_SSE2"
{
  rtx tmp = gen_reg_rtx (V4SImode);

  emit_insn (gen_vec_unpacku_lo_v8hi (tmp, operands[1]));
  emit_insn (gen_sse2_cvtdq2ps (operands[0], tmp));
  DONE;
})

(define_expand "vec_unpacks_float_hi_v4si"
  [(set (match_dup 2)
	(vec_select:V4SI
	  (match_operand:V4SI 1 "nonimmediate_operand" "")
	  (parallel [(const_int 2)
		     (const_int 3)
		     (const_int 2)
		     (const_int 3)])))
   (set (match_operand:V2DF 0 "register_operand" "")
        (float:V2DF
	  (vec_select:V2SI
	  (match_dup 2)
	    (parallel [(const_int 0) (const_int 1)]))))]
  "TARGET_SSE2"
  "operands[2] = gen_reg_rtx (V4SImode);")

(define_expand "vec_unpacks_float_lo_v4si"
  [(set (match_operand:V2DF 0 "register_operand" "")
	(float:V2DF
	  (vec_select:V2SI
	    (match_operand:V4SI 1 "nonimmediate_operand" "")
	    (parallel [(const_int 0) (const_int 1)]))))]
  "TARGET_SSE2")

(define_expand "vec_unpacks_float_hi_v8si"
  [(set (match_dup 2)
	(vec_select:V4SI
	  (match_operand:V8SI 1 "nonimmediate_operand" "")
	  (parallel [(const_int 4)
		     (const_int 5)
		     (const_int 6)
		     (const_int 7)])))
   (set (match_operand:V4DF 0 "register_operand" "")
        (float:V4DF
	  (match_dup 2)))]
  "TARGET_AVX"
  "operands[2] = gen_reg_rtx (V4SImode);")

(define_expand "vec_unpacks_float_lo_v8si"
  [(set (match_operand:V4DF 0 "register_operand" "")
	(float:V4DF
	  (vec_select:V4SI
	    (match_operand:V8SI 1 "nonimmediate_operand" "")
	    (parallel [(const_int 0) (const_int 1) (const_int 2) (const_int 3)]))))]
  "TARGET_AVX")

(define_expand "vec_unpacku_float_hi_v4si"
  [(set (match_dup 5)
	(vec_select:V4SI
	  (match_operand:V4SI 1 "nonimmediate_operand" "")
	  (parallel [(const_int 2)
		     (const_int 3)
		     (const_int 2)
		     (const_int 3)])))
   (set (match_dup 6)
        (float:V2DF
	  (vec_select:V2SI
	  (match_dup 5)
	    (parallel [(const_int 0) (const_int 1)]))))
   (set (match_dup 7)
	(lt:V2DF (match_dup 6) (match_dup 3)))
   (set (match_dup 8)
	(and:V2DF (match_dup 7) (match_dup 4)))
   (set (match_operand:V2DF 0 "register_operand" "")
	(plus:V2DF (match_dup 6) (match_dup 8)))]
  "TARGET_SSE2"
{
  REAL_VALUE_TYPE TWO32r;
  rtx x;
  int i;

  real_ldexp (&TWO32r, &dconst1, 32);
  x = const_double_from_real_value (TWO32r, DFmode);

  operands[3] = force_reg (V2DFmode, CONST0_RTX (V2DFmode));
  operands[4] = force_reg (V2DFmode,
			   ix86_build_const_vector (V2DFmode, 1, x));

  operands[5] = gen_reg_rtx (V4SImode);
 
  for (i = 6; i < 9; i++)
    operands[i] = gen_reg_rtx (V2DFmode);
})

(define_expand "vec_unpacku_float_lo_v4si"
  [(set (match_dup 5)
	(float:V2DF
	  (vec_select:V2SI
	    (match_operand:V4SI 1 "nonimmediate_operand" "")
	    (parallel [(const_int 0) (const_int 1)]))))
   (set (match_dup 6)
	(lt:V2DF (match_dup 5) (match_dup 3)))
   (set (match_dup 7)
	(and:V2DF (match_dup 6) (match_dup 4)))
   (set (match_operand:V2DF 0 "register_operand" "")
	(plus:V2DF (match_dup 5) (match_dup 7)))]
  "TARGET_SSE2"
{
  REAL_VALUE_TYPE TWO32r;
  rtx x;
  int i;

  real_ldexp (&TWO32r, &dconst1, 32);
  x = const_double_from_real_value (TWO32r, DFmode);

  operands[3] = force_reg (V2DFmode, CONST0_RTX (V2DFmode));
  operands[4] = force_reg (V2DFmode,
			   ix86_build_const_vector (V2DFmode, 1, x));

  for (i = 5; i < 8; i++)
    operands[i] = gen_reg_rtx (V2DFmode);
})

(define_expand "vec_pack_trunc_v4df"
  [(set (match_dup 3)
	(float_truncate:V4SF
	  (match_operand:V4DF 1 "nonimmediate_operand" "")))
   (set (match_dup 4)
	(float_truncate:V4SF
	  (match_operand:V4DF 2 "nonimmediate_operand" "")))
   (set (match_operand:V8SF 0 "register_operand" "")
	(vec_concat:V8SF
	  (match_dup 3)
	  (match_dup 4)))]
  "TARGET_AVX"
{
  operands[3] = gen_reg_rtx (V4SFmode);
  operands[4] = gen_reg_rtx (V4SFmode);
})

(define_expand "vec_pack_trunc_v2df"
  [(match_operand:V4SF 0 "register_operand" "")
   (match_operand:V2DF 1 "nonimmediate_operand" "")
   (match_operand:V2DF 2 "nonimmediate_operand" "")]
  "TARGET_SSE2"
{
  rtx r1, r2;

  r1 = gen_reg_rtx (V4SFmode);
  r2 = gen_reg_rtx (V4SFmode);

  emit_insn (gen_sse2_cvtpd2ps (r1, operands[1]));
  emit_insn (gen_sse2_cvtpd2ps (r2, operands[2]));
  emit_insn (gen_sse_movlhps (operands[0], r1, r2));
  DONE;
})

(define_expand "vec_pack_sfix_trunc_v2df"
  [(match_operand:V4SI 0 "register_operand" "")
   (match_operand:V2DF 1 "nonimmediate_operand" "")
   (match_operand:V2DF 2 "nonimmediate_operand" "")]
  "TARGET_SSE2"
{
  rtx r1, r2;

  r1 = gen_reg_rtx (V4SImode);
  r2 = gen_reg_rtx (V4SImode);

  emit_insn (gen_sse2_cvttpd2dq (r1, operands[1]));
  emit_insn (gen_sse2_cvttpd2dq (r2, operands[2]));
  emit_insn (gen_vec_interleave_lowv2di (gen_lowpart (V2DImode, operands[0]),
  	    				 gen_lowpart (V2DImode, r1),
 					 gen_lowpart (V2DImode, r2)));
  DONE;
})

(define_expand "vec_pack_sfix_v2df"
  [(match_operand:V4SI 0 "register_operand" "")
   (match_operand:V2DF 1 "nonimmediate_operand" "")
   (match_operand:V2DF 2 "nonimmediate_operand" "")]
  "TARGET_SSE2"
{
  rtx r1, r2;

  r1 = gen_reg_rtx (V4SImode);
  r2 = gen_reg_rtx (V4SImode);

  emit_insn (gen_sse2_cvtpd2dq (r1, operands[1]));
  emit_insn (gen_sse2_cvtpd2dq (r2, operands[2]));
  emit_insn (gen_vec_interleave_lowv2di (gen_lowpart (V2DImode, operands[0]),
  	    				 gen_lowpart (V2DImode, r1),
 					 gen_lowpart (V2DImode, r2)));
  DONE;
})

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Parallel single-precision floating point element swizzling
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define_expand "sse_movhlps_exp"
  [(set (match_operand:V4SF 0 "nonimmediate_operand" "")
	(vec_select:V4SF
	  (vec_concat:V8SF
	    (match_operand:V4SF 1 "nonimmediate_operand" "")
	    (match_operand:V4SF 2 "nonimmediate_operand" ""))
	  (parallel [(const_int 6)
		     (const_int 7)
		     (const_int 2)
		     (const_int 3)])))]
  "TARGET_SSE"
{
  rtx dst = ix86_fixup_binary_operands (UNKNOWN, V4SFmode, operands);
  
  emit_insn (gen_sse_movhlps (dst, operands[1], operands[2]));

  /* Fix up the destination if needed.  */
  if (dst != operands[0])
    emit_move_insn (operands[0], dst);

  DONE;
})

(define_insn "*avx_movhlps"
  [(set (match_operand:V4SF 0 "nonimmediate_operand"     "=x,x,m")
	(vec_select:V4SF
	  (vec_concat:V8SF
	    (match_operand:V4SF 1 "nonimmediate_operand" " x,x,0")
	    (match_operand:V4SF 2 "nonimmediate_operand" " x,o,x"))
	  (parallel [(const_int 6)
		     (const_int 7)
		     (const_int 2)
		     (const_int 3)])))]
  "TARGET_AVX && !(MEM_P (operands[1]) && MEM_P (operands[2]))"
  "@
   vmovhlps\t{%2, %1, %0|%0, %1, %2}
   vmovlps\t{%H2, %1, %0|%0, %1, %H2}
   vmovhps\t{%2, %0|%0, %2}"
  [(set_attr "type" "ssemov")
   (set_attr "prefix" "vex")
   (set_attr "mode" "V4SF,V2SF,V2SF")])

(define_insn "sse_movhlps"
  [(set (match_operand:V4SF 0 "nonimmediate_operand"     "=x,x,m")
	(vec_select:V4SF
	  (vec_concat:V8SF
	    (match_operand:V4SF 1 "nonimmediate_operand" " 0,0,0")
	    (match_operand:V4SF 2 "nonimmediate_operand" " x,o,x"))
	  (parallel [(const_int 6)
		     (const_int 7)
		     (const_int 2)
		     (const_int 3)])))]
  "TARGET_SSE && !(MEM_P (operands[1]) && MEM_P (operands[2]))"
  "@
   movhlps\t{%2, %0|%0, %2}
   movlps\t{%H2, %0|%0, %H2}
   movhps\t{%2, %0|%0, %2}"
  [(set_attr "type" "ssemov")
   (set_attr "mode" "V4SF,V2SF,V2SF")])

(define_expand "sse_movlhps_exp"
  [(set (match_operand:V4SF 0 "nonimmediate_operand" "")
	(vec_select:V4SF
	  (vec_concat:V8SF
	    (match_operand:V4SF 1 "nonimmediate_operand" "")
	    (match_operand:V4SF 2 "nonimmediate_operand" ""))
	  (parallel [(const_int 0)
		     (const_int 1)
		     (const_int 4)
		     (const_int 5)])))]
  "TARGET_SSE"
{
  rtx dst = ix86_fixup_binary_operands (UNKNOWN, V4SFmode, operands);
  
  emit_insn (gen_sse_movlhps (dst, operands[1], operands[2]));

  /* Fix up the destination if needed.  */
  if (dst != operands[0])
    emit_move_insn (operands[0], dst);

  DONE;
})

(define_insn "*avx_movlhps"
  [(set (match_operand:V4SF 0 "nonimmediate_operand"     "=x,x,o")
	(vec_select:V4SF
	  (vec_concat:V8SF
	    (match_operand:V4SF 1 "nonimmediate_operand" " x,x,0")
	    (match_operand:V4SF 2 "nonimmediate_operand" " x,m,x"))
	  (parallel [(const_int 0)
		     (const_int 1)
		     (const_int 4)
		     (const_int 5)])))]
  "TARGET_AVX && ix86_binary_operator_ok (UNKNOWN, V4SFmode, operands)"
  "@
   vmovlhps\t{%2, %1, %0|%0, %1, %2}
   vmovhps\t{%2, %1, %0|%0, %1, %2}
   vmovlps\t{%2, %H0|%H0, %2}"
  [(set_attr "type" "ssemov")
   (set_attr "prefix" "vex")
   (set_attr "mode" "V4SF,V2SF,V2SF")])

(define_insn "sse_movlhps"
  [(set (match_operand:V4SF 0 "nonimmediate_operand"     "=x,x,o")
	(vec_select:V4SF
	  (vec_concat:V8SF
	    (match_operand:V4SF 1 "nonimmediate_operand" " 0,0,0")
	    (match_operand:V4SF 2 "nonimmediate_operand" " x,m,x"))
	  (parallel [(const_int 0)
		     (const_int 1)
		     (const_int 4)
		     (const_int 5)])))]
  "TARGET_SSE && ix86_binary_operator_ok (UNKNOWN, V4SFmode, operands)"
  "@
   movlhps\t{%2, %0|%0, %2}
   movhps\t{%2, %0|%0, %2}
   movlps\t{%2, %H0|%H0, %2}"
  [(set_attr "type" "ssemov")
   (set_attr "mode" "V4SF,V2SF,V2SF")])

;; Recall that the 256-bit unpck insns only shuffle within their lanes.
(define_insn "avx_unpckhps256"
  [(set (match_operand:V8SF 0 "register_operand" "=x")
	(vec_select:V8SF
	  (vec_concat:V16SF
	    (match_operand:V8SF 1 "register_operand" "x")
	    (match_operand:V8SF 2 "nonimmediate_operand" "xm"))
	  (parallel [(const_int 2) (const_int 10)
		     (const_int 3) (const_int 11)
		     (const_int 6) (const_int 14)
		     (const_int 7) (const_int 15)])))]
  "TARGET_AVX"
  "vunpckhps\t{%2, %1, %0|%0, %1, %2}"
  [(set_attr "type" "sselog")
   (set_attr "prefix" "vex")
   (set_attr "mode" "V8SF")])

(define_insn "*avx_interleave_highv4sf"
  [(set (match_operand:V4SF 0 "register_operand" "=x")
	(vec_select:V4SF
	  (vec_concat:V8SF
	    (match_operand:V4SF 1 "register_operand" "x")
	    (match_operand:V4SF 2 "nonimmediate_operand" "xm"))
	  (parallel [(const_int 2) (const_int 6)
		     (const_int 3) (const_int 7)])))]
  "TARGET_AVX"
  "vunpckhps\t{%2, %1, %0|%0, %1, %2}"
  [(set_attr "type" "sselog")
   (set_attr "prefix" "vex")
   (set_attr "mode" "V4SF")])

(define_expand "vec_interleave_highv8sf"
  [(set (match_dup 3)
	(vec_select:V8SF
	  (vec_concat:V16SF
	    (match_operand:V8SF 1 "register_operand" "x")
	    (match_operand:V8SF 2 "nonimmediate_operand" "xm"))
	  (parallel [(const_int 0) (const_int 8)
		     (const_int 1) (const_int 9)
		     (const_int 4) (const_int 12)
		     (const_int 5) (const_int 13)])))
   (set (match_dup 4)
	(vec_select:V8SF
	  (vec_concat:V16SF
	    (match_dup 1)
	    (match_dup 2))
	  (parallel [(const_int 2) (const_int 10)
		     (const_int 3) (const_int 11)
		     (const_int 6) (const_int 14)
		     (const_int 7) (const_int 15)])))
   (set (match_operand:V8SF 0 "register_operand" "")
	(vec_select:V8SF
	  (vec_concat:V16SF
	    (match_dup 3)
	    (match_dup 4))
	  (parallel [(const_int 4) (const_int 5)
		     (const_int 6) (const_int 7)
		     (const_int 12) (const_int 13)
		     (const_int 14) (const_int 15)])))]
 "TARGET_AVX"
{
  operands[3] = gen_reg_rtx (V8SFmode);
  operands[4] = gen_reg_rtx (V8SFmode);
})

(define_insn "vec_interleave_highv4sf"
  [(set (match_operand:V4SF 0 "register_operand" "=x")
	(vec_select:V4SF
	  (vec_concat:V8SF
	    (match_operand:V4SF 1 "register_operand" "0")
	    (match_operand:V4SF 2 "nonimmediate_operand" "xm"))
	  (parallel [(const_int 2) (const_int 6)
		     (const_int 3) (const_int 7)])))]
  "TARGET_SSE"
  "unpckhps\t{%2, %0|%0, %2}"
  [(set_attr "type" "sselog")
   (set_attr "mode" "V4SF")])

;; Recall that the 256-bit unpck insns only shuffle within their lanes.
(define_insn "avx_unpcklps256"
  [(set (match_operand:V8SF 0 "register_operand" "=x")
	(vec_select:V8SF
	  (vec_concat:V16SF
	    (match_operand:V8SF 1 "register_operand" "x")
	    (match_operand:V8SF 2 "nonimmediate_operand" "xm"))
	  (parallel [(const_int 0) (const_int 8)
		     (const_int 1) (const_int 9)
		     (const_int 4) (const_int 12)
		     (const_int 5) (const_int 13)])))]
  "TARGET_AVX"
  "vunpcklps\t{%2, %1, %0|%0, %1, %2}"
  [(set_attr "type" "sselog")
   (set_attr "prefix" "vex")
   (set_attr "mode" "V8SF")])

(define_insn "*avx_interleave_lowv4sf"
  [(set (match_operand:V4SF 0 "register_operand" "=x")
	(vec_select:V4SF
	  (vec_concat:V8SF
	    (match_operand:V4SF 1 "register_operand" "x")
	    (match_operand:V4SF 2 "nonimmediate_operand" "xm"))
	  (parallel [(const_int 0) (const_int 4)
		     (const_int 1) (const_int 5)])))]
  "TARGET_AVX"
  "vunpcklps\t{%2, %1, %0|%0, %1, %2}"
  [(set_attr "type" "sselog")
   (set_attr "prefix" "vex")
   (set_attr "mode" "V4SF")])

(define_expand "vec_interleave_lowv8sf"
  [(set (match_dup 3)
	(vec_select:V8SF
	  (vec_concat:V16SF
	    (match_operand:V8SF 1 "register_operand" "x")
	    (match_operand:V8SF 2 "nonimmediate_operand" "xm"))
	  (parallel [(const_int 0) (const_int 8)
		     (const_int 1) (const_int 9)
		     (const_int 4) (const_int 12)
		     (const_int 5) (const_int 13)])))
   (set (match_dup 4)
	(vec_select:V8SF
	  (vec_concat:V16SF
	    (match_dup 1)
	    (match_dup 2))
	  (parallel [(const_int 2) (const_int 10)
		     (const_int 3) (const_int 11)
		     (const_int 6) (const_int 14)
		     (const_int 7) (const_int 15)])))
   (set (match_operand:V8SF 0 "register_operand" "")
	(vec_select:V8SF
	  (vec_concat:V16SF
	    (match_dup 3)
	    (match_dup 4))
	  (parallel [(const_int 0) (const_int 1)
		     (const_int 2) (const_int 3)
		     (const_int 8) (const_int 9)
		     (const_int 10) (const_int 11)])))]
 "TARGET_AVX"
{
  operands[3] = gen_reg_rtx (V8SFmode);
  operands[4] = gen_reg_rtx (V8SFmode);
})

(define_insn "vec_interleave_lowv4sf"
  [(set (match_operand:V4SF 0 "register_operand" "=x")
	(vec_select:V4SF
	  (vec_concat:V8SF
	    (match_operand:V4SF 1 "register_operand" "0")
	    (match_operand:V4SF 2 "nonimmediate_operand" "xm"))
	  (parallel [(const_int 0) (const_int 4)
		     (const_int 1) (const_int 5)])))]
  "TARGET_SSE"
  "unpcklps\t{%2, %0|%0, %2}"
  [(set_attr "type" "sselog")
   (set_attr "mode" "V4SF")])

;; These are modeled with the same vec_concat as the others so that we
;; capture users of shufps that can use the new instructions
(define_insn "avx_movshdup256"
  [(set (match_operand:V8SF 0 "register_operand" "=x")
	(vec_select:V8SF
	  (vec_concat:V16SF
	    (match_operand:V8SF 1 "nonimmediate_operand" "xm")
	    (match_dup 1))
	  (parallel [(const_int 1) (const_int 1)
		     (const_int 3) (const_int 3)
		     (const_int 5) (const_int 5)
		     (const_int 7) (const_int 7)])))]
  "TARGET_AVX"
  "vmovshdup\t{%1, %0|%0, %1}"
  [(set_attr "type" "sse")
   (set_attr "prefix" "vex")
   (set_attr "mode" "V8SF")])

(define_insn "sse3_movshdup"
  [(set (match_operand:V4SF 0 "register_operand" "=x")
	(vec_select:V4SF
	  (vec_concat:V8SF
	    (match_operand:V4SF 1 "nonimmediate_operand" "xm")
	    (match_dup 1))
	  (parallel [(const_int 1)
		     (const_int 1)
		     (const_int 7)
		     (const_int 7)])))]
  "TARGET_SSE3"
  "%vmovshdup\t{%1, %0|%0, %1}"
  [(set_attr "type" "sse")
   (set_attr "prefix_rep" "1")
   (set_attr "prefix" "maybe_vex")
   (set_attr "mode" "V4SF")])

(define_insn "avx_movsldup256"
  [(set (match_operand:V8SF 0 "register_operand" "=x")
	(vec_select:V8SF
	  (vec_concat:V16SF
	    (match_operand:V8SF 1 "nonimmediate_operand" "xm")
	    (match_dup 1))
	  (parallel [(const_int 0) (const_int 0)
		     (const_int 2) (const_int 2)
		     (const_int 4) (const_int 4)
		     (const_int 6) (const_int 6)])))]
  "TARGET_AVX"
  "vmovsldup\t{%1, %0|%0, %1}"
  [(set_attr "type" "sse")
   (set_attr "prefix" "vex")
   (set_attr "mode" "V8SF")])

(define_insn "sse3_movsldup"
  [(set (match_operand:V4SF 0 "register_operand" "=x")
	(vec_select:V4SF
	  (vec_concat:V8SF
	    (match_operand:V4SF 1 "nonimmediate_operand" "xm")
	    (match_dup 1))
	  (parallel [(const_int 0)
		     (const_int 0)
		     (const_int 6)
		     (const_int 6)])))]
  "TARGET_SSE3"
  "%vmovsldup\t{%1, %0|%0, %1}"
  [(set_attr "type" "sse")
   (set_attr "prefix_rep" "1")
   (set_attr "prefix" "maybe_vex")
   (set_attr "mode" "V4SF")])

(define_expand "avx_shufps256"
  [(match_operand:V8SF 0 "register_operand" "")
   (match_operand:V8SF 1 "register_operand" "")
   (match_operand:V8SF 2 "nonimmediate_operand" "")
   (match_operand:SI 3 "const_int_operand" "")]
  "TARGET_AVX"
{
  int mask = INTVAL (operands[3]);
  emit_insn (gen_avx_shufps256_1 (operands[0], operands[1], operands[2],
				  GEN_INT ((mask >> 0) & 3),
				  GEN_INT ((mask >> 2) & 3),
				  GEN_INT (((mask >> 4) & 3) + 8),
				  GEN_INT (((mask >> 6) & 3) + 8),
				  GEN_INT (((mask >> 0) & 3) + 4),
				  GEN_INT (((mask >> 2) & 3) + 4),
				  GEN_INT (((mask >> 4) & 3) + 12),
				  GEN_INT (((mask >> 6) & 3) + 12)));
  DONE;
})

;; One bit in mask selects 2 elements.
(define_insn "avx_shufps256_1"
  [(set (match_operand:V8SF 0 "register_operand" "=x")
	(vec_select:V8SF
	  (vec_concat:V16SF
	    (match_operand:V8SF 1 "register_operand" "x")
	    (match_operand:V8SF 2 "nonimmediate_operand" "xm"))
	  (parallel [(match_operand 3  "const_0_to_3_operand"   "")
		     (match_operand 4  "const_0_to_3_operand"   "")
		     (match_operand 5  "const_8_to_11_operand"  "")
		     (match_operand 6  "const_8_to_11_operand"  "")
		     (match_operand 7  "const_4_to_7_operand"   "")
		     (match_operand 8  "const_4_to_7_operand"   "")
		     (match_operand 9  "const_12_to_15_operand" "")
		     (match_operand 10 "const_12_to_15_operand" "")])))]
  "TARGET_AVX
   && (INTVAL (operands[3]) == (INTVAL (operands[7]) - 4)
       && INTVAL (operands[4]) == (INTVAL (operands[8]) - 4)
       && INTVAL (operands[5]) == (INTVAL (operands[9]) - 4)
       && INTVAL (operands[6]) == (INTVAL (operands[10]) - 4))"
{
  int mask;
  mask = INTVAL (operands[3]);
  mask |= INTVAL (operands[4]) << 2;
  mask |= (INTVAL (operands[5]) - 8) << 4;
  mask |= (INTVAL (operands[6]) - 8) << 6;
  operands[3] = GEN_INT (mask);

  return "vshufps\t{%3, %2, %1, %0|%0, %1, %2, %3}";
}
  [(set_attr "type" "sselog")
   (set_attr "length_immediate" "1")
   (set_attr "prefix" "vex")
   (set_attr "mode" "V8SF")])

(define_expand "sse_shufps"
  [(match_operand:V4SF 0 "register_operand" "")
   (match_operand:V4SF 1 "register_operand" "")
   (match_operand:V4SF 2 "nonimmediate_operand" "")
   (match_operand:SI 3 "const_int_operand" "")]
  "TARGET_SSE"
{
  int mask = INTVAL (operands[3]);
  emit_insn (gen_sse_shufps_v4sf (operands[0], operands[1], operands[2],
			       GEN_INT ((mask >> 0) & 3),
			       GEN_INT ((mask >> 2) & 3),
			       GEN_INT (((mask >> 4) & 3) + 4),
			       GEN_INT (((mask >> 6) & 3) + 4)));
  DONE;
})

(define_insn "*avx_shufps_<mode>"
  [(set (match_operand:SSEMODE4S 0 "register_operand" "=x")
	(vec_select:SSEMODE4S
	  (vec_concat:<ssedoublesizemode>
	    (match_operand:SSEMODE4S 1 "register_operand" "x")
	    (match_operand:SSEMODE4S 2 "nonimmediate_operand" "xm"))
	  (parallel [(match_operand 3 "const_0_to_3_operand" "")
		     (match_operand 4 "const_0_to_3_operand" "")
		     (match_operand 5 "const_4_to_7_operand" "")
		     (match_operand 6 "const_4_to_7_operand" "")])))]
  "TARGET_AVX"
{
  int mask = 0;
  mask |= INTVAL (operands[3]) << 0;
  mask |= INTVAL (operands[4]) << 2;
  mask |= (INTVAL (operands[5]) - 4) << 4;
  mask |= (INTVAL (operands[6]) - 4) << 6;
  operands[3] = GEN_INT (mask);

  return "vshufps\t{%3, %2, %1, %0|%0, %1, %2, %3}";
}
  [(set_attr "type" "sselog")
   (set_attr "length_immediate" "1")
   (set_attr "prefix" "vex")
   (set_attr "mode" "V4SF")])

(define_insn "sse_shufps_<mode>"
  [(set (match_operand:SSEMODE4S 0 "register_operand" "=x")
	(vec_select:SSEMODE4S
	  (vec_concat:<ssedoublesizemode>
	    (match_operand:SSEMODE4S 1 "register_operand" "0")
	    (match_operand:SSEMODE4S 2 "nonimmediate_operand" "xm"))
	  (parallel [(match_operand 3 "const_0_to_3_operand" "")
		     (match_operand 4 "const_0_to_3_operand" "")
		     (match_operand 5 "const_4_to_7_operand" "")
		     (match_operand 6 "const_4_to_7_operand" "")])))]
  "TARGET_SSE"
{
  int mask = 0;
  mask |= INTVAL (operands[3]) << 0;
  mask |= INTVAL (operands[4]) << 2;
  mask |= (INTVAL (operands[5]) - 4) << 4;
  mask |= (INTVAL (operands[6]) - 4) << 6;
  operands[3] = GEN_INT (mask);

  return "shufps\t{%3, %2, %0|%0, %2, %3}";
}
  [(set_attr "type" "sselog")
   (set_attr "length_immediate" "1")
   (set_attr "mode" "V4SF")])

(define_insn "sse_storehps"
  [(set (match_operand:V2SF 0 "nonimmediate_operand" "=m,x,x")
	(vec_select:V2SF
	  (match_operand:V4SF 1 "nonimmediate_operand" "x,x,o")
	  (parallel [(const_int 2) (const_int 3)])))]
  "TARGET_SSE"
  "@
   %vmovhps\t{%1, %0|%0, %1}
   %vmovhlps\t{%1, %d0|%d0, %1}
   %vmovlps\t{%H1, %d0|%d0, %H1}"
  [(set_attr "type" "ssemov")
   (set_attr "prefix" "maybe_vex")
   (set_attr "mode" "V2SF,V4SF,V2SF")])

(define_expand "sse_loadhps_exp"
  [(set (match_operand:V4SF 0 "nonimmediate_operand" "")
	(vec_concat:V4SF
	  (vec_select:V2SF
	    (match_operand:V4SF 1 "nonimmediate_operand" "")
	    (parallel [(const_int 0) (const_int 1)]))
	  (match_operand:V2SF 2 "nonimmediate_operand" "")))]
  "TARGET_SSE"
{
  rtx dst = ix86_fixup_binary_operands (UNKNOWN, V4SFmode, operands);
  
  emit_insn (gen_sse_loadhps (dst, operands[1], operands[2]));

  /* Fix up the destination if needed.  */
  if (dst != operands[0])
    emit_move_insn (operands[0], dst);

  DONE;
})

(define_insn "*avx_loadhps"
  [(set (match_operand:V4SF 0 "nonimmediate_operand" "=x,x,o")
	(vec_concat:V4SF
	  (vec_select:V2SF
	    (match_operand:V4SF 1 "nonimmediate_operand" "x,x,0")
	    (parallel [(const_int 0) (const_int 1)]))
	  (match_operand:V2SF 2 "nonimmediate_operand" "m,x,x")))]
  "TARGET_AVX"
  "@
   vmovhps\t{%2, %1, %0|%0, %1, %2}
   vmovlhps\t{%2, %1, %0|%0, %1, %2}
   vmovlps\t{%2, %H0|%H0, %2}"
  [(set_attr "type" "ssemov")
   (set_attr "prefix" "vex")
   (set_attr "mode" "V2SF,V4SF,V2SF")])

(define_insn "sse_loadhps"
  [(set (match_operand:V4SF 0 "nonimmediate_operand" "=x,x,o")
	(vec_concat:V4SF
	  (vec_select:V2SF
	    (match_operand:V4SF 1 "nonimmediate_operand" "0,0,0")
	    (parallel [(const_int 0) (const_int 1)]))
	  (match_operand:V2SF 2 "nonimmediate_operand" "m,x,x")))]
  "TARGET_SSE"
  "@
   movhps\t{%2, %0|%0, %2}
   movlhps\t{%2, %0|%0, %2}
   movlps\t{%2, %H0|%H0, %2}"
  [(set_attr "type" "ssemov")
   (set_attr "mode" "V2SF,V4SF,V2SF")])

(define_insn "*avx_storelps"
  [(set (match_operand:V2SF 0 "nonimmediate_operand" "=m,x,x")
	(vec_select:V2SF
	  (match_operand:V4SF 1 "nonimmediate_operand" "x,x,m")
	  (parallel [(const_int 0) (const_int 1)])))]
  "TARGET_AVX"
  "@
   vmovlps\t{%1, %0|%0, %1}
   vmovaps\t{%1, %0|%0, %1}
   vmovlps\t{%1, %0, %0|%0, %0, %1}"
  [(set_attr "type" "ssemov")
   (set_attr "prefix" "vex")
   (set_attr "mode" "V2SF,V2DF,V2SF")])

(define_insn "sse_storelps"
  [(set (match_operand:V2SF 0 "nonimmediate_operand" "=m,x,x")
	(vec_select:V2SF
	  (match_operand:V4SF 1 "nonimmediate_operand" "x,x,m")
	  (parallel [(const_int 0) (const_int 1)])))]
  "TARGET_SSE"
  "@
   movlps\t{%1, %0|%0, %1}
   movaps\t{%1, %0|%0, %1}
   movlps\t{%1, %0|%0, %1}"
  [(set_attr "type" "ssemov")
   (set_attr "mode" "V2SF,V4SF,V2SF")])

(define_expand "sse_loadlps_exp"
  [(set (match_operand:V4SF 0 "nonimmediate_operand" "")
	(vec_concat:V4SF
	  (match_operand:V2SF 2 "nonimmediate_operand" "")
	  (vec_select:V2SF
	    (match_operand:V4SF 1 "nonimmediate_operand" "")
	    (parallel [(const_int 2) (const_int 3)]))))]
  "TARGET_SSE"
{
  rtx dst = ix86_fixup_binary_operands (UNKNOWN, V4SFmode, operands);
  
  emit_insn (gen_sse_loadlps (dst, operands[1], operands[2]));

  /* Fix up the destination if needed.  */
  if (dst != operands[0])
    emit_move_insn (operands[0], dst);

  DONE;
})

(define_insn "*avx_loadlps"
  [(set (match_operand:V4SF 0 "nonimmediate_operand" "=x,x,m")
	(vec_concat:V4SF
	  (match_operand:V2SF 2 "nonimmediate_operand" "x,m,x")
	  (vec_select:V2SF
	    (match_operand:V4SF 1 "nonimmediate_operand" "x,x,0")
	    (parallel [(const_int 2) (const_int 3)]))))]
  "TARGET_AVX"
  "@
   shufps\t{$0xe4, %1, %2, %0|%0, %2, %1, 0xe4}
   vmovlps\t{%2, %1, %0|%0, %1, %2}
   vmovlps\t{%2, %0|%0, %2}"
  [(set_attr "type" "sselog,ssemov,ssemov")
   (set_attr "length_immediate" "1,*,*")
   (set_attr "prefix" "vex")
   (set_attr "mode" "V4SF,V2SF,V2SF")])

(define_insn "sse_loadlps"
  [(set (match_operand:V4SF 0 "nonimmediate_operand" "=x,x,m")
	(vec_concat:V4SF
	  (match_operand:V2SF 2 "nonimmediate_operand" "0,m,x")
	  (vec_select:V2SF
	    (match_operand:V4SF 1 "nonimmediate_operand" "x,0,0")
	    (parallel [(const_int 2) (const_int 3)]))))]
  "TARGET_SSE"
  "@
   shufps\t{$0xe4, %1, %0|%0, %1, 0xe4}
   movlps\t{%2, %0|%0, %2}
   movlps\t{%2, %0|%0, %2}"
  [(set_attr "type" "sselog,ssemov,ssemov")
   (set_attr "length_immediate" "1,*,*")
   (set_attr "mode" "V4SF,V2SF,V2SF")])

(define_insn "*avx_movss"
  [(set (match_operand:V4SF 0 "register_operand" "=x")
	(vec_merge:V4SF
	  (match_operand:V4SF 2 "register_operand" "x")
	  (match_operand:V4SF 1 "register_operand" "x")
	  (const_int 1)))]
  "TARGET_AVX"
  "vmovss\t{%2, %1, %0|%0, %1, %2}"
  [(set_attr "type" "ssemov")
   (set_attr "prefix" "vex")
   (set_attr "mode" "SF")])

(define_insn "sse_movss"
  [(set (match_operand:V4SF 0 "register_operand" "=x")
	(vec_merge:V4SF
	  (match_operand:V4SF 2 "register_operand" "x")
	  (match_operand:V4SF 1 "register_operand" "0")
	  (const_int 1)))]
  "TARGET_SSE"
  "movss\t{%2, %0|%0, %2}"
  [(set_attr "type" "ssemov")
   (set_attr "mode" "SF")])

(define_expand "vec_dupv4sf"
  [(set (match_operand:V4SF 0 "register_operand" "")
	(vec_duplicate:V4SF
	  (match_operand:SF 1 "nonimmediate_operand" "")))]
  "TARGET_SSE"
{
  if (!TARGET_AVX)
    operands[1] = force_reg (SFmode, operands[1]);
})

(define_insn "*vec_dupv4sf_avx"
  [(set (match_operand:V4SF 0 "register_operand" "=x,x")
	(vec_duplicate:V4SF
	  (match_operand:SF 1 "nonimmediate_operand" "x,m")))]
  "TARGET_AVX"
  "@
   vshufps\t{$0, %1, %1, %0|%0, %1, %1, 0}
   vbroadcastss\t{%1, %0|%0, %1}"
  [(set_attr "type" "sselog1,ssemov")
   (set_attr "length_immediate" "1,0")
   (set_attr "prefix_extra" "0,1")
   (set_attr "prefix" "vex")
   (set_attr "mode" "V4SF")])

(define_insn "*vec_dupv4sf"
  [(set (match_operand:V4SF 0 "register_operand" "=x")
	(vec_duplicate:V4SF
	  (match_operand:SF 1 "register_operand" "0")))]
  "TARGET_SSE"
  "shufps\t{$0, %0, %0|%0, %0, 0}"
  [(set_attr "type" "sselog1")
   (set_attr "length_immediate" "1")
   (set_attr "mode" "V4SF")])

(define_insn "*vec_concatv2sf_avx"
  [(set (match_operand:V2SF 0 "register_operand"     "=x,x,x,*y ,*y")
	(vec_concat:V2SF
	  (match_operand:SF 1 "nonimmediate_operand" " x,x,m, 0 , m")
	  (match_operand:SF 2 "vector_move_operand"  " x,m,C,*ym, C")))]
  "TARGET_AVX"
  "@
   vunpcklps\t{%2, %1, %0|%0, %1, %2}
   vinsertps\t{$0x10, %2, %1, %0|%0, %1, %2, 0x10}
   vmovss\t{%1, %0|%0, %1}
   punpckldq\t{%2, %0|%0, %2}
   movd\t{%1, %0|%0, %1}"
  [(set_attr "type" "sselog,sselog,ssemov,mmxcvt,mmxmov")
   (set_attr "length_immediate" "*,1,*,*,*")
   (set_attr "prefix_extra" "*,1,*,*,*")
   (set (attr "prefix")
     (if_then_else (eq_attr "alternative" "3,4")
       (const_string "orig")
       (const_string "vex")))
   (set_attr "mode" "V4SF,V4SF,SF,DI,DI")])

;; Although insertps takes register source, we prefer
;; unpcklps with register source since it is shorter.
(define_insn "*vec_concatv2sf_sse4_1"
  [(set (match_operand:V2SF 0 "register_operand"     "=x,x,x,*y ,*y")
	(vec_concat:V2SF
	  (match_operand:SF 1 "nonimmediate_operand" " 0,0,m, 0 , m")
	  (match_operand:SF 2 "vector_move_operand"  " x,m,C,*ym, C")))]
  "TARGET_SSE4_1"
  "@
   unpcklps\t{%2, %0|%0, %2}
   insertps\t{$0x10, %2, %0|%0, %2, 0x10}
   movss\t{%1, %0|%0, %1}
   punpckldq\t{%2, %0|%0, %2}
   movd\t{%1, %0|%0, %1}"
  [(set_attr "type" "sselog,sselog,ssemov,mmxcvt,mmxmov")
   (set_attr "prefix_data16" "*,1,*,*,*")
   (set_attr "prefix_extra" "*,1,*,*,*")
   (set_attr "length_immediate" "*,1,*,*,*")
   (set_attr "mode" "V4SF,V4SF,SF,DI,DI")])

;; ??? In theory we can match memory for the MMX alternative, but allowing
;; nonimmediate_operand for operand 2 and *not* allowing memory for the SSE
;; alternatives pretty much forces the MMX alternative to be chosen.
(define_insn "*vec_concatv2sf_sse"
  [(set (match_operand:V2SF 0 "register_operand"     "=x,x,*y,*y")
	(vec_concat:V2SF
	  (match_operand:SF 1 "nonimmediate_operand" " 0,m, 0, m")
	  (match_operand:SF 2 "reg_or_0_operand"     " x,C,*y, C")))]
  "TARGET_SSE"
  "@
   unpcklps\t{%2, %0|%0, %2}
   movss\t{%1, %0|%0, %1}
   punpckldq\t{%2, %0|%0, %2}
   movd\t{%1, %0|%0, %1}"
  [(set_attr "type" "sselog,ssemov,mmxcvt,mmxmov")
   (set_attr "mode" "V4SF,SF,DI,DI")])

(define_insn "*vec_concatv4sf_avx"
  [(set (match_operand:V4SF 0 "register_operand"   "=x,x")
	(vec_concat:V4SF
	  (match_operand:V2SF 1 "register_operand" " x,x")
	  (match_operand:V2SF 2 "nonimmediate_operand" " x,m")))]
  "TARGET_AVX"
  "@
   vmovlhps\t{%2, %1, %0|%0, %1, %2}
   vmovhps\t{%2, %1, %0|%0, %1, %2}"
  [(set_attr "type" "ssemov")
   (set_attr "prefix" "vex")
   (set_attr "mode" "V4SF,V2SF")])

(define_insn "*vec_concatv4sf_sse"
  [(set (match_operand:V4SF 0 "register_operand"   "=x,x")
	(vec_concat:V4SF
	  (match_operand:V2SF 1 "register_operand" " 0,0")
	  (match_operand:V2SF 2 "nonimmediate_operand" " x,m")))]
  "TARGET_SSE"
  "@
   movlhps\t{%2, %0|%0, %2}
   movhps\t{%2, %0|%0, %2}"
  [(set_attr "type" "ssemov")
   (set_attr "mode" "V4SF,V2SF")])

(define_expand "vec_init<mode>"
  [(match_operand:SSEMODE 0 "register_operand" "")
   (match_operand 1 "" "")]
  "TARGET_SSE"
{
  ix86_expand_vector_init (false, operands[0], operands[1]);
  DONE;
})

;; Avoid combining registers from different units in a single alternative,
;; see comment above inline_secondary_memory_needed function in i386.c
(define_insn "*vec_set<mode>_0_avx"
  [(set (match_operand:SSEMODE4S 0 "nonimmediate_operand"  "=x,x, x,x,  x,m, m,m")
	(vec_merge:SSEMODE4S
	  (vec_duplicate:SSEMODE4S
	    (match_operand:<ssescalarmode> 2
	      "general_operand"                            " x,m,*r,x,*rm,x,*r,fF"))
	  (match_operand:SSEMODE4S 1 "vector_move_operand" " C,C, C,x,  x,0, 0,0")
	  (const_int 1)))]
  "TARGET_AVX"
  "@
   vinsertps\t{$0xe, %2, %2, %0|%0, %2, %2, 0xe}
   vmov<ssescalarmodesuffix>\t{%2, %0|%0, %2}
   vmovd\t{%2, %0|%0, %2}
   vmovss\t{%2, %1, %0|%0, %1, %2}
   vpinsrd\t{$0, %2, %1, %0|%0, %1, %2, 0}
   #
   #
   #"
  [(set_attr "type" "sselog,ssemov,ssemov,ssemov,sselog,*,*,*")
   (set_attr "prefix_extra" "*,*,*,*,1,*,*,*")
   (set_attr "length_immediate" "*,*,*,*,1,*,*,*")
   (set_attr "prefix" "vex")
   (set_attr "mode" "SF,<ssescalarmode>,SI,SF,TI,*,*,*")])

;; Avoid combining registers from different units in a single alternative,
;; see comment above inline_secondary_memory_needed function in i386.c
(define_insn "*vec_set<mode>_0_sse4_1"
  [(set (match_operand:SSEMODE4S 0 "nonimmediate_operand"  "=x,x, x,x,  x, m,m")
	(vec_merge:SSEMODE4S
	  (vec_duplicate:SSEMODE4S
	    (match_operand:<ssescalarmode> 2
	      "general_operand"                            " x,m,*r,x,*rm,*r,fF"))
	  (match_operand:SSEMODE4S 1 "vector_move_operand" " C,C, C,0,  0, 0,0")
	  (const_int 1)))]
  "TARGET_SSE4_1"
  "@
   insertps\t{$0xe, %2, %0|%0, %2, 0xe}
   mov<ssescalarmodesuffix>\t{%2, %0|%0, %2}
   movd\t{%2, %0|%0, %2}
   movss\t{%2, %0|%0, %2}
   pinsrd\t{$0, %2, %0|%0, %2, 0}
   #
   #"
  [(set_attr "type" "sselog,ssemov,ssemov,ssemov,sselog,*,*")
   (set_attr "prefix_extra" "*,*,*,*,1,*,*")
   (set_attr "length_immediate" "*,*,*,*,1,*,*")
   (set_attr "mode" "SF,<ssescalarmode>,SI,SF,TI,*,*")])

;; Avoid combining registers from different units in a single alternative,
;; see comment above inline_secondary_memory_needed function in i386.c
(define_insn "*vec_set<mode>_0_sse2"
  [(set (match_operand:SSEMODE4S 0 "nonimmediate_operand"  "=x, x,x,m, m,m")
	(vec_merge:SSEMODE4S
	  (vec_duplicate:SSEMODE4S
	    (match_operand:<ssescalarmode> 2
	      "general_operand"                            " m,*r,x,x,*r,fF"))
	  (match_operand:SSEMODE4S 1 "vector_move_operand" " C, C,0,0, 0,0")
	  (const_int 1)))]
  "TARGET_SSE2"
  "@
   mov<ssescalarmodesuffix>\t{%2, %0|%0, %2}
   movd\t{%2, %0|%0, %2}
   movss\t{%2, %0|%0, %2}
   #
   #
   #"
  [(set_attr "type" "ssemov")
   (set_attr "mode" "<ssescalarmode>,SI,SF,*,*,*")])

;; Avoid combining registers from different units in a single alternative,
;; see comment above inline_secondary_memory_needed function in i386.c
(define_insn "vec_set<mode>_0"
  [(set (match_operand:SSEMODE4S 0 "nonimmediate_operand"  "=x,x,m, m,m")
	(vec_merge:SSEMODE4S
	  (vec_duplicate:SSEMODE4S
	    (match_operand:<ssescalarmode> 2
	      "general_operand"                            " m,x,x,*r,fF"))
	  (match_operand:SSEMODE4S 1 "vector_move_operand" " C,0,0, 0,0")
	  (const_int 1)))]
  "TARGET_SSE"
  "@
   movss\t{%2, %0|%0, %2}
   movss\t{%2, %0|%0, %2}
   #
   #
   #"
  [(set_attr "type" "ssemov")
   (set_attr "mode" "SF,SF,*,*,*")])

;; A subset is vec_setv4sf.
(define_insn "*vec_setv4sf_avx"
  [(set (match_operand:V4SF 0 "register_operand" "=x")
	(vec_merge:V4SF
	  (vec_duplicate:V4SF
	    (match_operand:SF 2 "nonimmediate_operand" "xm"))
	  (match_operand:V4SF 1 "register_operand" "x")
	  (match_operand:SI 3 "const_pow2_1_to_8_operand" "n")))]
  "TARGET_AVX"
{
  operands[3] = GEN_INT (exact_log2 (INTVAL (operands[3])) << 4);
  return "vinsertps\t{%3, %2, %1, %0|%0, %1, %2, %3}";
}
  [(set_attr "type" "sselog")
   (set_attr "prefix_extra" "1")
   (set_attr "length_immediate" "1")
   (set_attr "prefix" "vex")
   (set_attr "mode" "V4SF")])

(define_insn "*vec_setv4sf_sse4_1"
  [(set (match_operand:V4SF 0 "register_operand" "=x")
	(vec_merge:V4SF
	  (vec_duplicate:V4SF
	    (match_operand:SF 2 "nonimmediate_operand" "xm"))
	  (match_operand:V4SF 1 "register_operand" "0")
	  (match_operand:SI 3 "const_pow2_1_to_8_operand" "n")))]
  "TARGET_SSE4_1"
{
  operands[3] = GEN_INT (exact_log2 (INTVAL (operands[3])) << 4);
  return "insertps\t{%3, %2, %0|%0, %2, %3}";
}
  [(set_attr "type" "sselog")
   (set_attr "prefix_data16" "1")
   (set_attr "prefix_extra" "1")
   (set_attr "length_immediate" "1")
   (set_attr "mode" "V4SF")])

(define_insn "*avx_insertps"
  [(set (match_operand:V4SF 0 "register_operand" "=x")
	(unspec:V4SF [(match_operand:V4SF 2 "nonimmediate_operand" "xm")
		      (match_operand:V4SF 1 "register_operand" "x")
		      (match_operand:SI 3 "const_0_to_255_operand" "n")]
		     UNSPEC_INSERTPS))]
  "TARGET_AVX"
{
  if (MEM_P (operands[2]))
    {
      unsigned count_s = INTVAL (operands[3]) >> 6;
      if (count_s)
	operands[3] = GEN_INT (INTVAL (operands[3]) & 0x3f);
      operands[2] = adjust_address_nv (operands[2], SFmode, count_s * 4);
    }
  return "vinsertps\t{%3, %2, %1, %0|%0, %1, %2, %3}";
}
  [(set_attr "type" "sselog")
   (set_attr "prefix" "vex")
   (set_attr "prefix_extra" "1")
   (set_attr "length_immediate" "1")
   (set_attr "mode" "V4SF")])

(define_insn "sse4_1_insertps"
  [(set (match_operand:V4SF 0 "register_operand" "=x")
	(unspec:V4SF [(match_operand:V4SF 2 "nonimmediate_operand" "xm")
		      (match_operand:V4SF 1 "register_operand" "0")
		      (match_operand:SI 3 "const_0_to_255_operand" "n")]
		     UNSPEC_INSERTPS))]
  "TARGET_SSE4_1"
{
  if (MEM_P (operands[2]))
    {
      unsigned count_s = INTVAL (operands[3]) >> 6;
      if (count_s)
	operands[3] = GEN_INT (INTVAL (operands[3]) & 0x3f);
      operands[2] = adjust_address_nv (operands[2], SFmode, count_s * 4);
    }
  return "insertps\t{%3, %2, %0|%0, %2, %3}";
}
  [(set_attr "type" "sselog")
   (set_attr "prefix_data16" "1")
   (set_attr "prefix_extra" "1")
   (set_attr "length_immediate" "1")
   (set_attr "mode" "V4SF")])

(define_split
  [(set (match_operand:SSEMODE4S 0 "memory_operand" "")
	(vec_merge:SSEMODE4S
	  (vec_duplicate:SSEMODE4S
	    (match_operand:<ssescalarmode> 1 "nonmemory_operand" ""))
	  (match_dup 0)
	  (const_int 1)))]
  "TARGET_SSE && reload_completed"
  [(const_int 0)]
{
  emit_move_insn (adjust_address (operands[0], <ssescalarmode>mode, 0),
		  operands[1]);
  DONE;
})

(define_expand "vec_set<mode>"
  [(match_operand:SSEMODE 0 "register_operand" "")
   (match_operand:<ssescalarmode> 1 "register_operand" "")
   (match_operand 2 "const_int_operand" "")]
  "TARGET_SSE"
{
  ix86_expand_vector_set (false, operands[0], operands[1],
			  INTVAL (operands[2]));
  DONE;
})

(define_insn_and_split "*vec_extractv4sf_0"
  [(set (match_operand:SF 0 "nonimmediate_operand" "=x,m,f,r")
	(vec_select:SF
	  (match_operand:V4SF 1 "nonimmediate_operand" "xm,x,m,m")
	  (parallel [(const_int 0)])))]
  "TARGET_SSE && !(MEM_P (operands[0]) && MEM_P (operands[1]))"
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

(define_expand "avx_vextractf128<mode>"
  [(match_operand:<avxhalfvecmode> 0 "nonimmediate_operand" "")
   (match_operand:AVX256MODE 1 "register_operand" "")
   (match_operand:SI 2 "const_0_to_1_operand" "")]
  "TARGET_AVX"
{
  switch (INTVAL (operands[2]))
    {
    case 0:
      emit_insn (gen_vec_extract_lo_<mode> (operands[0], operands[1]));
      break;
    case 1:
      emit_insn (gen_vec_extract_hi_<mode> (operands[0], operands[1]));
      break;
    default:
      gcc_unreachable ();
    }
  DONE;
})

(define_insn_and_split "vec_extract_lo_<mode>"
  [(set (match_operand:<avxhalfvecmode> 0 "nonimmediate_operand" "=x,m")
	(vec_select:<avxhalfvecmode>
	  (match_operand:AVX256MODE4P 1 "nonimmediate_operand" "xm,x")
	  (parallel [(const_int 0) (const_int 1)])))]
  "TARGET_AVX && !(MEM_P (operands[0]) && MEM_P (operands[1]))"
  "#"
  "&& reload_completed"
  [(const_int 0)]
{
  rtx op1 = operands[1];
  if (REG_P (op1))
    op1 = gen_rtx_REG (<avxhalfvecmode>mode, REGNO (op1));
  else
    op1 = gen_lowpart (<avxhalfvecmode>mode, op1);
  emit_move_insn (operands[0], op1);
  DONE;
})

(define_insn "vec_extract_hi_<mode>"
  [(set (match_operand:<avxhalfvecmode> 0 "nonimmediate_operand" "=x,m")
	(vec_select:<avxhalfvecmode>
	  (match_operand:AVX256MODE4P 1 "register_operand" "x,x")
	  (parallel [(const_int 2) (const_int 3)])))]
  "TARGET_AVX"
  "vextractf128\t{$0x1, %1, %0|%0, %1, 0x1}"
  [(set_attr "type" "sselog")
   (set_attr "prefix_extra" "1")
   (set_attr "length_immediate" "1")
   (set_attr "memory" "none,store")
   (set_attr "prefix" "vex")
   (set_attr "mode" "V8SF")])

(define_insn_and_split "vec_extract_lo_<mode>"
  [(set (match_operand:<avxhalfvecmode> 0 "nonimmediate_operand" "=x,m")
	(vec_select:<avxhalfvecmode>
	  (match_operand:AVX256MODE8P 1 "nonimmediate_operand" "xm,x")
	  (parallel [(const_int 0) (const_int 1)
		     (const_int 2) (const_int 3)])))]
  "TARGET_AVX && !(MEM_P (operands[0]) && MEM_P (operands[1]))"
  "#"
  "&& reload_completed"
  [(const_int 0)]
{
  rtx op1 = operands[1];
  if (REG_P (op1))
    op1 = gen_rtx_REG (<avxhalfvecmode>mode, REGNO (op1));
  else
    op1 = gen_lowpart (<avxhalfvecmode>mode, op1);
  emit_move_insn (operands[0], op1);
  DONE;
})

(define_insn "vec_extract_hi_<mode>"
  [(set (match_operand:<avxhalfvecmode> 0 "nonimmediate_operand" "=x,m")
	(vec_select:<avxhalfvecmode>
	  (match_operand:AVX256MODE8P 1 "register_operand" "x,x")
	  (parallel [(const_int 4) (const_int 5)
		     (const_int 6) (const_int 7)])))]
  "TARGET_AVX"
  "vextractf128\t{$0x1, %1, %0|%0, %1, 0x1}"
  [(set_attr "type" "sselog")
   (set_attr "prefix_extra" "1")
   (set_attr "length_immediate" "1")
   (set_attr "memory" "none,store")
   (set_attr "prefix" "vex")
   (set_attr "mode" "V8SF")])

(define_insn_and_split "vec_extract_lo_v16hi"
  [(set (match_operand:V8HI 0 "nonimmediate_operand" "=x,m")
	(vec_select:V8HI
	  (match_operand:V16HI 1 "nonimmediate_operand" "xm,x")
	  (parallel [(const_int 0) (const_int 1)
		     (const_int 2) (const_int 3)
		     (const_int 4) (const_int 5)
		     (const_int 6) (const_int 7)])))]
  "TARGET_AVX && !(MEM_P (operands[0]) && MEM_P (operands[1]))"
  "#"
  "&& reload_completed"
  [(const_int 0)]
{
  rtx op1 = operands[1];
  if (REG_P (op1))
    op1 = gen_rtx_REG (V8HImode, REGNO (op1));
  else
    op1 = gen_lowpart (V8HImode, op1);
  emit_move_insn (operands[0], op1);
  DONE;
})

(define_insn "vec_extract_hi_v16hi"
  [(set (match_operand:V8HI 0 "nonimmediate_operand" "=x,m")
	(vec_select:V8HI
	  (match_operand:V16HI 1 "register_operand" "x,x")
	  (parallel [(const_int 8) (const_int 9)
		     (const_int 10) (const_int 11)
		     (const_int 12) (const_int 13)
		     (const_int 14) (const_int 15)])))]
  "TARGET_AVX"
  "vextractf128\t{$0x1, %1, %0|%0, %1, 0x1}"
  [(set_attr "type" "sselog")
   (set_attr "prefix_extra" "1")
   (set_attr "length_immediate" "1")
   (set_attr "memory" "none,store")
   (set_attr "prefix" "vex")
   (set_attr "mode" "V8SF")])

(define_insn_and_split "vec_extract_lo_v32qi"
  [(set (match_operand:V16QI 0 "nonimmediate_operand" "=x,m")
	(vec_select:V16QI
	  (match_operand:V32QI 1 "nonimmediate_operand" "xm,x")
	  (parallel [(const_int 0) (const_int 1)
		     (const_int 2) (const_int 3)
		     (const_int 4) (const_int 5)
		     (const_int 6) (const_int 7)
		     (const_int 8) (const_int 9)
		     (const_int 10) (const_int 11)
		     (const_int 12) (const_int 13)
		     (const_int 14) (const_int 15)])))]
  "TARGET_AVX && !(MEM_P (operands[0]) && MEM_P (operands[1]))"
  "#"
  "&& reload_completed"
  [(const_int 0)]
{
  rtx op1 = operands[1];
  if (REG_P (op1))
    op1 = gen_rtx_REG (V16QImode, REGNO (op1));
  else
    op1 = gen_lowpart (V16QImode, op1);
  emit_move_insn (operands[0], op1);
  DONE;
})

(define_insn "vec_extract_hi_v32qi"
  [(set (match_operand:V16QI 0 "nonimmediate_operand" "=x,m")
	(vec_select:V16QI
	  (match_operand:V32QI 1 "register_operand" "x,x")
	  (parallel [(const_int 16) (const_int 17)
		     (const_int 18) (const_int 19)
		     (const_int 20) (const_int 21)
		     (const_int 22) (const_int 23)
		     (const_int 24) (const_int 25)
		     (const_int 26) (const_int 27)
		     (const_int 28) (const_int 29)
		     (const_int 30) (const_int 31)])))]
  "TARGET_AVX"
  "vextractf128\t{$0x1, %1, %0|%0, %1, 0x1}"
  [(set_attr "type" "sselog")
   (set_attr "prefix_extra" "1")
   (set_attr "length_immediate" "1")
   (set_attr "memory" "none,store")
   (set_attr "prefix" "vex")
   (set_attr "mode" "V8SF")])

(define_insn "*sse4_1_extractps"
  [(set (match_operand:SF 0 "nonimmediate_operand" "=rm")
	(vec_select:SF
	  (match_operand:V4SF 1 "register_operand" "x")
	  (parallel [(match_operand:SI 2 "const_0_to_3_operand" "n")])))]
  "TARGET_SSE4_1"
  "%vextractps\t{%2, %1, %0|%0, %1, %2}"
  [(set_attr "type" "sselog")
   (set_attr "prefix_data16" "1")
   (set_attr "prefix_extra" "1")
   (set_attr "length_immediate" "1")
   (set_attr "prefix" "maybe_vex")
   (set_attr "mode" "V4SF")])

(define_insn_and_split "*vec_extract_v4sf_mem"
  [(set (match_operand:SF 0 "register_operand" "=x*rf")
       (vec_select:SF
	 (match_operand:V4SF 1 "memory_operand" "o")
	 (parallel [(match_operand 2 "const_0_to_3_operand" "n")])))]
  "TARGET_SSE"
  "#"
  "&& reload_completed"
  [(const_int 0)]
{
  int i = INTVAL (operands[2]);

  emit_move_insn (operands[0], adjust_address (operands[1], SFmode, i*4));
  DONE;
})

(define_expand "vec_extract<mode>"
  [(match_operand:<avxscalarmode> 0 "register_operand" "")
   (match_operand:VEC_EXTRACT_MODE 1 "register_operand" "")
   (match_operand 2 "const_int_operand" "")]
  "TARGET_SSE"
{
  ix86_expand_vector_extract (false, operands[0], operands[1],
			      INTVAL (operands[2]));
  DONE;
})

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Parallel double-precision floating point element swizzling
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Recall that the 256-bit unpck insns only shuffle within their lanes.
(define_insn "avx_unpckhpd256"
  [(set (match_operand:V4DF 0 "register_operand" "=x")
	(vec_select:V4DF
	  (vec_concat:V8DF
	    (match_operand:V4DF 1 "register_operand" "x")
	    (match_operand:V4DF 2 "nonimmediate_operand" "xm"))
	  (parallel [(const_int 1) (const_int 5)
		     (const_int 3) (const_int 7)])))]
  "TARGET_AVX"
  "vunpckhpd\t{%2, %1, %0|%0, %1, %2}"
  [(set_attr "type" "sselog")
   (set_attr "prefix" "vex")
   (set_attr "mode" "V4DF")])

(define_expand "vec_interleave_highv4df"
  [(set (match_dup 3)
	(vec_select:V4DF
	  (vec_concat:V8DF
	    (match_operand:V4DF 1 "register_operand" "x")
	    (match_operand:V4DF 2 "nonimmediate_operand" "xm"))
	  (parallel [(const_int 0) (const_int 4)
		     (const_int 2) (const_int 6)])))
   (set (match_dup 4)
	(vec_select:V4DF
	  (vec_concat:V8DF
	    (match_dup 1)
	    (match_dup 2))
	  (parallel [(const_int 1) (const_int 5)
		     (const_int 3) (const_int 7)])))
   (set (match_operand:V4DF 0 "register_operand" "")
	(vec_select:V4DF
	  (vec_concat:V8DF
	    (match_dup 3)
	    (match_dup 4))
	  (parallel [(const_int 2) (const_int 3)
		     (const_int 6) (const_int 7)])))]
 "TARGET_AVX"
{
  operands[3] = gen_reg_rtx (V4DFmode);
  operands[4] = gen_reg_rtx (V4DFmode);
})


(define_expand "vec_interleave_highv2df"
  [(set (match_operand:V2DF 0 "register_operand" "")
	(vec_select:V2DF
	  (vec_concat:V4DF
	    (match_operand:V2DF 1 "nonimmediate_operand" "")
	    (match_operand:V2DF 2 "nonimmediate_operand" ""))
	  (parallel [(const_int 1)
		     (const_int 3)])))]
  "TARGET_SSE2"
{
  if (!ix86_vec_interleave_v2df_operator_ok (operands, 1))
    operands[2] = force_reg (V2DFmode, operands[2]);
})

(define_insn "*avx_interleave_highv2df"
  [(set (match_operand:V2DF 0 "nonimmediate_operand"     "=x,x,x,m")
	(vec_select:V2DF
	  (vec_concat:V4DF
	    (match_operand:V2DF 1 "nonimmediate_operand" " x,o,o,x")
	    (match_operand:V2DF 2 "nonimmediate_operand" " x,1,x,0"))
	  (parallel [(const_int 1)
		     (const_int 3)])))]
  "TARGET_AVX && ix86_vec_interleave_v2df_operator_ok (operands, 1)"
  "@
   vunpckhpd\t{%2, %1, %0|%0, %1, %2}
   vmovddup\t{%H1, %0|%0, %H1}
   vmovlpd\t{%H1, %2, %0|%0, %2, %H1}
   vmovhpd\t{%1, %0|%0, %1}"
  [(set_attr "type" "sselog,sselog,ssemov,ssemov")
   (set_attr "prefix" "vex")
   (set_attr "mode" "V2DF,V2DF,V1DF,V1DF")])

(define_insn "*sse3_interleave_highv2df"
  [(set (match_operand:V2DF 0 "nonimmediate_operand"     "=x,x,x,m")
	(vec_select:V2DF
	  (vec_concat:V4DF
	    (match_operand:V2DF 1 "nonimmediate_operand" " 0,o,o,x")
	    (match_operand:V2DF 2 "nonimmediate_operand" " x,1,0,0"))
	  (parallel [(const_int 1)
		     (const_int 3)])))]
  "TARGET_SSE3 && ix86_vec_interleave_v2df_operator_ok (operands, 1)"
  "@
   unpckhpd\t{%2, %0|%0, %2}
   movddup\t{%H1, %0|%0, %H1}
   movlpd\t{%H1, %0|%0, %H1}
   movhpd\t{%1, %0|%0, %1}"
  [(set_attr "type" "sselog,sselog,ssemov,ssemov")
   (set_attr "prefix_data16" "*,*,1,1")
   (set_attr "mode" "V2DF,V2DF,V1DF,V1DF")])

(define_insn "*sse2_interleave_highv2df"
  [(set (match_operand:V2DF 0 "nonimmediate_operand"     "=x,x,m")
	(vec_select:V2DF
	  (vec_concat:V4DF
	    (match_operand:V2DF 1 "nonimmediate_operand" " 0,o,x")
	    (match_operand:V2DF 2 "nonimmediate_operand" " x,0,0"))
	  (parallel [(const_int 1)
		     (const_int 3)])))]
  "TARGET_SSE2 && ix86_vec_interleave_v2df_operator_ok (operands, 1)"
  "@
   unpckhpd\t{%2, %0|%0, %2}
   movlpd\t{%H1, %0|%0, %H1}
   movhpd\t{%1, %0|%0, %1}"
  [(set_attr "type" "sselog,ssemov,ssemov")
   (set_attr "prefix_data16" "*,1,1")
   (set_attr "mode" "V2DF,V1DF,V1DF")])

;; Recall that the 256-bit unpck insns only shuffle within their lanes.
(define_expand "avx_movddup256"
  [(set (match_operand:V4DF 0 "register_operand" "")
	(vec_select:V4DF
	  (vec_concat:V8DF
	    (match_operand:V4DF 1 "nonimmediate_operand" "")
	    (match_dup 1))
	  (parallel [(const_int 0) (const_int 4)
		     (const_int 2) (const_int 6)])))]
  "TARGET_AVX")

(define_expand "avx_unpcklpd256"
  [(set (match_operand:V4DF 0 "register_operand" "")
	(vec_select:V4DF
	  (vec_concat:V8DF
	    (match_operand:V4DF 1 "register_operand" "")
	    (match_operand:V4DF 2 "nonimmediate_operand" ""))
	  (parallel [(const_int 0) (const_int 4)
		     (const_int 2) (const_int 6)])))]
  "TARGET_AVX")

(define_insn "*avx_unpcklpd256"
  [(set (match_operand:V4DF 0 "register_operand"         "=x,x")
	(vec_select:V4DF
	  (vec_concat:V8DF
	    (match_operand:V4DF 1 "nonimmediate_operand" "xm,x")
	    (match_operand:V4DF 2 "nonimmediate_operand" " 1,xm"))
	  (parallel [(const_int 0) (const_int 4)
		     (const_int 2) (const_int 6)])))]
  "TARGET_AVX
   && (!MEM_P (operands[1]) || rtx_equal_p (operands[1], operands[2]))"
  "@
   vmovddup\t{%1, %0|%0, %1}
   vunpcklpd\t{%2, %1, %0|%0, %1, %2}"
  [(set_attr "type" "sselog")
   (set_attr "prefix" "vex")
   (set_attr "mode" "V4DF")])

(define_expand "vec_interleave_lowv4df"
  [(set (match_dup 3)
	(vec_select:V4DF
	  (vec_concat:V8DF
	    (match_operand:V4DF 1 "register_operand" "x")
	    (match_operand:V4DF 2 "nonimmediate_operand" "xm"))
	  (parallel [(const_int 0) (const_int 4)
		     (const_int 2) (const_int 6)])))
   (set (match_dup 4)
	(vec_select:V4DF
	  (vec_concat:V8DF
	    (match_dup 1)
	    (match_dup 2))
	  (parallel [(const_int 1) (const_int 5)
		     (const_int 3) (const_int 7)])))
   (set (match_operand:V4DF 0 "register_operand" "")
	(vec_select:V4DF
	  (vec_concat:V8DF
	    (match_dup 3)
	    (match_dup 4))
	  (parallel [(const_int 0) (const_int 1)
	  	     (const_int 4) (const_int 5)])))]
 "TARGET_AVX"
{
  operands[3] = gen_reg_rtx (V4DFmode);
  operands[4] = gen_reg_rtx (V4DFmode);
})

(define_expand "vec_interleave_lowv2df"
  [(set (match_operand:V2DF 0 "register_operand" "")
	(vec_select:V2DF
	  (vec_concat:V4DF
	    (match_operand:V2DF 1 "nonimmediate_operand" "")
	    (match_operand:V2DF 2 "nonimmediate_operand" ""))
	  (parallel [(const_int 0)
		     (const_int 2)])))]
  "TARGET_SSE2"
{
  if (!ix86_vec_interleave_v2df_operator_ok (operands, 0))
    operands[1] = force_reg (V2DFmode, operands[1]);
})

(define_insn "*avx_interleave_lowv2df"
  [(set (match_operand:V2DF 0 "nonimmediate_operand"     "=x,x,x,o")
	(vec_select:V2DF
	  (vec_concat:V4DF
	    (match_operand:V2DF 1 "nonimmediate_operand" " x,m,x,0")
	    (match_operand:V2DF 2 "nonimmediate_operand" " x,1,m,x"))
	  (parallel [(const_int 0)
		     (const_int 2)])))]
  "TARGET_AVX && ix86_vec_interleave_v2df_operator_ok (operands, 0)"
  "@
   vunpcklpd\t{%2, %1, %0|%0, %1, %2}
   vmovddup\t{%1, %0|%0, %1}
   vmovhpd\t{%2, %1, %0|%0, %1, %2}
   vmovlpd\t{%2, %H0|%H0, %2}"
  [(set_attr "type" "sselog,sselog,ssemov,ssemov")
   (set_attr "prefix" "vex")
   (set_attr "mode" "V2DF,V2DF,V1DF,V1DF")])

(define_insn "*sse3_interleave_lowv2df"
  [(set (match_operand:V2DF 0 "nonimmediate_operand"     "=x,x,x,o")
	(vec_select:V2DF
	  (vec_concat:V4DF
	    (match_operand:V2DF 1 "nonimmediate_operand" " 0,m,0,0")
	    (match_operand:V2DF 2 "nonimmediate_operand" " x,1,m,x"))
	  (parallel [(const_int 0)
		     (const_int 2)])))]
  "TARGET_SSE3 && ix86_vec_interleave_v2df_operator_ok (operands, 0)"
  "@
   unpcklpd\t{%2, %0|%0, %2}
   movddup\t{%1, %0|%0, %1}
   movhpd\t{%2, %0|%0, %2}
   movlpd\t{%2, %H0|%H0, %2}"
  [(set_attr "type" "sselog,sselog,ssemov,ssemov")
   (set_attr "prefix_data16" "*,*,1,1")
   (set_attr "mode" "V2DF,V2DF,V1DF,V1DF")])

(define_insn "*sse2_interleave_lowv2df"
  [(set (match_operand:V2DF 0 "nonimmediate_operand"     "=x,x,o")
	(vec_select:V2DF
	  (vec_concat:V4DF
	    (match_operand:V2DF 1 "nonimmediate_operand" " 0,0,0")
	    (match_operand:V2DF 2 "nonimmediate_operand" " x,m,x"))
	  (parallel [(const_int 0)
		     (const_int 2)])))]
  "TARGET_SSE2 && ix86_vec_interleave_v2df_operator_ok (operands, 0)"
  "@
   unpcklpd\t{%2, %0|%0, %2}
   movhpd\t{%2, %0|%0, %2}
   movlpd\t{%2, %H0|%H0, %2}"
  [(set_attr "type" "sselog,ssemov,ssemov")
   (set_attr "prefix_data16" "*,1,1")
   (set_attr "mode" "V2DF,V1DF,V1DF")])

(define_split
  [(set (match_operand:V2DF 0 "memory_operand" "")
	(vec_select:V2DF
	  (vec_concat:V4DF
	    (match_operand:V2DF 1 "register_operand" "")
	    (match_dup 1))
	  (parallel [(const_int 0)
		     (const_int 2)])))]
  "TARGET_SSE3 && reload_completed"
  [(const_int 0)]
{
  rtx low = gen_rtx_REG (DFmode, REGNO (operands[1]));
  emit_move_insn (adjust_address (operands[0], DFmode, 0), low);
  emit_move_insn (adjust_address (operands[0], DFmode, 8), low);
  DONE;
})

(define_split
  [(set (match_operand:V2DF 0 "register_operand" "")
	(vec_select:V2DF
	  (vec_concat:V4DF
	    (match_operand:V2DF 1 "memory_operand" "")
	    (match_dup 1))
	  (parallel [(match_operand:SI 2 "const_0_to_1_operand" "")
		     (match_operand:SI 3 "const_int_operand" "")])))]
  "TARGET_SSE3 && INTVAL (operands[2]) + 2 == INTVAL (operands[3])"
  [(set (match_dup 0) (vec_duplicate:V2DF (match_dup 1)))]
{
  operands[1] = adjust_address (operands[1], DFmode, INTVAL (operands[2]) * 8);
})

(define_expand "avx_shufpd256"
  [(match_operand:V4DF 0 "register_operand" "")
   (match_operand:V4DF 1 "register_operand" "")
   (match_operand:V4DF 2 "nonimmediate_operand" "")
   (match_operand:SI 3 "const_int_operand" "")]
  "TARGET_AVX"
{
  int mask = INTVAL (operands[3]);
  emit_insn (gen_avx_shufpd256_1 (operands[0], operands[1], operands[2],
				   GEN_INT (mask & 1),
				   GEN_INT (mask & 2 ? 5 : 4),
				   GEN_INT (mask & 4 ? 3 : 2),
				   GEN_INT (mask & 8 ? 7 : 6)));
  DONE;
})

(define_insn "avx_shufpd256_1"
  [(set (match_operand:V4DF 0 "register_operand" "=x")
	(vec_select:V4DF
	  (vec_concat:V8DF
	    (match_operand:V4DF 1 "register_operand" "x")
	    (match_operand:V4DF 2 "nonimmediate_operand" "xm"))
	  (parallel [(match_operand 3 "const_0_to_1_operand" "")
		     (match_operand 4 "const_4_to_5_operand" "")
		     (match_operand 5 "const_2_to_3_operand" "")
		     (match_operand 6 "const_6_to_7_operand" "")])))]
  "TARGET_AVX"
{
  int mask;
  mask = INTVAL (operands[3]);
  mask |= (INTVAL (operands[4]) - 4) << 1;
  mask |= (INTVAL (operands[5]) - 2) << 2;
  mask |= (INTVAL (operands[6]) - 6) << 3;
  operands[3] = GEN_INT (mask);

  return "vshufpd\t{%3, %2, %1, %0|%0, %1, %2, %3}";
}
  [(set_attr "type" "sselog")
   (set_attr "length_immediate" "1")
   (set_attr "prefix" "vex")
   (set_attr "mode" "V4DF")])

(define_expand "sse2_shufpd"
  [(match_operand:V2DF 0 "register_operand" "")
   (match_operand:V2DF 1 "register_operand" "")
   (match_operand:V2DF 2 "nonimmediate_operand" "")
   (match_operand:SI 3 "const_int_operand" "")]
  "TARGET_SSE2"
{
  int mask = INTVAL (operands[3]);
  emit_insn (gen_sse2_shufpd_v2df (operands[0], operands[1], operands[2],
				GEN_INT (mask & 1),
				GEN_INT (mask & 2 ? 3 : 2)));
  DONE;
})

(define_expand "vec_extract_even<mode>"
  [(match_operand:SSEMODE_EO 0 "register_operand" "")
   (match_operand:SSEMODE_EO 1 "register_operand" "")
   (match_operand:SSEMODE_EO 2 "register_operand" "")]
  ""
{
  ix86_expand_vec_extract_even_odd (operands[0], operands[1], operands[2], 0);
  DONE;
})

(define_expand "vec_extract_odd<mode>"
  [(match_operand:SSEMODE_EO 0 "register_operand" "")
   (match_operand:SSEMODE_EO 1 "register_operand" "")
   (match_operand:SSEMODE_EO 2 "register_operand" "")]
  ""
{
  ix86_expand_vec_extract_even_odd (operands[0], operands[1], operands[2], 1);
  DONE;
})

;; punpcklqdq and punpckhqdq are shorter than shufpd.
(define_insn "*avx_interleave_highv2di"
  [(set (match_operand:V2DI 0 "register_operand" "=x")
	(vec_select:V2DI
	  (vec_concat:V4DI
	    (match_operand:V2DI 1 "register_operand" "x")
	    (match_operand:V2DI 2 "nonimmediate_operand" "xm"))
	  (parallel [(const_int 1)
		     (const_int 3)])))]
  "TARGET_AVX"
  "vpunpckhqdq\t{%2, %1, %0|%0, %1, %2}"
  [(set_attr "type" "sselog")
   (set_attr "prefix" "vex")
   (set_attr "mode" "TI")])

(define_insn "vec_interleave_highv2di"
  [(set (match_operand:V2DI 0 "register_operand" "=x")
	(vec_select:V2DI
	  (vec_concat:V4DI
	    (match_operand:V2DI 1 "register_operand" "0")
	    (match_operand:V2DI 2 "nonimmediate_operand" "xm"))
	  (parallel [(const_int 1)
		     (const_int 3)])))]
  "TARGET_SSE2"
  "punpckhqdq\t{%2, %0|%0, %2}"
  [(set_attr "type" "sselog")
   (set_attr "prefix_data16" "1")
   (set_attr "mode" "TI")])

(define_insn "*avx_interleave_lowv2di"
  [(set (match_operand:V2DI 0 "register_operand" "=x")
	(vec_select:V2DI
	  (vec_concat:V4DI
	    (match_operand:V2DI 1 "register_operand" "x")
	    (match_operand:V2DI 2 "nonimmediate_operand" "xm"))
	  (parallel [(const_int 0)
		     (const_int 2)])))]
  "TARGET_AVX"
  "vpunpcklqdq\t{%2, %1, %0|%0, %1, %2}"
  [(set_attr "type" "sselog")
   (set_attr "prefix" "vex")
   (set_attr "mode" "TI")])

(define_insn "vec_interleave_lowv2di"
  [(set (match_operand:V2DI 0 "register_operand" "=x")
	(vec_select:V2DI
	  (vec_concat:V4DI
	    (match_operand:V2DI 1 "register_operand" "0")
	    (match_operand:V2DI 2 "nonimmediate_operand" "xm"))
	  (parallel [(const_int 0)
		     (const_int 2)])))]
  "TARGET_SSE2"
  "punpcklqdq\t{%2, %0|%0, %2}"
  [(set_attr "type" "sselog")
   (set_attr "prefix_data16" "1")
   (set_attr "mode" "TI")])

(define_insn "*avx_shufpd_<mode>"
  [(set (match_operand:SSEMODE2D 0 "register_operand" "=x")
	(vec_select:SSEMODE2D
	  (vec_concat:<ssedoublesizemode>
	    (match_operand:SSEMODE2D 1 "register_operand" "x")
	    (match_operand:SSEMODE2D 2 "nonimmediate_operand" "xm"))
	  (parallel [(match_operand 3 "const_0_to_1_operand" "")
		     (match_operand 4 "const_2_to_3_operand" "")])))]
  "TARGET_AVX"
{
  int mask;
  mask = INTVAL (operands[3]);
  mask |= (INTVAL (operands[4]) - 2) << 1;
  operands[3] = GEN_INT (mask);

  return "vshufpd\t{%3, %2, %1, %0|%0, %1, %2, %3}";
}
  [(set_attr "type" "sselog")
   (set_attr "length_immediate" "1")
   (set_attr "prefix" "vex")
   (set_attr "mode" "V2DF")])

(define_insn "sse2_shufpd_<mode>"
  [(set (match_operand:SSEMODE2D 0 "register_operand" "=x")
	(vec_select:SSEMODE2D
	  (vec_concat:<ssedoublesizemode>
	    (match_operand:SSEMODE2D 1 "register_operand" "0")
	    (match_operand:SSEMODE2D 2 "nonimmediate_operand" "xm"))
	  (parallel [(match_operand 3 "const_0_to_1_operand" "")
		     (match_operand 4 "const_2_to_3_operand" "")])))]
  "TARGET_SSE2"
{
  int mask;
  mask = INTVAL (operands[3]);
  mask |= (INTVAL (operands[4]) - 2) << 1;
  operands[3] = GEN_INT (mask);

  return "shufpd\t{%3, %2, %0|%0, %2, %3}";
}
  [(set_attr "type" "sselog")
   (set_attr "length_immediate" "1")
   (set_attr "mode" "V2DF")])

;; Avoid combining registers from different units in a single alternative,
;; see comment above inline_secondary_memory_needed function in i386.c
(define_insn "*avx_storehpd"
  [(set (match_operand:DF 0 "nonimmediate_operand"     "=m,x,x,*f,r")
	(vec_select:DF
	  (match_operand:V2DF 1 "nonimmediate_operand" " x,x,o,o,o")
	  (parallel [(const_int 1)])))]
  "TARGET_AVX && !(MEM_P (operands[0]) && MEM_P (operands[1]))"
  "@
   vmovhpd\t{%1, %0|%0, %1}
   vunpckhpd\t{%1, %1, %0|%0, %1, %1}
   #
   #
   #"
  [(set_attr "type" "ssemov,sselog1,ssemov,fmov,imov")
   (set_attr "prefix" "vex")
   (set_attr "mode" "V1DF,V2DF,DF,DF,DF")])

;; Avoid combining registers from different units in a single alternative,
;; see comment above inline_secondary_memory_needed function in i386.c
(define_insn "sse2_storehpd"
  [(set (match_operand:DF 0 "nonimmediate_operand"     "=m,x,x,*f,r")
	(vec_select:DF
	  (match_operand:V2DF 1 "nonimmediate_operand" " x,0,o,o,o")
	  (parallel [(const_int 1)])))]
  "TARGET_SSE2 && !(MEM_P (operands[0]) && MEM_P (operands[1]))"
  "@
   movhpd\t{%1, %0|%0, %1}
   unpckhpd\t%0, %0
   #
   #
   #"
  [(set_attr "type" "ssemov,sselog1,ssemov,fmov,imov")
   (set_attr "prefix_data16" "1,*,*,*,*")
   (set_attr "mode" "V1DF,V2DF,DF,DF,DF")])

(define_split
  [(set (match_operand:DF 0 "register_operand" "")
	(vec_select:DF
	  (match_operand:V2DF 1 "memory_operand" "")
	  (parallel [(const_int 1)])))]
  "TARGET_SSE2 && reload_completed"
  [(set (match_dup 0) (match_dup 1))]
  "operands[1] = adjust_address (operands[1], DFmode, 8);")

;; Avoid combining registers from different units in a single alternative,
;; see comment above inline_secondary_memory_needed function in i386.c
(define_insn "sse2_storelpd"
  [(set (match_operand:DF 0 "nonimmediate_operand"     "=m,x,x,*f,r")
	(vec_select:DF
	  (match_operand:V2DF 1 "nonimmediate_operand" " x,x,m,m,m")
	  (parallel [(const_int 0)])))]
  "TARGET_SSE2 && !(MEM_P (operands[0]) && MEM_P (operands[1]))"
  "@
   %vmovlpd\t{%1, %0|%0, %1}
   #
   #
   #
   #"
  [(set_attr "type" "ssemov,ssemov,ssemov,fmov,imov")
   (set_attr "prefix_data16" "1,*,*,*,*")
   (set_attr "prefix" "maybe_vex")
   (set_attr "mode" "V1DF,DF,DF,DF,DF")])

(define_split
  [(set (match_operand:DF 0 "register_operand" "")
	(vec_select:DF
	  (match_operand:V2DF 1 "nonimmediate_operand" "")
	  (parallel [(const_int 0)])))]
  "TARGET_SSE2 && reload_completed"
  [(const_int 0)]
{
  rtx op1 = operands[1];
  if (REG_P (op1))
    op1 = gen_rtx_REG (DFmode, REGNO (op1));
  else
    op1 = gen_lowpart (DFmode, op1);
  emit_move_insn (operands[0], op1);
  DONE;
})

(define_expand "sse2_loadhpd_exp"
  [(set (match_operand:V2DF 0 "nonimmediate_operand" "")
	(vec_concat:V2DF
	  (vec_select:DF
	    (match_operand:V2DF 1 "nonimmediate_operand" "")
	    (parallel [(const_int 0)]))
	  (match_operand:DF 2 "nonimmediate_operand" "")))]
  "TARGET_SSE2"
{
  rtx dst = ix86_fixup_binary_operands (UNKNOWN, V2DFmode, operands);
  
  emit_insn (gen_sse2_loadhpd (dst, operands[1], operands[2]));

  /* Fix up the destination if needed.  */
  if (dst != operands[0])
    emit_move_insn (operands[0], dst);

  DONE;
})

;; Avoid combining registers from different units in a single alternative,
;; see comment above inline_secondary_memory_needed function in i386.c
(define_insn "*avx_loadhpd"
  [(set (match_operand:V2DF 0 "nonimmediate_operand"     "=x,x,o,o,o")
	(vec_concat:V2DF
	  (vec_select:DF
	    (match_operand:V2DF 1 "nonimmediate_operand" " x,x,0,0,0")
	    (parallel [(const_int 0)]))
	  (match_operand:DF 2 "nonimmediate_operand"     " m,x,x,*f,r")))]
  "TARGET_AVX && !(MEM_P (operands[1]) && MEM_P (operands[2]))"
  "@
   vmovhpd\t{%2, %1, %0|%0, %1, %2}
   vunpcklpd\t{%2, %1, %0|%0, %1, %2}
   #
   #
   #"
  [(set_attr "type" "ssemov,sselog,ssemov,fmov,imov")
   (set_attr "prefix" "vex")
   (set_attr "mode" "V1DF,V2DF,DF,DF,DF")])

;; Avoid combining registers from different units in a single alternative,
;; see comment above inline_secondary_memory_needed function in i386.c
(define_insn "sse2_loadhpd"
  [(set (match_operand:V2DF 0 "nonimmediate_operand"     "=x,x,o,o,o")
	(vec_concat:V2DF
	  (vec_select:DF
	    (match_operand:V2DF 1 "nonimmediate_operand" " 0,0,0,0,0")
	    (parallel [(const_int 0)]))
	  (match_operand:DF 2 "nonimmediate_operand"     " m,x,x,*f,r")))]
  "TARGET_SSE2 && !(MEM_P (operands[1]) && MEM_P (operands[2]))"
  "@
   movhpd\t{%2, %0|%0, %2}
   unpcklpd\t{%2, %0|%0, %2}
   #
   #
   #"
  [(set_attr "type" "ssemov,sselog,ssemov,fmov,imov")
   (set_attr "prefix_data16" "1,*,*,*,*")
   (set_attr "mode" "V1DF,V2DF,DF,DF,DF")])

(define_split
  [(set (match_operand:V2DF 0 "memory_operand" "")
	(vec_concat:V2DF
	  (vec_select:DF (match_dup 0) (parallel [(const_int 0)]))
	  (match_operand:DF 1 "register_operand" "")))]
  "TARGET_SSE2 && reload_completed"
  [(set (match_dup 0) (match_dup 1))]
  "operands[0] = adjust_address (operands[0], DFmode, 8);")

(define_expand "sse2_loadlpd_exp"
  [(set (match_operand:V2DF 0 "nonimmediate_operand" "")
	(vec_concat:V2DF
	  (match_operand:DF 2 "nonimmediate_operand" "")
	  (vec_select:DF
	    (match_operand:V2DF 1 "nonimmediate_operand" "")
	    (parallel [(const_int 1)]))))]
  "TARGET_SSE2"
{
  rtx dst = ix86_fixup_binary_operands (UNKNOWN, V2DFmode, operands);
  
  emit_insn (gen_sse2_loadlpd (dst, operands[1], operands[2]));

  /* Fix up the destination if needed.  */
  if (dst != operands[0])
    emit_move_insn (operands[0], dst);

  DONE;
})

;; Avoid combining registers from different units in a single alternative,
;; see comment above inline_secondary_memory_needed function in i386.c
(define_insn "*avx_loadlpd"
  [(set (match_operand:V2DF 0 "nonimmediate_operand"    "=x,x,x,x,m,m,m")
	(vec_concat:V2DF
	  (match_operand:DF 2 "nonimmediate_operand"    " m,m,x,x,x,*f,r")
	  (vec_select:DF
	    (match_operand:V2DF 1 "vector_move_operand" " C,x,x,o,0,0,0")
	    (parallel [(const_int 1)]))))]
  "TARGET_AVX && !(MEM_P (operands[1]) && MEM_P (operands[2]))"
  "@
   vmovsd\t{%2, %0|%0, %2}
   vmovlpd\t{%2, %1, %0|%0, %1, %2}
   vmovsd\t{%2, %1, %0|%0, %1, %2}
   vmovhpd\t{%H1, %2, %0|%0, %2, %H1}
   #
   #
   #"
  [(set_attr "type" "ssemov,ssemov,ssemov,ssemov,ssemov,fmov,imov")
   (set_attr "prefix" "vex")
   (set_attr "mode" "DF,V1DF,V1DF,V1DF,DF,DF,DF")])

;; Avoid combining registers from different units in a single alternative,
;; see comment above inline_secondary_memory_needed function in i386.c
(define_insn "sse2_loadlpd"
  [(set (match_operand:V2DF 0 "nonimmediate_operand"    "=x,x,x,x,x,m,m,m")
	(vec_concat:V2DF
	  (match_operand:DF 2 "nonimmediate_operand"    " m,m,x,0,0,x,*f,r")
	  (vec_select:DF
	    (match_operand:V2DF 1 "vector_move_operand" " C,0,0,x,o,0,0,0")
	    (parallel [(const_int 1)]))))]
  "TARGET_SSE2 && !(MEM_P (operands[1]) && MEM_P (operands[2]))"
  "@
   movsd\t{%2, %0|%0, %2}
   movlpd\t{%2, %0|%0, %2}
   movsd\t{%2, %0|%0, %2}
   shufpd\t{$2, %1, %0|%0, %1, 2}
   movhpd\t{%H1, %0|%0, %H1}
   #
   #
   #"
  [(set_attr "type" "ssemov,ssemov,ssemov,sselog,ssemov,ssemov,fmov,imov")
   (set_attr "prefix_data16" "*,1,*,*,1,*,*,*")
   (set_attr "length_immediate" "*,*,*,1,*,*,*,*")
   (set_attr "mode" "DF,V1DF,V1DF,V2DF,V1DF,DF,DF,DF")])

(define_split
  [(set (match_operand:V2DF 0 "memory_operand" "")
	(vec_concat:V2DF
	  (match_operand:DF 1 "register_operand" "")
	  (vec_select:DF (match_dup 0) (parallel [(const_int 1)]))))]
  "TARGET_SSE2 && reload_completed"
  [(set (match_dup 0) (match_dup 1))]
  "operands[0] = adjust_address (operands[0], DFmode, 8);")

;; Not sure these two are ever used, but it doesn't hurt to have
;; them. -aoliva
(define_insn "*vec_extractv2df_1_sse"
  [(set (match_operand:DF 0 "nonimmediate_operand" "=m,x,x")
	(vec_select:DF
	  (match_operand:V2DF 1 "nonimmediate_operand" "x,x,o")
	  (parallel [(const_int 1)])))]
  "!TARGET_SSE2 && TARGET_SSE
   && !(MEM_P (operands[0]) && MEM_P (operands[1]))"
  "@
   movhps\t{%1, %0|%0, %1}
   movhlps\t{%1, %0|%0, %1}
   movlps\t{%H1, %0|%0, %H1}"
  [(set_attr "type" "ssemov")
   (set_attr "mode" "V2SF,V4SF,V2SF")])

(define_insn "*vec_extractv2df_0_sse"
  [(set (match_operand:DF 0 "nonimmediate_operand" "=m,x,x")
	(vec_select:DF
	  (match_operand:V2DF 1 "nonimmediate_operand" "x,x,m")
	  (parallel [(const_int 0)])))]
  "!TARGET_SSE2 && TARGET_SSE
   && !(MEM_P (operands[0]) && MEM_P (operands[1]))"
  "@
   movlps\t{%1, %0|%0, %1}
   movaps\t{%1, %0|%0, %1}
   movlps\t{%1, %0|%0, %1}"
  [(set_attr "type" "ssemov")
   (set_attr "mode" "V2SF,V4SF,V2SF")])

(define_insn "*avx_movsd"
  [(set (match_operand:V2DF 0 "nonimmediate_operand"   "=x,x,m,x,o")
	(vec_merge:V2DF
	  (match_operand:V2DF 2 "nonimmediate_operand" " x,m,x,x,0")
	  (match_operand:V2DF 1 "nonimmediate_operand" " x,x,0,o,x")
	  (const_int 1)))]
  "TARGET_AVX"
  "@
   vmovsd\t{%2, %1, %0|%0, %1, %2}
   vmovlpd\t{%2, %1, %0|%0, %1, %2}
   vmovlpd\t{%2, %0|%0, %2}
   vmovhps\t{%H1, %2, %0|%0, %2, %H1}
   vmovhps\t{%1, %H0|%H0, %1}"
  [(set_attr "type" "ssemov,ssemov,ssemov,ssemov,ssemov")
   (set_attr "prefix" "vex")
   (set_attr "mode" "DF,V1DF,V1DF,V1DF,V1DF")])

(define_insn "sse2_movsd"
  [(set (match_operand:V2DF 0 "nonimmediate_operand"   "=x,x,m,x,x,o")
	(vec_merge:V2DF
	  (match_operand:V2DF 2 "nonimmediate_operand" " x,m,x,0,0,0")
	  (match_operand:V2DF 1 "nonimmediate_operand" " 0,0,0,x,o,x")
	  (const_int 1)))]
  "TARGET_SSE2"
  "@
   movsd\t{%2, %0|%0, %2}
   movlpd\t{%2, %0|%0, %2}
   movlpd\t{%2, %0|%0, %2}
   shufpd\t{$2, %1, %0|%0, %1, 2}
   movhps\t{%H1, %0|%0, %H1}
   movhps\t{%1, %H0|%H0, %1}"
  [(set_attr "type" "ssemov,ssemov,ssemov,sselog,ssemov,ssemov")
   (set_attr "prefix_data16" "*,1,1,*,*,*")
   (set_attr "length_immediate" "*,*,*,1,*,*")
   (set_attr "mode" "DF,V1DF,V1DF,V2DF,V1DF,V1DF")])

(define_expand "vec_dupv2df"
  [(set (match_operand:V2DF 0 "register_operand" "")
	(vec_duplicate:V2DF
	  (match_operand:DF 1 "nonimmediate_operand" "")))]
  "TARGET_SSE2"
{
  if (!TARGET_SSE3)
    operands[1] = force_reg (DFmode, operands[1]);
})

(define_insn "*vec_dupv2df_sse3"
  [(set (match_operand:V2DF 0 "register_operand" "=x")
	(vec_duplicate:V2DF
	  (match_operand:DF 1 "nonimmediate_operand" "xm")))]
  "TARGET_SSE3"
  "%vmovddup\t{%1, %0|%0, %1}"
  [(set_attr "type" "sselog1")
   (set_attr "prefix" "maybe_vex")
   (set_attr "mode" "DF")])

(define_insn "*vec_dupv2df"
  [(set (match_operand:V2DF 0 "register_operand" "=x")
	(vec_duplicate:V2DF
	  (match_operand:DF 1 "register_operand" "0")))]
  "TARGET_SSE2"
  "unpcklpd\t%0, %0"
  [(set_attr "type" "sselog1")
   (set_attr "mode" "V2DF")])

(define_insn "*vec_concatv2df_sse3"
  [(set (match_operand:V2DF 0 "register_operand" "=x")
	(vec_concat:V2DF
	  (match_operand:DF 1 "nonimmediate_operand" "xm")
	  (match_dup 1)))]
  "TARGET_SSE3"
  "%vmovddup\t{%1, %0|%0, %1}"
  [(set_attr "type" "sselog1")
   (set_attr "prefix" "maybe_vex")
   (set_attr "mode" "DF")])

(define_insn "*vec_concatv2df_avx"
  [(set (match_operand:V2DF 0 "register_operand"     "=x,x,x")
	(vec_concat:V2DF
	  (match_operand:DF 1 "nonimmediate_operand" " x,x,m")
	  (match_operand:DF 2 "vector_move_operand"  " x,m,C")))]
  "TARGET_AVX"
  "@
   vunpcklpd\t{%2, %1, %0|%0, %1, %2}
   vmovhpd\t{%2, %1, %0|%0, %1, %2}
   vmovsd\t{%1, %0|%0, %1}"
  [(set_attr "type" "ssemov")
   (set_attr "prefix" "vex")
   (set_attr "mode" "DF,V1DF,DF")])

(define_insn "*vec_concatv2df"
  [(set (match_operand:V2DF 0 "register_operand"     "=Y2,Y2,Y2,x,x")
	(vec_concat:V2DF
	  (match_operand:DF 1 "nonimmediate_operand" " 0 ,0 ,m ,0,0")
	  (match_operand:DF 2 "vector_move_operand"  " Y2,m ,C ,x,m")))]
  "TARGET_SSE"
  "@
   unpcklpd\t{%2, %0|%0, %2}
   movhpd\t{%2, %0|%0, %2}
   movsd\t{%1, %0|%0, %1}
   movlhps\t{%2, %0|%0, %2}
   movhps\t{%2, %0|%0, %2}"
  [(set_attr "type" "sselog,ssemov,ssemov,ssemov,ssemov")
   (set_attr "prefix_data16" "*,1,*,*,*")
   (set_attr "mode" "V2DF,V1DF,DF,V4SF,V2SF")])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Parallel integral arithmetic
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define_expand "neg<mode>2"
  [(set (match_operand:SSEMODEI 0 "register_operand" "")
	(minus:SSEMODEI
	  (match_dup 2)
	  (match_operand:SSEMODEI 1 "nonimmediate_operand" "")))]
  "TARGET_SSE2"
  "operands[2] = force_reg (<MODE>mode, CONST0_RTX (<MODE>mode));")

(define_expand "<plusminus_insn><mode>3"
  [(set (match_operand:SSEMODEI 0 "register_operand" "")
	(plusminus:SSEMODEI
	  (match_operand:SSEMODEI 1 "nonimmediate_operand" "")
	  (match_operand:SSEMODEI 2 "nonimmediate_operand" "")))]
  "TARGET_SSE2"
  "ix86_fixup_binary_operands_no_copy (<CODE>, <MODE>mode, operands);")

(define_insn "*avx_<plusminus_insn><mode>3"
  [(set (match_operand:SSEMODEI 0 "register_operand" "=x")
	(plusminus:SSEMODEI
	  (match_operand:SSEMODEI 1 "nonimmediate_operand" "<comm>x")
	  (match_operand:SSEMODEI 2 "nonimmediate_operand" "xm")))]
  "TARGET_AVX && ix86_binary_operator_ok (<CODE>, <MODE>mode, operands)"
  "vp<plusminus_mnemonic><ssevecsize>\t{%2, %1, %0|%0, %1, %2}"
  [(set_attr "type" "sseiadd")
   (set_attr "prefix" "vex")
   (set_attr "mode" "TI")])

(define_insn "*<plusminus_insn><mode>3"
  [(set (match_operand:SSEMODEI 0 "register_operand" "=x")
	(plusminus:SSEMODEI
	  (match_operand:SSEMODEI 1 "nonimmediate_operand" "<comm>0")
	  (match_operand:SSEMODEI 2 "nonimmediate_operand" "xm")))]
  "TARGET_SSE2 && ix86_binary_operator_ok (<CODE>, <MODE>mode, operands)"
  "p<plusminus_mnemonic><ssevecsize>\t{%2, %0|%0, %2}"
  [(set_attr "type" "sseiadd")
   (set_attr "prefix_data16" "1")
   (set_attr "mode" "TI")])

(define_expand "sse2_<plusminus_insn><mode>3"
  [(set (match_operand:SSEMODE12 0 "register_operand" "")
	(sat_plusminus:SSEMODE12
	  (match_operand:SSEMODE12 1 "nonimmediate_operand" "")
	  (match_operand:SSEMODE12 2 "nonimmediate_operand" "")))]
  "TARGET_SSE2"
  "ix86_fixup_binary_operands_no_copy (<CODE>, <MODE>mode, operands);")

(define_insn "*avx_<plusminus_insn><mode>3"
  [(set (match_operand:SSEMODE12 0 "register_operand" "=x")
	(sat_plusminus:SSEMODE12
	  (match_operand:SSEMODE12 1 "nonimmediate_operand" "<comm>x")
	  (match_operand:SSEMODE12 2 "nonimmediate_operand" "xm")))]
  "TARGET_AVX && ix86_binary_operator_ok (<CODE>, <MODE>mode, operands)"
  "vp<plusminus_mnemonic><ssevecsize>\t{%2, %1, %0|%0, %1, %2}"
  [(set_attr "type" "sseiadd")
   (set_attr "prefix" "vex")
   (set_attr "mode" "TI")])

(define_insn "*sse2_<plusminus_insn><mode>3"
  [(set (match_operand:SSEMODE12 0 "register_operand" "=x")
	(sat_plusminus:SSEMODE12
	  (match_operand:SSEMODE12 1 "nonimmediate_operand" "<comm>0")
	  (match_operand:SSEMODE12 2 "nonimmediate_operand" "xm")))]
  "TARGET_SSE2 && ix86_binary_operator_ok (<CODE>, <MODE>mode, operands)"
  "p<plusminus_mnemonic><ssevecsize>\t{%2, %0|%0, %2}"
  [(set_attr "type" "sseiadd")
   (set_attr "prefix_data16" "1")
   (set_attr "mode" "TI")])

(define_insn_and_split "mulv16qi3"
  [(set (match_operand:V16QI 0 "register_operand" "")
	(mult:V16QI (match_operand:V16QI 1 "register_operand" "")
		    (match_operand:V16QI 2 "register_operand" "")))]
  "TARGET_SSE2
   && can_create_pseudo_p ()"
  "#"
  "&& 1"
  [(const_int 0)]
{
  rtx t[6];
  int i;

  for (i = 0; i < 6; ++i)
    t[i] = gen_reg_rtx (V16QImode);

  /* Unpack data such that we've got a source byte in each low byte of
     each word.  We don't care what goes into the high byte of each word.
     Rather than trying to get zero in there, most convenient is to let
     it be a copy of the low byte.  */
  emit_insn (gen_vec_interleave_highv16qi (t[0], operands[1], operands[1]));
  emit_insn (gen_vec_interleave_highv16qi (t[1], operands[2], operands[2]));
  emit_insn (gen_vec_interleave_lowv16qi (t[2], operands[1], operands[1]));
  emit_insn (gen_vec_interleave_lowv16qi (t[3], operands[2], operands[2]));

  /* Multiply words.  The end-of-line annotations here give a picture of what
     the output of that instruction looks like.  Dot means don't care; the
     letters are the bytes of the result with A being the most significant.  */
  emit_insn (gen_mulv8hi3 (gen_lowpart (V8HImode, t[4]), /* .A.B.C.D.E.F.G.H */
			   gen_lowpart (V8HImode, t[0]),
			   gen_lowpart (V8HImode, t[1])));
  emit_insn (gen_mulv8hi3 (gen_lowpart (V8HImode, t[5]), /* .I.J.K.L.M.N.O.P */
			   gen_lowpart (V8HImode, t[2]),
			   gen_lowpart (V8HImode, t[3])));

  /* Extract the even bytes and merge them back together.  */
  ix86_expand_vec_extract_even_odd (operands[0], t[5], t[4], 0);
  DONE;
})

(define_expand "mulv8hi3"
  [(set (match_operand:V8HI 0 "register_operand" "")
	(mult:V8HI (match_operand:V8HI 1 "nonimmediate_operand" "")
		   (match_operand:V8HI 2 "nonimmediate_operand" "")))]
  "TARGET_SSE2"
  "ix86_fixup_binary_operands_no_copy (MULT, V8HImode, operands);")

(define_insn "*avx_mulv8hi3"
  [(set (match_operand:V8HI 0 "register_operand" "=x")
	(mult:V8HI (match_operand:V8HI 1 "nonimmediate_operand" "%x")
		   (match_operand:V8HI 2 "nonimmediate_operand" "xm")))]
  "TARGET_AVX && ix86_binary_operator_ok (MULT, V8HImode, operands)"
  "vpmullw\t{%2, %1, %0|%0, %1, %2}"
  [(set_attr "type" "sseimul")
   (set_attr "prefix" "vex")
   (set_attr "mode" "TI")])

(define_insn "*mulv8hi3"
  [(set (match_operand:V8HI 0 "register_operand" "=x")
	(mult:V8HI (match_operand:V8HI 1 "nonimmediate_operand" "%0")
		   (match_operand:V8HI 2 "nonimmediate_operand" "xm")))]
  "TARGET_SSE2 && ix86_binary_operator_ok (MULT, V8HImode, operands)"
  "pmullw\t{%2, %0|%0, %2}"
  [(set_attr "type" "sseimul")
   (set_attr "prefix_data16" "1")
   (set_attr "mode" "TI")])

(define_expand "<s>mulv8hi3_highpart"
  [(set (match_operand:V8HI 0 "register_operand" "")
        (truncate:V8HI
          (lshiftrt:V8SI
            (mult:V8SI
              (any_extend:V8SI
                (match_operand:V8HI 1 "nonimmediate_operand" ""))
              (any_extend:V8SI
                (match_operand:V8HI 2 "nonimmediate_operand" "")))
            (const_int 16))))]
  "TARGET_SSE2"
  "ix86_fixup_binary_operands_no_copy (MULT, V8HImode, operands);")

(define_insn "*avx_<s>mulv8hi3_highpart"
  [(set (match_operand:V8HI 0 "register_operand" "=x")
	(truncate:V8HI
	  (lshiftrt:V8SI
	    (mult:V8SI
	      (any_extend:V8SI
		(match_operand:V8HI 1 "nonimmediate_operand" "%x"))
	      (any_extend:V8SI
		(match_operand:V8HI 2 "nonimmediate_operand" "xm")))
	    (const_int 16))))]
  "TARGET_AVX && ix86_binary_operator_ok (MULT, V8HImode, operands)"
  "vpmulh<u>w\t{%2, %1, %0|%0, %1, %2}"
  [(set_attr "type" "sseimul")
   (set_attr "prefix" "vex")
   (set_attr "mode" "TI")])

(define_insn "*<s>mulv8hi3_highpart"
  [(set (match_operand:V8HI 0 "register_operand" "=x")
	(truncate:V8HI
	  (lshiftrt:V8SI
	    (mult:V8SI
	      (any_extend:V8SI
		(match_operand:V8HI 1 "nonimmediate_operand" "%0"))
	      (any_extend:V8SI
		(match_operand:V8HI 2 "nonimmediate_operand" "xm")))
	    (const_int 16))))]
  "TARGET_SSE2 && ix86_binary_operator_ok (MULT, V8HImode, operands)"
  "pmulh<u>w\t{%2, %0|%0, %2}"
  [(set_attr "type" "sseimul")
   (set_attr "prefix_data16" "1")
   (set_attr "mode" "TI")])

(define_expand "sse2_umulv2siv2di3"
  [(set (match_operand:V2DI 0 "register_operand" "")
	(mult:V2DI
	  (zero_extend:V2DI
	    (vec_select:V2SI
	      (match_operand:V4SI 1 "nonimmediate_operand" "")
	      (parallel [(const_int 0) (const_int 2)])))
	  (zero_extend:V2DI
	    (vec_select:V2SI
	      (match_operand:V4SI 2 "nonimmediate_operand" "")
	      (parallel [(const_int 0) (const_int 2)])))))]
  "TARGET_SSE2"
  "ix86_fixup_binary_operands_no_copy (MULT, V4SImode, operands);")

(define_insn "*avx_umulv2siv2di3"
  [(set (match_operand:V2DI 0 "register_operand" "=x")
	(mult:V2DI
	  (zero_extend:V2DI
	    (vec_select:V2SI
	      (match_operand:V4SI 1 "nonimmediate_operand" "%x")
	      (parallel [(const_int 0) (const_int 2)])))
	  (zero_extend:V2DI
	    (vec_select:V2SI
	      (match_operand:V4SI 2 "nonimmediate_operand" "xm")
	      (parallel [(const_int 0) (const_int 2)])))))]
  "TARGET_AVX && ix86_binary_operator_ok (MULT, V4SImode, operands)"
  "vpmuludq\t{%2, %1, %0|%0, %1, %2}"
  [(set_attr "type" "sseimul")
   (set_attr "prefix" "vex")
   (set_attr "mode" "TI")])

(define_insn "*sse2_umulv2siv2di3"
  [(set (match_operand:V2DI 0 "register_operand" "=x")
	(mult:V2DI
	  (zero_extend:V2DI
	    (vec_select:V2SI
	      (match_operand:V4SI 1 "nonimmediate_operand" "%0")
	      (parallel [(const_int 0) (const_int 2)])))
	  (zero_extend:V2DI
	    (vec_select:V2SI
	      (match_operand:V4SI 2 "nonimmediate_operand" "xm")
	      (parallel [(const_int 0) (const_int 2)])))))]
  "TARGET_SSE2 && ix86_binary_operator_ok (MULT, V4SImode, operands)"
  "pmuludq\t{%2, %0|%0, %2}"
  [(set_attr "type" "sseimul")
   (set_attr "prefix_data16" "1")
   (set_attr "mode" "TI")])

(define_expand "sse4_1_mulv2siv2di3"
  [(set (match_operand:V2DI 0 "register_operand" "")
	(mult:V2DI
	  (sign_extend:V2DI
	    (vec_select:V2SI
	      (match_operand:V4SI 1 "nonimmediate_operand" "")
	      (parallel [(const_int 0) (const_int 2)])))
	  (sign_extend:V2DI
	    (vec_select:V2SI
	      (match_operand:V4SI 2 "nonimmediate_operand" "")
	      (parallel [(const_int 0) (const_int 2)])))))]
  "TARGET_SSE4_1"
  "ix86_fixup_binary_operands_no_copy (MULT, V4SImode, operands);")

(define_insn "*avx_mulv2siv2di3"
  [(set (match_operand:V2DI 0 "register_operand" "=x")
	(mult:V2DI
	  (sign_extend:V2DI
	    (vec_select:V2SI
	      (match_operand:V4SI 1 "nonimmediate_operand" "%x")
	      (parallel [(const_int 0) (const_int 2)])))
	  (sign_extend:V2DI
	    (vec_select:V2SI
	      (match_operand:V4SI 2 "nonimmediate_operand" "xm")
	      (parallel [(const_int 0) (const_int 2)])))))]
  "TARGET_AVX && ix86_binary_operator_ok (MULT, V4SImode, operands)"
  "vpmuldq\t{%2, %1, %0|%0, %1, %2}"
  [(set_attr "type" "sseimul")
   (set_attr "prefix_extra" "1")
   (set_attr "prefix" "vex")
   (set_attr "mode" "TI")])

(define_insn "*sse4_1_mulv2siv2di3"
  [(set (match_operand:V2DI 0 "register_operand" "=x")
	(mult:V2DI
	  (sign_extend:V2DI
	    (vec_select:V2SI
	      (match_operand:V4SI 1 "nonimmediate_operand" "%0")
	      (parallel [(const_int 0) (const_int 2)])))
	  (sign_extend:V2DI
	    (vec_select:V2SI
	      (match_operand:V4SI 2 "nonimmediate_operand" "xm")
	      (parallel [(const_int 0) (const_int 2)])))))]
  "TARGET_SSE4_1 && ix86_binary_operator_ok (MULT, V4SImode, operands)"
  "pmuldq\t{%2, %0|%0, %2}"
  [(set_attr "type" "sseimul")
   (set_attr "prefix_extra" "1")
   (set_attr "mode" "TI")])

(define_expand "sse2_pmaddwd"
  [(set (match_operand:V4SI 0 "register_operand" "")
	(plus:V4SI
	  (mult:V4SI
	    (sign_extend:V4SI
	      (vec_select:V4HI
		(match_operand:V8HI 1 "nonimmediate_operand" "")
		(parallel [(const_int 0)
			   (const_int 2)
			   (const_int 4)
			   (const_int 6)])))
	    (sign_extend:V4SI
	      (vec_select:V4HI
		(match_operand:V8HI 2 "nonimmediate_operand" "")
		(parallel [(const_int 0)
			   (const_int 2)
			   (const_int 4)
			   (const_int 6)]))))
	  (mult:V4SI
	    (sign_extend:V4SI
	      (vec_select:V4HI (match_dup 1)
		(parallel [(const_int 1)
			   (const_int 3)
			   (const_int 5)
			   (const_int 7)])))
	    (sign_extend:V4SI
	      (vec_select:V4HI (match_dup 2)
		(parallel [(const_int 1)
			   (const_int 3)
			   (const_int 5)
			   (const_int 7)]))))))]
  "TARGET_SSE2"
  "ix86_fixup_binary_operands_no_copy (MULT, V8HImode, operands);")

(define_insn "*avx_pmaddwd"
  [(set (match_operand:V4SI 0 "register_operand" "=x")
	(plus:V4SI
	  (mult:V4SI
	    (sign_extend:V4SI
	      (vec_select:V4HI
		(match_operand:V8HI 1 "nonimmediate_operand" "%x")
		(parallel [(const_int 0)
			   (const_int 2)
			   (const_int 4)
			   (const_int 6)])))
	    (sign_extend:V4SI
	      (vec_select:V4HI
		(match_operand:V8HI 2 "nonimmediate_operand" "xm")
		(parallel [(const_int 0)
			   (const_int 2)
			   (const_int 4)
			   (const_int 6)]))))
	  (mult:V4SI
	    (sign_extend:V4SI
	      (vec_select:V4HI (match_dup 1)
		(parallel [(const_int 1)
			   (const_int 3)
			   (const_int 5)
			   (const_int 7)])))
	    (sign_extend:V4SI
	      (vec_select:V4HI (match_dup 2)
		(parallel [(const_int 1)
			   (const_int 3)
			   (const_int 5)
			   (const_int 7)]))))))]
  "TARGET_AVX && ix86_binary_operator_ok (MULT, V8HImode, operands)"
  "vpmaddwd\t{%2, %1, %0|%0, %1, %2}"
  [(set_attr "type" "sseiadd")
   (set_attr "prefix" "vex")
   (set_attr "mode" "TI")])

(define_insn "*sse2_pmaddwd"
  [(set (match_operand:V4SI 0 "register_operand" "=x")
	(plus:V4SI
	  (mult:V4SI
	    (sign_extend:V4SI
	      (vec_select:V4HI
		(match_operand:V8HI 1 "nonimmediate_operand" "%0")
		(parallel [(const_int 0)
			   (const_int 2)
			   (const_int 4)
			   (const_int 6)])))
	    (sign_extend:V4SI
	      (vec_select:V4HI
		(match_operand:V8HI 2 "nonimmediate_operand" "xm")
		(parallel [(const_int 0)
			   (const_int 2)
			   (const_int 4)
			   (const_int 6)]))))
	  (mult:V4SI
	    (sign_extend:V4SI
	      (vec_select:V4HI (match_dup 1)
		(parallel [(const_int 1)
			   (const_int 3)
			   (const_int 5)
			   (const_int 7)])))
	    (sign_extend:V4SI
	      (vec_select:V4HI (match_dup 2)
		(parallel [(const_int 1)
			   (const_int 3)
			   (const_int 5)
			   (const_int 7)]))))))]
  "TARGET_SSE2 && ix86_binary_operator_ok (MULT, V8HImode, operands)"
  "pmaddwd\t{%2, %0|%0, %2}"
  [(set_attr "type" "sseiadd")
   (set_attr "atom_unit" "simul")
   (set_attr "prefix_data16" "1")
   (set_attr "mode" "TI")])

(define_expand "mulv4si3"
  [(set (match_operand:V4SI 0 "register_operand" "")
	(mult:V4SI (match_operand:V4SI 1 "register_operand" "")
		   (match_operand:V4SI 2 "register_operand" "")))]
  "TARGET_SSE2"
{
  if (TARGET_SSE4_1 || TARGET_AVX)
    ix86_fixup_binary_operands_no_copy (MULT, V4SImode, operands);
})

(define_insn "*avx_mulv4si3"
  [(set (match_operand:V4SI 0 "register_operand" "=x")
	(mult:V4SI (match_operand:V4SI 1 "nonimmediate_operand" "%x")
		   (match_operand:V4SI 2 "nonimmediate_operand" "xm")))]
  "TARGET_AVX && ix86_binary_operator_ok (MULT, V4SImode, operands)"
  "vpmulld\t{%2, %1, %0|%0, %1, %2}"
  [(set_attr "type" "sseimul")
   (set_attr "prefix_extra" "1")
   (set_attr "prefix" "vex")
   (set_attr "mode" "TI")])

(define_insn "*sse4_1_mulv4si3"
  [(set (match_operand:V4SI 0 "register_operand" "=x")
	(mult:V4SI (match_operand:V4SI 1 "nonimmediate_operand" "%0")
		   (match_operand:V4SI 2 "nonimmediate_operand" "xm")))]
  "TARGET_SSE4_1 && ix86_binary_operator_ok (MULT, V4SImode, operands)"
  "pmulld\t{%2, %0|%0, %2}"
  [(set_attr "type" "sseimul")
   (set_attr "prefix_extra" "1")
   (set_attr "mode" "TI")])

(define_insn_and_split "*sse2_mulv4si3"
  [(set (match_operand:V4SI 0 "register_operand" "")
	(mult:V4SI (match_operand:V4SI 1 "register_operand" "")
		   (match_operand:V4SI 2 "register_operand" "")))]
  "TARGET_SSE2 && !TARGET_SSE4_1 && !TARGET_AVX
   && can_create_pseudo_p ()"
  "#"
  "&& 1"
  [(const_int 0)]
{
  rtx t1, t2, t3, t4, t5, t6, thirtytwo;
  rtx op0, op1, op2;

  op0 = operands[0];
  op1 = operands[1];
  op2 = operands[2];
  t1 = gen_reg_rtx (V4SImode);
  t2 = gen_reg_rtx (V4SImode);
  t3 = gen_reg_rtx (V4SImode);
  t4 = gen_reg_rtx (V4SImode);
  t5 = gen_reg_rtx (V4SImode);
  t6 = gen_reg_rtx (V4SImode);
  thirtytwo = GEN_INT (32);

  /* Multiply elements 2 and 0.  */
  emit_insn (gen_sse2_umulv2siv2di3 (gen_lowpart (V2DImode, t1),
				     op1, op2));

  /* Shift both input vectors down one element, so that elements 3
     and 1 are now in the slots for elements 2 and 0.  For K8, at
     least, this is faster than using a shuffle.  */
  emit_insn (gen_sse2_lshrv1ti3 (gen_lowpart (V1TImode, t2),
				 gen_lowpart (V1TImode, op1),
				 thirtytwo));
  emit_insn (gen_sse2_lshrv1ti3 (gen_lowpart (V1TImode, t3),
				 gen_lowpart (V1TImode, op2),
				 thirtytwo));
  /* Multiply elements 3 and 1.  */
  emit_insn (gen_sse2_umulv2siv2di3 (gen_lowpart (V2DImode, t4),
				     t2, t3));

  /* Move the results in element 2 down to element 1; we don't care
     what goes in elements 2 and 3.  */
  emit_insn (gen_sse2_pshufd_1 (t5, t1, const0_rtx, const2_rtx,
				const0_rtx, const0_rtx));
  emit_insn (gen_sse2_pshufd_1 (t6, t4, const0_rtx, const2_rtx,
				const0_rtx, const0_rtx));

  /* Merge the parts back together.  */
  emit_insn (gen_vec_interleave_lowv4si (op0, t5, t6));
  DONE;
})

(define_insn_and_split "mulv2di3"
  [(set (match_operand:V2DI 0 "register_operand" "")
	(mult:V2DI (match_operand:V2DI 1 "register_operand" "")
		   (match_operand:V2DI 2 "register_operand" "")))]
  "TARGET_SSE2
   && can_create_pseudo_p ()"
  "#"
  "&& 1"
  [(const_int 0)]
{
  rtx t1, t2, t3, t4, t5, t6, thirtytwo;
  rtx op0, op1, op2;

  op0 = operands[0];
  op1 = operands[1];
  op2 = operands[2];

  if (TARGET_XOP)
    {
      /* op1: A,B,C,D, op2: E,F,G,H */
      op1 = gen_lowpart (V4SImode, op1);
      op2 = gen_lowpart (V4SImode, op2);

      t1 = gen_reg_rtx (V4SImode);
      t2 = gen_reg_rtx (V4SImode);
      t3 = gen_reg_rtx (V2DImode);
      t4 = gen_reg_rtx (V2DImode);

      /* t1: B,A,D,C */
      emit_insn (gen_sse2_pshufd_1 (t1, op1,
				    GEN_INT (1),
				    GEN_INT (0),
				    GEN_INT (3),
				    GEN_INT (2)));

      /* t2: (B*E),(A*F),(D*G),(C*H) */
      emit_insn (gen_mulv4si3 (t2, t1, op2));

      /* t4: (B*E)+(A*F), (D*G)+(C*H) */
      emit_insn (gen_xop_phadddq (t3, t2));

      /* t5: ((B*E)+(A*F))<<32, ((D*G)+(C*H))<<32 */
      emit_insn (gen_ashlv2di3 (t4, t3, GEN_INT (32)));

      /* op0: (((B*E)+(A*F))<<32)+(B*F), (((D*G)+(C*H))<<32)+(D*H) */
      emit_insn (gen_xop_pmacsdql (op0, op1, op2, t4));
    }
  else
    {
      t1 = gen_reg_rtx (V2DImode);
      t2 = gen_reg_rtx (V2DImode);
      t3 = gen_reg_rtx (V2DImode);
      t4 = gen_reg_rtx (V2DImode);
      t5 = gen_reg_rtx (V2DImode);
      t6 = gen_reg_rtx (V2DImode);
      thirtytwo = GEN_INT (32);

      /* Multiply low parts.  */
      emit_insn (gen_sse2_umulv2siv2di3 (t1, gen_lowpart (V4SImode, op1),
				         gen_lowpart (V4SImode, op2)));

      /* Shift input vectors left 32 bits so we can multiply high parts.  */
      emit_insn (gen_lshrv2di3 (t2, op1, thirtytwo));
      emit_insn (gen_lshrv2di3 (t3, op2, thirtytwo));

      /* Multiply high parts by low parts.  */
      emit_insn (gen_sse2_umulv2siv2di3 (t4, gen_lowpart (V4SImode, op1),
					 gen_lowpart (V4SImode, t3)));
      emit_insn (gen_sse2_umulv2siv2di3 (t5, gen_lowpart (V4SImode, op2),
					 gen_lowpart (V4SImode, t2)));

      /* Shift them back.  */
      emit_insn (gen_ashlv2di3 (t4, t4, thirtytwo));
      emit_insn (gen_ashlv2di3 (t5, t5, thirtytwo));

      /* Add the three parts together.  */
      emit_insn (gen_addv2di3 (t6, t1, t4));
      emit_insn (gen_addv2di3 (op0, t6, t5));
    }
  DONE;
})

(define_expand "vec_widen_smult_hi_v8hi"
  [(match_operand:V4SI 0 "register_operand" "")
   (match_operand:V8HI 1 "register_operand" "")
   (match_operand:V8HI 2 "register_operand" "")]
  "TARGET_SSE2"
{
  rtx op1, op2, t1, t2, dest;

  op1 = operands[1];
  op2 = operands[2];
  t1 = gen_reg_rtx (V8HImode);
  t2 = gen_reg_rtx (V8HImode);
  dest = gen_lowpart (V8HImode, operands[0]);

  emit_insn (gen_mulv8hi3 (t1, op1, op2));
  emit_insn (gen_smulv8hi3_highpart (t2, op1, op2));
  emit_insn (gen_vec_interleave_highv8hi (dest, t1, t2));
  DONE;
})

(define_expand "vec_widen_smult_lo_v8hi"
  [(match_operand:V4SI 0 "register_operand" "")
   (match_operand:V8HI 1 "register_operand" "")
   (match_operand:V8HI 2 "register_operand" "")]
  "TARGET_SSE2"
{
  rtx op1, op2, t1, t2, dest;

  op1 = operands[1];
  op2 = operands[2];
  t1 = gen_reg_rtx (V8HImode);
  t2 = gen_reg_rtx (V8HImode);
  dest = gen_lowpart (V8HImode, operands[0]);

  emit_insn (gen_mulv8hi3 (t1, op1, op2));
  emit_insn (gen_smulv8hi3_highpart (t2, op1, op2));
  emit_insn (gen_vec_interleave_lowv8hi (dest, t1, t2));
  DONE;
})

(define_expand "vec_widen_umult_hi_v8hi"
  [(match_operand:V4SI 0 "register_operand" "")
   (match_operand:V8HI 1 "register_operand" "")
   (match_operand:V8HI 2 "register_operand" "")]
  "TARGET_SSE2"
{
  rtx op1, op2, t1, t2, dest;

  op1 = operands[1];
  op2 = operands[2];
  t1 = gen_reg_rtx (V8HImode);
  t2 = gen_reg_rtx (V8HImode);
  dest = gen_lowpart (V8HImode, operands[0]);

  emit_insn (gen_mulv8hi3 (t1, op1, op2));
  emit_insn (gen_umulv8hi3_highpart (t2, op1, op2));
  emit_insn (gen_vec_interleave_highv8hi (dest, t1, t2));
  DONE;
})

(define_expand "vec_widen_umult_lo_v8hi"
  [(match_operand:V4SI 0 "register_operand" "")
   (match_operand:V8HI 1 "register_operand" "")
   (match_operand:V8HI 2 "register_operand" "")]
  "TARGET_SSE2"
{
  rtx op1, op2, t1, t2, dest;

  op1 = operands[1];
  op2 = operands[2];
  t1 = gen_reg_rtx (V8HImode);
  t2 = gen_reg_rtx (V8HImode);
  dest = gen_lowpart (V8HImode, operands[0]);

  emit_insn (gen_mulv8hi3 (t1, op1, op2));
  emit_insn (gen_umulv8hi3_highpart (t2, op1, op2));
  emit_insn (gen_vec_interleave_lowv8hi (dest, t1, t2));
  DONE;
})

(define_expand "vec_widen_smult_hi_v4si"
  [(match_operand:V2DI 0 "register_operand" "")
   (match_operand:V4SI 1 "register_operand" "")
   (match_operand:V4SI 2 "register_operand" "")]
  "TARGET_XOP"
{
  rtx t1, t2;

  t1 = gen_reg_rtx (V4SImode);
  t2 = gen_reg_rtx (V4SImode);

  emit_insn (gen_sse2_pshufd_1 (t1, operands[1],
				GEN_INT (0),
				GEN_INT (2),
				GEN_INT (1),
				GEN_INT (3)));
  emit_insn (gen_sse2_pshufd_1 (t2, operands[2],
				GEN_INT (0),
				GEN_INT (2),
				GEN_INT (1),
				GEN_INT (3)));
  emit_insn (gen_xop_mulv2div2di3_high (operands[0], t1, t2));
  DONE;
})

(define_expand "vec_widen_smult_lo_v4si"
  [(match_operand:V2DI 0 "register_operand" "")
   (match_operand:V4SI 1 "register_operand" "")
   (match_operand:V4SI 2 "register_operand" "")]
  "TARGET_XOP"
{
  rtx t1, t2;

  t1 = gen_reg_rtx (V4SImode);
  t2 = gen_reg_rtx (V4SImode);

  emit_insn (gen_sse2_pshufd_1 (t1, operands[1],
				GEN_INT (0),
				GEN_INT (2),
				GEN_INT (1),
				GEN_INT (3)));
  emit_insn (gen_sse2_pshufd_1 (t2, operands[2],
				GEN_INT (0),
				GEN_INT (2),
				GEN_INT (1),
				GEN_INT (3)));
  emit_insn (gen_xop_mulv2div2di3_low (operands[0], t1, t2));
  DONE;
})

(define_expand "vec_widen_umult_hi_v4si"
  [(match_operand:V2DI 0 "register_operand" "")
   (match_operand:V4SI 1 "register_operand" "")
   (match_operand:V4SI 2 "register_operand" "")]
  "TARGET_SSE2"
{
  rtx op1, op2, t1, t2;

  op1 = operands[1];
  op2 = operands[2];
  t1 = gen_reg_rtx (V4SImode);
  t2 = gen_reg_rtx (V4SImode);

  emit_insn (gen_vec_interleave_highv4si (t1, op1, op1));
  emit_insn (gen_vec_interleave_highv4si (t2, op2, op2));
  emit_insn (gen_sse2_umulv2siv2di3 (operands[0], t1, t2));
  DONE;
})

(define_expand "vec_widen_umult_lo_v4si"
  [(match_operand:V2DI 0 "register_operand" "")
   (match_operand:V4SI 1 "register_operand" "")
   (match_operand:V4SI 2 "register_operand" "")]
  "TARGET_SSE2"
{
  rtx op1, op2, t1, t2;

  op1 = operands[1];
  op2 = operands[2];
  t1 = gen_reg_rtx (V4SImode);
  t2 = gen_reg_rtx (V4SImode);

  emit_insn (gen_vec_interleave_lowv4si (t1, op1, op1));
  emit_insn (gen_vec_interleave_lowv4si (t2, op2, op2));
  emit_insn (gen_sse2_umulv2siv2di3 (operands[0], t1, t2));
  DONE;
})

(define_expand "sdot_prodv8hi"
  [(match_operand:V4SI 0 "register_operand" "")
   (match_operand:V8HI 1 "register_operand" "")
   (match_operand:V8HI 2 "register_operand" "")
   (match_operand:V4SI 3 "register_operand" "")]
  "TARGET_SSE2"
{
  rtx t = gen_reg_rtx (V4SImode);
  emit_insn (gen_sse2_pmaddwd (t, operands[1], operands[2]));
  emit_insn (gen_addv4si3 (operands[0], operands[3], t));
  DONE;
})

(define_expand "udot_prodv4si"
  [(match_operand:V2DI 0 "register_operand" "")
   (match_operand:V4SI 1 "register_operand" "")
   (match_operand:V4SI 2 "register_operand" "")
   (match_operand:V2DI 3 "register_operand" "")]
  "TARGET_SSE2"
{
  rtx t1, t2, t3, t4;

  t1 = gen_reg_rtx (V2DImode);
  emit_insn (gen_sse2_umulv2siv2di3 (t1, operands[1], operands[2]));
  emit_insn (gen_addv2di3 (t1, t1, operands[3]));

  t2 = gen_reg_rtx (V4SImode);
  t3 = gen_reg_rtx (V4SImode);
  emit_insn (gen_sse2_lshrv1ti3 (gen_lowpart (V1TImode, t2),
				 gen_lowpart (V1TImode, operands[1]),
				 GEN_INT (32)));
  emit_insn (gen_sse2_lshrv1ti3 (gen_lowpart (V1TImode, t3),
				 gen_lowpart (V1TImode, operands[2]),
				 GEN_INT (32)));

  t4 = gen_reg_rtx (V2DImode);
  emit_insn (gen_sse2_umulv2siv2di3 (t4, t2, t3));

  emit_insn (gen_addv2di3 (operands[0], t1, t4));
  DONE;
})

(define_insn "*avx_ashr<mode>3"
  [(set (match_operand:SSEMODE24 0 "register_operand" "=x")
	(ashiftrt:SSEMODE24
	  (match_operand:SSEMODE24 1 "register_operand" "x")
	  (match_operand:SI 2 "nonmemory_operand" "xN")))]
  "TARGET_AVX"
  "vpsra<ssevecsize>\t{%2, %1, %0|%0, %1, %2}"
  [(set_attr "type" "sseishft")
   (set_attr "prefix" "vex")
   (set (attr "length_immediate")
     (if_then_else (match_operand 2 "const_int_operand" "")
       (const_string "1")
       (const_string "0")))
   (set_attr "mode" "TI")])

(define_insn "ashr<mode>3"
  [(set (match_operand:SSEMODE24 0 "register_operand" "=x")
	(ashiftrt:SSEMODE24
	  (match_operand:SSEMODE24 1 "register_operand" "0")
	  (match_operand:SI 2 "nonmemory_operand" "xN")))]
  "TARGET_SSE2"
  "psra<ssevecsize>\t{%2, %0|%0, %2}"
  [(set_attr "type" "sseishft")
   (set_attr "prefix_data16" "1")
   (set (attr "length_immediate")
     (if_then_else (match_operand 2 "const_int_operand" "")
       (const_string "1")
       (const_string "0")))
   (set_attr "mode" "TI")])

(define_insn "*avx_lshrv1ti3"
  [(set (match_operand:V1TI 0 "register_operand" "=x")
 	(lshiftrt:V1TI
	 (match_operand:V1TI 1 "register_operand" "x")
	 (match_operand:SI 2 "const_0_to_255_mul_8_operand" "n")))]
  "TARGET_AVX"
{
  operands[2] = GEN_INT (INTVAL (operands[2]) / 8);
  return "vpsrldq\t{%2, %1, %0|%0, %1, %2}";
}
  [(set_attr "type" "sseishft")
   (set_attr "prefix" "vex")
   (set_attr "length_immediate" "1")
   (set_attr "mode" "TI")])

(define_insn "*avx_lshr<mode>3"
  [(set (match_operand:SSEMODE248 0 "register_operand" "=x")
	(lshiftrt:SSEMODE248
	  (match_operand:SSEMODE248 1 "register_operand" "x")
	  (match_operand:SI 2 "nonmemory_operand" "xN")))]
  "TARGET_AVX"
  "vpsrl<ssevecsize>\t{%2, %1, %0|%0, %1, %2}"
  [(set_attr "type" "sseishft")
   (set_attr "prefix" "vex")
   (set (attr "length_immediate")
     (if_then_else (match_operand 2 "const_int_operand" "")
       (const_string "1")
       (const_string "0")))
   (set_attr "mode" "TI")])

(define_insn "sse2_lshrv1ti3"
  [(set (match_operand:V1TI 0 "register_operand" "=x")
 	(lshiftrt:V1TI
	 (match_operand:V1TI 1 "register_operand" "0")
	 (match_operand:SI 2 "const_0_to_255_mul_8_operand" "n")))]
  "TARGET_SSE2"
{
  operands[2] = GEN_INT (INTVAL (operands[2]) / 8);
  return "psrldq\t{%2, %0|%0, %2}";
}
  [(set_attr "type" "sseishft")
   (set_attr "prefix_data16" "1")
   (set_attr "length_immediate" "1")
   (set_attr "atom_unit" "sishuf")
   (set_attr "mode" "TI")])

(define_insn "lshr<mode>3"
  [(set (match_operand:SSEMODE248 0 "register_operand" "=x")
	(lshiftrt:SSEMODE248
	  (match_operand:SSEMODE248 1 "register_operand" "0")
	  (match_operand:SI 2 "nonmemory_operand" "xN")))]
  "TARGET_SSE2"
  "psrl<ssevecsize>\t{%2, %0|%0, %2}"
  [(set_attr "type" "sseishft")
   (set_attr "prefix_data16" "1")
   (set (attr "length_immediate")
     (if_then_else (match_operand 2 "const_int_operand" "")
       (const_string "1")
       (const_string "0")))
   (set_attr "mode" "TI")])

(define_insn "*avx_ashlv1ti3"
  [(set (match_operand:V1TI 0 "register_operand" "=x")
	(ashift:V1TI (match_operand:V1TI 1 "register_operand" "x")
		     (match_operand:SI 2 "const_0_to_255_mul_8_operand" "n")))]
  "TARGET_AVX"
{
  operands[2] = GEN_INT (INTVAL (operands[2]) / 8);
  return "vpslldq\t{%2, %1, %0|%0, %1, %2}";
}
  [(set_attr "type" "sseishft")
   (set_attr "prefix" "vex")
   (set_attr "length_immediate" "1")
   (set_attr "mode" "TI")])

(define_insn "*avx_ashl<mode>3"
  [(set (match_operand:SSEMODE248 0 "register_operand" "=x")
	(ashift:SSEMODE248
	  (match_operand:SSEMODE248 1 "register_operand" "x")
	  (match_operand:SI 2 "nonmemory_operand" "xN")))]
  "TARGET_AVX"
  "vpsll<ssevecsize>\t{%2, %1, %0|%0, %1, %2}"
  [(set_attr "type" "sseishft")
   (set_attr "prefix" "vex")
   (set (attr "length_immediate")
     (if_then_else (match_operand 2 "const_int_operand" "")
       (const_string "1")
       (const_string "0")))
   (set_attr "mode" "TI")])

(define_insn "sse2_ashlv1ti3"
  [(set (match_operand:V1TI 0 "register_operand" "=x")
	(ashift:V1TI (match_operand:V1TI 1 "register_operand" "0")
		     (match_operand:SI 2 "const_0_to_255_mul_8_operand" "n")))]
  "TARGET_SSE2"
{
  operands[2] = GEN_INT (INTVAL (operands[2]) / 8);
  return "pslldq\t{%2, %0|%0, %2}";
}
  [(set_attr "type" "sseishft")
   (set_attr "prefix_data16" "1")
   (set_attr "length_immediate" "1")
   (set_attr "mode" "TI")])

(define_insn "ashl<mode>3"
  [(set (match_operand:SSEMODE248 0 "register_operand" "=x")
	(ashift:SSEMODE248
	  (match_operand:SSEMODE248 1 "register_operand" "0")
	  (match_operand:SI 2 "nonmemory_operand" "xN")))]
  "TARGET_SSE2"
  "psll<ssevecsize>\t{%2, %0|%0, %2}"
  [(set_attr "type" "sseishft")
   (set_attr "prefix_data16" "1")
   (set (attr "length_immediate")
     (if_then_else (match_operand 2 "const_int_operand" "")
       (const_string "1")
       (const_string "0")))
   (set_attr "mode" "TI")])

(define_expand "vec_shl_<mode>"
  [(set (match_operand:SSEMODEI 0 "register_operand" "")
        (ashift:V1TI
	 (match_operand:SSEMODEI 1 "register_operand" "")
	 (match_operand:SI 2 "const_0_to_255_mul_8_operand" "")))]
  "TARGET_SSE2"
{
  operands[0] = gen_lowpart (V1TImode, operands[0]);
  operands[1] = gen_lowpart (V1TImode, operands[1]);
})

(define_expand "vec_shr_<mode>"
  [(set (match_operand:SSEMODEI 0 "register_operand" "")
        (lshiftrt:V1TI
	 (match_operand:SSEMODEI 1 "register_operand" "")
	 (match_operand:SI 2 "const_0_to_255_mul_8_operand" "")))]
  "TARGET_SSE2"
{
  operands[0] = gen_lowpart (V1TImode, operands[0]);
  operands[1] = gen_lowpart (V1TImode, operands[1]);
})

(define_insn "*avx_<code><mode>3"
  [(set (match_operand:SSEMODE124 0 "register_operand" "=x")
	(umaxmin:SSEMODE124
	  (match_operand:SSEMODE124 1 "nonimmediate_operand" "%x")
	  (match_operand:SSEMODE124 2 "nonimmediate_operand" "xm")))]
  "TARGET_AVX && ix86_binary_operator_ok (<CODE>, <MODE>mode, operands)"
  "vp<maxmin_int><ssevecsize>\t{%2, %1, %0|%0, %1, %2}"
  [(set_attr "type" "sseiadd")
   (set (attr "prefix_extra")
     (if_then_else (match_operand:V16QI 0 "" "")
       (const_string "0")
       (const_string "1")))
   (set_attr "prefix" "vex")
   (set_attr "mode" "TI")])

(define_expand "<code>v16qi3"
  [(set (match_operand:V16QI 0 "register_operand" "")
	(umaxmin:V16QI
	  (match_operand:V16QI 1 "nonimmediate_operand" "")
	  (match_operand:V16QI 2 "nonimmediate_operand" "")))]
  "TARGET_SSE2"
  "ix86_fixup_binary_operands_no_copy (<CODE>, V16QImode, operands);")

(define_insn "*<code>v16qi3"
  [(set (match_operand:V16QI 0 "register_operand" "=x")
	(umaxmin:V16QI
	  (match_operand:V16QI 1 "nonimmediate_operand" "%0")
	  (match_operand:V16QI 2 "nonimmediate_operand" "xm")))]
  "TARGET_SSE2 && ix86_binary_operator_ok (<CODE>, V16QImode, operands)"
  "p<maxmin_int>b\t{%2, %0|%0, %2}"
  [(set_attr "type" "sseiadd")
   (set_attr "prefix_data16" "1")
   (set_attr "mode" "TI")])

(define_insn "*avx_<code><mode>3"
  [(set (match_operand:SSEMODE124 0 "register_operand" "=x")
	(smaxmin:SSEMODE124
	  (match_operand:SSEMODE124 1 "nonimmediate_operand" "%x")
	  (match_operand:SSEMODE124 2 "nonimmediate_operand" "xm")))]
  "TARGET_AVX && ix86_binary_operator_ok (<CODE>, <MODE>mode, operands)"
  "vp<maxmin_int><ssevecsize>\t{%2, %1, %0|%0, %1, %2}"
  [(set_attr "type" "sseiadd")
   (set (attr "prefix_extra")
     (if_then_else (match_operand:V8HI 0 "" "")
       (const_string "0")
       (const_string "1")))
   (set_attr "prefix" "vex")
   (set_attr "mode" "TI")])

(define_expand "<code>v8hi3"
  [(set (match_operand:V8HI 0 "register_operand" "")
	(smaxmin:V8HI
	  (match_operand:V8HI 1 "nonimmediate_operand" "")
	  (match_operand:V8HI 2 "nonimmediate_operand" "")))]
  "TARGET_SSE2"
  "ix86_fixup_binary_operands_no_copy (<CODE>, V8HImode, operands);")

(define_insn "*<code>v8hi3"
  [(set (match_operand:V8HI 0 "register_operand" "=x")
	(smaxmin:V8HI
	  (match_operand:V8HI 1 "nonimmediate_operand" "%0")
	  (match_operand:V8HI 2 "nonimmediate_operand" "xm")))]
  "TARGET_SSE2 && ix86_binary_operator_ok (<CODE>, V8HImode, operands)"
  "p<maxmin_int>w\t{%2, %0|%0, %2}"
  [(set_attr "type" "sseiadd")
   (set_attr "prefix_data16" "1")
   (set_attr "mode" "TI")])

(define_expand "umaxv8hi3"
  [(set (match_operand:V8HI 0 "register_operand" "")
	(umax:V8HI (match_operand:V8HI 1 "register_operand" "")
		   (match_operand:V8HI 2 "nonimmediate_operand" "")))]
  "TARGET_SSE2"
{
  if (TARGET_SSE4_1)
    ix86_fixup_binary_operands_no_copy (UMAX, V8HImode, operands);
  else
    {
      rtx op0 = operands[0], op2 = operands[2], op3 = op0;
      if (rtx_equal_p (op3, op2))
	op3 = gen_reg_rtx (V8HImode);
      emit_insn (gen_sse2_ussubv8hi3 (op3, operands[1], op2));
      emit_insn (gen_addv8hi3 (op0, op3, op2));
      DONE;
    }
})

(define_expand "smax<mode>3"
  [(set (match_operand:SSEMODE14 0 "register_operand" "")
	(smax:SSEMODE14 (match_operand:SSEMODE14 1 "register_operand" "")
			(match_operand:SSEMODE14 2 "register_operand" "")))]
  "TARGET_SSE2"
{
  if (TARGET_SSE4_1)
    ix86_fixup_binary_operands_no_copy (SMAX, <MODE>mode, operands);
  else
    {
      rtx xops[6];
      bool ok;

      xops[0] = operands[0];
      xops[1] = operands[1];
      xops[2] = operands[2];
      xops[3] = gen_rtx_GT (VOIDmode, operands[1], operands[2]);
      xops[4] = operands[1];
      xops[5] = operands[2];
      ok = ix86_expand_int_vcond (xops);
      gcc_assert (ok);
      DONE;
    }
})

(define_insn "*sse4_1_<code><mode>3"
  [(set (match_operand:SSEMODE14 0 "register_operand" "=x")
	(smaxmin:SSEMODE14
	  (match_operand:SSEMODE14 1 "nonimmediate_operand" "%0")
	  (match_operand:SSEMODE14 2 "nonimmediate_operand" "xm")))]
  "TARGET_SSE4_1 && ix86_binary_operator_ok (<CODE>, <MODE>mode, operands)"
  "p<maxmin_int><ssevecsize>\t{%2, %0|%0, %2}"
  [(set_attr "type" "sseiadd")
   (set_attr "prefix_extra" "1")
   (set_attr "mode" "TI")])

(define_expand "smaxv2di3"
  [(set (match_operand:V2DI 0 "register_operand" "")
	(smax:V2DI (match_operand:V2DI 1 "register_operand" "")
		   (match_operand:V2DI 2 "register_operand" "")))]
  "TARGET_SSE4_2"
{
  rtx xops[6];
  bool ok;

  xops[0] = operands[0];
  xops[1] = operands[1];
  xops[2] = operands[2];
  xops[3] = gen_rtx_GT (VOIDmode, operands[1], operands[2]);
  xops[4] = operands[1];
  xops[5] = operands[2];
  ok = ix86_expand_int_vcond (xops);
  gcc_assert (ok);
  DONE;
})

(define_expand "umaxv4si3"
  [(set (match_operand:V4SI 0 "register_operand" "")
	(umax:V4SI (match_operand:V4SI 1 "register_operand" "")
		   (match_operand:V4SI 2 "register_operand" "")))]
  "TARGET_SSE2"
{
  if (TARGET_SSE4_1)
    ix86_fixup_binary_operands_no_copy (UMAX, V4SImode, operands);
  else
    {
      rtx xops[6];
      bool ok;

      xops[0] = operands[0];
      xops[1] = operands[1];
      xops[2] = operands[2];
      xops[3] = gen_rtx_GTU (VOIDmode, operands[1], operands[2]);
      xops[4] = operands[1];
      xops[5] = operands[2];
      ok = ix86_expand_int_vcond (xops);
      gcc_assert (ok);
      DONE;
    }
})

(define_insn "*sse4_1_<code><mode>3"
  [(set (match_operand:SSEMODE24 0 "register_operand" "=x")
	(umaxmin:SSEMODE24
	  (match_operand:SSEMODE24 1 "nonimmediate_operand" "%0")
	  (match_operand:SSEMODE24 2 "nonimmediate_operand" "xm")))]
  "TARGET_SSE4_1 && ix86_binary_operator_ok (<CODE>, <MODE>mode, operands)"
  "p<maxmin_int><ssevecsize>\t{%2, %0|%0, %2}"
  [(set_attr "type" "sseiadd")
   (set_attr "prefix_extra" "1")
   (set_attr "mode" "TI")])

(define_expand "umaxv2di3"
  [(set (match_operand:V2DI 0 "register_operand" "")
	(umax:V2DI (match_operand:V2DI 1 "register_operand" "")
		   (match_operand:V2DI 2 "register_operand" "")))]
  "TARGET_SSE4_2"
{
  rtx xops[6];
  bool ok;

  xops[0] = operands[0];
  xops[1] = operands[1];
  xops[2] = operands[2];
  xops[3] = gen_rtx_GTU (VOIDmode, operands[1], operands[2]);
  xops[4] = operands[1];
  xops[5] = operands[2];
  ok = ix86_expand_int_vcond (xops);
  gcc_assert (ok);
  DONE;
})

(define_expand "smin<mode>3"
  [(set (match_operand:SSEMODE14 0 "register_operand" "")
	(smin:SSEMODE14 (match_operand:SSEMODE14 1 "register_operand" "")
			(match_operand:SSEMODE14 2 "register_operand" "")))]
  "TARGET_SSE2"
{
  if (TARGET_SSE4_1)
    ix86_fixup_binary_operands_no_copy (SMIN, <MODE>mode, operands);
  else
    {
      rtx xops[6];
      bool ok;

      xops[0] = operands[0];
      xops[1] = operands[2];
      xops[2] = operands[1];
      xops[3] = gen_rtx_GT (VOIDmode, operands[1], operands[2]);
      xops[4] = operands[1];
      xops[5] = operands[2];
      ok = ix86_expand_int_vcond (xops);
      gcc_assert (ok);
      DONE;
    }
})

(define_expand "sminv2di3"
  [(set (match_operand:V2DI 0 "register_operand" "")
	(smin:V2DI (match_operand:V2DI 1 "register_operand" "")
		   (match_operand:V2DI 2 "register_operand" "")))]
  "TARGET_SSE4_2"
{
  rtx xops[6];
  bool ok;

  xops[0] = operands[0];
  xops[1] = operands[2];
  xops[2] = operands[1];
  xops[3] = gen_rtx_GT (VOIDmode, operands[1], operands[2]);
  xops[4] = operands[1];
  xops[5] = operands[2];
  ok = ix86_expand_int_vcond (xops);
  gcc_assert (ok);
  DONE;
})

(define_expand "umin<mode>3"
  [(set (match_operand:SSEMODE24 0 "register_operand" "")
	(umin:SSEMODE24 (match_operand:SSEMODE24 1 "register_operand" "")
			(match_operand:SSEMODE24 2 "register_operand" "")))]
  "TARGET_SSE2"
{
  if (TARGET_SSE4_1)
    ix86_fixup_binary_operands_no_copy (UMIN, <MODE>mode, operands);
  else
    {
      rtx xops[6];
      bool ok;

      xops[0] = operands[0];
      xops[1] = operands[2];
      xops[2] = operands[1];
      xops[3] = gen_rtx_GTU (VOIDmode, operands[1], operands[2]);
      xops[4] = operands[1];
      xops[5] = operands[2];
      ok = ix86_expand_int_vcond (xops);
      gcc_assert (ok);
      DONE;
    }
})

(define_expand "uminv2di3"
  [(set (match_operand:V2DI 0 "register_operand" "")
	(umin:V2DI (match_operand:V2DI 1 "register_operand" "")
		   (match_operand:V2DI 2 "register_operand" "")))]
  "TARGET_SSE4_2"
{
  rtx xops[6];
  bool ok;

  xops[0] = operands[0];
  xops[1] = operands[2];
  xops[2] = operands[1];
  xops[3] = gen_rtx_GTU (VOIDmode, operands[1], operands[2]);
  xops[4] = operands[1];
  xops[5] = operands[2];
  ok = ix86_expand_int_vcond (xops);
  gcc_assert (ok);
  DONE;
})

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Parallel integral comparisons
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define_expand "sse2_eq<mode>3"
  [(set (match_operand:SSEMODE124 0 "register_operand" "")
	(eq:SSEMODE124
	  (match_operand:SSEMODE124 1 "nonimmediate_operand" "")
	  (match_operand:SSEMODE124 2 "nonimmediate_operand" "")))]
  "TARGET_SSE2 && !TARGET_XOP "
  "ix86_fixup_binary_operands_no_copy (EQ, <MODE>mode, operands);")

(define_insn "*avx_eq<mode>3"
  [(set (match_operand:SSEMODE1248 0 "register_operand" "=x")
	(eq:SSEMODE1248
	  (match_operand:SSEMODE1248 1 "nonimmediate_operand" "%x")
	  (match_operand:SSEMODE1248 2 "nonimmediate_operand" "xm")))]
  "TARGET_AVX && ix86_binary_operator_ok (EQ, <MODE>mode, operands)"
  "vpcmpeq<ssevecsize>\t{%2, %1, %0|%0, %1, %2}"
  [(set_attr "type" "ssecmp")
   (set (attr "prefix_extra")
     (if_then_else (match_operand:V2DI 0 "" "")
       (const_string "1")
       (const_string "*")))
   (set_attr "prefix" "vex")
   (set_attr "mode" "TI")])

(define_insn "*sse2_eq<mode>3"
  [(set (match_operand:SSEMODE124 0 "register_operand" "=x")
	(eq:SSEMODE124
	  (match_operand:SSEMODE124 1 "nonimmediate_operand" "%0")
	  (match_operand:SSEMODE124 2 "nonimmediate_operand" "xm")))]
  "TARGET_SSE2 && !TARGET_XOP
   && ix86_binary_operator_ok (EQ, <MODE>mode, operands)"
  "pcmpeq<ssevecsize>\t{%2, %0|%0, %2}"
  [(set_attr "type" "ssecmp")
   (set_attr "prefix_data16" "1")
   (set_attr "mode" "TI")])

(define_expand "sse4_1_eqv2di3"
  [(set (match_operand:V2DI 0 "register_operand" "")
	(eq:V2DI
	  (match_operand:V2DI 1 "nonimmediate_operand" "")
	  (match_operand:V2DI 2 "nonimmediate_operand" "")))]
  "TARGET_SSE4_1"
  "ix86_fixup_binary_operands_no_copy (EQ, V2DImode, operands);")

(define_insn "*sse4_1_eqv2di3"
  [(set (match_operand:V2DI 0 "register_operand" "=x")
	(eq:V2DI
	  (match_operand:V2DI 1 "nonimmediate_operand" "%0")
	  (match_operand:V2DI 2 "nonimmediate_operand" "xm")))]
  "TARGET_SSE4_1 && ix86_binary_operator_ok (EQ, V2DImode, operands)"
  "pcmpeqq\t{%2, %0|%0, %2}"
  [(set_attr "type" "ssecmp")
   (set_attr "prefix_extra" "1")
   (set_attr "mode" "TI")])

(define_insn "*avx_gt<mode>3"
  [(set (match_operand:SSEMODE1248 0 "register_operand" "=x")
	(gt:SSEMODE1248
	  (match_operand:SSEMODE1248 1 "register_operand" "x")
	  (match_operand:SSEMODE1248 2 "nonimmediate_operand" "xm")))]
  "TARGET_AVX"
  "vpcmpgt<ssevecsize>\t{%2, %1, %0|%0, %1, %2}"
  [(set_attr "type" "ssecmp")
   (set (attr "prefix_extra")
     (if_then_else (match_operand:V2DI 0 "" "")
       (const_string "1")
       (const_string "*")))
   (set_attr "prefix" "vex")
   (set_attr "mode" "TI")])

(define_insn "sse2_gt<mode>3"
  [(set (match_operand:SSEMODE124 0 "register_operand" "=x")
	(gt:SSEMODE124
	  (match_operand:SSEMODE124 1 "register_operand" "0")
	  (match_operand:SSEMODE124 2 "nonimmediate_operand" "xm")))]
  "TARGET_SSE2 && !TARGET_XOP"
  "pcmpgt<ssevecsize>\t{%2, %0|%0, %2}"
  [(set_attr "type" "ssecmp")
   (set_attr "prefix_data16" "1")
   (set_attr "mode" "TI")])

(define_insn "sse4_2_gtv2di3"
  [(set (match_operand:V2DI 0 "register_operand" "=x")
	(gt:V2DI
	  (match_operand:V2DI 1 "register_operand" "0")
	  (match_operand:V2DI 2 "nonimmediate_operand" "xm")))]
  "TARGET_SSE4_2"
  "pcmpgtq\t{%2, %0|%0, %2}"
  [(set_attr "type" "ssecmp")
   (set_attr "prefix_extra" "1")
   (set_attr "mode" "TI")])

(define_expand "vcond<mode>"
  [(set (match_operand:SSEMODE124C8 0 "register_operand" "")
        (if_then_else:SSEMODE124C8
          (match_operator 3 ""
            [(match_operand:SSEMODE124C8 4 "nonimmediate_operand" "")
             (match_operand:SSEMODE124C8 5 "nonimmediate_operand" "")])
          (match_operand:SSEMODE124C8 1 "general_operand" "")
          (match_operand:SSEMODE124C8 2 "general_operand" "")))]
  "TARGET_SSE2"
{
  bool ok = ix86_expand_int_vcond (operands);
  gcc_assert (ok);
  DONE;
})

(define_expand "vcondu<mode>"
  [(set (match_operand:SSEMODE124C8 0 "register_operand" "")
        (if_then_else:SSEMODE124C8
          (match_operator 3 ""
            [(match_operand:SSEMODE124C8 4 "nonimmediate_operand" "")
             (match_operand:SSEMODE124C8 5 "nonimmediate_operand" "")])
          (match_operand:SSEMODE124C8 1 "general_operand" "")
          (match_operand:SSEMODE124C8 2 "general_operand" "")))]
  "TARGET_SSE2"
{
  bool ok = ix86_expand_int_vcond (operands);
  gcc_assert (ok);
  DONE;
})

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Parallel bitwise logical operations
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define_expand "one_cmpl<mode>2"
  [(set (match_operand:SSEMODEI 0 "register_operand" "")
	(xor:SSEMODEI (match_operand:SSEMODEI 1 "nonimmediate_operand" "")
		      (match_dup 2)))]
  "TARGET_SSE2"
{
  int i, n = GET_MODE_NUNITS (<MODE>mode);
  rtvec v = rtvec_alloc (n);

  for (i = 0; i < n; ++i)
    RTVEC_ELT (v, i) = constm1_rtx;

  operands[2] = force_reg (<MODE>mode, gen_rtx_CONST_VECTOR (<MODE>mode, v));
})

(define_insn "*avx_andnot<mode>3"
  [(set (match_operand:AVX256MODEI 0 "register_operand" "=x")
	(and:AVX256MODEI
	  (not:AVX256MODEI (match_operand:AVX256MODEI 1 "register_operand" "x"))
          (match_operand:AVX256MODEI 2 "nonimmediate_operand" "xm")))]
  "TARGET_AVX"
  "vandnps\t{%2, %1, %0|%0, %1, %2}"
  [(set_attr "type" "sselog")
   (set_attr "prefix" "vex")
   (set_attr "mode" "<avxvecpsmode>")])

(define_insn "*sse_andnot<mode>3"
  [(set (match_operand:SSEMODEI 0 "register_operand" "=x")
	(and:SSEMODEI
	  (not:SSEMODEI (match_operand:SSEMODEI 1 "register_operand" "0"))
          (match_operand:SSEMODEI 2 "nonimmediate_operand" "xm")))]
  "(TARGET_SSE && !TARGET_SSE2)"
  "andnps\t{%2, %0|%0, %2}"
  [(set_attr "type" "sselog")
   (set_attr "mode" "V4SF")])

(define_insn "*avx_andnot<mode>3"
  [(set (match_operand:SSEMODEI 0 "register_operand" "=x")
	(and:SSEMODEI
	  (not:SSEMODEI (match_operand:SSEMODEI 1 "register_operand" "x"))
	  (match_operand:SSEMODEI 2 "nonimmediate_operand" "xm")))]
  "TARGET_AVX"
  "vpandn\t{%2, %1, %0|%0, %1, %2}"
  [(set_attr "type" "sselog")
   (set_attr "prefix" "vex")
   (set_attr "mode" "TI")])

(define_insn "sse2_andnot<mode>3"
  [(set (match_operand:SSEMODEI 0 "register_operand" "=x")
	(and:SSEMODEI
	  (not:SSEMODEI (match_operand:SSEMODEI 1 "register_operand" "0"))
	  (match_operand:SSEMODEI 2 "nonimmediate_operand" "xm")))]
  "TARGET_SSE2"
  "pandn\t{%2, %0|%0, %2}"
  [(set_attr "type" "sselog")
   (set_attr "prefix_data16" "1")
   (set_attr "mode" "TI")])

(define_insn "*andnottf3"
  [(set (match_operand:TF 0 "register_operand" "=x")
	(and:TF
	  (not:TF (match_operand:TF 1 "register_operand" "0"))
	  (match_operand:TF 2 "nonimmediate_operand" "xm")))]
  "TARGET_SSE2"
  "pandn\t{%2, %0|%0, %2}"
  [(set_attr "type" "sselog")
   (set_attr "prefix_data16" "1")
   (set_attr "mode" "TI")])

(define_expand "<code><mode>3"
  [(set (match_operand:SSEMODEI 0 "register_operand" "")
	(any_logic:SSEMODEI
	  (match_operand:SSEMODEI 1 "nonimmediate_operand" "")
	  (match_operand:SSEMODEI 2 "nonimmediate_operand" "")))]
  "TARGET_SSE"
  "ix86_fixup_binary_operands_no_copy (<CODE>, <MODE>mode, operands);")

(define_insn "*avx_<code><mode>3"
  [(set (match_operand:AVX256MODEI 0 "register_operand" "=x")
        (any_logic:AVX256MODEI
          (match_operand:AVX256MODEI 1 "nonimmediate_operand" "%x")
          (match_operand:AVX256MODEI 2 "nonimmediate_operand" "xm")))]
  "TARGET_AVX
   && ix86_binary_operator_ok (<CODE>, <MODE>mode, operands)"
  "v<logic>ps\t{%2, %1, %0|%0, %1, %2}"
  [(set_attr "type" "sselog")
   (set_attr "prefix" "vex")
   (set_attr "mode" "<avxvecpsmode>")])

(define_insn "*sse_<code><mode>3"
  [(set (match_operand:SSEMODEI 0 "register_operand" "=x")
        (any_logic:SSEMODEI
          (match_operand:SSEMODEI 1 "nonimmediate_operand" "%0")
          (match_operand:SSEMODEI 2 "nonimmediate_operand" "xm")))]
  "(TARGET_SSE && !TARGET_SSE2)
   && ix86_binary_operator_ok (<CODE>, <MODE>mode, operands)"
  "<logic>ps\t{%2, %0|%0, %2}"
  [(set_attr "type" "sselog")
   (set_attr "mode" "V4SF")])

(define_insn "*avx_<code><mode>3"
  [(set (match_operand:SSEMODEI 0 "register_operand" "=x")
        (any_logic:SSEMODEI
          (match_operand:SSEMODEI 1 "nonimmediate_operand" "%x")
          (match_operand:SSEMODEI 2 "nonimmediate_operand" "xm")))]
  "TARGET_AVX
   && ix86_binary_operator_ok (<CODE>, <MODE>mode, operands)"
  "vp<logic>\t{%2, %1, %0|%0, %1, %2}"
  [(set_attr "type" "sselog")
   (set_attr "prefix" "vex")
   (set_attr "mode" "TI")])

(define_insn "*sse2_<code><mode>3"
  [(set (match_operand:SSEMODEI 0 "register_operand" "=x")
	(any_logic:SSEMODEI
	  (match_operand:SSEMODEI 1 "nonimmediate_operand" "%0")
	  (match_operand:SSEMODEI 2 "nonimmediate_operand" "xm")))]
  "TARGET_SSE2 && ix86_binary_operator_ok (<CODE>, <MODE>mode, operands)"
  "p<logic>\t{%2, %0|%0, %2}"
  [(set_attr "type" "sselog")
   (set_attr "prefix_data16" "1")
   (set_attr "mode" "TI")])

(define_expand "<code>tf3"
  [(set (match_operand:TF 0 "register_operand" "")
	(any_logic:TF
	  (match_operand:TF 1 "nonimmediate_operand" "")
	  (match_operand:TF 2 "nonimmediate_operand" "")))]
  "TARGET_SSE2"
  "ix86_fixup_binary_operands_no_copy (<CODE>, TFmode, operands);")

(define_insn "*<code>tf3"
  [(set (match_operand:TF 0 "register_operand" "=x")
	(any_logic:TF
	  (match_operand:TF 1 "nonimmediate_operand" "%0")
	  (match_operand:TF 2 "nonimmediate_operand" "xm")))]
  "TARGET_SSE2 && ix86_binary_operator_ok (<CODE>, TFmode, operands)"
  "p<logic>\t{%2, %0|%0, %2}"
  [(set_attr "type" "sselog")
   (set_attr "prefix_data16" "1")
   (set_attr "mode" "TI")])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Parallel integral element swizzling
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define_expand "vec_pack_trunc_v8hi"
  [(match_operand:V16QI 0 "register_operand" "")
   (match_operand:V8HI 1 "register_operand" "")
   (match_operand:V8HI 2 "register_operand" "")]
  "TARGET_SSE2"
{
  rtx op1 = gen_lowpart (V16QImode, operands[1]);
  rtx op2 = gen_lowpart (V16QImode, operands[2]);
  ix86_expand_vec_extract_even_odd (operands[0], op1, op2, 0);
  DONE;
})

(define_expand "vec_pack_trunc_v4si"
  [(match_operand:V8HI 0 "register_operand" "")
   (match_operand:V4SI 1 "register_operand" "")
   (match_operand:V4SI 2 "register_operand" "")]
  "TARGET_SSE2"
{
  rtx op1 = gen_lowpart (V8HImode, operands[1]);
  rtx op2 = gen_lowpart (V8HImode, operands[2]);
  ix86_expand_vec_extract_even_odd (operands[0], op1, op2, 0);
  DONE;
})

(define_expand "vec_pack_trunc_v2di"
  [(match_operand:V4SI 0 "register_operand" "")
   (match_operand:V2DI 1 "register_operand" "")
   (match_operand:V2DI 2 "register_operand" "")]
  "TARGET_SSE2"
{
  rtx op1 = gen_lowpart (V4SImode, operands[1]);
  rtx op2 = gen_lowpart (V4SImode, operands[2]);
  ix86_expand_vec_extract_even_odd (operands[0], op1, op2, 0);
  DONE;
})

(define_insn "*avx_packsswb"
  [(set (match_operand:V16QI 0 "register_operand" "=x")
	(vec_concat:V16QI
	  (ss_truncate:V8QI
	    (match_operand:V8HI 1 "register_operand" "x"))
	  (ss_truncate:V8QI
	    (match_operand:V8HI 2 "nonimmediate_operand" "xm"))))]
  "TARGET_AVX"
  "vpacksswb\t{%2, %1, %0|%0, %1, %2}"
  [(set_attr "type" "sselog")
   (set_attr "prefix" "vex")
   (set_attr "mode" "TI")])

(define_insn "sse2_packsswb"
  [(set (match_operand:V16QI 0 "register_operand" "=x")
	(vec_concat:V16QI
	  (ss_truncate:V8QI
	    (match_operand:V8HI 1 "register_operand" "0"))
	  (ss_truncate:V8QI
	    (match_operand:V8HI 2 "nonimmediate_operand" "xm"))))]
  "TARGET_SSE2"
  "packsswb\t{%2, %0|%0, %2}"
  [(set_attr "type" "sselog")
   (set_attr "prefix_data16" "1")
   (set_attr "mode" "TI")])

(define_insn "*avx_packssdw"
  [(set (match_operand:V8HI 0 "register_operand" "=x")
	(vec_concat:V8HI
	  (ss_truncate:V4HI
	    (match_operand:V4SI 1 "register_operand" "x"))
	  (ss_truncate:V4HI
	    (match_operand:V4SI 2 "nonimmediate_operand" "xm"))))]
  "TARGET_AVX"
  "vpackssdw\t{%2, %1, %0|%0, %1, %2}"
  [(set_attr "type" "sselog")
   (set_attr "prefix" "vex")
   (set_attr "mode" "TI")])

(define_insn "sse2_packssdw"
  [(set (match_operand:V8HI 0 "register_operand" "=x")
	(vec_concat:V8HI
	  (ss_truncate:V4HI
	    (match_operand:V4SI 1 "register_operand" "0"))
	  (ss_truncate:V4HI
	    (match_operand:V4SI 2 "nonimmediate_operand" "xm"))))]
  "TARGET_SSE2"
  "packssdw\t{%2, %0|%0, %2}"
  [(set_attr "type" "sselog")
   (set_attr "prefix_data16" "1")
   (set_attr "mode" "TI")])

(define_insn "*avx_packuswb"
  [(set (match_operand:V16QI 0 "register_operand" "=x")
	(vec_concat:V16QI
	  (us_truncate:V8QI
	    (match_operand:V8HI 1 "register_operand" "x"))
	  (us_truncate:V8QI
	    (match_operand:V8HI 2 "nonimmediate_operand" "xm"))))]
  "TARGET_AVX"
  "vpackuswb\t{%2, %1, %0|%0, %1, %2}"
  [(set_attr "type" "sselog")
   (set_attr "prefix" "vex")
   (set_attr "mode" "TI")])

(define_insn "sse2_packuswb"
  [(set (match_operand:V16QI 0 "register_operand" "=x")
	(vec_concat:V16QI
	  (us_truncate:V8QI
	    (match_operand:V8HI 1 "register_operand" "0"))
	  (us_truncate:V8QI
	    (match_operand:V8HI 2 "nonimmediate_operand" "xm"))))]
  "TARGET_SSE2"
  "packuswb\t{%2, %0|%0, %2}"
  [(set_attr "type" "sselog")
   (set_attr "prefix_data16" "1")
   (set_attr "mode" "TI")])

(define_insn "*avx_interleave_highv16qi"
  [(set (match_operand:V16QI 0 "register_operand" "=x")
	(vec_select:V16QI
	  (vec_concat:V32QI
	    (match_operand:V16QI 1 "register_operand" "x")
	    (match_operand:V16QI 2 "nonimmediate_operand" "xm"))
	  (parallel [(const_int 8)  (const_int 24)
		     (const_int 9)  (const_int 25)
		     (const_int 10) (const_int 26)
		     (const_int 11) (const_int 27)
		     (const_int 12) (const_int 28)
		     (const_int 13) (const_int 29)
		     (const_int 14) (const_int 30)
		     (const_int 15) (const_int 31)])))]
  "TARGET_AVX"
  "vpunpckhbw\t{%2, %1, %0|%0, %1, %2}"
  [(set_attr "type" "sselog")
   (set_attr "prefix" "vex")
   (set_attr "mode" "TI")])

(define_insn "vec_interleave_highv16qi"
  [(set (match_operand:V16QI 0 "register_operand" "=x")
	(vec_select:V16QI
	  (vec_concat:V32QI
	    (match_operand:V16QI 1 "register_operand" "0")
	    (match_operand:V16QI 2 "nonimmediate_operand" "xm"))
	  (parallel [(const_int 8)  (const_int 24)
		     (const_int 9)  (const_int 25)
		     (const_int 10) (const_int 26)
		     (const_int 11) (const_int 27)
		     (const_int 12) (const_int 28)
		     (const_int 13) (const_int 29)
		     (const_int 14) (const_int 30)
		     (const_int 15) (const_int 31)])))]
  "TARGET_SSE2"
  "punpckhbw\t{%2, %0|%0, %2}"
  [(set_attr "type" "sselog")
   (set_attr "prefix_data16" "1")
   (set_attr "mode" "TI")])

(define_insn "*avx_interleave_lowv16qi"
  [(set (match_operand:V16QI 0 "register_operand" "=x")
	(vec_select:V16QI
	  (vec_concat:V32QI
	    (match_operand:V16QI 1 "register_operand" "x")
	    (match_operand:V16QI 2 "nonimmediate_operand" "xm"))
	  (parallel [(const_int 0) (const_int 16)
		     (const_int 1) (const_int 17)
		     (const_int 2) (const_int 18)
		     (const_int 3) (const_int 19)
		     (const_int 4) (const_int 20)
		     (const_int 5) (const_int 21)
		     (const_int 6) (const_int 22)
		     (const_int 7) (const_int 23)])))]
  "TARGET_AVX"
  "vpunpcklbw\t{%2, %1, %0|%0, %1, %2}"
  [(set_attr "type" "sselog")
   (set_attr "prefix" "vex")
   (set_attr "mode" "TI")])

(define_insn "vec_interleave_lowv16qi"
  [(set (match_operand:V16QI 0 "register_operand" "=x")
	(vec_select:V16QI
	  (vec_concat:V32QI
	    (match_operand:V16QI 1 "register_operand" "0")
	    (match_operand:V16QI 2 "nonimmediate_operand" "xm"))
	  (parallel [(const_int 0) (const_int 16)
		     (const_int 1) (const_int 17)
		     (const_int 2) (const_int 18)
		     (const_int 3) (const_int 19)
		     (const_int 4) (const_int 20)
		     (const_int 5) (const_int 21)
		     (const_int 6) (const_int 22)
		     (const_int 7) (const_int 23)])))]
  "TARGET_SSE2"
  "punpcklbw\t{%2, %0|%0, %2}"
  [(set_attr "type" "sselog")
   (set_attr "prefix_data16" "1")
   (set_attr "mode" "TI")])

(define_insn "*avx_interleave_highv8hi"
  [(set (match_operand:V8HI 0 "register_operand" "=x")
	(vec_select:V8HI
	  (vec_concat:V16HI
	    (match_operand:V8HI 1 "register_operand" "x")
	    (match_operand:V8HI 2 "nonimmediate_operand" "xm"))
	  (parallel [(const_int 4) (const_int 12)
		     (const_int 5) (const_int 13)
		     (const_int 6) (const_int 14)
		     (const_int 7) (const_int 15)])))]
  "TARGET_AVX"
  "vpunpckhwd\t{%2, %1, %0|%0, %1, %2}"
  [(set_attr "type" "sselog")
   (set_attr "prefix" "vex")
   (set_attr "mode" "TI")])

(define_insn "vec_interleave_highv8hi"
  [(set (match_operand:V8HI 0 "register_operand" "=x")
	(vec_select:V8HI
	  (vec_concat:V16HI
	    (match_operand:V8HI 1 "register_operand" "0")
	    (match_operand:V8HI 2 "nonimmediate_operand" "xm"))
	  (parallel [(const_int 4) (const_int 12)
		     (const_int 5) (const_int 13)
		     (const_int 6) (const_int 14)
		     (const_int 7) (const_int 15)])))]
  "TARGET_SSE2"
  "punpckhwd\t{%2, %0|%0, %2}"
  [(set_attr "type" "sselog")
   (set_attr "prefix_data16" "1")
   (set_attr "mode" "TI")])

(define_insn "*avx_interleave_lowv8hi"
  [(set (match_operand:V8HI 0 "register_operand" "=x")
	(vec_select:V8HI
	  (vec_concat:V16HI
	    (match_operand:V8HI 1 "register_operand" "x")
	    (match_operand:V8HI 2 "nonimmediate_operand" "xm"))
	  (parallel [(const_int 0) (const_int 8)
		     (const_int 1) (const_int 9)
		     (const_int 2) (const_int 10)
		     (const_int 3) (const_int 11)])))]
  "TARGET_AVX"
  "vpunpcklwd\t{%2, %1, %0|%0, %1, %2}"
  [(set_attr "type" "sselog")
   (set_attr "prefix" "vex")
   (set_attr "mode" "TI")])

(define_insn "vec_interleave_lowv8hi"
  [(set (match_operand:V8HI 0 "register_operand" "=x")
	(vec_select:V8HI
	  (vec_concat:V16HI
	    (match_operand:V8HI 1 "register_operand" "0")
	    (match_operand:V8HI 2 "nonimmediate_operand" "xm"))
	  (parallel [(const_int 0) (const_int 8)
		     (const_int 1) (const_int 9)
		     (const_int 2) (const_int 10)
		     (const_int 3) (const_int 11)])))]
  "TARGET_SSE2"
  "punpcklwd\t{%2, %0|%0, %2}"
  [(set_attr "type" "sselog")
   (set_attr "prefix_data16" "1")
   (set_attr "mode" "TI")])

(define_insn "*avx_interleave_highv4si"
  [(set (match_operand:V4SI 0 "register_operand" "=x")
	(vec_select:V4SI
	  (vec_concat:V8SI
	    (match_operand:V4SI 1 "register_operand" "x")
	    (match_operand:V4SI 2 "nonimmediate_operand" "xm"))
	  (parallel [(const_int 2) (const_int 6)
		     (const_int 3) (const_int 7)])))]
  "TARGET_AVX"
  "vpunpckhdq\t{%2, %1, %0|%0, %1, %2}"
  [(set_attr "type" "sselog")
   (set_attr "prefix" "vex")
   (set_attr "mode" "TI")])

(define_insn "vec_interleave_highv4si"
  [(set (match_operand:V4SI 0 "register_operand" "=x")
	(vec_select:V4SI
	  (vec_concat:V8SI
	    (match_operand:V4SI 1 "register_operand" "0")
	    (match_operand:V4SI 2 "nonimmediate_operand" "xm"))
	  (parallel [(const_int 2) (const_int 6)
		     (const_int 3) (const_int 7)])))]
  "TARGET_SSE2"
  "punpckhdq\t{%2, %0|%0, %2}"
  [(set_attr "type" "sselog")
   (set_attr "prefix_data16" "1")
   (set_attr "mode" "TI")])

(define_insn "*avx_interleave_lowv4si"
  [(set (match_operand:V4SI 0 "register_operand" "=x")
	(vec_select:V4SI
	  (vec_concat:V8SI
	    (match_operand:V4SI 1 "register_operand" "x")
	    (match_operand:V4SI 2 "nonimmediate_operand" "xm"))
	  (parallel [(const_int 0) (const_int 4)
		     (const_int 1) (const_int 5)])))]
  "TARGET_AVX"
  "vpunpckldq\t{%2, %1, %0|%0, %1, %2}"
  [(set_attr "type" "sselog")
   (set_attr "prefix" "vex")
   (set_attr "mode" "TI")])

(define_insn "vec_interleave_lowv4si"
  [(set (match_operand:V4SI 0 "register_operand" "=x")
	(vec_select:V4SI
	  (vec_concat:V8SI
	    (match_operand:V4SI 1 "register_operand" "0")
	    (match_operand:V4SI 2 "nonimmediate_operand" "xm"))
	  (parallel [(const_int 0) (const_int 4)
		     (const_int 1) (const_int 5)])))]
  "TARGET_SSE2"
  "punpckldq\t{%2, %0|%0, %2}"
  [(set_attr "type" "sselog")
   (set_attr "prefix_data16" "1")
   (set_attr "mode" "TI")])

(define_insn "*avx_pinsr<ssevecsize>"
  [(set (match_operand:SSEMODE124 0 "register_operand" "=x")
	(vec_merge:SSEMODE124
	  (vec_duplicate:SSEMODE124
	    (match_operand:<avxscalarmode> 2 "nonimmediate_operand" "rm"))
	  (match_operand:SSEMODE124 1 "register_operand" "x")
	  (match_operand:SI 3 "const_pow2_1_to_<pinsrbits>_operand" "n")))]
  "TARGET_AVX"
{
  operands[3] = GEN_INT (exact_log2 (INTVAL (operands[3])));
  if (MEM_P (operands[2]))
    return "vpinsr<ssevecsize>\t{%3, %2, %1, %0|%0, %1, %2, %3}";
  else
    return "vpinsr<ssevecsize>\t{%3, %k2, %1, %0|%0, %1, %k2, %3}";
}
  [(set_attr "type" "sselog")
   (set (attr "prefix_extra")
     (if_then_else (match_operand:V8HI 0 "" "")
       (const_string "0")
       (const_string "1")))
   (set_attr "length_immediate" "1")
   (set_attr "prefix" "vex")
   (set_attr "mode" "TI")])

(define_insn "*sse4_1_pinsrb"
  [(set (match_operand:V16QI 0 "register_operand" "=x")
	(vec_merge:V16QI
	  (vec_duplicate:V16QI
	    (match_operand:QI 2 "nonimmediate_operand" "rm"))
	  (match_operand:V16QI 1 "register_operand" "0")
	  (match_operand:SI 3 "const_pow2_1_to_32768_operand" "n")))]
  "TARGET_SSE4_1"
{
  operands[3] = GEN_INT (exact_log2 (INTVAL (operands[3])));
  if (MEM_P (operands[2]))
    return "pinsrb\t{%3, %2, %0|%0, %2, %3}";
  else
    return "pinsrb\t{%3, %k2, %0|%0, %k2, %3}";
}
  [(set_attr "type" "sselog")
   (set_attr "prefix_extra" "1")
   (set_attr "length_immediate" "1")
   (set_attr "mode" "TI")])

(define_insn "*sse2_pinsrw"
  [(set (match_operand:V8HI 0 "register_operand" "=x")
	(vec_merge:V8HI
	  (vec_duplicate:V8HI
	    (match_operand:HI 2 "nonimmediate_operand" "rm"))
	  (match_operand:V8HI 1 "register_operand" "0")
	  (match_operand:SI 3 "const_pow2_1_to_128_operand" "n")))]
  "TARGET_SSE2"
{
  operands[3] = GEN_INT (exact_log2 (INTVAL (operands[3])));
  if (MEM_P (operands[2]))
    return "pinsrw\t{%3, %2, %0|%0, %2, %3}";
  else
    return "pinsrw\t{%3, %k2, %0|%0, %k2, %3}";
}
  [(set_attr "type" "sselog")
   (set_attr "prefix_data16" "1")
   (set_attr "length_immediate" "1")
   (set_attr "mode" "TI")])

;; It must come before sse2_loadld since it is preferred.
(define_insn "*sse4_1_pinsrd"
  [(set (match_operand:V4SI 0 "register_operand" "=x")
	(vec_merge:V4SI
	  (vec_duplicate:V4SI
	    (match_operand:SI 2 "nonimmediate_operand" "rm"))
	  (match_operand:V4SI 1 "register_operand" "0")
	  (match_operand:SI 3 "const_pow2_1_to_8_operand" "n")))]
  "TARGET_SSE4_1"
{
  operands[3] = GEN_INT (exact_log2 (INTVAL (operands[3])));
  return "pinsrd\t{%3, %2, %0|%0, %2, %3}";
}
  [(set_attr "type" "sselog")
   (set_attr "prefix_extra" "1")
   (set_attr "length_immediate" "1")
   (set_attr "mode" "TI")])

(define_insn "*avx_pinsrq"
  [(set (match_operand:V2DI 0 "register_operand" "=x")
	(vec_merge:V2DI
	  (vec_duplicate:V2DI
	    (match_operand:DI 2 "nonimmediate_operand" "rm"))
	  (match_operand:V2DI 1 "register_operand" "x")
	  (match_operand:SI 3 "const_pow2_1_to_2_operand" "n")))]
  "TARGET_AVX && TARGET_64BIT"
{
  operands[3] = GEN_INT (exact_log2 (INTVAL (operands[3])));
  return "vpinsrq\t{%3, %2, %1, %0|%0, %1, %2, %3}";
}
  [(set_attr "type" "sselog")
   (set_attr "prefix_extra" "1")
   (set_attr "length_immediate" "1")
   (set_attr "prefix" "vex")
   (set_attr "mode" "TI")])

(define_insn "*sse4_1_pinsrq"
  [(set (match_operand:V2DI 0 "register_operand" "=x")
	(vec_merge:V2DI
	  (vec_duplicate:V2DI
	    (match_operand:DI 2 "nonimmediate_operand" "rm"))
	  (match_operand:V2DI 1 "register_operand" "0")
	  (match_operand:SI 3 "const_pow2_1_to_2_operand" "n")))]
  "TARGET_SSE4_1 && TARGET_64BIT"
{
  operands[3] = GEN_INT (exact_log2 (INTVAL (operands[3])));
  return "pinsrq\t{%3, %2, %0|%0, %2, %3}";
}
  [(set_attr "type" "sselog")
   (set_attr "prefix_rex" "1")
   (set_attr "prefix_extra" "1")
   (set_attr "length_immediate" "1")
   (set_attr "mode" "TI")])

(define_insn "*sse4_1_pextrb_<mode>"
  [(set (match_operand:SWI48 0 "register_operand" "=r")
	(zero_extend:SWI48
	  (vec_select:QI
	    (match_operand:V16QI 1 "register_operand" "x")
	    (parallel [(match_operand:SI 2 "const_0_to_15_operand" "n")]))))]
  "TARGET_SSE4_1"
  "%vpextrb\t{%2, %1, %k0|%k0, %1, %2}"
  [(set_attr "type" "sselog")
   (set_attr "prefix_extra" "1")
   (set_attr "length_immediate" "1")
   (set_attr "prefix" "maybe_vex")
   (set_attr "mode" "TI")])

(define_insn "*sse4_1_pextrb_memory"
  [(set (match_operand:QI 0 "memory_operand" "=m")
	(vec_select:QI
	  (match_operand:V16QI 1 "register_operand" "x")
	  (parallel [(match_operand:SI 2 "const_0_to_15_operand" "n")])))]
  "TARGET_SSE4_1"
  "%vpextrb\t{%2, %1, %0|%0, %1, %2}"
  [(set_attr "type" "sselog")
   (set_attr "prefix_extra" "1")
   (set_attr "length_immediate" "1")
   (set_attr "prefix" "maybe_vex")
   (set_attr "mode" "TI")])

(define_insn "*sse2_pextrw_<mode>"
  [(set (match_operand:SWI48 0 "register_operand" "=r")
	(zero_extend:SWI48
	  (vec_select:HI
	    (match_operand:V8HI 1 "register_operand" "x")
	    (parallel [(match_operand:SI 2 "const_0_to_7_operand" "n")]))))]
  "TARGET_SSE2"
  "%vpextrw\t{%2, %1, %k0|%k0, %1, %2}"
  [(set_attr "type" "sselog")
   (set_attr "prefix_data16" "1")
   (set_attr "length_immediate" "1")
   (set_attr "prefix" "maybe_vex")
   (set_attr "mode" "TI")])

(define_insn "*sse4_1_pextrw_memory"
  [(set (match_operand:HI 0 "memory_operand" "=m")
	(vec_select:HI
	  (match_operand:V8HI 1 "register_operand" "x")
	  (parallel [(match_operand:SI 2 "const_0_to_7_operand" "n")])))]
  "TARGET_SSE4_1"
  "%vpextrw\t{%2, %1, %0|%0, %1, %2}"
  [(set_attr "type" "sselog")
   (set_attr "prefix_extra" "1")
   (set_attr "length_immediate" "1")
   (set_attr "prefix" "maybe_vex")
   (set_attr "mode" "TI")])

(define_insn "*sse4_1_pextrd"
  [(set (match_operand:SI 0 "nonimmediate_operand" "=rm")
	(vec_select:SI
	  (match_operand:V4SI 1 "register_operand" "x")
	  (parallel [(match_operand:SI 2 "const_0_to_3_operand" "n")])))]
  "TARGET_SSE4_1"
  "%vpextrd\t{%2, %1, %0|%0, %1, %2}"
  [(set_attr "type" "sselog")
   (set_attr "prefix_extra" "1")
   (set_attr "length_immediate" "1")
   (set_attr "prefix" "maybe_vex")
   (set_attr "mode" "TI")])

(define_insn "*sse4_1_pextrd_zext"
  [(set (match_operand:DI 0 "register_operand" "=r")
	(zero_extend:DI
	  (vec_select:SI
	    (match_operand:V4SI 1 "register_operand" "x")
	    (parallel [(match_operand:SI 2 "const_0_to_3_operand" "n")]))))]
  "TARGET_64BIT && TARGET_SSE4_1"
  "%vpextrd\t{%2, %1, %k0|%k0, %1, %2}"
  [(set_attr "type" "sselog")
   (set_attr "prefix_extra" "1")
   (set_attr "length_immediate" "1")
   (set_attr "prefix" "maybe_vex")
   (set_attr "mode" "TI")])

;; It must come before *vec_extractv2di_1_sse since it is preferred.
(define_insn "*sse4_1_pextrq"
  [(set (match_operand:DI 0 "nonimmediate_operand" "=rm")
	(vec_select:DI
	  (match_operand:V2DI 1 "register_operand" "x")
	  (parallel [(match_operand:SI 2 "const_0_to_1_operand" "n")])))]
  "TARGET_SSE4_1 && TARGET_64BIT"
  "%vpextrq\t{%2, %1, %0|%0, %1, %2}"
  [(set_attr "type" "sselog")
   (set_attr "prefix_rex" "1")
   (set_attr "prefix_extra" "1")
   (set_attr "length_immediate" "1")
   (set_attr "prefix" "maybe_vex")
   (set_attr "mode" "TI")])

(define_expand "sse2_pshufd"
  [(match_operand:V4SI 0 "register_operand" "")
   (match_operand:V4SI 1 "nonimmediate_operand" "")
   (match_operand:SI 2 "const_int_operand" "")]
  "TARGET_SSE2"
{
  int mask = INTVAL (operands[2]);
  emit_insn (gen_sse2_pshufd_1 (operands[0], operands[1],
				GEN_INT ((mask >> 0) & 3),
				GEN_INT ((mask >> 2) & 3),
				GEN_INT ((mask >> 4) & 3),
				GEN_INT ((mask >> 6) & 3)));
  DONE;
})

(define_insn "sse2_pshufd_1"
  [(set (match_operand:V4SI 0 "register_operand" "=x")
	(vec_select:V4SI
	  (match_operand:V4SI 1 "nonimmediate_operand" "xm")
	  (parallel [(match_operand 2 "const_0_to_3_operand" "")
		     (match_operand 3 "const_0_to_3_operand" "")
		     (match_operand 4 "const_0_to_3_operand" "")
		     (match_operand 5 "const_0_to_3_operand" "")])))]
  "TARGET_SSE2"
{
  int mask = 0;
  mask |= INTVAL (operands[2]) << 0;
  mask |= INTVAL (operands[3]) << 2;
  mask |= INTVAL (operands[4]) << 4;
  mask |= INTVAL (operands[5]) << 6;
  operands[2] = GEN_INT (mask);

  return "%vpshufd\t{%2, %1, %0|%0, %1, %2}";
}
  [(set_attr "type" "sselog1")
   (set_attr "prefix_data16" "1")
   (set_attr "prefix" "maybe_vex")
   (set_attr "length_immediate" "1")
   (set_attr "mode" "TI")])

(define_expand "sse2_pshuflw"
  [(match_operand:V8HI 0 "register_operand" "")
   (match_operand:V8HI 1 "nonimmediate_operand" "")
   (match_operand:SI 2 "const_int_operand" "")]
  "TARGET_SSE2"
{
  int mask = INTVAL (operands[2]);
  emit_insn (gen_sse2_pshuflw_1 (operands[0], operands[1],
				 GEN_INT ((mask >> 0) & 3),
				 GEN_INT ((mask >> 2) & 3),
				 GEN_INT ((mask >> 4) & 3),
				 GEN_INT ((mask >> 6) & 3)));
  DONE;
})

(define_insn "sse2_pshuflw_1"
  [(set (match_operand:V8HI 0 "register_operand" "=x")
	(vec_select:V8HI
	  (match_operand:V8HI 1 "nonimmediate_operand" "xm")
	  (parallel [(match_operand 2 "const_0_to_3_operand" "")
		     (match_operand 3 "const_0_to_3_operand" "")
		     (match_operand 4 "const_0_to_3_operand" "")
		     (match_operand 5 "const_0_to_3_operand" "")
		     (const_int 4)
		     (const_int 5)
		     (const_int 6)
		     (const_int 7)])))]
  "TARGET_SSE2"
{
  int mask = 0;
  mask |= INTVAL (operands[2]) << 0;
  mask |= INTVAL (operands[3]) << 2;
  mask |= INTVAL (operands[4]) << 4;
  mask |= INTVAL (operands[5]) << 6;
  operands[2] = GEN_INT (mask);

  return "%vpshuflw\t{%2, %1, %0|%0, %1, %2}";
}
  [(set_attr "type" "sselog")
   (set_attr "prefix_data16" "0")
   (set_attr "prefix_rep" "1")
   (set_attr "prefix" "maybe_vex")
   (set_attr "length_immediate" "1")
   (set_attr "mode" "TI")])

(define_expand "sse2_pshufhw"
  [(match_operand:V8HI 0 "register_operand" "")
   (match_operand:V8HI 1 "nonimmediate_operand" "")
   (match_operand:SI 2 "const_int_operand" "")]
  "TARGET_SSE2"
{
  int mask = INTVAL (operands[2]);
  emit_insn (gen_sse2_pshufhw_1 (operands[0], operands[1],
				 GEN_INT (((mask >> 0) & 3) + 4),
				 GEN_INT (((mask >> 2) & 3) + 4),
				 GEN_INT (((mask >> 4) & 3) + 4),
				 GEN_INT (((mask >> 6) & 3) + 4)));
  DONE;
})

(define_insn "sse2_pshufhw_1"
  [(set (match_operand:V8HI 0 "register_operand" "=x")
	(vec_select:V8HI
	  (match_operand:V8HI 1 "nonimmediate_operand" "xm")
	  (parallel [(const_int 0)
		     (const_int 1)
		     (const_int 2)
		     (const_int 3)
		     (match_operand 2 "const_4_to_7_operand" "")
		     (match_operand 3 "const_4_to_7_operand" "")
		     (match_operand 4 "const_4_to_7_operand" "")
		     (match_operand 5 "const_4_to_7_operand" "")])))]
  "TARGET_SSE2"
{
  int mask = 0;
  mask |= (INTVAL (operands[2]) - 4) << 0;
  mask |= (INTVAL (operands[3]) - 4) << 2;
  mask |= (INTVAL (operands[4]) - 4) << 4;
  mask |= (INTVAL (operands[5]) - 4) << 6;
  operands[2] = GEN_INT (mask);

  return "%vpshufhw\t{%2, %1, %0|%0, %1, %2}";
}
  [(set_attr "type" "sselog")
   (set_attr "prefix_rep" "1")
   (set_attr "prefix_data16" "0")
   (set_attr "prefix" "maybe_vex")
   (set_attr "length_immediate" "1")
   (set_attr "mode" "TI")])

(define_expand "sse2_loadd"
  [(set (match_operand:V4SI 0 "register_operand" "")
	(vec_merge:V4SI
	  (vec_duplicate:V4SI
	    (match_operand:SI 1 "nonimmediate_operand" ""))
	  (match_dup 2)
	  (const_int 1)))]
  "TARGET_SSE"
  "operands[2] = CONST0_RTX (V4SImode);")

(define_insn "*avx_loadld"
  [(set (match_operand:V4SI 0 "register_operand"       "=x,Yi,x")
	(vec_merge:V4SI
	  (vec_duplicate:V4SI
	    (match_operand:SI 2 "nonimmediate_operand" "m ,r ,x"))
	  (match_operand:V4SI 1 "reg_or_0_operand"     "C ,C ,x")
	  (const_int 1)))]
  "TARGET_AVX"
  "@
   vmovd\t{%2, %0|%0, %2}
   vmovd\t{%2, %0|%0, %2}
   vmovss\t{%2, %1, %0|%0, %1, %2}"
  [(set_attr "type" "ssemov")
   (set_attr "prefix" "vex")
   (set_attr "mode" "TI,TI,V4SF")])

(define_insn "sse2_loadld"
  [(set (match_operand:V4SI 0 "register_operand"       "=Y2,Yi,x,x")
	(vec_merge:V4SI
	  (vec_duplicate:V4SI
	    (match_operand:SI 2 "nonimmediate_operand" "m  ,r ,m,x"))
	  (match_operand:V4SI 1 "reg_or_0_operand"     "C  ,C ,C,0")
	  (const_int 1)))]
  "TARGET_SSE"
  "@
   movd\t{%2, %0|%0, %2}
   movd\t{%2, %0|%0, %2}
   movss\t{%2, %0|%0, %2}
   movss\t{%2, %0|%0, %2}"
  [(set_attr "type" "ssemov")
   (set_attr "mode" "TI,TI,V4SF,SF")])

(define_insn_and_split "sse2_stored"
  [(set (match_operand:SI 0 "nonimmediate_operand" "=mx,r")
	(vec_select:SI
	  (match_operand:V4SI 1 "register_operand" "x,Yi")
	  (parallel [(const_int 0)])))]
  "TARGET_SSE"
  "#"
  "&& reload_completed
   && (TARGET_INTER_UNIT_MOVES
       || MEM_P (operands [0])
       || !GENERAL_REGNO_P (true_regnum (operands [0])))"
  [(set (match_dup 0) (match_dup 1))]
  "operands[1] = gen_rtx_REG (SImode, REGNO (operands[1]));")

(define_insn_and_split "*vec_ext_v4si_mem"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(vec_select:SI
	  (match_operand:V4SI 1 "memory_operand" "o")
	  (parallel [(match_operand 2 "const_0_to_3_operand" "")])))]
  ""
  "#"
  "reload_completed"
  [(const_int 0)]
{
  int i = INTVAL (operands[2]);

  emit_move_insn (operands[0], adjust_address (operands[1], SImode, i*4));
  DONE;
})

(define_expand "sse_storeq"
  [(set (match_operand:DI 0 "nonimmediate_operand" "")
	(vec_select:DI
	  (match_operand:V2DI 1 "register_operand" "")
	  (parallel [(const_int 0)])))]
  "TARGET_SSE")

(define_insn "*sse2_storeq_rex64"
  [(set (match_operand:DI 0 "nonimmediate_operand" "=mx,*r,r")
	(vec_select:DI
	  (match_operand:V2DI 1 "nonimmediate_operand" "x,Yi,o")
	  (parallel [(const_int 0)])))]
  "TARGET_64BIT && !(MEM_P (operands[0]) && MEM_P (operands[1]))"
  "@
   #
   #
   mov{q}\t{%1, %0|%0, %1}"
  [(set_attr "type" "*,*,imov")
   (set_attr "mode" "*,*,DI")])

(define_insn "*sse2_storeq"
  [(set (match_operand:DI 0 "nonimmediate_operand" "=mx")
	(vec_select:DI
	  (match_operand:V2DI 1 "register_operand" "x")
	  (parallel [(const_int 0)])))]
  "TARGET_SSE"
  "#")

(define_split
  [(set (match_operand:DI 0 "nonimmediate_operand" "")
	(vec_select:DI
	  (match_operand:V2DI 1 "register_operand" "")
	  (parallel [(const_int 0)])))]
  "TARGET_SSE
   && reload_completed
   && (TARGET_INTER_UNIT_MOVES
       || MEM_P (operands [0])
       || !GENERAL_REGNO_P (true_regnum (operands [0])))"
  [(set (match_dup 0) (match_dup 1))]
  "operands[1] = gen_rtx_REG (DImode, REGNO (operands[1]));")

(define_insn "*vec_extractv2di_1_rex64_avx"
  [(set (match_operand:DI 0 "nonimmediate_operand" "=m,x,x,r")
	(vec_select:DI
	  (match_operand:V2DI 1 "nonimmediate_operand" "x,x,o,o")
	  (parallel [(const_int 1)])))]
  "TARGET_64BIT
   && TARGET_AVX
   && !(MEM_P (operands[0]) && MEM_P (operands[1]))"
  "@
   vmovhps\t{%1, %0|%0, %1}
   vpsrldq\t{$8, %1, %0|%0, %1, 8}
   vmovq\t{%H1, %0|%0, %H1}
   mov{q}\t{%H1, %0|%0, %H1}"
  [(set_attr "type" "ssemov,sseishft1,ssemov,imov")
   (set_attr "length_immediate" "*,1,*,*")
   (set_attr "memory" "*,none,*,*")
   (set_attr "prefix" "vex,vex,vex,orig")
   (set_attr "mode" "V2SF,TI,TI,DI")])

(define_insn "*vec_extractv2di_1_rex64"
  [(set (match_operand:DI 0 "nonimmediate_operand" "=m,x,x,r")
	(vec_select:DI
	  (match_operand:V2DI 1 "nonimmediate_operand" "x,0,o,o")
	  (parallel [(const_int 1)])))]
  "TARGET_64BIT && !(MEM_P (operands[0]) && MEM_P (operands[1]))"
  "@
   movhps\t{%1, %0|%0, %1}
   psrldq\t{$8, %0|%0, 8}
   movq\t{%H1, %0|%0, %H1}
   mov{q}\t{%H1, %0|%0, %H1}"
  [(set_attr "type" "ssemov,sseishft1,ssemov,imov")
   (set_attr "length_immediate" "*,1,*,*")
   (set_attr "memory" "*,none,*,*")
   (set_attr "mode" "V2SF,TI,TI,DI")])

(define_insn "*vec_extractv2di_1_avx"
  [(set (match_operand:DI 0 "nonimmediate_operand" "=m,x,x")
	(vec_select:DI
	  (match_operand:V2DI 1 "nonimmediate_operand" "x,x,o")
	  (parallel [(const_int 1)])))]
  "!TARGET_64BIT
   && TARGET_AVX
   && !(MEM_P (operands[0]) && MEM_P (operands[1]))"
  "@
   vmovhps\t{%1, %0|%0, %1}
   vpsrldq\t{$8, %1, %0|%0, %1, 8}
   vmovq\t{%H1, %0|%0, %H1}"
  [(set_attr "type" "ssemov,sseishft1,ssemov")
   (set_attr "length_immediate" "*,1,*")
   (set_attr "memory" "*,none,*")
   (set_attr "prefix" "vex")
   (set_attr "mode" "V2SF,TI,TI")])

(define_insn "*vec_extractv2di_1_sse2"
  [(set (match_operand:DI 0 "nonimmediate_operand" "=m,x,x")
	(vec_select:DI
	  (match_operand:V2DI 1 "nonimmediate_operand" "x,0,o")
	  (parallel [(const_int 1)])))]
  "!TARGET_64BIT
   && TARGET_SSE2 && !(MEM_P (operands[0]) && MEM_P (operands[1]))"
  "@
   movhps\t{%1, %0|%0, %1}
   psrldq\t{$8, %0|%0, 8}
   movq\t{%H1, %0|%0, %H1}"
  [(set_attr "type" "ssemov,sseishft1,ssemov")
   (set_attr "length_immediate" "*,1,*")
   (set_attr "memory" "*,none,*")
   (set_attr "mode" "V2SF,TI,TI")])

;; Not sure this is ever used, but it doesn't hurt to have it. -aoliva
(define_insn "*vec_extractv2di_1_sse"
  [(set (match_operand:DI 0 "nonimmediate_operand" "=m,x,x")
	(vec_select:DI
	  (match_operand:V2DI 1 "nonimmediate_operand" "x,x,o")
	  (parallel [(const_int 1)])))]
  "!TARGET_SSE2 && TARGET_SSE
   && !(MEM_P (operands[0]) && MEM_P (operands[1]))"
  "@
   movhps\t{%1, %0|%0, %1}
   movhlps\t{%1, %0|%0, %1}
   movlps\t{%H1, %0|%0, %H1}"
  [(set_attr "type" "ssemov")
   (set_attr "mode" "V2SF,V4SF,V2SF")])

(define_insn "*vec_dupv4si_avx"
  [(set (match_operand:V4SI 0 "register_operand" "=x,x")
	(vec_duplicate:V4SI
	  (match_operand:SI 1 "register_operand" "x,m")))]
  "TARGET_AVX"
  "@
   vpshufd\t{$0, %1, %0|%0, %1, 0}
   vbroadcastss\t{%1, %0|%0, %1}"
  [(set_attr "type" "sselog1,ssemov")
   (set_attr "length_immediate" "1,0")
   (set_attr "prefix_extra" "0,1")
   (set_attr "prefix" "vex")
   (set_attr "mode" "TI,V4SF")])

(define_insn "*vec_dupv4si"
  [(set (match_operand:V4SI 0 "register_operand" "=Y2,x")
	(vec_duplicate:V4SI
	  (match_operand:SI 1 "register_operand" " Y2,0")))]
  "TARGET_SSE"
  "@
   %vpshufd\t{$0, %1, %0|%0, %1, 0}
   shufps\t{$0, %0, %0|%0, %0, 0}"
  [(set_attr "type" "sselog1")
   (set_attr "length_immediate" "1")
   (set_attr "mode" "TI,V4SF")])

(define_insn "*vec_dupv2di_avx"
  [(set (match_operand:V2DI 0 "register_operand"     "=x,x")
	(vec_duplicate:V2DI
	  (match_operand:DI 1 "nonimmediate_operand" " x,m")))]
  "TARGET_AVX"
  "@
   vpunpcklqdq\t{%1, %1, %0|%0, %1, %1}
   vmovddup\t{%1, %0|%0, %1}"
  [(set_attr "type" "sselog1")
   (set_attr "prefix" "vex")
   (set_attr "mode" "TI,DF")])

(define_insn "*vec_dupv2di_sse3"
  [(set (match_operand:V2DI 0 "register_operand"     "=x,x")
	(vec_duplicate:V2DI
	  (match_operand:DI 1 "nonimmediate_operand" " 0,m")))]
  "TARGET_SSE3"
  "@
   punpcklqdq\t%0, %0
   movddup\t{%1, %0|%0, %1}"
  [(set_attr "type" "sselog1")
   (set_attr "mode" "TI,DF")])

(define_insn "*vec_dupv2di"
  [(set (match_operand:V2DI 0 "register_operand" "=Y2,x")
	(vec_duplicate:V2DI
	  (match_operand:DI 1 "register_operand" " 0 ,0")))]
  "TARGET_SSE"
  "@
   punpcklqdq\t%0, %0
   movlhps\t%0, %0"
  [(set_attr "type" "sselog1,ssemov")
   (set_attr "mode" "TI,V4SF")])

(define_insn "*vec_concatv2si_avx"
  [(set (match_operand:V2SI 0 "register_operand"     "=x,x,x ,*y ,*y")
	(vec_concat:V2SI
	  (match_operand:SI 1 "nonimmediate_operand" "x ,x,rm, 0 ,rm")
	  (match_operand:SI 2 "vector_move_operand"  "rm,x,C ,*ym,C")))]
  "TARGET_AVX"
  "@
   vpinsrd\t{$0x1, %2, %1, %0|%0, %1, %2, 0x1}
   vpunpckldq\t{%2, %1, %0|%0, %1, %2}
   vmovd\t{%1, %0|%0, %1}
   punpckldq\t{%2, %0|%0, %2}
   movd\t{%1, %0|%0, %1}"
  [(set_attr "type" "sselog,sselog,ssemov,mmxcvt,mmxmov")
   (set_attr "prefix_extra" "1,*,*,*,*")
   (set_attr "length_immediate" "1,*,*,*,*")
   (set (attr "prefix")
     (if_then_else (eq_attr "alternative" "3,4")
       (const_string "orig")
       (const_string "vex")))
   (set_attr "mode" "TI,TI,TI,DI,DI")])

(define_insn "*vec_concatv2si_sse4_1"
  [(set (match_operand:V2SI 0 "register_operand"     "=x,x,x ,*y ,*y")
	(vec_concat:V2SI
	  (match_operand:SI 1 "nonimmediate_operand" "0 ,0,rm, 0 ,rm")
	  (match_operand:SI 2 "vector_move_operand"  "rm,x,C ,*ym,C")))]
  "TARGET_SSE4_1"
  "@
   pinsrd\t{$0x1, %2, %0|%0, %2, 0x1}
   punpckldq\t{%2, %0|%0, %2}
   movd\t{%1, %0|%0, %1}
   punpckldq\t{%2, %0|%0, %2}
   movd\t{%1, %0|%0, %1}"
  [(set_attr "type" "sselog,sselog,ssemov,mmxcvt,mmxmov")
   (set_attr "prefix_extra" "1,*,*,*,*")
   (set_attr "length_immediate" "1,*,*,*,*")
   (set_attr "mode" "TI,TI,TI,DI,DI")])

;; ??? In theory we can match memory for the MMX alternative, but allowing
;; nonimmediate_operand for operand 2 and *not* allowing memory for the SSE
;; alternatives pretty much forces the MMX alternative to be chosen.
(define_insn "*vec_concatv2si_sse2"
  [(set (match_operand:V2SI 0 "register_operand"     "=x,x ,*y,*y")
	(vec_concat:V2SI
	  (match_operand:SI 1 "nonimmediate_operand" " 0,rm, 0,rm")
	  (match_operand:SI 2 "reg_or_0_operand"     " x,C ,*y, C")))]
  "TARGET_SSE2"
  "@
   punpckldq\t{%2, %0|%0, %2}
   movd\t{%1, %0|%0, %1}
   punpckldq\t{%2, %0|%0, %2}
   movd\t{%1, %0|%0, %1}"
  [(set_attr "type" "sselog,ssemov,mmxcvt,mmxmov")
   (set_attr "mode" "TI,TI,DI,DI")])

(define_insn "*vec_concatv2si_sse"
  [(set (match_operand:V2SI 0 "register_operand"     "=x,x,*y,*y")
	(vec_concat:V2SI
	  (match_operand:SI 1 "nonimmediate_operand" " 0,m, 0,*rm")
	  (match_operand:SI 2 "reg_or_0_operand"     " x,C,*y,C")))]
  "TARGET_SSE"
  "@
   unpcklps\t{%2, %0|%0, %2}
   movss\t{%1, %0|%0, %1}
   punpckldq\t{%2, %0|%0, %2}
   movd\t{%1, %0|%0, %1}"
  [(set_attr "type" "sselog,ssemov,mmxcvt,mmxmov")
   (set_attr "mode" "V4SF,V4SF,DI,DI")])

(define_insn "*vec_concatv4si_1_avx"
  [(set (match_operand:V4SI 0 "register_operand"       "=x,x")
	(vec_concat:V4SI
	  (match_operand:V2SI 1 "register_operand"     " x,x")
	  (match_operand:V2SI 2 "nonimmediate_operand" " x,m")))]
  "TARGET_AVX"
  "@
   vpunpcklqdq\t{%2, %1, %0|%0, %1, %2}
   vmovhps\t{%2, %1, %0|%0, %1, %2}"
  [(set_attr "type" "sselog,ssemov")
   (set_attr "prefix" "vex")
   (set_attr "mode" "TI,V2SF")])

(define_insn "*vec_concatv4si_1"
  [(set (match_operand:V4SI 0 "register_operand"       "=Y2,x,x")
	(vec_concat:V4SI
	  (match_operand:V2SI 1 "register_operand"     " 0 ,0,0")
	  (match_operand:V2SI 2 "nonimmediate_operand" " Y2,x,m")))]
  "TARGET_SSE"
  "@
   punpcklqdq\t{%2, %0|%0, %2}
   movlhps\t{%2, %0|%0, %2}
   movhps\t{%2, %0|%0, %2}"
  [(set_attr "type" "sselog,ssemov,ssemov")
   (set_attr "mode" "TI,V4SF,V2SF")])

(define_insn "*vec_concatv2di_avx"
  [(set (match_operand:V2DI 0 "register_operand"     "=x,?x,x,x")
	(vec_concat:V2DI
	  (match_operand:DI 1 "nonimmediate_operand" " m,*y,x,x")
	  (match_operand:DI 2 "vector_move_operand"  " C, C,x,m")))]
  "!TARGET_64BIT && TARGET_AVX"
  "@
   vmovq\t{%1, %0|%0, %1}
   movq2dq\t{%1, %0|%0, %1}
   vpunpcklqdq\t{%2, %1, %0|%0, %1, %2}
   vmovhps\t{%2, %1, %0|%0, %1, %2}"
  [(set_attr "type" "ssemov,ssemov,sselog,ssemov")
   (set (attr "prefix")
     (if_then_else (eq_attr "alternative" "1")
       (const_string "orig")
       (const_string "vex")))
   (set_attr "mode" "TI,TI,TI,V2SF")])

(define_insn "vec_concatv2di"
  [(set (match_operand:V2DI 0 "register_operand"     "=Y2 ,?Y2,Y2,x,x")
	(vec_concat:V2DI
	  (match_operand:DI 1 "nonimmediate_operand" " mY2,*y ,0 ,0,0")
	  (match_operand:DI 2 "vector_move_operand"  " C  ,  C,Y2,x,m")))]
  "!TARGET_64BIT && TARGET_SSE"
  "@
   movq\t{%1, %0|%0, %1}
   movq2dq\t{%1, %0|%0, %1}
   punpcklqdq\t{%2, %0|%0, %2}
   movlhps\t{%2, %0|%0, %2}
   movhps\t{%2, %0|%0, %2}"
  [(set_attr "type" "ssemov,ssemov,sselog,ssemov,ssemov")
   (set_attr "mode" "TI,TI,TI,V4SF,V2SF")])

(define_insn "*vec_concatv2di_rex64_avx"
  [(set (match_operand:V2DI 0 "register_operand"     "=x,x,Yi,!x,x,x")
	(vec_concat:V2DI
	  (match_operand:DI 1 "nonimmediate_operand" " x,m,r ,*y,x,x")
	  (match_operand:DI 2 "vector_move_operand"  "rm,C,C ,C ,x,m")))]
  "TARGET_64BIT && TARGET_AVX"
  "@
   vpinsrq\t{$0x1, %2, %1, %0|%0, %1, %2, 0x1}
   vmovq\t{%1, %0|%0, %1}
   vmovq\t{%1, %0|%0, %1}
   movq2dq\t{%1, %0|%0, %1}
   vpunpcklqdq\t{%2, %1, %0|%0, %1, %2}
   vmovhps\t{%2, %1, %0|%0, %1, %2}"
  [(set_attr "type" "sselog,ssemov,ssemov,ssemov,sselog,ssemov")
   (set_attr "prefix_extra" "1,*,*,*,*,*")
   (set_attr "length_immediate" "1,*,*,*,*,*")
   (set (attr "prefix")
     (if_then_else (eq_attr "alternative" "3")
       (const_string "orig")
       (const_string "vex")))
   (set_attr "mode" "TI,TI,TI,TI,TI,V2SF")])

;; movd instead of movq is required to handle broken assemblers.
(define_insn "*vec_concatv2di_rex64_sse4_1"
  [(set (match_operand:V2DI 0 "register_operand"     "=x ,x ,Yi,!x,x,x,x")
	(vec_concat:V2DI
	  (match_operand:DI 1 "nonimmediate_operand" " 0 ,mx,r ,*y,0,0,0")
	  (match_operand:DI 2 "vector_move_operand"  " rm,C ,C ,C ,x,x,m")))]
  "TARGET_64BIT && TARGET_SSE4_1"
  "@
   pinsrq\t{$0x1, %2, %0|%0, %2, 0x1}
   movq\t{%1, %0|%0, %1}
   movd\t{%1, %0|%0, %1}
   movq2dq\t{%1, %0|%0, %1}
   punpcklqdq\t{%2, %0|%0, %2}
   movlhps\t{%2, %0|%0, %2}
   movhps\t{%2, %0|%0, %2}"
  [(set_attr "type" "sselog,ssemov,ssemov,ssemov,sselog,ssemov,ssemov")
   (set_attr "prefix_rex" "1,*,1,*,*,*,*")
   (set_attr "prefix_extra" "1,*,*,*,*,*,*")
   (set_attr "length_immediate" "1,*,*,*,*,*,*")
   (set_attr "mode" "TI,TI,TI,TI,TI,V4SF,V2SF")])

;; movd instead of movq is required to handle broken assemblers.
(define_insn "*vec_concatv2di_rex64_sse"
  [(set (match_operand:V2DI 0 "register_operand"     "=Y2 ,Yi,!Y2,Y2,x,x")
	(vec_concat:V2DI
	  (match_operand:DI 1 "nonimmediate_operand" " mY2,r ,*y ,0 ,0,0")
	  (match_operand:DI 2 "vector_move_operand"  " C  ,C ,C  ,Y2,x,m")))]
  "TARGET_64BIT && TARGET_SSE"
  "@
   movq\t{%1, %0|%0, %1}
   movd\t{%1, %0|%0, %1}
   movq2dq\t{%1, %0|%0, %1}
   punpcklqdq\t{%2, %0|%0, %2}
   movlhps\t{%2, %0|%0, %2}
   movhps\t{%2, %0|%0, %2}"
  [(set_attr "type" "ssemov,ssemov,ssemov,sselog,ssemov,ssemov")
   (set_attr "prefix_rex" "*,1,*,*,*,*")
   (set_attr "mode" "TI,TI,TI,TI,V4SF,V2SF")])

(define_expand "vec_unpacku_hi_v16qi"
  [(match_operand:V8HI 0 "register_operand" "")
   (match_operand:V16QI 1 "register_operand" "")]
  "TARGET_SSE2"
{
  if (TARGET_SSE4_1)
    ix86_expand_sse4_unpack (operands, true, true);
  else
    ix86_expand_sse_unpack (operands, true, true);
  DONE;
})

(define_expand "vec_unpacks_hi_v16qi"
  [(match_operand:V8HI 0 "register_operand" "")
   (match_operand:V16QI 1 "register_operand" "")]
  "TARGET_SSE2"
{
  if (TARGET_SSE4_1)
    ix86_expand_sse4_unpack (operands, false, true);
  else
    ix86_expand_sse_unpack (operands, false, true);
  DONE;
})

(define_expand "vec_unpacku_lo_v16qi"
  [(match_operand:V8HI 0 "register_operand" "")
   (match_operand:V16QI 1 "register_operand" "")]
  "TARGET_SSE2"
{
  if (TARGET_SSE4_1)
    ix86_expand_sse4_unpack (operands, true, false);
  else
    ix86_expand_sse_unpack (operands, true, false);
  DONE;
})

(define_expand "vec_unpacks_lo_v16qi"
  [(match_operand:V8HI 0 "register_operand" "")
   (match_operand:V16QI 1 "register_operand" "")]
  "TARGET_SSE2"
{
  if (TARGET_SSE4_1)
    ix86_expand_sse4_unpack (operands, false, false);
  else
    ix86_expand_sse_unpack (operands, false, false);
  DONE;
})

(define_expand "vec_unpacku_hi_v8hi"
  [(match_operand:V4SI 0 "register_operand" "")
   (match_operand:V8HI 1 "register_operand" "")]
  "TARGET_SSE2"
{
  if (TARGET_SSE4_1)
    ix86_expand_sse4_unpack (operands, true, true);
  else
    ix86_expand_sse_unpack (operands, true, true);
  DONE;
})

(define_expand "vec_unpacks_hi_v8hi"
  [(match_operand:V4SI 0 "register_operand" "")
   (match_operand:V8HI 1 "register_operand" "")]
  "TARGET_SSE2"
{
  if (TARGET_SSE4_1)
    ix86_expand_sse4_unpack (operands, false, true);
  else
    ix86_expand_sse_unpack (operands, false, true);
  DONE;
})

(define_expand "vec_unpacku_lo_v8hi"
  [(match_operand:V4SI 0 "register_operand" "")
   (match_operand:V8HI 1 "register_operand" "")]
  "TARGET_SSE2"
{
  if (TARGET_SSE4_1)
    ix86_expand_sse4_unpack (operands, true, false);
  else
    ix86_expand_sse_unpack (operands, true, false);
  DONE;
})

(define_expand "vec_unpacks_lo_v8hi"
  [(match_operand:V4SI 0 "register_operand" "")
   (match_operand:V8HI 1 "register_operand" "")]
  "TARGET_SSE2"
{
  if (TARGET_SSE4_1)
    ix86_expand_sse4_unpack (operands, false, false);
  else
    ix86_expand_sse_unpack (operands, false, false);
  DONE;
})

(define_expand "vec_unpacku_hi_v4si"
  [(match_operand:V2DI 0 "register_operand" "")
   (match_operand:V4SI 1 "register_operand" "")]
  "TARGET_SSE2"
{
  if (TARGET_SSE4_1)
    ix86_expand_sse4_unpack (operands, true, true);
  else
    ix86_expand_sse_unpack (operands, true, true);
  DONE;
})

(define_expand "vec_unpacks_hi_v4si"
  [(match_operand:V2DI 0 "register_operand" "")
   (match_operand:V4SI 1 "register_operand" "")]
  "TARGET_SSE2"
{
  if (TARGET_SSE4_1)
    ix86_expand_sse4_unpack (operands, false, true);
  else
    ix86_expand_sse_unpack (operands, false, true);
  DONE;
})

(define_expand "vec_unpacku_lo_v4si"
  [(match_operand:V2DI 0 "register_operand" "")
   (match_operand:V4SI 1 "register_operand" "")]
  "TARGET_SSE2"
{
  if (TARGET_SSE4_1)
    ix86_expand_sse4_unpack (operands, true, false);
  else
    ix86_expand_sse_unpack (operands, true, false);
  DONE;
})

(define_expand "vec_unpacks_lo_v4si"
  [(match_operand:V2DI 0 "register_operand" "")
   (match_operand:V4SI 1 "register_operand" "")]
  "TARGET_SSE2"
{
  if (TARGET_SSE4_1)
    ix86_expand_sse4_unpack (operands, false, false);
  else
    ix86_expand_sse_unpack (operands, false, false);
  DONE;
})

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Miscellaneous
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define_expand "sse2_uavgv16qi3"
  [(set (match_operand:V16QI 0 "register_operand" "")
	(truncate:V16QI
	  (lshiftrt:V16HI
	    (plus:V16HI
	      (plus:V16HI
		(zero_extend:V16HI
		  (match_operand:V16QI 1 "nonimmediate_operand" ""))
		(zero_extend:V16HI
		  (match_operand:V16QI 2 "nonimmediate_operand" "")))
	      (const_vector:V16QI [(const_int 1) (const_int 1)
				   (const_int 1) (const_int 1)
				   (const_int 1) (const_int 1)
				   (const_int 1) (const_int 1)
				   (const_int 1) (const_int 1)
				   (const_int 1) (const_int 1)
				   (const_int 1) (const_int 1)
				   (const_int 1) (const_int 1)]))
	    (const_int 1))))]
  "TARGET_SSE2"
  "ix86_fixup_binary_operands_no_copy (PLUS, V16QImode, operands);")

(define_insn "*avx_uavgv16qi3"
  [(set (match_operand:V16QI 0 "register_operand" "=x")
	(truncate:V16QI
	  (lshiftrt:V16HI
	    (plus:V16HI
	      (plus:V16HI
		(zero_extend:V16HI
		  (match_operand:V16QI 1 "nonimmediate_operand" "%x"))
		(zero_extend:V16HI
		  (match_operand:V16QI 2 "nonimmediate_operand" "xm")))
	      (const_vector:V16QI [(const_int 1) (const_int 1)
				   (const_int 1) (const_int 1)
				   (const_int 1) (const_int 1)
				   (const_int 1) (const_int 1)
				   (const_int 1) (const_int 1)
				   (const_int 1) (const_int 1)
				   (const_int 1) (const_int 1)
				   (const_int 1) (const_int 1)]))
	    (const_int 1))))]
  "TARGET_AVX && ix86_binary_operator_ok (PLUS, V16QImode, operands)"
  "vpavgb\t{%2, %1, %0|%0, %1, %2}"
  [(set_attr "type" "sseiadd")
   (set_attr "prefix" "vex")
   (set_attr "mode" "TI")])

(define_insn "*sse2_uavgv16qi3"
  [(set (match_operand:V16QI 0 "register_operand" "=x")
	(truncate:V16QI
	  (lshiftrt:V16HI
	    (plus:V16HI
	      (plus:V16HI
		(zero_extend:V16HI
		  (match_operand:V16QI 1 "nonimmediate_operand" "%0"))
		(zero_extend:V16HI
		  (match_operand:V16QI 2 "nonimmediate_operand" "xm")))
	      (const_vector:V16QI [(const_int 1) (const_int 1)
				   (const_int 1) (const_int 1)
				   (const_int 1) (const_int 1)
				   (const_int 1) (const_int 1)
				   (const_int 1) (const_int 1)
				   (const_int 1) (const_int 1)
				   (const_int 1) (const_int 1)
				   (const_int 1) (const_int 1)]))
	    (const_int 1))))]
  "TARGET_SSE2 && ix86_binary_operator_ok (PLUS, V16QImode, operands)"
  "pavgb\t{%2, %0|%0, %2}"
  [(set_attr "type" "sseiadd")
   (set_attr "prefix_data16" "1")
   (set_attr "mode" "TI")])

(define_expand "sse2_uavgv8hi3"
  [(set (match_operand:V8HI 0 "register_operand" "")
	(truncate:V8HI
	  (lshiftrt:V8SI
	    (plus:V8SI
	      (plus:V8SI
		(zero_extend:V8SI
		  (match_operand:V8HI 1 "nonimmediate_operand" ""))
		(zero_extend:V8SI
		  (match_operand:V8HI 2 "nonimmediate_operand" "")))
	      (const_vector:V8HI [(const_int 1) (const_int 1)
				  (const_int 1) (const_int 1)
				  (const_int 1) (const_int 1)
				  (const_int 1) (const_int 1)]))
	    (const_int 1))))]
  "TARGET_SSE2"
  "ix86_fixup_binary_operands_no_copy (PLUS, V8HImode, operands);")

(define_insn "*avx_uavgv8hi3"
  [(set (match_operand:V8HI 0 "register_operand" "=x")
	(truncate:V8HI
	  (lshiftrt:V8SI
	    (plus:V8SI
	      (plus:V8SI
		(zero_extend:V8SI
		  (match_operand:V8HI 1 "nonimmediate_operand" "%x"))
		(zero_extend:V8SI
		  (match_operand:V8HI 2 "nonimmediate_operand" "xm")))
	      (const_vector:V8HI [(const_int 1) (const_int 1)
				  (const_int 1) (const_int 1)
				  (const_int 1) (const_int 1)
				  (const_int 1) (const_int 1)]))
	    (const_int 1))))]
  "TARGET_AVX && ix86_binary_operator_ok (PLUS, V8HImode, operands)"
  "vpavgw\t{%2, %1, %0|%0, %1, %2}"
  [(set_attr "type" "sseiadd")
   (set_attr "prefix" "vex")
   (set_attr "mode" "TI")])

(define_insn "*sse2_uavgv8hi3"
  [(set (match_operand:V8HI 0 "register_operand" "=x")
	(truncate:V8HI
	  (lshiftrt:V8SI
	    (plus:V8SI
	      (plus:V8SI
		(zero_extend:V8SI
		  (match_operand:V8HI 1 "nonimmediate_operand" "%0"))
		(zero_extend:V8SI
		  (match_operand:V8HI 2 "nonimmediate_operand" "xm")))
	      (const_vector:V8HI [(const_int 1) (const_int 1)
				  (const_int 1) (const_int 1)
				  (const_int 1) (const_int 1)
				  (const_int 1) (const_int 1)]))
	    (const_int 1))))]
  "TARGET_SSE2 && ix86_binary_operator_ok (PLUS, V8HImode, operands)"
  "pavgw\t{%2, %0|%0, %2}"
  [(set_attr "type" "sseiadd")
   (set_attr "prefix_data16" "1")
   (set_attr "mode" "TI")])

;; The correct representation for this is absolutely enormous, and
;; surely not generally useful.
(define_insn "*avx_psadbw"
  [(set (match_operand:V2DI 0 "register_operand" "=x")
	(unspec:V2DI [(match_operand:V16QI 1 "register_operand" "x")
		      (match_operand:V16QI 2 "nonimmediate_operand" "xm")]
		     UNSPEC_PSADBW))]
  "TARGET_AVX"
  "vpsadbw\t{%2, %1, %0|%0, %1, %2}"
  [(set_attr "type" "sseiadd")
   (set_attr "prefix" "vex")
   (set_attr "mode" "TI")])

(define_insn "sse2_psadbw"
  [(set (match_operand:V2DI 0 "register_operand" "=x")
	(unspec:V2DI [(match_operand:V16QI 1 "register_operand" "0")
		      (match_operand:V16QI 2 "nonimmediate_operand" "xm")]
		     UNSPEC_PSADBW))]
  "TARGET_SSE2"
  "psadbw\t{%2, %0|%0, %2}"
  [(set_attr "type" "sseiadd")
   (set_attr "atom_unit" "simul")
   (set_attr "prefix_data16" "1")
   (set_attr "mode" "TI")])

(define_insn "avx_movmsk<ssemodesuffix>256"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(unspec:SI
	  [(match_operand:AVX256MODEF2P 1 "register_operand" "x")]
	  UNSPEC_MOVMSK))]
  "AVX256_VEC_FLOAT_MODE_P (<MODE>mode)"
  "vmovmsk<ssemodesuffix>\t{%1, %0|%0, %1}"
  [(set_attr "type" "ssecvt")
   (set_attr "prefix" "vex")
   (set_attr "mode" "<MODE>")])

(define_insn "<sse>_movmsk<ssemodesuffix>"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(unspec:SI
	  [(match_operand:SSEMODEF2P 1 "register_operand" "x")]
	  UNSPEC_MOVMSK))]
  "SSE_VEC_FLOAT_MODE_P (<MODE>mode)"
  "%vmovmsk<ssemodesuffix>\t{%1, %0|%0, %1}"
  [(set_attr "type" "ssemov")
   (set_attr "prefix" "maybe_vex")
   (set_attr "mode" "<MODE>")])

(define_insn "sse2_pmovmskb"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(unspec:SI [(match_operand:V16QI 1 "register_operand" "x")]
		   UNSPEC_MOVMSK))]
  "TARGET_SSE2"
  "%vpmovmskb\t{%1, %0|%0, %1}"
  [(set_attr "type" "ssemov")
   (set_attr "prefix_data16" "1")
   (set_attr "prefix" "maybe_vex")
   (set_attr "mode" "SI")])

(define_expand "sse2_maskmovdqu"
  [(set (match_operand:V16QI 0 "memory_operand" "")
	(unspec:V16QI [(match_operand:V16QI 1 "register_operand" "")
		       (match_operand:V16QI 2 "register_operand" "")
		       (match_dup 0)]
		      UNSPEC_MASKMOV))]
  "TARGET_SSE2")

(define_insn "*sse2_maskmovdqu"
  [(set (mem:V16QI (match_operand:SI 0 "register_operand" "D"))
	(unspec:V16QI [(match_operand:V16QI 1 "register_operand" "x")
		       (match_operand:V16QI 2 "register_operand" "x")
		       (mem:V16QI (match_dup 0))]
		      UNSPEC_MASKMOV))]
  "TARGET_SSE2 && !TARGET_64BIT"
  ;; @@@ check ordering of operands in intel/nonintel syntax
  "%vmaskmovdqu\t{%2, %1|%1, %2}"
  [(set_attr "type" "ssemov")
   (set_attr "prefix_data16" "1")
   ;; The implicit %rdi operand confuses default length_vex computation.
   (set_attr "length_vex" "3")
   (set_attr "prefix" "maybe_vex")
   (set_attr "mode" "TI")])

(define_insn "*sse2_maskmovdqu_rex64"
  [(set (mem:V16QI (match_operand:DI 0 "register_operand" "D"))
	(unspec:V16QI [(match_operand:V16QI 1 "register_operand" "x")
		       (match_operand:V16QI 2 "register_operand" "x")
		       (mem:V16QI (match_dup 0))]
		      UNSPEC_MASKMOV))]
  "TARGET_SSE2 && TARGET_64BIT"
  ;; @@@ check ordering of operands in intel/nonintel syntax
  "%vmaskmovdqu\t{%2, %1|%1, %2}"
  [(set_attr "type" "ssemov")
   (set_attr "prefix_data16" "1")
   ;; The implicit %rdi operand confuses default length_vex computation.
   (set (attr "length_vex")
     (symbol_ref ("REGNO (operands[2]) >= FIRST_REX_SSE_REG ? 3 + 1 : 2 + 1")))
   (set_attr "prefix" "maybe_vex")
   (set_attr "mode" "TI")])

(define_insn "sse_ldmxcsr"
  [(unspec_volatile [(match_operand:SI 0 "memory_operand" "m")]
		    UNSPECV_LDMXCSR)]
  "TARGET_SSE"
  "%vldmxcsr\t%0"
  [(set_attr "type" "sse")
   (set_attr "atom_sse_attr" "mxcsr")
   (set_attr "prefix" "maybe_vex")
   (set_attr "memory" "load")])

(define_insn "sse_stmxcsr"
  [(set (match_operand:SI 0 "memory_operand" "=m")
	(unspec_volatile:SI [(const_int 0)] UNSPECV_STMXCSR))]
  "TARGET_SSE"
  "%vstmxcsr\t%0"
  [(set_attr "type" "sse")
   (set_attr "atom_sse_attr" "mxcsr")
   (set_attr "prefix" "maybe_vex")
   (set_attr "memory" "store")])

(define_expand "sse_sfence"
  [(set (match_dup 0)
	(unspec:BLK [(match_dup 0)] UNSPEC_SFENCE))]
  "TARGET_SSE || TARGET_3DNOW_A"
{
  operands[0] = gen_rtx_MEM (BLKmode, gen_rtx_SCRATCH (Pmode));
  MEM_VOLATILE_P (operands[0]) = 1;
})

(define_insn "*sse_sfence"
  [(set (match_operand:BLK 0 "" "")
	(unspec:BLK [(match_dup 0)] UNSPEC_SFENCE))]
  "TARGET_SSE || TARGET_3DNOW_A"
  "sfence"
  [(set_attr "type" "sse")
   (set_attr "length_address" "0")
   (set_attr "atom_sse_attr" "fence")
   (set_attr "memory" "unknown")])

(define_insn "sse2_clflush"
  [(unspec_volatile [(match_operand 0 "address_operand" "p")]
		    UNSPECV_CLFLUSH)]
  "TARGET_SSE2"
  "clflush\t%a0"
  [(set_attr "type" "sse")
   (set_attr "atom_sse_attr" "fence")
   (set_attr "memory" "unknown")])

(define_expand "sse2_mfence"
  [(set (match_dup 0)
	(unspec:BLK [(match_dup 0)] UNSPEC_MFENCE))]
  "TARGET_SSE2"
{
  operands[0] = gen_rtx_MEM (BLKmode, gen_rtx_SCRATCH (Pmode));
  MEM_VOLATILE_P (operands[0]) = 1;
})

(define_insn "*sse2_mfence"
  [(set (match_operand:BLK 0 "" "")
	(unspec:BLK [(match_dup 0)] UNSPEC_MFENCE))]
  "TARGET_64BIT || TARGET_SSE2"
  "mfence"
  [(set_attr "type" "sse")
   (set_attr "length_address" "0")
   (set_attr "atom_sse_attr" "fence")
   (set_attr "memory" "unknown")])

(define_expand "sse2_lfence"
  [(set (match_dup 0)
	(unspec:BLK [(match_dup 0)] UNSPEC_LFENCE))]
  "TARGET_SSE2"
{
  operands[0] = gen_rtx_MEM (BLKmode, gen_rtx_SCRATCH (Pmode));
  MEM_VOLATILE_P (operands[0]) = 1;
})

(define_insn "*sse2_lfence"
  [(set (match_operand:BLK 0 "" "")
	(unspec:BLK [(match_dup 0)] UNSPEC_LFENCE))]
  "TARGET_SSE2"
  "lfence"
  [(set_attr "type" "sse")
   (set_attr "length_address" "0")
   (set_attr "atom_sse_attr" "lfence")
   (set_attr "memory" "unknown")])

(define_insn "sse3_mwait"
  [(unspec_volatile [(match_operand:SI 0 "register_operand" "a")
		     (match_operand:SI 1 "register_operand" "c")]
		    UNSPECV_MWAIT)]
  "TARGET_SSE3"
;; 64bit version is "mwait %rax,%rcx". But only lower 32bits are used.
;; Since 32bit register operands are implicitly zero extended to 64bit,
;; we only need to set up 32bit registers.
  "mwait"
  [(set_attr "length" "3")])

(define_insn "sse3_monitor"
  [(unspec_volatile [(match_operand:SI 0 "register_operand" "a")
		     (match_operand:SI 1 "register_operand" "c")
		     (match_operand:SI 2 "register_operand" "d")]
		    UNSPECV_MONITOR)]
  "TARGET_SSE3 && !TARGET_64BIT"
  "monitor\t%0, %1, %2"
  [(set_attr "length" "3")])

(define_insn "sse3_monitor64"
  [(unspec_volatile [(match_operand:DI 0 "register_operand" "a")
		     (match_operand:SI 1 "register_operand" "c")
		     (match_operand:SI 2 "register_operand" "d")]
		    UNSPECV_MONITOR)]
  "TARGET_SSE3 && TARGET_64BIT"
;; 64bit version is "monitor %rax,%rcx,%rdx". But only lower 32bits in
;; RCX and RDX are used.  Since 32bit register operands are implicitly
;; zero extended to 64bit, we only need to set up 32bit registers.
  "monitor"
  [(set_attr "length" "3")])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; SSSE3 instructions
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define_insn "*avx_phaddwv8hi3"
  [(set (match_operand:V8HI 0 "register_operand" "=x")
	(vec_concat:V8HI
	  (vec_concat:V4HI
	    (vec_concat:V2HI
	      (plus:HI
		(vec_select:HI
		  (match_operand:V8HI 1 "register_operand" "x")
		  (parallel [(const_int 0)]))
		(vec_select:HI (match_dup 1) (parallel [(const_int 1)])))
	      (plus:HI
		(vec_select:HI (match_dup 1) (parallel [(const_int 2)]))
		(vec_select:HI (match_dup 1) (parallel [(const_int 3)]))))
	    (vec_concat:V2HI
	      (plus:HI
		(vec_select:HI (match_dup 1) (parallel [(const_int 4)]))
		(vec_select:HI (match_dup 1) (parallel [(const_int 5)])))
	      (plus:HI
		(vec_select:HI (match_dup 1) (parallel [(const_int 6)]))
		(vec_select:HI (match_dup 1) (parallel [(const_int 7)])))))
	  (vec_concat:V4HI
	    (vec_concat:V2HI
	      (plus:HI
		(vec_select:HI
		  (match_operand:V8HI 2 "nonimmediate_operand" "xm")
		  (parallel [(const_int 0)]))
		(vec_select:HI (match_dup 2) (parallel [(const_int 1)])))
	      (plus:HI
		(vec_select:HI (match_dup 2) (parallel [(const_int 2)]))
		(vec_select:HI (match_dup 2) (parallel [(const_int 3)]))))
	    (vec_concat:V2HI
	      (plus:HI
		(vec_select:HI (match_dup 2) (parallel [(const_int 4)]))
		(vec_select:HI (match_dup 2) (parallel [(const_int 5)])))
	      (plus:HI
		(vec_select:HI (match_dup 2) (parallel [(const_int 6)]))
		(vec_select:HI (match_dup 2) (parallel [(const_int 7)])))))))]
  "TARGET_AVX"
  "vphaddw\t{%2, %1, %0|%0, %1, %2}"
  [(set_attr "type" "sseiadd")
   (set_attr "prefix_extra" "1")
   (set_attr "prefix" "vex")
   (set_attr "mode" "TI")])

(define_insn "ssse3_phaddwv8hi3"
  [(set (match_operand:V8HI 0 "register_operand" "=x")
	(vec_concat:V8HI
	  (vec_concat:V4HI
	    (vec_concat:V2HI
	      (plus:HI
		(vec_select:HI
		  (match_operand:V8HI 1 "register_operand" "0")
		  (parallel [(const_int 0)]))
		(vec_select:HI (match_dup 1) (parallel [(const_int 1)])))
	      (plus:HI
		(vec_select:HI (match_dup 1) (parallel [(const_int 2)]))
		(vec_select:HI (match_dup 1) (parallel [(const_int 3)]))))
	    (vec_concat:V2HI
	      (plus:HI
		(vec_select:HI (match_dup 1) (parallel [(const_int 4)]))
		(vec_select:HI (match_dup 1) (parallel [(const_int 5)])))
	      (plus:HI
		(vec_select:HI (match_dup 1) (parallel [(const_int 6)]))
		(vec_select:HI (match_dup 1) (parallel [(const_int 7)])))))
	  (vec_concat:V4HI
	    (vec_concat:V2HI
	      (plus:HI
		(vec_select:HI
		  (match_operand:V8HI 2 "nonimmediate_operand" "xm")
		  (parallel [(const_int 0)]))
		(vec_select:HI (match_dup 2) (parallel [(const_int 1)])))
	      (plus:HI
		(vec_select:HI (match_dup 2) (parallel [(const_int 2)]))
		(vec_select:HI (match_dup 2) (parallel [(const_int 3)]))))
	    (vec_concat:V2HI
	      (plus:HI
		(vec_select:HI (match_dup 2) (parallel [(const_int 4)]))
		(vec_select:HI (match_dup 2) (parallel [(const_int 5)])))
	      (plus:HI
		(vec_select:HI (match_dup 2) (parallel [(const_int 6)]))
		(vec_select:HI (match_dup 2) (parallel [(const_int 7)])))))))]
  "TARGET_SSSE3"
  "phaddw\t{%2, %0|%0, %2}"
  [(set_attr "type" "sseiadd")
   (set_attr "atom_unit" "complex")
   (set_attr "prefix_data16" "1")
   (set_attr "prefix_extra" "1")
   (set_attr "mode" "TI")])

(define_insn "ssse3_phaddwv4hi3"
  [(set (match_operand:V4HI 0 "register_operand" "=y")
	(vec_concat:V4HI
	  (vec_concat:V2HI
	    (plus:HI
	      (vec_select:HI
		(match_operand:V4HI 1 "register_operand" "0")
		(parallel [(const_int 0)]))
	      (vec_select:HI (match_dup 1) (parallel [(const_int 1)])))
	    (plus:HI
	      (vec_select:HI (match_dup 1) (parallel [(const_int 2)]))
	      (vec_select:HI (match_dup 1) (parallel [(const_int 3)]))))
	  (vec_concat:V2HI
	    (plus:HI
	      (vec_select:HI
		(match_operand:V4HI 2 "nonimmediate_operand" "ym")
		(parallel [(const_int 0)]))
	      (vec_select:HI (match_dup 2) (parallel [(const_int 1)])))
	    (plus:HI
	      (vec_select:HI (match_dup 2) (parallel [(const_int 2)]))
	      (vec_select:HI (match_dup 2) (parallel [(const_int 3)]))))))]
  "TARGET_SSSE3"
  "phaddw\t{%2, %0|%0, %2}"
  [(set_attr "type" "sseiadd")
   (set_attr "atom_unit" "complex")
   (set_attr "prefix_extra" "1")
   (set (attr "prefix_rex") (symbol_ref "x86_extended_reg_mentioned_p (insn)"))
   (set_attr "mode" "DI")])

(define_insn "*avx_phadddv4si3"
  [(set (match_operand:V4SI 0 "register_operand" "=x")
	(vec_concat:V4SI
	  (vec_concat:V2SI
	    (plus:SI
	      (vec_select:SI
		(match_operand:V4SI 1 "register_operand" "x")
		(parallel [(const_int 0)]))
	      (vec_select:SI (match_dup 1) (parallel [(const_int 1)])))
	    (plus:SI
	      (vec_select:SI (match_dup 1) (parallel [(const_int 2)]))
	      (vec_select:SI (match_dup 1) (parallel [(const_int 3)]))))
	  (vec_concat:V2SI
	    (plus:SI
	      (vec_select:SI
		(match_operand:V4SI 2 "nonimmediate_operand" "xm")
		(parallel [(const_int 0)]))
	      (vec_select:SI (match_dup 2) (parallel [(const_int 1)])))
	    (plus:SI
	      (vec_select:SI (match_dup 2) (parallel [(const_int 2)]))
	      (vec_select:SI (match_dup 2) (parallel [(const_int 3)]))))))]
  "TARGET_AVX"
  "vphaddd\t{%2, %1, %0|%0, %1, %2}"
  [(set_attr "type" "sseiadd")
   (set_attr "prefix_extra" "1")
   (set_attr "prefix" "vex")
   (set_attr "mode" "TI")])

(define_insn "ssse3_phadddv4si3"
  [(set (match_operand:V4SI 0 "register_operand" "=x")
	(vec_concat:V4SI
	  (vec_concat:V2SI
	    (plus:SI
	      (vec_select:SI
		(match_operand:V4SI 1 "register_operand" "0")
		(parallel [(const_int 0)]))
	      (vec_select:SI (match_dup 1) (parallel [(const_int 1)])))
	    (plus:SI
	      (vec_select:SI (match_dup 1) (parallel [(const_int 2)]))
	      (vec_select:SI (match_dup 1) (parallel [(const_int 3)]))))
	  (vec_concat:V2SI
	    (plus:SI
	      (vec_select:SI
		(match_operand:V4SI 2 "nonimmediate_operand" "xm")
		(parallel [(const_int 0)]))
	      (vec_select:SI (match_dup 2) (parallel [(const_int 1)])))
	    (plus:SI
	      (vec_select:SI (match_dup 2) (parallel [(const_int 2)]))
	      (vec_select:SI (match_dup 2) (parallel [(const_int 3)]))))))]
  "TARGET_SSSE3"
  "phaddd\t{%2, %0|%0, %2}"
  [(set_attr "type" "sseiadd")
   (set_attr "atom_unit" "complex")
   (set_attr "prefix_data16" "1")
   (set_attr "prefix_extra" "1")
   (set_attr "mode" "TI")])

(define_insn "ssse3_phadddv2si3"
  [(set (match_operand:V2SI 0 "register_operand" "=y")
	(vec_concat:V2SI
	  (plus:SI
	    (vec_select:SI
	      (match_operand:V2SI 1 "register_operand" "0")
	      (parallel [(const_int 0)]))
	    (vec_select:SI (match_dup 1) (parallel [(const_int 1)])))
	  (plus:SI
	    (vec_select:SI
	      (match_operand:V2SI 2 "nonimmediate_operand" "ym")
	      (parallel [(const_int 0)]))
	    (vec_select:SI (match_dup 2) (parallel [(const_int 1)])))))]
  "TARGET_SSSE3"
  "phaddd\t{%2, %0|%0, %2}"
  [(set_attr "type" "sseiadd")
   (set_attr "atom_unit" "complex")
   (set_attr "prefix_extra" "1")
   (set (attr "prefix_rex") (symbol_ref "x86_extended_reg_mentioned_p (insn)"))
   (set_attr "mode" "DI")])

(define_insn "*avx_phaddswv8hi3"
  [(set (match_operand:V8HI 0 "register_operand" "=x")
	(vec_concat:V8HI
	  (vec_concat:V4HI
	    (vec_concat:V2HI
	      (ss_plus:HI
		(vec_select:HI
		  (match_operand:V8HI 1 "register_operand" "x")
		  (parallel [(const_int 0)]))
		(vec_select:HI (match_dup 1) (parallel [(const_int 1)])))
	      (ss_plus:HI
		(vec_select:HI (match_dup 1) (parallel [(const_int 2)]))
		(vec_select:HI (match_dup 1) (parallel [(const_int 3)]))))
	    (vec_concat:V2HI
	      (ss_plus:HI
		(vec_select:HI (match_dup 1) (parallel [(const_int 4)]))
		(vec_select:HI (match_dup 1) (parallel [(const_int 5)])))
	      (ss_plus:HI
		(vec_select:HI (match_dup 1) (parallel [(const_int 6)]))
		(vec_select:HI (match_dup 1) (parallel [(const_int 7)])))))
	  (vec_concat:V4HI
	    (vec_concat:V2HI
	      (ss_plus:HI
		(vec_select:HI
		  (match_operand:V8HI 2 "nonimmediate_operand" "xm")
		  (parallel [(const_int 0)]))
		(vec_select:HI (match_dup 2) (parallel [(const_int 1)])))
	      (ss_plus:HI
		(vec_select:HI (match_dup 2) (parallel [(const_int 2)]))
		(vec_select:HI (match_dup 2) (parallel [(const_int 3)]))))
	    (vec_concat:V2HI
	      (ss_plus:HI
		(vec_select:HI (match_dup 2) (parallel [(const_int 4)]))
		(vec_select:HI (match_dup 2) (parallel [(const_int 5)])))
	      (ss_plus:HI
		(vec_select:HI (match_dup 2) (parallel [(const_int 6)]))
		(vec_select:HI (match_dup 2) (parallel [(const_int 7)])))))))]
  "TARGET_AVX"
  "vphaddsw\t{%2, %1, %0|%0, %1, %2}"
  [(set_attr "type" "sseiadd")
   (set_attr "prefix_extra" "1")
   (set_attr "prefix" "vex")
   (set_attr "mode" "TI")])

(define_insn "ssse3_phaddswv8hi3"
  [(set (match_operand:V8HI 0 "register_operand" "=x")
	(vec_concat:V8HI
	  (vec_concat:V4HI
	    (vec_concat:V2HI
	      (ss_plus:HI
		(vec_select:HI
		  (match_operand:V8HI 1 "register_operand" "0")
		  (parallel [(const_int 0)]))
		(vec_select:HI (match_dup 1) (parallel [(const_int 1)])))
	      (ss_plus:HI
		(vec_select:HI (match_dup 1) (parallel [(const_int 2)]))
		(vec_select:HI (match_dup 1) (parallel [(const_int 3)]))))
	    (vec_concat:V2HI
	      (ss_plus:HI
		(vec_select:HI (match_dup 1) (parallel [(const_int 4)]))
		(vec_select:HI (match_dup 1) (parallel [(const_int 5)])))
	      (ss_plus:HI
		(vec_select:HI (match_dup 1) (parallel [(const_int 6)]))
		(vec_select:HI (match_dup 1) (parallel [(const_int 7)])))))
	  (vec_concat:V4HI
	    (vec_concat:V2HI
	      (ss_plus:HI
		(vec_select:HI
		  (match_operand:V8HI 2 "nonimmediate_operand" "xm")
		  (parallel [(const_int 0)]))
		(vec_select:HI (match_dup 2) (parallel [(const_int 1)])))
	      (ss_plus:HI
		(vec_select:HI (match_dup 2) (parallel [(const_int 2)]))
		(vec_select:HI (match_dup 2) (parallel [(const_int 3)]))))
	    (vec_concat:V2HI
	      (ss_plus:HI
		(vec_select:HI (match_dup 2) (parallel [(const_int 4)]))
		(vec_select:HI (match_dup 2) (parallel [(const_int 5)])))
	      (ss_plus:HI
		(vec_select:HI (match_dup 2) (parallel [(const_int 6)]))
		(vec_select:HI (match_dup 2) (parallel [(const_int 7)])))))))]
  "TARGET_SSSE3"
  "phaddsw\t{%2, %0|%0, %2}"
  [(set_attr "type" "sseiadd")
   (set_attr "atom_unit" "complex")
   (set_attr "prefix_data16" "1")
   (set_attr "prefix_extra" "1")
   (set_attr "mode" "TI")])

(define_insn "ssse3_phaddswv4hi3"
  [(set (match_operand:V4HI 0 "register_operand" "=y")
	(vec_concat:V4HI
	  (vec_concat:V2HI
	    (ss_plus:HI
	      (vec_select:HI
		(match_operand:V4HI 1 "register_operand" "0")
		(parallel [(const_int 0)]))
	      (vec_select:HI (match_dup 1) (parallel [(const_int 1)])))
	    (ss_plus:HI
	      (vec_select:HI (match_dup 1) (parallel [(const_int 2)]))
	      (vec_select:HI (match_dup 1) (parallel [(const_int 3)]))))
	  (vec_concat:V2HI
	    (ss_plus:HI
	      (vec_select:HI
		(match_operand:V4HI 2 "nonimmediate_operand" "ym")
		(parallel [(const_int 0)]))
	      (vec_select:HI (match_dup 2) (parallel [(const_int 1)])))
	    (ss_plus:HI
	      (vec_select:HI (match_dup 2) (parallel [(const_int 2)]))
	      (vec_select:HI (match_dup 2) (parallel [(const_int 3)]))))))]
  "TARGET_SSSE3"
  "phaddsw\t{%2, %0|%0, %2}"
  [(set_attr "type" "sseiadd")
   (set_attr "atom_unit" "complex")
   (set_attr "prefix_extra" "1")
   (set (attr "prefix_rex") (symbol_ref "x86_extended_reg_mentioned_p (insn)"))
   (set_attr "mode" "DI")])

(define_insn "*avx_phsubwv8hi3"
  [(set (match_operand:V8HI 0 "register_operand" "=x")
	(vec_concat:V8HI
	  (vec_concat:V4HI
	    (vec_concat:V2HI
	      (minus:HI
		(vec_select:HI
		  (match_operand:V8HI 1 "register_operand" "x")
		  (parallel [(const_int 0)]))
		(vec_select:HI (match_dup 1) (parallel [(const_int 1)])))
	      (minus:HI
		(vec_select:HI (match_dup 1) (parallel [(const_int 2)]))
		(vec_select:HI (match_dup 1) (parallel [(const_int 3)]))))
	    (vec_concat:V2HI
	      (minus:HI
		(vec_select:HI (match_dup 1) (parallel [(const_int 4)]))
		(vec_select:HI (match_dup 1) (parallel [(const_int 5)])))
	      (minus:HI
		(vec_select:HI (match_dup 1) (parallel [(const_int 6)]))
		(vec_select:HI (match_dup 1) (parallel [(const_int 7)])))))
	  (vec_concat:V4HI
	    (vec_concat:V2HI
	      (minus:HI
		(vec_select:HI
		  (match_operand:V8HI 2 "nonimmediate_operand" "xm")
		  (parallel [(const_int 0)]))
		(vec_select:HI (match_dup 2) (parallel [(const_int 1)])))
	      (minus:HI
		(vec_select:HI (match_dup 2) (parallel [(const_int 2)]))
		(vec_select:HI (match_dup 2) (parallel [(const_int 3)]))))
	    (vec_concat:V2HI
	      (minus:HI
		(vec_select:HI (match_dup 2) (parallel [(const_int 4)]))
		(vec_select:HI (match_dup 2) (parallel [(const_int 5)])))
	      (minus:HI
		(vec_select:HI (match_dup 2) (parallel [(const_int 6)]))
		(vec_select:HI (match_dup 2) (parallel [(const_int 7)])))))))]
  "TARGET_AVX"
  "vphsubw\t{%2, %1, %0|%0, %1, %2}"
  [(set_attr "type" "sseiadd")
   (set_attr "prefix_extra" "1")
   (set_attr "prefix" "vex")
   (set_attr "mode" "TI")])

(define_insn "ssse3_phsubwv8hi3"
  [(set (match_operand:V8HI 0 "register_operand" "=x")
	(vec_concat:V8HI
	  (vec_concat:V4HI
	    (vec_concat:V2HI
	      (minus:HI
		(vec_select:HI
		  (match_operand:V8HI 1 "register_operand" "0")
		  (parallel [(const_int 0)]))
		(vec_select:HI (match_dup 1) (parallel [(const_int 1)])))
	      (minus:HI
		(vec_select:HI (match_dup 1) (parallel [(const_int 2)]))
		(vec_select:HI (match_dup 1) (parallel [(const_int 3)]))))
	    (vec_concat:V2HI
	      (minus:HI
		(vec_select:HI (match_dup 1) (parallel [(const_int 4)]))
		(vec_select:HI (match_dup 1) (parallel [(const_int 5)])))
	      (minus:HI
		(vec_select:HI (match_dup 1) (parallel [(const_int 6)]))
		(vec_select:HI (match_dup 1) (parallel [(const_int 7)])))))
	  (vec_concat:V4HI
	    (vec_concat:V2HI
	      (minus:HI
		(vec_select:HI
		  (match_operand:V8HI 2 "nonimmediate_operand" "xm")
		  (parallel [(const_int 0)]))
		(vec_select:HI (match_dup 2) (parallel [(const_int 1)])))
	      (minus:HI
		(vec_select:HI (match_dup 2) (parallel [(const_int 2)]))
		(vec_select:HI (match_dup 2) (parallel [(const_int 3)]))))
	    (vec_concat:V2HI
	      (minus:HI
		(vec_select:HI (match_dup 2) (parallel [(const_int 4)]))
		(vec_select:HI (match_dup 2) (parallel [(const_int 5)])))
	      (minus:HI
		(vec_select:HI (match_dup 2) (parallel [(const_int 6)]))
		(vec_select:HI (match_dup 2) (parallel [(const_int 7)])))))))]
  "TARGET_SSSE3"
  "phsubw\t{%2, %0|%0, %2}"
  [(set_attr "type" "sseiadd")
   (set_attr "atom_unit" "complex")
   (set_attr "prefix_data16" "1")
   (set_attr "prefix_extra" "1")
   (set_attr "mode" "TI")])

(define_insn "ssse3_phsubwv4hi3"
  [(set (match_operand:V4HI 0 "register_operand" "=y")
	(vec_concat:V4HI
	  (vec_concat:V2HI
	    (minus:HI
	      (vec_select:HI
		(match_operand:V4HI 1 "register_operand" "0")
		(parallel [(const_int 0)]))
	      (vec_select:HI (match_dup 1) (parallel [(const_int 1)])))
	    (minus:HI
	      (vec_select:HI (match_dup 1) (parallel [(const_int 2)]))
	      (vec_select:HI (match_dup 1) (parallel [(const_int 3)]))))
	  (vec_concat:V2HI
	    (minus:HI
	      (vec_select:HI
		(match_operand:V4HI 2 "nonimmediate_operand" "ym")
		(parallel [(const_int 0)]))
	      (vec_select:HI (match_dup 2) (parallel [(const_int 1)])))
	    (minus:HI
	      (vec_select:HI (match_dup 2) (parallel [(const_int 2)]))
	      (vec_select:HI (match_dup 2) (parallel [(const_int 3)]))))))]
  "TARGET_SSSE3"
  "phsubw\t{%2, %0|%0, %2}"
  [(set_attr "type" "sseiadd")
   (set_attr "atom_unit" "complex")
   (set_attr "prefix_extra" "1")
   (set (attr "prefix_rex") (symbol_ref "x86_extended_reg_mentioned_p (insn)"))
   (set_attr "mode" "DI")])

(define_insn "*avx_phsubdv4si3"
  [(set (match_operand:V4SI 0 "register_operand" "=x")
	(vec_concat:V4SI
	  (vec_concat:V2SI
	    (minus:SI
	      (vec_select:SI
		(match_operand:V4SI 1 "register_operand" "x")
		(parallel [(const_int 0)]))
	      (vec_select:SI (match_dup 1) (parallel [(const_int 1)])))
	    (minus:SI
	      (vec_select:SI (match_dup 1) (parallel [(const_int 2)]))
	      (vec_select:SI (match_dup 1) (parallel [(const_int 3)]))))
	  (vec_concat:V2SI
	    (minus:SI
	      (vec_select:SI
		(match_operand:V4SI 2 "nonimmediate_operand" "xm")
		(parallel [(const_int 0)]))
	      (vec_select:SI (match_dup 2) (parallel [(const_int 1)])))
	    (minus:SI
	      (vec_select:SI (match_dup 2) (parallel [(const_int 2)]))
	      (vec_select:SI (match_dup 2) (parallel [(const_int 3)]))))))]
  "TARGET_AVX"
  "vphsubd\t{%2, %1, %0|%0, %1, %2}"
  [(set_attr "type" "sseiadd")
   (set_attr "prefix_extra" "1")
   (set_attr "prefix" "vex")
   (set_attr "mode" "TI")])

(define_insn "ssse3_phsubdv4si3"
  [(set (match_operand:V4SI 0 "register_operand" "=x")
	(vec_concat:V4SI
	  (vec_concat:V2SI
	    (minus:SI
	      (vec_select:SI
		(match_operand:V4SI 1 "register_operand" "0")
		(parallel [(const_int 0)]))
	      (vec_select:SI (match_dup 1) (parallel [(const_int 1)])))
	    (minus:SI
	      (vec_select:SI (match_dup 1) (parallel [(const_int 2)]))
	      (vec_select:SI (match_dup 1) (parallel [(const_int 3)]))))
	  (vec_concat:V2SI
	    (minus:SI
	      (vec_select:SI
		(match_operand:V4SI 2 "nonimmediate_operand" "xm")
		(parallel [(const_int 0)]))
	      (vec_select:SI (match_dup 2) (parallel [(const_int 1)])))
	    (minus:SI
	      (vec_select:SI (match_dup 2) (parallel [(const_int 2)]))
	      (vec_select:SI (match_dup 2) (parallel [(const_int 3)]))))))]
  "TARGET_SSSE3"
  "phsubd\t{%2, %0|%0, %2}"
  [(set_attr "type" "sseiadd")
   (set_attr "atom_unit" "complex")
   (set_attr "prefix_data16" "1")
   (set_attr "prefix_extra" "1")
   (set_attr "mode" "TI")])

(define_insn "ssse3_phsubdv2si3"
  [(set (match_operand:V2SI 0 "register_operand" "=y")
	(vec_concat:V2SI
	  (minus:SI
	    (vec_select:SI
	      (match_operand:V2SI 1 "register_operand" "0")
	      (parallel [(const_int 0)]))
	    (vec_select:SI (match_dup 1) (parallel [(const_int 1)])))
	  (minus:SI
	    (vec_select:SI
	      (match_operand:V2SI 2 "nonimmediate_operand" "ym")
	      (parallel [(const_int 0)]))
	    (vec_select:SI (match_dup 2) (parallel [(const_int 1)])))))]
  "TARGET_SSSE3"
  "phsubd\t{%2, %0|%0, %2}"
  [(set_attr "type" "sseiadd")
   (set_attr "atom_unit" "complex")
   (set_attr "prefix_extra" "1")
   (set (attr "prefix_rex") (symbol_ref "x86_extended_reg_mentioned_p (insn)"))
   (set_attr "mode" "DI")])

(define_insn "*avx_phsubswv8hi3"
  [(set (match_operand:V8HI 0 "register_operand" "=x")
	(vec_concat:V8HI
	  (vec_concat:V4HI
	    (vec_concat:V2HI
	      (ss_minus:HI
		(vec_select:HI
		  (match_operand:V8HI 1 "register_operand" "x")
		  (parallel [(const_int 0)]))
		(vec_select:HI (match_dup 1) (parallel [(const_int 1)])))
	      (ss_minus:HI
		(vec_select:HI (match_dup 1) (parallel [(const_int 2)]))
		(vec_select:HI (match_dup 1) (parallel [(const_int 3)]))))
	    (vec_concat:V2HI
	      (ss_minus:HI
		(vec_select:HI (match_dup 1) (parallel [(const_int 4)]))
		(vec_select:HI (match_dup 1) (parallel [(const_int 5)])))
	      (ss_minus:HI
		(vec_select:HI (match_dup 1) (parallel [(const_int 6)]))
		(vec_select:HI (match_dup 1) (parallel [(const_int 7)])))))
	  (vec_concat:V4HI
	    (vec_concat:V2HI
	      (ss_minus:HI
		(vec_select:HI
		  (match_operand:V8HI 2 "nonimmediate_operand" "xm")
		  (parallel [(const_int 0)]))
		(vec_select:HI (match_dup 2) (parallel [(const_int 1)])))
	      (ss_minus:HI
		(vec_select:HI (match_dup 2) (parallel [(const_int 2)]))
		(vec_select:HI (match_dup 2) (parallel [(const_int 3)]))))
	    (vec_concat:V2HI
	      (ss_minus:HI
		(vec_select:HI (match_dup 2) (parallel [(const_int 4)]))
		(vec_select:HI (match_dup 2) (parallel [(const_int 5)])))
	      (ss_minus:HI
		(vec_select:HI (match_dup 2) (parallel [(const_int 6)]))
		(vec_select:HI (match_dup 2) (parallel [(const_int 7)])))))))]
  "TARGET_AVX"
  "vphsubsw\t{%2, %1, %0|%0, %1, %2}"
  [(set_attr "type" "sseiadd")
   (set_attr "prefix_extra" "1")
   (set_attr "prefix" "vex")
   (set_attr "mode" "TI")])

(define_insn "ssse3_phsubswv8hi3"
  [(set (match_operand:V8HI 0 "register_operand" "=x")
	(vec_concat:V8HI
	  (vec_concat:V4HI
	    (vec_concat:V2HI
	      (ss_minus:HI
		(vec_select:HI
		  (match_operand:V8HI 1 "register_operand" "0")
		  (parallel [(const_int 0)]))
		(vec_select:HI (match_dup 1) (parallel [(const_int 1)])))
	      (ss_minus:HI
		(vec_select:HI (match_dup 1) (parallel [(const_int 2)]))
		(vec_select:HI (match_dup 1) (parallel [(const_int 3)]))))
	    (vec_concat:V2HI
	      (ss_minus:HI
		(vec_select:HI (match_dup 1) (parallel [(const_int 4)]))
		(vec_select:HI (match_dup 1) (parallel [(const_int 5)])))
	      (ss_minus:HI
		(vec_select:HI (match_dup 1) (parallel [(const_int 6)]))
		(vec_select:HI (match_dup 1) (parallel [(const_int 7)])))))
	  (vec_concat:V4HI
	    (vec_concat:V2HI
	      (ss_minus:HI
		(vec_select:HI
		  (match_operand:V8HI 2 "nonimmediate_operand" "xm")
		  (parallel [(const_int 0)]))
		(vec_select:HI (match_dup 2) (parallel [(const_int 1)])))
	      (ss_minus:HI
		(vec_select:HI (match_dup 2) (parallel [(const_int 2)]))
		(vec_select:HI (match_dup 2) (parallel [(const_int 3)]))))
	    (vec_concat:V2HI
	      (ss_minus:HI
		(vec_select:HI (match_dup 2) (parallel [(const_int 4)]))
		(vec_select:HI (match_dup 2) (parallel [(const_int 5)])))
	      (ss_minus:HI
		(vec_select:HI (match_dup 2) (parallel [(const_int 6)]))
		(vec_select:HI (match_dup 2) (parallel [(const_int 7)])))))))]
  "TARGET_SSSE3"
  "phsubsw\t{%2, %0|%0, %2}"
  [(set_attr "type" "sseiadd")
   (set_attr "atom_unit" "complex")
   (set_attr "prefix_data16" "1")
   (set_attr "prefix_extra" "1")
   (set_attr "mode" "TI")])

(define_insn "ssse3_phsubswv4hi3"
  [(set (match_operand:V4HI 0 "register_operand" "=y")
	(vec_concat:V4HI
	  (vec_concat:V2HI
	    (ss_minus:HI
	      (vec_select:HI
		(match_operand:V4HI 1 "register_operand" "0")
		(parallel [(const_int 0)]))
	      (vec_select:HI (match_dup 1) (parallel [(const_int 1)])))
	    (ss_minus:HI
	      (vec_select:HI (match_dup 1) (parallel [(const_int 2)]))
	      (vec_select:HI (match_dup 1) (parallel [(const_int 3)]))))
	  (vec_concat:V2HI
	    (ss_minus:HI
	      (vec_select:HI
		(match_operand:V4HI 2 "nonimmediate_operand" "ym")
		(parallel [(const_int 0)]))
	      (vec_select:HI (match_dup 2) (parallel [(const_int 1)])))
	    (ss_minus:HI
	      (vec_select:HI (match_dup 2) (parallel [(const_int 2)]))
	      (vec_select:HI (match_dup 2) (parallel [(const_int 3)]))))))]
  "TARGET_SSSE3"
  "phsubsw\t{%2, %0|%0, %2}"
  [(set_attr "type" "sseiadd")
   (set_attr "atom_unit" "complex")
   (set_attr "prefix_extra" "1")
   (set (attr "prefix_rex") (symbol_ref "x86_extended_reg_mentioned_p (insn)"))
   (set_attr "mode" "DI")])

(define_insn "*avx_pmaddubsw128"
  [(set (match_operand:V8HI 0 "register_operand" "=x")
	(ss_plus:V8HI
	  (mult:V8HI
	    (zero_extend:V8HI
	      (vec_select:V8QI
		(match_operand:V16QI 1 "register_operand" "x")
		(parallel [(const_int 0)
			   (const_int 2)
			   (const_int 4)
			   (const_int 6)
			   (const_int 8)
			   (const_int 10)
			   (const_int 12)
			   (const_int 14)])))
	    (sign_extend:V8HI
	      (vec_select:V8QI
		(match_operand:V16QI 2 "nonimmediate_operand" "xm")
		(parallel [(const_int 0)
			   (const_int 2)
			   (const_int 4)
			   (const_int 6)
			   (const_int 8)
			   (const_int 10)
			   (const_int 12)
			   (const_int 14)]))))
	  (mult:V8HI
	    (zero_extend:V8HI
	      (vec_select:V8QI (match_dup 1)
		(parallel [(const_int 1)
			   (const_int 3)
			   (const_int 5)
			   (const_int 7)
			   (const_int 9)
			   (const_int 11)
			   (const_int 13)
			   (const_int 15)])))
	    (sign_extend:V8HI
	      (vec_select:V8QI (match_dup 2)
		(parallel [(const_int 1)
			   (const_int 3)
			   (const_int 5)
			   (const_int 7)
			   (const_int 9)
			   (const_int 11)
			   (const_int 13)
			   (const_int 15)]))))))]
  "TARGET_AVX"
  "vpmaddubsw\t{%2, %1, %0|%0, %1, %2}"
  [(set_attr "type" "sseiadd")
   (set_attr "prefix_extra" "1")
   (set_attr "prefix" "vex")
   (set_attr "mode" "TI")])

(define_insn "ssse3_pmaddubsw128"
  [(set (match_operand:V8HI 0 "register_operand" "=x")
	(ss_plus:V8HI
	  (mult:V8HI
	    (zero_extend:V8HI
	      (vec_select:V8QI
		(match_operand:V16QI 1 "register_operand" "0")
		(parallel [(const_int 0)
			   (const_int 2)
			   (const_int 4)
			   (const_int 6)
			   (const_int 8)
			   (const_int 10)
			   (const_int 12)
			   (const_int 14)])))
	    (sign_extend:V8HI
	      (vec_select:V8QI
		(match_operand:V16QI 2 "nonimmediate_operand" "xm")
		(parallel [(const_int 0)
			   (const_int 2)
			   (const_int 4)
			   (const_int 6)
			   (const_int 8)
			   (const_int 10)
			   (const_int 12)
			   (const_int 14)]))))
	  (mult:V8HI
	    (zero_extend:V8HI
	      (vec_select:V8QI (match_dup 1)
		(parallel [(const_int 1)
			   (const_int 3)
			   (const_int 5)
			   (const_int 7)
			   (const_int 9)
			   (const_int 11)
			   (const_int 13)
			   (const_int 15)])))
	    (sign_extend:V8HI
	      (vec_select:V8QI (match_dup 2)
		(parallel [(const_int 1)
			   (const_int 3)
			   (const_int 5)
			   (const_int 7)
			   (const_int 9)
			   (const_int 11)
			   (const_int 13)
			   (const_int 15)]))))))]
  "TARGET_SSSE3"
  "pmaddubsw\t{%2, %0|%0, %2}"
  [(set_attr "type" "sseiadd")
   (set_attr "atom_unit" "simul")
   (set_attr "prefix_data16" "1")
   (set_attr "prefix_extra" "1")
   (set_attr "mode" "TI")])

(define_insn "ssse3_pmaddubsw"
  [(set (match_operand:V4HI 0 "register_operand" "=y")
	(ss_plus:V4HI
	  (mult:V4HI
	    (zero_extend:V4HI
	      (vec_select:V4QI
		(match_operand:V8QI 1 "register_operand" "0")
		(parallel [(const_int 0)
			   (const_int 2)
			   (const_int 4)
			   (const_int 6)])))
	    (sign_extend:V4HI
	      (vec_select:V4QI
		(match_operand:V8QI 2 "nonimmediate_operand" "ym")
		(parallel [(const_int 0)
			   (const_int 2)
			   (const_int 4)
			   (const_int 6)]))))
	  (mult:V4HI
	    (zero_extend:V4HI
	      (vec_select:V4QI (match_dup 1)
		(parallel [(const_int 1)
			   (const_int 3)
			   (const_int 5)
			   (const_int 7)])))
	    (sign_extend:V4HI
	      (vec_select:V4QI (match_dup 2)
		(parallel [(const_int 1)
			   (const_int 3)
			   (const_int 5)
			   (const_int 7)]))))))]
  "TARGET_SSSE3"
  "pmaddubsw\t{%2, %0|%0, %2}"
  [(set_attr "type" "sseiadd")
   (set_attr "atom_unit" "simul")
   (set_attr "prefix_extra" "1")
   (set (attr "prefix_rex") (symbol_ref "x86_extended_reg_mentioned_p (insn)"))
   (set_attr "mode" "DI")])

(define_expand "ssse3_pmulhrswv8hi3"
  [(set (match_operand:V8HI 0 "register_operand" "")
	(truncate:V8HI
	  (lshiftrt:V8SI
	    (plus:V8SI
	      (lshiftrt:V8SI
		(mult:V8SI
		  (sign_extend:V8SI
		    (match_operand:V8HI 1 "nonimmediate_operand" ""))
		  (sign_extend:V8SI
		    (match_operand:V8HI 2 "nonimmediate_operand" "")))
		(const_int 14))
	      (const_vector:V8HI [(const_int 1) (const_int 1)
				  (const_int 1) (const_int 1)
				  (const_int 1) (const_int 1)
				  (const_int 1) (const_int 1)]))
	    (const_int 1))))]
  "TARGET_SSSE3"
  "ix86_fixup_binary_operands_no_copy (MULT, V8HImode, operands);")

(define_insn "*avx_pmulhrswv8hi3"
  [(set (match_operand:V8HI 0 "register_operand" "=x")
	(truncate:V8HI
	  (lshiftrt:V8SI
	    (plus:V8SI
	      (lshiftrt:V8SI
		(mult:V8SI
		  (sign_extend:V8SI
		    (match_operand:V8HI 1 "nonimmediate_operand" "%x"))
		  (sign_extend:V8SI
		    (match_operand:V8HI 2 "nonimmediate_operand" "xm")))
		(const_int 14))
	      (const_vector:V8HI [(const_int 1) (const_int 1)
				  (const_int 1) (const_int 1)
				  (const_int 1) (const_int 1)
				  (const_int 1) (const_int 1)]))
	    (const_int 1))))]
  "TARGET_AVX && ix86_binary_operator_ok (MULT, V8HImode, operands)"
  "vpmulhrsw\t{%2, %1, %0|%0, %1, %2}"
  [(set_attr "type" "sseimul")
   (set_attr "prefix_extra" "1")
   (set_attr "prefix" "vex")
   (set_attr "mode" "TI")])

(define_insn "*ssse3_pmulhrswv8hi3"
  [(set (match_operand:V8HI 0 "register_operand" "=x")
	(truncate:V8HI
	  (lshiftrt:V8SI
	    (plus:V8SI
	      (lshiftrt:V8SI
		(mult:V8SI
		  (sign_extend:V8SI
		    (match_operand:V8HI 1 "nonimmediate_operand" "%0"))
		  (sign_extend:V8SI
		    (match_operand:V8HI 2 "nonimmediate_operand" "xm")))
		(const_int 14))
	      (const_vector:V8HI [(const_int 1) (const_int 1)
				  (const_int 1) (const_int 1)
				  (const_int 1) (const_int 1)
				  (const_int 1) (const_int 1)]))
	    (const_int 1))))]
  "TARGET_SSSE3 && ix86_binary_operator_ok (MULT, V8HImode, operands)"
  "pmulhrsw\t{%2, %0|%0, %2}"
  [(set_attr "type" "sseimul")
   (set_attr "prefix_data16" "1")
   (set_attr "prefix_extra" "1")
   (set_attr "mode" "TI")])

(define_expand "ssse3_pmulhrswv4hi3"
  [(set (match_operand:V4HI 0 "register_operand" "")
	(truncate:V4HI
	  (lshiftrt:V4SI
	    (plus:V4SI
	      (lshiftrt:V4SI
		(mult:V4SI
		  (sign_extend:V4SI
		    (match_operand:V4HI 1 "nonimmediate_operand" ""))
		  (sign_extend:V4SI
		    (match_operand:V4HI 2 "nonimmediate_operand" "")))
		(const_int 14))
	      (const_vector:V4HI [(const_int 1) (const_int 1)
				  (const_int 1) (const_int 1)]))
	    (const_int 1))))]
  "TARGET_SSSE3"
  "ix86_fixup_binary_operands_no_copy (MULT, V4HImode, operands);")

(define_insn "*ssse3_pmulhrswv4hi3"
  [(set (match_operand:V4HI 0 "register_operand" "=y")
	(truncate:V4HI
	  (lshiftrt:V4SI
	    (plus:V4SI
	      (lshiftrt:V4SI
		(mult:V4SI
		  (sign_extend:V4SI
		    (match_operand:V4HI 1 "nonimmediate_operand" "%0"))
		  (sign_extend:V4SI
		    (match_operand:V4HI 2 "nonimmediate_operand" "ym")))
		(const_int 14))
	      (const_vector:V4HI [(const_int 1) (const_int 1)
				  (const_int 1) (const_int 1)]))
	    (const_int 1))))]
  "TARGET_SSSE3 && ix86_binary_operator_ok (MULT, V4HImode, operands)"
  "pmulhrsw\t{%2, %0|%0, %2}"
  [(set_attr "type" "sseimul")
   (set_attr "prefix_extra" "1")
   (set (attr "prefix_rex") (symbol_ref "x86_extended_reg_mentioned_p (insn)"))
   (set_attr "mode" "DI")])

(define_insn "*avx_pshufbv16qi3"
  [(set (match_operand:V16QI 0 "register_operand" "=x")
	(unspec:V16QI [(match_operand:V16QI 1 "register_operand" "x")
		       (match_operand:V16QI 2 "nonimmediate_operand" "xm")]
		      UNSPEC_PSHUFB))]
  "TARGET_AVX"
  "vpshufb\t{%2, %1, %0|%0, %1, %2}";
  [(set_attr "type" "sselog1")
   (set_attr "prefix_extra" "1")
   (set_attr "prefix" "vex")
   (set_attr "mode" "TI")])

(define_insn "ssse3_pshufbv16qi3"
  [(set (match_operand:V16QI 0 "register_operand" "=x")
	(unspec:V16QI [(match_operand:V16QI 1 "register_operand" "0")
		       (match_operand:V16QI 2 "nonimmediate_operand" "xm")]
		      UNSPEC_PSHUFB))]
  "TARGET_SSSE3"
  "pshufb\t{%2, %0|%0, %2}";
  [(set_attr "type" "sselog1")
   (set_attr "prefix_data16" "1")
   (set_attr "prefix_extra" "1")
   (set_attr "mode" "TI")])

(define_insn "ssse3_pshufbv8qi3"
  [(set (match_operand:V8QI 0 "register_operand" "=y")
	(unspec:V8QI [(match_operand:V8QI 1 "register_operand" "0")
		      (match_operand:V8QI 2 "nonimmediate_operand" "ym")]
		     UNSPEC_PSHUFB))]
  "TARGET_SSSE3"
  "pshufb\t{%2, %0|%0, %2}";
  [(set_attr "type" "sselog1")
   (set_attr "prefix_extra" "1")
   (set (attr "prefix_rex") (symbol_ref "x86_extended_reg_mentioned_p (insn)"))
   (set_attr "mode" "DI")])

(define_insn "*avx_psign<mode>3"
  [(set (match_operand:SSEMODE124 0 "register_operand" "=x")
	(unspec:SSEMODE124
	  [(match_operand:SSEMODE124 1 "register_operand" "x")
	   (match_operand:SSEMODE124 2 "nonimmediate_operand" "xm")]
	  UNSPEC_PSIGN))]
  "TARGET_AVX"
  "vpsign<ssevecsize>\t{%2, %1, %0|%0, %1, %2}";
  [(set_attr "type" "sselog1")
   (set_attr "prefix_extra" "1")
   (set_attr "prefix" "vex")
   (set_attr "mode" "TI")])

(define_insn "ssse3_psign<mode>3"
  [(set (match_operand:SSEMODE124 0 "register_operand" "=x")
	(unspec:SSEMODE124
	  [(match_operand:SSEMODE124 1 "register_operand" "0")
	   (match_operand:SSEMODE124 2 "nonimmediate_operand" "xm")]
	  UNSPEC_PSIGN))]
  "TARGET_SSSE3"
  "psign<ssevecsize>\t{%2, %0|%0, %2}";
  [(set_attr "type" "sselog1")
   (set_attr "prefix_data16" "1")
   (set_attr "prefix_extra" "1")
   (set_attr "mode" "TI")])

(define_insn "ssse3_psign<mode>3"
  [(set (match_operand:MMXMODEI 0 "register_operand" "=y")
	(unspec:MMXMODEI
	  [(match_operand:MMXMODEI 1 "register_operand" "0")
	   (match_operand:MMXMODEI 2 "nonimmediate_operand" "ym")]
	  UNSPEC_PSIGN))]
  "TARGET_SSSE3"
  "psign<mmxvecsize>\t{%2, %0|%0, %2}";
  [(set_attr "type" "sselog1")
   (set_attr "prefix_extra" "1")
   (set (attr "prefix_rex") (symbol_ref "x86_extended_reg_mentioned_p (insn)"))
   (set_attr "mode" "DI")])

(define_insn "*avx_palignrti"
  [(set (match_operand:TI 0 "register_operand" "=x")
	(unspec:TI [(match_operand:TI 1 "register_operand" "x")
		    (match_operand:TI 2 "nonimmediate_operand" "xm")
		    (match_operand:SI 3 "const_0_to_255_mul_8_operand" "n")]
		   UNSPEC_PALIGNR))]
  "TARGET_AVX"
{
  operands[3] = GEN_INT (INTVAL (operands[3]) / 8);
  return "vpalignr\t{%3, %2, %1, %0|%0, %1, %2, %3}";
}
  [(set_attr "type" "sseishft")
   (set_attr "prefix_extra" "1")
   (set_attr "length_immediate" "1")
   (set_attr "prefix" "vex")
   (set_attr "mode" "TI")])

(define_insn "ssse3_palignrti"
  [(set (match_operand:TI 0 "register_operand" "=x")
	(unspec:TI [(match_operand:TI 1 "register_operand" "0")
		    (match_operand:TI 2 "nonimmediate_operand" "xm")
		    (match_operand:SI 3 "const_0_to_255_mul_8_operand" "n")]
		   UNSPEC_PALIGNR))]
  "TARGET_SSSE3"
{
  operands[3] = GEN_INT (INTVAL (operands[3]) / 8);
  return "palignr\t{%3, %2, %0|%0, %2, %3}";
}
  [(set_attr "type" "sseishft")
   (set_attr "atom_unit" "sishuf")
   (set_attr "prefix_data16" "1")
   (set_attr "prefix_extra" "1")
   (set_attr "length_immediate" "1")
   (set_attr "mode" "TI")])

(define_insn "ssse3_palignrdi"
  [(set (match_operand:DI 0 "register_operand" "=y")
	(unspec:DI [(match_operand:DI 1 "register_operand" "0")
		    (match_operand:DI 2 "nonimmediate_operand" "ym")
		    (match_operand:SI 3 "const_0_to_255_mul_8_operand" "n")]
		   UNSPEC_PALIGNR))]
  "TARGET_SSSE3"
{
  operands[3] = GEN_INT (INTVAL (operands[3]) / 8);
  return "palignr\t{%3, %2, %0|%0, %2, %3}";
}
  [(set_attr "type" "sseishft")
   (set_attr "atom_unit" "sishuf")
   (set_attr "prefix_extra" "1")
   (set_attr "length_immediate" "1")
   (set (attr "prefix_rex") (symbol_ref "x86_extended_reg_mentioned_p (insn)"))
   (set_attr "mode" "DI")])

(define_insn "abs<mode>2"
  [(set (match_operand:SSEMODE124 0 "register_operand" "=x")
	(abs:SSEMODE124 (match_operand:SSEMODE124 1 "nonimmediate_operand" "xm")))]
  "TARGET_SSSE3"
  "%vpabs<ssevecsize>\t{%1, %0|%0, %1}"
  [(set_attr "type" "sselog1")
   (set_attr "prefix_data16" "1")
   (set_attr "prefix_extra" "1")
   (set_attr "prefix" "maybe_vex")
   (set_attr "mode" "TI")])

(define_insn "abs<mode>2"
  [(set (match_operand:MMXMODEI 0 "register_operand" "=y")
	(abs:MMXMODEI (match_operand:MMXMODEI 1 "nonimmediate_operand" "ym")))]
  "TARGET_SSSE3"
  "pabs<mmxvecsize>\t{%1, %0|%0, %1}";
  [(set_attr "type" "sselog1")
   (set_attr "prefix_rep" "0")
   (set_attr "prefix_extra" "1")
   (set (attr "prefix_rex") (symbol_ref "x86_extended_reg_mentioned_p (insn)"))
   (set_attr "mode" "DI")])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; AMD SSE4A instructions
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define_insn "sse4a_movnt<mode>"
  [(set (match_operand:MODEF 0 "memory_operand" "=m")
	(unspec:MODEF
	  [(match_operand:MODEF 1 "register_operand" "x")]
          UNSPEC_MOVNT))]
  "TARGET_SSE4A"
  "movnts<ssemodefsuffix>\t{%1, %0|%0, %1}"
  [(set_attr "type" "ssemov")
   (set_attr "mode" "<MODE>")])

(define_insn "sse4a_vmmovnt<mode>"
  [(set (match_operand:<ssescalarmode> 0 "memory_operand" "=m")
	(unspec:<ssescalarmode>
	  [(vec_select:<ssescalarmode>
	     (match_operand:SSEMODEF2P 1 "register_operand" "x")
	     (parallel [(const_int 0)]))]
	  UNSPEC_MOVNT))]
  "TARGET_SSE4A"
  "movnt<ssescalarmodesuffix>\t{%1, %0|%0, %1}"
  [(set_attr "type" "ssemov")
   (set_attr "mode" "<ssescalarmode>")])

(define_insn "sse4a_extrqi"
  [(set (match_operand:V2DI 0 "register_operand" "=x")
        (unspec:V2DI [(match_operand:V2DI 1 "register_operand" "0")
                      (match_operand 2 "const_0_to_255_operand" "")
                      (match_operand 3 "const_0_to_255_operand" "")]
                     UNSPEC_EXTRQI))]
  "TARGET_SSE4A"
  "extrq\t{%3, %2, %0|%0, %2, %3}"
  [(set_attr "type" "sse")
   (set_attr "prefix_data16" "1")
   (set_attr "length_immediate" "2")
   (set_attr "mode" "TI")])

(define_insn "sse4a_extrq"
  [(set (match_operand:V2DI 0 "register_operand" "=x")
        (unspec:V2DI [(match_operand:V2DI 1 "register_operand" "0")
                      (match_operand:V16QI 2 "register_operand" "x")]
                     UNSPEC_EXTRQ))]
  "TARGET_SSE4A"
  "extrq\t{%2, %0|%0, %2}"
  [(set_attr "type" "sse")
   (set_attr "prefix_data16" "1")
   (set_attr "mode" "TI")])

(define_insn "sse4a_insertqi"
  [(set (match_operand:V2DI 0 "register_operand" "=x")
        (unspec:V2DI [(match_operand:V2DI 1 "register_operand" "0")
        	      (match_operand:V2DI 2 "register_operand" "x")
                      (match_operand 3 "const_0_to_255_operand" "")
                      (match_operand 4 "const_0_to_255_operand" "")]
                     UNSPEC_INSERTQI))]
  "TARGET_SSE4A"
  "insertq\t{%4, %3, %2, %0|%0, %2, %3, %4}"
  [(set_attr "type" "sseins")
   (set_attr "prefix_data16" "0")
   (set_attr "prefix_rep" "1")
   (set_attr "length_immediate" "2")
   (set_attr "mode" "TI")])

(define_insn "sse4a_insertq"
  [(set (match_operand:V2DI 0 "register_operand" "=x")
        (unspec:V2DI [(match_operand:V2DI 1 "register_operand" "0")
        	      (match_operand:V2DI 2 "register_operand" "x")]
        	     UNSPEC_INSERTQ))]
  "TARGET_SSE4A"
  "insertq\t{%2, %0|%0, %2}"
  [(set_attr "type" "sseins")
   (set_attr "prefix_data16" "0")
   (set_attr "prefix_rep" "1")
   (set_attr "mode" "TI")])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Intel SSE4.1 instructions
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define_insn "avx_blend<ssemodesuffix><avxmodesuffix>"
  [(set (match_operand:AVXMODEF2P 0 "register_operand" "=x")
	(vec_merge:AVXMODEF2P
	  (match_operand:AVXMODEF2P 2 "nonimmediate_operand" "xm")
	  (match_operand:AVXMODEF2P 1 "register_operand" "x")
	  (match_operand:SI 3 "const_0_to_<blendbits>_operand" "n")))]
  "TARGET_AVX"
  "vblend<ssemodesuffix>\t{%3, %2, %1, %0|%0, %1, %2, %3}"
  [(set_attr "type" "ssemov")
   (set_attr "prefix_extra" "1")
   (set_attr "length_immediate" "1")
   (set_attr "prefix" "vex")
   (set_attr "mode" "<avxvecmode>")])

(define_insn "avx_blendv<ssemodesuffix><avxmodesuffix>"
  [(set (match_operand:AVXMODEF2P 0 "register_operand" "=x")
	(unspec:AVXMODEF2P
	  [(match_operand:AVXMODEF2P 1 "register_operand" "x")
	   (match_operand:AVXMODEF2P 2 "nonimmediate_operand" "xm")
	   (match_operand:AVXMODEF2P 3 "register_operand" "x")]
	  UNSPEC_BLENDV))]
  "TARGET_AVX"
  "vblendv<ssemodesuffix>\t{%3, %2, %1, %0|%0, %1, %2, %3}"
  [(set_attr "type" "ssemov")
   (set_attr "prefix_extra" "1")
   (set_attr "length_immediate" "1")
   (set_attr "prefix" "vex")
   (set_attr "mode" "<avxvecmode>")])

(define_insn "sse4_1_blend<ssemodesuffix>"
  [(set (match_operand:SSEMODEF2P 0 "register_operand" "=x")
	(vec_merge:SSEMODEF2P
	  (match_operand:SSEMODEF2P 2 "nonimmediate_operand" "xm")
	  (match_operand:SSEMODEF2P 1 "register_operand" "0")
	  (match_operand:SI 3 "const_0_to_<blendbits>_operand" "n")))]
  "TARGET_SSE4_1"
  "blend<ssemodesuffix>\t{%3, %2, %0|%0, %2, %3}"
  [(set_attr "type" "ssemov")
   (set_attr "prefix_data16" "1")
   (set_attr "prefix_extra" "1")
   (set_attr "length_immediate" "1")
   (set_attr "mode" "<MODE>")])

(define_insn "sse4_1_blendv<ssemodesuffix>"
  [(set (match_operand:SSEMODEF2P 0 "reg_not_xmm0_operand" "=x")
	(unspec:SSEMODEF2P
	  [(match_operand:SSEMODEF2P 1 "reg_not_xmm0_operand" "0")
	   (match_operand:SSEMODEF2P 2 "nonimm_not_xmm0_operand" "xm")
	   (match_operand:SSEMODEF2P 3 "register_operand" "Yz")]
	  UNSPEC_BLENDV))]
  "TARGET_SSE4_1"
  "blendv<ssemodesuffix>\t{%3, %2, %0|%0, %2, %3}"
  [(set_attr "type" "ssemov")
   (set_attr "prefix_data16" "1")
   (set_attr "prefix_extra" "1")
   (set_attr "mode" "<MODE>")])

(define_insn "avx_dp<ssemodesuffix><avxmodesuffix>"
  [(set (match_operand:AVXMODEF2P 0 "register_operand" "=x")
	(unspec:AVXMODEF2P
	  [(match_operand:AVXMODEF2P 1 "nonimmediate_operand" "%x")
	   (match_operand:AVXMODEF2P 2 "nonimmediate_operand" "xm")
	   (match_operand:SI 3 "const_0_to_255_operand" "n")]
	  UNSPEC_DP))]
  "TARGET_AVX"
  "vdp<ssemodesuffix>\t{%3, %2, %1, %0|%0, %1, %2, %3}"
  [(set_attr "type" "ssemul")
   (set_attr "prefix" "vex")
   (set_attr "prefix_extra" "1")
   (set_attr "length_immediate" "1")
   (set_attr "mode" "<avxvecmode>")])

(define_insn "sse4_1_dp<ssemodesuffix>"
  [(set (match_operand:SSEMODEF2P 0 "register_operand" "=x")
	(unspec:SSEMODEF2P
	  [(match_operand:SSEMODEF2P 1 "nonimmediate_operand" "%0")
	   (match_operand:SSEMODEF2P 2 "nonimmediate_operand" "xm")
	   (match_operand:SI 3 "const_0_to_255_operand" "n")]
	  UNSPEC_DP))]
  "TARGET_SSE4_1"
  "dp<ssemodesuffix>\t{%3, %2, %0|%0, %2, %3}"
  [(set_attr "type" "ssemul")
   (set_attr "prefix_data16" "1")
   (set_attr "prefix_extra" "1")
   (set_attr "length_immediate" "1")
   (set_attr "mode" "<MODE>")])

(define_insn "sse4_1_movntdqa"
  [(set (match_operand:V2DI 0 "register_operand" "=x")
	(unspec:V2DI [(match_operand:V2DI 1 "memory_operand" "m")]
		     UNSPEC_MOVNTDQA))]
  "TARGET_SSE4_1"
  "%vmovntdqa\t{%1, %0|%0, %1}"
  [(set_attr "type" "ssemov")
   (set_attr "prefix_extra" "1")
   (set_attr "prefix" "maybe_vex")
   (set_attr "mode" "TI")])

(define_insn "*avx_mpsadbw"
  [(set (match_operand:V16QI 0 "register_operand" "=x")
	(unspec:V16QI [(match_operand:V16QI 1 "register_operand" "x")
		       (match_operand:V16QI 2 "nonimmediate_operand" "xm")
		       (match_operand:SI 3 "const_0_to_255_operand" "n")]
		      UNSPEC_MPSADBW))]
  "TARGET_AVX"
  "vmpsadbw\t{%3, %2, %1, %0|%0, %1, %2, %3}"
  [(set_attr "type" "sselog1")
   (set_attr "prefix" "vex")
   (set_attr "prefix_extra" "1")
   (set_attr "length_immediate" "1")
   (set_attr "mode" "TI")])

(define_insn "sse4_1_mpsadbw"
  [(set (match_operand:V16QI 0 "register_operand" "=x")
	(unspec:V16QI [(match_operand:V16QI 1 "register_operand" "0")
		       (match_operand:V16QI 2 "nonimmediate_operand" "xm")
		       (match_operand:SI 3 "const_0_to_255_operand" "n")]
		      UNSPEC_MPSADBW))]
  "TARGET_SSE4_1"
  "mpsadbw\t{%3, %2, %0|%0, %2, %3}"
  [(set_attr "type" "sselog1")
   (set_attr "prefix_extra" "1")
   (set_attr "length_immediate" "1")
   (set_attr "mode" "TI")])

(define_insn "*avx_packusdw"
  [(set (match_operand:V8HI 0 "register_operand" "=x")
	(vec_concat:V8HI
	  (us_truncate:V4HI
	    (match_operand:V4SI 1 "register_operand" "x"))
	  (us_truncate:V4HI
	    (match_operand:V4SI 2 "nonimmediate_operand" "xm"))))]
  "TARGET_AVX"
  "vpackusdw\t{%2, %1, %0|%0, %1, %2}"
  [(set_attr "type" "sselog")
   (set_attr "prefix_extra" "1")
   (set_attr "prefix" "vex")
   (set_attr "mode" "TI")])

(define_insn "sse4_1_packusdw"
  [(set (match_operand:V8HI 0 "register_operand" "=x")
	(vec_concat:V8HI
	  (us_truncate:V4HI
	    (match_operand:V4SI 1 "register_operand" "0"))
	  (us_truncate:V4HI
	    (match_operand:V4SI 2 "nonimmediate_operand" "xm"))))]
  "TARGET_SSE4_1"
  "packusdw\t{%2, %0|%0, %2}"
  [(set_attr "type" "sselog")
   (set_attr "prefix_extra" "1")
   (set_attr "mode" "TI")])

(define_insn "*avx_pblendvb"
  [(set (match_operand:V16QI 0 "register_operand" "=x")
	(unspec:V16QI [(match_operand:V16QI 1 "register_operand"  "x")
		       (match_operand:V16QI 2 "nonimmediate_operand" "xm")
		       (match_operand:V16QI 3 "register_operand" "x")]
		      UNSPEC_BLENDV))]
  "TARGET_AVX"
  "vpblendvb\t{%3, %2, %1, %0|%0, %1, %2, %3}"
  [(set_attr "type" "ssemov")
   (set_attr "prefix_extra" "1")
   (set_attr "length_immediate" "1")
   (set_attr "prefix" "vex")
   (set_attr "mode" "TI")])

(define_insn "sse4_1_pblendvb"
  [(set (match_operand:V16QI 0 "reg_not_xmm0_operand" "=x")
	(unspec:V16QI [(match_operand:V16QI 1 "reg_not_xmm0_operand"  "0")
		       (match_operand:V16QI 2 "nonimm_not_xmm0_operand" "xm")
		       (match_operand:V16QI 3 "register_operand" "Yz")]
		      UNSPEC_BLENDV))]
  "TARGET_SSE4_1"
  "pblendvb\t{%3, %2, %0|%0, %2, %3}"
  [(set_attr "type" "ssemov")
   (set_attr "prefix_extra" "1")
   (set_attr "mode" "TI")])

(define_insn "*avx_pblendw"
  [(set (match_operand:V8HI 0 "register_operand" "=x")
	(vec_merge:V8HI
	  (match_operand:V8HI 2 "nonimmediate_operand" "xm")
	  (match_operand:V8HI 1 "register_operand" "x")
	  (match_operand:SI 3 "const_0_to_255_operand" "n")))]
  "TARGET_AVX"
  "vpblendw\t{%3, %2, %1, %0|%0, %1, %2, %3}"
  [(set_attr "type" "ssemov")
   (set_attr "prefix" "vex")
   (set_attr "prefix_extra" "1")
   (set_attr "length_immediate" "1")
   (set_attr "mode" "TI")])

(define_insn "sse4_1_pblendw"
  [(set (match_operand:V8HI 0 "register_operand" "=x")
	(vec_merge:V8HI
	  (match_operand:V8HI 2 "nonimmediate_operand" "xm")
	  (match_operand:V8HI 1 "register_operand" "0")
	  (match_operand:SI 3 "const_0_to_255_operand" "n")))]
  "TARGET_SSE4_1"
  "pblendw\t{%3, %2, %0|%0, %2, %3}"
  [(set_attr "type" "ssemov")
   (set_attr "prefix_extra" "1")
   (set_attr "length_immediate" "1")
   (set_attr "mode" "TI")])

(define_insn "sse4_1_phminposuw"
  [(set (match_operand:V8HI 0 "register_operand" "=x")
	(unspec:V8HI [(match_operand:V8HI 1 "nonimmediate_operand" "xm")]
		     UNSPEC_PHMINPOSUW))]
  "TARGET_SSE4_1"
  "%vphminposuw\t{%1, %0|%0, %1}"
  [(set_attr "type" "sselog1")
   (set_attr "prefix_extra" "1")
   (set_attr "prefix" "maybe_vex")
   (set_attr "mode" "TI")])

(define_insn "sse4_1_<code>v8qiv8hi2"
  [(set (match_operand:V8HI 0 "register_operand" "=x")
	(any_extend:V8HI
	  (vec_select:V8QI
	    (match_operand:V16QI 1 "nonimmediate_operand" "xm")
	    (parallel [(const_int 0)
		       (const_int 1)
		       (const_int 2)
		       (const_int 3)
		       (const_int 4)
		       (const_int 5)
		       (const_int 6)
		       (const_int 7)]))))]
  "TARGET_SSE4_1"
  "%vpmov<extsuffix>bw\t{%1, %0|%0, %1}"
  [(set_attr "type" "ssemov")
   (set_attr "prefix_extra" "1")
   (set_attr "prefix" "maybe_vex")
   (set_attr "mode" "TI")])

(define_insn "sse4_1_<code>v4qiv4si2"
  [(set (match_operand:V4SI 0 "register_operand" "=x")
	(any_extend:V4SI
	  (vec_select:V4QI
	    (match_operand:V16QI 1 "nonimmediate_operand" "xm")
	    (parallel [(const_int 0)
		       (const_int 1)
		       (const_int 2)
		       (const_int 3)]))))]
  "TARGET_SSE4_1"
  "%vpmov<extsuffix>bd\t{%1, %0|%0, %1}"
  [(set_attr "type" "ssemov")
   (set_attr "prefix_extra" "1")
   (set_attr "prefix" "maybe_vex")
   (set_attr "mode" "TI")])

(define_insn "sse4_1_<code>v4hiv4si2"
  [(set (match_operand:V4SI 0 "register_operand" "=x")
	(any_extend:V4SI
	  (vec_select:V4HI
	    (match_operand:V8HI 1 "nonimmediate_operand" "xm")
	    (parallel [(const_int 0)
		       (const_int 1)
		       (const_int 2)
		       (const_int 3)]))))]
  "TARGET_SSE4_1"
  "%vpmov<extsuffix>wd\t{%1, %0|%0, %1}"
  [(set_attr "type" "ssemov")
   (set_attr "prefix_extra" "1")
   (set_attr "prefix" "maybe_vex")
   (set_attr "mode" "TI")])

(define_insn "sse4_1_<code>v2qiv2di2"
  [(set (match_operand:V2DI 0 "register_operand" "=x")
	(any_extend:V2DI
	  (vec_select:V2QI
	    (match_operand:V16QI 1 "nonimmediate_operand" "xm")
	    (parallel [(const_int 0)
		       (const_int 1)]))))]
  "TARGET_SSE4_1"
  "%vpmov<extsuffix>bq\t{%1, %0|%0, %1}"
  [(set_attr "type" "ssemov")
   (set_attr "prefix_extra" "1")
   (set_attr "prefix" "maybe_vex")
   (set_attr "mode" "TI")])

(define_insn "sse4_1_<code>v2hiv2di2"
  [(set (match_operand:V2DI 0 "register_operand" "=x")
	(any_extend:V2DI
	  (vec_select:V2HI
	    (match_operand:V8HI 1 "nonimmediate_operand" "xm")
	    (parallel [(const_int 0)
		       (const_int 1)]))))]
  "TARGET_SSE4_1"
  "%vpmov<extsuffix>wq\t{%1, %0|%0, %1}"
  [(set_attr "type" "ssemov")
   (set_attr "prefix_extra" "1")
   (set_attr "prefix" "maybe_vex")
   (set_attr "mode" "TI")])

(define_insn "sse4_1_<code>v2siv2di2"
  [(set (match_operand:V2DI 0 "register_operand" "=x")
	(any_extend:V2DI
	  (vec_select:V2SI
	    (match_operand:V4SI 1 "nonimmediate_operand" "xm")
	    (parallel [(const_int 0)
		       (const_int 1)]))))]
  "TARGET_SSE4_1"
  "%vpmov<extsuffix>dq\t{%1, %0|%0, %1}"
  [(set_attr "type" "ssemov")
   (set_attr "prefix_extra" "1")
   (set_attr "prefix" "maybe_vex")
   (set_attr "mode" "TI")])

;; ptestps/ptestpd are very similar to comiss and ucomiss when
;; setting FLAGS_REG. But it is not a really compare instruction.
(define_insn "avx_vtest<ssemodesuffix><avxmodesuffix>"
  [(set (reg:CC FLAGS_REG)
	(unspec:CC [(match_operand:AVXMODEF2P 0 "register_operand" "x")
		    (match_operand:AVXMODEF2P 1 "nonimmediate_operand" "xm")]
		   UNSPEC_VTESTP))]
  "TARGET_AVX"
  "vtest<ssemodesuffix>\t{%1, %0|%0, %1}"
  [(set_attr "type" "ssecomi")
   (set_attr "prefix_extra" "1")
   (set_attr "prefix" "vex")
   (set_attr "mode" "<MODE>")])

;; ptest is very similar to comiss and ucomiss when setting FLAGS_REG.
;; But it is not a really compare instruction.
(define_insn "avx_ptest256"
  [(set (reg:CC FLAGS_REG)
	(unspec:CC [(match_operand:V4DI 0 "register_operand" "x")
		    (match_operand:V4DI 1 "nonimmediate_operand" "xm")]
		   UNSPEC_PTEST))]
  "TARGET_AVX"
  "vptest\t{%1, %0|%0, %1}"
  [(set_attr "type" "ssecomi")
   (set_attr "prefix_extra" "1")
   (set_attr "prefix" "vex")
   (set_attr "mode" "OI")])

(define_insn "sse4_1_ptest"
  [(set (reg:CC FLAGS_REG)
	(unspec:CC [(match_operand:V2DI 0 "register_operand" "x")
		    (match_operand:V2DI 1 "nonimmediate_operand" "xm")]
		   UNSPEC_PTEST))]
  "TARGET_SSE4_1"
  "%vptest\t{%1, %0|%0, %1}"
  [(set_attr "type" "ssecomi")
   (set_attr "prefix_extra" "1")
   (set_attr "prefix" "maybe_vex")
   (set_attr "mode" "TI")])

(define_insn "avx_round<ssemodesuffix>256"
  [(set (match_operand:AVX256MODEF2P 0 "register_operand" "=x")
	(unspec:AVX256MODEF2P
	  [(match_operand:AVX256MODEF2P 1 "nonimmediate_operand" "xm")
	   (match_operand:SI 2 "const_0_to_15_operand" "n")]
	  UNSPEC_ROUND))]
  "TARGET_AVX"
  "vround<ssemodesuffix>\t{%2, %1, %0|%0, %1, %2}"
  [(set_attr "type" "ssecvt")
   (set_attr "prefix_extra" "1")
   (set_attr "length_immediate" "1")
   (set_attr "prefix" "vex")
   (set_attr "mode" "<MODE>")])

(define_insn "sse4_1_round<ssemodesuffix>"
  [(set (match_operand:SSEMODEF2P 0 "register_operand" "=x")
	(unspec:SSEMODEF2P
	  [(match_operand:SSEMODEF2P 1 "nonimmediate_operand" "xm")
	   (match_operand:SI 2 "const_0_to_15_operand" "n")]
	  UNSPEC_ROUND))]
  "TARGET_ROUND"
  "%vround<ssemodesuffix>\t{%2, %1, %0|%0, %1, %2}"
  [(set_attr "type" "ssecvt")
   (set_attr "prefix_data16" "1")
   (set_attr "prefix_extra" "1")
   (set_attr "length_immediate" "1")
   (set_attr "prefix" "maybe_vex")
   (set_attr "mode" "<MODE>")])

(define_insn "*avx_round<ssescalarmodesuffix>"
  [(set (match_operand:SSEMODEF2P 0 "register_operand" "=x")
	(vec_merge:SSEMODEF2P
	  (unspec:SSEMODEF2P
	    [(match_operand:SSEMODEF2P 2 "register_operand" "x")
	     (match_operand:SI 3 "const_0_to_15_operand" "n")]
	    UNSPEC_ROUND)
	  (match_operand:SSEMODEF2P 1 "register_operand" "x")
	  (const_int 1)))]
  "TARGET_AVX"
  "vround<ssescalarmodesuffix>\t{%3, %2, %1, %0|%0, %1, %2, %3}"
  [(set_attr "type" "ssecvt")
   (set_attr "prefix_extra" "1")
   (set_attr "length_immediate" "1")
   (set_attr "prefix" "vex")
   (set_attr "mode" "<MODE>")])

(define_insn "sse4_1_round<ssescalarmodesuffix>"
  [(set (match_operand:SSEMODEF2P 0 "register_operand" "=x")
	(vec_merge:SSEMODEF2P
	  (unspec:SSEMODEF2P
	    [(match_operand:SSEMODEF2P 2 "register_operand" "x")
	     (match_operand:SI 3 "const_0_to_15_operand" "n")]
	    UNSPEC_ROUND)
	  (match_operand:SSEMODEF2P 1 "register_operand" "0")
	  (const_int 1)))]
  "TARGET_ROUND"
  "round<ssescalarmodesuffix>\t{%3, %2, %0|%0, %2, %3}"
  [(set_attr "type" "ssecvt")
   (set_attr "prefix_data16" "1")
   (set_attr "prefix_extra" "1")
   (set_attr "length_immediate" "1")
   (set_attr "mode" "<MODE>")])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Intel SSE4.2 string/text processing instructions
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define_insn_and_split "sse4_2_pcmpestr"
  [(set (match_operand:SI 0 "register_operand" "=c,c")
	(unspec:SI
	  [(match_operand:V16QI 2 "reg_not_xmm0_operand" "x,x")
	   (match_operand:SI 3 "register_operand" "a,a")
	   (match_operand:V16QI 4 "nonimm_not_xmm0_operand" "x,m")
	   (match_operand:SI 5 "register_operand" "d,d")
	   (match_operand:SI 6 "const_0_to_255_operand" "n,n")]
	  UNSPEC_PCMPESTR))
   (set (match_operand:V16QI 1 "register_operand" "=Yz,Yz")
	(unspec:V16QI
	  [(match_dup 2)
	   (match_dup 3)
	   (match_dup 4)
	   (match_dup 5)
	   (match_dup 6)]
	  UNSPEC_PCMPESTR))
   (set (reg:CC FLAGS_REG)
	(unspec:CC
	  [(match_dup 2)
	   (match_dup 3)
	   (match_dup 4)
	   (match_dup 5)
	   (match_dup 6)]
	  UNSPEC_PCMPESTR))]
  "TARGET_SSE4_2
   && can_create_pseudo_p ()"
  "#"
  "&& 1"
  [(const_int 0)]
{
  int ecx = !find_regno_note (curr_insn, REG_UNUSED, REGNO (operands[0]));
  int xmm0 = !find_regno_note (curr_insn, REG_UNUSED, REGNO (operands[1]));
  int flags = !find_regno_note (curr_insn, REG_UNUSED, FLAGS_REG);

  if (ecx)
    emit_insn (gen_sse4_2_pcmpestri (operands[0], operands[2],
				     operands[3], operands[4],
				     operands[5], operands[6]));
  if (xmm0)
    emit_insn (gen_sse4_2_pcmpestrm (operands[1], operands[2],
				     operands[3], operands[4],
				     operands[5], operands[6]));
  if (flags && !(ecx || xmm0))
    emit_insn (gen_sse4_2_pcmpestr_cconly (NULL, NULL,
					   operands[2], operands[3],
					   operands[4], operands[5],
					   operands[6]));
  if (!(flags || ecx || xmm0))
    emit_note (NOTE_INSN_DELETED);

  DONE;
}
  [(set_attr "type" "sselog")
   (set_attr "prefix_data16" "1")
   (set_attr "prefix_extra" "1")
   (set_attr "length_immediate" "1")
   (set_attr "memory" "none,load")
   (set_attr "mode" "TI")])

(define_insn "sse4_2_pcmpestri"
  [(set (match_operand:SI 0 "register_operand" "=c,c")
	(unspec:SI
	  [(match_operand:V16QI 1 "register_operand" "x,x")
	   (match_operand:SI 2 "register_operand" "a,a")
	   (match_operand:V16QI 3 "nonimmediate_operand" "x,m")
	   (match_operand:SI 4 "register_operand" "d,d")
	   (match_operand:SI 5 "const_0_to_255_operand" "n,n")]
	  UNSPEC_PCMPESTR))
   (set (reg:CC FLAGS_REG)
	(unspec:CC
	  [(match_dup 1)
	   (match_dup 2)
	   (match_dup 3)
	   (match_dup 4)
	   (match_dup 5)]
	  UNSPEC_PCMPESTR))]
  "TARGET_SSE4_2"
  "%vpcmpestri\t{%5, %3, %1|%1, %3, %5}"
  [(set_attr "type" "sselog")
   (set_attr "prefix_data16" "1")
   (set_attr "prefix_extra" "1")
   (set_attr "prefix" "maybe_vex")
   (set_attr "length_immediate" "1")
   (set_attr "memory" "none,load")
   (set_attr "mode" "TI")])

(define_insn "sse4_2_pcmpestrm"
  [(set (match_operand:V16QI 0 "register_operand" "=Yz,Yz")
	(unspec:V16QI
	  [(match_operand:V16QI 1 "register_operand" "x,x")
	   (match_operand:SI 2 "register_operand" "a,a")
	   (match_operand:V16QI 3 "nonimmediate_operand" "x,m")
	   (match_operand:SI 4 "register_operand" "d,d")
	   (match_operand:SI 5 "const_0_to_255_operand" "n,n")]
	  UNSPEC_PCMPESTR))
   (set (reg:CC FLAGS_REG)
	(unspec:CC
	  [(match_dup 1)
	   (match_dup 2)
	   (match_dup 3)
	   (match_dup 4)
	   (match_dup 5)]
	  UNSPEC_PCMPESTR))]
  "TARGET_SSE4_2"
  "%vpcmpestrm\t{%5, %3, %1|%1, %3, %5}"
  [(set_attr "type" "sselog")
   (set_attr "prefix_data16" "1")
   (set_attr "prefix_extra" "1")
   (set_attr "length_immediate" "1")
   (set_attr "prefix" "maybe_vex")
   (set_attr "memory" "none,load")
   (set_attr "mode" "TI")])

(define_insn "sse4_2_pcmpestr_cconly"
  [(set (reg:CC FLAGS_REG)
	(unspec:CC
	  [(match_operand:V16QI 2 "register_operand" "x,x,x,x")
	   (match_operand:SI 3 "register_operand" "a,a,a,a")
	   (match_operand:V16QI 4 "nonimmediate_operand" "x,m,x,m")
	   (match_operand:SI 5 "register_operand" "d,d,d,d")
	   (match_operand:SI 6 "const_0_to_255_operand" "n,n,n,n")]
	  UNSPEC_PCMPESTR))
   (clobber (match_scratch:V16QI 0 "=Yz,Yz,X,X"))
   (clobber (match_scratch:SI    1 "= X, X,c,c"))]
  "TARGET_SSE4_2"
  "@
   %vpcmpestrm\t{%6, %4, %2|%2, %4, %6}
   %vpcmpestrm\t{%6, %4, %2|%2, %4, %6}
   %vpcmpestri\t{%6, %4, %2|%2, %4, %6}
   %vpcmpestri\t{%6, %4, %2|%2, %4, %6}"
  [(set_attr "type" "sselog")
   (set_attr "prefix_data16" "1")
   (set_attr "prefix_extra" "1")
   (set_attr "length_immediate" "1")
   (set_attr "memory" "none,load,none,load")
   (set_attr "prefix" "maybe_vex")
   (set_attr "mode" "TI")])

(define_insn_and_split "sse4_2_pcmpistr"
  [(set (match_operand:SI 0 "register_operand" "=c,c")
	(unspec:SI
	  [(match_operand:V16QI 2 "reg_not_xmm0_operand" "x,x")
	   (match_operand:V16QI 3 "nonimm_not_xmm0_operand" "x,m")
	   (match_operand:SI 4 "const_0_to_255_operand" "n,n")]
	  UNSPEC_PCMPISTR))
   (set (match_operand:V16QI 1 "register_operand" "=Yz,Yz")
	(unspec:V16QI
	  [(match_dup 2)
	   (match_dup 3)
	   (match_dup 4)]
	  UNSPEC_PCMPISTR))
   (set (reg:CC FLAGS_REG)
	(unspec:CC
	  [(match_dup 2)
	   (match_dup 3)
	   (match_dup 4)]
	  UNSPEC_PCMPISTR))]
  "TARGET_SSE4_2
   && can_create_pseudo_p ()"
  "#"
  "&& 1"
  [(const_int 0)]
{
  int ecx = !find_regno_note (curr_insn, REG_UNUSED, REGNO (operands[0]));
  int xmm0 = !find_regno_note (curr_insn, REG_UNUSED, REGNO (operands[1]));
  int flags = !find_regno_note (curr_insn, REG_UNUSED, FLAGS_REG);

  if (ecx)
    emit_insn (gen_sse4_2_pcmpistri (operands[0], operands[2],
				     operands[3], operands[4]));
  if (xmm0)
    emit_insn (gen_sse4_2_pcmpistrm (operands[1], operands[2],
				     operands[3], operands[4]));
  if (flags && !(ecx || xmm0))
    emit_insn (gen_sse4_2_pcmpistr_cconly (NULL, NULL,
					   operands[2], operands[3],
					   operands[4]));
  if (!(flags || ecx || xmm0))
    emit_note (NOTE_INSN_DELETED);

  DONE;
}
  [(set_attr "type" "sselog")
   (set_attr "prefix_data16" "1")
   (set_attr "prefix_extra" "1")
   (set_attr "length_immediate" "1")
   (set_attr "memory" "none,load")
   (set_attr "mode" "TI")])

(define_insn "sse4_2_pcmpistri"
  [(set (match_operand:SI 0 "register_operand" "=c,c")
	(unspec:SI
	  [(match_operand:V16QI 1 "register_operand" "x,x")
	   (match_operand:V16QI 2 "nonimmediate_operand" "x,m")
	   (match_operand:SI 3 "const_0_to_255_operand" "n,n")]
	  UNSPEC_PCMPISTR))
   (set (reg:CC FLAGS_REG)
	(unspec:CC
	  [(match_dup 1)
	   (match_dup 2)
	   (match_dup 3)]
	  UNSPEC_PCMPISTR))]
  "TARGET_SSE4_2"
  "%vpcmpistri\t{%3, %2, %1|%1, %2, %3}"
  [(set_attr "type" "sselog")
   (set_attr "prefix_data16" "1")
   (set_attr "prefix_extra" "1")
   (set_attr "length_immediate" "1")
   (set_attr "prefix" "maybe_vex")
   (set_attr "memory" "none,load")
   (set_attr "mode" "TI")])

(define_insn "sse4_2_pcmpistrm"
  [(set (match_operand:V16QI 0 "register_operand" "=Yz,Yz")
	(unspec:V16QI
	  [(match_operand:V16QI 1 "register_operand" "x,x")
	   (match_operand:V16QI 2 "nonimmediate_operand" "x,m")
	   (match_operand:SI 3 "const_0_to_255_operand" "n,n")]
	  UNSPEC_PCMPISTR))
   (set (reg:CC FLAGS_REG)
	(unspec:CC
	  [(match_dup 1)
	   (match_dup 2)
	   (match_dup 3)]
	  UNSPEC_PCMPISTR))]
  "TARGET_SSE4_2"
  "%vpcmpistrm\t{%3, %2, %1|%1, %2, %3}"
  [(set_attr "type" "sselog")
   (set_attr "prefix_data16" "1")
   (set_attr "prefix_extra" "1")
   (set_attr "length_immediate" "1")
   (set_attr "prefix" "maybe_vex")
   (set_attr "memory" "none,load")
   (set_attr "mode" "TI")])

(define_insn "sse4_2_pcmpistr_cconly"
  [(set (reg:CC FLAGS_REG)
	(unspec:CC
	  [(match_operand:V16QI 2 "register_operand" "x,x,x,x")
	   (match_operand:V16QI 3 "nonimmediate_operand" "x,m,x,m")
	   (match_operand:SI 4 "const_0_to_255_operand" "n,n,n,n")]
	  UNSPEC_PCMPISTR))
   (clobber (match_scratch:V16QI 0 "=Yz,Yz,X,X"))
   (clobber (match_scratch:SI    1 "= X, X,c,c"))]
  "TARGET_SSE4_2"
  "@
   %vpcmpistrm\t{%4, %3, %2|%2, %3, %4}
   %vpcmpistrm\t{%4, %3, %2|%2, %3, %4}
   %vpcmpistri\t{%4, %3, %2|%2, %3, %4}
   %vpcmpistri\t{%4, %3, %2|%2, %3, %4}"
  [(set_attr "type" "sselog")
   (set_attr "prefix_data16" "1")
   (set_attr "prefix_extra" "1")
   (set_attr "length_immediate" "1")
   (set_attr "memory" "none,load,none,load")
   (set_attr "prefix" "maybe_vex")
   (set_attr "mode" "TI")])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; XOP instructions
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; XOP parallel integer multiply/add instructions.
;; Note the XOP multiply/add instructions
;;     a[i] = b[i] * c[i] + d[i];
;; do not allow the value being added to be a memory operation.
(define_insn "xop_pmacsww"
  [(set (match_operand:V8HI 0 "register_operand" "=x")
        (plus:V8HI
	 (mult:V8HI
	  (match_operand:V8HI 1 "nonimmediate_operand" "%x")
	  (match_operand:V8HI 2 "nonimmediate_operand" "xm"))
	 (match_operand:V8HI 3 "nonimmediate_operand" "x")))]
  "TARGET_XOP"
  "vpmacsww\t{%3, %2, %1, %0|%0, %1, %2, %3}"
  [(set_attr "type" "ssemuladd")
   (set_attr "mode" "TI")])

(define_insn "xop_pmacssww"
  [(set (match_operand:V8HI 0 "register_operand" "=x")
        (ss_plus:V8HI
	 (mult:V8HI (match_operand:V8HI 1 "nonimmediate_operand" "%x")
		    (match_operand:V8HI 2 "nonimmediate_operand" "xm"))
	 (match_operand:V8HI 3 "nonimmediate_operand" "x")))]
  "TARGET_XOP"
  "vpmacssww\t{%3, %2, %1, %0|%0, %1, %2, %3}"
  [(set_attr "type" "ssemuladd")
   (set_attr "mode" "TI")])

(define_insn "xop_pmacsdd"
  [(set (match_operand:V4SI 0 "register_operand" "=x")
        (plus:V4SI
	 (mult:V4SI
	  (match_operand:V4SI 1 "nonimmediate_operand" "%x")
	  (match_operand:V4SI 2 "nonimmediate_operand" "xm"))
	 (match_operand:V4SI 3 "nonimmediate_operand" "x")))]
  "TARGET_XOP"
  "vpmacsdd\t{%3, %2, %1, %0|%0, %1, %2, %3}"
  [(set_attr "type" "ssemuladd")
   (set_attr "mode" "TI")])

(define_insn "xop_pmacssdd"
  [(set (match_operand:V4SI 0 "register_operand" "=x")
        (ss_plus:V4SI
	 (mult:V4SI (match_operand:V4SI 1 "nonimmediate_operand" "%x")
		    (match_operand:V4SI 2 "nonimmediate_operand" "xm"))
	 (match_operand:V4SI 3 "nonimmediate_operand" "x")))]
  "TARGET_XOP"
  "vpmacssdd\t{%3, %2, %1, %0|%0, %1, %2, %3}"
  [(set_attr "type" "ssemuladd")
   (set_attr "mode" "TI")])

(define_insn "xop_pmacssdql"
  [(set (match_operand:V2DI 0 "register_operand" "=x")
	(ss_plus:V2DI
	 (mult:V2DI
	  (sign_extend:V2DI
	   (vec_select:V2SI
	    (match_operand:V4SI 1 "nonimmediate_operand" "%x")
	    (parallel [(const_int 1)
		       (const_int 3)])))
	  (vec_select:V2SI
	   (match_operand:V4SI 2 "nonimmediate_operand" "xm")
	   (parallel [(const_int 1)
		      (const_int 3)])))
	 (match_operand:V2DI 3 "nonimmediate_operand" "x")))]
  "TARGET_XOP"
  "vpmacssdql\t{%3, %2, %1, %0|%0, %1, %2, %3}"
  [(set_attr "type" "ssemuladd")
   (set_attr "mode" "TI")])

(define_insn "xop_pmacssdqh"
  [(set (match_operand:V2DI 0 "register_operand" "=x")
	(ss_plus:V2DI
	 (mult:V2DI
	  (sign_extend:V2DI
	   (vec_select:V2SI
	    (match_operand:V4SI 1 "nonimmediate_operand" "%x")
	    (parallel [(const_int 0)
		       (const_int 2)])))
	  (sign_extend:V2DI
	   (vec_select:V2SI
	    (match_operand:V4SI 2 "nonimmediate_operand" "xm")
	    (parallel [(const_int 0)
		       (const_int 2)]))))
	 (match_operand:V2DI 3 "nonimmediate_operand" "x")))]
  "TARGET_XOP"
  "vpmacssdqh\t{%3, %2, %1, %0|%0, %1, %2, %3}"
  [(set_attr "type" "ssemuladd")
   (set_attr "mode" "TI")])

(define_insn "xop_pmacsdql"
  [(set (match_operand:V2DI 0 "register_operand" "=x")
	(plus:V2DI
	 (mult:V2DI
	  (sign_extend:V2DI
	   (vec_select:V2SI
	    (match_operand:V4SI 1 "nonimmediate_operand" "%x")
	    (parallel [(const_int 1)
		       (const_int 3)])))
	  (sign_extend:V2DI
	   (vec_select:V2SI
	    (match_operand:V4SI 2 "nonimmediate_operand" "xm")
	    (parallel [(const_int 1)
		       (const_int 3)]))))
	 (match_operand:V2DI 3 "nonimmediate_operand" "x")))]
  "TARGET_XOP"
  "vpmacsdql\t{%3, %2, %1, %0|%0, %1, %2, %3}"
  [(set_attr "type" "ssemuladd")
   (set_attr "mode" "TI")])

;; We don't have a straight 32-bit parallel multiply and extend on XOP, so
;; fake it with a multiply/add.  In general, we expect the define_split to
;; occur before register allocation, so we have to handle the corner case where
;; the target is the same as operands 1/2
(define_insn_and_split "xop_mulv2div2di3_low"
  [(set (match_operand:V2DI 0 "register_operand" "=&x")
	(mult:V2DI
	  (sign_extend:V2DI
	    (vec_select:V2SI
	      (match_operand:V4SI 1 "register_operand" "%x")
	      (parallel [(const_int 1)
			 (const_int 3)])))
	  (sign_extend:V2DI
	    (vec_select:V2SI
	      (match_operand:V4SI 2 "nonimmediate_operand" "xm")
	      (parallel [(const_int 1)
			 (const_int 3)])))))]
  "TARGET_XOP"
  "#"
  "&& reload_completed"
  [(set (match_dup 0)
	(match_dup 3))
   (set (match_dup 0)
	(plus:V2DI
	 (mult:V2DI
	  (sign_extend:V2DI
	   (vec_select:V2SI
	    (match_dup 1)
	    (parallel [(const_int 1)
		       (const_int 3)])))
	  (sign_extend:V2DI
	   (vec_select:V2SI
	    (match_dup 2)
	    (parallel [(const_int 1)
		       (const_int 3)]))))
	 (match_dup 0)))]
{
  operands[3] = CONST0_RTX (V2DImode);
}
  [(set_attr "type" "ssemul")
   (set_attr "mode" "TI")])

(define_insn "xop_pmacsdqh"
  [(set (match_operand:V2DI 0 "register_operand" "=x")
	(plus:V2DI
	 (mult:V2DI
	  (sign_extend:V2DI
	   (vec_select:V2SI
	    (match_operand:V4SI 1 "nonimmediate_operand" "%x")
	    (parallel [(const_int 0)
		       (const_int 2)])))
	  (sign_extend:V2DI
	   (vec_select:V2SI
	    (match_operand:V4SI 2 "nonimmediate_operand" "xm")
	    (parallel [(const_int 0)
		       (const_int 2)]))))
	 (match_operand:V2DI 3 "nonimmediate_operand" "x")))]
  "TARGET_XOP"
  "vpmacsdqh\t{%3, %2, %1, %0|%0, %1, %2, %3}"
  [(set_attr "type" "ssemuladd")
   (set_attr "mode" "TI")])

;; We don't have a straight 32-bit parallel multiply and extend on XOP, so
;; fake it with a multiply/add.  In general, we expect the define_split to
;; occur before register allocation, so we have to handle the corner case where
;; the target is the same as either operands[1] or operands[2]
(define_insn_and_split "xop_mulv2div2di3_high"
  [(set (match_operand:V2DI 0 "register_operand" "=&x")
	(mult:V2DI
	  (sign_extend:V2DI
	    (vec_select:V2SI
	      (match_operand:V4SI 1 "register_operand" "%x")
	      (parallel [(const_int 0)
			 (const_int 2)])))
	  (sign_extend:V2DI
	    (vec_select:V2SI
	      (match_operand:V4SI 2 "nonimmediate_operand" "xm")
	      (parallel [(const_int 0)
			 (const_int 2)])))))]
  "TARGET_XOP"
  "#"
  "&& reload_completed"
  [(set (match_dup 0)
	(match_dup 3))
   (set (match_dup 0)
	(plus:V2DI
	 (mult:V2DI
	  (sign_extend:V2DI
	   (vec_select:V2SI
	    (match_dup 1)
	    (parallel [(const_int 0)
		       (const_int 2)])))
	  (sign_extend:V2DI
	   (vec_select:V2SI
	    (match_dup 2)
	    (parallel [(const_int 0)
		       (const_int 2)]))))
	 (match_dup 0)))]
{
  operands[3] = CONST0_RTX (V2DImode);
}
  [(set_attr "type" "ssemul")
   (set_attr "mode" "TI")])

;; XOP parallel integer multiply/add instructions for the intrinisics
(define_insn "xop_pmacsswd"
  [(set (match_operand:V4SI 0 "register_operand" "=x")
	(ss_plus:V4SI
	 (mult:V4SI
	  (sign_extend:V4SI
	   (vec_select:V4HI
	    (match_operand:V8HI 1 "nonimmediate_operand" "%x")
	    (parallel [(const_int 1)
		       (const_int 3)
		       (const_int 5)
		       (const_int 7)])))
	  (sign_extend:V4SI
	   (vec_select:V4HI
	    (match_operand:V8HI 2 "nonimmediate_operand" "xm")
	    (parallel [(const_int 1)
		       (const_int 3)
		       (const_int 5)
		       (const_int 7)]))))
	 (match_operand:V4SI 3 "nonimmediate_operand" "x")))]
  "TARGET_XOP"
  "vpmacsswd\t{%3, %2, %1, %0|%0, %1, %2, %3}"
  [(set_attr "type" "ssemuladd")
   (set_attr "mode" "TI")])

(define_insn "xop_pmacswd"
  [(set (match_operand:V4SI 0 "register_operand" "=x")
	(plus:V4SI
	 (mult:V4SI
	  (sign_extend:V4SI
	   (vec_select:V4HI
	    (match_operand:V8HI 1 "nonimmediate_operand" "%x")
	    (parallel [(const_int 1)
		       (const_int 3)
		       (const_int 5)
		       (const_int 7)])))
	  (sign_extend:V4SI
	   (vec_select:V4HI
	    (match_operand:V8HI 2 "nonimmediate_operand" "xm")
	    (parallel [(const_int 1)
		       (const_int 3)
		       (const_int 5)
		       (const_int 7)]))))
	 (match_operand:V4SI 3 "nonimmediate_operand" "x")))]
  "TARGET_XOP"
  "vpmacswd\t{%3, %2, %1, %0|%0, %1, %2, %3}"
  [(set_attr "type" "ssemuladd")
   (set_attr "mode" "TI")])

(define_insn "xop_pmadcsswd"
  [(set (match_operand:V4SI 0 "register_operand" "=x")
	(ss_plus:V4SI
	 (plus:V4SI
	  (mult:V4SI
	   (sign_extend:V4SI
	    (vec_select:V4HI
	     (match_operand:V8HI 1 "nonimmediate_operand" "%x")
	     (parallel [(const_int 0)
			(const_int 2)
			(const_int 4)
			(const_int 6)])))
	   (sign_extend:V4SI
	    (vec_select:V4HI
	     (match_operand:V8HI 2 "nonimmediate_operand" "xm")
	     (parallel [(const_int 0)
			(const_int 2)
			(const_int 4)
			(const_int 6)]))))
	  (mult:V4SI
	   (sign_extend:V4SI
	    (vec_select:V4HI
	     (match_dup 1)
	     (parallel [(const_int 1)
			(const_int 3)
			(const_int 5)
			(const_int 7)])))
	   (sign_extend:V4SI
	    (vec_select:V4HI
	     (match_dup 2)
	     (parallel [(const_int 1)
			(const_int 3)
			(const_int 5)
			(const_int 7)])))))
	 (match_operand:V4SI 3 "nonimmediate_operand" "x")))]
  "TARGET_XOP"
  "vpmadcsswd\t{%3, %2, %1, %0|%0, %1, %2, %3}"
  [(set_attr "type" "ssemuladd")
   (set_attr "mode" "TI")])

(define_insn "xop_pmadcswd"
  [(set (match_operand:V4SI 0 "register_operand" "=x")
	(plus:V4SI
	 (plus:V4SI
	  (mult:V4SI
	   (sign_extend:V4SI
	    (vec_select:V4HI
	     (match_operand:V8HI 1 "nonimmediate_operand" "%x")
	     (parallel [(const_int 0)
			(const_int 2)
			(const_int 4)
			(const_int 6)])))
	   (sign_extend:V4SI
	    (vec_select:V4HI
	     (match_operand:V8HI 2 "nonimmediate_operand" "xm")
	     (parallel [(const_int 0)
			(const_int 2)
			(const_int 4)
			(const_int 6)]))))
	  (mult:V4SI
	   (sign_extend:V4SI
	    (vec_select:V4HI
	     (match_dup 1)
	     (parallel [(const_int 1)
			(const_int 3)
			(const_int 5)
			(const_int 7)])))
	   (sign_extend:V4SI
	    (vec_select:V4HI
	     (match_dup 2)
	     (parallel [(const_int 1)
			(const_int 3)
			(const_int 5)
			(const_int 7)])))))
	 (match_operand:V4SI 3 "nonimmediate_operand" "x")))]
  "TARGET_XOP"
  "vpmadcswd\t{%3, %2, %1, %0|%0, %1, %2, %3}"
  [(set_attr "type" "ssemuladd")
   (set_attr "mode" "TI")])

;; XOP parallel XMM conditional moves
(define_insn "xop_pcmov_<mode>"
  [(set (match_operand:SSEMODE 0 "register_operand" "=x,x")
	(if_then_else:SSEMODE
	  (match_operand:SSEMODE 3 "nonimmediate_operand" "x,m")
	  (match_operand:SSEMODE 1 "vector_move_operand" "x,x")
	  (match_operand:SSEMODE 2 "vector_move_operand" "xm,x")))]
  "TARGET_XOP"
  "vpcmov\t{%3, %2, %1, %0|%0, %1, %2, %3}"
  [(set_attr "type" "sse4arg")])

(define_insn "xop_pcmov_<mode>256"
  [(set (match_operand:AVX256MODE 0 "register_operand" "=x,x")
	(if_then_else:AVX256MODE
	  (match_operand:AVX256MODE 3 "nonimmediate_operand" "x,m")
	  (match_operand:AVX256MODE 1 "vector_move_operand" "x,x")
	  (match_operand:AVX256MODE 2 "vector_move_operand" "xm,x")))]
  "TARGET_XOP"
  "vpcmov\t{%3, %2, %1, %0|%0, %1, %2, %3}"
  [(set_attr "type" "sse4arg")])

;; XOP horizontal add/subtract instructions
(define_insn "xop_phaddbw"
  [(set (match_operand:V8HI 0 "register_operand" "=x")
	(plus:V8HI
	 (sign_extend:V8HI
	  (vec_select:V8QI
	   (match_operand:V16QI 1 "nonimmediate_operand" "xm")
	   (parallel [(const_int 0)
		      (const_int 2)
		      (const_int 4)
		      (const_int 6)
		      (const_int 8)
		      (const_int 10)
		      (const_int 12)
		      (const_int 14)])))
	 (sign_extend:V8HI
	  (vec_select:V8QI
	   (match_dup 1)
	   (parallel [(const_int 1)
		      (const_int 3)
		      (const_int 5)
		      (const_int 7)
		      (const_int 9)
		      (const_int 11)
		      (const_int 13)
		      (const_int 15)])))))]
  "TARGET_XOP"
  "vphaddbw\t{%1, %0|%0, %1}"
  [(set_attr "type" "sseiadd1")])

(define_insn "xop_phaddbd"
  [(set (match_operand:V4SI 0 "register_operand" "=x")
	(plus:V4SI
	 (plus:V4SI
	  (sign_extend:V4SI
	   (vec_select:V4QI
	    (match_operand:V16QI 1 "nonimmediate_operand" "xm")
	    (parallel [(const_int 0)
		       (const_int 4)
		       (const_int 8)
		       (const_int 12)])))
	  (sign_extend:V4SI
	   (vec_select:V4QI
	    (match_dup 1)
	    (parallel [(const_int 1)
		       (const_int 5)
		       (const_int 9)
		       (const_int 13)]))))
	 (plus:V4SI
	  (sign_extend:V4SI
	   (vec_select:V4QI
	    (match_dup 1)
	    (parallel [(const_int 2)
		       (const_int 6)
		       (const_int 10)
		       (const_int 14)])))
	  (sign_extend:V4SI
	   (vec_select:V4QI
	    (match_dup 1)
	    (parallel [(const_int 3)
		       (const_int 7)
		       (const_int 11)
		       (const_int 15)]))))))]
  "TARGET_XOP"
  "vphaddbd\t{%1, %0|%0, %1}"
  [(set_attr "type" "sseiadd1")])

(define_insn "xop_phaddbq"
  [(set (match_operand:V2DI 0 "register_operand" "=x")
	(plus:V2DI
	 (plus:V2DI
	  (plus:V2DI
	   (sign_extend:V2DI
	    (vec_select:V2QI
	     (match_operand:V16QI 1 "nonimmediate_operand" "xm")
	     (parallel [(const_int 0)
			(const_int 4)])))
	   (sign_extend:V2DI
	    (vec_select:V2QI
	     (match_dup 1)
	     (parallel [(const_int 1)
			(const_int 5)]))))
	  (plus:V2DI
	   (sign_extend:V2DI
	    (vec_select:V2QI
	     (match_dup 1)
	     (parallel [(const_int 2)
			(const_int 6)])))
	   (sign_extend:V2DI
	    (vec_select:V2QI
	     (match_dup 1)
	     (parallel [(const_int 3)
			(const_int 7)])))))
	 (plus:V2DI
	  (plus:V2DI
	   (sign_extend:V2DI
	    (vec_select:V2QI
	     (match_dup 1)
	     (parallel [(const_int 8)
			(const_int 12)])))
	   (sign_extend:V2DI
	    (vec_select:V2QI
	     (match_dup 1)
	     (parallel [(const_int 9)
			(const_int 13)]))))
	  (plus:V2DI
	   (sign_extend:V2DI
	    (vec_select:V2QI
	     (match_dup 1)
	     (parallel [(const_int 10)
			(const_int 14)])))
	   (sign_extend:V2DI
	    (vec_select:V2QI
	     (match_dup 1)
	     (parallel [(const_int 11)
			(const_int 15)])))))))]
  "TARGET_XOP"
  "vphaddbq\t{%1, %0|%0, %1}"
  [(set_attr "type" "sseiadd1")])

(define_insn "xop_phaddwd"
  [(set (match_operand:V4SI 0 "register_operand" "=x")
	(plus:V4SI
	 (sign_extend:V4SI
	  (vec_select:V4HI
	   (match_operand:V8HI 1 "nonimmediate_operand" "xm")
	   (parallel [(const_int 0)
		      (const_int 2)
		      (const_int 4)
		      (const_int 6)])))
	 (sign_extend:V4SI
	  (vec_select:V4HI
	   (match_dup 1)
	   (parallel [(const_int 1)
		      (const_int 3)
		      (const_int 5)
		      (const_int 7)])))))]
  "TARGET_XOP"
  "vphaddwd\t{%1, %0|%0, %1}"
  [(set_attr "type" "sseiadd1")])

(define_insn "xop_phaddwq"
  [(set (match_operand:V2DI 0 "register_operand" "=x")
	(plus:V2DI
	 (plus:V2DI
	  (sign_extend:V2DI
	   (vec_select:V2HI
	    (match_operand:V8HI 1 "nonimmediate_operand" "xm")
	    (parallel [(const_int 0)
		       (const_int 4)])))
	  (sign_extend:V2DI
	   (vec_select:V2HI
	    (match_dup 1)
	    (parallel [(const_int 1)
		       (const_int 5)]))))
	 (plus:V2DI
	  (sign_extend:V2DI
	   (vec_select:V2HI
	    (match_dup 1)
	    (parallel [(const_int 2)
		       (const_int 6)])))
	  (sign_extend:V2DI
	   (vec_select:V2HI
	    (match_dup 1)
	    (parallel [(const_int 3)
		       (const_int 7)]))))))]
  "TARGET_XOP"
  "vphaddwq\t{%1, %0|%0, %1}"
  [(set_attr "type" "sseiadd1")])

(define_insn "xop_phadddq"
  [(set (match_operand:V2DI 0 "register_operand" "=x")
	(plus:V2DI
	 (sign_extend:V2DI
	  (vec_select:V2SI
	   (match_operand:V4SI 1 "nonimmediate_operand" "xm")
	   (parallel [(const_int 0)
		      (const_int 2)])))
	 (sign_extend:V2DI
	  (vec_select:V2SI
	   (match_dup 1)
	   (parallel [(const_int 1)
		      (const_int 3)])))))]
  "TARGET_XOP"
  "vphadddq\t{%1, %0|%0, %1}"
  [(set_attr "type" "sseiadd1")])

(define_insn "xop_phaddubw"
  [(set (match_operand:V8HI 0 "register_operand" "=x")
	(plus:V8HI
	 (zero_extend:V8HI
	  (vec_select:V8QI
	   (match_operand:V16QI 1 "nonimmediate_operand" "xm")
	   (parallel [(const_int 0)
		      (const_int 2)
		      (const_int 4)
		      (const_int 6)
		      (const_int 8)
		      (const_int 10)
		      (const_int 12)
		      (const_int 14)])))
	 (zero_extend:V8HI
	  (vec_select:V8QI
	   (match_dup 1)
	   (parallel [(const_int 1)
		      (const_int 3)
		      (const_int 5)
		      (const_int 7)
		      (const_int 9)
		      (const_int 11)
		      (const_int 13)
		      (const_int 15)])))))]
  "TARGET_XOP"
  "vphaddubw\t{%1, %0|%0, %1}"
  [(set_attr "type" "sseiadd1")])

(define_insn "xop_phaddubd"
  [(set (match_operand:V4SI 0 "register_operand" "=x")
	(plus:V4SI
	 (plus:V4SI
	  (zero_extend:V4SI
	   (vec_select:V4QI
	    (match_operand:V16QI 1 "nonimmediate_operand" "xm")
	    (parallel [(const_int 0)
		       (const_int 4)
		       (const_int 8)
		       (const_int 12)])))
	  (zero_extend:V4SI
	   (vec_select:V4QI
	    (match_dup 1)
	    (parallel [(const_int 1)
		       (const_int 5)
		       (const_int 9)
		       (const_int 13)]))))
	 (plus:V4SI
	  (zero_extend:V4SI
	   (vec_select:V4QI
	    (match_dup 1)
	    (parallel [(const_int 2)
		       (const_int 6)
		       (const_int 10)
		       (const_int 14)])))
	  (zero_extend:V4SI
	   (vec_select:V4QI
	    (match_dup 1)
	    (parallel [(const_int 3)
		       (const_int 7)
		       (const_int 11)
		       (const_int 15)]))))))]
  "TARGET_XOP"
  "vphaddubd\t{%1, %0|%0, %1}"
  [(set_attr "type" "sseiadd1")])

(define_insn "xop_phaddubq"
  [(set (match_operand:V2DI 0 "register_operand" "=x")
	(plus:V2DI
	 (plus:V2DI
	  (plus:V2DI
	   (zero_extend:V2DI
	    (vec_select:V2QI
	     (match_operand:V16QI 1 "nonimmediate_operand" "xm")
	     (parallel [(const_int 0)
			(const_int 4)])))
	   (sign_extend:V2DI
	    (vec_select:V2QI
	     (match_dup 1)
	     (parallel [(const_int 1)
			(const_int 5)]))))
	  (plus:V2DI
	   (zero_extend:V2DI
	    (vec_select:V2QI
	     (match_dup 1)
	     (parallel [(const_int 2)
			(const_int 6)])))
	   (zero_extend:V2DI
	    (vec_select:V2QI
	     (match_dup 1)
	     (parallel [(const_int 3)
			(const_int 7)])))))
	 (plus:V2DI
	  (plus:V2DI
	   (zero_extend:V2DI
	    (vec_select:V2QI
	     (match_dup 1)
	     (parallel [(const_int 8)
			(const_int 12)])))
	   (sign_extend:V2DI
	    (vec_select:V2QI
	     (match_dup 1)
	     (parallel [(const_int 9)
			(const_int 13)]))))
	  (plus:V2DI
	   (zero_extend:V2DI
	    (vec_select:V2QI
	     (match_dup 1)
	     (parallel [(const_int 10)
			(const_int 14)])))
	   (zero_extend:V2DI
	    (vec_select:V2QI
	     (match_dup 1)
	     (parallel [(const_int 11)
			(const_int 15)])))))))]
  "TARGET_XOP"
  "vphaddubq\t{%1, %0|%0, %1}"
  [(set_attr "type" "sseiadd1")])

(define_insn "xop_phadduwd"
  [(set (match_operand:V4SI 0 "register_operand" "=x")
	(plus:V4SI
	 (zero_extend:V4SI
	  (vec_select:V4HI
	   (match_operand:V8HI 1 "nonimmediate_operand" "xm")
	   (parallel [(const_int 0)
		      (const_int 2)
		      (const_int 4)
		      (const_int 6)])))
	 (zero_extend:V4SI
	  (vec_select:V4HI
	   (match_dup 1)
	   (parallel [(const_int 1)
		      (const_int 3)
		      (const_int 5)
		      (const_int 7)])))))]
  "TARGET_XOP"
  "vphadduwd\t{%1, %0|%0, %1}"
  [(set_attr "type" "sseiadd1")])

(define_insn "xop_phadduwq"
  [(set (match_operand:V2DI 0 "register_operand" "=x")
	(plus:V2DI
	 (plus:V2DI
	  (zero_extend:V2DI
	   (vec_select:V2HI
	    (match_operand:V8HI 1 "nonimmediate_operand" "xm")
	    (parallel [(const_int 0)
		       (const_int 4)])))
	  (zero_extend:V2DI
	   (vec_select:V2HI
	    (match_dup 1)
	    (parallel [(const_int 1)
		       (const_int 5)]))))
	 (plus:V2DI
	  (zero_extend:V2DI
	   (vec_select:V2HI
	    (match_dup 1)
	    (parallel [(const_int 2)
		       (const_int 6)])))
	  (zero_extend:V2DI
	   (vec_select:V2HI
	    (match_dup 1)
	    (parallel [(const_int 3)
		       (const_int 7)]))))))]
  "TARGET_XOP"
  "vphadduwq\t{%1, %0|%0, %1}"
  [(set_attr "type" "sseiadd1")])

(define_insn "xop_phaddudq"
  [(set (match_operand:V2DI 0 "register_operand" "=x")
	(plus:V2DI
	 (zero_extend:V2DI
	  (vec_select:V2SI
	   (match_operand:V4SI 1 "nonimmediate_operand" "xm")
	   (parallel [(const_int 0)
		      (const_int 2)])))
	 (zero_extend:V2DI
	  (vec_select:V2SI
	   (match_dup 1)
	   (parallel [(const_int 1)
		      (const_int 3)])))))]
  "TARGET_XOP"
  "vphaddudq\t{%1, %0|%0, %1}"
  [(set_attr "type" "sseiadd1")])

(define_insn "xop_phsubbw"
  [(set (match_operand:V8HI 0 "register_operand" "=x")
	(minus:V8HI
	 (sign_extend:V8HI
	  (vec_select:V8QI
	   (match_operand:V16QI 1 "nonimmediate_operand" "xm")
	   (parallel [(const_int 0)
		      (const_int 2)
		      (const_int 4)
		      (const_int 6)
		      (const_int 8)
		      (const_int 10)
		      (const_int 12)
		      (const_int 14)])))
	 (sign_extend:V8HI
	  (vec_select:V8QI
	   (match_dup 1)
	   (parallel [(const_int 1)
		      (const_int 3)
		      (const_int 5)
		      (const_int 7)
		      (const_int 9)
		      (const_int 11)
		      (const_int 13)
		      (const_int 15)])))))]
  "TARGET_XOP"
  "vphsubbw\t{%1, %0|%0, %1}"
  [(set_attr "type" "sseiadd1")])

(define_insn "xop_phsubwd"
  [(set (match_operand:V4SI 0 "register_operand" "=x")
	(minus:V4SI
	 (sign_extend:V4SI
	  (vec_select:V4HI
	   (match_operand:V8HI 1 "nonimmediate_operand" "xm")
	   (parallel [(const_int 0)
		      (const_int 2)
		      (const_int 4)
		      (const_int 6)])))
	 (sign_extend:V4SI
	  (vec_select:V4HI
	   (match_dup 1)
	   (parallel [(const_int 1)
		      (const_int 3)
		      (const_int 5)
		      (const_int 7)])))))]
  "TARGET_XOP"
  "vphsubwd\t{%1, %0|%0, %1}"
  [(set_attr "type" "sseiadd1")])

(define_insn "xop_phsubdq"
  [(set (match_operand:V2DI 0 "register_operand" "=x")
	(minus:V2DI
	 (sign_extend:V2DI
	  (vec_select:V2SI
	   (match_operand:V4SI 1 "nonimmediate_operand" "xm")
	   (parallel [(const_int 0)
		      (const_int 2)])))
	 (sign_extend:V2DI
	  (vec_select:V2SI
	   (match_dup 1)
	   (parallel [(const_int 1)
		      (const_int 3)])))))]
  "TARGET_XOP"
  "vphsubdq\t{%1, %0|%0, %1}"
  [(set_attr "type" "sseiadd1")])

;; XOP permute instructions
(define_insn "xop_pperm"
  [(set (match_operand:V16QI 0 "register_operand" "=x,x")
	(unspec:V16QI
	  [(match_operand:V16QI 1 "register_operand" "x,x")
	   (match_operand:V16QI 2 "nonimmediate_operand" "x,m")
	   (match_operand:V16QI 3 "nonimmediate_operand" "xm,x")]
	  UNSPEC_XOP_PERMUTE))]
  "TARGET_XOP && !(MEM_P (operands[2]) && MEM_P (operands[3]))"
  "vpperm\t{%3, %2, %1, %0|%0, %1, %2, %3}"
  [(set_attr "type" "sse4arg")
   (set_attr "mode" "TI")])

;; XOP pack instructions that combine two vectors into a smaller vector
(define_insn "xop_pperm_pack_v2di_v4si"
  [(set (match_operand:V4SI 0 "register_operand" "=x,x")
	(vec_concat:V4SI
	 (truncate:V2SI
	  (match_operand:V2DI 1 "register_operand" "x,x"))
	 (truncate:V2SI
	  (match_operand:V2DI 2 "nonimmediate_operand" "x,m"))))
   (use (match_operand:V16QI 3 "nonimmediate_operand" "xm,x"))]
  "TARGET_XOP && !(MEM_P (operands[2]) && MEM_P (operands[3]))"
  "vpperm\t{%3, %2, %1, %0|%0, %1, %2, %3}"
  [(set_attr "type" "sse4arg")
   (set_attr "mode" "TI")])

(define_insn "xop_pperm_pack_v4si_v8hi"
  [(set (match_operand:V8HI 0 "register_operand" "=x,x")
	(vec_concat:V8HI
	 (truncate:V4HI
	  (match_operand:V4SI 1 "register_operand" "x,x"))
	 (truncate:V4HI
	  (match_operand:V4SI 2 "nonimmediate_operand" "x,m"))))
   (use (match_operand:V16QI 3 "nonimmediate_operand" "xm,x"))]
  "TARGET_XOP && !(MEM_P (operands[2]) && MEM_P (operands[3]))"
  "vpperm\t{%3, %2, %1, %0|%0, %1, %2, %3}"
  [(set_attr "type" "sse4arg")
   (set_attr "mode" "TI")])

(define_insn "xop_pperm_pack_v8hi_v16qi"
  [(set (match_operand:V16QI 0 "register_operand" "=x,x")
	(vec_concat:V16QI
	 (truncate:V8QI
	  (match_operand:V8HI 1 "register_operand" "x,x"))
	 (truncate:V8QI
	  (match_operand:V8HI 2 "nonimmediate_operand" "x,m"))))
   (use (match_operand:V16QI 3 "nonimmediate_operand" "xm,x"))]
  "TARGET_XOP && !(MEM_P (operands[2]) && MEM_P (operands[3]))"
  "vpperm\t{%3, %2, %1, %0|%0, %1, %2, %3}"
  [(set_attr "type" "sse4arg")
   (set_attr "mode" "TI")])

;; XOP packed rotate instructions
(define_expand "rotl<mode>3"
  [(set (match_operand:SSEMODE1248 0 "register_operand" "")
	(rotate:SSEMODE1248
	 (match_operand:SSEMODE1248 1 "nonimmediate_operand" "")
	 (match_operand:SI 2 "general_operand")))]
  "TARGET_XOP"
{
  /* If we were given a scalar, convert it to parallel */
  if (! const_0_to_<sserotatemax>_operand (operands[2], SImode))
    {
      rtvec vs = rtvec_alloc (<ssescalarnum>);
      rtx par = gen_rtx_PARALLEL (<MODE>mode, vs);
      rtx reg = gen_reg_rtx (<MODE>mode);
      rtx op2 = operands[2];
      int i;

      if (GET_MODE (op2) != <ssescalarmode>mode)
        {
	  op2 = gen_reg_rtx (<ssescalarmode>mode);
	  convert_move (op2, operands[2], false);
	}

      for (i = 0; i < <ssescalarnum>; i++)
	RTVEC_ELT (vs, i) = op2;

      emit_insn (gen_vec_init<mode> (reg, par));
      emit_insn (gen_xop_vrotl<mode>3 (operands[0], operands[1], reg));
      DONE;
    }
})

(define_expand "rotr<mode>3"
  [(set (match_operand:SSEMODE1248 0 "register_operand" "")
	(rotatert:SSEMODE1248
	 (match_operand:SSEMODE1248 1 "nonimmediate_operand" "")
	 (match_operand:SI 2 "general_operand")))]
  "TARGET_XOP"
{
  /* If we were given a scalar, convert it to parallel */
  if (! const_0_to_<sserotatemax>_operand (operands[2], SImode))
    {
      rtvec vs = rtvec_alloc (<ssescalarnum>);
      rtx par = gen_rtx_PARALLEL (<MODE>mode, vs);
      rtx neg = gen_reg_rtx (<MODE>mode);
      rtx reg = gen_reg_rtx (<MODE>mode);
      rtx op2 = operands[2];
      int i;

      if (GET_MODE (op2) != <ssescalarmode>mode)
        {
	  op2 = gen_reg_rtx (<ssescalarmode>mode);
	  convert_move (op2, operands[2], false);
	}

      for (i = 0; i < <ssescalarnum>; i++)
	RTVEC_ELT (vs, i) = op2;

      emit_insn (gen_vec_init<mode> (reg, par));
      emit_insn (gen_neg<mode>2 (neg, reg));
      emit_insn (gen_xop_vrotl<mode>3 (operands[0], operands[1], neg));
      DONE;
    }
})

(define_insn "xop_rotl<mode>3"
  [(set (match_operand:SSEMODE1248 0 "register_operand" "=x")
	(rotate:SSEMODE1248
	 (match_operand:SSEMODE1248 1 "nonimmediate_operand" "xm")
	 (match_operand:SI 2 "const_0_to_<sserotatemax>_operand" "n")))]
  "TARGET_XOP"
  "vprot<ssevecsize>\t{%2, %1, %0|%0, %1, %2}"
  [(set_attr "type" "sseishft")
   (set_attr "length_immediate" "1")
   (set_attr "mode" "TI")])

(define_insn "xop_rotr<mode>3"
  [(set (match_operand:SSEMODE1248 0 "register_operand" "=x")
	(rotatert:SSEMODE1248
	 (match_operand:SSEMODE1248 1 "nonimmediate_operand" "xm")
	 (match_operand:SI 2 "const_0_to_<sserotatemax>_operand" "n")))]
  "TARGET_XOP"
{
  operands[3] = GEN_INT ((<ssescalarnum> * 8) - INTVAL (operands[2]));
  return \"vprot<ssevecsize>\t{%3, %1, %0|%0, %1, %3}\";
}
  [(set_attr "type" "sseishft")
   (set_attr "length_immediate" "1")
   (set_attr "mode" "TI")])

(define_expand "vrotr<mode>3"
  [(match_operand:SSEMODE1248 0 "register_operand" "")
   (match_operand:SSEMODE1248 1 "register_operand" "")
   (match_operand:SSEMODE1248 2 "register_operand" "")]
  "TARGET_XOP"
{
  rtx reg = gen_reg_rtx (<MODE>mode);
  emit_insn (gen_neg<mode>2 (reg, operands[2]));
  emit_insn (gen_xop_vrotl<mode>3 (operands[0], operands[1], reg));
  DONE;
})

(define_expand "vrotl<mode>3"
  [(match_operand:SSEMODE1248 0 "register_operand" "")
   (match_operand:SSEMODE1248 1 "register_operand" "")
   (match_operand:SSEMODE1248 2 "register_operand" "")]
  "TARGET_XOP"
{
  emit_insn (gen_xop_vrotl<mode>3 (operands[0], operands[1], operands[2]));
  DONE;
})

(define_insn "xop_vrotl<mode>3"
  [(set (match_operand:SSEMODE1248 0 "register_operand" "=x,x")
	(if_then_else:SSEMODE1248
	 (ge:SSEMODE1248
	  (match_operand:SSEMODE1248 2 "nonimmediate_operand" "x,m")
	  (const_int 0))
	 (rotate:SSEMODE1248
	  (match_operand:SSEMODE1248 1 "nonimmediate_operand" "xm,x")
	  (match_dup 2))
	 (rotatert:SSEMODE1248
	  (match_dup 1)
	  (neg:SSEMODE1248 (match_dup 2)))))]
  "TARGET_XOP && !(MEM_P (operands[1]) && MEM_P (operands[2]))"
  "vprot<ssevecsize>\t{%2, %1, %0|%0, %1, %2}"
  [(set_attr "type" "sseishft")
   (set_attr "prefix_data16" "0")
   (set_attr "prefix_extra" "2")
   (set_attr "mode" "TI")])

;; XOP packed shift instructions.
;; FIXME: add V2DI back in
(define_expand "vlshr<mode>3"
  [(match_operand:SSEMODE124 0 "register_operand" "")
   (match_operand:SSEMODE124 1 "register_operand" "")
   (match_operand:SSEMODE124 2 "register_operand" "")]
  "TARGET_XOP"
{
  rtx neg = gen_reg_rtx (<MODE>mode);
  emit_insn (gen_neg<mode>2 (neg, operands[2]));
  emit_insn (gen_xop_lshl<mode>3 (operands[0], operands[1], neg));
  DONE;
})

(define_expand "vashr<mode>3"
  [(match_operand:SSEMODE124 0 "register_operand" "")
   (match_operand:SSEMODE124 1 "register_operand" "")
   (match_operand:SSEMODE124 2 "register_operand" "")]
  "TARGET_XOP"
{
  rtx neg = gen_reg_rtx (<MODE>mode);
  emit_insn (gen_neg<mode>2 (neg, operands[2]));
  emit_insn (gen_xop_ashl<mode>3 (operands[0], operands[1], neg));
  DONE;
})

(define_expand "vashl<mode>3"
  [(match_operand:SSEMODE124 0 "register_operand" "")
   (match_operand:SSEMODE124 1 "register_operand" "")
   (match_operand:SSEMODE124 2 "register_operand" "")]
  "TARGET_XOP"
{
  emit_insn (gen_xop_ashl<mode>3 (operands[0], operands[1], operands[2]));
  DONE;
})

(define_insn "xop_ashl<mode>3"
  [(set (match_operand:SSEMODE1248 0 "register_operand" "=x,x")
	(if_then_else:SSEMODE1248
	 (ge:SSEMODE1248
	  (match_operand:SSEMODE1248 2 "nonimmediate_operand" "x,m")
	  (const_int 0))
	 (ashift:SSEMODE1248
	  (match_operand:SSEMODE1248 1 "nonimmediate_operand" "xm,x")
	  (match_dup 2))
	 (ashiftrt:SSEMODE1248
	  (match_dup 1)
	  (neg:SSEMODE1248 (match_dup 2)))))]
  "TARGET_XOP && !(MEM_P (operands[1]) && MEM_P (operands[2]))"
  "vpsha<ssevecsize>\t{%2, %1, %0|%0, %1, %2}"
  [(set_attr "type" "sseishft")
   (set_attr "prefix_data16" "0")
   (set_attr "prefix_extra" "2")
   (set_attr "mode" "TI")])

(define_insn "xop_lshl<mode>3"
  [(set (match_operand:SSEMODE1248 0 "register_operand" "=x,x")
	(if_then_else:SSEMODE1248
	 (ge:SSEMODE1248
	  (match_operand:SSEMODE1248 2 "nonimmediate_operand" "x,m")
	  (const_int 0))
	 (ashift:SSEMODE1248
	  (match_operand:SSEMODE1248 1 "nonimmediate_operand" "xm,x")
	  (match_dup 2))
	 (lshiftrt:SSEMODE1248
	  (match_dup 1)
	  (neg:SSEMODE1248 (match_dup 2)))))]
  "TARGET_XOP && !(MEM_P (operands[1]) && MEM_P (operands[2]))"
  "vpshl<ssevecsize>\t{%2, %1, %0|%0, %1, %2}"
  [(set_attr "type" "sseishft")
   (set_attr "prefix_data16" "0")
   (set_attr "prefix_extra" "2")
   (set_attr "mode" "TI")])

;; SSE2 doesn't have some shift varients, so define versions for XOP
(define_expand "ashlv16qi3"
  [(match_operand:V16QI 0 "register_operand" "")
   (match_operand:V16QI 1 "register_operand" "")
   (match_operand:SI 2 "nonmemory_operand" "")]
  "TARGET_XOP"
{
  rtvec vs = rtvec_alloc (16);
  rtx par = gen_rtx_PARALLEL (V16QImode, vs);
  rtx reg = gen_reg_rtx (V16QImode);
  int i;
  for (i = 0; i < 16; i++)
    RTVEC_ELT (vs, i) = operands[2];

  emit_insn (gen_vec_initv16qi (reg, par));
  emit_insn (gen_xop_ashlv16qi3 (operands[0], operands[1], reg));
  DONE;
})

(define_expand "lshlv16qi3"
  [(match_operand:V16QI 0 "register_operand" "")
   (match_operand:V16QI 1 "register_operand" "")
   (match_operand:SI 2 "nonmemory_operand" "")]
  "TARGET_XOP"
{
  rtvec vs = rtvec_alloc (16);
  rtx par = gen_rtx_PARALLEL (V16QImode, vs);
  rtx reg = gen_reg_rtx (V16QImode);
  int i;
  for (i = 0; i < 16; i++)
    RTVEC_ELT (vs, i) = operands[2];

  emit_insn (gen_vec_initv16qi (reg, par));
  emit_insn (gen_xop_lshlv16qi3 (operands[0], operands[1], reg));
  DONE;
})

(define_expand "ashrv16qi3"
  [(match_operand:V16QI 0 "register_operand" "")
   (match_operand:V16QI 1 "register_operand" "")
   (match_operand:SI 2 "nonmemory_operand" "")]
  "TARGET_XOP"
{
  rtvec vs = rtvec_alloc (16);
  rtx par = gen_rtx_PARALLEL (V16QImode, vs);
  rtx reg = gen_reg_rtx (V16QImode);
  int i;
  rtx ele = ((CONST_INT_P (operands[2]))
	     ? GEN_INT (- INTVAL (operands[2]))
	     : operands[2]);

  for (i = 0; i < 16; i++)
    RTVEC_ELT (vs, i) = ele;

  emit_insn (gen_vec_initv16qi (reg, par));

  if (!CONST_INT_P (operands[2]))
    {
      rtx neg = gen_reg_rtx (V16QImode);
      emit_insn (gen_negv16qi2 (neg, reg));
      emit_insn (gen_xop_ashlv16qi3 (operands[0], operands[1], neg));
    }
  else
    emit_insn (gen_xop_ashlv16qi3 (operands[0], operands[1], reg));

  DONE;
})

(define_expand "ashrv2di3"
  [(match_operand:V2DI 0 "register_operand" "")
   (match_operand:V2DI 1 "register_operand" "")
   (match_operand:DI 2 "nonmemory_operand" "")]
  "TARGET_XOP"
{
  rtvec vs = rtvec_alloc (2);
  rtx par = gen_rtx_PARALLEL (V2DImode, vs);
  rtx reg = gen_reg_rtx (V2DImode);
  rtx ele;

  if (CONST_INT_P (operands[2]))
    ele = GEN_INT (- INTVAL (operands[2]));
  else if (GET_MODE (operands[2]) != DImode)
    {
      rtx move = gen_reg_rtx (DImode);
      ele = gen_reg_rtx (DImode);
      convert_move (move, operands[2], false);
      emit_insn (gen_negdi2 (ele, move));
    }
  else
    {
      ele = gen_reg_rtx (DImode);
      emit_insn (gen_negdi2 (ele, operands[2]));
    }

  RTVEC_ELT (vs, 0) = ele;
  RTVEC_ELT (vs, 1) = ele;
  emit_insn (gen_vec_initv2di (reg, par));
  emit_insn (gen_xop_ashlv2di3 (operands[0], operands[1], reg));
  DONE;
})

;; XOP FRCZ support
(define_insn "xop_frcz<mode>2"
  [(set (match_operand:FMAMODE 0 "register_operand" "=x")
	(unspec:FMAMODE
	 [(match_operand:FMAMODE 1 "nonimmediate_operand" "xm")]
	 UNSPEC_FRCZ))]
  "TARGET_XOP"
  "vfrcz<ssemodesuffix>\t{%1, %0|%0, %1}"
  [(set_attr "type" "ssecvt1")
   (set_attr "mode" "<MODE>")])

;; scalar insns
(define_expand "xop_vmfrcz<mode>2"
  [(set (match_operand:SSEMODEF2P 0 "register_operand")
	(vec_merge:SSEMODEF2P
	  (unspec:SSEMODEF2P
	   [(match_operand:SSEMODEF2P 1 "nonimmediate_operand")]
	   UNSPEC_FRCZ)
	  (match_dup 3)
	  (const_int 1)))]
  "TARGET_XOP"
{
  operands[3] = CONST0_RTX (<MODE>mode);
})

(define_insn "*xop_vmfrcz_<mode>"
  [(set (match_operand:SSEMODEF2P 0 "register_operand" "=x")
	(vec_merge:SSEMODEF2P
	  (unspec:SSEMODEF2P
	   [(match_operand:SSEMODEF2P 1 "nonimmediate_operand" "xm")]
	   UNSPEC_FRCZ)
	  (match_operand:SSEMODEF2P 2 "const0_operand")
	  (const_int 1)))]
  "TARGET_XOP"
  "vfrcz<ssescalarmodesuffix>\t{%1, %0|%0, %1}"
  [(set_attr "type" "ssecvt1")
   (set_attr "mode" "<MODE>")])

(define_insn "xop_maskcmp<mode>3"
  [(set (match_operand:SSEMODE1248 0 "register_operand" "=x")
	(match_operator:SSEMODE1248 1 "ix86_comparison_int_operator"
	 [(match_operand:SSEMODE1248 2 "register_operand" "x")
	  (match_operand:SSEMODE1248 3 "nonimmediate_operand" "xm")]))]
  "TARGET_XOP"
  "vpcom%Y1<ssevecsize>\t{%3, %2, %0|%0, %2, %3}"
  [(set_attr "type" "sse4arg")
   (set_attr "prefix_data16" "0")
   (set_attr "prefix_rep" "0")
   (set_attr "prefix_extra" "2")
   (set_attr "length_immediate" "1")
   (set_attr "mode" "TI")])

(define_insn "xop_maskcmp_uns<mode>3"
  [(set (match_operand:SSEMODE1248 0 "register_operand" "=x")
	(match_operator:SSEMODE1248 1 "ix86_comparison_uns_operator"
	 [(match_operand:SSEMODE1248 2 "register_operand" "x")
	  (match_operand:SSEMODE1248 3 "nonimmediate_operand" "xm")]))]
  "TARGET_XOP"
  "vpcom%Y1u<ssevecsize>\t{%3, %2, %0|%0, %2, %3}"
  [(set_attr "type" "ssecmp")
   (set_attr "prefix_data16" "0")
   (set_attr "prefix_rep" "0")
   (set_attr "prefix_extra" "2")
   (set_attr "length_immediate" "1")
   (set_attr "mode" "TI")])

;; Version of pcom*u* that is called from the intrinsics that allows pcomequ*
;; and pcomneu* not to be converted to the signed ones in case somebody needs
;; the exact instruction generated for the intrinsic.
(define_insn "xop_maskcmp_uns2<mode>3"
  [(set (match_operand:SSEMODE1248 0 "register_operand" "=x")
	(unspec:SSEMODE1248
	 [(match_operator:SSEMODE1248 1 "ix86_comparison_uns_operator"
	  [(match_operand:SSEMODE1248 2 "register_operand" "x")
	   (match_operand:SSEMODE1248 3 "nonimmediate_operand" "xm")])]
	 UNSPEC_XOP_UNSIGNED_CMP))]
  "TARGET_XOP"
  "vpcom%Y1u<ssevecsize>\t{%3, %2, %0|%0, %2, %3}"
  [(set_attr "type" "ssecmp")
   (set_attr "prefix_data16" "0")
   (set_attr "prefix_extra" "2")
   (set_attr "length_immediate" "1")
   (set_attr "mode" "TI")])

;; Pcomtrue and pcomfalse support.  These are useless instructions, but are
;; being added here to be complete.
(define_insn "xop_pcom_tf<mode>3"
  [(set (match_operand:SSEMODE1248 0 "register_operand" "=x")
	(unspec:SSEMODE1248
	  [(match_operand:SSEMODE1248 1 "register_operand" "x")
	   (match_operand:SSEMODE1248 2 "nonimmediate_operand" "xm")
	   (match_operand:SI 3 "const_int_operand" "n")]
	  UNSPEC_XOP_TRUEFALSE))]
  "TARGET_XOP"
{
  return ((INTVAL (operands[3]) != 0)
	  ? "vpcomtrue<ssevecsize>\t{%2, %1, %0|%0, %1, %2}"
	  : "vpcomfalse<ssevecsize>\t{%2, %1, %0|%0, %1, %2}");
}
  [(set_attr "type" "ssecmp")
   (set_attr "prefix_data16" "0")
   (set_attr "prefix_extra" "2")
   (set_attr "length_immediate" "1")
   (set_attr "mode" "TI")])

(define_insn "xop_vpermil2<mode>3"
  [(set (match_operand:AVXMODEF2P 0 "register_operand" "=x")
	(unspec:AVXMODEF2P
	  [(match_operand:AVXMODEF2P 1 "register_operand" "x")
	   (match_operand:AVXMODEF2P 2 "nonimmediate_operand" "%x")
	   (match_operand:<avxpermvecmode> 3 "nonimmediate_operand" "xm")
	   (match_operand:SI 4 "const_0_to_3_operand" "n")]
	  UNSPEC_VPERMIL2))]
  "TARGET_XOP"
  "vpermil2<ssemodesuffix>\t{%4, %3, %2, %1, %0|%0, %1, %2, %3, %4}"
  [(set_attr "type" "sse4arg")
   (set_attr "length_immediate" "1")
   (set_attr "mode" "<MODE>")])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define_insn "*avx_aesenc"
  [(set (match_operand:V2DI 0 "register_operand" "=x")
	(unspec:V2DI [(match_operand:V2DI 1 "register_operand" "x")
		       (match_operand:V2DI 2 "nonimmediate_operand" "xm")]
		      UNSPEC_AESENC))]
  "TARGET_AES && TARGET_AVX"
  "vaesenc\t{%2, %1, %0|%0, %1, %2}"
  [(set_attr "type" "sselog1")
   (set_attr "prefix_extra" "1")
   (set_attr "prefix" "vex")
   (set_attr "mode" "TI")])

(define_insn "aesenc"
  [(set (match_operand:V2DI 0 "register_operand" "=x")
	(unspec:V2DI [(match_operand:V2DI 1 "register_operand" "0")
		       (match_operand:V2DI 2 "nonimmediate_operand" "xm")]
		      UNSPEC_AESENC))]
  "TARGET_AES"
  "aesenc\t{%2, %0|%0, %2}"
  [(set_attr "type" "sselog1")
   (set_attr "prefix_extra" "1")
   (set_attr "mode" "TI")])

(define_insn "*avx_aesenclast"
  [(set (match_operand:V2DI 0 "register_operand" "=x")
	(unspec:V2DI [(match_operand:V2DI 1 "register_operand" "x")
		       (match_operand:V2DI 2 "nonimmediate_operand" "xm")]
		      UNSPEC_AESENCLAST))]
  "TARGET_AES && TARGET_AVX"
  "vaesenclast\t{%2, %1, %0|%0, %1, %2}"
  [(set_attr "type" "sselog1")
   (set_attr "prefix_extra" "1")
   (set_attr "prefix" "vex")
   (set_attr "mode" "TI")])

(define_insn "aesenclast"
  [(set (match_operand:V2DI 0 "register_operand" "=x")
	(unspec:V2DI [(match_operand:V2DI 1 "register_operand" "0")
		       (match_operand:V2DI 2 "nonimmediate_operand" "xm")]
		      UNSPEC_AESENCLAST))]
  "TARGET_AES"
  "aesenclast\t{%2, %0|%0, %2}"
  [(set_attr "type" "sselog1")
   (set_attr "prefix_extra" "1")
   (set_attr "mode" "TI")])

(define_insn "*avx_aesdec"
  [(set (match_operand:V2DI 0 "register_operand" "=x")
	(unspec:V2DI [(match_operand:V2DI 1 "register_operand" "x")
		       (match_operand:V2DI 2 "nonimmediate_operand" "xm")]
		      UNSPEC_AESDEC))]
  "TARGET_AES && TARGET_AVX"
  "vaesdec\t{%2, %1, %0|%0, %1, %2}"
  [(set_attr "type" "sselog1")
   (set_attr "prefix_extra" "1")
   (set_attr "prefix" "vex")
   (set_attr "mode" "TI")])

(define_insn "aesdec"
  [(set (match_operand:V2DI 0 "register_operand" "=x")
	(unspec:V2DI [(match_operand:V2DI 1 "register_operand" "0")
		       (match_operand:V2DI 2 "nonimmediate_operand" "xm")]
		      UNSPEC_AESDEC))]
  "TARGET_AES"
  "aesdec\t{%2, %0|%0, %2}"
  [(set_attr "type" "sselog1")
   (set_attr "prefix_extra" "1")
   (set_attr "mode" "TI")])

(define_insn "*avx_aesdeclast"
  [(set (match_operand:V2DI 0 "register_operand" "=x")
	(unspec:V2DI [(match_operand:V2DI 1 "register_operand" "x")
		       (match_operand:V2DI 2 "nonimmediate_operand" "xm")]
		      UNSPEC_AESDECLAST))]
  "TARGET_AES && TARGET_AVX"
  "vaesdeclast\t{%2, %1, %0|%0, %1, %2}"
  [(set_attr "type" "sselog1")
   (set_attr "prefix_extra" "1")
   (set_attr "prefix" "vex")
   (set_attr "mode" "TI")])

(define_insn "aesdeclast"
  [(set (match_operand:V2DI 0 "register_operand" "=x")
	(unspec:V2DI [(match_operand:V2DI 1 "register_operand" "0")
		       (match_operand:V2DI 2 "nonimmediate_operand" "xm")]
		      UNSPEC_AESDECLAST))]
  "TARGET_AES"
  "aesdeclast\t{%2, %0|%0, %2}"
  [(set_attr "type" "sselog1")
   (set_attr "prefix_extra" "1")
   (set_attr "mode" "TI")])

(define_insn "aesimc"
  [(set (match_operand:V2DI 0 "register_operand" "=x")
	(unspec:V2DI [(match_operand:V2DI 1 "nonimmediate_operand" "xm")]
		      UNSPEC_AESIMC))]
  "TARGET_AES"
  "%vaesimc\t{%1, %0|%0, %1}"
  [(set_attr "type" "sselog1")
   (set_attr "prefix_extra" "1")
   (set_attr "prefix" "maybe_vex")
   (set_attr "mode" "TI")])

(define_insn "aeskeygenassist"
  [(set (match_operand:V2DI 0 "register_operand" "=x")
	(unspec:V2DI [(match_operand:V2DI 1 "nonimmediate_operand" "xm")
		      (match_operand:SI 2 "const_0_to_255_operand" "n")]
		     UNSPEC_AESKEYGENASSIST))]
  "TARGET_AES"
  "%vaeskeygenassist\t{%2, %1, %0|%0, %1, %2}"
  [(set_attr "type" "sselog1")
   (set_attr "prefix_extra" "1")
   (set_attr "length_immediate" "1")
   (set_attr "prefix" "maybe_vex")
   (set_attr "mode" "TI")])

(define_insn "*vpclmulqdq"
  [(set (match_operand:V2DI 0 "register_operand" "=x")
	(unspec:V2DI [(match_operand:V2DI 1 "register_operand" "x")
		      (match_operand:V2DI 2 "nonimmediate_operand" "xm")
		      (match_operand:SI 3 "const_0_to_255_operand" "n")]
		     UNSPEC_PCLMUL))]
  "TARGET_PCLMUL && TARGET_AVX"
  "vpclmulqdq\t{%3, %2, %1, %0|%0, %1, %2, %3}"
  [(set_attr "type" "sselog1")
   (set_attr "prefix_extra" "1")
   (set_attr "length_immediate" "1")
   (set_attr "prefix" "vex")
   (set_attr "mode" "TI")])

(define_insn "pclmulqdq"
  [(set (match_operand:V2DI 0 "register_operand" "=x")
	(unspec:V2DI [(match_operand:V2DI 1 "register_operand" "0")
		      (match_operand:V2DI 2 "nonimmediate_operand" "xm")
		      (match_operand:SI 3 "const_0_to_255_operand" "n")]
		     UNSPEC_PCLMUL))]
  "TARGET_PCLMUL"
  "pclmulqdq\t{%3, %2, %0|%0, %2, %3}"
  [(set_attr "type" "sselog1")
   (set_attr "prefix_extra" "1")
   (set_attr "length_immediate" "1")
   (set_attr "mode" "TI")])

(define_expand "avx_vzeroall"
  [(match_par_dup 0 [(const_int 0)])]
  "TARGET_AVX"
{
  int nregs = TARGET_64BIT ? 16 : 8;
  int regno;

  operands[0] = gen_rtx_PARALLEL (VOIDmode, rtvec_alloc (nregs + 1));

  XVECEXP (operands[0], 0, 0)
    = gen_rtx_UNSPEC_VOLATILE (VOIDmode, gen_rtvec (1, const0_rtx),
			       UNSPECV_VZEROALL);

  for (regno = 0; regno < nregs; regno++)
    XVECEXP (operands[0], 0, regno + 1)
      = gen_rtx_SET (VOIDmode,
		     gen_rtx_REG (V8SImode, SSE_REGNO (regno)),
		     CONST0_RTX (V8SImode));
})

(define_insn "*avx_vzeroall"
  [(match_parallel 0 "vzeroall_operation"
    [(unspec_volatile [(const_int 0)] UNSPECV_VZEROALL)])]
  "TARGET_AVX"
  "vzeroall"
  [(set_attr "type" "sse")
   (set_attr "modrm" "0")
   (set_attr "memory" "none")
   (set_attr "prefix" "vex")
   (set_attr "mode" "OI")])

;; Clear the upper 128bits of AVX registers, equivalent to a NOP
;; if the upper 128bits are unused.
(define_insn "avx_vzeroupper"
  [(unspec_volatile [(match_operand 0 "const_int_operand" "")]
		    UNSPECV_VZEROUPPER)]
  "TARGET_AVX"
  "vzeroupper"
  [(set_attr "type" "sse")
   (set_attr "modrm" "0")
   (set_attr "memory" "none")
   (set_attr "prefix" "vex")
   (set_attr "mode" "OI")])

(define_insn_and_split "vec_dup<mode>"
  [(set (match_operand:AVX256MODE24P 0 "register_operand" "=x,x")
	(vec_duplicate:AVX256MODE24P
	  (match_operand:<avxscalarmode> 1 "nonimmediate_operand" "m,?x")))]
  "TARGET_AVX"
  "@
   vbroadcast<ssescalarmodesuffix>\t{%1, %0|%0, %1}
   #"
  "&& reload_completed && REG_P (operands[1])"
  [(set (match_dup 2) (vec_duplicate:<avxhalfvecmode> (match_dup 1)))
   (set (match_dup 0) (vec_concat:AVX256MODE24P (match_dup 2) (match_dup 2)))]
  "operands[2] = gen_rtx_REG (<avxhalfvecmode>mode, REGNO (operands[0]));"
  [(set_attr "type" "ssemov")
   (set_attr "prefix_extra" "1")
   (set_attr "prefix" "vex")
   (set_attr "mode" "V8SF")])

(define_insn "avx_vbroadcastf128_<mode>"
  [(set (match_operand:AVX256MODE 0 "register_operand" "=x,x,x")
	(vec_concat:AVX256MODE
	  (match_operand:<avxhalfvecmode> 1 "nonimmediate_operand" "m,0,?x")
	  (match_dup 1)))]
  "TARGET_AVX"
  "@
   vbroadcastf128\t{%1, %0|%0, %1}
   vinsertf128\t{$1, %1, %0, %0|%0, %0, %1, 1}
   vperm2f128\t{$0, %t1, %t1, %0|%0, %t1, %t1, 0}"
  [(set_attr "type" "ssemov,sselog1,sselog1")
   (set_attr "prefix_extra" "1")
   (set_attr "length_immediate" "0,1,1")
   (set_attr "prefix" "vex")
   (set_attr "mode" "V4SF,V8SF,V8SF")])

;; Recognize broadcast as a vec_select as produced by builtin_vec_perm.
;; If it so happens that the input is in memory, use vbroadcast.
;; Otherwise use vpermilp (and in the case of 256-bit modes, vperm2f128).
(define_insn "*avx_vperm_broadcast_v4sf"
  [(set (match_operand:V4SF 0 "register_operand" "=x,x,x")
	(vec_select:V4SF
	  (match_operand:V4SF 1 "nonimmediate_operand" "m,o,x")
	  (match_parallel 2 "avx_vbroadcast_operand"
	    [(match_operand 3 "const_int_operand" "C,n,n")])))]
  "TARGET_AVX"
{
  int elt = INTVAL (operands[3]);
  switch (which_alternative)
    {
    case 0:
    case 1:
      operands[1] = adjust_address_nv (operands[1], SFmode, elt * 4);
      return "vbroadcastss\t{%1, %0|%0, %1}";
    case 2:
      operands[2] = GEN_INT (elt * 0x55);
      return "vpermilps\t{%2, %1, %0|%0, %1, %2}";
    default:
      gcc_unreachable ();
    }
}
  [(set_attr "type" "ssemov,ssemov,sselog1")
   (set_attr "prefix_extra" "1")
   (set_attr "length_immediate" "0,0,1")
   (set_attr "prefix" "vex")
   (set_attr "mode" "SF,SF,V4SF")])

(define_insn_and_split "*avx_vperm_broadcast_<mode>"
  [(set (match_operand:AVX256MODEF2P 0 "register_operand" "=x,x,x")
	(vec_select:AVX256MODEF2P
	  (match_operand:AVX256MODEF2P 1 "nonimmediate_operand" "m,o,?x")
	  (match_parallel 2 "avx_vbroadcast_operand"
	    [(match_operand 3 "const_int_operand" "C,n,n")])))]
  "TARGET_AVX"
  "#"
  "&& reload_completed"
  [(set (match_dup 0) (vec_duplicate:AVX256MODEF2P (match_dup 1)))]
{
  rtx op0 = operands[0], op1 = operands[1];
  int elt = INTVAL (operands[3]);

  if (REG_P (op1))
    {
      int mask;

      /* Shuffle element we care about into all elements of the 128-bit lane.
	 The other lane gets shuffled too, but we don't care.  */
      if (<MODE>mode == V4DFmode)
	mask = (elt & 1 ? 15 : 0);
      else
	mask = (elt & 3) * 0x55;
      emit_insn (gen_avx_vpermil<mode> (op0, op1, GEN_INT (mask)));

      /* Shuffle the lane we care about into both lanes of the dest.  */
      mask = (elt / (<ssescalarnum> / 2)) * 0x11;
      emit_insn (gen_avx_vperm2f128<mode>3 (op0, op0, op0, GEN_INT (mask)));
      DONE;
    }

  operands[1] = adjust_address_nv (op1, <avxscalarmode>mode,
  	      			   elt * GET_MODE_SIZE (<avxscalarmode>mode));
})

(define_expand "avx_vpermil<mode>"
  [(set (match_operand:AVXMODEFDP 0 "register_operand" "")
	(vec_select:AVXMODEFDP
	  (match_operand:AVXMODEFDP 1 "nonimmediate_operand" "")
	  (match_operand:SI 2 "const_0_to_255_operand" "")))]
  "TARGET_AVX"
{
  int mask = INTVAL (operands[2]);
  rtx perm[<ssescalarnum>];

  perm[0] = GEN_INT (mask & 1);
  perm[1] = GEN_INT ((mask >> 1) & 1);
  if (<MODE>mode == V4DFmode)
    {
      perm[2] = GEN_INT (((mask >> 2) & 1) + 2);
      perm[3] = GEN_INT (((mask >> 3) & 1) + 2);
    }

  operands[2]
    = gen_rtx_PARALLEL (VOIDmode, gen_rtvec_v (<ssescalarnum>, perm));
})

(define_expand "avx_vpermil<mode>"
  [(set (match_operand:AVXMODEFSP 0 "register_operand" "")
	(vec_select:AVXMODEFSP
	  (match_operand:AVXMODEFSP 1 "nonimmediate_operand" "")
	  (match_operand:SI 2 "const_0_to_255_operand" "")))]
  "TARGET_AVX"
{
  int mask = INTVAL (operands[2]);
  rtx perm[<ssescalarnum>];

  perm[0] = GEN_INT (mask & 3);
  perm[1] = GEN_INT ((mask >> 2) & 3);
  perm[2] = GEN_INT ((mask >> 4) & 3);
  perm[3] = GEN_INT ((mask >> 6) & 3);
  if (<MODE>mode == V8SFmode)
    {
      perm[4] = GEN_INT ((mask & 3) + 4);
      perm[5] = GEN_INT (((mask >> 2) & 3) + 4);
      perm[6] = GEN_INT (((mask >> 4) & 3) + 4);
      perm[7] = GEN_INT (((mask >> 6) & 3) + 4);
    }

  operands[2]
    = gen_rtx_PARALLEL (VOIDmode, gen_rtvec_v (<ssescalarnum>, perm));
})

(define_insn "*avx_vpermilp<mode>"
  [(set (match_operand:AVXMODEF2P 0 "register_operand" "=x")
	(vec_select:AVXMODEF2P
	  (match_operand:AVXMODEF2P 1 "nonimmediate_operand" "xm")
	  (match_parallel 2 "avx_vpermilp_<mode>_operand"
	    [(match_operand 3 "const_int_operand" "")])))]
  "TARGET_AVX"
{
  int mask = avx_vpermilp_parallel (operands[2], <MODE>mode) - 1;
  operands[2] = GEN_INT (mask);
  return "vpermil<ssemodesuffix>\t{%2, %1, %0|%0, %1, %2}";
}
  [(set_attr "type" "sselog")
   (set_attr "prefix_extra" "1")
   (set_attr "length_immediate" "1")
   (set_attr "prefix" "vex")
   (set_attr "mode" "<MODE>")])

(define_insn "avx_vpermilvar<mode>3"
  [(set (match_operand:AVXMODEF2P 0 "register_operand" "=x")
	(unspec:AVXMODEF2P
	  [(match_operand:AVXMODEF2P 1 "register_operand" "x")
	   (match_operand:<avxpermvecmode> 2 "nonimmediate_operand" "xm")]
	  UNSPEC_VPERMIL))]
  "TARGET_AVX"
  "vpermil<ssemodesuffix>\t{%2, %1, %0|%0, %1, %2}"
  [(set_attr "type" "sselog")
   (set_attr "prefix_extra" "1")
   (set_attr "prefix" "vex")
   (set_attr "mode" "<MODE>")])

(define_expand "avx_vperm2f128<mode>3"
  [(set (match_operand:AVX256MODE2P 0 "register_operand" "")
	(unspec:AVX256MODE2P
	  [(match_operand:AVX256MODE2P 1 "register_operand" "")
	   (match_operand:AVX256MODE2P 2 "nonimmediate_operand" "")
	   (match_operand:SI 3 "const_0_to_255_operand" "")]
	  UNSPEC_VPERMIL2F128))]
  "TARGET_AVX"
{
  int mask = INTVAL (operands[3]);
  if ((mask & 0x88) == 0)
    {
      rtx perm[<ssescalarnum>], t1, t2;
      int i, base, nelt = <ssescalarnum>, nelt2 = nelt / 2;

      base = (mask & 3) * nelt2;
      for (i = 0; i < nelt2; ++i)
	perm[i] = GEN_INT (base + i);

      base = ((mask >> 4) & 3) * nelt2;
      for (i = 0; i < nelt2; ++i)
	perm[i + nelt2] = GEN_INT (base + i);

      t2 = gen_rtx_VEC_CONCAT (<ssedoublesizemode>mode,
			       operands[1], operands[2]);
      t1 = gen_rtx_PARALLEL (VOIDmode, gen_rtvec_v (nelt, perm));
      t2 = gen_rtx_VEC_SELECT (<MODE>mode, t2, t1);
      t2 = gen_rtx_SET (VOIDmode, operands[0], t2);
      emit_insn (t2);
      DONE;
    }
})

;; Note that bits 7 and 3 of the imm8 allow lanes to be zeroed, which
;; means that in order to represent this properly in rtl we'd have to
;; nest *another* vec_concat with a zero operand and do the select from
;; a 4x wide vector.  That doesn't seem very nice.
(define_insn "*avx_vperm2f128<mode>_full"
  [(set (match_operand:AVX256MODE2P 0 "register_operand" "=x")
	(unspec:AVX256MODE2P
	  [(match_operand:AVX256MODE2P 1 "register_operand" "x")
	   (match_operand:AVX256MODE2P 2 "nonimmediate_operand" "xm")
	   (match_operand:SI 3 "const_0_to_255_operand" "n")]
	  UNSPEC_VPERMIL2F128))]
  "TARGET_AVX"
  "vperm2f128\t{%3, %2, %1, %0|%0, %1, %2, %3}"
  [(set_attr "type" "sselog")
   (set_attr "prefix_extra" "1")
   (set_attr "length_immediate" "1")
   (set_attr "prefix" "vex")
   (set_attr "mode" "V8SF")])

(define_insn "*avx_vperm2f128<mode>_nozero"
  [(set (match_operand:AVX256MODE2P 0 "register_operand" "=x")
	(vec_select:AVX256MODE2P
	  (vec_concat:<ssedoublesizemode>
	    (match_operand:AVX256MODE2P 1 "register_operand" "x")
	    (match_operand:AVX256MODE2P 2 "nonimmediate_operand" "xm"))
	  (match_parallel 3 "avx_vperm2f128_<mode>_operand"
	    [(match_operand 4 "const_int_operand" "")])))]
  "TARGET_AVX"
{
  int mask = avx_vperm2f128_parallel (operands[3], <MODE>mode) - 1;
  operands[3] = GEN_INT (mask);
  return "vperm2f128\t{%3, %2, %1, %0|%0, %1, %2, %3}";
}
  [(set_attr "type" "sselog")
   (set_attr "prefix_extra" "1")
   (set_attr "length_immediate" "1")
   (set_attr "prefix" "vex")
   (set_attr "mode" "V8SF")])

(define_expand "avx_vinsertf128<mode>"
  [(match_operand:AVX256MODE 0 "register_operand" "")
   (match_operand:AVX256MODE 1 "register_operand" "")
   (match_operand:<avxhalfvecmode> 2 "nonimmediate_operand" "")
   (match_operand:SI 3 "const_0_to_1_operand" "")]
  "TARGET_AVX"
{
  switch (INTVAL (operands[3]))
    {
    case 0:
      emit_insn (gen_vec_set_lo_<mode> (operands[0], operands[1],
					operands[2]));
      break;
    case 1:
      emit_insn (gen_vec_set_hi_<mode> (operands[0], operands[1],
					operands[2]));
      break;
    default:
      gcc_unreachable ();
    }
  DONE;
})

(define_insn "vec_set_lo_<mode>"
  [(set (match_operand:AVX256MODE4P 0 "register_operand" "=x")
	(vec_concat:AVX256MODE4P
	  (match_operand:<avxhalfvecmode> 2 "nonimmediate_operand" "xm")
	  (vec_select:<avxhalfvecmode>
	    (match_operand:AVX256MODE4P 1 "register_operand" "x")
	    (parallel [(const_int 2) (const_int 3)]))))]
  "TARGET_AVX"
  "vinsertf128\t{$0x0, %2, %1, %0|%0, %1, %2, 0x0}"
  [(set_attr "type" "sselog")
   (set_attr "prefix_extra" "1")
   (set_attr "length_immediate" "1")
   (set_attr "prefix" "vex")
   (set_attr "mode" "V8SF")])

(define_insn "vec_set_hi_<mode>"
  [(set (match_operand:AVX256MODE4P 0 "register_operand" "=x")
	(vec_concat:AVX256MODE4P
	  (vec_select:<avxhalfvecmode>
	    (match_operand:AVX256MODE4P 1 "register_operand" "x")
	    (parallel [(const_int 0) (const_int 1)]))
	  (match_operand:<avxhalfvecmode> 2 "nonimmediate_operand" "xm")))]
  "TARGET_AVX"
  "vinsertf128\t{$0x1, %2, %1, %0|%0, %1, %2, 0x1}"
  [(set_attr "type" "sselog")
   (set_attr "prefix_extra" "1")
   (set_attr "length_immediate" "1")
   (set_attr "prefix" "vex")
   (set_attr "mode" "V8SF")])

(define_insn "vec_set_lo_<mode>"
  [(set (match_operand:AVX256MODE8P 0 "register_operand" "=x")
	(vec_concat:AVX256MODE8P
	  (match_operand:<avxhalfvecmode> 2 "nonimmediate_operand" "xm")
	  (vec_select:<avxhalfvecmode>
	    (match_operand:AVX256MODE8P 1 "register_operand" "x")
	    (parallel [(const_int 4) (const_int 5)
		       (const_int 6) (const_int 7)]))))]
  "TARGET_AVX"
  "vinsertf128\t{$0x0, %2, %1, %0|%0, %1, %2, 0x0}"
  [(set_attr "type" "sselog")
   (set_attr "prefix_extra" "1")
   (set_attr "length_immediate" "1")
   (set_attr "prefix" "vex")
   (set_attr "mode" "V8SF")])

(define_insn "vec_set_hi_<mode>"
  [(set (match_operand:AVX256MODE8P 0 "register_operand" "=x")
	(vec_concat:AVX256MODE8P
	  (vec_select:<avxhalfvecmode>
	    (match_operand:AVX256MODE8P 1 "register_operand" "x")
	    (parallel [(const_int 0) (const_int 1)
		       (const_int 2) (const_int 3)]))
	  (match_operand:<avxhalfvecmode> 2 "nonimmediate_operand" "xm")))]
  "TARGET_AVX"
  "vinsertf128\t{$0x1, %2, %1, %0|%0, %1, %2, 0x1}"
  [(set_attr "type" "sselog")
   (set_attr "prefix_extra" "1")
   (set_attr "length_immediate" "1")
   (set_attr "prefix" "vex")
   (set_attr "mode" "V8SF")])

(define_insn "vec_set_lo_v16hi"
  [(set (match_operand:V16HI 0 "register_operand" "=x")
	(vec_concat:V16HI
	  (match_operand:V8HI 2 "nonimmediate_operand" "xm")
	  (vec_select:V8HI
	    (match_operand:V16HI 1 "register_operand" "x")
	    (parallel [(const_int 8) (const_int 9)
		       (const_int 10) (const_int 11)
		       (const_int 12) (const_int 13)
		       (const_int 14) (const_int 15)]))))]
  "TARGET_AVX"
  "vinsertf128\t{$0x0, %2, %1, %0|%0, %1, %2, 0x0}"
  [(set_attr "type" "sselog")
   (set_attr "prefix_extra" "1")
   (set_attr "length_immediate" "1")
   (set_attr "prefix" "vex")
   (set_attr "mode" "V8SF")])

(define_insn "vec_set_hi_v16hi"
  [(set (match_operand:V16HI 0 "register_operand" "=x")
	(vec_concat:V16HI
	  (vec_select:V8HI
	    (match_operand:V16HI 1 "register_operand" "x")
	    (parallel [(const_int 0) (const_int 1)
		       (const_int 2) (const_int 3)
		       (const_int 4) (const_int 5)
		       (const_int 6) (const_int 7)]))
	  (match_operand:V8HI 2 "nonimmediate_operand" "xm")))]
  "TARGET_AVX"
  "vinsertf128\t{$0x1, %2, %1, %0|%0, %1, %2, 0x1}"
  [(set_attr "type" "sselog")
   (set_attr "prefix_extra" "1")
   (set_attr "length_immediate" "1")
   (set_attr "prefix" "vex")
   (set_attr "mode" "V8SF")])

(define_insn "vec_set_lo_v32qi"
  [(set (match_operand:V32QI 0 "register_operand" "=x")
	(vec_concat:V32QI
	  (match_operand:V16QI 2 "nonimmediate_operand" "xm")
	  (vec_select:V16QI
	    (match_operand:V32QI 1 "register_operand" "x")
	    (parallel [(const_int 16) (const_int 17)
		       (const_int 18) (const_int 19)
		       (const_int 20) (const_int 21)
		       (const_int 22) (const_int 23)
		       (const_int 24) (const_int 25)
		       (const_int 26) (const_int 27)
		       (const_int 28) (const_int 29)
		       (const_int 30) (const_int 31)]))))]
  "TARGET_AVX"
  "vinsertf128\t{$0x0, %2, %1, %0|%0, %1, %2, 0x0}"
  [(set_attr "type" "sselog")
   (set_attr "prefix_extra" "1")
   (set_attr "length_immediate" "1")
   (set_attr "prefix" "vex")
   (set_attr "mode" "V8SF")])

(define_insn "vec_set_hi_v32qi"
  [(set (match_operand:V32QI 0 "register_operand" "=x")
	(vec_concat:V32QI
	  (vec_select:V16QI
	    (match_operand:V32QI 1 "register_operand" "x")
	    (parallel [(const_int 0) (const_int 1)
		       (const_int 2) (const_int 3)
		       (const_int 4) (const_int 5)
		       (const_int 6) (const_int 7)
		       (const_int 8) (const_int 9)
		       (const_int 10) (const_int 11)
		       (const_int 12) (const_int 13)
		       (const_int 14) (const_int 15)]))
	  (match_operand:V16QI 2 "nonimmediate_operand" "xm")))]
  "TARGET_AVX"
  "vinsertf128\t{$0x1, %2, %1, %0|%0, %1, %2, 0x1}"
  [(set_attr "type" "sselog")
   (set_attr "prefix_extra" "1")
   (set_attr "length_immediate" "1")
   (set_attr "prefix" "vex")
   (set_attr "mode" "V8SF")])

(define_insn "avx_maskload<ssemodesuffix><avxmodesuffix>"
  [(set (match_operand:AVXMODEF2P 0 "register_operand" "=x")
	(unspec:AVXMODEF2P
	  [(match_operand:AVXMODEF2P 1 "memory_operand" "m")
	   (match_operand:<avxpermvecmode> 2 "register_operand" "x")
	   (match_dup 0)]
	  UNSPEC_MASKLOAD))]
  "TARGET_AVX"
  "vmaskmov<ssemodesuffix>\t{%1, %2, %0|%0, %2, %1}"
  [(set_attr "type" "sselog1")
   (set_attr "prefix_extra" "1")
   (set_attr "prefix" "vex")
   (set_attr "mode" "<MODE>")])

(define_insn "avx_maskstore<ssemodesuffix><avxmodesuffix>"
  [(set (match_operand:AVXMODEF2P 0 "memory_operand" "=m")
	(unspec:AVXMODEF2P
	  [(match_operand:<avxpermvecmode> 1 "register_operand" "x")
	   (match_operand:AVXMODEF2P 2 "register_operand" "x")
	   (match_dup 0)]
	  UNSPEC_MASKSTORE))]
  "TARGET_AVX"
  "vmaskmov<ssemodesuffix>\t{%2, %1, %0|%0, %1, %2}"
  [(set_attr "type" "sselog1")
   (set_attr "prefix_extra" "1")
   (set_attr "prefix" "vex")
   (set_attr "mode" "<MODE>")])

(define_insn_and_split "avx_<avxmodesuffixp><avxmodesuffix>_<avxmodesuffixp>"
  [(set (match_operand:AVX256MODE2P 0 "nonimmediate_operand" "=x,m")
	(unspec:AVX256MODE2P
	  [(match_operand:<avxhalfvecmode> 1 "nonimmediate_operand" "xm,x")]
	  UNSPEC_CAST))]
  "TARGET_AVX"
  "#"
  "&& reload_completed"
  [(const_int 0)]
{
  rtx op0 = operands[0];
  rtx op1 = operands[1];
  if (REG_P (op0))
    op0 = gen_rtx_REG (<avxhalfvecmode>mode, REGNO (op0));
  else 
    op1 = gen_rtx_REG (<MODE>mode, REGNO (op1));
  emit_move_insn (op0, op1);
  DONE;
})

(define_expand "vec_init<mode>"
  [(match_operand:AVX256MODE 0 "register_operand" "")
   (match_operand 1 "" "")]
  "TARGET_AVX"
{
  ix86_expand_vector_init (false, operands[0], operands[1]);
  DONE;
})

(define_insn "*vec_concat<mode>_avx"
  [(set (match_operand:AVX256MODE 0 "register_operand"   "=x,x")
	(vec_concat:AVX256MODE
	  (match_operand:<avxhalfvecmode> 1 "register_operand" "x,x")
	  (match_operand:<avxhalfvecmode> 2 "vector_move_operand" "xm,C")))]
  "TARGET_AVX"
{
  switch (which_alternative)
    {
    case 0:
      return "vinsertf128\t{$0x1, %2, %t1, %0|%0, %t1, %2, 0x1}";
    case 1:
      switch (get_attr_mode (insn))
        {
	case MODE_V8SF:
	  return "vmovaps\t{%1, %x0|%x0, %1}";
	case MODE_V4DF:
	  return "vmovapd\t{%1, %x0|%x0, %1}";
	default:
	  return "vmovdqa\t{%1, %x0|%x0, %1}";
	}
    default:
      gcc_unreachable ();
    }
}
  [(set_attr "type" "sselog,ssemov")
   (set_attr "prefix_extra" "1,*")
   (set_attr "length_immediate" "1,*")
   (set_attr "prefix" "vex")
   (set_attr "mode" "<avxvecmode>")])

(define_insn "vcvtph2ps"
  [(set (match_operand:V4SF 0 "register_operand" "=x")
	(vec_select:V4SF
	  (unspec:V8SF [(match_operand:V8HI 1 "register_operand" "x")]
		       UNSPEC_VCVTPH2PS)
	  (parallel [(const_int 0) (const_int 1)
		     (const_int 1) (const_int 2)])))]
  "TARGET_F16C"
  "vcvtph2ps\t{%1, %0|%0, %1}"
  [(set_attr "type" "ssecvt")
   (set_attr "prefix" "vex")
   (set_attr "mode" "V4SF")])

(define_insn "*vcvtph2ps_load"
  [(set (match_operand:V4SF 0 "register_operand" "=x")
	(unspec:V4SF [(match_operand:V4HI 1 "memory_operand" "m")]
		     UNSPEC_VCVTPH2PS))]
  "TARGET_F16C"
  "vcvtph2ps\t{%1, %0|%0, %1}"
  [(set_attr "type" "ssecvt")
   (set_attr "prefix" "vex")
   (set_attr "mode" "V8SF")])

(define_insn "vcvtph2ps256"
  [(set (match_operand:V8SF 0 "register_operand" "=x")
	(unspec:V8SF [(match_operand:V8HI 1 "nonimmediate_operand" "xm")]
		     UNSPEC_VCVTPH2PS))]
  "TARGET_F16C"
  "vcvtph2ps\t{%1, %0|%0, %1}"
  [(set_attr "type" "ssecvt")
   (set_attr "prefix" "vex")
   (set_attr "mode" "V8SF")])

(define_expand "vcvtps2ph"
  [(set (match_operand:V8HI 0 "register_operand" "")
	(vec_concat:V8HI
	  (unspec:V4HI [(match_operand:V4SF 1 "register_operand" "")
			(match_operand:SI 2 "const_0_to_255_operand" "")]
		       UNSPEC_VCVTPS2PH)
	  (match_dup 3)))]
  "TARGET_F16C"
  "operands[3] = CONST0_RTX (V4HImode);")

(define_insn "*vcvtps2ph"
  [(set (match_operand:V8HI 0 "register_operand" "=x")
	(vec_concat:V8HI
	  (unspec:V4HI [(match_operand:V4SF 1 "register_operand" "x")
			(match_operand:SI 2 "const_0_to_255_operand" "N")]
		       UNSPEC_VCVTPS2PH)
	  (match_operand:V4HI 3 "const0_operand" "")))]
  "TARGET_F16C"
  "vcvtps2ph\t{%2, %1, %0|%0, %1, %2}"
  [(set_attr "type" "ssecvt")
   (set_attr "prefix" "vex")
   (set_attr "mode" "V4SF")])

(define_insn "*vcvtps2ph_store"
  [(set (match_operand:V4HI 0 "memory_operand" "=m")
	(unspec:V4HI [(match_operand:V4SF 1 "register_operand" "x")
		      (match_operand:SI 2 "const_0_to_255_operand" "N")]
		     UNSPEC_VCVTPS2PH))]
  "TARGET_F16C"
  "vcvtps2ph\t{%2, %1, %0|%0, %1, %2}"
  [(set_attr "type" "ssecvt")
   (set_attr "prefix" "vex")
   (set_attr "mode" "V4SF")])

(define_insn "vcvtps2ph256"
  [(set (match_operand:V8HI 0 "nonimmediate_operand" "=xm")
	(unspec:V8HI [(match_operand:V8SF 1 "register_operand" "x")
		      (match_operand:SI 2 "const_0_to_255_operand" "N")]
		     UNSPEC_VCVTPS2PH))]
  "TARGET_F16C"
  "vcvtps2ph\t{%2, %1, %0|%0, %1, %2}"
  [(set_attr "type" "ssecvt")
   (set_attr "prefix" "vex")
   (set_attr "mode" "V8SF")])
