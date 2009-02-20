;; Machine description for ST Microelectronics Loongson-2E/2F.
;; Copyright (C) 2008, 2009 Free Software Foundation, Inc.
;; Contributed by CodeSourcery.
;;
;; This file is part of GCC.
;;
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

;; Mode iterators and attributes.

;; 64-bit vectors of bytes.
(define_mode_iterator VB [V8QI])

;; 64-bit vectors of halfwords.
(define_mode_iterator VH [V4HI])

;; 64-bit vectors of words.
(define_mode_iterator VW [V2SI])

;; 64-bit vectors of halfwords and bytes.
(define_mode_iterator VHB [V4HI V8QI])

;; 64-bit vectors of words and halfwords.
(define_mode_iterator VWH [V2SI V4HI])

;; 64-bit vectors of words, halfwords and bytes.
(define_mode_iterator VWHB [V2SI V4HI V8QI])

;; 64-bit vectors of words, halfwords and bytes; and DImode.
(define_mode_iterator VWHBDI [V2SI V4HI V8QI DI])

;; The Loongson instruction suffixes corresponding to the modes in the
;; VWHBDI iterator.
(define_mode_attr V_suffix [(V2SI "w") (V4HI "h") (V8QI "b") (DI "d")])

;; Given a vector type T, the mode of a vector half the size of T
;; and with the same number of elements.
(define_mode_attr V_squash [(V2SI "V2HI") (V4HI "V4QI")])

;; Given a vector type T, the mode of a vector the same size as T
;; but with half as many elements.
(define_mode_attr V_stretch_half [(V2SI "DI") (V4HI "V2SI") (V8QI "V4HI")])

;; The Loongson instruction suffixes corresponding to the transformation
;; expressed by V_stretch_half.
(define_mode_attr V_stretch_half_suffix [(V2SI "wd") (V4HI "hw") (V8QI "bh")])

;; Given a vector type T, the mode of a vector the same size as T
;; but with twice as many elements.
(define_mode_attr V_squash_double [(V2SI "V4HI") (V4HI "V8QI")])

;; The Loongson instruction suffixes corresponding to the conversions
;; specified by V_half_width.
(define_mode_attr V_squash_double_suffix [(V2SI "wh") (V4HI "hb")])

;; Move patterns.

;; Expander to legitimize moves involving values of vector modes.
(define_expand "mov<mode>"
  [(set (match_operand:VWHB 0)
	(match_operand:VWHB 1))]
  "TARGET_HARD_FLOAT && TARGET_LOONGSON_VECTORS"
{
  if (mips_legitimize_move (<MODE>mode, operands[0], operands[1]))
    DONE;
})

;; Handle legitimized moves between values of vector modes.
(define_insn "mov<mode>_internal"
  [(set (match_operand:VWHB 0 "nonimmediate_operand" "=m,f,d,f,  d,  m,  d")
	(match_operand:VWHB 1 "move_operand"          "f,m,f,dYG,dYG,dYG,m"))]
  "TARGET_HARD_FLOAT && TARGET_LOONGSON_VECTORS"
  { return mips_output_move (operands[0], operands[1]); }
  [(set_attr "move_type" "fpstore,fpload,mfc,mtc,move,store,load")
   (set_attr "mode" "DI")])

;; Initialization of a vector.

(define_expand "vec_init<mode>"
  [(set (match_operand:VWHB 0 "register_operand")
	(match_operand 1 ""))]
  "TARGET_HARD_FLOAT && TARGET_LOONGSON_VECTORS"
{
  mips_expand_vector_init (operands[0], operands[1]);
  DONE;
})

;; Instruction patterns for SIMD instructions.

;; Pack with signed saturation.
(define_insn "vec_pack_ssat_<mode>"
  [(set (match_operand:<V_squash_double> 0 "register_operand" "=f")
        (vec_concat:<V_squash_double>
	 (ss_truncate:<V_squash>
	  (match_operand:VWH 1 "register_operand" "f"))
	 (ss_truncate:<V_squash>
	  (match_operand:VWH 2 "register_operand" "f"))))]
  "TARGET_HARD_FLOAT && TARGET_LOONGSON_VECTORS"
  "packss<V_squash_double_suffix>\t%0,%1,%2"
  [(set_attr "type" "fmul")])

;; Pack with unsigned saturation.
(define_insn "vec_pack_usat_<mode>"
  [(set (match_operand:<V_squash_double> 0 "register_operand" "=f")
        (vec_concat:<V_squash_double>
	 (us_truncate:<V_squash>
	  (match_operand:VH 1 "register_operand" "f"))
	 (us_truncate:<V_squash>
	  (match_operand:VH 2 "register_operand" "f"))))]
  "TARGET_HARD_FLOAT && TARGET_LOONGSON_VECTORS"
  "packus<V_squash_double_suffix>\t%0,%1,%2"
  [(set_attr "type" "fmul")])

;; Addition, treating overflow by wraparound.
(define_insn "add<mode>3"
  [(set (match_operand:VWHB 0 "register_operand" "=f")
        (plus:VWHB (match_operand:VWHB 1 "register_operand" "f")
		   (match_operand:VWHB 2 "register_operand" "f")))]
  "TARGET_HARD_FLOAT && TARGET_LOONGSON_VECTORS"
  "padd<V_suffix>\t%0,%1,%2"
  [(set_attr "type" "fadd")])

;; Addition of doubleword integers stored in FP registers.
;; Overflow is treated by wraparound.
;; We use 'unspec' instead of 'plus' here to avoid clash with
;; mips.md::add<mode>3.  If 'plus' was used, then such instruction
;; would be recognized as adddi3 and reload would make it use
;; GPRs instead of FPRs.
(define_insn "loongson_paddd"
  [(set (match_operand:DI 0 "register_operand" "=f")
        (unspec:DI [(match_operand:DI 1 "register_operand" "f")
		    (match_operand:DI 2 "register_operand" "f")]
		   UNSPEC_LOONGSON_PADDD))]
  "TARGET_HARD_FLOAT && TARGET_LOONGSON_VECTORS"
  "paddd\t%0,%1,%2"
  [(set_attr "type" "fadd")])

;; Addition, treating overflow by signed saturation.
(define_insn "ssadd<mode>3"
  [(set (match_operand:VHB 0 "register_operand" "=f")
        (ss_plus:VHB (match_operand:VHB 1 "register_operand" "f")
		     (match_operand:VHB 2 "register_operand" "f")))]
  "TARGET_HARD_FLOAT && TARGET_LOONGSON_VECTORS"
  "padds<V_suffix>\t%0,%1,%2"
  [(set_attr "type" "fadd")])

;; Addition, treating overflow by unsigned saturation.
(define_insn "usadd<mode>3"
  [(set (match_operand:VHB 0 "register_operand" "=f")
        (us_plus:VHB (match_operand:VHB 1 "register_operand" "f")
		     (match_operand:VHB 2 "register_operand" "f")))]
  "TARGET_HARD_FLOAT && TARGET_LOONGSON_VECTORS"
  "paddus<V_suffix>\t%0,%1,%2"
  [(set_attr "type" "fadd")])

;; Logical AND NOT.
(define_insn "loongson_pandn_<V_suffix>"
  [(set (match_operand:VWHBDI 0 "register_operand" "=f")
        (and:VWHBDI
	 (not:VWHBDI (match_operand:VWHBDI 1 "register_operand" "f"))
	 (match_operand:VWHBDI 2 "register_operand" "f")))]
  "TARGET_HARD_FLOAT && TARGET_LOONGSON_VECTORS"
  "pandn\t%0,%1,%2"
  [(set_attr "type" "fmul")])

;; Average.
(define_insn "loongson_pavg<V_suffix>"
  [(set (match_operand:VHB 0 "register_operand" "=f")
        (unspec:VHB [(match_operand:VHB 1 "register_operand" "f")
		     (match_operand:VHB 2 "register_operand" "f")]
		    UNSPEC_LOONGSON_PAVG))]
  "TARGET_HARD_FLOAT && TARGET_LOONGSON_VECTORS"
  "pavg<V_suffix>\t%0,%1,%2"
  [(set_attr "type" "fadd")])

;; Equality test.
(define_insn "loongson_pcmpeq<V_suffix>"
  [(set (match_operand:VWHB 0 "register_operand" "=f")
        (unspec:VWHB [(match_operand:VWHB 1 "register_operand" "f")
		      (match_operand:VWHB 2 "register_operand" "f")]
		     UNSPEC_LOONGSON_PCMPEQ))]
  "TARGET_HARD_FLOAT && TARGET_LOONGSON_VECTORS"
  "pcmpeq<V_suffix>\t%0,%1,%2"
  [(set_attr "type" "fadd")])

;; Greater-than test.
(define_insn "loongson_pcmpgt<V_suffix>"
  [(set (match_operand:VWHB 0 "register_operand" "=f")
        (unspec:VWHB [(match_operand:VWHB 1 "register_operand" "f")
		      (match_operand:VWHB 2 "register_operand" "f")]
		     UNSPEC_LOONGSON_PCMPGT))]
  "TARGET_HARD_FLOAT && TARGET_LOONGSON_VECTORS"
  "pcmpgt<V_suffix>\t%0,%1,%2"
  [(set_attr "type" "fadd")])

;; Extract halfword.
(define_insn "loongson_pextr<V_suffix>"
  [(set (match_operand:VH 0 "register_operand" "=f")
        (unspec:VH [(match_operand:VH 1 "register_operand" "f")
 		    (match_operand:SI 2 "register_operand" "f")]
		   UNSPEC_LOONGSON_PEXTR))]
  "TARGET_HARD_FLOAT && TARGET_LOONGSON_VECTORS"
  "pextr<V_suffix>\t%0,%1,%2"
  [(set_attr "type" "fmul")])

;; Insert halfword.
(define_insn "loongson_pinsr<V_suffix>_0"
  [(set (match_operand:VH 0 "register_operand" "=f")
        (unspec:VH [(match_operand:VH 1 "register_operand" "f")
		    (match_operand:VH 2 "register_operand" "f")]
		   UNSPEC_LOONGSON_PINSR_0))]
  "TARGET_HARD_FLOAT && TARGET_LOONGSON_VECTORS"
  "pinsr<V_suffix>_0\t%0,%1,%2"
  [(set_attr "type" "fdiv")])

(define_insn "loongson_pinsr<V_suffix>_1"
  [(set (match_operand:VH 0 "register_operand" "=f")
        (unspec:VH [(match_operand:VH 1 "register_operand" "f")
		    (match_operand:VH 2 "register_operand" "f")]
		   UNSPEC_LOONGSON_PINSR_1))]
  "TARGET_HARD_FLOAT && TARGET_LOONGSON_VECTORS"
  "pinsr<V_suffix>_1\t%0,%1,%2"
  [(set_attr "type" "fdiv")])

(define_insn "loongson_pinsr<V_suffix>_2"
  [(set (match_operand:VH 0 "register_operand" "=f")
        (unspec:VH [(match_operand:VH 1 "register_operand" "f")
		    (match_operand:VH 2 "register_operand" "f")]
		   UNSPEC_LOONGSON_PINSR_2))]
  "TARGET_HARD_FLOAT && TARGET_LOONGSON_VECTORS"
  "pinsr<V_suffix>_2\t%0,%1,%2"
  [(set_attr "type" "fdiv")])

(define_insn "loongson_pinsr<V_suffix>_3"
  [(set (match_operand:VH 0 "register_operand" "=f")
        (unspec:VH [(match_operand:VH 1 "register_operand" "f")
		    (match_operand:VH 2 "register_operand" "f")]
		   UNSPEC_LOONGSON_PINSR_3))]
  "TARGET_HARD_FLOAT && TARGET_LOONGSON_VECTORS"
  "pinsr<V_suffix>_3\t%0,%1,%2"
  [(set_attr "type" "fdiv")])

;; Multiply and add packed integers.
(define_insn "loongson_pmadd<V_stretch_half_suffix>"
  [(set (match_operand:<V_stretch_half> 0 "register_operand" "=f")
        (unspec:<V_stretch_half> [(match_operand:VH 1 "register_operand" "f")
				  (match_operand:VH 2 "register_operand" "f")]
				 UNSPEC_LOONGSON_PMADD))]
  "TARGET_HARD_FLOAT && TARGET_LOONGSON_VECTORS"
  "pmadd<V_stretch_half_suffix>\t%0,%1,%2"
  [(set_attr "type" "fmul")])

;; Maximum of signed halfwords.
(define_insn "smax<mode>3"
  [(set (match_operand:VH 0 "register_operand" "=f")
        (smax:VH (match_operand:VH 1 "register_operand" "f")
		 (match_operand:VH 2 "register_operand" "f")))]
  "TARGET_HARD_FLOAT && TARGET_LOONGSON_VECTORS"
  "pmaxs<V_suffix>\t%0,%1,%2"
  [(set_attr "type" "fadd")])

;; Maximum of unsigned bytes.
(define_insn "umax<mode>3"
  [(set (match_operand:VB 0 "register_operand" "=f")
        (umax:VB (match_operand:VB 1 "register_operand" "f")
		 (match_operand:VB 2 "register_operand" "f")))]
  "TARGET_HARD_FLOAT && TARGET_LOONGSON_VECTORS"
  "pmaxu<V_suffix>\t%0,%1,%2"
  [(set_attr "type" "fadd")])

;; Minimum of signed halfwords.
(define_insn "smin<mode>3"
  [(set (match_operand:VH 0 "register_operand" "=f")
        (smin:VH (match_operand:VH 1 "register_operand" "f")
		 (match_operand:VH 2 "register_operand" "f")))]
  "TARGET_HARD_FLOAT && TARGET_LOONGSON_VECTORS"
  "pmins<V_suffix>\t%0,%1,%2"
  [(set_attr "type" "fadd")])

;; Minimum of unsigned bytes.
(define_insn "umin<mode>3"
  [(set (match_operand:VB 0 "register_operand" "=f")
        (umin:VB (match_operand:VB 1 "register_operand" "f")
		 (match_operand:VB 2 "register_operand" "f")))]
  "TARGET_HARD_FLOAT && TARGET_LOONGSON_VECTORS"
  "pminu<V_suffix>\t%0,%1,%2"
  [(set_attr "type" "fadd")])

;; Move byte mask.
(define_insn "loongson_pmovmsk<V_suffix>"
  [(set (match_operand:VB 0 "register_operand" "=f")
        (unspec:VB [(match_operand:VB 1 "register_operand" "f")]
		   UNSPEC_LOONGSON_PMOVMSK))]
  "TARGET_HARD_FLOAT && TARGET_LOONGSON_VECTORS"
  "pmovmsk<V_suffix>\t%0,%1"
  [(set_attr "type" "fabs")])

;; Multiply unsigned integers and store high result.
(define_insn "umul<mode>3_highpart"
  [(set (match_operand:VH 0 "register_operand" "=f")
        (unspec:VH [(match_operand:VH 1 "register_operand" "f")
		    (match_operand:VH 2 "register_operand" "f")]
		   UNSPEC_LOONGSON_PMULHU))]
  "TARGET_HARD_FLOAT && TARGET_LOONGSON_VECTORS"
  "pmulhu<V_suffix>\t%0,%1,%2"
  [(set_attr "type" "fmul")])

;; Multiply signed integers and store high result.
(define_insn "smul<mode>3_highpart"
  [(set (match_operand:VH 0 "register_operand" "=f")
        (unspec:VH [(match_operand:VH 1 "register_operand" "f")
		    (match_operand:VH 2 "register_operand" "f")]
		   UNSPEC_LOONGSON_PMULH))]
  "TARGET_HARD_FLOAT && TARGET_LOONGSON_VECTORS"
  "pmulh<V_suffix>\t%0,%1,%2"
  [(set_attr "type" "fmul")])

;; Multiply signed integers and store low result.
(define_insn "loongson_pmull<V_suffix>"
  [(set (match_operand:VH 0 "register_operand" "=f")
        (unspec:VH [(match_operand:VH 1 "register_operand" "f")
		    (match_operand:VH 2 "register_operand" "f")]
		   UNSPEC_LOONGSON_PMULL))]
  "TARGET_HARD_FLOAT && TARGET_LOONGSON_VECTORS"
  "pmull<V_suffix>\t%0,%1,%2"
  [(set_attr "type" "fmul")])

;; Multiply unsigned word integers.
(define_insn "loongson_pmulu<V_suffix>"
  [(set (match_operand:DI 0 "register_operand" "=f")
        (unspec:DI [(match_operand:VW 1 "register_operand" "f")
		    (match_operand:VW 2 "register_operand" "f")]
		   UNSPEC_LOONGSON_PMULU))]
  "TARGET_HARD_FLOAT && TARGET_LOONGSON_VECTORS"
  "pmulu<V_suffix>\t%0,%1,%2"
  [(set_attr "type" "fmul")])

;; Absolute difference.
(define_insn "loongson_pasubub"
  [(set (match_operand:VB 0 "register_operand" "=f")
        (unspec:VB [(match_operand:VB 1 "register_operand" "f")
		    (match_operand:VB 2 "register_operand" "f")]
		   UNSPEC_LOONGSON_PASUBUB))]
  "TARGET_HARD_FLOAT && TARGET_LOONGSON_VECTORS"
  "pasubub\t%0,%1,%2"
  [(set_attr "type" "fadd")])

;; Sum of unsigned byte integers.
(define_insn "loongson_biadd"
  [(set (match_operand:<V_stretch_half> 0 "register_operand" "=f")
        (unspec:<V_stretch_half> [(match_operand:VB 1 "register_operand" "f")]
				 UNSPEC_LOONGSON_BIADD))]
  "TARGET_HARD_FLOAT && TARGET_LOONGSON_VECTORS"
  "biadd\t%0,%1"
  [(set_attr "type" "fabs")])

;; Sum of absolute differences.
(define_insn "loongson_psadbh"
  [(set (match_operand:<V_stretch_half> 0 "register_operand" "=f")
        (unspec:<V_stretch_half> [(match_operand:VB 1 "register_operand" "f")
				  (match_operand:VB 2 "register_operand" "f")]
				 UNSPEC_LOONGSON_PSADBH))]
  "TARGET_HARD_FLOAT && TARGET_LOONGSON_VECTORS"
  "pasubub\t%0,%1,%2;biadd\t%0,%0"
  [(set_attr "type" "fadd")])

;; Shuffle halfwords.
(define_insn "loongson_pshufh"
  [(set (match_operand:VH 0 "register_operand" "=f")
        (unspec:VH [(match_operand:VH 1 "register_operand" "0")
		    (match_operand:VH 2 "register_operand" "f")
		    (match_operand:SI 3 "register_operand" "f")]
		   UNSPEC_LOONGSON_PSHUFH))]
  "TARGET_HARD_FLOAT && TARGET_LOONGSON_VECTORS"
  "pshufh\t%0,%2,%3"
  [(set_attr "type" "fmul")])

;; Shift left logical.
(define_insn "loongson_psll<V_suffix>"
  [(set (match_operand:VWH 0 "register_operand" "=f")
        (ashift:VWH (match_operand:VWH 1 "register_operand" "f")
		    (match_operand:SI 2 "register_operand" "f")))]
  "TARGET_HARD_FLOAT && TARGET_LOONGSON_VECTORS"
  "psll<V_suffix>\t%0,%1,%2"
  [(set_attr "type" "fmul")])

;; Shift right arithmetic.
(define_insn "loongson_psra<V_suffix>"
  [(set (match_operand:VWH 0 "register_operand" "=f")
        (ashiftrt:VWH (match_operand:VWH 1 "register_operand" "f")
		      (match_operand:SI 2 "register_operand" "f")))]
  "TARGET_HARD_FLOAT && TARGET_LOONGSON_VECTORS"
  "psra<V_suffix>\t%0,%1,%2"
  [(set_attr "type" "fdiv")])

;; Shift right logical.
(define_insn "loongson_psrl<V_suffix>"
  [(set (match_operand:VWH 0 "register_operand" "=f")
        (lshiftrt:VWH (match_operand:VWH 1 "register_operand" "f")
		      (match_operand:SI 2 "register_operand" "f")))]
  "TARGET_HARD_FLOAT && TARGET_LOONGSON_VECTORS"
  "psrl<V_suffix>\t%0,%1,%2"
  [(set_attr "type" "fdiv")])

;; Subtraction, treating overflow by wraparound.
(define_insn "sub<mode>3"
  [(set (match_operand:VWHB 0 "register_operand" "=f")
        (minus:VWHB (match_operand:VWHB 1 "register_operand" "f")
		    (match_operand:VWHB 2 "register_operand" "f")))]
  "TARGET_HARD_FLOAT && TARGET_LOONGSON_VECTORS"
  "psub<V_suffix>\t%0,%1,%2"
  [(set_attr "type" "fadd")])

;; Subtraction of doubleword integers stored in FP registers.
;; Overflow is treated by wraparound.
;; See loongson_paddd for the reason we use 'unspec' rather than
;; 'minus' here.
(define_insn "loongson_psubd"
  [(set (match_operand:DI 0 "register_operand" "=f")
        (unspec:DI [(match_operand:DI 1 "register_operand" "f")
		    (match_operand:DI 2 "register_operand" "f")]
		   UNSPEC_LOONGSON_PSUBD))]
  "TARGET_HARD_FLOAT && TARGET_LOONGSON_VECTORS"
  "psubd\t%0,%1,%2"
  [(set_attr "type" "fadd")])

;; Subtraction, treating overflow by signed saturation.
(define_insn "sssub<mode>3"
  [(set (match_operand:VHB 0 "register_operand" "=f")
        (ss_minus:VHB (match_operand:VHB 1 "register_operand" "f")
		      (match_operand:VHB 2 "register_operand" "f")))]
  "TARGET_HARD_FLOAT && TARGET_LOONGSON_VECTORS"
  "psubs<V_suffix>\t%0,%1,%2"
  [(set_attr "type" "fadd")])

;; Subtraction, treating overflow by unsigned saturation.
(define_insn "ussub<mode>3"
  [(set (match_operand:VHB 0 "register_operand" "=f")
        (us_minus:VHB (match_operand:VHB 1 "register_operand" "f")
		      (match_operand:VHB 2 "register_operand" "f")))]
  "TARGET_HARD_FLOAT && TARGET_LOONGSON_VECTORS"
  "psubus<V_suffix>\t%0,%1,%2"
  [(set_attr "type" "fadd")])

;; Unpack high data.
(define_insn "vec_interleave_high<mode>"
  [(set (match_operand:VWHB 0 "register_operand" "=f")
        (unspec:VWHB [(match_operand:VWHB 1 "register_operand" "f")
		      (match_operand:VWHB 2 "register_operand" "f")]
		     UNSPEC_LOONGSON_PUNPCKH))]
  "TARGET_HARD_FLOAT && TARGET_LOONGSON_VECTORS"
  "punpckh<V_stretch_half_suffix>\t%0,%1,%2"
  [(set_attr "type" "fdiv")])

;; Unpack low data.
(define_insn "vec_interleave_low<mode>"
  [(set (match_operand:VWHB 0 "register_operand" "=f")
        (unspec:VWHB [(match_operand:VWHB 1 "register_operand" "f")
		      (match_operand:VWHB 2 "register_operand" "f")]
		     UNSPEC_LOONGSON_PUNPCKL))]
  "TARGET_HARD_FLOAT && TARGET_LOONGSON_VECTORS"
  "punpckl<V_stretch_half_suffix>\t%0,%1,%2"
  [(set_attr "type" "fdiv")])

;; Integer division and modulus.

(define_insn "<u>div<mode>3"
  [(set (match_operand:GPR 0 "register_operand" "=&d")
	(any_div:GPR (match_operand:GPR 1 "register_operand" "d")
		     (match_operand:GPR 2 "register_operand" "d")))]
  "TARGET_LOONGSON_2EF"
  { return mips_output_division ("<d>div<u>.g\t%0,%1,%2", operands); }
  [(set_attr "type" "idiv3")
   (set_attr "mode" "<MODE>")])

(define_insn "<u>mod<mode>3"
  [(set (match_operand:GPR 0 "register_operand" "=&d")
	(any_mod:GPR (match_operand:GPR 1 "register_operand" "d")
		     (match_operand:GPR 2 "register_operand" "d")))]
  "TARGET_LOONGSON_2EF"
  { return mips_output_division ("<d>mod<u>.g\t%0,%1,%2", operands); }
  [(set_attr "type" "idiv3")
   (set_attr "mode" "<MODE>")])
