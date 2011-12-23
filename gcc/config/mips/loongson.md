;; Machine description for Loongson-specific patterns, such as
;; ST Microelectronics Loongson-2E/2F etc.
;; Copyright (C) 2008, 2009, 2010 Free Software Foundation, Inc.
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

(define_c_enum "unspec" [
  UNSPEC_LOONGSON_PAVG
  UNSPEC_LOONGSON_PCMPEQ
  UNSPEC_LOONGSON_PCMPGT
  UNSPEC_LOONGSON_PEXTR
  UNSPEC_LOONGSON_PINSRH
  UNSPEC_LOONGSON_VINIT
  UNSPEC_LOONGSON_PMADD
  UNSPEC_LOONGSON_PMOVMSK
  UNSPEC_LOONGSON_PMULHU
  UNSPEC_LOONGSON_PMULH
  UNSPEC_LOONGSON_PMULU
  UNSPEC_LOONGSON_PASUBUB
  UNSPEC_LOONGSON_BIADD
  UNSPEC_LOONGSON_PSADBH
  UNSPEC_LOONGSON_PSHUFH
  UNSPEC_LOONGSON_PUNPCKH
  UNSPEC_LOONGSON_PUNPCKL
  UNSPEC_LOONGSON_PADDD
  UNSPEC_LOONGSON_PSUBD
  UNSPEC_LOONGSON_DSLL
  UNSPEC_LOONGSON_DSRL
])

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

;; 64-bit vectors of words and bytes
(define_mode_iterator VWB [V2SI V8QI])

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

;; Given a vector type T, the inner mode.
(define_mode_attr V_inner [(V8QI "QI") (V4HI "HI") (V2SI "SI")])

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

;; Helper for vec_init.  Initialize element 0 of the output from the input.
;; All other elements are undefined.
(define_insn "loongson_vec_init1_<mode>"
  [(set (match_operand:VHB 0 "register_operand" "=f")
	(unspec:VHB [(truncate:<V_inner>
		       (match_operand:DI 1 "reg_or_0_operand" "Jd"))]
		    UNSPEC_LOONGSON_VINIT))]
  "TARGET_HARD_FLOAT && TARGET_LOONGSON_VECTORS"
  "dmtc1\t%z1,%0"
  [(set_attr "move_type" "mtc")
   (set_attr "mode" "DI")])

;; Helper for vec_initv2si.
(define_insn "*vec_concatv2si"
  [(set (match_operand:V2SI 0 "register_operand" "=f")
	(vec_concat:V2SI
	  (match_operand:SI 1 "register_operand" "f")
	  (match_operand:SI 2 "register_operand" "f")))]
  "TARGET_HARD_FLOAT && TARGET_LOONGSON_VECTORS"
  "punpcklwd\t%0,%1,%2"
  [(set_attr "type" "fcvt")])

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

;; Logical AND.
(define_insn "and<mode>3"
  [(set (match_operand:VWHB 0 "register_operand" "=f")
	(and:VWHB (match_operand:VWHB 1 "register_operand" "f")
		  (match_operand:VWHB 2 "register_operand" "f")))]
  "TARGET_HARD_FLOAT && TARGET_LOONGSON_VECTORS"
  "and\t%0,%1,%2"
  [(set_attr "type" "fmul")])

;; Logical OR.
(define_insn "ior<mode>3"
  [(set (match_operand:VWHB 0 "register_operand" "=f")
	(ior:VWHB (match_operand:VWHB 1 "register_operand" "f")
		  (match_operand:VWHB 2 "register_operand" "f")))]
  "TARGET_HARD_FLOAT && TARGET_LOONGSON_VECTORS"
  "or\t%0,%1,%2"
  [(set_attr "type" "fcvt")])

;; Logical XOR.
(define_insn "xor<mode>3"
  [(set (match_operand:VWHB 0 "register_operand" "=f")
	(xor:VWHB (match_operand:VWHB 1 "register_operand" "f")
		  (match_operand:VWHB 2 "register_operand" "f")))]
  "TARGET_HARD_FLOAT && TARGET_LOONGSON_VECTORS"
  "xor\t%0,%1,%2"
  [(set_attr "type" "fmul")])

;; Logical NOR.
(define_insn "*loongson_nor"
  [(set (match_operand:VWHB 0 "register_operand" "=f")
	(and:VWHB
	  (not:VWHB (match_operand:VWHB 1 "register_operand" "f"))
	  (not:VWHB (match_operand:VWHB 2 "register_operand" "f"))))]
  "TARGET_HARD_FLOAT && TARGET_LOONGSON_VECTORS"
  "nor\t%0,%1,%2"
  [(set_attr "type" "fmul")])

;; Logical NOT.
(define_insn "one_cmpl<mode>2"
  [(set (match_operand:VWHB 0 "register_operand" "=f")
	(not:VWHB (match_operand:VWHB 1 "register_operand" "f")))]
  "TARGET_HARD_FLOAT && TARGET_LOONGSON_VECTORS"
  "nor\t%0,%1,%1"
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
(define_insn "loongson_pextrh"
  [(set (match_operand:V4HI 0 "register_operand" "=f")
        (unspec:V4HI [(match_operand:V4HI 1 "register_operand" "f")
		      (match_operand:SI 2 "register_operand" "f")]
		   UNSPEC_LOONGSON_PEXTR))]
  "TARGET_HARD_FLOAT && TARGET_LOONGSON_VECTORS"
  "pextrh\t%0,%1,%2"
  [(set_attr "type" "fcvt")])

;; Insert halfword.
(define_insn "loongson_pinsrh_0"
  [(set (match_operand:V4HI 0 "register_operand" "=f")
	(vec_select:V4HI
	  (vec_concat:V8HI
	    (match_operand:V4HI 1 "register_operand" "f")
	    (match_operand:V4HI 2 "register_operand" "f"))
	  (parallel [(const_int 4) (const_int 1)
		     (const_int 2) (const_int 3)])))]
  "TARGET_HARD_FLOAT && TARGET_LOONGSON_VECTORS"
  "pinsrh_0\t%0,%1,%2"
  [(set_attr "type" "fdiv")])

(define_insn "loongson_pinsrh_1"
  [(set (match_operand:V4HI 0 "register_operand" "=f")
	(vec_select:V4HI
	  (vec_concat:V8HI
	    (match_operand:V4HI 1 "register_operand" "f")
	    (match_operand:V4HI 2 "register_operand" "f"))
	  (parallel [(const_int 0) (const_int 4)
		     (const_int 2) (const_int 3)])))]
  "TARGET_HARD_FLOAT && TARGET_LOONGSON_VECTORS"
  "pinsrh_1\t%0,%1,%2"
  [(set_attr "type" "fdiv")])

(define_insn "loongson_pinsrh_2"
  [(set (match_operand:V4HI 0 "register_operand" "=f")
	(vec_select:V4HI
	  (vec_concat:V8HI
	    (match_operand:V4HI 1 "register_operand" "f")
	    (match_operand:V4HI 2 "register_operand" "f"))
	  (parallel [(const_int 0) (const_int 1)
		     (const_int 4) (const_int 3)])))]
  "TARGET_HARD_FLOAT && TARGET_LOONGSON_VECTORS"
  "pinsrh_2\t%0,%1,%2"
  [(set_attr "type" "fdiv")])

(define_insn "loongson_pinsrh_3"
  [(set (match_operand:V4HI 0 "register_operand" "=f")
	(vec_select:V4HI
	  (vec_concat:V8HI
	    (match_operand:V4HI 1 "register_operand" "f")
	    (match_operand:V4HI 2 "register_operand" "f"))
	  (parallel [(const_int 0) (const_int 1)
		     (const_int 2) (const_int 4)])))]
  "TARGET_HARD_FLOAT && TARGET_LOONGSON_VECTORS"
  "pinsrh_3\t%0,%1,%2"
  [(set_attr "type" "fdiv")])

(define_insn "*vec_setv4hi"
  [(set (match_operand:V4HI 0 "register_operand" "=f")
	(unspec:V4HI [(match_operand:V4HI 1 "register_operand" "f")
		      (match_operand:SI 2 "register_operand" "f")
		      (match_operand:SI 3 "const_0_to_3_operand" "")]
		     UNSPEC_LOONGSON_PINSRH))]
  "TARGET_HARD_FLOAT && TARGET_LOONGSON_VECTORS"
  "pinsrh_%3\t%0,%1,%2"
  [(set_attr "type" "fdiv")])

(define_expand "vec_setv4hi"
  [(set (match_operand:V4HI 0 "register_operand" "=f")
	(unspec:V4HI [(match_operand:V4HI 1 "register_operand" "f")
		      (match_operand:HI 2 "register_operand" "f")
		      (match_operand:SI 3 "const_0_to_3_operand" "")]
		     UNSPEC_LOONGSON_PINSRH))]
  "TARGET_HARD_FLOAT && TARGET_LOONGSON_VECTORS"
{
  rtx ext = gen_reg_rtx (SImode);
  emit_move_insn (ext, gen_lowpart (SImode, operands[1]));
  operands[1] = ext;
})

;; Multiply and add packed integers.
(define_insn "loongson_pmaddhw"
  [(set (match_operand:V2SI 0 "register_operand" "=f")
        (unspec:V2SI [(match_operand:V4HI 1 "register_operand" "f")
		      (match_operand:V4HI 2 "register_operand" "f")]
		     UNSPEC_LOONGSON_PMADD))]
  "TARGET_HARD_FLOAT && TARGET_LOONGSON_VECTORS"
  "pmaddhw\t%0,%1,%2"
  [(set_attr "type" "fmul")])

(define_expand "sdot_prodv4hi"
  [(match_operand:V2SI 0 "register_operand" "")
   (match_operand:V4HI 1 "register_operand" "")
   (match_operand:V4HI 2 "register_operand" "")
   (match_operand:V2SI 3 "register_operand" "")]
  "TARGET_HARD_FLOAT && TARGET_LOONGSON_VECTORS"
{
  rtx t = gen_reg_rtx (V2SImode);
  emit_insn (gen_loongson_pmaddhw (t, operands[1], operands[2]));
  emit_insn (gen_addv2si3 (operands[0], t, operands[3]));
  DONE;
})

;; Maximum of signed halfwords.
(define_insn "smaxv4hi3"
  [(set (match_operand:V4HI 0 "register_operand" "=f")
        (smax:V4HI (match_operand:V4HI 1 "register_operand" "f")
		   (match_operand:V4HI 2 "register_operand" "f")))]
  "TARGET_HARD_FLOAT && TARGET_LOONGSON_VECTORS"
  "pmaxsh\t%0,%1,%2"
  [(set_attr "type" "fadd")])

(define_expand "smax<mode>3"
  [(match_operand:VWB 0 "register_operand" "")
   (match_operand:VWB 1 "register_operand" "")
   (match_operand:VWB 2 "register_operand" "")]
  "TARGET_HARD_FLOAT && TARGET_LOONGSON_VECTORS"
{
  mips_expand_vec_minmax (operands[0], operands[1], operands[2],
			  gen_loongson_pcmpgt<V_suffix>, false);
  DONE;
})

;; Maximum of unsigned bytes.
(define_insn "umaxv8qi3"
  [(set (match_operand:V8QI 0 "register_operand" "=f")
        (umax:V8QI (match_operand:V8QI 1 "register_operand" "f")
		   (match_operand:V8QI 2 "register_operand" "f")))]
  "TARGET_HARD_FLOAT && TARGET_LOONGSON_VECTORS"
  "pmaxub\t%0,%1,%2"
  [(set_attr "type" "fadd")])

;; Minimum of signed halfwords.
(define_insn "sminv4hi3"
  [(set (match_operand:V4HI 0 "register_operand" "=f")
        (smin:V4HI (match_operand:V4HI 1 "register_operand" "f")
		   (match_operand:V4HI 2 "register_operand" "f")))]
  "TARGET_HARD_FLOAT && TARGET_LOONGSON_VECTORS"
  "pminsh\t%0,%1,%2"
  [(set_attr "type" "fadd")])

(define_expand "smin<mode>3"
  [(match_operand:VWB 0 "register_operand" "")
   (match_operand:VWB 1 "register_operand" "")
   (match_operand:VWB 2 "register_operand" "")]
  "TARGET_HARD_FLOAT && TARGET_LOONGSON_VECTORS"
{
  mips_expand_vec_minmax (operands[0], operands[1], operands[2],
			  gen_loongson_pcmpgt<V_suffix>, true);
  DONE;
})

;; Minimum of unsigned bytes.
(define_insn "uminv8qi3"
  [(set (match_operand:V8QI 0 "register_operand" "=f")
        (umin:V8QI (match_operand:V8QI 1 "register_operand" "f")
		   (match_operand:V8QI 2 "register_operand" "f")))]
  "TARGET_HARD_FLOAT && TARGET_LOONGSON_VECTORS"
  "pminub\t%0,%1,%2"
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
(define_insn "mul<mode>3"
  [(set (match_operand:VH 0 "register_operand" "=f")
        (mult:VH (match_operand:VH 1 "register_operand" "f")
                 (match_operand:VH 2 "register_operand" "f")))]
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

(define_insn "reduc_uplus_v8qi"
  [(set (match_operand:V8QI 0 "register_operand" "=f")
	(unspec:V8QI [(match_operand:V8QI 1 "register_operand" "f")]
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
        (unspec:VH [(match_operand:VH 1 "register_operand" "f")
		    (match_operand:SI 2 "register_operand" "f")]
		   UNSPEC_LOONGSON_PSHUFH))]
  "TARGET_HARD_FLOAT && TARGET_LOONGSON_VECTORS"
  "pshufh\t%0,%1,%2"
  [(set_attr "type" "fmul")])

;; Shift left logical.
(define_insn "ashl<mode>3"
  [(set (match_operand:VWH 0 "register_operand" "=f")
        (ashift:VWH (match_operand:VWH 1 "register_operand" "f")
		    (match_operand:SI 2 "register_operand" "f")))]
  "TARGET_HARD_FLOAT && TARGET_LOONGSON_VECTORS"
  "psll<V_suffix>\t%0,%1,%2"
  [(set_attr "type" "fcvt")])

;; Shift right arithmetic.
(define_insn "ashr<mode>3"
  [(set (match_operand:VWH 0 "register_operand" "=f")
        (ashiftrt:VWH (match_operand:VWH 1 "register_operand" "f")
		      (match_operand:SI 2 "register_operand" "f")))]
  "TARGET_HARD_FLOAT && TARGET_LOONGSON_VECTORS"
  "psra<V_suffix>\t%0,%1,%2"
  [(set_attr "type" "fcvt")])

;; Shift right logical.
(define_insn "lshr<mode>3"
  [(set (match_operand:VWH 0 "register_operand" "=f")
        (lshiftrt:VWH (match_operand:VWH 1 "register_operand" "f")
		      (match_operand:SI 2 "register_operand" "f")))]
  "TARGET_HARD_FLOAT && TARGET_LOONGSON_VECTORS"
  "psrl<V_suffix>\t%0,%1,%2"
  [(set_attr "type" "fcvt")])

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

;; Unpack high data.  Recall that Loongson only runs in little-endian.
(define_insn "loongson_punpckhbh"
  [(set (match_operand:V8QI 0 "register_operand" "=f")
	(vec_select:V8QI
	  (vec_concat:V16QI
	    (match_operand:V8QI 1 "register_operand" "f")
	    (match_operand:V8QI 2 "register_operand" "f"))
	  (parallel [(const_int 4) (const_int 12)
		     (const_int 5) (const_int 13)
		     (const_int 6) (const_int 14)
		     (const_int 7) (const_int 15)])))]
  "TARGET_HARD_FLOAT && TARGET_LOONGSON_VECTORS"
  "punpckhbh\t%0,%1,%2"
  [(set_attr "type" "fdiv")])

(define_insn "loongson_punpckhhw"
  [(set (match_operand:V4HI 0 "register_operand" "=f")
	(vec_select:V4HI
	  (vec_concat:V8HI
	    (match_operand:V4HI 1 "register_operand" "f")
	    (match_operand:V4HI 2 "register_operand" "f"))
	  (parallel [(const_int 2) (const_int 6)
		     (const_int 3) (const_int 7)])))]
  "TARGET_HARD_FLOAT && TARGET_LOONGSON_VECTORS"
  "punpckhhw\t%0,%1,%2"
  [(set_attr "type" "fdiv")])

(define_insn "loongson_punpckhhw_qi"
  [(set (match_operand:V8QI 0 "register_operand" "=f")
	(vec_select:V8QI
	  (vec_concat:V16QI
	    (match_operand:V8QI 1 "register_operand" "f")
	    (match_operand:V8QI 2 "register_operand" "f"))
	  (parallel [(const_int 4)  (const_int 5)
		     (const_int 12) (const_int 13)
		     (const_int 6)  (const_int 7)
		     (const_int 14) (const_int 15)])))]
  "TARGET_HARD_FLOAT && TARGET_LOONGSON_VECTORS"
  "punpckhhw\t%0,%1,%2"
  [(set_attr "type" "fdiv")])

(define_insn "loongson_punpckhwd"
  [(set (match_operand:V2SI 0 "register_operand" "=f")
	(vec_select:V2SI
	  (vec_concat:V4SI
	    (match_operand:V2SI 1 "register_operand" "f")
	    (match_operand:V2SI 2 "register_operand" "f"))
	  (parallel [(const_int 1) (const_int 3)])))]
  "TARGET_HARD_FLOAT && TARGET_LOONGSON_VECTORS"
  "punpckhwd\t%0,%1,%2"
  [(set_attr "type" "fcvt")])

(define_insn "loongson_punpckhwd_qi"
  [(set (match_operand:V8QI 0 "register_operand" "=f")
	(vec_select:V8QI
	  (vec_concat:V16QI
	    (match_operand:V8QI 1 "register_operand" "f")
	    (match_operand:V8QI 2 "register_operand" "f"))
	  (parallel [(const_int 4) (const_int 5)
		     (const_int 6) (const_int 7)
		     (const_int 12) (const_int 13)
		     (const_int 14) (const_int 15)])))]
  "TARGET_HARD_FLOAT && TARGET_LOONGSON_VECTORS"
  "punpckhwd\t%0,%1,%2"
  [(set_attr "type" "fcvt")])

(define_insn "loongson_punpckhwd_hi"
  [(set (match_operand:V4HI 0 "register_operand" "=f")
	(vec_select:V4HI
	  (vec_concat:V8HI
	    (match_operand:V4HI 1 "register_operand" "f")
	    (match_operand:V4HI 2 "register_operand" "f"))
	  (parallel [(const_int 2) (const_int 3)
		     (const_int 6) (const_int 7)])))]
  "TARGET_HARD_FLOAT && TARGET_LOONGSON_VECTORS"
  "punpckhwd\t%0,%1,%2"
  [(set_attr "type" "fcvt")])

;; Unpack low data.
(define_insn "loongson_punpcklbh"
  [(set (match_operand:V8QI 0 "register_operand" "=f")
	(vec_select:V8QI
	  (vec_concat:V16QI
	    (match_operand:V8QI 1 "register_operand" "f")
	    (match_operand:V8QI 2 "register_operand" "f"))
	  (parallel [(const_int 0) (const_int 8)
		     (const_int 1) (const_int 9)
		     (const_int 2) (const_int 10)
		     (const_int 3) (const_int 11)])))]
  "TARGET_HARD_FLOAT && TARGET_LOONGSON_VECTORS"
  "punpcklbh\t%0,%1,%2"
  [(set_attr "type" "fdiv")])

(define_insn "loongson_punpcklhw"
  [(set (match_operand:V4HI 0 "register_operand" "=f")
	(vec_select:V4HI
	  (vec_concat:V8HI
	    (match_operand:V4HI 1 "register_operand" "f")
	    (match_operand:V4HI 2 "register_operand" "f"))
	  (parallel [(const_int 0) (const_int 4)
		     (const_int 1) (const_int 5)])))]
  "TARGET_HARD_FLOAT && TARGET_LOONGSON_VECTORS"
  "punpcklhw\t%0,%1,%2"
  [(set_attr "type" "fdiv")])

(define_insn "*loongson_punpcklhw_qi"
  [(set (match_operand:V8QI 0 "register_operand" "=f")
	(vec_select:V8QI
	  (vec_concat:V16QI
	    (match_operand:V8QI 1 "register_operand" "f")
	    (match_operand:V8QI 2 "register_operand" "f"))
	  (parallel [(const_int 0)  (const_int 1)
		     (const_int 8)  (const_int 9)
		     (const_int 2)  (const_int 3)
		     (const_int 10) (const_int 11)])))]
  "TARGET_HARD_FLOAT && TARGET_LOONGSON_VECTORS"
  "punpcklhw\t%0,%1,%2"
  [(set_attr "type" "fdiv")])

(define_insn "loongson_punpcklwd"
  [(set (match_operand:V2SI 0 "register_operand" "=f")
	(vec_select:V2SI
	  (vec_concat:V4SI
	    (match_operand:V2SI 1 "register_operand" "f")
	    (match_operand:V2SI 2 "register_operand" "f"))
	  (parallel [(const_int 0) (const_int 2)])))]
  "TARGET_HARD_FLOAT && TARGET_LOONGSON_VECTORS"
  "punpcklwd\t%0,%1,%2"
  [(set_attr "type" "fcvt")])

(define_insn "*loongson_punpcklwd_qi"
  [(set (match_operand:V8QI 0 "register_operand" "=f")
	(vec_select:V8QI
	  (vec_concat:V16QI
	    (match_operand:V8QI 1 "register_operand" "f")
	    (match_operand:V8QI 2 "register_operand" "f"))
	  (parallel [(const_int 0) (const_int 1)
		     (const_int 2) (const_int 3)
		     (const_int 8) (const_int 9)
		     (const_int 10) (const_int 11)])))]
  "TARGET_HARD_FLOAT && TARGET_LOONGSON_VECTORS"
  "punpcklwd\t%0,%1,%2"
  [(set_attr "type" "fcvt")])

(define_insn "*loongson_punpcklwd_hi"
  [(set (match_operand:V4HI 0 "register_operand" "=f")
	(vec_select:V4HI
	  (vec_concat:V8HI
	    (match_operand:V4HI 1 "register_operand" "f")
	    (match_operand:V4HI 2 "register_operand" "f"))
	  (parallel [(const_int 0) (const_int 1)
		     (const_int 4) (const_int 5)])))]
  "TARGET_HARD_FLOAT && TARGET_LOONGSON_VECTORS"
  "punpcklwd\t%0,%1,%2"
  [(set_attr "type" "fcvt")])

(define_expand "vec_perm_const<mode>"
  [(match_operand:VWHB 0 "register_operand" "")
   (match_operand:VWHB 1 "register_operand" "")
   (match_operand:VWHB 2 "register_operand" "")
   (match_operand:VWHB 3 "" "")]
  "TARGET_HARD_FLOAT && TARGET_LOONGSON_VECTORS"
{
  if (mips_expand_vec_perm_const (operands))
    DONE;
  else
    FAIL;
})

(define_expand "vec_unpacks_lo_<mode>"
  [(match_operand:<V_stretch_half> 0 "register_operand" "")
   (match_operand:VHB 1 "register_operand" "")]
  "TARGET_HARD_FLOAT && TARGET_LOONGSON_VECTORS"
{
  mips_expand_vec_unpack (operands, false, false);
  DONE;
})

(define_expand "vec_unpacks_hi_<mode>"
  [(match_operand:<V_stretch_half> 0 "register_operand" "")
   (match_operand:VHB 1 "register_operand" "")]
  "TARGET_HARD_FLOAT && TARGET_LOONGSON_VECTORS"
{
  mips_expand_vec_unpack (operands, false, true);
  DONE;
})

(define_expand "vec_unpacku_lo_<mode>"
  [(match_operand:<V_stretch_half> 0 "register_operand" "")
   (match_operand:VHB 1 "register_operand" "")]
  "TARGET_HARD_FLOAT && TARGET_LOONGSON_VECTORS"
{
  mips_expand_vec_unpack (operands, true, false);
  DONE;
})

(define_expand "vec_unpacku_hi_<mode>"
  [(match_operand:<V_stretch_half> 0 "register_operand" "")
   (match_operand:VHB 1 "register_operand" "")]
  "TARGET_HARD_FLOAT && TARGET_LOONGSON_VECTORS"
{
  mips_expand_vec_unpack (operands, true, true);
  DONE;
})

;; Whole vector shifts, used for reduction epilogues.
(define_insn "vec_shl_<mode>"
  [(set (match_operand:VWHBDI 0 "register_operand" "=f")
        (unspec:VWHBDI [(match_operand:VWHBDI 1 "register_operand" "f")
                        (match_operand:SI 2 "register_operand" "f")]
                       UNSPEC_LOONGSON_DSLL))]
  "TARGET_HARD_FLOAT && TARGET_LOONGSON_VECTORS"
  "dsll\t%0,%1,%2"
  [(set_attr "type" "fcvt")])

(define_insn "vec_shr_<mode>"
  [(set (match_operand:VWHBDI 0 "register_operand" "=f")
        (unspec:VWHBDI [(match_operand:VWHBDI 1 "register_operand" "f")
                        (match_operand:SI 2 "register_operand" "f")]
                       UNSPEC_LOONGSON_DSRL))]
  "TARGET_HARD_FLOAT && TARGET_LOONGSON_VECTORS"
  "dsrl\t%0,%1,%2"
  [(set_attr "type" "fcvt")])

(define_expand "reduc_uplus_<mode>"
  [(match_operand:VWH 0 "register_operand" "")
   (match_operand:VWH 1 "register_operand" "")]
  "TARGET_HARD_FLOAT && TARGET_LOONGSON_VECTORS"
{
  mips_expand_vec_reduc (operands[0], operands[1], gen_add<mode>3);
  DONE;
})

; ??? Given that we're not describing a widening reduction, we should
; not have separate optabs for signed and unsigned.
(define_expand "reduc_splus_<mode>"
  [(match_operand:VWHB 0 "register_operand" "")
   (match_operand:VWHB 1 "register_operand" "")]
  "TARGET_HARD_FLOAT && TARGET_LOONGSON_VECTORS"
{
  emit_insn (gen_reduc_uplus_<mode>(operands[0], operands[1]));
  DONE;
})

(define_expand "reduc_smax_<mode>"
  [(match_operand:VWHB 0 "register_operand" "")
   (match_operand:VWHB 1 "register_operand" "")]
  "TARGET_HARD_FLOAT && TARGET_LOONGSON_VECTORS"
{
  mips_expand_vec_reduc (operands[0], operands[1], gen_smax<mode>3);
  DONE;
})

(define_expand "reduc_smin_<mode>"
  [(match_operand:VWHB 0 "register_operand" "")
   (match_operand:VWHB 1 "register_operand" "")]
  "TARGET_HARD_FLOAT && TARGET_LOONGSON_VECTORS"
{
  mips_expand_vec_reduc (operands[0], operands[1], gen_smin<mode>3);
  DONE;
})

(define_expand "reduc_umax_<mode>"
  [(match_operand:VB 0 "register_operand" "")
   (match_operand:VB 1 "register_operand" "")]
  "TARGET_HARD_FLOAT && TARGET_LOONGSON_VECTORS"
{
  mips_expand_vec_reduc (operands[0], operands[1], gen_umax<mode>3);
  DONE;
})

(define_expand "reduc_umin_<mode>"
  [(match_operand:VB 0 "register_operand" "")
   (match_operand:VB 1 "register_operand" "")]
  "TARGET_HARD_FLOAT && TARGET_LOONGSON_VECTORS"
{
  mips_expand_vec_reduc (operands[0], operands[1], gen_umin<mode>3);
  DONE;
})

;; Integer division and modulus.  For integer multiplication, see mips.md.

(define_insn "<u>div<mode>3"
  [(set (match_operand:GPR 0 "register_operand" "=&d")
	(any_div:GPR (match_operand:GPR 1 "register_operand" "d")
		     (match_operand:GPR 2 "register_operand" "d")))]
  "TARGET_LOONGSON_2EF || TARGET_LOONGSON_3A"
  {
    if (TARGET_LOONGSON_2EF)
      return mips_output_division ("<d>div<u>.g\t%0,%1,%2", operands);
    else
      return mips_output_division ("gs<d>div<u>\t%0,%1,%2", operands);
  }
  [(set_attr "type" "idiv3")
   (set_attr "mode" "<MODE>")])

(define_insn "<u>mod<mode>3"
  [(set (match_operand:GPR 0 "register_operand" "=&d")
	(any_mod:GPR (match_operand:GPR 1 "register_operand" "d")
		     (match_operand:GPR 2 "register_operand" "d")))]
  "TARGET_LOONGSON_2EF || TARGET_LOONGSON_3A"
  {
    if (TARGET_LOONGSON_2EF)
      return mips_output_division ("<d>mod<u>.g\t%0,%1,%2", operands);
    else
      return mips_output_division ("gs<d>mod<u>\t%0,%1,%2", operands);
  }
  [(set_attr "type" "idiv3")
   (set_attr "mode" "<MODE>")])
