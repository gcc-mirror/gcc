;; Copyright (C) 2011-2013 Free Software Foundation, Inc.
;;
;; This file is part of GCC.
;;
;; GCC is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published
;; by the Free Software Foundation; either version 3, or (at your
;; option) any later version.
;;
;; GCC is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
;; License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GCC; see the file COPYING3.  If not see
;; <http://www.gnu.org/licenses/>.
;;
;; This file contains ARM instructions that support fixed-point operations.

(define_insn "add<mode>3"
  [(set (match_operand:FIXED 0 "s_register_operand" "=r")
	(plus:FIXED (match_operand:FIXED 1 "s_register_operand" "r")
		    (match_operand:FIXED 2 "s_register_operand" "r")))]
  "TARGET_32BIT"
  "add%?\\t%0, %1, %2"
  [(set_attr "predicable" "yes")])

(define_insn "add<mode>3"
  [(set (match_operand:ADDSUB 0 "s_register_operand" "=r")
	(plus:ADDSUB (match_operand:ADDSUB 1 "s_register_operand" "r")
		     (match_operand:ADDSUB 2 "s_register_operand" "r")))]
  "TARGET_INT_SIMD"
  "sadd<qaddsub_suf>%?\\t%0, %1, %2"
  [(set_attr "predicable" "yes")])

(define_insn "usadd<mode>3"
  [(set (match_operand:UQADDSUB 0 "s_register_operand" "=r")
	(us_plus:UQADDSUB (match_operand:UQADDSUB 1 "s_register_operand" "r")
			  (match_operand:UQADDSUB 2 "s_register_operand" "r")))]
  "TARGET_INT_SIMD"
  "uqadd<qaddsub_suf>%?\\t%0, %1, %2"
  [(set_attr "predicable" "yes")])

(define_insn "ssadd<mode>3"
  [(set (match_operand:QADDSUB 0 "s_register_operand" "=r")
	(ss_plus:QADDSUB (match_operand:QADDSUB 1 "s_register_operand" "r")
			 (match_operand:QADDSUB 2 "s_register_operand" "r")))]
  "TARGET_INT_SIMD"
  "qadd<qaddsub_suf>%?\\t%0, %1, %2"
  [(set_attr "predicable" "yes")])

(define_insn "sub<mode>3"
  [(set (match_operand:FIXED 0 "s_register_operand" "=r")
	(minus:FIXED (match_operand:FIXED 1 "s_register_operand" "r")
		     (match_operand:FIXED 2 "s_register_operand" "r")))]
  "TARGET_32BIT"
  "sub%?\\t%0, %1, %2"
  [(set_attr "predicable" "yes")])

(define_insn "sub<mode>3"
  [(set (match_operand:ADDSUB 0 "s_register_operand" "=r")
	(minus:ADDSUB (match_operand:ADDSUB 1 "s_register_operand" "r")
		      (match_operand:ADDSUB 2 "s_register_operand" "r")))]
  "TARGET_INT_SIMD"
  "ssub<qaddsub_suf>%?\\t%0, %1, %2"
  [(set_attr "predicable" "yes")])

(define_insn "ussub<mode>3"
  [(set (match_operand:UQADDSUB 0 "s_register_operand" "=r")
	(us_minus:UQADDSUB
	  (match_operand:UQADDSUB 1 "s_register_operand" "r")
	  (match_operand:UQADDSUB 2 "s_register_operand" "r")))]
  "TARGET_INT_SIMD"
  "uqsub<qaddsub_suf>%?\\t%0, %1, %2"
  [(set_attr "predicable" "yes")])

(define_insn "sssub<mode>3"
  [(set (match_operand:QADDSUB 0 "s_register_operand" "=r")
	(ss_minus:QADDSUB (match_operand:QADDSUB 1 "s_register_operand" "r")
			  (match_operand:QADDSUB 2 "s_register_operand" "r")))]
  "TARGET_INT_SIMD"
  "qsub<qaddsub_suf>%?\\t%0, %1, %2"
  [(set_attr "predicable" "yes")])

;; Fractional multiplies.

; Note: none of these do any rounding.

(define_expand "mulqq3"
  [(set (match_operand:QQ 0 "s_register_operand" "")
	(mult:QQ (match_operand:QQ 1 "s_register_operand" "")
		 (match_operand:QQ 2 "s_register_operand" "")))]
  "TARGET_DSP_MULTIPLY && arm_arch_thumb2"
{
  rtx tmp1 = gen_reg_rtx (HImode);
  rtx tmp2 = gen_reg_rtx (HImode);
  rtx tmp3 = gen_reg_rtx (SImode);
  
  emit_insn (gen_extendqihi2 (tmp1, gen_lowpart (QImode, operands[1])));
  emit_insn (gen_extendqihi2 (tmp2, gen_lowpart (QImode, operands[2])));
  emit_insn (gen_mulhisi3 (tmp3, tmp1, tmp2));
  emit_insn (gen_extv (gen_lowpart (SImode, operands[0]), tmp3, GEN_INT (8),
		       GEN_INT (7)));
  DONE;
})

(define_expand "mulhq3"
  [(set (match_operand:HQ 0 "s_register_operand" "")
	(mult:HQ (match_operand:HQ 1 "s_register_operand" "")
		 (match_operand:HQ 2 "s_register_operand" "")))]
  "TARGET_DSP_MULTIPLY && arm_arch_thumb2"
{
  rtx tmp = gen_reg_rtx (SImode);

  emit_insn (gen_mulhisi3 (tmp, gen_lowpart (HImode, operands[1]),
			   gen_lowpart (HImode, operands[2])));
  /* We're doing a s.15 * s.15 multiplication, getting an s.30 result.  Extract
     an s.15 value from that.  This won't overflow/saturate for _Fract
     values.  */
  emit_insn (gen_extv (gen_lowpart (SImode, operands[0]), tmp,
		       GEN_INT (16), GEN_INT (15)));
  DONE;
})

(define_expand "mulsq3"
  [(set (match_operand:SQ 0 "s_register_operand" "")
	(mult:SQ (match_operand:SQ 1 "s_register_operand" "")
		 (match_operand:SQ 2 "s_register_operand" "")))]
  "TARGET_32BIT && arm_arch3m"
{
  rtx tmp1 = gen_reg_rtx (DImode);
  rtx tmp2 = gen_reg_rtx (SImode);
  rtx tmp3 = gen_reg_rtx (SImode);
  
  /* s.31 * s.31 -> s.62 multiplication.  */
  emit_insn (gen_mulsidi3 (tmp1, gen_lowpart (SImode, operands[1]),
			   gen_lowpart (SImode, operands[2])));
  emit_insn (gen_lshrsi3 (tmp2, gen_lowpart (SImode, tmp1), GEN_INT (31)));
  emit_insn (gen_ashlsi3 (tmp3, gen_highpart (SImode, tmp1), GEN_INT (1)));
  emit_insn (gen_iorsi3 (gen_lowpart (SImode, operands[0]), tmp2, tmp3));

  DONE;
})

;; Accumulator multiplies.

(define_expand "mulsa3"
  [(set (match_operand:SA 0 "s_register_operand" "")
	(mult:SA (match_operand:SA 1 "s_register_operand" "")
		 (match_operand:SA 2 "s_register_operand" "")))]
  "TARGET_32BIT && arm_arch3m"
{
  rtx tmp1 = gen_reg_rtx (DImode);
  rtx tmp2 = gen_reg_rtx (SImode);
  rtx tmp3 = gen_reg_rtx (SImode);
  
  emit_insn (gen_mulsidi3 (tmp1, gen_lowpart (SImode, operands[1]),
			   gen_lowpart (SImode, operands[2])));
  emit_insn (gen_lshrsi3 (tmp2, gen_lowpart (SImode, tmp1), GEN_INT (15)));
  emit_insn (gen_ashlsi3 (tmp3, gen_highpart (SImode, tmp1), GEN_INT (17)));
  emit_insn (gen_iorsi3 (gen_lowpart (SImode, operands[0]), tmp2, tmp3));

  DONE;
})

(define_expand "mulusa3"
  [(set (match_operand:USA 0 "s_register_operand" "")
	(mult:USA (match_operand:USA 1 "s_register_operand" "")
		  (match_operand:USA 2 "s_register_operand" "")))]
  "TARGET_32BIT && arm_arch3m"
{
  rtx tmp1 = gen_reg_rtx (DImode);
  rtx tmp2 = gen_reg_rtx (SImode);
  rtx tmp3 = gen_reg_rtx (SImode);
  
  emit_insn (gen_umulsidi3 (tmp1, gen_lowpart (SImode, operands[1]),
			    gen_lowpart (SImode, operands[2])));
  emit_insn (gen_lshrsi3 (tmp2, gen_lowpart (SImode, tmp1), GEN_INT (16)));
  emit_insn (gen_ashlsi3 (tmp3, gen_highpart (SImode, tmp1), GEN_INT (16)));
  emit_insn (gen_iorsi3 (gen_lowpart (SImode, operands[0]), tmp2, tmp3));
  
  DONE;
})

;; The code sequence emitted by this insn pattern uses the Q flag, which GCC
;; doesn't generally know about, so we don't bother expanding to individual
;; instructions.  It may be better to just use an out-of-line asm libcall for
;; this.

(define_insn "ssmulsa3"
  [(set (match_operand:SA 0 "s_register_operand" "=r")
	(ss_mult:SA (match_operand:SA 1 "s_register_operand" "r")
		    (match_operand:SA 2 "s_register_operand" "r")))
   (clobber (match_scratch:DI 3 "=r"))
   (clobber (match_scratch:SI 4 "=r"))
   (clobber (reg:CC CC_REGNUM))]
  "TARGET_32BIT && arm_arch6"
{
  /* s16.15 * s16.15 -> s32.30.  */
  output_asm_insn ("smull\\t%Q3, %R3, %1, %2", operands);

  if (TARGET_ARM)
    output_asm_insn ("msr\\tAPSR_nzcvq, #0", operands);
  else
    {
      output_asm_insn ("mov\\t%4, #0", operands);
      output_asm_insn ("msr\\tAPSR_nzcvq, %4", operands);
    }

  /* We have:
      31  high word  0     31  low word  0 

    [ S i i .... i i i ] [ i f f f ... f f ]
                        |
			v
	     [ S i ... i f ... f f ]

    Need 16 integral bits, so saturate at 15th bit of high word.  */

  output_asm_insn ("ssat\\t%R3, #15, %R3", operands);
  output_asm_insn ("mrs\\t%4, APSR", operands);
  output_asm_insn ("tst\\t%4, #1<<27", operands);
  if (TARGET_THUMB2)
    output_asm_insn ("it\\tne", operands);
  output_asm_insn ("mvnne\\t%Q3, %R3, asr #32", operands);
  output_asm_insn ("mov\\t%0, %Q3, lsr #15", operands);
  output_asm_insn ("orr\\t%0, %0, %R3, asl #17", operands);
  return "";
}
  [(set_attr "conds" "clob")
   (set (attr "length")
	(if_then_else (eq_attr "is_thumb" "yes")
		      (const_int 38)
		      (const_int 32)))])

;; Same goes for this.

(define_insn "usmulusa3"
  [(set (match_operand:USA 0 "s_register_operand" "=r")
	(us_mult:USA (match_operand:USA 1 "s_register_operand" "r")
		     (match_operand:USA 2 "s_register_operand" "r")))
   (clobber (match_scratch:DI 3 "=r"))
   (clobber (match_scratch:SI 4 "=r"))
   (clobber (reg:CC CC_REGNUM))]
  "TARGET_32BIT && arm_arch6"
{
  /* 16.16 * 16.16 -> 32.32.  */
  output_asm_insn ("umull\\t%Q3, %R3, %1, %2", operands);

  if (TARGET_ARM)
    output_asm_insn ("msr\\tAPSR_nzcvq, #0", operands);
  else
    {
      output_asm_insn ("mov\\t%4, #0", operands);
      output_asm_insn ("msr\\tAPSR_nzcvq, %4", operands);
    }

  /* We have:
      31  high word  0     31  low word  0 

    [ i i i .... i i i ] [ f f f f ... f f ]
                        |
			v
	     [ i i ... i f ... f f ]

    Need 16 integral bits, so saturate at 16th bit of high word.  */

  output_asm_insn ("usat\\t%R3, #16, %R3", operands);
  output_asm_insn ("mrs\\t%4, APSR", operands);
  output_asm_insn ("tst\\t%4, #1<<27", operands);
  if (TARGET_THUMB2)
    output_asm_insn ("it\\tne", operands);
  output_asm_insn ("sbfxne\\t%Q3, %R3, #15, #1", operands);
  output_asm_insn ("lsr\\t%0, %Q3, #16", operands);
  output_asm_insn ("orr\\t%0, %0, %R3, asl #16", operands);
  return "";
}
  [(set_attr "conds" "clob")
   (set (attr "length")
	(if_then_else (eq_attr "is_thumb" "yes")
		      (const_int 38)
		      (const_int 32)))])

(define_expand "mulha3"
  [(set (match_operand:HA 0 "s_register_operand" "")
	(mult:HA (match_operand:HA 1 "s_register_operand" "")
		 (match_operand:HA 2 "s_register_operand" "")))]
  "TARGET_DSP_MULTIPLY && arm_arch_thumb2"
{
  rtx tmp = gen_reg_rtx (SImode);
  
  emit_insn (gen_mulhisi3 (tmp, gen_lowpart (HImode, operands[1]),
			   gen_lowpart (HImode, operands[2])));
  emit_insn (gen_extv (gen_lowpart (SImode, operands[0]), tmp, GEN_INT (16),
		       GEN_INT (7)));

  DONE;
})

(define_expand "muluha3"
  [(set (match_operand:UHA 0 "s_register_operand" "")
	(mult:UHA (match_operand:UHA 1 "s_register_operand" "")
		  (match_operand:UHA 2 "s_register_operand" "")))]
  "TARGET_DSP_MULTIPLY"
{
  rtx tmp1 = gen_reg_rtx (SImode);
  rtx tmp2 = gen_reg_rtx (SImode);
  rtx tmp3 = gen_reg_rtx (SImode);
  
  /* 8.8 * 8.8 -> 16.16 multiply.  */
  emit_insn (gen_zero_extendhisi2 (tmp1, gen_lowpart (HImode, operands[1])));
  emit_insn (gen_zero_extendhisi2 (tmp2, gen_lowpart (HImode, operands[2])));
  emit_insn (gen_mulsi3 (tmp3, tmp1, tmp2));
  emit_insn (gen_extzv (gen_lowpart (SImode, operands[0]), tmp3,
			GEN_INT (16), GEN_INT (8)));

  DONE;
})

(define_expand "ssmulha3"
  [(set (match_operand:HA 0 "s_register_operand" "")
	(ss_mult:HA (match_operand:HA 1 "s_register_operand" "")
		    (match_operand:HA 2 "s_register_operand" "")))]
  "TARGET_32BIT && TARGET_DSP_MULTIPLY && arm_arch6"
{
  rtx tmp = gen_reg_rtx (SImode);
  rtx rshift;
  
  emit_insn (gen_mulhisi3 (tmp, gen_lowpart (HImode, operands[1]),
			   gen_lowpart (HImode, operands[2])));

  rshift = gen_rtx_ASHIFTRT (SImode, tmp, GEN_INT (7));

  emit_insn (gen_rtx_SET (VOIDmode, gen_lowpart (HImode, operands[0]),
			  gen_rtx_SS_TRUNCATE (HImode, rshift)));

  DONE;
})

(define_expand "usmuluha3"
  [(set (match_operand:UHA 0 "s_register_operand" "")
	(us_mult:UHA (match_operand:UHA 1 "s_register_operand" "")
		     (match_operand:UHA 2 "s_register_operand" "")))]
  "TARGET_INT_SIMD"
{
  rtx tmp1 = gen_reg_rtx (SImode);
  rtx tmp2 = gen_reg_rtx (SImode);
  rtx tmp3 = gen_reg_rtx (SImode);
  rtx rshift_tmp = gen_reg_rtx (SImode);
  
  /* Note: there's no smul[bt][bt] equivalent for unsigned multiplies.  Use a
     normal 32x32->32-bit multiply instead.  */
  emit_insn (gen_zero_extendhisi2 (tmp1, gen_lowpart (HImode, operands[1])));
  emit_insn (gen_zero_extendhisi2 (tmp2, gen_lowpart (HImode, operands[2])));
  
  emit_insn (gen_mulsi3 (tmp3, tmp1, tmp2));

  /* The operand to "usat" is signed, so we cannot use the "..., asr #8"
     form of that instruction since the multiplication result TMP3 may have the
     top bit set, thus be negative and saturate to zero.  Use a separate
     logical right-shift instead.  */
  emit_insn (gen_lshrsi3 (rshift_tmp, tmp3, GEN_INT (8)));
  emit_insn (gen_arm_usatsihi (gen_lowpart (HImode, operands[0]), rshift_tmp));

  DONE;
})

(define_insn "arm_ssatsihi_shift"
  [(set (match_operand:HI 0 "s_register_operand" "=r")
	(ss_truncate:HI (match_operator:SI 1 "sat_shift_operator"
			  [(match_operand:SI 2 "s_register_operand" "r")
			   (match_operand:SI 3 "immediate_operand" "I")])))]
  "TARGET_32BIT && arm_arch6"
  "ssat%?\\t%0, #16, %2%S1"
  [(set_attr "predicable" "yes")
   (set_attr "insn" "sat")
   (set_attr "shift" "1")
   (set_attr "type" "alu_shift")])

(define_insn "arm_usatsihi"
  [(set (match_operand:HI 0 "s_register_operand" "=r")
	(us_truncate:HI (match_operand:SI 1 "s_register_operand")))]
  "TARGET_INT_SIMD"
  "usat%?\\t%0, #16, %1"
  [(set_attr "predicable" "yes")
   (set_attr "insn" "sat")])
