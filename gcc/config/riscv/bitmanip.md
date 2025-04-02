;; Machine description for RISC-V Bit Manipulation operations.
;; Copyright (C) 2021-2025 Free Software Foundation, Inc.

;; This file is part of GCC.

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

;; ZBA extension.

(define_insn "*zero_extendsidi2_bitmanip"
  [(set (match_operand:DI 0 "register_operand" "=r,r")
	(zero_extend:DI (match_operand:SI 1 "nonimmediate_operand" "r,m")))]
  "TARGET_64BIT && TARGET_ZBA && !TARGET_XTHEADMEMIDX"
  "@
   zext.w\t%0,%1
   lwu\t%0,%1"
  [(set_attr "type" "bitmanip,load")
   (set_attr "mode" "DI")])

(define_insn "*shNadd"
  [(set (match_operand:X 0 "register_operand" "=r")
	(plus:X (ashift:X (match_operand:X 1 "register_operand" "r")
			  (match_operand:QI 2 "imm123_operand" "Ds3"))
		(match_operand:X 3 "register_operand" "r")))]
  "TARGET_ZBA"
  "sh%2add\t%0,%1,%3"
  [(set_attr "type" "bitmanip")
   (set_attr "mode" "<X:MODE>")])

; When using strength-reduction, we will reduce a multiplication to a
; sequence of shifts and adds.  If this is performed with 32-bit types
; and followed by a division, the lack of w-form sh[123]add will make
; combination impossible and lead to a slli + addw being generated.
; Split the sequence with the knowledge that a w-form div will perform
; implicit sign-extensions.
(define_split
  [(set (match_operand:DI 0 "register_operand")
	(sign_extend:DI (div:SI (plus:SI (ashift:SI (subreg:SI (match_operand:DI 1 "register_operand") 0)
						    (match_operand:QI 2 "imm123_operand"))
					 (subreg:SI (match_operand:DI 3 "register_operand") 0))
				(match_operand:SI 4 "register_operand"))))
   (clobber (match_operand:DI 5 "register_operand"))]
  "TARGET_64BIT && TARGET_ZBA"
   [(set (match_dup 5) (plus:DI (ashift:DI (match_dup 1) (match_dup 2)) (match_dup 3)))
    (set (match_dup 0) (sign_extend:DI (div:SI (subreg:SI (match_dup 5) 0) (match_dup 4))))])

; Zba does not provide W-forms of sh[123]add(.uw)?, which leads to an
; interesting irregularity: we can generate a signed 32-bit result
; using slli(.uw)?+ addw, but a unsigned 32-bit result can be more
; efficiently be generated as sh[123]add+zext.w (the .uw can be
; dropped, if we zero-extend the output anyway).
;
; To enable this optimization, we split [ slli(.uw)?, addw, zext.w ]
; into [ sh[123]add, zext.w ] for use during combine.
(define_split
  [(set (match_operand:DI 0 "register_operand")
	(zero_extend:DI (plus:SI (ashift:SI (subreg:SI (match_operand:DI 1 "register_operand") 0)
						       (match_operand:QI 2 "imm123_operand"))
				 (subreg:SI (match_operand:DI 3 "register_operand") 0))))]
  "TARGET_64BIT && TARGET_ZBA"
  [(set (match_dup 0) (plus:DI (ashift:DI (match_dup 1) (match_dup 2)) (match_dup 3)))
   (set (match_dup 0) (zero_extend:DI (subreg:SI (match_dup 0) 0)))])

(define_split
  [(set (match_operand:DI 0 "register_operand")
	(zero_extend:DI (plus:SI (subreg:SI (and:DI (ashift:DI (match_operand:DI 1 "register_operand")
							       (match_operand:QI 2 "imm123_operand"))
						    (match_operand:DI 3 "consecutive_bits_operand")) 0)
				 (subreg:SI (match_operand:DI 4 "register_operand") 0))))]
  "TARGET_64BIT && TARGET_ZBA
   && riscv_shamt_matches_mask_p (INTVAL (operands[2]), INTVAL (operands[3]))
   /* Ensure the mask includes all the bits in SImode.  */
   && ((INTVAL (operands[3]) & (HOST_WIDE_INT_1U << 31)) != 0)"
  [(set (match_dup 0) (plus:DI (ashift:DI (match_dup 1) (match_dup 2)) (match_dup 4)))
   (set (match_dup 0) (zero_extend:DI (subreg:SI (match_dup 0) 0)))])

; Make sure that an andi followed by a sh[123]add remains a two instruction
; sequence--and is not torn apart into slli, slri, add.
(define_insn_and_split "*andi_add.uw"
  [(set (match_operand:DI 0 "register_operand" "=r")
	(plus:DI (and:DI (ashift:DI (match_operand:DI 1 "register_operand" "r")
				    (match_operand:QI 2 "imm123_operand" "Ds3"))
			 (match_operand:DI 3 "consecutive_bits_operand" ""))
		 (match_operand:DI 4 "register_operand" "r")))
   (clobber (match_scratch:DI 5 "=&r"))]
  "TARGET_64BIT && TARGET_ZBA
   && riscv_shamt_matches_mask_p (INTVAL (operands[2]), INTVAL (operands[3]))
   && SMALL_OPERAND (INTVAL (operands[3]) >> INTVAL (operands[2]))"
  "#"
  "&& reload_completed"
  [(set (match_dup 5) (and:DI (match_dup 1) (match_dup 3)))
   (set (match_dup 0) (plus:DI (ashift:DI (match_dup 5) (match_dup 2))
			       (match_dup 4)))]
{
	operands[3] = GEN_INT (INTVAL (operands[3]) >> INTVAL (operands[2]));
}
[(set_attr "type" "bitmanip")])

(define_insn "*shNadduw"
  [(set (match_operand:DI 0 "register_operand" "=r")
	(plus:DI
	  (and:DI (ashift:DI (match_operand:DI 1 "register_operand" "r")
			     (match_operand:QI 2 "imm123_operand" "Ds3"))
		 (match_operand 3 "immediate_operand" "n"))
	  (match_operand:DI 4 "register_operand" "r")))]
  "TARGET_64BIT && TARGET_ZBA
   && (INTVAL (operands[3]) >> INTVAL (operands[2])) == 0xffffffff"
  "sh%2add.uw\t%0,%1,%4"
  [(set_attr "type" "bitmanip")
   (set_attr "mode" "DI")])

;; During combine, we may encounter an attempt to combine
;;   slli rtmp, rs, #imm
;;   zext.w rtmp, rtmp
;;   sh[123]add rd, rtmp, rs2
;; which will lead to the immediate not satisfying the above constraints.
;; By splitting the compound expression, we can simplify to a slli and a
;; sh[123]add.uw.
(define_split
  [(set (match_operand:DI 0 "register_operand")
	(plus:DI (and:DI (ashift:DI (match_operand:DI 1 "register_operand")
				    (match_operand:QI 2 "immediate_operand"))
			 (match_operand:DI 3 "consecutive_bits_operand"))
		 (match_operand:DI 4 "register_operand")))
   (clobber (match_operand:DI 5 "register_operand"))]
  "TARGET_64BIT && TARGET_ZBA"
  [(set (match_dup 5) (ashift:DI (match_dup 1) (match_dup 6)))
   (set (match_dup 0) (plus:DI (and:DI (ashift:DI (match_dup 5)
						  (match_dup 7))
				       (match_dup 8))
			       (match_dup 4)))]
{
	unsigned HOST_WIDE_INT mask = UINTVAL (operands[3]);
	/* scale: shift within the sh[123]add.uw */
	unsigned HOST_WIDE_INT scale = 32 - clz_hwi (mask);
	/* bias:  pre-scale amount (i.e. the prior shift amount) */
	int bias = ctz_hwi (mask) - scale;

	/* If the bias + scale don't add up to operand[2], reject. */
	if ((scale + bias) != UINTVAL (operands[2]))
	   FAIL;

	/* If the shift-amount is out-of-range for sh[123]add.uw, reject. */
	if ((scale < 1) || (scale > 3))
	   FAIL;

	/* If there's no bias, the '*shNadduw' pattern should have matched. */
	if (bias == 0)
	   FAIL;

	operands[6] = GEN_INT (bias);
	operands[7] = GEN_INT (scale);
	operands[8] = GEN_INT (0xffffffffULL << scale);
})

(define_insn "*add.uw"
  [(set (match_operand:DI 0 "register_operand" "=r")
	(plus:DI (zero_extend:DI
		   (match_operand:SI 1 "register_operand" "r"))
		 (match_operand:DI 2 "register_operand" "r")))]
  "TARGET_64BIT && TARGET_ZBA"
  "add.uw\t%0,%1,%2"
  [(set_attr "type" "bitmanip")
   (set_attr "mode" "DI")])

(define_insn "*slliuw"
  [(set (match_operand:DI 0 "register_operand" "=r")
	(and:DI (ashift:DI (match_operand:DI 1 "register_operand" "r")
			   (match_operand:QI 2 "immediate_operand" "I"))
		(match_operand 3 "immediate_operand" "n")))]
  "TARGET_64BIT && TARGET_ZBA
   && (INTVAL (operands[3]) >> INTVAL (operands[2])) == 0xffffffff"
  "slli.uw\t%0,%1,%2"
  [(set_attr "type" "bitmanip")
   (set_attr "mode" "DI")])

;; Combine will reassociate the operands in the most useful way here.  We
;; just have to give it guidance on where to split the result to facilitate
;; shNadd.uw generation.
(define_split
  [(set (match_operand:DI 0 "register_operand")
	(plus:DI (plus:DI (and:DI (ashift:DI (match_operand:DI 1 "register_operand")
					     (match_operand:QI 2 "imm123_operand"))
				  (match_operand 3 "consecutive_bits32_operand"))
			  (match_operand:DI 4 "register_operand"))
		 (match_operand 5 "immediate_operand")))]
  "TARGET_64BIT && TARGET_ZBA"
  [(set (match_dup 0)
	(plus:DI (and:DI (ashift:DI (match_dup 1) (match_dup 2))
			 (match_dup 3))
		 (match_dup 4)))
   (set (match_dup 0) (plus:DI (match_dup 0) (match_dup 5)))])

;; ZBB extension.

(define_expand "clzdi2"
  [(set (match_operand:DI 0 "register_operand")
	(clz:DI (match_operand:DI 1 "register_operand")))]
  "TARGET_64BIT && (TARGET_ZBB || TARGET_XTHEADBB)")

(define_expand "clzsi2"
  [(set (match_operand:SI 0 "register_operand")
	(clz:SI (match_operand:SI 1 "register_operand")))]
  "TARGET_ZBB || (!TARGET_64BIT && TARGET_XTHEADBB)")

(define_expand "ctz<mode>2"
  [(set (match_operand:GPR 0 "register_operand")
	(ctz:GPR (match_operand:GPR 1 "register_operand")))]
  "TARGET_ZBB")

(define_expand "popcount<mode>2"
  [(set (match_operand:GPR 0 "register_operand")
	(popcount:GPR (match_operand:GPR 1 "register_operand")))]
  "TARGET_ZBB")

(define_insn "<optab>_not<mode>3"
  [(set (match_operand:X 0 "register_operand" "=r")
        (bitmanip_bitwise:X (not:X (match_operand:X 1 "register_operand" "r"))
                            (match_operand:X 2 "register_operand" "r")))]
  "TARGET_ZBB || TARGET_ZBKB"
  "<insn>n\t%0,%2,%1"
  [(set_attr "type" "bitmanip")
   (set_attr "mode" "<X:MODE>")])

(define_insn_and_split "*<optab>_not_const<mode>"
  [(set (match_operand:X 0 "register_operand" "=r")
       (bitmanip_bitwise:X (not:X (match_operand:X 1 "register_operand" "r"))
              (match_operand:X 2 "const_arith_operand" "I")))
  (clobber (match_scratch:X 3 "=&r"))]
  "(TARGET_ZBB || TARGET_ZBKB) && !TARGET_ZCB
   && !optimize_function_for_size_p (cfun)"
  "#"
  "&& reload_completed"
  [(set (match_dup 3) (match_dup 2))
   (set (match_dup 0) (bitmanip_bitwise:X (not:X (match_dup 1)) (match_dup 3)))]
  ""
  [(set_attr "type" "bitmanip")])

;; '(a >= 0) ? b : 0' is emitted branchless (from if-conversion).  Without a
;; bit of extra help for combine (i.e., the below split), we end up emitting
;; not/srai/and instead of combining the not into an andn.
(define_split
  [(set (match_operand:DI 0 "register_operand")
	(and:DI (neg:DI (ge:DI (match_operand:DI 1 "register_operand")
			       (const_int 0)))
		(match_operand:DI 2 "register_operand")))
   (clobber (match_operand:DI 3 "register_operand"))]
  "TARGET_ZBB || TARGET_ZBKB"
  [(set (match_dup 3) (ashiftrt:DI (match_dup 1) (const_int 63)))
   (set (match_dup 0) (and:DI (not:DI (match_dup 3)) (match_dup 2)))])

(define_insn "*xor_not<mode>"
  [(set (match_operand:X 0 "register_operand" "=r")
        (not:X (xor:X (match_operand:X 1 "register_operand" "r")
                      (match_operand:X 2 "register_operand" "r"))))]
  "TARGET_ZBB || TARGET_ZBKB"
  "xnor\t%0,%1,%2"
  [(set_attr "type" "bitmanip")
   (set_attr "mode" "<X:MODE>")])

(define_insn "*<bitmanip_optab>si2"
  [(set (match_operand:SI 0 "register_operand" "=r")
        (clz_ctz_pcnt:SI (match_operand:SI 1 "register_operand" "r")))]
  "TARGET_ZBB"
  "<bitmanip_insn>%~\t%0,%1"
  [(set_attr "type" "<bitmanip_insn>")
   (set_attr "mode" "SI")])

(define_insn "*<bitmanip_optab>disi2"
  [(set (match_operand:DI 0 "register_operand" "=r")
        (any_extend:DI
          (clz_ctz_pcnt:SI (match_operand:SI 1 "register_operand" "r"))))]
  "TARGET_64BIT && TARGET_ZBB"
  "<bitmanip_insn>w\t%0,%1"
  [(set_attr "type" "<bitmanip_insn>")
   (set_attr "mode" "SI")])

;; A SImode clz_ctz_pcnt may be extended to DImode via subreg.
(define_insn "*<bitmanip_optab>disi2_sext"
  [(set (match_operand:DI 0 "register_operand" "=r")
        (and:DI (subreg:DI
          (clz_ctz_pcnt:SI (match_operand:SI 1 "register_operand" "r")) 0)
          (match_operand:DI 2 "const_int_operand")))]
  "TARGET_64BIT && TARGET_ZBB && ((INTVAL (operands[2]) & 0x3f) == 0x3f)"
  "<bitmanip_insn>w\t%0,%1"
  [(set_attr "type" "<bitmanip_insn>")
   (set_attr "mode" "SI")])

(define_insn "*<bitmanip_optab>di2"
  [(set (match_operand:DI 0 "register_operand" "=r")
        (clz_ctz_pcnt:DI (match_operand:DI 1 "register_operand" "r")))]
  "TARGET_64BIT && TARGET_ZBB"
  "<bitmanip_insn>\t%0,%1"
  [(set_attr "type" "<bitmanip_insn>")
   (set_attr "mode" "DI")])

(define_insn "*zero_extendhi<GPR:mode>2_bitmanip"
  [(set (match_operand:GPR 0 "register_operand" "=r,r")
        (zero_extend:GPR (match_operand:HI 1 "nonimmediate_operand" "r,m")))]
  "TARGET_ZBB && !TARGET_XTHEADMEMIDX"
  "@
   zext.h\t%0,%1
   lhu\t%0,%1"
  [(set_attr "type" "bitmanip,load")
   (set_attr "mode" "<GPR:MODE>")])

(define_insn "*extend<SHORT:mode><SUPERQI:mode>2_bitmanip"
  [(set (match_operand:SUPERQI   0 "register_operand"     "=r,r")
	(sign_extend:SUPERQI
	    (match_operand:SHORT 1 "nonimmediate_operand" " r,m")))]
  "TARGET_ZBB && !TARGET_XTHEADMEMIDX"
  "@
   sext.<SHORT:size>\t%0,%1
   l<SHORT:size>\t%0,%1"
  [(set_attr "type" "bitmanip,load")
   (set_attr "mode" "<SUPERQI:MODE>")])

(define_expand "rotrdi3"
  [(set (match_operand:DI 0 "register_operand")
	(rotatert:DI (match_operand:DI 1 "register_operand")
		     (match_operand:QI 2 "arith_operand")))]
  "TARGET_64BIT && (TARGET_ZBB || TARGET_XTHEADBB || TARGET_ZBKB)"
{
  if (TARGET_XTHEADBB && !immediate_operand (operands[2], VOIDmode))
    FAIL;
})

(define_insn "*rotrsi3"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(rotatert:SI (match_operand:SI 1 "register_operand" "r")
		     (match_operand:QI 2 "arith_operand" "rI")))]
  "TARGET_ZBB || TARGET_ZBKB"
  "ror%i2%~\t%0,%1,%2"
  [(set_attr "type" "bitmanip")])

(define_expand "rotrsi3"
  [(set (match_operand:SI 0 "register_operand" "=r")
       (rotatert:SI (match_operand:SI 1 "register_operand" "r")
                    (match_operand:QI 2 "arith_operand" "rI")))]
  "TARGET_ZBB || TARGET_ZBKB || TARGET_XTHEADBB"
{
  if (TARGET_XTHEADBB && !immediate_operand (operands[2], VOIDmode))
    FAIL;
  if (TARGET_64BIT && register_operand (operands[2], QImode))
    {
      rtx t = gen_reg_rtx (DImode);
      emit_insn (gen_rotrsi3_sext (t, operands[1], operands[2]));
      t = gen_lowpart (SImode, t);
      SUBREG_PROMOTED_VAR_P (t) = 1;
      SUBREG_PROMOTED_SET (t, SRP_SIGNED);
      emit_move_insn (operands[0], t);
      DONE;
    }
})

(define_insn "*rotrdi3"
  [(set (match_operand:DI 0 "register_operand" "=r")
	(rotatert:DI (match_operand:DI 1 "register_operand" "r")
		     (match_operand:QI 2 "arith_operand" "rI")))]
  "TARGET_64BIT && (TARGET_ZBB || TARGET_ZBKB)"
  "ror%i2\t%0,%1,%2"
  [(set_attr "type" "bitmanip")])

(define_insn "rotrsi3_sext"
  [(set (match_operand:DI 0 "register_operand" "=r")
	(sign_extend:DI (rotatert:SI (match_operand:SI 1 "register_operand" "r")
                                 (match_operand:QI 2 "arith_operand" "rI"))))]
  "TARGET_64BIT && (TARGET_ZBB || TARGET_ZBKB)"
  "ror%i2%~\t%0,%1,%2"
  [(set_attr "type" "bitmanip")])

(define_insn "*rotlsi3"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(rotate:SI (match_operand:SI 1 "register_operand" "r")
		   (match_operand:QI 2 "register_operand" "r")))]
  "TARGET_ZBB || TARGET_ZBKB"
  "rol%~\t%0,%1,%2"
  [(set_attr "type" "bitmanip")])

(define_expand "rotlsi3"
  [(set (match_operand:SI 0 "register_operand" "=r")
       (rotate:SI (match_operand:SI 1 "register_operand" "r")
                  (match_operand:QI 2 "register_operand" "r")))]
  "TARGET_ZBB || TARGET_ZBKB"
{
  if (TARGET_64BIT)
    {
      rtx t = gen_reg_rtx (DImode);
      emit_insn (gen_rotlsi3_sext (t, operands[1], operands[2]));
      t = gen_lowpart (SImode, t);
      SUBREG_PROMOTED_VAR_P (t) = 1;
      SUBREG_PROMOTED_SET (t, SRP_SIGNED);
      emit_move_insn (operands[0], t);
      DONE;
    }
})

(define_insn "rotldi3"
  [(set (match_operand:DI 0 "register_operand" "=r")
	(rotate:DI (match_operand:DI 1 "register_operand" "r")
		   (match_operand:QI 2 "register_operand" "r")))]
  "TARGET_64BIT && (TARGET_ZBB || TARGET_ZBKB)"
  "rol\t%0,%1,%2"
  [(set_attr "type" "bitmanip")])

(define_insn "rotlsi3_sext"
  [(set (match_operand:DI 0 "register_operand" "=r")
	(sign_extend:DI (rotate:SI (match_operand:SI 1 "register_operand" "r")
				   (match_operand:QI 2 "register_operand" "r"))))]
  "TARGET_64BIT && (TARGET_ZBB || TARGET_ZBKB)"
  "rolw\t%0,%1,%2"
  [(set_attr "type" "bitmanip")])

(define_insn_and_split "*<bitmanip_optab><GPR:mode>3_mask"
  [(set (match_operand:GPR     0 "register_operand" "= r")
        (bitmanip_rotate:GPR
            (match_operand:GPR 1 "register_operand" "  r")
            (match_operator 4 "subreg_lowpart_operator"
             [(and:GPR2
               (match_operand:GPR2 2 "register_operand"  "r")
               (match_operand 3 "<GPR:shiftm1>" "<GPR:shiftm1p>"))])))]
  "TARGET_ZBB || TARGET_ZBKB"
  "#"
  "&& 1"
  [(set (match_dup 0)
        (bitmanip_rotate:GPR (match_dup 1)
                             (match_dup 2)))]
  "operands[2] = gen_lowpart (QImode, operands[2]);"
  [(set_attr "type" "bitmanip")
   (set_attr "mode" "<GPR:MODE>")])

(define_insn_and_split "*<bitmanip_optab>si3_sext_mask"
  [(set (match_operand:DI     0 "register_operand" "= r")
  (sign_extend:DI (bitmanip_rotate:SI
            (match_operand:SI 1 "register_operand" "  r")
            (match_operator 4 "subreg_lowpart_operator"
             [(and:GPR
               (match_operand:GPR 2 "register_operand"  "r")
               (match_operand 3 "const_si_mask_operand"))]))))]
  "TARGET_64BIT && (TARGET_ZBB || TARGET_ZBKB)"
  "#"
  "&& 1"
  [(set (match_dup 0)
  (sign_extend:DI (bitmanip_rotate:SI (match_dup 1)
                           (match_dup 2))))]
  "operands[2] = gen_lowpart (QImode, operands[2]);"
  [(set_attr "type" "bitmanip")
   (set_attr "mode" "DI")])

;; orc.b (or-combine) is added as an unspec for the benefit of the support
;; for optimized string functions (such as strcmp).
(define_insn "orcb<mode>2"
  [(set (match_operand:GPR 0 "register_operand" "=r")
	(unspec:GPR [(match_operand:GPR 1 "register_operand" "r")] UNSPEC_ORC_B))]
  "TARGET_ZBB"
  "orc.b\t%0,%1"
  [(set_attr "type" "bitmanip")])

(define_expand "bswapdi2"
  [(set (match_operand:DI 0 "register_operand")
	(bswap:DI (match_operand:DI 1 "register_operand")))]
  "TARGET_64BIT && (TARGET_ZBB || TARGET_XTHEADBB || TARGET_ZBKB)")

(define_expand "bswapsi2"
  [(set (match_operand:SI 0 "register_operand")
	(bswap:SI (match_operand:SI 1 "register_operand")))]
  "TARGET_ZBB || TARGET_ZBKB || TARGET_XTHEADBB"
{
  /* Expose bswapsi2 on TARGET_64BIT so that the gimple store
     merging pass will create suitable bswap insns.  We can actually
     just FAIL that case when generating RTL and let the generic code
     handle it.  */
  if (TARGET_64BIT && !TARGET_XTHEADBB)
    FAIL;
})


(define_insn "*bswap<mode>2"
  [(set (match_operand:X 0 "register_operand" "=r")
        (bswap:X (match_operand:X 1 "register_operand" "r")))]
  "TARGET_ZBB || TARGET_ZBKB"
  "rev8\t%0,%1"
  [(set_attr "type" "bitmanip")])

;; HI bswap can be emulated using SI/DI bswap followed
;; by a logical shift right
;; SI bswap for TARGET_64BIT is already similarly in
;; the common code.
(define_expand "bswaphi2"
  [(set (match_operand:HI 0 "register_operand" "=r")
        (bswap:HI (match_operand:HI 1 "register_operand" "r")))]
  "TARGET_ZBB"
{
  rtx tmp = gen_reg_rtx (word_mode);
  rtx newop1 = gen_lowpart (word_mode, operands[1]);
  if (TARGET_64BIT)
    emit_insn (gen_bswapdi2 (tmp, newop1));
  else
    emit_insn (gen_bswapsi2 (tmp, newop1));
  rtx tmp1 = gen_reg_rtx (word_mode);
  if (TARGET_64BIT)
    emit_insn (gen_lshrdi3 (tmp1, tmp, GEN_INT (64 - 16)));
  else
    emit_insn (gen_lshrsi3 (tmp1, tmp, GEN_INT (32 - 16)));
  emit_move_insn (operands[0], gen_lowpart (HImode, tmp1));
  DONE;
})

(define_expand "<bitmanip_optab>di3"
  [(set (match_operand:DI 0 "register_operand" "=r")
        (bitmanip_minmax:DI (match_operand:DI 1 "register_operand" "r")
                            (match_operand:DI 2 "register_operand" "r")))]
  "TARGET_64BIT && TARGET_ZBB")

(define_expand "<bitmanip_optab>si3"
  [(set (match_operand:SI 0 "register_operand" "=r")
        (bitmanip_minmax:SI (match_operand:SI 1 "register_operand" "r")
                            (match_operand:SI 2 "register_operand" "r")))]
  "TARGET_ZBB"
{
  if (TARGET_64BIT)
    {
      rtx t = gen_reg_rtx (DImode);
      operands[1] = force_reg (DImode, gen_rtx_SIGN_EXTEND (DImode, operands[1]));
      operands[2] = force_reg (DImode, gen_rtx_SIGN_EXTEND (DImode, operands[2]));
      emit_insn (gen_<bitmanip_optab>di3 (t, operands[1], operands[2]));
      emit_move_insn (operands[0], gen_lowpart (SImode, t));
      DONE;
    }
})

(define_insn "*<bitmanip_optab><mode>3"
  [(set (match_operand:X 0 "register_operand" "=r")
        (bitmanip_minmax:X (match_operand:X 1 "register_operand" "r")
			   (match_operand:X 2 "reg_or_0_operand" "rJ")))]
  "TARGET_ZBB"
  "<bitmanip_insn>\t%0,%1,%z2"
  [(set_attr "type" "<bitmanip_insn>")])

;; Optimize the common case of a SImode min/max against a constant
;; that is safe both for sign- and zero-extension.
(define_split
  [(set (match_operand:DI 0 "register_operand")
	(sign_extend:DI
	  (subreg:SI
	    (bitmanip_minmax:DI (zero_extend:DI
				  (match_operand:SI 1 "register_operand"))
				(match_operand:DI 2 "immediate_operand")) 0)))
   (clobber (match_operand:DI 3 "register_operand"))
   (clobber (match_operand:DI 4 "register_operand"))]
  "TARGET_64BIT && TARGET_ZBB && sext_hwi (INTVAL (operands[2]), 32) >= 0"
  [(set (match_dup 0) (<uminmax_optab>:DI (match_dup 4) (match_dup 3)))]
  "
{
  /* Load the constant into a register.  */
  emit_move_insn (operands[3], operands[2]);

  /* If operands[1] is a sign extended SUBREG, then we can use it
     directly.  Otherwise extend it into another temporary.  */
  if (SUBREG_P (operands[1])
      && SUBREG_PROMOTED_VAR_P (operands[1])
      && SUBREG_PROMOTED_SIGNED_P (operands[1]))
    operands[4] = SUBREG_REG (operands[1]);
  else
    emit_move_insn (operands[4], gen_rtx_SIGN_EXTEND (DImode, operands[1]));

  /* The minmax is actually emitted from the split pattern.  */
}")

;; ZBS extension.

(define_insn "*<bit_optab><mode>"
  [(set (match_operand:X 0 "register_operand" "=r")
	(any_or:X (ashift:X (const_int 1)
			    (match_operand:QI 2 "register_operand" "r"))
		  (match_operand:X 1 "register_operand" "r")))]
  "TARGET_ZBS"
  "<bit_optab>\t%0,%1,%2"
  [(set_attr "type" "bitmanip")])

(define_insn "*<bit_optab><mode>_mask"
  [(set (match_operand:X 0 "register_operand" "=r")
	(any_or:X
	  (ashift:X
	    (const_int 1)
	    (subreg:QI
	      (and:X (match_operand:X 2 "register_operand" "r")
		     (match_operand 3 "<X:shiftm1>" "<X:shiftm1p>")) 0))
	  (match_operand:X 1 "register_operand" "r")))]
  "TARGET_ZBS"
  "<bit_optab>\t%0,%1,%2"
  [(set_attr "type" "bitmanip")])

(define_insn "*bset<mode>_1"
  [(set (match_operand:X 0 "register_operand" "=r")
	(ashift:X (const_int 1)
		  (match_operand:QI 1 "register_operand" "r")))]
  "TARGET_ZBS"
  "bset\t%0,x0,%1"
  [(set_attr "type" "bitmanip")])

;; The result will always have bits 32..63 clear, so the zero-extend
;; is redundant.  We could split it to bset<mode>_1, but it seems
;; unnecessary.
(define_insn "*bsetdi_2"
  [(set (match_operand:DI 0 "register_operand" "=r")
	(zero_extend:DI
	  (ashift:SI (const_int 1)
		     (match_operand:QI 1 "register_operand" "r"))))]
  "TARGET_64BIT && TARGET_ZBS"
  "bset\t%0,x0,%1"
  [(set_attr "type" "bitmanip")])

;; These two splitters take advantage of the limited range of the
;; shift constant.  With the limited range we know the SImode sign
;; bit is never set, thus we can treat this as zero extending and
;; generate the bsetdi_2 pattern.
(define_insn_and_split ""
  [(set (match_operand:DI 0 "register_operand" "=r")
	(any_extend:DI
	 (ashift:SI (const_int 1)
		    (subreg:QI
		      (and:DI (not:DI (match_operand:DI 1 "register_operand" "r"))
			      (match_operand 2 "const_int_operand")) 0))))
   (clobber (match_scratch:X 3 "=&r"))]
  "TARGET_64BIT
   && TARGET_ZBS
   && (TARGET_ZBB || TARGET_ZBKB)
   && (INTVAL (operands[2]) & 0x1f) != 0x1f"
  "#"
  "&& reload_completed"
   [(set (match_dup 3) (match_dup 2))
    (set (match_dup 3) (and:DI (not:DI (match_dup 1)) (match_dup 3)))
    (set (match_dup 0) (zero_extend:DI
			 (ashift:SI (const_int 1) (match_dup 4))))]
{
  operands[4] = gen_lowpart (QImode, operands[3]);
  operands[2] = GEN_INT (INTVAL (operands[2]) & 0x1f);
}
  [(set_attr "type" "bitmanip")])

(define_insn_and_split ""
  [(set (match_operand:DI 0 "register_operand" "=r")
        (any_extend:DI
	 (ashift:SI (const_int 1)
		    (subreg:QI
		      (and:DI (match_operand:DI 1 "register_operand" "r")
			      (match_operand 2 "const_int_operand")) 0))))]
  "TARGET_64BIT
   && TARGET_ZBS
   && (INTVAL (operands[2]) & 0x1f) != 0x1f"
  "#"
  "&& 1"
  [(set (match_dup 0) (and:DI (match_dup 1) (match_dup 2)))
   (set (match_dup 0) (zero_extend:DI (ashift:SI
				     (const_int 1)
				     (subreg:QI (match_dup 0) 0))))]
  { operands[2] = GEN_INT (INTVAL (operands[2]) & 0x1f); }
  [(set_attr "type" "bitmanip")])

;; Similarly two patterns for IOR/XOR generating bset/binv to
;; manipulate a bit in a register
(define_insn_and_split ""
  [(set (match_operand:DI 0 "register_operand" "=r")
	(any_or:DI
	  (any_extend:DI
	    (ashift:SI
	      (const_int 1)
	      (subreg:QI
		(and:DI (not:DI (match_operand:DI 1 "register_operand" "r"))
			(match_operand 2 "const_int_operand")) 0)))
	  (match_operand:DI 3 "register_operand" "r")))
   (clobber (match_scratch:X 4 "=&r"))]
  "TARGET_64BIT
   && TARGET_ZBS
   && (TARGET_ZBB || TARGET_ZBKB)
   && (INTVAL (operands[2]) & 0x1f) != 0x1f"
  "#"
  "&& reload_completed"
   [(set (match_dup 4) (match_dup 2))
    (set (match_dup 4) (and:DI (not:DI (match_dup 1)) (match_dup 4)))
    (set (match_dup 0) (any_or:DI (ashift:DI (const_int 1) (match_dup 5)) (match_dup 3)))]
{
  operands[5] = gen_lowpart (QImode, operands[4]);
  operands[2] = GEN_INT (INTVAL (operands[2]) & 0x1f);
}
  [(set_attr "type" "bitmanip")])

(define_insn_and_split ""
  [(set (match_operand:DI 0 "register_operand" "=r")
	(any_or:DI
	  (any_extend:DI
	    (ashift:SI
	      (const_int 1)
	      (subreg:QI
		(and:DI (match_operand:DI 1 "register_operand" "r")
			(match_operand 2 "const_int_operand")) 0)))
	  (match_operand:DI 3 "register_operand" "r")))
   (clobber (match_scratch:X 4 "=&r"))]
  "TARGET_64BIT
   && TARGET_ZBS
   && (INTVAL (operands[2]) & 0x1f) != 0x1f"
  "#"
  "&& reload_completed"
   [(set (match_dup 4) (and:DI (match_dup 1) (match_dup 2)))
    (set (match_dup 0) (any_or:DI (ashift:DI (const_int 1) (subreg:QI (match_dup 4) 0)) (match_dup 3)))]
  { operands[2] = GEN_INT (INTVAL (operands[2]) & 0x1f); }
  [(set_attr "type" "bitmanip")])

;; Similarly two patterns for AND generating bclr to
;; manipulate a bit in a register
(define_insn_and_split ""
  [(set (match_operand:DI 0 "register_operand" "=r")
	(and:DI
	  (not:DI
	    (any_extend:DI
	      (ashift:SI
	        (const_int 1)
	        (subreg:QI
		  (and:DI (not:DI (match_operand:DI 1 "register_operand" "r"))
			  (match_operand 2 "const_int_operand")) 0))))
	  (match_operand:DI 3 "register_operand" "r")))
   (clobber (match_scratch:X 4 "=&r"))]
  "TARGET_64BIT
   && TARGET_ZBS
   && (TARGET_ZBB || TARGET_ZBKB)
   && (INTVAL (operands[2]) & 0x1f) != 0x1f"
  "#"
  "&& reload_completed"
   [(set (match_dup 4) (match_dup 2))
    (set (match_dup 4) (and:DI (not:DI (match_dup 1)) (match_dup 4)))
    (set (match_dup 0) (and:DI (rotate:DI (const_int -2) (match_dup 5)) (match_dup 3)))]
{
  operands[5] = gen_lowpart (QImode, operands[4]);
  operands[2] = GEN_INT (INTVAL (operands[2]) & 0x1f);
}
  [(set_attr "type" "bitmanip")])


(define_insn_and_split ""
  [(set (match_operand:DI 0 "register_operand" "=r")
	(and:DI
	  (not:DI
	    (any_extend:DI
	      (ashift:SI
	        (const_int 1)
	        (subreg:QI
		  (and:DI (match_operand:DI 1 "register_operand" "r")
			  (match_operand 2 "const_int_operand")) 0))))
	  (match_operand:DI 3 "register_operand" "r")))
   (clobber (match_scratch:X 4 "=&r"))]
  "TARGET_64BIT
   && TARGET_ZBS
   && (INTVAL (operands[2]) & 0x1f) != 0x1f"
  "#"
  "&& reload_completed"
   [(set (match_dup 4) (and:DI (match_dup 1) (match_dup 2)))
    (set (match_dup 0) (and:DI (rotate:DI (const_int -2) (match_dup 5)) (match_dup 3)))]
{
  operands[5] = gen_lowpart (QImode, operands[4]);
  operands[2] = GEN_INT (INTVAL (operands[2]) & 0x1f);
}
  [(set_attr "type" "bitmanip")])

(define_insn "*bset<mode>_1_mask"
  [(set (match_operand:X 0 "register_operand" "=r")
	(ashift:X (const_int 1)
		  (subreg:QI
		   (and:X (match_operand:X 1 "register_operand" "r")
			  (match_operand 2 "<X:shiftm1>" "<X:shiftm1p>")) 0)))]
  "TARGET_ZBS"
  "bset\t%0,x0,%1"
  [(set_attr "type" "bitmanip")])

(define_insn "*<bit_optab>i<mode>"
  [(set (match_operand:X 0 "register_operand" "=r")
	(any_or:X (match_operand:X 1 "register_operand" "r")
		  (match_operand:X 2 "single_bit_mask_operand" "DbS")))]
  "TARGET_ZBS"
  "<bit_optab>i\t%0,%1,%S2"
  [(set_attr "type" "bitmanip")])

;; We can easily handle zero extensions
(define_split
  [(set (match_operand:DI 0 "register_operand")
    (any_or:DI (zero_extend:DI
		 (ashift:SI (const_int 1)
			    (match_operand:QI 1 "register_operand")))
	       (match_operand:DI 2 "single_bit_mask_operand")))
   (clobber (match_operand:DI 3 "register_operand"))]
  "TARGET_64BIT && TARGET_ZBS"
  [(set (match_dup 3)
        (match_dup 2))
   (set (match_dup 0)
     (any_or:DI (ashift:DI (const_int 1) (match_dup 1))
		(match_dup 3)))])

;; Yet another form of a bset/bclr that can be created by combine.
(define_insn "*bsetclr_zero_extract"
  [(set (zero_extract:X (match_operand:X 0 "register_operand" "+r")
			(const_int 1)
			(zero_extend:X
			  (match_operand:QI 1 "register_operand" "r")))
	(match_operand 2 "immediate_operand" "n"))]
  "TARGET_ZBS
   && (operands[2] == CONST0_RTX (<MODE>mode)
       || operands[2] == CONST1_RTX (<MODE>mode))"
  {
    return (operands[2] == CONST0_RTX (<MODE>mode)
	    ? "bclr\t%0,%0,%1"
	    : "bset\t%0,%0,%1");
  }
  [(set_attr "type" "bitmanip")])

(define_insn "*bclr<mode>"
  [(set (match_operand:X 0 "register_operand" "=r")
	(and:X (rotate:X (const_int -2)
			 (match_operand:QI 2 "register_operand" "r"))
	       (match_operand:X 1 "register_operand" "r")))]
  "TARGET_ZBS"
  "bclr\t%0,%1,%2"
  [(set_attr "type" "bitmanip")])

(define_insn "*bclri<mode>"
  [(set (match_operand:X 0 "register_operand" "=r")
	(and:X (match_operand:X 1 "register_operand" "r")
	       (match_operand:X 2 "not_single_bit_mask_operand" "DnS")))]
  "TARGET_ZBS"
  "bclri\t%0,%1,%T2"
  [(set_attr "type" "bitmanip")])

;; In case we have "val & ~IMM" where ~IMM has 2 bits set.
(define_insn_and_split "*bclri<mode>_nottwobits"
  [(set (match_operand:X 0 "register_operand" "=r")
	(and:X (match_operand:X 1 "register_operand" "r")
	       (match_operand:X 2 "const_nottwobits_not_arith_operand" "i")))]
  "TARGET_ZBS && !paradoxical_subreg_p (operands[1])"
  "#"
  "&& reload_completed"
  [(set (match_dup 0) (and:X (match_dup 1) (match_dup 3)))
   (set (match_dup 0) (and:X (match_dup 0) (match_dup 4)))]
{
	unsigned HOST_WIDE_INT bits = ~UINTVAL (operands[2]);
	unsigned HOST_WIDE_INT topbit = HOST_WIDE_INT_1U << floor_log2 (bits);

	operands[3] = GEN_INT (~bits | topbit);
	operands[4] = GEN_INT (~topbit);
}
[(set_attr "type" "bitmanip")])

;; In case of a paradoxical subreg, the sign bit and the high bits are
;; not allowed to be changed
(define_insn_and_split "*bclridisi_nottwobits"
  [(set (match_operand:DI 0 "register_operand" "=r")
	(and:DI (match_operand:DI 1 "register_operand" "r")
		(match_operand:DI 2 "const_nottwobits_not_arith_operand" "i")))]
  "TARGET_64BIT && TARGET_ZBS
   && clz_hwi (~UINTVAL (operands[2])) > 33"
  "#"
  "&& reload_completed"
  [(set (match_dup 0) (and:DI (match_dup 1) (match_dup 3)))
   (set (match_dup 0) (and:DI (match_dup 0) (match_dup 4)))]
{
	unsigned HOST_WIDE_INT bits = ~UINTVAL (operands[2]);
	unsigned HOST_WIDE_INT topbit = HOST_WIDE_INT_1U << floor_log2 (bits);

	operands[3] = GEN_INT (~bits | topbit);
	operands[4] = GEN_INT (~topbit);
}
[(set_attr "type" "bitmanip")])

;; An outer AND with a constant where bits 31..63 are 0 can be seen as
;; a virtual zero extension from 31 to 64 bits.
(define_split
  [(set (match_operand:DI 0 "register_operand")
    (and:DI (not:DI (subreg:DI
		      (ashift:SI (const_int 1)
				 (match_operand:QI 1 "register_operand")) 0))
            (match_operand:DI 2 "arith_operand")))
   (clobber (match_operand:DI 3 "register_operand"))]
  "TARGET_64BIT && TARGET_ZBS
   && clz_hwi (INTVAL (operands[2])) >= 33"
  [(set (match_dup 3)
        (match_dup 2))
   (set (match_dup 0)
	  (and:DI (rotate:DI (const_int -2) (match_dup 1))
		  (match_dup 3)))])

(define_insn "*bext<mode>"
  [(set (match_operand:X 0 "register_operand" "=r")
	(zero_extract:X (match_operand:X 1 "register_operand" "r")
			(const_int 1)
			(zero_extend:X
			 (match_operand:QI 2 "register_operand" "r"))))]
  "TARGET_ZBS"
  "bext\t%0,%1,%2"
  [(set_attr "type" "bitmanip")])

;; This is a bext followed by a seqz.  Normally this would be a 3->2 split
;; But the and-not pattern with a constant operand is a define_insn_and_split,
;; so this looks like a 2->2 split, which combine rejects.  So implement it
;; as a define_insn_and_split as well.
(define_insn_and_split "*bextseqzdisi"
  [(set (match_operand:DI 0 "register_operand" "=r")
	(and:DI
	  (not:DI
	    (subreg:DI
	      (lshiftrt:SI
		(match_operand:SI 1 "register_operand" "r")
		(match_operand:QI 2 "register_operand" "r")) 0))
	  (const_int 1)))]
  "TARGET_64BIT && TARGET_ZBS"
  "#"
  "&& 1"
  [(set (match_dup 0)
	(zero_extract:DI (match_dup 1)
			 (const_int 1)
			 (zero_extend:DI (match_dup 2))))
   (set (match_dup 0) (eq:DI (match_dup 0) (const_int 0)))]
  "operands[1] = gen_lowpart (word_mode, operands[1]);"
  [(set_attr "type" "bitmanip")])

(define_insn_and_split "*bextseqzdisi"
  [(set (match_operand:X 0 "register_operand" "=r")
	(and:X
	  (not:X
	    (lshiftrt:X
	      (match_operand:X 1 "register_operand" "r")
	      (match_operand:QI 2 "register_operand" "r")))
	  (const_int 1)))]
  "TARGET_ZBS"
  "#"
  "&& 1"
  [(set (match_dup 0)
	(zero_extract:X (match_dup 1)
			(const_int 1)
			(zero_extend:X (match_dup 2))))
   (set (match_dup 0) (eq:X (match_dup 0) (const_int 0)))]
  "operands[1] = gen_lowpart (word_mode, operands[1]);"
  [(set_attr "type" "bitmanip")])

;; The logical-and against 0x1 implicitly extends the result.   So we can treat
;; an SImode bext as-if it's DImode without any explicit extension.
(define_insn "*bextdisi"
  [(set (match_operand:DI 0 "register_operand" "=r")
    (and:DI (subreg:DI (lshiftrt:SI
			 (match_operand:SI 1 "register_operand" "r")
			 (match_operand:QI 2 "register_operand" "r")) 0)
            (const_int 1)))]
  "TARGET_64BIT && TARGET_ZBS"
  "bext\t%0,%1,%2"
  [(set_attr "type" "bitmanip")])

;; When performing `(a & (1UL << bitno)) ? 0 : -1` the combiner
;; usually has the `bitno` typed as X-mode (i.e. no further
;; zero-extension is performed around the bitno).
(define_insn "*bext<mode>"
  [(set (match_operand:X 0 "register_operand" "=r")
	(zero_extract:X (match_operand:X 1 "register_operand" "r")
			(const_int 1)
			(match_operand:X 2 "register_operand" "r")))]
  "TARGET_ZBS"
  "bext\t%0,%1,%2"
  [(set_attr "type" "bitmanip")])

(define_insn "*bexti"
  [(set (match_operand:X 0 "register_operand" "=r")
	(zero_extract:X (match_operand:X 1 "register_operand" "r")
			(const_int 1)
			(match_operand 2 "immediate_operand" "n")))]
  "TARGET_ZBS && UINTVAL (operands[2]) < GET_MODE_BITSIZE (<MODE>mode)"
  "bexti\t%0,%1,%2"
  [(set_attr "type" "bitmanip")])

;; Split for "(a & (1 << BIT_NO)) ? 0 : 1":
;; We avoid reassociating "(~(a >> BIT_NO)) & 1" into "((~a) >> BIT_NO) & 1",
;; so we don't have to use a temporary.  Instead we extract the bit and then
;; invert bit 0 ("a ^ 1") only.
(define_split
  [(set (match_operand:X 0 "register_operand")
	(and:X (not:X (lshiftrt:X (match_operand:X 1 "register_operand")
				  (match_operand:QI 2 "register_operand")))
	       (const_int 1)))]
  "TARGET_ZBS"
  [(set (match_dup 0) (zero_extract:X (match_dup 1)
				      (const_int 1)
				      (match_dup 2)))
   (set (match_dup 0) (xor:X (match_dup 0) (const_int 1)))]
  "operands[2] = gen_lowpart (<MODE>mode, operands[2]);")

;; We can create a polarity-reversed mask (i.e. bit N -> { set = 0, clear = -1 })
;; using a bext(i) followed by an addi instruction.
;; This splits the canonical representation of "(a & (1 << BIT_NO)) ? 0 : -1".
(define_split
  [(set (match_operand:GPR 0 "register_operand")
       (neg:GPR (eq:GPR (zero_extract:GPR (match_operand:GPR 1 "register_operand")
                                          (const_int 1)
                                          (match_operand 2))
                        (const_int 0))))]
  "TARGET_ZBS"
  [(set (match_dup 0) (zero_extract:GPR (match_dup 1) (const_int 1) (match_dup 2)))
   (set (match_dup 0) (plus:GPR (match_dup 0) (const_int -1)))])

;; Catch those cases where we can use a bseti/binvi + ori/xori or
;; bseti/binvi + bseti/binvi instead of a lui + addi + or/xor sequence.
(define_insn_and_split "*<or_optab>i<mode>_extrabit"
  [(set (match_operand:X 0 "register_operand" "=r")
	(any_or:X (match_operand:X 1 "register_operand" "r")
	          (match_operand:X 2 "uimm_extra_bit_or_twobits" "i")))]
  "TARGET_ZBS && !single_bit_mask_operand (operands[2], VOIDmode)"
  "#"
  "&& reload_completed"
  [(set (match_dup 0) (<or_optab>:X (match_dup 1) (match_dup 3)))
   (set (match_dup 0) (<or_optab>:X (match_dup 0) (match_dup 4)))]
{
  unsigned HOST_WIDE_INT bits = UINTVAL (operands[2]);
  unsigned HOST_WIDE_INT topbit = HOST_WIDE_INT_1U << floor_log2 (bits);

  operands[3] = GEN_INT (bits &~ topbit);
  operands[4] = GEN_INT (topbit);
}
[(set_attr "type" "bitmanip")])

;; Same to use blcri + andi and blcri + bclri
(define_insn_and_split "*andi<mode>_extrabit"
  [(set (match_operand:X 0 "register_operand" "=r")
	(and:X (match_operand:X 1 "register_operand" "r")
	       (match_operand:X 2 "not_uimm_extra_bit_or_nottwobits" "i")))]
  "TARGET_ZBS && !not_single_bit_mask_operand (operands[2], VOIDmode)"
  "#"
  "&& reload_completed"
  [(set (match_dup 0) (and:X (match_dup 1) (match_dup 3)))
   (set (match_dup 0) (and:X (match_dup 0) (match_dup 4)))]
{
  unsigned HOST_WIDE_INT bits = UINTVAL (operands[2]);
  unsigned HOST_WIDE_INT topbit = HOST_WIDE_INT_1U << floor_log2 (~bits);

  operands[3] = GEN_INT (bits | topbit);
  operands[4] = GEN_INT (~topbit);
}
[(set_attr "type" "bitmanip")])

;; If we have the ZBA extension, then we can clear the upper half of a 64
;; bit object with a zext.w.  So if we have AND where the constant would
;; require synthesis of two or more instructions, but 32->64 sign extension
;; of the constant is a simm12, then we can use zext.w+andi.  If the adjusted
;; constant is a single bit constant, then we can use zext.w+bclri
;;
;; With the mvconst_internal pattern claiming a single insn to synthesize
;; constants, this must be a define_insn_and_split.
(define_insn_and_split ""
  [(set (match_operand:DI 0 "register_operand" "=r")
	(and:DI (match_operand:DI 1 "register_operand" "r")
		(match_operand 2 "const_int_operand" "n")))]
  "TARGET_64BIT
   && TARGET_ZBA
   && !paradoxical_subreg_p (operands[1])
   /* Only profitable if synthesis takes more than one insn.  */
   && riscv_const_insns (operands[2], false) != 1
   /* We need the upper half to be zero.  */
   && (INTVAL (operands[2]) & HOST_WIDE_INT_C (0xffffffff00000000)) == 0
   /* And the the adjusted constant must either be something we can
      implement with andi or bclri.  */
   && ((SMALL_OPERAND (sext_hwi (INTVAL (operands[2]), 32))
        || (TARGET_ZBS && popcount_hwi (INTVAL (operands[2])) == 31))
       && INTVAL (operands[2]) != 0x7fffffff)"
  "#"
  "&& 1"
  [(set (match_dup 0) (zero_extend:DI (match_dup 3)))
   (set (match_dup 0) (and:DI (match_dup 0) (match_dup 2)))]
  "{
     operands[3] = gen_lowpart (SImode, operands[1]);
     operands[2] = GEN_INT (sext_hwi (INTVAL (operands[2]), 32));
   }"
  [(set_attr "type" "bitmanip")])

;; IF_THEN_ELSE: test for 2 bits of opposite polarity
(define_insn_and_split "*branch<X:mode>_mask_twobits_equals_singlebit"
  [(set (pc)
	(if_then_else
	  (match_operator 1 "equality_operator"
	    [(and:X (match_operand:X 2 "register_operand" "r")
		    (match_operand:X 3 "const_twobits_not_arith_operand" "i"))
	     (match_operand:X 4 "single_bit_mask_operand" "i")])
	 (label_ref (match_operand 0 "" ""))
	 (pc)))
   (clobber (match_scratch:X 5 "=&r"))
   (clobber (match_scratch:X 6 "=&r"))]
  "TARGET_ZBS && TARGET_ZBB"
  "#"
  "&& reload_completed"
  [(set (match_dup 5) (zero_extract:X (match_dup 2)
				      (const_int 1)
				      (match_dup 8)))
   (set (match_dup 6) (zero_extract:X (match_dup 2)
				      (const_int 1)
				      (match_dup 9)))
   (set (match_dup 6) (and:X (not:X (match_dup 6)) (match_dup 5)))
   (set (pc) (if_then_else (match_op_dup 1 [(match_dup 6) (const_int 0)])
			   (label_ref (match_dup 0))
			   (pc)))]
{
   unsigned HOST_WIDE_INT twobits_mask = UINTVAL (operands[3]);
   unsigned HOST_WIDE_INT singlebit_mask = UINTVAL (operands[4]);

   /* We should never see an unsatisfiable condition.  */
   gcc_assert (twobits_mask & singlebit_mask);

   int setbit = ctz_hwi (singlebit_mask);
   int clearbit = ctz_hwi (twobits_mask & ~singlebit_mask);

   operands[1] = gen_rtx_fmt_ee (GET_CODE (operands[1]) == NE ? EQ : NE,
				 <X:MODE>mode, operands[6], GEN_INT(0));

   operands[8] = GEN_INT (setbit);
   operands[9] = GEN_INT (clearbit);
}
[(set_attr "type" "bitmanip")])

;; IF_THEN_ELSE: test for (a & (1 << BIT_NO))
(define_insn_and_split "*branch<X:mode>_bext"
  [(set (pc)
	(if_then_else
	  (match_operator 1 "equality_operator"
         [(zero_extract:X (match_operand:X 2 "register_operand" "r")
                          (const_int 1)
                          (zero_extend:X
                            (match_operand:QI 3 "register_operand" "r")))
	    (const_int 0)])
	(label_ref (match_operand 0 "" ""))
	(pc)))
  (clobber (match_scratch:X 4 "=&r"))]
  "TARGET_ZBS"
  "#"
  "&& reload_completed"
  [(set (match_dup 4) (zero_extract:X (match_dup 2)
					(const_int 1)
					(zero_extend:X (match_dup 3))))
   (set (pc) (if_then_else (match_op_dup 1 [(match_dup 4) (const_int 0)])
			   (label_ref (match_dup 0))
			   (pc)))]
   ""
  [(set_attr "type" "bitmanip")])

;; ZBKC or ZBC extension
(define_insn "riscv_clmul_<mode>"
  [(set (match_operand:GPR 0 "register_operand" "=r")
        (unspec:GPR [(match_operand:GPR 1 "register_operand" "r")
                  (match_operand:GPR 2 "register_operand" "r")]
                  UNSPEC_CLMUL))]
  "TARGET_ZBKC || TARGET_ZBC"
  "clmul\t%0,%1,%2"
  [(set_attr "type" "clmul")])

(define_insn "riscv_clmulh_<mode>"
  [(set (match_operand:X 0 "register_operand" "=r")
        (unspec:X [(match_operand:X 1 "register_operand" "r")
                  (match_operand:X 2 "register_operand" "r")]
                  UNSPEC_CLMULH))]
  "TARGET_ZBKC || TARGET_ZBC"
  "clmulh\t%0,%1,%2"
  [(set_attr "type" "clmul")])

;; ZBC extension
(define_insn "riscv_clmulr_<mode>"
  [(set (match_operand:X 0 "register_operand" "=r")
        (unspec:X [(match_operand:X 1 "register_operand" "r")
                  (match_operand:X 2 "register_operand" "r")]
                  UNSPEC_CLMULR))]
  "TARGET_ZBC"
  "clmulr\t%0,%1,%2"
  [(set_attr "type" "clmul")])

;; Reversed CRC 8, 16, 32 for TARGET_64
(define_expand "crc_rev<ANYI1:mode><ANYI:mode>4"
	;; return value (calculated CRC)
  [(set (match_operand:ANYI 0 "register_operand" "=r")
		      ;; initial CRC
	(unspec:ANYI [(match_operand:ANYI 1 "register_operand" "r")
		      ;; data
		      (match_operand:ANYI1 2 "register_operand" "r")
		      ;; polynomial without leading 1
		      (match_operand:ANYI 3)]
		      UNSPEC_CRC_REV))]
  /* We don't support the case when data's size is bigger than CRC's size.  */
  "<ANYI:MODE>mode >= <ANYI1:MODE>mode"
{
  /* If we have the ZBC or ZBKC extension (ie, clmul) and
     it is possible to store the quotient within a single variable
     (E.g.  CRC64's quotient may need 65 bits,
     we can't keep it in 64 bit variable.)
     then use clmul instruction to implement the CRC,
     otherwise (TARGET_ZBKB) generate table based using brev.  */
  if ((TARGET_ZBKC || TARGET_ZBC) && <ANYI:MODE>mode < word_mode)
    expand_reversed_crc_using_clmul (<ANYI:MODE>mode, <ANYI1:MODE>mode,
				     operands);
  else if (TARGET_ZBKB)
    /* Generate table-based CRC.
       To reflect values use brev and bswap instructions.  */
    expand_reversed_crc_table_based (operands[0], operands[1],
				     operands[2], operands[3],
				     GET_MODE (operands[2]),
				     generate_reflecting_code_using_brev);
  else
    /* Generate table-based CRC.
       To reflect values use standard reflecting algorithm.  */
    expand_reversed_crc_table_based (operands[0], operands[1],
				     operands[2], operands[3],
				     GET_MODE (operands[2]),
				     generate_reflecting_code_standard);
  DONE;
})

;; CRC 8, 16, (32 for TARGET_64)
(define_expand "crc<SUBX1:mode><SUBX:mode>4"
	;; return value (calculated CRC)
  [(set (match_operand:SUBX 0 "register_operand" "=r")
		      ;; initial CRC
	(unspec:SUBX [(match_operand:SUBX 1 "register_operand" "r")
		      ;; data
		      (match_operand:SUBX1 2 "register_operand" "r")
		      ;; polynomial without leading 1
		      (match_operand:SUBX 3)]
		      UNSPEC_CRC))]
  /* We don't support the case when data's size is bigger than CRC's size.  */
  "(TARGET_ZBKC || TARGET_ZBC) && <SUBX:MODE>mode >= <SUBX1:MODE>mode"
{
  /* If we have the ZBC or ZBKC extension (ie, clmul) and
     it is possible to store the quotient within a single variable
     (E.g.  CRC64's quotient may need 65 bits,
     we can't keep it in 64 bit variable.)
     then use clmul instruction to implement the CRC.  */
  expand_crc_using_clmul (<SUBX:MODE>mode, <SUBX1:MODE>mode, operands);
  DONE;
})
