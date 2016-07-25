;; Machine description for AArch64 architecture.
;; Copyright (C) 2009-2016 Free Software Foundation, Inc.
;; Contributed by ARM Ltd.
;;
;; This file is part of GCC.
;;
;; GCC is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; GCC is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GCC; see the file COPYING3.  If not see
;; <http://www.gnu.org/licenses/>.

;; Register numbers
(define_constants
  [
    (R0_REGNUM		0)
    (R1_REGNUM		1)
    (R2_REGNUM		2)
    (R3_REGNUM		3)
    (R4_REGNUM		4)
    (R5_REGNUM		5)
    (R6_REGNUM		6)
    (R7_REGNUM		7)
    (R8_REGNUM		8)
    (R9_REGNUM		9)
    (R10_REGNUM		10)
    (R11_REGNUM		11)
    (R12_REGNUM		12)
    (R13_REGNUM		13)
    (R14_REGNUM		14)
    (R15_REGNUM		15)
    (R16_REGNUM		16)
    (IP0_REGNUM		16)
    (R17_REGNUM		17)
    (IP1_REGNUM		17)
    (R18_REGNUM		18)
    (R19_REGNUM		19)
    (R20_REGNUM		20)
    (R21_REGNUM		21)
    (R22_REGNUM		22)
    (R23_REGNUM		23)
    (R24_REGNUM		24)
    (R25_REGNUM		25)
    (R26_REGNUM		26)
    (R27_REGNUM		27)
    (R28_REGNUM		28)
    (R29_REGNUM		29)
    (R30_REGNUM		30)
    (LR_REGNUM		30)
    (SP_REGNUM		31)
    (V0_REGNUM		32)
    (V15_REGNUM		47)
    (V31_REGNUM		63)
    (SFP_REGNUM		64)
    (AP_REGNUM		65)
    (CC_REGNUM		66)
  ]
)

(define_c_enum "unspec" [
    UNSPEC_CASESI
    UNSPEC_CRC32B
    UNSPEC_CRC32CB
    UNSPEC_CRC32CH
    UNSPEC_CRC32CW
    UNSPEC_CRC32CX
    UNSPEC_CRC32H
    UNSPEC_CRC32W
    UNSPEC_CRC32X
    UNSPEC_FCVTZS
    UNSPEC_FCVTZU
    UNSPEC_URECPE
    UNSPEC_FRECPE
    UNSPEC_FRECPS
    UNSPEC_FRECPX
    UNSPEC_FRINTA
    UNSPEC_FRINTI
    UNSPEC_FRINTM
    UNSPEC_FRINTN
    UNSPEC_FRINTP
    UNSPEC_FRINTX
    UNSPEC_FRINTZ
    UNSPEC_GOTSMALLPIC
    UNSPEC_GOTSMALLPIC28K
    UNSPEC_GOTSMALLTLS
    UNSPEC_GOTTINYPIC
    UNSPEC_GOTTINYTLS
    UNSPEC_LD1
    UNSPEC_LD2
    UNSPEC_LD2_DUP
    UNSPEC_LD3
    UNSPEC_LD3_DUP
    UNSPEC_LD4
    UNSPEC_LD4_DUP
    UNSPEC_LD2_LANE
    UNSPEC_LD3_LANE
    UNSPEC_LD4_LANE
    UNSPEC_MB
    UNSPEC_NOP
    UNSPEC_PRLG_STK
    UNSPEC_RBIT
    UNSPEC_SCVTF
    UNSPEC_SISD_NEG
    UNSPEC_SISD_SSHL
    UNSPEC_SISD_USHL
    UNSPEC_SSHL_2S
    UNSPEC_ST1
    UNSPEC_ST2
    UNSPEC_ST3
    UNSPEC_ST4
    UNSPEC_ST2_LANE
    UNSPEC_ST3_LANE
    UNSPEC_ST4_LANE
    UNSPEC_TLS
    UNSPEC_TLSDESC
    UNSPEC_TLSLE12
    UNSPEC_TLSLE24
    UNSPEC_TLSLE32
    UNSPEC_TLSLE48
    UNSPEC_UCVTF
    UNSPEC_USHL_2S
    UNSPEC_VSTRUCTDUMMY
    UNSPEC_SP_SET
    UNSPEC_SP_TEST
    UNSPEC_RSQRT
    UNSPEC_RSQRTE
    UNSPEC_RSQRTS
    UNSPEC_NZCV
])

(define_c_enum "unspecv" [
    UNSPECV_EH_RETURN		; Represent EH_RETURN
    UNSPECV_GET_FPCR		; Represent fetch of FPCR content.
    UNSPECV_SET_FPCR		; Represent assign of FPCR content.
    UNSPECV_GET_FPSR		; Represent fetch of FPSR content.
    UNSPECV_SET_FPSR		; Represent assign of FPSR content.
    UNSPECV_BLOCKAGE		; Represent a blockage
    UNSPECV_PROBE_STACK_RANGE	; Represent stack range probing.
  ]
)

;; If further include files are added the defintion of MD_INCLUDES
;; must be updated.

(include "constraints.md")
(include "predicates.md")
(include "iterators.md")

;; -------------------------------------------------------------------
;; Instruction types and attributes
;; -------------------------------------------------------------------

; The "type" attribute is included here from AArch32 backend to be able
; to share pipeline descriptions.
(include "../arm/types.md")

;; It is important to set the fp or simd attributes to yes when a pattern
;; alternative uses the FP or SIMD register files, usually signified by use of
;; the 'w' constraint.  This will ensure that the alternative will be
;; disabled when compiling with -mgeneral-regs-only or with the +nofp/+nosimd
;; architecture extensions.  If all the alternatives in a pattern use the
;; FP or SIMD registers then the pattern predicate should include TARGET_FLOAT
;; or TARGET_SIMD.

;; Attribute that specifies whether or not the instruction touches fp
;; registers.  When this is set to yes for an alternative, that alternative
;; will be disabled when !TARGET_FLOAT.
(define_attr "fp" "no,yes" (const_string "no"))

;; Attribute that specifies whether or not the instruction touches simd
;; registers.  When this is set to yes for an alternative, that alternative
;; will be disabled when !TARGET_SIMD.
(define_attr "simd" "no,yes" (const_string "no"))

(define_attr "length" ""
  (const_int 4))

;; Attribute that controls whether an alternative is enabled or not.
;; Currently it is only used to disable alternatives which touch fp or simd
;; registers when -mgeneral-regs-only is specified.
(define_attr "enabled" "no,yes"
  (cond [(ior
	(and (eq_attr "fp" "yes")
	     (eq (symbol_ref "TARGET_FLOAT") (const_int 0)))
	(and (eq_attr "simd" "yes")
	     (eq (symbol_ref "TARGET_SIMD") (const_int 0))))
	     (const_string "no")
	] (const_string "yes")))

;; Attribute that specifies whether we are dealing with a branch to a
;; label that is far away, i.e. further away than the maximum/minimum
;; representable in a signed 21-bits number.
;; 0 :=: no
;; 1 :=: yes
(define_attr "far_branch" "" (const_int 0))

;; Strictly for compatibility with AArch32 in pipeline models, since AArch64 has
;; no predicated insns.
(define_attr "predicated" "yes,no" (const_string "no"))

;; -------------------------------------------------------------------
;; Pipeline descriptions and scheduling
;; -------------------------------------------------------------------

;; Processor types.
(include "aarch64-tune.md")

;; Scheduling
(include "../arm/cortex-a53.md")
(include "../arm/cortex-a57.md")
(include "../arm/exynos-m1.md")
(include "thunderx.md")
(include "../arm/xgene1.md")

;; -------------------------------------------------------------------
;; Jumps and other miscellaneous insns
;; -------------------------------------------------------------------

(define_insn "indirect_jump"
  [(set (pc) (match_operand:DI 0 "register_operand" "r"))]
  ""
  "br\\t%0"
  [(set_attr "type" "branch")]
)

(define_insn "jump"
  [(set (pc) (label_ref (match_operand 0 "" "")))]
  ""
  "b\\t%l0"
  [(set_attr "type" "branch")]
)

(define_expand "cbranch<mode>4"
  [(set (pc) (if_then_else (match_operator 0 "aarch64_comparison_operator"
			    [(match_operand:GPI 1 "register_operand" "")
			     (match_operand:GPI 2 "aarch64_plus_operand" "")])
			   (label_ref (match_operand 3 "" ""))
			   (pc)))]
  ""
  "
  operands[1] = aarch64_gen_compare_reg (GET_CODE (operands[0]), operands[1],
					 operands[2]);
  operands[2] = const0_rtx;
  "
)

(define_expand "cbranch<mode>4"
  [(set (pc) (if_then_else (match_operator 0 "aarch64_comparison_operator"
			    [(match_operand:GPF 1 "register_operand" "")
			     (match_operand:GPF 2 "aarch64_fp_compare_operand" "")])
			   (label_ref (match_operand 3 "" ""))
			   (pc)))]
  ""
  "
  operands[1] = aarch64_gen_compare_reg (GET_CODE (operands[0]), operands[1],
					 operands[2]);
  operands[2] = const0_rtx;
  "
)

(define_expand "cbranchcc4"
  [(set (pc) (if_then_else
	      (match_operator 0 "aarch64_comparison_operator"
	       [(match_operand 1 "cc_register" "")
	        (match_operand 2 "const0_operand")])
	      (label_ref (match_operand 3 "" ""))
	      (pc)))]
  ""
  "")

(define_insn "ccmp<mode>"
  [(set (match_operand:CC 1 "cc_register" "")
	(if_then_else:CC
	  (match_operator 4 "aarch64_comparison_operator"
	   [(match_operand 0 "cc_register" "")
	    (const_int 0)])
	  (compare:CC
	    (match_operand:GPI 2 "register_operand" "r,r,r")
	    (match_operand:GPI 3 "aarch64_ccmp_operand" "r,Uss,Usn"))
	  (unspec:CC [(match_operand 5 "immediate_operand")] UNSPEC_NZCV)))]
  ""
  "@
   ccmp\\t%<w>2, %<w>3, %k5, %m4
   ccmp\\t%<w>2, %3, %k5, %m4
   ccmn\\t%<w>2, #%n3, %k5, %m4"
  [(set_attr "type" "alus_sreg,alus_imm,alus_imm")]
)

(define_insn "fccmp<mode>"
  [(set (match_operand:CCFP 1 "cc_register" "")
	(if_then_else:CCFP
	  (match_operator 4 "aarch64_comparison_operator"
	   [(match_operand 0 "cc_register" "")
	    (const_int 0)])
	  (compare:CCFP
	    (match_operand:GPF 2 "register_operand" "w")
	    (match_operand:GPF 3 "register_operand" "w"))
	  (unspec:CCFP [(match_operand 5 "immediate_operand")] UNSPEC_NZCV)))]
  "TARGET_FLOAT"
  "fccmp\\t%<s>2, %<s>3, %k5, %m4"
  [(set_attr "type" "fccmp<s>")]
)

(define_insn "fccmpe<mode>"
  [(set (match_operand:CCFPE 1 "cc_register" "")
	 (if_then_else:CCFPE
	  (match_operator 4 "aarch64_comparison_operator"
	   [(match_operand 0 "cc_register" "")
	  (const_int 0)])
	   (compare:CCFPE
	    (match_operand:GPF 2 "register_operand" "w")
	    (match_operand:GPF 3 "register_operand" "w"))
	  (unspec:CCFPE [(match_operand 5 "immediate_operand")] UNSPEC_NZCV)))]
  "TARGET_FLOAT"
  "fccmpe\\t%<s>2, %<s>3, %k5, %m4"
  [(set_attr "type" "fccmp<s>")]
)

;; Expansion of signed mod by a power of 2 using CSNEG.
;; For x0 % n where n is a power of 2 produce:
;; negs   x1, x0
;; and    x0, x0, #(n - 1)
;; and    x1, x1, #(n - 1)
;; csneg  x0, x0, x1, mi

(define_expand "mod<mode>3"
  [(match_operand:GPI 0 "register_operand" "")
   (match_operand:GPI 1 "register_operand" "")
   (match_operand:GPI 2 "const_int_operand" "")]
  ""
  {
    HOST_WIDE_INT val = INTVAL (operands[2]);

    if (val <= 0
       || exact_log2 (val) <= 0
       || !aarch64_bitmask_imm (val - 1, <MODE>mode))
      FAIL;

    rtx mask = GEN_INT (val - 1);

    /* In the special case of x0 % 2 we can do the even shorter:
	cmp    x0, xzr
	and    x0, x0, 1
	cneg   x0, x0, lt.  */
    if (val == 2)
      {
	rtx masked = gen_reg_rtx (<MODE>mode);
	rtx ccreg = aarch64_gen_compare_reg (LT, operands[1], const0_rtx);
	emit_insn (gen_and<mode>3 (masked, operands[1], mask));
	rtx x = gen_rtx_LT (VOIDmode, ccreg, const0_rtx);
	emit_insn (gen_csneg3<mode>_insn (operands[0], x, masked, masked));
	DONE;
      }

    rtx neg_op = gen_reg_rtx (<MODE>mode);
    rtx_insn *insn = emit_insn (gen_neg<mode>2_compare0 (neg_op, operands[1]));

    /* Extract the condition register and mode.  */
    rtx cmp = XVECEXP (PATTERN (insn), 0, 0);
    rtx cc_reg = SET_DEST (cmp);
    rtx cond = gen_rtx_GE (VOIDmode, cc_reg, const0_rtx);

    rtx masked_pos = gen_reg_rtx (<MODE>mode);
    emit_insn (gen_and<mode>3 (masked_pos, operands[1], mask));

    rtx masked_neg = gen_reg_rtx (<MODE>mode);
    emit_insn (gen_and<mode>3 (masked_neg, neg_op, mask));

    emit_insn (gen_csneg3<mode>_insn (operands[0], cond,
				       masked_neg, masked_pos));
    DONE;
  }
)

(define_insn "condjump"
  [(set (pc) (if_then_else (match_operator 0 "aarch64_comparison_operator"
			    [(match_operand 1 "cc_register" "") (const_int 0)])
			   (label_ref (match_operand 2 "" ""))
			   (pc)))]
  ""
  {
    if (get_attr_length (insn) == 8)
      return aarch64_gen_far_branch (operands, 2, "Lbcond", "b%M0\\t");
    else
      return  "b%m0\\t%l2";
  }
  [(set_attr "type" "branch")
   (set (attr "length")
	(if_then_else (and (ge (minus (match_dup 2) (pc)) (const_int -1048576))
			   (lt (minus (match_dup 2) (pc)) (const_int 1048572)))
		      (const_int 4)
		      (const_int 8)))
   (set (attr "far_branch")
	(if_then_else (and (ge (minus (match_dup 2) (pc)) (const_int -1048576))
			   (lt (minus (match_dup 2) (pc)) (const_int 1048572)))
		      (const_int 0)
		      (const_int 1)))]
)

;; For a 24-bit immediate CST we can optimize the compare for equality
;; and branch sequence from:
;; 	mov	x0, #imm1
;; 	movk	x0, #imm2, lsl 16 /* x0 contains CST.  */
;; 	cmp	x1, x0
;; 	b<ne,eq> .Label
;; into the shorter:
;; 	sub	x0, x1, #(CST & 0xfff000)
;; 	subs	x0, x0, #(CST & 0x000fff)
;; 	b<ne,eq> .Label
(define_insn_and_split "*compare_condjump<mode>"
  [(set (pc) (if_then_else (EQL
			      (match_operand:GPI 0 "register_operand" "r")
			      (match_operand:GPI 1 "aarch64_imm24" "n"))
			   (label_ref:P (match_operand 2 "" ""))
			   (pc)))]
  "!aarch64_move_imm (INTVAL (operands[1]), <MODE>mode)
   && !aarch64_plus_operand (operands[1], <MODE>mode)
   && !reload_completed"
  "#"
  "&& true"
  [(const_int 0)]
  {
    HOST_WIDE_INT lo_imm = UINTVAL (operands[1]) & 0xfff;
    HOST_WIDE_INT hi_imm = UINTVAL (operands[1]) & 0xfff000;
    rtx tmp = gen_reg_rtx (<MODE>mode);
    emit_insn (gen_add<mode>3 (tmp, operands[0], GEN_INT (-hi_imm)));
    emit_insn (gen_add<mode>3_compare0 (tmp, tmp, GEN_INT (-lo_imm)));
    rtx cc_reg = gen_rtx_REG (CC_NZmode, CC_REGNUM);
    rtx cmp_rtx = gen_rtx_fmt_ee (<EQL:CMP>, <MODE>mode, cc_reg, const0_rtx);
    emit_jump_insn (gen_condjump (cmp_rtx, cc_reg, operands[2]));
    DONE;
  }
)

(define_expand "casesi"
  [(match_operand:SI 0 "register_operand" "")	; Index
   (match_operand:SI 1 "const_int_operand" "")	; Lower bound
   (match_operand:SI 2 "const_int_operand" "")	; Total range
   (match_operand:DI 3 "" "")			; Table label
   (match_operand:DI 4 "" "")]			; Out of range label
  ""
  {
    if (operands[1] != const0_rtx)
      {
	rtx reg = gen_reg_rtx (SImode);

	/* Canonical RTL says that if you have:

	   (minus (X) (CONST))

           then this should be emitted as:

           (plus (X) (-CONST))

	   The use of trunc_int_for_mode ensures that the resulting
	   constant can be represented in SImode, this is important
	   for the corner case where operand[1] is INT_MIN.  */

	operands[1] = GEN_INT (trunc_int_for_mode (-INTVAL (operands[1]), SImode));

	if (!(*insn_data[CODE_FOR_addsi3].operand[2].predicate)
	      (operands[1], SImode))
	  operands[1] = force_reg (SImode, operands[1]);
	emit_insn (gen_addsi3 (reg, operands[0], operands[1]));
	operands[0] = reg;
      }

    if (!aarch64_plus_operand (operands[2], SImode))
      operands[2] = force_reg (SImode, operands[2]);
    emit_jump_insn (gen_cbranchsi4 (gen_rtx_GTU (SImode, const0_rtx,
						 const0_rtx),
				    operands[0], operands[2], operands[4]));

    operands[2] = force_reg (DImode, gen_rtx_LABEL_REF (VOIDmode, operands[3]));
    emit_jump_insn (gen_casesi_dispatch (operands[2], operands[0],
					 operands[3]));
    DONE;
  }
)

(define_insn "casesi_dispatch"
  [(parallel
    [(set (pc)
	  (mem:DI (unspec [(match_operand:DI 0 "register_operand" "r")
			   (match_operand:SI 1 "register_operand" "r")]
			UNSPEC_CASESI)))
     (clobber (reg:CC CC_REGNUM))
     (clobber (match_scratch:DI 3 "=r"))
     (clobber (match_scratch:DI 4 "=r"))
     (use (label_ref (match_operand 2 "" "")))])]
  ""
  "*
  return aarch64_output_casesi (operands);
  "
  [(set_attr "length" "16")
   (set_attr "type" "branch")]
)

(define_insn "nop"
  [(unspec[(const_int 0)] UNSPEC_NOP)]
  ""
  "nop"
  [(set_attr "type" "no_insn")]
)

(define_insn "prefetch"
  [(prefetch (match_operand:DI 0 "register_operand" "r")
            (match_operand:QI 1 "const_int_operand" "")
            (match_operand:QI 2 "const_int_operand" ""))]
  ""
  {
    const char * pftype[2][4] = 
    {
      {"prfm\\tPLDL1STRM, %a0",
       "prfm\\tPLDL3KEEP, %a0",
       "prfm\\tPLDL2KEEP, %a0",
       "prfm\\tPLDL1KEEP, %a0"},
      {"prfm\\tPSTL1STRM, %a0",
       "prfm\\tPSTL3KEEP, %a0",
       "prfm\\tPSTL2KEEP, %a0",
       "prfm\\tPSTL1KEEP, %a0"},
    };

    int locality = INTVAL (operands[2]);

    gcc_assert (IN_RANGE (locality, 0, 3));

    return pftype[INTVAL(operands[1])][locality];
  }
  [(set_attr "type" "load1")]
)

(define_insn "trap"
  [(trap_if (const_int 1) (const_int 8))]
  ""
  "brk #1000"
  [(set_attr "type" "trap")])

(define_expand "prologue"
  [(clobber (const_int 0))]
  ""
  "
  aarch64_expand_prologue ();
  DONE;
  "
)

(define_expand "epilogue"
  [(clobber (const_int 0))]
  ""
  "
  aarch64_expand_epilogue (false);
  DONE;
  "
)

(define_expand "sibcall_epilogue"
  [(clobber (const_int 0))]
  ""
  "
  aarch64_expand_epilogue (true);
  DONE;
  "
)

(define_insn "*do_return"
  [(return)]
  ""
  "ret"
  [(set_attr "type" "branch")]
)

(define_expand "return"
  [(simple_return)]
  "aarch64_use_return_insn_p ()"
  ""
)

(define_insn "simple_return"
  [(simple_return)]
  ""
  "ret"
  [(set_attr "type" "branch")]
)

(define_insn "eh_return"
  [(unspec_volatile [(match_operand:DI 0 "register_operand" "r")]
    UNSPECV_EH_RETURN)]
  ""
  "#"
  [(set_attr "type" "branch")]

)

(define_split
  [(unspec_volatile [(match_operand:DI 0 "register_operand" "")]
    UNSPECV_EH_RETURN)]
  "reload_completed"
  [(set (match_dup 1) (match_dup 0))]
  {
    operands[1] = aarch64_final_eh_return_addr ();
  }
)

(define_insn "*cb<optab><mode>1"
  [(set (pc) (if_then_else (EQL (match_operand:GPI 0 "register_operand" "r")
				(const_int 0))
			   (label_ref (match_operand 1 "" ""))
			   (pc)))]
  ""
  {
    if (get_attr_length (insn) == 8)
      return aarch64_gen_far_branch (operands, 1, "Lcb", "<inv_cb>\\t%<w>0, ");
    else
      return "<cbz>\\t%<w>0, %l1";
  }
  [(set_attr "type" "branch")
   (set (attr "length")
	(if_then_else (and (ge (minus (match_dup 1) (pc)) (const_int -1048576))
			   (lt (minus (match_dup 1) (pc)) (const_int 1048572)))
		      (const_int 4)
		      (const_int 8)))
   (set (attr "far_branch")
	(if_then_else (and (ge (minus (match_dup 2) (pc)) (const_int -1048576))
			   (lt (minus (match_dup 2) (pc)) (const_int 1048572)))
		      (const_int 0)
		      (const_int 1)))]
)

(define_insn "*tb<optab><mode>1"
  [(set (pc) (if_then_else
	      (EQL (zero_extract:DI (match_operand:GPI 0 "register_operand" "r")
				    (const_int 1)
				    (match_operand 1 "const_int_operand" "n"))
		   (const_int 0))
	     (label_ref (match_operand 2 "" ""))
	     (pc)))
   (clobber (reg:CC CC_REGNUM))]
  ""
  {
    if (get_attr_length (insn) == 8)
      {
	if (get_attr_far_branch (insn) == 1)
	  return aarch64_gen_far_branch (operands, 2, "Ltb",
					 "<inv_tb>\\t%<w>0, %1, ");
	else
	  {
	    operands[1] = GEN_INT (HOST_WIDE_INT_1U << UINTVAL (operands[1]));
	    return "tst\t%<w>0, %1\;<bcond>\t%l2";
	  }
      }
    else
      return "<tbz>\t%<w>0, %1, %l2";
  }
  [(set_attr "type" "branch")
   (set (attr "length")
	(if_then_else (and (ge (minus (match_dup 2) (pc)) (const_int -32768))
			   (lt (minus (match_dup 2) (pc)) (const_int 32764)))
		      (const_int 4)
		      (const_int 8)))
   (set (attr "far_branch")
	(if_then_else (and (ge (minus (match_dup 2) (pc)) (const_int -1048576))
			   (lt (minus (match_dup 2) (pc)) (const_int 1048572)))
		      (const_int 0)
		      (const_int 1)))]

)

(define_insn "*cb<optab><mode>1"
  [(set (pc) (if_then_else (LTGE (match_operand:ALLI 0 "register_operand" "r")
				 (const_int 0))
			   (label_ref (match_operand 1 "" ""))
			   (pc)))
   (clobber (reg:CC CC_REGNUM))]
  ""
  {
    if (get_attr_length (insn) == 8)
      {
	if (get_attr_far_branch (insn) == 1)
	  return aarch64_gen_far_branch (operands, 1, "Ltb",
					 "<inv_tb>\\t%<w>0, <sizem1>, ");
	else
	  {
	    char buf[64];
	    uint64_t val = ((uint64_t) 1)
		<< (GET_MODE_SIZE (<MODE>mode) * BITS_PER_UNIT - 1);
	    sprintf (buf, "tst\t%%<w>0, %" PRId64, val);
	    output_asm_insn (buf, operands);
	    return "<bcond>\t%l1";
	  }
      }
    else
      return "<tbz>\t%<w>0, <sizem1>, %l1";
  }
  [(set_attr "type" "branch")
   (set (attr "length")
	(if_then_else (and (ge (minus (match_dup 1) (pc)) (const_int -32768))
			   (lt (minus (match_dup 1) (pc)) (const_int 32764)))
		      (const_int 4)
		      (const_int 8)))
   (set (attr "far_branch")
	(if_then_else (and (ge (minus (match_dup 1) (pc)) (const_int -1048576))
			   (lt (minus (match_dup 1) (pc)) (const_int 1048572)))
		      (const_int 0)
		      (const_int 1)))]
)

;; -------------------------------------------------------------------
;; Subroutine calls and sibcalls
;; -------------------------------------------------------------------

(define_expand "call_internal"
  [(parallel [(call (match_operand 0 "memory_operand" "")
		    (match_operand 1 "general_operand" ""))
	      (use (match_operand 2 "" ""))
	      (clobber (reg:DI LR_REGNUM))])])

(define_expand "call"
  [(parallel [(call (match_operand 0 "memory_operand" "")
		    (match_operand 1 "general_operand" ""))
	      (use (match_operand 2 "" ""))
	      (clobber (reg:DI LR_REGNUM))])]
  ""
  "
  {
    rtx callee, pat;

    /* In an untyped call, we can get NULL for operand 2.  */
    if (operands[2] == NULL)
      operands[2] = const0_rtx;

    /* Decide if we should generate indirect calls by loading the
       64-bit address of the callee into a register before performing
       the branch-and-link.  */
    callee = XEXP (operands[0], 0);
    if (GET_CODE (callee) == SYMBOL_REF
	? (aarch64_is_long_call_p (callee)
	   || aarch64_is_noplt_call_p (callee))
	: !REG_P (callee))
      XEXP (operands[0], 0) = force_reg (Pmode, callee);

    pat = gen_call_internal (operands[0], operands[1], operands[2]);
    aarch64_emit_call_insn (pat);
    DONE;
  }"
)

(define_insn "*call_reg"
  [(call (mem:DI (match_operand:DI 0 "register_operand" "r"))
	 (match_operand 1 "" ""))
   (use (match_operand 2 "" ""))
   (clobber (reg:DI LR_REGNUM))]
  ""
  "blr\\t%0"
  [(set_attr "type" "call")]
)

(define_insn "*call_symbol"
  [(call (mem:DI (match_operand:DI 0 "" ""))
	 (match_operand 1 "" ""))
   (use (match_operand 2 "" ""))
   (clobber (reg:DI LR_REGNUM))]
  "GET_CODE (operands[0]) == SYMBOL_REF
   && !aarch64_is_long_call_p (operands[0])
   && !aarch64_is_noplt_call_p (operands[0])"
  "bl\\t%a0"
  [(set_attr "type" "call")]
)

(define_expand "call_value_internal"
  [(parallel [(set (match_operand 0 "" "")
		   (call (match_operand 1 "memory_operand" "")
			 (match_operand 2 "general_operand" "")))
	      (use (match_operand 3 "" ""))
	      (clobber (reg:DI LR_REGNUM))])])

(define_expand "call_value"
  [(parallel [(set (match_operand 0 "" "")
		   (call (match_operand 1 "memory_operand" "")
			 (match_operand 2 "general_operand" "")))
	      (use (match_operand 3 "" ""))
	      (clobber (reg:DI LR_REGNUM))])]
  ""
  "
  {
    rtx callee, pat;

    /* In an untyped call, we can get NULL for operand 3.  */
    if (operands[3] == NULL)
      operands[3] = const0_rtx;

    /* Decide if we should generate indirect calls by loading the
       64-bit address of the callee into a register before performing
       the branch-and-link.  */
    callee = XEXP (operands[1], 0);
    if (GET_CODE (callee) == SYMBOL_REF
	? (aarch64_is_long_call_p (callee)
	   || aarch64_is_noplt_call_p (callee))
	: !REG_P (callee))
      XEXP (operands[1], 0) = force_reg (Pmode, callee);

    pat = gen_call_value_internal (operands[0], operands[1], operands[2],
                                   operands[3]);
    aarch64_emit_call_insn (pat);
    DONE;
  }"
)

(define_insn "*call_value_reg"
  [(set (match_operand 0 "" "")
	(call (mem:DI (match_operand:DI 1 "register_operand" "r"))
		      (match_operand 2 "" "")))
   (use (match_operand 3 "" ""))
   (clobber (reg:DI LR_REGNUM))]
  ""
  "blr\\t%1"
  [(set_attr "type" "call")]

)

(define_insn "*call_value_symbol"
  [(set (match_operand 0 "" "")
	(call (mem:DI (match_operand:DI 1 "" ""))
	      (match_operand 2 "" "")))
   (use (match_operand 3 "" ""))
   (clobber (reg:DI LR_REGNUM))]
  "GET_CODE (operands[1]) == SYMBOL_REF
   && !aarch64_is_long_call_p (operands[1])
   && !aarch64_is_noplt_call_p (operands[1])"
  "bl\\t%a1"
  [(set_attr "type" "call")]
)

(define_expand "sibcall_internal"
  [(parallel [(call (match_operand 0 "memory_operand" "")
		    (match_operand 1 "general_operand" ""))
	      (return)
	      (use (match_operand 2 "" ""))])])

(define_expand "sibcall"
  [(parallel [(call (match_operand 0 "memory_operand" "")
		    (match_operand 1 "general_operand" ""))
	      (return)
	      (use (match_operand 2 "" ""))])]
  ""
  {
    rtx pat;
    rtx callee = XEXP (operands[0], 0);
    if (!REG_P (callee)
       && ((GET_CODE (callee) != SYMBOL_REF)
	   || aarch64_is_noplt_call_p (callee)))
      XEXP (operands[0], 0) = force_reg (Pmode, callee);

    /* FIXME: This is a band-aid.  Need to analyze why expand_expr_addr_expr
       is generating an SImode symbol reference.  See PR 64971.  */
    if (TARGET_ILP32
	&& GET_CODE (XEXP (operands[0], 0)) == SYMBOL_REF
	&& GET_MODE (XEXP (operands[0], 0)) == SImode)
      XEXP (operands[0], 0) = convert_memory_address (Pmode,
						      XEXP (operands[0], 0));
    if (operands[2] == NULL_RTX)
      operands[2] = const0_rtx;

    pat = gen_sibcall_internal (operands[0], operands[1], operands[2]);
    aarch64_emit_call_insn (pat);
    DONE;
  }
)

(define_expand "sibcall_value_internal"
  [(parallel [(set (match_operand 0 "" "")
		   (call (match_operand 1 "memory_operand" "")
			 (match_operand 2 "general_operand" "")))
	      (return)
	      (use (match_operand 3 "" ""))])])

(define_expand "sibcall_value"
  [(parallel [(set (match_operand 0 "" "")
		   (call (match_operand 1 "memory_operand" "")
			 (match_operand 2 "general_operand" "")))
	      (return)
	      (use (match_operand 3 "" ""))])]
  ""
  {
    rtx pat;
    rtx callee = XEXP (operands[1], 0);
    if (!REG_P (callee)
       && ((GET_CODE (callee) != SYMBOL_REF)
	   || aarch64_is_noplt_call_p (callee)))
      XEXP (operands[1], 0) = force_reg (Pmode, callee);

    /* FIXME: This is a band-aid.  Need to analyze why expand_expr_addr_expr
       is generating an SImode symbol reference.  See PR 64971.  */
    if (TARGET_ILP32
	&& GET_CODE (XEXP (operands[1], 0)) == SYMBOL_REF
	&& GET_MODE (XEXP (operands[1], 0)) == SImode)
      XEXP (operands[1], 0) = convert_memory_address (Pmode,
						      XEXP (operands[1], 0));

    if (operands[3] == NULL_RTX)
      operands[3] = const0_rtx;

    pat = gen_sibcall_value_internal (operands[0], operands[1], operands[2],
                                      operands[3]);
    aarch64_emit_call_insn (pat);
    DONE;
  }
)

(define_insn "*sibcall_insn"
  [(call (mem:DI (match_operand:DI 0 "aarch64_call_insn_operand" "Ucs, Usf"))
	 (match_operand 1 "" ""))
   (return)
   (use (match_operand 2 "" ""))]
  "SIBLING_CALL_P (insn)"
  "@
   br\\t%0
   b\\t%a0"
  [(set_attr "type" "branch, branch")]
)

(define_insn "*sibcall_value_insn"
  [(set (match_operand 0 "" "")
	(call (mem:DI
		(match_operand:DI 1 "aarch64_call_insn_operand" "Ucs, Usf"))
	      (match_operand 2 "" "")))
   (return)
   (use (match_operand 3 "" ""))]
  "SIBLING_CALL_P (insn)"
  "@
   br\\t%1
   b\\t%a1"
  [(set_attr "type" "branch, branch")]
)

;; Call subroutine returning any type.

(define_expand "untyped_call"
  [(parallel [(call (match_operand 0 "")
		    (const_int 0))
	      (match_operand 1 "")
	      (match_operand 2 "")])]
  ""
{
  int i;

  emit_call_insn (gen_call (operands[0], const0_rtx, NULL));

  for (i = 0; i < XVECLEN (operands[2], 0); i++)
    {
      rtx set = XVECEXP (operands[2], 0, i);
      emit_move_insn (SET_DEST (set), SET_SRC (set));
    }

  /* The optimizer does not know that the call sets the function value
     registers we stored in the result block.  We avoid problems by
     claiming that all hard registers are used and clobbered at this
     point.  */
  emit_insn (gen_blockage ());
  DONE;
})

;; -------------------------------------------------------------------
;; Moves
;; -------------------------------------------------------------------

(define_expand "mov<mode>"
  [(set (match_operand:SHORT 0 "nonimmediate_operand" "")
	(match_operand:SHORT 1 "general_operand" ""))]
  ""
  "
    if (GET_CODE (operands[0]) == MEM && operands[1] != const0_rtx)
      operands[1] = force_reg (<MODE>mode, operands[1]);
  "
)

(define_insn "*mov<mode>_aarch64"
  [(set (match_operand:SHORT 0 "nonimmediate_operand" "=r,r,   *w,r,*w, m, m, r,*w,*w")
        (match_operand:SHORT 1 "general_operand"      " r,M,D<hq>,m, m,rZ,*w,*w, r,*w"))]
  "(register_operand (operands[0], <MODE>mode)
    || aarch64_reg_or_zero (operands[1], <MODE>mode))"
{
   switch (which_alternative)
     {
     case 0:
       return "mov\t%w0, %w1";
     case 1:
       return "mov\t%w0, %1";
     case 2:
       return aarch64_output_scalar_simd_mov_immediate (operands[1],
							<MODE>mode);
     case 3:
       return "ldr<size>\t%w0, %1";
     case 4:
       return "ldr\t%<size>0, %1";
     case 5:
       return "str<size>\t%w1, %0";
     case 6:
       return "str\t%<size>1, %0";
     case 7:
       return "umov\t%w0, %1.<v>[0]";
     case 8:
       return "dup\t%0.<Vallxd>, %w1";
     case 9:
       return "dup\t%<Vetype>0, %1.<v>[0]";
     default:
       gcc_unreachable ();
     }
}
  [(set_attr "type" "mov_reg,mov_imm,neon_move,load1,load1,store1,store1,\
                     neon_to_gp<q>,neon_from_gp<q>,neon_dup")
   (set_attr "simd" "*,*,yes,*,*,*,*,yes,yes,yes")]
)

(define_expand "mov<mode>"
  [(set (match_operand:GPI 0 "nonimmediate_operand" "")
	(match_operand:GPI 1 "general_operand" ""))]
  ""
  "
    if (GET_CODE (operands[0]) == MEM && operands[1] != const0_rtx)
      operands[1] = force_reg (<MODE>mode, operands[1]);

    /* FIXME: RR we still need to fix up what we are doing with
       symbol_refs and other types of constants.  */
    if (CONSTANT_P (operands[1])
        && !CONST_INT_P (operands[1]))
     {
       aarch64_expand_mov_immediate (operands[0], operands[1]);
       DONE;
     }
  "
)

(define_insn_and_split "*movsi_aarch64"
  [(set (match_operand:SI 0 "nonimmediate_operand" "=r,k,r,r,r,r,*w,m,  m,r,r  ,*w, r,*w")
	(match_operand:SI 1 "aarch64_mov_operand"  " r,r,k,M,n,m, m,rZ,*w,S,Ush,rZ,*w,*w"))]
  "(register_operand (operands[0], SImode)
    || aarch64_reg_or_zero (operands[1], SImode))"
  "@
   mov\\t%w0, %w1
   mov\\t%w0, %w1
   mov\\t%w0, %w1
   mov\\t%w0, %1
   #
   ldr\\t%w0, %1
   ldr\\t%s0, %1
   str\\t%w1, %0
   str\\t%s1, %0
   adr\\t%x0, %a1
   adrp\\t%x0, %A1
   fmov\\t%s0, %w1
   fmov\\t%w0, %s1
   fmov\\t%s0, %s1"
   "CONST_INT_P (operands[1]) && !aarch64_move_imm (INTVAL (operands[1]), SImode)
    && REG_P (operands[0]) && GP_REGNUM_P (REGNO (operands[0]))"
   [(const_int 0)]
   "{
       aarch64_expand_mov_immediate (operands[0], operands[1]);
       DONE;
    }"
  [(set_attr "type" "mov_reg,mov_reg,mov_reg,mov_imm,mov_imm,load1,load1,store1,store1,\
                     adr,adr,f_mcr,f_mrc,fmov")
   (set_attr "fp" "*,*,*,*,*,*,yes,*,yes,*,*,yes,yes,yes")]
)

(define_insn_and_split "*movdi_aarch64"
  [(set (match_operand:DI 0 "nonimmediate_operand" "=r,k,r,r,r,r,*w,m,  m,r,r,  *w, r,*w,w")
	(match_operand:DI 1 "aarch64_mov_operand"  " r,r,k,N,n,m, m,rZ,*w,S,Ush,rZ,*w,*w,Dd"))]
  "(register_operand (operands[0], DImode)
    || aarch64_reg_or_zero (operands[1], DImode))"
  "@
   mov\\t%x0, %x1
   mov\\t%0, %x1
   mov\\t%x0, %1
   mov\\t%x0, %1
   #
   ldr\\t%x0, %1
   ldr\\t%d0, %1
   str\\t%x1, %0
   str\\t%d1, %0
   adr\\t%x0, %a1
   adrp\\t%x0, %A1
   fmov\\t%d0, %x1
   fmov\\t%x0, %d1
   fmov\\t%d0, %d1
   movi\\t%d0, %1"
   "(CONST_INT_P (operands[1]) && !aarch64_move_imm (INTVAL (operands[1]), DImode))
    && REG_P (operands[0]) && GP_REGNUM_P (REGNO (operands[0]))"
   [(const_int 0)]
   "{
       aarch64_expand_mov_immediate (operands[0], operands[1]);
       DONE;
    }"
  [(set_attr "type" "mov_reg,mov_reg,mov_reg,mov_imm,mov_imm,load1,load1,store1,store1,\
                     adr,adr,f_mcr,f_mrc,fmov,neon_move")
   (set_attr "fp" "*,*,*,*,*,*,yes,*,yes,*,*,yes,yes,yes,*")
   (set_attr "simd" "*,*,*,*,*,*,*,*,*,*,*,*,*,*,yes")]
)

(define_insn "insv_imm<mode>"
  [(set (zero_extract:GPI (match_operand:GPI 0 "register_operand" "+r")
			  (const_int 16)
			  (match_operand:GPI 1 "const_int_operand" "n"))
	(match_operand:GPI 2 "const_int_operand" "n"))]
  "UINTVAL (operands[1]) < GET_MODE_BITSIZE (<MODE>mode)
   && UINTVAL (operands[1]) % 16 == 0"
  "movk\\t%<w>0, %X2, lsl %1"
  [(set_attr "type" "mov_imm")]
)

(define_expand "movti"
  [(set (match_operand:TI 0 "nonimmediate_operand" "")
	(match_operand:TI 1 "general_operand" ""))]
  ""
  "
    if (GET_CODE (operands[0]) == MEM && operands[1] != const0_rtx)
      operands[1] = force_reg (TImode, operands[1]);
  "
)

(define_insn "*movti_aarch64"
  [(set (match_operand:TI 0
	 "nonimmediate_operand"  "=r, *w,r ,*w,r  ,Ump,Ump,*w,m")
	(match_operand:TI 1
	 "aarch64_movti_operand" " rn,r ,*w,*w,Ump,r  ,Z  , m,*w"))]
  "(register_operand (operands[0], TImode)
    || aarch64_reg_or_zero (operands[1], TImode))"
  "@
   #
   #
   #
   orr\\t%0.16b, %1.16b, %1.16b
   ldp\\t%0, %H0, %1
   stp\\t%1, %H1, %0
   stp\\txzr, xzr, %0
   ldr\\t%q0, %1
   str\\t%q1, %0"
  [(set_attr "type" "multiple,f_mcr,f_mrc,neon_logic_q, \
		             load2,store2,store2,f_loadd,f_stored")
   (set_attr "length" "8,8,8,4,4,4,4,4,4")
   (set_attr "simd" "*,*,*,yes,*,*,*,*,*")
   (set_attr "fp" "*,*,*,*,*,*,*,yes,yes")]
)

;; Split a TImode register-register or register-immediate move into
;; its component DImode pieces, taking care to handle overlapping
;; source and dest registers.
(define_split
   [(set (match_operand:TI 0 "register_operand" "")
	 (match_operand:TI 1 "aarch64_reg_or_imm" ""))]
  "reload_completed && aarch64_split_128bit_move_p (operands[0], operands[1])"
  [(const_int 0)]
{
  aarch64_split_128bit_move (operands[0], operands[1]);
  DONE;
})

(define_expand "mov<mode>"
  [(set (match_operand:GPF_TF_F16 0 "nonimmediate_operand" "")
	(match_operand:GPF_TF_F16 1 "general_operand" ""))]
  ""
  {
    if (!TARGET_FLOAT)
      {
	aarch64_err_no_fpadvsimd (<MODE>mode, "code");
	FAIL;
      }

    if (GET_CODE (operands[0]) == MEM
        && ! (GET_CODE (operands[1]) == CONST_DOUBLE
	      && aarch64_float_const_zero_rtx_p (operands[1])))
      operands[1] = force_reg (<MODE>mode, operands[1]);
  }
)

(define_insn "*movhf_aarch64"
  [(set (match_operand:HF 0 "nonimmediate_operand" "=w,w  ,?r,w,w,m,r,m ,r")
	(match_operand:HF 1 "general_operand"      "Y ,?rY, w,w,m,w,m,rY,r"))]
  "TARGET_FLOAT && (register_operand (operands[0], HFmode)
    || aarch64_reg_or_fp_zero (operands[1], HFmode))"
  "@
   movi\\t%0.4h, #0
   mov\\t%0.h[0], %w1
   umov\\t%w0, %1.h[0]
   mov\\t%0.h[0], %1.h[0]
   ldr\\t%h0, %1
   str\\t%h1, %0
   ldrh\\t%w0, %1
   strh\\t%w1, %0
   mov\\t%w0, %w1"
  [(set_attr "type" "neon_move,neon_from_gp,neon_to_gp,neon_move,\
                     f_loads,f_stores,load1,store1,mov_reg")
   (set_attr "simd" "yes,yes,yes,yes,*,*,*,*,*")]
)

(define_insn "*movsf_aarch64"
  [(set (match_operand:SF 0 "nonimmediate_operand" "=w,w  ,?r,w,w  ,w,m,r,m ,r")
	(match_operand:SF 1 "general_operand"      "Y ,?rY, w,w,Ufc,m,w,m,rY,r"))]
  "TARGET_FLOAT && (register_operand (operands[0], SFmode)
    || aarch64_reg_or_fp_zero (operands[1], SFmode))"
  "@
   movi\\t%0.2s, #0
   fmov\\t%s0, %w1
   fmov\\t%w0, %s1
   fmov\\t%s0, %s1
   fmov\\t%s0, %1
   ldr\\t%s0, %1
   str\\t%s1, %0
   ldr\\t%w0, %1
   str\\t%w1, %0
   mov\\t%w0, %w1"
  [(set_attr "type" "neon_move,f_mcr,f_mrc,fmov,fconsts,\
                     f_loads,f_stores,load1,store1,mov_reg")
   (set_attr "simd" "yes,*,*,*,*,*,*,*,*,*")]
)

(define_insn "*movdf_aarch64"
  [(set (match_operand:DF 0 "nonimmediate_operand" "=w,w  ,?r,w,w  ,w,m,r,m ,r")
	(match_operand:DF 1 "general_operand"      "Y ,?rY, w,w,Ufc,m,w,m,rY,r"))]
  "TARGET_FLOAT && (register_operand (operands[0], DFmode)
    || aarch64_reg_or_fp_zero (operands[1], DFmode))"
  "@
   movi\\t%d0, #0
   fmov\\t%d0, %x1
   fmov\\t%x0, %d1
   fmov\\t%d0, %d1
   fmov\\t%d0, %1
   ldr\\t%d0, %1
   str\\t%d1, %0
   ldr\\t%x0, %1
   str\\t%x1, %0
   mov\\t%x0, %x1"
  [(set_attr "type" "neon_move,f_mcr,f_mrc,fmov,fconstd,\
                     f_loadd,f_stored,load1,store1,mov_reg")
   (set_attr "simd" "yes,*,*,*,*,*,*,*,*,*")]
)

(define_insn "*movtf_aarch64"
  [(set (match_operand:TF 0
	 "nonimmediate_operand" "=w,?&r,w ,?r,w,?w,w,m,?r ,Ump,Ump")
	(match_operand:TF 1
	 "general_operand"      " w,?r, ?r,w ,Y,Y ,m,w,Ump,?r ,Y"))]
  "TARGET_FLOAT && (register_operand (operands[0], TFmode)
    || aarch64_reg_or_fp_zero (operands[1], TFmode))"
  "@
   orr\\t%0.16b, %1.16b, %1.16b
   #
   #
   #
   movi\\t%0.2d, #0
   fmov\\t%s0, wzr
   ldr\\t%q0, %1
   str\\t%q1, %0
   ldp\\t%0, %H0, %1
   stp\\t%1, %H1, %0
   stp\\txzr, xzr, %0"
  [(set_attr "type" "logic_reg,multiple,f_mcr,f_mrc,neon_move_q,f_mcr,\
                     f_loadd,f_stored,load2,store2,store2")
   (set_attr "length" "4,8,8,8,4,4,4,4,4,4,4")
   (set_attr "simd" "yes,*,*,*,yes,*,*,*,*,*,*")]
)

(define_split
   [(set (match_operand:TF 0 "register_operand" "")
	 (match_operand:TF 1 "aarch64_reg_or_imm" ""))]
  "reload_completed && aarch64_split_128bit_move_p (operands[0], operands[1])"
  [(const_int 0)]
  {
    aarch64_split_128bit_move (operands[0], operands[1]);
    DONE;
  }
)

;; 0 is dst
;; 1 is src
;; 2 is size of move in bytes
;; 3 is alignment

(define_expand "movmemdi"
  [(match_operand:BLK 0 "memory_operand")
   (match_operand:BLK 1 "memory_operand")
   (match_operand:DI 2 "immediate_operand")
   (match_operand:DI 3 "immediate_operand")]
   "!STRICT_ALIGNMENT"
{
  if (aarch64_expand_movmem (operands))
    DONE;
  FAIL;
}
)

;; Operands 1 and 3 are tied together by the final condition; so we allow
;; fairly lax checking on the second memory operation.
(define_insn "load_pairsi"
  [(set (match_operand:SI 0 "register_operand" "=r,*w")
	(match_operand:SI 1 "aarch64_mem_pair_operand" "Ump,Ump"))
   (set (match_operand:SI 2 "register_operand" "=r,*w")
	(match_operand:SI 3 "memory_operand" "m,m"))]
  "rtx_equal_p (XEXP (operands[3], 0),
		plus_constant (Pmode,
			       XEXP (operands[1], 0),
			       GET_MODE_SIZE (SImode)))"
  "@
   ldp\\t%w0, %w2, %1
   ldp\\t%s0, %s2, %1"
  [(set_attr "type" "load2,neon_load1_2reg")
   (set_attr "fp" "*,yes")]
)

(define_insn "load_pairdi"
  [(set (match_operand:DI 0 "register_operand" "=r,*w")
	(match_operand:DI 1 "aarch64_mem_pair_operand" "Ump,Ump"))
   (set (match_operand:DI 2 "register_operand" "=r,*w")
	(match_operand:DI 3 "memory_operand" "m,m"))]
  "rtx_equal_p (XEXP (operands[3], 0),
		plus_constant (Pmode,
			       XEXP (operands[1], 0),
			       GET_MODE_SIZE (DImode)))"
  "@
   ldp\\t%x0, %x2, %1
   ldp\\t%d0, %d2, %1"
  [(set_attr "type" "load2,neon_load1_2reg")
   (set_attr "fp" "*,yes")]
)


;; Operands 0 and 2 are tied together by the final condition; so we allow
;; fairly lax checking on the second memory operation.
(define_insn "store_pairsi"
  [(set (match_operand:SI 0 "aarch64_mem_pair_operand" "=Ump,Ump")
	(match_operand:SI 1 "aarch64_reg_or_zero" "rZ,*w"))
   (set (match_operand:SI 2 "memory_operand" "=m,m")
	(match_operand:SI 3 "aarch64_reg_or_zero" "rZ,*w"))]
  "rtx_equal_p (XEXP (operands[2], 0),
		plus_constant (Pmode,
			       XEXP (operands[0], 0),
			       GET_MODE_SIZE (SImode)))"
  "@
   stp\\t%w1, %w3, %0
   stp\\t%s1, %s3, %0"
  [(set_attr "type" "store2,neon_store1_2reg")
   (set_attr "fp" "*,yes")]
)

(define_insn "store_pairdi"
  [(set (match_operand:DI 0 "aarch64_mem_pair_operand" "=Ump,Ump")
	(match_operand:DI 1 "aarch64_reg_or_zero" "rZ,*w"))
   (set (match_operand:DI 2 "memory_operand" "=m,m")
	(match_operand:DI 3 "aarch64_reg_or_zero" "rZ,*w"))]
  "rtx_equal_p (XEXP (operands[2], 0),
		plus_constant (Pmode,
			       XEXP (operands[0], 0),
			       GET_MODE_SIZE (DImode)))"
  "@
   stp\\t%x1, %x3, %0
   stp\\t%d1, %d3, %0"
  [(set_attr "type" "store2,neon_store1_2reg")
   (set_attr "fp" "*,yes")]
)

;; Operands 1 and 3 are tied together by the final condition; so we allow
;; fairly lax checking on the second memory operation.
(define_insn "load_pairsf"
  [(set (match_operand:SF 0 "register_operand" "=w,*r")
	(match_operand:SF 1 "aarch64_mem_pair_operand" "Ump,Ump"))
   (set (match_operand:SF 2 "register_operand" "=w,*r")
	(match_operand:SF 3 "memory_operand" "m,m"))]
  "rtx_equal_p (XEXP (operands[3], 0),
		plus_constant (Pmode,
			       XEXP (operands[1], 0),
			       GET_MODE_SIZE (SFmode)))"
  "@
   ldp\\t%s0, %s2, %1
   ldp\\t%w0, %w2, %1"
  [(set_attr "type" "neon_load1_2reg,load2")
   (set_attr "fp" "yes,*")]
)

(define_insn "load_pairdf"
  [(set (match_operand:DF 0 "register_operand" "=w,*r")
	(match_operand:DF 1 "aarch64_mem_pair_operand" "Ump,Ump"))
   (set (match_operand:DF 2 "register_operand" "=w,*r")
	(match_operand:DF 3 "memory_operand" "m,m"))]
  "rtx_equal_p (XEXP (operands[3], 0),
		plus_constant (Pmode,
			       XEXP (operands[1], 0),
			       GET_MODE_SIZE (DFmode)))"
  "@
   ldp\\t%d0, %d2, %1
   ldp\\t%x0, %x2, %1"
  [(set_attr "type" "neon_load1_2reg,load2")
   (set_attr "fp" "yes,*")]
)

;; Operands 0 and 2 are tied together by the final condition; so we allow
;; fairly lax checking on the second memory operation.
(define_insn "store_pairsf"
  [(set (match_operand:SF 0 "aarch64_mem_pair_operand" "=Ump,Ump")
	(match_operand:SF 1 "aarch64_reg_or_fp_zero" "w,*rY"))
   (set (match_operand:SF 2 "memory_operand" "=m,m")
	(match_operand:SF 3 "aarch64_reg_or_fp_zero" "w,*rY"))]
  "rtx_equal_p (XEXP (operands[2], 0),
		plus_constant (Pmode,
			       XEXP (operands[0], 0),
			       GET_MODE_SIZE (SFmode)))"
  "@
   stp\\t%s1, %s3, %0
   stp\\t%w1, %w3, %0"
  [(set_attr "type" "neon_store1_2reg,store2")
   (set_attr "fp" "yes,*")]
)

(define_insn "store_pairdf"
  [(set (match_operand:DF 0 "aarch64_mem_pair_operand" "=Ump,Ump")
	(match_operand:DF 1 "aarch64_reg_or_fp_zero" "w,*rY"))
   (set (match_operand:DF 2 "memory_operand" "=m,m")
	(match_operand:DF 3 "aarch64_reg_or_fp_zero" "w,*rY"))]
  "rtx_equal_p (XEXP (operands[2], 0),
		plus_constant (Pmode,
			       XEXP (operands[0], 0),
			       GET_MODE_SIZE (DFmode)))"
  "@
   stp\\t%d1, %d3, %0
   stp\\t%x1, %x3, %0"
  [(set_attr "type" "neon_store1_2reg,store2")
   (set_attr "fp" "yes,*")]
)

;; Load pair with post-index writeback.  This is primarily used in function
;; epilogues.
(define_insn "loadwb_pair<GPI:mode>_<P:mode>"
  [(parallel
    [(set (match_operand:P 0 "register_operand" "=k")
          (plus:P (match_operand:P 1 "register_operand" "0")
                  (match_operand:P 4 "aarch64_mem_pair_offset" "n")))
     (set (match_operand:GPI 2 "register_operand" "=r")
          (mem:GPI (match_dup 1)))
     (set (match_operand:GPI 3 "register_operand" "=r")
          (mem:GPI (plus:P (match_dup 1)
                   (match_operand:P 5 "const_int_operand" "n"))))])]
  "INTVAL (operands[5]) == GET_MODE_SIZE (<GPI:MODE>mode)"
  "ldp\\t%<w>2, %<w>3, [%1], %4"
  [(set_attr "type" "load2")]
)

(define_insn "loadwb_pair<GPF:mode>_<P:mode>"
  [(parallel
    [(set (match_operand:P 0 "register_operand" "=k")
          (plus:P (match_operand:P 1 "register_operand" "0")
                  (match_operand:P 4 "aarch64_mem_pair_offset" "n")))
     (set (match_operand:GPF 2 "register_operand" "=w")
          (mem:GPF (match_dup 1)))
     (set (match_operand:GPF 3 "register_operand" "=w")
          (mem:GPF (plus:P (match_dup 1)
                   (match_operand:P 5 "const_int_operand" "n"))))])]
  "INTVAL (operands[5]) == GET_MODE_SIZE (<GPF:MODE>mode)"
  "ldp\\t%<w>2, %<w>3, [%1], %4"
  [(set_attr "type" "neon_load1_2reg")]
)

;; Store pair with pre-index writeback.  This is primarily used in function
;; prologues.
(define_insn "storewb_pair<GPI:mode>_<P:mode>"
  [(parallel
    [(set (match_operand:P 0 "register_operand" "=&k")
          (plus:P (match_operand:P 1 "register_operand" "0")
                  (match_operand:P 4 "aarch64_mem_pair_offset" "n")))
     (set (mem:GPI (plus:P (match_dup 0)
                   (match_dup 4)))
          (match_operand:GPI 2 "register_operand" "r"))
     (set (mem:GPI (plus:P (match_dup 0)
                   (match_operand:P 5 "const_int_operand" "n")))
          (match_operand:GPI 3 "register_operand" "r"))])]
  "INTVAL (operands[5]) == INTVAL (operands[4]) + GET_MODE_SIZE (<GPI:MODE>mode)"
  "stp\\t%<w>2, %<w>3, [%0, %4]!"
  [(set_attr "type" "store2")]
)

(define_insn "storewb_pair<GPF:mode>_<P:mode>"
  [(parallel
    [(set (match_operand:P 0 "register_operand" "=&k")
          (plus:P (match_operand:P 1 "register_operand" "0")
                  (match_operand:P 4 "aarch64_mem_pair_offset" "n")))
     (set (mem:GPF (plus:P (match_dup 0)
                   (match_dup 4)))
          (match_operand:GPF 2 "register_operand" "w"))
     (set (mem:GPF (plus:P (match_dup 0)
                   (match_operand:P 5 "const_int_operand" "n")))
          (match_operand:GPF 3 "register_operand" "w"))])]
  "INTVAL (operands[5]) == INTVAL (operands[4]) + GET_MODE_SIZE (<GPF:MODE>mode)"
  "stp\\t%<w>2, %<w>3, [%0, %4]!"
  [(set_attr "type" "neon_store1_2reg<q>")]
)

;; -------------------------------------------------------------------
;; Sign/Zero extension
;; -------------------------------------------------------------------

(define_expand "<optab>sidi2"
  [(set (match_operand:DI 0 "register_operand")
	(ANY_EXTEND:DI (match_operand:SI 1 "nonimmediate_operand")))]
  ""
)

(define_insn "*extendsidi2_aarch64"
  [(set (match_operand:DI 0 "register_operand" "=r,r")
        (sign_extend:DI (match_operand:SI 1 "nonimmediate_operand" "r,m")))]
  ""
  "@
   sxtw\t%0, %w1
   ldrsw\t%0, %1"
  [(set_attr "type" "extend,load1")]
)

(define_insn "*load_pair_extendsidi2_aarch64"
  [(set (match_operand:DI 0 "register_operand" "=r")
	(sign_extend:DI (match_operand:SI 1 "aarch64_mem_pair_operand" "Ump")))
   (set (match_operand:DI 2 "register_operand" "=r")
	(sign_extend:DI (match_operand:SI 3 "memory_operand" "m")))]
  "rtx_equal_p (XEXP (operands[3], 0),
		plus_constant (Pmode,
			       XEXP (operands[1], 0),
			       GET_MODE_SIZE (SImode)))"
  "ldpsw\\t%0, %2, %1"
  [(set_attr "type" "load2")]
)

(define_insn "*zero_extendsidi2_aarch64"
  [(set (match_operand:DI 0 "register_operand" "=r,r")
        (zero_extend:DI (match_operand:SI 1 "nonimmediate_operand" "r,m")))]
  ""
  "@
   uxtw\t%0, %w1
   ldr\t%w0, %1"
  [(set_attr "type" "extend,load1")]
)

(define_insn "*load_pair_zero_extendsidi2_aarch64"
  [(set (match_operand:DI 0 "register_operand" "=r")
	(zero_extend:DI (match_operand:SI 1 "aarch64_mem_pair_operand" "Ump")))
   (set (match_operand:DI 2 "register_operand" "=r")
	(zero_extend:DI (match_operand:SI 3 "memory_operand" "m")))]
  "rtx_equal_p (XEXP (operands[3], 0),
		plus_constant (Pmode,
			       XEXP (operands[1], 0),
			       GET_MODE_SIZE (SImode)))"
  "ldp\\t%w0, %w2, %1"
  [(set_attr "type" "load2")]
)

(define_expand "<ANY_EXTEND:optab><SHORT:mode><GPI:mode>2"
  [(set (match_operand:GPI 0 "register_operand")
        (ANY_EXTEND:GPI (match_operand:SHORT 1 "nonimmediate_operand")))]
  ""
)

(define_insn "*extend<SHORT:mode><GPI:mode>2_aarch64"
  [(set (match_operand:GPI 0 "register_operand" "=r,r")
        (sign_extend:GPI (match_operand:SHORT 1 "nonimmediate_operand" "r,m")))]
  ""
  "@
   sxt<SHORT:size>\t%<GPI:w>0, %w1
   ldrs<SHORT:size>\t%<GPI:w>0, %1"
  [(set_attr "type" "extend,load1")]
)

(define_insn "*zero_extend<SHORT:mode><GPI:mode>2_aarch64"
  [(set (match_operand:GPI 0 "register_operand" "=r,r,*w")
        (zero_extend:GPI (match_operand:SHORT 1 "nonimmediate_operand" "r,m,m")))]
  ""
  "@
   uxt<SHORT:size>\t%<GPI:w>0, %w1
   ldr<SHORT:size>\t%w0, %1
   ldr\t%<SHORT:size>0, %1"
  [(set_attr "type" "extend,load1,load1")]
)

(define_expand "<optab>qihi2"
  [(set (match_operand:HI 0 "register_operand")
        (ANY_EXTEND:HI (match_operand:QI 1 "nonimmediate_operand")))]
  ""
)

(define_insn "*<optab>qihi2_aarch64"
  [(set (match_operand:HI 0 "register_operand" "=r,r")
        (ANY_EXTEND:HI (match_operand:QI 1 "nonimmediate_operand" "r,m")))]
  ""
  "@
   <su>xtb\t%w0, %w1
   <ldrxt>b\t%w0, %1"
  [(set_attr "type" "extend,load1")]
)

;; -------------------------------------------------------------------
;; Simple arithmetic
;; -------------------------------------------------------------------

(define_expand "add<mode>3"
  [(set
    (match_operand:GPI 0 "register_operand" "")
    (plus:GPI (match_operand:GPI 1 "register_operand" "")
	      (match_operand:GPI 2 "aarch64_pluslong_operand" "")))]
  ""
{
  if (aarch64_pluslong_strict_immedate (operands[2], <MODE>mode))
    {
      /* Give CSE the opportunity to share this constant across additions.  */
      if (!cse_not_expected && can_create_pseudo_p ())
        operands[2] = force_reg (<MODE>mode, operands[2]);

      /* Split will refuse to operate on a modification to the stack pointer.
	 Aid the prologue and epilogue expanders by splitting this now.  */
      else if (reload_completed && operands[0] == stack_pointer_rtx)
	{
	  HOST_WIDE_INT i = INTVAL (operands[2]);
	  HOST_WIDE_INT s = (i >= 0 ? i & 0xfff : -(-i & 0xfff));
	  emit_insn (gen_rtx_SET (operands[0],
				  gen_rtx_PLUS (<MODE>mode, operands[1],
						GEN_INT (i - s))));
	  operands[1] = operands[0];
	  operands[2] = GEN_INT (s);
	}
    }
})

(define_insn "*add<mode>3_aarch64"
  [(set
    (match_operand:GPI 0 "register_operand" "=rk,rk,w,rk,r")
    (plus:GPI
     (match_operand:GPI 1 "register_operand" "%rk,rk,w,rk,rk")
     (match_operand:GPI 2 "aarch64_pluslong_operand" "I,r,w,J,Upl")))]
  ""
  "@
  add\\t%<w>0, %<w>1, %2
  add\\t%<w>0, %<w>1, %<w>2
  add\\t%<rtn>0<vas>, %<rtn>1<vas>, %<rtn>2<vas>
  sub\\t%<w>0, %<w>1, #%n2
  #"
  [(set_attr "type" "alu_imm,alu_sreg,neon_add,alu_imm,multiple")
   (set_attr "simd" "*,*,yes,*,*")]
)

;; zero_extend version of above
(define_insn "*addsi3_aarch64_uxtw"
  [(set
    (match_operand:DI 0 "register_operand" "=rk,rk,rk,r")
    (zero_extend:DI
     (plus:SI (match_operand:SI 1 "register_operand" "%rk,rk,rk,rk")
              (match_operand:SI 2 "aarch64_pluslong_operand" "I,r,J,Upl"))))]
  ""
  "@
  add\\t%w0, %w1, %2
  add\\t%w0, %w1, %w2
  sub\\t%w0, %w1, #%n2
  #"
  [(set_attr "type" "alu_imm,alu_sreg,alu_imm,multiple")]
)

;; If there's a free register, and we can load the constant with a
;; single instruction, do so.  This has a chance to improve scheduling.
(define_peephole2
  [(match_scratch:GPI 3 "r")
   (set (match_operand:GPI 0 "register_operand")
	(plus:GPI
	  (match_operand:GPI 1 "register_operand")
	  (match_operand:GPI 2 "aarch64_pluslong_strict_immedate")))]
  "aarch64_move_imm (INTVAL (operands[2]), <MODE>mode)"
  [(set (match_dup 3) (match_dup 2))
   (set (match_dup 0) (plus:GPI (match_dup 1) (match_dup 3)))]
)

(define_peephole2
  [(match_scratch:SI 3 "r")
   (set (match_operand:DI 0 "register_operand")
	(zero_extend:DI
	  (plus:SI
	    (match_operand:SI 1 "register_operand")
	    (match_operand:SI 2 "aarch64_pluslong_strict_immedate"))))]
  "aarch64_move_imm (INTVAL (operands[2]), SImode)"
  [(set (match_dup 3) (match_dup 2))
   (set (match_dup 0) (zero_extend:DI (plus:SI (match_dup 1) (match_dup 3))))]
)

;; After peephole2 has had a chance to run, split any remaining long
;; additions into two add immediates.
(define_split
  [(set (match_operand:GPI 0 "register_operand")
	(plus:GPI
	  (match_operand:GPI 1 "register_operand")
	  (match_operand:GPI 2 "aarch64_pluslong_strict_immedate")))]
  "epilogue_completed"
  [(set (match_dup 0) (plus:GPI (match_dup 1) (match_dup 3)))
   (set (match_dup 0) (plus:GPI (match_dup 0) (match_dup 4)))]
  {
    HOST_WIDE_INT i = INTVAL (operands[2]);
    HOST_WIDE_INT s = (i >= 0 ? i & 0xfff : -(-i & 0xfff));
    operands[3] = GEN_INT (i - s);
    operands[4] = GEN_INT (s);
  }
)

(define_split
  [(set (match_operand:DI 0 "register_operand")
	(zero_extend:DI
	  (plus:SI
	    (match_operand:SI 1 "register_operand")
	    (match_operand:SI 2 "aarch64_pluslong_strict_immedate"))))]
  "epilogue_completed"
  [(set (match_dup 5) (plus:SI (match_dup 1) (match_dup 3)))
   (set (match_dup 0) (zero_extend:DI (plus:SI (match_dup 5) (match_dup 4))))]
  {
    HOST_WIDE_INT i = INTVAL (operands[2]);
    HOST_WIDE_INT s = (i >= 0 ? i & 0xfff : -(-i & 0xfff));
    operands[3] = GEN_INT (i - s);
    operands[4] = GEN_INT (s);
    operands[5] = gen_lowpart (SImode, operands[0]);
  }
)

(define_expand "addti3"
  [(set (match_operand:TI 0 "register_operand" "")
	(plus:TI (match_operand:TI 1 "register_operand" "")
		 (match_operand:TI 2 "register_operand" "")))]
  ""
{
  rtx low = gen_reg_rtx (DImode);
  emit_insn (gen_adddi3_compareC (low, gen_lowpart (DImode, operands[1]),
				  gen_lowpart (DImode, operands[2])));

  rtx high = gen_reg_rtx (DImode);
  emit_insn (gen_adddi3_carryin (high, gen_highpart (DImode, operands[1]),
				 gen_highpart (DImode, operands[2])));

  emit_move_insn (gen_lowpart (DImode, operands[0]), low);
  emit_move_insn (gen_highpart (DImode, operands[0]), high);
  DONE;
})

(define_insn "add<mode>3_compare0"
  [(set (reg:CC_NZ CC_REGNUM)
	(compare:CC_NZ
	 (plus:GPI (match_operand:GPI 1 "register_operand" "%r,r,r")
		   (match_operand:GPI 2 "aarch64_plus_operand" "r,I,J"))
	 (const_int 0)))
   (set (match_operand:GPI 0 "register_operand" "=r,r,r")
	(plus:GPI (match_dup 1) (match_dup 2)))]
  ""
  "@
  adds\\t%<w>0, %<w>1, %<w>2
  adds\\t%<w>0, %<w>1, %2
  subs\\t%<w>0, %<w>1, #%n2"
  [(set_attr "type" "alus_sreg,alus_imm,alus_imm")]
)

;; zero_extend version of above
(define_insn "*addsi3_compare0_uxtw"
  [(set (reg:CC_NZ CC_REGNUM)
	(compare:CC_NZ
	 (plus:SI (match_operand:SI 1 "register_operand" "%r,r,r")
		  (match_operand:SI 2 "aarch64_plus_operand" "r,I,J"))
	 (const_int 0)))
   (set (match_operand:DI 0 "register_operand" "=r,r,r")
	(zero_extend:DI (plus:SI (match_dup 1) (match_dup 2))))]
  ""
  "@
  adds\\t%w0, %w1, %w2
  adds\\t%w0, %w1, %2
  subs\\t%w0, %w1, #%n2"
  [(set_attr "type" "alus_sreg,alus_imm,alus_imm")]
)

(define_insn "*add<mode>3_compareC_cconly_imm"
  [(set (reg:CC_C CC_REGNUM)
	(ne:CC_C
	  (plus:<DWI>
	    (zero_extend:<DWI> (match_operand:GPI 0 "register_operand" "r,r"))
	    (match_operand:<DWI> 2 "const_scalar_int_operand" ""))
	  (zero_extend:<DWI>
	    (plus:GPI
	      (match_dup 0)
	      (match_operand:GPI 1 "aarch64_plus_immediate" "I,J")))))]
  "aarch64_zero_extend_const_eq (<DWI>mode, operands[2],
				 <MODE>mode, operands[1])"
  "@
  cmn\\t%<w>0, %1
  cmp\\t%<w>0, #%n1"
  [(set_attr "type" "alus_imm")]
)

(define_insn "*add<mode>3_compareC_cconly"
  [(set (reg:CC_C CC_REGNUM)
	(ne:CC_C
	  (plus:<DWI>
	    (zero_extend:<DWI> (match_operand:GPI 0 "register_operand" "r"))
	    (zero_extend:<DWI> (match_operand:GPI 1 "register_operand" "r")))
	  (zero_extend:<DWI> (plus:GPI (match_dup 0) (match_dup 1)))))]
  ""
  "cmn\\t%<w>0, %<w>1"
  [(set_attr "type" "alus_sreg")]
)

(define_insn "*add<mode>3_compareC_imm"
  [(set (reg:CC_C CC_REGNUM)
	(ne:CC_C
	  (plus:<DWI>
	    (zero_extend:<DWI> (match_operand:GPI 1 "register_operand" "r,r"))
	    (match_operand:<DWI> 3 "const_scalar_int_operand" ""))
	  (zero_extend:<DWI>
	    (plus:GPI
	      (match_dup 1)
	      (match_operand:GPI 2 "aarch64_plus_immediate" "I,J")))))
   (set (match_operand:GPI 0 "register_operand" "=r,r")
	(plus:GPI (match_dup 1) (match_dup 2)))]
  "aarch64_zero_extend_const_eq (<DWI>mode, operands[3],
                                 <MODE>mode, operands[2])"
  "@
  adds\\t%<w>0, %<w>1, %2
  subs\\t%<w>0, %<w>1, #%n2"
  [(set_attr "type" "alus_imm")]
)

(define_insn "add<mode>3_compareC"
  [(set (reg:CC_C CC_REGNUM)
	(ne:CC_C
	  (plus:<DWI>
	    (zero_extend:<DWI> (match_operand:GPI 1 "register_operand" "r"))
	    (zero_extend:<DWI> (match_operand:GPI 2 "register_operand" "r")))
	  (zero_extend:<DWI>
	    (plus:GPI (match_dup 1) (match_dup 2)))))
   (set (match_operand:GPI 0 "register_operand" "=r")
	(plus:GPI (match_dup 1) (match_dup 2)))]
  ""
  "adds\\t%<w>0, %<w>1, %<w>2"
  [(set_attr "type" "alus_sreg")]
)

(define_insn "*adds_shift_imm_<mode>"
  [(set (reg:CC_NZ CC_REGNUM)
	(compare:CC_NZ
	 (plus:GPI (ASHIFT:GPI 
		    (match_operand:GPI 1 "register_operand" "r")
		    (match_operand:QI 2 "aarch64_shift_imm_<mode>" "n"))
		   (match_operand:GPI 3 "register_operand" "r"))
	 (const_int 0)))
   (set (match_operand:GPI 0 "register_operand" "=r")
	(plus:GPI (ASHIFT:GPI (match_dup 1) (match_dup 2))
		  (match_dup 3)))]
  ""
  "adds\\t%<w>0, %<w>3, %<w>1, <shift> %2"
  [(set_attr "type" "alus_shift_imm")]
)

(define_insn "*subs_shift_imm_<mode>"
  [(set (reg:CC_NZ CC_REGNUM)
	(compare:CC_NZ
	 (minus:GPI (match_operand:GPI 1 "register_operand" "r")
		    (ASHIFT:GPI
		     (match_operand:GPI 2 "register_operand" "r")
		     (match_operand:QI 3 "aarch64_shift_imm_<mode>" "n")))
	 (const_int 0)))
   (set (match_operand:GPI 0 "register_operand" "=r")
	(minus:GPI (match_dup 1)
		   (ASHIFT:GPI (match_dup 2) (match_dup 3))))]
  ""
  "subs\\t%<w>0, %<w>1, %<w>2, <shift> %3"
  [(set_attr "type" "alus_shift_imm")]
)

(define_insn "*adds_mul_imm_<mode>"
  [(set (reg:CC_NZ CC_REGNUM)
	(compare:CC_NZ
	 (plus:GPI (mult:GPI
		    (match_operand:GPI 1 "register_operand" "r")
		    (match_operand:QI 2 "aarch64_pwr_2_<mode>" "n"))
		   (match_operand:GPI 3 "register_operand" "r"))
	 (const_int 0)))
   (set (match_operand:GPI 0 "register_operand" "=r")
	(plus:GPI (mult:GPI (match_dup 1) (match_dup 2))
		  (match_dup 3)))]
  ""
  "adds\\t%<w>0, %<w>3, %<w>1, lsl %p2"
  [(set_attr "type" "alus_shift_imm")]
)

(define_insn "*subs_mul_imm_<mode>"
  [(set (reg:CC_NZ CC_REGNUM)
	(compare:CC_NZ
	 (minus:GPI (match_operand:GPI 1 "register_operand" "r")
		    (mult:GPI
		     (match_operand:GPI 2 "register_operand" "r")
		     (match_operand:QI 3 "aarch64_pwr_2_<mode>" "n")))
	 (const_int 0)))
   (set (match_operand:GPI 0 "register_operand" "=r")
	(minus:GPI (match_dup 1)
		   (mult:GPI (match_dup 2) (match_dup 3))))]
  ""
  "subs\\t%<w>0, %<w>1, %<w>2, lsl %p3"
  [(set_attr "type" "alus_shift_imm")]
)

(define_insn "*adds_<optab><ALLX:mode>_<GPI:mode>"
  [(set (reg:CC_NZ CC_REGNUM)
	(compare:CC_NZ
	 (plus:GPI
	  (ANY_EXTEND:GPI (match_operand:ALLX 1 "register_operand" "r"))
	  (match_operand:GPI 2 "register_operand" "r"))
	(const_int 0)))
   (set (match_operand:GPI 0 "register_operand" "=r")
	(plus:GPI (ANY_EXTEND:GPI (match_dup 1)) (match_dup 2)))]
  ""
  "adds\\t%<GPI:w>0, %<GPI:w>2, %<GPI:w>1, <su>xt<ALLX:size>"
  [(set_attr "type" "alus_ext")]
)

(define_insn "*subs_<optab><ALLX:mode>_<GPI:mode>"
  [(set (reg:CC_NZ CC_REGNUM)
	(compare:CC_NZ
	 (minus:GPI (match_operand:GPI 1 "register_operand" "r")
		    (ANY_EXTEND:GPI
		     (match_operand:ALLX 2 "register_operand" "r")))
	(const_int 0)))
   (set (match_operand:GPI 0 "register_operand" "=r")
	(minus:GPI (match_dup 1) (ANY_EXTEND:GPI (match_dup 2))))]
  ""
  "subs\\t%<GPI:w>0, %<GPI:w>1, %<GPI:w>2, <su>xt<ALLX:size>"
  [(set_attr "type" "alus_ext")]
)

(define_insn "*adds_<optab><ALLX:mode>_shift_<GPI:mode>"
  [(set (reg:CC_NZ CC_REGNUM)
	(compare:CC_NZ
	 (plus:GPI (ashift:GPI 
		    (ANY_EXTEND:GPI 
		     (match_operand:ALLX 1 "register_operand" "r"))
		    (match_operand 2 "aarch64_imm3" "Ui3"))
		   (match_operand:GPI 3 "register_operand" "r"))
	 (const_int 0)))
   (set (match_operand:GPI 0 "register_operand" "=rk")
	(plus:GPI (ashift:GPI (ANY_EXTEND:GPI (match_dup 1))
			      (match_dup 2))
		  (match_dup 3)))]
  ""
  "adds\\t%<GPI:w>0, %<GPI:w>3, %<GPI:w>1, <su>xt<ALLX:size> %2"
  [(set_attr "type" "alus_ext")]
)

(define_insn "*subs_<optab><ALLX:mode>_shift_<GPI:mode>"
  [(set (reg:CC_NZ CC_REGNUM)
	(compare:CC_NZ
	 (minus:GPI (match_operand:GPI 1 "register_operand" "r")
		    (ashift:GPI 
		     (ANY_EXTEND:GPI
		      (match_operand:ALLX 2 "register_operand" "r"))
		     (match_operand 3 "aarch64_imm3" "Ui3")))
	 (const_int 0)))
   (set (match_operand:GPI 0 "register_operand" "=rk")
	(minus:GPI (match_dup 1)
		   (ashift:GPI (ANY_EXTEND:GPI (match_dup 2))
			       (match_dup 3))))]
  ""
  "subs\\t%<GPI:w>0, %<GPI:w>1, %<GPI:w>2, <su>xt<ALLX:size> %3"
  [(set_attr "type" "alus_ext")]
)

(define_insn "*adds_<optab><mode>_multp2"
  [(set (reg:CC_NZ CC_REGNUM)
	(compare:CC_NZ
	 (plus:GPI (ANY_EXTRACT:GPI
		    (mult:GPI (match_operand:GPI 1 "register_operand" "r")
			      (match_operand 2 "aarch64_pwr_imm3" "Up3"))
		    (match_operand 3 "const_int_operand" "n")
		    (const_int 0))
		   (match_operand:GPI 4 "register_operand" "r"))
	(const_int 0)))
   (set (match_operand:GPI 0 "register_operand" "=r")
	(plus:GPI (ANY_EXTRACT:GPI (mult:GPI (match_dup 1) (match_dup 2))
				   (match_dup 3)
				   (const_int 0))
		  (match_dup 4)))]
  "aarch64_is_extend_from_extract (<MODE>mode, operands[2], operands[3])"
  "adds\\t%<w>0, %<w>4, %<w>1, <su>xt%e3 %p2"
  [(set_attr "type" "alus_ext")]
)

(define_insn "*subs_<optab><mode>_multp2"
  [(set (reg:CC_NZ CC_REGNUM)
	(compare:CC_NZ
	 (minus:GPI (match_operand:GPI 4 "register_operand" "r")
		    (ANY_EXTRACT:GPI
		     (mult:GPI (match_operand:GPI 1 "register_operand" "r")
			       (match_operand 2 "aarch64_pwr_imm3" "Up3"))
		     (match_operand 3 "const_int_operand" "n")
		     (const_int 0)))
	(const_int 0)))
   (set (match_operand:GPI 0 "register_operand" "=r")
	(minus:GPI (match_dup 4) (ANY_EXTRACT:GPI
				  (mult:GPI (match_dup 1) (match_dup 2))
				  (match_dup 3)
				  (const_int 0))))]
  "aarch64_is_extend_from_extract (<MODE>mode, operands[2], operands[3])"
  "subs\\t%<w>0, %<w>4, %<w>1, <su>xt%e3 %p2"
  [(set_attr "type" "alus_ext")]
)

(define_insn "*add<mode>3nr_compare0"
  [(set (reg:CC_NZ CC_REGNUM)
	(compare:CC_NZ
	 (plus:GPI (match_operand:GPI 0 "register_operand" "%r,r,r")
		   (match_operand:GPI 1 "aarch64_plus_operand" "r,I,J"))
	 (const_int 0)))]
  ""
  "@
  cmn\\t%<w>0, %<w>1
  cmn\\t%<w>0, %1
  cmp\\t%<w>0, #%n1"
  [(set_attr "type" "alus_sreg,alus_imm,alus_imm")]
)

(define_insn "*compare_neg<mode>"
  [(set (reg:CC_Z CC_REGNUM)
	(compare:CC_Z
	 (neg:GPI (match_operand:GPI 0 "register_operand" "r"))
	 (match_operand:GPI 1 "register_operand" "r")))]
  ""
  "cmn\\t%<w>1, %<w>0"
  [(set_attr "type" "alus_sreg")]
)

(define_insn "*add_<shift>_<mode>"
  [(set (match_operand:GPI 0 "register_operand" "=r")
	(plus:GPI (ASHIFT:GPI (match_operand:GPI 1 "register_operand" "r")
			      (match_operand:QI 2 "aarch64_shift_imm_<mode>" "n"))
		  (match_operand:GPI 3 "register_operand" "r")))]
  ""
  "add\\t%<w>0, %<w>3, %<w>1, <shift> %2"
  [(set_attr "type" "alu_shift_imm")]
)

;; zero_extend version of above
(define_insn "*add_<shift>_si_uxtw"
  [(set (match_operand:DI 0 "register_operand" "=r")
	(zero_extend:DI
	 (plus:SI (ASHIFT:SI (match_operand:SI 1 "register_operand" "r")
			     (match_operand:QI 2 "aarch64_shift_imm_si" "n"))
		  (match_operand:SI 3 "register_operand" "r"))))]
  ""
  "add\\t%w0, %w3, %w1, <shift> %2"
  [(set_attr "type" "alu_shift_imm")]
)

(define_insn "*add_mul_imm_<mode>"
  [(set (match_operand:GPI 0 "register_operand" "=r")
	(plus:GPI (mult:GPI (match_operand:GPI 1 "register_operand" "r")
			    (match_operand:QI 2 "aarch64_pwr_2_<mode>" "n"))
		  (match_operand:GPI 3 "register_operand" "r")))]
  ""
  "add\\t%<w>0, %<w>3, %<w>1, lsl %p2"
  [(set_attr "type" "alu_shift_imm")]
)

(define_insn "*add_<optab><ALLX:mode>_<GPI:mode>"
  [(set (match_operand:GPI 0 "register_operand" "=rk")
	(plus:GPI (ANY_EXTEND:GPI (match_operand:ALLX 1 "register_operand" "r"))
		  (match_operand:GPI 2 "register_operand" "r")))]
  ""
  "add\\t%<GPI:w>0, %<GPI:w>2, %<GPI:w>1, <su>xt<ALLX:size>"
  [(set_attr "type" "alu_ext")]
)

;; zero_extend version of above
(define_insn "*add_<optab><SHORT:mode>_si_uxtw"
  [(set (match_operand:DI 0 "register_operand" "=rk")
	(zero_extend:DI
         (plus:SI (ANY_EXTEND:SI (match_operand:SHORT 1 "register_operand" "r"))
		  (match_operand:GPI 2 "register_operand" "r"))))]
  ""
  "add\\t%w0, %w2, %w1, <su>xt<SHORT:size>"
  [(set_attr "type" "alu_ext")]
)

(define_insn "*add_<optab><ALLX:mode>_shft_<GPI:mode>"
  [(set (match_operand:GPI 0 "register_operand" "=rk")
	(plus:GPI (ashift:GPI (ANY_EXTEND:GPI
			       (match_operand:ALLX 1 "register_operand" "r"))
			      (match_operand 2 "aarch64_imm3" "Ui3"))
		  (match_operand:GPI 3 "register_operand" "r")))]
  ""
  "add\\t%<GPI:w>0, %<GPI:w>3, %<GPI:w>1, <su>xt<ALLX:size> %2"
  [(set_attr "type" "alu_ext")]
)

;; zero_extend version of above
(define_insn "*add_<optab><SHORT:mode>_shft_si_uxtw"
  [(set (match_operand:DI 0 "register_operand" "=rk")
	(zero_extend:DI
	 (plus:SI (ashift:SI (ANY_EXTEND:SI
			      (match_operand:SHORT 1 "register_operand" "r"))
			     (match_operand 2 "aarch64_imm3" "Ui3"))
		  (match_operand:SI 3 "register_operand" "r"))))]
  ""
  "add\\t%w0, %w3, %w1, <su>xt<SHORT:size> %2"
  [(set_attr "type" "alu_ext")]
)

(define_insn "*add_<optab><ALLX:mode>_mult_<GPI:mode>"
  [(set (match_operand:GPI 0 "register_operand" "=rk")
	(plus:GPI (mult:GPI (ANY_EXTEND:GPI
			     (match_operand:ALLX 1 "register_operand" "r"))
			    (match_operand 2 "aarch64_pwr_imm3" "Up3"))
		  (match_operand:GPI 3 "register_operand" "r")))]
  ""
  "add\\t%<GPI:w>0, %<GPI:w>3, %<GPI:w>1, <su>xt<ALLX:size> %p2"
  [(set_attr "type" "alu_ext")]
)

;; zero_extend version of above
(define_insn "*add_<optab><SHORT:mode>_mult_si_uxtw"
  [(set (match_operand:DI 0 "register_operand" "=rk")
	(zero_extend:DI (plus:SI (mult:SI (ANY_EXTEND:SI
			     (match_operand:SHORT 1 "register_operand" "r"))
			    (match_operand 2 "aarch64_pwr_imm3" "Up3"))
		  (match_operand:SI 3 "register_operand" "r"))))]
  ""
  "add\\t%w0, %w3, %w1, <su>xt<SHORT:size> %p2"
  [(set_attr "type" "alu_ext")]
)

(define_insn "*add_<optab><mode>_multp2"
  [(set (match_operand:GPI 0 "register_operand" "=rk")
	(plus:GPI (ANY_EXTRACT:GPI
		   (mult:GPI (match_operand:GPI 1 "register_operand" "r")
			     (match_operand 2 "aarch64_pwr_imm3" "Up3"))
		   (match_operand 3 "const_int_operand" "n")
		   (const_int 0))
		  (match_operand:GPI 4 "register_operand" "r")))]
  "aarch64_is_extend_from_extract (<MODE>mode, operands[2], operands[3])"
  "add\\t%<w>0, %<w>4, %<w>1, <su>xt%e3 %p2"
  [(set_attr "type" "alu_ext")]
)

;; zero_extend version of above
(define_insn "*add_<optab>si_multp2_uxtw"
  [(set (match_operand:DI 0 "register_operand" "=rk")
	(zero_extend:DI
         (plus:SI (ANY_EXTRACT:SI
		   (mult:SI (match_operand:SI 1 "register_operand" "r")
			    (match_operand 2 "aarch64_pwr_imm3" "Up3"))
		   (match_operand 3 "const_int_operand" "n")
                   (const_int 0))
		  (match_operand:SI 4 "register_operand" "r"))))]
  "aarch64_is_extend_from_extract (SImode, operands[2], operands[3])"
  "add\\t%w0, %w4, %w1, <su>xt%e3 %p2"
  [(set_attr "type" "alu_ext")]
)

(define_expand "add<mode>3_carryin"
  [(set (match_operand:GPI 0 "register_operand")
	(plus:GPI
	  (plus:GPI
	    (ne:GPI (reg:CC_C CC_REGNUM) (const_int 0))
	    (match_operand:GPI 1 "aarch64_reg_or_zero"))
	  (match_operand:GPI 2 "aarch64_reg_or_zero")))]
   ""
   ""
)

;; Note that add with carry with two zero inputs is matched by cset,
;; and that add with carry with one zero input is matched by cinc.

(define_insn "*add<mode>3_carryin"
  [(set (match_operand:GPI 0 "register_operand" "=r")
	(plus:GPI
	  (plus:GPI
	    (match_operand:GPI 3 "aarch64_carry_operation" "")
	    (match_operand:GPI 1 "register_operand" "r"))
	  (match_operand:GPI 2 "register_operand" "r")))]
   ""
   "adc\\t%<w>0, %<w>1, %<w>2"
  [(set_attr "type" "adc_reg")]
)

;; zero_extend version of above
(define_insn "*addsi3_carryin_uxtw"
  [(set (match_operand:DI 0 "register_operand" "=r")
	(zero_extend:DI
	  (plus:SI
	    (plus:SI
	      (match_operand:SI 3 "aarch64_carry_operation" "")
	      (match_operand:SI 1 "register_operand" "r"))
	    (match_operand:SI 2 "register_operand" "r"))))]
   ""
   "adc\\t%w0, %w1, %w2"
  [(set_attr "type" "adc_reg")]
)

(define_insn "*add_uxt<mode>_shift2"
  [(set (match_operand:GPI 0 "register_operand" "=rk")
	(plus:GPI (and:GPI
		   (ashift:GPI (match_operand:GPI 1 "register_operand" "r")
			       (match_operand 2 "aarch64_imm3" "Ui3"))
		   (match_operand 3 "const_int_operand" "n"))
		  (match_operand:GPI 4 "register_operand" "r")))]
  "aarch64_uxt_size (INTVAL (operands[2]), INTVAL (operands[3])) != 0"
  "*
  operands[3] = GEN_INT (aarch64_uxt_size (INTVAL(operands[2]),
					   INTVAL (operands[3])));
  return \"add\t%<w>0, %<w>4, %<w>1, uxt%e3 %2\";"
  [(set_attr "type" "alu_ext")]
)

;; zero_extend version of above
(define_insn "*add_uxtsi_shift2_uxtw"
  [(set (match_operand:DI 0 "register_operand" "=rk")
	(zero_extend:DI
	 (plus:SI (and:SI
		   (ashift:SI (match_operand:SI 1 "register_operand" "r")
			      (match_operand 2 "aarch64_imm3" "Ui3"))
		   (match_operand 3 "const_int_operand" "n"))
		  (match_operand:SI 4 "register_operand" "r"))))]
  "aarch64_uxt_size (INTVAL (operands[2]), INTVAL (operands[3])) != 0"
  "*
  operands[3] = GEN_INT (aarch64_uxt_size (INTVAL (operands[2]),
					   INTVAL (operands[3])));
  return \"add\t%w0, %w4, %w1, uxt%e3 %2\";"
  [(set_attr "type" "alu_ext")]
)

(define_insn "*add_uxt<mode>_multp2"
  [(set (match_operand:GPI 0 "register_operand" "=rk")
	(plus:GPI (and:GPI
		   (mult:GPI (match_operand:GPI 1 "register_operand" "r")
			     (match_operand 2 "aarch64_pwr_imm3" "Up3"))
		   (match_operand 3 "const_int_operand" "n"))
		  (match_operand:GPI 4 "register_operand" "r")))]
  "aarch64_uxt_size (exact_log2 (INTVAL (operands[2])), INTVAL (operands[3])) != 0"
  "*
  operands[3] = GEN_INT (aarch64_uxt_size (exact_log2 (INTVAL (operands[2])),
					   INTVAL (operands[3])));
  return \"add\t%<w>0, %<w>4, %<w>1, uxt%e3 %p2\";"
  [(set_attr "type" "alu_ext")]
)

;; zero_extend version of above
(define_insn "*add_uxtsi_multp2_uxtw"
  [(set (match_operand:DI 0 "register_operand" "=rk")
	(zero_extend:DI
         (plus:SI (and:SI
		   (mult:SI (match_operand:SI 1 "register_operand" "r")
			    (match_operand 2 "aarch64_pwr_imm3" "Up3"))
		   (match_operand 3 "const_int_operand" "n"))
		  (match_operand:SI 4 "register_operand" "r"))))]
  "aarch64_uxt_size (exact_log2 (INTVAL (operands[2])), INTVAL (operands[3])) != 0"
  "*
  operands[3] = GEN_INT (aarch64_uxt_size (exact_log2 (INTVAL (operands[2])),
					   INTVAL (operands[3])));
  return \"add\t%w0, %w4, %w1, uxt%e3 %p2\";"
  [(set_attr "type" "alu_ext")]
)

(define_insn "subsi3"
  [(set (match_operand:SI 0 "register_operand" "=rk")
	(minus:SI (match_operand:SI 1 "register_operand" "rk")
		  (match_operand:SI 2 "register_operand" "r")))]
  ""
  "sub\\t%w0, %w1, %w2"
  [(set_attr "type" "alu_sreg")]
)

;; zero_extend version of above
(define_insn "*subsi3_uxtw"
  [(set (match_operand:DI 0 "register_operand" "=rk")
	(zero_extend:DI
         (minus:SI (match_operand:SI 1 "register_operand" "rk")
		   (match_operand:SI 2 "register_operand" "r"))))]
  ""
  "sub\\t%w0, %w1, %w2"
  [(set_attr "type" "alu_sreg")]
)

(define_insn "subdi3"
  [(set (match_operand:DI 0 "register_operand" "=rk,w")
	(minus:DI (match_operand:DI 1 "register_operand" "rk,w")
		  (match_operand:DI 2 "register_operand" "r,w")))]
  ""
  "@
   sub\\t%x0, %x1, %x2
   sub\\t%d0, %d1, %d2"
  [(set_attr "type" "alu_sreg, neon_sub")
   (set_attr "simd" "*,yes")]
)

(define_expand "subti3"
  [(set (match_operand:TI 0 "register_operand" "")
	(minus:TI (match_operand:TI 1 "register_operand" "")
		  (match_operand:TI 2 "register_operand" "")))]
  ""
{
  rtx low = gen_reg_rtx (DImode);
  emit_insn (gen_subdi3_compare1 (low, gen_lowpart (DImode, operands[1]),
				  gen_lowpart (DImode, operands[2])));

  rtx high = gen_reg_rtx (DImode);
  emit_insn (gen_subdi3_carryin (high, gen_highpart (DImode, operands[1]),
				 gen_highpart (DImode, operands[2])));

  emit_move_insn (gen_lowpart (DImode, operands[0]), low);
  emit_move_insn (gen_highpart (DImode, operands[0]), high);
  DONE;
})

(define_insn "*sub<mode>3_compare0"
  [(set (reg:CC_NZ CC_REGNUM)
	(compare:CC_NZ (minus:GPI (match_operand:GPI 1 "register_operand" "r")
				  (match_operand:GPI 2 "register_operand" "r"))
		       (const_int 0)))
   (set (match_operand:GPI 0 "register_operand" "=r")
	(minus:GPI (match_dup 1) (match_dup 2)))]
  ""
  "subs\\t%<w>0, %<w>1, %<w>2"
  [(set_attr "type" "alus_sreg")]
)

;; zero_extend version of above
(define_insn "*subsi3_compare0_uxtw"
  [(set (reg:CC_NZ CC_REGNUM)
	(compare:CC_NZ (minus:SI (match_operand:SI 1 "register_operand" "r")
				 (match_operand:SI 2 "register_operand" "r"))
		       (const_int 0)))
   (set (match_operand:DI 0 "register_operand" "=r")
	(zero_extend:DI (minus:SI (match_dup 1) (match_dup 2))))]
  ""
  "subs\\t%w0, %w1, %w2"
  [(set_attr "type" "alus_sreg")]
)

(define_insn "sub<mode>3_compare1"
  [(set (reg:CC CC_REGNUM)
	(compare:CC
	  (match_operand:GPI 1 "aarch64_reg_or_zero" "rZ")
	  (match_operand:GPI 2 "aarch64_reg_or_zero" "rZ")))
   (set (match_operand:GPI 0 "register_operand" "=r")
	(minus:GPI (match_dup 1) (match_dup 2)))]
  ""
  "subs\\t%<w>0, %<w>1, %<w>2"
  [(set_attr "type" "alus_sreg")]
)

(define_insn "*sub_<shift>_<mode>"
  [(set (match_operand:GPI 0 "register_operand" "=r")
	(minus:GPI (match_operand:GPI 3 "register_operand" "r")
		   (ASHIFT:GPI
		    (match_operand:GPI 1 "register_operand" "r")
		    (match_operand:QI 2 "aarch64_shift_imm_<mode>" "n"))))]
  ""
  "sub\\t%<w>0, %<w>3, %<w>1, <shift> %2"
  [(set_attr "type" "alu_shift_imm")]
)

;; zero_extend version of above
(define_insn "*sub_<shift>_si_uxtw"
  [(set (match_operand:DI 0 "register_operand" "=r")
	(zero_extend:DI
         (minus:SI (match_operand:SI 3 "register_operand" "r")
		   (ASHIFT:SI
		    (match_operand:SI 1 "register_operand" "r")
		    (match_operand:QI 2 "aarch64_shift_imm_si" "n")))))]
  ""
  "sub\\t%w0, %w3, %w1, <shift> %2"
  [(set_attr "type" "alu_shift_imm")]
)

(define_insn "*sub_mul_imm_<mode>"
  [(set (match_operand:GPI 0 "register_operand" "=r")
	(minus:GPI (match_operand:GPI 3 "register_operand" "r")
		   (mult:GPI
		    (match_operand:GPI 1 "register_operand" "r")
		    (match_operand:QI 2 "aarch64_pwr_2_<mode>" "n"))))]
  ""
  "sub\\t%<w>0, %<w>3, %<w>1, lsl %p2"
  [(set_attr "type" "alu_shift_imm")]
)

;; zero_extend version of above
(define_insn "*sub_mul_imm_si_uxtw"
  [(set (match_operand:DI 0 "register_operand" "=r")
	(zero_extend:DI
         (minus:SI (match_operand:SI 3 "register_operand" "r")
		   (mult:SI
		    (match_operand:SI 1 "register_operand" "r")
		    (match_operand:QI 2 "aarch64_pwr_2_si" "n")))))]
  ""
  "sub\\t%w0, %w3, %w1, lsl %p2"
  [(set_attr "type" "alu_shift_imm")]
)

(define_insn "*sub_<optab><ALLX:mode>_<GPI:mode>"
  [(set (match_operand:GPI 0 "register_operand" "=rk")
	(minus:GPI (match_operand:GPI 1 "register_operand" "rk")
		   (ANY_EXTEND:GPI
		    (match_operand:ALLX 2 "register_operand" "r"))))]
  ""
  "sub\\t%<GPI:w>0, %<GPI:w>1, %<GPI:w>2, <su>xt<ALLX:size>"
  [(set_attr "type" "alu_ext")]
)

;; zero_extend version of above
(define_insn "*sub_<optab><SHORT:mode>_si_uxtw"
  [(set (match_operand:DI 0 "register_operand" "=rk")
	(zero_extend:DI
         (minus:SI (match_operand:SI 1 "register_operand" "rk")
		   (ANY_EXTEND:SI
		    (match_operand:SHORT 2 "register_operand" "r")))))]
  ""
  "sub\\t%w0, %w1, %w2, <su>xt<SHORT:size>"
  [(set_attr "type" "alu_ext")]
)

(define_insn "*sub_<optab><ALLX:mode>_shft_<GPI:mode>"
  [(set (match_operand:GPI 0 "register_operand" "=rk")
	(minus:GPI (match_operand:GPI 1 "register_operand" "rk")
		   (ashift:GPI (ANY_EXTEND:GPI
				(match_operand:ALLX 2 "register_operand" "r"))
			       (match_operand 3 "aarch64_imm3" "Ui3"))))]
  ""
  "sub\\t%<GPI:w>0, %<GPI:w>1, %<GPI:w>2, <su>xt<ALLX:size> %3"
  [(set_attr "type" "alu_ext")]
)

;; zero_extend version of above
(define_insn "*sub_<optab><SHORT:mode>_shft_si_uxtw"
  [(set (match_operand:DI 0 "register_operand" "=rk")
	(zero_extend:DI
         (minus:SI (match_operand:SI 1 "register_operand" "rk")
		   (ashift:SI (ANY_EXTEND:SI
			       (match_operand:SHORT 2 "register_operand" "r"))
			      (match_operand 3 "aarch64_imm3" "Ui3")))))]
  ""
  "sub\\t%w0, %w1, %w2, <su>xt<SHORT:size> %3"
  [(set_attr "type" "alu_ext")]
)

(define_insn "*sub_<optab><mode>_multp2"
  [(set (match_operand:GPI 0 "register_operand" "=rk")
	(minus:GPI (match_operand:GPI 4 "register_operand" "rk")
		   (ANY_EXTRACT:GPI
		    (mult:GPI (match_operand:GPI 1 "register_operand" "r")
			      (match_operand 2 "aarch64_pwr_imm3" "Up3"))
		    (match_operand 3 "const_int_operand" "n")
		    (const_int 0))))]
  "aarch64_is_extend_from_extract (<MODE>mode, operands[2], operands[3])"
  "sub\\t%<w>0, %<w>4, %<w>1, <su>xt%e3 %p2"
  [(set_attr "type" "alu_ext")]
)

;; zero_extend version of above
(define_insn "*sub_<optab>si_multp2_uxtw"
  [(set (match_operand:DI 0 "register_operand" "=rk")
	(zero_extend:DI
         (minus:SI (match_operand:SI 4 "register_operand" "rk")
		   (ANY_EXTRACT:SI
		    (mult:SI (match_operand:SI 1 "register_operand" "r")
			     (match_operand 2 "aarch64_pwr_imm3" "Up3"))
		    (match_operand 3 "const_int_operand" "n")
		    (const_int 0)))))]
  "aarch64_is_extend_from_extract (SImode, operands[2], operands[3])"
  "sub\\t%w0, %w4, %w1, <su>xt%e3 %p2"
  [(set_attr "type" "alu_ext")]
)

;; The hardware description is op1 + ~op2 + C.
;;                           = op1 + (-op2 + 1) + (1 - !C)
;;                           = op1 - op2 - 1 + 1 - !C
;;                           = op1 - op2 - !C.
;; We describe the latter.

(define_insn "*sub<mode>3_carryin0"
  [(set (match_operand:GPI 0 "register_operand" "=r")
	(minus:GPI
	  (match_operand:GPI 1 "aarch64_reg_or_zero" "rZ")
	  (match_operand:GPI 2 "aarch64_borrow_operation" "")))]
   ""
   "sbc\\t%<w>0, %<w>1, <w>zr"
  [(set_attr "type" "adc_reg")]
)

;; zero_extend version of the above
(define_insn "*subsi3_carryin_uxtw"
  [(set (match_operand:DI 0 "register_operand" "=r")
	(zero_extend:DI
	  (minus:SI
	    (match_operand:SI 1 "aarch64_reg_or_zero" "rZ")
	    (match_operand:SI 2 "aarch64_borrow_operation" ""))))]
   ""
   "sbc\\t%w0, %w1, wzr"
  [(set_attr "type" "adc_reg")]
)

(define_expand "sub<mode>3_carryin"
  [(set (match_operand:GPI 0 "register_operand")
	(minus:GPI
	  (minus:GPI
	    (match_operand:GPI 1 "aarch64_reg_or_zero")
	    (match_operand:GPI 2 "register_operand"))
	  (ltu:GPI (reg:CC CC_REGNUM) (const_int 0))))]
   ""
   ""
)

(define_insn "*sub<mode>3_carryin"
  [(set (match_operand:GPI 0 "register_operand" "=r")
	(minus:GPI
	  (minus:GPI
	    (match_operand:GPI 1 "aarch64_reg_or_zero" "rZ")
	    (match_operand:GPI 2 "register_operand" "r"))
	  (match_operand:GPI 3 "aarch64_borrow_operation" "")))]

   ""
   "sbc\\t%<w>0, %<w>1, %<w>2"
  [(set_attr "type" "adc_reg")]
)

;; zero_extend version of the above
(define_insn "*subsi3_carryin_uxtw"
  [(set (match_operand:DI 0 "register_operand" "=r")
	(zero_extend:DI
	  (minus:SI
	    (minus:SI
	      (match_operand:SI 1 "aarch64_reg_or_zero" "rZ")
	      (match_operand:SI 2 "register_operand" "r"))
	    (match_operand:SI 3 "aarch64_borrow_operation" ""))))]

   ""
   "sbc\\t%w0, %w1, %w2"
  [(set_attr "type" "adc_reg")]
)

(define_insn "*sub<mode>3_carryin_alt"
  [(set (match_operand:GPI 0 "register_operand" "=r")
	(minus:GPI
	  (minus:GPI
	    (match_operand:GPI 1 "aarch64_reg_or_zero" "rZ")
	    (match_operand:GPI 3 "aarch64_borrow_operation" ""))
	  (match_operand:GPI 2 "register_operand" "r")))]
   ""
   "sbc\\t%<w>0, %<w>1, %<w>2"
  [(set_attr "type" "adc_reg")]
)

;; zero_extend version of the above
(define_insn "*subsi3_carryin_alt_uxtw"
  [(set (match_operand:DI 0 "register_operand" "=r")
	(zero_extend:DI
	  (minus:SI
	    (minus:SI
	      (match_operand:SI 1 "aarch64_reg_or_zero" "rZ")
	      (match_operand:SI 3 "aarch64_borrow_operation" ""))
	    (match_operand:SI 2 "register_operand" "r"))))]
   ""
   "sbc\\t%w0, %w1, %w2"
  [(set_attr "type" "adc_reg")]
)

(define_insn "*sub_uxt<mode>_shift2"
  [(set (match_operand:GPI 0 "register_operand" "=rk")
	(minus:GPI (match_operand:GPI 4 "register_operand" "rk")
		   (and:GPI
		    (ashift:GPI (match_operand:GPI 1 "register_operand" "r")
				(match_operand 2 "aarch64_imm3" "Ui3"))
		    (match_operand 3 "const_int_operand" "n"))))]
  "aarch64_uxt_size (INTVAL (operands[2]),INTVAL (operands[3])) != 0"
  "*
  operands[3] = GEN_INT (aarch64_uxt_size (INTVAL (operands[2]),
					   INTVAL (operands[3])));
  return \"sub\t%<w>0, %<w>4, %<w>1, uxt%e3 %2\";"
  [(set_attr "type" "alu_ext")]
)

;; zero_extend version of above
(define_insn "*sub_uxtsi_shift2_uxtw"
  [(set (match_operand:DI 0 "register_operand" "=rk")
	(zero_extend:DI
	 (minus:SI (match_operand:SI 4 "register_operand" "rk")
		   (and:SI
		    (ashift:SI (match_operand:SI 1 "register_operand" "r")
			       (match_operand 2 "aarch64_imm3" "Ui3"))
		    (match_operand 3 "const_int_operand" "n")))))]
  "aarch64_uxt_size (INTVAL (operands[2]),INTVAL (operands[3])) != 0"
  "*
  operands[3] = GEN_INT (aarch64_uxt_size (INTVAL (operands[2]),
					   INTVAL (operands[3])));
  return \"sub\t%w0, %w4, %w1, uxt%e3 %2\";"
  [(set_attr "type" "alu_ext")]
)

(define_insn "*sub_uxt<mode>_multp2"
  [(set (match_operand:GPI 0 "register_operand" "=rk")
	(minus:GPI (match_operand:GPI 4 "register_operand" "rk")
		   (and:GPI
		    (mult:GPI (match_operand:GPI 1 "register_operand" "r")
			      (match_operand 2 "aarch64_pwr_imm3" "Up3"))
		    (match_operand 3 "const_int_operand" "n"))))]
  "aarch64_uxt_size (exact_log2 (INTVAL (operands[2])),INTVAL (operands[3])) != 0"
  "*
  operands[3] = GEN_INT (aarch64_uxt_size (exact_log2 (INTVAL (operands[2])),
					   INTVAL (operands[3])));
  return \"sub\t%<w>0, %<w>4, %<w>1, uxt%e3 %p2\";"
  [(set_attr "type" "alu_ext")]
)

;; zero_extend version of above
(define_insn "*sub_uxtsi_multp2_uxtw"
  [(set (match_operand:DI 0 "register_operand" "=rk")
	(zero_extend:DI
         (minus:SI (match_operand:SI 4 "register_operand" "rk")
		   (and:SI
		    (mult:SI (match_operand:SI 1 "register_operand" "r")
			     (match_operand 2 "aarch64_pwr_imm3" "Up3"))
		    (match_operand 3 "const_int_operand" "n")))))]
  "aarch64_uxt_size (exact_log2 (INTVAL (operands[2])),INTVAL (operands[3])) != 0"
  "*
  operands[3] = GEN_INT (aarch64_uxt_size (exact_log2 (INTVAL (operands[2])),
					   INTVAL (operands[3])));
  return \"sub\t%w0, %w4, %w1, uxt%e3 %p2\";"
  [(set_attr "type" "alu_ext")]
)

(define_expand "abs<mode>2"
  [(match_operand:GPI 0 "register_operand" "")
   (match_operand:GPI 1 "register_operand" "")]
  ""
  {
    rtx ccreg = aarch64_gen_compare_reg (LT, operands[1], const0_rtx);
    rtx x = gen_rtx_LT (VOIDmode, ccreg, const0_rtx);
    emit_insn (gen_csneg3<mode>_insn (operands[0], x, operands[1], operands[1]));
    DONE;
  }
)

(define_insn "neg<mode>2"
  [(set (match_operand:GPI 0 "register_operand" "=r,w")
	(neg:GPI (match_operand:GPI 1 "register_operand" "r,w")))]
  ""
  "@
   neg\\t%<w>0, %<w>1
   neg\\t%<rtn>0<vas>, %<rtn>1<vas>"
  [(set_attr "type" "alu_sreg, neon_neg<q>")
   (set_attr "simd" "*,yes")]
)

;; zero_extend version of above
(define_insn "*negsi2_uxtw"
  [(set (match_operand:DI 0 "register_operand" "=r")
	(zero_extend:DI (neg:SI (match_operand:SI 1 "register_operand" "r"))))]
  ""
  "neg\\t%w0, %w1"
  [(set_attr "type" "alu_sreg")]
)

(define_insn "*ngc<mode>"
  [(set (match_operand:GPI 0 "register_operand" "=r")
	(minus:GPI
	  (neg:GPI (match_operand:GPI 2 "aarch64_borrow_operation" ""))
	  (match_operand:GPI 1 "register_operand" "r")))]
  ""
  "ngc\\t%<w>0, %<w>1"
  [(set_attr "type" "adc_reg")]
)

(define_insn "*ngcsi_uxtw"
  [(set (match_operand:DI 0 "register_operand" "=r")
	(zero_extend:DI
	  (minus:SI
	    (neg:SI (match_operand:SI 2 "aarch64_borrow_operation" ""))
	    (match_operand:SI 1 "register_operand" "r"))))]
  ""
  "ngc\\t%w0, %w1"
  [(set_attr "type" "adc_reg")]
)

(define_insn "neg<mode>2_compare0"
  [(set (reg:CC_NZ CC_REGNUM)
	(compare:CC_NZ (neg:GPI (match_operand:GPI 1 "register_operand" "r"))
		       (const_int 0)))
   (set (match_operand:GPI 0 "register_operand" "=r")
	(neg:GPI (match_dup 1)))]
  ""
  "negs\\t%<w>0, %<w>1"
  [(set_attr "type" "alus_sreg")]
)

;; zero_extend version of above
(define_insn "*negsi2_compare0_uxtw"
  [(set (reg:CC_NZ CC_REGNUM)
	(compare:CC_NZ (neg:SI (match_operand:SI 1 "register_operand" "r"))
		       (const_int 0)))
   (set (match_operand:DI 0 "register_operand" "=r")
	(zero_extend:DI (neg:SI (match_dup 1))))]
  ""
  "negs\\t%w0, %w1"
  [(set_attr "type" "alus_sreg")]
)

(define_insn "*neg_<shift><mode>3_compare0"
  [(set (reg:CC_NZ CC_REGNUM)
	(compare:CC_NZ
	 (neg:GPI (ASHIFT:GPI
		   (match_operand:GPI 1 "register_operand" "r")
		   (match_operand:QI 2 "aarch64_shift_imm_<mode>" "n")))
	 (const_int 0)))
   (set (match_operand:GPI 0 "register_operand" "=r")
	(neg:GPI (ASHIFT:GPI (match_dup 1) (match_dup 2))))]
  ""
  "negs\\t%<w>0, %<w>1, <shift> %2"
  [(set_attr "type" "alus_shift_imm")]
)

(define_insn "*neg_<shift>_<mode>2"
  [(set (match_operand:GPI 0 "register_operand" "=r")
	(neg:GPI (ASHIFT:GPI
		  (match_operand:GPI 1 "register_operand" "r")
		  (match_operand:QI 2 "aarch64_shift_imm_<mode>" "n"))))]
  ""
  "neg\\t%<w>0, %<w>1, <shift> %2"
  [(set_attr "type" "alu_shift_imm")]
)

;; zero_extend version of above
(define_insn "*neg_<shift>_si2_uxtw"
  [(set (match_operand:DI 0 "register_operand" "=r")
	(zero_extend:DI
         (neg:SI (ASHIFT:SI
		  (match_operand:SI 1 "register_operand" "r")
		  (match_operand:QI 2 "aarch64_shift_imm_si" "n")))))]
  ""
  "neg\\t%w0, %w1, <shift> %2"
  [(set_attr "type" "alu_shift_imm")]
)

(define_insn "*neg_mul_imm_<mode>2"
  [(set (match_operand:GPI 0 "register_operand" "=r")
	(neg:GPI (mult:GPI
		  (match_operand:GPI 1 "register_operand" "r")
		  (match_operand:QI 2 "aarch64_pwr_2_<mode>" "n"))))]
  ""
  "neg\\t%<w>0, %<w>1, lsl %p2"
  [(set_attr "type" "alu_shift_imm")]
)

;; zero_extend version of above
(define_insn "*neg_mul_imm_si2_uxtw"
  [(set (match_operand:DI 0 "register_operand" "=r")
	(zero_extend:DI
         (neg:SI (mult:SI
		  (match_operand:SI 1 "register_operand" "r")
		  (match_operand:QI 2 "aarch64_pwr_2_si" "n")))))]
  ""
  "neg\\t%w0, %w1, lsl %p2"
  [(set_attr "type" "alu_shift_imm")]
)

(define_insn "mul<mode>3"
  [(set (match_operand:GPI 0 "register_operand" "=r")
	(mult:GPI (match_operand:GPI 1 "register_operand" "r")
		  (match_operand:GPI 2 "register_operand" "r")))]
  ""
  "mul\\t%<w>0, %<w>1, %<w>2"
  [(set_attr "type" "mul")]
)

;; zero_extend version of above
(define_insn "*mulsi3_uxtw"
  [(set (match_operand:DI 0 "register_operand" "=r")
	(zero_extend:DI
         (mult:SI (match_operand:SI 1 "register_operand" "r")
		  (match_operand:SI 2 "register_operand" "r"))))]
  ""
  "mul\\t%w0, %w1, %w2"
  [(set_attr "type" "mul")]
)

(define_insn "madd<mode>"
  [(set (match_operand:GPI 0 "register_operand" "=r")
	(plus:GPI (mult:GPI (match_operand:GPI 1 "register_operand" "r")
			    (match_operand:GPI 2 "register_operand" "r"))
		  (match_operand:GPI 3 "register_operand" "r")))]
  ""
  "madd\\t%<w>0, %<w>1, %<w>2, %<w>3"
  [(set_attr "type" "mla")]
)

;; zero_extend version of above
(define_insn "*maddsi_uxtw"
  [(set (match_operand:DI 0 "register_operand" "=r")
	(zero_extend:DI
         (plus:SI (mult:SI (match_operand:SI 1 "register_operand" "r")
			   (match_operand:SI 2 "register_operand" "r"))
		  (match_operand:SI 3 "register_operand" "r"))))]
  ""
  "madd\\t%w0, %w1, %w2, %w3"
  [(set_attr "type" "mla")]
)

(define_insn "*msub<mode>"
  [(set (match_operand:GPI 0 "register_operand" "=r")
	(minus:GPI (match_operand:GPI 3 "register_operand" "r")
		   (mult:GPI (match_operand:GPI 1 "register_operand" "r")
			     (match_operand:GPI 2 "register_operand" "r"))))]

  ""
  "msub\\t%<w>0, %<w>1, %<w>2, %<w>3"
  [(set_attr "type" "mla")]
)

;; zero_extend version of above
(define_insn "*msubsi_uxtw"
  [(set (match_operand:DI 0 "register_operand" "=r")
	(zero_extend:DI
         (minus:SI (match_operand:SI 3 "register_operand" "r")
		   (mult:SI (match_operand:SI 1 "register_operand" "r")
			    (match_operand:SI 2 "register_operand" "r")))))]

  ""
  "msub\\t%w0, %w1, %w2, %w3"
  [(set_attr "type" "mla")]
)

(define_insn "*mul<mode>_neg"
  [(set (match_operand:GPI 0 "register_operand" "=r")
	(mult:GPI (neg:GPI (match_operand:GPI 1 "register_operand" "r"))
		  (match_operand:GPI 2 "register_operand" "r")))]

  ""
  "mneg\\t%<w>0, %<w>1, %<w>2"
  [(set_attr "type" "mul")]
)

;; zero_extend version of above
(define_insn "*mulsi_neg_uxtw"
  [(set (match_operand:DI 0 "register_operand" "=r")
	(zero_extend:DI
         (mult:SI (neg:SI (match_operand:SI 1 "register_operand" "r"))
		  (match_operand:SI 2 "register_operand" "r"))))]

  ""
  "mneg\\t%w0, %w1, %w2"
  [(set_attr "type" "mul")]
)

(define_insn "<su_optab>mulsidi3"
  [(set (match_operand:DI 0 "register_operand" "=r")
	(mult:DI (ANY_EXTEND:DI (match_operand:SI 1 "register_operand" "r"))
		 (ANY_EXTEND:DI (match_operand:SI 2 "register_operand" "r"))))]
  ""
  "<su>mull\\t%0, %w1, %w2"
  [(set_attr "type" "<su>mull")]
)

(define_insn "<su_optab>maddsidi4"
  [(set (match_operand:DI 0 "register_operand" "=r")
	(plus:DI (mult:DI
		  (ANY_EXTEND:DI (match_operand:SI 1 "register_operand" "r"))
		  (ANY_EXTEND:DI (match_operand:SI 2 "register_operand" "r")))
		 (match_operand:DI 3 "register_operand" "r")))]
  ""
  "<su>maddl\\t%0, %w1, %w2, %3"
  [(set_attr "type" "<su>mlal")]
)

(define_insn "<su_optab>msubsidi4"
  [(set (match_operand:DI 0 "register_operand" "=r")
	(minus:DI
	 (match_operand:DI 3 "register_operand" "r")
	 (mult:DI (ANY_EXTEND:DI (match_operand:SI 1 "register_operand" "r"))
		  (ANY_EXTEND:DI
		   (match_operand:SI 2 "register_operand" "r")))))]
  ""
  "<su>msubl\\t%0, %w1, %w2, %3"
  [(set_attr "type" "<su>mlal")]
)

(define_insn "*<su_optab>mulsidi_neg"
  [(set (match_operand:DI 0 "register_operand" "=r")
	(mult:DI (neg:DI
		  (ANY_EXTEND:DI (match_operand:SI 1 "register_operand" "r")))
		  (ANY_EXTEND:DI (match_operand:SI 2 "register_operand" "r"))))]
  ""
  "<su>mnegl\\t%0, %w1, %w2"
  [(set_attr "type" "<su>mull")]
)

(define_expand "<su_optab>mulditi3"
  [(set (match_operand:TI 0 "register_operand")
	(mult:TI (ANY_EXTEND:TI (match_operand:DI 1 "register_operand"))
		 (ANY_EXTEND:TI (match_operand:DI 2 "register_operand"))))]
  ""
{
  rtx low = gen_reg_rtx (DImode);
  emit_insn (gen_muldi3 (low, operands[1], operands[2]));

  rtx high = gen_reg_rtx (DImode);
  emit_insn (gen_<su>muldi3_highpart (high, operands[1], operands[2]));

  emit_move_insn (gen_lowpart (DImode, operands[0]), low);
  emit_move_insn (gen_highpart (DImode, operands[0]), high);
  DONE;
})

;; The default expansion of multi3 using umuldi3_highpart will perform
;; the additions in an order that fails to combine into two madd insns.
(define_expand "multi3"
  [(set (match_operand:TI 0 "register_operand")
	(mult:TI (match_operand:TI 1 "register_operand")
		 (match_operand:TI 2 "register_operand")))]
  ""
{
  rtx l0 = gen_reg_rtx (DImode);
  rtx l1 = gen_lowpart (DImode, operands[1]);
  rtx l2 = gen_lowpart (DImode, operands[2]);
  rtx h0 = gen_reg_rtx (DImode);
  rtx h1 = gen_highpart (DImode, operands[1]);
  rtx h2 = gen_highpart (DImode, operands[2]);

  emit_insn (gen_muldi3 (l0, l1, l2));
  emit_insn (gen_umuldi3_highpart (h0, l1, l2));
  emit_insn (gen_madddi (h0, h1, l2, h0));
  emit_insn (gen_madddi (h0, l1, h2, h0));

  emit_move_insn (gen_lowpart (DImode, operands[0]), l0);
  emit_move_insn (gen_highpart (DImode, operands[0]), h0);
  DONE;
})

(define_insn "<su>muldi3_highpart"
  [(set (match_operand:DI 0 "register_operand" "=r")
	(truncate:DI
	 (lshiftrt:TI
	  (mult:TI
	   (ANY_EXTEND:TI (match_operand:DI 1 "register_operand" "r"))
	   (ANY_EXTEND:TI (match_operand:DI 2 "register_operand" "r")))
	  (const_int 64))))]
  ""
  "<su>mulh\\t%0, %1, %2"
  [(set_attr "type" "<su>mull")]
)

(define_insn "<su_optab>div<mode>3"
  [(set (match_operand:GPI 0 "register_operand" "=r")
	(ANY_DIV:GPI (match_operand:GPI 1 "register_operand" "r")
		     (match_operand:GPI 2 "register_operand" "r")))]
  ""
  "<su>div\\t%<w>0, %<w>1, %<w>2"
  [(set_attr "type" "<su>div")]
)

;; zero_extend version of above
(define_insn "*<su_optab>divsi3_uxtw"
  [(set (match_operand:DI 0 "register_operand" "=r")
	(zero_extend:DI
         (ANY_DIV:SI (match_operand:SI 1 "register_operand" "r")
		     (match_operand:SI 2 "register_operand" "r"))))]
  ""
  "<su>div\\t%w0, %w1, %w2"
  [(set_attr "type" "<su>div")]
)

;; -------------------------------------------------------------------
;; Comparison insns
;; -------------------------------------------------------------------

(define_insn "cmp<mode>"
  [(set (reg:CC CC_REGNUM)
	(compare:CC (match_operand:GPI 0 "register_operand" "r,r,r")
		    (match_operand:GPI 1 "aarch64_plus_operand" "r,I,J")))]
  ""
  "@
   cmp\\t%<w>0, %<w>1
   cmp\\t%<w>0, %1
   cmn\\t%<w>0, #%n1"
  [(set_attr "type" "alus_sreg,alus_imm,alus_imm")]
)

(define_insn "fcmp<mode>"
  [(set (reg:CCFP CC_REGNUM)
        (compare:CCFP (match_operand:GPF 0 "register_operand" "w,w")
		      (match_operand:GPF 1 "aarch64_fp_compare_operand" "Y,w")))]
   "TARGET_FLOAT"
   "@
    fcmp\\t%<s>0, #0.0
    fcmp\\t%<s>0, %<s>1"
  [(set_attr "type" "fcmp<s>")]
)

(define_insn "fcmpe<mode>"
  [(set (reg:CCFPE CC_REGNUM)
        (compare:CCFPE (match_operand:GPF 0 "register_operand" "w,w")
		       (match_operand:GPF 1 "aarch64_fp_compare_operand" "Y,w")))]
   "TARGET_FLOAT"
   "@
    fcmpe\\t%<s>0, #0.0
    fcmpe\\t%<s>0, %<s>1"
  [(set_attr "type" "fcmp<s>")]
)

(define_insn "*cmp_swp_<shift>_reg<mode>"
  [(set (reg:CC_SWP CC_REGNUM)
	(compare:CC_SWP (ASHIFT:GPI
			 (match_operand:GPI 0 "register_operand" "r")
			 (match_operand:QI 1 "aarch64_shift_imm_<mode>" "n"))
			(match_operand:GPI 2 "aarch64_reg_or_zero" "rZ")))]
  ""
  "cmp\\t%<w>2, %<w>0, <shift> %1"
  [(set_attr "type" "alus_shift_imm")]
)

(define_insn "*cmp_swp_<optab><ALLX:mode>_reg<GPI:mode>"
  [(set (reg:CC_SWP CC_REGNUM)
	(compare:CC_SWP (ANY_EXTEND:GPI
			 (match_operand:ALLX 0 "register_operand" "r"))
			(match_operand:GPI 1 "register_operand" "r")))]
  ""
  "cmp\\t%<GPI:w>1, %<GPI:w>0, <su>xt<ALLX:size>"
  [(set_attr "type" "alus_ext")]
)

(define_insn "*cmp_swp_<optab><ALLX:mode>_shft_<GPI:mode>"
  [(set (reg:CC_SWP CC_REGNUM)
	(compare:CC_SWP (ashift:GPI
			 (ANY_EXTEND:GPI
			  (match_operand:ALLX 0 "register_operand" "r"))
			 (match_operand 1 "aarch64_imm3" "Ui3"))
	(match_operand:GPI 2 "register_operand" "r")))]
  ""
  "cmp\\t%<GPI:w>2, %<GPI:w>0, <su>xt<ALLX:size> %1"
  [(set_attr "type" "alus_ext")]
)

;; -------------------------------------------------------------------
;; Store-flag and conditional select insns
;; -------------------------------------------------------------------

(define_expand "cstore<mode>4"
  [(set (match_operand:SI 0 "register_operand" "")
	(match_operator:SI 1 "aarch64_comparison_operator"
	 [(match_operand:GPI 2 "register_operand" "")
	  (match_operand:GPI 3 "aarch64_plus_operand" "")]))]
  ""
  "
  operands[2] = aarch64_gen_compare_reg (GET_CODE (operands[1]), operands[2],
				      operands[3]);
  operands[3] = const0_rtx;
  "
)

(define_expand "cstorecc4"
  [(set (match_operand:SI 0 "register_operand")
       (match_operator 1 "aarch64_comparison_operator_mode"
	[(match_operand 2 "cc_register")
         (match_operand 3 "const0_operand")]))]
  ""
"{
  emit_insn (gen_rtx_SET (operands[0], operands[1]));
  DONE;
}")


(define_expand "cstore<mode>4"
  [(set (match_operand:SI 0 "register_operand" "")
	(match_operator:SI 1 "aarch64_comparison_operator_mode"
	 [(match_operand:GPF 2 "register_operand" "")
	  (match_operand:GPF 3 "aarch64_fp_compare_operand" "")]))]
  ""
  "
  operands[2] = aarch64_gen_compare_reg (GET_CODE (operands[1]), operands[2],
				      operands[3]);
  operands[3] = const0_rtx;
  "
)

(define_insn "aarch64_cstore<mode>"
  [(set (match_operand:ALLI 0 "register_operand" "=r")
	(match_operator:ALLI 1 "aarch64_comparison_operator_mode"
	 [(match_operand 2 "cc_register" "") (const_int 0)]))]
  ""
  "cset\\t%<w>0, %m1"
  [(set_attr "type" "csel")]
)

;; For a 24-bit immediate CST we can optimize the compare for equality
;; and branch sequence from:
;; 	mov	x0, #imm1
;; 	movk	x0, #imm2, lsl 16 /* x0 contains CST.  */
;; 	cmp	x1, x0
;; 	cset	x2, <ne,eq>
;; into the shorter:
;; 	sub	x0, x1, #(CST & 0xfff000)
;; 	subs	x0, x0, #(CST & 0x000fff)
;; 	cset x2, <ne, eq>.
(define_insn_and_split "*compare_cstore<mode>_insn"
  [(set (match_operand:GPI 0 "register_operand" "=r")
	 (EQL:GPI (match_operand:GPI 1 "register_operand" "r")
		  (match_operand:GPI 2 "aarch64_imm24" "n")))]
  "!aarch64_move_imm (INTVAL (operands[2]), <MODE>mode)
   && !aarch64_plus_operand (operands[2], <MODE>mode)
   && !reload_completed"
  "#"
  "&& true"
  [(const_int 0)]
  {
    HOST_WIDE_INT lo_imm = UINTVAL (operands[2]) & 0xfff;
    HOST_WIDE_INT hi_imm = UINTVAL (operands[2]) & 0xfff000;
    rtx tmp = gen_reg_rtx (<MODE>mode);
    emit_insn (gen_add<mode>3 (tmp, operands[1], GEN_INT (-hi_imm)));
    emit_insn (gen_add<mode>3_compare0 (tmp, tmp, GEN_INT (-lo_imm)));
    rtx cc_reg = gen_rtx_REG (CC_NZmode, CC_REGNUM);
    rtx cmp_rtx = gen_rtx_fmt_ee (<EQL:CMP>, <MODE>mode, cc_reg, const0_rtx);
    emit_insn (gen_aarch64_cstore<mode> (operands[0], cmp_rtx, cc_reg));
    DONE;
  }
  [(set_attr "type" "csel")]
)

;; zero_extend version of the above
(define_insn "*cstoresi_insn_uxtw"
  [(set (match_operand:DI 0 "register_operand" "=r")
	(zero_extend:DI
	 (match_operator:SI 1 "aarch64_comparison_operator_mode"
	  [(match_operand 2 "cc_register" "") (const_int 0)])))]
  ""
  "cset\\t%w0, %m1"
  [(set_attr "type" "csel")]
)

(define_insn "cstore<mode>_neg"
  [(set (match_operand:ALLI 0 "register_operand" "=r")
	(neg:ALLI (match_operator:ALLI 1 "aarch64_comparison_operator_mode"
		  [(match_operand 2 "cc_register" "") (const_int 0)])))]
  ""
  "csetm\\t%<w>0, %m1"
  [(set_attr "type" "csel")]
)

;; zero_extend version of the above
(define_insn "*cstoresi_neg_uxtw"
  [(set (match_operand:DI 0 "register_operand" "=r")
	(zero_extend:DI
	 (neg:SI (match_operator:SI 1 "aarch64_comparison_operator_mode"
		  [(match_operand 2 "cc_register" "") (const_int 0)]))))]
  ""
  "csetm\\t%w0, %m1"
  [(set_attr "type" "csel")]
)

(define_expand "cmov<mode>6"
  [(set (match_operand:GPI 0 "register_operand" "")
	(if_then_else:GPI
	 (match_operator 1 "aarch64_comparison_operator"
	  [(match_operand:GPI 2 "register_operand" "")
	   (match_operand:GPI 3 "aarch64_plus_operand" "")])
	 (match_operand:GPI 4 "register_operand" "")
	 (match_operand:GPI 5 "register_operand" "")))]
  ""
  "
  operands[2] = aarch64_gen_compare_reg (GET_CODE (operands[1]), operands[2],
				      operands[3]);
  operands[3] = const0_rtx;
  "
)

(define_expand "cmov<mode>6"
  [(set (match_operand:GPF 0 "register_operand" "")
	(if_then_else:GPF
	 (match_operator 1 "aarch64_comparison_operator"
	  [(match_operand:GPF 2 "register_operand" "")
	   (match_operand:GPF 3 "aarch64_fp_compare_operand" "")])
	 (match_operand:GPF 4 "register_operand" "")
	 (match_operand:GPF 5 "register_operand" "")))]
  ""
  "
  operands[2] = aarch64_gen_compare_reg (GET_CODE (operands[1]), operands[2],
				      operands[3]);
  operands[3] = const0_rtx;
  "
)

(define_insn "*cmov<mode>_insn"
  [(set (match_operand:ALLI 0 "register_operand" "=r,r,r,r,r,r,r")
	(if_then_else:ALLI
	 (match_operator 1 "aarch64_comparison_operator"
	  [(match_operand 2 "cc_register" "") (const_int 0)])
	 (match_operand:ALLI 3 "aarch64_reg_zero_or_m1_or_1" "rZ,rZ,UsM,rZ,Ui1,UsM,Ui1")
	 (match_operand:ALLI 4 "aarch64_reg_zero_or_m1_or_1" "rZ,UsM,rZ,Ui1,rZ,UsM,Ui1")))]
  "!((operands[3] == const1_rtx && operands[4] == constm1_rtx)
     || (operands[3] == constm1_rtx && operands[4] == const1_rtx))"
  ;; Final two alternatives should be unreachable, but included for completeness
  "@
   csel\\t%<w>0, %<w>3, %<w>4, %m1
   csinv\\t%<w>0, %<w>3, <w>zr, %m1
   csinv\\t%<w>0, %<w>4, <w>zr, %M1
   csinc\\t%<w>0, %<w>3, <w>zr, %m1
   csinc\\t%<w>0, %<w>4, <w>zr, %M1
   mov\\t%<w>0, -1
   mov\\t%<w>0, 1"
  [(set_attr "type" "csel, csel, csel, csel, csel, mov_imm, mov_imm")]
)

;; zero_extend version of above
(define_insn "*cmovsi_insn_uxtw"
  [(set (match_operand:DI 0 "register_operand" "=r,r,r,r,r,r,r")
	(zero_extend:DI
	 (if_then_else:SI
	  (match_operator 1 "aarch64_comparison_operator"
	   [(match_operand 2 "cc_register" "") (const_int 0)])
	  (match_operand:SI 3 "aarch64_reg_zero_or_m1_or_1" "rZ,rZ,UsM,rZ,Ui1,UsM,Ui1")
	  (match_operand:SI 4 "aarch64_reg_zero_or_m1_or_1" "rZ,UsM,rZ,Ui1,rZ,UsM,Ui1"))))]
  "!((operands[3] == const1_rtx && operands[4] == constm1_rtx)
     || (operands[3] == constm1_rtx && operands[4] == const1_rtx))"
  ;; Final two alternatives should be unreachable, but included for completeness
  "@
   csel\\t%w0, %w3, %w4, %m1
   csinv\\t%w0, %w3, wzr, %m1
   csinv\\t%w0, %w4, wzr, %M1
   csinc\\t%w0, %w3, wzr, %m1
   csinc\\t%w0, %w4, wzr, %M1
   mov\\t%w0, -1
   mov\\t%w0, 1"
  [(set_attr "type" "csel, csel, csel, csel, csel, mov_imm, mov_imm")]
)

(define_insn "*cmovdi_insn_uxtw"
  [(set (match_operand:DI 0 "register_operand" "=r")
	(if_then_else:DI
	 (match_operator 1 "aarch64_comparison_operator"
	  [(match_operand 2 "cc_register" "") (const_int 0)])
	 (zero_extend:DI (match_operand:SI 3 "register_operand" "r"))
	 (zero_extend:DI (match_operand:SI 4 "register_operand" "r"))))]
  ""
  "csel\\t%w0, %w3, %w4, %m1"
  [(set_attr "type" "csel")]
)

(define_insn "*cmov<mode>_insn"
  [(set (match_operand:GPF 0 "register_operand" "=w")
	(if_then_else:GPF
	 (match_operator 1 "aarch64_comparison_operator"
	  [(match_operand 2 "cc_register" "") (const_int 0)])
	 (match_operand:GPF 3 "register_operand" "w")
	 (match_operand:GPF 4 "register_operand" "w")))]
  "TARGET_FLOAT"
  "fcsel\\t%<s>0, %<s>3, %<s>4, %m1"
  [(set_attr "type" "fcsel")]
)

(define_expand "mov<mode>cc"
  [(set (match_operand:ALLI 0 "register_operand" "")
	(if_then_else:ALLI (match_operand 1 "aarch64_comparison_operator" "")
			   (match_operand:ALLI 2 "register_operand" "")
			   (match_operand:ALLI 3 "register_operand" "")))]
  ""
  {
    rtx ccreg;
    enum rtx_code code = GET_CODE (operands[1]);

    if (code == UNEQ || code == LTGT)
      FAIL;

    ccreg = aarch64_gen_compare_reg (code, XEXP (operands[1], 0),
				     XEXP (operands[1], 1));
    operands[1] = gen_rtx_fmt_ee (code, VOIDmode, ccreg, const0_rtx);
  }
)

(define_expand "mov<GPF:mode><GPI:mode>cc"
  [(set (match_operand:GPI 0 "register_operand" "")
	(if_then_else:GPI (match_operand 1 "aarch64_comparison_operator" "")
			  (match_operand:GPF 2 "register_operand" "")
			  (match_operand:GPF 3 "register_operand" "")))]
  ""
  {
    rtx ccreg;
    enum rtx_code code = GET_CODE (operands[1]);

    if (code == UNEQ || code == LTGT)
      FAIL;

    ccreg = aarch64_gen_compare_reg (code, XEXP (operands[1], 0),
				  XEXP (operands[1], 1));
    operands[1] = gen_rtx_fmt_ee (code, VOIDmode, ccreg, const0_rtx);
  }
)

(define_expand "mov<mode>cc"
  [(set (match_operand:GPF 0 "register_operand" "")
	(if_then_else:GPF (match_operand 1 "aarch64_comparison_operator" "")
			  (match_operand:GPF 2 "register_operand" "")
			  (match_operand:GPF 3 "register_operand" "")))]
  ""
  {
    rtx ccreg;
    enum rtx_code code = GET_CODE (operands[1]);

    if (code == UNEQ || code == LTGT)
      FAIL;

    ccreg = aarch64_gen_compare_reg (code, XEXP (operands[1], 0),
				  XEXP (operands[1], 1));
    operands[1] = gen_rtx_fmt_ee (code, VOIDmode, ccreg, const0_rtx);
  }
)

(define_expand "<neg_not_op><mode>cc"
  [(set (match_operand:GPI 0 "register_operand" "")
	(if_then_else:GPI (match_operand 1 "aarch64_comparison_operator" "")
			  (NEG_NOT:GPI (match_operand:GPI 2 "register_operand" ""))
			  (match_operand:GPI 3 "register_operand" "")))]
  ""
  {
    rtx ccreg;
    enum rtx_code code = GET_CODE (operands[1]);

    if (code == UNEQ || code == LTGT)
      FAIL;

    ccreg = aarch64_gen_compare_reg (code, XEXP (operands[1], 0),
				      XEXP (operands[1], 1));
    operands[1] = gen_rtx_fmt_ee (code, VOIDmode, ccreg, const0_rtx);
  }
)

;; CRC32 instructions.
(define_insn "aarch64_<crc_variant>"
  [(set (match_operand:SI 0 "register_operand" "=r")
        (unspec:SI [(match_operand:SI 1 "register_operand" "r")
                    (match_operand:<crc_mode> 2 "register_operand" "r")]
         CRC))]
  "TARGET_CRC32"
  {
    if (GET_MODE_BITSIZE (GET_MODE (operands[2])) >= 64)
      return "<crc_variant>\\t%w0, %w1, %x2";
    else
      return "<crc_variant>\\t%w0, %w1, %w2";
  }
  [(set_attr "type" "crc")]
)

(define_insn "*csinc2<mode>_insn"
  [(set (match_operand:GPI 0 "register_operand" "=r")
        (plus:GPI (match_operand 2 "aarch64_comparison_operation" "")
                  (match_operand:GPI 1 "register_operand" "r")))]
  ""
  "cinc\\t%<w>0, %<w>1, %m2"
  [(set_attr "type" "csel")]
)

(define_insn "csinc3<mode>_insn"
  [(set (match_operand:GPI 0 "register_operand" "=r")
        (if_then_else:GPI
	  (match_operand 1 "aarch64_comparison_operation" "")
	  (plus:GPI (match_operand:GPI 2 "register_operand" "r")
		    (const_int 1))
	  (match_operand:GPI 3 "aarch64_reg_or_zero" "rZ")))]
  ""
  "csinc\\t%<w>0, %<w>3, %<w>2, %M1"
  [(set_attr "type" "csel")]
)

(define_insn "*csinv3<mode>_insn"
  [(set (match_operand:GPI 0 "register_operand" "=r")
        (if_then_else:GPI
	  (match_operand 1 "aarch64_comparison_operation" "")
	  (not:GPI (match_operand:GPI 2 "register_operand" "r"))
	  (match_operand:GPI 3 "aarch64_reg_or_zero" "rZ")))]
  ""
  "csinv\\t%<w>0, %<w>3, %<w>2, %M1"
  [(set_attr "type" "csel")]
)

(define_insn "csneg3_uxtw_insn"
  [(set (match_operand:DI 0 "register_operand" "=r")
	(zero_extend:DI
	  (if_then_else:SI
	    (match_operand 1 "aarch64_comparison_operation" "")
	    (neg:SI (match_operand:SI 2 "register_operand" "r"))
	    (match_operand:SI 3 "aarch64_reg_or_zero" "rZ"))))]
  ""
  "csneg\\t%w0, %w3, %w2, %M1"
  [(set_attr "type" "csel")]
)

(define_insn "csneg3<mode>_insn"
  [(set (match_operand:GPI 0 "register_operand" "=r")
        (if_then_else:GPI
	  (match_operand 1 "aarch64_comparison_operation" "")
	  (neg:GPI (match_operand:GPI 2 "register_operand" "r"))
	  (match_operand:GPI 3 "aarch64_reg_or_zero" "rZ")))]
  ""
  "csneg\\t%<w>0, %<w>3, %<w>2, %M1"
  [(set_attr "type" "csel")]
)

;; -------------------------------------------------------------------
;; Logical operations
;; -------------------------------------------------------------------

(define_insn "<optab><mode>3"
  [(set (match_operand:GPI 0 "register_operand" "=r,rk,w")
	(LOGICAL:GPI (match_operand:GPI 1 "register_operand" "%r,r,w")
		     (match_operand:GPI 2 "aarch64_logical_operand" "r,<lconst>,w")))]
  ""
  "@
  <logical>\\t%<w>0, %<w>1, %<w>2
  <logical>\\t%<w>0, %<w>1, %2
  <logical>\\t%0.<Vbtype>, %1.<Vbtype>, %2.<Vbtype>"
  [(set_attr "type" "logic_reg,logic_imm,neon_logic")
   (set_attr "simd" "*,*,yes")]
)

;; zero_extend version of above
(define_insn "*<optab>si3_uxtw"
  [(set (match_operand:DI 0 "register_operand" "=r,rk")
	(zero_extend:DI
         (LOGICAL:SI (match_operand:SI 1 "register_operand" "%r,r")
		     (match_operand:SI 2 "aarch64_logical_operand" "r,K"))))]
  ""
  "@
   <logical>\\t%w0, %w1, %w2
   <logical>\\t%w0, %w1, %2"
  [(set_attr "type" "logic_reg,logic_imm")]
)

(define_insn "*and<mode>3_compare0"
  [(set (reg:CC_NZ CC_REGNUM)
	(compare:CC_NZ
	 (and:GPI (match_operand:GPI 1 "register_operand" "%r,r")
		  (match_operand:GPI 2 "aarch64_logical_operand" "r,<lconst>"))
	 (const_int 0)))
   (set (match_operand:GPI 0 "register_operand" "=r,r")
	(and:GPI (match_dup 1) (match_dup 2)))]
  ""
  "@
   ands\\t%<w>0, %<w>1, %<w>2
   ands\\t%<w>0, %<w>1, %2"
  [(set_attr "type" "logics_reg,logics_imm")]
)

;; zero_extend version of above
(define_insn "*andsi3_compare0_uxtw"
  [(set (reg:CC_NZ CC_REGNUM)
	(compare:CC_NZ
	 (and:SI (match_operand:SI 1 "register_operand" "%r,r")
		 (match_operand:SI 2 "aarch64_logical_operand" "r,K"))
	 (const_int 0)))
   (set (match_operand:DI 0 "register_operand" "=r,r")
	(zero_extend:DI (and:SI (match_dup 1) (match_dup 2))))]
  ""
  "@
   ands\\t%w0, %w1, %w2
   ands\\t%w0, %w1, %2"
  [(set_attr "type" "logics_reg,logics_imm")]
)

(define_insn "*and_<SHIFT:optab><mode>3_compare0"
  [(set (reg:CC_NZ CC_REGNUM)
	(compare:CC_NZ
	 (and:GPI (SHIFT:GPI
		   (match_operand:GPI 1 "register_operand" "r")
		   (match_operand:QI 2 "aarch64_shift_imm_<mode>" "n"))
		  (match_operand:GPI 3 "register_operand" "r"))
	 (const_int 0)))
   (set (match_operand:GPI 0 "register_operand" "=r")
	(and:GPI (SHIFT:GPI (match_dup 1) (match_dup 2)) (match_dup 3)))]
  ""
  "ands\\t%<w>0, %<w>3, %<w>1, <SHIFT:shift> %2"
  [(set_attr "type" "logics_shift_imm")]
)

;; zero_extend version of above
(define_insn "*and_<SHIFT:optab>si3_compare0_uxtw"
  [(set (reg:CC_NZ CC_REGNUM)
	(compare:CC_NZ
	 (and:SI (SHIFT:SI
		  (match_operand:SI 1 "register_operand" "r")
		  (match_operand:QI 2 "aarch64_shift_imm_si" "n"))
		 (match_operand:SI 3 "register_operand" "r"))
	 (const_int 0)))
   (set (match_operand:DI 0 "register_operand" "=r")
	(zero_extend:DI (and:SI (SHIFT:SI (match_dup 1) (match_dup 2))
				(match_dup 3))))]
  ""
  "ands\\t%w0, %w3, %w1, <SHIFT:shift> %2"
  [(set_attr "type" "logics_shift_imm")]
)

(define_insn "*<LOGICAL:optab>_<SHIFT:optab><mode>3"
  [(set (match_operand:GPI 0 "register_operand" "=r")
	(LOGICAL:GPI (SHIFT:GPI
		      (match_operand:GPI 1 "register_operand" "r")
		      (match_operand:QI 2 "aarch64_shift_imm_<mode>" "n"))
		     (match_operand:GPI 3 "register_operand" "r")))]
  ""
  "<LOGICAL:logical>\\t%<w>0, %<w>3, %<w>1, <SHIFT:shift> %2"
  [(set_attr "type" "logic_shift_imm")]
)

(define_insn "*<optab>_rol<mode>3"
  [(set (match_operand:GPI 0 "register_operand" "=r")
	(LOGICAL:GPI (rotate:GPI
		      (match_operand:GPI 1 "register_operand" "r")
		      (match_operand:QI 2 "aarch64_shift_imm_<mode>" "n"))
		     (match_operand:GPI 3 "register_operand" "r")))]
  ""
  "<logical>\\t%<w>0, %<w>3, %<w>1, ror (<sizen> - %2)"
  [(set_attr "type" "logic_shift_imm")]
)

;; zero_extend versions of above
(define_insn "*<LOGICAL:optab>_<SHIFT:optab>si3_uxtw"
  [(set (match_operand:DI 0 "register_operand" "=r")
	(zero_extend:DI
	 (LOGICAL:SI (SHIFT:SI
		      (match_operand:SI 1 "register_operand" "r")
		      (match_operand:QI 2 "aarch64_shift_imm_si" "n"))
		     (match_operand:SI 3 "register_operand" "r"))))]
  ""
  "<LOGICAL:logical>\\t%w0, %w3, %w1, <SHIFT:shift> %2"
  [(set_attr "type" "logic_shift_imm")]
)

(define_insn "*<optab>_rolsi3_uxtw"
  [(set (match_operand:DI 0 "register_operand" "=r")
	(zero_extend:DI
	 (LOGICAL:SI (rotate:SI
		      (match_operand:SI 1 "register_operand" "r")
		      (match_operand:QI 2 "aarch64_shift_imm_si" "n"))
		     (match_operand:SI 3 "register_operand" "r"))))]
  ""
  "<logical>\\t%w0, %w3, %w1, ror (32 - %2)"
  [(set_attr "type" "logic_shift_imm")]
)

(define_insn "one_cmpl<mode>2"
  [(set (match_operand:GPI 0 "register_operand" "=r,w")
	(not:GPI (match_operand:GPI 1 "register_operand" "r,w")))]
  ""
  "@
  mvn\\t%<w>0, %<w>1
  mvn\\t%0.8b, %1.8b"
  [(set_attr "type" "logic_reg,neon_logic")
   (set_attr "simd" "*,yes")]
)

(define_insn "*one_cmpl_<optab><mode>2"
  [(set (match_operand:GPI 0 "register_operand" "=r")
	(not:GPI (SHIFT:GPI (match_operand:GPI 1 "register_operand" "r")
			    (match_operand:QI 2 "aarch64_shift_imm_<mode>" "n"))))]
  ""
  "mvn\\t%<w>0, %<w>1, <shift> %2"
  [(set_attr "type" "logic_shift_imm")]
)

;; Binary logical operators negating one operand, i.e. (a & !b), (a | !b).

(define_insn "*<NLOGICAL:optab>_one_cmpl<mode>3"
  [(set (match_operand:GPI 0 "register_operand" "=r,w")
	(NLOGICAL:GPI (not:GPI (match_operand:GPI 1 "register_operand" "r,w"))
		     (match_operand:GPI 2 "register_operand" "r,w")))]
  ""
  "@
  <NLOGICAL:nlogical>\\t%<w>0, %<w>2, %<w>1
  <NLOGICAL:nlogical>\\t%0.<Vbtype>, %2.<Vbtype>, %1.<Vbtype>"
  [(set_attr "type" "logic_reg,neon_logic")
   (set_attr "simd" "*,yes")]
)

(define_insn "*<NLOGICAL:optab>_one_cmplsidi3_ze"
  [(set (match_operand:DI 0 "register_operand" "=r")
	(zero_extend:DI
	  (NLOGICAL:SI (not:SI (match_operand:SI 1 "register_operand" "r"))
	               (match_operand:SI 2 "register_operand" "r"))))]
  ""
  "<NLOGICAL:nlogical>\\t%w0, %w2, %w1"
  [(set_attr "type" "logic_reg")]
)

(define_insn "*xor_one_cmplsidi3_ze"
  [(set (match_operand:DI 0 "register_operand" "=r")
        (zero_extend:DI
          (not:SI (xor:SI (match_operand:SI 1 "register_operand" "r")
                          (match_operand:SI 2 "register_operand" "r")))))]
  ""
  "eon\\t%w0, %w1, %w2"
  [(set_attr "type" "logic_reg")]
)

;; (xor (not a) b) is simplify_rtx-ed down to (not (xor a b)).
;; eon does not operate on SIMD registers so the vector variant must be split.
(define_insn_and_split "*xor_one_cmpl<mode>3"
  [(set (match_operand:GPI 0 "register_operand" "=r,w")
        (not:GPI (xor:GPI (match_operand:GPI 1 "register_operand" "r,?w")
                          (match_operand:GPI 2 "register_operand" "r,w"))))]
  ""
  "@
  eon\\t%<w>0, %<w>1, %<w>2
  #"
  "reload_completed && FP_REGNUM_P (REGNO (operands[0]))" ;; For SIMD registers.
  [(set (match_operand:GPI 0 "register_operand" "=w")
        (xor:GPI (match_operand:GPI 1 "register_operand" "w")
                 (match_operand:GPI 2 "register_operand" "w")))
   (set (match_dup 0) (not:GPI (match_dup 0)))]
  ""
  [(set_attr "type" "logic_reg,multiple")
   (set_attr "simd" "*,yes")]
)

(define_insn "*and_one_cmpl<mode>3_compare0"
  [(set (reg:CC_NZ CC_REGNUM)
	(compare:CC_NZ
	 (and:GPI (not:GPI
		   (match_operand:GPI 1 "register_operand" "r"))
		  (match_operand:GPI 2 "register_operand" "r"))
	 (const_int 0)))
   (set (match_operand:GPI 0 "register_operand" "=r")
	(and:GPI (not:GPI (match_dup 1)) (match_dup 2)))]
  ""
  "bics\\t%<w>0, %<w>2, %<w>1"
  [(set_attr "type" "logics_reg")]
)

;; zero_extend version of above
(define_insn "*and_one_cmplsi3_compare0_uxtw"
  [(set (reg:CC_NZ CC_REGNUM)
	(compare:CC_NZ
	 (and:SI (not:SI
		  (match_operand:SI 1 "register_operand" "r"))
		 (match_operand:SI 2 "register_operand" "r"))
	 (const_int 0)))
   (set (match_operand:DI 0 "register_operand" "=r")
	(zero_extend:DI (and:SI (not:SI (match_dup 1)) (match_dup 2))))]
  ""
  "bics\\t%w0, %w2, %w1"
  [(set_attr "type" "logics_reg")]
)

(define_insn "*and_one_cmpl<mode>3_compare0_no_reuse"
  [(set (reg:CC_NZ CC_REGNUM)
    (compare:CC_NZ
     (and:GPI (not:GPI
           (match_operand:GPI 0 "register_operand" "r"))
          (match_operand:GPI 1 "register_operand" "r"))
     (const_int 0)))]
  ""
  "bics\\t<w>zr, %<w>1, %<w>0"
  [(set_attr "type" "logics_reg")]
)

(define_insn "<LOGICAL:optab>_one_cmpl_<SHIFT:optab><mode>3"
  [(set (match_operand:GPI 0 "register_operand" "=r")
	(LOGICAL:GPI (not:GPI
		      (SHIFT:GPI
		       (match_operand:GPI 1 "register_operand" "r")
		       (match_operand:QI 2 "aarch64_shift_imm_<mode>" "n")))
		     (match_operand:GPI 3 "register_operand" "r")))]
  ""
  "<LOGICAL:nlogical>\\t%<w>0, %<w>3, %<w>1, <SHIFT:shift> %2"
  [(set_attr "type" "logic_shift_imm")]
)

(define_insn "*eor_one_cmpl_<SHIFT:optab><mode>3_alt"
  [(set (match_operand:GPI 0 "register_operand" "=r")
	(not:GPI (xor:GPI
		      (SHIFT:GPI
		       (match_operand:GPI 1 "register_operand" "r")
		       (match_operand:QI 2 "aarch64_shift_imm_<mode>" "n"))
		     (match_operand:GPI 3 "register_operand" "r"))))]
  ""
  "eon\\t%<w>0, %<w>3, %<w>1, <SHIFT:shift> %2"
  [(set_attr "type" "logic_shift_imm")]
)

;; Zero-extend version of the above.
(define_insn "*eor_one_cmpl_<SHIFT:optab>sidi3_alt_ze"
  [(set (match_operand:DI 0 "register_operand" "=r")
	(zero_extend:DI
	  (not:SI (xor:SI
		    (SHIFT:SI
		      (match_operand:SI 1 "register_operand" "r")
		      (match_operand:QI 2 "aarch64_shift_imm_si" "n"))
		    (match_operand:SI 3 "register_operand" "r")))))]
  ""
  "eon\\t%w0, %w3, %w1, <SHIFT:shift> %2"
  [(set_attr "type" "logic_shift_imm")]
)

(define_insn "*and_one_cmpl_<SHIFT:optab><mode>3_compare0"
  [(set (reg:CC_NZ CC_REGNUM)
	(compare:CC_NZ
	 (and:GPI (not:GPI
		   (SHIFT:GPI
		    (match_operand:GPI 1 "register_operand" "r")
		    (match_operand:QI 2 "aarch64_shift_imm_<mode>" "n")))
		  (match_operand:GPI 3 "register_operand" "r"))
	 (const_int 0)))
   (set (match_operand:GPI 0 "register_operand" "=r")
	(and:GPI (not:GPI
		  (SHIFT:GPI
		   (match_dup 1) (match_dup 2))) (match_dup 3)))]
  ""
  "bics\\t%<w>0, %<w>3, %<w>1, <SHIFT:shift> %2"
  [(set_attr "type" "logics_shift_imm")]
)

;; zero_extend version of above
(define_insn "*and_one_cmpl_<SHIFT:optab>si3_compare0_uxtw"
  [(set (reg:CC_NZ CC_REGNUM)
	(compare:CC_NZ
	 (and:SI (not:SI
		  (SHIFT:SI
		   (match_operand:SI 1 "register_operand" "r")
		   (match_operand:QI 2 "aarch64_shift_imm_si" "n")))
		 (match_operand:SI 3 "register_operand" "r"))
	 (const_int 0)))
   (set (match_operand:DI 0 "register_operand" "=r")
	(zero_extend:DI (and:SI
			 (not:SI
			  (SHIFT:SI (match_dup 1) (match_dup 2))) (match_dup 3))))]
  ""
  "bics\\t%w0, %w3, %w1, <SHIFT:shift> %2"
  [(set_attr "type" "logics_shift_imm")]
)

(define_insn "*and_one_cmpl_<SHIFT:optab><mode>3_compare0_no_reuse"
  [(set (reg:CC_NZ CC_REGNUM)
    (compare:CC_NZ
     (and:GPI (not:GPI
           (SHIFT:GPI
            (match_operand:GPI 0 "register_operand" "r")
            (match_operand:QI 1 "aarch64_shift_imm_<mode>" "n")))
          (match_operand:GPI 2 "register_operand" "r"))
     (const_int 0)))]
  ""
  "bics\\t<w>zr, %<w>2, %<w>0, <SHIFT:shift> %1"
  [(set_attr "type" "logics_shift_imm")]
)

(define_insn "clz<mode>2"
  [(set (match_operand:GPI 0 "register_operand" "=r")
	(clz:GPI (match_operand:GPI 1 "register_operand" "r")))]
  ""
  "clz\\t%<w>0, %<w>1"
  [(set_attr "type" "clz")]
)

(define_expand "ffs<mode>2"
  [(match_operand:GPI 0 "register_operand")
   (match_operand:GPI 1 "register_operand")]
  ""
  {
    rtx ccreg = aarch64_gen_compare_reg (EQ, operands[1], const0_rtx);
    rtx x = gen_rtx_NE (VOIDmode, ccreg, const0_rtx);

    emit_insn (gen_rbit<mode>2 (operands[0], operands[1]));
    emit_insn (gen_clz<mode>2 (operands[0], operands[0]));
    emit_insn (gen_csinc3<mode>_insn (operands[0], x, operands[0], const0_rtx));
    DONE;
  }
)

(define_insn "clrsb<mode>2"
  [(set (match_operand:GPI 0 "register_operand" "=r")
        (clrsb:GPI (match_operand:GPI 1 "register_operand" "r")))]
  ""
  "cls\\t%<w>0, %<w>1"
  [(set_attr "type" "clz")]
)

(define_insn "rbit<mode>2"
  [(set (match_operand:GPI 0 "register_operand" "=r")
	(unspec:GPI [(match_operand:GPI 1 "register_operand" "r")] UNSPEC_RBIT))]
  ""
  "rbit\\t%<w>0, %<w>1"
  [(set_attr "type" "rbit")]
)

;; Split after reload into RBIT + CLZ.  Since RBIT is represented as an UNSPEC
;; it is unlikely to fold with any other operation, so keep this as a CTZ
;; expression and split after reload to enable scheduling them apart if
;; needed.

(define_insn_and_split "ctz<mode>2"
 [(set (match_operand:GPI           0 "register_operand" "=r")
       (ctz:GPI (match_operand:GPI  1 "register_operand" "r")))]
  ""
  "#"
  "reload_completed"
  [(const_int 0)]
  "
  emit_insn (gen_rbit<mode>2 (operands[0], operands[1]));
  emit_insn (gen_clz<mode>2 (operands[0], operands[0]));
  DONE;
")

(define_insn "*and<mode>_compare0"
  [(set (reg:CC_NZ CC_REGNUM)
	(compare:CC_NZ
	 (match_operand:SHORT 0 "register_operand" "r")
	 (const_int 0)))]
  ""
  "tst\\t%<w>0, <short_mask>"
  [(set_attr "type" "alus_imm")]
)

(define_insn "*and<mode>3nr_compare0"
  [(set (reg:CC_NZ CC_REGNUM)
	(compare:CC_NZ
	 (and:GPI (match_operand:GPI 0 "register_operand" "%r,r")
		  (match_operand:GPI 1 "aarch64_logical_operand" "r,<lconst>"))
	 (const_int 0)))]
  ""
  "@
   tst\\t%<w>0, %<w>1
   tst\\t%<w>0, %1"
  [(set_attr "type" "logics_reg,logics_imm")]
)

(define_insn "*and<mode>3nr_compare0_zextract"
  [(set (reg:CC_NZ CC_REGNUM)
	(compare:CC_NZ
	 (zero_extract:GPI (match_operand:GPI 0 "register_operand" "r")
		  (match_operand:GPI 1 "const_int_operand" "n")
		  (match_operand:GPI 2 "const_int_operand" "n"))
	 (const_int 0)))]
  "INTVAL (operands[1]) > 0
   && ((INTVAL (operands[1]) + INTVAL (operands[2]))
	<= GET_MODE_BITSIZE (<MODE>mode))
   && aarch64_bitmask_imm (
	UINTVAL (aarch64_mask_from_zextract_ops (operands[1],
						 operands[2])),
	<MODE>mode)"
  {
    operands[1]
      = aarch64_mask_from_zextract_ops (operands[1], operands[2]);
    return "tst\\t%<w>0, %1";
  }
  [(set_attr "type" "logics_shift_imm")]
)

(define_insn "*and_<SHIFT:optab><mode>3nr_compare0"
  [(set (reg:CC_NZ CC_REGNUM)
	(compare:CC_NZ
	 (and:GPI (SHIFT:GPI
		   (match_operand:GPI 0 "register_operand" "r")
		   (match_operand:QI 1 "aarch64_shift_imm_<mode>" "n"))
		  (match_operand:GPI 2 "register_operand" "r"))
	(const_int 0)))]
  ""
  "tst\\t%<w>2, %<w>0, <SHIFT:shift> %1"
  [(set_attr "type" "logics_shift_imm")]
)

;; -------------------------------------------------------------------
;; Shifts
;; -------------------------------------------------------------------

(define_expand "<optab><mode>3"
  [(set (match_operand:GPI 0 "register_operand")
	(ASHIFT:GPI (match_operand:GPI 1 "register_operand")
		    (match_operand:QI 2 "nonmemory_operand")))]
  ""
  {
    if (CONST_INT_P (operands[2]))
      {
        operands[2] = GEN_INT (INTVAL (operands[2])
                               & (GET_MODE_BITSIZE (<MODE>mode) - 1));

        if (operands[2] == const0_rtx)
          {
	    emit_insn (gen_mov<mode> (operands[0], operands[1]));
	    DONE;
          }
      }
  }
)

(define_expand "ashl<mode>3"
  [(set (match_operand:SHORT 0 "register_operand")
	(ashift:SHORT (match_operand:SHORT 1 "register_operand")
		      (match_operand:QI 2 "const_int_operand")))]
  ""
  {
    operands[2] = GEN_INT (INTVAL (operands[2]) & GET_MODE_MASK (<MODE>mode));

    if (operands[2] == const0_rtx)
      {
	emit_insn (gen_mov<mode> (operands[0], operands[1]));
	DONE;
      }
  }
)

(define_expand "rotr<mode>3"
  [(set (match_operand:GPI 0 "register_operand")
	(rotatert:GPI (match_operand:GPI 1 "register_operand")
		      (match_operand:QI 2 "nonmemory_operand")))]
  ""
  {
    if (CONST_INT_P (operands[2]))
      {
        operands[2] = GEN_INT (INTVAL (operands[2])
                               & (GET_MODE_BITSIZE (<MODE>mode) - 1));

        if (operands[2] == const0_rtx)
          {
	    emit_insn (gen_mov<mode> (operands[0], operands[1]));
	    DONE;
          }
      }
  }
)

(define_expand "rotl<mode>3"
  [(set (match_operand:GPI 0 "register_operand")
	(rotatert:GPI (match_operand:GPI 1 "register_operand")
		      (match_operand:QI 2 "nonmemory_operand")))]
  ""
  {
    /* (SZ - cnt) % SZ == -cnt % SZ */
    if (CONST_INT_P (operands[2]))
      {
        operands[2] = GEN_INT ((-INTVAL (operands[2]))
			       & (GET_MODE_BITSIZE (<MODE>mode) - 1));
        if (operands[2] == const0_rtx)
          {
	    emit_insn (gen_mov<mode> (operands[0], operands[1]));
	    DONE;
          }
      }
    else
      operands[2] = expand_simple_unop (QImode, NEG, operands[2],
					NULL_RTX, 1);
  }
)

;; Logical left shift using SISD or Integer instruction
(define_insn "*aarch64_ashl_sisd_or_int_<mode>3"
  [(set (match_operand:GPI 0 "register_operand" "=r,r,w,w")
	(ashift:GPI
	  (match_operand:GPI 1 "register_operand" "r,r,w,w")
	  (match_operand:QI 2 "aarch64_reg_or_shift_imm_<mode>" "Us<cmode>,r,Us<cmode>,w")))]
  ""
  "@
   lsl\t%<w>0, %<w>1, %2
   lsl\t%<w>0, %<w>1, %<w>2
   shl\t%<rtn>0<vas>, %<rtn>1<vas>, %2
   ushl\t%<rtn>0<vas>, %<rtn>1<vas>, %<rtn>2<vas>"
  [(set_attr "simd" "no,no,yes,yes")
   (set_attr "type" "bfm,shift_reg,neon_shift_imm<q>, neon_shift_reg<q>")]
)

;; Logical right shift using SISD or Integer instruction
(define_insn "*aarch64_lshr_sisd_or_int_<mode>3"
  [(set (match_operand:GPI 0 "register_operand" "=r,r,w,&w,&w")
	(lshiftrt:GPI
	 (match_operand:GPI 1 "register_operand" "r,r,w,w,w")
	 (match_operand:QI 2 "aarch64_reg_or_shift_imm_<mode>" "Us<cmode>,r,Us<cmode>,w,0")))]
  ""
  "@
   lsr\t%<w>0, %<w>1, %2
   lsr\t%<w>0, %<w>1, %<w>2
   ushr\t%<rtn>0<vas>, %<rtn>1<vas>, %2
   #
   #"
  [(set_attr "simd" "no,no,yes,yes,yes")
   (set_attr "type" "bfm,shift_reg,neon_shift_imm<q>,neon_shift_reg<q>,neon_shift_reg<q>")]
)

(define_split
  [(set (match_operand:DI 0 "aarch64_simd_register")
        (lshiftrt:DI
           (match_operand:DI 1 "aarch64_simd_register")
           (match_operand:QI 2 "aarch64_simd_register")))]
  "TARGET_SIMD && reload_completed"
  [(set (match_dup 3)
        (unspec:QI [(match_dup 2)] UNSPEC_SISD_NEG))
   (set (match_dup 0)
        (unspec:DI [(match_dup 1) (match_dup 3)] UNSPEC_SISD_USHL))]
  {
    operands[3] = gen_lowpart (QImode, operands[0]);
  }
)

(define_split
  [(set (match_operand:SI 0 "aarch64_simd_register")
        (lshiftrt:SI
           (match_operand:SI 1 "aarch64_simd_register")
           (match_operand:QI 2 "aarch64_simd_register")))]
  "TARGET_SIMD && reload_completed"
  [(set (match_dup 3)
        (unspec:QI [(match_dup 2)] UNSPEC_SISD_NEG))
   (set (match_dup 0)
        (unspec:SI [(match_dup 1) (match_dup 3)] UNSPEC_USHL_2S))]
  {
    operands[3] = gen_lowpart (QImode, operands[0]);
  }
)

;; Arithmetic right shift using SISD or Integer instruction
(define_insn "*aarch64_ashr_sisd_or_int_<mode>3"
  [(set (match_operand:GPI 0 "register_operand" "=r,r,w,&w,&w")
        (ashiftrt:GPI
          (match_operand:GPI 1 "register_operand" "r,r,w,w,w")
          (match_operand:QI 2 "aarch64_reg_or_shift_imm_di" "Us<cmode>,r,Us<cmode>,w,0")))]
  ""
  "@
   asr\t%<w>0, %<w>1, %2
   asr\t%<w>0, %<w>1, %<w>2
   sshr\t%<rtn>0<vas>, %<rtn>1<vas>, %2
   #
   #"
  [(set_attr "simd" "no,no,yes,yes,yes")
   (set_attr "type" "bfm,shift_reg,neon_shift_imm<q>,neon_shift_reg<q>,neon_shift_reg<q>")]
)

(define_split
  [(set (match_operand:DI 0 "aarch64_simd_register")
        (ashiftrt:DI
           (match_operand:DI 1 "aarch64_simd_register")
           (match_operand:QI 2 "aarch64_simd_register")))]
  "TARGET_SIMD && reload_completed"
  [(set (match_dup 3)
        (unspec:QI [(match_dup 2)] UNSPEC_SISD_NEG))
   (set (match_dup 0)
        (unspec:DI [(match_dup 1) (match_dup 3)] UNSPEC_SISD_SSHL))]
{
  operands[3] = gen_lowpart (QImode, operands[0]);
}
)

(define_split
  [(set (match_operand:SI 0 "aarch64_simd_register")
        (ashiftrt:SI
           (match_operand:SI 1 "aarch64_simd_register")
           (match_operand:QI 2 "aarch64_simd_register")))]
  "TARGET_SIMD && reload_completed"
  [(set (match_dup 3)
        (unspec:QI [(match_dup 2)] UNSPEC_SISD_NEG))
   (set (match_dup 0)
        (unspec:SI [(match_dup 1) (match_dup 3)] UNSPEC_SSHL_2S))]
{
  operands[3] = gen_lowpart (QImode, operands[0]);
}
)

(define_insn "*aarch64_sisd_ushl"
  [(set (match_operand:DI 0 "register_operand" "=w")
        (unspec:DI [(match_operand:DI 1 "register_operand" "w")
                    (match_operand:QI 2 "register_operand" "w")]
                   UNSPEC_SISD_USHL))]
  "TARGET_SIMD"
  "ushl\t%d0, %d1, %d2"
  [(set_attr "simd" "yes")
   (set_attr "type" "neon_shift_reg")]
)

(define_insn "*aarch64_ushl_2s"
  [(set (match_operand:SI 0 "register_operand" "=w")
        (unspec:SI [(match_operand:SI 1 "register_operand" "w")
                    (match_operand:QI 2 "register_operand" "w")]
                   UNSPEC_USHL_2S))]
  "TARGET_SIMD"
  "ushl\t%0.2s, %1.2s, %2.2s"
  [(set_attr "simd" "yes")
   (set_attr "type" "neon_shift_reg")]
)

(define_insn "*aarch64_sisd_sshl"
  [(set (match_operand:DI 0 "register_operand" "=w")
        (unspec:DI [(match_operand:DI 1 "register_operand" "w")
                    (match_operand:QI 2 "register_operand" "w")]
                   UNSPEC_SISD_SSHL))]
  "TARGET_SIMD"
  "sshl\t%d0, %d1, %d2"
  [(set_attr "simd" "yes")
   (set_attr "type" "neon_shift_reg")]
)

(define_insn "*aarch64_sshl_2s"
  [(set (match_operand:SI 0 "register_operand" "=w")
        (unspec:SI [(match_operand:SI 1 "register_operand" "w")
                    (match_operand:QI 2 "register_operand" "w")]
                   UNSPEC_SSHL_2S))]
  "TARGET_SIMD"
  "sshl\t%0.2s, %1.2s, %2.2s"
  [(set_attr "simd" "yes")
   (set_attr "type" "neon_shift_reg")]
)

(define_insn "*aarch64_sisd_neg_qi"
  [(set (match_operand:QI 0 "register_operand" "=w")
        (unspec:QI [(match_operand:QI 1 "register_operand" "w")]
                   UNSPEC_SISD_NEG))]
  "TARGET_SIMD"
  "neg\t%d0, %d1"
  [(set_attr "simd" "yes")
   (set_attr "type" "neon_neg")]
)

;; Rotate right
(define_insn "*ror<mode>3_insn"
  [(set (match_operand:GPI 0 "register_operand" "=r,r")
     (rotatert:GPI
       (match_operand:GPI 1 "register_operand" "r,r")
       (match_operand:QI 2 "aarch64_reg_or_shift_imm_<mode>" "Us<cmode>,r")))]
  ""
  "@
   ror\\t%<w>0, %<w>1, %2
   ror\\t%<w>0, %<w>1, %<w>2"
  [(set_attr "type" "rotate_imm,shift_reg")]
)

;; zero_extend version of above
(define_insn "*<optab>si3_insn_uxtw"
  [(set (match_operand:DI 0 "register_operand" "=r,r")
	(zero_extend:DI (SHIFT:SI
	 (match_operand:SI 1 "register_operand" "r,r")
	 (match_operand:QI 2 "aarch64_reg_or_shift_imm_si" "Uss,r"))))]
  ""
  "@
   <shift>\\t%w0, %w1, %2
   <shift>\\t%w0, %w1, %w2"
  [(set_attr "type" "bfm,shift_reg")]
)

(define_insn "*<optab><mode>3_insn"
  [(set (match_operand:SHORT 0 "register_operand" "=r")
	(ASHIFT:SHORT (match_operand:SHORT 1 "register_operand" "r")
		      (match_operand 2 "const_int_operand" "n")))]
  "UINTVAL (operands[2]) < GET_MODE_BITSIZE (<MODE>mode)"
{
  operands[3] = GEN_INT (<sizen> - UINTVAL (operands[2]));
  return "<bfshift>\t%w0, %w1, %2, %3";
}
  [(set_attr "type" "bfm")]
)

(define_insn "*extr<mode>5_insn"
  [(set (match_operand:GPI 0 "register_operand" "=r")
	(ior:GPI (ashift:GPI (match_operand:GPI 1 "register_operand" "r")
			     (match_operand 3 "const_int_operand" "n"))
		 (lshiftrt:GPI (match_operand:GPI 2 "register_operand" "r")
			       (match_operand 4 "const_int_operand" "n"))))]
  "UINTVAL (operands[3]) < GET_MODE_BITSIZE (<MODE>mode) &&
   (UINTVAL (operands[3]) + UINTVAL (operands[4]) == GET_MODE_BITSIZE (<MODE>mode))"
  "extr\\t%<w>0, %<w>1, %<w>2, %4"
  [(set_attr "type" "rotate_imm")]
)

;; There are no canonicalisation rules for ashift and lshiftrt inside an ior
;; so we have to match both orderings.
(define_insn "*extr<mode>5_insn_alt"
  [(set (match_operand:GPI 0 "register_operand" "=r")
	(ior:GPI  (lshiftrt:GPI (match_operand:GPI 2 "register_operand" "r")
			        (match_operand 4 "const_int_operand" "n"))
		  (ashift:GPI (match_operand:GPI 1 "register_operand" "r")
			      (match_operand 3 "const_int_operand" "n"))))]
  "UINTVAL (operands[3]) < GET_MODE_BITSIZE (<MODE>mode)
   && (UINTVAL (operands[3]) + UINTVAL (operands[4])
       == GET_MODE_BITSIZE (<MODE>mode))"
  "extr\\t%<w>0, %<w>1, %<w>2, %4"
  [(set_attr "type" "rotate_imm")]
)

;; zero_extend version of the above
(define_insn "*extrsi5_insn_uxtw"
  [(set (match_operand:DI 0 "register_operand" "=r")
	(zero_extend:DI
	 (ior:SI (ashift:SI (match_operand:SI 1 "register_operand" "r")
			    (match_operand 3 "const_int_operand" "n"))
		 (lshiftrt:SI (match_operand:SI 2 "register_operand" "r")
			      (match_operand 4 "const_int_operand" "n")))))]
  "UINTVAL (operands[3]) < 32 &&
   (UINTVAL (operands[3]) + UINTVAL (operands[4]) == 32)"
  "extr\\t%w0, %w1, %w2, %4"
  [(set_attr "type" "rotate_imm")]
)

(define_insn "*extrsi5_insn_uxtw_alt"
  [(set (match_operand:DI 0 "register_operand" "=r")
	(zero_extend:DI
	 (ior:SI (lshiftrt:SI (match_operand:SI 2 "register_operand" "r")
			       (match_operand 4 "const_int_operand" "n"))
		 (ashift:SI (match_operand:SI 1 "register_operand" "r")
			    (match_operand 3 "const_int_operand" "n")))))]
  "UINTVAL (operands[3]) < 32 &&
   (UINTVAL (operands[3]) + UINTVAL (operands[4]) == 32)"
  "extr\\t%w0, %w1, %w2, %4"
  [(set_attr "type" "rotate_imm")]
)

(define_insn "*ror<mode>3_insn"
  [(set (match_operand:GPI 0 "register_operand" "=r")
	(rotate:GPI (match_operand:GPI 1 "register_operand" "r")
		    (match_operand 2 "const_int_operand" "n")))]
  "UINTVAL (operands[2]) < GET_MODE_BITSIZE (<MODE>mode)"
{
  operands[3] = GEN_INT (<sizen> - UINTVAL (operands[2]));
  return "ror\\t%<w>0, %<w>1, %3";
}
  [(set_attr "type" "rotate_imm")]
)

;; zero_extend version of the above
(define_insn "*rorsi3_insn_uxtw"
  [(set (match_operand:DI 0 "register_operand" "=r")
	(zero_extend:DI
	 (rotate:SI (match_operand:SI 1 "register_operand" "r")
		    (match_operand 2 "const_int_operand" "n"))))]
  "UINTVAL (operands[2]) < 32"
{
  operands[3] = GEN_INT (32 - UINTVAL (operands[2]));
  return "ror\\t%w0, %w1, %3";
}
  [(set_attr "type" "rotate_imm")]
)

(define_insn "*<ANY_EXTEND:optab><GPI:mode>_ashl<SHORT:mode>"
  [(set (match_operand:GPI 0 "register_operand" "=r")
	(ANY_EXTEND:GPI
	 (ashift:SHORT (match_operand:SHORT 1 "register_operand" "r")
		       (match_operand 2 "const_int_operand" "n"))))]
  "UINTVAL (operands[2]) < GET_MODE_BITSIZE (<SHORT:MODE>mode)"
{
  operands[3] = GEN_INT (<SHORT:sizen> - UINTVAL (operands[2]));
  return "<su>bfiz\t%<GPI:w>0, %<GPI:w>1, %2, %3";
}
  [(set_attr "type" "bfm")]
)

(define_insn "*zero_extend<GPI:mode>_lshr<SHORT:mode>"
  [(set (match_operand:GPI 0 "register_operand" "=r")
	(zero_extend:GPI
	 (lshiftrt:SHORT (match_operand:SHORT 1 "register_operand" "r")
			 (match_operand 2 "const_int_operand" "n"))))]
  "UINTVAL (operands[2]) < GET_MODE_BITSIZE (<SHORT:MODE>mode)"
{
  operands[3] = GEN_INT (<SHORT:sizen> - UINTVAL (operands[2]));
  return "ubfx\t%<GPI:w>0, %<GPI:w>1, %2, %3";
}
  [(set_attr "type" "bfm")]
)

(define_insn "*extend<GPI:mode>_ashr<SHORT:mode>"
  [(set (match_operand:GPI 0 "register_operand" "=r")
	(sign_extend:GPI
	 (ashiftrt:SHORT (match_operand:SHORT 1 "register_operand" "r")
			 (match_operand 2 "const_int_operand" "n"))))]
  "UINTVAL (operands[2]) < GET_MODE_BITSIZE (<SHORT:MODE>mode)"
{
  operands[3] = GEN_INT (<SHORT:sizen> - UINTVAL (operands[2]));
  return "sbfx\\t%<GPI:w>0, %<GPI:w>1, %2, %3";
}
  [(set_attr "type" "bfm")]
)

;; -------------------------------------------------------------------
;; Bitfields
;; -------------------------------------------------------------------

(define_expand "<optab>"
  [(set (match_operand:DI 0 "register_operand" "=r")
	(ANY_EXTRACT:DI (match_operand:DI 1 "register_operand" "r")
			(match_operand 2 "const_int_operand" "n")
			(match_operand 3 "const_int_operand" "n")))]
  ""
  ""
)

(define_insn "*<optab><mode>"
  [(set (match_operand:GPI 0 "register_operand" "=r")
	(ANY_EXTRACT:GPI (match_operand:GPI 1 "register_operand" "r")
			 (match_operand 2 "const_int_operand" "n")
			 (match_operand 3 "const_int_operand" "n")))]
  ""
  "<su>bfx\\t%<w>0, %<w>1, %3, %2"
  [(set_attr "type" "bfm")]
)

;; Bitfield Insert (insv)
(define_expand "insv<mode>"
  [(set (zero_extract:GPI (match_operand:GPI 0 "register_operand")
			  (match_operand 1 "const_int_operand")
			  (match_operand 2 "const_int_operand"))
	(match_operand:GPI 3 "general_operand"))]
  ""
{
  unsigned HOST_WIDE_INT width = UINTVAL (operands[1]);
  unsigned HOST_WIDE_INT pos = UINTVAL (operands[2]);
  rtx value = operands[3];

  if (width == 0 || (pos + width) > GET_MODE_BITSIZE (<MODE>mode))
    FAIL;

  if (CONST_INT_P (value))
    {
      unsigned HOST_WIDE_INT mask = ((unsigned HOST_WIDE_INT)1 << width) - 1;

      /* Prefer AND/OR for inserting all zeros or all ones.  */
      if ((UINTVAL (value) & mask) == 0
	   || (UINTVAL (value) & mask) == mask)
	FAIL;

      /* 16-bit aligned 16-bit wide insert is handled by insv_imm.  */
      if (width == 16 && (pos % 16) == 0)
	DONE;
    }
  operands[3] = force_reg (<MODE>mode, value);
})

(define_insn "*insv_reg<mode>"
  [(set (zero_extract:GPI (match_operand:GPI 0 "register_operand" "+r")
			  (match_operand 1 "const_int_operand" "n")
			  (match_operand 2 "const_int_operand" "n"))
	(match_operand:GPI 3 "register_operand" "r"))]
  "!(UINTVAL (operands[1]) == 0
     || (UINTVAL (operands[2]) + UINTVAL (operands[1])
	 > GET_MODE_BITSIZE (<MODE>mode)))"
  "bfi\\t%<w>0, %<w>3, %2, %1"
  [(set_attr "type" "bfm")]
)

(define_insn "*aarch64_bfi<GPI:mode><ALLX:mode>4"
  [(set (zero_extract:GPI (match_operand:GPI 0 "register_operand" "+r")
			  (match_operand 1 "const_int_operand" "n")
			  (match_operand 2 "const_int_operand" "n"))
	(zero_extend:GPI (match_operand:ALLX 3  "register_operand" "r")))]
  "UINTVAL (operands[1]) <= <ALLX:sizen>"
  "bfi\\t%<GPI:w>0, %<GPI:w>3, %2, %1"
  [(set_attr "type" "bfm")]
)

(define_insn "*extr_insv_lower_reg<mode>"
  [(set (zero_extract:GPI (match_operand:GPI 0 "register_operand" "+r")
			  (match_operand 1 "const_int_operand" "n")
			  (const_int 0))
	(zero_extract:GPI (match_operand:GPI 2 "register_operand" "r")
			  (match_dup 1)
			  (match_operand 3 "const_int_operand" "n")))]
  "!(UINTVAL (operands[1]) == 0
     || (UINTVAL (operands[3]) + UINTVAL (operands[1])
	 > GET_MODE_BITSIZE (<MODE>mode)))"
  "bfxil\\t%<w>0, %<w>2, %3, %1"
  [(set_attr "type" "bfm")]
)

(define_insn "*<optab><ALLX:mode>_shft_<GPI:mode>"
  [(set (match_operand:GPI 0 "register_operand" "=r")
	(ashift:GPI (ANY_EXTEND:GPI
		     (match_operand:ALLX 1 "register_operand" "r"))
		    (match_operand 2 "const_int_operand" "n")))]
  "UINTVAL (operands[2]) < <GPI:sizen>"
{
  operands[3] = (<ALLX:sizen> <= (<GPI:sizen> - UINTVAL (operands[2])))
	      ? GEN_INT (<ALLX:sizen>)
	      : GEN_INT (<GPI:sizen> - UINTVAL (operands[2]));
  return "<su>bfiz\t%<GPI:w>0, %<GPI:w>1, %2, %3";
}
  [(set_attr "type" "bfm")]
)

;; XXX We should match (any_extend (ashift)) here, like (and (ashift)) below

(define_insn "*andim_ashift<mode>_bfiz"
  [(set (match_operand:GPI 0 "register_operand" "=r")
	(and:GPI (ashift:GPI (match_operand:GPI 1 "register_operand" "r")
			     (match_operand 2 "const_int_operand" "n"))
		 (match_operand 3 "const_int_operand" "n")))]
  "aarch64_mask_and_shift_for_ubfiz_p (<MODE>mode, operands[3], operands[2])"
  "ubfiz\\t%<w>0, %<w>1, %2, %P3"
  [(set_attr "type" "bfm")]
)

(define_insn "bswap<mode>2"
  [(set (match_operand:GPI 0 "register_operand" "=r")
        (bswap:GPI (match_operand:GPI 1 "register_operand" "r")))]
  ""
  "rev\\t%<w>0, %<w>1"
  [(set_attr "type" "rev")]
)

(define_insn "bswaphi2"
  [(set (match_operand:HI 0 "register_operand" "=r")
        (bswap:HI (match_operand:HI 1 "register_operand" "r")))]
  ""
  "rev16\\t%w0, %w1"
  [(set_attr "type" "rev")]
)

;; There are no canonicalisation rules for the position of the lshiftrt, ashift
;; operations within an IOR/AND RTX, therefore we have two patterns matching
;; each valid permutation.

(define_insn "rev16<mode>2"
  [(set (match_operand:GPI 0 "register_operand" "=r")
        (ior:GPI (and:GPI (ashift:GPI (match_operand:GPI 1 "register_operand" "r")
                                      (const_int 8))
                          (match_operand:GPI 3 "const_int_operand" "n"))
                 (and:GPI (lshiftrt:GPI (match_dup 1)
                                        (const_int 8))
                          (match_operand:GPI 2 "const_int_operand" "n"))))]
  "aarch_rev16_shleft_mask_imm_p (operands[3], <MODE>mode)
   && aarch_rev16_shright_mask_imm_p (operands[2], <MODE>mode)"
  "rev16\\t%<w>0, %<w>1"
  [(set_attr "type" "rev")]
)

(define_insn "rev16<mode>2_alt"
  [(set (match_operand:GPI 0 "register_operand" "=r")
        (ior:GPI (and:GPI (lshiftrt:GPI (match_operand:GPI 1 "register_operand" "r")
                                        (const_int 8))
                          (match_operand:GPI 2 "const_int_operand" "n"))
                 (and:GPI (ashift:GPI (match_dup 1)
                                      (const_int 8))
                          (match_operand:GPI 3 "const_int_operand" "n"))))]
  "aarch_rev16_shleft_mask_imm_p (operands[3], <MODE>mode)
   && aarch_rev16_shright_mask_imm_p (operands[2], <MODE>mode)"
  "rev16\\t%<w>0, %<w>1"
  [(set_attr "type" "rev")]
)

;; zero_extend version of above
(define_insn "*bswapsi2_uxtw"
  [(set (match_operand:DI 0 "register_operand" "=r")
        (zero_extend:DI (bswap:SI (match_operand:SI 1 "register_operand" "r"))))]
  ""
  "rev\\t%w0, %w1"
  [(set_attr "type" "rev")]
)

;; -------------------------------------------------------------------
;; Floating-point intrinsics
;; -------------------------------------------------------------------

;; frint floating-point round to integral standard patterns.
;; Expands to btrunc, ceil, floor, nearbyint, rint, round, frintn.

(define_insn "<frint_pattern><mode>2"
  [(set (match_operand:GPF_F16 0 "register_operand" "=w")
	(unspec:GPF_F16 [(match_operand:GPF_F16 1 "register_operand" "w")]
	 FRINT))]
  "TARGET_FLOAT"
  "frint<frint_suffix>\\t%<s>0, %<s>1"
  [(set_attr "type" "f_rint<stype>")]
)

;; frcvt floating-point round to integer and convert standard patterns.
;; Expands to lbtrunc, lceil, lfloor, lround.
(define_insn "l<fcvt_pattern><su_optab><GPF_F16:mode><GPI:mode>2"
  [(set (match_operand:GPI 0 "register_operand" "=r")
	(FIXUORS:GPI
	  (unspec:GPF_F16 [(match_operand:GPF_F16 1 "register_operand" "w")]
	   FCVT)))]
  "TARGET_FLOAT"
  "fcvt<frint_suffix><su>\\t%<GPI:w>0, %<GPF_F16:s>1"
  [(set_attr "type" "f_cvtf2i")]
)

(define_insn "*aarch64_fcvt<su_optab><GPF:mode><GPI:mode>2_mult"
  [(set (match_operand:GPI 0 "register_operand" "=r")
	(FIXUORS:GPI
	  (mult:GPF
	    (match_operand:GPF 1 "register_operand" "w")
	    (match_operand:GPF 2 "aarch64_fp_pow2" "F"))))]
  "TARGET_FLOAT
   && IN_RANGE (aarch64_fpconst_pow_of_2 (operands[2]), 1,
		GET_MODE_BITSIZE (<GPI:MODE>mode))"
  {
    int fbits = aarch64_fpconst_pow_of_2 (operands[2]);
    char buf[64];
    snprintf (buf, 64, "fcvtz<su>\\t%%<GPI:w>0, %%<GPF:s>1, #%d", fbits);
    output_asm_insn (buf, operands);
    return "";
  }
  [(set_attr "type" "f_cvtf2i")]
)

;; fma - no throw

(define_insn "fma<mode>4"
  [(set (match_operand:GPF_F16 0 "register_operand" "=w")
        (fma:GPF_F16 (match_operand:GPF_F16 1 "register_operand" "w")
		     (match_operand:GPF_F16 2 "register_operand" "w")
		     (match_operand:GPF_F16 3 "register_operand" "w")))]
  "TARGET_FLOAT"
  "fmadd\\t%<s>0, %<s>1, %<s>2, %<s>3"
  [(set_attr "type" "fmac<stype>")]
)

(define_insn "fnma<mode>4"
  [(set (match_operand:GPF_F16 0 "register_operand" "=w")
	(fma:GPF_F16
	  (neg:GPF_F16 (match_operand:GPF_F16 1 "register_operand" "w"))
	  (match_operand:GPF_F16 2 "register_operand" "w")
	  (match_operand:GPF_F16 3 "register_operand" "w")))]
  "TARGET_FLOAT"
  "fmsub\\t%<s>0, %<s>1, %<s>2, %<s>3"
  [(set_attr "type" "fmac<stype>")]
)

(define_insn "fms<mode>4"
  [(set (match_operand:GPF 0 "register_operand" "=w")
        (fma:GPF (match_operand:GPF 1 "register_operand" "w")
		 (match_operand:GPF 2 "register_operand" "w")
		 (neg:GPF (match_operand:GPF 3 "register_operand" "w"))))]
  "TARGET_FLOAT"
  "fnmsub\\t%<s>0, %<s>1, %<s>2, %<s>3"
  [(set_attr "type" "fmac<s>")]
)

(define_insn "fnms<mode>4"
  [(set (match_operand:GPF 0 "register_operand" "=w")
	(fma:GPF (neg:GPF (match_operand:GPF 1 "register_operand" "w"))
		 (match_operand:GPF 2 "register_operand" "w")
		 (neg:GPF (match_operand:GPF 3 "register_operand" "w"))))]
  "TARGET_FLOAT"
  "fnmadd\\t%<s>0, %<s>1, %<s>2, %<s>3"
  [(set_attr "type" "fmac<s>")]
)

;; If signed zeros are ignored, -(a * b + c) = -a * b - c.
(define_insn "*fnmadd<mode>4"
  [(set (match_operand:GPF 0 "register_operand" "=w")
	(neg:GPF (fma:GPF (match_operand:GPF 1 "register_operand" "w")
			  (match_operand:GPF 2 "register_operand" "w")
			  (match_operand:GPF 3 "register_operand" "w"))))]
  "!HONOR_SIGNED_ZEROS (<MODE>mode) && TARGET_FLOAT"
  "fnmadd\\t%<s>0, %<s>1, %<s>2, %<s>3"
  [(set_attr "type" "fmac<s>")]
)

;; -------------------------------------------------------------------
;; Floating-point conversions
;; -------------------------------------------------------------------

(define_insn "extendsfdf2"
  [(set (match_operand:DF 0 "register_operand" "=w")
        (float_extend:DF (match_operand:SF 1 "register_operand" "w")))]
  "TARGET_FLOAT"
  "fcvt\\t%d0, %s1"
  [(set_attr "type" "f_cvt")]
)

(define_insn "extendhfsf2"
  [(set (match_operand:SF 0 "register_operand" "=w")
        (float_extend:SF (match_operand:HF 1 "register_operand" "w")))]
  "TARGET_FLOAT"
  "fcvt\\t%s0, %h1"
  [(set_attr "type" "f_cvt")]
)

(define_insn "extendhfdf2"
  [(set (match_operand:DF 0 "register_operand" "=w")
        (float_extend:DF (match_operand:HF 1 "register_operand" "w")))]
  "TARGET_FLOAT"
  "fcvt\\t%d0, %h1"
  [(set_attr "type" "f_cvt")]
)

(define_insn "truncdfsf2"
  [(set (match_operand:SF 0 "register_operand" "=w")
        (float_truncate:SF (match_operand:DF 1 "register_operand" "w")))]
  "TARGET_FLOAT"
  "fcvt\\t%s0, %d1"
  [(set_attr "type" "f_cvt")]
)

(define_insn "truncsfhf2"
  [(set (match_operand:HF 0 "register_operand" "=w")
        (float_truncate:HF (match_operand:SF 1 "register_operand" "w")))]
  "TARGET_FLOAT"
  "fcvt\\t%h0, %s1"
  [(set_attr "type" "f_cvt")]
)

(define_insn "truncdfhf2"
  [(set (match_operand:HF 0 "register_operand" "=w")
        (float_truncate:HF (match_operand:DF 1 "register_operand" "w")))]
  "TARGET_FLOAT"
  "fcvt\\t%h0, %d1"
  [(set_attr "type" "f_cvt")]
)

(define_insn "<optab>_trunc<GPF_F16:mode><GPI:mode>2"
  [(set (match_operand:GPI 0 "register_operand" "=r")
	(FIXUORS:GPI (match_operand:GPF_F16 1 "register_operand" "w")))]
  "TARGET_FLOAT"
  "fcvtz<su>\t%<GPI:w>0, %<GPF_F16:s>1"
  [(set_attr "type" "f_cvtf2i")]
)

(define_insn "<optab><fcvt_target><GPF:mode>2"
  [(set (match_operand:GPF 0 "register_operand" "=w,w")
        (FLOATUORS:GPF (match_operand:<FCVT_TARGET> 1 "register_operand" "w,r")))]
  "TARGET_FLOAT"
  "@
   <su_optab>cvtf\t%<GPF:s>0, %<s>1
   <su_optab>cvtf\t%<GPF:s>0, %<w1>1"
  [(set_attr "simd" "yes,no")
   (set_attr "fp" "no,yes")
   (set_attr "type" "neon_int_to_fp_<Vetype>,f_cvti2f")]
)

(define_insn "<optab><fcvt_iesize><GPF:mode>2"
  [(set (match_operand:GPF 0 "register_operand" "=w")
        (FLOATUORS:GPF (match_operand:<FCVT_IESIZE> 1 "register_operand" "r")))]
  "TARGET_FLOAT"
  "<su_optab>cvtf\t%<GPF:s>0, %<w2>1"
  [(set_attr "type" "f_cvti2f")]
)

(define_insn "<optab><mode>hf2"
  [(set (match_operand:HF 0 "register_operand" "=w")
	(FLOATUORS:HF (match_operand:GPI 1 "register_operand" "r")))]
  "TARGET_FP_F16INST"
  "<su_optab>cvtf\t%h0, %<w>1"
  [(set_attr "type" "f_cvti2f")]
)

;; Convert between fixed-point and floating-point (scalar modes)

(define_insn "<FCVT_F2FIXED:fcvt_fixed_insn><GPF:mode>3"
  [(set (match_operand:<GPF:FCVT_TARGET> 0 "register_operand" "=r, w")
	(unspec:<GPF:FCVT_TARGET> [(match_operand:GPF 1 "register_operand" "w, w")
				   (match_operand:SI 2 "immediate_operand" "i, i")]
	 FCVT_F2FIXED))]
  ""
  "@
   <FCVT_F2FIXED:fcvt_fixed_insn>\t%<GPF:w1>0, %<GPF:s>1, #%2
   <FCVT_F2FIXED:fcvt_fixed_insn>\t%<GPF:s>0, %<GPF:s>1, #%2"
  [(set_attr "type" "f_cvtf2i, neon_fp_to_int_<GPF:Vetype>")
   (set_attr "fp" "yes, *")
   (set_attr "simd" "*, yes")]
)

(define_insn "<FCVT_FIXED2F:fcvt_fixed_insn><GPI:mode>3"
  [(set (match_operand:<GPI:FCVT_TARGET> 0 "register_operand" "=w, w")
	(unspec:<GPI:FCVT_TARGET> [(match_operand:GPI 1 "register_operand" "r, w")
				   (match_operand:SI 2 "immediate_operand" "i, i")]
	 FCVT_FIXED2F))]
  ""
  "@
   <FCVT_FIXED2F:fcvt_fixed_insn>\t%<GPI:v>0, %<GPI:w>1, #%2
   <FCVT_FIXED2F:fcvt_fixed_insn>\t%<GPI:v>0, %<GPI:v>1, #%2"
  [(set_attr "type" "f_cvti2f, neon_int_to_fp_<GPI:Vetype>")
   (set_attr "fp" "yes, *")
   (set_attr "simd" "*, yes")]
)

(define_insn "<FCVT_F2FIXED:fcvt_fixed_insn>hf<mode>3"
  [(set (match_operand:GPI 0 "register_operand" "=r")
	(unspec:GPI [(match_operand:HF 1 "register_operand" "w")
		     (match_operand:SI 2 "immediate_operand" "i")]
	 FCVT_F2FIXED))]
  "TARGET_FP_F16INST"
   "<FCVT_F2FIXED:fcvt_fixed_insn>\t%<GPI:w>0, %h1, #%2"
  [(set_attr "type" "f_cvtf2i")]
)

(define_insn "<FCVT_FIXED2F:fcvt_fixed_insn><mode>hf3"
  [(set (match_operand:HF 0 "register_operand" "=w")
	(unspec:HF [(match_operand:GPI 1 "register_operand" "r")
		    (match_operand:SI 2 "immediate_operand" "i")]
	 FCVT_FIXED2F))]
  "TARGET_FP_F16INST"
  "<FCVT_FIXED2F:fcvt_fixed_insn>\t%h0, %<GPI:w>1, #%2"
  [(set_attr "type" "f_cvti2f")]
)

(define_insn "<FCVT_F2FIXED:fcvt_fixed_insn>hf3"
  [(set (match_operand:HI 0 "register_operand" "=w")
	(unspec:HI [(match_operand:HF 1 "register_operand" "w")
		    (match_operand:SI 2 "immediate_operand" "i")]
	 FCVT_F2FIXED))]
  "TARGET_SIMD"
  "<FCVT_F2FIXED:fcvt_fixed_insn>\t%h0, %h1, #%2"
  [(set_attr "type" "neon_fp_to_int_s")]
)

(define_insn "<FCVT_FIXED2F:fcvt_fixed_insn>hi3"
  [(set (match_operand:HF 0 "register_operand" "=w")
	(unspec:HF [(match_operand:HI 1 "register_operand" "w")
		    (match_operand:SI 2 "immediate_operand" "i")]
	 FCVT_FIXED2F))]
  "TARGET_SIMD"
  "<FCVT_FIXED2F:fcvt_fixed_insn>\t%h0, %h1, #%2"
  [(set_attr "type" "neon_int_to_fp_s")]
)

;; -------------------------------------------------------------------
;; Floating-point arithmetic
;; -------------------------------------------------------------------

(define_insn "add<mode>3"
  [(set (match_operand:GPF_F16 0 "register_operand" "=w")
	(plus:GPF_F16
	 (match_operand:GPF_F16 1 "register_operand" "w")
	 (match_operand:GPF_F16 2 "register_operand" "w")))]
  "TARGET_FLOAT"
  "fadd\\t%<s>0, %<s>1, %<s>2"
  [(set_attr "type" "fadd<stype>")]
)

(define_insn "sub<mode>3"
  [(set (match_operand:GPF_F16 0 "register_operand" "=w")
	(minus:GPF_F16
	 (match_operand:GPF_F16 1 "register_operand" "w")
	 (match_operand:GPF_F16 2 "register_operand" "w")))]
  "TARGET_FLOAT"
  "fsub\\t%<s>0, %<s>1, %<s>2"
  [(set_attr "type" "fadd<stype>")]
)

(define_insn "mul<mode>3"
  [(set (match_operand:GPF_F16 0 "register_operand" "=w")
	(mult:GPF_F16
	 (match_operand:GPF_F16 1 "register_operand" "w")
	 (match_operand:GPF_F16 2 "register_operand" "w")))]
  "TARGET_FLOAT"
  "fmul\\t%<s>0, %<s>1, %<s>2"
  [(set_attr "type" "fmul<stype>")]
)

(define_insn "*fnmul<mode>3"
  [(set (match_operand:GPF 0 "register_operand" "=w")
        (mult:GPF
		 (neg:GPF (match_operand:GPF 1 "register_operand" "w"))
		 (match_operand:GPF 2 "register_operand" "w")))]
  "TARGET_FLOAT && !flag_rounding_math"
  "fnmul\\t%<s>0, %<s>1, %<s>2"
  [(set_attr "type" "fmul<s>")]
)

(define_insn "*fnmul<mode>3"
  [(set (match_operand:GPF 0 "register_operand" "=w")
        (neg:GPF (mult:GPF
		 (match_operand:GPF 1 "register_operand" "w")
		 (match_operand:GPF 2 "register_operand" "w"))))]
  "TARGET_FLOAT"
  "fnmul\\t%<s>0, %<s>1, %<s>2"
  [(set_attr "type" "fmul<s>")]
)

(define_expand "div<mode>3"
 [(set (match_operand:GPF_F16 0 "register_operand")
       (div:GPF_F16 (match_operand:GPF_F16 1 "general_operand")
		    (match_operand:GPF_F16 2 "register_operand")))]
 "TARGET_SIMD"
{
  if (aarch64_emit_approx_div (operands[0], operands[1], operands[2]))
    DONE;

  operands[1] = force_reg (<MODE>mode, operands[1]);
})

(define_insn "*div<mode>3"
  [(set (match_operand:GPF_F16 0 "register_operand" "=w")
	(div:GPF_F16 (match_operand:GPF_F16 1 "register_operand" "w")
		     (match_operand:GPF_F16 2 "register_operand" "w")))]
  "TARGET_FLOAT"
  "fdiv\\t%<s>0, %<s>1, %<s>2"
  [(set_attr "type" "fdiv<stype>")]
)

(define_insn "neg<mode>2"
  [(set (match_operand:GPF_F16 0 "register_operand" "=w")
	(neg:GPF_F16 (match_operand:GPF_F16 1 "register_operand" "w")))]
  "TARGET_FLOAT"
  "fneg\\t%<s>0, %<s>1"
  [(set_attr "type" "ffarith<stype>")]
)

(define_expand "sqrt<mode>2"
  [(set (match_operand:GPF_F16 0 "register_operand" "=w")
	(sqrt:GPF_F16 (match_operand:GPF_F16 1 "register_operand" "w")))]
  "TARGET_FLOAT"
{
  if (aarch64_emit_approx_sqrt (operands[0], operands[1], false))
    DONE;
})

(define_insn "*sqrt<mode>2"
  [(set (match_operand:GPF_F16 0 "register_operand" "=w")
	(sqrt:GPF_F16 (match_operand:GPF_F16 1 "register_operand" "w")))]
  "TARGET_FLOAT"
  "fsqrt\\t%<s>0, %<s>1"
  [(set_attr "type" "fsqrt<stype>")]
)

(define_insn "abs<mode>2"
  [(set (match_operand:GPF_F16 0 "register_operand" "=w")
	(abs:GPF_F16 (match_operand:GPF_F16 1 "register_operand" "w")))]
  "TARGET_FLOAT"
  "fabs\\t%<s>0, %<s>1"
  [(set_attr "type" "ffarith<stype>")]
)

;; Given that smax/smin do not specify the result when either input is NaN,
;; we could use either FMAXNM or FMAX for smax, and either FMINNM or FMIN
;; for smin.

(define_insn "smax<mode>3"
  [(set (match_operand:GPF 0 "register_operand" "=w")
        (smax:GPF (match_operand:GPF 1 "register_operand" "w")
		  (match_operand:GPF 2 "register_operand" "w")))]
  "TARGET_FLOAT"
  "fmaxnm\\t%<s>0, %<s>1, %<s>2"
  [(set_attr "type" "f_minmax<s>")]
)

(define_insn "smin<mode>3"
  [(set (match_operand:GPF 0 "register_operand" "=w")
        (smin:GPF (match_operand:GPF 1 "register_operand" "w")
		  (match_operand:GPF 2 "register_operand" "w")))]
  "TARGET_FLOAT"
  "fminnm\\t%<s>0, %<s>1, %<s>2"
  [(set_attr "type" "f_minmax<s>")]
)

;; Scalar forms for the IEEE-754 fmax()/fmin() functions
(define_insn "<fmaxmin><mode>3"
  [(set (match_operand:GPF_F16 0 "register_operand" "=w")
	(unspec:GPF_F16 [(match_operand:GPF_F16 1 "register_operand" "w")
		     (match_operand:GPF_F16 2 "register_operand" "w")]
		     FMAXMIN))]
  "TARGET_FLOAT"
  "<fmaxmin_op>\\t%<s>0, %<s>1, %<s>2"
  [(set_attr "type" "f_minmax<stype>")]
)

;; For copysign (x, y), we want to generate:
;;
;;   LDR d2, #(1 << 63)
;;   BSL v2.8b, [y], [x]
;;
;; or another, equivalent, sequence using one of BSL/BIT/BIF.
;; aarch64_simd_bsldf will select the best suited of these instructions
;; to generate based on register allocation, and knows how to partially
;; constant fold based on the values of X and Y, so expand through that.

(define_expand "copysigndf3"
  [(match_operand:DF 0 "register_operand")
   (match_operand:DF 1 "register_operand")
   (match_operand:DF 2 "register_operand")]
  "TARGET_FLOAT && TARGET_SIMD"
{
  rtx mask = gen_reg_rtx (DImode);
  emit_move_insn (mask, GEN_INT (HOST_WIDE_INT_1U << 63));
  emit_insn (gen_aarch64_simd_bsldf (operands[0], mask,
				     operands[2], operands[1]));
  DONE;
}
)

;; As above, but we must first get to a 64-bit value if we wish to use
;; aarch64_simd_bslv2sf.

(define_expand "copysignsf3"
  [(match_operand:SF 0 "register_operand")
   (match_operand:SF 1 "register_operand")
   (match_operand:SF 2 "register_operand")]
  "TARGET_FLOAT && TARGET_SIMD"
{
  rtx mask = gen_reg_rtx (DImode);

  /* Juggle modes to get us in to a vector mode for BSL.  */
  rtx op1 = lowpart_subreg (V2SFmode, operands[1], SFmode);
  rtx op2 = lowpart_subreg (V2SFmode, operands[2], SFmode);
  rtx tmp = gen_reg_rtx (V2SFmode);
  emit_move_insn (mask, GEN_INT (HOST_WIDE_INT_1U << 31));
  emit_insn (gen_aarch64_simd_bslv2sf (tmp, mask, op2, op1));
  emit_move_insn (operands[0], lowpart_subreg (SFmode, tmp, V2SFmode));
  DONE;
}
)

;; -------------------------------------------------------------------
;; Reload support
;; -------------------------------------------------------------------
;; Reload Scalar Floating point modes from constant pool.
;; The AArch64 port doesn't have __int128 constant move support.
(define_expand "aarch64_reload_movcp<GPF_TF:mode><P:mode>"
 [(set (match_operand:GPF_TF 0 "register_operand" "=w")
       (mem:GPF_TF (match_operand 1 "aarch64_constant_pool_symref" "S")))
  (clobber (match_operand:P 2 "register_operand" "=&r"))]
 "TARGET_FLOAT && aarch64_nopcrelative_literal_loads"
 {
   aarch64_expand_mov_immediate (operands[2], XEXP (operands[1], 0));
   emit_move_insn (operands[0], gen_rtx_MEM (<GPF_TF:MODE>mode, operands[2]));
   DONE;
 }
)

;; Reload Vector modes from constant pool.
(define_expand "aarch64_reload_movcp<VALL:mode><P:mode>"
 [(set (match_operand:VALL 0 "register_operand" "=w")
       (mem:VALL (match_operand 1 "aarch64_constant_pool_symref" "S")))
  (clobber (match_operand:P 2 "register_operand" "=&r"))]
 "TARGET_FLOAT && aarch64_nopcrelative_literal_loads"
 {
   aarch64_expand_mov_immediate (operands[2], XEXP (operands[1], 0));
   emit_move_insn (operands[0], gen_rtx_MEM (<VALL:MODE>mode, operands[2]));
   DONE;
 }
)

(define_expand "aarch64_reload_mov<mode>"
  [(set (match_operand:TX 0 "register_operand" "=w")
        (match_operand:TX 1 "register_operand" "w"))
   (clobber (match_operand:DI 2 "register_operand" "=&r"))
  ]
  "TARGET_FLOAT"
  {
    rtx op0 = simplify_gen_subreg (TImode, operands[0], <MODE>mode, 0);
    rtx op1 = simplify_gen_subreg (TImode, operands[1], <MODE>mode, 0);
    gen_aarch64_movtilow_tilow (op0, op1);
    gen_aarch64_movdi_tihigh (operands[2], op1);
    gen_aarch64_movtihigh_di (op0, operands[2]);
    DONE;
  }
)

;; The following secondary reload helpers patterns are invoked
;; after or during reload as we don't want these patterns to start
;; kicking in during the combiner.

(define_insn "aarch64_movdi_<mode>low"
  [(set (match_operand:DI 0 "register_operand" "=r")
	(zero_extract:DI (match_operand:TX 1 "register_operand" "w")
			 (const_int 64) (const_int 0)))]
  "TARGET_FLOAT && (reload_completed || reload_in_progress)"
  "fmov\\t%x0, %d1"
  [(set_attr "type" "f_mrc")
   (set_attr "length" "4")
  ])

(define_insn "aarch64_movdi_<mode>high"
  [(set (match_operand:DI 0 "register_operand" "=r")
	(zero_extract:DI (match_operand:TX 1 "register_operand" "w")
			 (const_int 64) (const_int 64)))]
  "TARGET_FLOAT && (reload_completed || reload_in_progress)"
  "fmov\\t%x0, %1.d[1]"
  [(set_attr "type" "f_mrc")
   (set_attr "length" "4")
  ])

(define_insn "aarch64_mov<mode>high_di"
  [(set (zero_extract:TX (match_operand:TX 0 "register_operand" "+w")
                         (const_int 64) (const_int 64))
        (zero_extend:TX (match_operand:DI 1 "register_operand" "r")))]
  "TARGET_FLOAT && (reload_completed || reload_in_progress)"
  "fmov\\t%0.d[1], %x1"
  [(set_attr "type" "f_mcr")
   (set_attr "length" "4")
  ])

(define_insn "aarch64_mov<mode>low_di"
  [(set (match_operand:TX 0 "register_operand" "=w")
        (zero_extend:TX (match_operand:DI 1 "register_operand" "r")))]
  "TARGET_FLOAT && (reload_completed || reload_in_progress)"
  "fmov\\t%d0, %x1"
  [(set_attr "type" "f_mcr")
   (set_attr "length" "4")
  ])

(define_insn "aarch64_movtilow_tilow"
  [(set (match_operand:TI 0 "register_operand" "=w")
        (zero_extend:TI
	  (truncate:DI (match_operand:TI 1 "register_operand" "w"))))]
  "TARGET_FLOAT && (reload_completed || reload_in_progress)"
  "fmov\\t%d0, %d1"
  [(set_attr "type" "fmov")
   (set_attr "length" "4")
  ])

;; There is a deliberate reason why the parameters of high and lo_sum's
;; don't have modes for ADRP and ADD instructions.  This is to allow high
;; and lo_sum's to be used with the labels defining the jump tables in
;; rodata section.

(define_expand "add_losym"
  [(set (match_operand 0 "register_operand" "=r")
	(lo_sum (match_operand 1 "register_operand" "r")
		(match_operand 2 "aarch64_valid_symref" "S")))]
  ""
{
  machine_mode mode = GET_MODE (operands[0]);

  emit_insn ((mode == DImode
	      ? gen_add_losym_di
	      : gen_add_losym_si) (operands[0],
				   operands[1],
				   operands[2]));
  DONE;
})

(define_insn "add_losym_<mode>"
  [(set (match_operand:P 0 "register_operand" "=r")
	(lo_sum:P (match_operand:P 1 "register_operand" "r")
		  (match_operand 2 "aarch64_valid_symref" "S")))]
  ""
  "add\\t%<w>0, %<w>1, :lo12:%a2"
  [(set_attr "type" "alu_imm")]
)

(define_insn "ldr_got_small_<mode>"
  [(set (match_operand:PTR 0 "register_operand" "=r")
	(unspec:PTR [(mem:PTR (lo_sum:PTR
			      (match_operand:PTR 1 "register_operand" "r")
			      (match_operand:PTR 2 "aarch64_valid_symref" "S")))]
		    UNSPEC_GOTSMALLPIC))]
  ""
  "ldr\\t%<w>0, [%1, #:got_lo12:%a2]"
  [(set_attr "type" "load1")]
)

(define_insn "ldr_got_small_sidi"
  [(set (match_operand:DI 0 "register_operand" "=r")
	(zero_extend:DI
	 (unspec:SI [(mem:SI (lo_sum:DI
			     (match_operand:DI 1 "register_operand" "r")
			     (match_operand:DI 2 "aarch64_valid_symref" "S")))]
		    UNSPEC_GOTSMALLPIC)))]
  "TARGET_ILP32"
  "ldr\\t%w0, [%1, #:got_lo12:%a2]"
  [(set_attr "type" "load1")]
)

(define_insn "ldr_got_small_28k_<mode>"
  [(set (match_operand:PTR 0 "register_operand" "=r")
	(unspec:PTR [(mem:PTR (lo_sum:PTR
			      (match_operand:PTR 1 "register_operand" "r")
			      (match_operand:PTR 2 "aarch64_valid_symref" "S")))]
		    UNSPEC_GOTSMALLPIC28K))]
  ""
  "ldr\\t%<w>0, [%1, #:<got_modifier>:%a2]"
  [(set_attr "type" "load1")]
)

(define_insn "ldr_got_small_28k_sidi"
  [(set (match_operand:DI 0 "register_operand" "=r")
	(zero_extend:DI
	 (unspec:SI [(mem:SI (lo_sum:DI
			     (match_operand:DI 1 "register_operand" "r")
			     (match_operand:DI 2 "aarch64_valid_symref" "S")))]
		    UNSPEC_GOTSMALLPIC28K)))]
  "TARGET_ILP32"
  "ldr\\t%w0, [%1, #:gotpage_lo14:%a2]"
  [(set_attr "type" "load1")]
)

(define_insn "ldr_got_tiny"
  [(set (match_operand:DI 0 "register_operand" "=r")
	(unspec:DI [(match_operand:DI 1 "aarch64_valid_symref" "S")]
		   UNSPEC_GOTTINYPIC))]
  ""
  "ldr\\t%0, %L1"
  [(set_attr "type" "load1")]
)

(define_insn "aarch64_load_tp_hard"
  [(set (match_operand:DI 0 "register_operand" "=r")
	(unspec:DI [(const_int 0)] UNSPEC_TLS))]
  ""
  "mrs\\t%0, tpidr_el0"
  [(set_attr "type" "mrs")]
)

;; The TLS ABI specifically requires that the compiler does not schedule
;; instructions in the TLS stubs, in order to enable linker relaxation.
;; Therefore we treat the stubs as an atomic sequence.
(define_expand "tlsgd_small"
 [(parallel [(set (match_operand 0 "register_operand" "")
                  (call (mem:DI (match_dup 2)) (const_int 1)))
	     (unspec:DI [(match_operand:DI 1 "aarch64_valid_symref" "")] UNSPEC_GOTSMALLTLS)
	     (clobber (reg:DI LR_REGNUM))])]
 ""
{
  operands[2] = aarch64_tls_get_addr ();
})

(define_insn "*tlsgd_small"
  [(set (match_operand 0 "register_operand" "")
	(call (mem:DI (match_operand:DI 2 "" "")) (const_int 1)))
   (unspec:DI [(match_operand:DI 1 "aarch64_valid_symref" "S")] UNSPEC_GOTSMALLTLS)
   (clobber (reg:DI LR_REGNUM))
  ]
  ""
  "adrp\\tx0, %A1\;add\\tx0, x0, %L1\;bl\\t%2\;nop"
  [(set_attr "type" "call")
   (set_attr "length" "16")])

(define_insn "tlsie_small_<mode>"
  [(set (match_operand:PTR 0 "register_operand" "=r")
        (unspec:PTR [(match_operand 1 "aarch64_tls_ie_symref" "S")]
		   UNSPEC_GOTSMALLTLS))]
  ""
  "adrp\\t%0, %A1\;ldr\\t%<w>0, [%0, #%L1]"
  [(set_attr "type" "load1")
   (set_attr "length" "8")]
)

(define_insn "tlsie_small_sidi"
  [(set (match_operand:DI 0 "register_operand" "=r")
	(zero_extend:DI
          (unspec:SI [(match_operand 1 "aarch64_tls_ie_symref" "S")]
		      UNSPEC_GOTSMALLTLS)))]
  ""
  "adrp\\t%0, %A1\;ldr\\t%w0, [%0, #%L1]"
  [(set_attr "type" "load1")
   (set_attr "length" "8")]
)

(define_insn "tlsie_tiny_<mode>"
  [(set (match_operand:PTR 0 "register_operand" "=&r")
	(unspec:PTR [(match_operand 1 "aarch64_tls_ie_symref" "S")
		     (match_operand:PTR 2 "register_operand" "r")]
		   UNSPEC_GOTTINYTLS))]
  ""
  "ldr\\t%<w>0, %L1\;add\\t%<w>0, %<w>0, %<w>2"
  [(set_attr "type" "multiple")
   (set_attr "length" "8")]
)

(define_insn "tlsie_tiny_sidi"
  [(set (match_operand:DI 0 "register_operand" "=&r")
	(zero_extend:DI
	  (unspec:SI [(match_operand 1 "aarch64_tls_ie_symref" "S")
		      (match_operand:DI 2 "register_operand" "r")
		      ]
		      UNSPEC_GOTTINYTLS)))]
  ""
  "ldr\\t%w0, %L1\;add\\t%w0, %w0, %w2"
  [(set_attr "type" "multiple")
   (set_attr "length" "8")]
)

(define_insn "tlsle12_<mode>"
  [(set (match_operand:P 0 "register_operand" "=r")
	(unspec:P [(match_operand:P 1 "register_operand" "r")
		   (match_operand 2 "aarch64_tls_le_symref" "S")]
		   UNSPEC_TLSLE12))]
  ""
  "add\\t%<w>0, %<w>1, #%L2";
  [(set_attr "type" "alu_sreg")
   (set_attr "length" "4")]
)

(define_insn "tlsle24_<mode>"
  [(set (match_operand:P 0 "register_operand" "=r")
	(unspec:P [(match_operand:P 1 "register_operand" "r")
		   (match_operand 2 "aarch64_tls_le_symref" "S")]
		   UNSPEC_TLSLE24))]
  ""
  "add\\t%<w>0, %<w>1, #%G2, lsl #12\;add\\t%<w>0, %<w>0, #%L2"
  [(set_attr "type" "multiple")
   (set_attr "length" "8")]
)

(define_insn "tlsle32_<mode>"
  [(set (match_operand:P 0 "register_operand" "=r")
	(unspec:P [(match_operand 1 "aarch64_tls_le_symref" "S")]
		   UNSPEC_TLSLE32))]
  ""
  "movz\\t%<w>0, #:tprel_g1:%1\;movk\\t%<w>0, #:tprel_g0_nc:%1"
  [(set_attr "type" "multiple")
   (set_attr "length" "8")]
)

(define_insn "tlsle48_<mode>"
  [(set (match_operand:P 0 "register_operand" "=r")
	(unspec:P [(match_operand 1 "aarch64_tls_le_symref" "S")]
		   UNSPEC_TLSLE48))]
  ""
  "movz\\t%<w>0, #:tprel_g2:%1\;movk\\t%<w>0, #:tprel_g1_nc:%1\;movk\\t%<w>0, #:tprel_g0_nc:%1"
  [(set_attr "type" "multiple")
   (set_attr "length" "12")]
)

(define_insn "tlsdesc_small_<mode>"
  [(set (reg:PTR R0_REGNUM)
        (unspec:PTR [(match_operand 0 "aarch64_valid_symref" "S")]
		   UNSPEC_TLSDESC))
   (clobber (reg:DI LR_REGNUM))
   (clobber (reg:CC CC_REGNUM))
   (clobber (match_scratch:DI 1 "=r"))]
  "TARGET_TLS_DESC"
  "adrp\\tx0, %A0\;ldr\\t%<w>1, [x0, #%L0]\;add\\t<w>0, <w>0, %L0\;.tlsdesccall\\t%0\;blr\\t%1"
  [(set_attr "type" "call")
   (set_attr "length" "16")])

(define_insn "stack_tie"
  [(set (mem:BLK (scratch))
	(unspec:BLK [(match_operand:DI 0 "register_operand" "rk")
		     (match_operand:DI 1 "register_operand" "rk")]
		    UNSPEC_PRLG_STK))]
  ""
  ""
  [(set_attr "length" "0")]
)

;; UNSPEC_VOLATILE is considered to use and clobber all hard registers and
;; all of memory.  This blocks insns from being moved across this point.

(define_insn "blockage"
  [(unspec_volatile [(const_int 0)] UNSPECV_BLOCKAGE)]
  ""
  ""
  [(set_attr "length" "0")
   (set_attr "type" "block")]
)

(define_insn "probe_stack_range_<PTR:mode>"
  [(set (match_operand:PTR 0 "register_operand" "=r")
	(unspec_volatile:PTR [(match_operand:PTR 1 "register_operand" "0")
			      (match_operand:PTR 2 "register_operand" "r")]
			       UNSPECV_PROBE_STACK_RANGE))]
  ""
{
  return aarch64_output_probe_stack_range (operands[0], operands[2]);
}
  [(set_attr "length" "32")]
)

;; Named pattern for expanding thread pointer reference.
(define_expand "get_thread_pointerdi"
  [(match_operand:DI 0 "register_operand" "=r")]
  ""
{
  rtx tmp = aarch64_load_tp (operands[0]);
  if (tmp != operands[0])
    emit_move_insn (operands[0], tmp);
  DONE;
})

;; Named patterns for stack smashing protection.
(define_expand "stack_protect_set"
  [(match_operand 0 "memory_operand")
   (match_operand 1 "memory_operand")]
  ""
{
  machine_mode mode = GET_MODE (operands[0]);

  emit_insn ((mode == DImode
	      ? gen_stack_protect_set_di
	      : gen_stack_protect_set_si) (operands[0], operands[1]));
  DONE;
})

(define_insn "stack_protect_set_<mode>"
  [(set (match_operand:PTR 0 "memory_operand" "=m")
	(unspec:PTR [(match_operand:PTR 1 "memory_operand" "m")]
	 UNSPEC_SP_SET))
   (set (match_scratch:PTR 2 "=&r") (const_int 0))]
  ""
  "ldr\\t%<w>2, %1\;str\\t%<w>2, %0\;mov\t%<w>2,0"
  [(set_attr "length" "12")
   (set_attr "type" "multiple")])

(define_expand "stack_protect_test"
  [(match_operand 0 "memory_operand")
   (match_operand 1 "memory_operand")
   (match_operand 2)]
  ""
{
  rtx result;
  machine_mode mode = GET_MODE (operands[0]);

  result = gen_reg_rtx(mode);

  emit_insn ((mode == DImode
	      ? gen_stack_protect_test_di
	      : gen_stack_protect_test_si) (result,
					    operands[0],
					    operands[1]));

  if (mode == DImode)
    emit_jump_insn (gen_cbranchdi4 (gen_rtx_EQ (VOIDmode, result, const0_rtx),
				    result, const0_rtx, operands[2]));
  else
    emit_jump_insn (gen_cbranchsi4 (gen_rtx_EQ (VOIDmode, result, const0_rtx),
				    result, const0_rtx, operands[2]));
  DONE;
})

(define_insn "stack_protect_test_<mode>"
  [(set (match_operand:PTR 0 "register_operand" "=r")
	(unspec:PTR [(match_operand:PTR 1 "memory_operand" "m")
		     (match_operand:PTR 2 "memory_operand" "m")]
	 UNSPEC_SP_TEST))
   (clobber (match_scratch:PTR 3 "=&r"))]
  ""
  "ldr\t%<w>3, %1\;ldr\t%<w>0, %2\;eor\t%<w>0, %<w>3, %<w>0"
  [(set_attr "length" "12")
   (set_attr "type" "multiple")])

;; Write Floating-point Control Register.
(define_insn "set_fpcr"
  [(unspec_volatile [(match_operand:SI 0 "register_operand" "r")] UNSPECV_SET_FPCR)]
  ""
  "msr\\tfpcr, %0"
  [(set_attr "type" "mrs")])

;; Read Floating-point Control Register.
(define_insn "get_fpcr"
  [(set (match_operand:SI 0 "register_operand" "=r")
        (unspec_volatile:SI [(const_int 0)] UNSPECV_GET_FPCR))]
  ""
  "mrs\\t%0, fpcr"
  [(set_attr "type" "mrs")])

;; Write Floating-point Status Register.
(define_insn "set_fpsr"
  [(unspec_volatile [(match_operand:SI 0 "register_operand" "r")] UNSPECV_SET_FPSR)]
  ""
  "msr\\tfpsr, %0"
  [(set_attr "type" "mrs")])

;; Read Floating-point Status Register.
(define_insn "get_fpsr"
  [(set (match_operand:SI 0 "register_operand" "=r")
        (unspec_volatile:SI [(const_int 0)] UNSPECV_GET_FPSR))]
  ""
  "mrs\\t%0, fpsr"
  [(set_attr "type" "mrs")])


;; Define the subtract-one-and-jump insns so loop.c
;; knows what to generate.
(define_expand "doloop_end"
  [(use (match_operand 0 "" ""))      ; loop pseudo
   (use (match_operand 1 "" ""))]     ; label
  "optimize > 0 && flag_modulo_sched"
{
  rtx s0;
  rtx bcomp;
  rtx loc_ref;
  rtx cc_reg;
  rtx insn;
  rtx cmp;

  /* Currently SMS relies on the do-loop pattern to recognize loops
     where (1) the control part consists of all insns defining and/or
     using a certain 'count' register and (2) the loop count can be
     adjusted by modifying this register prior to the loop.
     ??? The possible introduction of a new block to initialize the
     new IV can potentially affect branch optimizations.  */

  if (GET_MODE (operands[0]) != DImode)
    FAIL;

  s0 = operands [0];
  insn = emit_insn (gen_adddi3_compare0 (s0, s0, GEN_INT (-1)));

  cmp = XVECEXP (PATTERN (insn), 0, 0);
  cc_reg = SET_DEST (cmp);
  bcomp = gen_rtx_NE (VOIDmode, cc_reg, const0_rtx);
  loc_ref = gen_rtx_LABEL_REF (VOIDmode, operands [1]);
  emit_jump_insn (gen_rtx_SET (pc_rtx,
			       gen_rtx_IF_THEN_ELSE (VOIDmode, bcomp,
						     loc_ref, pc_rtx)));
  DONE;
})

;; AdvSIMD Stuff
(include "aarch64-simd.md")

;; Atomic Operations
(include "atomics.md")

;; ldp/stp peephole patterns
(include "aarch64-ldpstp.md")
