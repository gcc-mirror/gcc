;; Machine description for AArch64 architecture.
;; Copyright (C) 2009-2022 Free Software Foundation, Inc.
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
    (R17_REGNUM		17)
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
    (SP_REGNUM		31)
    (V0_REGNUM		32)
    (V1_REGNUM		33)
    (V2_REGNUM		34)
    (V3_REGNUM		35)
    (V4_REGNUM		36)
    (V5_REGNUM		37)
    (V6_REGNUM		38)
    (V7_REGNUM		39)
    (V8_REGNUM		40)
    (V9_REGNUM		41)
    (V10_REGNUM		42)
    (V11_REGNUM		43)
    (V12_REGNUM		44)
    (V13_REGNUM		45)
    (V14_REGNUM		46)
    (V15_REGNUM		47)
    (V16_REGNUM		48)
    (V17_REGNUM		49)
    (V18_REGNUM		50)
    (V19_REGNUM		51)
    (V20_REGNUM		52)
    (V21_REGNUM		53)
    (V22_REGNUM		54)
    (V23_REGNUM		55)
    (V24_REGNUM		56)
    (V25_REGNUM		57)
    (V26_REGNUM		58)
    (V27_REGNUM		59)
    (V28_REGNUM		60)
    (V29_REGNUM		61)
    (V30_REGNUM		62)
    (V31_REGNUM		63)
    (SFP_REGNUM		64)
    (AP_REGNUM		65)
    (CC_REGNUM		66)
    ;; Defined only to make the DWARF description simpler.
    (VG_REGNUM		67)
    (P0_REGNUM		68)
    (P1_REGNUM		69)
    (P2_REGNUM		70)
    (P3_REGNUM		71)
    (P4_REGNUM		72)
    (P5_REGNUM		73)
    (P6_REGNUM		74)
    (P7_REGNUM		75)
    (P8_REGNUM		76)
    (P9_REGNUM		77)
    (P10_REGNUM		78)
    (P11_REGNUM		79)
    (P12_REGNUM		80)
    (P13_REGNUM		81)
    (P14_REGNUM		82)
    (P15_REGNUM		83)
    (LAST_SAVED_REGNUM	83)
    (FFR_REGNUM		84)
    ;; "FFR token": a fake register used for representing the scheduling
    ;; restrictions on FFR-related operations.
    (FFRT_REGNUM	85)
    ;; The pair of scratch registers used for stack probing with -fstack-check.
    ;; Leave R9 alone as a possible choice for the static chain.
    ;; Note that the use of these registers is mutually exclusive with the use
    ;; of STACK_CLASH_SVE_CFA_REGNUM, which is for -fstack-clash-protection
    ;; rather than -fstack-check.
    (PROBE_STACK_FIRST_REGNUM  10)
    (PROBE_STACK_SECOND_REGNUM 11)
    ;; Scratch register used by stack clash protection to calculate
    ;; SVE CFA offsets during probing.
    (STACK_CLASH_SVE_CFA_REGNUM 11)
    ;; Scratch registers for prologue/epilogue use.
    (EP0_REGNUM         12)
    (EP1_REGNUM         13)
    ;; A couple of call-clobbered registers that we need to reserve when
    ;; tracking speculation this is not ABI, so is subject to change.
    (SPECULATION_SCRATCH_REGNUM 14)
    (SPECULATION_TRACKER_REGNUM 15)
    ;; Scratch registers used in frame layout.
    (IP0_REGNUM         16)
    (IP1_REGNUM         17)
    (FP_REGNUM		29)
    (LR_REGNUM          30)
  ]
)

(define_c_enum "unspec" [
    UNSPEC_AUTIA1716
    UNSPEC_AUTIB1716
    UNSPEC_AUTIASP
    UNSPEC_AUTIBSP
    UNSPEC_CALLEE_ABI
    UNSPEC_CASESI
    UNSPEC_CPYMEM
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
    UNSPEC_FJCVTZS
    UNSPEC_FRINT32Z
    UNSPEC_FRINT32X
    UNSPEC_FRINT64Z
    UNSPEC_FRINT64X
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
    UNSPEC_LD2_DREG
    UNSPEC_LD2_DUP
    UNSPEC_LD3
    UNSPEC_LD3_DREG
    UNSPEC_LD3_DUP
    UNSPEC_LD4
    UNSPEC_LD4_DREG
    UNSPEC_LD4_DUP
    UNSPEC_LD2_LANE
    UNSPEC_LD3_LANE
    UNSPEC_LD4_LANE
    UNSPEC_LD64B
    UNSPEC_ST64B
    UNSPEC_ST64BV
    UNSPEC_ST64BV_RET
    UNSPEC_ST64BV0
    UNSPEC_ST64BV0_RET
    UNSPEC_MB
    UNSPEC_MOVMEM
    UNSPEC_NOP
    UNSPEC_PACIA1716
    UNSPEC_PACIB1716
    UNSPEC_PACIASP
    UNSPEC_PACIBSP
    UNSPEC_PRLG_STK
    UNSPEC_REV
    UNSPEC_RBIT
    UNSPEC_SABAL
    UNSPEC_SABAL2
    UNSPEC_SABDL
    UNSPEC_SABDL2
    UNSPEC_SADALP
    UNSPEC_SCVTF
    UNSPEC_SETMEM
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
    UNSPEC_UABAL
    UNSPEC_UABAL2
    UNSPEC_UABDL
    UNSPEC_UABDL2
    UNSPEC_UADALP
    UNSPEC_UCVTF
    UNSPEC_USHL_2S
    UNSPEC_VSTRUCTDUMMY
    UNSPEC_SSP_SYSREG
    UNSPEC_SP_SET
    UNSPEC_SP_TEST
    UNSPEC_RSHRN
    UNSPEC_RSQRT
    UNSPEC_RSQRTE
    UNSPEC_RSQRTS
    UNSPEC_NZCV
    UNSPEC_XPACLRI
    UNSPEC_LD1_SVE
    UNSPEC_ST1_SVE
    UNSPEC_LDNT1_SVE
    UNSPEC_STNT1_SVE
    UNSPEC_LD1RQ
    UNSPEC_LD1_GATHER
    UNSPEC_LDFF1_GATHER
    UNSPEC_LDNT1_GATHER
    UNSPEC_ST1_SCATTER
    UNSPEC_STNT1_SCATTER
    UNSPEC_PRED_X
    UNSPEC_PRED_Z
    UNSPEC_PTEST
    UNSPEC_PTRUE
    UNSPEC_UNPACKSHI
    UNSPEC_UNPACKUHI
    UNSPEC_UNPACKSLO
    UNSPEC_UNPACKULO
    UNSPEC_PACK
    UNSPEC_WHILEGE
    UNSPEC_WHILEGT
    UNSPEC_WHILEHI
    UNSPEC_WHILEHS
    UNSPEC_WHILELE
    UNSPEC_WHILELO
    UNSPEC_WHILELS
    UNSPEC_WHILELT
    UNSPEC_WHILERW
    UNSPEC_WHILEWR
    UNSPEC_LDN
    UNSPEC_STN
    UNSPEC_INSR
    UNSPEC_CLASTA
    UNSPEC_CLASTB
    UNSPEC_FADDA
    UNSPEC_REV_SUBREG
    UNSPEC_REINTERPRET
    UNSPEC_SPECULATION_TRACKER
    UNSPEC_SPECULATION_TRACKER_REV
    UNSPEC_COPYSIGN
    UNSPEC_TTEST		; Represent transaction test.
    UNSPEC_UPDATE_FFR
    UNSPEC_UPDATE_FFRT
    UNSPEC_RDFFR
    UNSPEC_WRFFR
    ;; Represents an SVE-style lane index, in which the indexing applies
    ;; within the containing 128-bit block.
    UNSPEC_SVE_LANE_SELECT
    UNSPEC_SVE_CNT_PAT
    UNSPEC_SVE_PREFETCH
    UNSPEC_SVE_PREFETCH_GATHER
    UNSPEC_SVE_COMPACT
    UNSPEC_SVE_SPLICE
    UNSPEC_GEN_TAG		; Generate a 4-bit MTE tag.
    UNSPEC_GEN_TAG_RND		; Generate a random 4-bit MTE tag.
    UNSPEC_TAG_SPACE		; Translate address to MTE tag address space.
    UNSPEC_LD1RO
    UNSPEC_SALT_ADDR
])

(define_c_enum "unspecv" [
    UNSPECV_EH_RETURN		; Represent EH_RETURN
    UNSPECV_GET_FPCR		; Represent fetch of FPCR content.
    UNSPECV_SET_FPCR		; Represent assign of FPCR content.
    UNSPECV_GET_FPSR		; Represent fetch of FPSR content.
    UNSPECV_SET_FPSR		; Represent assign of FPSR content.
    UNSPECV_BLOCKAGE		; Represent a blockage
    UNSPECV_PROBE_STACK_RANGE	; Represent stack range probing.
    UNSPECV_SPECULATION_BARRIER ; Represent speculation barrier.
    UNSPECV_BTI_NOARG		; Represent BTI.
    UNSPECV_BTI_C		; Represent BTI c.
    UNSPECV_BTI_J		; Represent BTI j.
    UNSPECV_BTI_JC		; Represent BTI jc.
    UNSPECV_TSTART		; Represent transaction start.
    UNSPECV_TCOMMIT		; Represent transaction commit.
    UNSPECV_TCANCEL		; Represent transaction cancel.
    UNSPEC_RNDR			; Represent RNDR
    UNSPEC_RNDRRS		; Represent RNDRRS
  ]
)

;; These constants are used as a const_int in various SVE unspecs
;; to indicate whether the governing predicate is known to be a PTRUE.
(define_constants
  [; Indicates that the predicate might not be a PTRUE.
   (SVE_MAYBE_NOT_PTRUE 0)

   ; Indicates that the predicate is known to be a PTRUE.
   (SVE_KNOWN_PTRUE 1)])

;; These constants are used as a const_int in predicated SVE FP arithmetic
;; to indicate whether the operation is allowed to make additional lanes
;; active without worrying about the effect on faulting behavior.
(define_constants
  [; Indicates either that all lanes are active or that the instruction may
   ; operate on inactive inputs even if doing so could induce a fault.
   (SVE_RELAXED_GP 0)

   ; Indicates that some lanes might be inactive and that the instruction
   ; must not operate on inactive inputs if doing so could induce a fault.
   (SVE_STRICT_GP 1)])

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

;; Attributes of the architecture required to support the instruction (or
;; alternative). This attribute is used to compute attribute "enabled", use type
;; "any" to enable an alternative in all cases.
;;
;; As a convenience, "fp_q" means "fp" + the ability to move between
;; Q registers and is equivalent to "simd".

(define_enum "arches" [ any rcpc8_4 fp fp_q simd sve fp16])

(define_enum_attr "arch" "arches" (const_string "any"))

;; [For compatibility with Arm in pipeline models]
;; Attribute that specifies whether or not the instruction touches fp
;; registers.
;; Note that this attribute is not used anywhere in either the arm or aarch64
;; backends except in the scheduling description for xgene1.  In that
;; scheduling description this attribute is used to subclass the load_4 and
;; load_8 types.
(define_attr "fp" "no,yes"
  (if_then_else
    (eq_attr "arch" "fp")
    (const_string "yes")
    (const_string "no")))

(define_attr "arch_enabled" "no,yes"
  (if_then_else
    (ior
	(eq_attr "arch" "any")

	(and (eq_attr "arch" "rcpc8_4")
	     (match_test "AARCH64_ISA_RCPC8_4"))

	(and (eq_attr "arch" "fp")
	     (match_test "TARGET_FLOAT"))

	(and (eq_attr "arch" "fp_q, simd")
	     (match_test "TARGET_SIMD"))

	(and (eq_attr "arch" "fp16")
	     (match_test "TARGET_FP_F16INST"))

	(and (eq_attr "arch" "sve")
	     (match_test "TARGET_SVE")))
    (const_string "yes")
    (const_string "no")))

;; Attribute that controls whether an alternative is enabled or not.
;; Currently it is only used to disable alternatives which touch fp or simd
;; registers when -mgeneral-regs-only is specified or to require a special
;; architecture support.
(define_attr "enabled" "no,yes" (attr "arch_enabled"))

;; Attribute that specifies whether we are dealing with a branch to a
;; label that is far away, i.e. further away than the maximum/minimum
;; representable in a signed 21-bits number.
;; 0 :=: no
;; 1 :=: yes
(define_attr "far_branch" "" (const_int 0))

;; Attribute that specifies whether the alternative uses MOVPRFX.
(define_attr "movprfx" "no,yes" (const_string "no"))

;; Attribute to specify that an alternative has the length of a single
;; instruction plus a speculation barrier.
(define_attr "sls_length" "none,retbr,casesi" (const_string "none"))

(define_attr "length" ""
  (cond [(eq_attr "movprfx" "yes")
           (const_int 8)

	 (eq_attr "sls_length" "retbr")
	   (cond [(match_test "!aarch64_harden_sls_retbr_p ()") (const_int 4)
		  (match_test "TARGET_SB") (const_int 8)]
		 (const_int 12))

	 (eq_attr "sls_length" "casesi")
	   (cond [(match_test "!aarch64_harden_sls_retbr_p ()") (const_int 16)
		  (match_test "TARGET_SB") (const_int 20)]
		 (const_int 24))
	]
	  (const_int 4)))

;; Strictly for compatibility with AArch32 in pipeline models, since AArch64 has
;; no predicated insns.
(define_attr "predicated" "yes,no" (const_string "no"))

;; Set to true on an insn that requires the speculation tracking state to be
;; in the tracking register before the insn issues.  Otherwise the compiler
;; may chose to hold the tracking state encoded in SP.
(define_attr "speculation_barrier" "true,false" (const_string "false"))

;; -------------------------------------------------------------------
;; Pipeline descriptions and scheduling
;; -------------------------------------------------------------------

;; Processor types.
(include "aarch64-tune.md")

;; Scheduling
(include "../arm/cortex-a53.md")
(include "../arm/cortex-a57.md")
(include "../arm/exynos-m1.md")
(include "falkor.md")
(include "saphira.md")
(include "thunderx.md")
(include "../arm/xgene1.md")
(include "thunderx2t99.md")
(include "tsv110.md")
(include "thunderx3t110.md")

;; -------------------------------------------------------------------
;; Jumps and other miscellaneous insns
;; -------------------------------------------------------------------

(define_insn "indirect_jump"
  [(set (pc) (match_operand:DI 0 "register_operand" "r"))]
  ""
  {
    output_asm_insn ("br\\t%0", operands);
    return aarch64_sls_barrier (aarch64_harden_sls_retbr_p ());
  }
  [(set_attr "type" "branch")
   (set_attr "sls_length" "retbr")]
)

(define_insn "jump"
  [(set (pc) (label_ref (match_operand 0 "" "")))]
  ""
  "b\\t%l0"
  [(set_attr "type" "branch")]
)

(define_expand "cbranch<mode>4"
  [(set (pc) (if_then_else (match_operator 0 "aarch64_comparison_operator"
			    [(match_operand:GPI 1 "register_operand")
			     (match_operand:GPI 2 "aarch64_plus_operand")])
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
			    [(match_operand:GPF 1 "register_operand")
			     (match_operand:GPF 2 "aarch64_fp_compare_operand")])
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
	       [(match_operand 1 "cc_register")
	        (match_operand 2 "const0_operand")])
	      (label_ref (match_operand 3 "" ""))
	      (pc)))]
  ""
  "")

(define_insn "@ccmp<CC_ONLY:mode><GPI:mode>"
  [(set (match_operand:CC_ONLY 1 "cc_register" "")
	(if_then_else:CC_ONLY
	  (match_operator 4 "aarch64_comparison_operator"
	   [(match_operand 0 "cc_register" "")
	    (const_int 0)])
	  (compare:CC_ONLY
	    (match_operand:GPI 2 "register_operand" "r,r,r")
	    (match_operand:GPI 3 "aarch64_ccmp_operand" "r,Uss,Usn"))
	  (unspec:CC_ONLY
	    [(match_operand 5 "immediate_operand")]
	    UNSPEC_NZCV)))]
  ""
  "@
   ccmp\\t%<w>2, %<w>3, %k5, %m4
   ccmp\\t%<w>2, %3, %k5, %m4
   ccmn\\t%<w>2, #%n3, %k5, %m4"
  [(set_attr "type" "alus_sreg,alus_imm,alus_imm")]
)

(define_insn "@ccmp<CCFP_CCFPE:mode><GPF:mode>"
  [(set (match_operand:CCFP_CCFPE 1 "cc_register" "")
	(if_then_else:CCFP_CCFPE
	  (match_operator 4 "aarch64_comparison_operator"
	   [(match_operand 0 "cc_register" "")
	    (const_int 0)])
	  (compare:CCFP_CCFPE
	    (match_operand:GPF 2 "register_operand" "w")
	    (match_operand:GPF 3 "register_operand" "w"))
	  (unspec:CCFP_CCFPE
	    [(match_operand 5 "immediate_operand")]
	    UNSPEC_NZCV)))]
  "TARGET_FLOAT"
  "fccmp<e>\\t%<s>2, %<s>3, %k5, %m4"
  [(set_attr "type" "fccmp<s>")]
)

(define_insn "@ccmp<CC_ONLY:mode><GPI:mode>_rev"
  [(set (match_operand:CC_ONLY 1 "cc_register" "")
	(if_then_else:CC_ONLY
	  (match_operator 4 "aarch64_comparison_operator"
	   [(match_operand 0 "cc_register" "")
	    (const_int 0)])
	  (unspec:CC_ONLY
	    [(match_operand 5 "immediate_operand")]
	    UNSPEC_NZCV)
	  (compare:CC_ONLY
	    (match_operand:GPI 2 "register_operand" "r,r,r")
	    (match_operand:GPI 3 "aarch64_ccmp_operand" "r,Uss,Usn"))))]
  ""
  "@
   ccmp\\t%<w>2, %<w>3, %k5, %M4
   ccmp\\t%<w>2, %3, %k5, %M4
   ccmn\\t%<w>2, #%n3, %k5, %M4"
  [(set_attr "type" "alus_sreg,alus_imm,alus_imm")]
)

(define_insn "@ccmp<CCFP_CCFPE:mode><GPF:mode>_rev"
  [(set (match_operand:CCFP_CCFPE 1 "cc_register" "")
	(if_then_else:CCFP_CCFPE
	  (match_operator 4 "aarch64_comparison_operator"
	   [(match_operand 0 "cc_register" "")
	    (const_int 0)])
	  (unspec:CCFP_CCFPE
	    [(match_operand 5 "immediate_operand")]
	    UNSPEC_NZCV)
	  (compare:CCFP_CCFPE
	    (match_operand:GPF 2 "register_operand" "w")
	    (match_operand:GPF 3 "register_operand" "w"))))]
  "TARGET_FLOAT"
  "fccmp<e>\\t%<s>2, %<s>3, %k5, %M4"
  [(set_attr "type" "fccmp<s>")]
)

;; Expansion of signed mod by a power of 2 using CSNEG.
;; For x0 % n where n is a power of 2 produce:
;; negs   x1, x0
;; and    x0, x0, #(n - 1)
;; and    x1, x1, #(n - 1)
;; csneg  x0, x0, x1, mi

(define_expand "mod<mode>3"
  [(match_operand:GPI 0 "register_operand")
   (match_operand:GPI 1 "register_operand")
   (match_operand:GPI 2 "const_int_operand")]
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
    /* GCC's traditional style has been to use "beq" instead of "b.eq", etc.,
       but the "." is required for SVE conditions.  */
    bool use_dot_p = GET_MODE (operands[1]) == CC_NZCmode;
    if (get_attr_length (insn) == 8)
      return aarch64_gen_far_branch (operands, 2, "Lbcond",
				     use_dot_p ? "b.%M0\\t" : "b%M0\\t");
    else
      return use_dot_p ? "b.%m0\\t%l2" : "b%m0\\t%l2";
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
(define_insn_and_split "*compare_condjump<GPI:mode>"
  [(set (pc) (if_then_else (EQL
			      (match_operand:GPI 0 "register_operand" "r")
			      (match_operand:GPI 1 "aarch64_imm24" "n"))
			   (label_ref:P (match_operand 2 "" ""))
			   (pc)))]
  "!aarch64_move_imm (INTVAL (operands[1]), <GPI:MODE>mode)
   && !aarch64_plus_operand (operands[1], <GPI:MODE>mode)
   && !reload_completed"
  "#"
  "&& true"
  [(const_int 0)]
  {
    HOST_WIDE_INT lo_imm = UINTVAL (operands[1]) & 0xfff;
    HOST_WIDE_INT hi_imm = UINTVAL (operands[1]) & 0xfff000;
    rtx tmp = gen_reg_rtx (<GPI:MODE>mode);
    emit_insn (gen_add<GPI:mode>3 (tmp, operands[0], GEN_INT (-hi_imm)));
    emit_insn (gen_add<GPI:mode>3_compare0 (tmp, tmp, GEN_INT (-lo_imm)));
    rtx cc_reg = gen_rtx_REG (CC_NZmode, CC_REGNUM);
    rtx cmp_rtx = gen_rtx_fmt_ee (<EQL:CMP>, <GPI:MODE>mode,
				  cc_reg, const0_rtx);
    emit_jump_insn (gen_condjump (cmp_rtx, cc_reg, operands[2]));
    DONE;
  }
)

(define_expand "casesi"
  [(match_operand:SI 0 "register_operand")	; Index
   (match_operand:SI 1 "const_int_operand")	; Lower bound
   (match_operand:SI 2 "const_int_operand")	; Total range
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

	operands[1]
	  = GEN_INT (trunc_int_for_mode (-UINTVAL (operands[1]), SImode));

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

    operands[2] = force_reg (DImode, gen_rtx_LABEL_REF (DImode, operands[3]));
    operands[2]
      = gen_rtx_UNSPEC (Pmode, gen_rtvec (2, operands[2], operands[0]),
			UNSPEC_CASESI);
    operands[2] = gen_rtx_MEM (DImode, operands[2]);
    MEM_READONLY_P (operands[2]) = 1;
    MEM_NOTRAP_P (operands[2]) = 1;
    emit_jump_insn (gen_casesi_dispatch (operands[2], operands[3]));
    DONE;
  }
)

(define_expand "casesi_dispatch"
  [(parallel
    [(set (pc) (match_operand:DI 0 ""))
     (clobber (reg:CC CC_REGNUM))
     (clobber (match_scratch:DI 2))
     (clobber (match_scratch:DI 3))
     (use (label_ref:DI (match_operand 1 "")))])]
  "")

(define_insn "*casesi_dispatch"
  [(parallel
    [(set (pc)
	  (mem:DI (unspec [(match_operand:DI 0 "register_operand" "r")
			   (match_operand:SI 1 "register_operand" "r")]
			UNSPEC_CASESI)))
     (clobber (reg:CC CC_REGNUM))
     (clobber (match_scratch:DI 3 "=r"))
     (clobber (match_scratch:DI 4 "=r"))
     (use (label_ref:DI (match_operand 2 "" "")))])]
  ""
  "*
  return aarch64_output_casesi (operands);
  "
  [(set_attr "sls_length" "casesi")
   (set_attr "type" "branch")]
)

(define_insn "nop"
  [(unspec[(const_int 0)] UNSPEC_NOP)]
  ""
  "nop"
  [(set_attr "type" "no_insn")]
)

(define_insn "prefetch"
  [(prefetch (match_operand:DI 0 "aarch64_prefetch_operand" "Dp")
            (match_operand:QI 1 "const_int_operand" "")
            (match_operand:QI 2 "const_int_operand" ""))]
  ""
  {
    const char * pftype[2][4] =
    {
      {"prfm\\tPLDL1STRM, %0",
       "prfm\\tPLDL3KEEP, %0",
       "prfm\\tPLDL2KEEP, %0",
       "prfm\\tPLDL1KEEP, %0"},
      {"prfm\\tPSTL1STRM, %0",
       "prfm\\tPSTL3KEEP, %0",
       "prfm\\tPSTL2KEEP, %0",
       "prfm\\tPSTL1KEEP, %0"},
    };

    int locality = INTVAL (operands[2]);

    gcc_assert (IN_RANGE (locality, 0, 3));

    /* PRFM accepts the same addresses as a 64-bit LDR so wrap
       the address into a DImode MEM so that aarch64_print_operand knows
       how to print it.  */
    operands[0] = gen_rtx_MEM (DImode, operands[0]);
    return pftype[INTVAL(operands[1])][locality];
  }
  [(set_attr "type" "load_4")]
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
  {
    const char *ret = NULL;
    if (aarch64_return_address_signing_enabled ()
	&& (TARGET_PAUTH))
      {
	if (aarch64_ra_sign_key == AARCH64_KEY_B)
	  ret = "retab";
	else
	  ret = "retaa";
      }
    else
      ret = "ret";
    output_asm_insn (ret, operands);
    return aarch64_sls_barrier (aarch64_harden_sls_retbr_p ());
  }
  [(set_attr "type" "branch")
   (set_attr "sls_length" "retbr")]
)

(define_expand "return"
  [(simple_return)]
  "aarch64_use_return_insn_p ()"
  ""
)

(define_insn "simple_return"
  [(simple_return)]
  ""
  {
    output_asm_insn ("ret", operands);
    return aarch64_sls_barrier (aarch64_harden_sls_retbr_p ());
  }
  [(set_attr "type" "branch")
   (set_attr "sls_length" "retbr")]
)

(define_insn "*cb<optab><mode>1"
  [(set (pc) (if_then_else (EQL (match_operand:GPI 0 "register_operand" "r")
				(const_int 0))
			   (label_ref (match_operand 1 "" ""))
			   (pc)))]
  "!aarch64_track_speculation"
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
				    (match_operand 1
				      "aarch64_simd_shift_imm_<mode>" "n"))
		   (const_int 0))
	     (label_ref (match_operand 2 "" ""))
	     (pc)))
   (clobber (reg:CC CC_REGNUM))]
  "!aarch64_track_speculation"
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
  "!aarch64_track_speculation"
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

(define_expand "call"
  [(parallel
     [(call (match_operand 0 "memory_operand")
	    (match_operand 1 "general_operand"))
      (unspec:DI [(match_operand 2 "const_int_operand")] UNSPEC_CALLEE_ABI)
      (clobber (reg:DI LR_REGNUM))])]
  ""
  "
  {
    aarch64_expand_call (NULL_RTX, operands[0], operands[2], false);
    DONE;
  }"
)

(define_insn "*call_insn"
  [(call (mem:DI (match_operand:DI 0 "aarch64_call_insn_operand" "Ucr, Usf"))
	 (match_operand 1 "" ""))
   (unspec:DI [(match_operand:DI 2 "const_int_operand")] UNSPEC_CALLEE_ABI)
   (clobber (reg:DI LR_REGNUM))]
  ""
  "@
  * return aarch64_indirect_call_asm (operands[0]);
  bl\\t%c0"
  [(set_attr "type" "call, call")])

(define_expand "call_value"
  [(parallel
     [(set (match_operand 0 "")
	   (call (match_operand 1 "memory_operand")
		 (match_operand 2 "general_operand")))
     (unspec:DI [(match_operand 3 "const_int_operand")] UNSPEC_CALLEE_ABI)
     (clobber (reg:DI LR_REGNUM))])]
  ""
  "
  {
    aarch64_expand_call (operands[0], operands[1], operands[3], false);
    DONE;
  }"
)

(define_insn "*call_value_insn"
  [(set (match_operand 0 "" "")
	(call (mem:DI (match_operand:DI 1 "aarch64_call_insn_operand" "Ucr, Usf"))
		      (match_operand 2 "" "")))
   (unspec:DI [(match_operand:DI 3 "const_int_operand")] UNSPEC_CALLEE_ABI)
   (clobber (reg:DI LR_REGNUM))]
  ""
  "@
  * return aarch64_indirect_call_asm (operands[1]);
  bl\\t%c1"
  [(set_attr "type" "call, call")]
)

(define_expand "sibcall"
  [(parallel
     [(call (match_operand 0 "memory_operand")
	    (match_operand 1 "general_operand"))
      (unspec:DI [(match_operand 2 "const_int_operand")] UNSPEC_CALLEE_ABI)
      (return)])]
  ""
  {
    aarch64_expand_call (NULL_RTX, operands[0], operands[2], true);
    DONE;
  }
)

(define_expand "sibcall_value"
  [(parallel
     [(set (match_operand 0 "")
	   (call (match_operand 1 "memory_operand")
		 (match_operand 2 "general_operand")))
      (unspec:DI [(match_operand 3 "const_int_operand")] UNSPEC_CALLEE_ABI)
      (return)])]
  ""
  {
    aarch64_expand_call (operands[0], operands[1], operands[3], true);
    DONE;
  }
)

(define_insn "*sibcall_insn"
  [(call (mem:DI (match_operand:DI 0 "aarch64_call_insn_operand" "Ucs, Usf"))
	 (match_operand 1 ""))
   (unspec:DI [(match_operand:DI 2 "const_int_operand")] UNSPEC_CALLEE_ABI)
   (return)]
  "SIBLING_CALL_P (insn)"
  {
    if (which_alternative == 0)
      {
	output_asm_insn ("br\\t%0", operands);
	return aarch64_sls_barrier (aarch64_harden_sls_retbr_p ());
      }
    return "b\\t%c0";
  }
  [(set_attr "type" "branch, branch")
   (set_attr "sls_length" "retbr,none")]
)

(define_insn "*sibcall_value_insn"
  [(set (match_operand 0 "")
	(call (mem:DI
		(match_operand:DI 1 "aarch64_call_insn_operand" "Ucs, Usf"))
	      (match_operand 2 "")))
   (unspec:DI [(match_operand:DI 3 "const_int_operand")] UNSPEC_CALLEE_ABI)
   (return)]
  "SIBLING_CALL_P (insn)"
  {
    if (which_alternative == 0)
      {
	output_asm_insn ("br\\t%1", operands);
	return aarch64_sls_barrier (aarch64_harden_sls_retbr_p ());
      }
    return "b\\t%c1";
  }
  [(set_attr "type" "branch, branch")
   (set_attr "sls_length" "retbr,none")]
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

  /* Untyped calls always use the default ABI.  It's only possible to use
     ABI variants if we know the type of the target function.  */
  emit_call_insn (gen_call (operands[0], const0_rtx, const0_rtx));

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
  [(set (match_operand:SHORT 0 "nonimmediate_operand")
	(match_operand:SHORT 1 "general_operand"))]
  ""
  "
    if (GET_CODE (operands[0]) == MEM && operands[1] != const0_rtx)
      operands[1] = force_reg (<MODE>mode, operands[1]);

    if (GET_CODE (operands[1]) == CONST_POLY_INT)
      {
	aarch64_expand_mov_immediate (operands[0], operands[1]);
	DONE;
      }
  "
)

(define_insn "*mov<mode>_aarch64"
  [(set (match_operand:SHORT 0 "nonimmediate_operand" "=r,r,    w,r  ,r,w, m,m,r,w,w")
	(match_operand:SHORT 1 "aarch64_mov_operand"  " r,M,D<hq>,Usv,m,m,rZ,w,w,rZ,w"))]
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
       return aarch64_output_sve_cnt_immediate (\"cnt\", \"%x0\", operands[1]);
     case 4:
       return "ldr<size>\t%w0, %1";
     case 5:
       return "ldr\t%<size>0, %1";
     case 6:
       return "str<size>\t%w1, %0";
     case 7:
       return "str\t%<size>1, %0";
     case 8:
       return TARGET_SIMD ? "umov\t%w0, %1.<v>[0]" : "fmov\t%w0, %s1";
     case 9:
       return TARGET_SIMD ? "dup\t%0.<Vallxd>, %w1" : "fmov\t%s0, %w1";
     case 10:
       return TARGET_SIMD ? "dup\t%<Vetype>0, %1.<v>[0]" : "fmov\t%s0, %s1";
     default:
       gcc_unreachable ();
     }
}
  ;; The "mov_imm" type for CNT is just a placeholder.
  [(set_attr "type" "mov_reg,mov_imm,neon_move,mov_imm,load_4,load_4,store_4,
		     store_4,neon_to_gp<q>,neon_from_gp<q>,neon_dup")
   (set_attr "arch" "*,*,simd,sve,*,*,*,*,*,*,*")]
)

(define_expand "mov<mode>"
  [(set (match_operand:GPI 0 "nonimmediate_operand")
	(match_operand:GPI 1 "general_operand"))]
  ""
  "
    if (MEM_P (operands[0]) && !MEM_VOLATILE_P (operands[0])
	&& CONST_INT_P (operands[1]) && <MODE>mode == DImode
	&& aarch64_split_dimode_const_store (operands[0], operands[1]))
      DONE;

    if (GET_CODE (operands[0]) == MEM && operands[1] != const0_rtx)
      operands[1] = force_reg (<MODE>mode, operands[1]);

    /* Lower moves of symbolic constants into individual instructions.
       Doing this now is sometimes necessary for correctness, since some
       sequences require temporary pseudo registers.  Lowering now is also
       often better for optimization, since more RTL passes get the
       chance to optimize the individual instructions.

       When called after RA, also split multi-instruction moves into
       smaller pieces now, since we can't be sure that sure that there
       will be a following split pass.  */
    if (CONST_INT_P (operands[1])
	? (reload_completed
	   && !aarch64_mov_imm_operand (operands[1], <MODE>mode))
	: CONSTANT_P (operands[1]))
     {
       aarch64_expand_mov_immediate (operands[0], operands[1]);
       DONE;
     }
  "
)

(define_insn_and_split "*movsi_aarch64"
  [(set (match_operand:SI 0 "nonimmediate_operand" "=r,k,r,r,r,r, r,w, m, m,  r,  r,  r, w,r,w, w")
	(match_operand:SI 1 "aarch64_mov_operand"  " r,r,k,M,n,Usv,m,m,rZ,w,Usw,Usa,Ush,rZ,w,w,Ds"))]
  "(register_operand (operands[0], SImode)
    || aarch64_reg_or_zero (operands[1], SImode))"
  "@
   mov\\t%w0, %w1
   mov\\t%w0, %w1
   mov\\t%w0, %w1
   mov\\t%w0, %1
   #
   * return aarch64_output_sve_cnt_immediate (\"cnt\", \"%x0\", operands[1]);
   ldr\\t%w0, %1
   ldr\\t%s0, %1
   str\\t%w1, %0
   str\\t%s1, %0
   adrp\\t%x0, %A1\;ldr\\t%w0, [%x0, %L1]
   adr\\t%x0, %c1
   adrp\\t%x0, %A1
   fmov\\t%s0, %w1
   fmov\\t%w0, %s1
   fmov\\t%s0, %s1
   * return aarch64_output_scalar_simd_mov_immediate (operands[1], SImode);"
  "CONST_INT_P (operands[1]) && !aarch64_move_imm (INTVAL (operands[1]), SImode)
    && REG_P (operands[0]) && GP_REGNUM_P (REGNO (operands[0]))"
   [(const_int 0)]
   "{
       aarch64_expand_mov_immediate (operands[0], operands[1]);
       DONE;
    }"
  ;; The "mov_imm" type for CNT is just a placeholder.
  [(set_attr "type" "mov_reg,mov_reg,mov_reg,mov_imm,mov_imm,mov_imm,load_4,
		    load_4,store_4,store_4,load_4,adr,adr,f_mcr,f_mrc,fmov,neon_move")
   (set_attr "arch"   "*,*,*,*,*,sve,*,fp,*,fp,*,*,*,fp,fp,fp,simd")
   (set_attr "length" "4,4,4,4,*,  4,4, 4,4, 4,8,4,4, 4, 4, 4,   4")
]
)

(define_insn_and_split "*movdi_aarch64"
  [(set (match_operand:DI 0 "nonimmediate_operand" "=r,k,r,r,r,r,r, r,w, m,m,   r,  r,  r, w,r,w, w")
	(match_operand:DI 1 "aarch64_mov_operand"  " r,r,k,N,M,n,Usv,m,m,rZ,w,Usw,Usa,Ush,rZ,w,w,Dd"))]
  "(register_operand (operands[0], DImode)
    || aarch64_reg_or_zero (operands[1], DImode))"
  "@
   mov\\t%x0, %x1
   mov\\t%0, %x1
   mov\\t%x0, %1
   mov\\t%x0, %1
   mov\\t%w0, %1
   #
   * return aarch64_output_sve_cnt_immediate (\"cnt\", \"%x0\", operands[1]);
   ldr\\t%x0, %1
   ldr\\t%d0, %1
   str\\t%x1, %0
   str\\t%d1, %0
   * return TARGET_ILP32 ? \"adrp\\t%0, %A1\;ldr\\t%w0, [%0, %L1]\" : \"adrp\\t%0, %A1\;ldr\\t%0, [%0, %L1]\";
   adr\\t%x0, %c1
   adrp\\t%x0, %A1
   fmov\\t%d0, %x1
   fmov\\t%x0, %d1
   fmov\\t%d0, %d1
   * return aarch64_output_scalar_simd_mov_immediate (operands[1], DImode);"
   "CONST_INT_P (operands[1]) && !aarch64_move_imm (INTVAL (operands[1]), DImode)
    && REG_P (operands[0]) && GP_REGNUM_P (REGNO (operands[0]))"
   [(const_int 0)]
   "{
       aarch64_expand_mov_immediate (operands[0], operands[1]);
       DONE;
    }"
  ;; The "mov_imm" type for CNTD is just a placeholder.
  [(set_attr "type" "mov_reg,mov_reg,mov_reg,mov_imm,mov_imm,mov_imm,mov_imm,
		     load_8,load_8,store_8,store_8,load_8,adr,adr,f_mcr,f_mrc,
		     fmov,neon_move")
   (set_attr "arch"   "*,*,*,*,*,*,sve,*,fp,*,fp,*,*,*,fp,fp,fp,simd")
   (set_attr "length" "4,4,4,4,4,*,  4,4, 4,4, 4,8,4,4, 4, 4, 4,   4")]
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

;; Match MOVK as a normal AND and IOR operation.
(define_insn "aarch64_movk<mode>"
  [(set (match_operand:GPI 0 "register_operand" "=r")
	(ior:GPI (and:GPI (match_operand:GPI 1 "register_operand" "0")
			  (match_operand:GPI 2 "const_int_operand"))
		 (match_operand:GPI 3 "const_int_operand")))]
  "aarch64_movk_shift (rtx_mode_t (operands[2], <MODE>mode),
		       rtx_mode_t (operands[3], <MODE>mode)) >= 0"
  {
    int shift = aarch64_movk_shift (rtx_mode_t (operands[2], <MODE>mode),
				    rtx_mode_t (operands[3], <MODE>mode));
    operands[2] = gen_int_mode (UINTVAL (operands[3]) >> shift, SImode);
    operands[3] = gen_int_mode (shift, SImode);
    return "movk\\t%<w>0, #%X2, lsl %3";
  }
  [(set_attr "type" "mov_imm")]
)

(define_expand "movti"
  [(set (match_operand:TI 0 "nonimmediate_operand")
	(match_operand:TI 1 "general_operand"))]
  ""
  "
    if (GET_CODE (operands[0]) == MEM && operands[1] != const0_rtx)
      operands[1] = force_reg (TImode, operands[1]);

    if (GET_CODE (operands[1]) == CONST_POLY_INT)
      {
	emit_move_insn (gen_lowpart (DImode, operands[0]),
			gen_lowpart (DImode, operands[1]));
	emit_move_insn (gen_highpart (DImode, operands[0]), const0_rtx);
	DONE;
      }
  "
)

(define_insn "*movti_aarch64"
  [(set (match_operand:TI 0
	 "nonimmediate_operand"  "=   r,w,w,w, r,w,r,m,m,w,m")
	(match_operand:TI 1
	 "aarch64_movti_operand" " rUti,Z,Z,r, w,w,m,r,Z,m,w"))]
  "(register_operand (operands[0], TImode)
    || aarch64_reg_or_zero (operands[1], TImode))"
  "@
   #
   movi\\t%0.2d, #0
   fmov\t%d0, xzr
   #
   #
   mov\\t%0.16b, %1.16b
   ldp\\t%0, %H0, %1
   stp\\t%1, %H1, %0
   stp\\txzr, xzr, %0
   ldr\\t%q0, %1
   str\\t%q1, %0"
  [(set_attr "type" "multiple,neon_move,f_mcr,f_mcr,f_mrc,neon_logic_q, \
		             load_16,store_16,store_16,\
                             load_16,store_16")
   (set_attr "length" "8,4,4,8,8,4,4,4,4,4,4")
   (set_attr "arch" "*,simd,*,*,*,simd,*,*,*,fp,fp")]
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
  [(set (match_operand:GPF_TF_F16_MOV 0 "nonimmediate_operand")
	(match_operand:GPF_TF_F16_MOV 1 "general_operand"))]
  ""
  {
    if (!TARGET_FLOAT)
      {
	aarch64_err_no_fpadvsimd (<MODE>mode);
	machine_mode intmode
	  = int_mode_for_size (GET_MODE_BITSIZE (<MODE>mode), 0).require ();
	emit_move_insn (gen_lowpart (intmode, operands[0]),
			gen_lowpart (intmode, operands[1]));
	DONE;
      }

    if (GET_CODE (operands[0]) == MEM
        && ! (GET_CODE (operands[1]) == CONST_DOUBLE
	      && aarch64_float_const_zero_rtx_p (operands[1])))
      operands[1] = force_reg (<MODE>mode, operands[1]);
  }
)

(define_insn "*mov<mode>_aarch64"
  [(set (match_operand:HFBF 0 "nonimmediate_operand" "=w,w  ,w ,w  ,?r,?r,w,w,w  ,w  ,w,m,r,m ,r")
	(match_operand:HFBF 1 "general_operand"      "Y ,?rY,?r,?rY, w, w,w,w,Ufc,Uvi,m,w,m,rY,r"))]
  "TARGET_FLOAT && (register_operand (operands[0], <MODE>mode)
    || aarch64_reg_or_fp_zero (operands[1], <MODE>mode))"
  "@
   movi\\t%0.4h, #0
   fmov\\t%h0, %w1
   dup\\t%w0.4h, %w1
   fmov\\t%s0, %w1
   umov\\t%w0, %1.h[0]
   fmov\\t%w0, %s1
   mov\\t%0.h[0], %1.h[0]
   fmov\\t%s0, %s1
   fmov\\t%h0, %1
   * return aarch64_output_scalar_simd_mov_immediate (operands[1], HImode);
   ldr\\t%h0, %1
   str\\t%h1, %0
   ldrh\\t%w0, %1
   strh\\t%w1, %0
   mov\\t%w0, %w1"
  [(set_attr "type" "neon_move,f_mcr,neon_move,f_mcr,neon_to_gp,f_mrc,
		     neon_move,fmov,fconsts,neon_move,f_loads,f_stores,
		     load_4,store_4,mov_reg")
   (set_attr "arch" "simd,fp16,simd,*,simd,*,simd,*,fp16,simd,*,*,*,*,*")]
)

(define_insn "*mov<mode>_aarch64"
  [(set (match_operand:SFD 0 "nonimmediate_operand" "=w,w  ,?r,w,w  ,w  ,w,m,r,m ,r,r")
	(match_operand:SFD 1 "general_operand"      "Y ,?rY, w,w,Ufc,Uvi,m,w,m,rY,r,M"))]
  "TARGET_FLOAT && (register_operand (operands[0], <MODE>mode)
    || aarch64_reg_or_fp_zero (operands[1], <MODE>mode))"
  "@
   movi\\t%0.2s, #0
   fmov\\t%s0, %w1
   fmov\\t%w0, %s1
   fmov\\t%s0, %s1
   fmov\\t%s0, %1
   * return aarch64_output_scalar_simd_mov_immediate (operands[1], SImode);
   ldr\\t%s0, %1
   str\\t%s1, %0
   ldr\\t%w0, %1
   str\\t%w1, %0
   mov\\t%w0, %w1
   mov\\t%w0, %1"
  [(set_attr "type" "neon_move,f_mcr,f_mrc,fmov,fconsts,neon_move,\
		     f_loads,f_stores,load_4,store_4,mov_reg,\
		     fconsts")
   (set_attr "arch" "simd,*,*,*,*,simd,*,*,*,*,*,*")]
)

(define_insn "*mov<mode>_aarch64"
  [(set (match_operand:DFD 0 "nonimmediate_operand" "=w, w  ,?r,w,w  ,w  ,w,m,r,m ,r,r")
	(match_operand:DFD 1 "general_operand"      "Y , ?rY, w,w,Ufc,Uvi,m,w,m,rY,r,N"))]
  "TARGET_FLOAT && (register_operand (operands[0], <MODE>mode)
    || aarch64_reg_or_fp_zero (operands[1], <MODE>mode))"
  "@
   movi\\t%d0, #0
   fmov\\t%d0, %x1
   fmov\\t%x0, %d1
   fmov\\t%d0, %d1
   fmov\\t%d0, %1
   * return aarch64_output_scalar_simd_mov_immediate (operands[1], DImode);
   ldr\\t%d0, %1
   str\\t%d1, %0
   ldr\\t%x0, %1
   str\\t%x1, %0
   mov\\t%x0, %x1
   mov\\t%x0, %1"
  [(set_attr "type" "neon_move,f_mcr,f_mrc,fmov,fconstd,neon_move,\
		     f_loadd,f_stored,load_8,store_8,mov_reg,\
		     fconstd")
   (set_attr "arch" "simd,*,*,*,*,simd,*,*,*,*,*,*")]
)

(define_split
  [(set (match_operand:GPF_HF 0 "nonimmediate_operand")
	(match_operand:GPF_HF 1 "const_double_operand"))]
  "can_create_pseudo_p ()
   && !aarch64_can_const_movi_rtx_p (operands[1], <MODE>mode)
   && !aarch64_float_const_representable_p (operands[1])
   && !aarch64_float_const_zero_rtx_p (operands[1])
   &&  aarch64_float_const_rtx_p (operands[1])"
  [(const_int 0)]
  {
    unsigned HOST_WIDE_INT ival;
    if (!aarch64_reinterpret_float_as_int (operands[1], &ival))
      FAIL;

    rtx tmp = gen_reg_rtx (<FCVT_TARGET>mode);
    emit_move_insn (tmp, gen_int_mode (ival, <FCVT_TARGET>mode));
    emit_move_insn (operands[0], gen_lowpart (<MODE>mode, tmp));
    DONE;
  }
)

(define_insn "*mov<mode>_aarch64"
  [(set (match_operand:TFD 0
	 "nonimmediate_operand" "=w,?r ,w ,?r,w,?w,w,m,?r,m ,m")
	(match_operand:TFD 1
	 "general_operand"      " w,?rY,?r,w ,Y,Y ,m,w,m ,?r,Y"))]
  "TARGET_FLOAT && (register_operand (operands[0], <MODE>mode)
    || aarch64_reg_or_fp_zero (operands[1], <MODE>mode))"
  "@
   mov\\t%0.16b, %1.16b
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
                     f_loadd,f_stored,load_16,store_16,store_16")
   (set_attr "length" "4,8,8,8,4,4,4,4,4,4,4")
   (set_attr "arch" "simd,*,*,*,simd,*,*,*,*,*,*")]
)

(define_split
   [(set (match_operand:TFD 0 "register_operand" "")
	 (match_operand:TFD 1 "nonmemory_operand" ""))]
  "reload_completed && aarch64_split_128bit_move_p (operands[0], operands[1])"
  [(const_int 0)]
  {
    aarch64_split_128bit_move (operands[0], operands[1]);
    DONE;
  }
)

(define_expand "aarch64_cpymemdi"
  [(parallel
     [(set (match_operand 2) (const_int 0))
      (clobber (match_dup 3))
      (clobber (match_dup 4))
      (set (match_operand 0)
	   (unspec:BLK [(match_operand 1) (match_dup 2)] UNSPEC_CPYMEM))])]
  "TARGET_MOPS"
  {
    operands[3] = XEXP (operands[0], 0);
    operands[4] = XEXP (operands[1], 0);
  }
)

(define_insn "*aarch64_cpymemdi"
  [(set (match_operand:DI 2 "register_operand" "+&r") (const_int 0))
   (clobber (match_operand:DI 0 "register_operand" "+&r"))
   (clobber (match_operand:DI 1 "register_operand" "+&r"))
   (set (mem:BLK (match_dup 0))
        (unspec:BLK [(mem:BLK (match_dup 1)) (match_dup 2)] UNSPEC_CPYMEM))]
  "TARGET_MOPS"
  "cpyfp\t[%x0]!, [%x1]!, %x2!\;cpyfm\t[%x0]!, [%x1]!, %x2!\;cpyfe\t[%x0]!, [%x1]!, %x2!"
  [(set_attr "length" "12")]
)

;; 0 is dst
;; 1 is src
;; 2 is size of copy in bytes
;; 3 is alignment

(define_expand "cpymemdi"
  [(match_operand:BLK 0 "memory_operand")
   (match_operand:BLK 1 "memory_operand")
   (match_operand:DI 2 "general_operand")
   (match_operand:DI 3 "immediate_operand")]
   "!STRICT_ALIGNMENT || TARGET_MOPS"
{
  if (aarch64_expand_cpymem (operands))
    DONE;
  FAIL;
}
)

(define_insn "aarch64_movmemdi"
  [(parallel [
   (set (match_operand:DI 2 "register_operand" "+&r") (const_int 0))
   (clobber (match_operand:DI 0 "register_operand" "+&r"))
   (clobber (match_operand:DI 1 "register_operand" "+&r"))
   (set (mem:BLK (match_dup 0))
        (unspec:BLK [(mem:BLK (match_dup 1)) (match_dup 2)] UNSPEC_MOVMEM))])]
 "TARGET_MOPS"
 "cpyp\t[%x0]!, [%x1]!, %x2!\;cpym\t[%x0]!, [%x1]!, %x2!\;cpye\t[%x0]!, [%x1]!, %x2!"
 [(set_attr "length" "12")]
)

;; 0 is dst
;; 1 is src
;; 2 is size of copy in bytes
;; 3 is alignment

(define_expand "movmemdi"
  [(match_operand:BLK 0 "memory_operand")
   (match_operand:BLK 1 "memory_operand")
   (match_operand:DI 2 "general_operand")
   (match_operand:DI 3 "immediate_operand")]
   "TARGET_MOPS"
{
   rtx sz_reg = operands[2];
   /* For constant-sized memmoves check the threshold.
      FIXME: We should add a non-MOPS memmove expansion for smaller,
      constant-sized memmove to avoid going to a libcall.  */
   if (CONST_INT_P (sz_reg)
       && INTVAL (sz_reg) < aarch64_mops_memmove_size_threshold)
     FAIL;

   rtx addr_dst = XEXP (operands[0], 0);
   rtx addr_src = XEXP (operands[1], 0);

   if (!REG_P (sz_reg))
     sz_reg = force_reg (DImode, sz_reg);
   if (!REG_P (addr_dst))
     addr_dst = force_reg (DImode, addr_dst);
   if (!REG_P (addr_src))
     addr_src = force_reg (DImode, addr_src);
   emit_insn (gen_aarch64_movmemdi (addr_dst, addr_src, sz_reg));
   DONE;
}
)

(define_expand "aarch64_setmemdi"
  [(parallel
     [(set (match_operand 2) (const_int 0))
      (clobber (match_dup 3))
      (set (match_operand 0)
	   (unspec:BLK [(match_operand 1)
			(match_dup 2)] UNSPEC_SETMEM))])]
  "TARGET_MOPS"
  {
    operands[3] = XEXP (operands[0], 0);
  }
)

(define_insn "*aarch64_setmemdi"
  [(set (match_operand:DI 2 "register_operand" "+&r") (const_int 0))
   (clobber (match_operand:DI 0 "register_operand" "+&r"))
   (set (mem:BLK (match_dup 0))
        (unspec:BLK [(match_operand:QI 1 "aarch64_reg_or_zero" "rZ")
		     (match_dup 2)] UNSPEC_SETMEM))]
  "TARGET_MOPS"
  "setp\t[%x0]!, %x2!, %x1\;setm\t[%x0]!, %x2!, %x1\;sete\t[%x0]!, %x2!, %x1"
  [(set_attr "length" "12")]
)

;; 0 is dst
;; 1 is val
;; 2 is size of copy in bytes
;; 3 is alignment
(define_expand "setmemdi"
  [(set (match_operand:BLK 0 "memory_operand")     ;; Dest
        (match_operand:QI  2 "nonmemory_operand")) ;; Value
   (use (match_operand:DI  1 "general_operand")) ;; Length
   (match_operand          3 "immediate_operand")] ;; Align
 "TARGET_SIMD || TARGET_MOPS"
 {
  if (aarch64_expand_setmem (operands))
    DONE;

  FAIL;
})

;; Operands 1 and 3 are tied together by the final condition; so we allow
;; fairly lax checking on the second memory operation.
(define_insn "load_pair_sw_<SX:mode><SX2:mode>"
  [(set (match_operand:SX 0 "register_operand" "=r,w")
	(match_operand:SX 1 "aarch64_mem_pair_operand" "Ump,Ump"))
   (set (match_operand:SX2 2 "register_operand" "=r,w")
	(match_operand:SX2 3 "memory_operand" "m,m"))]
   "rtx_equal_p (XEXP (operands[3], 0),
		 plus_constant (Pmode,
				XEXP (operands[1], 0),
				GET_MODE_SIZE (<SX:MODE>mode)))"
  "@
   ldp\\t%w0, %w2, %z1
   ldp\\t%s0, %s2, %z1"
  [(set_attr "type" "load_8,neon_load1_2reg")
   (set_attr "arch" "*,fp")]
)

;; Storing different modes that can still be merged
(define_insn "load_pair_dw_<DX:mode><DX2:mode>"
  [(set (match_operand:DX 0 "register_operand" "=r,w")
	(match_operand:DX 1 "aarch64_mem_pair_operand" "Ump,Ump"))
   (set (match_operand:DX2 2 "register_operand" "=r,w")
	(match_operand:DX2 3 "memory_operand" "m,m"))]
   "rtx_equal_p (XEXP (operands[3], 0),
		 plus_constant (Pmode,
				XEXP (operands[1], 0),
				GET_MODE_SIZE (<DX:MODE>mode)))"
  "@
   ldp\\t%x0, %x2, %z1
   ldp\\t%d0, %d2, %z1"
  [(set_attr "type" "load_16,neon_load1_2reg")
   (set_attr "arch" "*,fp")]
)

(define_insn "load_pair_dw_tftf"
  [(set (match_operand:TF 0 "register_operand" "=w")
	(match_operand:TF 1 "aarch64_mem_pair_operand" "Ump"))
   (set (match_operand:TF 2 "register_operand" "=w")
	(match_operand:TF 3 "memory_operand" "m"))]
   "TARGET_SIMD
    && rtx_equal_p (XEXP (operands[3], 0),
		    plus_constant (Pmode,
				   XEXP (operands[1], 0),
				   GET_MODE_SIZE (TFmode)))"
  "ldp\\t%q0, %q2, %z1"
  [(set_attr "type" "neon_ldp_q")
   (set_attr "fp" "yes")]
)

;; Operands 0 and 2 are tied together by the final condition; so we allow
;; fairly lax checking on the second memory operation.
(define_insn "store_pair_sw_<SX:mode><SX2:mode>"
  [(set (match_operand:SX 0 "aarch64_mem_pair_operand" "=Ump,Ump")
	(match_operand:SX 1 "aarch64_reg_zero_or_fp_zero" "rYZ,w"))
   (set (match_operand:SX2 2 "memory_operand" "=m,m")
	(match_operand:SX2 3 "aarch64_reg_zero_or_fp_zero" "rYZ,w"))]
   "rtx_equal_p (XEXP (operands[2], 0),
		 plus_constant (Pmode,
				XEXP (operands[0], 0),
				GET_MODE_SIZE (<SX:MODE>mode)))"
  "@
   stp\\t%w1, %w3, %z0
   stp\\t%s1, %s3, %z0"
  [(set_attr "type" "store_8,neon_store1_2reg")
   (set_attr "arch" "*,fp")]
)

;; Storing different modes that can still be merged
(define_insn "store_pair_dw_<DX:mode><DX2:mode>"
  [(set (match_operand:DX 0 "aarch64_mem_pair_operand" "=Ump,Ump")
	(match_operand:DX 1 "aarch64_reg_zero_or_fp_zero" "rYZ,w"))
   (set (match_operand:DX2 2 "memory_operand" "=m,m")
	(match_operand:DX2 3 "aarch64_reg_zero_or_fp_zero" "rYZ,w"))]
   "rtx_equal_p (XEXP (operands[2], 0),
		 plus_constant (Pmode,
				XEXP (operands[0], 0),
				GET_MODE_SIZE (<DX:MODE>mode)))"
  "@
   stp\\t%x1, %x3, %z0
   stp\\t%d1, %d3, %z0"
  [(set_attr "type" "store_16,neon_store1_2reg")
   (set_attr "arch" "*,fp")]
)

(define_insn "store_pair_dw_tftf"
  [(set (match_operand:TF 0 "aarch64_mem_pair_operand" "=Ump")
	(match_operand:TF 1 "register_operand" "w"))
   (set (match_operand:TF 2 "memory_operand" "=m")
	(match_operand:TF 3 "register_operand" "w"))]
   "TARGET_SIMD &&
    rtx_equal_p (XEXP (operands[2], 0),
		 plus_constant (Pmode,
				XEXP (operands[0], 0),
				GET_MODE_SIZE (TFmode)))"
  "stp\\t%q1, %q3, %z0"
  [(set_attr "type" "neon_stp_q")
   (set_attr "fp" "yes")]
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
  "ldp\\t%<GPI:w>2, %<GPI:w>3, [%1], %4"
  [(set_attr "type" "load_<GPI:ldpstp_sz>")]
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
  "ldp\\t%<GPF:w>2, %<GPF:w>3, [%1], %4"
  [(set_attr "type" "neon_load1_2reg")]
)

(define_insn "loadwb_pair<TX:mode>_<P:mode>"
  [(parallel
    [(set (match_operand:P 0 "register_operand" "=k")
          (plus:P (match_operand:P 1 "register_operand" "0")
                  (match_operand:P 4 "aarch64_mem_pair_offset" "n")))
     (set (match_operand:TX 2 "register_operand" "=w")
          (mem:TX (match_dup 1)))
     (set (match_operand:TX 3 "register_operand" "=w")
          (mem:TX (plus:P (match_dup 1)
			  (match_operand:P 5 "const_int_operand" "n"))))])]
  "TARGET_SIMD && INTVAL (operands[5]) == GET_MODE_SIZE (<TX:MODE>mode)"
  "ldp\\t%q2, %q3, [%1], %4"
  [(set_attr "type" "neon_ldp_q")]
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
  "stp\\t%<GPI:w>2, %<GPI:w>3, [%0, %4]!"
  [(set_attr "type" "store_<GPI:ldpstp_sz>")]
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
  "stp\\t%<GPF:w>2, %<GPF:w>3, [%0, %4]!"
  [(set_attr "type" "neon_store1_2reg<q>")]
)

(define_insn "storewb_pair<TX:mode>_<P:mode>"
  [(parallel
    [(set (match_operand:P 0 "register_operand" "=&k")
          (plus:P (match_operand:P 1 "register_operand" "0")
                  (match_operand:P 4 "aarch64_mem_pair_offset" "n")))
     (set (mem:TX (plus:P (match_dup 0)
			  (match_dup 4)))
          (match_operand:TX 2 "register_operand" "w"))
     (set (mem:TX (plus:P (match_dup 0)
			  (match_operand:P 5 "const_int_operand" "n")))
          (match_operand:TX 3 "register_operand" "w"))])]
  "TARGET_SIMD
   && INTVAL (operands[5])
      == INTVAL (operands[4]) + GET_MODE_SIZE (<TX:MODE>mode)"
  "stp\\t%q2, %q3, [%0, %4]!"
  [(set_attr "type" "neon_stp_q")]
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
  [(set_attr "type" "extend,load_4")]
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
  "ldpsw\\t%0, %2, %z1"
  [(set_attr "type" "load_8")]
)

(define_insn "*zero_extendsidi2_aarch64"
  [(set (match_operand:DI 0 "register_operand" "=r,r,w,w,r,w")
        (zero_extend:DI (match_operand:SI 1 "nonimmediate_operand" "r,m,r,m,w,w")))]
  ""
  "@
   uxtw\t%0, %w1
   ldr\t%w0, %1
   fmov\t%s0, %w1
   ldr\t%s0, %1
   fmov\t%w0, %s1
   fmov\t%s0, %s1"
  [(set_attr "type" "mov_reg,load_4,f_mcr,f_loads,f_mrc,fmov")
   (set_attr "arch" "*,*,fp,fp,fp,fp")]
)

(define_insn "*load_pair_zero_extendsidi2_aarch64"
  [(set (match_operand:DI 0 "register_operand" "=r,w")
	(zero_extend:DI (match_operand:SI 1 "aarch64_mem_pair_operand" "Ump,Ump")))
   (set (match_operand:DI 2 "register_operand" "=r,w")
	(zero_extend:DI (match_operand:SI 3 "memory_operand" "m,m")))]
  "rtx_equal_p (XEXP (operands[3], 0),
		plus_constant (Pmode,
			       XEXP (operands[1], 0),
			       GET_MODE_SIZE (SImode)))"
  "@
   ldp\t%w0, %w2, %z1
   ldp\t%s0, %s2, %z1"
  [(set_attr "type" "load_8,neon_load1_2reg")
   (set_attr "arch" "*,fp")]
)

(define_expand "<ANY_EXTEND:optab><SHORT:mode><GPI:mode>2"
  [(set (match_operand:GPI 0 "register_operand")
        (ANY_EXTEND:GPI (match_operand:SHORT 1 "nonimmediate_operand")))]
  ""
)

(define_insn "*extend<SHORT:mode><GPI:mode>2_aarch64"
  [(set (match_operand:GPI 0 "register_operand" "=r,r,r")
        (sign_extend:GPI (match_operand:SHORT 1 "nonimmediate_operand" "r,m,w")))]
  ""
  "@
   sxt<SHORT:size>\t%<GPI:w>0, %w1
   ldrs<SHORT:size>\t%<GPI:w>0, %1
   smov\t%<GPI:w>0, %1.<SHORT:size>[0]"
  [(set_attr "type" "extend,load_4,neon_to_gp")
   (set_attr "arch" "*,*,fp")]
)

(define_insn "*zero_extend<SHORT:mode><GPI:mode>2_aarch64"
  [(set (match_operand:GPI 0 "register_operand" "=r,r,w,r")
        (zero_extend:GPI (match_operand:SHORT 1 "nonimmediate_operand" "r,m,m,w")))]
  ""
  "@
   and\t%<GPI:w>0, %<GPI:w>1, <SHORT:short_mask>
   ldr<SHORT:size>\t%w0, %1
   ldr\t%<SHORT:size>0, %1
   umov\t%w0, %1.<SHORT:size>[0]"
  [(set_attr "type" "logic_imm,load_4,f_loads,neon_to_gp")
   (set_attr "arch" "*,*,fp,fp")]
)

(define_expand "<optab>qihi2"
  [(set (match_operand:HI 0 "register_operand")
        (ANY_EXTEND:HI (match_operand:QI 1 "nonimmediate_operand")))]
  ""
)

(define_insn "*extendqihi2_aarch64"
  [(set (match_operand:HI 0 "register_operand" "=r,r")
	(sign_extend:HI (match_operand:QI 1 "nonimmediate_operand" "r,m")))]
  ""
  "@
   sxtb\t%w0, %w1
   ldrsb\t%w0, %1"
  [(set_attr "type" "extend,load_4")]
)

(define_insn "*zero_extendqihi2_aarch64"
  [(set (match_operand:HI 0 "register_operand" "=r,r")
	(zero_extend:HI (match_operand:QI 1 "nonimmediate_operand" "r,m")))]
  ""
  "@
   and\t%w0, %w1, 255
   ldrb\t%w0, %1"
  [(set_attr "type" "logic_imm,load_4")]
)

;; -------------------------------------------------------------------
;; Simple arithmetic
;; -------------------------------------------------------------------

(define_expand "add<mode>3"
  [(set
    (match_operand:GPI 0 "register_operand")
    (plus:GPI (match_operand:GPI 1 "register_operand")
	      (match_operand:GPI 2 "aarch64_pluslong_or_poly_operand")))]
  ""
{
  /* If operands[1] is a subreg extract the inner RTX.  */
  rtx op1 = REG_P (operands[1]) ? operands[1] : SUBREG_REG (operands[1]);

  /* If the constant is too large for a single instruction and isn't frame
     based, split off the immediate so it is available for CSE.  */
  if (!aarch64_plus_immediate (operands[2], <MODE>mode)
      && !(TARGET_SVE && aarch64_sve_plus_immediate (operands[2], <MODE>mode))
      && can_create_pseudo_p ()
      && (!REG_P (op1)
	 || !REGNO_PTR_FRAME_P (REGNO (op1))))
    operands[2] = force_reg (<MODE>mode, operands[2]);
  /* Some tunings prefer to avoid VL-based operations.
     Split off the poly immediate here.  The rtx costs hook will reject attempts
     to combine them back.  */
  else if (GET_CODE (operands[2]) == CONST_POLY_INT
	   && can_create_pseudo_p ()
	   && (aarch64_tune_params.extra_tuning_flags
	       & AARCH64_EXTRA_TUNE_CSE_SVE_VL_CONSTANTS))
    operands[2] = force_reg (<MODE>mode, operands[2]);
  /* Expand polynomial additions now if the destination is the stack
     pointer, since we don't want to use that as a temporary.  */
  else if (operands[0] == stack_pointer_rtx
	   && aarch64_split_add_offset_immediate (operands[2], <MODE>mode))
    {
      aarch64_split_add_offset (<MODE>mode, operands[0], operands[1],
				operands[2], NULL_RTX, NULL_RTX);
      DONE;
    }
})

(define_insn "*add<mode>3_aarch64"
  [(set
    (match_operand:GPI 0 "register_operand" "=rk,rk,w,rk,r,r,rk")
    (plus:GPI
     (match_operand:GPI 1 "register_operand" "%rk,rk,w,rk,rk,0,rk")
     (match_operand:GPI 2 "aarch64_pluslong_operand" "I,r,w,J,Uaa,Uai,Uav")))]
  ""
  "@
  add\\t%<w>0, %<w>1, %2
  add\\t%<w>0, %<w>1, %<w>2
  add\\t%<rtn>0<vas>, %<rtn>1<vas>, %<rtn>2<vas>
  sub\\t%<w>0, %<w>1, #%n2
  #
  * return aarch64_output_sve_scalar_inc_dec (operands[2]);
  * return aarch64_output_sve_addvl_addpl (operands[2]);"
  ;; The "alu_imm" types for INC/DEC and ADDVL/ADDPL are just placeholders.
  [(set_attr "type" "alu_imm,alu_sreg,neon_add,alu_imm,multiple,alu_imm,alu_imm")
   (set_attr "arch" "*,*,simd,*,*,sve,sve")]
)

;; zero_extend version of above
(define_insn "*addsi3_aarch64_uxtw"
  [(set
    (match_operand:DI 0 "register_operand" "=rk,rk,rk,r")
    (zero_extend:DI
     (plus:SI (match_operand:SI 1 "register_operand" "%rk,rk,rk,rk")
	      (match_operand:SI 2 "aarch64_pluslong_operand" "I,r,J,Uaa"))))]
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

;; Match addition of polynomial offsets that require one temporary, for which
;; we can use the early-clobbered destination register.  This is a separate
;; pattern so that the early clobber doesn't affect register allocation
;; for other forms of addition.  However, we still need to provide an
;; all-register alternative, in case the offset goes out of range after
;; elimination.  For completeness we might as well provide all GPR-based
;; alternatives from the main pattern.
;;
;; We don't have a pattern for additions requiring two temporaries since at
;; present LRA doesn't allow new scratches to be added during elimination.
;; Such offsets should be rare anyway.
;;
;; ??? But if we added LRA support for new scratches, much of the ugliness
;; here would go away.  We could just handle all polynomial constants in
;; this pattern.
(define_insn_and_split "*add<mode>3_poly_1"
  [(set
    (match_operand:GPI 0 "register_operand" "=r,r,r,r,r,r,&r")
    (plus:GPI
     (match_operand:GPI 1 "register_operand" "%rk,rk,rk,rk,0,rk,rk")
     (match_operand:GPI 2 "aarch64_pluslong_or_poly_operand" "I,r,J,Uaa,Uai,Uav,Uat")))]
  "TARGET_SVE && operands[0] != stack_pointer_rtx"
  "@
  add\\t%<w>0, %<w>1, %2
  add\\t%<w>0, %<w>1, %<w>2
  sub\\t%<w>0, %<w>1, #%n2
  #
  * return aarch64_output_sve_scalar_inc_dec (operands[2]);
  * return aarch64_output_sve_addvl_addpl (operands[2]);
  #"
  "&& epilogue_completed
   && !reg_overlap_mentioned_p (operands[0], operands[1])
   && aarch64_split_add_offset_immediate (operands[2], <MODE>mode)"
  [(const_int 0)]
  {
    aarch64_split_add_offset (<MODE>mode, operands[0], operands[1],
			      operands[2], operands[0], NULL_RTX);
    DONE;
  }
  ;; The "alu_imm" types for INC/DEC and ADDVL/ADDPL are just placeholders.
  [(set_attr "type" "alu_imm,alu_sreg,alu_imm,multiple,alu_imm,alu_imm,multiple")]
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

(define_expand "addv<mode>4"
  [(match_operand:GPI 0 "register_operand")
   (match_operand:GPI 1 "register_operand")
   (match_operand:GPI 2 "aarch64_plus_operand")
   (label_ref (match_operand 3 "" ""))]
  ""
{
  if (CONST_INT_P (operands[2]))
    emit_insn (gen_add<mode>3_compareV_imm (operands[0], operands[1],
					    operands[2]));
  else
    emit_insn (gen_add<mode>3_compareV (operands[0], operands[1], operands[2]));
  aarch64_gen_unlikely_cbranch (NE, CC_Vmode, operands[3]);

  DONE;
})

(define_expand "uaddv<mode>4"
  [(match_operand:GPI 0 "register_operand")
   (match_operand:GPI 1 "register_operand")
   (match_operand:GPI 2 "register_operand")
   (label_ref (match_operand 3 "" ""))]
  ""
{
  emit_insn (gen_add<mode>3_compareC (operands[0], operands[1], operands[2]));
  aarch64_gen_unlikely_cbranch (LTU, CC_Cmode, operands[3]);

  DONE;
})

(define_expand "addti3"
  [(set (match_operand:TI 0 "register_operand")
	(plus:TI (match_operand:TI 1 "register_operand")
		 (match_operand:TI 2 "aarch64_reg_or_imm")))]
  ""
{
  rtx low_dest, op1_low, op2_low, high_dest, op1_high, op2_high;

  aarch64_addti_scratch_regs (operands[1], operands[2],
			      &low_dest, &op1_low, &op2_low,
			      &high_dest, &op1_high, &op2_high);

  if (op2_low == const0_rtx)
    {
      low_dest = op1_low;
      if (!aarch64_pluslong_operand (op2_high, DImode))
	op2_high = force_reg (DImode, op2_high);
      emit_insn (gen_adddi3 (high_dest, op1_high, op2_high));
    }
  else
    {
      emit_insn (gen_adddi3_compareC (low_dest, op1_low,
				      force_reg (DImode, op2_low)));
      emit_insn (gen_adddi3_carryin (high_dest, op1_high,
				     force_reg (DImode, op2_high)));
    }

  emit_move_insn (gen_lowpart (DImode, operands[0]), low_dest);
  emit_move_insn (gen_highpart (DImode, operands[0]), high_dest);

  DONE;
})

(define_expand "addvti4"
  [(match_operand:TI 0 "register_operand")
   (match_operand:TI 1 "register_operand")
   (match_operand:TI 2 "aarch64_reg_or_imm")
   (label_ref (match_operand 3 "" ""))]
  ""
{
  rtx low_dest, op1_low, op2_low, high_dest, op1_high, op2_high;

  aarch64_addti_scratch_regs (operands[1], operands[2],
			      &low_dest, &op1_low, &op2_low,
			      &high_dest, &op1_high, &op2_high);

  if (op2_low == const0_rtx)
    {
      low_dest = op1_low;
      emit_insn (gen_adddi3_compareV (high_dest, op1_high,
				      force_reg (DImode, op2_high)));
    }
  else
    {
      emit_insn (gen_adddi3_compareC (low_dest, op1_low,
				      force_reg (DImode, op2_low)));
      emit_insn (gen_adddi3_carryinV (high_dest, op1_high,
				      force_reg (DImode, op2_high)));
    }

  emit_move_insn (gen_lowpart (DImode, operands[0]), low_dest);
  emit_move_insn (gen_highpart (DImode, operands[0]), high_dest);

  aarch64_gen_unlikely_cbranch (NE, CC_Vmode, operands[3]);
  DONE;
})

(define_expand "uaddvti4"
  [(match_operand:TI 0 "register_operand")
   (match_operand:TI 1 "register_operand")
   (match_operand:TI 2 "aarch64_reg_or_imm")
   (label_ref (match_operand 3 "" ""))]
  ""
{
  rtx low_dest, op1_low, op2_low, high_dest, op1_high, op2_high;

  aarch64_addti_scratch_regs (operands[1], operands[2],
			      &low_dest, &op1_low, &op2_low,
			      &high_dest, &op1_high, &op2_high);

  if (op2_low == const0_rtx)
    {
      low_dest = op1_low;
      emit_insn (gen_adddi3_compareC (high_dest, op1_high,
				      force_reg (DImode, op2_high)));
    }
  else
    {
      emit_insn (gen_adddi3_compareC (low_dest, op1_low,
				      force_reg (DImode, op2_low)));
      emit_insn (gen_adddi3_carryinC (high_dest, op1_high,
				      force_reg (DImode, op2_high)));
    }

  emit_move_insn (gen_lowpart (DImode, operands[0]), low_dest);
  emit_move_insn (gen_highpart (DImode, operands[0]), high_dest);

  aarch64_gen_unlikely_cbranch (GEU, CC_ADCmode, operands[3]);
  DONE;
 })

(define_insn "add<mode>3_compare0"
  [(set (reg:CC_NZ CC_REGNUM)
	(compare:CC_NZ
	 (plus:GPI (match_operand:GPI 1 "register_operand" "%rk,rk,rk")
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
	 (plus:SI (match_operand:SI 1 "register_operand" "%rk,rk,rk")
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

(define_insn "*add<mode>3_compareC_cconly"
  [(set (reg:CC_C CC_REGNUM)
	(compare:CC_C
	  (plus:GPI
	    (match_operand:GPI 0 "register_operand" "r,r,r")
	    (match_operand:GPI 1 "aarch64_plus_operand" "r,I,J"))
	  (match_dup 0)))]
  ""
  "@
  cmn\\t%<w>0, %<w>1
  cmn\\t%<w>0, %1
  cmp\\t%<w>0, #%n1"
  [(set_attr "type" "alus_sreg,alus_imm,alus_imm")]
)

(define_insn "add<mode>3_compareC"
  [(set (reg:CC_C CC_REGNUM)
	(compare:CC_C
	  (plus:GPI
	    (match_operand:GPI 1 "register_operand" "rk,rk,rk")
	    (match_operand:GPI 2 "aarch64_plus_operand" "r,I,J"))
	  (match_dup 1)))
   (set (match_operand:GPI 0 "register_operand" "=r,r,r")
	(plus:GPI (match_dup 1) (match_dup 2)))]
  ""
  "@
  adds\\t%<w>0, %<w>1, %<w>2
  adds\\t%<w>0, %<w>1, %2
  subs\\t%<w>0, %<w>1, #%n2"
  [(set_attr "type" "alus_sreg,alus_imm,alus_imm")]
)

(define_insn "*add<mode>3_compareV_cconly_imm"
  [(set (reg:CC_V CC_REGNUM)
	(compare:CC_V
	  (plus:<DWI>
	    (sign_extend:<DWI> (match_operand:GPI 0 "register_operand" "r,r"))
	    (match_operand:<DWI> 1 "const_scalar_int_operand" ""))
	  (sign_extend:<DWI>
	   (plus:GPI
	    (match_dup 0)
	    (match_operand:GPI 2 "aarch64_plus_immediate" "I,J")))))]
  "INTVAL (operands[1]) == INTVAL (operands[2])"
  "@
  cmn\\t%<w>0, %<w>1
  cmp\\t%<w>0, #%n1"
  [(set_attr "type" "alus_imm")]
)

(define_insn "*add<mode>3_compareV_cconly"
  [(set (reg:CC_V CC_REGNUM)
	(compare:CC_V
	  (plus:<DWI>
	    (sign_extend:<DWI> (match_operand:GPI 0 "register_operand" "r"))
	    (sign_extend:<DWI> (match_operand:GPI 1 "register_operand" "r")))
	  (sign_extend:<DWI> (plus:GPI (match_dup 0) (match_dup 1)))))]
  ""
  "cmn\\t%<w>0, %<w>1"
  [(set_attr "type" "alus_sreg")]
)

(define_insn "add<mode>3_compareV_imm"
  [(set (reg:CC_V CC_REGNUM)
	(compare:CC_V
	  (plus:<DWI>
	    (sign_extend:<DWI>
	      (match_operand:GPI 1 "register_operand" "rk,rk"))
	    (match_operand:GPI 2 "aarch64_plus_immediate" "I,J"))
	  (sign_extend:<DWI>
	    (plus:GPI (match_dup 1) (match_dup 2)))))
   (set (match_operand:GPI 0 "register_operand" "=r,r")
	(plus:GPI (match_dup 1) (match_dup 2)))]
   ""
   "@
   adds\\t%<w>0, %<w>1, %<w>2
   subs\\t%<w>0, %<w>1, #%n2"
  [(set_attr "type" "alus_imm,alus_imm")]
)

(define_insn "add<mode>3_compareV"
  [(set (reg:CC_V CC_REGNUM)
	(compare:CC_V
	  (plus:<DWI>
	    (sign_extend:<DWI> (match_operand:GPI 1 "register_operand" "rk"))
	    (sign_extend:<DWI> (match_operand:GPI 2 "register_operand" "r")))
	  (sign_extend:<DWI> (plus:GPI (match_dup 1) (match_dup 2)))))
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

(define_insn "*adds_<optab><ALLX:mode>_<GPI:mode>"
  [(set (reg:CC_NZ CC_REGNUM)
	(compare:CC_NZ
	 (plus:GPI
	  (ANY_EXTEND:GPI (match_operand:ALLX 1 "register_operand" "r"))
	  (match_operand:GPI 2 "register_operand" "rk"))
	(const_int 0)))
   (set (match_operand:GPI 0 "register_operand" "=r")
	(plus:GPI (ANY_EXTEND:GPI (match_dup 1)) (match_dup 2)))]
  ""
  "adds\\t%<GPI:w>0, %<GPI:w>2, %w1, <su>xt<ALLX:size>"
  [(set_attr "type" "alus_ext")]
)

(define_insn "*subs_<optab><ALLX:mode>_<GPI:mode>"
  [(set (reg:CC_NZ CC_REGNUM)
	(compare:CC_NZ
	 (minus:GPI (match_operand:GPI 1 "register_operand" "rk")
		    (ANY_EXTEND:GPI
		     (match_operand:ALLX 2 "register_operand" "r")))
	(const_int 0)))
   (set (match_operand:GPI 0 "register_operand" "=r")
	(minus:GPI (match_dup 1) (ANY_EXTEND:GPI (match_dup 2))))]
  ""
  "subs\\t%<GPI:w>0, %<GPI:w>1, %w2, <su>xt<ALLX:size>"
  [(set_attr "type" "alus_ext")]
)

(define_insn "*adds_<optab><ALLX:mode>_shift_<GPI:mode>"
  [(set (reg:CC_NZ CC_REGNUM)
	(compare:CC_NZ
	 (plus:GPI (ashift:GPI 
		    (ANY_EXTEND:GPI 
		     (match_operand:ALLX 1 "register_operand" "r"))
		    (match_operand 2 "aarch64_imm3" "Ui3"))
		   (match_operand:GPI 3 "register_operand" "rk"))
	 (const_int 0)))
   (set (match_operand:GPI 0 "register_operand" "=rk")
	(plus:GPI (ashift:GPI (ANY_EXTEND:GPI (match_dup 1))
			      (match_dup 2))
		  (match_dup 3)))]
  ""
  "adds\\t%<GPI:w>0, %<GPI:w>3, %w1, <su>xt<ALLX:size> %2"
  [(set_attr "type" "alus_ext")]
)

(define_insn "*subs_<optab><ALLX:mode>_shift_<GPI:mode>"
  [(set (reg:CC_NZ CC_REGNUM)
	(compare:CC_NZ
	 (minus:GPI (match_operand:GPI 1 "register_operand" "rk")
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
  "subs\\t%<GPI:w>0, %<GPI:w>1, %w2, <su>xt<ALLX:size> %3"
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

(define_insn "aarch64_sub<mode>_compare0"
  [(set (reg:CC_NZ CC_REGNUM)
	(compare:CC_NZ
	 (minus:GPI (match_operand:GPI 0 "register_operand" "r")
		   (match_operand:GPI 1 "aarch64_plus_operand" "r"))
	 (const_int 0)))]
  ""
  "cmp\\t%<w>0, %<w>1"
  [(set_attr "type" "alus_sreg")]
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
  [(set_attr "autodetect_type" "alu_shift_<shift>_op2")]
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
  [(set_attr "autodetect_type" "alu_shift_<shift>_op2")]
)

(define_insn "*add_<optab><ALLX:mode>_<GPI:mode>"
  [(set (match_operand:GPI 0 "register_operand" "=rk")
	(plus:GPI (ANY_EXTEND:GPI (match_operand:ALLX 1 "register_operand" "r"))
		  (match_operand:GPI 2 "register_operand" "r")))]
  ""
  "add\\t%<GPI:w>0, %<GPI:w>2, %w1, <su>xt<ALLX:size>"
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
  "add\\t%<GPI:w>0, %<GPI:w>3, %w1, <su>xt<ALLX:size> %2"
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

(define_expand "add<mode>3_carryin"
  [(set (match_operand:GPI 0 "register_operand")
	(plus:GPI
	  (plus:GPI
	    (ltu:GPI (reg:CC_C CC_REGNUM) (const_int 0))
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

(define_expand "add<mode>3_carryinC"
  [(parallel
     [(set (match_dup 3)
	   (compare:CC_ADC
	     (plus:<DWI>
	       (plus:<DWI>
		 (match_dup 4)
		 (zero_extend:<DWI>
		   (match_operand:GPI 1 "register_operand")))
	       (zero_extend:<DWI>
		 (match_operand:GPI 2 "register_operand")))
	     (match_dup 6)))
      (set (match_operand:GPI 0 "register_operand")
	   (plus:GPI
	     (plus:GPI (match_dup 5) (match_dup 1))
	     (match_dup 2)))])]
   ""
{
  operands[3] = gen_rtx_REG (CC_ADCmode, CC_REGNUM);
  rtx ccin = gen_rtx_REG (CC_Cmode, CC_REGNUM);
  operands[4] = gen_rtx_LTU (<DWI>mode, ccin, const0_rtx);
  operands[5] = gen_rtx_LTU (<MODE>mode, ccin, const0_rtx);
  operands[6] = immed_wide_int_const (wi::shwi (1, <DWI>mode)
				      << GET_MODE_BITSIZE (<MODE>mode),
				      TImode);
})

(define_insn "*add<mode>3_carryinC_zero"
  [(set (reg:CC_ADC CC_REGNUM)
	(compare:CC_ADC
	  (plus:<DWI>
	    (match_operand:<DWI> 2 "aarch64_carry_operation" "")
	    (zero_extend:<DWI> (match_operand:GPI 1 "register_operand" "r")))
	  (match_operand 4 "const_scalar_int_operand" "")))
   (set (match_operand:GPI 0 "register_operand" "=r")
	(plus:GPI (match_operand:GPI 3 "aarch64_carry_operation" "")
		  (match_dup 1)))]
  "rtx_mode_t (operands[4], <DWI>mode)
   == (wi::shwi (1, <DWI>mode) << (unsigned) GET_MODE_BITSIZE (<MODE>mode))"
   "adcs\\t%<w>0, %<w>1, <w>zr"
  [(set_attr "type" "adc_reg")]
)

(define_insn "*add<mode>3_carryinC"
  [(set (reg:CC_ADC CC_REGNUM)
	(compare:CC_ADC
	  (plus:<DWI>
	    (plus:<DWI>
	      (match_operand:<DWI> 3 "aarch64_carry_operation" "")
	      (zero_extend:<DWI> (match_operand:GPI 1 "register_operand" "r")))
	    (zero_extend:<DWI> (match_operand:GPI 2 "register_operand" "r")))
	  (match_operand 5 "const_scalar_int_operand" "")))
   (set (match_operand:GPI 0 "register_operand" "=r")
	(plus:GPI
	  (plus:GPI (match_operand:GPI 4 "aarch64_carry_operation" "")
		    (match_dup 1))
	  (match_dup 2)))]
  "rtx_mode_t (operands[5], <DWI>mode)
   == (wi::shwi (1, <DWI>mode) << (unsigned) GET_MODE_BITSIZE (<MODE>mode))"
   "adcs\\t%<w>0, %<w>1, %<w>2"
  [(set_attr "type" "adc_reg")]
)

(define_expand "add<mode>3_carryinV"
  [(parallel
     [(set (reg:CC_V CC_REGNUM)
	   (compare:CC_V
	     (plus:<DWI>
	       (plus:<DWI>
		 (match_dup 3)
		 (sign_extend:<DWI>
		   (match_operand:GPI 1 "register_operand")))
	       (sign_extend:<DWI>
		 (match_operand:GPI 2 "register_operand")))
	   (sign_extend:<DWI>
	     (plus:GPI
	       (plus:GPI (match_dup 4) (match_dup 1))
	       (match_dup 2)))))
      (set (match_operand:GPI 0 "register_operand")
	   (plus:GPI
	     (plus:GPI (match_dup 4) (match_dup 1))
	     (match_dup 2)))])]
   ""
{
  rtx cc = gen_rtx_REG (CC_Cmode, CC_REGNUM);
  operands[3] = gen_rtx_LTU (<DWI>mode, cc, const0_rtx);
  operands[4] = gen_rtx_LTU (<MODE>mode, cc, const0_rtx);
})

(define_insn "*add<mode>3_carryinV_zero"
  [(set (reg:CC_V CC_REGNUM)
	(compare:CC_V
	  (plus:<DWI>
	    (match_operand:<DWI> 2 "aarch64_carry_operation" "")
	    (sign_extend:<DWI> (match_operand:GPI 1 "register_operand" "r")))
	  (sign_extend:<DWI>
	    (plus:GPI
	      (match_operand:GPI 3 "aarch64_carry_operation" "")
	      (match_dup 1)))))
   (set (match_operand:GPI 0 "register_operand" "=r")
	(plus:GPI (match_dup 3) (match_dup 1)))]
   ""
   "adcs\\t%<w>0, %<w>1, <w>zr"
  [(set_attr "type" "adc_reg")]
)

(define_insn "*add<mode>3_carryinV"
  [(set (reg:CC_V CC_REGNUM)
	(compare:CC_V
	  (plus:<DWI>
	    (plus:<DWI>
	      (match_operand:<DWI> 3 "aarch64_carry_operation" "")
	      (sign_extend:<DWI> (match_operand:GPI 1 "register_operand" "r")))
	    (sign_extend:<DWI> (match_operand:GPI 2 "register_operand" "r")))
	  (sign_extend:<DWI>
	    (plus:GPI
	      (plus:GPI
		(match_operand:GPI 4 "aarch64_carry_operation" "")
		(match_dup 1))
	      (match_dup 2)))))
   (set (match_operand:GPI 0 "register_operand" "=r")
	(plus:GPI
	  (plus:GPI (match_dup 4) (match_dup 1))
	  (match_dup 2)))]
   ""
   "adcs\\t%<w>0, %<w>1, %<w>2"
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
  return \"add\t%<w>0, %<w>4, %w1, uxt%e3 %2\";"
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
   (set_attr "arch" "*,simd")]
)

(define_expand "subv<GPI:mode>4"
  [(match_operand:GPI 0 "register_operand")
   (match_operand:GPI 1 "register_operand")
   (match_operand:GPI 2 "aarch64_plus_operand")
   (label_ref (match_operand 3 "" ""))]
  ""
{
  if (CONST_INT_P (operands[2]))
    emit_insn (gen_subv<mode>_imm (operands[0], operands[1], operands[2]));
  else
    emit_insn (gen_subv<mode>_insn (operands[0], operands[1], operands[2]));
  aarch64_gen_unlikely_cbranch (NE, CC_Vmode, operands[3]);

  DONE;
})

(define_insn "subv<GPI:mode>_insn"
  [(set (reg:CC_V CC_REGNUM)
	(compare:CC_V
	 (sign_extend:<DWI>
	  (minus:GPI
	   (match_operand:GPI 1 "register_operand" "rk")
	   (match_operand:GPI 2 "register_operand" "r")))
	 (minus:<DWI> (sign_extend:<DWI> (match_dup 1))
		      (sign_extend:<DWI> (match_dup 2)))))
   (set (match_operand:GPI 0 "register_operand" "=r")
	(minus:GPI (match_dup 1) (match_dup 2)))]
  ""
  "subs\\t%<w>0, %<w>1, %<w>2"
  [(set_attr "type" "alus_sreg")]
)

(define_insn "subv<GPI:mode>_imm"
  [(set (reg:CC_V CC_REGNUM)
	(compare:CC_V
	 (sign_extend:<DWI>
	  (minus:GPI
	   (match_operand:GPI 1 "register_operand" "rk,rk")
	   (match_operand:GPI 2 "aarch64_plus_immediate" "I,J")))
	 (minus:<DWI> (sign_extend:<DWI> (match_dup 1))
		      (match_dup 2))))
   (set (match_operand:GPI 0 "register_operand" "=r,r")
	(minus:GPI (match_dup 1) (match_dup 2)))]
  ""
  "@
   subs\\t%<w>0, %<w>1, %2
   adds\\t%<w>0, %<w>1, #%n2"
  [(set_attr "type" "alus_sreg")]
)

(define_expand "negv<GPI:mode>3"
  [(match_operand:GPI 0 "register_operand")
   (match_operand:GPI 1 "register_operand")
   (label_ref (match_operand 2 "" ""))]
  ""
  {
    emit_insn (gen_negv<mode>_insn (operands[0], operands[1]));
    aarch64_gen_unlikely_cbranch (NE, CC_Vmode, operands[2]);

    DONE;
  }
)

(define_insn "negv<GPI:mode>_insn"
  [(set (reg:CC_V CC_REGNUM)
	(compare:CC_V
	 (sign_extend:<DWI>
	  (neg:GPI (match_operand:GPI 1 "register_operand" "r")))
	 (neg:<DWI> (sign_extend:<DWI> (match_dup 1)))))
   (set (match_operand:GPI 0 "register_operand" "=r")
	(neg:GPI (match_dup 1)))]
  ""
  "negs\\t%<w>0, %<w>1"
  [(set_attr "type" "alus_sreg")]
)

(define_insn "negv<GPI:mode>_cmp_only"
  [(set (reg:CC_V CC_REGNUM)
	(compare:CC_V
	 (sign_extend:<DWI>
	  (neg:GPI (match_operand:GPI 0 "register_operand" "r")))
	 (neg:<DWI> (sign_extend:<DWI> (match_dup 0)))))]
  ""
  "negs\\t%<w>zr, %<w>0"
  [(set_attr "type" "alus_sreg")]
)

(define_insn "*cmpv<GPI:mode>_insn"
  [(set (reg:CC_V CC_REGNUM)
	(compare:CC_V
	 (sign_extend:<DWI>
	  (minus:GPI (match_operand:GPI 0 "register_operand" "r,r,r")
		     (match_operand:GPI 1 "aarch64_plus_operand" "r,I,J")))
	 (minus:<DWI> (sign_extend:<DWI> (match_dup 0))
		    (sign_extend:<DWI> (match_dup 1)))))]
  ""
  "@
   cmp\\t%<w>0, %<w>1
   cmp\\t%<w>0, %1
   cmp\\t%<w>0, #%n1"
  [(set_attr "type" "alus_sreg")]
)

(define_expand "usubv<mode>4"
  [(match_operand:GPI 0 "register_operand")
   (match_operand:GPI 1 "aarch64_reg_or_zero")
   (match_operand:GPI 2 "aarch64_reg_or_zero")
   (label_ref (match_operand 3 "" ""))]
  ""
{
  emit_insn (gen_sub<mode>3_compare1 (operands[0], operands[1], operands[2]));
  aarch64_gen_unlikely_cbranch (LTU, CCmode, operands[3]);

  DONE;
})

(define_expand "subti3"
  [(set (match_operand:TI 0 "register_operand")
	(minus:TI (match_operand:TI 1 "aarch64_reg_or_zero")
		  (match_operand:TI 2 "register_operand")))]
  ""
{
  rtx low_dest, op1_low, op2_low, high_dest, op1_high, op2_high;

  aarch64_subvti_scratch_regs (operands[1], operands[2],
			       &low_dest, &op1_low, &op2_low,
			       &high_dest, &op1_high, &op2_high);

  emit_insn (gen_subdi3_compare1 (low_dest, op1_low, op2_low));
  emit_insn (gen_subdi3_carryin (high_dest, op1_high, op2_high));

  emit_move_insn (gen_lowpart (DImode, operands[0]), low_dest);
  emit_move_insn (gen_highpart (DImode, operands[0]), high_dest);
  DONE;
})

(define_expand "subvti4"
  [(match_operand:TI 0 "register_operand")
   (match_operand:TI 1 "register_operand")
   (match_operand:TI 2 "aarch64_reg_or_imm")
   (label_ref (match_operand 3 "" ""))]
  ""
{
  rtx low_dest, op1_low, op2_low, high_dest, op1_high, op2_high;

  aarch64_subvti_scratch_regs (operands[1], operands[2],
			       &low_dest, &op1_low, &op2_low,
			       &high_dest, &op1_high, &op2_high);
  aarch64_expand_subvti (operands[0], low_dest, op1_low, op2_low,
			 high_dest, op1_high, op2_high, false);

  aarch64_gen_unlikely_cbranch (NE, CC_Vmode, operands[3]);
  DONE;
})

(define_expand "usubvti4"
  [(match_operand:TI 0 "register_operand")
   (match_operand:TI 1 "register_operand")
   (match_operand:TI 2 "aarch64_reg_or_imm")
   (label_ref (match_operand 3 "" ""))]
  ""
{
  rtx low_dest, op1_low, op2_low, high_dest, op1_high, op2_high;

  aarch64_subvti_scratch_regs (operands[1], operands[2],
				    &low_dest, &op1_low, &op2_low,
			       &high_dest, &op1_high, &op2_high);
  aarch64_expand_subvti (operands[0], low_dest, op1_low, op2_low,
			 high_dest, op1_high, op2_high, true);

  aarch64_gen_unlikely_cbranch (LTU, CCmode, operands[3]);
  DONE;
})

(define_expand "negvti3"
  [(match_operand:TI 0 "register_operand")
   (match_operand:TI 1 "register_operand")
   (label_ref (match_operand 2 "" ""))]
  ""
  {
    emit_insn (gen_negdi_carryout (gen_lowpart (DImode, operands[0]),
				   gen_lowpart (DImode, operands[1])));
    emit_insn (gen_negvdi_carryinV (gen_highpart (DImode, operands[0]),
				    gen_highpart (DImode, operands[1])));
    aarch64_gen_unlikely_cbranch (NE, CC_Vmode, operands[2]);

    DONE;
  }
)

(define_insn "negdi_carryout"
  [(set (reg:CC CC_REGNUM)
	(compare:CC
	 (const_int 0) (match_operand:DI 1 "register_operand" "r")))
   (set (match_operand:DI 0 "register_operand" "=r")
	(neg:DI (match_dup 1)))]
  ""
  "negs\\t%0, %1"
  [(set_attr "type" "alus_sreg")]
)

(define_insn "negvdi_carryinV"
  [(set (reg:CC_V CC_REGNUM)
	(compare:CC_V
	 (neg:TI (plus:TI
		  (ltu:TI (reg:CC CC_REGNUM) (const_int 0))
		  (sign_extend:TI (match_operand:DI 1 "register_operand" "r"))))
	 (sign_extend:TI
	  (neg:DI (plus:DI (ltu:DI (reg:CC CC_REGNUM) (const_int 0))
			   (match_dup 1))))))
   (set (match_operand:DI 0 "register_operand" "=r")
	(neg:DI (plus:DI (ltu:DI (reg:CC CC_REGNUM) (const_int 0))
			 (match_dup 1))))]
  ""
  "ngcs\\t%0, %1"
  [(set_attr "type" "alus_sreg")]
)

(define_insn "*sub<mode>3_compare0"
  [(set (reg:CC_NZ CC_REGNUM)
	(compare:CC_NZ (minus:GPI (match_operand:GPI 1 "register_operand" "rk")
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
	(compare:CC_NZ (minus:SI (match_operand:SI 1 "register_operand" "rk")
				 (match_operand:SI 2 "register_operand" "r"))
		       (const_int 0)))
   (set (match_operand:DI 0 "register_operand" "=r")
	(zero_extend:DI (minus:SI (match_dup 1) (match_dup 2))))]
  ""
  "subs\\t%w0, %w1, %w2"
  [(set_attr "type" "alus_sreg")]
)

(define_insn "sub<mode>3_compare1_imm"
  [(set (reg:CC CC_REGNUM)
	(compare:CC
	  (match_operand:GPI 1 "register_operand" "rk,rk")
	  (match_operand:GPI 2 "aarch64_plus_immediate" "I,J")))
   (set (match_operand:GPI 0 "register_operand" "=r,r")
	(plus:GPI
	  (match_dup 1)
	  (match_operand:GPI 3 "aarch64_plus_immediate" "J,I")))]
  "UINTVAL (operands[2]) == -UINTVAL (operands[3])"
  "@
  subs\\t%<w>0, %<w>1, %2
  adds\\t%<w>0, %<w>1, #%n2"
  [(set_attr "type" "alus_imm")]
)

(define_insn "sub<mode>3_compare1"
  [(set (reg:CC CC_REGNUM)
	(compare:CC
	  (match_operand:GPI 1 "aarch64_reg_or_zero" "rkZ")
	  (match_operand:GPI 2 "aarch64_reg_or_zero" "rZ")))
   (set (match_operand:GPI 0 "register_operand" "=r")
	(minus:GPI (match_dup 1) (match_dup 2)))]
  ""
  "subs\\t%<w>0, %<w>1, %<w>2"
  [(set_attr "type" "alus_sreg")]
)

(define_peephole2
  [(set (match_operand:GPI 0 "aarch64_general_reg")
	(minus:GPI (match_operand:GPI 1 "aarch64_reg_or_zero")
		    (match_operand:GPI 2 "aarch64_reg_or_zero")))
   (set (reg:CC CC_REGNUM)
	(compare:CC
	  (match_dup 1)
	  (match_dup 2)))]
  "!reg_overlap_mentioned_p (operands[0], operands[1])
   && !reg_overlap_mentioned_p (operands[0], operands[2])"
  [(const_int 0)]
  {
    emit_insn (gen_sub<mode>3_compare1 (operands[0], operands[1],
					 operands[2]));
    DONE;
  }
)

;; Same as the above peephole but with the compare and minus in
;; swapped order.  The restriction on overlap between operand 0
;; and operands 1 and 2 doesn't apply here.
(define_peephole2
  [(set (reg:CC CC_REGNUM)
	(compare:CC
	  (match_operand:GPI 1 "aarch64_reg_or_zero")
	  (match_operand:GPI 2 "aarch64_reg_or_zero")))
   (set (match_operand:GPI 0 "aarch64_general_reg")
	(minus:GPI (match_dup 1)
		   (match_dup 2)))]
  ""
  [(const_int 0)]
  {
    emit_insn (gen_sub<mode>3_compare1 (operands[0], operands[1],
					 operands[2]));
    DONE;
  }
)

(define_peephole2
  [(set (match_operand:GPI 0 "aarch64_general_reg")
	(plus:GPI (match_operand:GPI 1 "register_operand")
		  (match_operand:GPI 2 "aarch64_plus_immediate")))
   (set (reg:CC CC_REGNUM)
	(compare:CC
	  (match_dup 1)
	  (match_operand:GPI 3 "const_int_operand")))]
  "!reg_overlap_mentioned_p (operands[0], operands[1])
   && INTVAL (operands[3]) == -INTVAL (operands[2])"
  [(const_int 0)]
  {
    emit_insn (gen_sub<mode>3_compare1_imm (operands[0], operands[1],
					 operands[3], operands[2]));
    DONE;
  }
)

;; Same as the above peephole but with the compare and minus in
;; swapped order.  The restriction on overlap between operand 0
;; and operands 1 doesn't apply here.
(define_peephole2
  [(set (reg:CC CC_REGNUM)
	(compare:CC
	  (match_operand:GPI 1 "register_operand")
	  (match_operand:GPI 3 "const_int_operand")))
   (set (match_operand:GPI 0 "aarch64_general_reg")
	(plus:GPI (match_dup 1)
		  (match_operand:GPI 2 "aarch64_plus_immediate")))]
  "INTVAL (operands[3]) == -INTVAL (operands[2])"
  [(const_int 0)]
  {
    emit_insn (gen_sub<mode>3_compare1_imm (operands[0], operands[1],
					 operands[3], operands[2]));
    DONE;
  }
)

(define_insn "*sub_<shift>_<mode>"
  [(set (match_operand:GPI 0 "register_operand" "=r")
	(minus:GPI (match_operand:GPI 3 "register_operand" "r")
		   (ASHIFT:GPI
		    (match_operand:GPI 1 "register_operand" "r")
		    (match_operand:QI 2 "aarch64_shift_imm_<mode>" "n"))))]
  ""
  "sub\\t%<w>0, %<w>3, %<w>1, <shift> %2"
  [(set_attr "autodetect_type" "alu_shift_<shift>_op2")]
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
  [(set_attr "autodetect_type" "alu_shift_<shift>_op2")]
)

(define_insn "*sub_<optab><ALLX:mode>_<GPI:mode>"
  [(set (match_operand:GPI 0 "register_operand" "=rk")
	(minus:GPI (match_operand:GPI 1 "register_operand" "rk")
		   (ANY_EXTEND:GPI
		    (match_operand:ALLX 2 "register_operand" "r"))))]
  ""
  "sub\\t%<GPI:w>0, %<GPI:w>1, %w2, <su>xt<ALLX:size>"
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
  "sub\\t%<GPI:w>0, %<GPI:w>1, %w2, <su>xt<ALLX:size> %3"
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

(define_expand "usub<GPI:mode>3_carryinC"
  [(parallel
     [(set (reg:CC CC_REGNUM)
	   (compare:CC
	     (zero_extend:<DWI>
	       (match_operand:GPI 1 "aarch64_reg_or_zero"))
	     (plus:<DWI>
	       (zero_extend:<DWI>
		 (match_operand:GPI 2 "register_operand"))
	       (ltu:<DWI> (reg:CC CC_REGNUM) (const_int 0)))))
      (set (match_operand:GPI 0 "register_operand")
	   (minus:GPI
	     (minus:GPI (match_dup 1) (match_dup 2))
	     (ltu:GPI (reg:CC CC_REGNUM) (const_int 0))))])]
   ""
)

(define_insn "*usub<GPI:mode>3_carryinC_z1"
  [(set (reg:CC CC_REGNUM)
	(compare:CC
	  (const_int 0)
	  (plus:<DWI>
	    (zero_extend:<DWI>
	      (match_operand:GPI 1 "register_operand" "r"))
	    (match_operand:<DWI> 2 "aarch64_borrow_operation" ""))))
   (set (match_operand:GPI 0 "register_operand" "=r")
	(minus:GPI
	  (neg:GPI (match_dup 1))
	  (match_operand:GPI 3 "aarch64_borrow_operation" "")))]
   ""
   "sbcs\\t%<w>0, <w>zr, %<w>1"
  [(set_attr "type" "adc_reg")]
)

(define_insn "*usub<GPI:mode>3_carryinC_z2"
  [(set (reg:CC CC_REGNUM)
	(compare:CC
	  (zero_extend:<DWI>
	    (match_operand:GPI 1 "register_operand" "r"))
	  (match_operand:<DWI> 2 "aarch64_borrow_operation" "")))
   (set (match_operand:GPI 0 "register_operand" "=r")
	(minus:GPI
	  (match_dup 1)
	  (match_operand:GPI 3 "aarch64_borrow_operation" "")))]
   ""
   "sbcs\\t%<w>0, %<w>1, <w>zr"
  [(set_attr "type" "adc_reg")]
)

(define_insn "*usub<GPI:mode>3_carryinC"
  [(set (reg:CC CC_REGNUM)
	(compare:CC
	  (zero_extend:<DWI>
	    (match_operand:GPI 1 "register_operand" "r"))
	  (plus:<DWI>
	    (zero_extend:<DWI>
	      (match_operand:GPI 2 "register_operand" "r"))
	    (match_operand:<DWI> 3 "aarch64_borrow_operation" ""))))
   (set (match_operand:GPI 0 "register_operand" "=r")
	(minus:GPI
	  (minus:GPI (match_dup 1) (match_dup 2))
	  (match_operand:GPI 4 "aarch64_borrow_operation" "")))]
   ""
   "sbcs\\t%<w>0, %<w>1, %<w>2"
  [(set_attr "type" "adc_reg")]
)

(define_expand "sub<GPI:mode>3_carryinV"
  [(parallel
     [(set (reg:CC_V CC_REGNUM)
	   (compare:CC_V
	    (minus:<DWI>
	     (sign_extend:<DWI>
	       (match_operand:GPI 1 "aarch64_reg_or_zero"))
	     (plus:<DWI>
	       (sign_extend:<DWI>
		 (match_operand:GPI 2 "register_operand"))
	       (ltu:<DWI> (reg:CC CC_REGNUM) (const_int 0))))
	    (sign_extend:<DWI>
	     (minus:GPI (match_dup 1)
			(plus:GPI (ltu:GPI (reg:CC CC_REGNUM) (const_int 0))
				  (match_dup 2))))))
      (set (match_operand:GPI 0 "register_operand")
	   (minus:GPI
	     (minus:GPI (match_dup 1) (match_dup 2))
	     (ltu:GPI (reg:CC CC_REGNUM) (const_int 0))))])]
   ""
)

(define_insn "*sub<mode>3_carryinV_z2"
  [(set (reg:CC_V CC_REGNUM)
	(compare:CC_V
	 (minus:<DWI>
	  (sign_extend:<DWI> (match_operand:GPI 1 "register_operand" "r"))
	  (match_operand:<DWI> 2 "aarch64_borrow_operation" ""))
	 (sign_extend:<DWI>
	  (minus:GPI (match_dup 1)
		     (match_operand:GPI 3 "aarch64_borrow_operation" "")))))
   (set (match_operand:GPI 0 "register_operand" "=r")
	(minus:GPI
	 (match_dup 1) (match_dup 3)))]
   ""
   "sbcs\\t%<w>0, %<w>1, <w>zr"
  [(set_attr "type" "adc_reg")]
)

(define_insn "*sub<mode>3_carryinV"
  [(set (reg:CC_V CC_REGNUM)
	(compare:CC_V
	 (minus:<DWI>
	  (sign_extend:<DWI>
	    (match_operand:GPI 1 "register_operand" "r"))
	  (plus:<DWI>
	    (sign_extend:<DWI>
	      (match_operand:GPI 2 "register_operand" "r"))
	    (match_operand:<DWI> 3 "aarch64_borrow_operation" "")))
	 (sign_extend:<DWI>
	  (minus:GPI
	   (match_dup 1)
	   (plus:GPI (match_operand:GPI 4 "aarch64_borrow_operation" "")
		     (match_dup 2))))))
   (set (match_operand:GPI 0 "register_operand" "=r")
	(minus:GPI
	  (minus:GPI (match_dup 1) (match_dup 2))
	  (match_dup 4)))]
   ""
   "sbcs\\t%<w>0, %<w>1, %<w>2"
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
  return \"sub\t%<w>0, %<w>4, %w1, uxt%e3 %2\";"
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

(define_expand "abs<mode>2"
  [(match_operand:GPI 0 "register_operand")
   (match_operand:GPI 1 "register_operand")]
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
   (set_attr "arch" "*,simd")]
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
  [(set_attr "autodetect_type" "alu_shift_<shift>_op2")]
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
  [(set_attr "autodetect_type" "alu_shift_<shift>_op2")]
)

(define_insn "*neg_asr_si2_extr"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(neg:SI (match_operator:SI 4 "subreg_lowpart_operator"
		  [(sign_extract:DI
		     (match_operand:DI 1 "register_operand" "r")
		     (match_operand 3 "aarch64_simd_shift_imm_offset_si" "n")
		     (match_operand 2 "aarch64_simd_shift_imm_offset_si" "n"))])))]
  "INTVAL (operands[2]) + INTVAL (operands[3]) == 32"
  "neg\\t%w0, %w1, asr %2"
  [(set_attr "autodetect_type" "alu_shift_asr_op2")]
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
	(compare:CC (match_operand:GPI 0 "register_operand" "rk,rk,rk")
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
  "cmp\\t%<GPI:w>1, %w0, <su>xt<ALLX:size>"
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
  "cmp\\t%<GPI:w>2, %w0, <su>xt<ALLX:size> %1"
  [(set_attr "type" "alus_ext")]
)

;; -------------------------------------------------------------------
;; Store-flag and conditional select insns
;; -------------------------------------------------------------------

(define_expand "cstore<mode>4"
  [(set (match_operand:SI 0 "register_operand")
	(match_operator:SI 1 "aarch64_comparison_operator"
	 [(match_operand:GPI 2 "register_operand")
	  (match_operand:GPI 3 "aarch64_plus_operand")]))]
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
  [(set (match_operand:SI 0 "register_operand")
	(match_operator:SI 1 "aarch64_comparison_operator_mode"
	 [(match_operand:GPF 2 "register_operand")
	  (match_operand:GPF 3 "aarch64_fp_compare_operand")]))]
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
		  (match_operand:GPI 2 "aarch64_imm24" "n")))
   (clobber (reg:CC CC_REGNUM))]
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
  [(set (match_operand:GPI 0 "register_operand")
	(if_then_else:GPI
	 (match_operator 1 "aarch64_comparison_operator"
	  [(match_operand:GPI 2 "register_operand")
	   (match_operand:GPI 3 "aarch64_plus_operand")])
	 (match_operand:GPI 4 "register_operand")
	 (match_operand:GPI 5 "register_operand")))]
  ""
  "
  operands[2] = aarch64_gen_compare_reg (GET_CODE (operands[1]), operands[2],
				      operands[3]);
  operands[3] = const0_rtx;
  "
)

(define_expand "cmov<mode>6"
  [(set (match_operand:GPF 0 "register_operand")
	(if_then_else:GPF
	 (match_operator 1 "aarch64_comparison_operator"
	  [(match_operand:GPF 2 "register_operand")
	   (match_operand:GPF 3 "aarch64_fp_compare_operand")])
	 (match_operand:GPF 4 "register_operand")
	 (match_operand:GPF 5 "register_operand")))]
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
  [(set (match_operand:ALLI 0 "register_operand")
	(if_then_else:ALLI (match_operand 1 "aarch64_comparison_operator")
			   (match_operand:ALLI 2 "register_operand")
			   (match_operand:ALLI 3 "register_operand")))]
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
  [(set (match_operand:GPI 0 "register_operand")
	(if_then_else:GPI (match_operand 1 "aarch64_comparison_operator")
			  (match_operand:GPF 2 "register_operand")
			  (match_operand:GPF 3 "register_operand")))]
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
  [(set (match_operand:GPF 0 "register_operand")
	(if_then_else:GPF (match_operand 1 "aarch64_comparison_operator")
			  (match_operand:GPF 2 "register_operand")
			  (match_operand:GPF 3 "register_operand")))]
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
  [(set (match_operand:GPI 0 "register_operand")
	(if_then_else:GPI (match_operand 1 "aarch64_comparison_operator")
			  (NEG_NOT:GPI (match_operand:GPI 2 "register_operand"))
			  (match_operand:GPI 3 "register_operand")))]
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
    if (GET_MODE_BITSIZE (<crc_mode>mode) >= 64)
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

(define_insn "*cs<neg_not_cs>3_uxtw_insn4"
  [(set (match_operand:DI 0 "register_operand" "=r")
	(zero_extend:DI
	  (if_then_else:SI
	    (match_operand 1 "aarch64_comparison_operation" "")
	    (NEG_NOT:SI (match_operand:SI 2 "register_operand" "r"))
	    (match_operand:SI 3 "aarch64_reg_or_zero" "rZ"))))]
  ""
  "cs<neg_not_cs>\\t%w0, %w3, %w2, %M1"
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

(define_insn "*csinv3_uxtw_insn1"
  [(set (match_operand:DI 0 "register_operand" "=r")
	(if_then_else:DI
	  (match_operand 1 "aarch64_comparison_operation" "")
	  (zero_extend:DI
	    (match_operand:SI 2 "register_operand" "r"))
	  (zero_extend:DI
	    (NEG_NOT:SI (match_operand:SI 3 "register_operand" "r")))))]
  ""
  "cs<neg_not_cs>\\t%w0, %w2, %w3, %m1"
  [(set_attr "type" "csel")]
)

(define_insn "*csinv3_uxtw_insn2"
  [(set (match_operand:DI 0 "register_operand" "=r")
	(if_then_else:DI
	  (match_operand 1 "aarch64_comparison_operation" "")
	  (zero_extend:DI
	    (NEG_NOT:SI (match_operand:SI 2 "register_operand" "r")))
	  (zero_extend:DI
	    (match_operand:SI 3 "register_operand" "r"))))]
  ""
  "cs<neg_not_cs>\\t%w0, %w3, %w2, %M1"
  [(set_attr "type" "csel")]
)

(define_insn "*csinv3_uxtw_insn3"
  [(set (match_operand:DI 0 "register_operand" "=r")
	(if_then_else:DI
	  (match_operand 1 "aarch64_comparison_operation" "")
	  (zero_extend:DI
	    (NEG_NOT:SI (match_operand:SI 2 "register_operand" "r")))
	  (const_int 0)))]
  ""
  "cs<neg_not_cs>\\t%w0, wzr, %w2, %M1"
  [(set_attr "type" "csel")]
)

;; If X can be loaded by a single CNT[BHWD] instruction,
;;
;;    A = UMAX (B, X)
;;
;; is equivalent to:
;;
;;    TMP = UQDEC[BHWD] (B, X)
;;    A = TMP + X
;;
;; Defining the pattern this way means that:
;;
;;    A = UMAX (B, X) - X
;;
;; becomes:
;;
;;    TMP1 = UQDEC[BHWD] (B, X)
;;    TMP2 = TMP1 + X
;;    A = TMP2 - X
;;
;; which combine can optimize to:
;;
;;    A = UQDEC[BHWD] (B, X)
;;
;; We don't use match_operand predicates because the order of the operands
;; can vary: the CNT[BHWD] constant will come first if the other operand is
;; a simpler constant (such as a CONST_INT), otherwise it will come second.
(define_expand "umax<mode>3"
  [(set (match_operand:GPI 0 "register_operand")
	(umax:GPI (match_operand:GPI 1 "")
		  (match_operand:GPI 2 "")))]
  "TARGET_SVE"
  {
    if (aarch64_sve_cnt_immediate (operands[1], <MODE>mode))
      std::swap (operands[1], operands[2]);
    else if (!aarch64_sve_cnt_immediate (operands[2], <MODE>mode))
      FAIL;
    rtx temp = gen_reg_rtx (<MODE>mode);
    operands[1] = force_reg (<MODE>mode, operands[1]);
    emit_insn (gen_aarch64_uqdec<mode> (temp, operands[1], operands[2]));
    emit_insn (gen_add<mode>3 (operands[0], temp, operands[2]));
    DONE;
  }
)

;; Saturating unsigned subtraction of a CNT[BHWD] immediate.
(define_insn "aarch64_uqdec<mode>"
  [(set (match_operand:GPI 0 "register_operand" "=r")
	(minus:GPI
	 (umax:GPI (match_operand:GPI 1 "register_operand" "0")
		   (match_operand:GPI 2 "aarch64_sve_cnt_immediate" "Usv"))
	 (match_dup 2)))]
  "TARGET_SVE"
  {
    return aarch64_output_sve_cnt_immediate ("uqdec", "%<w>0", operands[2]);
  }
)

;; Implement MAX/MIN (A, B) - C using SUBS/ADDS followed by CSEL/CSINV/CSINC.
;; See aarch64_maxmin_plus_const for details about the supported cases.
(define_insn_and_split "*aarch64_minmax_plus"
  [(set (match_operand:GPI 0 "register_operand" "=r")
	(plus:GPI
	  (MAXMIN:GPI
	    (match_operand:GPI 1 "register_operand" "r")
	    (match_operand:GPI 2 "const_int_operand"))
	  (match_operand:GPI 3 "aarch64_plus_immediate")))
   (clobber (reg:CC CC_REGNUM))]
  "aarch64_maxmin_plus_const (<CODE>, operands, false)"
  "#"
  "&& 1"
  [(parallel
     [(set (reg:CC CC_REGNUM)
	   (compare:CC (match_dup 1) (match_dup 4)))
      (set (match_dup 6)
	   (plus:GPI (match_dup 1) (match_dup 3)))])
   (set (match_dup 0)
	(if_then_else:GPI (match_dup 5) (match_dup 6) (match_dup 7)))]
  {
    if (!aarch64_maxmin_plus_const (<CODE>, operands, true))
      gcc_unreachable ();
  }
  [(set_attr "length" "8")]
)

;; -------------------------------------------------------------------
;; Logical operations
;; -------------------------------------------------------------------


(define_insn_and_split "*aarch64_and<mode>_imm2"
  [(set (match_operand:GPI 0 "register_operand" "=rk")
	(and:GPI (match_operand:GPI 1 "register_operand" "%r")
		 (match_operand:GPI 2 "aarch64_logical_and_immediate" "<lconst2>")))]
  ""
  "#"
  "true"
  [(const_int 0)]
  {
     HOST_WIDE_INT val = INTVAL (operands[2]);
     rtx imm1 = GEN_INT (aarch64_and_split_imm1 (val));
     rtx imm2 = GEN_INT (aarch64_and_split_imm2 (val));

     emit_insn (gen_and<mode>3 (operands[0], operands[1], imm1));
     emit_insn (gen_and<mode>3 (operands[0], operands[0], imm2));
     DONE;
  }
)

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
   (set_attr "arch" "*,*,simd")]
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
  [(set (reg:CC_NZV CC_REGNUM)
	(compare:CC_NZV
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
  [(set (reg:CC_NZV CC_REGNUM)
	(compare:CC_NZV
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
  [(set (reg:CC_NZV CC_REGNUM)
	(compare:CC_NZV
	 (and:GPI (SHIFT:GPI
		   (match_operand:GPI 1 "register_operand" "r")
		   (match_operand:QI 2 "aarch64_shift_imm_<mode>" "n"))
		  (match_operand:GPI 3 "register_operand" "r"))
	 (const_int 0)))
   (set (match_operand:GPI 0 "register_operand" "=r")
	(and:GPI (SHIFT:GPI (match_dup 1) (match_dup 2)) (match_dup 3)))]
  ""
{
  if (<SHIFT:is_rotl>)
    operands[2] = GEN_INT (<sizen> - UINTVAL (operands[2]));
  return "ands\\t%<w>0, %<w>3, %<w>1, <SHIFT:shift> %2";
}
  [(set_attr "type" "logics_shift_imm")]
)

;; zero_extend version of above
(define_insn "*and_<SHIFT:optab>si3_compare0_uxtw"
  [(set (reg:CC_NZV CC_REGNUM)
	(compare:CC_NZV
	 (and:SI (SHIFT:SI
		  (match_operand:SI 1 "register_operand" "r")
		  (match_operand:QI 2 "aarch64_shift_imm_si" "n"))
		 (match_operand:SI 3 "register_operand" "r"))
	 (const_int 0)))
   (set (match_operand:DI 0 "register_operand" "=r")
	(zero_extend:DI (and:SI (SHIFT:SI (match_dup 1) (match_dup 2))
				(match_dup 3))))]
  ""
{
  if (<SHIFT:is_rotl>)
    operands[2] = GEN_INT (32 - UINTVAL (operands[2]));
  return "ands\\t%w0, %w3, %w1, <SHIFT:shift> %2";
}
  [(set_attr "type" "logics_shift_imm")]
)

(define_insn "*<LOGICAL:optab>_<SHIFT:optab><mode>3"
  [(set (match_operand:GPI 0 "register_operand" "=r")
	(LOGICAL:GPI (SHIFT:GPI
		      (match_operand:GPI 1 "register_operand" "r")
		      (match_operand:QI 2 "aarch64_shift_imm_<mode>" "n"))
		     (match_operand:GPI 3 "register_operand" "r")))]
  ""
{
  if (<SHIFT:is_rotl>)
    operands[2] = GEN_INT (<sizen> - UINTVAL (operands[2]));
  return "<LOGICAL:logical>\\t%<w>0, %<w>3, %<w>1, <SHIFT:shift> %2";
}
  [(set_attr "type" "logic_shift_imm")]
)

(define_split
  [(set (match_operand:GPI 0 "register_operand")
	(LOGICAL_OR_PLUS:GPI
	  (and:GPI (ashift:GPI (match_operand:GPI 1 "register_operand")
			       (match_operand:QI 2 "aarch64_shift_imm_<mode>"))
		   (match_operand:GPI 3 "const_int_operand"))
	  (zero_extend:GPI (match_operand 4 "register_operand"))))]
  "can_create_pseudo_p ()
   && ((paradoxical_subreg_p (operands[1])
	&& rtx_equal_p (SUBREG_REG (operands[1]), operands[4]))
       || (REG_P (operands[1])
	   && REG_P (operands[4])
	   && REGNO (operands[1]) == REGNO (operands[4])))
   && (trunc_int_for_mode (GET_MODE_MASK (GET_MODE (operands[4]))
			   << INTVAL (operands[2]), <MODE>mode)
       == INTVAL (operands[3]))
   && (<CODE> != PLUS
       || (GET_MODE_MASK (GET_MODE (operands[4]))
	   & INTVAL (operands[3])) == 0)"
  [(set (match_dup 5) (zero_extend:GPI (match_dup 4)))
   (set (match_dup 0) (match_dup 6))]
  {
    operands[5] = gen_reg_rtx (<MODE>mode);
    rtx shift = gen_rtx_ASHIFT (<MODE>mode, operands[5], operands[2]);
    rtx_code new_code = (<CODE> == PLUS ? IOR : <CODE>);
    operands[6] = gen_rtx_fmt_ee (new_code, <MODE>mode, shift, operands[5]);
  }
)

(define_split
  [(set (match_operand:GPI 0 "register_operand")
	(LOGICAL_OR_PLUS:GPI
	  (and:GPI (ashift:GPI (match_operand:GPI 1 "register_operand")
			       (match_operand:QI 2 "aarch64_shift_imm_<mode>"))
		   (match_operand:GPI 4 "const_int_operand"))
	  (and:GPI (match_dup 1) (match_operand:GPI 3 "const_int_operand"))))]
  "can_create_pseudo_p ()
   && pow2_or_zerop (UINTVAL (operands[3]) + 1)
   && (trunc_int_for_mode (UINTVAL (operands[3])
			   << INTVAL (operands[2]), <MODE>mode)
       == INTVAL (operands[4]))
   && (<CODE> != PLUS
       || (INTVAL (operands[4]) & INTVAL (operands[3])) == 0)"
  [(set (match_dup 5) (and:GPI (match_dup 1) (match_dup 3)))
   (set (match_dup 0) (match_dup 6))]
  {
    operands[5] = gen_reg_rtx (<MODE>mode);
    rtx shift = gen_rtx_ASHIFT (<MODE>mode, operands[5], operands[2]);
    rtx_code new_code = (<CODE> == PLUS ? IOR : <CODE>);
    operands[6] = gen_rtx_fmt_ee (new_code, <MODE>mode, shift, operands[5]);
  }
)

(define_split
  [(set (match_operand:GPI 0 "register_operand")
	(LOGICAL:GPI
	  (ashift:GPI (sign_extend:GPI (match_operand 1 "register_operand"))
		      (match_operand:QI 2 "aarch64_shift_imm_<mode>"))
	  (sign_extend:GPI (match_dup 1))))]
  "can_create_pseudo_p ()"
  [(set (match_dup 3) (sign_extend:GPI (match_dup 1)))
   (set (match_dup 0) (LOGICAL:GPI (ashift:GPI (match_dup 3) (match_dup 2))
				   (match_dup 3)))]
  "operands[3] = gen_reg_rtx (<MODE>mode);"
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
{
  if (<SHIFT:is_rotl>)
    operands[2] = GEN_INT (32 - UINTVAL (operands[2]));
  return "<LOGICAL:logical>\\t%w0, %w3, %w1, <SHIFT:shift> %2";
}
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
   (set_attr "arch" "*,simd")]
)

(define_insn "*one_cmpl_zero_extend"
  [(set (match_operand:DI 0 "register_operand" "=r")
        (zero_extend:DI
          (not:SI (match_operand:SI 1 "register_operand" "r"))))]
  ""
  "mvn\\t%w0, %w1"
  [(set_attr "type" "logic_reg")]
)

(define_insn "*one_cmpl_<optab><mode>2"
  [(set (match_operand:GPI 0 "register_operand" "=r")
	(not:GPI (SHIFT:GPI (match_operand:GPI 1 "register_operand" "r")
			    (match_operand:QI 2 "aarch64_shift_imm_<mode>" "n"))))]
  ""
{
  if (<SHIFT:is_rotl>)
    operands[2] = GEN_INT (<sizen> - UINTVAL (operands[2]));
  return "mvn\\t%<w>0, %<w>1, <shift> %2";
}
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
   (set_attr "arch" "*,simd")]
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
   (set_attr "arch" "*,simd")]
)

(define_insn "*and_one_cmpl<mode>3_compare0"
  [(set (reg:CC_NZV CC_REGNUM)
	(compare:CC_NZV
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
  [(set (reg:CC_NZV CC_REGNUM)
	(compare:CC_NZV
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
  [(set (reg:CC_NZV CC_REGNUM)
    (compare:CC_NZV
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
{
  if (<SHIFT:is_rotl>)
    operands[2] = GEN_INT (<sizen> - UINTVAL (operands[2]));
  return "<LOGICAL:nlogical>\\t%<w>0, %<w>3, %<w>1, <SHIFT:shift> %2";
}
  [(set_attr "type" "logic_shift_imm")]
)

;; Zero-extend version of the above.
(define_insn "<LOGICAL:optab>_one_cmpl_<SHIFT:optab>sidi_uxtw"
  [(set (match_operand:DI 0 "register_operand" "=r")
	(zero_extend:DI (LOGICAL:SI (not:SI
			 (SHIFT:SI
			  (match_operand:SI 1 "register_operand" "r")
			  (match_operand:QI 2 "aarch64_shift_imm_si" "n")))
			 (match_operand:SI 3 "register_operand" "r"))))]
  ""
{
  if (<SHIFT:is_rotl>)
    operands[2] = GEN_INT (32 - UINTVAL (operands[2]));
  return "<LOGICAL:nlogical>\\t%w0, %w3, %w1, <SHIFT:shift> %2";
}
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
{
  if (<SHIFT:is_rotl>)
    operands[2] = GEN_INT (<sizen> - UINTVAL (operands[2]));
  return "eon\\t%<w>0, %<w>3, %<w>1, <SHIFT:shift> %2";
}
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
{
  if (<SHIFT:is_rotl>)
    operands[2] = GEN_INT (32 - UINTVAL (operands[2]));
  return "eon\\t%w0, %w3, %w1, <SHIFT:shift> %2";
}
  [(set_attr "type" "logic_shift_imm")]
)

(define_insn "*and_one_cmpl_<SHIFT:optab><mode>3_compare0"
  [(set (reg:CC_NZV CC_REGNUM)
	(compare:CC_NZV
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
{
  if (<SHIFT:is_rotl>)
    operands[2] = GEN_INT (<sizen> - UINTVAL (operands[2]));
  return "bics\\t%<w>0, %<w>3, %<w>1, <SHIFT:shift> %2";
}
  [(set_attr "type" "logics_shift_imm")]
)

;; zero_extend version of above
(define_insn "*and_one_cmpl_<SHIFT:optab>si3_compare0_uxtw"
  [(set (reg:CC_NZV CC_REGNUM)
	(compare:CC_NZV
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
{
  if (<SHIFT:is_rotl>)
    operands[2] = GEN_INT (32 - UINTVAL (operands[2]));
  return "bics\\t%w0, %w3, %w1, <SHIFT:shift> %2";
}
  [(set_attr "type" "logics_shift_imm")]
)

(define_insn "*and_one_cmpl_<SHIFT:optab><mode>3_compare0_no_reuse"
  [(set (reg:CC_NZV CC_REGNUM)
    (compare:CC_NZV
     (and:GPI (not:GPI
           (SHIFT:GPI
            (match_operand:GPI 0 "register_operand" "r")
            (match_operand:QI 1 "aarch64_shift_imm_<mode>" "n")))
          (match_operand:GPI 2 "register_operand" "r"))
     (const_int 0)))]
  ""
{
  if (<SHIFT:is_rotl>)
    operands[1] = GEN_INT (<sizen> - UINTVAL (operands[1]));
  return "bics\\t<w>zr, %<w>2, %<w>0, <SHIFT:shift> %1";
}
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

    emit_insn (gen_aarch64_rbit (<MODE>mode, operands[0], operands[1]));
    emit_insn (gen_clz<mode>2 (operands[0], operands[0]));
    emit_insn (gen_csinc3<mode>_insn (operands[0], x, operands[0], const0_rtx));
    DONE;
  }
)

;; Pop count be done via the "CNT" instruction in AdvSIMD.
;;
;; MOV	v.1d, x0
;; CNT	v1.8b, v.8b
;; ADDV b2, v1.8b
;; MOV	w0, v2.b[0]

(define_expand "popcount<mode>2"
  [(match_operand:GPI 0 "register_operand")
   (match_operand:GPI 1 "register_operand")]
  "TARGET_SIMD"
{
  rtx v = gen_reg_rtx (V8QImode);
  rtx v1 = gen_reg_rtx (V8QImode);
  rtx in = operands[1];
  rtx out = operands[0];
  if(<MODE>mode == SImode)
    {
      rtx tmp;
      tmp = gen_reg_rtx (DImode);
      /* If we have SImode, zero extend to DImode, pop count does
         not change if we have extra zeros. */
      emit_insn (gen_zero_extendsidi2 (tmp, in));
      in = tmp;
    }
  emit_move_insn (v, gen_lowpart (V8QImode, in));
  emit_insn (gen_popcountv8qi2 (v1, v));
  emit_insn (gen_aarch64_zero_extend<mode>_reduc_plus_v8qi (out, v1));
  DONE;
})

(define_insn "clrsb<mode>2"
  [(set (match_operand:GPI 0 "register_operand" "=r")
        (clrsb:GPI (match_operand:GPI 1 "register_operand" "r")))]
  ""
  "cls\\t%<w>0, %<w>1"
  [(set_attr "type" "clz")]
)

(define_insn "@aarch64_rbit<mode>"
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
  emit_insn (gen_aarch64_rbit (<MODE>mode, operands[0], operands[1]));
  emit_insn (gen_clz<mode>2 (operands[0], operands[0]));
  DONE;
")

(define_insn "*and<mode>_compare0"
  [(set (reg:CC_Z CC_REGNUM)
	(compare:CC_Z
	 (match_operand:SHORT 0 "register_operand" "r")
	 (const_int 0)))]
  ""
  "tst\\t%<w>0, <short_mask>"
  [(set_attr "type" "alus_imm")]
)

(define_insn "*ands<GPI:mode>_compare0"
  [(set (reg:CC_Z CC_REGNUM)
	(compare:CC_Z
	 (zero_extend:GPI (match_operand:SHORT 1 "register_operand" "r"))
	 (const_int 0)))
   (set (match_operand:GPI 0 "register_operand" "=r")
	(zero_extend:GPI (match_dup 1)))]
  ""
  "ands\\t%<GPI:w>0, %<GPI:w>1, <short_mask>"
  [(set_attr "type" "alus_imm")]
)

(define_insn "*and<mode>3nr_compare0"
  [(set (reg:CC_NZV CC_REGNUM)
	(compare:CC_NZV
	 (and:GPI (match_operand:GPI 0 "register_operand" "%r,r")
		  (match_operand:GPI 1 "aarch64_logical_operand" "r,<lconst>"))
	 (const_int 0)))]
  ""
  "@
   tst\\t%<w>0, %<w>1
   tst\\t%<w>0, %1"
  [(set_attr "type" "logics_reg,logics_imm")]
)

(define_split
  [(set (reg:CC_NZV CC_REGNUM)
	(compare:CC_NZV
	 (and:GPI (match_operand:GPI 0 "register_operand")
		  (match_operand:GPI 1 "aarch64_mov_imm_operand"))
	 (const_int 0)))
   (clobber (match_operand:SI 2 "register_operand"))]
  ""
  [(set (match_dup 2) (match_dup 1))
   (set (reg:CC_NZV CC_REGNUM)
	(compare:CC_NZV
	 (and:GPI (match_dup 0)
		  (match_dup 2))
	 (const_int 0)))]
)

(define_insn "*and<mode>3nr_compare0_zextract"
  [(set (reg:CC_Z CC_REGNUM)
	(compare:CC_Z
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
  [(set (reg:CC_NZV CC_REGNUM)
	(compare:CC_NZV
	 (and:GPI (SHIFT:GPI
		   (match_operand:GPI 0 "register_operand" "r")
		   (match_operand:QI 1 "aarch64_shift_imm_<mode>" "n"))
		  (match_operand:GPI 2 "register_operand" "r"))
	(const_int 0)))]
  ""
{
  if (<SHIFT:is_rotl>)
    operands[1] = GEN_INT (<sizen> - UINTVAL (operands[1]));
  return "tst\\t%<w>2, %<w>0, <SHIFT:shift> %1";
}
  [(set_attr "type" "logics_shift_imm")]
)

(define_split
  [(set (reg:CC_NZV CC_REGNUM)
	(compare:CC_NZV
	 (and:GPI (SHIFT:GPI
		   (match_operand:GPI 0 "register_operand")
		   (match_operand:QI 1 "aarch64_shift_imm_<mode>"))
		  (match_operand:GPI 2 "aarch64_mov_imm_operand"))
	(const_int 0)))
    (clobber (match_operand:SI 3 "register_operand"))]
  ""
  [(set (match_dup 3) (match_dup 2))
   (set (reg:CC_NZV CC_REGNUM)
	(compare:CC_NZV
	 (and:GPI (SHIFT:GPI
		   (match_dup 0)
		   (match_dup 1))
		  (match_dup 3))
	 (const_int 0)))]
)

;; -------------------------------------------------------------------
;; Shifts
;; -------------------------------------------------------------------

(define_expand "<optab><mode>3"
  [(set (match_operand:GPI 0 "register_operand")
	(ASHIFT:GPI (match_operand:GPI 1 "register_operand")
		    (match_operand:QI 2 "aarch64_reg_or_imm")))]
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
		      (match_operand:QI 2 "aarch64_reg_or_imm")))]
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
		      (match_operand:QI 2 "aarch64_reg_or_imm")))]
  ""
  {
    /* (SZ - cnt) % SZ == -cnt % SZ */
    if (CONST_INT_P (operands[2]))
      {
        operands[2] = GEN_INT ((-UINTVAL (operands[2]))
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

;; When the LSL, LSR, ASR, ROR instructions operate on all register arguments
;; they truncate the shift/rotate amount by the size of the registers they
;; operate on: 32 for W-regs, 64 for X-regs.  This allows us to optimise away
;; such redundant masking instructions.  GCC can do that automatically when
;; SHIFT_COUNT_TRUNCATED is true, but we can't enable it for TARGET_SIMD
;; because some of the SISD shift alternatives don't perform this truncations.
;; So this pattern exists to catch such cases.

(define_insn "*aarch64_<optab>_reg_<mode>3_mask1"
  [(set (match_operand:GPI 0 "register_operand" "=r")
	(SHIFT:GPI
	  (match_operand:GPI 1 "register_operand" "r")
	  (match_operator 4 "subreg_lowpart_operator"
	   [(and:GPI (match_operand:GPI 2 "register_operand" "r")
		     (match_operand 3 "const_int_operand" "n"))])))]
  "(~INTVAL (operands[3]) & (GET_MODE_BITSIZE (<MODE>mode) - 1)) == 0"
  "<shift>\t%<w>0, %<w>1, %<w>2"
  [(set_attr "type" "shift_reg")]
)

(define_insn_and_split "*aarch64_<optab>_reg_<mode>3_neg_mask2"
  [(set (match_operand:GPI 0 "register_operand" "=&r")
	(SHIFT:GPI
	  (match_operand:GPI 1 "register_operand" "r")
	  (match_operator 4 "subreg_lowpart_operator"
	  [(neg:SI (and:SI (match_operand:SI 2 "register_operand" "r")
			   (match_operand 3 "const_int_operand" "n")))])))]
  "((~INTVAL (operands[3]) & (GET_MODE_BITSIZE (<MODE>mode) - 1)) == 0)"
  "#"
  "&& true"
  [(const_int 0)]
  {
    rtx tmp = (can_create_pseudo_p () ? gen_reg_rtx (SImode)
	       : lowpart_subreg (SImode, operands[0], <MODE>mode));
    emit_insn (gen_negsi2 (tmp, operands[2]));

    rtx and_op = gen_rtx_AND (SImode, tmp, operands[3]);
    rtx subreg_tmp = gen_rtx_SUBREG (GET_MODE (operands[4]), and_op,
				     SUBREG_BYTE (operands[4]));
    emit_insn (gen_<optab><mode>3 (operands[0], operands[1], subreg_tmp));
    DONE;
  }
)

(define_insn_and_split "*aarch64_ashl_reg_<mode>3_minus_mask"
  [(set (match_operand:GPI 0 "register_operand" "=&r")
	(ashift:GPI
	  (match_operand:GPI 1 "register_operand" "r")
	  (minus:QI (match_operand 2 "const_int_operand" "n")
		    (match_operator 5 "subreg_lowpart_operator"
		    [(and:SI (match_operand:SI 3 "register_operand" "r")
			     (match_operand 4 "const_int_operand" "n"))]))))]
  "((~INTVAL (operands[4]) & (GET_MODE_BITSIZE (<MODE>mode) - 1)) == 0)
   && INTVAL (operands[2]) == GET_MODE_BITSIZE (<MODE>mode)"
  "#"
  "&& true"
  [(const_int 0)]
  {
    rtx tmp = (can_create_pseudo_p () ? gen_reg_rtx (SImode)
	       : operands[0]);

    emit_insn (gen_negsi2 (tmp, operands[3]));

    rtx and_op = gen_rtx_AND (SImode, tmp, operands[4]);
    rtx subreg_tmp = gen_rtx_SUBREG (GET_MODE (operands[5]), and_op,
				     SUBREG_BYTE (operands[5]));

    emit_insn (gen_ashl<mode>3 (operands[0], operands[1], subreg_tmp));
    DONE;
  }
)

(define_insn "*aarch64_<optab>_reg_di3_mask2"
  [(set (match_operand:DI 0 "register_operand" "=r")
	(SHIFT:DI
	  (match_operand:DI 1 "register_operand" "r")
	  (match_operator 4 "subreg_lowpart_operator"
	   [(and:SI (match_operand:SI 2 "register_operand" "r")
		    (match_operand 3 "const_int_operand" "n"))])))]
  "((~INTVAL (operands[3]) & (GET_MODE_BITSIZE (DImode) - 1)) == 0)"
{
  rtx xop[3];
  xop[0] = operands[0];
  xop[1] = operands[1];
  xop[2] = gen_lowpart (GET_MODE (operands[4]), operands[2]);
  output_asm_insn ("<shift>\t%x0, %x1, %x2", xop);
  return "";
}
  [(set_attr "type" "shift_reg")]
)

(define_insn_and_split "*aarch64_<optab>_reg_minus<mode>3"
  [(set (match_operand:GPI 0 "register_operand" "=&r")
	(ASHIFT:GPI
	  (match_operand:GPI 1 "register_operand" "r")
	  (minus:QI (match_operand 2 "const_int_operand" "n")
		    (match_operand:QI 3 "register_operand" "r"))))]
  "INTVAL (operands[2]) == GET_MODE_BITSIZE (<MODE>mode)"
  "#"
  "&& true"
  [(const_int 0)]
  {
    rtx subreg_tmp = gen_lowpart (SImode, operands[3]);

    rtx tmp = (can_create_pseudo_p () ? gen_reg_rtx (SImode)
	       : gen_lowpart (SImode, operands[0]));

    emit_insn (gen_negsi2 (tmp, subreg_tmp));

    rtx and_op = gen_rtx_AND (SImode, tmp,
			      GEN_INT (GET_MODE_BITSIZE (<MODE>mode) - 1));

    rtx subreg_tmp2 = gen_lowpart_SUBREG (QImode, and_op);

    emit_insn (gen_<optab><mode>3 (operands[0], operands[1], subreg_tmp2));
    DONE;
  }
  [(set_attr "length" "8")]
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
  [(set_attr "type" "bfx,shift_reg,neon_shift_imm<q>, neon_shift_reg<q>")
   (set_attr "arch" "*,*,simd,simd")]
)

;; Logical right shift using SISD or Integer instruction
(define_insn "*aarch64_lshr_sisd_or_int_<mode>3"
  [(set (match_operand:GPI 0 "register_operand" "=r,r,w,&w,&w")
	(lshiftrt:GPI
	 (match_operand:GPI 1 "register_operand" "r,r,w,w,w")
	 (match_operand:QI 2 "aarch64_reg_or_shift_imm_<mode>"
			      "Us<cmode>,r,Us<cmode_simd>,w,0")))]
  ""
  "@
   lsr\t%<w>0, %<w>1, %2
   lsr\t%<w>0, %<w>1, %<w>2
   ushr\t%<rtn>0<vas>, %<rtn>1<vas>, %2
   #
   #"
  [(set_attr "type" "bfx,shift_reg,neon_shift_imm<q>,neon_shift_reg<q>,neon_shift_reg<q>")
   (set_attr "arch" "*,*,simd,simd,simd")]
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
	  (match_operand:QI 2 "aarch64_reg_or_shift_imm_di"
			       "Us<cmode>,r,Us<cmode_simd>,w,0")))]
  ""
  "@
   asr\t%<w>0, %<w>1, %2
   asr\t%<w>0, %<w>1, %<w>2
   sshr\t%<rtn>0<vas>, %<rtn>1<vas>, %2
   #
   #"
  [(set_attr "type" "bfx,shift_reg,neon_shift_imm<q>,neon_shift_reg<q>,neon_shift_reg<q>")
   (set_attr "arch" "*,*,simd,simd,simd")]
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
  [(set_attr "type" "neon_shift_reg")]
)

(define_insn "*aarch64_ushl_2s"
  [(set (match_operand:SI 0 "register_operand" "=w")
        (unspec:SI [(match_operand:SI 1 "register_operand" "w")
                    (match_operand:QI 2 "register_operand" "w")]
                   UNSPEC_USHL_2S))]
  "TARGET_SIMD"
  "ushl\t%0.2s, %1.2s, %2.2s"
  [(set_attr "type" "neon_shift_reg")]
)

(define_insn "*aarch64_sisd_sshl"
  [(set (match_operand:DI 0 "register_operand" "=w")
        (unspec:DI [(match_operand:DI 1 "register_operand" "w")
                    (match_operand:QI 2 "register_operand" "w")]
                   UNSPEC_SISD_SSHL))]
  "TARGET_SIMD"
  "sshl\t%d0, %d1, %d2"
  [(set_attr "type" "neon_shift_reg")]
)

(define_insn "*aarch64_sshl_2s"
  [(set (match_operand:SI 0 "register_operand" "=w")
        (unspec:SI [(match_operand:SI 1 "register_operand" "w")
                    (match_operand:QI 2 "register_operand" "w")]
                   UNSPEC_SSHL_2S))]
  "TARGET_SIMD"
  "sshl\t%0.2s, %1.2s, %2.2s"
  [(set_attr "type" "neon_shift_reg")]
)

(define_insn "*aarch64_sisd_neg_qi"
  [(set (match_operand:QI 0 "register_operand" "=w")
        (unspec:QI [(match_operand:QI 1 "register_operand" "w")]
                   UNSPEC_SISD_NEG))]
  "TARGET_SIMD"
  "neg\t%d0, %d1"
  [(set_attr "type" "neon_neg")]
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

(define_insn "*rol<mode>3_insn"
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

;; zero_extend version of shifts
(define_insn "*<optab>si3_insn_uxtw"
  [(set (match_operand:DI 0 "register_operand" "=r,r")
	(zero_extend:DI (SHIFT_no_rotate:SI
	 (match_operand:SI 1 "register_operand" "r,r")
	 (match_operand:QI 2 "aarch64_reg_or_shift_imm_si" "Uss,r"))))]
  ""
  "@
   <shift>\\t%w0, %w1, %2
   <shift>\\t%w0, %w1, %w2"
  [(set_attr "type" "bfx,shift_reg")]
)

;; zero_extend version of rotate right
(define_insn "*rorsi3_insn_uxtw"
  [(set (match_operand:DI 0 "register_operand" "=r")
        (zero_extend:DI
         (rotatert:SI (match_operand:SI 1 "register_operand" "r")
                    (match_operand 2 "const_int_operand" "n"))))]
  "UINTVAL (operands[2]) < 32"
  "ror\\t%w0, %w1, %2"
  [(set_attr "type" "rotate_imm")]
)

;; zero_extend version of rotate left
(define_insn "*rolsi3_insn_uxtw"
  [(set (match_operand:DI 0 "register_operand" "=r")
        (zero_extend:DI
         (rotate:SI (match_operand:SI 1 "register_operand" "r")
                    (match_operand 2 "const_int_operand" "n"))))]
  "UINTVAL (operands[2]) < 32"
{
  operands[2] = GEN_INT (32 - UINTVAL (operands[2]));
  return "ror\\t%w0, %w1, %2";
}
  [(set_attr "type" "rotate_imm")]
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
  [(set_attr "type" "bfx")]
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

(define_insn "*extrsi5_insn_di"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(ior:SI (ashift:SI (match_operand:SI 1 "register_operand" "r")
			   (match_operand 3 "const_int_operand" "n"))
		(match_operator:SI 6 "subreg_lowpart_operator"
		  [(zero_extract:DI
		     (match_operand:DI 2 "register_operand" "r")
		     (match_operand 5 "const_int_operand" "n")
		     (match_operand 4 "const_int_operand" "n"))])))]
  "UINTVAL (operands[3]) < 32
   && UINTVAL (operands[3]) + UINTVAL (operands[4]) == 32
   && INTVAL (operands[3]) == INTVAL (operands[5])"
  "extr\\t%w0, %w1, %w2, %4"
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
  [(set_attr "type" "bfx")]
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
  [(set_attr "type" "bfx")]
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
  [(set_attr "type" "bfx")]
)

;; -------------------------------------------------------------------
;; Bitfields
;; -------------------------------------------------------------------

(define_expand "<optab>"
  [(set (match_operand:DI 0 "register_operand")
	(ANY_EXTRACT:DI (match_operand:DI 1 "register_operand")
			(match_operand 2
			  "aarch64_simd_shift_imm_offset_di")
			(match_operand 3 "aarch64_simd_shift_imm_di")))]
  ""
  {
    if (!IN_RANGE (INTVAL (operands[2]) + INTVAL (operands[3]),
		   1, GET_MODE_BITSIZE (DImode) - 1))
     FAIL;
  }
)


(define_insn "*<optab><mode>"
  [(set (match_operand:GPI 0 "register_operand" "=r")
	(ANY_EXTRACT:GPI (match_operand:GPI 1 "register_operand" "r")
			 (match_operand 2
			   "aarch64_simd_shift_imm_offset_<mode>" "n")
			 (match_operand 3
			   "aarch64_simd_shift_imm_<mode>" "n")))]
  "IN_RANGE (INTVAL (operands[2]) + INTVAL (operands[3]),
	     1, GET_MODE_BITSIZE (<MODE>mode) - 1)"
  "<su>bfx\\t%<w>0, %<w>1, %3, %2"
  [(set_attr "type" "bfx")]
)

;; When the bit position and width add up to 32 we can use a W-reg LSR
;; instruction taking advantage of the implicit zero-extension of the X-reg.
(define_split
  [(set (match_operand:DI 0 "register_operand")
	(zero_extract:DI (match_operand:DI 1 "register_operand")
			 (match_operand 2
			   "aarch64_simd_shift_imm_offset_di")
			 (match_operand 3
			   "aarch64_simd_shift_imm_di")))]
  "IN_RANGE (INTVAL (operands[2]) + INTVAL (operands[3]), 1,
	     GET_MODE_BITSIZE (DImode) - 1)
   && (INTVAL (operands[2]) + INTVAL (operands[3]))
       == GET_MODE_BITSIZE (SImode)"
  [(set (match_dup 0)
	(zero_extend:DI (lshiftrt:SI (match_dup 4) (match_dup 3))))]
  {
    operands[4] = gen_lowpart (SImode, operands[1]);
  }
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

;;  Match a bfi instruction where the shift of OP3 means that we are
;;  actually copying the least significant bits of OP3 into OP0 by way
;;  of the AND masks and the IOR instruction.  A similar instruction
;;  with the two parts of the IOR swapped around was never triggered
;;  in a bootstrap build and test of GCC so it was not included.

(define_insn "*aarch64_bfi<GPI:mode>5_shift"
  [(set (match_operand:GPI 0 "register_operand" "=r")
        (ior:GPI (and:GPI (match_operand:GPI 1 "register_operand" "0")
                          (match_operand:GPI 2 "const_int_operand" "n"))
                 (and:GPI (ashift:GPI
                           (match_operand:GPI 3 "register_operand" "r")
                           (match_operand:GPI 4 "aarch64_simd_shift_imm_<mode>" "n"))
                          (match_operand:GPI 5 "const_int_operand" "n"))))]
  "aarch64_masks_and_shift_for_bfi_p (<MODE>mode, UINTVAL (operands[2]),
				      UINTVAL (operands[4]),
				      UINTVAL(operands[5]))"
  "bfi\t%<GPI:w>0, %<GPI:w>3, %4, %P5"
  [(set_attr "type" "bfm")]
)

(define_insn "*aarch64_bfi<GPI:mode>5_shift_alt"
  [(set (match_operand:GPI 0 "register_operand" "=r")
        (ior:GPI (and:GPI (ashift:GPI
                           (match_operand:GPI 1 "register_operand" "r")
                           (match_operand:GPI 2 "aarch64_simd_shift_imm_<mode>" "n"))
                          (match_operand:GPI 3 "const_int_operand" "n"))
		 (and:GPI (match_operand:GPI 4 "register_operand" "0")
                          (match_operand:GPI 5 "const_int_operand" "n"))))]
  "aarch64_masks_and_shift_for_bfi_p (<MODE>mode, UINTVAL (operands[5]),
				      UINTVAL (operands[2]),
				      UINTVAL(operands[3]))"
  "bfi\t%<GPI:w>0, %<GPI:w>1, %2, %P3"
  [(set_attr "type" "bfm")]
)

;; Like *aarch64_bfi<GPI:mode>5_shift but with no and of the ashift because
;; the shift is large enough to remove the need for an AND instruction.

(define_insn "*aarch64_bfi<GPI:mode>4_noand"
  [(set (match_operand:GPI 0 "register_operand" "=r")
        (ior:GPI (and:GPI (match_operand:GPI 1 "register_operand" "0")
                          (match_operand:GPI 2 "const_int_operand" "n"))
                 (ashift:GPI
                          (match_operand:GPI 3 "register_operand" "r")
                          (match_operand:GPI 4 "aarch64_simd_shift_imm_<mode>" "n"))))]
  "aarch64_masks_and_shift_for_bfi_p (<MODE>mode, UINTVAL (operands[2]),
				      UINTVAL (operands[4]),
				      HOST_WIDE_INT_M1U << UINTVAL (operands[4]) )"
{
  operands[5] = GEN_INT (GET_MODE_BITSIZE (<MODE>mode) - UINTVAL (operands[4]));
  return "bfi\t%<GPI:w>0, %<GPI:w>3, %4, %5";
}
  [(set_attr "type" "bfm")]
)

(define_insn "*aarch64_bfi<GPI:mode>4_noand_alt"
  [(set (match_operand:GPI 0 "register_operand" "=r")
        (ior:GPI (ashift:GPI
                          (match_operand:GPI 1 "register_operand" "r")
                          (match_operand:GPI 2 "aarch64_simd_shift_imm_<mode>" "n"))
		 (and:GPI (match_operand:GPI 3 "register_operand" "0")
                          (match_operand:GPI 4 "const_int_operand" "n"))))]
  "aarch64_masks_and_shift_for_bfi_p (<MODE>mode, UINTVAL (operands[4]),
				      UINTVAL (operands[2]),
				      HOST_WIDE_INT_M1U << UINTVAL (operands[2]) )"
{
  operands[5] = GEN_INT (GET_MODE_BITSIZE (<MODE>mode) - UINTVAL (operands[2]));
  return "bfi\t%<GPI:w>0, %<GPI:w>1, %2, %5";
}
  [(set_attr "type" "bfm")]
)

;; Like *aarch64_bfi<GPI:mode>5_shift but with no shifting, we are just
;; copying the least significant bits of OP3 to OP0.  We need two versions
;; of the instruction to handle different checks on the constant values.

(define_insn "*aarch64_bfi<GPI:mode>4_noshift"
  [(set (match_operand:GPI 0 "register_operand" "=r")
        (ior:GPI (and:GPI (match_operand:GPI 1 "register_operand" "0")
                          (match_operand:GPI 2 "const_int_operand" "n"))
                 (and:GPI (match_operand:GPI 3 "register_operand" "r")
                          (match_operand:GPI 4 "const_int_operand" "n"))))]
  "aarch64_masks_and_shift_for_bfi_p (<MODE>mode, UINTVAL (operands[2]), 0,
				      UINTVAL (operands[4]))"
  "bfi\t%<GPI:w>0, %<GPI:w>3, 0, %P4"
  [(set_attr "type" "bfm")]
)

(define_insn "*aarch64_bfi<GPI:mode>4_noshift_alt"
  [(set (match_operand:GPI 0 "register_operand" "=r")
        (ior:GPI (and:GPI (match_operand:GPI 3 "register_operand" "r")
                          (match_operand:GPI 4 "const_int_operand" "n"))
                 (and:GPI (match_operand:GPI 1 "register_operand" "0")
                          (match_operand:GPI 2 "const_int_operand" "n"))))]
  "aarch64_masks_and_shift_for_bfi_p (<MODE>mode, UINTVAL (operands[2]), 0,
				      UINTVAL (operands[4]))"
  "bfi\t%<GPI:w>0, %<GPI:w>3, 0, %P4"
  [(set_attr "type" "bfm")]
)

(define_insn "*aarch64_bfxil<mode>_extr"
  [(set (match_operand:GPI 0 "register_operand" "=r")
        (ior:GPI (and:GPI (match_operand:GPI 1 "register_operand" "0")
			  (match_operand:GPI 2 "const_int_operand" "n"))
		 (zero_extract:GPI
		   (match_operand:GPI 3 "register_operand" "r")
		   (match_operand:GPI 4 "aarch64_simd_shift_imm_<mode>" "n")
		   (match_operand:GPI 5 "aarch64_simd_shift_imm_<mode>" "n"))))]
  "UINTVAL (operands[2]) == HOST_WIDE_INT_M1U << INTVAL (operands[4])
   && INTVAL (operands[4])
   && (UINTVAL (operands[4]) + UINTVAL (operands[5])
       <= GET_MODE_BITSIZE (<MODE>mode))"
  "bfxil\t%<GPI:w>0, %<GPI:w>3, %5, %4"
  [(set_attr "type" "bfm")]
)

(define_insn "*aarch64_bfxilsi_extrdi"
  [(set (match_operand:SI 0 "register_operand" "=r")
        (ior:SI (and:SI (match_operand:SI 1 "register_operand" "0")
			(match_operand:SI 2 "const_int_operand" "n"))
		(match_operator:SI 6 "subreg_lowpart_operator"
		  [(zero_extract:DI
		     (match_operand:DI 3 "register_operand" "r")
		     (match_operand:SI 4 "aarch64_simd_shift_imm_si" "n")
		     (match_operand:SI 5 "aarch64_simd_shift_imm_si" "n"))])))]
  "UINTVAL (operands[2]) == HOST_WIDE_INT_M1U << INTVAL (operands[4])
   && INTVAL (operands[4])
   && UINTVAL (operands[4]) + UINTVAL (operands[5]) <= 32"
  "bfxil\t%w0, %w3, %5, %4"
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
  [(set_attr "type" "bfx")]
)

;; XXX We should match (any_extend (ashift)) here, like (and (ashift)) below

(define_insn "*andim_ashift<mode>_bfiz"
  [(set (match_operand:GPI 0 "register_operand" "=r")
	(and:GPI (ashift:GPI (match_operand:GPI 1 "register_operand" "r")
			     (match_operand 2 "const_int_operand" "n"))
		 (match_operand 3 "const_int_operand" "n")))]
  "aarch64_mask_and_shift_for_ubfiz_p (<MODE>mode, operands[3], operands[2])"
  "ubfiz\\t%<w>0, %<w>1, %2, %P3"
  [(set_attr "type" "bfx")]
)

;; Match sbfiz pattern in a shift left + shift right operation.

(define_insn "*ashift<mode>_extv_bfiz"
  [(set (match_operand:GPI 0 "register_operand" "=r")
	(ashift:GPI (sign_extract:GPI (match_operand:GPI 1 "register_operand" "r")
				      (match_operand 2 "aarch64_simd_shift_imm_offset_<mode>" "n")
				      (const_int 0))
		     (match_operand 3 "aarch64_simd_shift_imm_<mode>" "n")))]
  "IN_RANGE (INTVAL (operands[2]) + INTVAL (operands[3]),
	     1, GET_MODE_BITSIZE (<MODE>mode) - 1)"
  "sbfiz\\t%<w>0, %<w>1, %3, %2"
  [(set_attr "type" "bfx")]
)

(define_insn "*ashiftsi_extvdi_bfiz"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(ashift:SI
	  (match_operator:SI 4 "subreg_lowpart_operator"
	    [(sign_extract:DI
	       (match_operand:DI 1 "register_operand" "r")
	       (match_operand 2 "aarch64_simd_shift_imm_offset_si")
	       (const_int 0))])
	  (match_operand 3 "aarch64_simd_shift_imm_si")))]
  "IN_RANGE (INTVAL (operands[2]) + INTVAL (operands[3]),
	     1, GET_MODE_BITSIZE (SImode) - 1)"
  "sbfiz\\t%w0, %w1, %3, %2"
  [(set_attr "type" "bfx")]
)

;; When the bit position and width of the equivalent extraction add up to 32
;; we can use a W-reg LSL instruction taking advantage of the implicit
;; zero-extension of the X-reg.
(define_split
  [(set (match_operand:DI 0 "register_operand")
	(and:DI (ashift:DI (match_operand:DI 1 "register_operand")
			     (match_operand 2 "const_int_operand"))
		 (match_operand 3 "const_int_operand")))]
 "aarch64_mask_and_shift_for_ubfiz_p (DImode, operands[3], operands[2])
  && (INTVAL (operands[2]) + popcount_hwi (INTVAL (operands[3])))
      == GET_MODE_BITSIZE (SImode)"
  [(set (match_dup 0)
	(zero_extend:DI (ashift:SI (match_dup 4) (match_dup 2))))]
  {
    operands[4] = gen_lowpart (SImode, operands[1]);
  }
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

(define_insn "@aarch64_rev16<mode>"
  [(set (match_operand:GPI 0 "register_operand" "=r")
	(unspec:GPI [(match_operand:GPI 1 "register_operand" "r")] UNSPEC_REV))]
  ""
  "rev16\\t%<w>0, %<w>1"
  [(set_attr "type" "rev")])

(define_insn "*aarch64_bfxil<mode>"
  [(set (match_operand:GPI 0 "register_operand" "=r,r")
    (ior:GPI (and:GPI (match_operand:GPI 1 "register_operand" "r,0")
		    (match_operand:GPI 3 "const_int_operand" "n, Ulc"))
	    (and:GPI (match_operand:GPI 2 "register_operand" "0,r")
		    (match_operand:GPI 4 "const_int_operand" "Ulc, n"))))]
  "(INTVAL (operands[3]) == ~INTVAL (operands[4]))
  && (aarch64_high_bits_all_ones_p (INTVAL (operands[3]))
    || aarch64_high_bits_all_ones_p (INTVAL (operands[4])))"
  {
    switch (which_alternative)
    {
      case 0:
	operands[3] = GEN_INT (ctz_hwi (~INTVAL (operands[3])));
	return "bfxil\\t%<w>0, %<w>1, 0, %3";
      case 1:
	operands[3] = GEN_INT (ctz_hwi (~INTVAL (operands[4])));
	return "bfxil\\t%<w>0, %<w>2, 0, %3";
      default:
	gcc_unreachable ();
    }
  }
  [(set_attr "type" "bfm")]
)

; Zero-extended version of above (aarch64_bfxil)
(define_insn "*aarch64_bfxilsi_uxtw"
  [(set (match_operand:DI 0 "register_operand" "=r,r")
	(zero_extend:DI (ior:SI (and:SI (match_operand:SI 1 "register_operand"
					"r,0")
		    (match_operand:SI 3 "const_int_operand" "n, Ulc"))
	    (and:SI (match_operand:SI 2 "register_operand" "0,r")
		    (match_operand:SI 4 "const_int_operand" "Ulc, n")))))]
  "(INTVAL (operands[3]) == ~INTVAL (operands[4]))
  && (aarch64_high_bits_all_ones_p (INTVAL (operands[3]))
    || aarch64_high_bits_all_ones_p (INTVAL (operands[4])))"
  {
    switch (which_alternative)
    {
      case 0:
	operands[3] = GEN_INT (ctz_hwi (~INTVAL (operands[3])));
	return "bfxil\\t%w0, %w1, 0, %3";
      case 1:
	operands[3] = GEN_INT (ctz_hwi (~INTVAL (operands[4])));
	return "bfxil\\t%w0, %w2, 0, %3";
      default:
	gcc_unreachable ();
    }
  }
  [(set_attr "type" "bfm")]
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
;; Expands to btrunc, ceil, floor, nearbyint, rint, round, roundeven.

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

;; fma - expand fma into patterns with the accumulator operand first since
;; reusing the accumulator results in better register allocation.
;; The register allocator considers copy preferences in operand order,
;; so this prefers fmadd s0, s1, s2, s0 over fmadd s1, s1, s2, s0.

(define_expand "fma<mode>4"
  [(set (match_operand:GPF_F16 0 "register_operand")
	(fma:GPF_F16 (match_operand:GPF_F16 1 "register_operand")
		     (match_operand:GPF_F16 2 "register_operand")
		     (match_operand:GPF_F16 3 "register_operand")))]
  "TARGET_FLOAT"
)

(define_insn "*aarch64_fma<mode>4"
  [(set (match_operand:GPF_F16 0 "register_operand" "=w")
	(fma:GPF_F16 (match_operand:GPF_F16 2 "register_operand" "w")
		     (match_operand:GPF_F16 3 "register_operand" "w")
		     (match_operand:GPF_F16 1 "register_operand" "w")))]
  "TARGET_FLOAT"
  "fmadd\\t%<s>0, %<s>2, %<s>3, %<s>1"
  [(set_attr "type" "fmac<stype>")]
)

(define_expand "fnma<mode>4"
  [(set (match_operand:GPF_F16 0 "register_operand")
	(fma:GPF_F16
	  (neg:GPF_F16 (match_operand:GPF_F16 1 "register_operand"))
	  (match_operand:GPF_F16 2 "register_operand")
	  (match_operand:GPF_F16 3 "register_operand")))]
  "TARGET_FLOAT"
)

(define_insn "*aarch64_fnma<mode>4"
  [(set (match_operand:GPF_F16 0 "register_operand" "=w")
	(fma:GPF_F16
	  (neg:GPF_F16 (match_operand:GPF_F16 2 "register_operand" "w"))
	  (match_operand:GPF_F16 3 "register_operand" "w")
	  (match_operand:GPF_F16 1 "register_operand" "w")))]
  "TARGET_FLOAT"
  "fmsub\\t%<s>0, %<s>2, %<s>3, %<s>1"
  [(set_attr "type" "fmac<stype>")]
)


(define_expand "fms<mode>4"
  [(set (match_operand:GPF 0 "register_operand")
	(fma:GPF (match_operand:GPF 1 "register_operand")
		 (match_operand:GPF 2 "register_operand")
		 (neg:GPF (match_operand:GPF 3 "register_operand"))))]
  "TARGET_FLOAT"
)

(define_insn "*aarch64_fms<mode>4"
  [(set (match_operand:GPF 0 "register_operand" "=w")
	(fma:GPF (match_operand:GPF 2 "register_operand" "w")
		 (match_operand:GPF 3 "register_operand" "w")
		 (neg:GPF (match_operand:GPF 1 "register_operand" "w"))))]
  "TARGET_FLOAT"
  "fnmsub\\t%<s>0, %<s>2, %<s>3, %<s>1"
  [(set_attr "type" "fmac<s>")]
)

(define_expand "fnms<mode>4"
  [(set (match_operand:GPF 0 "register_operand")
	(fma:GPF (neg:GPF (match_operand:GPF 1 "register_operand"))
		 (match_operand:GPF 2 "register_operand")
		 (neg:GPF (match_operand:GPF 3 "register_operand"))))]
  "TARGET_FLOAT"
)

(define_insn "*aarch64_fnms<mode>4"
  [(set (match_operand:GPF 0 "register_operand" "=w")
	(fma:GPF (neg:GPF (match_operand:GPF 2 "register_operand" "w"))
		 (match_operand:GPF 3 "register_operand" "w")
		 (neg:GPF (match_operand:GPF 1 "register_operand" "w"))))]
  "TARGET_FLOAT"
  "fnmadd\\t%<s>0, %<s>2, %<s>3, %<s>1"
  [(set_attr "type" "fmac<s>")]
)

;; If signed zeros are ignored, -(a * b + c) = -a * b - c.
(define_insn "*aarch64_fnmadd<mode>4"
  [(set (match_operand:GPF 0 "register_operand" "=w")
	(neg:GPF (fma:GPF (match_operand:GPF 2 "register_operand" "w")
			  (match_operand:GPF 3 "register_operand" "w")
			  (match_operand:GPF 1 "register_operand" "w"))))]
  "!HONOR_SIGNED_ZEROS (<MODE>mode) && TARGET_FLOAT"
  "fnmadd\\t%<s>0, %<s>2, %<s>3, %<s>1"
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

;; Convert SF -> SI or DF -> DI while preferring w = w register constraints
;; and making r = w more expensive

(define_insn "<optab>_trunc<fcvt_target><GPI:mode>2"
  [(set (match_operand:GPI 0 "register_operand" "=w,?r")
	(FIXUORS:GPI (match_operand:<FCVT_TARGET> 1 "register_operand" "w,w")))]
  "TARGET_FLOAT"
  "@
   fcvtz<su>\t%<s>0, %<s>1
   fcvtz<su>\t%<w>0, %<s>1"
  [(set_attr "type" "neon_fp_to_int_s,f_cvtf2i")
   (set_attr "arch" "simd,fp")]
)

;; Convert HF -> SI or DI

(define_insn "<optab>_trunchf<GPI:mode>2"
  [(set (match_operand:GPI 0 "register_operand" "=r")
	(FIXUORS:GPI (match_operand:HF 1 "register_operand" "w")))]
  "TARGET_FP_F16INST"
  "fcvtz<su>\t%<w>0, %h1"
  [(set_attr "type" "f_cvtf2i")]
)

;; Convert DF -> SI or SF -> DI which can only be accomplished with
;; input in a fp register and output in a integer register

(define_insn "<optab>_trunc<fcvt_change_mode><GPI:mode>2"
  [(set (match_operand:GPI 0 "register_operand" "=r")
	(FIXUORS:GPI (match_operand:<FCVT_CHANGE_MODE> 1 "register_operand" "w")))]
  "TARGET_FLOAT"
  "fcvtz<su>\t%<w>0, %<fpw>1"
  [(set_attr "type" "f_cvtf2i")]
)

(define_insn "*fix_to_zero_extend<mode>di2"
  [(set (match_operand:DI 0 "register_operand" "=r")
	(zero_extend:DI
	 (unsigned_fix:SI
	  (match_operand:GPF 1 "register_operand" "w"))))]
  "TARGET_FLOAT"
  "fcvtzu\t%w0, %<s>1"
  [(set_attr "type" "f_cvtf2i")]
)

;; Equal width integer to fp and multiply combine.
(define_insn "*aarch64_<su_optab>cvtf<fcvt_target><GPF:mode>2_mult"
  [(set (match_operand:GPF 0 "register_operand" "=w,w")
	(mult:GPF (FLOATUORS:GPF
		   (match_operand:<FCVT_TARGET> 1 "register_operand" "w,?r"))
		   (match_operand:GPF 2 "aarch64_fp_pow2_recip" "Dt,Dt")))]
  "TARGET_FLOAT"
  {
    operands[2] = GEN_INT (aarch64_fpconst_pow2_recip (operands[2]));
    switch (which_alternative)
    {
      case 0:
	return "<su_optab>cvtf\t%<GPF:s>0, %<s>1, #%2";
      case 1:
	return "<su_optab>cvtf\t%<GPF:s>0, %<w1>1, #%2";
      default:
	gcc_unreachable ();
    }
  }
  [(set_attr "type" "neon_int_to_fp_<Vetype>,f_cvti2f")
   (set_attr "arch" "simd,fp")]
)

;; Unequal width integer to fp and multiply combine.
(define_insn "*aarch64_<su_optab>cvtf<fcvt_iesize><GPF:mode>2_mult"
  [(set (match_operand:GPF 0 "register_operand" "=w")
	(mult:GPF (FLOATUORS:GPF
		   (match_operand:<FCVT_IESIZE> 1 "register_operand" "r"))
		   (match_operand:GPF 2 "aarch64_fp_pow2_recip" "Dt")))]
  "TARGET_FLOAT"
  {
    operands[2] = GEN_INT (aarch64_fpconst_pow2_recip (operands[2]));
    return "<su_optab>cvtf\t%<GPF:s>0, %<w2>1, #%2";
  }
  [(set_attr "type" "f_cvti2f")]
)

;; Equal width integer to fp conversion.
(define_insn "<optab><fcvt_target><GPF:mode>2"
  [(set (match_operand:GPF 0 "register_operand" "=w,w")
        (FLOATUORS:GPF (match_operand:<FCVT_TARGET> 1 "register_operand" "w,?r")))]
  "TARGET_FLOAT"
  "@
   <su_optab>cvtf\t%<GPF:s>0, %<s>1
   <su_optab>cvtf\t%<GPF:s>0, %<w1>1"
  [(set_attr "type" "neon_int_to_fp_<Vetype>,f_cvti2f")
   (set_attr "arch" "simd,fp")]
)

;; Unequal width integer to fp conversions.
(define_insn "<optab><fcvt_iesize><GPF:mode>2"
  [(set (match_operand:GPF 0 "register_operand" "=w")
        (FLOATUORS:GPF (match_operand:<FCVT_IESIZE> 1 "register_operand" "r")))]
  "TARGET_FLOAT"
  "<su_optab>cvtf\t%<GPF:s>0, %<w2>1"
  [(set_attr "type" "f_cvti2f")]
)

;; If we do not have ARMv8.2-A 16-bit floating point extensions, the
;; midend will arrange for an SImode conversion to HFmode to first go
;; through DFmode, then to HFmode.  But first it will try converting
;; to DImode then down, which would match our DImode pattern below and
;; give very poor code-generation.  So, we must provide our own emulation
;; of the mid-end logic.

(define_insn "aarch64_fp16_<optab><mode>hf2"
  [(set (match_operand:HF 0 "register_operand" "=w")
	(FLOATUORS:HF (match_operand:GPI 1 "register_operand" "r")))]
  "TARGET_FP_F16INST"
  "<su_optab>cvtf\t%h0, %<w>1"
  [(set_attr "type" "f_cvti2f")]
)

(define_expand "<optab>sihf2"
  [(set (match_operand:HF 0 "register_operand")
	(FLOATUORS:HF (match_operand:SI 1 "register_operand")))]
  "TARGET_FLOAT"
{
  if (TARGET_FP_F16INST)
    emit_insn (gen_aarch64_fp16_<optab>sihf2 (operands[0], operands[1]));
  else
    {
      rtx convert_target = gen_reg_rtx (DFmode);
      emit_insn (gen_<optab>sidf2 (convert_target, operands[1]));
      emit_insn (gen_truncdfhf2 (operands[0], convert_target));
    }
  DONE;
}
)

;; For DImode there is no wide enough floating-point mode that we
;; can convert through natively (TFmode would work, but requires a library
;; call).  However, we know that any value >= 65504 will be rounded
;; to infinity on conversion.  This is well within the range of SImode, so
;; we can:
;;   Saturate to SImode.
;;   Convert from that to DFmode
;;   Convert from that to HFmode (phew!).
;; Note that the saturation to SImode requires the SIMD extensions.  If
;; we ever need to provide this pattern where the SIMD extensions are not
;; available, we would need a different approach.

(define_expand "<optab>dihf2"
  [(set (match_operand:HF 0 "register_operand")
	(FLOATUORS:HF (match_operand:DI 1 "register_operand")))]
  "TARGET_FP_F16INST || TARGET_SIMD"
{
  if (TARGET_FP_F16INST)
    emit_insn (gen_aarch64_fp16_<optab>dihf2 (operands[0], operands[1]));
  else
    {
      rtx sat_target = gen_reg_rtx (SImode);
      emit_insn (gen_aarch64_<su_optab>qmovndi (sat_target, operands[1]));
      emit_insn (gen_<optab>sihf2 (operands[0], sat_target));
    }

  DONE;
}
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
   (set_attr "arch" "fp,simd")]
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
   (set_attr "arch" "fp,simd")]
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
 "TARGET_FLOAT"
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
  [(set (match_operand:GPF_F16 0 "register_operand")
	(sqrt:GPF_F16 (match_operand:GPF_F16 1 "register_operand")))]
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

;; Scalar forms for fmax, fmin, fmaxnm, fminnm.
;; fmaxnm and fminnm are used for the fmax<mode>3 standard pattern names,
;; which implement the IEEE fmax ()/fmin () functions.
(define_insn "<fmaxmin><mode>3"
  [(set (match_operand:GPF_F16 0 "register_operand" "=w")
	(unspec:GPF_F16 [(match_operand:GPF_F16 1 "register_operand" "w")
		     (match_operand:GPF_F16 2 "register_operand" "w")]
		     FMAXMIN_UNS))]
  "TARGET_FLOAT"
  "<maxmin_uns_op>\\t%<s>0, %<s>1, %<s>2"
  [(set_attr "type" "f_minmax<stype>")]
)

(define_expand "lrint<GPF:mode><GPI:mode>2"
  [(match_operand:GPI 0 "register_operand")
   (match_operand:GPF 1 "register_operand")]
  "TARGET_FLOAT
   && ((GET_MODE_BITSIZE (<GPF:MODE>mode) <= LONG_TYPE_SIZE)
   || !flag_trapping_math || flag_fp_int_builtin_inexact)"
{
  rtx cvt = gen_reg_rtx (<GPF:MODE>mode);
  emit_insn (gen_rint<GPF:mode>2 (cvt, operands[1]));
  emit_insn (gen_lbtrunc<GPF:mode><GPI:mode>2 (operands[0], cvt));
  DONE;
}
)

;; For copysign (x, y), we want to generate:
;;
;;   LDR d2, #(1 << 63)
;;   BSL v2.8b, [y], [x]
;;
;; or another, equivalent, sequence using one of BSL/BIT/BIF.  Because
;; we expect these operations to nearly always operate on
;; floating-point values, we do not want the operation to be
;; simplified into a bit-field insert operation that operates on the
;; integer side, since typically that would involve three inter-bank
;; register copies.  As we do not expect copysign to be followed by
;; other logical operations on the result, it seems preferable to keep
;; this as an unspec operation, rather than exposing the underlying
;; logic to the compiler.

(define_expand "copysign<GPF:mode>3"
  [(match_operand:GPF 0 "register_operand")
   (match_operand:GPF 1 "register_operand")
   (match_operand:GPF 2 "register_operand")]
  "TARGET_SIMD"
{
  rtx bitmask = gen_reg_rtx (<V_INT_EQUIV>mode);
  emit_move_insn (bitmask, GEN_INT (HOST_WIDE_INT_M1U
				    << (GET_MODE_BITSIZE (<MODE>mode) - 1)));
  emit_insn (gen_copysign<mode>3_insn (operands[0], operands[1], operands[2],
				       bitmask));
  DONE;
}
)

(define_insn "copysign<GPF:mode>3_insn"
  [(set (match_operand:GPF 0 "register_operand" "=w,w,w,r")
	(unspec:GPF [(match_operand:GPF 1 "register_operand" "w,0,w,r")
		     (match_operand:GPF 2 "register_operand" "w,w,0,0")
		     (match_operand:<V_INT_EQUIV> 3 "register_operand" "0,w,w,X")]
	 UNSPEC_COPYSIGN))]
  "TARGET_SIMD"
  "@
   bsl\\t%0.<Vbtype>, %2.<Vbtype>, %1.<Vbtype>
   bit\\t%0.<Vbtype>, %2.<Vbtype>, %3.<Vbtype>
   bif\\t%0.<Vbtype>, %1.<Vbtype>, %3.<Vbtype>
   bfxil\\t%<w1>0, %<w1>1, #0, <sizem1>"
  [(set_attr "type" "neon_bsl<q>,neon_bsl<q>,neon_bsl<q>,bfm")]
)


;; For xorsign (x, y), we want to generate:
;;
;; LDR   d2, #1<<63
;; AND   v3.8B, v1.8B, v2.8B
;; EOR   v0.8B, v0.8B, v3.8B
;;

(define_expand "xorsign<mode>3"
  [(match_operand:GPF 0 "register_operand")
   (match_operand:GPF 1 "register_operand")
   (match_operand:GPF 2 "register_operand")]
  "TARGET_SIMD"
{

  machine_mode imode = <V_INT_EQUIV>mode;
  rtx mask = gen_reg_rtx (imode);
  rtx op1x = gen_reg_rtx (imode);
  rtx op2x = gen_reg_rtx (imode);

  int bits = GET_MODE_BITSIZE (<MODE>mode) - 1;
  emit_move_insn (mask, GEN_INT (trunc_int_for_mode (HOST_WIDE_INT_M1U << bits,
						     imode)));

  emit_insn (gen_and<v_int_equiv>3 (op2x, mask,
				    lowpart_subreg (imode, operands[2],
						    <MODE>mode)));
  emit_insn (gen_xor<v_int_equiv>3 (op1x,
				    lowpart_subreg (imode, operands[1],
						    <MODE>mode),
				    op2x));
  emit_move_insn (operands[0],
		  lowpart_subreg (<MODE>mode, op1x, imode));
  DONE;
}
)

;; -------------------------------------------------------------------
;; Reload support
;; -------------------------------------------------------------------
;; Reload Scalar Floating point modes from constant pool.
;; The AArch64 port doesn't have __int128 constant move support.
;; The patterns need constraints due to TARGET_SECONDARY_RELOAD hook.
(define_expand "@aarch64_reload_movcp<GPF_TF:mode><P:mode>"
 [(set (match_operand:GPF_TF 0 "register_operand" "=w")
       (mem:GPF_TF (match_operand 1 "aarch64_constant_pool_symref" "S")))
  (clobber (match_operand:P 2 "register_operand" "=&r"))]
 "TARGET_FLOAT"
 {
   aarch64_expand_mov_immediate (operands[2], XEXP (operands[1], 0));
   emit_move_insn (operands[0], gen_rtx_MEM (<GPF_TF:MODE>mode, operands[2]));
   DONE;
 }
)

;; Reload Vector modes from constant pool.
(define_expand "@aarch64_reload_movcp<VALL:mode><P:mode>"
 [(set (match_operand:VALL 0 "register_operand" "=w")
       (mem:VALL (match_operand 1 "aarch64_constant_pool_symref" "S")))
  (clobber (match_operand:P 2 "register_operand" "=&r"))]
 "TARGET_FLOAT"
 {
   aarch64_expand_mov_immediate (operands[2], XEXP (operands[1], 0));
   emit_move_insn (operands[0], gen_rtx_MEM (<VALL:MODE>mode, operands[2]));
   DONE;
 }
)

(define_expand "@aarch64_reload_mov<mode>"
  [(set (match_operand:VTX 0 "register_operand" "=w")
        (match_operand:VTX 1 "register_operand" "w"))
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

(define_insn "@aarch64_movdi_<mode>low"
  [(set (match_operand:DI 0 "register_operand" "=r")
	(zero_extract:DI (match_operand:TX 1 "register_operand" "w")
			 (const_int 64) (const_int 0)))]
  "TARGET_FLOAT && (reload_completed || reload_in_progress)"
  "fmov\\t%x0, %d1"
  [(set_attr "type" "f_mrc")
   (set_attr "length" "4")
  ])

(define_insn "@aarch64_movdi_<mode>high"
  [(set (match_operand:DI 0 "register_operand" "=r")
	(zero_extract:DI (match_operand:TX 1 "register_operand" "w")
			 (const_int 64) (const_int 64)))]
  "TARGET_FLOAT && (reload_completed || reload_in_progress)"
  "fmov\\t%x0, %1.d[1]"
  [(set_attr "type" "f_mrc")
   (set_attr "length" "4")
  ])

(define_insn "@aarch64_mov<mode>high_di"
  [(set (zero_extract:TX (match_operand:TX 0 "register_operand" "+w")
                         (const_int 64) (const_int 64))
        (zero_extend:TX (match_operand:DI 1 "register_operand" "r")))]
  "TARGET_FLOAT && (reload_completed || reload_in_progress)"
  "fmov\\t%0.d[1], %x1"
  [(set_attr "type" "f_mcr")
   (set_attr "length" "4")
  ])

(define_insn "@aarch64_mov<mode>low_di"
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
  [(set (match_operand 0 "register_operand")
	(lo_sum (match_operand 1 "register_operand")
		(match_operand 2 "aarch64_valid_symref")))]
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
  "add\\t%<w>0, %<w>1, :lo12:%c2"
  [(set_attr "type" "alu_imm")]
)

(define_insn "ldr_got_small_28k_<mode>"
  [(set (match_operand:PTR 0 "register_operand" "=r")
	(unspec:PTR [(mem:PTR (lo_sum:PTR
			      (match_operand:PTR 1 "register_operand" "r")
			      (match_operand:PTR 2 "aarch64_valid_symref" "S")))]
		    UNSPEC_GOTSMALLPIC28K))]
  ""
  "ldr\\t%<w>0, [%1, #:<got_modifier>:%c2]"
  [(set_attr "type" "load_<ldst_sz>")]
)

(define_insn "ldr_got_small_28k_sidi"
  [(set (match_operand:DI 0 "register_operand" "=r")
	(zero_extend:DI
	 (unspec:SI [(mem:SI (lo_sum:DI
			     (match_operand:DI 1 "register_operand" "r")
			     (match_operand:DI 2 "aarch64_valid_symref" "S")))]
		    UNSPEC_GOTSMALLPIC28K)))]
  "TARGET_ILP32"
  "ldr\\t%w0, [%1, #:gotpage_lo14:%c2]"
  [(set_attr "type" "load_4")]
)

(define_insn "@ldr_got_tiny_<mode>"
  [(set (match_operand:PTR 0 "register_operand" "=r")
	(unspec:PTR [(match_operand:PTR 1 "aarch64_valid_symref" "S")]
		    UNSPEC_GOTTINYPIC))]
  ""
  "ldr\t%<w>0, %L1"
  [(set_attr "type" "load_<ldst_sz>")]
)

(define_insn "ldr_got_tiny_sidi"
  [(set (match_operand:DI 0 "register_operand" "=r")
	(zero_extend:DI
	  (unspec:SI [(match_operand:DI 1 "aarch64_valid_symref" "S")]
		     UNSPEC_GOTTINYPIC)))]
  "TARGET_ILP32"
  "ldr\t%w0, %L1"
  [(set_attr "type" "load_4")]
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
(define_expand "tlsgd_small_<mode>"
 [(parallel [(set (match_operand:PTR 0 "register_operand")
                  (call (mem:DI (match_dup 2)) (const_int 1)))
	     (unspec:DI [(const_int 0)] UNSPEC_CALLEE_ABI)
	     (unspec:DI [(match_operand 1 "aarch64_valid_symref")] UNSPEC_GOTSMALLTLS)
	     (clobber (reg:DI LR_REGNUM))])]
 ""
{
  operands[2] = aarch64_tls_get_addr ();
})

(define_insn "*tlsgd_small_<mode>"
  [(set (match_operand:PTR 0 "register_operand" "")
	(call (mem:DI (match_operand:DI 2 "" "")) (const_int 1)))
   (unspec:DI [(const_int 0)] UNSPEC_CALLEE_ABI)
   (unspec:DI [(match_operand 1 "aarch64_valid_symref" "S")] UNSPEC_GOTSMALLTLS)
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
  [(set_attr "type" "load_4")
   (set_attr "length" "8")]
)

(define_insn "tlsie_small_sidi"
  [(set (match_operand:DI 0 "register_operand" "=r")
	(zero_extend:DI
          (unspec:SI [(match_operand 1 "aarch64_tls_ie_symref" "S")]
		      UNSPEC_GOTSMALLTLS)))]
  ""
  "adrp\\t%0, %A1\;ldr\\t%w0, [%0, #%L1]"
  [(set_attr "type" "load_4")
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

(define_expand "tlsdesc_small_<mode>"
  [(unspec:PTR [(match_operand 0 "aarch64_valid_symref")] UNSPEC_TLSDESC)]
  "TARGET_TLS_DESC"
  {
    if (TARGET_SVE)
      {
	rtx abi = gen_int_mode (aarch64_tlsdesc_abi_id (), DImode);
	rtx_insn *call
	  = emit_call_insn (gen_tlsdesc_small_sve_<mode> (operands[0], abi));
	RTL_CONST_CALL_P (call) = 1;
      }
    else
      emit_insn (gen_tlsdesc_small_advsimd_<mode> (operands[0]));
    DONE;
  }
)

;; tlsdesc calls preserve all core and Advanced SIMD registers except
;; R0 and LR.
(define_insn "tlsdesc_small_advsimd_<mode>"
  [(set (reg:PTR R0_REGNUM)
        (unspec:PTR [(match_operand 0 "aarch64_valid_symref" "S")]
		    UNSPEC_TLSDESC))
   (clobber (reg:DI LR_REGNUM))
   (clobber (reg:CC CC_REGNUM))
   (clobber (match_scratch:DI 1 "=r"))
   (use (reg:DI FP_REGNUM))]
  "TARGET_TLS_DESC && !TARGET_SVE"
  "adrp\\tx0, %A0\;ldr\\t%<w>1, [x0, #%L0]\;add\\t<w>0, <w>0, %L0\;.tlsdesccall\\t%0\;blr\\t%1"
  [(set_attr "type" "call")
   (set_attr "length" "16")])

;; For SVE, model tlsdesc calls as normal calls, with the callee ABI
;; describing the extra call-preserved guarantees.  This would work
;; for non-SVE too, but avoiding a call is probably better if we can.
(define_insn "tlsdesc_small_sve_<mode>"
  [(set (reg:PTR R0_REGNUM)
	(call (mem:DI (unspec:PTR
			[(match_operand 0 "aarch64_valid_symref")]
			UNSPEC_TLSDESC))
	      (const_int 0)))
   (unspec:DI [(match_operand:DI 1 "const_int_operand")] UNSPEC_CALLEE_ABI)
   (clobber (reg:DI LR_REGNUM))]
  "TARGET_TLS_DESC && TARGET_SVE"
  "adrp\\tx0, %A0\;ldr\\t<w>30, [x0, #%L0]\;add\\t<w>0, <w>0, %L0\;.tlsdesccall\\t%0\;blr\\tx30"
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

(define_insn "aarch64_fjcvtzs"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(unspec:SI [(match_operand:DF 1 "register_operand" "w")]
		   UNSPEC_FJCVTZS))
   (clobber (reg:CC CC_REGNUM))]
  "TARGET_JSCVT"
  "fjcvtzs\\t%w0, %d1"
  [(set_attr "type" "f_cvtf2i")]
)

;; Pointer authentication patterns are always provided.  In architecture
;; revisions prior to ARMv8.3-A these HINT instructions operate as NOPs.
;; This lets the user write portable software which authenticates pointers
;; when run on something which implements ARMv8.3-A, and which runs
;; correctly, but does not authenticate pointers, where ARMv8.3-A is not
;; implemented.

;; Signing/Authenticating R30 using SP as the salt.

(define_insn "<pauth_mnem_prefix>sp"
  [(set (reg:DI R30_REGNUM)
	(unspec:DI [(reg:DI R30_REGNUM) (reg:DI SP_REGNUM)] PAUTH_LR_SP))]
  ""
  "hint\t<pauth_hint_num> // <pauth_mnem_prefix>sp";
)

;; Signing/Authenticating X17 using X16 as the salt.

(define_insn "<pauth_mnem_prefix>1716"
  [(set (reg:DI R17_REGNUM)
	(unspec:DI [(reg:DI R17_REGNUM) (reg:DI R16_REGNUM)] PAUTH_17_16))]
  ""
  "hint\t<pauth_hint_num> // <pauth_mnem_prefix>1716";
)

;; Stripping the signature in R30.

(define_insn "xpaclri"
  [(set (reg:DI R30_REGNUM) (unspec:DI [(reg:DI R30_REGNUM)] UNSPEC_XPACLRI))]
  ""
  "hint\t7 // xpaclri"
)

;; Save X30 in the X18-based POST_INC stack (consistent with clang).
(define_expand "scs_push"
  [(set (mem:DI (post_inc:DI (reg:DI R18_REGNUM)))
	(reg:DI R30_REGNUM))])

;; Load X30 form the X18-based PRE_DEC stack (consistent with clang).
(define_expand "scs_pop"
  [(set (reg:DI R30_REGNUM)
	(mem:DI (pre_dec:DI (reg:DI R18_REGNUM))))])

;; UNSPEC_VOLATILE is considered to use and clobber all hard registers and
;; all of memory.  This blocks insns from being moved across this point.

(define_insn "blockage"
  [(unspec_volatile [(const_int 0)] UNSPECV_BLOCKAGE)]
  ""
  ""
  [(set_attr "length" "0")
   (set_attr "type" "block")]
)

(define_insn "probe_stack_range"
  [(set (match_operand:DI 0 "register_operand" "=rk")
	(unspec_volatile:DI [(match_operand:DI 1 "register_operand" "0")
			     (match_operand:DI 2 "register_operand" "r")]
			      UNSPECV_PROBE_STACK_RANGE))]
  ""
{
  return aarch64_output_probe_stack_range (operands[0], operands[2]);
}
  [(set_attr "length" "32")]
)

;; This instruction is used to generate the stack clash stack adjustment and
;; probing loop.  We can't change the control flow during prologue and epilogue
;; code generation.  So we must emit a volatile unspec and expand it later on.

(define_insn "@probe_sve_stack_clash_<mode>"
  [(set (match_operand:P 0 "register_operand" "=rk")
	(unspec_volatile:P [(match_operand:P 1 "register_operand" "0")
			    (match_operand:P 2 "register_operand" "r")
			    (match_operand:P 3 "const_int_operand" "n")
			    (match_operand:P 4 "aarch64_plus_immediate" "L")]
			     UNSPECV_PROBE_STACK_RANGE))]
  "TARGET_SVE"
{
  return aarch64_output_probe_sve_stack_clash (operands[0], operands[2],
					       operands[3], operands[4]);
}
  [(set_attr "length" "28")]
)

;; Named pattern for expanding thread pointer reference.
(define_expand "get_thread_pointerdi"
  [(match_operand:DI 0 "register_operand")]
  ""
{
  rtx tmp = aarch64_load_tp (operands[0]);
  if (tmp != operands[0])
    emit_move_insn (operands[0], tmp);
  DONE;
})

;; Defined for -mstack-protector-guard=sysreg, which goes through this
;; pattern rather than stack_protect_combined_set.  Our implementation
;; of the latter can handle both.
(define_expand "stack_protect_set"
  [(match_operand 0 "memory_operand")
   (match_operand 1 "")]
  ""
{
  emit_insn (gen_stack_protect_combined_set (operands[0], operands[1]));
  DONE;
})

(define_expand "stack_protect_combined_set"
  [(match_operand 0 "memory_operand")
   (match_operand 1 "")]
  ""
{
  machine_mode mode = GET_MODE (operands[0]);
  operands[1] = aarch64_stack_protect_canary_mem (mode, operands[1],
						  AARCH64_SALT_SSP_SET);
  emit_insn ((mode == DImode
	      ? gen_stack_protect_set_di
	      : gen_stack_protect_set_si) (operands[0], operands[1]));
  DONE;
})

;; Operand 1 is either AARCH64_SALT_SSP_SET or AARCH64_SALT_SSP_TEST.
(define_insn "reg_stack_protect_address_<mode>"
 [(set (match_operand:PTR 0 "register_operand" "=r")
       (unspec:PTR [(match_operand 1 "const_int_operand")]
		   UNSPEC_SSP_SYSREG))]
 "aarch64_stack_protector_guard != SSP_GLOBAL"
 {
   char buf[150];
   snprintf (buf, 150, "mrs\\t%%<w>0, %s",
	    aarch64_stack_protector_guard_reg_str);
   output_asm_insn (buf, operands);
   return "";
 }
 [(set_attr "type" "mrs")])

;; DO NOT SPLIT THIS PATTERN.  It is important for security reasons that the
;; canary value does not live beyond the life of this sequence.
(define_insn "stack_protect_set_<mode>"
  [(set (match_operand:PTR 0 "memory_operand" "=m")
	(unspec:PTR [(match_operand:PTR 1 "memory_operand" "m")]
	 UNSPEC_SP_SET))
   (set (match_scratch:PTR 2 "=&r") (const_int 0))]
  ""
  "ldr\\t%<w>2, %1\;str\\t%<w>2, %0\;mov\t%<w>2, 0"
  [(set_attr "length" "12")
   (set_attr "type" "multiple")])

;; Defined for -mstack-protector-guard=sysreg, which goes through this
;; pattern rather than stack_protect_combined_test.  Our implementation
;; of the latter can handle both.
(define_expand "stack_protect_test"
  [(match_operand 0 "memory_operand")
   (match_operand 1 "")
   (match_operand 2)]
  ""
{
  emit_insn (gen_stack_protect_combined_test (operands[0], operands[1],
					      operands[2]));
  DONE;
})

(define_expand "stack_protect_combined_test"
  [(match_operand 0 "memory_operand")
   (match_operand 1 "")
   (match_operand 2)]
  ""
{
  machine_mode mode = GET_MODE (operands[0]);
  operands[1] = aarch64_stack_protect_canary_mem (mode, operands[1],
						  AARCH64_SALT_SSP_TEST);
  emit_insn ((mode == DImode
	     ? gen_stack_protect_test_di
	     : gen_stack_protect_test_si) (operands[0], operands[1]));

  rtx cc_reg = gen_rtx_REG (CCmode, CC_REGNUM);
  emit_jump_insn (gen_condjump (gen_rtx_EQ (VOIDmode, cc_reg, const0_rtx),
				cc_reg, operands[2]));
  DONE;
})

;; DO NOT SPLIT THIS PATTERN.  It is important for security reasons that the
;; canary value does not live beyond the end of this sequence.
(define_insn "stack_protect_test_<mode>"
  [(set (reg:CC CC_REGNUM)
	(unspec:CC [(match_operand:PTR 0 "memory_operand" "m")
		    (match_operand:PTR 1 "memory_operand" "m")]
		   UNSPEC_SP_TEST))
   (clobber (match_scratch:PTR 2 "=&r"))
   (clobber (match_scratch:PTR 3 "=&r"))]
  ""
  "ldr\t%<w>2, %0\;ldr\t%<w>3, %1\;subs\t%<w>2, %<w>2, %<w>3\;mov\t%3, 0"
  [(set_attr "length" "16")
   (set_attr "type" "multiple")])

;; Write into the Floating-point Status or Control Register.
(define_insn "@aarch64_set_<fpscr_name><GPI:mode>"
  [(unspec_volatile [(match_operand:GPI 0 "register_operand" "r")] SET_FPSCR)]
  ""
  "msr\\t<fpscr_name>, %0"
  [(set_attr "type" "mrs")])

;; Read into the Floating-point Status or Control Register.
(define_insn "@aarch64_get_<fpscr_name><GPI:mode>"
  [(set (match_operand:GPI 0 "register_operand" "=r")
        (unspec_volatile:GPI [(const_int 0)] GET_FPSCR))]
  ""
  "mrs\\t%0, <fpscr_name>"
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

;; Track speculation through conditional branches.  We assume that
;; SPECULATION_TRACKER_REGNUM is reserved for this purpose when necessary.
(define_insn "speculation_tracker"
  [(set (reg:DI SPECULATION_TRACKER_REGNUM)
	(unspec:DI [(reg:DI SPECULATION_TRACKER_REGNUM) (match_operand 0)]
	 UNSPEC_SPECULATION_TRACKER))]
  ""
  {
    operands[1] = gen_rtx_REG (DImode, SPECULATION_TRACKER_REGNUM);
    output_asm_insn ("csel\\t%1, %1, xzr, %m0", operands);
    return "";
  }
  [(set_attr "type" "csel")]
)

;; Like speculation_tracker, but track the inverse condition.
(define_insn "speculation_tracker_rev"
  [(set (reg:DI SPECULATION_TRACKER_REGNUM)
	(unspec:DI [(reg:DI SPECULATION_TRACKER_REGNUM) (match_operand 0)]
	 UNSPEC_SPECULATION_TRACKER_REV))]
  ""
  {
    operands[1] = gen_rtx_REG (DImode, SPECULATION_TRACKER_REGNUM);
    output_asm_insn ("csel\\t%1, %1, xzr, %M0", operands);
    return "";
  }
  [(set_attr "type" "csel")]
)

;; BTI <target> instructions
(define_insn "bti_noarg"
  [(unspec_volatile [(const_int 0)] UNSPECV_BTI_NOARG)]
  ""
  "hint\t32 // bti"
  [(set_attr "type" "no_insn")]
)

(define_insn "bti_c"
  [(unspec_volatile [(const_int 0)] UNSPECV_BTI_C)]
  ""
  "hint\t34 // bti c"
  [(set_attr "type" "no_insn")]
)

(define_insn "bti_j"
  [(unspec_volatile [(const_int 0)] UNSPECV_BTI_J)]
  ""
  "hint\t36 // bti j"
  [(set_attr "type" "no_insn")]
)

(define_insn "bti_jc"
  [(unspec_volatile [(const_int 0)] UNSPECV_BTI_JC)]
  ""
  "hint\t38 // bti jc"
  [(set_attr "type" "no_insn")]
)

;; Hard speculation barrier.
(define_insn "speculation_barrier"
  [(unspec_volatile [(const_int 0)] UNSPECV_SPECULATION_BARRIER)]
  ""
  "isb\;dsb\\tsy"
  [(set_attr "length" "8")
   (set_attr "type" "block")
   (set_attr "speculation_barrier" "true")]
)

;; Support for __builtin_speculation_safe_value when we have speculation
;; tracking enabled.  Use the speculation tracker to decide whether to
;; copy operand 1 to the target, or to copy the fail value (operand 2).
(define_expand "@despeculate_copy<ALLI_TI:mode>"
  [(set (match_operand:ALLI_TI 0 "register_operand")
	(unspec_volatile:ALLI_TI
	 [(match_operand:ALLI_TI 1 "register_operand")
	  (match_operand:ALLI_TI 2 "aarch64_reg_or_zero")
	  (use (reg:DI SPECULATION_TRACKER_REGNUM))
	  (clobber (reg:CC CC_REGNUM))] UNSPECV_SPECULATION_BARRIER))]
  ""
  "
  {
    if (operands[2] == const0_rtx)
      {
	rtx tracker;
	if (<MODE>mode == TImode)
	  tracker = gen_rtx_REG (DImode, SPECULATION_TRACKER_REGNUM);
	else
	  tracker = gen_rtx_REG (<MODE>mode, SPECULATION_TRACKER_REGNUM);

	emit_insn (gen_despeculate_simple<mode> (operands[0], operands[1],
						 tracker));
	DONE;
      }
  }
  "
)

;; Patterns to match despeculate_copy<mode>.  Note that "hint 0x14" is the
;; encoding for CSDB, but will work in older versions of the assembler.
(define_insn "*despeculate_copy<ALLI:mode>_insn"
  [(set (match_operand:ALLI 0 "register_operand" "=r")
	(unspec_volatile:ALLI
	 [(match_operand:ALLI 1 "register_operand" "r")
	  (match_operand:ALLI 2 "aarch64_reg_or_zero" "rZ")
	  (use (reg:DI SPECULATION_TRACKER_REGNUM))
	  (clobber (reg:CC CC_REGNUM))] UNSPECV_SPECULATION_BARRIER))]
  ""
  {
    operands[3] = gen_rtx_REG (DImode, SPECULATION_TRACKER_REGNUM);
    output_asm_insn ("cmp\\t%3, #0\;csel\\t%<w>0, %<w>1, %<w>2, ne\;hint\t0x14 // csdb",
		     operands);
    return "";
  }
  [(set_attr "length" "12")
   (set_attr "type" "block")
   (set_attr "speculation_barrier" "true")]
)

;; Pattern to match despeculate_copyti
(define_insn "*despeculate_copyti_insn"
  [(set (match_operand:TI 0 "register_operand" "=r")
	(unspec_volatile:TI
	 [(match_operand:TI 1 "register_operand" "r")
	  (match_operand:TI 2 "aarch64_reg_or_zero" "rZ")
	  (use (reg:DI SPECULATION_TRACKER_REGNUM))
	  (clobber (reg:CC CC_REGNUM))] UNSPECV_SPECULATION_BARRIER))]
  ""
  {
    operands[3] = gen_rtx_REG (DImode, SPECULATION_TRACKER_REGNUM);
    output_asm_insn
      ("cmp\\t%3, #0\;csel\\t%0, %1, %2, ne\;csel\\t%H0, %H1, %H2, ne\;hint\t0x14 // csdb",
       operands);
    return "";
  }
  [(set_attr "length" "16")
   (set_attr "type" "block")
   (set_attr "speculation_barrier" "true")]
)

(define_insn "despeculate_simple<ALLI:mode>"
  [(set (match_operand:ALLI 0 "register_operand" "=r")
	(unspec_volatile:ALLI
	 [(match_operand:ALLI 1 "register_operand" "r")
	  (use (match_operand:ALLI 2 "register_operand" ""))]
	 UNSPECV_SPECULATION_BARRIER))]
  ""
  "and\\t%<w>0, %<w>1, %<w>2\;hint\t0x14 // csdb"
  [(set_attr "type" "block")
   (set_attr "length" "8")
   (set_attr "speculation_barrier" "true")]
)

(define_insn "despeculate_simpleti"
  [(set (match_operand:TI 0 "register_operand" "=r")
	(unspec_volatile:TI
	 [(match_operand:TI 1 "register_operand" "r")
	  (use (match_operand:DI 2 "register_operand" ""))]
	 UNSPECV_SPECULATION_BARRIER))]
  ""
  "and\\t%0, %1, %2\;and\\t%H0, %H1, %2\;hint\t0x14 // csdb"
  [(set_attr "type" "block")
   (set_attr "length" "12")
   (set_attr "speculation_barrier" "true")]
)

(define_insn "aarch64_<frintnzs_op><mode>"
  [(set (match_operand:VSFDF 0 "register_operand" "=w")
	(unspec:VSFDF [(match_operand:VSFDF 1 "register_operand" "w")]
		      FRINTNZX))]
  "TARGET_FRINT && TARGET_FLOAT
   && !(VECTOR_MODE_P (<MODE>mode) && !TARGET_SIMD)"
  "<frintnzs_op>\\t%<v>0<Vmtype>, %<v>1<Vmtype>"
  [(set_attr "type" "f_rint<stype>")]
)

;; Transactional Memory Extension (TME) instructions.

(define_insn "tstart"
  [(set (match_operand:DI 0 "register_operand" "=r")
	(unspec_volatile:DI [(const_int 0)] UNSPECV_TSTART))
   (clobber (mem:BLK (scratch)))]
  "TARGET_TME"
  "tstart\\t%0"
  [(set_attr "type" "tme")]
)

(define_insn "ttest"
  [(set (match_operand:DI 0 "register_operand" "=r")
	(unspec_volatile:DI [(const_int 0)] UNSPEC_TTEST))
   (clobber (mem:BLK (scratch)))]
  "TARGET_TME"
  "ttest\\t%0"
  [(set_attr "type" "tme")]
)

(define_insn "tcommit"
  [(unspec_volatile:BLK [(const_int 0)] UNSPECV_TCOMMIT)
   (clobber (mem:BLK (scratch)))]
  "TARGET_TME"
  "tcommit"
  [(set_attr "type" "tme")]
)

(define_insn "tcancel"
  [(unspec_volatile:BLK
     [(match_operand 0 "const_int_operand" "n")] UNSPECV_TCANCEL)
   (clobber (mem:BLK (scratch)))]
  "TARGET_TME && (UINTVAL (operands[0]) <= 65535)"
  "tcancel\\t#%0"
  [(set_attr "type" "tme")]
)

(define_insn "aarch64_rndr"
  [(set (match_operand:DI 0 "register_operand" "=r")
	(unspec_volatile:DI [(const_int 0)] UNSPEC_RNDR))
   (set (reg:CC_Z CC_REGNUM)
	(unspec_volatile:CC_Z [(const_int 0)] UNSPEC_RNDR))]
  "TARGET_RNG"
  "mrs\t%0, RNDR"
  [(set_attr "type" "mrs")]
)

(define_insn "aarch64_rndrrs"
  [(set (match_operand:DI 0 "register_operand" "=r")
	(unspec_volatile:DI [(const_int 0)] UNSPEC_RNDRRS))
   (set (reg:CC_Z CC_REGNUM)
	(unspec_volatile:CC_Z [(const_int 0)] UNSPEC_RNDRRS))]
  "TARGET_RNG"
  "mrs\t%0, RNDRRS"
  [(set_attr "type" "mrs")]
)

;; Memory Tagging Extension (MTE) instructions.

(define_insn "irg"
  [(set (match_operand:DI 0 "register_operand" "=rk")
	(ior:DI
	 (and:DI (match_operand:DI 1 "register_operand" "rk")
		 (const_int -1080863910568919041)) ;; 0xf0ff...
	 (ashift:DI (unspec:QI [(match_operand:DI 2 "register_operand" "r")]
		     UNSPEC_GEN_TAG_RND)
		    (const_int 56))))]
  "TARGET_MEMTAG"
  "irg\\t%0, %1, %2"
  [(set_attr "type" "memtag")]
)

(define_insn "gmi"
  [(set (match_operand:DI 0 "register_operand" "=r")
	(ior:DI (ashift:DI
		 (const_int 1)
		 (and:QI (lshiftrt:DI
			  (match_operand:DI 1 "register_operand" "rk")
			  (const_int 56)) (const_int 15)))
		(match_operand:DI 2 "register_operand" "r")))]
  "TARGET_MEMTAG"
  "gmi\\t%0, %1, %2"
  [(set_attr "type" "memtag")]
)

(define_insn "addg"
  [(set (match_operand:DI 0 "register_operand" "=rk")
	(ior:DI
	 (and:DI (plus:DI (match_operand:DI 1 "register_operand" "rk")
			  (match_operand:DI 2 "aarch64_granule16_uimm6" "i"))
		 (const_int -1080863910568919041)) ;; 0xf0ff...
	 (ashift:DI
	  (unspec:QI
	   [(and:QI (lshiftrt:DI (match_dup 1) (const_int 56)) (const_int 15))
	    (match_operand:QI 3 "aarch64_memtag_tag_offset" "i")]
	   UNSPEC_GEN_TAG)
	  (const_int 56))))]
  "TARGET_MEMTAG"
  "addg\\t%0, %1, #%2, #%3"
  [(set_attr "type" "memtag")]
)

(define_insn "subp"
  [(set (match_operand:DI 0 "register_operand" "=r")
	(minus:DI
	  (and:DI (match_operand:DI 1 "register_operand" "rk")
		  (const_int 72057594037927935)) ;; 0x00ff...
	  (and:DI (match_operand:DI 2 "register_operand" "rk")
		  (const_int 72057594037927935))))] ;; 0x00ff...
  "TARGET_MEMTAG"
  "subp\\t%0, %1, %2"
  [(set_attr "type" "memtag")]
)

;; LDG will use the 16-byte aligned value of the address.
(define_insn "ldg"
  [(set (match_operand:DI 0 "register_operand" "+r")
	(ior:DI
	 (and:DI (match_dup 0) (const_int -1080863910568919041)) ;; 0xf0ff...
	 (ashift:DI
	  (mem:QI (unspec:DI
	   [(and:DI (plus:DI (match_operand:DI 1 "register_operand" "rk")
			     (match_operand:DI 2 "aarch64_granule16_simm9" "i"))
		    (const_int -16))] UNSPEC_TAG_SPACE))
	  (const_int 56))))]
  "TARGET_MEMTAG"
  "ldg\\t%0, [%1, #%2]"
  [(set_attr "type" "memtag")]
)

;; STG doesn't align the address but aborts with alignment fault
;; when the address is not 16-byte aligned.
(define_insn "stg"
  [(set (mem:QI (unspec:DI
	 [(plus:DI (match_operand:DI 1 "register_operand" "rk")
		   (match_operand:DI 2 "aarch64_granule16_simm9" "i"))]
	 UNSPEC_TAG_SPACE))
	(and:QI (lshiftrt:DI (match_operand:DI 0 "register_operand" "rk")
			     (const_int 56)) (const_int 15)))]
  "TARGET_MEMTAG"
  "stg\\t%0, [%1, #%2]"
  [(set_attr "type" "memtag")]
)

;; Load/Store 64-bit (LS64) instructions.
(define_insn "ld64b"
  [(set (match_operand:V8DI 0 "register_operand" "=r")
        (unspec_volatile:V8DI
          [(mem:V8DI (match_operand:DI 1 "register_operand" "r"))]
            UNSPEC_LD64B)
  )]
  "TARGET_LS64"
  "ld64b\\t%0, [%1]"
  [(set_attr "type" "ls64")]
)

(define_insn "st64b"
  [(set (mem:V8DI (match_operand:DI 0 "register_operand" "=r"))
        (unspec_volatile:V8DI [(match_operand:V8DI 1 "register_operand" "r")]
            UNSPEC_ST64B)
  )]
  "TARGET_LS64"
  "st64b\\t%1, [%0]"
  [(set_attr "type" "ls64")]
)

(define_insn "st64bv"
  [(set (match_operand:DI 0 "register_operand" "=r")
        (unspec_volatile:DI [(const_int 0)] UNSPEC_ST64BV_RET))
   (set (mem:V8DI (match_operand:DI 1 "register_operand" "r"))
        (unspec_volatile:V8DI [(match_operand:V8DI 2 "register_operand" "r")]
            UNSPEC_ST64BV)
  )]
  "TARGET_LS64"
  "st64bv\\t%0, %2, [%1]"
  [(set_attr "type" "ls64")]
)

(define_insn "st64bv0"
  [(set (match_operand:DI 0 "register_operand" "=r")
        (unspec_volatile:DI [(const_int 0)] UNSPEC_ST64BV0_RET))
   (set (mem:V8DI (match_operand:DI 1 "register_operand" "r"))
        (unspec_volatile:V8DI [(match_operand:V8DI 2 "register_operand" "r")]
            UNSPEC_ST64BV0)
  )]
  "TARGET_LS64"
  "st64bv0\\t%0, %2, [%1]"
  [(set_attr "type" "ls64")]
)

;; AdvSIMD Stuff
(include "aarch64-simd.md")

;; Atomic Operations
(include "atomics.md")

;; ldp/stp peephole patterns
(include "aarch64-ldpstp.md")

;; SVE.
(include "aarch64-sve.md")

;; SVE2.
(include "aarch64-sve2.md")
