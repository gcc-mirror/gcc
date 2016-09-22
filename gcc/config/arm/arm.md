;;- Machine description for ARM for GNU compiler
;;  Copyright (C) 1991-2016 Free Software Foundation, Inc.
;;  Contributed by Pieter `Tiggr' Schoenmakers (rcpieter@win.tue.nl)
;;  and Martin Simmons (@harleqn.co.uk).
;;  More major hacks by Richard Earnshaw (rearnsha@arm.com).

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

;;- See file "rtl.def" for documentation on define_insn, match_*, et. al.


;;---------------------------------------------------------------------------
;; Constants

;; Register numbers -- All machine registers should be defined here
(define_constants
  [(R0_REGNUM         0)	; First CORE register
   (R1_REGNUM	      1)	; Second CORE register
   (IP_REGNUM	     12)	; Scratch register
   (SP_REGNUM	     13)	; Stack pointer
   (LR_REGNUM        14)	; Return address register
   (PC_REGNUM	     15)	; Program counter
   (LAST_ARM_REGNUM  15)	;
   (CC_REGNUM       100)	; Condition code pseudo register
   (VFPCC_REGNUM    101)	; VFP Condition code pseudo register
  ]
)
;; 3rd operand to select_dominance_cc_mode
(define_constants
  [(DOM_CC_X_AND_Y  0)
   (DOM_CC_NX_OR_Y  1)
   (DOM_CC_X_OR_Y   2)
  ]
)
;; conditional compare combination
(define_constants
  [(CMP_CMP 0)
   (CMN_CMP 1)
   (CMP_CMN 2)
   (CMN_CMN 3)
   (NUM_OF_COND_CMP 4)
  ]
)


;;---------------------------------------------------------------------------
;; Attributes

;; Processor type.  This is created automatically from arm-cores.def.
(include "arm-tune.md")

;; Instruction classification types
(include "types.md")

; IS_THUMB is set to 'yes' when we are generating Thumb code, and 'no' when
; generating ARM code.  This is used to control the length of some insn
; patterns that share the same RTL in both ARM and Thumb code.
(define_attr "is_thumb" "yes,no"
  (const (if_then_else (symbol_ref "TARGET_THUMB")
		       (const_string "yes") (const_string "no"))))

; IS_ARCH6 is set to 'yes' when we are generating code form ARMv6.
(define_attr "is_arch6" "no,yes" (const (symbol_ref "arm_arch6")))

; IS_THUMB1 is set to 'yes' iff we are generating Thumb-1 code.
(define_attr "is_thumb1" "yes,no"
  (const (if_then_else (symbol_ref "TARGET_THUMB1")
		       (const_string "yes") (const_string "no"))))

; We use this attribute to disable alternatives that can produce 32-bit
; instructions inside an IT-block in Thumb2 state.  ARMv8 deprecates IT blocks
; that contain 32-bit instructions.
(define_attr "enabled_for_depr_it" "no,yes" (const_string "yes"))

; This attribute is used to disable a predicated alternative when we have
; arm_restrict_it.
(define_attr "predicable_short_it" "no,yes" (const_string "yes"))

;; Operand number of an input operand that is shifted.  Zero if the
;; given instruction does not shift one of its input operands.
(define_attr "shift" "" (const_int 0))

;; [For compatibility with AArch64 in pipeline models]
;; Attribute that specifies whether or not the instruction touches fp
;; registers.
(define_attr "fp" "no,yes" (const_string "no"))

; Floating Point Unit.  If we only have floating point emulation, then there
; is no point in scheduling the floating point insns.  (Well, for best
; performance we should try and group them together).
(define_attr "fpu" "none,vfp"
  (const (symbol_ref "arm_fpu_attr")))

; Predicated means that the insn form is conditionally executed based on a
; predicate.  We default to 'no' because no Thumb patterns match this rule
; and not all ARM insns do.
(define_attr "predicated" "yes,no" (const_string "no"))

; LENGTH of an instruction (in bytes)
(define_attr "length" ""
  (const_int 4))

; The architecture which supports the instruction (or alternative).
; This can be "a" for ARM, "t" for either of the Thumbs, "32" for
; TARGET_32BIT, "t1" or "t2" to specify a specific Thumb mode.  "v6"
; for ARM or Thumb-2 with arm_arch6, and nov6 for ARM without
; arm_arch6.  "v6t2" for Thumb-2 with arm_arch6 and "v8mb" for ARMv8-M
; Baseline.  This attribute is used to compute attribute "enabled",
; use type "any" to enable an alternative in all cases.
(define_attr "arch" "any,a,t,32,t1,t2,v6,nov6,v6t2,v8mb,neon_for_64bits,avoid_neon_for_64bits,iwmmxt,iwmmxt2,armv6_or_vfpv3,neon"
  (const_string "any"))

(define_attr "arch_enabled" "no,yes"
  (cond [(eq_attr "arch" "any")
	 (const_string "yes")

	 (and (eq_attr "arch" "a")
	      (match_test "TARGET_ARM"))
	 (const_string "yes")

	 (and (eq_attr "arch" "t")
	      (match_test "TARGET_THUMB"))
	 (const_string "yes")

	 (and (eq_attr "arch" "t1")
	      (match_test "TARGET_THUMB1"))
	 (const_string "yes")

	 (and (eq_attr "arch" "t2")
	      (match_test "TARGET_THUMB2"))
	 (const_string "yes")

	 (and (eq_attr "arch" "32")
	      (match_test "TARGET_32BIT"))
	 (const_string "yes")

	 (and (eq_attr "arch" "v6")
	      (match_test "TARGET_32BIT && arm_arch6"))
	 (const_string "yes")

	 (and (eq_attr "arch" "nov6")
	      (match_test "TARGET_32BIT && !arm_arch6"))
	 (const_string "yes")

	 (and (eq_attr "arch" "v6t2")
	      (match_test "TARGET_32BIT && arm_arch6 && arm_arch_thumb2"))
	 (const_string "yes")

	 (and (eq_attr "arch" "v8mb")
	      (match_test "TARGET_THUMB1 && arm_arch8"))
	 (const_string "yes")

	 (and (eq_attr "arch" "avoid_neon_for_64bits")
	      (match_test "TARGET_NEON")
	      (not (match_test "TARGET_PREFER_NEON_64BITS")))
	 (const_string "yes")

	 (and (eq_attr "arch" "neon_for_64bits")
	      (match_test "TARGET_NEON")
	      (match_test "TARGET_PREFER_NEON_64BITS"))
	 (const_string "yes")

	 (and (eq_attr "arch" "iwmmxt2")
	      (match_test "TARGET_REALLY_IWMMXT2"))
	 (const_string "yes")

	 (and (eq_attr "arch" "armv6_or_vfpv3")
	      (match_test "arm_arch6 || TARGET_VFP3"))
	 (const_string "yes")

	 (and (eq_attr "arch" "neon")
	      (match_test "TARGET_NEON"))
	 (const_string "yes")
	]

	(const_string "no")))

(define_attr "opt" "any,speed,size"
  (const_string "any"))

(define_attr "opt_enabled" "no,yes"
  (cond [(eq_attr "opt" "any")
         (const_string "yes")

	 (and (eq_attr "opt" "speed")
	      (match_test "optimize_function_for_speed_p (cfun)"))
	 (const_string "yes")

	 (and (eq_attr "opt" "size")
	      (match_test "optimize_function_for_size_p (cfun)"))
	 (const_string "yes")]
	(const_string "no")))

(define_attr "use_literal_pool" "no,yes"
   (cond [(and (eq_attr "type" "f_loads,f_loadd")
	       (match_test "CONSTANT_P (operands[1])"))
	  (const_string "yes")]
	 (const_string "no")))

; Enable all alternatives that are both arch_enabled and insn_enabled.
; FIXME:: opt_enabled has been temporarily removed till the time we have
; an attribute that allows the use of such alternatives.
; This depends on caching of speed_p, size_p on a per
; alternative basis. The problem is that the enabled attribute
; cannot depend on any state that is not cached or is not constant
; for a compilation unit. We probably need a generic "hot/cold"
; alternative which if implemented can help with this. We disable this
; until such a time as this is implemented and / or the improvements or
; regressions with removing this attribute are double checked.
; See ashldi3_neon and <shift>di3_neon in neon.md.

 (define_attr "enabled" "no,yes"
   (cond [(and (eq_attr "predicable_short_it" "no")
	       (and (eq_attr "predicated" "yes")
	            (match_test "arm_restrict_it")))
	  (const_string "no")

	  (and (eq_attr "enabled_for_depr_it" "no")
	       (match_test "arm_restrict_it"))
	  (const_string "no")

	  (and (eq_attr "use_literal_pool" "yes")
	       (match_test "arm_disable_literal_pool"))
	  (const_string "no")

	  (eq_attr "arch_enabled" "no")
	  (const_string "no")]
	 (const_string "yes")))

; POOL_RANGE is how far away from a constant pool entry that this insn
; can be placed.  If the distance is zero, then this insn will never
; reference the pool.
; Note that for Thumb constant pools the PC value is rounded down to the
; nearest multiple of four.  Therefore, THUMB2_POOL_RANGE (and POOL_RANGE for
; Thumb insns) should be set to <max_range> - 2.
; NEG_POOL_RANGE is nonzero for insns that can reference a constant pool entry
; before its address.  It is set to <max_range> - (8 + <data_size>).
(define_attr "arm_pool_range" "" (const_int 0))
(define_attr "thumb2_pool_range" "" (const_int 0))
(define_attr "arm_neg_pool_range" "" (const_int 0))
(define_attr "thumb2_neg_pool_range" "" (const_int 0))

(define_attr "pool_range" ""
  (cond [(eq_attr "is_thumb" "yes") (attr "thumb2_pool_range")]
	(attr "arm_pool_range")))
(define_attr "neg_pool_range" ""
  (cond [(eq_attr "is_thumb" "yes") (attr "thumb2_neg_pool_range")]
	(attr "arm_neg_pool_range")))

; An assembler sequence may clobber the condition codes without us knowing.
; If such an insn references the pool, then we have no way of knowing how,
; so use the most conservative value for pool_range.
(define_asm_attributes
 [(set_attr "conds" "clob")
  (set_attr "length" "4")
  (set_attr "pool_range" "250")])

; Load scheduling, set from the arm_ld_sched variable
; initialized by arm_option_override()
(define_attr "ldsched" "no,yes" (const (symbol_ref "arm_ld_sched")))

; condition codes: this one is used by final_prescan_insn to speed up
; conditionalizing instructions.  It saves having to scan the rtl to see if
; it uses or alters the condition codes.
; 
; USE means that the condition codes are used by the insn in the process of
;   outputting code, this means (at present) that we can't use the insn in
;   inlined branches
;
; SET means that the purpose of the insn is to set the condition codes in a
;   well defined manner.
;
; CLOB means that the condition codes are altered in an undefined manner, if
;   they are altered at all
;
; UNCONDITIONAL means the instruction can not be conditionally executed and
;   that the instruction does not use or alter the condition codes.
;
; NOCOND means that the instruction does not use or alter the condition
;   codes but can be converted into a conditionally exectuted instruction.

(define_attr "conds" "use,set,clob,unconditional,nocond"
	(if_then_else
	 (ior (eq_attr "is_thumb1" "yes")
	      (eq_attr "type" "call"))
	 (const_string "clob")
	 (if_then_else (eq_attr "is_neon_type" "no")
	 (const_string "nocond")
	 (const_string "unconditional"))))

; Predicable means that the insn can be conditionally executed based on
; an automatically added predicate (additional patterns are generated by 
; gen...).  We default to 'no' because no Thumb patterns match this rule
; and not all ARM patterns do.
(define_attr "predicable" "no,yes" (const_string "no"))

; Only model the write buffer for ARM6 and ARM7.  Earlier processors don't
; have one.  Later ones, such as StrongARM, have write-back caches, so don't
; suffer blockages enough to warrant modelling this (and it can adversely
; affect the schedule).
(define_attr "model_wbuf" "no,yes" (const (symbol_ref "arm_tune_wbuf")))

; WRITE_CONFLICT implies that a read following an unrelated write is likely
; to stall the processor.  Used with model_wbuf above.
(define_attr "write_conflict" "no,yes"
  (if_then_else (eq_attr "type"
		 "block,call,load1")
		(const_string "yes")
		(const_string "no")))

; Classify the insns into those that take one cycle and those that take more
; than one on the main cpu execution unit.
(define_attr "core_cycles" "single,multi"
  (if_then_else (eq_attr "type"
    "adc_imm, adc_reg, adcs_imm, adcs_reg, adr, alu_ext, alu_imm, alu_sreg,\
    alu_shift_imm, alu_shift_reg, alu_dsp_reg, alus_ext, alus_imm, alus_sreg,\
    alus_shift_imm, alus_shift_reg, bfm, csel, rev, logic_imm, logic_reg,\
    logic_shift_imm, logic_shift_reg, logics_imm, logics_reg,\
    logics_shift_imm, logics_shift_reg, extend, shift_imm, float, fcsel,\
    wmmx_wor, wmmx_wxor, wmmx_wand, wmmx_wandn, wmmx_wmov, wmmx_tmcrr,\
    wmmx_tmrrc, wmmx_wldr, wmmx_wstr, wmmx_tmcr, wmmx_tmrc, wmmx_wadd,\
    wmmx_wsub, wmmx_wmul, wmmx_wmac, wmmx_wavg2, wmmx_tinsr, wmmx_textrm,\
    wmmx_wshufh, wmmx_wcmpeq, wmmx_wcmpgt, wmmx_wmax, wmmx_wmin, wmmx_wpack,\
    wmmx_wunpckih, wmmx_wunpckil, wmmx_wunpckeh, wmmx_wunpckel, wmmx_wror,\
    wmmx_wsra, wmmx_wsrl, wmmx_wsll, wmmx_wmadd, wmmx_tmia, wmmx_tmiaph,\
    wmmx_tmiaxy, wmmx_tbcst, wmmx_tmovmsk, wmmx_wacc, wmmx_waligni,\
    wmmx_walignr, wmmx_tandc, wmmx_textrc, wmmx_torc, wmmx_torvsc, wmmx_wsad,\
    wmmx_wabs, wmmx_wabsdiff, wmmx_waddsubhx, wmmx_wsubaddhx, wmmx_wavg4,\
    wmmx_wmulw, wmmx_wqmulm, wmmx_wqmulwm, wmmx_waddbhus, wmmx_wqmiaxy,\
    wmmx_wmiaxy, wmmx_wmiawxy, wmmx_wmerge")
		(const_string "single")
	        (const_string "multi")))

;; FAR_JUMP is "yes" if a BL instruction is used to generate a branch to a
;; distant label.  Only applicable to Thumb code.
(define_attr "far_jump" "yes,no" (const_string "no"))


;; The number of machine instructions this pattern expands to.
;; Used for Thumb-2 conditional execution.
(define_attr "ce_count" "" (const_int 1))

;;---------------------------------------------------------------------------
;; Unspecs

(include "unspecs.md")

;;---------------------------------------------------------------------------
;; Mode iterators

(include "iterators.md")

;;---------------------------------------------------------------------------
;; Predicates

(include "predicates.md")
(include "constraints.md")

;;---------------------------------------------------------------------------
;; Pipeline descriptions

(define_attr "tune_cortexr4" "yes,no"
  (const (if_then_else
	  (eq_attr "tune" "cortexr4,cortexr4f,cortexr5")
	  (const_string "yes")
	  (const_string "no"))))

;; True if the generic scheduling description should be used.

(define_attr "generic_sched" "yes,no"
  (const (if_then_else
          (ior (eq_attr "tune" "fa526,fa626,fa606te,fa626te,fmp626,fa726te,\
                                arm926ejs,arm1020e,arm1026ejs,arm1136js,\
                                arm1136jfs,cortexa5,cortexa7,cortexa8,\
                                cortexa9,cortexa12,cortexa15,cortexa17,\
                                cortexa53,cortexa57,cortexm4,cortexm7,\
				exynosm1,marvell_pj4,xgene1")
	       (eq_attr "tune_cortexr4" "yes"))
          (const_string "no")
          (const_string "yes"))))

(define_attr "generic_vfp" "yes,no"
  (const (if_then_else
	  (and (eq_attr "fpu" "vfp")
	       (eq_attr "tune" "!arm1020e,arm1022e,cortexa5,cortexa7,\
                                cortexa8,cortexa9,cortexa53,cortexm4,\
                                cortexm7,marvell_pj4,xgene1")
	       (eq_attr "tune_cortexr4" "no"))
	  (const_string "yes")
	  (const_string "no"))))

(include "marvell-f-iwmmxt.md")
(include "arm-generic.md")
(include "arm926ejs.md")
(include "arm1020e.md")
(include "arm1026ejs.md")
(include "arm1136jfs.md")
(include "fa526.md")
(include "fa606te.md")
(include "fa626te.md")
(include "fmp626.md")
(include "fa726te.md")
(include "cortex-a5.md")
(include "cortex-a7.md")
(include "cortex-a8.md")
(include "cortex-a9.md")
(include "cortex-a15.md")
(include "cortex-a17.md")
(include "cortex-a53.md")
(include "cortex-a57.md")
(include "cortex-r4.md")
(include "cortex-r4f.md")
(include "cortex-m7.md")
(include "cortex-m4.md")
(include "cortex-m4-fpu.md")
(include "exynos-m1.md")
(include "vfp11.md")
(include "marvell-pj4.md")
(include "xgene1.md")


;;---------------------------------------------------------------------------
;; Insn patterns
;;
;; Addition insns.

;; Note: For DImode insns, there is normally no reason why operands should
;; not be in the same register, what we don't want is for something being
;; written to partially overlap something that is an input.

(define_expand "adddi3"
 [(parallel
   [(set (match_operand:DI           0 "s_register_operand" "")
	  (plus:DI (match_operand:DI 1 "s_register_operand" "")
	           (match_operand:DI 2 "arm_adddi_operand"  "")))
    (clobber (reg:CC CC_REGNUM))])]
  "TARGET_EITHER"
  "
  if (TARGET_THUMB1)
    {
      if (!REG_P (operands[1]))
        operands[1] = force_reg (DImode, operands[1]);
      if (!REG_P (operands[2]))
        operands[2] = force_reg (DImode, operands[2]);
     }
  "
)

(define_insn_and_split "*arm_adddi3"
  [(set (match_operand:DI          0 "s_register_operand" "=&r,&r,&r,&r,&r")
	(plus:DI (match_operand:DI 1 "s_register_operand" "%0, 0, r, 0, r")
		 (match_operand:DI 2 "arm_adddi_operand"  "r,  0, r, Dd, Dd")))
   (clobber (reg:CC CC_REGNUM))]
  "TARGET_32BIT && !TARGET_NEON"
  "#"
  "TARGET_32BIT && reload_completed
   && ! (TARGET_NEON && IS_VFP_REGNUM (REGNO (operands[0])))"
  [(parallel [(set (reg:CC_C CC_REGNUM)
		   (compare:CC_C (plus:SI (match_dup 1) (match_dup 2))
				 (match_dup 1)))
	      (set (match_dup 0) (plus:SI (match_dup 1) (match_dup 2)))])
   (set (match_dup 3) (plus:SI (plus:SI (match_dup 4) (match_dup 5))
			       (ltu:SI (reg:CC_C CC_REGNUM) (const_int 0))))]
  "
  {
    operands[3] = gen_highpart (SImode, operands[0]);
    operands[0] = gen_lowpart (SImode, operands[0]);
    operands[4] = gen_highpart (SImode, operands[1]);
    operands[1] = gen_lowpart (SImode, operands[1]);
    operands[5] = gen_highpart_mode (SImode, DImode, operands[2]);
    operands[2] = gen_lowpart (SImode, operands[2]);
  }"
  [(set_attr "conds" "clob")
   (set_attr "length" "8")
   (set_attr "type" "multiple")]
)

(define_insn_and_split "*adddi_sesidi_di"
  [(set (match_operand:DI 0 "s_register_operand" "=&r,&r")
	(plus:DI (sign_extend:DI
		  (match_operand:SI 2 "s_register_operand" "r,r"))
		 (match_operand:DI 1 "s_register_operand" "0,r")))
   (clobber (reg:CC CC_REGNUM))]
  "TARGET_32BIT"
  "#"
  "TARGET_32BIT && reload_completed"
  [(parallel [(set (reg:CC_C CC_REGNUM)
		   (compare:CC_C (plus:SI (match_dup 1) (match_dup 2))
				 (match_dup 1)))
	      (set (match_dup 0) (plus:SI (match_dup 1) (match_dup 2)))])
   (set (match_dup 3) (plus:SI (plus:SI (ashiftrt:SI (match_dup 2)
						     (const_int 31))
					(match_dup 4))
			       (ltu:SI (reg:CC_C CC_REGNUM) (const_int 0))))]
  "
  {
    operands[3] = gen_highpart (SImode, operands[0]);
    operands[0] = gen_lowpart (SImode, operands[0]);
    operands[4] = gen_highpart (SImode, operands[1]);
    operands[1] = gen_lowpart (SImode, operands[1]);
    operands[2] = gen_lowpart (SImode, operands[2]);
  }"
  [(set_attr "conds" "clob")
   (set_attr "length" "8")
   (set_attr "type" "multiple")]
)

(define_insn_and_split "*adddi_zesidi_di"
  [(set (match_operand:DI 0 "s_register_operand" "=&r,&r")
	(plus:DI (zero_extend:DI
		  (match_operand:SI 2 "s_register_operand" "r,r"))
		 (match_operand:DI 1 "s_register_operand" "0,r")))
   (clobber (reg:CC CC_REGNUM))]
  "TARGET_32BIT"
  "#"
  "TARGET_32BIT && reload_completed"
  [(parallel [(set (reg:CC_C CC_REGNUM)
		   (compare:CC_C (plus:SI (match_dup 1) (match_dup 2))
				 (match_dup 1)))
	      (set (match_dup 0) (plus:SI (match_dup 1) (match_dup 2)))])
   (set (match_dup 3) (plus:SI (plus:SI (match_dup 4) (const_int 0))
			       (ltu:SI (reg:CC_C CC_REGNUM) (const_int 0))))]
  "
  {
    operands[3] = gen_highpart (SImode, operands[0]);
    operands[0] = gen_lowpart (SImode, operands[0]);
    operands[4] = gen_highpart (SImode, operands[1]);
    operands[1] = gen_lowpart (SImode, operands[1]);
    operands[2] = gen_lowpart (SImode, operands[2]);
  }"
  [(set_attr "conds" "clob")
   (set_attr "length" "8")
   (set_attr "type" "multiple")]
)

(define_expand "addv<mode>4"
  [(match_operand:SIDI 0 "register_operand")
   (match_operand:SIDI 1 "register_operand")
   (match_operand:SIDI 2 "register_operand")
   (match_operand 3 "")]
  "TARGET_32BIT"
{
  emit_insn (gen_add<mode>3_compareV (operands[0], operands[1], operands[2]));
  arm_gen_unlikely_cbranch (NE, CC_Vmode, operands[3]);

  DONE;
})

(define_expand "uaddv<mode>4"
  [(match_operand:SIDI 0 "register_operand")
   (match_operand:SIDI 1 "register_operand")
   (match_operand:SIDI 2 "register_operand")
   (match_operand 3 "")]
  "TARGET_32BIT"
{
  emit_insn (gen_add<mode>3_compareC (operands[0], operands[1], operands[2]));
  arm_gen_unlikely_cbranch (NE, CC_Cmode, operands[3]);

  DONE;
})

(define_expand "addsi3"
  [(set (match_operand:SI          0 "s_register_operand" "")
	(plus:SI (match_operand:SI 1 "s_register_operand" "")
		 (match_operand:SI 2 "reg_or_int_operand" "")))]
  "TARGET_EITHER"
  "
  if (TARGET_32BIT && CONST_INT_P (operands[2]))
    {
      arm_split_constant (PLUS, SImode, NULL_RTX,
	                  INTVAL (operands[2]), operands[0], operands[1],
			  optimize && can_create_pseudo_p ());
      DONE;
    }
  "
)

; If there is a scratch available, this will be faster than synthesizing the
; addition.
(define_peephole2
  [(match_scratch:SI 3 "r")
   (set (match_operand:SI          0 "arm_general_register_operand" "")
	(plus:SI (match_operand:SI 1 "arm_general_register_operand" "")
		 (match_operand:SI 2 "const_int_operand"  "")))]
  "TARGET_32BIT &&
   !(const_ok_for_arm (INTVAL (operands[2]))
     || const_ok_for_arm (-INTVAL (operands[2])))
    && const_ok_for_arm (~INTVAL (operands[2]))"
  [(set (match_dup 3) (match_dup 2))
   (set (match_dup 0) (plus:SI (match_dup 1) (match_dup 3)))]
  ""
)

;; The r/r/k alternative is required when reloading the address
;;  (plus (reg rN) (reg sp)) into (reg rN).  In this case reload will
;; put the duplicated register first, and not try the commutative version.
(define_insn_and_split "*arm_addsi3"
  [(set (match_operand:SI          0 "s_register_operand" "=rk,l,l ,l ,r ,k ,r,r ,k ,r ,k,k,r ,k ,r")
        (plus:SI (match_operand:SI 1 "s_register_operand" "%0 ,l,0 ,l ,rk,k ,r,rk,k ,rk,k,r,rk,k ,rk")
                 (match_operand:SI 2 "reg_or_int_operand" "rk ,l,Py,Pd,rI,rI,k,Pj,Pj,L ,L,L,PJ,PJ,?n")))]
  "TARGET_32BIT"
  "@
   add%?\\t%0, %0, %2
   add%?\\t%0, %1, %2
   add%?\\t%0, %1, %2
   add%?\\t%0, %1, %2
   add%?\\t%0, %1, %2
   add%?\\t%0, %1, %2
   add%?\\t%0, %2, %1
   addw%?\\t%0, %1, %2
   addw%?\\t%0, %1, %2
   sub%?\\t%0, %1, #%n2
   sub%?\\t%0, %1, #%n2
   sub%?\\t%0, %1, #%n2
   subw%?\\t%0, %1, #%n2
   subw%?\\t%0, %1, #%n2
   #"
  "TARGET_32BIT
   && CONST_INT_P (operands[2])
   && !const_ok_for_op (INTVAL (operands[2]), PLUS)
   && (reload_completed || !arm_eliminable_register (operands[1]))"
  [(clobber (const_int 0))]
  "
  arm_split_constant (PLUS, SImode, curr_insn,
	              INTVAL (operands[2]), operands[0],
		      operands[1], 0);
  DONE;
  "
  [(set_attr "length" "2,4,4,4,4,4,4,4,4,4,4,4,4,4,16")
   (set_attr "predicable" "yes")
   (set_attr "predicable_short_it" "yes,yes,yes,yes,no,no,no,no,no,no,no,no,no,no,no")
   (set_attr "arch" "t2,t2,t2,t2,*,*,*,t2,t2,*,*,a,t2,t2,*")
   (set (attr "type") (if_then_else (match_operand 2 "const_int_operand" "")
		      (const_string "alu_imm")
		      (const_string "alu_sreg")))
 ]
)

(define_insn_and_split "adddi3_compareV"
  [(set (reg:CC_V CC_REGNUM)
	(ne:CC_V
	  (plus:TI
	    (sign_extend:TI (match_operand:DI 1 "register_operand" "r"))
	    (sign_extend:TI (match_operand:DI 2 "register_operand" "r")))
	  (sign_extend:TI (plus:DI (match_dup 1) (match_dup 2)))))
   (set (match_operand:DI 0 "register_operand" "=&r")
	(plus:DI (match_dup 1) (match_dup 2)))]
  "TARGET_32BIT"
  "#"
  "&& reload_completed"
  [(parallel [(set (reg:CC_C CC_REGNUM)
		   (compare:CC_C (plus:SI (match_dup 1) (match_dup 2))
				 (match_dup 1)))
	      (set (match_dup 0) (plus:SI (match_dup 1) (match_dup 2)))])
   (parallel [(set (reg:CC_V CC_REGNUM)
		   (ne:CC_V
		    (plus:DI (plus:DI
			      (sign_extend:DI (match_dup 4))
			      (sign_extend:DI (match_dup 5)))
			     (ltu:DI (reg:CC_C CC_REGNUM) (const_int 0)))
		    (plus:DI (sign_extend:DI
			      (plus:SI (match_dup 4) (match_dup 5)))
			     (ltu:DI (reg:CC_C CC_REGNUM) (const_int 0)))))
	     (set (match_dup 3) (plus:SI (plus:SI
					  (match_dup 4) (match_dup 5))
					 (ltu:SI (reg:CC_C CC_REGNUM)
						 (const_int 0))))])]
  "
  {
    operands[3] = gen_highpart (SImode, operands[0]);
    operands[0] = gen_lowpart (SImode, operands[0]);
    operands[4] = gen_highpart (SImode, operands[1]);
    operands[1] = gen_lowpart (SImode, operands[1]);
    operands[5] = gen_highpart (SImode, operands[2]);
    operands[2] = gen_lowpart (SImode, operands[2]);
  }"
 [(set_attr "conds" "set")
   (set_attr "length" "8")
   (set_attr "type" "multiple")]
)

(define_insn "addsi3_compareV"
  [(set (reg:CC_V CC_REGNUM)
	(ne:CC_V
	  (plus:DI
	    (sign_extend:DI (match_operand:SI 1 "register_operand" "r"))
	    (sign_extend:DI (match_operand:SI 2 "register_operand" "r")))
	  (sign_extend:DI (plus:SI (match_dup 1) (match_dup 2)))))
   (set (match_operand:SI 0 "register_operand" "=r")
	(plus:SI (match_dup 1) (match_dup 2)))]
  "TARGET_32BIT"
  "adds%?\\t%0, %1, %2"
  [(set_attr "conds" "set")
   (set_attr "type" "alus_sreg")]
)

(define_insn "*addsi3_compareV_upper"
  [(set (reg:CC_V CC_REGNUM)
	(ne:CC_V
	  (plus:DI
	   (plus:DI
	    (sign_extend:DI (match_operand:SI 1 "register_operand" "r"))
	    (sign_extend:DI (match_operand:SI 2 "register_operand" "r")))
	   (ltu:DI (reg:CC_C CC_REGNUM) (const_int 0)))
	  (plus:DI (sign_extend:DI
		    (plus:SI (match_dup 1) (match_dup 2)))
		   (ltu:DI (reg:CC_C CC_REGNUM) (const_int 0)))))
   (set (match_operand:SI 0 "register_operand" "=r")
	(plus:SI
	 (plus:SI (match_dup 1) (match_dup 2))
	 (ltu:SI (reg:CC_C CC_REGNUM) (const_int 0))))]
  "TARGET_32BIT"
  "adcs%?\\t%0, %1, %2"
  [(set_attr "conds" "set")
   (set_attr "type" "adcs_reg")]
)

(define_insn_and_split "adddi3_compareC"
  [(set (reg:CC_C CC_REGNUM)
	(ne:CC_C
	  (plus:TI
	    (zero_extend:TI (match_operand:DI 1 "register_operand" "r"))
	    (zero_extend:TI (match_operand:DI 2 "register_operand" "r")))
	  (zero_extend:TI (plus:DI (match_dup 1) (match_dup 2)))))
   (set (match_operand:DI 0 "register_operand" "=&r")
	(plus:DI (match_dup 1) (match_dup 2)))]
  "TARGET_32BIT"
  "#"
  "&& reload_completed"
  [(parallel [(set (reg:CC_C CC_REGNUM)
		   (compare:CC_C (plus:SI (match_dup 1) (match_dup 2))
				 (match_dup 1)))
	      (set (match_dup 0) (plus:SI (match_dup 1) (match_dup 2)))])
   (parallel [(set (reg:CC_C CC_REGNUM)
		   (ne:CC_C
		    (plus:DI (plus:DI
			      (zero_extend:DI (match_dup 4))
			      (zero_extend:DI (match_dup 5)))
			     (ltu:DI (reg:CC_C CC_REGNUM) (const_int 0)))
		    (plus:DI (zero_extend:DI
			      (plus:SI (match_dup 4) (match_dup 5)))
			     (ltu:DI (reg:CC_C CC_REGNUM) (const_int 0)))))
	     (set (match_dup 3) (plus:SI
				 (plus:SI (match_dup 4) (match_dup 5))
				 (ltu:SI (reg:CC_C CC_REGNUM)
					 (const_int 0))))])]
  "
  {
    operands[3] = gen_highpart (SImode, operands[0]);
    operands[0] = gen_lowpart (SImode, operands[0]);
    operands[4] = gen_highpart (SImode, operands[1]);
    operands[5] = gen_highpart (SImode, operands[2]);
    operands[1] = gen_lowpart (SImode, operands[1]);
    operands[2] = gen_lowpart (SImode, operands[2]);
  }"
 [(set_attr "conds" "set")
   (set_attr "length" "8")
   (set_attr "type" "multiple")]
)

(define_insn "*addsi3_compareC_upper"
  [(set (reg:CC_C CC_REGNUM)
	(ne:CC_C
	  (plus:DI
	   (plus:DI
	    (zero_extend:DI (match_operand:SI 1 "register_operand" "r"))
	    (zero_extend:DI (match_operand:SI 2 "register_operand" "r")))
	   (ltu:DI (reg:CC_C CC_REGNUM) (const_int 0)))
	  (plus:DI (zero_extend:DI
		    (plus:SI (match_dup 1) (match_dup 2)))
		   (ltu:DI (reg:CC_C CC_REGNUM) (const_int 0)))))
   (set (match_operand:SI 0 "register_operand" "=r")
	(plus:SI
	 (plus:SI (match_dup 1) (match_dup 2))
	 (ltu:SI (reg:CC_C CC_REGNUM) (const_int 0))))]
  "TARGET_32BIT"
  "adcs%?\\t%0, %1, %2"
  [(set_attr "conds" "set")
   (set_attr "type" "adcs_reg")]
)

(define_insn "addsi3_compareC"
   [(set (reg:CC_C CC_REGNUM)
	 (ne:CC_C
	  (plus:DI
	   (zero_extend:DI (match_operand:SI 1 "register_operand" "r"))
	   (zero_extend:DI (match_operand:SI 2 "register_operand" "r")))
	  (zero_extend:DI
	   (plus:SI (match_dup 1) (match_dup 2)))))
    (set (match_operand:SI 0 "register_operand" "=r")
	 (plus:SI (match_dup 1) (match_dup 2)))]
  "TARGET_32BIT"
  "adds%?\\t%0, %1, %2"
  [(set_attr "conds" "set")
   (set_attr "type" "alus_sreg")]
)

(define_insn "addsi3_compare0"
  [(set (reg:CC_NOOV CC_REGNUM)
	(compare:CC_NOOV
	 (plus:SI (match_operand:SI 1 "s_register_operand" "r, r,r")
		  (match_operand:SI 2 "arm_add_operand"    "I,L,r"))
	 (const_int 0)))
   (set (match_operand:SI 0 "s_register_operand" "=r,r,r")
	(plus:SI (match_dup 1) (match_dup 2)))]
  "TARGET_ARM"
  "@
   adds%?\\t%0, %1, %2
   subs%?\\t%0, %1, #%n2
   adds%?\\t%0, %1, %2"
  [(set_attr "conds" "set")
   (set_attr "type" "alus_imm,alus_imm,alus_sreg")]
)

(define_insn "*addsi3_compare0_scratch"
  [(set (reg:CC_NOOV CC_REGNUM)
	(compare:CC_NOOV
	 (plus:SI (match_operand:SI 0 "s_register_operand" "r, r, r")
		  (match_operand:SI 1 "arm_add_operand"    "I,L, r"))
	 (const_int 0)))]
  "TARGET_ARM"
  "@
   cmn%?\\t%0, %1
   cmp%?\\t%0, #%n1
   cmn%?\\t%0, %1"
  [(set_attr "conds" "set")
   (set_attr "predicable" "yes")
   (set_attr "type" "alus_imm,alus_imm,alus_sreg")]
)

(define_insn "*compare_negsi_si"
  [(set (reg:CC_Z CC_REGNUM)
	(compare:CC_Z
	 (neg:SI (match_operand:SI 0 "s_register_operand" "l,r"))
	 (match_operand:SI 1 "s_register_operand" "l,r")))]
  "TARGET_32BIT"
  "cmn%?\\t%1, %0"
  [(set_attr "conds" "set")
   (set_attr "predicable" "yes")
   (set_attr "arch" "t2,*")
   (set_attr "length" "2,4")
   (set_attr "predicable_short_it" "yes,no")
   (set_attr "type" "alus_sreg")]
)

;; This is the canonicalization of addsi3_compare0_for_combiner when the
;; addend is a constant.
(define_insn "cmpsi2_addneg"
  [(set (reg:CC CC_REGNUM)
	(compare:CC
	 (match_operand:SI 1 "s_register_operand" "r,r")
	 (match_operand:SI 2 "arm_addimm_operand" "L,I")))
   (set (match_operand:SI 0 "s_register_operand" "=r,r")
	(plus:SI (match_dup 1)
		 (match_operand:SI 3 "arm_addimm_operand" "I,L")))]
  "TARGET_32BIT && INTVAL (operands[2]) == -INTVAL (operands[3])"
  "@
   adds%?\\t%0, %1, %3
   subs%?\\t%0, %1, #%n3"
  [(set_attr "conds" "set")
   (set_attr "type" "alus_sreg")]
)

;; Convert the sequence
;;  sub  rd, rn, #1
;;  cmn  rd, #1	(equivalent to cmp rd, #-1)
;;  bne  dest
;; into
;;  subs rd, rn, #1
;;  bcs  dest	((unsigned)rn >= 1)
;; similarly for the beq variant using bcc.
;; This is a common looping idiom (while (n--))
(define_peephole2
  [(set (match_operand:SI 0 "arm_general_register_operand" "")
	(plus:SI (match_operand:SI 1 "arm_general_register_operand" "")
		 (const_int -1)))
   (set (match_operand 2 "cc_register" "")
	(compare (match_dup 0) (const_int -1)))
   (set (pc)
	(if_then_else (match_operator 3 "equality_operator"
		       [(match_dup 2) (const_int 0)])
		      (match_operand 4 "" "")
		      (match_operand 5 "" "")))]
  "TARGET_32BIT && peep2_reg_dead_p (3, operands[2])"
  [(parallel[
    (set (match_dup 2)
	 (compare:CC
	  (match_dup 1) (const_int 1)))
    (set (match_dup 0) (plus:SI (match_dup 1) (const_int -1)))])
   (set (pc)
	(if_then_else (match_op_dup 3 [(match_dup 2) (const_int 0)])
		      (match_dup 4)
		      (match_dup 5)))]
  "operands[2] = gen_rtx_REG (CCmode, CC_REGNUM);
   operands[3] = gen_rtx_fmt_ee ((GET_CODE (operands[3]) == NE
				  ? GEU : LTU),
				 VOIDmode, 
				 operands[2], const0_rtx);"
)

;; The next four insns work because they compare the result with one of
;; the operands, and we know that the use of the condition code is
;; either GEU or LTU, so we can use the carry flag from the addition
;; instead of doing the compare a second time.
(define_insn "*addsi3_compare_op1"
  [(set (reg:CC_C CC_REGNUM)
	(compare:CC_C
	 (plus:SI (match_operand:SI 1 "s_register_operand" "r,r,r")
		  (match_operand:SI 2 "arm_add_operand" "I,L,r"))
	 (match_dup 1)))
   (set (match_operand:SI 0 "s_register_operand" "=r,r,r")
	(plus:SI (match_dup 1) (match_dup 2)))]
  "TARGET_32BIT"
  "@
   adds%?\\t%0, %1, %2
   subs%?\\t%0, %1, #%n2
   adds%?\\t%0, %1, %2"
  [(set_attr "conds" "set")
   (set_attr "type"  "alus_imm,alus_imm,alus_sreg")]
)

(define_insn "*addsi3_compare_op2"
  [(set (reg:CC_C CC_REGNUM)
	(compare:CC_C
	 (plus:SI (match_operand:SI 1 "s_register_operand" "r,r,r")
		  (match_operand:SI 2 "arm_add_operand" "I,L,r"))
	 (match_dup 2)))
   (set (match_operand:SI 0 "s_register_operand" "=r,r,r")
	(plus:SI (match_dup 1) (match_dup 2)))]
  "TARGET_32BIT"
  "@
   adds%?\\t%0, %1, %2
   subs%?\\t%0, %1, #%n2
   adds%?\\t%0, %1, %2"
  [(set_attr "conds" "set")
   (set_attr "type" "alus_imm,alus_imm,alus_sreg")]
)

(define_insn "*compare_addsi2_op0"
  [(set (reg:CC_C CC_REGNUM)
        (compare:CC_C
          (plus:SI (match_operand:SI 0 "s_register_operand" "l,l,r,r,r")
                   (match_operand:SI 1 "arm_add_operand" "Pv,l,I,L,r"))
          (match_dup 0)))]
  "TARGET_32BIT"
  "@
   cmp%?\\t%0, #%n1
   cmn%?\\t%0, %1
   cmn%?\\t%0, %1
   cmp%?\\t%0, #%n1
   cmn%?\\t%0, %1"
  [(set_attr "conds" "set")
   (set_attr "predicable" "yes")
   (set_attr "arch" "t2,t2,*,*,*")
   (set_attr "predicable_short_it" "yes,yes,no,no,no")
   (set_attr "length" "2,2,4,4,4")
   (set_attr "type" "alus_imm,alus_sreg,alus_imm,alus_imm,alus_sreg")]
)

(define_insn "*compare_addsi2_op1"
  [(set (reg:CC_C CC_REGNUM)
        (compare:CC_C
          (plus:SI (match_operand:SI 0 "s_register_operand" "l,l,r,r,r")
                   (match_operand:SI 1 "arm_add_operand" "Pv,l,I,L,r"))
          (match_dup 1)))]
  "TARGET_32BIT"
  "@
   cmp%?\\t%0, #%n1
   cmn%?\\t%0, %1
   cmn%?\\t%0, %1
   cmp%?\\t%0, #%n1
   cmn%?\\t%0, %1"
  [(set_attr "conds" "set")
   (set_attr "predicable" "yes")
   (set_attr "arch" "t2,t2,*,*,*")
   (set_attr "predicable_short_it" "yes,yes,no,no,no")
   (set_attr "length" "2,2,4,4,4")
   (set_attr "type" "alus_imm,alus_sreg,alus_imm,alus_imm,alus_sreg")]
 )

(define_insn "*addsi3_carryin_<optab>"
  [(set (match_operand:SI 0 "s_register_operand" "=l,r,r")
        (plus:SI (plus:SI (match_operand:SI 1 "s_register_operand" "%l,r,r")
                          (match_operand:SI 2 "arm_not_operand" "0,rI,K"))
                 (LTUGEU:SI (reg:<cnb> CC_REGNUM) (const_int 0))))]
  "TARGET_32BIT"
  "@
   adc%?\\t%0, %1, %2
   adc%?\\t%0, %1, %2
   sbc%?\\t%0, %1, #%B2"
  [(set_attr "conds" "use")
   (set_attr "predicable" "yes")
   (set_attr "arch" "t2,*,*")
   (set_attr "length" "4")
   (set_attr "predicable_short_it" "yes,no,no")
   (set_attr "type" "adc_reg,adc_reg,adc_imm")]
)

(define_insn "*addsi3_carryin_alt2_<optab>"
  [(set (match_operand:SI 0 "s_register_operand" "=l,r,r")
        (plus:SI (plus:SI (LTUGEU:SI (reg:<cnb> CC_REGNUM) (const_int 0))
                          (match_operand:SI 1 "s_register_operand" "%l,r,r"))
                 (match_operand:SI 2 "arm_rhs_operand" "l,rI,K")))]
  "TARGET_32BIT"
  "@
   adc%?\\t%0, %1, %2
   adc%?\\t%0, %1, %2
   sbc%?\\t%0, %1, #%B2"
  [(set_attr "conds" "use")
   (set_attr "predicable" "yes")
   (set_attr "arch" "t2,*,*")
   (set_attr "length" "4")
   (set_attr "predicable_short_it" "yes,no,no")
   (set_attr "type" "adc_reg,adc_reg,adc_imm")]
)

(define_insn "*addsi3_carryin_shift_<optab>"
  [(set (match_operand:SI 0 "s_register_operand" "=r")
	(plus:SI (plus:SI
		  (match_operator:SI 2 "shift_operator"
		    [(match_operand:SI 3 "s_register_operand" "r")
		     (match_operand:SI 4 "reg_or_int_operand" "rM")])
		  (match_operand:SI 1 "s_register_operand" "r"))
		 (LTUGEU:SI (reg:<cnb> CC_REGNUM) (const_int 0))))]
  "TARGET_32BIT"
  "adc%?\\t%0, %1, %3%S2"
  [(set_attr "conds" "use")
   (set_attr "predicable" "yes")
   (set_attr "predicable_short_it" "no")
   (set (attr "type") (if_then_else (match_operand 4 "const_int_operand" "")
		      (const_string "alu_shift_imm")
		      (const_string "alu_shift_reg")))]
)

(define_insn "*addsi3_carryin_clobercc_<optab>"
  [(set (match_operand:SI 0 "s_register_operand" "=r")
	(plus:SI (plus:SI (match_operand:SI 1 "s_register_operand" "%r")
			  (match_operand:SI 2 "arm_rhs_operand" "rI"))
		 (LTUGEU:SI (reg:<cnb> CC_REGNUM) (const_int 0))))
   (clobber (reg:CC CC_REGNUM))]
   "TARGET_32BIT"
   "adcs%?\\t%0, %1, %2"
   [(set_attr "conds" "set")
    (set_attr "type" "adcs_reg")]
)

(define_expand "subv<mode>4"
  [(match_operand:SIDI 0 "register_operand")
   (match_operand:SIDI 1 "register_operand")
   (match_operand:SIDI 2 "register_operand")
   (match_operand 3 "")]
  "TARGET_32BIT"
{
  emit_insn (gen_sub<mode>3_compare1 (operands[0], operands[1], operands[2]));
  arm_gen_unlikely_cbranch (NE, CC_Vmode, operands[3]);

  DONE;
})

(define_expand "usubv<mode>4"
  [(match_operand:SIDI 0 "register_operand")
   (match_operand:SIDI 1 "register_operand")
   (match_operand:SIDI 2 "register_operand")
   (match_operand 3 "")]
  "TARGET_32BIT"
{
  emit_insn (gen_sub<mode>3_compare1 (operands[0], operands[1], operands[2]));
  arm_gen_unlikely_cbranch (LTU, CCmode, operands[3]);

  DONE;
})

(define_insn_and_split "subdi3_compare1"
  [(set (reg:CC CC_REGNUM)
	(compare:CC
	  (match_operand:DI 1 "register_operand" "r")
	  (match_operand:DI 2 "register_operand" "r")))
   (set (match_operand:DI 0 "register_operand" "=&r")
	(minus:DI (match_dup 1) (match_dup 2)))]
  "TARGET_32BIT"
  "#"
  "&& reload_completed"
  [(parallel [(set (reg:CC CC_REGNUM)
		   (compare:CC (match_dup 1) (match_dup 2)))
	      (set (match_dup 0) (minus:SI (match_dup 1) (match_dup 2)))])
   (parallel [(set (reg:CC CC_REGNUM)
		   (compare:CC (match_dup 4) (match_dup 5)))
	     (set (match_dup 3) (minus:SI (minus:SI (match_dup 4) (match_dup 5))
			       (ltu:SI (reg:CC_C CC_REGNUM) (const_int 0))))])]
  {
    operands[3] = gen_highpart (SImode, operands[0]);
    operands[0] = gen_lowpart (SImode, operands[0]);
    operands[4] = gen_highpart (SImode, operands[1]);
    operands[1] = gen_lowpart (SImode, operands[1]);
    operands[5] = gen_highpart (SImode, operands[2]);
    operands[2] = gen_lowpart (SImode, operands[2]);
   }
  [(set_attr "conds" "set")
   (set_attr "length" "8")
   (set_attr "type" "multiple")]
)

(define_insn "subsi3_compare1"
  [(set (reg:CC CC_REGNUM)
	(compare:CC
	  (match_operand:SI 1 "register_operand" "r")
	  (match_operand:SI 2 "register_operand" "r")))
   (set (match_operand:SI 0 "register_operand" "=r")
	(minus:SI (match_dup 1) (match_dup 2)))]
  "TARGET_32BIT"
  "subs%?\\t%0, %1, %2"
  [(set_attr "conds" "set")
   (set_attr "type" "alus_sreg")]
)

(define_insn "*subsi3_carryin"
  [(set (match_operand:SI 0 "s_register_operand" "=r,r")
        (minus:SI (minus:SI (match_operand:SI 1 "reg_or_int_operand" "r,I")
                            (match_operand:SI 2 "s_register_operand" "r,r"))
                  (ltu:SI (reg:CC_C CC_REGNUM) (const_int 0))))]
  "TARGET_32BIT"
  "@
   sbc%?\\t%0, %1, %2
   rsc%?\\t%0, %2, %1"
  [(set_attr "conds" "use")
   (set_attr "arch" "*,a")
   (set_attr "predicable" "yes")
   (set_attr "predicable_short_it" "no")
   (set_attr "type" "adc_reg,adc_imm")]
)

(define_insn "*subsi3_carryin_const"
  [(set (match_operand:SI 0 "s_register_operand" "=r")
        (minus:SI (plus:SI (match_operand:SI 1 "s_register_operand" "r")
                           (match_operand:SI 2 "arm_not_immediate_operand" "K"))
                  (ltu:SI (reg:CC_C CC_REGNUM) (const_int 0))))]
  "TARGET_32BIT"
  "sbc\\t%0, %1, #%B2"
  [(set_attr "conds" "use")
   (set_attr "type" "adc_imm")]
)

(define_insn "*subsi3_carryin_compare"
  [(set (reg:CC CC_REGNUM)
        (compare:CC (match_operand:SI 1 "s_register_operand" "r")
                    (match_operand:SI 2 "s_register_operand" "r")))
   (set (match_operand:SI 0 "s_register_operand" "=r")
        (minus:SI (minus:SI (match_dup 1)
                            (match_dup 2))
                  (ltu:SI (reg:CC_C CC_REGNUM) (const_int 0))))]
  "TARGET_32BIT"
  "sbcs\\t%0, %1, %2"
  [(set_attr "conds" "set")
   (set_attr "type" "adcs_reg")]
)

(define_insn "*subsi3_carryin_compare_const"
  [(set (reg:CC CC_REGNUM)
        (compare:CC (match_operand:SI 1 "reg_or_int_operand" "r")
                    (match_operand:SI 2 "arm_not_operand" "K")))
   (set (match_operand:SI 0 "s_register_operand" "=r")
        (minus:SI (plus:SI (match_dup 1)
                           (match_dup 2))
                  (ltu:SI (reg:CC_C CC_REGNUM) (const_int 0))))]
  "TARGET_32BIT"
  "sbcs\\t%0, %1, #%B2"
  [(set_attr "conds" "set")
   (set_attr "type" "adcs_imm")]
)

(define_insn "*subsi3_carryin_shift"
  [(set (match_operand:SI 0 "s_register_operand" "=r")
	(minus:SI (minus:SI
		  (match_operand:SI 1 "s_register_operand" "r")
                  (match_operator:SI 2 "shift_operator"
                   [(match_operand:SI 3 "s_register_operand" "r")
                    (match_operand:SI 4 "reg_or_int_operand" "rM")]))
                 (ltu:SI (reg:CC_C CC_REGNUM) (const_int 0))))]
  "TARGET_32BIT"
  "sbc%?\\t%0, %1, %3%S2"
  [(set_attr "conds" "use")
   (set_attr "predicable" "yes")
   (set (attr "type") (if_then_else (match_operand 4 "const_int_operand" "")
		      (const_string "alu_shift_imm")
                     (const_string "alu_shift_reg")))]
)

(define_insn "*rsbsi3_carryin_shift"
  [(set (match_operand:SI 0 "s_register_operand" "=r")
	(minus:SI (minus:SI
                  (match_operator:SI 2 "shift_operator"
                   [(match_operand:SI 3 "s_register_operand" "r")
                    (match_operand:SI 4 "reg_or_int_operand" "rM")])
		   (match_operand:SI 1 "s_register_operand" "r"))
                 (ltu:SI (reg:CC_C CC_REGNUM) (const_int 0))))]
  "TARGET_ARM"
  "rsc%?\\t%0, %1, %3%S2"
  [(set_attr "conds" "use")
   (set_attr "predicable" "yes")
   (set (attr "type") (if_then_else (match_operand 4 "const_int_operand" "")
		      (const_string "alu_shift_imm")
		      (const_string "alu_shift_reg")))]
)

; transform ((x << y) - 1) to ~(~(x-1) << y)  Where X is a constant.
(define_split
  [(set (match_operand:SI 0 "s_register_operand" "")
	(plus:SI (ashift:SI (match_operand:SI 1 "const_int_operand" "")
			    (match_operand:SI 2 "s_register_operand" ""))
		 (const_int -1)))
   (clobber (match_operand:SI 3 "s_register_operand" ""))]
  "TARGET_32BIT"
  [(set (match_dup 3) (match_dup 1))
   (set (match_dup 0) (not:SI (ashift:SI (match_dup 3) (match_dup 2))))]
  "
  operands[1] = GEN_INT (~(INTVAL (operands[1]) - 1));
")

(define_expand "addsf3"
  [(set (match_operand:SF          0 "s_register_operand" "")
	(plus:SF (match_operand:SF 1 "s_register_operand" "")
		 (match_operand:SF 2 "s_register_operand" "")))]
  "TARGET_32BIT && TARGET_HARD_FLOAT"
  "
")

(define_expand "adddf3"
  [(set (match_operand:DF          0 "s_register_operand" "")
	(plus:DF (match_operand:DF 1 "s_register_operand" "")
		 (match_operand:DF 2 "s_register_operand" "")))]
  "TARGET_32BIT && TARGET_HARD_FLOAT && !TARGET_VFP_SINGLE"
  "
")

(define_expand "subdi3"
 [(parallel
   [(set (match_operand:DI            0 "s_register_operand" "")
	  (minus:DI (match_operand:DI 1 "s_register_operand" "")
	            (match_operand:DI 2 "s_register_operand" "")))
    (clobber (reg:CC CC_REGNUM))])]
  "TARGET_EITHER"
  "
  if (TARGET_THUMB1)
    {
      if (!REG_P (operands[1]))
        operands[1] = force_reg (DImode, operands[1]);
      if (!REG_P (operands[2]))
        operands[2] = force_reg (DImode, operands[2]);
     }	
  "
)

(define_insn_and_split "*arm_subdi3"
  [(set (match_operand:DI           0 "s_register_operand" "=&r,&r,&r")
	(minus:DI (match_operand:DI 1 "s_register_operand" "0,r,0")
		  (match_operand:DI 2 "s_register_operand" "r,0,0")))
   (clobber (reg:CC CC_REGNUM))]
  "TARGET_32BIT && !TARGET_NEON"
  "#"  ; "subs\\t%Q0, %Q1, %Q2\;sbc\\t%R0, %R1, %R2"
  "&& reload_completed"
  [(parallel [(set (reg:CC CC_REGNUM)
		   (compare:CC (match_dup 1) (match_dup 2)))
	      (set (match_dup 0) (minus:SI (match_dup 1) (match_dup 2)))])
   (set (match_dup 3) (minus:SI (minus:SI (match_dup 4) (match_dup 5))
			       (ltu:SI (reg:CC_C CC_REGNUM) (const_int 0))))]
  {
    operands[3] = gen_highpart (SImode, operands[0]);
    operands[0] = gen_lowpart (SImode, operands[0]);
    operands[4] = gen_highpart (SImode, operands[1]);
    operands[1] = gen_lowpart (SImode, operands[1]);
    operands[5] = gen_highpart (SImode, operands[2]);
    operands[2] = gen_lowpart (SImode, operands[2]);
   }
  [(set_attr "conds" "clob")
   (set_attr "length" "8")
   (set_attr "type" "multiple")]
)

(define_insn_and_split "*subdi_di_zesidi"
  [(set (match_operand:DI           0 "s_register_operand" "=&r,&r")
	(minus:DI (match_operand:DI 1 "s_register_operand"  "0,r")
		  (zero_extend:DI
		   (match_operand:SI 2 "s_register_operand"  "r,r"))))
   (clobber (reg:CC CC_REGNUM))]
  "TARGET_32BIT"
  "#"   ; "subs\\t%Q0, %Q1, %2\;sbc\\t%R0, %R1, #0"
  "&& reload_completed"
  [(parallel [(set (reg:CC CC_REGNUM)
		   (compare:CC (match_dup 1) (match_dup 2)))
	      (set (match_dup 0) (minus:SI (match_dup 1) (match_dup 2)))])
   (set (match_dup 3) (minus:SI (plus:SI (match_dup 4) (match_dup 5))
                                (ltu:SI (reg:CC_C CC_REGNUM) (const_int 0))))]
  {
    operands[3] = gen_highpart (SImode, operands[0]);
    operands[0] = gen_lowpart (SImode, operands[0]);
    operands[4] = gen_highpart (SImode, operands[1]);
    operands[1] = gen_lowpart (SImode, operands[1]);
    operands[5] = GEN_INT (~0);
   }
  [(set_attr "conds" "clob")
   (set_attr "length" "8")
   (set_attr "type" "multiple")]
)

(define_insn_and_split "*subdi_di_sesidi"
  [(set (match_operand:DI            0 "s_register_operand" "=&r,&r")
	(minus:DI (match_operand:DI  1 "s_register_operand"  "0,r")
		  (sign_extend:DI
		   (match_operand:SI 2 "s_register_operand"  "r,r"))))
   (clobber (reg:CC CC_REGNUM))]
  "TARGET_32BIT"
  "#"   ; "subs\\t%Q0, %Q1, %2\;sbc\\t%R0, %R1, %2, asr #31"
  "&& reload_completed"
  [(parallel [(set (reg:CC CC_REGNUM)
		   (compare:CC (match_dup 1) (match_dup 2)))
	      (set (match_dup 0) (minus:SI (match_dup 1) (match_dup 2)))])
   (set (match_dup 3) (minus:SI (minus:SI (match_dup 4)
                                         (ashiftrt:SI (match_dup 2)
                                                      (const_int 31)))
                                (ltu:SI (reg:CC_C CC_REGNUM) (const_int 0))))]
  {
    operands[3] = gen_highpart (SImode, operands[0]);
    operands[0] = gen_lowpart (SImode, operands[0]);
    operands[4] = gen_highpart (SImode, operands[1]);
    operands[1] = gen_lowpart (SImode, operands[1]);
  }
  [(set_attr "conds" "clob")
   (set_attr "length" "8")
   (set_attr "type" "multiple")]
)

(define_insn_and_split "*subdi_zesidi_di"
  [(set (match_operand:DI            0 "s_register_operand" "=&r,&r")
	(minus:DI (zero_extend:DI
		   (match_operand:SI 2 "s_register_operand"  "r,r"))
		  (match_operand:DI  1 "s_register_operand" "0,r")))
   (clobber (reg:CC CC_REGNUM))]
  "TARGET_ARM"
  "#"   ; "rsbs\\t%Q0, %Q1, %2\;rsc\\t%R0, %R1, #0"
        ; is equivalent to:
        ; "subs\\t%Q0, %2, %Q1\;rsc\\t%R0, %R1, #0"
  "&& reload_completed"
  [(parallel [(set (reg:CC CC_REGNUM)
		   (compare:CC (match_dup 2) (match_dup 1)))
	      (set (match_dup 0) (minus:SI (match_dup 2) (match_dup 1)))])
   (set (match_dup 3) (minus:SI (minus:SI (const_int 0) (match_dup 4))
			       (ltu:SI (reg:CC_C CC_REGNUM) (const_int 0))))]
  {
    operands[3] = gen_highpart (SImode, operands[0]);
    operands[0] = gen_lowpart (SImode, operands[0]);
    operands[4] = gen_highpart (SImode, operands[1]);
    operands[1] = gen_lowpart (SImode, operands[1]);
  }
  [(set_attr "conds" "clob")
   (set_attr "length" "8")
   (set_attr "type" "multiple")]
)

(define_insn_and_split "*subdi_sesidi_di"
  [(set (match_operand:DI            0 "s_register_operand" "=&r,&r")
	(minus:DI (sign_extend:DI
		   (match_operand:SI 2 "s_register_operand"   "r,r"))
		  (match_operand:DI  1 "s_register_operand"  "0,r")))
   (clobber (reg:CC CC_REGNUM))]
  "TARGET_ARM"
  "#"   ; "rsbs\\t%Q0, %Q1, %2\;rsc\\t%R0, %R1, %2, asr #31"
        ; is equivalent to:
        ; "subs\\t%Q0, %2, %Q1\;rsc\\t%R0, %R1, %2, asr #31"
  "&& reload_completed"
  [(parallel [(set (reg:CC CC_REGNUM)
		   (compare:CC (match_dup 2) (match_dup 1)))
	      (set (match_dup 0) (minus:SI (match_dup 2) (match_dup 1)))])
   (set (match_dup 3) (minus:SI (minus:SI
                                (ashiftrt:SI (match_dup 2)
                                             (const_int 31))
                                (match_dup 4))
			       (ltu:SI (reg:CC_C CC_REGNUM) (const_int 0))))]
  {
    operands[3] = gen_highpart (SImode, operands[0]);
    operands[0] = gen_lowpart (SImode, operands[0]);
    operands[4] = gen_highpart (SImode, operands[1]);
    operands[1] = gen_lowpart (SImode, operands[1]);
  }
  [(set_attr "conds" "clob")
   (set_attr "length" "8")
   (set_attr "type" "multiple")]
)

(define_insn_and_split "*subdi_zesidi_zesidi"
  [(set (match_operand:DI            0 "s_register_operand" "=r")
	(minus:DI (zero_extend:DI
		   (match_operand:SI 1 "s_register_operand"  "r"))
		  (zero_extend:DI
		   (match_operand:SI 2 "s_register_operand"  "r"))))
   (clobber (reg:CC CC_REGNUM))]
  "TARGET_32BIT"
  "#"   ; "subs\\t%Q0, %1, %2\;sbc\\t%R0, %1, %1"
  "&& reload_completed"
  [(parallel [(set (reg:CC CC_REGNUM)
		   (compare:CC (match_dup 1) (match_dup 2)))
	      (set (match_dup 0) (minus:SI (match_dup 1) (match_dup 2)))])
   (set (match_dup 3) (minus:SI (minus:SI (match_dup 1) (match_dup 1))
			       (ltu:SI (reg:CC_C CC_REGNUM) (const_int 0))))]
  {
       operands[3] = gen_highpart (SImode, operands[0]);
       operands[0] = gen_lowpart (SImode, operands[0]);
  }
  [(set_attr "conds" "clob")
   (set_attr "length" "8")
   (set_attr "type" "multiple")]
)

(define_expand "subsi3"
  [(set (match_operand:SI           0 "s_register_operand" "")
	(minus:SI (match_operand:SI 1 "reg_or_int_operand" "")
		  (match_operand:SI 2 "s_register_operand" "")))]
  "TARGET_EITHER"
  "
  if (CONST_INT_P (operands[1]))
    {
      if (TARGET_32BIT)
        {
	  if (DONT_EARLY_SPLIT_CONSTANT (INTVAL (operands[1]), MINUS))
	    operands[1] = force_reg (SImode, operands[1]);
	  else
	    {
	      arm_split_constant (MINUS, SImode, NULL_RTX,
				  INTVAL (operands[1]), operands[0],
				  operands[2],
				  optimize && can_create_pseudo_p ());
	      DONE;
	    }
	}
      else /* TARGET_THUMB1 */
        operands[1] = force_reg (SImode, operands[1]);
    }
  "
)

; ??? Check Thumb-2 split length
(define_insn_and_split "*arm_subsi3_insn"
  [(set (match_operand:SI           0 "s_register_operand" "=l,l ,l ,l ,r,r,r,rk,r")
	(minus:SI (match_operand:SI 1 "reg_or_int_operand" "l ,0 ,l ,Pz,I,r,r,k ,?n")
		  (match_operand:SI 2 "reg_or_int_operand" "l ,Py,Pd,l ,r,I,r,r ,r")))]
  "TARGET_32BIT"
  "@
   sub%?\\t%0, %1, %2
   sub%?\\t%0, %2
   sub%?\\t%0, %1, %2
   rsb%?\\t%0, %2, %1
   rsb%?\\t%0, %2, %1
   sub%?\\t%0, %1, %2
   sub%?\\t%0, %1, %2
   sub%?\\t%0, %1, %2
   #"
  "&& (CONST_INT_P (operands[1])
       && !const_ok_for_arm (INTVAL (operands[1])))"
  [(clobber (const_int 0))]
  "
  arm_split_constant (MINUS, SImode, curr_insn,
                      INTVAL (operands[1]), operands[0], operands[2], 0);
  DONE;
  "
  [(set_attr "length" "4,4,4,4,4,4,4,4,16")
   (set_attr "arch" "t2,t2,t2,t2,*,*,*,*,*")
   (set_attr "predicable" "yes")
   (set_attr "predicable_short_it" "yes,yes,yes,yes,no,no,no,no,no")
   (set_attr "type" "alu_sreg,alu_sreg,alu_sreg,alu_sreg,alu_imm,alu_imm,alu_sreg,alu_sreg,multiple")]
)

(define_peephole2
  [(match_scratch:SI 3 "r")
   (set (match_operand:SI 0 "arm_general_register_operand" "")
	(minus:SI (match_operand:SI 1 "const_int_operand" "")
		  (match_operand:SI 2 "arm_general_register_operand" "")))]
  "TARGET_32BIT
   && !const_ok_for_arm (INTVAL (operands[1]))
   && const_ok_for_arm (~INTVAL (operands[1]))"
  [(set (match_dup 3) (match_dup 1))
   (set (match_dup 0) (minus:SI (match_dup 3) (match_dup 2)))]
  ""
)

(define_insn "subsi3_compare0"
  [(set (reg:CC_NOOV CC_REGNUM)
	(compare:CC_NOOV
	 (minus:SI (match_operand:SI 1 "arm_rhs_operand" "r,r,I")
		   (match_operand:SI 2 "arm_rhs_operand" "I,r,r"))
	 (const_int 0)))
   (set (match_operand:SI 0 "s_register_operand" "=r,r,r")
	(minus:SI (match_dup 1) (match_dup 2)))]
  "TARGET_32BIT"
  "@
   subs%?\\t%0, %1, %2
   subs%?\\t%0, %1, %2
   rsbs%?\\t%0, %2, %1"
  [(set_attr "conds" "set")
   (set_attr "type"  "alus_imm,alus_sreg,alus_sreg")]
)

(define_insn "subsi3_compare"
  [(set (reg:CC CC_REGNUM)
	(compare:CC (match_operand:SI 1 "arm_rhs_operand" "r,r,I")
		    (match_operand:SI 2 "arm_rhs_operand" "I,r,r")))
   (set (match_operand:SI 0 "s_register_operand" "=r,r,r")
	(minus:SI (match_dup 1) (match_dup 2)))]
  "TARGET_32BIT"
  "@
   subs%?\\t%0, %1, %2
   subs%?\\t%0, %1, %2
   rsbs%?\\t%0, %2, %1"
  [(set_attr "conds" "set")
   (set_attr "type" "alus_imm,alus_sreg,alus_sreg")]
)

(define_expand "subsf3"
  [(set (match_operand:SF           0 "s_register_operand" "")
	(minus:SF (match_operand:SF 1 "s_register_operand" "")
		  (match_operand:SF 2 "s_register_operand" "")))]
  "TARGET_32BIT && TARGET_HARD_FLOAT"
  "
")

(define_expand "subdf3"
  [(set (match_operand:DF           0 "s_register_operand" "")
	(minus:DF (match_operand:DF 1 "s_register_operand" "")
		  (match_operand:DF 2 "s_register_operand" "")))]
  "TARGET_32BIT && TARGET_HARD_FLOAT && !TARGET_VFP_SINGLE"
  "
")


;; Multiplication insns

(define_expand "mulhi3"
  [(set (match_operand:HI 0 "s_register_operand" "")
	(mult:HI (match_operand:HI 1 "s_register_operand" "")
		 (match_operand:HI 2 "s_register_operand" "")))]
  "TARGET_DSP_MULTIPLY"
  "
  {
    rtx result = gen_reg_rtx (SImode);
    emit_insn (gen_mulhisi3 (result, operands[1], operands[2]));
    emit_move_insn (operands[0], gen_lowpart (HImode, result));
    DONE;
  }"
)

(define_expand "mulsi3"
  [(set (match_operand:SI          0 "s_register_operand" "")
	(mult:SI (match_operand:SI 2 "s_register_operand" "")
		 (match_operand:SI 1 "s_register_operand" "")))]
  "TARGET_EITHER"
  ""
)

;; Use `&' and then `0' to prevent the operands 0 and 1 being the same
(define_insn "*arm_mulsi3"
  [(set (match_operand:SI          0 "s_register_operand" "=&r,&r")
	(mult:SI (match_operand:SI 2 "s_register_operand" "r,r")
		 (match_operand:SI 1 "s_register_operand" "%0,r")))]
  "TARGET_32BIT && !arm_arch6"
  "mul%?\\t%0, %2, %1"
  [(set_attr "type" "mul")
   (set_attr "predicable" "yes")]
)

(define_insn "*arm_mulsi3_v6"
  [(set (match_operand:SI          0 "s_register_operand" "=l,l,r")
	(mult:SI (match_operand:SI 1 "s_register_operand" "0,l,r")
		 (match_operand:SI 2 "s_register_operand" "l,0,r")))]
  "TARGET_32BIT && arm_arch6"
  "mul%?\\t%0, %1, %2"
  [(set_attr "type" "mul")
   (set_attr "predicable" "yes")
   (set_attr "arch" "t2,t2,*")
   (set_attr "length" "4")
   (set_attr "predicable_short_it" "yes,yes,no")]
)

(define_insn "*mulsi3_compare0"
  [(set (reg:CC_NOOV CC_REGNUM)
	(compare:CC_NOOV (mult:SI
			  (match_operand:SI 2 "s_register_operand" "r,r")
			  (match_operand:SI 1 "s_register_operand" "%0,r"))
			 (const_int 0)))
   (set (match_operand:SI 0 "s_register_operand" "=&r,&r")
	(mult:SI (match_dup 2) (match_dup 1)))]
  "TARGET_ARM && !arm_arch6"
  "muls%?\\t%0, %2, %1"
  [(set_attr "conds" "set")
   (set_attr "type" "muls")]
)

(define_insn "*mulsi3_compare0_v6"
  [(set (reg:CC_NOOV CC_REGNUM)
	(compare:CC_NOOV (mult:SI
			  (match_operand:SI 2 "s_register_operand" "r")
			  (match_operand:SI 1 "s_register_operand" "r"))
			 (const_int 0)))
   (set (match_operand:SI 0 "s_register_operand" "=r")
	(mult:SI (match_dup 2) (match_dup 1)))]
  "TARGET_ARM && arm_arch6 && optimize_size"
  "muls%?\\t%0, %2, %1"
  [(set_attr "conds" "set")
   (set_attr "type" "muls")]
)

(define_insn "*mulsi_compare0_scratch"
  [(set (reg:CC_NOOV CC_REGNUM)
	(compare:CC_NOOV (mult:SI
			  (match_operand:SI 2 "s_register_operand" "r,r")
			  (match_operand:SI 1 "s_register_operand" "%0,r"))
			 (const_int 0)))
   (clobber (match_scratch:SI 0 "=&r,&r"))]
  "TARGET_ARM && !arm_arch6"
  "muls%?\\t%0, %2, %1"
  [(set_attr "conds" "set")
   (set_attr "type" "muls")]
)

(define_insn "*mulsi_compare0_scratch_v6"
  [(set (reg:CC_NOOV CC_REGNUM)
	(compare:CC_NOOV (mult:SI
			  (match_operand:SI 2 "s_register_operand" "r")
			  (match_operand:SI 1 "s_register_operand" "r"))
			 (const_int 0)))
   (clobber (match_scratch:SI 0 "=r"))]
  "TARGET_ARM && arm_arch6 && optimize_size"
  "muls%?\\t%0, %2, %1"
  [(set_attr "conds" "set")
   (set_attr "type" "muls")]
)

;; Unnamed templates to match MLA instruction.

(define_insn "*mulsi3addsi"
  [(set (match_operand:SI 0 "s_register_operand" "=&r,&r,&r,&r")
	(plus:SI
	  (mult:SI (match_operand:SI 2 "s_register_operand" "r,r,r,r")
		   (match_operand:SI 1 "s_register_operand" "%0,r,0,r"))
	  (match_operand:SI 3 "s_register_operand" "r,r,0,0")))]
  "TARGET_32BIT && !arm_arch6"
  "mla%?\\t%0, %2, %1, %3"
  [(set_attr "type" "mla")
   (set_attr "predicable" "yes")]
)

(define_insn "*mulsi3addsi_v6"
  [(set (match_operand:SI 0 "s_register_operand" "=r")
	(plus:SI
	  (mult:SI (match_operand:SI 2 "s_register_operand" "r")
		   (match_operand:SI 1 "s_register_operand" "r"))
	  (match_operand:SI 3 "s_register_operand" "r")))]
  "TARGET_32BIT && arm_arch6"
  "mla%?\\t%0, %2, %1, %3"
  [(set_attr "type" "mla")
   (set_attr "predicable" "yes")
   (set_attr "predicable_short_it" "no")]
)

(define_insn "*mulsi3addsi_compare0"
  [(set (reg:CC_NOOV CC_REGNUM)
	(compare:CC_NOOV
	 (plus:SI (mult:SI
		   (match_operand:SI 2 "s_register_operand" "r,r,r,r")
		   (match_operand:SI 1 "s_register_operand" "%0,r,0,r"))
		  (match_operand:SI 3 "s_register_operand" "r,r,0,0"))
	 (const_int 0)))
   (set (match_operand:SI 0 "s_register_operand" "=&r,&r,&r,&r")
	(plus:SI (mult:SI (match_dup 2) (match_dup 1))
		 (match_dup 3)))]
  "TARGET_ARM && arm_arch6"
  "mlas%?\\t%0, %2, %1, %3"
  [(set_attr "conds" "set")
   (set_attr "type" "mlas")]
)

(define_insn "*mulsi3addsi_compare0_v6"
  [(set (reg:CC_NOOV CC_REGNUM)
	(compare:CC_NOOV
	 (plus:SI (mult:SI
		   (match_operand:SI 2 "s_register_operand" "r")
		   (match_operand:SI 1 "s_register_operand" "r"))
		  (match_operand:SI 3 "s_register_operand" "r"))
	 (const_int 0)))
   (set (match_operand:SI 0 "s_register_operand" "=r")
	(plus:SI (mult:SI (match_dup 2) (match_dup 1))
		 (match_dup 3)))]
  "TARGET_ARM && arm_arch6 && optimize_size"
  "mlas%?\\t%0, %2, %1, %3"
  [(set_attr "conds" "set")
   (set_attr "type" "mlas")]
)

(define_insn "*mulsi3addsi_compare0_scratch"
  [(set (reg:CC_NOOV CC_REGNUM)
	(compare:CC_NOOV
	 (plus:SI (mult:SI
		   (match_operand:SI 2 "s_register_operand" "r,r,r,r")
		   (match_operand:SI 1 "s_register_operand" "%0,r,0,r"))
		  (match_operand:SI 3 "s_register_operand" "?r,r,0,0"))
	 (const_int 0)))
   (clobber (match_scratch:SI 0 "=&r,&r,&r,&r"))]
  "TARGET_ARM && !arm_arch6"
  "mlas%?\\t%0, %2, %1, %3"
  [(set_attr "conds" "set")
   (set_attr "type" "mlas")]
)

(define_insn "*mulsi3addsi_compare0_scratch_v6"
  [(set (reg:CC_NOOV CC_REGNUM)
	(compare:CC_NOOV
	 (plus:SI (mult:SI
		   (match_operand:SI 2 "s_register_operand" "r")
		   (match_operand:SI 1 "s_register_operand" "r"))
		  (match_operand:SI 3 "s_register_operand" "r"))
	 (const_int 0)))
   (clobber (match_scratch:SI 0 "=r"))]
  "TARGET_ARM && arm_arch6 && optimize_size"
  "mlas%?\\t%0, %2, %1, %3"
  [(set_attr "conds" "set")
   (set_attr "type" "mlas")]
)

(define_insn "*mulsi3subsi"
  [(set (match_operand:SI 0 "s_register_operand" "=r")
	(minus:SI
	  (match_operand:SI 3 "s_register_operand" "r")
	  (mult:SI (match_operand:SI 2 "s_register_operand" "r")
		   (match_operand:SI 1 "s_register_operand" "r"))))]
  "TARGET_32BIT && arm_arch_thumb2"
  "mls%?\\t%0, %2, %1, %3"
  [(set_attr "type" "mla")
   (set_attr "predicable" "yes")
   (set_attr "predicable_short_it" "no")]
)

(define_expand "maddsidi4"
  [(set (match_operand:DI 0 "s_register_operand" "")
	(plus:DI
	 (mult:DI
	  (sign_extend:DI (match_operand:SI 1 "s_register_operand" ""))
	  (sign_extend:DI (match_operand:SI 2 "s_register_operand" "")))
	 (match_operand:DI 3 "s_register_operand" "")))]
  "TARGET_32BIT && arm_arch3m"
  "")

(define_insn "*mulsidi3adddi"
  [(set (match_operand:DI 0 "s_register_operand" "=&r")
	(plus:DI
	 (mult:DI
	  (sign_extend:DI (match_operand:SI 2 "s_register_operand" "%r"))
	  (sign_extend:DI (match_operand:SI 3 "s_register_operand" "r")))
	 (match_operand:DI 1 "s_register_operand" "0")))]
  "TARGET_32BIT && arm_arch3m && !arm_arch6"
  "smlal%?\\t%Q0, %R0, %3, %2"
  [(set_attr "type" "smlal")
   (set_attr "predicable" "yes")]
)

(define_insn "*mulsidi3adddi_v6"
  [(set (match_operand:DI 0 "s_register_operand" "=r")
	(plus:DI
	 (mult:DI
	  (sign_extend:DI (match_operand:SI 2 "s_register_operand" "r"))
	  (sign_extend:DI (match_operand:SI 3 "s_register_operand" "r")))
	 (match_operand:DI 1 "s_register_operand" "0")))]
  "TARGET_32BIT && arm_arch6"
  "smlal%?\\t%Q0, %R0, %3, %2"
  [(set_attr "type" "smlal")
   (set_attr "predicable" "yes")
   (set_attr "predicable_short_it" "no")]
)

;; 32x32->64 widening multiply.
;; As with mulsi3, the only difference between the v3-5 and v6+
;; versions of these patterns is the requirement that the output not
;; overlap the inputs, but that still means we have to have a named
;; expander and two different starred insns.

(define_expand "mulsidi3"
  [(set (match_operand:DI 0 "s_register_operand" "")
	(mult:DI
	 (sign_extend:DI (match_operand:SI 1 "s_register_operand" ""))
	 (sign_extend:DI (match_operand:SI 2 "s_register_operand" ""))))]
  "TARGET_32BIT && arm_arch3m"
  ""
)

(define_insn "*mulsidi3_nov6"
  [(set (match_operand:DI 0 "s_register_operand" "=&r")
	(mult:DI
	 (sign_extend:DI (match_operand:SI 1 "s_register_operand" "%r"))
	 (sign_extend:DI (match_operand:SI 2 "s_register_operand" "r"))))]
  "TARGET_32BIT && arm_arch3m && !arm_arch6"
  "smull%?\\t%Q0, %R0, %1, %2"
  [(set_attr "type" "smull")
   (set_attr "predicable" "yes")]
)

(define_insn "*mulsidi3_v6"
  [(set (match_operand:DI 0 "s_register_operand" "=r")
	(mult:DI
	 (sign_extend:DI (match_operand:SI 1 "s_register_operand" "r"))
	 (sign_extend:DI (match_operand:SI 2 "s_register_operand" "r"))))]
  "TARGET_32BIT && arm_arch6"
  "smull%?\\t%Q0, %R0, %1, %2"
  [(set_attr "type" "smull")
   (set_attr "predicable" "yes")
   (set_attr "predicable_short_it" "no")]
)

(define_expand "umulsidi3"
  [(set (match_operand:DI 0 "s_register_operand" "")
	(mult:DI
	 (zero_extend:DI (match_operand:SI 1 "s_register_operand" ""))
	 (zero_extend:DI (match_operand:SI 2 "s_register_operand" ""))))]
  "TARGET_32BIT && arm_arch3m"
  ""
)

(define_insn "*umulsidi3_nov6"
  [(set (match_operand:DI 0 "s_register_operand" "=&r")
	(mult:DI
	 (zero_extend:DI (match_operand:SI 1 "s_register_operand" "%r"))
	 (zero_extend:DI (match_operand:SI 2 "s_register_operand" "r"))))]
  "TARGET_32BIT && arm_arch3m && !arm_arch6"
  "umull%?\\t%Q0, %R0, %1, %2"
  [(set_attr "type" "umull")
   (set_attr "predicable" "yes")]
)

(define_insn "*umulsidi3_v6"
  [(set (match_operand:DI 0 "s_register_operand" "=r")
	(mult:DI
	 (zero_extend:DI (match_operand:SI 1 "s_register_operand" "r"))
	 (zero_extend:DI (match_operand:SI 2 "s_register_operand" "r"))))]
  "TARGET_32BIT && arm_arch6"
  "umull%?\\t%Q0, %R0, %1, %2"
  [(set_attr "type" "umull")
   (set_attr "predicable" "yes")
   (set_attr "predicable_short_it" "no")]
)

(define_expand "umaddsidi4"
  [(set (match_operand:DI 0 "s_register_operand" "")
	(plus:DI
	 (mult:DI
	  (zero_extend:DI (match_operand:SI 1 "s_register_operand" ""))
	  (zero_extend:DI (match_operand:SI 2 "s_register_operand" "")))
	 (match_operand:DI 3 "s_register_operand" "")))]
  "TARGET_32BIT && arm_arch3m"
  "")

(define_insn "*umulsidi3adddi"
  [(set (match_operand:DI 0 "s_register_operand" "=&r")
	(plus:DI
	 (mult:DI
	  (zero_extend:DI (match_operand:SI 2 "s_register_operand" "%r"))
	  (zero_extend:DI (match_operand:SI 3 "s_register_operand" "r")))
	 (match_operand:DI 1 "s_register_operand" "0")))]
  "TARGET_32BIT && arm_arch3m && !arm_arch6"
  "umlal%?\\t%Q0, %R0, %3, %2"
  [(set_attr "type" "umlal")
   (set_attr "predicable" "yes")]
)

(define_insn "*umulsidi3adddi_v6"
  [(set (match_operand:DI 0 "s_register_operand" "=r")
	(plus:DI
	 (mult:DI
	  (zero_extend:DI (match_operand:SI 2 "s_register_operand" "r"))
	  (zero_extend:DI (match_operand:SI 3 "s_register_operand" "r")))
	 (match_operand:DI 1 "s_register_operand" "0")))]
  "TARGET_32BIT && arm_arch6"
  "umlal%?\\t%Q0, %R0, %3, %2"
  [(set_attr "type" "umlal")
   (set_attr "predicable" "yes")
   (set_attr "predicable_short_it" "no")]
)

(define_expand "smulsi3_highpart"
  [(parallel
    [(set (match_operand:SI 0 "s_register_operand" "")
	  (truncate:SI
	   (lshiftrt:DI
	    (mult:DI
	     (sign_extend:DI (match_operand:SI 1 "s_register_operand" ""))
	     (sign_extend:DI (match_operand:SI 2 "s_register_operand" "")))
	    (const_int 32))))
     (clobber (match_scratch:SI 3 ""))])]
  "TARGET_32BIT && arm_arch3m"
  ""
)

(define_insn "*smulsi3_highpart_nov6"
  [(set (match_operand:SI 0 "s_register_operand" "=&r,&r")
	(truncate:SI
	 (lshiftrt:DI
	  (mult:DI
	   (sign_extend:DI (match_operand:SI 1 "s_register_operand" "%0,r"))
	   (sign_extend:DI (match_operand:SI 2 "s_register_operand" "r,r")))
	  (const_int 32))))
   (clobber (match_scratch:SI 3 "=&r,&r"))]
  "TARGET_32BIT && arm_arch3m && !arm_arch6"
  "smull%?\\t%3, %0, %2, %1"
  [(set_attr "type" "smull")
   (set_attr "predicable" "yes")]
)

(define_insn "*smulsi3_highpart_v6"
  [(set (match_operand:SI 0 "s_register_operand" "=r")
	(truncate:SI
	 (lshiftrt:DI
	  (mult:DI
	   (sign_extend:DI (match_operand:SI 1 "s_register_operand" "r"))
	   (sign_extend:DI (match_operand:SI 2 "s_register_operand" "r")))
	  (const_int 32))))
   (clobber (match_scratch:SI 3 "=r"))]
  "TARGET_32BIT && arm_arch6"
  "smull%?\\t%3, %0, %2, %1"
  [(set_attr "type" "smull")
   (set_attr "predicable" "yes")
   (set_attr "predicable_short_it" "no")]
)

(define_expand "umulsi3_highpart"
  [(parallel
    [(set (match_operand:SI 0 "s_register_operand" "")
	  (truncate:SI
	   (lshiftrt:DI
	    (mult:DI
	     (zero_extend:DI (match_operand:SI 1 "s_register_operand" ""))
	      (zero_extend:DI (match_operand:SI 2 "s_register_operand" "")))
	    (const_int 32))))
     (clobber (match_scratch:SI 3 ""))])]
  "TARGET_32BIT && arm_arch3m"
  ""
)

(define_insn "*umulsi3_highpart_nov6"
  [(set (match_operand:SI 0 "s_register_operand" "=&r,&r")
	(truncate:SI
	 (lshiftrt:DI
	  (mult:DI
	   (zero_extend:DI (match_operand:SI 1 "s_register_operand" "%0,r"))
	   (zero_extend:DI (match_operand:SI 2 "s_register_operand" "r,r")))
	  (const_int 32))))
   (clobber (match_scratch:SI 3 "=&r,&r"))]
  "TARGET_32BIT && arm_arch3m && !arm_arch6"
  "umull%?\\t%3, %0, %2, %1"
  [(set_attr "type" "umull")
   (set_attr "predicable" "yes")]
)

(define_insn "*umulsi3_highpart_v6"
  [(set (match_operand:SI 0 "s_register_operand" "=r")
	(truncate:SI
	 (lshiftrt:DI
	  (mult:DI
	   (zero_extend:DI (match_operand:SI 1 "s_register_operand" "r"))
	   (zero_extend:DI (match_operand:SI 2 "s_register_operand" "r")))
	  (const_int 32))))
   (clobber (match_scratch:SI 3 "=r"))]
  "TARGET_32BIT && arm_arch6"
  "umull%?\\t%3, %0, %2, %1"
  [(set_attr "type" "umull")
   (set_attr "predicable" "yes")
   (set_attr "predicable_short_it" "no")]
)

(define_insn "mulhisi3"
  [(set (match_operand:SI 0 "s_register_operand" "=r")
	(mult:SI (sign_extend:SI
		  (match_operand:HI 1 "s_register_operand" "%r"))
		 (sign_extend:SI
		  (match_operand:HI 2 "s_register_operand" "r"))))]
  "TARGET_DSP_MULTIPLY"
  "smulbb%?\\t%0, %1, %2"
  [(set_attr "type" "smulxy")
   (set_attr "predicable" "yes")]
)

(define_insn "*mulhisi3tb"
  [(set (match_operand:SI 0 "s_register_operand" "=r")
	(mult:SI (ashiftrt:SI
		  (match_operand:SI 1 "s_register_operand" "r")
		  (const_int 16))
		 (sign_extend:SI
		  (match_operand:HI 2 "s_register_operand" "r"))))]
  "TARGET_DSP_MULTIPLY"
  "smultb%?\\t%0, %1, %2"
  [(set_attr "type" "smulxy")
   (set_attr "predicable" "yes")
   (set_attr "predicable_short_it" "no")]
)

(define_insn "*mulhisi3bt"
  [(set (match_operand:SI 0 "s_register_operand" "=r")
	(mult:SI (sign_extend:SI
		  (match_operand:HI 1 "s_register_operand" "r"))
		 (ashiftrt:SI
		  (match_operand:SI 2 "s_register_operand" "r")
		  (const_int 16))))]
  "TARGET_DSP_MULTIPLY"
  "smulbt%?\\t%0, %1, %2"
  [(set_attr "type" "smulxy")
   (set_attr "predicable" "yes")
   (set_attr "predicable_short_it" "no")]
)

(define_insn "*mulhisi3tt"
  [(set (match_operand:SI 0 "s_register_operand" "=r")
	(mult:SI (ashiftrt:SI
		  (match_operand:SI 1 "s_register_operand" "r")
		  (const_int 16))
		 (ashiftrt:SI
		  (match_operand:SI 2 "s_register_operand" "r")
		  (const_int 16))))]
  "TARGET_DSP_MULTIPLY"
  "smultt%?\\t%0, %1, %2"
  [(set_attr "type" "smulxy")
   (set_attr "predicable" "yes")
   (set_attr "predicable_short_it" "no")]
)

(define_insn "maddhisi4"
  [(set (match_operand:SI 0 "s_register_operand" "=r")
	(plus:SI (mult:SI (sign_extend:SI
			   (match_operand:HI 1 "s_register_operand" "r"))
			  (sign_extend:SI
			   (match_operand:HI 2 "s_register_operand" "r")))
		 (match_operand:SI 3 "s_register_operand" "r")))]
  "TARGET_DSP_MULTIPLY"
  "smlabb%?\\t%0, %1, %2, %3"
  [(set_attr "type" "smlaxy")
   (set_attr "predicable" "yes")
   (set_attr "predicable_short_it" "no")]
)

;; Note: there is no maddhisi4ibt because this one is canonical form
(define_insn "*maddhisi4tb"
  [(set (match_operand:SI 0 "s_register_operand" "=r")
	(plus:SI (mult:SI (ashiftrt:SI
			   (match_operand:SI 1 "s_register_operand" "r")
			   (const_int 16))
			  (sign_extend:SI
			   (match_operand:HI 2 "s_register_operand" "r")))
		 (match_operand:SI 3 "s_register_operand" "r")))]
  "TARGET_DSP_MULTIPLY"
  "smlatb%?\\t%0, %1, %2, %3"
  [(set_attr "type" "smlaxy")
   (set_attr "predicable" "yes")
   (set_attr "predicable_short_it" "no")]
)

(define_insn "*maddhisi4tt"
  [(set (match_operand:SI 0 "s_register_operand" "=r")
	(plus:SI (mult:SI (ashiftrt:SI
			   (match_operand:SI 1 "s_register_operand" "r")
			   (const_int 16))
			  (ashiftrt:SI
			   (match_operand:SI 2 "s_register_operand" "r")
			   (const_int 16)))
		 (match_operand:SI 3 "s_register_operand" "r")))]
  "TARGET_DSP_MULTIPLY"
  "smlatt%?\\t%0, %1, %2, %3"
  [(set_attr "type" "smlaxy")
   (set_attr "predicable" "yes")
   (set_attr "predicable_short_it" "no")]
)

(define_insn "maddhidi4"
  [(set (match_operand:DI 0 "s_register_operand" "=r")
	(plus:DI
	  (mult:DI (sign_extend:DI
		    (match_operand:HI 1 "s_register_operand" "r"))
		   (sign_extend:DI
		    (match_operand:HI 2 "s_register_operand" "r")))
	  (match_operand:DI 3 "s_register_operand" "0")))]
  "TARGET_DSP_MULTIPLY"
  "smlalbb%?\\t%Q0, %R0, %1, %2"
  [(set_attr "type" "smlalxy")
   (set_attr "predicable" "yes")
   (set_attr "predicable_short_it" "no")])

;; Note: there is no maddhidi4ibt because this one is canonical form
(define_insn "*maddhidi4tb"
  [(set (match_operand:DI 0 "s_register_operand" "=r")
	(plus:DI
	  (mult:DI (sign_extend:DI
		    (ashiftrt:SI
		     (match_operand:SI 1 "s_register_operand" "r")
		     (const_int 16)))
		   (sign_extend:DI
		    (match_operand:HI 2 "s_register_operand" "r")))
	  (match_operand:DI 3 "s_register_operand" "0")))]
  "TARGET_DSP_MULTIPLY"
  "smlaltb%?\\t%Q0, %R0, %1, %2"
  [(set_attr "type" "smlalxy")
   (set_attr "predicable" "yes")
   (set_attr "predicable_short_it" "no")])

(define_insn "*maddhidi4tt"
  [(set (match_operand:DI 0 "s_register_operand" "=r")
	(plus:DI
	  (mult:DI (sign_extend:DI
		    (ashiftrt:SI
		     (match_operand:SI 1 "s_register_operand" "r")
		     (const_int 16)))
		   (sign_extend:DI
		    (ashiftrt:SI
		     (match_operand:SI 2 "s_register_operand" "r")
		     (const_int 16))))
	  (match_operand:DI 3 "s_register_operand" "0")))]
  "TARGET_DSP_MULTIPLY"
  "smlaltt%?\\t%Q0, %R0, %1, %2"
  [(set_attr "type" "smlalxy")
   (set_attr "predicable" "yes")
   (set_attr "predicable_short_it" "no")])

(define_expand "mulsf3"
  [(set (match_operand:SF          0 "s_register_operand" "")
	(mult:SF (match_operand:SF 1 "s_register_operand" "")
		 (match_operand:SF 2 "s_register_operand" "")))]
  "TARGET_32BIT && TARGET_HARD_FLOAT"
  "
")

(define_expand "muldf3"
  [(set (match_operand:DF          0 "s_register_operand" "")
	(mult:DF (match_operand:DF 1 "s_register_operand" "")
		 (match_operand:DF 2 "s_register_operand" "")))]
  "TARGET_32BIT && TARGET_HARD_FLOAT && !TARGET_VFP_SINGLE"
  "
")

;; Division insns

(define_expand "divsf3"
  [(set (match_operand:SF 0 "s_register_operand" "")
	(div:SF (match_operand:SF 1 "s_register_operand" "")
		(match_operand:SF 2 "s_register_operand" "")))]
  "TARGET_32BIT && TARGET_HARD_FLOAT && TARGET_VFP"
  "")

(define_expand "divdf3"
  [(set (match_operand:DF 0 "s_register_operand" "")
	(div:DF (match_operand:DF 1 "s_register_operand" "")
		(match_operand:DF 2 "s_register_operand" "")))]
  "TARGET_32BIT && TARGET_HARD_FLOAT && TARGET_VFP_DOUBLE"
  "")

;; Boolean and,ior,xor insns

;; Split up double word logical operations

;; Split up simple DImode logical operations.  Simply perform the logical
;; operation on the upper and lower halves of the registers.
(define_split
  [(set (match_operand:DI 0 "s_register_operand" "")
	(match_operator:DI 6 "logical_binary_operator"
	  [(match_operand:DI 1 "s_register_operand" "")
	   (match_operand:DI 2 "s_register_operand" "")]))]
  "TARGET_32BIT && reload_completed
   && ! (TARGET_NEON && IS_VFP_REGNUM (REGNO (operands[0])))
   && ! IS_IWMMXT_REGNUM (REGNO (operands[0]))"
  [(set (match_dup 0) (match_op_dup:SI 6 [(match_dup 1) (match_dup 2)]))
   (set (match_dup 3) (match_op_dup:SI 6 [(match_dup 4) (match_dup 5)]))]
  "
  {
    operands[3] = gen_highpart (SImode, operands[0]);
    operands[0] = gen_lowpart (SImode, operands[0]);
    operands[4] = gen_highpart (SImode, operands[1]);
    operands[1] = gen_lowpart (SImode, operands[1]);
    operands[5] = gen_highpart (SImode, operands[2]);
    operands[2] = gen_lowpart (SImode, operands[2]);
  }"
)

(define_split
  [(set (match_operand:DI 0 "s_register_operand" "")
	(match_operator:DI 6 "logical_binary_operator"
	  [(sign_extend:DI (match_operand:SI 2 "s_register_operand" ""))
	   (match_operand:DI 1 "s_register_operand" "")]))]
  "TARGET_32BIT && reload_completed"
  [(set (match_dup 0) (match_op_dup:SI 6 [(match_dup 1) (match_dup 2)]))
   (set (match_dup 3) (match_op_dup:SI 6
			[(ashiftrt:SI (match_dup 2) (const_int 31))
			 (match_dup 4)]))]
  "
  {
    operands[3] = gen_highpart (SImode, operands[0]);
    operands[0] = gen_lowpart (SImode, operands[0]);
    operands[4] = gen_highpart (SImode, operands[1]);
    operands[1] = gen_lowpart (SImode, operands[1]);
    operands[5] = gen_highpart (SImode, operands[2]);
    operands[2] = gen_lowpart (SImode, operands[2]);
  }"
)

;; The zero extend of operand 2 means we can just copy the high part of
;; operand1 into operand0.
(define_split
  [(set (match_operand:DI 0 "s_register_operand" "")
	(ior:DI
	  (zero_extend:DI (match_operand:SI 2 "s_register_operand" ""))
	  (match_operand:DI 1 "s_register_operand" "")))]
  "TARGET_32BIT && operands[0] != operands[1] && reload_completed"
  [(set (match_dup 0) (ior:SI (match_dup 1) (match_dup 2)))
   (set (match_dup 3) (match_dup 4))]
  "
  {
    operands[4] = gen_highpart (SImode, operands[1]);
    operands[3] = gen_highpart (SImode, operands[0]);
    operands[0] = gen_lowpart (SImode, operands[0]);
    operands[1] = gen_lowpart (SImode, operands[1]);
  }"
)

;; The zero extend of operand 2 means we can just copy the high part of
;; operand1 into operand0.
(define_split
  [(set (match_operand:DI 0 "s_register_operand" "")
	(xor:DI
	  (zero_extend:DI (match_operand:SI 2 "s_register_operand" ""))
	  (match_operand:DI 1 "s_register_operand" "")))]
  "TARGET_32BIT && operands[0] != operands[1] && reload_completed"
  [(set (match_dup 0) (xor:SI (match_dup 1) (match_dup 2)))
   (set (match_dup 3) (match_dup 4))]
  "
  {
    operands[4] = gen_highpart (SImode, operands[1]);
    operands[3] = gen_highpart (SImode, operands[0]);
    operands[0] = gen_lowpart (SImode, operands[0]);
    operands[1] = gen_lowpart (SImode, operands[1]);
  }"
)

(define_expand "anddi3"
  [(set (match_operand:DI         0 "s_register_operand" "")
	(and:DI (match_operand:DI 1 "s_register_operand" "")
		(match_operand:DI 2 "neon_inv_logic_op2" "")))]
  "TARGET_32BIT"
  ""
)

(define_insn_and_split "*anddi3_insn"
  [(set (match_operand:DI         0 "s_register_operand"     "=w,w ,&r,&r,&r,&r,?w,?w")
        (and:DI (match_operand:DI 1 "s_register_operand"     "%w,0 ,0 ,r ,0 ,r ,w ,0")
                (match_operand:DI 2 "arm_anddi_operand_neon" "w ,DL,r ,r ,De,De,w ,DL")))]
  "TARGET_32BIT && !TARGET_IWMMXT"
{
  switch (which_alternative)
    {
    case 0: /* fall through */
    case 6: return "vand\t%P0, %P1, %P2";
    case 1: /* fall through */
    case 7: return neon_output_logic_immediate ("vand", &operands[2],
                    DImode, 1, VALID_NEON_QREG_MODE (DImode));
    case 2:
    case 3:
    case 4:
    case 5: /* fall through */
      return "#";
    default: gcc_unreachable ();
    }
}
  "TARGET_32BIT && !TARGET_IWMMXT && reload_completed
   && !(IS_VFP_REGNUM (REGNO (operands[0])))"
  [(set (match_dup 3) (match_dup 4))
   (set (match_dup 5) (match_dup 6))]
  "
  {
    operands[3] = gen_lowpart (SImode, operands[0]);
    operands[5] = gen_highpart (SImode, operands[0]);

    operands[4] = simplify_gen_binary (AND, SImode,
                                           gen_lowpart (SImode, operands[1]),
                                           gen_lowpart (SImode, operands[2]));
    operands[6] = simplify_gen_binary (AND, SImode,
                                           gen_highpart (SImode, operands[1]),
                                           gen_highpart_mode (SImode, DImode, operands[2]));

  }"
  [(set_attr "type" "neon_logic,neon_logic,multiple,multiple,\
                     multiple,multiple,neon_logic,neon_logic")
   (set_attr "arch" "neon_for_64bits,neon_for_64bits,*,*,*,*,
                     avoid_neon_for_64bits,avoid_neon_for_64bits")
   (set_attr "length" "*,*,8,8,8,8,*,*")
  ]
)

(define_insn_and_split "*anddi_zesidi_di"
  [(set (match_operand:DI 0 "s_register_operand" "=&r,&r")
	(and:DI (zero_extend:DI
		 (match_operand:SI 2 "s_register_operand" "r,r"))
		(match_operand:DI 1 "s_register_operand" "0,r")))]
  "TARGET_32BIT"
  "#"
  "TARGET_32BIT && reload_completed"
  ; The zero extend of operand 2 clears the high word of the output
  ; operand.
  [(set (match_dup 0) (and:SI (match_dup 1) (match_dup 2)))
   (set (match_dup 3) (const_int 0))]
  "
  {
    operands[3] = gen_highpart (SImode, operands[0]);
    operands[0] = gen_lowpart (SImode, operands[0]);
    operands[1] = gen_lowpart (SImode, operands[1]);
  }"
  [(set_attr "length" "8")
   (set_attr "type" "multiple")]
)

(define_insn "*anddi_sesdi_di"
  [(set (match_operand:DI          0 "s_register_operand" "=&r,&r")
	(and:DI (sign_extend:DI
		 (match_operand:SI 2 "s_register_operand" "r,r"))
		(match_operand:DI  1 "s_register_operand" "0,r")))]
  "TARGET_32BIT"
  "#"
  [(set_attr "length" "8")
   (set_attr "type" "multiple")]
)

(define_expand "andsi3"
  [(set (match_operand:SI         0 "s_register_operand" "")
	(and:SI (match_operand:SI 1 "s_register_operand" "")
		(match_operand:SI 2 "reg_or_int_operand" "")))]
  "TARGET_EITHER"
  "
  if (TARGET_32BIT)
    {
      if (CONST_INT_P (operands[2]))
        {
	  if (INTVAL (operands[2]) == 255 && arm_arch6)
	    {
	      operands[1] = convert_to_mode (QImode, operands[1], 1);
	      emit_insn (gen_thumb2_zero_extendqisi2_v6 (operands[0],
							 operands[1]));
	      DONE;
	    }
	  else if (DONT_EARLY_SPLIT_CONSTANT (INTVAL (operands[2]), AND))
	    operands[2] = force_reg (SImode, operands[2]);
	  else
	    {
	      arm_split_constant (AND, SImode, NULL_RTX,
				  INTVAL (operands[2]), operands[0],
				  operands[1],
				  optimize && can_create_pseudo_p ());

	      DONE;
	    }
        }
    }
  else /* TARGET_THUMB1 */
    {
      if (!CONST_INT_P (operands[2]))
        {
          rtx tmp = force_reg (SImode, operands[2]);
	  if (rtx_equal_p (operands[0], operands[1]))
	    operands[2] = tmp;
	  else
	    {
              operands[2] = operands[1];
              operands[1] = tmp;
	    }
        }
      else
        {
          int i;
	  
          if (((unsigned HOST_WIDE_INT) ~INTVAL (operands[2])) < 256)
  	    {
	      operands[2] = force_reg (SImode,
				       GEN_INT (~INTVAL (operands[2])));
	      
	      emit_insn (gen_thumb1_bicsi3 (operands[0], operands[2], operands[1]));
	      
	      DONE;
	    }

          for (i = 9; i <= 31; i++)
	    {
	      if ((HOST_WIDE_INT_1 << i) - 1 == INTVAL (operands[2]))
	        {
	          emit_insn (gen_extzv (operands[0], operands[1], GEN_INT (i),
			 	        const0_rtx));
	          DONE;
	        }
	      else if ((HOST_WIDE_INT_1 << i) - 1
		       == ~INTVAL (operands[2]))
	        {
	          rtx shift = GEN_INT (i);
	          rtx reg = gen_reg_rtx (SImode);
		
	          emit_insn (gen_lshrsi3 (reg, operands[1], shift));
	          emit_insn (gen_ashlsi3 (operands[0], reg, shift));
		  
	          DONE;
	        }
	    }

          operands[2] = force_reg (SImode, operands[2]);
        }
    }
  "
)

; ??? Check split length for Thumb-2
(define_insn_and_split "*arm_andsi3_insn"
  [(set (match_operand:SI         0 "s_register_operand" "=r,l,r,r,r")
	(and:SI (match_operand:SI 1 "s_register_operand" "%r,0,r,r,r")
		(match_operand:SI 2 "reg_or_int_operand" "I,l,K,r,?n")))]
  "TARGET_32BIT"
  "@
   and%?\\t%0, %1, %2
   and%?\\t%0, %1, %2
   bic%?\\t%0, %1, #%B2
   and%?\\t%0, %1, %2
   #"
  "TARGET_32BIT
   && CONST_INT_P (operands[2])
   && !(const_ok_for_arm (INTVAL (operands[2]))
	|| const_ok_for_arm (~INTVAL (operands[2])))"
  [(clobber (const_int 0))]
  "
  arm_split_constant  (AND, SImode, curr_insn, 
	               INTVAL (operands[2]), operands[0], operands[1], 0);
  DONE;
  "
  [(set_attr "length" "4,4,4,4,16")
   (set_attr "predicable" "yes")
   (set_attr "predicable_short_it" "no,yes,no,no,no")
   (set_attr "type" "logic_imm,logic_imm,logic_reg,logic_reg,logic_imm")]
)

(define_insn "*andsi3_compare0"
  [(set (reg:CC_NOOV CC_REGNUM)
	(compare:CC_NOOV
	 (and:SI (match_operand:SI 1 "s_register_operand" "r,r,r")
		 (match_operand:SI 2 "arm_not_operand" "I,K,r"))
	 (const_int 0)))
   (set (match_operand:SI          0 "s_register_operand" "=r,r,r")
	(and:SI (match_dup 1) (match_dup 2)))]
  "TARGET_32BIT"
  "@
   ands%?\\t%0, %1, %2
   bics%?\\t%0, %1, #%B2
   ands%?\\t%0, %1, %2"
  [(set_attr "conds" "set")
   (set_attr "type" "logics_imm,logics_imm,logics_reg")]
)

(define_insn "*andsi3_compare0_scratch"
  [(set (reg:CC_NOOV CC_REGNUM)
	(compare:CC_NOOV
	 (and:SI (match_operand:SI 0 "s_register_operand" "r,r,r")
		 (match_operand:SI 1 "arm_not_operand" "I,K,r"))
	 (const_int 0)))
   (clobber (match_scratch:SI 2 "=X,r,X"))]
  "TARGET_32BIT"
  "@
   tst%?\\t%0, %1
   bics%?\\t%2, %0, #%B1
   tst%?\\t%0, %1"
  [(set_attr "conds" "set")
   (set_attr "type"  "logics_imm,logics_imm,logics_reg")]
)

(define_insn "*zeroextractsi_compare0_scratch"
  [(set (reg:CC_NOOV CC_REGNUM)
	(compare:CC_NOOV (zero_extract:SI
			  (match_operand:SI 0 "s_register_operand" "r")
			  (match_operand 1 "const_int_operand" "n")
			  (match_operand 2 "const_int_operand" "n"))
			 (const_int 0)))]
  "TARGET_32BIT
  && (INTVAL (operands[2]) >= 0 && INTVAL (operands[2]) < 32
      && INTVAL (operands[1]) > 0 
      && INTVAL (operands[1]) + (INTVAL (operands[2]) & 1) <= 8
      && INTVAL (operands[1]) + INTVAL (operands[2]) <= 32)"
  "*
  operands[1] = GEN_INT (((1 << INTVAL (operands[1])) - 1)
			 << INTVAL (operands[2]));
  output_asm_insn (\"tst%?\\t%0, %1\", operands);
  return \"\";
  "
  [(set_attr "conds" "set")
   (set_attr "predicable" "yes")
   (set_attr "predicable_short_it" "no")
   (set_attr "type" "logics_imm")]
)

(define_insn_and_split "*ne_zeroextractsi"
  [(set (match_operand:SI 0 "s_register_operand" "=r")
	(ne:SI (zero_extract:SI
		(match_operand:SI 1 "s_register_operand" "r")
		(match_operand:SI 2 "const_int_operand" "n")
		(match_operand:SI 3 "const_int_operand" "n"))
	       (const_int 0)))
   (clobber (reg:CC CC_REGNUM))]
  "TARGET_32BIT
   && (INTVAL (operands[3]) >= 0 && INTVAL (operands[3]) < 32
       && INTVAL (operands[2]) > 0 
       && INTVAL (operands[2]) + (INTVAL (operands[3]) & 1) <= 8
       && INTVAL (operands[2]) + INTVAL (operands[3]) <= 32)"
  "#"
  "TARGET_32BIT
   && (INTVAL (operands[3]) >= 0 && INTVAL (operands[3]) < 32
       && INTVAL (operands[2]) > 0 
       && INTVAL (operands[2]) + (INTVAL (operands[3]) & 1) <= 8
       && INTVAL (operands[2]) + INTVAL (operands[3]) <= 32)"
  [(parallel [(set (reg:CC_NOOV CC_REGNUM)
		   (compare:CC_NOOV (and:SI (match_dup 1) (match_dup 2))
				    (const_int 0)))
	      (set (match_dup 0) (and:SI (match_dup 1) (match_dup 2)))])
   (set (match_dup 0)
	(if_then_else:SI (eq (reg:CC_NOOV CC_REGNUM) (const_int 0))
			 (match_dup 0) (const_int 1)))]
  "
  operands[2] = GEN_INT (((1 << INTVAL (operands[2])) - 1)
			 << INTVAL (operands[3])); 
  "
  [(set_attr "conds" "clob")
   (set (attr "length")
	(if_then_else (eq_attr "is_thumb" "yes")
		      (const_int 12)
		      (const_int 8)))
   (set_attr "type" "multiple")]
)

(define_insn_and_split "*ne_zeroextractsi_shifted"
  [(set (match_operand:SI 0 "s_register_operand" "=r")
	(ne:SI (zero_extract:SI
		(match_operand:SI 1 "s_register_operand" "r")
		(match_operand:SI 2 "const_int_operand" "n")
		(const_int 0))
	       (const_int 0)))
   (clobber (reg:CC CC_REGNUM))]
  "TARGET_ARM"
  "#"
  "TARGET_ARM"
  [(parallel [(set (reg:CC_NOOV CC_REGNUM)
		   (compare:CC_NOOV (ashift:SI (match_dup 1) (match_dup 2))
				    (const_int 0)))
	      (set (match_dup 0) (ashift:SI (match_dup 1) (match_dup 2)))])
   (set (match_dup 0)
	(if_then_else:SI (eq (reg:CC_NOOV CC_REGNUM) (const_int 0))
			 (match_dup 0) (const_int 1)))]
  "
  operands[2] = GEN_INT (32 - INTVAL (operands[2]));
  "
  [(set_attr "conds" "clob")
   (set_attr "length" "8")
   (set_attr "type" "multiple")]
)

(define_insn_and_split "*ite_ne_zeroextractsi"
  [(set (match_operand:SI 0 "s_register_operand" "=r")
	(if_then_else:SI (ne (zero_extract:SI
			      (match_operand:SI 1 "s_register_operand" "r")
			      (match_operand:SI 2 "const_int_operand" "n")
			      (match_operand:SI 3 "const_int_operand" "n"))
			     (const_int 0))
			 (match_operand:SI 4 "arm_not_operand" "rIK")
			 (const_int 0)))
   (clobber (reg:CC CC_REGNUM))]
  "TARGET_ARM
   && (INTVAL (operands[3]) >= 0 && INTVAL (operands[3]) < 32
       && INTVAL (operands[2]) > 0 
       && INTVAL (operands[2]) + (INTVAL (operands[3]) & 1) <= 8
       && INTVAL (operands[2]) + INTVAL (operands[3]) <= 32)
   && !reg_overlap_mentioned_p (operands[0], operands[4])"
  "#"
  "TARGET_ARM
   && (INTVAL (operands[3]) >= 0 && INTVAL (operands[3]) < 32
       && INTVAL (operands[2]) > 0 
       && INTVAL (operands[2]) + (INTVAL (operands[3]) & 1) <= 8
       && INTVAL (operands[2]) + INTVAL (operands[3]) <= 32)
   && !reg_overlap_mentioned_p (operands[0], operands[4])"
  [(parallel [(set (reg:CC_NOOV CC_REGNUM)
		   (compare:CC_NOOV (and:SI (match_dup 1) (match_dup 2))
				    (const_int 0)))
	      (set (match_dup 0) (and:SI (match_dup 1) (match_dup 2)))])
   (set (match_dup 0)
	(if_then_else:SI (eq (reg:CC_NOOV CC_REGNUM) (const_int 0))
			 (match_dup 0) (match_dup 4)))]
  "
  operands[2] = GEN_INT (((1 << INTVAL (operands[2])) - 1)
			 << INTVAL (operands[3])); 
  "
  [(set_attr "conds" "clob")
   (set_attr "length" "8")
   (set_attr "type" "multiple")]
)

(define_insn_and_split "*ite_ne_zeroextractsi_shifted"
  [(set (match_operand:SI 0 "s_register_operand" "=r")
	(if_then_else:SI (ne (zero_extract:SI
			      (match_operand:SI 1 "s_register_operand" "r")
			      (match_operand:SI 2 "const_int_operand" "n")
			      (const_int 0))
			     (const_int 0))
			 (match_operand:SI 3 "arm_not_operand" "rIK")
			 (const_int 0)))
   (clobber (reg:CC CC_REGNUM))]
  "TARGET_ARM && !reg_overlap_mentioned_p (operands[0], operands[3])"
  "#"
  "TARGET_ARM && !reg_overlap_mentioned_p (operands[0], operands[3])"
  [(parallel [(set (reg:CC_NOOV CC_REGNUM)
		   (compare:CC_NOOV (ashift:SI (match_dup 1) (match_dup 2))
				    (const_int 0)))
	      (set (match_dup 0) (ashift:SI (match_dup 1) (match_dup 2)))])
   (set (match_dup 0)
	(if_then_else:SI (eq (reg:CC_NOOV CC_REGNUM) (const_int 0))
			 (match_dup 0) (match_dup 3)))]
  "
  operands[2] = GEN_INT (32 - INTVAL (operands[2]));
  "
  [(set_attr "conds" "clob")
   (set_attr "length" "8")
   (set_attr "type" "multiple")]
)

;; ??? Use Thumb-2 has bitfield insert/extract instructions.
(define_split
  [(set (match_operand:SI 0 "s_register_operand" "")
	(match_operator:SI 1 "shiftable_operator"
	 [(zero_extract:SI (match_operand:SI 2 "s_register_operand" "")
			   (match_operand:SI 3 "const_int_operand" "")
			   (match_operand:SI 4 "const_int_operand" ""))
	  (match_operand:SI 5 "s_register_operand" "")]))
   (clobber (match_operand:SI 6 "s_register_operand" ""))]
  "TARGET_ARM"
  [(set (match_dup 6) (ashift:SI (match_dup 2) (match_dup 3)))
   (set (match_dup 0)
	(match_op_dup 1
	 [(lshiftrt:SI (match_dup 6) (match_dup 4))
	  (match_dup 5)]))]
  "{
     HOST_WIDE_INT temp = INTVAL (operands[3]);

     operands[3] = GEN_INT (32 - temp - INTVAL (operands[4]));
     operands[4] = GEN_INT (32 - temp);
   }"
)
  
(define_split
  [(set (match_operand:SI 0 "s_register_operand" "")
	(match_operator:SI 1 "shiftable_operator"
	 [(sign_extract:SI (match_operand:SI 2 "s_register_operand" "")
			   (match_operand:SI 3 "const_int_operand" "")
			   (match_operand:SI 4 "const_int_operand" ""))
	  (match_operand:SI 5 "s_register_operand" "")]))
   (clobber (match_operand:SI 6 "s_register_operand" ""))]
  "TARGET_ARM"
  [(set (match_dup 6) (ashift:SI (match_dup 2) (match_dup 3)))
   (set (match_dup 0)
	(match_op_dup 1
	 [(ashiftrt:SI (match_dup 6) (match_dup 4))
	  (match_dup 5)]))]
  "{
     HOST_WIDE_INT temp = INTVAL (operands[3]);

     operands[3] = GEN_INT (32 - temp - INTVAL (operands[4]));
     operands[4] = GEN_INT (32 - temp);
   }"
)
  
;;; ??? This pattern is bogus.  If operand3 has bits outside the range
;;; represented by the bitfield, then this will produce incorrect results.
;;; Somewhere, the value needs to be truncated.  On targets like the m68k,
;;; which have a real bit-field insert instruction, the truncation happens
;;; in the bit-field insert instruction itself.  Since arm does not have a
;;; bit-field insert instruction, we would have to emit code here to truncate
;;; the value before we insert.  This loses some of the advantage of having
;;; this insv pattern, so this pattern needs to be reevalutated.

(define_expand "insv"
  [(set (zero_extract (match_operand 0 "nonimmediate_operand" "")
                      (match_operand 1 "general_operand" "")
                      (match_operand 2 "general_operand" ""))
        (match_operand 3 "reg_or_int_operand" ""))]
  "TARGET_ARM || arm_arch_thumb2"
  "
  {
    int start_bit = INTVAL (operands[2]);
    int width = INTVAL (operands[1]);
    HOST_WIDE_INT mask = (HOST_WIDE_INT_1 << width) - 1;
    rtx target, subtarget;

    if (arm_arch_thumb2)
      {
        if (unaligned_access && MEM_P (operands[0])
	    && s_register_operand (operands[3], GET_MODE (operands[3]))
	    && (width == 16 || width == 32) && (start_bit % BITS_PER_UNIT) == 0)
	  {
	    rtx base_addr;

	    if (BYTES_BIG_ENDIAN)
	      start_bit = GET_MODE_BITSIZE (GET_MODE (operands[3])) - width
			  - start_bit;

	    if (width == 32)
	      {
	        base_addr = adjust_address (operands[0], SImode,
					    start_bit / BITS_PER_UNIT);
		emit_insn (gen_unaligned_storesi (base_addr, operands[3]));
	      }
	    else
	      {
	        rtx tmp = gen_reg_rtx (HImode);

	        base_addr = adjust_address (operands[0], HImode,
					    start_bit / BITS_PER_UNIT);
		emit_move_insn (tmp, gen_lowpart (HImode, operands[3]));
		emit_insn (gen_unaligned_storehi (base_addr, tmp));
	      }
	    DONE;
	  }
	else if (s_register_operand (operands[0], GET_MODE (operands[0])))
	  {
	    bool use_bfi = TRUE;

	    if (CONST_INT_P (operands[3]))
	      {
		HOST_WIDE_INT val = INTVAL (operands[3]) & mask;

		if (val == 0)
		  {
		    emit_insn (gen_insv_zero (operands[0], operands[1],
					      operands[2]));
		    DONE;
		  }

		/* See if the set can be done with a single orr instruction.  */
		if (val == mask && const_ok_for_arm (val << start_bit))
		  use_bfi = FALSE;
	      }

	    if (use_bfi)
	      {
		if (!REG_P (operands[3]))
		  operands[3] = force_reg (SImode, operands[3]);

		emit_insn (gen_insv_t2 (operands[0], operands[1], operands[2],
					operands[3]));
		DONE;
	      }
	  }
	else
	  FAIL;
      }

    if (!s_register_operand (operands[0], GET_MODE (operands[0])))
      FAIL;

    target = copy_rtx (operands[0]);
    /* Avoid using a subreg as a subtarget, and avoid writing a paradoxical 
       subreg as the final target.  */
    if (GET_CODE (target) == SUBREG)
      {
	subtarget = gen_reg_rtx (SImode);
	if (GET_MODE_SIZE (GET_MODE (SUBREG_REG (target)))
	    < GET_MODE_SIZE (SImode))
	  target = SUBREG_REG (target);
      }
    else
      subtarget = target;    

    if (CONST_INT_P (operands[3]))
      {
	/* Since we are inserting a known constant, we may be able to
	   reduce the number of bits that we have to clear so that
	   the mask becomes simple.  */
	/* ??? This code does not check to see if the new mask is actually
	   simpler.  It may not be.  */
	rtx op1 = gen_reg_rtx (SImode);
	/* ??? Truncate operand3 to fit in the bitfield.  See comment before
	   start of this pattern.  */
	HOST_WIDE_INT op3_value = mask & INTVAL (operands[3]);
	HOST_WIDE_INT mask2 = ((mask & ~op3_value) << start_bit);

	emit_insn (gen_andsi3 (op1, operands[0],
			       gen_int_mode (~mask2, SImode)));
	emit_insn (gen_iorsi3 (subtarget, op1,
			       gen_int_mode (op3_value << start_bit, SImode)));
      }
    else if (start_bit == 0
	     && !(const_ok_for_arm (mask)
		  || const_ok_for_arm (~mask)))
      {
	/* A Trick, since we are setting the bottom bits in the word,
	   we can shift operand[3] up, operand[0] down, OR them together
	   and rotate the result back again.  This takes 3 insns, and
	   the third might be mergeable into another op.  */
	/* The shift up copes with the possibility that operand[3] is
           wider than the bitfield.  */
	rtx op0 = gen_reg_rtx (SImode);
	rtx op1 = gen_reg_rtx (SImode);

	emit_insn (gen_ashlsi3 (op0, operands[3], GEN_INT (32 - width)));
	emit_insn (gen_lshrsi3 (op1, operands[0], operands[1]));
	emit_insn (gen_iorsi3  (op1, op1, op0));
	emit_insn (gen_rotlsi3 (subtarget, op1, operands[1]));
      }
    else if ((width + start_bit == 32)
	     && !(const_ok_for_arm (mask)
		  || const_ok_for_arm (~mask)))
      {
	/* Similar trick, but slightly less efficient.  */

	rtx op0 = gen_reg_rtx (SImode);
	rtx op1 = gen_reg_rtx (SImode);

	emit_insn (gen_ashlsi3 (op0, operands[3], GEN_INT (32 - width)));
	emit_insn (gen_ashlsi3 (op1, operands[0], operands[1]));
	emit_insn (gen_lshrsi3 (op1, op1, operands[1]));
	emit_insn (gen_iorsi3 (subtarget, op1, op0));
      }
    else
      {
	rtx op0 = gen_int_mode (mask, SImode);
	rtx op1 = gen_reg_rtx (SImode);
	rtx op2 = gen_reg_rtx (SImode);

	if (!(const_ok_for_arm (mask) || const_ok_for_arm (~mask)))
	  {
	    rtx tmp = gen_reg_rtx (SImode);

	    emit_insn (gen_movsi (tmp, op0));
	    op0 = tmp;
	  }

	/* Mask out any bits in operand[3] that are not needed.  */
	   emit_insn (gen_andsi3 (op1, operands[3], op0));

	if (CONST_INT_P (op0)
	    && (const_ok_for_arm (mask << start_bit)
		|| const_ok_for_arm (~(mask << start_bit))))
	  {
	    op0 = gen_int_mode (~(mask << start_bit), SImode);
	    emit_insn (gen_andsi3 (op2, operands[0], op0));
	  }
	else
	  {
	    if (CONST_INT_P (op0))
	      {
		rtx tmp = gen_reg_rtx (SImode);

		emit_insn (gen_movsi (tmp, op0));
		op0 = tmp;
	      }

	    if (start_bit != 0)
	      emit_insn (gen_ashlsi3 (op0, op0, operands[2]));
	    
	    emit_insn (gen_andsi_notsi_si (op2, operands[0], op0));
	  }

	if (start_bit != 0)
          emit_insn (gen_ashlsi3 (op1, op1, operands[2]));

	emit_insn (gen_iorsi3 (subtarget, op1, op2));
      }

    if (subtarget != target)
      {
	/* If TARGET is still a SUBREG, then it must be wider than a word,
	   so we must be careful only to set the subword we were asked to.  */
	if (GET_CODE (target) == SUBREG)
	  emit_move_insn (target, subtarget);
	else
	  emit_move_insn (target, gen_lowpart (GET_MODE (target), subtarget));
      }

    DONE;
  }"
)

(define_insn "insv_zero"
  [(set (zero_extract:SI (match_operand:SI 0 "s_register_operand" "+r")
                         (match_operand:SI 1 "const_int_M_operand" "M")
                         (match_operand:SI 2 "const_int_M_operand" "M"))
        (const_int 0))]
  "arm_arch_thumb2"
  "bfc%?\t%0, %2, %1"
  [(set_attr "length" "4")
   (set_attr "predicable" "yes")
   (set_attr "predicable_short_it" "no")
   (set_attr "type" "bfm")]
)

(define_insn "insv_t2"
  [(set (zero_extract:SI (match_operand:SI 0 "s_register_operand" "+r")
                         (match_operand:SI 1 "const_int_M_operand" "M")
                         (match_operand:SI 2 "const_int_M_operand" "M"))
        (match_operand:SI 3 "s_register_operand" "r"))]
  "arm_arch_thumb2"
  "bfi%?\t%0, %3, %2, %1"
  [(set_attr "length" "4")
   (set_attr "predicable" "yes")
   (set_attr "predicable_short_it" "no")
   (set_attr "type" "bfm")]
)

; constants for op 2 will never be given to these patterns.
(define_insn_and_split "*anddi_notdi_di"
  [(set (match_operand:DI 0 "s_register_operand" "=&r,&r")
	(and:DI (not:DI (match_operand:DI 1 "s_register_operand" "0,r"))
		(match_operand:DI 2 "s_register_operand" "r,0")))]
  "TARGET_32BIT"
  "#"
  "TARGET_32BIT && reload_completed
   && ! (TARGET_NEON && IS_VFP_REGNUM (REGNO (operands[0])))
   && ! IS_IWMMXT_REGNUM (REGNO (operands[0]))"
  [(set (match_dup 0) (and:SI (not:SI (match_dup 1)) (match_dup 2)))
   (set (match_dup 3) (and:SI (not:SI (match_dup 4)) (match_dup 5)))]
  "
  {
    operands[3] = gen_highpart (SImode, operands[0]);
    operands[0] = gen_lowpart (SImode, operands[0]);
    operands[4] = gen_highpart (SImode, operands[1]);
    operands[1] = gen_lowpart (SImode, operands[1]);
    operands[5] = gen_highpart (SImode, operands[2]);
    operands[2] = gen_lowpart (SImode, operands[2]);
  }"
  [(set_attr "length" "8")
   (set_attr "predicable" "yes")
   (set_attr "type" "multiple")]
)

(define_insn_and_split "*anddi_notzesidi_di"
  [(set (match_operand:DI 0 "s_register_operand" "=&r,&r")
	(and:DI (not:DI (zero_extend:DI
			 (match_operand:SI 2 "s_register_operand" "r,r")))
		(match_operand:DI 1 "s_register_operand" "0,?r")))]
  "TARGET_32BIT"
  "@
   bic%?\\t%Q0, %Q1, %2
   #"
  ; (not (zero_extend ...)) allows us to just copy the high word from
  ; operand1 to operand0.
  "TARGET_32BIT
   && reload_completed
   && operands[0] != operands[1]"
  [(set (match_dup 0) (and:SI (not:SI (match_dup 2)) (match_dup 1)))
   (set (match_dup 3) (match_dup 4))]
  "
  {
    operands[3] = gen_highpart (SImode, operands[0]);
    operands[0] = gen_lowpart (SImode, operands[0]);
    operands[4] = gen_highpart (SImode, operands[1]);
    operands[1] = gen_lowpart (SImode, operands[1]);
  }"
  [(set_attr "length" "4,8")
   (set_attr "predicable" "yes")
   (set_attr "predicable_short_it" "no")
   (set_attr "type" "multiple")]
)

(define_insn_and_split "*anddi_notdi_zesidi"
  [(set (match_operand:DI 0 "s_register_operand" "=r")
        (and:DI (not:DI (match_operand:DI 2 "s_register_operand" "r"))
                (zero_extend:DI
                 (match_operand:SI 1 "s_register_operand" "r"))))]
  "TARGET_32BIT"
  "#"
  "TARGET_32BIT && reload_completed"
  [(set (match_dup 0) (and:SI (not:SI (match_dup 2)) (match_dup 1)))
   (set (match_dup 3) (const_int 0))]
  "
  {
    operands[3] = gen_highpart (SImode, operands[0]);
    operands[0] = gen_lowpart (SImode, operands[0]);
    operands[2] = gen_lowpart (SImode, operands[2]);
  }"
  [(set_attr "length" "8")
   (set_attr "predicable" "yes")
   (set_attr "predicable_short_it" "no")
   (set_attr "type" "multiple")]
)

(define_insn_and_split "*anddi_notsesidi_di"
  [(set (match_operand:DI 0 "s_register_operand" "=&r,&r")
	(and:DI (not:DI (sign_extend:DI
			 (match_operand:SI 2 "s_register_operand" "r,r")))
		(match_operand:DI 1 "s_register_operand" "0,r")))]
  "TARGET_32BIT"
  "#"
  "TARGET_32BIT && reload_completed"
  [(set (match_dup 0) (and:SI (not:SI (match_dup 2)) (match_dup 1)))
   (set (match_dup 3) (and:SI (not:SI
				(ashiftrt:SI (match_dup 2) (const_int 31)))
			       (match_dup 4)))]
  "
  {
    operands[3] = gen_highpart (SImode, operands[0]);
    operands[0] = gen_lowpart (SImode, operands[0]);
    operands[4] = gen_highpart (SImode, operands[1]);
    operands[1] = gen_lowpart (SImode, operands[1]);
  }"
  [(set_attr "length" "8")
   (set_attr "predicable" "yes")
   (set_attr "predicable_short_it" "no")
   (set_attr "type" "multiple")]
)

(define_insn "andsi_notsi_si"
  [(set (match_operand:SI 0 "s_register_operand" "=r")
	(and:SI (not:SI (match_operand:SI 2 "s_register_operand" "r"))
		(match_operand:SI 1 "s_register_operand" "r")))]
  "TARGET_32BIT"
  "bic%?\\t%0, %1, %2"
  [(set_attr "predicable" "yes")
   (set_attr "predicable_short_it" "no")
   (set_attr "type" "logic_reg")]
)

(define_insn "andsi_not_shiftsi_si"
  [(set (match_operand:SI 0 "s_register_operand" "=r")
	(and:SI (not:SI (match_operator:SI 4 "shift_operator"
			 [(match_operand:SI 2 "s_register_operand" "r")
			  (match_operand:SI 3 "arm_rhs_operand" "rM")]))
		(match_operand:SI 1 "s_register_operand" "r")))]
  "TARGET_ARM"
  "bic%?\\t%0, %1, %2%S4"
  [(set_attr "predicable" "yes")
   (set_attr "shift" "2")
   (set (attr "type") (if_then_else (match_operand 3 "const_int_operand" "")
		      (const_string "logic_shift_imm")
		      (const_string "logic_shift_reg")))]
)

;; Shifted bics pattern used to set up CC status register and not reusing
;; bics output.  Pattern restricts Thumb2 shift operand as bics for Thumb2
;; does not support shift by register.
(define_insn "andsi_not_shiftsi_si_scc_no_reuse"
  [(set (reg:CC_NOOV CC_REGNUM)
	(compare:CC_NOOV
		(and:SI (not:SI (match_operator:SI 0 "shift_operator"
			[(match_operand:SI 1 "s_register_operand" "r")
			 (match_operand:SI 2 "arm_rhs_operand" "rM")]))
			(match_operand:SI 3 "s_register_operand" "r"))
		(const_int 0)))
   (clobber (match_scratch:SI 4 "=r"))]
  "TARGET_ARM || (TARGET_THUMB2 && CONST_INT_P (operands[2]))"
  "bics%?\\t%4, %3, %1%S0"
  [(set_attr "predicable" "yes")
   (set_attr "predicable_short_it" "no")
   (set_attr "conds" "set")
   (set_attr "shift" "1")
   (set (attr "type") (if_then_else (match_operand 2 "const_int_operand" "")
		      (const_string "logic_shift_imm")
		      (const_string "logic_shift_reg")))]
)

;; Same as andsi_not_shiftsi_si_scc_no_reuse, but the bics result is also
;; getting reused later.
(define_insn "andsi_not_shiftsi_si_scc"
  [(parallel [(set (reg:CC_NOOV CC_REGNUM)
	(compare:CC_NOOV
		(and:SI (not:SI (match_operator:SI 0 "shift_operator"
			[(match_operand:SI 1 "s_register_operand" "r")
			 (match_operand:SI 2 "arm_rhs_operand" "rM")]))
			(match_operand:SI 3 "s_register_operand" "r"))
		(const_int 0)))
	(set (match_operand:SI 4 "s_register_operand" "=r")
	     (and:SI (not:SI (match_op_dup 0
		     [(match_dup 1)
		      (match_dup 2)]))
		     (match_dup 3)))])]
  "TARGET_ARM || (TARGET_THUMB2 && CONST_INT_P (operands[2]))"
  "bics%?\\t%4, %3, %1%S0"
  [(set_attr "predicable" "yes")
   (set_attr "predicable_short_it" "no")
   (set_attr "conds" "set")
   (set_attr "shift" "1")
   (set (attr "type") (if_then_else (match_operand 2 "const_int_operand" "")
		      (const_string "logic_shift_imm")
		      (const_string "logic_shift_reg")))]
)

(define_insn "*andsi_notsi_si_compare0"
  [(set (reg:CC_NOOV CC_REGNUM)
	(compare:CC_NOOV
	 (and:SI (not:SI (match_operand:SI 2 "s_register_operand" "r"))
		 (match_operand:SI 1 "s_register_operand" "r"))
	 (const_int 0)))
   (set (match_operand:SI 0 "s_register_operand" "=r")
	(and:SI (not:SI (match_dup 2)) (match_dup 1)))]
  "TARGET_32BIT"
  "bics\\t%0, %1, %2"
  [(set_attr "conds" "set")
   (set_attr "type" "logics_shift_reg")]
)

(define_insn "*andsi_notsi_si_compare0_scratch"
  [(set (reg:CC_NOOV CC_REGNUM)
	(compare:CC_NOOV
	 (and:SI (not:SI (match_operand:SI 2 "s_register_operand" "r"))
		 (match_operand:SI 1 "s_register_operand" "r"))
	 (const_int 0)))
   (clobber (match_scratch:SI 0 "=r"))]
  "TARGET_32BIT"
  "bics\\t%0, %1, %2"
  [(set_attr "conds" "set")
   (set_attr "type" "logics_shift_reg")]
)

(define_expand "iordi3"
  [(set (match_operand:DI         0 "s_register_operand" "")
	(ior:DI (match_operand:DI 1 "s_register_operand" "")
		(match_operand:DI 2 "neon_logic_op2" "")))]
  "TARGET_32BIT"
  ""
)

(define_insn_and_split "*iordi3_insn"
  [(set (match_operand:DI         0 "s_register_operand"     "=w,w ,&r,&r,&r,&r,?w,?w")
	(ior:DI (match_operand:DI 1 "s_register_operand"     "%w,0 ,0 ,r ,0 ,r ,w ,0")
		(match_operand:DI 2 "arm_iordi_operand_neon" "w ,Dl,r ,r ,Df,Df,w ,Dl")))]
  "TARGET_32BIT && !TARGET_IWMMXT"
  {
  switch (which_alternative)
    {
    case 0: /* fall through */
    case 6: return "vorr\t%P0, %P1, %P2";
    case 1: /* fall through */
    case 7: return neon_output_logic_immediate ("vorr", &operands[2],
		     DImode, 0, VALID_NEON_QREG_MODE (DImode));
    case 2:
    case 3:
    case 4:
    case 5:
      return "#";
    default: gcc_unreachable ();
    }
  }
  "TARGET_32BIT && !TARGET_IWMMXT && reload_completed
   && !(IS_VFP_REGNUM (REGNO (operands[0])))"
  [(set (match_dup 3) (match_dup 4))
   (set (match_dup 5) (match_dup 6))]
  "
  {
    operands[3] = gen_lowpart (SImode, operands[0]);
    operands[5] = gen_highpart (SImode, operands[0]);

    operands[4] = simplify_gen_binary (IOR, SImode,
                                           gen_lowpart (SImode, operands[1]),
                                           gen_lowpart (SImode, operands[2]));
    operands[6] = simplify_gen_binary (IOR, SImode,
                                           gen_highpart (SImode, operands[1]),
                                           gen_highpart_mode (SImode, DImode, operands[2]));

  }"
  [(set_attr "type" "neon_logic,neon_logic,multiple,multiple,multiple,\
                     multiple,neon_logic,neon_logic")
   (set_attr "length" "*,*,8,8,8,8,*,*")
   (set_attr "arch" "neon_for_64bits,neon_for_64bits,*,*,*,*,avoid_neon_for_64bits,avoid_neon_for_64bits")]
)

(define_insn "*iordi_zesidi_di"
  [(set (match_operand:DI 0 "s_register_operand" "=&r,&r")
	(ior:DI (zero_extend:DI
		 (match_operand:SI 2 "s_register_operand" "r,r"))
		(match_operand:DI 1 "s_register_operand" "0,?r")))]
  "TARGET_32BIT"
  "@
   orr%?\\t%Q0, %Q1, %2
   #"
  [(set_attr "length" "4,8")
   (set_attr "predicable" "yes")
   (set_attr "predicable_short_it" "no")
   (set_attr "type" "logic_reg,multiple")]
)

(define_insn "*iordi_sesidi_di"
  [(set (match_operand:DI 0 "s_register_operand" "=&r,&r")
	(ior:DI (sign_extend:DI
		 (match_operand:SI 2 "s_register_operand" "r,r"))
		(match_operand:DI 1 "s_register_operand" "0,r")))]
  "TARGET_32BIT"
  "#"
  [(set_attr "length" "8")
   (set_attr "predicable" "yes")
   (set_attr "type" "multiple")]
)

(define_expand "iorsi3"
  [(set (match_operand:SI         0 "s_register_operand" "")
	(ior:SI (match_operand:SI 1 "s_register_operand" "")
		(match_operand:SI 2 "reg_or_int_operand" "")))]
  "TARGET_EITHER"
  "
  if (CONST_INT_P (operands[2]))
    {
      if (TARGET_32BIT)
        {
	  if (DONT_EARLY_SPLIT_CONSTANT (INTVAL (operands[2]), IOR))
	    operands[2] = force_reg (SImode, operands[2]);
	  else
	    {
	      arm_split_constant (IOR, SImode, NULL_RTX,
				  INTVAL (operands[2]), operands[0],
				  operands[1],
				  optimize && can_create_pseudo_p ());
	      DONE;
	    }
	}
      else /* TARGET_THUMB1 */
        {
          rtx tmp = force_reg (SImode, operands[2]);
	  if (rtx_equal_p (operands[0], operands[1]))
	    operands[2] = tmp;
	  else
	    {
              operands[2] = operands[1];
              operands[1] = tmp;
	    }
        }
    }
  "
)

(define_insn_and_split "*iorsi3_insn"
  [(set (match_operand:SI 0 "s_register_operand" "=r,l,r,r,r")
	(ior:SI (match_operand:SI 1 "s_register_operand" "%r,0,r,r,r")
		(match_operand:SI 2 "reg_or_int_operand" "I,l,K,r,?n")))]
  "TARGET_32BIT"
  "@
   orr%?\\t%0, %1, %2
   orr%?\\t%0, %1, %2
   orn%?\\t%0, %1, #%B2
   orr%?\\t%0, %1, %2
   #"
  "TARGET_32BIT
   && CONST_INT_P (operands[2])
   && !(const_ok_for_arm (INTVAL (operands[2]))
        || (TARGET_THUMB2 && const_ok_for_arm (~INTVAL (operands[2]))))"
  [(clobber (const_int 0))]
{
  arm_split_constant (IOR, SImode, curr_insn,
                      INTVAL (operands[2]), operands[0], operands[1], 0);
  DONE;
}
  [(set_attr "length" "4,4,4,4,16")
   (set_attr "arch" "32,t2,t2,32,32")
   (set_attr "predicable" "yes")
   (set_attr "predicable_short_it" "no,yes,no,no,no")
   (set_attr "type" "logic_imm,logic_reg,logic_imm,logic_reg,logic_reg")]
)

(define_peephole2
  [(match_scratch:SI 3 "r")
   (set (match_operand:SI 0 "arm_general_register_operand" "")
	(ior:SI (match_operand:SI 1 "arm_general_register_operand" "")
		(match_operand:SI 2 "const_int_operand" "")))]
  "TARGET_ARM
   && !const_ok_for_arm (INTVAL (operands[2]))
   && const_ok_for_arm (~INTVAL (operands[2]))"
  [(set (match_dup 3) (match_dup 2))
   (set (match_dup 0) (ior:SI (match_dup 1) (match_dup 3)))]
  ""
)

(define_insn "*iorsi3_compare0"
  [(set (reg:CC_NOOV CC_REGNUM)
	(compare:CC_NOOV (ior:SI (match_operand:SI 1 "s_register_operand" "%r,r")
				 (match_operand:SI 2 "arm_rhs_operand" "I,r"))
			 (const_int 0)))
   (set (match_operand:SI 0 "s_register_operand" "=r,r")
	(ior:SI (match_dup 1) (match_dup 2)))]
  "TARGET_32BIT"
  "orrs%?\\t%0, %1, %2"
  [(set_attr "conds" "set")
   (set_attr "type" "logics_imm,logics_reg")]
)

(define_insn "*iorsi3_compare0_scratch"
  [(set (reg:CC_NOOV CC_REGNUM)
	(compare:CC_NOOV (ior:SI (match_operand:SI 1 "s_register_operand" "%r,r")
				 (match_operand:SI 2 "arm_rhs_operand" "I,r"))
			 (const_int 0)))
   (clobber (match_scratch:SI 0 "=r,r"))]
  "TARGET_32BIT"
  "orrs%?\\t%0, %1, %2"
  [(set_attr "conds" "set")
   (set_attr "type" "logics_imm,logics_reg")]
)

(define_expand "xordi3"
  [(set (match_operand:DI         0 "s_register_operand" "")
	(xor:DI (match_operand:DI 1 "s_register_operand" "")
		(match_operand:DI 2 "arm_xordi_operand" "")))]
  "TARGET_32BIT"
  ""
)

(define_insn_and_split "*xordi3_insn"
  [(set (match_operand:DI         0 "s_register_operand" "=w,&r,&r,&r,&r,?w")
	(xor:DI (match_operand:DI 1 "s_register_operand" "%w ,0,r ,0 ,r ,w")
		(match_operand:DI 2 "arm_xordi_operand"  "w ,r ,r ,Dg,Dg,w")))]
  "TARGET_32BIT && !TARGET_IWMMXT"
{
  switch (which_alternative)
    {
    case 1:
    case 2:
    case 3:
    case 4:  /* fall through */
      return "#";
    case 0: /* fall through */
    case 5: return "veor\t%P0, %P1, %P2";
    default: gcc_unreachable ();
    }
}
  "TARGET_32BIT && !TARGET_IWMMXT && reload_completed
   && !(IS_VFP_REGNUM (REGNO (operands[0])))"
  [(set (match_dup 3) (match_dup 4))
   (set (match_dup 5) (match_dup 6))]
  "
  {
    operands[3] = gen_lowpart (SImode, operands[0]);
    operands[5] = gen_highpart (SImode, operands[0]);

    operands[4] = simplify_gen_binary (XOR, SImode,
                                           gen_lowpart (SImode, operands[1]),
                                           gen_lowpart (SImode, operands[2]));
    operands[6] = simplify_gen_binary (XOR, SImode,
                                           gen_highpart (SImode, operands[1]),
                                           gen_highpart_mode (SImode, DImode, operands[2]));

  }"
  [(set_attr "length" "*,8,8,8,8,*")
   (set_attr "type" "neon_logic,multiple,multiple,multiple,multiple,neon_logic")
   (set_attr "arch" "neon_for_64bits,*,*,*,*,avoid_neon_for_64bits")]
)

(define_insn "*xordi_zesidi_di"
  [(set (match_operand:DI 0 "s_register_operand" "=&r,&r")
	(xor:DI (zero_extend:DI
		 (match_operand:SI 2 "s_register_operand" "r,r"))
		(match_operand:DI 1 "s_register_operand" "0,?r")))]
  "TARGET_32BIT"
  "@
   eor%?\\t%Q0, %Q1, %2
   #"
  [(set_attr "length" "4,8")
   (set_attr "predicable" "yes")
   (set_attr "predicable_short_it" "no")
   (set_attr "type" "logic_reg")]
)

(define_insn "*xordi_sesidi_di"
  [(set (match_operand:DI 0 "s_register_operand" "=&r,&r")
	(xor:DI (sign_extend:DI
		 (match_operand:SI 2 "s_register_operand" "r,r"))
		(match_operand:DI 1 "s_register_operand" "0,r")))]
  "TARGET_32BIT"
  "#"
  [(set_attr "length" "8")
   (set_attr "predicable" "yes")
   (set_attr "type" "multiple")]
)

(define_expand "xorsi3"
  [(set (match_operand:SI         0 "s_register_operand" "")
	(xor:SI (match_operand:SI 1 "s_register_operand" "")
		(match_operand:SI 2 "reg_or_int_operand" "")))]
  "TARGET_EITHER"
  "if (CONST_INT_P (operands[2]))
    {
      if (TARGET_32BIT)
        {
	  if (DONT_EARLY_SPLIT_CONSTANT (INTVAL (operands[2]), XOR))
	    operands[2] = force_reg (SImode, operands[2]);
	  else
	    {
	      arm_split_constant (XOR, SImode, NULL_RTX,
				  INTVAL (operands[2]), operands[0],
				  operands[1],
				  optimize && can_create_pseudo_p ());
	      DONE;
	    }
	}
      else /* TARGET_THUMB1 */
        {
          rtx tmp = force_reg (SImode, operands[2]);
	  if (rtx_equal_p (operands[0], operands[1]))
	    operands[2] = tmp;
	  else
	    {
              operands[2] = operands[1];
              operands[1] = tmp;
	    }
        }
    }"
)

(define_insn_and_split "*arm_xorsi3"
  [(set (match_operand:SI         0 "s_register_operand" "=r,l,r,r")
	(xor:SI (match_operand:SI 1 "s_register_operand" "%r,0,r,r")
		(match_operand:SI 2 "reg_or_int_operand" "I,l,r,?n")))]
  "TARGET_32BIT"
  "@
   eor%?\\t%0, %1, %2
   eor%?\\t%0, %1, %2
   eor%?\\t%0, %1, %2
   #"
  "TARGET_32BIT
   && CONST_INT_P (operands[2])
   && !const_ok_for_arm (INTVAL (operands[2]))"
  [(clobber (const_int 0))]
{
  arm_split_constant (XOR, SImode, curr_insn,
                      INTVAL (operands[2]), operands[0], operands[1], 0);
  DONE;
}
  [(set_attr "length" "4,4,4,16")
   (set_attr "predicable" "yes")
   (set_attr "predicable_short_it" "no,yes,no,no")
   (set_attr "type"  "logic_imm,logic_reg,logic_reg,multiple")]
)

(define_insn "*xorsi3_compare0"
  [(set (reg:CC_NOOV CC_REGNUM)
	(compare:CC_NOOV (xor:SI (match_operand:SI 1 "s_register_operand" "r,r")
				 (match_operand:SI 2 "arm_rhs_operand" "I,r"))
			 (const_int 0)))
   (set (match_operand:SI 0 "s_register_operand" "=r,r")
	(xor:SI (match_dup 1) (match_dup 2)))]
  "TARGET_32BIT"
  "eors%?\\t%0, %1, %2"
  [(set_attr "conds" "set")
   (set_attr "type" "logics_imm,logics_reg")]
)

(define_insn "*xorsi3_compare0_scratch"
  [(set (reg:CC_NOOV CC_REGNUM)
	(compare:CC_NOOV (xor:SI (match_operand:SI 0 "s_register_operand" "r,r")
				 (match_operand:SI 1 "arm_rhs_operand" "I,r"))
			 (const_int 0)))]
  "TARGET_32BIT"
  "teq%?\\t%0, %1"
  [(set_attr "conds" "set")
   (set_attr "type" "logics_imm,logics_reg")]
)

; By splitting (IOR (AND (NOT A) (NOT B)) C) as D = AND (IOR A B) (NOT C), 
; (NOT D) we can sometimes merge the final NOT into one of the following
; insns.

(define_split
  [(set (match_operand:SI 0 "s_register_operand" "")
	(ior:SI (and:SI (not:SI (match_operand:SI 1 "s_register_operand" ""))
			(not:SI (match_operand:SI 2 "arm_rhs_operand" "")))
		(match_operand:SI 3 "arm_rhs_operand" "")))
   (clobber (match_operand:SI 4 "s_register_operand" ""))]
  "TARGET_32BIT"
  [(set (match_dup 4) (and:SI (ior:SI (match_dup 1) (match_dup 2))
			      (not:SI (match_dup 3))))
   (set (match_dup 0) (not:SI (match_dup 4)))]
  ""
)

(define_insn_and_split "*andsi_iorsi3_notsi"
  [(set (match_operand:SI 0 "s_register_operand" "=&r,&r,&r")
	(and:SI (ior:SI (match_operand:SI 1 "s_register_operand" "%0,r,r")
			(match_operand:SI 2 "arm_rhs_operand" "rI,0,rI"))
		(not:SI (match_operand:SI 3 "arm_rhs_operand" "rI,rI,rI"))))]
  "TARGET_32BIT"
  "#"   ; "orr%?\\t%0, %1, %2\;bic%?\\t%0, %0, %3"
  "&& reload_completed"
  [(set (match_dup 0) (ior:SI (match_dup 1) (match_dup 2)))
   (set (match_dup 0) (and:SI (match_dup 4) (match_dup 5)))]
  {
     /* If operands[3] is a constant make sure to fold the NOT into it
	to avoid creating a NOT of a CONST_INT.  */
    rtx not_rtx = simplify_gen_unary (NOT, SImode, operands[3], SImode);
    if (CONST_INT_P (not_rtx))
      {
	operands[4] = operands[0];
	operands[5] = not_rtx;
      }
    else
      {
	operands[5] = operands[0];
	operands[4] = not_rtx;
      }
  }
  [(set_attr "length" "8")
   (set_attr "ce_count" "2")
   (set_attr "predicable" "yes")
   (set_attr "predicable_short_it" "no")
   (set_attr "type" "multiple")]
)

; ??? Are these four splitters still beneficial when the Thumb-2 bitfield
; insns are available?
(define_split
  [(set (match_operand:SI 0 "s_register_operand" "")
	(match_operator:SI 1 "logical_binary_operator"
	 [(zero_extract:SI (match_operand:SI 2 "s_register_operand" "")
			   (match_operand:SI 3 "const_int_operand" "")
			   (match_operand:SI 4 "const_int_operand" ""))
	  (match_operator:SI 9 "logical_binary_operator"
	   [(lshiftrt:SI (match_operand:SI 5 "s_register_operand" "")
			 (match_operand:SI 6 "const_int_operand" ""))
	    (match_operand:SI 7 "s_register_operand" "")])]))
   (clobber (match_operand:SI 8 "s_register_operand" ""))]
  "TARGET_32BIT
   && GET_CODE (operands[1]) == GET_CODE (operands[9])
   && INTVAL (operands[3]) == 32 - INTVAL (operands[6])"
  [(set (match_dup 8)
	(match_op_dup 1
	 [(ashift:SI (match_dup 2) (match_dup 4))
	  (match_dup 5)]))
   (set (match_dup 0)
	(match_op_dup 1
	 [(lshiftrt:SI (match_dup 8) (match_dup 6))
	  (match_dup 7)]))]
  "
  operands[4] = GEN_INT (32 - (INTVAL (operands[3]) + INTVAL (operands[4])));
")

(define_split
  [(set (match_operand:SI 0 "s_register_operand" "")
	(match_operator:SI 1 "logical_binary_operator"
	 [(match_operator:SI 9 "logical_binary_operator"
	   [(lshiftrt:SI (match_operand:SI 5 "s_register_operand" "")
			 (match_operand:SI 6 "const_int_operand" ""))
	    (match_operand:SI 7 "s_register_operand" "")])
	  (zero_extract:SI (match_operand:SI 2 "s_register_operand" "")
			   (match_operand:SI 3 "const_int_operand" "")
			   (match_operand:SI 4 "const_int_operand" ""))]))
   (clobber (match_operand:SI 8 "s_register_operand" ""))]
  "TARGET_32BIT
   && GET_CODE (operands[1]) == GET_CODE (operands[9])
   && INTVAL (operands[3]) == 32 - INTVAL (operands[6])"
  [(set (match_dup 8)
	(match_op_dup 1
	 [(ashift:SI (match_dup 2) (match_dup 4))
	  (match_dup 5)]))
   (set (match_dup 0)
	(match_op_dup 1
	 [(lshiftrt:SI (match_dup 8) (match_dup 6))
	  (match_dup 7)]))]
  "
  operands[4] = GEN_INT (32 - (INTVAL (operands[3]) + INTVAL (operands[4])));
")

(define_split
  [(set (match_operand:SI 0 "s_register_operand" "")
	(match_operator:SI 1 "logical_binary_operator"
	 [(sign_extract:SI (match_operand:SI 2 "s_register_operand" "")
			   (match_operand:SI 3 "const_int_operand" "")
			   (match_operand:SI 4 "const_int_operand" ""))
	  (match_operator:SI 9 "logical_binary_operator"
	   [(ashiftrt:SI (match_operand:SI 5 "s_register_operand" "")
			 (match_operand:SI 6 "const_int_operand" ""))
	    (match_operand:SI 7 "s_register_operand" "")])]))
   (clobber (match_operand:SI 8 "s_register_operand" ""))]
  "TARGET_32BIT
   && GET_CODE (operands[1]) == GET_CODE (operands[9])
   && INTVAL (operands[3]) == 32 - INTVAL (operands[6])"
  [(set (match_dup 8)
	(match_op_dup 1
	 [(ashift:SI (match_dup 2) (match_dup 4))
	  (match_dup 5)]))
   (set (match_dup 0)
	(match_op_dup 1
	 [(ashiftrt:SI (match_dup 8) (match_dup 6))
	  (match_dup 7)]))]
  "
  operands[4] = GEN_INT (32 - (INTVAL (operands[3]) + INTVAL (operands[4])));
")

(define_split
  [(set (match_operand:SI 0 "s_register_operand" "")
	(match_operator:SI 1 "logical_binary_operator"
	 [(match_operator:SI 9 "logical_binary_operator"
	   [(ashiftrt:SI (match_operand:SI 5 "s_register_operand" "")
			 (match_operand:SI 6 "const_int_operand" ""))
	    (match_operand:SI 7 "s_register_operand" "")])
	  (sign_extract:SI (match_operand:SI 2 "s_register_operand" "")
			   (match_operand:SI 3 "const_int_operand" "")
			   (match_operand:SI 4 "const_int_operand" ""))]))
   (clobber (match_operand:SI 8 "s_register_operand" ""))]
  "TARGET_32BIT
   && GET_CODE (operands[1]) == GET_CODE (operands[9])
   && INTVAL (operands[3]) == 32 - INTVAL (operands[6])"
  [(set (match_dup 8)
	(match_op_dup 1
	 [(ashift:SI (match_dup 2) (match_dup 4))
	  (match_dup 5)]))
   (set (match_dup 0)
	(match_op_dup 1
	 [(ashiftrt:SI (match_dup 8) (match_dup 6))
	  (match_dup 7)]))]
  "
  operands[4] = GEN_INT (32 - (INTVAL (operands[3]) + INTVAL (operands[4])));
")


;; Minimum and maximum insns

(define_expand "smaxsi3"
  [(parallel [
    (set (match_operand:SI 0 "s_register_operand" "")
	 (smax:SI (match_operand:SI 1 "s_register_operand" "")
		  (match_operand:SI 2 "arm_rhs_operand" "")))
    (clobber (reg:CC CC_REGNUM))])]
  "TARGET_32BIT"
  "
  if (operands[2] == const0_rtx || operands[2] == constm1_rtx)
    {
      /* No need for a clobber of the condition code register here.  */
      emit_insn (gen_rtx_SET (operands[0],
			      gen_rtx_SMAX (SImode, operands[1],
					    operands[2])));
      DONE;
    }
")

(define_insn "*smax_0"
  [(set (match_operand:SI 0 "s_register_operand" "=r")
	(smax:SI (match_operand:SI 1 "s_register_operand" "r")
		 (const_int 0)))]
  "TARGET_32BIT"
  "bic%?\\t%0, %1, %1, asr #31"
  [(set_attr "predicable" "yes")
   (set_attr "predicable_short_it" "no")
   (set_attr "type" "logic_shift_reg")]
)

(define_insn "*smax_m1"
  [(set (match_operand:SI 0 "s_register_operand" "=r")
	(smax:SI (match_operand:SI 1 "s_register_operand" "r")
		 (const_int -1)))]
  "TARGET_32BIT"
  "orr%?\\t%0, %1, %1, asr #31"
  [(set_attr "predicable" "yes")
   (set_attr "predicable_short_it" "no")
   (set_attr "type" "logic_shift_reg")]
)

(define_insn_and_split "*arm_smax_insn"
  [(set (match_operand:SI          0 "s_register_operand" "=r,r")
	(smax:SI (match_operand:SI 1 "s_register_operand"  "%0,?r")
		 (match_operand:SI 2 "arm_rhs_operand"    "rI,rI")))
   (clobber (reg:CC CC_REGNUM))]
  "TARGET_ARM"
  "#"
   ; cmp\\t%1, %2\;movlt\\t%0, %2
   ; cmp\\t%1, %2\;movge\\t%0, %1\;movlt\\t%0, %2"
  "TARGET_ARM"
  [(set (reg:CC CC_REGNUM)
        (compare:CC (match_dup 1) (match_dup 2)))
   (set (match_dup 0)
        (if_then_else:SI (ge:SI (reg:CC CC_REGNUM) (const_int 0))
                         (match_dup 1)
                         (match_dup 2)))]
  ""
  [(set_attr "conds" "clob")
   (set_attr "length" "8,12")
   (set_attr "type" "multiple")]
)

(define_expand "sminsi3"
  [(parallel [
    (set (match_operand:SI 0 "s_register_operand" "")
	 (smin:SI (match_operand:SI 1 "s_register_operand" "")
		  (match_operand:SI 2 "arm_rhs_operand" "")))
    (clobber (reg:CC CC_REGNUM))])]
  "TARGET_32BIT"
  "
  if (operands[2] == const0_rtx)
    {
      /* No need for a clobber of the condition code register here.  */
      emit_insn (gen_rtx_SET (operands[0],
			      gen_rtx_SMIN (SImode, operands[1],
					    operands[2])));
      DONE;
    }
")

(define_insn "*smin_0"
  [(set (match_operand:SI 0 "s_register_operand" "=r")
	(smin:SI (match_operand:SI 1 "s_register_operand" "r")
		 (const_int 0)))]
  "TARGET_32BIT"
  "and%?\\t%0, %1, %1, asr #31"
  [(set_attr "predicable" "yes")
   (set_attr "predicable_short_it" "no")
   (set_attr "type" "logic_shift_reg")]
)

(define_insn_and_split "*arm_smin_insn"
  [(set (match_operand:SI 0 "s_register_operand" "=r,r")
	(smin:SI (match_operand:SI 1 "s_register_operand" "%0,?r")
		 (match_operand:SI 2 "arm_rhs_operand" "rI,rI")))
   (clobber (reg:CC CC_REGNUM))]
  "TARGET_ARM"
  "#"
    ; cmp\\t%1, %2\;movge\\t%0, %2
    ; cmp\\t%1, %2\;movlt\\t%0, %1\;movge\\t%0, %2"
  "TARGET_ARM"
  [(set (reg:CC CC_REGNUM)
        (compare:CC (match_dup 1) (match_dup 2)))
   (set (match_dup 0)
        (if_then_else:SI (lt:SI (reg:CC CC_REGNUM) (const_int 0))
                         (match_dup 1)
                         (match_dup 2)))]
  ""
  [(set_attr "conds" "clob")
   (set_attr "length" "8,12")
   (set_attr "type" "multiple,multiple")]
)

(define_expand "umaxsi3"
  [(parallel [
    (set (match_operand:SI 0 "s_register_operand" "")
	 (umax:SI (match_operand:SI 1 "s_register_operand" "")
		  (match_operand:SI 2 "arm_rhs_operand" "")))
    (clobber (reg:CC CC_REGNUM))])]
  "TARGET_32BIT"
  ""
)

(define_insn_and_split "*arm_umaxsi3"
  [(set (match_operand:SI 0 "s_register_operand" "=r,r,r")
	(umax:SI (match_operand:SI 1 "s_register_operand" "0,r,?r")
		 (match_operand:SI 2 "arm_rhs_operand" "rI,0,rI")))
   (clobber (reg:CC CC_REGNUM))]
  "TARGET_ARM"
  "#"
    ; cmp\\t%1, %2\;movcc\\t%0, %2
    ; cmp\\t%1, %2\;movcs\\t%0, %1
    ; cmp\\t%1, %2\;movcs\\t%0, %1\;movcc\\t%0, %2"
  "TARGET_ARM"
  [(set (reg:CC CC_REGNUM)
        (compare:CC (match_dup 1) (match_dup 2)))
   (set (match_dup 0)
        (if_then_else:SI (geu:SI (reg:CC CC_REGNUM) (const_int 0))
                         (match_dup 1)
                         (match_dup 2)))]
  ""
  [(set_attr "conds" "clob")
   (set_attr "length" "8,8,12")
   (set_attr "type" "store1")]
)

(define_expand "uminsi3"
  [(parallel [
    (set (match_operand:SI 0 "s_register_operand" "")
	 (umin:SI (match_operand:SI 1 "s_register_operand" "")
		  (match_operand:SI 2 "arm_rhs_operand" "")))
    (clobber (reg:CC CC_REGNUM))])]
  "TARGET_32BIT"
  ""
)

(define_insn_and_split "*arm_uminsi3"
  [(set (match_operand:SI 0 "s_register_operand" "=r,r,r")
	(umin:SI (match_operand:SI 1 "s_register_operand" "0,r,?r")
		 (match_operand:SI 2 "arm_rhs_operand" "rI,0,rI")))
   (clobber (reg:CC CC_REGNUM))]
  "TARGET_ARM"
  "#"
   ; cmp\\t%1, %2\;movcs\\t%0, %2
   ; cmp\\t%1, %2\;movcc\\t%0, %1
   ; cmp\\t%1, %2\;movcc\\t%0, %1\;movcs\\t%0, %2"
  "TARGET_ARM"
  [(set (reg:CC CC_REGNUM)
        (compare:CC (match_dup 1) (match_dup 2)))
   (set (match_dup 0)
        (if_then_else:SI (ltu:SI (reg:CC CC_REGNUM) (const_int 0))
                         (match_dup 1)
                         (match_dup 2)))]
  ""
  [(set_attr "conds" "clob")
   (set_attr "length" "8,8,12")
   (set_attr "type" "store1")]
)

(define_insn "*store_minmaxsi"
  [(set (match_operand:SI 0 "memory_operand" "=m")
	(match_operator:SI 3 "minmax_operator"
	 [(match_operand:SI 1 "s_register_operand" "r")
	  (match_operand:SI 2 "s_register_operand" "r")]))
   (clobber (reg:CC CC_REGNUM))]
  "TARGET_32BIT && optimize_function_for_size_p (cfun) && !arm_restrict_it"
  "*
  operands[3] = gen_rtx_fmt_ee (minmax_code (operands[3]), SImode,
				operands[1], operands[2]);
  output_asm_insn (\"cmp\\t%1, %2\", operands);
  if (TARGET_THUMB2)
    output_asm_insn (\"ite\t%d3\", operands);
  output_asm_insn (\"str%d3\\t%1, %0\", operands);
  output_asm_insn (\"str%D3\\t%2, %0\", operands);
  return \"\";
  "
  [(set_attr "conds" "clob")
   (set (attr "length")
	(if_then_else (eq_attr "is_thumb" "yes")
		      (const_int 14)
		      (const_int 12)))
   (set_attr "type" "store1")]
)

; Reject the frame pointer in operand[1], since reloading this after
; it has been eliminated can cause carnage.
(define_insn "*minmax_arithsi"
  [(set (match_operand:SI 0 "s_register_operand" "=r,r")
	(match_operator:SI 4 "shiftable_operator"
	 [(match_operator:SI 5 "minmax_operator"
	   [(match_operand:SI 2 "s_register_operand" "r,r")
	    (match_operand:SI 3 "arm_rhs_operand" "rI,rI")])
	  (match_operand:SI 1 "s_register_operand" "0,?r")]))
   (clobber (reg:CC CC_REGNUM))]
  "TARGET_32BIT && !arm_eliminable_register (operands[1]) && !arm_restrict_it"
  "*
  {
    enum rtx_code code = GET_CODE (operands[4]);
    bool need_else;

    if (which_alternative != 0 || operands[3] != const0_rtx
        || (code != PLUS && code != IOR && code != XOR))
      need_else = true;
    else
      need_else = false;

    operands[5] = gen_rtx_fmt_ee (minmax_code (operands[5]), SImode,
				  operands[2], operands[3]);
    output_asm_insn (\"cmp\\t%2, %3\", operands);
    if (TARGET_THUMB2)
      {
	if (need_else)
	  output_asm_insn (\"ite\\t%d5\", operands);
	else
	  output_asm_insn (\"it\\t%d5\", operands);
      }
    output_asm_insn (\"%i4%d5\\t%0, %1, %2\", operands);
    if (need_else)
      output_asm_insn (\"%i4%D5\\t%0, %1, %3\", operands);
    return \"\";
  }"
  [(set_attr "conds" "clob")
   (set (attr "length")
	(if_then_else (eq_attr "is_thumb" "yes")
		      (const_int 14)
		      (const_int 12)))
   (set_attr "type" "multiple")]
)

; Reject the frame pointer in operand[1], since reloading this after
; it has been eliminated can cause carnage.
(define_insn_and_split "*minmax_arithsi_non_canon"
  [(set (match_operand:SI 0 "s_register_operand" "=Ts,Ts")
	(minus:SI
	 (match_operand:SI 1 "s_register_operand" "0,?Ts")
	  (match_operator:SI 4 "minmax_operator"
	   [(match_operand:SI 2 "s_register_operand" "Ts,Ts")
	    (match_operand:SI 3 "arm_rhs_operand" "TsI,TsI")])))
   (clobber (reg:CC CC_REGNUM))]
  "TARGET_32BIT && !arm_eliminable_register (operands[1])
   && !(arm_restrict_it && CONST_INT_P (operands[3]))"
  "#"
  "TARGET_32BIT && !arm_eliminable_register (operands[1]) && reload_completed"
  [(set (reg:CC CC_REGNUM)
        (compare:CC (match_dup 2) (match_dup 3)))

   (cond_exec (match_op_dup 4 [(reg:CC CC_REGNUM) (const_int 0)])
              (set (match_dup 0)
                   (minus:SI (match_dup 1)
                             (match_dup 2))))
   (cond_exec (match_op_dup 5 [(reg:CC CC_REGNUM) (const_int 0)])
              (set (match_dup 0)
                   (match_dup 6)))]
  {
  machine_mode mode = SELECT_CC_MODE (GET_CODE (operands[1]),
                                           operands[2], operands[3]);
  enum rtx_code rc = minmax_code (operands[4]);
  operands[4] = gen_rtx_fmt_ee (rc, VOIDmode,
                                operands[2], operands[3]);

  if (mode == CCFPmode || mode == CCFPEmode)
    rc = reverse_condition_maybe_unordered (rc);
  else
    rc = reverse_condition (rc);
  operands[5] = gen_rtx_fmt_ee (rc, SImode, operands[2], operands[3]);
  if (CONST_INT_P (operands[3]))
    operands[6] = plus_constant (SImode, operands[1], -INTVAL (operands[3]));
  else
    operands[6] = gen_rtx_MINUS (SImode, operands[1], operands[3]);
  }
  [(set_attr "conds" "clob")
   (set (attr "length")
	(if_then_else (eq_attr "is_thumb" "yes")
		      (const_int 14)
		      (const_int 12)))
   (set_attr "type" "multiple")]
)

(define_code_iterator SAT [smin smax])
(define_code_iterator SATrev [smin smax])
(define_code_attr SATlo [(smin "1") (smax "2")])
(define_code_attr SAThi [(smin "2") (smax "1")])

(define_insn "*satsi_<SAT:code>"
  [(set (match_operand:SI 0 "s_register_operand" "=r")
        (SAT:SI (SATrev:SI (match_operand:SI 3 "s_register_operand" "r")
                           (match_operand:SI 1 "const_int_operand" "i"))
                (match_operand:SI 2 "const_int_operand" "i")))]
  "TARGET_32BIT && arm_arch6 && <SAT:CODE> != <SATrev:CODE>
   && arm_sat_operator_match (operands[<SAT:SATlo>], operands[<SAT:SAThi>], NULL, NULL)"
{
  int mask;
  bool signed_sat;
  if (!arm_sat_operator_match (operands[<SAT:SATlo>], operands[<SAT:SAThi>],
                               &mask, &signed_sat))
    gcc_unreachable ();

  operands[1] = GEN_INT (mask);
  if (signed_sat)
    return "ssat%?\t%0, %1, %3";
  else
    return "usat%?\t%0, %1, %3";
}
  [(set_attr "predicable" "yes")
   (set_attr "predicable_short_it" "no")
   (set_attr "type" "alus_imm")]
)

(define_insn "*satsi_<SAT:code>_shift"
  [(set (match_operand:SI 0 "s_register_operand" "=r")
        (SAT:SI (SATrev:SI (match_operator:SI 3 "sat_shift_operator"
                             [(match_operand:SI 4 "s_register_operand" "r")
                              (match_operand:SI 5 "const_int_operand" "i")])
                           (match_operand:SI 1 "const_int_operand" "i"))
                (match_operand:SI 2 "const_int_operand" "i")))]
  "TARGET_32BIT && arm_arch6 && <SAT:CODE> != <SATrev:CODE>
   && arm_sat_operator_match (operands[<SAT:SATlo>], operands[<SAT:SAThi>], NULL, NULL)"
{
  int mask;
  bool signed_sat;
  if (!arm_sat_operator_match (operands[<SAT:SATlo>], operands[<SAT:SAThi>],
                               &mask, &signed_sat))
    gcc_unreachable ();

  operands[1] = GEN_INT (mask);
  if (signed_sat)
    return "ssat%?\t%0, %1, %4%S3";
  else
    return "usat%?\t%0, %1, %4%S3";
}
  [(set_attr "predicable" "yes")
   (set_attr "predicable_short_it" "no")
   (set_attr "shift" "3")
   (set_attr "type" "logic_shift_reg")])

;; Shift and rotation insns

(define_expand "ashldi3"
  [(set (match_operand:DI            0 "s_register_operand" "")
        (ashift:DI (match_operand:DI 1 "s_register_operand" "")
                   (match_operand:SI 2 "general_operand" "")))]
  "TARGET_32BIT"
  "
  if (TARGET_NEON)
    {
      /* Delay the decision whether to use NEON or core-regs until
	 register allocation.  */
      emit_insn (gen_ashldi3_neon (operands[0], operands[1], operands[2]));
      DONE;
    }
  else
    {
      /* Only the NEON case can handle in-memory shift counts.  */
      if (!reg_or_int_operand (operands[2], SImode))
        operands[2] = force_reg (SImode, operands[2]);
    }

  if (!CONST_INT_P (operands[2]) && TARGET_REALLY_IWMMXT)
    ; /* No special preparation statements; expand pattern as above.  */
  else
    {
      rtx scratch1, scratch2;

      if (operands[2] == CONST1_RTX (SImode))
        {
          emit_insn (gen_arm_ashldi3_1bit (operands[0], operands[1]));
          DONE;
        }

      /* Ideally we should use iwmmxt here if we could know that operands[1]
         ends up already living in an iwmmxt register. Otherwise it's
         cheaper to have the alternate code being generated than moving
         values to iwmmxt regs and back.  */

      /* If we're optimizing for size, we prefer the libgcc calls.  */
      if (optimize_function_for_size_p (cfun))
	FAIL;

      /* Expand operation using core-registers.
	 'FAIL' would achieve the same thing, but this is a bit smarter.  */
      scratch1 = gen_reg_rtx (SImode);
      scratch2 = gen_reg_rtx (SImode);
      arm_emit_coreregs_64bit_shift (ASHIFT, operands[0], operands[1],
				     operands[2], scratch1, scratch2);
      DONE;
    }
  "
)

(define_insn "arm_ashldi3_1bit"
  [(set (match_operand:DI            0 "s_register_operand" "=r,&r")
        (ashift:DI (match_operand:DI 1 "s_register_operand" "0,r")
                   (const_int 1)))
   (clobber (reg:CC CC_REGNUM))]
  "TARGET_32BIT"
  "movs\\t%Q0, %Q1, asl #1\;adc\\t%R0, %R1, %R1"
  [(set_attr "conds" "clob")
   (set_attr "length" "8")
   (set_attr "type" "multiple")]
)

(define_expand "ashlsi3"
  [(set (match_operand:SI            0 "s_register_operand" "")
	(ashift:SI (match_operand:SI 1 "s_register_operand" "")
		   (match_operand:SI 2 "arm_rhs_operand" "")))]
  "TARGET_EITHER"
  "
  if (CONST_INT_P (operands[2])
      && (UINTVAL (operands[2])) > 31)
    {
      emit_insn (gen_movsi (operands[0], const0_rtx));
      DONE;
    }
  "
)

(define_expand "ashrdi3"
  [(set (match_operand:DI              0 "s_register_operand" "")
        (ashiftrt:DI (match_operand:DI 1 "s_register_operand" "")
                     (match_operand:SI 2 "reg_or_int_operand" "")))]
  "TARGET_32BIT"
  "
  if (TARGET_NEON)
    {
      /* Delay the decision whether to use NEON or core-regs until
	 register allocation.  */
      emit_insn (gen_ashrdi3_neon (operands[0], operands[1], operands[2]));
      DONE;
    }

  if (!CONST_INT_P (operands[2]) && TARGET_REALLY_IWMMXT)
    ; /* No special preparation statements; expand pattern as above.  */
  else
    {
      rtx scratch1, scratch2;

      if (operands[2] == CONST1_RTX (SImode))
        {
          emit_insn (gen_arm_ashrdi3_1bit (operands[0], operands[1]));
          DONE;
        }

      /* Ideally we should use iwmmxt here if we could know that operands[1]
         ends up already living in an iwmmxt register. Otherwise it's
         cheaper to have the alternate code being generated than moving
         values to iwmmxt regs and back.  */

      /* If we're optimizing for size, we prefer the libgcc calls.  */
      if (optimize_function_for_size_p (cfun))
	FAIL;

      /* Expand operation using core-registers.
	 'FAIL' would achieve the same thing, but this is a bit smarter.  */
      scratch1 = gen_reg_rtx (SImode);
      scratch2 = gen_reg_rtx (SImode);
      arm_emit_coreregs_64bit_shift (ASHIFTRT, operands[0], operands[1],
				     operands[2], scratch1, scratch2);
      DONE;
    }
  "
)

(define_insn "arm_ashrdi3_1bit"
  [(set (match_operand:DI              0 "s_register_operand" "=r,&r")
        (ashiftrt:DI (match_operand:DI 1 "s_register_operand" "0,r")
                     (const_int 1)))
   (clobber (reg:CC CC_REGNUM))]
  "TARGET_32BIT"
  "movs\\t%R0, %R1, asr #1\;mov\\t%Q0, %Q1, rrx"
  [(set_attr "conds" "clob")
   (set_attr "length" "8")
   (set_attr "type" "multiple")]
)

(define_expand "ashrsi3"
  [(set (match_operand:SI              0 "s_register_operand" "")
	(ashiftrt:SI (match_operand:SI 1 "s_register_operand" "")
		     (match_operand:SI 2 "arm_rhs_operand" "")))]
  "TARGET_EITHER"
  "
  if (CONST_INT_P (operands[2])
      && UINTVAL (operands[2]) > 31)
    operands[2] = GEN_INT (31);
  "
)

(define_expand "lshrdi3"
  [(set (match_operand:DI              0 "s_register_operand" "")
        (lshiftrt:DI (match_operand:DI 1 "s_register_operand" "")
                     (match_operand:SI 2 "reg_or_int_operand" "")))]
  "TARGET_32BIT"
  "
  if (TARGET_NEON)
    {
      /* Delay the decision whether to use NEON or core-regs until
	 register allocation.  */
      emit_insn (gen_lshrdi3_neon (operands[0], operands[1], operands[2]));
      DONE;
    }

  if (!CONST_INT_P (operands[2]) && TARGET_REALLY_IWMMXT)
    ; /* No special preparation statements; expand pattern as above.  */
  else
    {
      rtx scratch1, scratch2;

      if (operands[2] == CONST1_RTX (SImode))
        {
          emit_insn (gen_arm_lshrdi3_1bit (operands[0], operands[1]));
          DONE;
        }

      /* Ideally we should use iwmmxt here if we could know that operands[1]
         ends up already living in an iwmmxt register. Otherwise it's
         cheaper to have the alternate code being generated than moving
         values to iwmmxt regs and back.  */

      /* If we're optimizing for size, we prefer the libgcc calls.  */
      if (optimize_function_for_size_p (cfun))
	FAIL;

      /* Expand operation using core-registers.
	 'FAIL' would achieve the same thing, but this is a bit smarter.  */
      scratch1 = gen_reg_rtx (SImode);
      scratch2 = gen_reg_rtx (SImode);
      arm_emit_coreregs_64bit_shift (LSHIFTRT, operands[0], operands[1],
				     operands[2], scratch1, scratch2);
      DONE;
    }
  "
)

(define_insn "arm_lshrdi3_1bit"
  [(set (match_operand:DI              0 "s_register_operand" "=r,&r")
        (lshiftrt:DI (match_operand:DI 1 "s_register_operand" "0,r")
                     (const_int 1)))
   (clobber (reg:CC CC_REGNUM))]
  "TARGET_32BIT"
  "movs\\t%R0, %R1, lsr #1\;mov\\t%Q0, %Q1, rrx"
  [(set_attr "conds" "clob")
   (set_attr "length" "8")
   (set_attr "type" "multiple")]
)

(define_expand "lshrsi3"
  [(set (match_operand:SI              0 "s_register_operand" "")
	(lshiftrt:SI (match_operand:SI 1 "s_register_operand" "")
		     (match_operand:SI 2 "arm_rhs_operand" "")))]
  "TARGET_EITHER"
  "
  if (CONST_INT_P (operands[2])
      && (UINTVAL (operands[2])) > 31)
    {
      emit_insn (gen_movsi (operands[0], const0_rtx));
      DONE;
    }
  "
)

(define_expand "rotlsi3"
  [(set (match_operand:SI              0 "s_register_operand" "")
	(rotatert:SI (match_operand:SI 1 "s_register_operand" "")
		     (match_operand:SI 2 "reg_or_int_operand" "")))]
  "TARGET_32BIT"
  "
  if (CONST_INT_P (operands[2]))
    operands[2] = GEN_INT ((32 - INTVAL (operands[2])) % 32);
  else
    {
      rtx reg = gen_reg_rtx (SImode);
      emit_insn (gen_subsi3 (reg, GEN_INT (32), operands[2]));
      operands[2] = reg;
    }
  "
)

(define_expand "rotrsi3"
  [(set (match_operand:SI              0 "s_register_operand" "")
	(rotatert:SI (match_operand:SI 1 "s_register_operand" "")
		     (match_operand:SI 2 "arm_rhs_operand" "")))]
  "TARGET_EITHER"
  "
  if (TARGET_32BIT)
    {
      if (CONST_INT_P (operands[2])
          && UINTVAL (operands[2]) > 31)
        operands[2] = GEN_INT (INTVAL (operands[2]) % 32);
    }
  else /* TARGET_THUMB1 */
    {
      if (CONST_INT_P (operands [2]))
        operands [2] = force_reg (SImode, operands[2]);
    }
  "
)

(define_insn "*arm_shiftsi3"
  [(set (match_operand:SI   0 "s_register_operand" "=l,l,r,r")
	(match_operator:SI  3 "shift_operator"
	 [(match_operand:SI 1 "s_register_operand"  "0,l,r,r")
	  (match_operand:SI 2 "reg_or_int_operand" "l,M,M,r")]))]
  "TARGET_32BIT"
  "* return arm_output_shift(operands, 0);"
  [(set_attr "predicable" "yes")
   (set_attr "arch" "t2,t2,*,*")
   (set_attr "predicable_short_it" "yes,yes,no,no")
   (set_attr "length" "4")
   (set_attr "shift" "1")
   (set_attr "type" "alu_shift_reg,alu_shift_imm,alu_shift_imm,alu_shift_reg")]
)

(define_insn "*shiftsi3_compare0"
  [(set (reg:CC_NOOV CC_REGNUM)
	(compare:CC_NOOV (match_operator:SI 3 "shift_operator"
			  [(match_operand:SI 1 "s_register_operand" "r,r")
			   (match_operand:SI 2 "arm_rhs_operand" "M,r")])
			 (const_int 0)))
   (set (match_operand:SI 0 "s_register_operand" "=r,r")
	(match_op_dup 3 [(match_dup 1) (match_dup 2)]))]
  "TARGET_32BIT"
  "* return arm_output_shift(operands, 1);"
  [(set_attr "conds" "set")
   (set_attr "shift" "1")
   (set_attr "type" "alus_shift_imm,alus_shift_reg")]
)

(define_insn "*shiftsi3_compare0_scratch"
  [(set (reg:CC_NOOV CC_REGNUM)
	(compare:CC_NOOV (match_operator:SI 3 "shift_operator"
			  [(match_operand:SI 1 "s_register_operand" "r,r")
			   (match_operand:SI 2 "arm_rhs_operand" "M,r")])
			 (const_int 0)))
   (clobber (match_scratch:SI 0 "=r,r"))]
  "TARGET_32BIT"
  "* return arm_output_shift(operands, 1);"
  [(set_attr "conds" "set")
   (set_attr "shift" "1")
   (set_attr "type" "shift_imm,shift_reg")]
)

(define_insn "*not_shiftsi"
  [(set (match_operand:SI 0 "s_register_operand" "=r,r")
	(not:SI (match_operator:SI 3 "shift_operator"
		 [(match_operand:SI 1 "s_register_operand" "r,r")
		  (match_operand:SI 2 "shift_amount_operand" "M,rM")])))]
  "TARGET_32BIT"
  "mvn%?\\t%0, %1%S3"
  [(set_attr "predicable" "yes")
   (set_attr "predicable_short_it" "no")
   (set_attr "shift" "1")
   (set_attr "arch" "32,a")
   (set_attr "type" "mvn_shift,mvn_shift_reg")])

(define_insn "*not_shiftsi_compare0"
  [(set (reg:CC_NOOV CC_REGNUM)
	(compare:CC_NOOV
	 (not:SI (match_operator:SI 3 "shift_operator"
		  [(match_operand:SI 1 "s_register_operand" "r,r")
		   (match_operand:SI 2 "shift_amount_operand" "M,rM")]))
	 (const_int 0)))
   (set (match_operand:SI 0 "s_register_operand" "=r,r")
	(not:SI (match_op_dup 3 [(match_dup 1) (match_dup 2)])))]
  "TARGET_32BIT"
  "mvns%?\\t%0, %1%S3"
  [(set_attr "conds" "set")
   (set_attr "shift" "1")
   (set_attr "arch" "32,a")
   (set_attr "type" "mvn_shift,mvn_shift_reg")])

(define_insn "*not_shiftsi_compare0_scratch"
  [(set (reg:CC_NOOV CC_REGNUM)
	(compare:CC_NOOV
	 (not:SI (match_operator:SI 3 "shift_operator"
		  [(match_operand:SI 1 "s_register_operand" "r,r")
		   (match_operand:SI 2 "shift_amount_operand" "M,rM")]))
	 (const_int 0)))
   (clobber (match_scratch:SI 0 "=r,r"))]
  "TARGET_32BIT"
  "mvns%?\\t%0, %1%S3"
  [(set_attr "conds" "set")
   (set_attr "shift" "1")
   (set_attr "arch" "32,a")
   (set_attr "type" "mvn_shift,mvn_shift_reg")])

;; We don't really have extzv, but defining this using shifts helps
;; to reduce register pressure later on.

(define_expand "extzv"
  [(set (match_operand 0 "s_register_operand" "")
	(zero_extract (match_operand 1 "nonimmediate_operand" "")
		      (match_operand 2 "const_int_operand" "")
		      (match_operand 3 "const_int_operand" "")))]
  "TARGET_THUMB1 || arm_arch_thumb2"
  "
  {
    HOST_WIDE_INT lshift = 32 - INTVAL (operands[2]) - INTVAL (operands[3]);
    HOST_WIDE_INT rshift = 32 - INTVAL (operands[2]);
    
    if (arm_arch_thumb2)
      {
	HOST_WIDE_INT width = INTVAL (operands[2]);
	HOST_WIDE_INT bitpos = INTVAL (operands[3]);

	if (unaligned_access && MEM_P (operands[1])
	    && (width == 16 || width == 32) && (bitpos % BITS_PER_UNIT) == 0)
	  {
	    rtx base_addr;

	    if (BYTES_BIG_ENDIAN)
	      bitpos = GET_MODE_BITSIZE (GET_MODE (operands[0])) - width
		       - bitpos;

	    if (width == 32)
              {
		base_addr = adjust_address (operands[1], SImode,
					    bitpos / BITS_PER_UNIT);
		emit_insn (gen_unaligned_loadsi (operands[0], base_addr));
              }
	    else
              {
		rtx dest = operands[0];
		rtx tmp = gen_reg_rtx (SImode);

		/* We may get a paradoxical subreg here.  Strip it off.  */
		if (GET_CODE (dest) == SUBREG
		    && GET_MODE (dest) == SImode
		    && GET_MODE (SUBREG_REG (dest)) == HImode)
		  dest = SUBREG_REG (dest);

		if (GET_MODE_BITSIZE (GET_MODE (dest)) != width)
		  FAIL;

		base_addr = adjust_address (operands[1], HImode,
					    bitpos / BITS_PER_UNIT);
		emit_insn (gen_unaligned_loadhiu (tmp, base_addr));
		emit_move_insn (gen_lowpart (SImode, dest), tmp);
	      }
	    DONE;
	  }
	else if (s_register_operand (operands[1], GET_MODE (operands[1])))
	  {
	    emit_insn (gen_extzv_t2 (operands[0], operands[1], operands[2],
				     operands[3]));
	    DONE;
	  }
	else
	  FAIL;
      }
    
    if (!s_register_operand (operands[1], GET_MODE (operands[1])))
      FAIL;

    operands[3] = GEN_INT (rshift);
    
    if (lshift == 0)
      {
        emit_insn (gen_lshrsi3 (operands[0], operands[1], operands[3]));
        DONE;
      }
      
    emit_insn (gen_extzv_t1 (operands[0], operands[1], GEN_INT (lshift),
			     operands[3], gen_reg_rtx (SImode)));
    DONE;
  }"
)

;; Helper for extzv, for the Thumb-1 register-shifts case.

(define_expand "extzv_t1"
  [(set (match_operand:SI 4 "s_register_operand" "")
	(ashift:SI (match_operand:SI 1 "nonimmediate_operand" "")
		   (match_operand:SI 2 "const_int_operand" "")))
   (set (match_operand:SI 0 "s_register_operand" "")
	(lshiftrt:SI (match_dup 4)
		     (match_operand:SI 3 "const_int_operand" "")))]
  "TARGET_THUMB1"
  "")

(define_expand "extv"
  [(set (match_operand 0 "s_register_operand" "")
	(sign_extract (match_operand 1 "nonimmediate_operand" "")
		      (match_operand 2 "const_int_operand" "")
		      (match_operand 3 "const_int_operand" "")))]
  "arm_arch_thumb2"
{
  HOST_WIDE_INT width = INTVAL (operands[2]);
  HOST_WIDE_INT bitpos = INTVAL (operands[3]);

  if (unaligned_access && MEM_P (operands[1]) && (width == 16 || width == 32)
      && (bitpos % BITS_PER_UNIT)  == 0)
    {
      rtx base_addr;
      
      if (BYTES_BIG_ENDIAN)
	bitpos = GET_MODE_BITSIZE (GET_MODE (operands[0])) - width - bitpos;
      
      if (width == 32)
        {
	  base_addr = adjust_address (operands[1], SImode,
				      bitpos / BITS_PER_UNIT);
	  emit_insn (gen_unaligned_loadsi (operands[0], base_addr));
        }
      else
        {
	  rtx dest = operands[0];
	  rtx tmp = gen_reg_rtx (SImode);
	  
	  /* We may get a paradoxical subreg here.  Strip it off.  */
	  if (GET_CODE (dest) == SUBREG
	      && GET_MODE (dest) == SImode
	      && GET_MODE (SUBREG_REG (dest)) == HImode)
	    dest = SUBREG_REG (dest);
	  
	  if (GET_MODE_BITSIZE (GET_MODE (dest)) != width)
	    FAIL;
	  
	  base_addr = adjust_address (operands[1], HImode,
				      bitpos / BITS_PER_UNIT);
	  emit_insn (gen_unaligned_loadhis (tmp, base_addr));
	  emit_move_insn (gen_lowpart (SImode, dest), tmp);
	}

      DONE;
    }
  else if (!s_register_operand (operands[1], GET_MODE (operands[1])))
    FAIL;
  else if (GET_MODE (operands[0]) == SImode
	   && GET_MODE (operands[1]) == SImode)
    {
      emit_insn (gen_extv_regsi (operands[0], operands[1], operands[2],
				 operands[3]));
      DONE;
    }

  FAIL;
})

; Helper to expand register forms of extv with the proper modes.

(define_expand "extv_regsi"
  [(set (match_operand:SI 0 "s_register_operand" "")
	(sign_extract:SI (match_operand:SI 1 "s_register_operand" "")
			 (match_operand 2 "const_int_operand" "")
			 (match_operand 3 "const_int_operand" "")))]
  ""
{
})

; ARMv6+ unaligned load/store instructions (used for packed structure accesses).

(define_insn "unaligned_loadsi"
  [(set (match_operand:SI 0 "s_register_operand" "=l,r")
	(unspec:SI [(match_operand:SI 1 "memory_operand" "Uw,m")]
		   UNSPEC_UNALIGNED_LOAD))]
  "unaligned_access"
  "ldr%?\t%0, %1\t@ unaligned"
  [(set_attr "arch" "t2,any")
   (set_attr "length" "2,4")
   (set_attr "predicable" "yes")
   (set_attr "predicable_short_it" "yes,no")
   (set_attr "type" "load1")])

(define_insn "unaligned_loadhis"
  [(set (match_operand:SI 0 "s_register_operand" "=l,r")
	(sign_extend:SI
	  (unspec:HI [(match_operand:HI 1 "memory_operand" "Uw,Uh")]
		     UNSPEC_UNALIGNED_LOAD)))]
  "unaligned_access"
  "ldrsh%?\t%0, %1\t@ unaligned"
  [(set_attr "arch" "t2,any")
   (set_attr "length" "2,4")
   (set_attr "predicable" "yes")
   (set_attr "predicable_short_it" "yes,no")
   (set_attr "type" "load_byte")])

(define_insn "unaligned_loadhiu"
  [(set (match_operand:SI 0 "s_register_operand" "=l,r")
	(zero_extend:SI
	  (unspec:HI [(match_operand:HI 1 "memory_operand" "Uw,m")]
		     UNSPEC_UNALIGNED_LOAD)))]
  "unaligned_access"
  "ldrh%?\t%0, %1\t@ unaligned"
  [(set_attr "arch" "t2,any")
   (set_attr "length" "2,4")
   (set_attr "predicable" "yes")
   (set_attr "predicable_short_it" "yes,no")
   (set_attr "type" "load_byte")])

(define_insn "unaligned_storesi"
  [(set (match_operand:SI 0 "memory_operand" "=Uw,m")
	(unspec:SI [(match_operand:SI 1 "s_register_operand" "l,r")]
		   UNSPEC_UNALIGNED_STORE))]
  "unaligned_access"
  "str%?\t%1, %0\t@ unaligned"
  [(set_attr "arch" "t2,any")
   (set_attr "length" "2,4")
   (set_attr "predicable" "yes")
   (set_attr "predicable_short_it" "yes,no")
   (set_attr "type" "store1")])

(define_insn "unaligned_storehi"
  [(set (match_operand:HI 0 "memory_operand" "=Uw,m")
	(unspec:HI [(match_operand:HI 1 "s_register_operand" "l,r")]
		   UNSPEC_UNALIGNED_STORE))]
  "unaligned_access"
  "strh%?\t%1, %0\t@ unaligned"
  [(set_attr "arch" "t2,any")
   (set_attr "length" "2,4")
   (set_attr "predicable" "yes")
   (set_attr "predicable_short_it" "yes,no")
   (set_attr "type" "store1")])


(define_insn "*extv_reg"
  [(set (match_operand:SI 0 "s_register_operand" "=r")
	(sign_extract:SI (match_operand:SI 1 "s_register_operand" "r")
                         (match_operand:SI 2 "const_int_M_operand" "M")
                         (match_operand:SI 3 "const_int_M_operand" "M")))]
  "arm_arch_thumb2"
  "sbfx%?\t%0, %1, %3, %2"
  [(set_attr "length" "4")
   (set_attr "predicable" "yes")
   (set_attr "predicable_short_it" "no")
   (set_attr "type" "bfm")]
)

(define_insn "extzv_t2"
  [(set (match_operand:SI 0 "s_register_operand" "=r")
	(zero_extract:SI (match_operand:SI 1 "s_register_operand" "r")
                         (match_operand:SI 2 "const_int_M_operand" "M")
                         (match_operand:SI 3 "const_int_M_operand" "M")))]
  "arm_arch_thumb2"
  "ubfx%?\t%0, %1, %3, %2"
  [(set_attr "length" "4")
   (set_attr "predicable" "yes")
   (set_attr "predicable_short_it" "no")
   (set_attr "type" "bfm")]
)


;; Division instructions
(define_insn "divsi3"
  [(set (match_operand:SI	  0 "s_register_operand" "=r,r")
	(div:SI (match_operand:SI 1 "s_register_operand"  "r,r")
		(match_operand:SI 2 "s_register_operand"  "r,r")))]
  "TARGET_IDIV"
  "@
   sdiv%?\t%0, %1, %2
   sdiv\t%0, %1, %2"
  [(set_attr "arch" "32,v8mb")
   (set_attr "predicable" "yes")
   (set_attr "predicable_short_it" "no")
   (set_attr "type" "sdiv")]
)

(define_insn "udivsi3"
  [(set (match_operand:SI	   0 "s_register_operand" "=r,r")
	(udiv:SI (match_operand:SI 1 "s_register_operand"  "r,r")
		 (match_operand:SI 2 "s_register_operand"  "r,r")))]
  "TARGET_IDIV"
  "@
   udiv%?\t%0, %1, %2
   udiv\t%0, %1, %2"
  [(set_attr "arch" "32,v8mb")
   (set_attr "predicable" "yes")
   (set_attr "predicable_short_it" "no")
   (set_attr "type" "udiv")]
)


;; Unary arithmetic insns

(define_expand "negvsi3"
  [(match_operand:SI 0 "register_operand")
   (match_operand:SI 1 "register_operand")
   (match_operand 2 "")]
  "TARGET_32BIT"
{
  emit_insn (gen_subsi3_compare (operands[0], const0_rtx, operands[1]));
  arm_gen_unlikely_cbranch (NE, CC_Vmode, operands[2]);

  DONE;
})

(define_expand "negvdi3"
  [(match_operand:DI 0 "register_operand")
   (match_operand:DI 1 "register_operand")
   (match_operand 2 "")]
  "TARGET_ARM"
{
  emit_insn (gen_negdi2_compare (operands[0], operands[1]));
  arm_gen_unlikely_cbranch (NE, CC_Vmode, operands[2]);

  DONE;
})


(define_insn_and_split "negdi2_compare"
  [(set (reg:CC CC_REGNUM)
	(compare:CC
	  (const_int 0)
	  (match_operand:DI 1 "register_operand" "0,r")))
   (set (match_operand:DI 0 "register_operand" "=r,&r")
	(minus:DI (const_int 0) (match_dup 1)))]
  "TARGET_ARM"
  "#"
  "&& reload_completed"
  [(parallel [(set (reg:CC CC_REGNUM)
		   (compare:CC (const_int 0) (match_dup 1)))
	      (set (match_dup 0) (minus:SI (const_int 0)
					   (match_dup 1)))])
   (parallel [(set (reg:CC CC_REGNUM)
		   (compare:CC (const_int 0) (match_dup 3)))
	     (set (match_dup 2)
		  (minus:SI
		   (minus:SI (const_int 0) (match_dup 3))
		   (ltu:SI (reg:CC_C CC_REGNUM)
			   (const_int 0))))])]
  {
    operands[2] = gen_highpart (SImode, operands[0]);
    operands[0] = gen_lowpart (SImode, operands[0]);
    operands[3] = gen_highpart (SImode, operands[1]);
    operands[1] = gen_lowpart (SImode, operands[1]);
  }
  [(set_attr "conds" "set")
   (set_attr "length" "8")
   (set_attr "type" "multiple")]
)

(define_expand "negdi2"
 [(parallel
   [(set (match_operand:DI 0 "s_register_operand" "")
	 (neg:DI (match_operand:DI 1 "s_register_operand" "")))
    (clobber (reg:CC CC_REGNUM))])]
  "TARGET_EITHER"
  {
    if (TARGET_NEON)
      {
        emit_insn (gen_negdi2_neon (operands[0], operands[1]));
	DONE;
      }
  }
)

;; The constraints here are to prevent a *partial* overlap (where %Q0 == %R1).
;; The first alternative allows the common case of a *full* overlap.
(define_insn_and_split "*arm_negdi2"
  [(set (match_operand:DI         0 "s_register_operand" "=r,&r")
	(neg:DI (match_operand:DI 1 "s_register_operand"  "0,r")))
   (clobber (reg:CC CC_REGNUM))]
  "TARGET_ARM"
  "#"   ; "rsbs\\t%Q0, %Q1, #0\;rsc\\t%R0, %R1, #0"
  "&& reload_completed"
  [(parallel [(set (reg:CC CC_REGNUM)
		   (compare:CC (const_int 0) (match_dup 1)))
	      (set (match_dup 0) (minus:SI (const_int 0) (match_dup 1)))])
   (set (match_dup 2) (minus:SI (minus:SI (const_int 0) (match_dup 3))
                                (ltu:SI (reg:CC_C CC_REGNUM) (const_int 0))))]
  {
    operands[2] = gen_highpart (SImode, operands[0]);
    operands[0] = gen_lowpart (SImode, operands[0]);
    operands[3] = gen_highpart (SImode, operands[1]);
    operands[1] = gen_lowpart (SImode, operands[1]);
  }
  [(set_attr "conds" "clob")
   (set_attr "length" "8")
   (set_attr "type" "multiple")]
)

(define_insn "*negsi2_carryin_compare"
  [(set (reg:CC CC_REGNUM)
	(compare:CC (const_int 0)
		    (match_operand:SI 1 "s_register_operand" "r")))
   (set (match_operand:SI 0 "s_register_operand" "=r")
	(minus:SI (minus:SI (const_int 0)
			    (match_dup 1))
		  (ltu:SI (reg:CC_C CC_REGNUM) (const_int 0))))]
  "TARGET_ARM"
  "rscs\\t%0, %1, #0"
  [(set_attr "conds" "set")
   (set_attr "type" "alus_imm")]
)

(define_expand "negsi2"
  [(set (match_operand:SI         0 "s_register_operand" "")
	(neg:SI (match_operand:SI 1 "s_register_operand" "")))]
  "TARGET_EITHER"
  ""
)

(define_insn "*arm_negsi2"
  [(set (match_operand:SI         0 "s_register_operand" "=l,r")
	(neg:SI (match_operand:SI 1 "s_register_operand" "l,r")))]
  "TARGET_32BIT"
  "rsb%?\\t%0, %1, #0"
  [(set_attr "predicable" "yes")
   (set_attr "predicable_short_it" "yes,no")
   (set_attr "arch" "t2,*")
   (set_attr "length" "4")
   (set_attr "type" "alu_sreg")]
)

(define_expand "negsf2"
  [(set (match_operand:SF         0 "s_register_operand" "")
	(neg:SF (match_operand:SF 1 "s_register_operand" "")))]
  "TARGET_32BIT && TARGET_HARD_FLOAT && TARGET_VFP"
  ""
)

(define_expand "negdf2"
  [(set (match_operand:DF         0 "s_register_operand" "")
	(neg:DF (match_operand:DF 1 "s_register_operand" "")))]
  "TARGET_32BIT && TARGET_HARD_FLOAT && TARGET_VFP_DOUBLE"
  "")

(define_insn_and_split "*zextendsidi_negsi"
  [(set (match_operand:DI 0 "s_register_operand" "=r")
        (zero_extend:DI (neg:SI (match_operand:SI 1 "s_register_operand" "r"))))]
   "TARGET_32BIT"
   "#"
   ""
   [(set (match_dup 2)
         (neg:SI (match_dup 1)))
    (set (match_dup 3)
         (const_int 0))]
   {
      operands[2] = gen_lowpart (SImode, operands[0]);
      operands[3] = gen_highpart (SImode, operands[0]);
   }
 [(set_attr "length" "8")
  (set_attr "type" "multiple")]
)

;; Negate an extended 32-bit value.
(define_insn_and_split "*negdi_extendsidi"
  [(set (match_operand:DI 0 "s_register_operand" "=l,r")
	(neg:DI (sign_extend:DI
		 (match_operand:SI 1 "s_register_operand" "l,r"))))
   (clobber (reg:CC CC_REGNUM))]
  "TARGET_32BIT"
  "#"
  "&& reload_completed"
  [(const_int 0)]
  {
    rtx low = gen_lowpart (SImode, operands[0]);
    rtx high = gen_highpart (SImode, operands[0]);

    if (reg_overlap_mentioned_p (low, operands[1]))
      {
	/* Input overlaps the low word of the output.  Use:
		asr	Rhi, Rin, #31
		rsbs	Rlo, Rin, #0
		rsc	Rhi, Rhi, #0 (thumb2: sbc Rhi, Rhi, Rhi, lsl #1).  */
	rtx cc_reg = gen_rtx_REG (CC_Cmode, CC_REGNUM);

	emit_insn (gen_rtx_SET (high,
				gen_rtx_ASHIFTRT (SImode, operands[1],
						  GEN_INT (31))));

	emit_insn (gen_subsi3_compare (low, const0_rtx, operands[1]));
	if (TARGET_ARM)
	  emit_insn (gen_rtx_SET (high,
				  gen_rtx_MINUS (SImode,
						 gen_rtx_MINUS (SImode,
								const0_rtx,
								high),
						 gen_rtx_LTU (SImode,
							      cc_reg,
							      const0_rtx))));
	else
	  {
	    rtx two_x = gen_rtx_ASHIFT (SImode, high, GEN_INT (1));
	    emit_insn (gen_rtx_SET (high,
				    gen_rtx_MINUS (SImode,
						   gen_rtx_MINUS (SImode,
								  high,
								  two_x),
						   gen_rtx_LTU (SImode,
								cc_reg,
								const0_rtx))));
	  }
      }
    else
      {
	/* No overlap, or overlap on high word.  Use:
		rsb	Rlo, Rin, #0
		bic	Rhi, Rlo, Rin
		asr	Rhi, Rhi, #31
	   Flags not needed for this sequence.  */
	emit_insn (gen_rtx_SET (low, gen_rtx_NEG (SImode, operands[1])));
	emit_insn (gen_rtx_SET (high,
				gen_rtx_AND (SImode,
					     gen_rtx_NOT (SImode, operands[1]),
					     low)));
	emit_insn (gen_rtx_SET (high,
				gen_rtx_ASHIFTRT (SImode, high,
						  GEN_INT (31))));
      }
    DONE;
  }
  [(set_attr "length" "12")
   (set_attr "arch" "t2,*")
   (set_attr "type" "multiple")]
)

(define_insn_and_split "*negdi_zero_extendsidi"
  [(set (match_operand:DI 0 "s_register_operand" "=r,&r")
	(neg:DI (zero_extend:DI (match_operand:SI 1 "s_register_operand" "0,r"))))
   (clobber (reg:CC CC_REGNUM))]
  "TARGET_32BIT"
  "#" ; "rsbs\\t%Q0, %1, #0\;sbc\\t%R0,%R0,%R0"
      ;; Don't care what register is input to sbc,
      ;; since we just need to propagate the carry.
  "&& reload_completed"
  [(parallel [(set (reg:CC CC_REGNUM)
                   (compare:CC (const_int 0) (match_dup 1)))
              (set (match_dup 0) (minus:SI (const_int 0) (match_dup 1)))])
   (set (match_dup 2) (minus:SI (minus:SI (match_dup 2) (match_dup 2))
                                (ltu:SI (reg:CC_C CC_REGNUM) (const_int 0))))]
  {
    operands[2] = gen_highpart (SImode, operands[0]);
    operands[0] = gen_lowpart (SImode, operands[0]);
  }
  [(set_attr "conds" "clob")
   (set_attr "length" "8")
   (set_attr "type" "multiple")]   ;; length in thumb is 4
)

;; abssi2 doesn't really clobber the condition codes if a different register
;; is being set.  To keep things simple, assume during rtl manipulations that
;; it does, but tell the final scan operator the truth.  Similarly for
;; (neg (abs...))

(define_expand "abssi2"
  [(parallel
    [(set (match_operand:SI         0 "s_register_operand" "")
	  (abs:SI (match_operand:SI 1 "s_register_operand" "")))
     (clobber (match_dup 2))])]
  "TARGET_EITHER"
  "
  if (TARGET_THUMB1)
    operands[2] = gen_rtx_SCRATCH (SImode);
  else
    operands[2] = gen_rtx_REG (CCmode, CC_REGNUM);
")

(define_insn_and_split "*arm_abssi2"
  [(set (match_operand:SI 0 "s_register_operand" "=r,&r")
	(abs:SI (match_operand:SI 1 "s_register_operand" "0,r")))
   (clobber (reg:CC CC_REGNUM))]
  "TARGET_ARM"
  "#"
  "&& reload_completed"
  [(const_int 0)]
  {
   /* if (which_alternative == 0) */
   if (REGNO(operands[0]) == REGNO(operands[1]))
     {
      /* Emit the pattern:
         cmp\\t%0, #0\;rsblt\\t%0, %0, #0
         [(set (reg:CC CC_REGNUM)
               (compare:CC (match_dup 0) (const_int 0)))
          (cond_exec (lt:CC (reg:CC CC_REGNUM) (const_int 0))
                     (set (match_dup 0) (minus:SI (const_int 0) (match_dup 1))))]
      */
      emit_insn (gen_rtx_SET (gen_rtx_REG (CCmode, CC_REGNUM),
                              gen_rtx_COMPARE (CCmode, operands[0], const0_rtx)));
      emit_insn (gen_rtx_COND_EXEC (VOIDmode,
                                    (gen_rtx_LT (SImode,
                                                 gen_rtx_REG (CCmode, CC_REGNUM),
                                                 const0_rtx)),
                                    (gen_rtx_SET (operands[0],
                                                  (gen_rtx_MINUS (SImode,
                                                                  const0_rtx,
                                                                  operands[1]))))));
      DONE;
     }
   else
     {
      /* Emit the pattern:
         alt1: eor%?\\t%0, %1, %1, asr #31\;sub%?\\t%0, %0, %1, asr #31
         [(set (match_dup 0)
               (xor:SI (match_dup 1)
                       (ashiftrt:SI (match_dup 1) (const_int 31))))
          (set (match_dup 0)
               (minus:SI (match_dup 0)
                      (ashiftrt:SI (match_dup 1) (const_int 31))))]
      */
      emit_insn (gen_rtx_SET (operands[0],
                              gen_rtx_XOR (SImode,
                                           gen_rtx_ASHIFTRT (SImode,
                                                             operands[1],
                                                             GEN_INT (31)),
                                           operands[1])));
      emit_insn (gen_rtx_SET (operands[0],
                              gen_rtx_MINUS (SImode,
                                             operands[0],
                                             gen_rtx_ASHIFTRT (SImode,
                                                               operands[1],
                                                               GEN_INT (31)))));
      DONE;
     }
  }
  [(set_attr "conds" "clob,*")
   (set_attr "shift" "1")
   (set_attr "predicable" "no, yes")
   (set_attr "length" "8")
   (set_attr "type" "multiple")]
)

(define_insn_and_split "*arm_neg_abssi2"
  [(set (match_operand:SI 0 "s_register_operand" "=r,&r")
	(neg:SI (abs:SI (match_operand:SI 1 "s_register_operand" "0,r"))))
   (clobber (reg:CC CC_REGNUM))]
  "TARGET_ARM"
  "#"
  "&& reload_completed"
  [(const_int 0)]
  {
   /* if (which_alternative == 0) */
   if (REGNO (operands[0]) == REGNO (operands[1]))
     {
      /* Emit the pattern:
         cmp\\t%0, #0\;rsbgt\\t%0, %0, #0
      */
      emit_insn (gen_rtx_SET (gen_rtx_REG (CCmode, CC_REGNUM),
                              gen_rtx_COMPARE (CCmode, operands[0], const0_rtx)));
      emit_insn (gen_rtx_COND_EXEC (VOIDmode,
                                    gen_rtx_GT (SImode,
                                                gen_rtx_REG (CCmode, CC_REGNUM),
                                                const0_rtx),
                                    gen_rtx_SET (operands[0],
                                                 (gen_rtx_MINUS (SImode,
                                                                 const0_rtx,
                                                                 operands[1])))));
     }
   else
     {
      /* Emit the pattern:
         eor%?\\t%0, %1, %1, asr #31\;rsb%?\\t%0, %0, %1, asr #31
      */
      emit_insn (gen_rtx_SET (operands[0],
                              gen_rtx_XOR (SImode,
                                           gen_rtx_ASHIFTRT (SImode,
                                                             operands[1],
                                                             GEN_INT (31)),
                                           operands[1])));
      emit_insn (gen_rtx_SET (operands[0],
                              gen_rtx_MINUS (SImode,
                                             gen_rtx_ASHIFTRT (SImode,
                                                               operands[1],
                                                               GEN_INT (31)),
                                             operands[0])));
     }
   DONE;
  }
  [(set_attr "conds" "clob,*")
   (set_attr "shift" "1")
   (set_attr "predicable" "no, yes")
   (set_attr "length" "8")
   (set_attr "type" "multiple")]
)

(define_expand "abssf2"
  [(set (match_operand:SF         0 "s_register_operand" "")
	(abs:SF (match_operand:SF 1 "s_register_operand" "")))]
  "TARGET_32BIT && TARGET_HARD_FLOAT"
  "")

(define_expand "absdf2"
  [(set (match_operand:DF         0 "s_register_operand" "")
	(abs:DF (match_operand:DF 1 "s_register_operand" "")))]
  "TARGET_32BIT && TARGET_HARD_FLOAT && !TARGET_VFP_SINGLE"
  "")

(define_expand "sqrtsf2"
  [(set (match_operand:SF 0 "s_register_operand" "")
	(sqrt:SF (match_operand:SF 1 "s_register_operand" "")))]
  "TARGET_32BIT && TARGET_HARD_FLOAT && TARGET_VFP"
  "")

(define_expand "sqrtdf2"
  [(set (match_operand:DF 0 "s_register_operand" "")
	(sqrt:DF (match_operand:DF 1 "s_register_operand" "")))]
  "TARGET_32BIT && TARGET_HARD_FLOAT && TARGET_VFP_DOUBLE"
  "")

(define_insn_and_split "one_cmpldi2"
  [(set (match_operand:DI 0 "s_register_operand"	 "=w,&r,&r,?w")
	(not:DI (match_operand:DI 1 "s_register_operand" " w, 0, r, w")))]
  "TARGET_32BIT"
  "@
   vmvn\t%P0, %P1
   #
   #
   vmvn\t%P0, %P1"
  "TARGET_32BIT && reload_completed
   && arm_general_register_operand (operands[0], DImode)"
  [(set (match_dup 0) (not:SI (match_dup 1)))
   (set (match_dup 2) (not:SI (match_dup 3)))]
  "
  {
    operands[2] = gen_highpart (SImode, operands[0]);
    operands[0] = gen_lowpart (SImode, operands[0]);
    operands[3] = gen_highpart (SImode, operands[1]);
    operands[1] = gen_lowpart (SImode, operands[1]);
  }"
  [(set_attr "length" "*,8,8,*")
   (set_attr "predicable" "no,yes,yes,no")
   (set_attr "type" "neon_move,multiple,multiple,neon_move")
   (set_attr "arch" "neon_for_64bits,*,*,avoid_neon_for_64bits")]
)

(define_expand "one_cmplsi2"
  [(set (match_operand:SI         0 "s_register_operand" "")
	(not:SI (match_operand:SI 1 "s_register_operand" "")))]
  "TARGET_EITHER"
  ""
)

(define_insn "*arm_one_cmplsi2"
  [(set (match_operand:SI         0 "s_register_operand" "=l,r")
	(not:SI (match_operand:SI 1 "s_register_operand"  "l,r")))]
  "TARGET_32BIT"
  "mvn%?\\t%0, %1"
  [(set_attr "predicable" "yes")
   (set_attr "predicable_short_it" "yes,no")
   (set_attr "arch" "t2,*")
   (set_attr "length" "4")
   (set_attr "type" "mvn_reg")]
)

(define_insn "*notsi_compare0"
  [(set (reg:CC_NOOV CC_REGNUM)
	(compare:CC_NOOV (not:SI (match_operand:SI 1 "s_register_operand" "r"))
			 (const_int 0)))
   (set (match_operand:SI 0 "s_register_operand" "=r")
	(not:SI (match_dup 1)))]
  "TARGET_32BIT"
  "mvns%?\\t%0, %1"
  [(set_attr "conds" "set")
   (set_attr "type" "mvn_reg")]
)

(define_insn "*notsi_compare0_scratch"
  [(set (reg:CC_NOOV CC_REGNUM)
	(compare:CC_NOOV (not:SI (match_operand:SI 1 "s_register_operand" "r"))
			 (const_int 0)))
   (clobber (match_scratch:SI 0 "=r"))]
  "TARGET_32BIT"
  "mvns%?\\t%0, %1"
  [(set_attr "conds" "set")
   (set_attr "type" "mvn_reg")]
)

;; Fixed <--> Floating conversion insns

(define_expand "floatsihf2"
  [(set (match_operand:HF           0 "general_operand" "")
	(float:HF (match_operand:SI 1 "general_operand" "")))]
  "TARGET_EITHER"
  "
  {
    rtx op1 = gen_reg_rtx (SFmode);
    expand_float (op1, operands[1], 0);
    op1 = convert_to_mode (HFmode, op1, 0);
    emit_move_insn (operands[0], op1);
    DONE;
  }"
)

(define_expand "floatdihf2"
  [(set (match_operand:HF           0 "general_operand" "")
	(float:HF (match_operand:DI 1 "general_operand" "")))]
  "TARGET_EITHER"
  "
  {
    rtx op1 = gen_reg_rtx (SFmode);
    expand_float (op1, operands[1], 0);
    op1 = convert_to_mode (HFmode, op1, 0);
    emit_move_insn (operands[0], op1);
    DONE;
  }"
)

(define_expand "floatsisf2"
  [(set (match_operand:SF           0 "s_register_operand" "")
	(float:SF (match_operand:SI 1 "s_register_operand" "")))]
  "TARGET_32BIT && TARGET_HARD_FLOAT"
  "
")

(define_expand "floatsidf2"
  [(set (match_operand:DF           0 "s_register_operand" "")
	(float:DF (match_operand:SI 1 "s_register_operand" "")))]
  "TARGET_32BIT && TARGET_HARD_FLOAT && !TARGET_VFP_SINGLE"
  "
")

(define_expand "fix_trunchfsi2"
  [(set (match_operand:SI         0 "general_operand" "")
	(fix:SI (fix:HF (match_operand:HF 1 "general_operand"  ""))))]
  "TARGET_EITHER"
  "
  {
    rtx op1 = convert_to_mode (SFmode, operands[1], 0);
    expand_fix (operands[0], op1, 0);
    DONE;
  }"
)

(define_expand "fix_trunchfdi2"
  [(set (match_operand:DI         0 "general_operand" "")
	(fix:DI (fix:HF (match_operand:HF 1 "general_operand"  ""))))]
  "TARGET_EITHER"
  "
  {
    rtx op1 = convert_to_mode (SFmode, operands[1], 0);
    expand_fix (operands[0], op1, 0);
    DONE;
  }"
)

(define_expand "fix_truncsfsi2"
  [(set (match_operand:SI         0 "s_register_operand" "")
	(fix:SI (fix:SF (match_operand:SF 1 "s_register_operand"  ""))))]
  "TARGET_32BIT && TARGET_HARD_FLOAT"
  "
")

(define_expand "fix_truncdfsi2"
  [(set (match_operand:SI         0 "s_register_operand" "")
	(fix:SI (fix:DF (match_operand:DF 1 "s_register_operand"  ""))))]
  "TARGET_32BIT && TARGET_HARD_FLOAT && !TARGET_VFP_SINGLE"
  "
")

;; Truncation insns

(define_expand "truncdfsf2"
  [(set (match_operand:SF  0 "s_register_operand" "")
	(float_truncate:SF
 	 (match_operand:DF 1 "s_register_operand" "")))]
  "TARGET_32BIT && TARGET_HARD_FLOAT && !TARGET_VFP_SINGLE"
  ""
)

/* DFmode -> HFmode conversions have to go through SFmode.  */
(define_expand "truncdfhf2"
  [(set (match_operand:HF  0 "general_operand" "")
	(float_truncate:HF
 	 (match_operand:DF 1 "general_operand" "")))]
  "TARGET_EITHER"
  "
  {
    rtx op1;
    op1 = convert_to_mode (SFmode, operands[1], 0);
    op1 = convert_to_mode (HFmode, op1, 0);
    emit_move_insn (operands[0], op1);
    DONE;
  }"
)

;; Zero and sign extension instructions.

(define_insn "zero_extend<mode>di2"
  [(set (match_operand:DI 0 "s_register_operand" "=w,r,?r,w")
        (zero_extend:DI (match_operand:QHSI 1 "<qhs_zextenddi_op>"
					    "<qhs_zextenddi_cstr>")))]
  "TARGET_32BIT <qhs_zextenddi_cond>"
  "#"
  [(set_attr "length" "8,4,8,8")
   (set_attr "arch" "neon_for_64bits,*,*,avoid_neon_for_64bits")
   (set_attr "ce_count" "2")
   (set_attr "predicable" "yes")
   (set_attr "type" "multiple,mov_reg,multiple,multiple")]
)

(define_insn "extend<mode>di2"
  [(set (match_operand:DI 0 "s_register_operand" "=w,r,?r,?r,w")
        (sign_extend:DI (match_operand:QHSI 1 "<qhs_extenddi_op>"
					    "<qhs_extenddi_cstr>")))]
  "TARGET_32BIT <qhs_sextenddi_cond>"
  "#"
  [(set_attr "length" "8,4,8,8,8")
   (set_attr "ce_count" "2")
   (set_attr "shift" "1")
   (set_attr "predicable" "yes")
   (set_attr "arch" "neon_for_64bits,*,a,t,avoid_neon_for_64bits")
   (set_attr "type" "multiple,mov_reg,multiple,multiple,multiple")]
)

;; Splits for all extensions to DImode
(define_split
  [(set (match_operand:DI 0 "s_register_operand" "")
        (zero_extend:DI (match_operand 1 "nonimmediate_operand" "")))]
  "TARGET_32BIT && reload_completed && !IS_VFP_REGNUM (REGNO (operands[0]))"
  [(set (match_dup 0) (match_dup 1))]
{
  rtx lo_part = gen_lowpart (SImode, operands[0]);
  machine_mode src_mode = GET_MODE (operands[1]);

  if (REG_P (operands[0])
      && !reg_overlap_mentioned_p (operands[0], operands[1]))
    emit_clobber (operands[0]);
  if (!REG_P (lo_part) || src_mode != SImode
      || !rtx_equal_p (lo_part, operands[1]))
    {
      if (src_mode == SImode)
        emit_move_insn (lo_part, operands[1]);
      else
        emit_insn (gen_rtx_SET (lo_part,
				gen_rtx_ZERO_EXTEND (SImode, operands[1])));
      operands[1] = lo_part;
    }
  operands[0] = gen_highpart (SImode, operands[0]);
  operands[1] = const0_rtx;
})

(define_split
  [(set (match_operand:DI 0 "s_register_operand" "")
        (sign_extend:DI (match_operand 1 "nonimmediate_operand" "")))]
  "TARGET_32BIT && reload_completed && !IS_VFP_REGNUM (REGNO (operands[0]))"
  [(set (match_dup 0) (ashiftrt:SI (match_dup 1) (const_int 31)))]
{
  rtx lo_part = gen_lowpart (SImode, operands[0]);
  machine_mode src_mode = GET_MODE (operands[1]);

  if (REG_P (operands[0])
      && !reg_overlap_mentioned_p (operands[0], operands[1]))
    emit_clobber (operands[0]);

  if (!REG_P (lo_part) || src_mode != SImode
      || !rtx_equal_p (lo_part, operands[1]))
    {
      if (src_mode == SImode)
        emit_move_insn (lo_part, operands[1]);
      else
        emit_insn (gen_rtx_SET (lo_part,
				gen_rtx_SIGN_EXTEND (SImode, operands[1])));
      operands[1] = lo_part;
    }
  operands[0] = gen_highpart (SImode, operands[0]);
})

(define_expand "zero_extendhisi2"
  [(set (match_operand:SI 0 "s_register_operand" "")
	(zero_extend:SI (match_operand:HI 1 "nonimmediate_operand" "")))]
  "TARGET_EITHER"
{
  if (TARGET_ARM && !arm_arch4 && MEM_P (operands[1]))
    {
      emit_insn (gen_movhi_bytes (operands[0], operands[1]));
      DONE;
    }
  if (!arm_arch6 && !MEM_P (operands[1]))
    {
      rtx t = gen_lowpart (SImode, operands[1]);
      rtx tmp = gen_reg_rtx (SImode);
      emit_insn (gen_ashlsi3 (tmp, t, GEN_INT (16)));
      emit_insn (gen_lshrsi3 (operands[0], tmp, GEN_INT (16)));
      DONE;
    }
})

(define_split
  [(set (match_operand:SI 0 "s_register_operand" "")
	(zero_extend:SI (match_operand:HI 1 "s_register_operand" "")))]
  "!TARGET_THUMB2 && !arm_arch6"
  [(set (match_dup 0) (ashift:SI (match_dup 2) (const_int 16)))
   (set (match_dup 0) (lshiftrt:SI (match_dup 0) (const_int 16)))]
{
  operands[2] = gen_lowpart (SImode, operands[1]);
})

(define_insn "*arm_zero_extendhisi2"
  [(set (match_operand:SI 0 "s_register_operand" "=r,r")
	(zero_extend:SI (match_operand:HI 1 "nonimmediate_operand" "r,m")))]
  "TARGET_ARM && arm_arch4 && !arm_arch6"
  "@
   #
   ldrh%?\\t%0, %1"
  [(set_attr "type" "alu_shift_reg,load_byte")
   (set_attr "predicable" "yes")]
)

(define_insn "*arm_zero_extendhisi2_v6"
  [(set (match_operand:SI 0 "s_register_operand" "=r,r")
	(zero_extend:SI (match_operand:HI 1 "nonimmediate_operand" "r,Uh")))]
  "TARGET_ARM && arm_arch6"
  "@
   uxth%?\\t%0, %1
   ldrh%?\\t%0, %1"
  [(set_attr "predicable" "yes")
   (set_attr "type" "extend,load_byte")]
)

(define_insn "*arm_zero_extendhisi2addsi"
  [(set (match_operand:SI 0 "s_register_operand" "=r")
	(plus:SI (zero_extend:SI (match_operand:HI 1 "s_register_operand" "r"))
		 (match_operand:SI 2 "s_register_operand" "r")))]
  "TARGET_INT_SIMD"
  "uxtah%?\\t%0, %2, %1"
  [(set_attr "type" "alu_shift_reg")
   (set_attr "predicable" "yes")
   (set_attr "predicable_short_it" "no")]
)

(define_expand "zero_extendqisi2"
  [(set (match_operand:SI 0 "s_register_operand" "")
	(zero_extend:SI (match_operand:QI 1 "nonimmediate_operand" "")))]
  "TARGET_EITHER"
{
  if (TARGET_ARM && !arm_arch6 && !MEM_P (operands[1]))
    {
      emit_insn (gen_andsi3 (operands[0],
			     gen_lowpart (SImode, operands[1]),
					  GEN_INT (255)));
      DONE;
    }
  if (!arm_arch6 && !MEM_P (operands[1]))
    {
      rtx t = gen_lowpart (SImode, operands[1]);
      rtx tmp = gen_reg_rtx (SImode);
      emit_insn (gen_ashlsi3 (tmp, t, GEN_INT (24)));
      emit_insn (gen_lshrsi3 (operands[0], tmp, GEN_INT (24)));
      DONE;
    }
})

(define_split
  [(set (match_operand:SI 0 "s_register_operand" "")
	(zero_extend:SI (match_operand:QI 1 "s_register_operand" "")))]
  "!arm_arch6"
  [(set (match_dup 0) (ashift:SI (match_dup 2) (const_int 24)))
   (set (match_dup 0) (lshiftrt:SI (match_dup 0) (const_int 24)))]
{
  operands[2] = simplify_gen_subreg (SImode, operands[1], QImode, 0);
  if (TARGET_ARM)
    {
      emit_insn (gen_andsi3 (operands[0], operands[2], GEN_INT (255)));
      DONE;
    }
})

(define_insn "*arm_zero_extendqisi2"
  [(set (match_operand:SI 0 "s_register_operand" "=r,r")
	(zero_extend:SI (match_operand:QI 1 "nonimmediate_operand" "r,m")))]
  "TARGET_ARM && !arm_arch6"
  "@
   #
   ldrb%?\\t%0, %1\\t%@ zero_extendqisi2"
  [(set_attr "length" "8,4")
   (set_attr "type" "alu_shift_reg,load_byte")
   (set_attr "predicable" "yes")]
)

(define_insn "*arm_zero_extendqisi2_v6"
  [(set (match_operand:SI 0 "s_register_operand" "=r,r")
	(zero_extend:SI (match_operand:QI 1 "nonimmediate_operand" "r,Uh")))]
  "TARGET_ARM && arm_arch6"
  "@
   uxtb%?\\t%0, %1
   ldrb%?\\t%0, %1\\t%@ zero_extendqisi2"
  [(set_attr "type" "extend,load_byte")
   (set_attr "predicable" "yes")]
)

(define_insn "*arm_zero_extendqisi2addsi"
  [(set (match_operand:SI 0 "s_register_operand" "=r")
	(plus:SI (zero_extend:SI (match_operand:QI 1 "s_register_operand" "r"))
		 (match_operand:SI 2 "s_register_operand" "r")))]
  "TARGET_INT_SIMD"
  "uxtab%?\\t%0, %2, %1"
  [(set_attr "predicable" "yes")
   (set_attr "predicable_short_it" "no")
   (set_attr "type" "alu_shift_reg")]
)

(define_split
  [(set (match_operand:SI 0 "s_register_operand" "")
	(zero_extend:SI (subreg:QI (match_operand:SI 1 "" "") 0)))
   (clobber (match_operand:SI 2 "s_register_operand" ""))]
  "TARGET_32BIT && (!MEM_P (operands[1])) && ! BYTES_BIG_ENDIAN"
  [(set (match_dup 2) (match_dup 1))
   (set (match_dup 0) (and:SI (match_dup 2) (const_int 255)))]
  ""
)

(define_split
  [(set (match_operand:SI 0 "s_register_operand" "")
	(zero_extend:SI (subreg:QI (match_operand:SI 1 "" "") 3)))
   (clobber (match_operand:SI 2 "s_register_operand" ""))]
  "TARGET_32BIT && (!MEM_P (operands[1])) && BYTES_BIG_ENDIAN"
  [(set (match_dup 2) (match_dup 1))
   (set (match_dup 0) (and:SI (match_dup 2) (const_int 255)))]
  ""
)


(define_split
  [(set (match_operand:SI 0 "s_register_operand" "")
	(IOR_XOR:SI (and:SI (ashift:SI
			     (match_operand:SI 1 "s_register_operand" "")
			     (match_operand:SI 2 "const_int_operand" ""))
			    (match_operand:SI 3 "const_int_operand" ""))
		    (zero_extend:SI
		     (match_operator 5 "subreg_lowpart_operator"
		      [(match_operand:SI 4 "s_register_operand" "")]))))]
  "TARGET_32BIT
   && (UINTVAL (operands[3])
       == (GET_MODE_MASK (GET_MODE (operands[5]))
           & (GET_MODE_MASK (GET_MODE (operands[5]))
	      << (INTVAL (operands[2])))))"
  [(set (match_dup 0) (IOR_XOR:SI (ashift:SI (match_dup 1) (match_dup 2))
				  (match_dup 4)))
   (set (match_dup 0) (zero_extend:SI (match_dup 5)))]
  "operands[5] = gen_lowpart (GET_MODE (operands[5]), operands[0]);"
)

(define_insn "*compareqi_eq0"
  [(set (reg:CC_Z CC_REGNUM)
	(compare:CC_Z (match_operand:QI 0 "s_register_operand" "r")
			 (const_int 0)))]
  "TARGET_32BIT"
  "tst%?\\t%0, #255"
  [(set_attr "conds" "set")
   (set_attr "predicable" "yes")
   (set_attr "predicable_short_it" "no")
   (set_attr "type" "logic_imm")]
)

(define_expand "extendhisi2"
  [(set (match_operand:SI 0 "s_register_operand" "")
	(sign_extend:SI (match_operand:HI 1 "nonimmediate_operand" "")))]
  "TARGET_EITHER"
{
  if (TARGET_THUMB1)
    {
      emit_insn (gen_thumb1_extendhisi2 (operands[0], operands[1]));
      DONE;
    }
  if (MEM_P (operands[1]) && TARGET_ARM && !arm_arch4)
    {
      emit_insn (gen_extendhisi2_mem (operands[0], operands[1]));
      DONE;
    }

  if (!arm_arch6 && !MEM_P (operands[1]))
    {
      rtx t = gen_lowpart (SImode, operands[1]);
      rtx tmp = gen_reg_rtx (SImode);
      emit_insn (gen_ashlsi3 (tmp, t, GEN_INT (16)));
      emit_insn (gen_ashrsi3 (operands[0], tmp, GEN_INT (16)));
      DONE;
    }
})

(define_split
  [(parallel
    [(set (match_operand:SI 0 "register_operand" "")
	  (sign_extend:SI (match_operand:HI 1 "register_operand" "")))
     (clobber (match_scratch:SI 2 ""))])]
  "!arm_arch6"
  [(set (match_dup 0) (ashift:SI (match_dup 2) (const_int 16)))
   (set (match_dup 0) (ashiftrt:SI (match_dup 0) (const_int 16)))]
{
  operands[2] = simplify_gen_subreg (SImode, operands[1], HImode, 0);
})

;; This pattern will only be used when ldsh is not available
(define_expand "extendhisi2_mem"
  [(set (match_dup 2) (zero_extend:SI (match_operand:HI 1 "" "")))
   (set (match_dup 3)
	(zero_extend:SI (match_dup 7)))
   (set (match_dup 6) (ashift:SI (match_dup 4) (const_int 24)))
   (set (match_operand:SI 0 "" "")
	(ior:SI (ashiftrt:SI (match_dup 6) (const_int 16)) (match_dup 5)))]
  "TARGET_ARM"
  "
  {
    rtx mem1, mem2;
    rtx addr = copy_to_mode_reg (SImode, XEXP (operands[1], 0));

    mem1 = change_address (operands[1], QImode, addr);
    mem2 = change_address (operands[1], QImode,
			   plus_constant (Pmode, addr, 1));
    operands[0] = gen_lowpart (SImode, operands[0]);
    operands[1] = mem1;
    operands[2] = gen_reg_rtx (SImode);
    operands[3] = gen_reg_rtx (SImode);
    operands[6] = gen_reg_rtx (SImode);
    operands[7] = mem2;

    if (BYTES_BIG_ENDIAN)
      {
	operands[4] = operands[2];
	operands[5] = operands[3];
      }
    else
      {
	operands[4] = operands[3];
	operands[5] = operands[2];
      }
  }"
)

(define_split
  [(set (match_operand:SI 0 "register_operand" "")
	(sign_extend:SI (match_operand:HI 1 "register_operand" "")))]
  "!arm_arch6"
  [(set (match_dup 0) (ashift:SI (match_dup 2) (const_int 16)))
   (set (match_dup 0) (ashiftrt:SI (match_dup 0) (const_int 16)))]
{
  operands[2] = simplify_gen_subreg (SImode, operands[1], HImode, 0);
})

(define_insn "*arm_extendhisi2"
  [(set (match_operand:SI 0 "s_register_operand" "=r,r")
	(sign_extend:SI (match_operand:HI 1 "nonimmediate_operand" "r,Uh")))]
  "TARGET_ARM && arm_arch4 && !arm_arch6"
  "@
   #
   ldrsh%?\\t%0, %1"
  [(set_attr "length" "8,4")
   (set_attr "type" "alu_shift_reg,load_byte")
   (set_attr "predicable" "yes")]
)

;; ??? Check Thumb-2 pool range
(define_insn "*arm_extendhisi2_v6"
  [(set (match_operand:SI 0 "s_register_operand" "=r,r")
	(sign_extend:SI (match_operand:HI 1 "nonimmediate_operand" "r,Uh")))]
  "TARGET_32BIT && arm_arch6"
  "@
   sxth%?\\t%0, %1
   ldrsh%?\\t%0, %1"
  [(set_attr "type" "extend,load_byte")
   (set_attr "predicable" "yes")
   (set_attr "predicable_short_it" "no")]
)

(define_insn "*arm_extendhisi2addsi"
  [(set (match_operand:SI 0 "s_register_operand" "=r")
	(plus:SI (sign_extend:SI (match_operand:HI 1 "s_register_operand" "r"))
		 (match_operand:SI 2 "s_register_operand" "r")))]
  "TARGET_INT_SIMD"
  "sxtah%?\\t%0, %2, %1"
  [(set_attr "type" "alu_shift_reg")]
)

(define_expand "extendqihi2"
  [(set (match_dup 2)
	(ashift:SI (match_operand:QI 1 "arm_reg_or_extendqisi_mem_op" "")
		   (const_int 24)))
   (set (match_operand:HI 0 "s_register_operand" "")
	(ashiftrt:SI (match_dup 2)
		     (const_int 24)))]
  "TARGET_ARM"
  "
  {
    if (arm_arch4 && MEM_P (operands[1]))
      {
	emit_insn (gen_rtx_SET (operands[0],
				gen_rtx_SIGN_EXTEND (HImode, operands[1])));
	DONE;
      }
    if (!s_register_operand (operands[1], QImode))
      operands[1] = copy_to_mode_reg (QImode, operands[1]);
    operands[0] = gen_lowpart (SImode, operands[0]);
    operands[1] = gen_lowpart (SImode, operands[1]);
    operands[2] = gen_reg_rtx (SImode);
  }"
)

(define_insn "*arm_extendqihi_insn"
  [(set (match_operand:HI 0 "s_register_operand" "=r")
	(sign_extend:HI (match_operand:QI 1 "arm_extendqisi_mem_op" "Uq")))]
  "TARGET_ARM && arm_arch4"
  "ldrsb%?\\t%0, %1"
  [(set_attr "type" "load_byte")
   (set_attr "predicable" "yes")]
)

(define_expand "extendqisi2"
  [(set (match_operand:SI 0 "s_register_operand" "")
	(sign_extend:SI (match_operand:QI 1 "arm_reg_or_extendqisi_mem_op" "")))]
  "TARGET_EITHER"
{
  if (!arm_arch4 && MEM_P (operands[1]))
    operands[1] = copy_to_mode_reg (QImode, operands[1]);

  if (!arm_arch6 && !MEM_P (operands[1]))
    {
      rtx t = gen_lowpart (SImode, operands[1]);
      rtx tmp = gen_reg_rtx (SImode);
      emit_insn (gen_ashlsi3 (tmp, t, GEN_INT (24)));
      emit_insn (gen_ashrsi3 (operands[0], tmp, GEN_INT (24)));
      DONE;
    }
})

(define_split
  [(set (match_operand:SI 0 "register_operand" "")
	(sign_extend:SI (match_operand:QI 1 "register_operand" "")))]
  "!arm_arch6"
  [(set (match_dup 0) (ashift:SI (match_dup 2) (const_int 24)))
   (set (match_dup 0) (ashiftrt:SI (match_dup 0) (const_int 24)))]
{
  operands[2] = simplify_gen_subreg (SImode, operands[1], QImode, 0);
})

(define_insn "*arm_extendqisi"
  [(set (match_operand:SI 0 "s_register_operand" "=r,r")
	(sign_extend:SI (match_operand:QI 1 "arm_reg_or_extendqisi_mem_op" "r,Uq")))]
  "TARGET_ARM && arm_arch4 && !arm_arch6"
  "@
   #
   ldrsb%?\\t%0, %1"
  [(set_attr "length" "8,4")
   (set_attr "type" "alu_shift_reg,load_byte")
   (set_attr "predicable" "yes")]
)

(define_insn "*arm_extendqisi_v6"
  [(set (match_operand:SI 0 "s_register_operand" "=r,r")
	(sign_extend:SI
	 (match_operand:QI 1 "arm_reg_or_extendqisi_mem_op" "r,Uq")))]
  "TARGET_ARM && arm_arch6"
  "@
   sxtb%?\\t%0, %1
   ldrsb%?\\t%0, %1"
  [(set_attr "type" "extend,load_byte")
   (set_attr "predicable" "yes")]
)

(define_insn "*arm_extendqisi2addsi"
  [(set (match_operand:SI 0 "s_register_operand" "=r")
	(plus:SI (sign_extend:SI (match_operand:QI 1 "s_register_operand" "r"))
		 (match_operand:SI 2 "s_register_operand" "r")))]
  "TARGET_INT_SIMD"
  "sxtab%?\\t%0, %2, %1"
  [(set_attr "type" "alu_shift_reg")
   (set_attr "predicable" "yes")
   (set_attr "predicable_short_it" "no")]
)

(define_expand "extendsfdf2"
  [(set (match_operand:DF                  0 "s_register_operand" "")
	(float_extend:DF (match_operand:SF 1 "s_register_operand"  "")))]
  "TARGET_32BIT && TARGET_HARD_FLOAT && !TARGET_VFP_SINGLE"
  ""
)

/* HFmode -> DFmode conversions have to go through SFmode.  */
(define_expand "extendhfdf2"
  [(set (match_operand:DF                  0 "general_operand" "")
	(float_extend:DF (match_operand:HF 1 "general_operand"  "")))]
  "TARGET_EITHER"
  "
  {
    rtx op1;
    op1 = convert_to_mode (SFmode, operands[1], 0);
    op1 = convert_to_mode (DFmode, op1, 0);
    emit_insn (gen_movdf (operands[0], op1));
    DONE;
  }"
)

;; Move insns (including loads and stores)

;; XXX Just some ideas about movti.
;; I don't think these are a good idea on the arm, there just aren't enough
;; registers
;;(define_expand "loadti"
;;  [(set (match_operand:TI 0 "s_register_operand" "")
;;	(mem:TI (match_operand:SI 1 "address_operand" "")))]
;;  "" "")

;;(define_expand "storeti"
;;  [(set (mem:TI (match_operand:TI 0 "address_operand" ""))
;;	(match_operand:TI 1 "s_register_operand" ""))]
;;  "" "")

;;(define_expand "movti"
;;  [(set (match_operand:TI 0 "general_operand" "")
;;	(match_operand:TI 1 "general_operand" ""))]
;;  ""
;;  "
;;{
;;  rtx insn;
;;
;;  if (MEM_P (operands[0]) && MEM_P (operands[1]))
;;    operands[1] = copy_to_reg (operands[1]);
;;  if (MEM_P (operands[0]))
;;    insn = gen_storeti (XEXP (operands[0], 0), operands[1]);
;;  else if (MEM_P (operands[1]))
;;    insn = gen_loadti (operands[0], XEXP (operands[1], 0));
;;  else
;;    FAIL;
;;
;;  emit_insn (insn);
;;  DONE;
;;}")

;; Recognize garbage generated above.

;;(define_insn ""
;;  [(set (match_operand:TI 0 "general_operand" "=r,r,r,<,>,m")
;;	(match_operand:TI 1 "general_operand" "<,>,m,r,r,r"))]
;;  ""
;;  "*
;;  {
;;    register mem = (which_alternative < 3);
;;    register const char *template;
;;
;;    operands[mem] = XEXP (operands[mem], 0);
;;    switch (which_alternative)
;;      {
;;      case 0: template = \"ldmdb\\t%1!, %M0\"; break;
;;      case 1: template = \"ldmia\\t%1!, %M0\"; break;
;;      case 2: template = \"ldmia\\t%1, %M0\"; break;
;;      case 3: template = \"stmdb\\t%0!, %M1\"; break;
;;      case 4: template = \"stmia\\t%0!, %M1\"; break;
;;      case 5: template = \"stmia\\t%0, %M1\"; break;
;;      }
;;    output_asm_insn (template, operands);
;;    return \"\";
;;  }")

(define_expand "movdi"
  [(set (match_operand:DI 0 "general_operand" "")
	(match_operand:DI 1 "general_operand" ""))]
  "TARGET_EITHER"
  "
  if (can_create_pseudo_p ())
    {
      if (!REG_P (operands[0]))
	operands[1] = force_reg (DImode, operands[1]);
    }
  if (REG_P (operands[0]) && REGNO (operands[0]) <= LAST_ARM_REGNUM
      && !HARD_REGNO_MODE_OK (REGNO (operands[0]), DImode))
    {
      /* Avoid LDRD's into an odd-numbered register pair in ARM state
	 when expanding function calls.  */
      gcc_assert (can_create_pseudo_p ());
      if (MEM_P (operands[1]) && MEM_VOLATILE_P (operands[1]))
	{
	  /* Perform load into legal reg pair first, then move.  */
	  rtx reg = gen_reg_rtx (DImode);
	  emit_insn (gen_movdi (reg, operands[1]));
	  operands[1] = reg;
	}
      emit_move_insn (gen_lowpart (SImode, operands[0]),
		      gen_lowpart (SImode, operands[1]));
      emit_move_insn (gen_highpart (SImode, operands[0]),
		      gen_highpart (SImode, operands[1]));
      DONE;
    }
  else if (REG_P (operands[1]) && REGNO (operands[1]) <= LAST_ARM_REGNUM
	   && !HARD_REGNO_MODE_OK (REGNO (operands[1]), DImode))
    {
      /* Avoid STRD's from an odd-numbered register pair in ARM state
	 when expanding function prologue.  */
      gcc_assert (can_create_pseudo_p ());
      rtx split_dest = (MEM_P (operands[0]) && MEM_VOLATILE_P (operands[0]))
		       ? gen_reg_rtx (DImode)
		       : operands[0];
      emit_move_insn (gen_lowpart (SImode, split_dest),
		      gen_lowpart (SImode, operands[1]));
      emit_move_insn (gen_highpart (SImode, split_dest),
		      gen_highpart (SImode, operands[1]));
      if (split_dest != operands[0])
	emit_insn (gen_movdi (operands[0], split_dest));
      DONE;
    }
  "
)

(define_insn "*arm_movdi"
  [(set (match_operand:DI 0 "nonimmediate_di_operand" "=r, r, r, q, m")
	(match_operand:DI 1 "di_operand"              "rDa,Db,Dc,mi,q"))]
  "TARGET_32BIT
   && !(TARGET_HARD_FLOAT && TARGET_VFP)
   && !TARGET_IWMMXT
   && (   register_operand (operands[0], DImode)
       || register_operand (operands[1], DImode))"
  "*
  switch (which_alternative)
    {
    case 0:
    case 1:
    case 2:
      return \"#\";
    default:
      return output_move_double (operands, true, NULL);
    }
  "
  [(set_attr "length" "8,12,16,8,8")
   (set_attr "type" "multiple,multiple,multiple,load2,store2")
   (set_attr "arm_pool_range" "*,*,*,1020,*")
   (set_attr "arm_neg_pool_range" "*,*,*,1004,*")
   (set_attr "thumb2_pool_range" "*,*,*,4094,*")
   (set_attr "thumb2_neg_pool_range" "*,*,*,0,*")]
)

(define_split
  [(set (match_operand:ANY64 0 "arm_general_register_operand" "")
	(match_operand:ANY64 1 "immediate_operand" ""))]
  "TARGET_32BIT
   && reload_completed
   && (arm_const_double_inline_cost (operands[1])
       <= arm_max_const_double_inline_cost ())"
  [(const_int 0)]
  "
  arm_split_constant (SET, SImode, curr_insn,
		      INTVAL (gen_lowpart (SImode, operands[1])),
		      gen_lowpart (SImode, operands[0]), NULL_RTX, 0);
  arm_split_constant (SET, SImode, curr_insn,
		      INTVAL (gen_highpart_mode (SImode,
						 GET_MODE (operands[0]),
						 operands[1])),
		      gen_highpart (SImode, operands[0]), NULL_RTX, 0);
  DONE;
  "
)

; If optimizing for size, or if we have load delay slots, then 
; we want to split the constant into two separate operations. 
; In both cases this may split a trivial part into a single data op
; leaving a single complex constant to load.  We can also get longer
; offsets in a LDR which means we get better chances of sharing the pool
; entries.  Finally, we can normally do a better job of scheduling
; LDR instructions than we can with LDM.
; This pattern will only match if the one above did not.
(define_split
  [(set (match_operand:ANY64 0 "arm_general_register_operand" "")
	(match_operand:ANY64 1 "const_double_operand" ""))]
  "TARGET_ARM && reload_completed
   && arm_const_double_by_parts (operands[1])"
  [(set (match_dup 0) (match_dup 1))
   (set (match_dup 2) (match_dup 3))]
  "
  operands[2] = gen_highpart (SImode, operands[0]);
  operands[3] = gen_highpart_mode (SImode, GET_MODE (operands[0]),
				   operands[1]);
  operands[0] = gen_lowpart (SImode, operands[0]);
  operands[1] = gen_lowpart (SImode, operands[1]);
  "
)

(define_split
  [(set (match_operand:ANY64 0 "arm_general_register_operand" "")
	(match_operand:ANY64 1 "arm_general_register_operand" ""))]
  "TARGET_EITHER && reload_completed"
  [(set (match_dup 0) (match_dup 1))
   (set (match_dup 2) (match_dup 3))]
  "
  operands[2] = gen_highpart (SImode, operands[0]);
  operands[3] = gen_highpart (SImode, operands[1]);
  operands[0] = gen_lowpart (SImode, operands[0]);
  operands[1] = gen_lowpart (SImode, operands[1]);

  /* Handle a partial overlap.  */
  if (rtx_equal_p (operands[0], operands[3]))
    {
      rtx tmp0 = operands[0];
      rtx tmp1 = operands[1];

      operands[0] = operands[2];
      operands[1] = operands[3];
      operands[2] = tmp0;
      operands[3] = tmp1;
    }
  "
)

;; We can't actually do base+index doubleword loads if the index and
;; destination overlap.  Split here so that we at least have chance to
;; schedule.
(define_split
  [(set (match_operand:DI 0 "s_register_operand" "")
	(mem:DI (plus:SI (match_operand:SI 1 "s_register_operand" "")
			 (match_operand:SI 2 "s_register_operand" ""))))]
  "TARGET_LDRD
  && reg_overlap_mentioned_p (operands[0], operands[1])
  && reg_overlap_mentioned_p (operands[0], operands[2])"
  [(set (match_dup 4)
	(plus:SI (match_dup 1)
		 (match_dup 2)))
   (set (match_dup 0)
	(mem:DI (match_dup 4)))]
  "
  operands[4] = gen_rtx_REG (SImode, REGNO(operands[0]));
  "
)

(define_expand "movsi"
  [(set (match_operand:SI 0 "general_operand" "")
        (match_operand:SI 1 "general_operand" ""))]
  "TARGET_EITHER"
  "
  {
  rtx base, offset, tmp;

  if (TARGET_32BIT)
    {
      /* Everything except mem = const or mem = mem can be done easily.  */
      if (MEM_P (operands[0]))
        operands[1] = force_reg (SImode, operands[1]);
      if (arm_general_register_operand (operands[0], SImode)
	  && CONST_INT_P (operands[1])
          && !(const_ok_for_arm (INTVAL (operands[1]))
               || const_ok_for_arm (~INTVAL (operands[1]))))
        {
	   if (DONT_EARLY_SPLIT_CONSTANT (INTVAL (operands[1]), SET))
	     {
		emit_insn (gen_rtx_SET (operands[0], operands[1]));
		DONE;
	     }
	  else
	     {
		arm_split_constant (SET, SImode, NULL_RTX,
	                            INTVAL (operands[1]), operands[0], NULL_RTX,
			            optimize && can_create_pseudo_p ());
		DONE;
	     }
        }
    }
  else /* TARGET_THUMB1...  */
    {
      if (can_create_pseudo_p ())
        {
          if (!REG_P (operands[0]))
	    operands[1] = force_reg (SImode, operands[1]);
        }
    }

  if (ARM_OFFSETS_MUST_BE_WITHIN_SECTIONS_P)
    {
      split_const (operands[1], &base, &offset);
      if (GET_CODE (base) == SYMBOL_REF
	  && !offset_within_block_p (base, INTVAL (offset)))
	{
	  tmp = can_create_pseudo_p () ? gen_reg_rtx (SImode) : operands[0];
	  emit_move_insn (tmp, base);
	  emit_insn (gen_addsi3 (operands[0], tmp, offset));
	  DONE;
	}
    }

  /* Recognize the case where operand[1] is a reference to thread-local
     data and load its address to a register.  */
  if (arm_tls_referenced_p (operands[1]))
    {
      rtx tmp = operands[1];
      rtx addend = NULL;

      if (GET_CODE (tmp) == CONST && GET_CODE (XEXP (tmp, 0)) == PLUS)
        {
          addend = XEXP (XEXP (tmp, 0), 1);
          tmp = XEXP (XEXP (tmp, 0), 0);
        }

      gcc_assert (GET_CODE (tmp) == SYMBOL_REF);
      gcc_assert (SYMBOL_REF_TLS_MODEL (tmp) != 0);

      tmp = legitimize_tls_address (tmp,
				    !can_create_pseudo_p () ? operands[0] : 0);
      if (addend)
        {
          tmp = gen_rtx_PLUS (SImode, tmp, addend);
          tmp = force_operand (tmp, operands[0]);
        }
      operands[1] = tmp;
    }
  else if (flag_pic
	   && (CONSTANT_P (operands[1])
	       || symbol_mentioned_p (operands[1])
	       || label_mentioned_p (operands[1])))
      operands[1] = legitimize_pic_address (operands[1], SImode,
					    (!can_create_pseudo_p ()
					     ? operands[0]
					     : 0));
  }
  "
)

;; The ARM LO_SUM and HIGH are backwards - HIGH sets the low bits, and
;; LO_SUM adds in the high bits.  Fortunately these are opaque operations
;; so this does not matter.
(define_insn "*arm_movt"
  [(set (match_operand:SI 0 "nonimmediate_operand" "=r,r")
	(lo_sum:SI (match_operand:SI 1 "nonimmediate_operand" "0,0")
		   (match_operand:SI 2 "general_operand"      "i,i")))]
  "TARGET_HAVE_MOVT && arm_valid_symbolic_address_p (operands[2])"
  "@
   movt%?\t%0, #:upper16:%c2
   movt\t%0, #:upper16:%c2"
  [(set_attr "arch"  "32,v8mb")
   (set_attr "predicable" "yes")
   (set_attr "predicable_short_it" "no")
   (set_attr "length" "4")
   (set_attr "type" "alu_sreg")]
)

(define_insn "*arm_movsi_insn"
  [(set (match_operand:SI 0 "nonimmediate_operand" "=rk,r,r,r,rk,m")
	(match_operand:SI 1 "general_operand"      "rk, I,K,j,mi,rk"))]
  "TARGET_ARM && ! TARGET_IWMMXT
   && !(TARGET_HARD_FLOAT && TARGET_VFP)
   && (   register_operand (operands[0], SImode)
       || register_operand (operands[1], SImode))"
  "@
   mov%?\\t%0, %1
   mov%?\\t%0, %1
   mvn%?\\t%0, #%B1
   movw%?\\t%0, %1
   ldr%?\\t%0, %1
   str%?\\t%1, %0"
  [(set_attr "type" "mov_reg,mov_imm,mvn_imm,mov_imm,load1,store1")
   (set_attr "predicable" "yes")
   (set_attr "pool_range" "*,*,*,*,4096,*")
   (set_attr "neg_pool_range" "*,*,*,*,4084,*")]
)

(define_split
  [(set (match_operand:SI 0 "arm_general_register_operand" "")
	(match_operand:SI 1 "const_int_operand" ""))]
  "TARGET_32BIT
  && (!(const_ok_for_arm (INTVAL (operands[1]))
        || const_ok_for_arm (~INTVAL (operands[1]))))"
  [(clobber (const_int 0))]
  "
  arm_split_constant (SET, SImode, NULL_RTX, 
                      INTVAL (operands[1]), operands[0], NULL_RTX, 0);
  DONE;
  "
)

;; A normal way to do (symbol + offset) requires three instructions at least
;; (depends on how big the offset is) as below:
;; movw r0, #:lower16:g
;; movw r0, #:upper16:g
;; adds r0, #4
;;
;; A better way would be:
;; movw r0, #:lower16:g+4
;; movw r0, #:upper16:g+4
;;
;; The limitation of this way is that the length of offset should be a 16-bit
;; signed value, because current assembler only supports REL type relocation for
;; such case.  If the more powerful RELA type is supported in future, we should
;; update this pattern to go with better way.
(define_split
  [(set (match_operand:SI 0 "arm_general_register_operand" "")
	(const:SI (plus:SI (match_operand:SI 1 "general_operand" "")
			   (match_operand:SI 2 "const_int_operand" ""))))]
  "TARGET_THUMB
   && TARGET_HAVE_MOVT
   && arm_disable_literal_pool
   && reload_completed
   && GET_CODE (operands[1]) == SYMBOL_REF"
  [(clobber (const_int 0))]
  "
    int offset = INTVAL (operands[2]);

    if (offset < -0x8000 || offset > 0x7fff)
      {
	arm_emit_movpair (operands[0], operands[1]);
	emit_insn (gen_rtx_SET (operands[0],
				gen_rtx_PLUS (SImode, operands[0], operands[2])));
      }
    else
      {
	rtx op = gen_rtx_CONST (SImode,
				gen_rtx_PLUS (SImode, operands[1], operands[2]));
	arm_emit_movpair (operands[0], op);
      }
  "
)

;; Split symbol_refs at the later stage (after cprop), instead of generating
;; movt/movw pair directly at expand.  Otherwise corresponding high_sum
;; and lo_sum would be merged back into memory load at cprop.  However,
;; if the default is to prefer movt/movw rather than a load from the constant
;; pool, the performance is better.
(define_split
  [(set (match_operand:SI 0 "arm_general_register_operand" "")
       (match_operand:SI 1 "general_operand" ""))]
  "TARGET_USE_MOVT && GET_CODE (operands[1]) == SYMBOL_REF
   && !flag_pic && !target_word_relocations
   && !arm_tls_referenced_p (operands[1])"
  [(clobber (const_int 0))]
{
  arm_emit_movpair (operands[0], operands[1]);
  DONE;
})

;; When generating pic, we need to load the symbol offset into a register.
;; So that the optimizer does not confuse this with a normal symbol load
;; we use an unspec.  The offset will be loaded from a constant pool entry,
;; since that is the only type of relocation we can use.

;; Wrap calculation of the whole PIC address in a single pattern for the
;; benefit of optimizers, particularly, PRE and HOIST.  Calculation of
;; a PIC address involves two loads from memory, so we want to CSE it
;; as often as possible.
;; This pattern will be split into one of the pic_load_addr_* patterns
;; and a move after GCSE optimizations.
;;
;; Note: Update arm.c: legitimize_pic_address() when changing this pattern.
(define_expand "calculate_pic_address"
  [(set (match_operand:SI 0 "register_operand" "")
	(mem:SI (plus:SI (match_operand:SI 1 "register_operand" "")
			 (unspec:SI [(match_operand:SI 2 "" "")]
				    UNSPEC_PIC_SYM))))]
  "flag_pic"
)

;; Split calculate_pic_address into pic_load_addr_* and a move.
(define_split
  [(set (match_operand:SI 0 "register_operand" "")
	(mem:SI (plus:SI (match_operand:SI 1 "register_operand" "")
			 (unspec:SI [(match_operand:SI 2 "" "")]
				    UNSPEC_PIC_SYM))))]
  "flag_pic"
  [(set (match_dup 3) (unspec:SI [(match_dup 2)] UNSPEC_PIC_SYM))
   (set (match_dup 0) (mem:SI (plus:SI (match_dup 1) (match_dup 3))))]
  "operands[3] = can_create_pseudo_p () ? gen_reg_rtx (SImode) : operands[0];"
)

;; operand1 is the memory address to go into 
;; pic_load_addr_32bit.
;; operand2 is the PIC label to be emitted 
;; from pic_add_dot_plus_eight.
;; We do this to allow hoisting of the entire insn.
(define_insn_and_split "pic_load_addr_unified"
  [(set (match_operand:SI 0 "s_register_operand" "=r,r,l")
	(unspec:SI [(match_operand:SI 1 "" "mX,mX,mX") 
		    (match_operand:SI 2 "" "")] 
		    UNSPEC_PIC_UNIFIED))]
 "flag_pic"
 "#"
 "&& reload_completed"
 [(set (match_dup 0) (unspec:SI [(match_dup 1)] UNSPEC_PIC_SYM))
  (set (match_dup 0) (unspec:SI [(match_dup 0) (match_dup 3)
       		     		 (match_dup 2)] UNSPEC_PIC_BASE))]
 "operands[3] = TARGET_THUMB ? GEN_INT (4) : GEN_INT (8);"
 [(set_attr "type" "load1,load1,load1")
  (set_attr "pool_range" "4096,4094,1022")
  (set_attr "neg_pool_range" "4084,0,0")
  (set_attr "arch"  "a,t2,t1")    
  (set_attr "length" "8,6,4")]
)

;; The rather odd constraints on the following are to force reload to leave
;; the insn alone, and to force the minipool generation pass to then move
;; the GOT symbol to memory.

(define_insn "pic_load_addr_32bit"
  [(set (match_operand:SI 0 "s_register_operand" "=r")
	(unspec:SI [(match_operand:SI 1 "" "mX")] UNSPEC_PIC_SYM))]
  "TARGET_32BIT && flag_pic"
  "ldr%?\\t%0, %1"
  [(set_attr "type" "load1")
   (set (attr "pool_range")
	(if_then_else (eq_attr "is_thumb" "no")
		      (const_int 4096)
		      (const_int 4094)))
   (set (attr "neg_pool_range")
	(if_then_else (eq_attr "is_thumb" "no")
		      (const_int 4084)
		      (const_int 0)))]
)

(define_insn "pic_load_addr_thumb1"
  [(set (match_operand:SI 0 "s_register_operand" "=l")
	(unspec:SI [(match_operand:SI 1 "" "mX")] UNSPEC_PIC_SYM))]
  "TARGET_THUMB1 && flag_pic"
  "ldr\\t%0, %1"
  [(set_attr "type" "load1")
   (set (attr "pool_range") (const_int 1018))]
)

(define_insn "pic_add_dot_plus_four"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(unspec:SI [(match_operand:SI 1 "register_operand" "0")
		    (const_int 4)
		    (match_operand 2 "" "")]
		   UNSPEC_PIC_BASE))]
  "TARGET_THUMB"
  "*
  (*targetm.asm_out.internal_label) (asm_out_file, \"LPIC\",
				     INTVAL (operands[2]));
  return \"add\\t%0, %|pc\";
  "
  [(set_attr "length" "2")
   (set_attr "type" "alu_sreg")]
)

(define_insn "pic_add_dot_plus_eight"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(unspec:SI [(match_operand:SI 1 "register_operand" "r")
		    (const_int 8)
		    (match_operand 2 "" "")]
		   UNSPEC_PIC_BASE))]
  "TARGET_ARM"
  "*
    (*targetm.asm_out.internal_label) (asm_out_file, \"LPIC\",
				       INTVAL (operands[2]));
    return \"add%?\\t%0, %|pc, %1\";
  "
  [(set_attr "predicable" "yes")
   (set_attr "type" "alu_sreg")]
)

(define_insn "tls_load_dot_plus_eight"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(mem:SI (unspec:SI [(match_operand:SI 1 "register_operand" "r")
			    (const_int 8)
			    (match_operand 2 "" "")]
			   UNSPEC_PIC_BASE)))]
  "TARGET_ARM"
  "*
    (*targetm.asm_out.internal_label) (asm_out_file, \"LPIC\",
				       INTVAL (operands[2]));
    return \"ldr%?\\t%0, [%|pc, %1]\t\t@ tls_load_dot_plus_eight\";
  "
  [(set_attr "predicable" "yes")
   (set_attr "type" "load1")]
)

;; PIC references to local variables can generate pic_add_dot_plus_eight
;; followed by a load.  These sequences can be crunched down to
;; tls_load_dot_plus_eight by a peephole.

(define_peephole2
  [(set (match_operand:SI 0 "register_operand" "")
	(unspec:SI [(match_operand:SI 3 "register_operand" "")
		    (const_int 8)
		    (match_operand 1 "" "")]
		   UNSPEC_PIC_BASE))
   (set (match_operand:SI 2 "arm_general_register_operand" "")
	(mem:SI (match_dup 0)))]
  "TARGET_ARM && peep2_reg_dead_p (2, operands[0])"
  [(set (match_dup 2)
	(mem:SI (unspec:SI [(match_dup 3)
			    (const_int 8)
			    (match_dup 1)]
			   UNSPEC_PIC_BASE)))]
  ""
)

(define_insn "pic_offset_arm"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(mem:SI (plus:SI (match_operand:SI 1 "register_operand" "r")
			 (unspec:SI [(match_operand:SI 2 "" "X")]
				    UNSPEC_PIC_OFFSET))))]
  "TARGET_VXWORKS_RTP && TARGET_ARM && flag_pic"
  "ldr%?\\t%0, [%1,%2]"
  [(set_attr "type" "load1")]
)

(define_expand "builtin_setjmp_receiver"
  [(label_ref (match_operand 0 "" ""))]
  "flag_pic"
  "
{
  /* r3 is clobbered by set/longjmp, so we can use it as a scratch
     register.  */
  if (arm_pic_register != INVALID_REGNUM)
    arm_load_pic_register (1UL << 3);
  DONE;
}")

;; If copying one reg to another we can set the condition codes according to
;; its value.  Such a move is common after a return from subroutine and the
;; result is being tested against zero.

(define_insn "*movsi_compare0"
  [(set (reg:CC CC_REGNUM)
	(compare:CC (match_operand:SI 1 "s_register_operand" "0,r")
		    (const_int 0)))
   (set (match_operand:SI 0 "s_register_operand" "=r,r")
	(match_dup 1))]
  "TARGET_32BIT"
  "@
   cmp%?\\t%0, #0
   subs%?\\t%0, %1, #0"
  [(set_attr "conds" "set")
   (set_attr "type" "alus_imm,alus_imm")]
)

;; Subroutine to store a half word from a register into memory.
;; Operand 0 is the source register (HImode)
;; Operand 1 is the destination address in a register (SImode)

;; In both this routine and the next, we must be careful not to spill
;; a memory address of reg+large_const into a separate PLUS insn, since this
;; can generate unrecognizable rtl.

(define_expand "storehi"
  [;; store the low byte
   (set (match_operand 1 "" "") (match_dup 3))
   ;; extract the high byte
   (set (match_dup 2)
	(ashiftrt:SI (match_operand 0 "" "") (const_int 8)))
   ;; store the high byte
   (set (match_dup 4) (match_dup 5))]
  "TARGET_ARM"
  "
  {
    rtx op1 = operands[1];
    rtx addr = XEXP (op1, 0);
    enum rtx_code code = GET_CODE (addr);

    if ((code == PLUS && !CONST_INT_P (XEXP (addr, 1)))
	|| code == MINUS)
      op1 = replace_equiv_address (operands[1], force_reg (SImode, addr));

    operands[4] = adjust_address (op1, QImode, 1);
    operands[1] = adjust_address (operands[1], QImode, 0);
    operands[3] = gen_lowpart (QImode, operands[0]);
    operands[0] = gen_lowpart (SImode, operands[0]);
    operands[2] = gen_reg_rtx (SImode);
    operands[5] = gen_lowpart (QImode, operands[2]);
  }"
)

(define_expand "storehi_bigend"
  [(set (match_dup 4) (match_dup 3))
   (set (match_dup 2)
	(ashiftrt:SI (match_operand 0 "" "") (const_int 8)))
   (set (match_operand 1 "" "")	(match_dup 5))]
  "TARGET_ARM"
  "
  {
    rtx op1 = operands[1];
    rtx addr = XEXP (op1, 0);
    enum rtx_code code = GET_CODE (addr);

    if ((code == PLUS && !CONST_INT_P (XEXP (addr, 1)))
	|| code == MINUS)
      op1 = replace_equiv_address (op1, force_reg (SImode, addr));

    operands[4] = adjust_address (op1, QImode, 1);
    operands[1] = adjust_address (operands[1], QImode, 0);
    operands[3] = gen_lowpart (QImode, operands[0]);
    operands[0] = gen_lowpart (SImode, operands[0]);
    operands[2] = gen_reg_rtx (SImode);
    operands[5] = gen_lowpart (QImode, operands[2]);
  }"
)

;; Subroutine to store a half word integer constant into memory.
(define_expand "storeinthi"
  [(set (match_operand 0 "" "")
	(match_operand 1 "" ""))
   (set (match_dup 3) (match_dup 2))]
  "TARGET_ARM"
  "
  {
    HOST_WIDE_INT value = INTVAL (operands[1]);
    rtx addr = XEXP (operands[0], 0);
    rtx op0 = operands[0];
    enum rtx_code code = GET_CODE (addr);

    if ((code == PLUS && !CONST_INT_P (XEXP (addr, 1)))
	|| code == MINUS)
      op0 = replace_equiv_address (op0, force_reg (SImode, addr));

    operands[1] = gen_reg_rtx (SImode);
    if (BYTES_BIG_ENDIAN)
      {
	emit_insn (gen_movsi (operands[1], GEN_INT ((value >> 8) & 255)));
	if ((value & 255) == ((value >> 8) & 255))
	  operands[2] = operands[1];
	else
	  {
	    operands[2] = gen_reg_rtx (SImode);
	    emit_insn (gen_movsi (operands[2], GEN_INT (value & 255)));
	  }
      }
    else
      {
	emit_insn (gen_movsi (operands[1], GEN_INT (value & 255)));
	if ((value & 255) == ((value >> 8) & 255))
	  operands[2] = operands[1];
	else
	  {
	    operands[2] = gen_reg_rtx (SImode);
	    emit_insn (gen_movsi (operands[2], GEN_INT ((value >> 8) & 255)));
	  }
      }

    operands[3] = adjust_address (op0, QImode, 1);
    operands[0] = adjust_address (operands[0], QImode, 0);
    operands[2] = gen_lowpart (QImode, operands[2]);
    operands[1] = gen_lowpart (QImode, operands[1]);
  }"
)

(define_expand "storehi_single_op"
  [(set (match_operand:HI 0 "memory_operand" "")
	(match_operand:HI 1 "general_operand" ""))]
  "TARGET_32BIT && arm_arch4"
  "
  if (!s_register_operand (operands[1], HImode))
    operands[1] = copy_to_mode_reg (HImode, operands[1]);
  "
)

(define_expand "movhi"
  [(set (match_operand:HI 0 "general_operand" "")
	(match_operand:HI 1 "general_operand" ""))]
  "TARGET_EITHER"
  "
  if (TARGET_ARM)
    {
      if (can_create_pseudo_p ())
        {
          if (MEM_P (operands[0]))
	    {
	      if (arm_arch4)
	        {
	          emit_insn (gen_storehi_single_op (operands[0], operands[1]));
	          DONE;
	        }
	      if (CONST_INT_P (operands[1]))
	        emit_insn (gen_storeinthi (operands[0], operands[1]));
	      else
	        {
	          if (MEM_P (operands[1]))
		    operands[1] = force_reg (HImode, operands[1]);
	          if (BYTES_BIG_ENDIAN)
		    emit_insn (gen_storehi_bigend (operands[1], operands[0]));
	          else
		   emit_insn (gen_storehi (operands[1], operands[0]));
	        }
	      DONE;
	    }
          /* Sign extend a constant, and keep it in an SImode reg.  */
          else if (CONST_INT_P (operands[1]))
	    {
	      rtx reg = gen_reg_rtx (SImode);
	      HOST_WIDE_INT val = INTVAL (operands[1]) & 0xffff;

	      /* If the constant is already valid, leave it alone.  */
	      if (!const_ok_for_arm (val))
	        {
	          /* If setting all the top bits will make the constant 
		     loadable in a single instruction, then set them.  
		     Otherwise, sign extend the number.  */

	          if (const_ok_for_arm (~(val | ~0xffff)))
		    val |= ~0xffff;
	          else if (val & 0x8000)
		    val |= ~0xffff;
	        }

	      emit_insn (gen_movsi (reg, GEN_INT (val)));
	      operands[1] = gen_lowpart (HImode, reg);
	    }
	  else if (arm_arch4 && optimize && can_create_pseudo_p ()
		   && MEM_P (operands[1]))
	    {
	      rtx reg = gen_reg_rtx (SImode);

	      emit_insn (gen_zero_extendhisi2 (reg, operands[1]));
	      operands[1] = gen_lowpart (HImode, reg);
	    }
          else if (!arm_arch4)
	    {
	      if (MEM_P (operands[1]))
	        {
		  rtx base;
		  rtx offset = const0_rtx;
		  rtx reg = gen_reg_rtx (SImode);

		  if ((REG_P (base = XEXP (operands[1], 0))
		       || (GET_CODE (base) == PLUS
			   && (CONST_INT_P (offset = XEXP (base, 1)))
                           && ((INTVAL(offset) & 1) != 1)
			   && REG_P (base = XEXP (base, 0))))
		      && REGNO_POINTER_ALIGN (REGNO (base)) >= 32)
		    {
		      rtx new_rtx;

		      new_rtx = widen_memory_access (operands[1], SImode,
						     ((INTVAL (offset) & ~3)
						      - INTVAL (offset)));
		      emit_insn (gen_movsi (reg, new_rtx));
		      if (((INTVAL (offset) & 2) != 0)
			  ^ (BYTES_BIG_ENDIAN ? 1 : 0))
			{
			  rtx reg2 = gen_reg_rtx (SImode);

			  emit_insn (gen_lshrsi3 (reg2, reg, GEN_INT (16)));
			  reg = reg2;
			}
		    }
		  else
		    emit_insn (gen_movhi_bytes (reg, operands[1]));

		  operands[1] = gen_lowpart (HImode, reg);
	       }
	   }
        }
      /* Handle loading a large integer during reload.  */
      else if (CONST_INT_P (operands[1])
	       && !const_ok_for_arm (INTVAL (operands[1]))
	       && !const_ok_for_arm (~INTVAL (operands[1])))
        {
          /* Writing a constant to memory needs a scratch, which should
	     be handled with SECONDARY_RELOADs.  */
          gcc_assert (REG_P (operands[0]));

          operands[0] = gen_rtx_SUBREG (SImode, operands[0], 0);
          emit_insn (gen_movsi (operands[0], operands[1]));
          DONE;
       }
    }
  else if (TARGET_THUMB2)
    {
      /* Thumb-2 can do everything except mem=mem and mem=const easily.  */
      if (can_create_pseudo_p ())
	{
	  if (!REG_P (operands[0]))
	    operands[1] = force_reg (HImode, operands[1]);
          /* Zero extend a constant, and keep it in an SImode reg.  */
          else if (CONST_INT_P (operands[1]))
	    {
	      rtx reg = gen_reg_rtx (SImode);
	      HOST_WIDE_INT val = INTVAL (operands[1]) & 0xffff;

	      emit_insn (gen_movsi (reg, GEN_INT (val)));
	      operands[1] = gen_lowpart (HImode, reg);
	    }
	}
    }
  else /* TARGET_THUMB1 */
    {
      if (can_create_pseudo_p ())
        {
	  if (CONST_INT_P (operands[1]))
	    {
	      rtx reg = gen_reg_rtx (SImode);

	      emit_insn (gen_movsi (reg, operands[1]));
	      operands[1] = gen_lowpart (HImode, reg);
	    }

          /* ??? We shouldn't really get invalid addresses here, but this can
	     happen if we are passed a SP (never OK for HImode/QImode) or 
	     virtual register (also rejected as illegitimate for HImode/QImode)
	     relative address.  */
          /* ??? This should perhaps be fixed elsewhere, for instance, in
	     fixup_stack_1, by checking for other kinds of invalid addresses,
	     e.g. a bare reference to a virtual register.  This may confuse the
	     alpha though, which must handle this case differently.  */
          if (MEM_P (operands[0])
	      && !memory_address_p (GET_MODE (operands[0]),
				    XEXP (operands[0], 0)))
	    operands[0]
	      = replace_equiv_address (operands[0],
				       copy_to_reg (XEXP (operands[0], 0)));
   
          if (MEM_P (operands[1])
	      && !memory_address_p (GET_MODE (operands[1]),
				    XEXP (operands[1], 0)))
	    operands[1]
	      = replace_equiv_address (operands[1],
				       copy_to_reg (XEXP (operands[1], 0)));

	  if (MEM_P (operands[1]) && optimize > 0)
	    {
	      rtx reg = gen_reg_rtx (SImode);

	      emit_insn (gen_zero_extendhisi2 (reg, operands[1]));
	      operands[1] = gen_lowpart (HImode, reg);
	    }

          if (MEM_P (operands[0]))
	    operands[1] = force_reg (HImode, operands[1]);
        }
      else if (CONST_INT_P (operands[1])
	        && !satisfies_constraint_I (operands[1]))
        {
	  /* Handle loading a large integer during reload.  */

          /* Writing a constant to memory needs a scratch, which should
	     be handled with SECONDARY_RELOADs.  */
          gcc_assert (REG_P (operands[0]));

          operands[0] = gen_rtx_SUBREG (SImode, operands[0], 0);
          emit_insn (gen_movsi (operands[0], operands[1]));
          DONE;
        }
    }
  "
)

(define_expand "movhi_bytes"
  [(set (match_dup 2) (zero_extend:SI (match_operand:HI 1 "" "")))
   (set (match_dup 3)
	(zero_extend:SI (match_dup 6)))
   (set (match_operand:SI 0 "" "")
	 (ior:SI (ashift:SI (match_dup 4) (const_int 8)) (match_dup 5)))]
  "TARGET_ARM"
  "
  {
    rtx mem1, mem2;
    rtx addr = copy_to_mode_reg (SImode, XEXP (operands[1], 0));

    mem1 = change_address (operands[1], QImode, addr);
    mem2 = change_address (operands[1], QImode,
			   plus_constant (Pmode, addr, 1));
    operands[0] = gen_lowpart (SImode, operands[0]);
    operands[1] = mem1;
    operands[2] = gen_reg_rtx (SImode);
    operands[3] = gen_reg_rtx (SImode);
    operands[6] = mem2;

    if (BYTES_BIG_ENDIAN)
      {
	operands[4] = operands[2];
	operands[5] = operands[3];
      }
    else
      {
	operands[4] = operands[3];
	operands[5] = operands[2];
      }
  }"
)

(define_expand "movhi_bigend"
  [(set (match_dup 2)
	(rotate:SI (subreg:SI (match_operand:HI 1 "memory_operand" "") 0)
		   (const_int 16)))
   (set (match_dup 3)
	(ashiftrt:SI (match_dup 2) (const_int 16)))
   (set (match_operand:HI 0 "s_register_operand" "")
	(match_dup 4))]
  "TARGET_ARM"
  "
  operands[2] = gen_reg_rtx (SImode);
  operands[3] = gen_reg_rtx (SImode);
  operands[4] = gen_lowpart (HImode, operands[3]);
  "
)

;; Pattern to recognize insn generated default case above
(define_insn "*movhi_insn_arch4"
  [(set (match_operand:HI 0 "nonimmediate_operand" "=r,r,r,m,r")
	(match_operand:HI 1 "general_operand"      "rIk,K,n,r,mi"))]
  "TARGET_ARM
   && arm_arch4
   && (register_operand (operands[0], HImode)
       || register_operand (operands[1], HImode))"
  "@
   mov%?\\t%0, %1\\t%@ movhi
   mvn%?\\t%0, #%B1\\t%@ movhi
   movw%?\\t%0, %L1\\t%@ movhi
   strh%?\\t%1, %0\\t%@ movhi
   ldrh%?\\t%0, %1\\t%@ movhi"
  [(set_attr "predicable" "yes")
   (set_attr "pool_range" "*,*,*,*,256")
   (set_attr "neg_pool_range" "*,*,*,*,244")
   (set_attr "arch" "*,*,v6t2,*,*")
   (set_attr_alternative "type"
                         [(if_then_else (match_operand 1 "const_int_operand" "")
                                        (const_string "mov_imm" )
                                        (const_string "mov_reg"))
                          (const_string "mvn_imm")
                          (const_string "mov_imm")
                          (const_string "store1")
                          (const_string "load1")])]
)

(define_insn "*movhi_bytes"
  [(set (match_operand:HI 0 "s_register_operand" "=r,r,r")
	(match_operand:HI 1 "arm_rhs_operand"  "I,rk,K"))]
  "TARGET_ARM"
  "@
   mov%?\\t%0, %1\\t%@ movhi
   mov%?\\t%0, %1\\t%@ movhi
   mvn%?\\t%0, #%B1\\t%@ movhi"
  [(set_attr "predicable" "yes")
   (set_attr "type" "mov_imm,mov_reg,mvn_imm")]
)
	
;; We use a DImode scratch because we may occasionally need an additional
;; temporary if the address isn't offsettable -- push_reload doesn't seem
;; to take any notice of the "o" constraints on reload_memory_operand operand.
(define_expand "reload_outhi"
  [(parallel [(match_operand:HI 0 "arm_reload_memory_operand" "=o")
	      (match_operand:HI 1 "s_register_operand"        "r")
	      (match_operand:DI 2 "s_register_operand"        "=&l")])]
  "TARGET_EITHER"
  "if (TARGET_ARM)
     arm_reload_out_hi (operands);
   else
     thumb_reload_out_hi (operands);
  DONE;
  "
)

(define_expand "reload_inhi"
  [(parallel [(match_operand:HI 0 "s_register_operand" "=r")
	      (match_operand:HI 1 "arm_reload_memory_operand" "o")
	      (match_operand:DI 2 "s_register_operand" "=&r")])]
  "TARGET_EITHER"
  "
  if (TARGET_ARM)
    arm_reload_in_hi (operands);
  else
    thumb_reload_out_hi (operands);
  DONE;
")

(define_expand "movqi"
  [(set (match_operand:QI 0 "general_operand" "")
        (match_operand:QI 1 "general_operand" ""))]
  "TARGET_EITHER"
  "
  /* Everything except mem = const or mem = mem can be done easily */

  if (can_create_pseudo_p ())
    {
      if (CONST_INT_P (operands[1]))
	{
	  rtx reg = gen_reg_rtx (SImode);

	  /* For thumb we want an unsigned immediate, then we are more likely 
	     to be able to use a movs insn.  */
	  if (TARGET_THUMB)
	    operands[1] = GEN_INT (INTVAL (operands[1]) & 255);

	  emit_insn (gen_movsi (reg, operands[1]));
	  operands[1] = gen_lowpart (QImode, reg);
	}

      if (TARGET_THUMB)
	{
          /* ??? We shouldn't really get invalid addresses here, but this can
	     happen if we are passed a SP (never OK for HImode/QImode) or
	     virtual register (also rejected as illegitimate for HImode/QImode)
	     relative address.  */
          /* ??? This should perhaps be fixed elsewhere, for instance, in
	     fixup_stack_1, by checking for other kinds of invalid addresses,
	     e.g. a bare reference to a virtual register.  This may confuse the
	     alpha though, which must handle this case differently.  */
          if (MEM_P (operands[0])
	      && !memory_address_p (GET_MODE (operands[0]),
		  		     XEXP (operands[0], 0)))
	    operands[0]
	      = replace_equiv_address (operands[0],
				       copy_to_reg (XEXP (operands[0], 0)));
          if (MEM_P (operands[1])
	      && !memory_address_p (GET_MODE (operands[1]),
				    XEXP (operands[1], 0)))
	     operands[1]
	       = replace_equiv_address (operands[1],
					copy_to_reg (XEXP (operands[1], 0)));
	}

      if (MEM_P (operands[1]) && optimize > 0)
	{
	  rtx reg = gen_reg_rtx (SImode);

	  emit_insn (gen_zero_extendqisi2 (reg, operands[1]));
	  operands[1] = gen_lowpart (QImode, reg);
	}

      if (MEM_P (operands[0]))
	operands[1] = force_reg (QImode, operands[1]);
    }
  else if (TARGET_THUMB
	   && CONST_INT_P (operands[1])
	   && !satisfies_constraint_I (operands[1]))
    {
      /* Handle loading a large integer during reload.  */

      /* Writing a constant to memory needs a scratch, which should
	 be handled with SECONDARY_RELOADs.  */
      gcc_assert (REG_P (operands[0]));

      operands[0] = gen_rtx_SUBREG (SImode, operands[0], 0);
      emit_insn (gen_movsi (operands[0], operands[1]));
      DONE;
    }
  "
)

(define_insn "*arm_movqi_insn"
  [(set (match_operand:QI 0 "nonimmediate_operand" "=r,r,r,l,r,l,Uu,r,m")
	(match_operand:QI 1 "general_operand" "rk,rk,I,Py,K,Uu,l,Uh,r"))]
  "TARGET_32BIT
   && (   register_operand (operands[0], QImode)
       || register_operand (operands[1], QImode))"
  "@
   mov%?\\t%0, %1
   mov%?\\t%0, %1
   mov%?\\t%0, %1
   mov%?\\t%0, %1
   mvn%?\\t%0, #%B1
   ldrb%?\\t%0, %1
   strb%?\\t%1, %0
   ldrb%?\\t%0, %1
   strb%?\\t%1, %0"
  [(set_attr "type" "mov_reg,mov_reg,mov_imm,mov_imm,mvn_imm,load1,store1,load1,store1")
   (set_attr "predicable" "yes")
   (set_attr "predicable_short_it" "yes,yes,no,yes,no,no,no,no,no")
   (set_attr "arch" "t2,any,any,t2,any,t2,t2,any,any")
   (set_attr "length" "2,4,4,2,4,2,2,4,4")]
)

;; HFmode moves
(define_expand "movhf"
  [(set (match_operand:HF 0 "general_operand" "")
	(match_operand:HF 1 "general_operand" ""))]
  "TARGET_EITHER"
  "
  if (TARGET_32BIT)
    {
      if (MEM_P (operands[0]))
        operands[1] = force_reg (HFmode, operands[1]);
    }
  else /* TARGET_THUMB1 */
    {
      if (can_create_pseudo_p ())
        {
           if (!REG_P (operands[0]))
	     operands[1] = force_reg (HFmode, operands[1]);
        }
    }
  "
)

(define_insn "*arm32_movhf"
  [(set (match_operand:HF 0 "nonimmediate_operand" "=r,m,r,r")
	(match_operand:HF 1 "general_operand"	   " m,r,r,F"))]
  "TARGET_32BIT && !(TARGET_HARD_FLOAT && TARGET_VFP)
   && (	  s_register_operand (operands[0], HFmode)
       || s_register_operand (operands[1], HFmode))"
  "*
  switch (which_alternative)
    {
    case 0:	/* ARM register from memory */
      return \"ldrh%?\\t%0, %1\\t%@ __fp16\";
    case 1:	/* memory from ARM register */
      return \"strh%?\\t%1, %0\\t%@ __fp16\";
    case 2:	/* ARM register from ARM register */
      return \"mov%?\\t%0, %1\\t%@ __fp16\";
    case 3:	/* ARM register from constant */
      {
	long bits;
	rtx ops[4];

	bits = real_to_target (NULL, CONST_DOUBLE_REAL_VALUE (operands[1]),
			       HFmode);
	ops[0] = operands[0];
	ops[1] = GEN_INT (bits);
	ops[2] = GEN_INT (bits & 0xff00);
	ops[3] = GEN_INT (bits & 0x00ff);

	if (arm_arch_thumb2)
	  output_asm_insn (\"movw%?\\t%0, %1\", ops);
	else
	  output_asm_insn (\"mov%?\\t%0, %2\;orr%?\\t%0, %0, %3\", ops);
	return \"\";
       }
    default:
      gcc_unreachable ();
    }
  "
  [(set_attr "conds" "unconditional")
   (set_attr "type" "load1,store1,mov_reg,multiple")
   (set_attr "length" "4,4,4,8")
   (set_attr "predicable" "yes")
   (set_attr "predicable_short_it" "no")]
)

(define_expand "movsf"
  [(set (match_operand:SF 0 "general_operand" "")
	(match_operand:SF 1 "general_operand" ""))]
  "TARGET_EITHER"
  "
  if (TARGET_32BIT)
    {
      if (MEM_P (operands[0]))
        operands[1] = force_reg (SFmode, operands[1]);
    }
  else /* TARGET_THUMB1 */
    {
      if (can_create_pseudo_p ())
        {
           if (!REG_P (operands[0]))
	     operands[1] = force_reg (SFmode, operands[1]);
        }
    }
  "
)

;; Transform a floating-point move of a constant into a core register into
;; an SImode operation.
(define_split
  [(set (match_operand:SF 0 "arm_general_register_operand" "")
	(match_operand:SF 1 "immediate_operand" ""))]
  "TARGET_EITHER
   && reload_completed
   && CONST_DOUBLE_P (operands[1])"
  [(set (match_dup 2) (match_dup 3))]
  "
  operands[2] = gen_lowpart (SImode, operands[0]);
  operands[3] = gen_lowpart (SImode, operands[1]);
  if (operands[2] == 0 || operands[3] == 0)
    FAIL;
  "
)

(define_insn "*arm_movsf_soft_insn"
  [(set (match_operand:SF 0 "nonimmediate_operand" "=r,r,m")
	(match_operand:SF 1 "general_operand"  "r,mE,r"))]
  "TARGET_32BIT
   && TARGET_SOFT_FLOAT
   && (!MEM_P (operands[0])
       || register_operand (operands[1], SFmode))"
  "@
   mov%?\\t%0, %1
   ldr%?\\t%0, %1\\t%@ float
   str%?\\t%1, %0\\t%@ float"
  [(set_attr "predicable" "yes")
   (set_attr "predicable_short_it" "no")
   (set_attr "type" "mov_reg,load1,store1")
   (set_attr "arm_pool_range" "*,4096,*")
   (set_attr "thumb2_pool_range" "*,4094,*")
   (set_attr "arm_neg_pool_range" "*,4084,*")
   (set_attr "thumb2_neg_pool_range" "*,0,*")]
)

(define_expand "movdf"
  [(set (match_operand:DF 0 "general_operand" "")
	(match_operand:DF 1 "general_operand" ""))]
  "TARGET_EITHER"
  "
  if (TARGET_32BIT)
    {
      if (MEM_P (operands[0]))
        operands[1] = force_reg (DFmode, operands[1]);
    }
  else /* TARGET_THUMB */
    {
      if (can_create_pseudo_p ())
        {
          if (!REG_P (operands[0]))
	    operands[1] = force_reg (DFmode, operands[1]);
        }
    }
  "
)

;; Reloading a df mode value stored in integer regs to memory can require a
;; scratch reg.
(define_expand "reload_outdf"
  [(match_operand:DF 0 "arm_reload_memory_operand" "=o")
   (match_operand:DF 1 "s_register_operand" "r")
   (match_operand:SI 2 "s_register_operand" "=&r")]
  "TARGET_THUMB2"
  "
  {
    enum rtx_code code = GET_CODE (XEXP (operands[0], 0));

    if (code == REG)
      operands[2] = XEXP (operands[0], 0);
    else if (code == POST_INC || code == PRE_DEC)
      {
	operands[0] = gen_rtx_SUBREG (DImode, operands[0], 0);
	operands[1] = gen_rtx_SUBREG (DImode, operands[1], 0);
	emit_insn (gen_movdi (operands[0], operands[1]));
	DONE;
      }
    else if (code == PRE_INC)
      {
	rtx reg = XEXP (XEXP (operands[0], 0), 0);

	emit_insn (gen_addsi3 (reg, reg, GEN_INT (8)));
	operands[2] = reg;
      }
    else if (code == POST_DEC)
      operands[2] = XEXP (XEXP (operands[0], 0), 0);
    else
      emit_insn (gen_addsi3 (operands[2], XEXP (XEXP (operands[0], 0), 0),
			     XEXP (XEXP (operands[0], 0), 1)));

    emit_insn (gen_rtx_SET (replace_equiv_address (operands[0], operands[2]),
			    operands[1]));

    if (code == POST_DEC)
      emit_insn (gen_addsi3 (operands[2], operands[2], GEN_INT (-8)));

    DONE;
  }"
)

(define_insn "*movdf_soft_insn"
  [(set (match_operand:DF 0 "nonimmediate_soft_df_operand" "=r,r,r,q,m")
	(match_operand:DF 1 "soft_df_operand" "rDa,Db,Dc,mF,q"))]
  "TARGET_32BIT && TARGET_SOFT_FLOAT
   && (   register_operand (operands[0], DFmode)
       || register_operand (operands[1], DFmode))"
  "*
  switch (which_alternative)
    {
    case 0:
    case 1:
    case 2:
      return \"#\";
    default:
      return output_move_double (operands, true, NULL);
    }
  "
  [(set_attr "length" "8,12,16,8,8")
   (set_attr "type" "multiple,multiple,multiple,load2,store2")
   (set_attr "arm_pool_range" "*,*,*,1020,*")
   (set_attr "thumb2_pool_range" "*,*,*,1018,*")
   (set_attr "arm_neg_pool_range" "*,*,*,1004,*")
   (set_attr "thumb2_neg_pool_range" "*,*,*,0,*")]
)


;; load- and store-multiple insns
;; The arm can load/store any set of registers, provided that they are in
;; ascending order, but these expanders assume a contiguous set.

(define_expand "load_multiple"
  [(match_par_dup 3 [(set (match_operand:SI 0 "" "")
                          (match_operand:SI 1 "" ""))
                     (use (match_operand:SI 2 "" ""))])]
  "TARGET_32BIT"
{
  HOST_WIDE_INT offset = 0;

  /* Support only fixed point registers.  */
  if (!CONST_INT_P (operands[2])
      || INTVAL (operands[2]) > MAX_LDM_STM_OPS
      || INTVAL (operands[2]) < 2
      || !MEM_P (operands[1])
      || !REG_P (operands[0])
      || REGNO (operands[0]) > (LAST_ARM_REGNUM - 1)
      || REGNO (operands[0]) + INTVAL (operands[2]) > LAST_ARM_REGNUM)
    FAIL;

  operands[3]
    = arm_gen_load_multiple (arm_regs_in_sequence + REGNO (operands[0]),
			     INTVAL (operands[2]),
			     force_reg (SImode, XEXP (operands[1], 0)),
			     FALSE, operands[1], &offset);
})

(define_expand "store_multiple"
  [(match_par_dup 3 [(set (match_operand:SI 0 "" "")
                          (match_operand:SI 1 "" ""))
                     (use (match_operand:SI 2 "" ""))])]
  "TARGET_32BIT"
{
  HOST_WIDE_INT offset = 0;

  /* Support only fixed point registers.  */
  if (!CONST_INT_P (operands[2])
      || INTVAL (operands[2]) > MAX_LDM_STM_OPS
      || INTVAL (operands[2]) < 2
      || !REG_P (operands[1])
      || !MEM_P (operands[0])
      || REGNO (operands[1]) > (LAST_ARM_REGNUM - 1)
      || REGNO (operands[1]) + INTVAL (operands[2]) > LAST_ARM_REGNUM)
    FAIL;

  operands[3]
    = arm_gen_store_multiple (arm_regs_in_sequence + REGNO (operands[1]),
			      INTVAL (operands[2]),
			      force_reg (SImode, XEXP (operands[0], 0)),
			      FALSE, operands[0], &offset);
})


(define_expand "setmemsi"
  [(match_operand:BLK 0 "general_operand" "")
   (match_operand:SI 1 "const_int_operand" "")
   (match_operand:SI 2 "const_int_operand" "")
   (match_operand:SI 3 "const_int_operand" "")]
  "TARGET_32BIT"
{
  if (arm_gen_setmem (operands))
    DONE;

  FAIL;
})


;; Move a block of memory if it is word aligned and MORE than 2 words long.
;; We could let this apply for blocks of less than this, but it clobbers so
;; many registers that there is then probably a better way.

(define_expand "movmemqi"
  [(match_operand:BLK 0 "general_operand" "")
   (match_operand:BLK 1 "general_operand" "")
   (match_operand:SI 2 "const_int_operand" "")
   (match_operand:SI 3 "const_int_operand" "")]
  ""
  "
  if (TARGET_32BIT)
    {
      if (TARGET_LDRD && current_tune->prefer_ldrd_strd
          && !optimize_function_for_size_p (cfun))
        {
          if (gen_movmem_ldrd_strd (operands))
            DONE;
          FAIL;
        }

      if (arm_gen_movmemqi (operands))
        DONE;
      FAIL;
    }
  else /* TARGET_THUMB1 */
    {
      if (   INTVAL (operands[3]) != 4
          || INTVAL (operands[2]) > 48)
        FAIL;

      thumb_expand_movmemqi (operands);
      DONE;
    }
  "
)


;; Compare & branch insns
;; The range calculations are based as follows:
;; For forward branches, the address calculation returns the address of
;; the next instruction.  This is 2 beyond the branch instruction.
;; For backward branches, the address calculation returns the address of
;; the first instruction in this pattern (cmp).  This is 2 before the branch
;; instruction for the shortest sequence, and 4 before the branch instruction
;; if we have to jump around an unconditional branch.
;; To the basic branch range the PC offset must be added (this is +4).
;; So for forward branches we have 
;;   (pos_range - pos_base_offs + pc_offs) = (pos_range - 2 + 4).
;; And for backward branches we have 
;;   (neg_range - neg_base_offs + pc_offs) = (neg_range - (-2 or -4) + 4).
;;
;; For a 'b'       pos_range = 2046, neg_range = -2048 giving (-2040->2048).
;; For a 'b<cond>' pos_range = 254,  neg_range = -256  giving (-250 ->256).

(define_expand "cbranchsi4"
  [(set (pc) (if_then_else
	      (match_operator 0 "expandable_comparison_operator"
	       [(match_operand:SI 1 "s_register_operand" "")
	        (match_operand:SI 2 "nonmemory_operand" "")])
	      (label_ref (match_operand 3 "" ""))
	      (pc)))]
  "TARGET_EITHER"
  "
  if (!TARGET_THUMB1)
    {
      if (!arm_validize_comparison (&operands[0], &operands[1], &operands[2]))
        FAIL;
      emit_jump_insn (gen_cbranch_cc (operands[0], operands[1], operands[2],
				      operands[3]));
      DONE;
    }
  if (thumb1_cmpneg_operand (operands[2], SImode))
    {
      emit_jump_insn (gen_cbranchsi4_scratch (NULL, operands[1], operands[2],
					      operands[3], operands[0]));
      DONE;
    }
  if (!thumb1_cmp_operand (operands[2], SImode))
    operands[2] = force_reg (SImode, operands[2]);
  ")

(define_expand "cbranchsf4"
  [(set (pc) (if_then_else
	      (match_operator 0 "expandable_comparison_operator"
	       [(match_operand:SF 1 "s_register_operand" "")
	        (match_operand:SF 2 "arm_float_compare_operand" "")])
	      (label_ref (match_operand 3 "" ""))
	      (pc)))]
  "TARGET_32BIT && TARGET_HARD_FLOAT"
  "emit_jump_insn (gen_cbranch_cc (operands[0], operands[1], operands[2],
				   operands[3])); DONE;"
)

(define_expand "cbranchdf4"
  [(set (pc) (if_then_else
	      (match_operator 0 "expandable_comparison_operator"
	       [(match_operand:DF 1 "s_register_operand" "")
	        (match_operand:DF 2 "arm_float_compare_operand" "")])
	      (label_ref (match_operand 3 "" ""))
	      (pc)))]
  "TARGET_32BIT && TARGET_HARD_FLOAT && !TARGET_VFP_SINGLE"
  "emit_jump_insn (gen_cbranch_cc (operands[0], operands[1], operands[2],
				   operands[3])); DONE;"
)

(define_expand "cbranchdi4"
  [(set (pc) (if_then_else
	      (match_operator 0 "expandable_comparison_operator"
	       [(match_operand:DI 1 "s_register_operand" "")
	        (match_operand:DI 2 "cmpdi_operand" "")])
	      (label_ref (match_operand 3 "" ""))
	      (pc)))]
  "TARGET_32BIT"
  "{
     if (!arm_validize_comparison (&operands[0], &operands[1], &operands[2]))
       FAIL;
     emit_jump_insn (gen_cbranch_cc (operands[0], operands[1], operands[2],
				       operands[3]));
     DONE;
   }"
)

;; Comparison and test insns

(define_insn "*arm_cmpsi_insn"
  [(set (reg:CC CC_REGNUM)
	(compare:CC (match_operand:SI 0 "s_register_operand" "l,r,r,r,r")
		    (match_operand:SI 1 "arm_add_operand"    "Py,r,r,I,L")))]
  "TARGET_32BIT"
  "@
   cmp%?\\t%0, %1
   cmp%?\\t%0, %1
   cmp%?\\t%0, %1
   cmp%?\\t%0, %1
   cmn%?\\t%0, #%n1"
  [(set_attr "conds" "set")
   (set_attr "arch" "t2,t2,any,any,any")
   (set_attr "length" "2,2,4,4,4")
   (set_attr "predicable" "yes")
   (set_attr "predicable_short_it" "yes,yes,yes,no,no")
   (set_attr "type" "alus_imm,alus_sreg,alus_sreg,alus_imm,alus_imm")]
)

(define_insn "*cmpsi_shiftsi"
  [(set (reg:CC CC_REGNUM)
	(compare:CC (match_operand:SI   0 "s_register_operand" "r,r,r")
		    (match_operator:SI  3 "shift_operator"
		     [(match_operand:SI 1 "s_register_operand" "r,r,r")
		      (match_operand:SI 2 "shift_amount_operand" "M,r,M")])))]
  "TARGET_32BIT"
  "cmp\\t%0, %1%S3"
  [(set_attr "conds" "set")
   (set_attr "shift" "1")
   (set_attr "arch" "32,a,a")
   (set_attr "type" "alus_shift_imm,alus_shift_reg,alus_shift_imm")])

(define_insn "*cmpsi_shiftsi_swp"
  [(set (reg:CC_SWP CC_REGNUM)
	(compare:CC_SWP (match_operator:SI 3 "shift_operator"
			 [(match_operand:SI 1 "s_register_operand" "r,r,r")
			  (match_operand:SI 2 "shift_amount_operand" "M,r,M")])
			(match_operand:SI 0 "s_register_operand" "r,r,r")))]
  "TARGET_32BIT"
  "cmp%?\\t%0, %1%S3"
  [(set_attr "conds" "set")
   (set_attr "shift" "1")
   (set_attr "arch" "32,a,a")
   (set_attr "type" "alus_shift_imm,alus_shift_reg,alus_shift_imm")])

(define_insn "*arm_cmpsi_negshiftsi_si"
  [(set (reg:CC_Z CC_REGNUM)
	(compare:CC_Z
	 (neg:SI (match_operator:SI 1 "shift_operator"
		    [(match_operand:SI 2 "s_register_operand" "r")
		     (match_operand:SI 3 "reg_or_int_operand" "rM")]))
	 (match_operand:SI 0 "s_register_operand" "r")))]
  "TARGET_ARM"
  "cmn%?\\t%0, %2%S1"
  [(set_attr "conds" "set")
   (set (attr "type") (if_then_else (match_operand 3 "const_int_operand" "")
				    (const_string "alus_shift_imm")
				    (const_string "alus_shift_reg")))
   (set_attr "predicable" "yes")]
)

;; DImode comparisons.  The generic code generates branches that
;; if-conversion can not reduce to a conditional compare, so we do
;; that directly.

(define_insn_and_split "*arm_cmpdi_insn"
  [(set (reg:CC_NCV CC_REGNUM)
	(compare:CC_NCV (match_operand:DI 0 "s_register_operand" "r")
			(match_operand:DI 1 "arm_di_operand"	   "rDi")))
   (clobber (match_scratch:SI 2 "=r"))]
  "TARGET_32BIT"
  "#"   ; "cmp\\t%Q0, %Q1\;sbcs\\t%2, %R0, %R1"
  "&& reload_completed"
  [(set (reg:CC CC_REGNUM)
        (compare:CC (match_dup 0) (match_dup 1)))
   (parallel [(set (reg:CC CC_REGNUM)
                   (compare:CC (match_dup 3) (match_dup 4)))
              (set (match_dup 2)
                   (minus:SI (match_dup 5)
                            (ltu:SI (reg:CC_C CC_REGNUM) (const_int 0))))])]
  {
    operands[3] = gen_highpart (SImode, operands[0]);
    operands[0] = gen_lowpart (SImode, operands[0]);
    if (CONST_INT_P (operands[1]))
      {
        operands[4] = GEN_INT (~INTVAL (gen_highpart_mode (SImode,
                                                           DImode,
                                                           operands[1])));
        operands[5] = gen_rtx_PLUS (SImode, operands[3], operands[4]);
      }
    else
      {
        operands[4] = gen_highpart (SImode, operands[1]);
        operands[5] = gen_rtx_MINUS (SImode, operands[3], operands[4]);
      }
    operands[1] = gen_lowpart (SImode, operands[1]);
    operands[2] = gen_lowpart (SImode, operands[2]);
  }
  [(set_attr "conds" "set")
   (set_attr "length" "8")
   (set_attr "type" "multiple")]
)

(define_insn_and_split "*arm_cmpdi_unsigned"
  [(set (reg:CC_CZ CC_REGNUM)
        (compare:CC_CZ (match_operand:DI 0 "s_register_operand" "l,r,r,r")
                       (match_operand:DI 1 "arm_di_operand"     "Py,r,Di,rDi")))]

  "TARGET_32BIT"
  "#"   ; "cmp\\t%R0, %R1\;it eq\;cmpeq\\t%Q0, %Q1"
  "&& reload_completed"
  [(set (reg:CC CC_REGNUM)
        (compare:CC (match_dup 2) (match_dup 3)))
   (cond_exec (eq:SI (reg:CC CC_REGNUM) (const_int 0))
              (set (reg:CC CC_REGNUM)
                   (compare:CC (match_dup 0) (match_dup 1))))]
  {
    operands[2] = gen_highpart (SImode, operands[0]);
    operands[0] = gen_lowpart (SImode, operands[0]);
    if (CONST_INT_P (operands[1]))
      operands[3] = gen_highpart_mode (SImode, DImode, operands[1]);
    else
      operands[3] = gen_highpart (SImode, operands[1]);
    operands[1] = gen_lowpart (SImode, operands[1]);
  }
  [(set_attr "conds" "set")
   (set_attr "enabled_for_depr_it" "yes,yes,no,*")
   (set_attr "arch" "t2,t2,t2,a")
   (set_attr "length" "6,6,10,8")
   (set_attr "type" "multiple")]
)

(define_insn "*arm_cmpdi_zero"
  [(set (reg:CC_Z CC_REGNUM)
	(compare:CC_Z (match_operand:DI 0 "s_register_operand" "r")
		      (const_int 0)))
   (clobber (match_scratch:SI 1 "=r"))]
  "TARGET_32BIT"
  "orrs%?\\t%1, %Q0, %R0"
  [(set_attr "conds" "set")
   (set_attr "type" "logics_reg")]
)

; This insn allows redundant compares to be removed by cse, nothing should
; ever appear in the output file since (set (reg x) (reg x)) is a no-op that
; is deleted later on. The match_dup will match the mode here, so that
; mode changes of the condition codes aren't lost by this even though we don't
; specify what they are.

(define_insn "*deleted_compare"
  [(set (match_operand 0 "cc_register" "") (match_dup 0))]
  "TARGET_32BIT"
  "\\t%@ deleted compare"
  [(set_attr "conds" "set")
   (set_attr "length" "0")
   (set_attr "type" "no_insn")]
)


;; Conditional branch insns

(define_expand "cbranch_cc"
  [(set (pc)
	(if_then_else (match_operator 0 "" [(match_operand 1 "" "")
					    (match_operand 2 "" "")])
		      (label_ref (match_operand 3 "" ""))
		      (pc)))]
  "TARGET_32BIT"
  "operands[1] = arm_gen_compare_reg (GET_CODE (operands[0]),
				      operands[1], operands[2], NULL_RTX);
   operands[2] = const0_rtx;"
)

;;
;; Patterns to match conditional branch insns.
;;

(define_insn "arm_cond_branch"
  [(set (pc)
	(if_then_else (match_operator 1 "arm_comparison_operator"
		       [(match_operand 2 "cc_register" "") (const_int 0)])
		      (label_ref (match_operand 0 "" ""))
		      (pc)))]
  "TARGET_32BIT"
  "*
  if (arm_ccfsm_state == 1 || arm_ccfsm_state == 2)
    {
      arm_ccfsm_state += 2;
      return \"\";
    }
  return \"b%d1\\t%l0\";
  "
  [(set_attr "conds" "use")
   (set_attr "type" "branch")
   (set (attr "length")
	(if_then_else
	   (and (match_test "TARGET_THUMB2")
		(and (ge (minus (match_dup 0) (pc)) (const_int -250))
		     (le (minus (match_dup 0) (pc)) (const_int 256))))
	   (const_int 2)
	   (const_int 4)))]
)

(define_insn "*arm_cond_branch_reversed"
  [(set (pc)
	(if_then_else (match_operator 1 "arm_comparison_operator"
		       [(match_operand 2 "cc_register" "") (const_int 0)])
		      (pc)
		      (label_ref (match_operand 0 "" ""))))]
  "TARGET_32BIT"
  "*
  if (arm_ccfsm_state == 1 || arm_ccfsm_state == 2)
    {
      arm_ccfsm_state += 2;
      return \"\";
    }
  return \"b%D1\\t%l0\";
  "
  [(set_attr "conds" "use")
   (set_attr "type" "branch")
   (set (attr "length")
	(if_then_else
	   (and (match_test "TARGET_THUMB2")
		(and (ge (minus (match_dup 0) (pc)) (const_int -250))
		     (le (minus (match_dup 0) (pc)) (const_int 256))))
	   (const_int 2)
	   (const_int 4)))]
)



; scc insns

(define_expand "cstore_cc"
  [(set (match_operand:SI 0 "s_register_operand" "")
	(match_operator:SI 1 "" [(match_operand 2 "" "")
				 (match_operand 3 "" "")]))]
  "TARGET_32BIT"
  "operands[2] = arm_gen_compare_reg (GET_CODE (operands[1]),
				      operands[2], operands[3], NULL_RTX);
   operands[3] = const0_rtx;"
)

(define_insn_and_split "*mov_scc"
  [(set (match_operand:SI 0 "s_register_operand" "=r")
	(match_operator:SI 1 "arm_comparison_operator_mode"
	 [(match_operand 2 "cc_register" "") (const_int 0)]))]
  "TARGET_ARM"
  "#"   ; "mov%D1\\t%0, #0\;mov%d1\\t%0, #1"
  "TARGET_ARM"
  [(set (match_dup 0)
        (if_then_else:SI (match_dup 1)
                         (const_int 1)
                         (const_int 0)))]
  ""
  [(set_attr "conds" "use")
   (set_attr "length" "8")
   (set_attr "type" "multiple")]
)

(define_insn_and_split "*mov_negscc"
  [(set (match_operand:SI 0 "s_register_operand" "=r")
	(neg:SI (match_operator:SI 1 "arm_comparison_operator_mode"
		 [(match_operand 2 "cc_register" "") (const_int 0)])))]
  "TARGET_ARM"
  "#"   ; "mov%D1\\t%0, #0\;mvn%d1\\t%0, #0"
  "TARGET_ARM"
  [(set (match_dup 0)
        (if_then_else:SI (match_dup 1)
                         (match_dup 3)
                         (const_int 0)))]
  {
    operands[3] = GEN_INT (~0);
  }
  [(set_attr "conds" "use")
   (set_attr "length" "8")
   (set_attr "type" "multiple")]
)

(define_insn_and_split "*mov_notscc"
  [(set (match_operand:SI 0 "s_register_operand" "=r")
	(not:SI (match_operator:SI 1 "arm_comparison_operator"
		 [(match_operand 2 "cc_register" "") (const_int 0)])))]
  "TARGET_ARM"
  "#"   ; "mvn%D1\\t%0, #0\;mvn%d1\\t%0, #1"
  "TARGET_ARM"
  [(set (match_dup 0)
        (if_then_else:SI (match_dup 1)
                         (match_dup 3)
                         (match_dup 4)))]
  {
    operands[3] = GEN_INT (~1);
    operands[4] = GEN_INT (~0);
  }
  [(set_attr "conds" "use")
   (set_attr "length" "8")
   (set_attr "type" "multiple")]
)

(define_expand "cstoresi4"
  [(set (match_operand:SI 0 "s_register_operand" "")
	(match_operator:SI 1 "expandable_comparison_operator"
	 [(match_operand:SI 2 "s_register_operand" "")
	  (match_operand:SI 3 "reg_or_int_operand" "")]))]
  "TARGET_32BIT || TARGET_THUMB1"
  "{
  rtx op3, scratch, scratch2;

  if (!TARGET_THUMB1)
    {
      if (!arm_add_operand (operands[3], SImode))
	operands[3] = force_reg (SImode, operands[3]);
      emit_insn (gen_cstore_cc (operands[0], operands[1],
				operands[2], operands[3]));
      DONE;
    }

  if (operands[3] == const0_rtx)
    {
      switch (GET_CODE (operands[1]))
	{
	case EQ:
	  emit_insn (gen_cstoresi_eq0_thumb1 (operands[0], operands[2]));
	  break;

	case NE:
	  emit_insn (gen_cstoresi_ne0_thumb1 (operands[0], operands[2]));
	  break;

	case LE:
          scratch = expand_binop (SImode, add_optab, operands[2], constm1_rtx,
				  NULL_RTX, 0, OPTAB_WIDEN);
          scratch = expand_binop (SImode, ior_optab, operands[2], scratch,
				  NULL_RTX, 0, OPTAB_WIDEN);
          expand_binop (SImode, lshr_optab, scratch, GEN_INT (31),
			operands[0], 1, OPTAB_WIDEN);
	  break;

        case GE:
          scratch = expand_unop (SImode, one_cmpl_optab, operands[2],
				 NULL_RTX, 1);
          expand_binop (SImode, lshr_optab, scratch, GEN_INT (31),
			NULL_RTX, 1, OPTAB_WIDEN);
          break;

        case GT:
          scratch = expand_binop (SImode, ashr_optab, operands[2],
				  GEN_INT (31), NULL_RTX, 0, OPTAB_WIDEN);
          scratch = expand_binop (SImode, sub_optab, scratch, operands[2],
				  NULL_RTX, 0, OPTAB_WIDEN);
          expand_binop (SImode, lshr_optab, scratch, GEN_INT (31), operands[0],
			0, OPTAB_WIDEN);
          break;

	/* LT is handled by generic code.  No need for unsigned with 0.  */
	default:
	  FAIL;
	}
      DONE;
    }

  switch (GET_CODE (operands[1]))
    {
    case EQ:
      scratch = expand_binop (SImode, sub_optab, operands[2], operands[3],
			      NULL_RTX, 0, OPTAB_WIDEN);
      emit_insn (gen_cstoresi_eq0_thumb1 (operands[0], scratch));
      break;

    case NE:
      scratch = expand_binop (SImode, sub_optab, operands[2], operands[3],
			      NULL_RTX, 0, OPTAB_WIDEN);
      emit_insn (gen_cstoresi_ne0_thumb1 (operands[0], scratch));
      break;

    case LE:
      op3 = force_reg (SImode, operands[3]);

      scratch = expand_binop (SImode, lshr_optab, operands[2], GEN_INT (31),
			      NULL_RTX, 1, OPTAB_WIDEN);
      scratch2 = expand_binop (SImode, ashr_optab, op3, GEN_INT (31),
			      NULL_RTX, 0, OPTAB_WIDEN);
      emit_insn (gen_thumb1_addsi3_addgeu (operands[0], scratch, scratch2,
					  op3, operands[2]));
      break;

    case GE:
      op3 = operands[3];
      if (!thumb1_cmp_operand (op3, SImode))
        op3 = force_reg (SImode, op3);
      scratch = expand_binop (SImode, ashr_optab, operands[2], GEN_INT (31),
			      NULL_RTX, 0, OPTAB_WIDEN);
      scratch2 = expand_binop (SImode, lshr_optab, op3, GEN_INT (31),
			       NULL_RTX, 1, OPTAB_WIDEN);
      emit_insn (gen_thumb1_addsi3_addgeu (operands[0], scratch, scratch2,
					  operands[2], op3));
      break;

    case LEU:
      op3 = force_reg (SImode, operands[3]);
      scratch = force_reg (SImode, const0_rtx);
      emit_insn (gen_thumb1_addsi3_addgeu (operands[0], scratch, scratch,
					  op3, operands[2]));
      break;

    case GEU:
      op3 = operands[3];
      if (!thumb1_cmp_operand (op3, SImode))
        op3 = force_reg (SImode, op3);
      scratch = force_reg (SImode, const0_rtx);
      emit_insn (gen_thumb1_addsi3_addgeu (operands[0], scratch, scratch,
					  operands[2], op3));
      break;

    case LTU:
      op3 = operands[3];
      if (!thumb1_cmp_operand (op3, SImode))
        op3 = force_reg (SImode, op3);
      scratch = gen_reg_rtx (SImode);
      emit_insn (gen_cstoresi_ltu_thumb1 (operands[0], operands[2], op3));
      break;

    case GTU:
      op3 = force_reg (SImode, operands[3]);
      scratch = gen_reg_rtx (SImode);
      emit_insn (gen_cstoresi_ltu_thumb1 (operands[0], op3, operands[2]));
      break;

    /* No good sequences for GT, LT.  */
    default:
      FAIL;
    }
  DONE;
}")

(define_expand "cstoresf4"
  [(set (match_operand:SI 0 "s_register_operand" "")
	(match_operator:SI 1 "expandable_comparison_operator"
	 [(match_operand:SF 2 "s_register_operand" "")
	  (match_operand:SF 3 "arm_float_compare_operand" "")]))]
  "TARGET_32BIT && TARGET_HARD_FLOAT"
  "emit_insn (gen_cstore_cc (operands[0], operands[1],
			     operands[2], operands[3])); DONE;"
)

(define_expand "cstoredf4"
  [(set (match_operand:SI 0 "s_register_operand" "")
	(match_operator:SI 1 "expandable_comparison_operator"
	 [(match_operand:DF 2 "s_register_operand" "")
	  (match_operand:DF 3 "arm_float_compare_operand" "")]))]
  "TARGET_32BIT && TARGET_HARD_FLOAT && !TARGET_VFP_SINGLE"
  "emit_insn (gen_cstore_cc (operands[0], operands[1],
			     operands[2], operands[3])); DONE;"
)

(define_expand "cstoredi4"
  [(set (match_operand:SI 0 "s_register_operand" "")
	(match_operator:SI 1 "expandable_comparison_operator"
	 [(match_operand:DI 2 "s_register_operand" "")
	  (match_operand:DI 3 "cmpdi_operand" "")]))]
  "TARGET_32BIT"
  "{
     if (!arm_validize_comparison (&operands[1],
     				   &operands[2],
				   &operands[3]))
       FAIL;
     emit_insn (gen_cstore_cc (operands[0], operands[1], operands[2],
		      	         operands[3]));
     DONE;
   }"
)


;; Conditional move insns

(define_expand "movsicc"
  [(set (match_operand:SI 0 "s_register_operand" "")
	(if_then_else:SI (match_operand 1 "expandable_comparison_operator" "")
			 (match_operand:SI 2 "arm_not_operand" "")
			 (match_operand:SI 3 "arm_not_operand" "")))]
  "TARGET_32BIT"
  "
  {
    enum rtx_code code;
    rtx ccreg;

    if (!arm_validize_comparison (&operands[1], &XEXP (operands[1], 0), 
       				  &XEXP (operands[1], 1)))
      FAIL;
    
    code = GET_CODE (operands[1]);
    ccreg = arm_gen_compare_reg (code, XEXP (operands[1], 0),
				 XEXP (operands[1], 1), NULL_RTX);
    operands[1] = gen_rtx_fmt_ee (code, VOIDmode, ccreg, const0_rtx);
  }"
)

(define_expand "movsfcc"
  [(set (match_operand:SF 0 "s_register_operand" "")
	(if_then_else:SF (match_operand 1 "arm_cond_move_operator" "")
			 (match_operand:SF 2 "s_register_operand" "")
			 (match_operand:SF 3 "s_register_operand" "")))]
  "TARGET_32BIT && TARGET_HARD_FLOAT"
  "
  {
    enum rtx_code code = GET_CODE (operands[1]);
    rtx ccreg;

    if (!arm_validize_comparison (&operands[1], &XEXP (operands[1], 0), 
       				  &XEXP (operands[1], 1)))
       FAIL;

    code = GET_CODE (operands[1]);
    ccreg = arm_gen_compare_reg (code, XEXP (operands[1], 0),
				 XEXP (operands[1], 1), NULL_RTX);
    operands[1] = gen_rtx_fmt_ee (code, VOIDmode, ccreg, const0_rtx);
  }"
)

(define_expand "movdfcc"
  [(set (match_operand:DF 0 "s_register_operand" "")
	(if_then_else:DF (match_operand 1 "arm_cond_move_operator" "")
			 (match_operand:DF 2 "s_register_operand" "")
			 (match_operand:DF 3 "s_register_operand" "")))]
  "TARGET_32BIT && TARGET_HARD_FLOAT && TARGET_VFP_DOUBLE"
  "
  {
    enum rtx_code code = GET_CODE (operands[1]);
    rtx ccreg;

    if (!arm_validize_comparison (&operands[1], &XEXP (operands[1], 0), 
       				  &XEXP (operands[1], 1)))
       FAIL;
    code = GET_CODE (operands[1]);
    ccreg = arm_gen_compare_reg (code, XEXP (operands[1], 0),
				 XEXP (operands[1], 1), NULL_RTX);
    operands[1] = gen_rtx_fmt_ee (code, VOIDmode, ccreg, const0_rtx);
  }"
)

(define_insn "*cmov<mode>"
    [(set (match_operand:SDF 0 "s_register_operand" "=<F_constraint>")
	(if_then_else:SDF (match_operator 1 "arm_vsel_comparison_operator"
			  [(match_operand 2 "cc_register" "") (const_int 0)])
			  (match_operand:SDF 3 "s_register_operand"
			                      "<F_constraint>")
			  (match_operand:SDF 4 "s_register_operand"
			                      "<F_constraint>")))]
  "TARGET_HARD_FLOAT && TARGET_FPU_ARMV8 <vfp_double_cond>"
  "*
  {
    enum arm_cond_code code = maybe_get_arm_condition_code (operands[1]);
    switch (code)
      {
      case ARM_GE:
      case ARM_GT:
      case ARM_EQ:
      case ARM_VS:
        return \"vsel%d1.<V_if_elem>\\t%<V_reg>0, %<V_reg>3, %<V_reg>4\";
      case ARM_LT:
      case ARM_LE:
      case ARM_NE:
      case ARM_VC:
        return \"vsel%D1.<V_if_elem>\\t%<V_reg>0, %<V_reg>4, %<V_reg>3\";
      default:
        gcc_unreachable ();
      }
    return \"\";
  }"
  [(set_attr "conds" "use")
   (set_attr "type" "fcsel")]
)

(define_insn_and_split "*movsicc_insn"
  [(set (match_operand:SI 0 "s_register_operand" "=r,r,r,r,r,r,r,r")
	(if_then_else:SI
	 (match_operator 3 "arm_comparison_operator"
	  [(match_operand 4 "cc_register" "") (const_int 0)])
	 (match_operand:SI 1 "arm_not_operand" "0,0,rI,K,rI,rI,K,K")
	 (match_operand:SI 2 "arm_not_operand" "rI,K,0,0,rI,K,rI,K")))]
  "TARGET_ARM"
  "@
   mov%D3\\t%0, %2
   mvn%D3\\t%0, #%B2
   mov%d3\\t%0, %1
   mvn%d3\\t%0, #%B1
   #
   #
   #
   #"
   ; alt4: mov%d3\\t%0, %1\;mov%D3\\t%0, %2
   ; alt5: mov%d3\\t%0, %1\;mvn%D3\\t%0, #%B2
   ; alt6: mvn%d3\\t%0, #%B1\;mov%D3\\t%0, %2
   ; alt7: mvn%d3\\t%0, #%B1\;mvn%D3\\t%0, #%B2"
  "&& reload_completed"
  [(const_int 0)]
  {
    enum rtx_code rev_code;
    machine_mode mode;
    rtx rev_cond;

    emit_insn (gen_rtx_COND_EXEC (VOIDmode,
                                  operands[3],
                                  gen_rtx_SET (operands[0], operands[1])));

    rev_code = GET_CODE (operands[3]);
    mode = GET_MODE (operands[4]);
    if (mode == CCFPmode || mode == CCFPEmode)
      rev_code = reverse_condition_maybe_unordered (rev_code);
    else
      rev_code = reverse_condition (rev_code);

    rev_cond = gen_rtx_fmt_ee (rev_code,
                               VOIDmode,
                               operands[4],
                               const0_rtx);
    emit_insn (gen_rtx_COND_EXEC (VOIDmode,
                                  rev_cond,
                                  gen_rtx_SET (operands[0], operands[2])));
    DONE;
  }
  [(set_attr "length" "4,4,4,4,8,8,8,8")
   (set_attr "conds" "use")
   (set_attr_alternative "type"
                         [(if_then_else (match_operand 2 "const_int_operand" "")
                                        (const_string "mov_imm")
                                        (const_string "mov_reg"))
                          (const_string "mvn_imm")
                          (if_then_else (match_operand 1 "const_int_operand" "")
                                        (const_string "mov_imm")
                                        (const_string "mov_reg"))
                          (const_string "mvn_imm")
                          (const_string "multiple")
                          (const_string "multiple")
                          (const_string "multiple")
                          (const_string "multiple")])]
)

(define_insn "*movsfcc_soft_insn"
  [(set (match_operand:SF 0 "s_register_operand" "=r,r")
	(if_then_else:SF (match_operator 3 "arm_comparison_operator"
			  [(match_operand 4 "cc_register" "") (const_int 0)])
			 (match_operand:SF 1 "s_register_operand" "0,r")
			 (match_operand:SF 2 "s_register_operand" "r,0")))]
  "TARGET_ARM && TARGET_SOFT_FLOAT"
  "@
   mov%D3\\t%0, %2
   mov%d3\\t%0, %1"
  [(set_attr "conds" "use")
   (set_attr "type" "mov_reg")]
)


;; Jump and linkage insns

(define_expand "jump"
  [(set (pc)
	(label_ref (match_operand 0 "" "")))]
  "TARGET_EITHER"
  ""
)

(define_insn "*arm_jump"
  [(set (pc)
	(label_ref (match_operand 0 "" "")))]
  "TARGET_32BIT"
  "*
  {
    if (arm_ccfsm_state == 1 || arm_ccfsm_state == 2)
      {
        arm_ccfsm_state += 2;
        return \"\";
      }
    return \"b%?\\t%l0\";
  }
  "
  [(set_attr "predicable" "yes")
   (set (attr "length")
	(if_then_else
	   (and (match_test "TARGET_THUMB2")
		(and (ge (minus (match_dup 0) (pc)) (const_int -2044))
		     (le (minus (match_dup 0) (pc)) (const_int 2048))))
	   (const_int 2)
	   (const_int 4)))
   (set_attr "type" "branch")]
)

(define_expand "call"
  [(parallel [(call (match_operand 0 "memory_operand" "")
	            (match_operand 1 "general_operand" ""))
	      (use (match_operand 2 "" ""))
	      (clobber (reg:SI LR_REGNUM))])]
  "TARGET_EITHER"
  "
  {
    rtx callee, pat;
    
    /* In an untyped call, we can get NULL for operand 2.  */
    if (operands[2] == NULL_RTX)
      operands[2] = const0_rtx;
      
    /* Decide if we should generate indirect calls by loading the
       32-bit address of the callee into a register before performing the
       branch and link.  */
    callee = XEXP (operands[0], 0);
    if (GET_CODE (callee) == SYMBOL_REF
	? arm_is_long_call_p (SYMBOL_REF_DECL (callee))
	: !REG_P (callee))
      XEXP (operands[0], 0) = force_reg (Pmode, callee);

    pat = gen_call_internal (operands[0], operands[1], operands[2]);
    arm_emit_call_insn (pat, XEXP (operands[0], 0), false);
    DONE;
  }"
)

(define_expand "call_internal"
  [(parallel [(call (match_operand 0 "memory_operand" "")
	            (match_operand 1 "general_operand" ""))
	      (use (match_operand 2 "" ""))
	      (clobber (reg:SI LR_REGNUM))])])

(define_insn "*call_reg_armv5"
  [(call (mem:SI (match_operand:SI 0 "s_register_operand" "r"))
         (match_operand 1 "" ""))
   (use (match_operand 2 "" ""))
   (clobber (reg:SI LR_REGNUM))]
  "TARGET_ARM && arm_arch5 && !SIBLING_CALL_P (insn)"
  "blx%?\\t%0"
  [(set_attr "type" "call")]
)

(define_insn "*call_reg_arm"
  [(call (mem:SI (match_operand:SI 0 "s_register_operand" "r"))
         (match_operand 1 "" ""))
   (use (match_operand 2 "" ""))
   (clobber (reg:SI LR_REGNUM))]
  "TARGET_ARM && !arm_arch5 && !SIBLING_CALL_P (insn)"
  "*
  return output_call (operands);
  "
  ;; length is worst case, normally it is only two
  [(set_attr "length" "12")
   (set_attr "type" "call")]
)


(define_expand "call_value"
  [(parallel [(set (match_operand       0 "" "")
	           (call (match_operand 1 "memory_operand" "")
		         (match_operand 2 "general_operand" "")))
	      (use (match_operand 3 "" ""))
	      (clobber (reg:SI LR_REGNUM))])]
  "TARGET_EITHER"
  "
  {
    rtx pat, callee;
    
    /* In an untyped call, we can get NULL for operand 2.  */
    if (operands[3] == 0)
      operands[3] = const0_rtx;
      
    /* Decide if we should generate indirect calls by loading the
       32-bit address of the callee into a register before performing the
       branch and link.  */
    callee = XEXP (operands[1], 0);
    if (GET_CODE (callee) == SYMBOL_REF
	? arm_is_long_call_p (SYMBOL_REF_DECL (callee))
	: !REG_P (callee))
      XEXP (operands[1], 0) = force_reg (Pmode, callee);

    pat = gen_call_value_internal (operands[0], operands[1],
				   operands[2], operands[3]);
    arm_emit_call_insn (pat, XEXP (operands[1], 0), false);
    DONE;
  }"
)

(define_expand "call_value_internal"
  [(parallel [(set (match_operand       0 "" "")
	           (call (match_operand 1 "memory_operand" "")
		         (match_operand 2 "general_operand" "")))
	      (use (match_operand 3 "" ""))
	      (clobber (reg:SI LR_REGNUM))])])

(define_insn "*call_value_reg_armv5"
  [(set (match_operand 0 "" "")
        (call (mem:SI (match_operand:SI 1 "s_register_operand" "r"))
	      (match_operand 2 "" "")))
   (use (match_operand 3 "" ""))
   (clobber (reg:SI LR_REGNUM))]
  "TARGET_ARM && arm_arch5 && !SIBLING_CALL_P (insn)"
  "blx%?\\t%1"
  [(set_attr "type" "call")]
)

(define_insn "*call_value_reg_arm"
  [(set (match_operand 0 "" "")
        (call (mem:SI (match_operand:SI 1 "s_register_operand" "r"))
	      (match_operand 2 "" "")))
   (use (match_operand 3 "" ""))
   (clobber (reg:SI LR_REGNUM))]
  "TARGET_ARM && !arm_arch5 && !SIBLING_CALL_P (insn)"
  "*
  return output_call (&operands[1]);
  "
  [(set_attr "length" "12")
   (set_attr "type" "call")]
)

;; Allow calls to SYMBOL_REFs specially as they are not valid general addresses
;; The 'a' causes the operand to be treated as an address, i.e. no '#' output.

(define_insn "*call_symbol"
  [(call (mem:SI (match_operand:SI 0 "" ""))
	 (match_operand 1 "" ""))
   (use (match_operand 2 "" ""))
   (clobber (reg:SI LR_REGNUM))]
  "TARGET_32BIT
   && !SIBLING_CALL_P (insn)
   && (GET_CODE (operands[0]) == SYMBOL_REF)
   && !arm_is_long_call_p (SYMBOL_REF_DECL (operands[0]))"
  "*
  {
   rtx op = operands[0];

   /* Switch mode now when possible.  */
   if (SYMBOL_REF_DECL (op) && !TREE_PUBLIC (SYMBOL_REF_DECL (op))
        && arm_arch5 && arm_change_mode_p (SYMBOL_REF_DECL (op)))
      return NEED_PLT_RELOC ? \"blx%?\\t%a0(PLT)\" : \"blx%?\\t(%a0)\";

    return NEED_PLT_RELOC ? \"bl%?\\t%a0(PLT)\" : \"bl%?\\t%a0\";
  }"
  [(set_attr "type" "call")]
)

(define_insn "*call_value_symbol"
  [(set (match_operand 0 "" "")
	(call (mem:SI (match_operand:SI 1 "" ""))
	(match_operand:SI 2 "" "")))
   (use (match_operand 3 "" ""))
   (clobber (reg:SI LR_REGNUM))]
  "TARGET_32BIT
   && !SIBLING_CALL_P (insn)
   && (GET_CODE (operands[1]) == SYMBOL_REF)
   && !arm_is_long_call_p (SYMBOL_REF_DECL (operands[1]))"
  "*
  {
   rtx op = operands[1];

   /* Switch mode now when possible.  */
   if (SYMBOL_REF_DECL (op) && !TREE_PUBLIC (SYMBOL_REF_DECL (op))
        && arm_arch5 && arm_change_mode_p (SYMBOL_REF_DECL (op)))
      return NEED_PLT_RELOC ? \"blx%?\\t%a1(PLT)\" : \"blx%?\\t(%a1)\";

    return NEED_PLT_RELOC ? \"bl%?\\t%a1(PLT)\" : \"bl%?\\t%a1\";
  }"
  [(set_attr "type" "call")]
)

(define_expand "sibcall_internal"
  [(parallel [(call (match_operand 0 "memory_operand" "")
		    (match_operand 1 "general_operand" ""))
	      (return)
	      (use (match_operand 2 "" ""))])])

;; We may also be able to do sibcalls for Thumb, but it's much harder...
(define_expand "sibcall"
  [(parallel [(call (match_operand 0 "memory_operand" "")
		    (match_operand 1 "general_operand" ""))
	      (return)
	      (use (match_operand 2 "" ""))])]
  "TARGET_32BIT"
  "
  {
    rtx pat;

    if ((!REG_P (XEXP (operands[0], 0))
	 && GET_CODE (XEXP (operands[0], 0)) != SYMBOL_REF)
	|| (GET_CODE (XEXP (operands[0], 0)) == SYMBOL_REF
	    && arm_is_long_call_p (SYMBOL_REF_DECL (XEXP (operands[0], 0)))))
     XEXP (operands[0], 0) = force_reg (SImode, XEXP (operands[0], 0));

    if (operands[2] == NULL_RTX)
      operands[2] = const0_rtx;

    pat = gen_sibcall_internal (operands[0], operands[1], operands[2]);
    arm_emit_call_insn (pat, operands[0], true);
    DONE;
  }"
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
  "TARGET_32BIT"
  "
  {
    rtx pat;

    if ((!REG_P (XEXP (operands[1], 0))
	 && GET_CODE (XEXP (operands[1], 0)) != SYMBOL_REF)
	|| (GET_CODE (XEXP (operands[1], 0)) == SYMBOL_REF
	    && arm_is_long_call_p (SYMBOL_REF_DECL (XEXP (operands[1], 0)))))
     XEXP (operands[1], 0) = force_reg (SImode, XEXP (operands[1], 0));

    if (operands[3] == NULL_RTX)
      operands[3] = const0_rtx;

    pat = gen_sibcall_value_internal (operands[0], operands[1],
                                      operands[2], operands[3]);
    arm_emit_call_insn (pat, operands[1], true);
    DONE;
  }"
)

(define_insn "*sibcall_insn"
 [(call (mem:SI (match_operand:SI 0 "call_insn_operand" "Cs, US"))
	(match_operand 1 "" ""))
  (return)
  (use (match_operand 2 "" ""))]
  "TARGET_32BIT && SIBLING_CALL_P (insn)"
  "*
  if (which_alternative == 1)
    return NEED_PLT_RELOC ? \"b%?\\t%a0(PLT)\" : \"b%?\\t%a0\";
  else
    {
      if (arm_arch5 || arm_arch4t)
	return \"bx%?\\t%0\\t%@ indirect register sibling call\";
      else
	return \"mov%?\\t%|pc, %0\\t%@ indirect register sibling call\";
    }
  "
  [(set_attr "type" "call")]
)

(define_insn "*sibcall_value_insn"
 [(set (match_operand 0 "" "")
       (call (mem:SI (match_operand:SI 1 "call_insn_operand" "Cs,US"))
	     (match_operand 2 "" "")))
  (return)
  (use (match_operand 3 "" ""))]
  "TARGET_32BIT && SIBLING_CALL_P (insn)"
  "*
  if (which_alternative == 1)
   return NEED_PLT_RELOC ? \"b%?\\t%a1(PLT)\" : \"b%?\\t%a1\";
  else
    {
      if (arm_arch5 || arm_arch4t)
	return \"bx%?\\t%1\";
      else
	return \"mov%?\\t%|pc, %1\\t@ indirect sibling call \";
    }
  "
  [(set_attr "type" "call")]
)

(define_expand "<return_str>return"
  [(RETURNS)]
  "(TARGET_ARM || (TARGET_THUMB2
                   && ARM_FUNC_TYPE (arm_current_func_type ()) == ARM_FT_NORMAL
                   && !IS_STACKALIGN (arm_current_func_type ())))
    <return_cond_false>"
  "
  {
    if (TARGET_THUMB2)
      {
        thumb2_expand_return (<return_simple_p>);
        DONE;
      }
  }
  "
)

;; Often the return insn will be the same as loading from memory, so set attr
(define_insn "*arm_return"
  [(return)]
  "TARGET_ARM && USE_RETURN_INSN (FALSE)"
  "*
  {
    if (arm_ccfsm_state == 2)
      {
        arm_ccfsm_state += 2;
        return \"\";
      }
    return output_return_instruction (const_true_rtx, true, false, false);
  }"
  [(set_attr "type" "load1")
   (set_attr "length" "12")
   (set_attr "predicable" "yes")]
)

(define_insn "*cond_<return_str>return"
  [(set (pc)
        (if_then_else (match_operator 0 "arm_comparison_operator"
		       [(match_operand 1 "cc_register" "") (const_int 0)])
                      (RETURNS)
                      (pc)))]
  "TARGET_ARM  <return_cond_true>"
  "*
  {
    if (arm_ccfsm_state == 2)
      {
        arm_ccfsm_state += 2;
        return \"\";
      }
    return output_return_instruction (operands[0], true, false,
				      <return_simple_p>);
  }"
  [(set_attr "conds" "use")
   (set_attr "length" "12")
   (set_attr "type" "load1")]
)

(define_insn "*cond_<return_str>return_inverted"
  [(set (pc)
        (if_then_else (match_operator 0 "arm_comparison_operator"
		       [(match_operand 1 "cc_register" "") (const_int 0)])
                      (pc)
		      (RETURNS)))]
  "TARGET_ARM <return_cond_true>"
  "*
  {
    if (arm_ccfsm_state == 2)
      {
        arm_ccfsm_state += 2;
        return \"\";
      }
    return output_return_instruction (operands[0], true, true,
				      <return_simple_p>);
  }"
  [(set_attr "conds" "use")
   (set_attr "length" "12")
   (set_attr "type" "load1")]
)

(define_insn "*arm_simple_return"
  [(simple_return)]
  "TARGET_ARM"
  "*
  {
    if (arm_ccfsm_state == 2)
      {
        arm_ccfsm_state += 2;
        return \"\";
      }
    return output_return_instruction (const_true_rtx, true, false, true);
  }"
  [(set_attr "type" "branch")
   (set_attr "length" "4")
   (set_attr "predicable" "yes")]
)

;; Generate a sequence of instructions to determine if the processor is
;; in 26-bit or 32-bit mode, and return the appropriate return address
;; mask.

(define_expand "return_addr_mask"
  [(set (match_dup 1)
      (compare:CC_NOOV (unspec [(const_int 0)] UNSPEC_CHECK_ARCH)
		       (const_int 0)))
   (set (match_operand:SI 0 "s_register_operand" "")
      (if_then_else:SI (eq (match_dup 1) (const_int 0))
		       (const_int -1)
		       (const_int 67108860)))] ; 0x03fffffc
  "TARGET_ARM"
  "
  operands[1] = gen_rtx_REG (CC_NOOVmode, CC_REGNUM);
  ")

(define_insn "*check_arch2"
  [(set (match_operand:CC_NOOV 0 "cc_register" "")
      (compare:CC_NOOV (unspec [(const_int 0)] UNSPEC_CHECK_ARCH)
		       (const_int 0)))]
  "TARGET_ARM"
  "teq\\t%|r0, %|r0\;teq\\t%|pc, %|pc"
  [(set_attr "length" "8")
   (set_attr "conds" "set")
   (set_attr "type" "multiple")]
)

;; Call subroutine returning any type.

(define_expand "untyped_call"
  [(parallel [(call (match_operand 0 "" "")
		    (const_int 0))
	      (match_operand 1 "" "")
	      (match_operand 2 "" "")])]
  "TARGET_EITHER"
  "
  {
    int i;
    rtx par = gen_rtx_PARALLEL (VOIDmode,
				rtvec_alloc (XVECLEN (operands[2], 0)));
    rtx addr = gen_reg_rtx (Pmode);
    rtx mem;
    int size = 0;

    emit_move_insn (addr, XEXP (operands[1], 0));
    mem = change_address (operands[1], BLKmode, addr);

    for (i = 0; i < XVECLEN (operands[2], 0); i++)
      {
	rtx src = SET_SRC (XVECEXP (operands[2], 0, i));

	/* Default code only uses r0 as a return value, but we could
	   be using anything up to 4 registers.  */
	if (REGNO (src) == R0_REGNUM)
	  src = gen_rtx_REG (TImode, R0_REGNUM);

        XVECEXP (par, 0, i) = gen_rtx_EXPR_LIST (VOIDmode, src,
						 GEN_INT (size));
        size += GET_MODE_SIZE (GET_MODE (src));
      }

    emit_call_insn (gen_call_value (par, operands[0], const0_rtx, NULL));

    size = 0;

    for (i = 0; i < XVECLEN (par, 0); i++)
      {
	HOST_WIDE_INT offset = 0;
	rtx reg = XEXP (XVECEXP (par, 0, i), 0);

	if (size != 0)
	  emit_move_insn (addr, plus_constant (Pmode, addr, size));

	mem = change_address (mem, GET_MODE (reg), NULL);
	if (REGNO (reg) == R0_REGNUM)
	  {
	    /* On thumb we have to use a write-back instruction.  */
	    emit_insn (arm_gen_store_multiple (arm_regs_in_sequence, 4, addr,
 		       TARGET_THUMB ? TRUE : FALSE, mem, &offset));
	    size = TARGET_ARM ? 16 : 0;
	  }
	else
	  {
	    emit_move_insn (mem, reg);
	    size = GET_MODE_SIZE (GET_MODE (reg));
	  }
      }

    /* The optimizer does not know that the call sets the function value
       registers we stored in the result block.  We avoid problems by
       claiming that all hard registers are used and clobbered at this
       point.  */
    emit_insn (gen_blockage ());

    DONE;
  }"
)

(define_expand "untyped_return"
  [(match_operand:BLK 0 "memory_operand" "")
   (match_operand 1 "" "")]
  "TARGET_EITHER"
  "
  {
    int i;
    rtx addr = gen_reg_rtx (Pmode);
    rtx mem;
    int size = 0;

    emit_move_insn (addr, XEXP (operands[0], 0));
    mem = change_address (operands[0], BLKmode, addr);

    for (i = 0; i < XVECLEN (operands[1], 0); i++)
      {
	HOST_WIDE_INT offset = 0;
	rtx reg = SET_DEST (XVECEXP (operands[1], 0, i));

	if (size != 0)
	  emit_move_insn (addr, plus_constant (Pmode, addr, size));

	mem = change_address (mem, GET_MODE (reg), NULL);
	if (REGNO (reg) == R0_REGNUM)
	  {
	    /* On thumb we have to use a write-back instruction.  */
	    emit_insn (arm_gen_load_multiple (arm_regs_in_sequence, 4, addr,
 		       TARGET_THUMB ? TRUE : FALSE, mem, &offset));
	    size = TARGET_ARM ? 16 : 0;
	  }
	else
	  {
	    emit_move_insn (reg, mem);
	    size = GET_MODE_SIZE (GET_MODE (reg));
	  }
      }

    /* Emit USE insns before the return.  */
    for (i = 0; i < XVECLEN (operands[1], 0); i++)
      emit_use (SET_DEST (XVECEXP (operands[1], 0, i)));

    /* Construct the return.  */
    expand_naked_return ();

    DONE;
  }"
)

;; UNSPEC_VOLATILE is considered to use and clobber all hard registers and
;; all of memory.  This blocks insns from being moved across this point.

(define_insn "blockage"
  [(unspec_volatile [(const_int 0)] VUNSPEC_BLOCKAGE)]
  "TARGET_EITHER"
  ""
  [(set_attr "length" "0")
   (set_attr "type" "block")]
)

(define_insn "probe_stack"
  [(set (match_operand:SI 0 "memory_operand" "=m")
        (unspec:SI [(const_int 0)] UNSPEC_PROBE_STACK))]
  "TARGET_32BIT"
  "str%?\\tr0, %0"
  [(set_attr "type" "store1")
   (set_attr "predicable" "yes")]
)

(define_insn "probe_stack_range"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(unspec_volatile:SI [(match_operand:SI 1 "register_operand" "0")
			     (match_operand:SI 2 "register_operand" "r")]
			     VUNSPEC_PROBE_STACK_RANGE))]
  "TARGET_32BIT"
{
  return output_probe_stack_range (operands[0], operands[2]);
}
  [(set_attr "type" "multiple")
   (set_attr "conds" "clob")]
)

(define_expand "casesi"
  [(match_operand:SI 0 "s_register_operand" "")	; index to jump on
   (match_operand:SI 1 "const_int_operand" "")	; lower bound
   (match_operand:SI 2 "const_int_operand" "")	; total range
   (match_operand:SI 3 "" "")			; table label
   (match_operand:SI 4 "" "")]			; Out of range label
  "(TARGET_32BIT || optimize_size || flag_pic) && !target_pure_code"
  "
  {
    enum insn_code code;
    if (operands[1] != const0_rtx)
      {
	rtx reg = gen_reg_rtx (SImode);

	emit_insn (gen_addsi3 (reg, operands[0],
			       gen_int_mode (-INTVAL (operands[1]),
			       		     SImode)));
	operands[0] = reg;
      }

    if (TARGET_ARM)
      code = CODE_FOR_arm_casesi_internal;
    else if (TARGET_THUMB1)
      code = CODE_FOR_thumb1_casesi_internal_pic;
    else if (flag_pic)
      code = CODE_FOR_thumb2_casesi_internal_pic;
    else
      code = CODE_FOR_thumb2_casesi_internal;

    if (!insn_data[(int) code].operand[1].predicate(operands[2], SImode))
      operands[2] = force_reg (SImode, operands[2]);

    emit_jump_insn (GEN_FCN ((int) code) (operands[0], operands[2],
					  operands[3], operands[4]));
    DONE;
  }"
)

;; The USE in this pattern is needed to tell flow analysis that this is
;; a CASESI insn.  It has no other purpose.
(define_insn "arm_casesi_internal"
  [(parallel [(set (pc)
	       (if_then_else
		(leu (match_operand:SI 0 "s_register_operand" "r")
		     (match_operand:SI 1 "arm_rhs_operand" "rI"))
		(mem:SI (plus:SI (mult:SI (match_dup 0) (const_int 4))
				 (label_ref (match_operand 2 "" ""))))
		(label_ref (match_operand 3 "" ""))))
	      (clobber (reg:CC CC_REGNUM))
	      (use (label_ref (match_dup 2)))])]
  "TARGET_ARM"
  "*
    if (flag_pic)
      return \"cmp\\t%0, %1\;addls\\t%|pc, %|pc, %0, asl #2\;b\\t%l3\";
    return   \"cmp\\t%0, %1\;ldrls\\t%|pc, [%|pc, %0, asl #2]\;b\\t%l3\";
  "
  [(set_attr "conds" "clob")
   (set_attr "length" "12")
   (set_attr "type" "multiple")]
)

(define_expand "indirect_jump"
  [(set (pc)
	(match_operand:SI 0 "s_register_operand" ""))]
  "TARGET_EITHER"
  "
  /* Thumb-2 doesn't have mov pc, reg.  Explicitly set the low bit of the
     address and use bx.  */
  if (TARGET_THUMB2)
    {
      rtx tmp;
      tmp = gen_reg_rtx (SImode);
      emit_insn (gen_iorsi3 (tmp, operands[0], GEN_INT(1)));
      operands[0] = tmp;
    }
  "
)

;; NB Never uses BX.
(define_insn "*arm_indirect_jump"
  [(set (pc)
	(match_operand:SI 0 "s_register_operand" "r"))]
  "TARGET_ARM"
  "mov%?\\t%|pc, %0\\t%@ indirect register jump"
  [(set_attr "predicable" "yes")
   (set_attr "type" "branch")]
)

(define_insn "*load_indirect_jump"
  [(set (pc)
	(match_operand:SI 0 "memory_operand" "m"))]
  "TARGET_ARM"
  "ldr%?\\t%|pc, %0\\t%@ indirect memory jump"
  [(set_attr "type" "load1")
   (set_attr "pool_range" "4096")
   (set_attr "neg_pool_range" "4084")
   (set_attr "predicable" "yes")]
)


;; Misc insns

(define_insn "nop"
  [(const_int 0)]
  "TARGET_EITHER"
  "nop"
  [(set (attr "length")
	(if_then_else (eq_attr "is_thumb" "yes")
		      (const_int 2)
		      (const_int 4)))
   (set_attr "type" "mov_reg")]
)

(define_insn "trap"
  [(trap_if (const_int 1) (const_int 0))]
  ""
  "*
  if (TARGET_ARM)
    return \".inst\\t0xe7f000f0\";
  else
    return \".inst\\t0xdeff\";
  "
  [(set (attr "length")
	(if_then_else (eq_attr "is_thumb" "yes")
		      (const_int 2)
		      (const_int 4)))
   (set_attr "type" "trap")
   (set_attr "conds" "unconditional")]
)


;; Patterns to allow combination of arithmetic, cond code and shifts

(define_insn "*<arith_shift_insn>_multsi"
  [(set (match_operand:SI 0 "s_register_operand" "=r,r")
	(SHIFTABLE_OPS:SI
	 (mult:SI (match_operand:SI 2 "s_register_operand" "r,r")
		  (match_operand:SI 3 "power_of_two_operand" ""))
	 (match_operand:SI 1 "s_register_operand" "rk,<t2_binop0>")))]
  "TARGET_32BIT"
  "<arith_shift_insn>%?\\t%0, %1, %2, lsl %b3"
  [(set_attr "predicable" "yes")
   (set_attr "predicable_short_it" "no")
   (set_attr "shift" "2")
   (set_attr "arch" "a,t2")
   (set_attr "type" "alu_shift_imm")])

(define_insn "*<arith_shift_insn>_shiftsi"
  [(set (match_operand:SI 0 "s_register_operand" "=r,r,r")
	(SHIFTABLE_OPS:SI
	 (match_operator:SI 2 "shift_nomul_operator"
	  [(match_operand:SI 3 "s_register_operand" "r,r,r")
	   (match_operand:SI 4 "shift_amount_operand" "M,M,r")])
	 (match_operand:SI 1 "s_register_operand" "rk,<t2_binop0>,rk")))]
  "TARGET_32BIT && GET_CODE (operands[2]) != MULT"
  "<arith_shift_insn>%?\\t%0, %1, %3%S2"
  [(set_attr "predicable" "yes")
   (set_attr "predicable_short_it" "no")
   (set_attr "shift" "3")
   (set_attr "arch" "a,t2,a")
   (set_attr "type" "alu_shift_imm,alu_shift_imm,alu_shift_reg")])

(define_split
  [(set (match_operand:SI 0 "s_register_operand" "")
	(match_operator:SI 1 "shiftable_operator"
	 [(match_operator:SI 2 "shiftable_operator"
	   [(match_operator:SI 3 "shift_operator"
	     [(match_operand:SI 4 "s_register_operand" "")
	      (match_operand:SI 5 "reg_or_int_operand" "")])
	    (match_operand:SI 6 "s_register_operand" "")])
	  (match_operand:SI 7 "arm_rhs_operand" "")]))
   (clobber (match_operand:SI 8 "s_register_operand" ""))]
  "TARGET_32BIT"
  [(set (match_dup 8)
	(match_op_dup 2 [(match_op_dup 3 [(match_dup 4) (match_dup 5)])
			 (match_dup 6)]))
   (set (match_dup 0)
	(match_op_dup 1 [(match_dup 8) (match_dup 7)]))]
  "")

(define_insn "*arith_shiftsi_compare0"
  [(set (reg:CC_NOOV CC_REGNUM)
        (compare:CC_NOOV
	 (match_operator:SI 1 "shiftable_operator"
	  [(match_operator:SI 3 "shift_operator"
	    [(match_operand:SI 4 "s_register_operand" "r,r")
	     (match_operand:SI 5 "shift_amount_operand" "M,r")])
	   (match_operand:SI 2 "s_register_operand" "r,r")])
	 (const_int 0)))
   (set (match_operand:SI 0 "s_register_operand" "=r,r")
	(match_op_dup 1 [(match_op_dup 3 [(match_dup 4) (match_dup 5)])
			 (match_dup 2)]))]
  "TARGET_32BIT"
  "%i1s%?\\t%0, %2, %4%S3"
  [(set_attr "conds" "set")
   (set_attr "shift" "4")
   (set_attr "arch" "32,a")
   (set_attr "type" "alus_shift_imm,alus_shift_reg")])

(define_insn "*arith_shiftsi_compare0_scratch"
  [(set (reg:CC_NOOV CC_REGNUM)
        (compare:CC_NOOV
	 (match_operator:SI 1 "shiftable_operator"
	  [(match_operator:SI 3 "shift_operator"
	    [(match_operand:SI 4 "s_register_operand" "r,r")
	     (match_operand:SI 5 "shift_amount_operand" "M,r")])
	   (match_operand:SI 2 "s_register_operand" "r,r")])
	 (const_int 0)))
   (clobber (match_scratch:SI 0 "=r,r"))]
  "TARGET_32BIT"
  "%i1s%?\\t%0, %2, %4%S3"
  [(set_attr "conds" "set")
   (set_attr "shift" "4")
   (set_attr "arch" "32,a")
   (set_attr "type" "alus_shift_imm,alus_shift_reg")])

(define_insn "*sub_shiftsi"
  [(set (match_operand:SI 0 "s_register_operand" "=r,r")
	(minus:SI (match_operand:SI 1 "s_register_operand" "r,r")
		  (match_operator:SI 2 "shift_operator"
		   [(match_operand:SI 3 "s_register_operand" "r,r")
		    (match_operand:SI 4 "shift_amount_operand" "M,r")])))]
  "TARGET_32BIT"
  "sub%?\\t%0, %1, %3%S2"
  [(set_attr "predicable" "yes")
   (set_attr "shift" "3")
   (set_attr "arch" "32,a")
   (set_attr "type" "alus_shift_imm,alus_shift_reg")])

(define_insn "*sub_shiftsi_compare0"
  [(set (reg:CC_NOOV CC_REGNUM)
	(compare:CC_NOOV
	 (minus:SI (match_operand:SI 1 "s_register_operand" "r,r,r")
		   (match_operator:SI 2 "shift_operator"
		    [(match_operand:SI 3 "s_register_operand" "r,r,r")
		     (match_operand:SI 4 "shift_amount_operand" "M,r,M")]))
	 (const_int 0)))
   (set (match_operand:SI 0 "s_register_operand" "=r,r,r")
	(minus:SI (match_dup 1)
		  (match_op_dup 2 [(match_dup 3) (match_dup 4)])))]
  "TARGET_32BIT"
  "subs%?\\t%0, %1, %3%S2"
  [(set_attr "conds" "set")
   (set_attr "shift" "3")
   (set_attr "arch" "32,a,a")
   (set_attr "type" "alus_shift_imm,alus_shift_reg,alus_shift_imm")])

(define_insn "*sub_shiftsi_compare0_scratch"
  [(set (reg:CC_NOOV CC_REGNUM)
	(compare:CC_NOOV
	 (minus:SI (match_operand:SI 1 "s_register_operand" "r,r,r")
		   (match_operator:SI 2 "shift_operator"
		    [(match_operand:SI 3 "s_register_operand" "r,r,r")
		     (match_operand:SI 4 "shift_amount_operand" "M,r,M")]))
	 (const_int 0)))
   (clobber (match_scratch:SI 0 "=r,r,r"))]
  "TARGET_32BIT"
  "subs%?\\t%0, %1, %3%S2"
  [(set_attr "conds" "set")
   (set_attr "shift" "3")
   (set_attr "arch" "32,a,a")
   (set_attr "type" "alus_shift_imm,alus_shift_reg,alus_shift_imm")])


(define_insn_and_split "*and_scc"
  [(set (match_operand:SI 0 "s_register_operand" "=r")
	(and:SI (match_operator:SI 1 "arm_comparison_operator"
		 [(match_operand 2 "cc_register" "") (const_int 0)])
		(match_operand:SI 3 "s_register_operand" "r")))]
  "TARGET_ARM"
  "#"   ; "mov%D1\\t%0, #0\;and%d1\\t%0, %3, #1"
  "&& reload_completed"
  [(cond_exec (match_dup 5) (set (match_dup 0) (const_int 0)))
   (cond_exec (match_dup 4) (set (match_dup 0)
                                 (and:SI (match_dup 3) (const_int 1))))]
  {
    machine_mode mode = GET_MODE (operands[2]);
    enum rtx_code rc = GET_CODE (operands[1]);

    /* Note that operands[4] is the same as operands[1],
       but with VOIDmode as the result. */
    operands[4] = gen_rtx_fmt_ee (rc, VOIDmode, operands[2], const0_rtx);
    if (mode == CCFPmode || mode == CCFPEmode)
      rc = reverse_condition_maybe_unordered (rc);
    else
      rc = reverse_condition (rc);
    operands[5] = gen_rtx_fmt_ee (rc, VOIDmode, operands[2], const0_rtx);
  }
  [(set_attr "conds" "use")
   (set_attr "type" "multiple")
   (set_attr "length" "8")]
)

(define_insn_and_split "*ior_scc"
  [(set (match_operand:SI 0 "s_register_operand" "=r,r")
	(ior:SI (match_operator:SI 1 "arm_comparison_operator"
		 [(match_operand 2 "cc_register" "") (const_int 0)])
		(match_operand:SI 3 "s_register_operand" "0,?r")))]
  "TARGET_ARM"
  "@
   orr%d1\\t%0, %3, #1
   #"
  "&& reload_completed
   && REGNO (operands [0]) != REGNO (operands[3])"
  ;; && which_alternative == 1
  ; mov%D1\\t%0, %3\;orr%d1\\t%0, %3, #1
  [(cond_exec (match_dup 5) (set (match_dup 0) (match_dup 3)))
   (cond_exec (match_dup 4) (set (match_dup 0)
                                 (ior:SI (match_dup 3) (const_int 1))))]
  {
    machine_mode mode = GET_MODE (operands[2]);
    enum rtx_code rc = GET_CODE (operands[1]);

    /* Note that operands[4] is the same as operands[1],
       but with VOIDmode as the result. */
    operands[4] = gen_rtx_fmt_ee (rc, VOIDmode, operands[2], const0_rtx);
    if (mode == CCFPmode || mode == CCFPEmode)
      rc = reverse_condition_maybe_unordered (rc);
    else
      rc = reverse_condition (rc);
    operands[5] = gen_rtx_fmt_ee (rc, VOIDmode, operands[2], const0_rtx);
  }
  [(set_attr "conds" "use")
   (set_attr "length" "4,8")
   (set_attr "type" "logic_imm,multiple")]
)

; A series of splitters for the compare_scc pattern below.  Note that
; order is important.
(define_split
  [(set (match_operand:SI 0 "s_register_operand" "")
	(lt:SI (match_operand:SI 1 "s_register_operand" "")
	       (const_int 0)))
   (clobber (reg:CC CC_REGNUM))]
  "TARGET_32BIT && reload_completed"
  [(set (match_dup 0) (lshiftrt:SI (match_dup 1) (const_int 31)))])

(define_split
  [(set (match_operand:SI 0 "s_register_operand" "")
	(ge:SI (match_operand:SI 1 "s_register_operand" "")
	       (const_int 0)))
   (clobber (reg:CC CC_REGNUM))]
  "TARGET_32BIT && reload_completed"
  [(set (match_dup 0) (not:SI (match_dup 1)))
   (set (match_dup 0) (lshiftrt:SI (match_dup 0) (const_int 31)))])

(define_split
  [(set (match_operand:SI 0 "s_register_operand" "")
	(eq:SI (match_operand:SI 1 "s_register_operand" "")
	       (const_int 0)))
   (clobber (reg:CC CC_REGNUM))]
  "arm_arch5 && TARGET_32BIT"
  [(set (match_dup 0) (clz:SI (match_dup 1)))
   (set (match_dup 0) (lshiftrt:SI (match_dup 0) (const_int 5)))]
)

(define_split
  [(set (match_operand:SI 0 "s_register_operand" "")
	(eq:SI (match_operand:SI 1 "s_register_operand" "")
	       (const_int 0)))
   (clobber (reg:CC CC_REGNUM))]
  "TARGET_32BIT && reload_completed"
  [(parallel
    [(set (reg:CC CC_REGNUM)
	  (compare:CC (const_int 1) (match_dup 1)))
     (set (match_dup 0)
	  (minus:SI (const_int 1) (match_dup 1)))])
   (cond_exec (ltu:CC (reg:CC CC_REGNUM) (const_int 0))
	      (set (match_dup 0) (const_int 0)))])

(define_split
  [(set (match_operand:SI 0 "s_register_operand" "")
	(ne:SI (match_operand:SI 1 "s_register_operand" "")
	       (match_operand:SI 2 "const_int_operand" "")))
   (clobber (reg:CC CC_REGNUM))]
  "TARGET_32BIT && reload_completed"
  [(parallel
    [(set (reg:CC CC_REGNUM)
	  (compare:CC (match_dup 1) (match_dup 2)))
     (set (match_dup 0) (plus:SI (match_dup 1) (match_dup 3)))])
   (cond_exec (ne:CC (reg:CC CC_REGNUM) (const_int 0))
	      (set (match_dup 0) (const_int 1)))]
{
  operands[3] = GEN_INT (-INTVAL (operands[2]));
})

(define_split
  [(set (match_operand:SI 0 "s_register_operand" "")
	(ne:SI (match_operand:SI 1 "s_register_operand" "")
	       (match_operand:SI 2 "arm_add_operand" "")))
   (clobber (reg:CC CC_REGNUM))]
  "TARGET_32BIT && reload_completed"
  [(parallel
    [(set (reg:CC_NOOV CC_REGNUM)
	  (compare:CC_NOOV (minus:SI (match_dup 1) (match_dup 2))
			   (const_int 0)))
     (set (match_dup 0) (minus:SI (match_dup 1) (match_dup 2)))])
   (cond_exec (ne:CC_NOOV (reg:CC_NOOV CC_REGNUM) (const_int 0))
	      (set (match_dup 0) (const_int 1)))])

(define_insn_and_split "*compare_scc"
  [(set (match_operand:SI 0 "s_register_operand" "=Ts,Ts")
	(match_operator:SI 1 "arm_comparison_operator"
	 [(match_operand:SI 2 "s_register_operand" "r,r")
	  (match_operand:SI 3 "arm_add_operand" "rI,L")]))
   (clobber (reg:CC CC_REGNUM))]
  "TARGET_32BIT"
  "#"
  "&& reload_completed"
  [(set (reg:CC CC_REGNUM) (compare:CC (match_dup 2) (match_dup 3)))
   (cond_exec (match_dup 4) (set (match_dup 0) (const_int 0)))
   (cond_exec (match_dup 5) (set (match_dup 0) (const_int 1)))]
{
  rtx tmp1;
  machine_mode mode = SELECT_CC_MODE (GET_CODE (operands[1]),
					   operands[2], operands[3]);
  enum rtx_code rc = GET_CODE (operands[1]);

  tmp1 = gen_rtx_REG (mode, CC_REGNUM);

  operands[5] = gen_rtx_fmt_ee (rc, VOIDmode, tmp1, const0_rtx);
  if (mode == CCFPmode || mode == CCFPEmode)
    rc = reverse_condition_maybe_unordered (rc);
  else
    rc = reverse_condition (rc);
  operands[4] = gen_rtx_fmt_ee (rc, VOIDmode, tmp1, const0_rtx);
}
  [(set_attr "type" "multiple")]
)

;; Attempt to improve the sequence generated by the compare_scc splitters
;; not to use conditional execution.

;; Rd = (eq (reg1) (const_int0))  // ARMv5
;;	clz Rd, reg1
;;	lsr Rd, Rd, #5
(define_peephole2
  [(set (reg:CC CC_REGNUM)
	(compare:CC (match_operand:SI 1 "register_operand" "")
		    (const_int 0)))
   (cond_exec (ne (reg:CC CC_REGNUM) (const_int 0))
	      (set (match_operand:SI 0 "register_operand" "") (const_int 0)))
   (cond_exec (eq (reg:CC CC_REGNUM) (const_int 0))
	      (set (match_dup 0) (const_int 1)))]
  "arm_arch5 && TARGET_32BIT && peep2_regno_dead_p (3, CC_REGNUM)"
  [(set (match_dup 0) (clz:SI (match_dup 1)))
   (set (match_dup 0) (lshiftrt:SI (match_dup 0) (const_int 5)))]
)

;; Rd = (eq (reg1) (const_int0))  // !ARMv5
;;	negs Rd, reg1
;;	adc  Rd, Rd, reg1
(define_peephole2
  [(set (reg:CC CC_REGNUM)
	(compare:CC (match_operand:SI 1 "register_operand" "")
		    (const_int 0)))
   (cond_exec (ne (reg:CC CC_REGNUM) (const_int 0))
	      (set (match_operand:SI 0 "register_operand" "") (const_int 0)))
   (cond_exec (eq (reg:CC CC_REGNUM) (const_int 0))
	      (set (match_dup 0) (const_int 1)))
   (match_scratch:SI 2 "r")]
  "TARGET_32BIT && peep2_regno_dead_p (3, CC_REGNUM)"
  [(parallel
    [(set (reg:CC CC_REGNUM)
	  (compare:CC (const_int 0) (match_dup 1)))
     (set (match_dup 2) (minus:SI (const_int 0) (match_dup 1)))])
   (set (match_dup 0)
	(plus:SI (plus:SI (match_dup 1) (match_dup 2))
		 (geu:SI (reg:CC CC_REGNUM) (const_int 0))))]
)

;; Rd = (eq (reg1) (reg2/imm))	// ARMv5 and optimising for speed.
;;	sub  Rd, Reg1, reg2
;;	clz  Rd, Rd
;;	lsr  Rd, Rd, #5
(define_peephole2
  [(set (reg:CC CC_REGNUM)
	(compare:CC (match_operand:SI 1 "register_operand" "")
		    (match_operand:SI 2 "arm_rhs_operand" "")))
   (cond_exec (ne (reg:CC CC_REGNUM) (const_int 0))
	      (set (match_operand:SI 0 "register_operand" "") (const_int 0)))
   (cond_exec (eq (reg:CC CC_REGNUM) (const_int 0))
	      (set (match_dup 0) (const_int 1)))]
  "arm_arch5 && TARGET_32BIT && peep2_regno_dead_p (3, CC_REGNUM)
  && !(TARGET_THUMB2 && optimize_insn_for_size_p ())"
  [(set (match_dup 0) (minus:SI (match_dup 1) (match_dup 2)))
   (set (match_dup 0) (clz:SI (match_dup 0)))
   (set (match_dup 0) (lshiftrt:SI (match_dup 0) (const_int 5)))]
)


;; Rd = (eq (reg1) (reg2))	// ! ARMv5 or optimising for size.
;;	sub  T1, Reg1, reg2
;;	negs Rd, T1
;;	adc  Rd, Rd, T1
(define_peephole2
  [(set (reg:CC CC_REGNUM)
	(compare:CC (match_operand:SI 1 "register_operand" "")
		    (match_operand:SI 2 "arm_rhs_operand" "")))
   (cond_exec (ne (reg:CC CC_REGNUM) (const_int 0))
	      (set (match_operand:SI 0 "register_operand" "") (const_int 0)))
   (cond_exec (eq (reg:CC CC_REGNUM) (const_int 0))
	      (set (match_dup 0) (const_int 1)))
   (match_scratch:SI 3 "r")]
  "TARGET_32BIT && peep2_regno_dead_p (3, CC_REGNUM)"
  [(set (match_dup 3) (match_dup 4))
   (parallel
    [(set (reg:CC CC_REGNUM)
	  (compare:CC (const_int 0) (match_dup 3)))
     (set (match_dup 0) (minus:SI (const_int 0) (match_dup 3)))])
   (set (match_dup 0)
	(plus:SI (plus:SI (match_dup 0) (match_dup 3))
		 (geu:SI (reg:CC CC_REGNUM) (const_int 0))))]
  "
  if (CONST_INT_P (operands[2]))
    operands[4] = plus_constant (SImode, operands[1], -INTVAL (operands[2]));
  else
    operands[4] = gen_rtx_MINUS (SImode, operands[1], operands[2]);
  ")

(define_insn "*cond_move"
  [(set (match_operand:SI 0 "s_register_operand" "=r,r,r")
	(if_then_else:SI (match_operator 3 "equality_operator"
			  [(match_operator 4 "arm_comparison_operator"
			    [(match_operand 5 "cc_register" "") (const_int 0)])
			   (const_int 0)])
			 (match_operand:SI 1 "arm_rhs_operand" "0,rI,?rI")
			 (match_operand:SI 2 "arm_rhs_operand" "rI,0,rI")))]
  "TARGET_ARM"
  "*
    if (GET_CODE (operands[3]) == NE)
      {
        if (which_alternative != 1)
	  output_asm_insn (\"mov%D4\\t%0, %2\", operands);
        if (which_alternative != 0)
	  output_asm_insn (\"mov%d4\\t%0, %1\", operands);
        return \"\";
      }
    if (which_alternative != 0)
      output_asm_insn (\"mov%D4\\t%0, %1\", operands);
    if (which_alternative != 1)
      output_asm_insn (\"mov%d4\\t%0, %2\", operands);
    return \"\";
  "
  [(set_attr "conds" "use")
   (set_attr_alternative "type"
                         [(if_then_else (match_operand 2 "const_int_operand" "")
                                        (const_string "mov_imm")
                                        (const_string "mov_reg"))
                          (if_then_else (match_operand 1 "const_int_operand" "")
                                        (const_string "mov_imm")
                                        (const_string "mov_reg"))
                          (const_string "multiple")])
   (set_attr "length" "4,4,8")]
)

(define_insn "*cond_arith"
  [(set (match_operand:SI 0 "s_register_operand" "=r,r")
        (match_operator:SI 5 "shiftable_operator" 
	 [(match_operator:SI 4 "arm_comparison_operator"
           [(match_operand:SI 2 "s_register_operand" "r,r")
	    (match_operand:SI 3 "arm_rhs_operand" "rI,rI")])
          (match_operand:SI 1 "s_register_operand" "0,?r")]))
   (clobber (reg:CC CC_REGNUM))]
  "TARGET_ARM"
  "*
    if (GET_CODE (operands[4]) == LT && operands[3] == const0_rtx)
      return \"%i5\\t%0, %1, %2, lsr #31\";

    output_asm_insn (\"cmp\\t%2, %3\", operands);
    if (GET_CODE (operands[5]) == AND)
      output_asm_insn (\"mov%D4\\t%0, #0\", operands);
    else if (GET_CODE (operands[5]) == MINUS)
      output_asm_insn (\"rsb%D4\\t%0, %1, #0\", operands);
    else if (which_alternative != 0)
      output_asm_insn (\"mov%D4\\t%0, %1\", operands);
    return \"%i5%d4\\t%0, %1, #1\";
  "
  [(set_attr "conds" "clob")
   (set_attr "length" "12")
   (set_attr "type" "multiple")]
)

(define_insn "*cond_sub"
  [(set (match_operand:SI 0 "s_register_operand" "=r,r")
        (minus:SI (match_operand:SI 1 "s_register_operand" "0,?r")
		  (match_operator:SI 4 "arm_comparison_operator"
                   [(match_operand:SI 2 "s_register_operand" "r,r")
		    (match_operand:SI 3 "arm_rhs_operand" "rI,rI")])))
   (clobber (reg:CC CC_REGNUM))]
  "TARGET_ARM"
  "*
    output_asm_insn (\"cmp\\t%2, %3\", operands);
    if (which_alternative != 0)
      output_asm_insn (\"mov%D4\\t%0, %1\", operands);
    return \"sub%d4\\t%0, %1, #1\";
  "
  [(set_attr "conds" "clob")
   (set_attr "length" "8,12")
   (set_attr "type" "multiple")]
)

(define_insn "*cmp_ite0"
  [(set (match_operand 6 "dominant_cc_register" "")
	(compare
	 (if_then_else:SI
	  (match_operator 4 "arm_comparison_operator"
	   [(match_operand:SI 0 "s_register_operand"
	        "l,l,l,r,r,r,r,r,r")
	    (match_operand:SI 1 "arm_add_operand"
	        "lPy,lPy,lPy,rI,L,rI,L,rI,L")])
	  (match_operator:SI 5 "arm_comparison_operator"
	   [(match_operand:SI 2 "s_register_operand"
	        "l,r,r,l,l,r,r,r,r")
	    (match_operand:SI 3 "arm_add_operand"
	        "lPy,rI,L,lPy,lPy,rI,rI,L,L")])
	  (const_int 0))
	 (const_int 0)))]
  "TARGET_32BIT"
  "*
  {
    static const char * const cmp1[NUM_OF_COND_CMP][2] =
    {
      {\"cmp%d5\\t%0, %1\",
       \"cmp%d4\\t%2, %3\"},
      {\"cmn%d5\\t%0, #%n1\",
       \"cmp%d4\\t%2, %3\"},
      {\"cmp%d5\\t%0, %1\",
       \"cmn%d4\\t%2, #%n3\"},
      {\"cmn%d5\\t%0, #%n1\",
       \"cmn%d4\\t%2, #%n3\"}
    };
    static const char * const cmp2[NUM_OF_COND_CMP][2] =
    {
      {\"cmp\\t%2, %3\",
       \"cmp\\t%0, %1\"},
      {\"cmp\\t%2, %3\",
       \"cmn\\t%0, #%n1\"},
      {\"cmn\\t%2, #%n3\",
       \"cmp\\t%0, %1\"},
      {\"cmn\\t%2, #%n3\",
       \"cmn\\t%0, #%n1\"}
    };
    static const char * const ite[2] =
    {
      \"it\\t%d5\",
      \"it\\t%d4\"
    };
    static const int cmp_idx[9] = {CMP_CMP, CMP_CMP, CMP_CMN,
                                   CMP_CMP, CMN_CMP, CMP_CMP,
                                   CMN_CMP, CMP_CMN, CMN_CMN};
    int swap =
      comparison_dominates_p (GET_CODE (operands[5]), GET_CODE (operands[4]));

    output_asm_insn (cmp2[cmp_idx[which_alternative]][swap], operands);
    if (TARGET_THUMB2) {
      output_asm_insn (ite[swap], operands);
    }
    output_asm_insn (cmp1[cmp_idx[which_alternative]][swap], operands);
    return \"\";
  }"
  [(set_attr "conds" "set")
   (set_attr "arch" "t2,t2,t2,t2,t2,any,any,any,any")
   (set_attr "type" "multiple")
   (set_attr_alternative "length"
      [(const_int 6)
       (const_int 8)
       (const_int 8)
       (const_int 8)
       (const_int 8)
       (if_then_else (eq_attr "is_thumb" "no")
           (const_int 8)
           (const_int 10))
       (if_then_else (eq_attr "is_thumb" "no")
           (const_int 8)
           (const_int 10))
       (if_then_else (eq_attr "is_thumb" "no")
           (const_int 8)
           (const_int 10))
       (if_then_else (eq_attr "is_thumb" "no")
           (const_int 8)
           (const_int 10))])]
)

(define_insn "*cmp_ite1"
  [(set (match_operand 6 "dominant_cc_register" "")
	(compare
	 (if_then_else:SI
	  (match_operator 4 "arm_comparison_operator"
	   [(match_operand:SI 0 "s_register_operand"
	        "l,l,l,r,r,r,r,r,r")
	    (match_operand:SI 1 "arm_add_operand"
	        "lPy,lPy,lPy,rI,L,rI,L,rI,L")])
	  (match_operator:SI 5 "arm_comparison_operator"
	   [(match_operand:SI 2 "s_register_operand"
	        "l,r,r,l,l,r,r,r,r")
	    (match_operand:SI 3 "arm_add_operand"
	        "lPy,rI,L,lPy,lPy,rI,rI,L,L")])
	  (const_int 1))
	 (const_int 0)))]
  "TARGET_32BIT"
  "*
  {
    static const char * const cmp1[NUM_OF_COND_CMP][2] =
    {
      {\"cmp\\t%0, %1\",
       \"cmp\\t%2, %3\"},
      {\"cmn\\t%0, #%n1\",
       \"cmp\\t%2, %3\"},
      {\"cmp\\t%0, %1\",
       \"cmn\\t%2, #%n3\"},
      {\"cmn\\t%0, #%n1\",
       \"cmn\\t%2, #%n3\"}
    };
    static const char * const cmp2[NUM_OF_COND_CMP][2] =
    {
      {\"cmp%d4\\t%2, %3\",
       \"cmp%D5\\t%0, %1\"},
      {\"cmp%d4\\t%2, %3\",
       \"cmn%D5\\t%0, #%n1\"},
      {\"cmn%d4\\t%2, #%n3\",
       \"cmp%D5\\t%0, %1\"},
      {\"cmn%d4\\t%2, #%n3\",
       \"cmn%D5\\t%0, #%n1\"}
    };
    static const char * const ite[2] =
    {
      \"it\\t%d4\",
      \"it\\t%D5\"
    };
    static const int cmp_idx[9] = {CMP_CMP, CMP_CMP, CMP_CMN,
                                   CMP_CMP, CMN_CMP, CMP_CMP,
                                   CMN_CMP, CMP_CMN, CMN_CMN};
    int swap =
      comparison_dominates_p (GET_CODE (operands[5]),
			      reverse_condition (GET_CODE (operands[4])));

    output_asm_insn (cmp1[cmp_idx[which_alternative]][swap], operands);
    if (TARGET_THUMB2) {
      output_asm_insn (ite[swap], operands);
    }
    output_asm_insn (cmp2[cmp_idx[which_alternative]][swap], operands);
    return \"\";
  }"
  [(set_attr "conds" "set")
   (set_attr "arch" "t2,t2,t2,t2,t2,any,any,any,any")
   (set_attr_alternative "length"
      [(const_int 6)
       (const_int 8)
       (const_int 8)
       (const_int 8)
       (const_int 8)
       (if_then_else (eq_attr "is_thumb" "no")
           (const_int 8)
           (const_int 10))
       (if_then_else (eq_attr "is_thumb" "no")
           (const_int 8)
           (const_int 10))
       (if_then_else (eq_attr "is_thumb" "no")
           (const_int 8)
           (const_int 10))
       (if_then_else (eq_attr "is_thumb" "no")
           (const_int 8)
           (const_int 10))])
   (set_attr "type" "multiple")]
)

(define_insn "*cmp_and"
  [(set (match_operand 6 "dominant_cc_register" "")
	(compare
	 (and:SI
	  (match_operator 4 "arm_comparison_operator"
	   [(match_operand:SI 0 "s_register_operand" 
	        "l,l,l,r,r,r,r,r,r")
	    (match_operand:SI 1 "arm_add_operand" 
	        "lPy,lPy,lPy,rI,L,rI,L,rI,L")])
	  (match_operator:SI 5 "arm_comparison_operator"
	   [(match_operand:SI 2 "s_register_operand" 
	        "l,r,r,l,l,r,r,r,r")
	    (match_operand:SI 3 "arm_add_operand" 
	        "lPy,rI,L,lPy,lPy,rI,rI,L,L")]))
	 (const_int 0)))]
  "TARGET_32BIT"
  "*
  {
    static const char *const cmp1[NUM_OF_COND_CMP][2] =
    {
      {\"cmp%d5\\t%0, %1\",
       \"cmp%d4\\t%2, %3\"},
      {\"cmn%d5\\t%0, #%n1\",
       \"cmp%d4\\t%2, %3\"},
      {\"cmp%d5\\t%0, %1\",
       \"cmn%d4\\t%2, #%n3\"},
      {\"cmn%d5\\t%0, #%n1\",
       \"cmn%d4\\t%2, #%n3\"}
    };
    static const char *const cmp2[NUM_OF_COND_CMP][2] =
    {
      {\"cmp\\t%2, %3\",
       \"cmp\\t%0, %1\"},
      {\"cmp\\t%2, %3\",
       \"cmn\\t%0, #%n1\"},
      {\"cmn\\t%2, #%n3\",
       \"cmp\\t%0, %1\"},
      {\"cmn\\t%2, #%n3\",
       \"cmn\\t%0, #%n1\"}
    };
    static const char *const ite[2] =
    {
      \"it\\t%d5\",
      \"it\\t%d4\"
    };
    static const int cmp_idx[9] = {CMP_CMP, CMP_CMP, CMP_CMN,
                                   CMP_CMP, CMN_CMP, CMP_CMP,
                                   CMN_CMP, CMP_CMN, CMN_CMN};
    int swap =
      comparison_dominates_p (GET_CODE (operands[5]), GET_CODE (operands[4]));

    output_asm_insn (cmp2[cmp_idx[which_alternative]][swap], operands);
    if (TARGET_THUMB2) {
      output_asm_insn (ite[swap], operands);
    }
    output_asm_insn (cmp1[cmp_idx[which_alternative]][swap], operands);
    return \"\";
  }"
  [(set_attr "conds" "set")
   (set_attr "predicable" "no")
   (set_attr "arch" "t2,t2,t2,t2,t2,any,any,any,any")
   (set_attr_alternative "length"
      [(const_int 6)
       (const_int 8)
       (const_int 8)
       (const_int 8)
       (const_int 8)
       (if_then_else (eq_attr "is_thumb" "no")
           (const_int 8)
           (const_int 10))
       (if_then_else (eq_attr "is_thumb" "no")
           (const_int 8)
           (const_int 10))
       (if_then_else (eq_attr "is_thumb" "no")
           (const_int 8)
           (const_int 10))
       (if_then_else (eq_attr "is_thumb" "no")
           (const_int 8)
           (const_int 10))])
   (set_attr "type" "multiple")]
)

(define_insn "*cmp_ior"
  [(set (match_operand 6 "dominant_cc_register" "")
	(compare
	 (ior:SI
	  (match_operator 4 "arm_comparison_operator"
	   [(match_operand:SI 0 "s_register_operand"
	        "l,l,l,r,r,r,r,r,r")
	    (match_operand:SI 1 "arm_add_operand"
	        "lPy,lPy,lPy,rI,L,rI,L,rI,L")])
	  (match_operator:SI 5 "arm_comparison_operator"
	   [(match_operand:SI 2 "s_register_operand"
	        "l,r,r,l,l,r,r,r,r")
	    (match_operand:SI 3 "arm_add_operand"
	        "lPy,rI,L,lPy,lPy,rI,rI,L,L")]))
	 (const_int 0)))]
  "TARGET_32BIT"
  "*
  {
    static const char *const cmp1[NUM_OF_COND_CMP][2] =
    {
      {\"cmp\\t%0, %1\",
       \"cmp\\t%2, %3\"},
      {\"cmn\\t%0, #%n1\",
       \"cmp\\t%2, %3\"},
      {\"cmp\\t%0, %1\",
       \"cmn\\t%2, #%n3\"},
      {\"cmn\\t%0, #%n1\",
       \"cmn\\t%2, #%n3\"}
    };
    static const char *const cmp2[NUM_OF_COND_CMP][2] =
    {
      {\"cmp%D4\\t%2, %3\",
       \"cmp%D5\\t%0, %1\"},
      {\"cmp%D4\\t%2, %3\",
       \"cmn%D5\\t%0, #%n1\"},
      {\"cmn%D4\\t%2, #%n3\",
       \"cmp%D5\\t%0, %1\"},
      {\"cmn%D4\\t%2, #%n3\",
       \"cmn%D5\\t%0, #%n1\"}
    };
    static const char *const ite[2] =
    {
      \"it\\t%D4\",
      \"it\\t%D5\"
    };
    static const int cmp_idx[9] = {CMP_CMP, CMP_CMP, CMP_CMN,
                                   CMP_CMP, CMN_CMP, CMP_CMP,
                                   CMN_CMP, CMP_CMN, CMN_CMN};
    int swap =
      comparison_dominates_p (GET_CODE (operands[5]), GET_CODE (operands[4]));

    output_asm_insn (cmp1[cmp_idx[which_alternative]][swap], operands);
    if (TARGET_THUMB2) {
      output_asm_insn (ite[swap], operands);
    }
    output_asm_insn (cmp2[cmp_idx[which_alternative]][swap], operands);
    return \"\";
  }
  "
  [(set_attr "conds" "set")
   (set_attr "arch" "t2,t2,t2,t2,t2,any,any,any,any")
   (set_attr_alternative "length"
      [(const_int 6)
       (const_int 8)
       (const_int 8)
       (const_int 8)
       (const_int 8)
       (if_then_else (eq_attr "is_thumb" "no")
           (const_int 8)
           (const_int 10))
       (if_then_else (eq_attr "is_thumb" "no")
           (const_int 8)
           (const_int 10))
       (if_then_else (eq_attr "is_thumb" "no")
           (const_int 8)
           (const_int 10))
       (if_then_else (eq_attr "is_thumb" "no")
           (const_int 8)
           (const_int 10))])
   (set_attr "type" "multiple")]
)

(define_insn_and_split "*ior_scc_scc"
  [(set (match_operand:SI 0 "s_register_operand" "=Ts")
	(ior:SI (match_operator:SI 3 "arm_comparison_operator"
		 [(match_operand:SI 1 "s_register_operand" "r")
		  (match_operand:SI 2 "arm_add_operand" "rIL")])
		(match_operator:SI 6 "arm_comparison_operator"
		 [(match_operand:SI 4 "s_register_operand" "r")
		  (match_operand:SI 5 "arm_add_operand" "rIL")])))
   (clobber (reg:CC CC_REGNUM))]
  "TARGET_32BIT
   && (arm_select_dominance_cc_mode (operands[3], operands[6], DOM_CC_X_OR_Y)
       != CCmode)"
  "#"
  "TARGET_32BIT && reload_completed"
  [(set (match_dup 7)
	(compare
	 (ior:SI
	  (match_op_dup 3 [(match_dup 1) (match_dup 2)])
	  (match_op_dup 6 [(match_dup 4) (match_dup 5)]))
	 (const_int 0)))
   (set (match_dup 0) (ne:SI (match_dup 7) (const_int 0)))]
  "operands[7]
     = gen_rtx_REG (arm_select_dominance_cc_mode (operands[3], operands[6],
						  DOM_CC_X_OR_Y),
		    CC_REGNUM);"
  [(set_attr "conds" "clob")
   (set_attr "length" "16")
   (set_attr "type" "multiple")]
)

; If the above pattern is followed by a CMP insn, then the compare is 
; redundant, since we can rework the conditional instruction that follows.
(define_insn_and_split "*ior_scc_scc_cmp"
  [(set (match_operand 0 "dominant_cc_register" "")
	(compare (ior:SI (match_operator:SI 3 "arm_comparison_operator"
			  [(match_operand:SI 1 "s_register_operand" "r")
			   (match_operand:SI 2 "arm_add_operand" "rIL")])
			 (match_operator:SI 6 "arm_comparison_operator"
			  [(match_operand:SI 4 "s_register_operand" "r")
			   (match_operand:SI 5 "arm_add_operand" "rIL")]))
		 (const_int 0)))
   (set (match_operand:SI 7 "s_register_operand" "=Ts")
	(ior:SI (match_op_dup 3 [(match_dup 1) (match_dup 2)])
		(match_op_dup 6 [(match_dup 4) (match_dup 5)])))]
  "TARGET_32BIT"
  "#"
  "TARGET_32BIT && reload_completed"
  [(set (match_dup 0)
	(compare
	 (ior:SI
	  (match_op_dup 3 [(match_dup 1) (match_dup 2)])
	  (match_op_dup 6 [(match_dup 4) (match_dup 5)]))
	 (const_int 0)))
   (set (match_dup 7) (ne:SI (match_dup 0) (const_int 0)))]
  ""
  [(set_attr "conds" "set")
   (set_attr "length" "16")
   (set_attr "type" "multiple")]
)

(define_insn_and_split "*and_scc_scc"
  [(set (match_operand:SI 0 "s_register_operand" "=Ts")
	(and:SI (match_operator:SI 3 "arm_comparison_operator"
		 [(match_operand:SI 1 "s_register_operand" "r")
		  (match_operand:SI 2 "arm_add_operand" "rIL")])
		(match_operator:SI 6 "arm_comparison_operator"
		 [(match_operand:SI 4 "s_register_operand" "r")
		  (match_operand:SI 5 "arm_add_operand" "rIL")])))
   (clobber (reg:CC CC_REGNUM))]
  "TARGET_32BIT
   && (arm_select_dominance_cc_mode (operands[3], operands[6], DOM_CC_X_AND_Y)
       != CCmode)"
  "#"
  "TARGET_32BIT && reload_completed
   && (arm_select_dominance_cc_mode (operands[3], operands[6], DOM_CC_X_AND_Y)
       != CCmode)"
  [(set (match_dup 7)
	(compare
	 (and:SI
	  (match_op_dup 3 [(match_dup 1) (match_dup 2)])
	  (match_op_dup 6 [(match_dup 4) (match_dup 5)]))
	 (const_int 0)))
   (set (match_dup 0) (ne:SI (match_dup 7) (const_int 0)))]
  "operands[7]
     = gen_rtx_REG (arm_select_dominance_cc_mode (operands[3], operands[6],
						  DOM_CC_X_AND_Y),
		    CC_REGNUM);"
  [(set_attr "conds" "clob")
   (set_attr "length" "16")
   (set_attr "type" "multiple")]
)

; If the above pattern is followed by a CMP insn, then the compare is 
; redundant, since we can rework the conditional instruction that follows.
(define_insn_and_split "*and_scc_scc_cmp"
  [(set (match_operand 0 "dominant_cc_register" "")
	(compare (and:SI (match_operator:SI 3 "arm_comparison_operator"
			  [(match_operand:SI 1 "s_register_operand" "r")
			   (match_operand:SI 2 "arm_add_operand" "rIL")])
			 (match_operator:SI 6 "arm_comparison_operator"
			  [(match_operand:SI 4 "s_register_operand" "r")
			   (match_operand:SI 5 "arm_add_operand" "rIL")]))
		 (const_int 0)))
   (set (match_operand:SI 7 "s_register_operand" "=Ts")
	(and:SI (match_op_dup 3 [(match_dup 1) (match_dup 2)])
		(match_op_dup 6 [(match_dup 4) (match_dup 5)])))]
  "TARGET_32BIT"
  "#"
  "TARGET_32BIT && reload_completed"
  [(set (match_dup 0)
	(compare
	 (and:SI
	  (match_op_dup 3 [(match_dup 1) (match_dup 2)])
	  (match_op_dup 6 [(match_dup 4) (match_dup 5)]))
	 (const_int 0)))
   (set (match_dup 7) (ne:SI (match_dup 0) (const_int 0)))]
  ""
  [(set_attr "conds" "set")
   (set_attr "length" "16")
   (set_attr "type" "multiple")]
)

;; If there is no dominance in the comparison, then we can still save an
;; instruction in the AND case, since we can know that the second compare
;; need only zero the value if false (if true, then the value is already
;; correct).
(define_insn_and_split "*and_scc_scc_nodom"
  [(set (match_operand:SI 0 "s_register_operand" "=&Ts,&Ts,&Ts")
	(and:SI (match_operator:SI 3 "arm_comparison_operator"
		 [(match_operand:SI 1 "s_register_operand" "r,r,0")
		  (match_operand:SI 2 "arm_add_operand" "rIL,0,rIL")])
		(match_operator:SI 6 "arm_comparison_operator"
		 [(match_operand:SI 4 "s_register_operand" "r,r,r")
		  (match_operand:SI 5 "arm_add_operand" "rIL,rIL,rIL")])))
   (clobber (reg:CC CC_REGNUM))]
  "TARGET_32BIT
   && (arm_select_dominance_cc_mode (operands[3], operands[6], DOM_CC_X_AND_Y)
       == CCmode)"
  "#"
  "TARGET_32BIT && reload_completed"
  [(parallel [(set (match_dup 0)
		   (match_op_dup 3 [(match_dup 1) (match_dup 2)]))
	      (clobber (reg:CC CC_REGNUM))])
   (set (match_dup 7) (match_op_dup 8 [(match_dup 4) (match_dup 5)]))
   (set (match_dup 0)
	(if_then_else:SI (match_op_dup 6 [(match_dup 7) (const_int 0)])
			 (match_dup 0)
			 (const_int 0)))]
  "operands[7] = gen_rtx_REG (SELECT_CC_MODE (GET_CODE (operands[6]),
					      operands[4], operands[5]),
			      CC_REGNUM);
   operands[8] = gen_rtx_COMPARE (GET_MODE (operands[7]), operands[4],
				  operands[5]);"
  [(set_attr "conds" "clob")
   (set_attr "length" "20")
   (set_attr "type" "multiple")]
)

(define_split
  [(set (reg:CC_NOOV CC_REGNUM)
	(compare:CC_NOOV (ior:SI
			  (and:SI (match_operand:SI 0 "s_register_operand" "")
				  (const_int 1))
			  (match_operator:SI 1 "arm_comparison_operator"
			   [(match_operand:SI 2 "s_register_operand" "")
			    (match_operand:SI 3 "arm_add_operand" "")]))
			 (const_int 0)))
   (clobber (match_operand:SI 4 "s_register_operand" ""))]
  "TARGET_ARM"
  [(set (match_dup 4)
	(ior:SI (match_op_dup 1 [(match_dup 2) (match_dup 3)])
		(match_dup 0)))
   (set (reg:CC_NOOV CC_REGNUM)
	(compare:CC_NOOV (and:SI (match_dup 4) (const_int 1))
			 (const_int 0)))]
  "")

(define_split
  [(set (reg:CC_NOOV CC_REGNUM)
	(compare:CC_NOOV (ior:SI
			  (match_operator:SI 1 "arm_comparison_operator"
			   [(match_operand:SI 2 "s_register_operand" "")
			    (match_operand:SI 3 "arm_add_operand" "")])
			  (and:SI (match_operand:SI 0 "s_register_operand" "")
				  (const_int 1)))
			 (const_int 0)))
   (clobber (match_operand:SI 4 "s_register_operand" ""))]
  "TARGET_ARM"
  [(set (match_dup 4)
	(ior:SI (match_op_dup 1 [(match_dup 2) (match_dup 3)])
		(match_dup 0)))
   (set (reg:CC_NOOV CC_REGNUM)
	(compare:CC_NOOV (and:SI (match_dup 4) (const_int 1))
			 (const_int 0)))]
  "")
;; ??? The conditional patterns above need checking for Thumb-2 usefulness

(define_insn_and_split "*negscc"
  [(set (match_operand:SI 0 "s_register_operand" "=r")
	(neg:SI (match_operator 3 "arm_comparison_operator"
		 [(match_operand:SI 1 "s_register_operand" "r")
		  (match_operand:SI 2 "arm_rhs_operand" "rI")])))
   (clobber (reg:CC CC_REGNUM))]
  "TARGET_ARM"
  "#"
  "&& reload_completed"
  [(const_int 0)]
  {
    rtx cc_reg = gen_rtx_REG (CCmode, CC_REGNUM);

    if (GET_CODE (operands[3]) == LT && operands[2] == const0_rtx)
       {
         /* Emit mov\\t%0, %1, asr #31 */
         emit_insn (gen_rtx_SET (operands[0],
                                 gen_rtx_ASHIFTRT (SImode,
                                                   operands[1],
                                                   GEN_INT (31))));
         DONE;
       }
     else if (GET_CODE (operands[3]) == NE)
       {
        /* Emit subs\\t%0, %1, %2\;mvnne\\t%0, #0 */
        if (CONST_INT_P (operands[2]))
          emit_insn (gen_cmpsi2_addneg (operands[0], operands[1], operands[2],
                                        GEN_INT (- INTVAL (operands[2]))));
        else
          emit_insn (gen_subsi3_compare (operands[0], operands[1], operands[2]));

        emit_insn (gen_rtx_COND_EXEC (VOIDmode,
                                      gen_rtx_NE (SImode,
                                                  cc_reg,
                                                  const0_rtx),
                                      gen_rtx_SET (operands[0],
                                                   GEN_INT (~0))));
        DONE;
      }
    else
      {
        /* Emit: cmp\\t%1, %2\;mov%D3\\t%0, #0\;mvn%d3\\t%0, #0 */
        emit_insn (gen_rtx_SET (cc_reg,
                                gen_rtx_COMPARE (CCmode, operands[1], operands[2])));
        enum rtx_code rc = GET_CODE (operands[3]);

        rc = reverse_condition (rc);
        emit_insn (gen_rtx_COND_EXEC (VOIDmode,
                                      gen_rtx_fmt_ee (rc,
                                                      VOIDmode,
                                                      cc_reg,
                                                      const0_rtx),
                                      gen_rtx_SET (operands[0], const0_rtx)));
        rc = GET_CODE (operands[3]);
        emit_insn (gen_rtx_COND_EXEC (VOIDmode,
                                      gen_rtx_fmt_ee (rc,
                                                      VOIDmode,
                                                      cc_reg,
                                                      const0_rtx),
                                      gen_rtx_SET (operands[0],
                                                   GEN_INT (~0))));
        DONE;
      }
     FAIL;
  }
  [(set_attr "conds" "clob")
   (set_attr "length" "12")
   (set_attr "type" "multiple")]
)

(define_insn_and_split "movcond_addsi"
  [(set (match_operand:SI 0 "s_register_operand" "=r,l,r")
	(if_then_else:SI
	 (match_operator 5 "comparison_operator"
	  [(plus:SI (match_operand:SI 3 "s_register_operand" "r,r,r")
	            (match_operand:SI 4 "arm_add_operand" "rIL,rIL,rIL"))
            (const_int 0)])
	 (match_operand:SI 1 "arm_rhs_operand" "rI,rPy,r")
	 (match_operand:SI 2 "arm_rhs_operand" "rI,rPy,r")))
   (clobber (reg:CC CC_REGNUM))]
   "TARGET_32BIT"
   "#"
   "&& reload_completed"
  [(set (reg:CC_NOOV CC_REGNUM)
	(compare:CC_NOOV
	 (plus:SI (match_dup 3)
		  (match_dup 4))
	 (const_int 0)))
   (set (match_dup 0) (match_dup 1))
   (cond_exec (match_dup 6)
	      (set (match_dup 0) (match_dup 2)))]
  "
  {
    machine_mode mode = SELECT_CC_MODE (GET_CODE (operands[5]),
					     operands[3], operands[4]);
    enum rtx_code rc = GET_CODE (operands[5]);
    operands[6] = gen_rtx_REG (mode, CC_REGNUM);
    gcc_assert (!(mode == CCFPmode || mode == CCFPEmode));
    if (!REG_P (operands[2]) || REGNO (operands[2]) != REGNO (operands[0]))
      rc = reverse_condition (rc);
    else
      std::swap (operands[1], operands[2]);

    operands[6] = gen_rtx_fmt_ee (rc, VOIDmode, operands[6], const0_rtx);
  }
  "
  [(set_attr "conds" "clob")
   (set_attr "enabled_for_depr_it" "no,yes,yes")
   (set_attr "type" "multiple")]
)

(define_insn "movcond"
  [(set (match_operand:SI 0 "s_register_operand" "=r,r,r")
	(if_then_else:SI
	 (match_operator 5 "arm_comparison_operator"
	  [(match_operand:SI 3 "s_register_operand" "r,r,r")
	   (match_operand:SI 4 "arm_add_operand" "rIL,rIL,rIL")])
	 (match_operand:SI 1 "arm_rhs_operand" "0,rI,?rI")
	 (match_operand:SI 2 "arm_rhs_operand" "rI,0,rI")))
   (clobber (reg:CC CC_REGNUM))]
  "TARGET_ARM"
  "*
  if (GET_CODE (operands[5]) == LT
      && (operands[4] == const0_rtx))
    {
      if (which_alternative != 1 && REG_P (operands[1]))
	{
	  if (operands[2] == const0_rtx)
	    return \"and\\t%0, %1, %3, asr #31\";
	  return \"ands\\t%0, %1, %3, asr #32\;movcc\\t%0, %2\";
	}
      else if (which_alternative != 0 && REG_P (operands[2]))
	{
	  if (operands[1] == const0_rtx)
	    return \"bic\\t%0, %2, %3, asr #31\";
	  return \"bics\\t%0, %2, %3, asr #32\;movcs\\t%0, %1\";
	}
      /* The only case that falls through to here is when both ops 1 & 2
	 are constants.  */
    }

  if (GET_CODE (operands[5]) == GE
      && (operands[4] == const0_rtx))
    {
      if (which_alternative != 1 && REG_P (operands[1]))
	{
	  if (operands[2] == const0_rtx)
	    return \"bic\\t%0, %1, %3, asr #31\";
	  return \"bics\\t%0, %1, %3, asr #32\;movcs\\t%0, %2\";
	}
      else if (which_alternative != 0 && REG_P (operands[2]))
	{
	  if (operands[1] == const0_rtx)
	    return \"and\\t%0, %2, %3, asr #31\";
	  return \"ands\\t%0, %2, %3, asr #32\;movcc\\t%0, %1\";
	}
      /* The only case that falls through to here is when both ops 1 & 2
	 are constants.  */
    }
  if (CONST_INT_P (operands[4])
      && !const_ok_for_arm (INTVAL (operands[4])))
    output_asm_insn (\"cmn\\t%3, #%n4\", operands);
  else
    output_asm_insn (\"cmp\\t%3, %4\", operands);
  if (which_alternative != 0)
    output_asm_insn (\"mov%d5\\t%0, %1\", operands);
  if (which_alternative != 1)
    output_asm_insn (\"mov%D5\\t%0, %2\", operands);
  return \"\";
  "
  [(set_attr "conds" "clob")
   (set_attr "length" "8,8,12")
   (set_attr "type" "multiple")]
)

;; ??? The patterns below need checking for Thumb-2 usefulness.

(define_insn "*ifcompare_plus_move"
  [(set (match_operand:SI 0 "s_register_operand" "=r,r")
	(if_then_else:SI (match_operator 6 "arm_comparison_operator"
			  [(match_operand:SI 4 "s_register_operand" "r,r")
			   (match_operand:SI 5 "arm_add_operand" "rIL,rIL")])
			 (plus:SI
			  (match_operand:SI 2 "s_register_operand" "r,r")
			  (match_operand:SI 3 "arm_add_operand" "rIL,rIL"))
			 (match_operand:SI 1 "arm_rhs_operand" "0,?rI")))
   (clobber (reg:CC CC_REGNUM))]
  "TARGET_ARM"
  "#"
  [(set_attr "conds" "clob")
   (set_attr "length" "8,12")
   (set_attr "type" "multiple")]
)

(define_insn "*if_plus_move"
  [(set (match_operand:SI 0 "s_register_operand" "=r,r,r,r")
	(if_then_else:SI
	 (match_operator 4 "arm_comparison_operator"
	  [(match_operand 5 "cc_register" "") (const_int 0)])
	 (plus:SI
	  (match_operand:SI 2 "s_register_operand" "r,r,r,r")
	  (match_operand:SI 3 "arm_add_operand" "rI,L,rI,L"))
	 (match_operand:SI 1 "arm_rhs_operand" "0,0,?rI,?rI")))]
  "TARGET_ARM"
  "@
   add%d4\\t%0, %2, %3
   sub%d4\\t%0, %2, #%n3
   add%d4\\t%0, %2, %3\;mov%D4\\t%0, %1
   sub%d4\\t%0, %2, #%n3\;mov%D4\\t%0, %1"
  [(set_attr "conds" "use")
   (set_attr "length" "4,4,8,8")
   (set_attr_alternative "type"
                         [(if_then_else (match_operand 3 "const_int_operand" "")
                                        (const_string "alu_imm" )
                                        (const_string "alu_sreg"))
                          (const_string "alu_imm")
                          (const_string "multiple")
                          (const_string "multiple")])]
)

(define_insn "*ifcompare_move_plus"
  [(set (match_operand:SI 0 "s_register_operand" "=r,r")
	(if_then_else:SI (match_operator 6 "arm_comparison_operator"
			  [(match_operand:SI 4 "s_register_operand" "r,r")
			   (match_operand:SI 5 "arm_add_operand" "rIL,rIL")])
			 (match_operand:SI 1 "arm_rhs_operand" "0,?rI")
			 (plus:SI
			  (match_operand:SI 2 "s_register_operand" "r,r")
			  (match_operand:SI 3 "arm_add_operand" "rIL,rIL"))))
   (clobber (reg:CC CC_REGNUM))]
  "TARGET_ARM"
  "#"
  [(set_attr "conds" "clob")
   (set_attr "length" "8,12")
   (set_attr "type" "multiple")]
)

(define_insn "*if_move_plus"
  [(set (match_operand:SI 0 "s_register_operand" "=r,r,r,r")
	(if_then_else:SI
	 (match_operator 4 "arm_comparison_operator"
	  [(match_operand 5 "cc_register" "") (const_int 0)])
	 (match_operand:SI 1 "arm_rhs_operand" "0,0,?rI,?rI")
	 (plus:SI
	  (match_operand:SI 2 "s_register_operand" "r,r,r,r")
	  (match_operand:SI 3 "arm_add_operand" "rI,L,rI,L"))))]
  "TARGET_ARM"
  "@
   add%D4\\t%0, %2, %3
   sub%D4\\t%0, %2, #%n3
   add%D4\\t%0, %2, %3\;mov%d4\\t%0, %1
   sub%D4\\t%0, %2, #%n3\;mov%d4\\t%0, %1"
  [(set_attr "conds" "use")
   (set_attr "length" "4,4,8,8")
   (set_attr_alternative "type"
                         [(if_then_else (match_operand 3 "const_int_operand" "")
                                        (const_string "alu_imm" )
                                        (const_string "alu_sreg"))
                          (const_string "alu_imm")
                          (const_string "multiple")
                          (const_string "multiple")])]
)

(define_insn "*ifcompare_arith_arith"
  [(set (match_operand:SI 0 "s_register_operand" "=r")
	(if_then_else:SI (match_operator 9 "arm_comparison_operator"
			  [(match_operand:SI 5 "s_register_operand" "r")
			   (match_operand:SI 6 "arm_add_operand" "rIL")])
			 (match_operator:SI 8 "shiftable_operator"
			  [(match_operand:SI 1 "s_register_operand" "r")
			   (match_operand:SI 2 "arm_rhs_operand" "rI")])
			 (match_operator:SI 7 "shiftable_operator"
			  [(match_operand:SI 3 "s_register_operand" "r")
			   (match_operand:SI 4 "arm_rhs_operand" "rI")])))
   (clobber (reg:CC CC_REGNUM))]
  "TARGET_ARM"
  "#"
  [(set_attr "conds" "clob")
   (set_attr "length" "12")
   (set_attr "type" "multiple")]
)

(define_insn "*if_arith_arith"
  [(set (match_operand:SI 0 "s_register_operand" "=r")
	(if_then_else:SI (match_operator 5 "arm_comparison_operator"
			  [(match_operand 8 "cc_register" "") (const_int 0)])
			 (match_operator:SI 6 "shiftable_operator"
			  [(match_operand:SI 1 "s_register_operand" "r")
			   (match_operand:SI 2 "arm_rhs_operand" "rI")])
			 (match_operator:SI 7 "shiftable_operator"
			  [(match_operand:SI 3 "s_register_operand" "r")
			   (match_operand:SI 4 "arm_rhs_operand" "rI")])))]
  "TARGET_ARM"
  "%I6%d5\\t%0, %1, %2\;%I7%D5\\t%0, %3, %4"
  [(set_attr "conds" "use")
   (set_attr "length" "8")
   (set_attr "type" "multiple")]
)

(define_insn "*ifcompare_arith_move"
  [(set (match_operand:SI 0 "s_register_operand" "=r,r")
	(if_then_else:SI (match_operator 6 "arm_comparison_operator"
			  [(match_operand:SI 2 "s_register_operand" "r,r")
			   (match_operand:SI 3 "arm_add_operand" "rIL,rIL")])
			 (match_operator:SI 7 "shiftable_operator"
			  [(match_operand:SI 4 "s_register_operand" "r,r")
			   (match_operand:SI 5 "arm_rhs_operand" "rI,rI")])
			 (match_operand:SI 1 "arm_rhs_operand" "0,?rI")))
   (clobber (reg:CC CC_REGNUM))]
  "TARGET_ARM"
  "*
  /* If we have an operation where (op x 0) is the identity operation and
     the conditional operator is LT or GE and we are comparing against zero and
     everything is in registers then we can do this in two instructions.  */
  if (operands[3] == const0_rtx
      && GET_CODE (operands[7]) != AND
      && REG_P (operands[5])
      && REG_P (operands[1])
      && REGNO (operands[1]) == REGNO (operands[4])
      && REGNO (operands[4]) != REGNO (operands[0]))
    {
      if (GET_CODE (operands[6]) == LT)
	return \"and\\t%0, %5, %2, asr #31\;%I7\\t%0, %4, %0\";
      else if (GET_CODE (operands[6]) == GE)
	return \"bic\\t%0, %5, %2, asr #31\;%I7\\t%0, %4, %0\";
    }
  if (CONST_INT_P (operands[3])
      && !const_ok_for_arm (INTVAL (operands[3])))
    output_asm_insn (\"cmn\\t%2, #%n3\", operands);
  else
    output_asm_insn (\"cmp\\t%2, %3\", operands);
  output_asm_insn (\"%I7%d6\\t%0, %4, %5\", operands);
  if (which_alternative != 0)
    return \"mov%D6\\t%0, %1\";
  return \"\";
  "
  [(set_attr "conds" "clob")
   (set_attr "length" "8,12")
   (set_attr "type" "multiple")]
)

(define_insn "*if_arith_move"
  [(set (match_operand:SI 0 "s_register_operand" "=r,r")
	(if_then_else:SI (match_operator 4 "arm_comparison_operator"
			  [(match_operand 6 "cc_register" "") (const_int 0)])
			 (match_operator:SI 5 "shiftable_operator"
			  [(match_operand:SI 2 "s_register_operand" "r,r")
			   (match_operand:SI 3 "arm_rhs_operand" "rI,rI")])
			 (match_operand:SI 1 "arm_rhs_operand" "0,?rI")))]
  "TARGET_ARM"
  "@
   %I5%d4\\t%0, %2, %3
   %I5%d4\\t%0, %2, %3\;mov%D4\\t%0, %1"
  [(set_attr "conds" "use")
   (set_attr "length" "4,8")
   (set_attr_alternative "type"
                         [(if_then_else (match_operand 3 "const_int_operand" "")
                                        (const_string "alu_shift_imm" )
                                        (const_string "alu_shift_reg"))
                          (const_string "multiple")])]
)

(define_insn "*ifcompare_move_arith"
  [(set (match_operand:SI 0 "s_register_operand" "=r,r")
	(if_then_else:SI (match_operator 6 "arm_comparison_operator"
			  [(match_operand:SI 4 "s_register_operand" "r,r")
			   (match_operand:SI 5 "arm_add_operand" "rIL,rIL")])
			 (match_operand:SI 1 "arm_rhs_operand" "0,?rI")
			 (match_operator:SI 7 "shiftable_operator"
			  [(match_operand:SI 2 "s_register_operand" "r,r")
			   (match_operand:SI 3 "arm_rhs_operand" "rI,rI")])))
   (clobber (reg:CC CC_REGNUM))]
  "TARGET_ARM"
  "*
  /* If we have an operation where (op x 0) is the identity operation and
     the conditional operator is LT or GE and we are comparing against zero and
     everything is in registers then we can do this in two instructions */
  if (operands[5] == const0_rtx
      && GET_CODE (operands[7]) != AND
      && REG_P (operands[3])
      && REG_P (operands[1])
      && REGNO (operands[1]) == REGNO (operands[2])
      && REGNO (operands[2]) != REGNO (operands[0]))
    {
      if (GET_CODE (operands[6]) == GE)
	return \"and\\t%0, %3, %4, asr #31\;%I7\\t%0, %2, %0\";
      else if (GET_CODE (operands[6]) == LT)
	return \"bic\\t%0, %3, %4, asr #31\;%I7\\t%0, %2, %0\";
    }

  if (CONST_INT_P (operands[5])
      && !const_ok_for_arm (INTVAL (operands[5])))
    output_asm_insn (\"cmn\\t%4, #%n5\", operands);
  else
    output_asm_insn (\"cmp\\t%4, %5\", operands);

  if (which_alternative != 0)
    output_asm_insn (\"mov%d6\\t%0, %1\", operands);
  return \"%I7%D6\\t%0, %2, %3\";
  "
  [(set_attr "conds" "clob")
   (set_attr "length" "8,12")
   (set_attr "type" "multiple")]
)

(define_insn "*if_move_arith"
  [(set (match_operand:SI 0 "s_register_operand" "=r,r")
	(if_then_else:SI
	 (match_operator 4 "arm_comparison_operator"
	  [(match_operand 6 "cc_register" "") (const_int 0)])
	 (match_operand:SI 1 "arm_rhs_operand" "0,?rI")
	 (match_operator:SI 5 "shiftable_operator"
	  [(match_operand:SI 2 "s_register_operand" "r,r")
	   (match_operand:SI 3 "arm_rhs_operand" "rI,rI")])))]
  "TARGET_ARM"
  "@
   %I5%D4\\t%0, %2, %3
   %I5%D4\\t%0, %2, %3\;mov%d4\\t%0, %1"
  [(set_attr "conds" "use")
   (set_attr "length" "4,8")
   (set_attr_alternative "type"
                         [(if_then_else (match_operand 3 "const_int_operand" "")
                                        (const_string "alu_shift_imm" )
                                        (const_string "alu_shift_reg"))
                          (const_string "multiple")])]
)

(define_insn "*ifcompare_move_not"
  [(set (match_operand:SI 0 "s_register_operand" "=r,r")
	(if_then_else:SI
	 (match_operator 5 "arm_comparison_operator"
	  [(match_operand:SI 3 "s_register_operand" "r,r")
	   (match_operand:SI 4 "arm_add_operand" "rIL,rIL")])
	 (match_operand:SI 1 "arm_not_operand" "0,?rIK")
	 (not:SI
	  (match_operand:SI 2 "s_register_operand" "r,r"))))
   (clobber (reg:CC CC_REGNUM))]
  "TARGET_ARM"
  "#"
  [(set_attr "conds" "clob")
   (set_attr "length" "8,12")
   (set_attr "type" "multiple")]
)

(define_insn "*if_move_not"
  [(set (match_operand:SI 0 "s_register_operand" "=r,r,r")
	(if_then_else:SI
	 (match_operator 4 "arm_comparison_operator"
	  [(match_operand 3 "cc_register" "") (const_int 0)])
	 (match_operand:SI 1 "arm_not_operand" "0,?rI,K")
	 (not:SI (match_operand:SI 2 "s_register_operand" "r,r,r"))))]
  "TARGET_ARM"
  "@
   mvn%D4\\t%0, %2
   mov%d4\\t%0, %1\;mvn%D4\\t%0, %2
   mvn%d4\\t%0, #%B1\;mvn%D4\\t%0, %2"
  [(set_attr "conds" "use")
   (set_attr "type" "mvn_reg")
   (set_attr "length" "4,8,8")
   (set_attr "type" "mvn_reg,multiple,multiple")]
)

(define_insn "*ifcompare_not_move"
  [(set (match_operand:SI 0 "s_register_operand" "=r,r")
	(if_then_else:SI 
	 (match_operator 5 "arm_comparison_operator"
	  [(match_operand:SI 3 "s_register_operand" "r,r")
	   (match_operand:SI 4 "arm_add_operand" "rIL,rIL")])
	 (not:SI
	  (match_operand:SI 2 "s_register_operand" "r,r"))
	 (match_operand:SI 1 "arm_not_operand" "0,?rIK")))
   (clobber (reg:CC CC_REGNUM))]
  "TARGET_ARM"
  "#"
  [(set_attr "conds" "clob")
   (set_attr "length" "8,12")
   (set_attr "type" "multiple")]
)

(define_insn "*if_not_move"
  [(set (match_operand:SI 0 "s_register_operand" "=r,r,r")
	(if_then_else:SI
	 (match_operator 4 "arm_comparison_operator"
	  [(match_operand 3 "cc_register" "") (const_int 0)])
	 (not:SI (match_operand:SI 2 "s_register_operand" "r,r,r"))
	 (match_operand:SI 1 "arm_not_operand" "0,?rI,K")))]
  "TARGET_ARM"
  "@
   mvn%d4\\t%0, %2
   mov%D4\\t%0, %1\;mvn%d4\\t%0, %2
   mvn%D4\\t%0, #%B1\;mvn%d4\\t%0, %2"
  [(set_attr "conds" "use")
   (set_attr "type" "mvn_reg,multiple,multiple")
   (set_attr "length" "4,8,8")]
)

(define_insn "*ifcompare_shift_move"
  [(set (match_operand:SI 0 "s_register_operand" "=r,r")
	(if_then_else:SI
	 (match_operator 6 "arm_comparison_operator"
	  [(match_operand:SI 4 "s_register_operand" "r,r")
	   (match_operand:SI 5 "arm_add_operand" "rIL,rIL")])
	 (match_operator:SI 7 "shift_operator"
	  [(match_operand:SI 2 "s_register_operand" "r,r")
	   (match_operand:SI 3 "arm_rhs_operand" "rM,rM")])
	 (match_operand:SI 1 "arm_not_operand" "0,?rIK")))
   (clobber (reg:CC CC_REGNUM))]
  "TARGET_ARM"
  "#"
  [(set_attr "conds" "clob")
   (set_attr "length" "8,12")
   (set_attr "type" "multiple")]
)

(define_insn "*if_shift_move"
  [(set (match_operand:SI 0 "s_register_operand" "=r,r,r")
	(if_then_else:SI
	 (match_operator 5 "arm_comparison_operator"
	  [(match_operand 6 "cc_register" "") (const_int 0)])
	 (match_operator:SI 4 "shift_operator"
	  [(match_operand:SI 2 "s_register_operand" "r,r,r")
	   (match_operand:SI 3 "arm_rhs_operand" "rM,rM,rM")])
	 (match_operand:SI 1 "arm_not_operand" "0,?rI,K")))]
  "TARGET_ARM"
  "@
   mov%d5\\t%0, %2%S4
   mov%D5\\t%0, %1\;mov%d5\\t%0, %2%S4
   mvn%D5\\t%0, #%B1\;mov%d5\\t%0, %2%S4"
  [(set_attr "conds" "use")
   (set_attr "shift" "2")
   (set_attr "length" "4,8,8")
   (set_attr_alternative "type"
                         [(if_then_else (match_operand 3 "const_int_operand" "")
                                        (const_string "mov_shift" )
                                        (const_string "mov_shift_reg"))
                          (const_string "multiple")
                          (const_string "multiple")])]
)

(define_insn "*ifcompare_move_shift"
  [(set (match_operand:SI 0 "s_register_operand" "=r,r")
	(if_then_else:SI
	 (match_operator 6 "arm_comparison_operator"
	  [(match_operand:SI 4 "s_register_operand" "r,r")
	   (match_operand:SI 5 "arm_add_operand" "rIL,rIL")])
	 (match_operand:SI 1 "arm_not_operand" "0,?rIK")
	 (match_operator:SI 7 "shift_operator"
	  [(match_operand:SI 2 "s_register_operand" "r,r")
	   (match_operand:SI 3 "arm_rhs_operand" "rM,rM")])))
   (clobber (reg:CC CC_REGNUM))]
  "TARGET_ARM"
  "#"
  [(set_attr "conds" "clob")
   (set_attr "length" "8,12")
   (set_attr "type" "multiple")]
)

(define_insn "*if_move_shift"
  [(set (match_operand:SI 0 "s_register_operand" "=r,r,r")
	(if_then_else:SI
	 (match_operator 5 "arm_comparison_operator"
	  [(match_operand 6 "cc_register" "") (const_int 0)])
	 (match_operand:SI 1 "arm_not_operand" "0,?rI,K")
	 (match_operator:SI 4 "shift_operator"
	  [(match_operand:SI 2 "s_register_operand" "r,r,r")
	   (match_operand:SI 3 "arm_rhs_operand" "rM,rM,rM")])))]
  "TARGET_ARM"
  "@
   mov%D5\\t%0, %2%S4
   mov%d5\\t%0, %1\;mov%D5\\t%0, %2%S4
   mvn%d5\\t%0, #%B1\;mov%D5\\t%0, %2%S4"
  [(set_attr "conds" "use")
   (set_attr "shift" "2")
   (set_attr "length" "4,8,8")
   (set_attr_alternative "type"
                         [(if_then_else (match_operand 3 "const_int_operand" "")
                                        (const_string "mov_shift" )
                                        (const_string "mov_shift_reg"))
                          (const_string "multiple")
                          (const_string "multiple")])]
)

(define_insn "*ifcompare_shift_shift"
  [(set (match_operand:SI 0 "s_register_operand" "=r")
	(if_then_else:SI
	 (match_operator 7 "arm_comparison_operator"
	  [(match_operand:SI 5 "s_register_operand" "r")
	   (match_operand:SI 6 "arm_add_operand" "rIL")])
	 (match_operator:SI 8 "shift_operator"
	  [(match_operand:SI 1 "s_register_operand" "r")
	   (match_operand:SI 2 "arm_rhs_operand" "rM")])
	 (match_operator:SI 9 "shift_operator"
	  [(match_operand:SI 3 "s_register_operand" "r")
	   (match_operand:SI 4 "arm_rhs_operand" "rM")])))
   (clobber (reg:CC CC_REGNUM))]
  "TARGET_ARM"
  "#"
  [(set_attr "conds" "clob")
   (set_attr "length" "12")
   (set_attr "type" "multiple")]
)

(define_insn "*if_shift_shift"
  [(set (match_operand:SI 0 "s_register_operand" "=r")
	(if_then_else:SI
	 (match_operator 5 "arm_comparison_operator"
	  [(match_operand 8 "cc_register" "") (const_int 0)])
	 (match_operator:SI 6 "shift_operator"
	  [(match_operand:SI 1 "s_register_operand" "r")
	   (match_operand:SI 2 "arm_rhs_operand" "rM")])
	 (match_operator:SI 7 "shift_operator"
	  [(match_operand:SI 3 "s_register_operand" "r")
	   (match_operand:SI 4 "arm_rhs_operand" "rM")])))]
  "TARGET_ARM"
  "mov%d5\\t%0, %1%S6\;mov%D5\\t%0, %3%S7"
  [(set_attr "conds" "use")
   (set_attr "shift" "1")
   (set_attr "length" "8")
   (set (attr "type") (if_then_else
		        (and (match_operand 2 "const_int_operand" "")
                             (match_operand 4 "const_int_operand" ""))
		      (const_string "mov_shift")
		      (const_string "mov_shift_reg")))]
)

(define_insn "*ifcompare_not_arith"
  [(set (match_operand:SI 0 "s_register_operand" "=r")
	(if_then_else:SI
	 (match_operator 6 "arm_comparison_operator"
	  [(match_operand:SI 4 "s_register_operand" "r")
	   (match_operand:SI 5 "arm_add_operand" "rIL")])
	 (not:SI (match_operand:SI 1 "s_register_operand" "r"))
	 (match_operator:SI 7 "shiftable_operator"
	  [(match_operand:SI 2 "s_register_operand" "r")
	   (match_operand:SI 3 "arm_rhs_operand" "rI")])))
   (clobber (reg:CC CC_REGNUM))]
  "TARGET_ARM"
  "#"
  [(set_attr "conds" "clob")
   (set_attr "length" "12")
   (set_attr "type" "multiple")]
)

(define_insn "*if_not_arith"
  [(set (match_operand:SI 0 "s_register_operand" "=r")
	(if_then_else:SI
	 (match_operator 5 "arm_comparison_operator"
	  [(match_operand 4 "cc_register" "") (const_int 0)])
	 (not:SI (match_operand:SI 1 "s_register_operand" "r"))
	 (match_operator:SI 6 "shiftable_operator"
	  [(match_operand:SI 2 "s_register_operand" "r")
	   (match_operand:SI 3 "arm_rhs_operand" "rI")])))]
  "TARGET_ARM"
  "mvn%d5\\t%0, %1\;%I6%D5\\t%0, %2, %3"
  [(set_attr "conds" "use")
   (set_attr "type" "mvn_reg")
   (set_attr "length" "8")]
)

(define_insn "*ifcompare_arith_not"
  [(set (match_operand:SI 0 "s_register_operand" "=r")
	(if_then_else:SI
	 (match_operator 6 "arm_comparison_operator"
	  [(match_operand:SI 4 "s_register_operand" "r")
	   (match_operand:SI 5 "arm_add_operand" "rIL")])
	 (match_operator:SI 7 "shiftable_operator"
	  [(match_operand:SI 2 "s_register_operand" "r")
	   (match_operand:SI 3 "arm_rhs_operand" "rI")])
	 (not:SI (match_operand:SI 1 "s_register_operand" "r"))))
   (clobber (reg:CC CC_REGNUM))]
  "TARGET_ARM"
  "#"
  [(set_attr "conds" "clob")
   (set_attr "length" "12")
   (set_attr "type" "multiple")]
)

(define_insn "*if_arith_not"
  [(set (match_operand:SI 0 "s_register_operand" "=r")
	(if_then_else:SI
	 (match_operator 5 "arm_comparison_operator"
	  [(match_operand 4 "cc_register" "") (const_int 0)])
	 (match_operator:SI 6 "shiftable_operator"
	  [(match_operand:SI 2 "s_register_operand" "r")
	   (match_operand:SI 3 "arm_rhs_operand" "rI")])
	 (not:SI (match_operand:SI 1 "s_register_operand" "r"))))]
  "TARGET_ARM"
  "mvn%D5\\t%0, %1\;%I6%d5\\t%0, %2, %3"
  [(set_attr "conds" "use")
   (set_attr "type" "multiple")
   (set_attr "length" "8")]
)

(define_insn "*ifcompare_neg_move"
  [(set (match_operand:SI 0 "s_register_operand" "=r,r")
	(if_then_else:SI
	 (match_operator 5 "arm_comparison_operator"
	  [(match_operand:SI 3 "s_register_operand" "r,r")
	   (match_operand:SI 4 "arm_add_operand" "rIL,rIL")])
	 (neg:SI (match_operand:SI 2 "s_register_operand" "r,r"))
	 (match_operand:SI 1 "arm_not_operand" "0,?rIK")))
   (clobber (reg:CC CC_REGNUM))]
  "TARGET_ARM"
  "#"
  [(set_attr "conds" "clob")
   (set_attr "length" "8,12")
   (set_attr "type" "multiple")]
)

(define_insn_and_split "*if_neg_move"
  [(set (match_operand:SI 0 "s_register_operand" "=l,r")
	(if_then_else:SI
	 (match_operator 4 "arm_comparison_operator"
	  [(match_operand 3 "cc_register" "") (const_int 0)])
	 (neg:SI (match_operand:SI 2 "s_register_operand" "l,r"))
	 (match_operand:SI 1 "s_register_operand" "0,0")))]
  "TARGET_32BIT"
  "#"
  "&& reload_completed"
  [(cond_exec (match_op_dup 4 [(match_dup 3) (const_int 0)])
	      (set (match_dup 0) (neg:SI (match_dup 2))))]
  ""
  [(set_attr "conds" "use")
   (set_attr "length" "4")
   (set_attr "arch" "t2,32")
   (set_attr "enabled_for_depr_it" "yes,no")
   (set_attr "type" "logic_shift_imm")]
)

(define_insn "*ifcompare_move_neg"
  [(set (match_operand:SI 0 "s_register_operand" "=r,r")
	(if_then_else:SI
	 (match_operator 5 "arm_comparison_operator"
	  [(match_operand:SI 3 "s_register_operand" "r,r")
	   (match_operand:SI 4 "arm_add_operand" "rIL,rIL")])
	 (match_operand:SI 1 "arm_not_operand" "0,?rIK")
	 (neg:SI (match_operand:SI 2 "s_register_operand" "r,r"))))
   (clobber (reg:CC CC_REGNUM))]
  "TARGET_ARM"
  "#"
  [(set_attr "conds" "clob")
   (set_attr "length" "8,12")
   (set_attr "type" "multiple")]
)

(define_insn_and_split "*if_move_neg"
  [(set (match_operand:SI 0 "s_register_operand" "=l,r")
	(if_then_else:SI
	 (match_operator 4 "arm_comparison_operator"
	  [(match_operand 3 "cc_register" "") (const_int 0)])
	 (match_operand:SI 1 "s_register_operand" "0,0")
	 (neg:SI (match_operand:SI 2 "s_register_operand" "l,r"))))]
  "TARGET_32BIT"
  "#"
  "&& reload_completed"
  [(cond_exec (match_dup 5)
	      (set (match_dup 0) (neg:SI (match_dup 2))))]
  {
    machine_mode mode = GET_MODE (operands[3]);
    rtx_code rc = GET_CODE (operands[4]);

    if (mode == CCFPmode || mode == CCFPEmode)
      rc = reverse_condition_maybe_unordered (rc);
    else
      rc = reverse_condition (rc);

    operands[5] = gen_rtx_fmt_ee (rc, VOIDmode, operands[3], const0_rtx);
  }
  [(set_attr "conds" "use")
   (set_attr "length" "4")
   (set_attr "arch" "t2,32")
   (set_attr "enabled_for_depr_it" "yes,no")
   (set_attr "type" "logic_shift_imm")]
)

(define_insn "*arith_adjacentmem"
  [(set (match_operand:SI 0 "s_register_operand" "=r")
	(match_operator:SI 1 "shiftable_operator"
	 [(match_operand:SI 2 "memory_operand" "m")
	  (match_operand:SI 3 "memory_operand" "m")]))
   (clobber (match_scratch:SI 4 "=r"))]
  "TARGET_ARM && adjacent_mem_locations (operands[2], operands[3])"
  "*
  {
    rtx ldm[3];
    rtx arith[4];
    rtx base_reg;
    HOST_WIDE_INT val1 = 0, val2 = 0;

    if (REGNO (operands[0]) > REGNO (operands[4]))
      {
	ldm[1] = operands[4];
	ldm[2] = operands[0];
      }
    else
      {
	ldm[1] = operands[0];
	ldm[2] = operands[4];
      }

    base_reg = XEXP (operands[2], 0);

    if (!REG_P (base_reg))
      {
	val1 = INTVAL (XEXP (base_reg, 1));
	base_reg = XEXP (base_reg, 0);
      }

    if (!REG_P (XEXP (operands[3], 0)))
      val2 = INTVAL (XEXP (XEXP (operands[3], 0), 1));

    arith[0] = operands[0];
    arith[3] = operands[1];

    if (val1 < val2)
      {
	arith[1] = ldm[1];
	arith[2] = ldm[2];
      }
    else
      {
	arith[1] = ldm[2];
	arith[2] = ldm[1];
      }

    ldm[0] = base_reg;
    if (val1 !=0 && val2 != 0)
      {
	rtx ops[3];

	if (val1 == 4 || val2 == 4)
	  /* Other val must be 8, since we know they are adjacent and neither
	     is zero.  */
	  output_asm_insn (\"ldmib%?\\t%0, {%1, %2}\", ldm);
	else if (const_ok_for_arm (val1) || const_ok_for_arm (-val1))
	  {
	    ldm[0] = ops[0] = operands[4];
	    ops[1] = base_reg;
	    ops[2] = GEN_INT (val1);
	    output_add_immediate (ops);
	    if (val1 < val2)
	      output_asm_insn (\"ldmia%?\\t%0, {%1, %2}\", ldm);
	    else
	      output_asm_insn (\"ldmda%?\\t%0, {%1, %2}\", ldm);
	  }
	else
	  {
	    /* Offset is out of range for a single add, so use two ldr.  */
	    ops[0] = ldm[1];
	    ops[1] = base_reg;
	    ops[2] = GEN_INT (val1);
	    output_asm_insn (\"ldr%?\\t%0, [%1, %2]\", ops);
	    ops[0] = ldm[2];
	    ops[2] = GEN_INT (val2);
	    output_asm_insn (\"ldr%?\\t%0, [%1, %2]\", ops);
	  }
      }
    else if (val1 != 0)
      {
	if (val1 < val2)
	  output_asm_insn (\"ldmda%?\\t%0, {%1, %2}\", ldm);
	else
	  output_asm_insn (\"ldmia%?\\t%0, {%1, %2}\", ldm);
      }
    else
      {
	if (val1 < val2)
	  output_asm_insn (\"ldmia%?\\t%0, {%1, %2}\", ldm);
	else
	  output_asm_insn (\"ldmda%?\\t%0, {%1, %2}\", ldm);
      }
    output_asm_insn (\"%I3%?\\t%0, %1, %2\", arith);
    return \"\";
  }"
  [(set_attr "length" "12")
   (set_attr "predicable" "yes")
   (set_attr "type" "load1")]
)

; This pattern is never tried by combine, so do it as a peephole

(define_peephole2
  [(set (match_operand:SI 0 "arm_general_register_operand" "")
	(match_operand:SI 1 "arm_general_register_operand" ""))
   (set (reg:CC CC_REGNUM)
	(compare:CC (match_dup 1) (const_int 0)))]
  "TARGET_ARM"
  [(parallel [(set (reg:CC CC_REGNUM) (compare:CC (match_dup 1) (const_int 0)))
	      (set (match_dup 0) (match_dup 1))])]
  ""
)

(define_split
  [(set (match_operand:SI 0 "s_register_operand" "")
	(and:SI (ge:SI (match_operand:SI 1 "s_register_operand" "")
		       (const_int 0))
		(neg:SI (match_operator:SI 2 "arm_comparison_operator"
			 [(match_operand:SI 3 "s_register_operand" "")
			  (match_operand:SI 4 "arm_rhs_operand" "")]))))
   (clobber (match_operand:SI 5 "s_register_operand" ""))]
  "TARGET_ARM"
  [(set (match_dup 5) (not:SI (ashiftrt:SI (match_dup 1) (const_int 31))))
   (set (match_dup 0) (and:SI (match_op_dup 2 [(match_dup 3) (match_dup 4)])
			      (match_dup 5)))]
  ""
)

;; This split can be used because CC_Z mode implies that the following
;; branch will be an equality, or an unsigned inequality, so the sign
;; extension is not needed.

(define_split
  [(set (reg:CC_Z CC_REGNUM)
	(compare:CC_Z
	 (ashift:SI (subreg:SI (match_operand:QI 0 "memory_operand" "") 0)
		    (const_int 24))
	 (match_operand 1 "const_int_operand" "")))
   (clobber (match_scratch:SI 2 ""))]
  "TARGET_ARM
   && ((UINTVAL (operands[1]))
       == ((UINTVAL (operands[1])) >> 24) << 24)"
  [(set (match_dup 2) (zero_extend:SI (match_dup 0)))
   (set (reg:CC CC_REGNUM) (compare:CC (match_dup 2) (match_dup 1)))]
  "
  operands[1] = GEN_INT (((unsigned long) INTVAL (operands[1])) >> 24);
  "
)
;; ??? Check the patterns above for Thumb-2 usefulness

(define_expand "prologue"
  [(clobber (const_int 0))]
  "TARGET_EITHER"
  "if (TARGET_32BIT)
     arm_expand_prologue ();
   else
     thumb1_expand_prologue ();
  DONE;
  "
)

(define_expand "epilogue"
  [(clobber (const_int 0))]
  "TARGET_EITHER"
  "
  if (crtl->calls_eh_return)
    emit_insn (gen_force_register_use (gen_rtx_REG (Pmode, 2)));
  if (TARGET_THUMB1)
   {
     thumb1_expand_epilogue ();
     emit_jump_insn (gen_rtx_UNSPEC_VOLATILE (VOIDmode,
                     gen_rtvec (1, ret_rtx), VUNSPEC_EPILOGUE));
   }
  else if (HAVE_return)
   {
     /* HAVE_return is testing for USE_RETURN_INSN (FALSE).  Hence,
        no need for explicit testing again.  */
     emit_jump_insn (gen_return ());
   }
  else if (TARGET_32BIT)
   {
    arm_expand_epilogue (true);
   }
  DONE;
  "
)

;; Note - although unspec_volatile's USE all hard registers,
;; USEs are ignored after relaod has completed.  Thus we need
;; to add an unspec of the link register to ensure that flow
;; does not think that it is unused by the sibcall branch that
;; will replace the standard function epilogue.
(define_expand "sibcall_epilogue"
   [(parallel [(unspec:SI [(reg:SI LR_REGNUM)] UNSPEC_REGISTER_USE)
               (unspec_volatile [(return)] VUNSPEC_EPILOGUE)])]
   "TARGET_32BIT"
   "
   arm_expand_epilogue (false);
   DONE;
   "
)

(define_expand "eh_epilogue"
  [(use (match_operand:SI 0 "register_operand" ""))
   (use (match_operand:SI 1 "register_operand" ""))
   (use (match_operand:SI 2 "register_operand" ""))]
  "TARGET_EITHER"
  "
  {
    cfun->machine->eh_epilogue_sp_ofs = operands[1];
    if (!REG_P (operands[2]) || REGNO (operands[2]) != 2)
      {
	rtx ra = gen_rtx_REG (Pmode, 2);

	emit_move_insn (ra, operands[2]);
	operands[2] = ra;
      }
    /* This is a hack -- we may have crystalized the function type too
       early.  */
    cfun->machine->func_type = 0;
  }"
)

;; This split is only used during output to reduce the number of patterns
;; that need assembler instructions adding to them.  We allowed the setting
;; of the conditions to be implicit during rtl generation so that
;; the conditional compare patterns would work.  However this conflicts to
;; some extent with the conditional data operations, so we have to split them
;; up again here.

;; ??? Need to audit these splitters for Thumb-2.  Why isn't normal
;; conditional execution sufficient?

(define_split
  [(set (match_operand:SI 0 "s_register_operand" "")
	(if_then_else:SI (match_operator 1 "arm_comparison_operator"
			  [(match_operand 2 "" "") (match_operand 3 "" "")])
			 (match_dup 0)
			 (match_operand 4 "" "")))
   (clobber (reg:CC CC_REGNUM))]
  "TARGET_ARM && reload_completed"
  [(set (match_dup 5) (match_dup 6))
   (cond_exec (match_dup 7)
	      (set (match_dup 0) (match_dup 4)))]
  "
  {
    machine_mode mode = SELECT_CC_MODE (GET_CODE (operands[1]),
					     operands[2], operands[3]);
    enum rtx_code rc = GET_CODE (operands[1]);

    operands[5] = gen_rtx_REG (mode, CC_REGNUM);
    operands[6] = gen_rtx_COMPARE (mode, operands[2], operands[3]);
    if (mode == CCFPmode || mode == CCFPEmode)
      rc = reverse_condition_maybe_unordered (rc);
    else
      rc = reverse_condition (rc);

    operands[7] = gen_rtx_fmt_ee (rc, VOIDmode, operands[5], const0_rtx);
  }"
)

(define_split
  [(set (match_operand:SI 0 "s_register_operand" "")
	(if_then_else:SI (match_operator 1 "arm_comparison_operator"
			  [(match_operand 2 "" "") (match_operand 3 "" "")])
			 (match_operand 4 "" "")
			 (match_dup 0)))
   (clobber (reg:CC CC_REGNUM))]
  "TARGET_ARM && reload_completed"
  [(set (match_dup 5) (match_dup 6))
   (cond_exec (match_op_dup 1 [(match_dup 5) (const_int 0)])
	      (set (match_dup 0) (match_dup 4)))]
  "
  {
    machine_mode mode = SELECT_CC_MODE (GET_CODE (operands[1]),
					     operands[2], operands[3]);

    operands[5] = gen_rtx_REG (mode, CC_REGNUM);
    operands[6] = gen_rtx_COMPARE (mode, operands[2], operands[3]);
  }"
)

(define_split
  [(set (match_operand:SI 0 "s_register_operand" "")
	(if_then_else:SI (match_operator 1 "arm_comparison_operator"
			  [(match_operand 2 "" "") (match_operand 3 "" "")])
			 (match_operand 4 "" "")
			 (match_operand 5 "" "")))
   (clobber (reg:CC CC_REGNUM))]
  "TARGET_ARM && reload_completed"
  [(set (match_dup 6) (match_dup 7))
   (cond_exec (match_op_dup 1 [(match_dup 6) (const_int 0)])
	      (set (match_dup 0) (match_dup 4)))
   (cond_exec (match_dup 8)
	      (set (match_dup 0) (match_dup 5)))]
  "
  {
    machine_mode mode = SELECT_CC_MODE (GET_CODE (operands[1]),
					     operands[2], operands[3]);
    enum rtx_code rc = GET_CODE (operands[1]);

    operands[6] = gen_rtx_REG (mode, CC_REGNUM);
    operands[7] = gen_rtx_COMPARE (mode, operands[2], operands[3]);
    if (mode == CCFPmode || mode == CCFPEmode)
      rc = reverse_condition_maybe_unordered (rc);
    else
      rc = reverse_condition (rc);

    operands[8] = gen_rtx_fmt_ee (rc, VOIDmode, operands[6], const0_rtx);
  }"
)

(define_split
  [(set (match_operand:SI 0 "s_register_operand" "")
	(if_then_else:SI (match_operator 1 "arm_comparison_operator"
			  [(match_operand:SI 2 "s_register_operand" "")
			   (match_operand:SI 3 "arm_add_operand" "")])
			 (match_operand:SI 4 "arm_rhs_operand" "")
			 (not:SI
			  (match_operand:SI 5 "s_register_operand" ""))))
   (clobber (reg:CC CC_REGNUM))]
  "TARGET_ARM && reload_completed"
  [(set (match_dup 6) (match_dup 7))
   (cond_exec (match_op_dup 1 [(match_dup 6) (const_int 0)])
	      (set (match_dup 0) (match_dup 4)))
   (cond_exec (match_dup 8)
	      (set (match_dup 0) (not:SI (match_dup 5))))]
  "
  {
    machine_mode mode = SELECT_CC_MODE (GET_CODE (operands[1]),
					     operands[2], operands[3]);
    enum rtx_code rc = GET_CODE (operands[1]);

    operands[6] = gen_rtx_REG (mode, CC_REGNUM);
    operands[7] = gen_rtx_COMPARE (mode, operands[2], operands[3]);
    if (mode == CCFPmode || mode == CCFPEmode)
      rc = reverse_condition_maybe_unordered (rc);
    else
      rc = reverse_condition (rc);

    operands[8] = gen_rtx_fmt_ee (rc, VOIDmode, operands[6], const0_rtx);
  }"
)

(define_insn "*cond_move_not"
  [(set (match_operand:SI 0 "s_register_operand" "=r,r")
	(if_then_else:SI (match_operator 4 "arm_comparison_operator"
			  [(match_operand 3 "cc_register" "") (const_int 0)])
			 (match_operand:SI 1 "arm_rhs_operand" "0,?rI")
			 (not:SI
			  (match_operand:SI 2 "s_register_operand" "r,r"))))]
  "TARGET_ARM"
  "@
   mvn%D4\\t%0, %2
   mov%d4\\t%0, %1\;mvn%D4\\t%0, %2"
  [(set_attr "conds" "use")
   (set_attr "type" "mvn_reg,multiple")
   (set_attr "length" "4,8")]
)

;; The next two patterns occur when an AND operation is followed by a
;; scc insn sequence 

(define_insn "*sign_extract_onebit"
  [(set (match_operand:SI 0 "s_register_operand" "=r")
	(sign_extract:SI (match_operand:SI 1 "s_register_operand" "r")
			 (const_int 1)
			 (match_operand:SI 2 "const_int_operand" "n")))
    (clobber (reg:CC CC_REGNUM))]
  "TARGET_ARM"
  "*
    operands[2] = GEN_INT (1 << INTVAL (operands[2]));
    output_asm_insn (\"ands\\t%0, %1, %2\", operands);
    return \"mvnne\\t%0, #0\";
  "
  [(set_attr "conds" "clob")
   (set_attr "length" "8")
   (set_attr "type" "multiple")]
)

(define_insn "*not_signextract_onebit"
  [(set (match_operand:SI 0 "s_register_operand" "=r")
	(not:SI
	 (sign_extract:SI (match_operand:SI 1 "s_register_operand" "r")
			  (const_int 1)
			  (match_operand:SI 2 "const_int_operand" "n"))))
   (clobber (reg:CC CC_REGNUM))]
  "TARGET_ARM"
  "*
    operands[2] = GEN_INT (1 << INTVAL (operands[2]));
    output_asm_insn (\"tst\\t%1, %2\", operands);
    output_asm_insn (\"mvneq\\t%0, #0\", operands);
    return \"movne\\t%0, #0\";
  "
  [(set_attr "conds" "clob")
   (set_attr "length" "12")
   (set_attr "type" "multiple")]
)
;; ??? The above patterns need auditing for Thumb-2

;; Push multiple registers to the stack.  Registers are in parallel (use ...)
;; expressions.  For simplicity, the first register is also in the unspec
;; part.
;; To avoid the usage of GNU extension, the length attribute is computed
;; in a C function arm_attr_length_push_multi.
(define_insn "*push_multi"
  [(match_parallel 2 "multi_register_push"
    [(set (match_operand:BLK 0 "push_mult_memory_operand" "")
	  (unspec:BLK [(match_operand:SI 1 "s_register_operand" "")]
		      UNSPEC_PUSH_MULT))])]
  ""
  "*
  {
    int num_saves = XVECLEN (operands[2], 0);
     
    /* For the StrongARM at least it is faster to
       use STR to store only a single register.
       In Thumb mode always use push, and the assembler will pick
       something appropriate.  */
    if (num_saves == 1 && TARGET_ARM)
      output_asm_insn (\"str%?\\t%1, [%m0, #-4]!\", operands);
    else
      {
	int i;
	char pattern[100];

	if (TARGET_32BIT)
	    strcpy (pattern, \"push%?\\t{%1\");
	else
	    strcpy (pattern, \"push\\t{%1\");

	for (i = 1; i < num_saves; i++)
	  {
	    strcat (pattern, \", %|\");
	    strcat (pattern,
		    reg_names[REGNO (XEXP (XVECEXP (operands[2], 0, i), 0))]);
	  }

	strcat (pattern, \"}\");
	output_asm_insn (pattern, operands);
      }

    return \"\";
  }"
  [(set_attr "type" "store4")
   (set (attr "length")
	(symbol_ref "arm_attr_length_push_multi (operands[2], operands[1])"))]
)

(define_insn "stack_tie"
  [(set (mem:BLK (scratch))
	(unspec:BLK [(match_operand:SI 0 "s_register_operand" "rk")
		     (match_operand:SI 1 "s_register_operand" "rk")]
		    UNSPEC_PRLG_STK))]
  ""
  ""
  [(set_attr "length" "0")
   (set_attr "type" "block")]
)

;; Pop (as used in epilogue RTL)
;;
(define_insn "*load_multiple_with_writeback"
  [(match_parallel 0 "load_multiple_operation"
    [(set (match_operand:SI 1 "s_register_operand" "+rk")
          (plus:SI (match_dup 1)
                   (match_operand:SI 2 "const_int_I_operand" "I")))
     (set (match_operand:SI 3 "s_register_operand" "=rk")
          (mem:SI (match_dup 1)))
        ])]
  "TARGET_32BIT && (reload_in_progress || reload_completed)"
  "*
  {
    arm_output_multireg_pop (operands, /*return_pc=*/false,
                                       /*cond=*/const_true_rtx,
                                       /*reverse=*/false,
                                       /*update=*/true);
    return \"\";
  }
  "
  [(set_attr "type" "load4")
   (set_attr "predicable" "yes")
   (set (attr "length")
	(symbol_ref "arm_attr_length_pop_multi (operands,
						/*return_pc=*/false,
						/*write_back_p=*/true)"))]
)

;; Pop with return (as used in epilogue RTL)
;;
;; This instruction is generated when the registers are popped at the end of
;; epilogue.  Here, instead of popping the value into LR and then generating
;; jump to LR, value is popped into PC directly.  Hence, the pattern is combined
;;  with (return).
(define_insn "*pop_multiple_with_writeback_and_return"
  [(match_parallel 0 "pop_multiple_return"
    [(return)
     (set (match_operand:SI 1 "s_register_operand" "+rk")
          (plus:SI (match_dup 1)
                   (match_operand:SI 2 "const_int_I_operand" "I")))
     (set (match_operand:SI 3 "s_register_operand" "=rk")
          (mem:SI (match_dup 1)))
        ])]
  "TARGET_32BIT && (reload_in_progress || reload_completed)"
  "*
  {
    arm_output_multireg_pop (operands, /*return_pc=*/true,
                                       /*cond=*/const_true_rtx,
                                       /*reverse=*/false,
                                       /*update=*/true);
    return \"\";
  }
  "
  [(set_attr "type" "load4")
   (set_attr "predicable" "yes")
   (set (attr "length")
	(symbol_ref "arm_attr_length_pop_multi (operands, /*return_pc=*/true,
						/*write_back_p=*/true)"))]
)

(define_insn "*pop_multiple_with_return"
  [(match_parallel 0 "pop_multiple_return"
    [(return)
     (set (match_operand:SI 2 "s_register_operand" "=rk")
          (mem:SI (match_operand:SI 1 "s_register_operand" "rk")))
        ])]
  "TARGET_32BIT && (reload_in_progress || reload_completed)"
  "*
  {
    arm_output_multireg_pop (operands, /*return_pc=*/true,
                                       /*cond=*/const_true_rtx,
                                       /*reverse=*/false,
                                       /*update=*/false);
    return \"\";
  }
  "
  [(set_attr "type" "load4")
   (set_attr "predicable" "yes")
   (set (attr "length")
	(symbol_ref "arm_attr_length_pop_multi (operands, /*return_pc=*/true,
						/*write_back_p=*/false)"))]
)

;; Load into PC and return
(define_insn "*ldr_with_return"
  [(return)
   (set (reg:SI PC_REGNUM)
        (mem:SI (post_inc:SI (match_operand:SI 0 "s_register_operand" "+rk"))))]
  "TARGET_32BIT && (reload_in_progress || reload_completed)"
  "ldr%?\t%|pc, [%0], #4"
  [(set_attr "type" "load1")
   (set_attr "predicable" "yes")]
)
;; Pop for floating point registers (as used in epilogue RTL)
(define_insn "*vfp_pop_multiple_with_writeback"
  [(match_parallel 0 "pop_multiple_fp"
    [(set (match_operand:SI 1 "s_register_operand" "+rk")
          (plus:SI (match_dup 1)
                   (match_operand:SI 2 "const_int_I_operand" "I")))
     (set (match_operand:DF 3 "vfp_hard_register_operand" "")
          (mem:DF (match_dup 1)))])]
  "TARGET_32BIT && TARGET_HARD_FLOAT && TARGET_VFP"
  "*
  {
    int num_regs = XVECLEN (operands[0], 0);
    char pattern[100];
    rtx op_list[2];
    strcpy (pattern, \"vldm\\t\");
    strcat (pattern, reg_names[REGNO (SET_DEST (XVECEXP (operands[0], 0, 0)))]);
    strcat (pattern, \"!, {\");
    op_list[0] = XEXP (XVECEXP (operands[0], 0, 1), 0);
    strcat (pattern, \"%P0\");
    if ((num_regs - 1) > 1)
      {
        strcat (pattern, \"-%P1\");
        op_list [1] = XEXP (XVECEXP (operands[0], 0, num_regs - 1), 0);
      }

    strcat (pattern, \"}\");
    output_asm_insn (pattern, op_list);
    return \"\";
  }
  "
  [(set_attr "type" "load4")
   (set_attr "conds" "unconditional")
   (set_attr "predicable" "no")]
)

;; Special patterns for dealing with the constant pool

(define_insn "align_4"
  [(unspec_volatile [(const_int 0)] VUNSPEC_ALIGN)]
  "TARGET_EITHER"
  "*
  assemble_align (32);
  return \"\";
  "
  [(set_attr "type" "no_insn")]
)

(define_insn "align_8"
  [(unspec_volatile [(const_int 0)] VUNSPEC_ALIGN8)]
  "TARGET_EITHER"
  "*
  assemble_align (64);
  return \"\";
  "
  [(set_attr "type" "no_insn")]
)

(define_insn "consttable_end"
  [(unspec_volatile [(const_int 0)] VUNSPEC_POOL_END)]
  "TARGET_EITHER"
  "*
  making_const_table = FALSE;
  return \"\";
  "
  [(set_attr "type" "no_insn")]
)

(define_insn "consttable_1"
  [(unspec_volatile [(match_operand 0 "" "")] VUNSPEC_POOL_1)]
  "TARGET_EITHER"
  "*
  making_const_table = TRUE;
  assemble_integer (operands[0], 1, BITS_PER_WORD, 1);
  assemble_zeros (3);
  return \"\";
  "
  [(set_attr "length" "4")
   (set_attr "type" "no_insn")]
)

(define_insn "consttable_2"
  [(unspec_volatile [(match_operand 0 "" "")] VUNSPEC_POOL_2)]
  "TARGET_EITHER"
  "*
  {
    rtx x = operands[0];
    making_const_table = TRUE;
    switch (GET_MODE_CLASS (GET_MODE (x)))
      {
      case MODE_FLOAT:
	arm_emit_fp16_const (x);
	break;
      default:
	assemble_integer (operands[0], 2, BITS_PER_WORD, 1);
	assemble_zeros (2);
	break;
      }
    return \"\";
  }"
  [(set_attr "length" "4")
   (set_attr "type" "no_insn")]
)

(define_insn "consttable_4"
  [(unspec_volatile [(match_operand 0 "" "")] VUNSPEC_POOL_4)]
  "TARGET_EITHER"
  "*
  {
    rtx x = operands[0];
    making_const_table = TRUE;
    switch (GET_MODE_CLASS (GET_MODE (x)))
      {
      case MODE_FLOAT:
	assemble_real (*CONST_DOUBLE_REAL_VALUE (x), GET_MODE (x),
		       BITS_PER_WORD);
	break;
      default:
	/* XXX: Sometimes gcc does something really dumb and ends up with
	   a HIGH in a constant pool entry, usually because it's trying to
	   load into a VFP register.  We know this will always be used in
	   combination with a LO_SUM which ignores the high bits, so just
	   strip off the HIGH.  */
	if (GET_CODE (x) == HIGH)
	  x = XEXP (x, 0);
        assemble_integer (x, 4, BITS_PER_WORD, 1);
	mark_symbol_refs_as_used (x);
        break;
      }
    return \"\";
  }"
  [(set_attr "length" "4")
   (set_attr "type" "no_insn")]
)

(define_insn "consttable_8"
  [(unspec_volatile [(match_operand 0 "" "")] VUNSPEC_POOL_8)]
  "TARGET_EITHER"
  "*
  {
    making_const_table = TRUE;
    switch (GET_MODE_CLASS (GET_MODE (operands[0])))
      {
      case MODE_FLOAT:
	assemble_real (*CONST_DOUBLE_REAL_VALUE (operands[0]),
		       GET_MODE (operands[0]), BITS_PER_WORD);
	break;
      default:
        assemble_integer (operands[0], 8, BITS_PER_WORD, 1);
        break;
      }
    return \"\";
  }"
  [(set_attr "length" "8")
   (set_attr "type" "no_insn")]
)

(define_insn "consttable_16"
  [(unspec_volatile [(match_operand 0 "" "")] VUNSPEC_POOL_16)]
  "TARGET_EITHER"
  "*
  {
    making_const_table = TRUE;
    switch (GET_MODE_CLASS (GET_MODE (operands[0])))
      {
      case MODE_FLOAT:
	assemble_real (*CONST_DOUBLE_REAL_VALUE (operands[0]),
		       GET_MODE (operands[0]), BITS_PER_WORD);
	break;
      default:
        assemble_integer (operands[0], 16, BITS_PER_WORD, 1);
        break;
      }
    return \"\";
  }"
  [(set_attr "length" "16")
   (set_attr "type" "no_insn")]
)

;; V5 Instructions,

(define_insn "clzsi2"
  [(set (match_operand:SI 0 "s_register_operand" "=r")
	(clz:SI (match_operand:SI 1 "s_register_operand" "r")))]
  "TARGET_32BIT && arm_arch5"
  "clz%?\\t%0, %1"
  [(set_attr "predicable" "yes")
   (set_attr "predicable_short_it" "no")
   (set_attr "type" "clz")])

(define_insn "rbitsi2"
  [(set (match_operand:SI 0 "s_register_operand" "=r")
	(unspec:SI [(match_operand:SI 1 "s_register_operand" "r")] UNSPEC_RBIT))]
  "TARGET_32BIT && arm_arch_thumb2"
  "rbit%?\\t%0, %1"
  [(set_attr "predicable" "yes")
   (set_attr "predicable_short_it" "no")
   (set_attr "type" "clz")])

;; Keep this as a CTZ expression until after reload and then split
;; into RBIT + CLZ.  Since RBIT is represented as an UNSPEC it is unlikely
;; to fold with any other expression.

(define_insn_and_split "ctzsi2"
 [(set (match_operand:SI           0 "s_register_operand" "=r")
       (ctz:SI (match_operand:SI  1 "s_register_operand" "r")))]
  "TARGET_32BIT && arm_arch_thumb2"
  "#"
  "&& reload_completed"
  [(const_int 0)]
  "
  emit_insn (gen_rbitsi2 (operands[0], operands[1]));
  emit_insn (gen_clzsi2 (operands[0], operands[0]));
  DONE;
")

;; V5E instructions.

(define_insn "prefetch"
  [(prefetch (match_operand:SI 0 "address_operand" "p")
	     (match_operand:SI 1 "" "")
	     (match_operand:SI 2 "" ""))]
  "TARGET_32BIT && arm_arch5e"
  "pld\\t%a0"
  [(set_attr "type" "load1")]
)

;; General predication pattern

(define_cond_exec
  [(match_operator 0 "arm_comparison_operator"
    [(match_operand 1 "cc_register" "")
     (const_int 0)])]
  "TARGET_32BIT
   && (!TARGET_NO_VOLATILE_CE || !volatile_refs_p (PATTERN (insn)))"
  ""
[(set_attr "predicated" "yes")]
)

(define_insn "force_register_use"
  [(unspec:SI [(match_operand:SI 0 "register_operand" "")] UNSPEC_REGISTER_USE)]
  ""
  "%@ %0 needed"
  [(set_attr "length" "0")
   (set_attr "type" "no_insn")]
)


;; Patterns for exception handling

(define_expand "eh_return"
  [(use (match_operand 0 "general_operand" ""))]
  "TARGET_EITHER"
  "
  {
    if (TARGET_32BIT)
      emit_insn (gen_arm_eh_return (operands[0]));
    else
      emit_insn (gen_thumb_eh_return (operands[0]));
    DONE;
  }"
)
				   
;; We can't expand this before we know where the link register is stored.
(define_insn_and_split "arm_eh_return"
  [(unspec_volatile [(match_operand:SI 0 "s_register_operand" "r")]
		    VUNSPEC_EH_RETURN)
   (clobber (match_scratch:SI 1 "=&r"))]
  "TARGET_ARM"
  "#"
  "&& reload_completed"
  [(const_int 0)]
  "
  {
    arm_set_return_address (operands[0], operands[1]);
    DONE;
  }"
)


;; TLS support

(define_insn "load_tp_hard"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(unspec:SI [(const_int 0)] UNSPEC_TLS))]
  "TARGET_HARD_TP"
  "mrc%?\\tp15, 0, %0, c13, c0, 3\\t@ load_tp_hard"
  [(set_attr "predicable" "yes")
   (set_attr "type" "mrs")]
)

;; Doesn't clobber R1-R3.  Must use r0 for the first operand.
(define_insn "load_tp_soft"
  [(set (reg:SI 0) (unspec:SI [(const_int 0)] UNSPEC_TLS))
   (clobber (reg:SI LR_REGNUM))
   (clobber (reg:SI IP_REGNUM))
   (clobber (reg:CC CC_REGNUM))]
  "TARGET_SOFT_TP"
  "bl\\t__aeabi_read_tp\\t@ load_tp_soft"
  [(set_attr "conds" "clob")
   (set_attr "type" "branch")]
)

;; tls descriptor call
(define_insn "tlscall"
  [(set (reg:SI R0_REGNUM)
        (unspec:SI [(reg:SI R0_REGNUM)
                    (match_operand:SI 0 "" "X")
	            (match_operand 1 "" "")] UNSPEC_TLS))
   (clobber (reg:SI R1_REGNUM))
   (clobber (reg:SI LR_REGNUM))
   (clobber (reg:SI CC_REGNUM))]
  "TARGET_GNU2_TLS"
  {
    targetm.asm_out.internal_label (asm_out_file, "LPIC",
				    INTVAL (operands[1]));
    return "bl\\t%c0(tlscall)";
  }
  [(set_attr "conds" "clob")
   (set_attr "length" "4")
   (set_attr "type" "branch")]
)

;; For thread pointer builtin
(define_expand "get_thread_pointersi"
  [(match_operand:SI 0 "s_register_operand" "=r")]
 ""
 "
 {
   arm_load_tp (operands[0]);
   DONE;
 }")

;;

;; We only care about the lower 16 bits of the constant 
;; being inserted into the upper 16 bits of the register.
(define_insn "*arm_movtas_ze" 
  [(set (zero_extract:SI (match_operand:SI 0 "s_register_operand" "+r,r")
                   (const_int 16)
                   (const_int 16))
        (match_operand:SI 1 "const_int_operand" ""))]
  "TARGET_HAVE_MOVT"
  "@
   movt%?\t%0, %L1
   movt\t%0, %L1"
 [(set_attr "arch" "32,v8mb")
  (set_attr "predicable" "yes")
  (set_attr "predicable_short_it" "no")
  (set_attr "length" "4")
  (set_attr "type" "alu_sreg")]
)

(define_insn "*arm_rev"
  [(set (match_operand:SI 0 "s_register_operand" "=l,l,r")
	(bswap:SI (match_operand:SI 1 "s_register_operand" "l,l,r")))]
  "arm_arch6"
  "@
   rev\t%0, %1
   rev%?\t%0, %1
   rev%?\t%0, %1"
  [(set_attr "arch" "t1,t2,32")
   (set_attr "length" "2,2,4")
   (set_attr "predicable" "no,yes,yes")
   (set_attr "predicable_short_it" "no")
   (set_attr "type" "rev")]
)

(define_expand "arm_legacy_rev"
  [(set (match_operand:SI 2 "s_register_operand" "")
	(xor:SI (rotatert:SI (match_operand:SI 1 "s_register_operand" "")
			     (const_int 16))
		(match_dup 1)))
   (set (match_dup 2)
	(lshiftrt:SI (match_dup 2)
		     (const_int 8)))
   (set (match_operand:SI 3 "s_register_operand" "")
	(rotatert:SI (match_dup 1)
		     (const_int 8)))
   (set (match_dup 2)
	(and:SI (match_dup 2)
		(const_int -65281)))
   (set (match_operand:SI 0 "s_register_operand" "")
	(xor:SI (match_dup 3)
		(match_dup 2)))]
  "TARGET_32BIT"
  ""
)

;; Reuse temporaries to keep register pressure down.
(define_expand "thumb_legacy_rev"
  [(set (match_operand:SI 2 "s_register_operand" "")
     (ashift:SI (match_operand:SI 1 "s_register_operand" "")
                (const_int 24)))
   (set (match_operand:SI 3 "s_register_operand" "")
     (lshiftrt:SI (match_dup 1)
		  (const_int 24)))
   (set (match_dup 3)
     (ior:SI (match_dup 3)
	     (match_dup 2)))
   (set (match_operand:SI 4 "s_register_operand" "")
     (const_int 16))
   (set (match_operand:SI 5 "s_register_operand" "")
     (rotatert:SI (match_dup 1)
		  (match_dup 4)))
   (set (match_dup 2)
     (ashift:SI (match_dup 5)
                (const_int 24)))
   (set (match_dup 5)
     (lshiftrt:SI (match_dup 5)
		  (const_int 24)))
   (set (match_dup 5)
     (ior:SI (match_dup 5)
	     (match_dup 2)))
   (set (match_dup 5)
     (rotatert:SI (match_dup 5)
		  (match_dup 4)))
   (set (match_operand:SI 0 "s_register_operand" "")
     (ior:SI (match_dup 5)
             (match_dup 3)))]
  "TARGET_THUMB"
  ""
)

;; ARM-specific expansion of signed mod by power of 2
;; using conditional negate.
;; For r0 % n where n is a power of 2 produce:
;; rsbs    r1, r0, #0
;; and     r0, r0, #(n - 1)
;; and     r1, r1, #(n - 1)
;; rsbpl   r0, r1, #0

(define_expand "modsi3"
  [(match_operand:SI 0 "register_operand" "")
   (match_operand:SI 1 "register_operand" "")
   (match_operand:SI 2 "const_int_operand" "")]
  "TARGET_32BIT"
  {
    HOST_WIDE_INT val = INTVAL (operands[2]);

    if (val <= 0
       || exact_log2 (val) <= 0)
      FAIL;

    rtx mask = GEN_INT (val - 1);

    /* In the special case of x0 % 2 we can do the even shorter:
	cmp     r0, #0
	and     r0, r0, #1
	rsblt   r0, r0, #0.  */

    if (val == 2)
      {
	rtx cc_reg = arm_gen_compare_reg (LT,
					  operands[1], const0_rtx, NULL_RTX);
	rtx cond = gen_rtx_LT (SImode, cc_reg, const0_rtx);
	rtx masked = gen_reg_rtx (SImode);

	emit_insn (gen_andsi3 (masked, operands[1], mask));
	emit_move_insn (operands[0],
			gen_rtx_IF_THEN_ELSE (SImode, cond,
					      gen_rtx_NEG (SImode,
							   masked),
					      masked));
	DONE;
      }

    rtx neg_op = gen_reg_rtx (SImode);
    rtx_insn *insn = emit_insn (gen_subsi3_compare0 (neg_op, const0_rtx,
						      operands[1]));

    /* Extract the condition register and mode.  */
    rtx cmp = XVECEXP (PATTERN (insn), 0, 0);
    rtx cc_reg = SET_DEST (cmp);
    rtx cond = gen_rtx_GE (SImode, cc_reg, const0_rtx);

    emit_insn (gen_andsi3 (operands[0], operands[1], mask));

    rtx masked_neg = gen_reg_rtx (SImode);
    emit_insn (gen_andsi3 (masked_neg, neg_op, mask));

    /* We want a conditional negate here, but emitting COND_EXEC rtxes
       during expand does not always work.  Do an IF_THEN_ELSE instead.  */
    emit_move_insn (operands[0],
		    gen_rtx_IF_THEN_ELSE (SImode, cond,
					  gen_rtx_NEG (SImode, masked_neg),
					  operands[0]));


    DONE;
  }
)

(define_expand "bswapsi2"
  [(set (match_operand:SI 0 "s_register_operand" "=r")
  	(bswap:SI (match_operand:SI 1 "s_register_operand" "r")))]
"TARGET_EITHER && (arm_arch6 || !optimize_size)"
"
    if (!arm_arch6)
      {
	rtx op2 = gen_reg_rtx (SImode);
	rtx op3 = gen_reg_rtx (SImode);

	if (TARGET_THUMB)
	  {
	    rtx op4 = gen_reg_rtx (SImode);
	    rtx op5 = gen_reg_rtx (SImode);

	    emit_insn (gen_thumb_legacy_rev (operands[0], operands[1],
					     op2, op3, op4, op5));
	  }
	else
	  {
	    emit_insn (gen_arm_legacy_rev (operands[0], operands[1],
					   op2, op3));
	  }

	DONE;
      }
  "
)

;; bswap16 patterns: use revsh and rev16 instructions for the signed
;; and unsigned variants, respectively. For rev16, expose
;; byte-swapping in the lower 16 bits only.
(define_insn "*arm_revsh"
  [(set (match_operand:SI 0 "s_register_operand" "=l,l,r")
	(sign_extend:SI (bswap:HI (match_operand:HI 1 "s_register_operand" "l,l,r"))))]
  "arm_arch6"
  "@
  revsh\t%0, %1
  revsh%?\t%0, %1
  revsh%?\t%0, %1"
  [(set_attr "arch" "t1,t2,32")
   (set_attr "length" "2,2,4")
   (set_attr "type" "rev")]
)

(define_insn "*arm_rev16"
  [(set (match_operand:HI 0 "s_register_operand" "=l,l,r")
	(bswap:HI (match_operand:HI 1 "s_register_operand" "l,l,r")))]
  "arm_arch6"
  "@
   rev16\t%0, %1
   rev16%?\t%0, %1
   rev16%?\t%0, %1"
  [(set_attr "arch" "t1,t2,32")
   (set_attr "length" "2,2,4")
   (set_attr "type" "rev")]
)

;; There are no canonicalisation rules for the position of the lshiftrt, ashift
;; operations within an IOR/AND RTX, therefore we have two patterns matching
;; each valid permutation.

(define_insn "arm_rev16si2"
  [(set (match_operand:SI 0 "register_operand" "=l,l,r")
        (ior:SI (and:SI (ashift:SI (match_operand:SI 1 "register_operand" "l,l,r")
                                   (const_int 8))
                        (match_operand:SI 3 "const_int_operand" "n,n,n"))
                (and:SI (lshiftrt:SI (match_dup 1)
                                     (const_int 8))
                        (match_operand:SI 2 "const_int_operand" "n,n,n"))))]
  "arm_arch6
   && aarch_rev16_shleft_mask_imm_p (operands[3], SImode)
   && aarch_rev16_shright_mask_imm_p (operands[2], SImode)"
  "rev16\\t%0, %1"
  [(set_attr "arch" "t1,t2,32")
   (set_attr "length" "2,2,4")
   (set_attr "type" "rev")]
)

(define_insn "arm_rev16si2_alt"
  [(set (match_operand:SI 0 "register_operand" "=l,l,r")
        (ior:SI (and:SI (lshiftrt:SI (match_operand:SI 1 "register_operand" "l,l,r")
                                     (const_int 8))
                        (match_operand:SI 2 "const_int_operand" "n,n,n"))
                (and:SI (ashift:SI (match_dup 1)
                                   (const_int 8))
                        (match_operand:SI 3 "const_int_operand" "n,n,n"))))]
  "arm_arch6
   && aarch_rev16_shleft_mask_imm_p (operands[3], SImode)
   && aarch_rev16_shright_mask_imm_p (operands[2], SImode)"
  "rev16\\t%0, %1"
  [(set_attr "arch" "t1,t2,32")
   (set_attr "length" "2,2,4")
   (set_attr "type" "rev")]
)

(define_expand "bswaphi2"
  [(set (match_operand:HI 0 "s_register_operand" "=r")
	(bswap:HI (match_operand:HI 1 "s_register_operand" "r")))]
"arm_arch6"
""
)

;; Patterns for LDRD/STRD in Thumb2 mode

(define_insn "*thumb2_ldrd"
  [(set (match_operand:SI 0 "s_register_operand" "=r")
        (mem:SI (plus:SI (match_operand:SI 1 "s_register_operand" "rk")
                         (match_operand:SI 2 "ldrd_strd_offset_operand" "Do"))))
   (set (match_operand:SI 3 "s_register_operand" "=r")
        (mem:SI (plus:SI (match_dup 1)
                         (match_operand:SI 4 "const_int_operand" ""))))]
  "TARGET_LDRD && TARGET_THUMB2 && reload_completed
     && current_tune->prefer_ldrd_strd
     && ((INTVAL (operands[2]) + 4) == INTVAL (operands[4]))
     && (operands_ok_ldrd_strd (operands[0], operands[3],
                                  operands[1], INTVAL (operands[2]),
                                  false, true))"
  "ldrd%?\t%0, %3, [%1, %2]"
  [(set_attr "type" "load2")
   (set_attr "predicable" "yes")
   (set_attr "predicable_short_it" "no")])

(define_insn "*thumb2_ldrd_base"
  [(set (match_operand:SI 0 "s_register_operand" "=r")
        (mem:SI (match_operand:SI 1 "s_register_operand" "rk")))
   (set (match_operand:SI 2 "s_register_operand" "=r")
        (mem:SI (plus:SI (match_dup 1)
                         (const_int 4))))]
  "TARGET_LDRD && TARGET_THUMB2 && reload_completed
     && current_tune->prefer_ldrd_strd
     && (operands_ok_ldrd_strd (operands[0], operands[2],
                                  operands[1], 0, false, true))"
  "ldrd%?\t%0, %2, [%1]"
  [(set_attr "type" "load2")
   (set_attr "predicable" "yes")
   (set_attr "predicable_short_it" "no")])

(define_insn "*thumb2_ldrd_base_neg"
  [(set (match_operand:SI 0 "s_register_operand" "=r")
	(mem:SI (plus:SI (match_operand:SI 1 "s_register_operand" "rk")
                         (const_int -4))))
   (set (match_operand:SI 2 "s_register_operand" "=r")
        (mem:SI (match_dup 1)))]
  "TARGET_LDRD && TARGET_THUMB2 && reload_completed
     && current_tune->prefer_ldrd_strd
     && (operands_ok_ldrd_strd (operands[0], operands[2],
                                  operands[1], -4, false, true))"
  "ldrd%?\t%0, %2, [%1, #-4]"
  [(set_attr "type" "load2")
   (set_attr "predicable" "yes")
   (set_attr "predicable_short_it" "no")])

(define_insn "*thumb2_strd"
  [(set (mem:SI (plus:SI (match_operand:SI 0 "s_register_operand" "rk")
                         (match_operand:SI 1 "ldrd_strd_offset_operand" "Do")))
        (match_operand:SI 2 "s_register_operand" "r"))
   (set (mem:SI (plus:SI (match_dup 0)
                         (match_operand:SI 3 "const_int_operand" "")))
        (match_operand:SI 4 "s_register_operand" "r"))]
  "TARGET_LDRD && TARGET_THUMB2 && reload_completed
     && current_tune->prefer_ldrd_strd
     && ((INTVAL (operands[1]) + 4) == INTVAL (operands[3]))
     && (operands_ok_ldrd_strd (operands[2], operands[4],
                                  operands[0], INTVAL (operands[1]),
                                  false, false))"
  "strd%?\t%2, %4, [%0, %1]"
  [(set_attr "type" "store2")
   (set_attr "predicable" "yes")
   (set_attr "predicable_short_it" "no")])

(define_insn "*thumb2_strd_base"
  [(set (mem:SI (match_operand:SI 0 "s_register_operand" "rk"))
        (match_operand:SI 1 "s_register_operand" "r"))
   (set (mem:SI (plus:SI (match_dup 0)
                         (const_int 4)))
        (match_operand:SI 2 "s_register_operand" "r"))]
  "TARGET_LDRD && TARGET_THUMB2 && reload_completed
     && current_tune->prefer_ldrd_strd
     && (operands_ok_ldrd_strd (operands[1], operands[2],
                                  operands[0], 0, false, false))"
  "strd%?\t%1, %2, [%0]"
  [(set_attr "type" "store2")
   (set_attr "predicable" "yes")
   (set_attr "predicable_short_it" "no")])

(define_insn "*thumb2_strd_base_neg"
  [(set (mem:SI (plus:SI (match_operand:SI 0 "s_register_operand" "rk")
                         (const_int -4)))
        (match_operand:SI 1 "s_register_operand" "r"))
   (set (mem:SI (match_dup 0))
        (match_operand:SI 2 "s_register_operand" "r"))]
  "TARGET_LDRD && TARGET_THUMB2 && reload_completed
     && current_tune->prefer_ldrd_strd
     && (operands_ok_ldrd_strd (operands[1], operands[2],
                                  operands[0], -4, false, false))"
  "strd%?\t%1, %2, [%0, #-4]"
  [(set_attr "type" "store2")
   (set_attr "predicable" "yes")
   (set_attr "predicable_short_it" "no")])

;; ARMv8 CRC32 instructions.
(define_insn "<crc_variant>"
  [(set (match_operand:SI 0 "s_register_operand" "=r")
        (unspec:SI [(match_operand:SI 1 "s_register_operand" "r")
                    (match_operand:<crc_mode> 2 "s_register_operand" "r")]
         CRC))]
  "TARGET_CRC32"
  "<crc_variant>\\t%0, %1, %2"
  [(set_attr "type" "crc")
   (set_attr "conds" "unconditional")]
)

;; Load the load/store double peephole optimizations.
(include "ldrdstrd.md")

;; Load the load/store multiple patterns
(include "ldmstm.md")

;; Patterns in ldmstm.md don't cover more than 4 registers. This pattern covers
;; large lists without explicit writeback generated for APCS_FRAME epilogue.
(define_insn "*load_multiple"
  [(match_parallel 0 "load_multiple_operation"
    [(set (match_operand:SI 2 "s_register_operand" "=rk")
          (mem:SI (match_operand:SI 1 "s_register_operand" "rk")))
        ])]
  "TARGET_32BIT"
  "*
  {
    arm_output_multireg_pop (operands, /*return_pc=*/false,
                                       /*cond=*/const_true_rtx,
                                       /*reverse=*/false,
                                       /*update=*/false);
    return \"\";
  }
  "
  [(set_attr "predicable" "yes")]
)

(define_expand "copysignsf3"
  [(match_operand:SF 0 "register_operand")
   (match_operand:SF 1 "register_operand")
   (match_operand:SF 2 "register_operand")]
  "TARGET_SOFT_FLOAT && arm_arch_thumb2"
  "{
     emit_move_insn (operands[0], operands[2]);
     emit_insn (gen_insv_t2 (simplify_gen_subreg (SImode, operands[0], SFmode, 0),
		GEN_INT (31), GEN_INT (0),
		simplify_gen_subreg (SImode, operands[1], SFmode, 0)));
     DONE;
  }"
)

(define_expand "copysigndf3"
  [(match_operand:DF 0 "register_operand")
   (match_operand:DF 1 "register_operand")
   (match_operand:DF 2 "register_operand")]
  "TARGET_SOFT_FLOAT && arm_arch_thumb2"
  "{
     rtx op0_low = gen_lowpart (SImode, operands[0]);
     rtx op0_high = gen_highpart (SImode, operands[0]);
     rtx op1_low = gen_lowpart (SImode, operands[1]);
     rtx op1_high = gen_highpart (SImode, operands[1]);
     rtx op2_high = gen_highpart (SImode, operands[2]);

     rtx scratch1 = gen_reg_rtx (SImode);
     rtx scratch2 = gen_reg_rtx (SImode);
     emit_move_insn (scratch1, op2_high);
     emit_move_insn (scratch2, op1_high);

     emit_insn(gen_rtx_SET(scratch1,
			   gen_rtx_LSHIFTRT (SImode, op2_high, GEN_INT(31))));
     emit_insn(gen_insv_t2(scratch2, GEN_INT(1), GEN_INT(31), scratch1));
     emit_move_insn (op0_low, op1_low);
     emit_move_insn (op0_high, scratch2);

     DONE;
  }"
)

;; movmisalign patterns for HImode and SImode.
(define_expand "movmisalign<mode>"
  [(match_operand:HSI 0 "general_operand")
   (match_operand:HSI 1 "general_operand")]
  "unaligned_access"
{
  /* This pattern is not permitted to fail during expansion: if both arguments
     are non-registers (e.g. memory := constant), force operand 1 into a
     register.  */
  rtx (* gen_unaligned_load)(rtx, rtx);
  rtx tmp_dest = operands[0];
  if (!s_register_operand (operands[0], <MODE>mode)
      && !s_register_operand (operands[1], <MODE>mode))
    operands[1] = force_reg (<MODE>mode, operands[1]);

  if (<MODE>mode == HImode)
   {
    gen_unaligned_load = gen_unaligned_loadhiu;
    tmp_dest = gen_reg_rtx (SImode);
   }
  else
    gen_unaligned_load = gen_unaligned_loadsi;

  if (MEM_P (operands[1]))
   {
    emit_insn (gen_unaligned_load (tmp_dest, operands[1]));
    if (<MODE>mode == HImode)
      emit_move_insn (operands[0], gen_lowpart (HImode, tmp_dest));
   }
  else
    emit_insn (gen_unaligned_store<mode> (operands[0], operands[1]));

  DONE;
})

;; Vector bits common to IWMMXT and Neon
(include "vec-common.md")
;; Load the Intel Wireless Multimedia Extension patterns
(include "iwmmxt.md")
;; Load the VFP co-processor patterns
(include "vfp.md")
;; Thumb-1 patterns
(include "thumb1.md")
;; Thumb-2 patterns
(include "thumb2.md")
;; Neon patterns
(include "neon.md")
;; Crypto patterns
(include "crypto.md")
;; Synchronization Primitives
(include "sync.md")
;; Fixed-point patterns
(include "arm-fixed.md")
