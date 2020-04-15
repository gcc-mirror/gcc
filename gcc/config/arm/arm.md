;;- Machine description for ARM for GNU compiler
;;  Copyright (C) 1991-2020 Free Software Foundation, Inc.
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
   (R4_REGNUM	      4)	; Fifth CORE register
   (FDPIC_REGNUM      9)	; FDPIC register
   (IP_REGNUM	     12)	; Scratch register
   (SP_REGNUM	     13)	; Stack pointer
   (LR_REGNUM        14)	; Return address register
   (PC_REGNUM	     15)	; Program counter
   (LAST_ARM_REGNUM  15)	;
   (CC_REGNUM       100)	; Condition code pseudo register
   (VFPCC_REGNUM    101)	; VFP Condition code pseudo register
   (APSRQ_REGNUM    104)	; Q bit pseudo register
   (APSRGE_REGNUM   105)	; GE bits pseudo register
   (VPR_REGNUM      106)	; Vector Predication Register - MVE register.
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

; Mark an instruction as suitable for "short IT" blocks in Thumb-2.
; The arm_restrict_it flag enables the "short IT" feature which
; restricts IT blocks to a single 16-bit instruction.
; This attribute should only be used on 16-bit Thumb-2 instructions
; which may be predicated (the "predicable" attribute must be set).
(define_attr "predicable_short_it" "no,yes" (const_string "no"))

; Mark an instruction as suitable for "short IT" blocks in Thumb-2.
; This attribute should only be used on instructions which may emit
; an IT block in their expansion which is not a short IT.
(define_attr "enabled_for_short_it" "no,yes" (const_string "yes"))

; Mark an instruction sequence as the required way of loading a
; constant when -mpure-code is enabled (which implies
; arm_disable_literal_pool)
(define_attr "required_for_purecode" "no,yes" (const_string "no"))

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
(define_attr "arch" "any,a,t,32,t1,t2,v6,nov6,v6t2,v8mb,iwmmxt,iwmmxt2,armv6_or_vfpv3,neon,mve"
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

	 (and (eq_attr "arch" "iwmmxt2")
	      (match_test "TARGET_REALLY_IWMMXT2"))
	 (const_string "yes")

	 (and (eq_attr "arch" "armv6_or_vfpv3")
	      (match_test "arm_arch6 || TARGET_VFP3"))
	 (const_string "yes")

	 (and (eq_attr "arch" "neon")
	      (match_test "TARGET_NEON"))
	 (const_string "yes")

	 (and (eq_attr "arch" "mve")
	      (match_test "TARGET_HAVE_MVE"))
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

	  (and (eq_attr "enabled_for_short_it" "no")
	       (match_test "arm_restrict_it"))
	  (const_string "no")

	  (and (eq_attr "required_for_purecode" "yes")
	       (not (match_test "arm_disable_literal_pool")))
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
; UNCONDITIONAL means the instruction cannot be conditionally executed and
;   that the instruction does not use or alter the condition codes.
;
; NOCOND means that the instruction does not use or alter the condition
;   codes but can be converted into a conditionally exectuted instruction.

(define_attr "conds" "use,set,clob,unconditional,nocond"
	(if_then_else
	 (ior (eq_attr "is_thumb1" "yes")
	      (eq_attr "type" "call"))
	 (const_string "clob")
         (if_then_else
	  (ior (eq_attr "is_neon_type" "yes")
	       (eq_attr "is_mve_type" "yes"))
	  (const_string "unconditional")
	  (const_string "nocond"))))

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
		 "block,call,load_4")
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
                                arm926ejs,arm10e,arm1026ejs,arm1136js,\
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
	       (eq_attr "tune" "!arm10e,cortexa5,cortexa7,\
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

;; define_subst and associated attributes

(define_subst "add_setq"
  [(set (match_operand:SI 0 "" "")
        (match_operand:SI 1 "" ""))]
  ""
  [(set (match_dup 0)
        (match_dup 1))
   (set (reg:CC APSRQ_REGNUM)
	(unspec:CC [(reg:CC APSRQ_REGNUM)] UNSPEC_Q_SET))])

(define_subst_attr "add_clobber_q_name" "add_setq" "" "_setq")
(define_subst_attr "add_clobber_q_pred" "add_setq" "!ARM_Q_BIT_READ"
		   "ARM_Q_BIT_READ")

;;---------------------------------------------------------------------------
;; Insn patterns
;;
;; Addition insns.

;; Note: For DImode insns, there is normally no reason why operands should
;; not be in the same register, what we don't want is for something being
;; written to partially overlap something that is an input.

(define_expand "adddi3"
 [(parallel
   [(set (match_operand:DI           0 "s_register_operand")
	  (plus:DI (match_operand:DI 1 "s_register_operand")
		   (match_operand:DI 2 "reg_or_int_operand")))
    (clobber (reg:CC CC_REGNUM))])]
  "TARGET_EITHER"
  "
  if (TARGET_THUMB1)
    {
      if (!REG_P (operands[2]))
	operands[2] = force_reg (DImode, operands[2]);
    }
  else
    {
      rtx lo_result, hi_result, lo_dest, hi_dest;
      rtx lo_op1, hi_op1, lo_op2, hi_op2;
      arm_decompose_di_binop (operands[1], operands[2], &lo_op1, &hi_op1,
			      &lo_op2, &hi_op2);
      lo_result = lo_dest = gen_lowpart (SImode, operands[0]);
      hi_result = hi_dest = gen_highpart (SImode, operands[0]);

      if (lo_op2 == const0_rtx)
	{
	  lo_dest = lo_op1;
	  if (!arm_add_operand (hi_op2, SImode))
	    hi_op2 = force_reg (SImode, hi_op2);
	  /* Assume hi_op2 won't also be zero.  */
	  emit_insn (gen_addsi3 (hi_dest, hi_op1, hi_op2));
	}
      else
	{
	  if (!arm_add_operand (lo_op2, SImode))
	    lo_op2 = force_reg (SImode, lo_op2);
	  if (!arm_not_operand (hi_op2, SImode))
	    hi_op2 = force_reg (SImode, hi_op2);

	  emit_insn (gen_addsi3_compare_op1 (lo_dest, lo_op1, lo_op2));
	  rtx carry = gen_rtx_LTU (SImode, gen_rtx_REG (CC_Cmode, CC_REGNUM),
				   const0_rtx);
	  if (hi_op2 == const0_rtx)
	    emit_insn (gen_add0si3_carryin (hi_dest, hi_op1, carry));
	  else
	    emit_insn (gen_addsi3_carryin (hi_dest, hi_op1, hi_op2, carry));
	}

      if (lo_result != lo_dest)
	emit_move_insn (lo_result, lo_dest);
      if (hi_result != hi_dest)
	emit_move_insn (gen_highpart (SImode, operands[0]), hi_dest);
      DONE;
    }
  "
)

(define_expand "addvsi4"
  [(match_operand:SI 0 "s_register_operand")
   (match_operand:SI 1 "s_register_operand")
   (match_operand:SI 2 "arm_add_operand")
   (match_operand 3 "")]
  "TARGET_32BIT"
{
  if (CONST_INT_P (operands[2]))
    emit_insn (gen_addsi3_compareV_imm (operands[0], operands[1], operands[2]));
  else
    emit_insn (gen_addsi3_compareV_reg (operands[0], operands[1], operands[2]));
  arm_gen_unlikely_cbranch (NE, CC_Vmode, operands[3]);

  DONE;
})

(define_expand "addvdi4"
  [(match_operand:DI 0 "s_register_operand")
   (match_operand:DI 1 "s_register_operand")
   (match_operand:DI 2 "reg_or_int_operand")
   (match_operand 3 "")]
  "TARGET_32BIT"
{
  rtx lo_result, hi_result;
  rtx lo_op1, hi_op1, lo_op2, hi_op2;
  arm_decompose_di_binop (operands[1], operands[2], &lo_op1, &hi_op1,
			  &lo_op2, &hi_op2);
  lo_result = gen_lowpart (SImode, operands[0]);
  hi_result = gen_highpart (SImode, operands[0]);

  if (lo_op2 == const0_rtx)
    {
      emit_move_insn (lo_result, lo_op1);
      if (!arm_add_operand (hi_op2, SImode))
	hi_op2 = force_reg (SImode, hi_op2);

      emit_insn (gen_addvsi4 (hi_result, hi_op1, hi_op2, operands[3]));
    }
  else
    {
      if (!arm_add_operand (lo_op2, SImode))
	lo_op2 = force_reg (SImode, lo_op2);
      if (!arm_not_operand (hi_op2, SImode))
	hi_op2 = force_reg (SImode, hi_op2);

      emit_insn (gen_addsi3_compare_op1 (lo_result, lo_op1, lo_op2));

      if (hi_op2 == const0_rtx)
        emit_insn (gen_addsi3_cin_vout_0 (hi_result, hi_op1));
      else if (CONST_INT_P (hi_op2))
        emit_insn (gen_addsi3_cin_vout_imm (hi_result, hi_op1, hi_op2));
      else
        emit_insn (gen_addsi3_cin_vout_reg (hi_result, hi_op1, hi_op2));

      arm_gen_unlikely_cbranch (NE, CC_Vmode, operands[3]);
    }

  DONE;
})

(define_expand "addsi3_cin_vout_reg"
  [(parallel
    [(set (match_dup 3)
	  (compare:CC_V
	   (plus:DI
	    (plus:DI (match_dup 4)
		     (sign_extend:DI (match_operand:SI 1 "s_register_operand")))
	    (sign_extend:DI (match_operand:SI 2 "s_register_operand")))
	   (sign_extend:DI (plus:SI (plus:SI (match_dup 5) (match_dup 1))
				    (match_dup 2)))))
     (set (match_operand:SI 0 "s_register_operand")
	  (plus:SI (plus:SI (match_dup 5) (match_dup 1))
		   (match_dup 2)))])]
  "TARGET_32BIT"
  {
    operands[3] = gen_rtx_REG (CC_Vmode, CC_REGNUM);
    rtx ccin = gen_rtx_REG (CC_Cmode, CC_REGNUM);
    operands[4] = gen_rtx_LTU (DImode, ccin, const0_rtx);
    operands[5] = gen_rtx_LTU (SImode, ccin, const0_rtx);
  }
)

(define_insn "*addsi3_cin_vout_reg_insn"
  [(set (reg:CC_V CC_REGNUM)
	(compare:CC_V
	 (plus:DI
	  (plus:DI
	   (match_operand:DI 3 "arm_carry_operation" "")
	   (sign_extend:DI (match_operand:SI 1 "s_register_operand" "%0,r")))
	  (sign_extend:DI (match_operand:SI 2 "s_register_operand" "l,r")))
	 (sign_extend:DI
	  (plus:SI (plus:SI (match_operand:SI 4 "arm_carry_operation" "")
			    (match_dup 1))
		   (match_dup 2)))))
   (set (match_operand:SI 0 "s_register_operand" "=l,r")
	(plus:SI (plus:SI (match_dup 4) (match_dup 1))
		 (match_dup 2)))]
  "TARGET_32BIT"
  "@
   adcs%?\\t%0, %0, %2
   adcs%?\\t%0, %1, %2"
  [(set_attr "type" "alus_sreg")
   (set_attr "arch" "t2,*")
   (set_attr "length" "2,4")]
)

(define_expand "addsi3_cin_vout_imm"
  [(parallel
    [(set (match_dup 3)
	  (compare:CC_V
	   (plus:DI
	    (plus:DI (match_dup 4)
		     (sign_extend:DI (match_operand:SI 1 "s_register_operand")))
	    (match_dup 2))
	   (sign_extend:DI (plus:SI (plus:SI (match_dup 5) (match_dup 1))
				    (match_dup 2)))))
     (set (match_operand:SI 0 "s_register_operand")
	  (plus:SI (plus:SI (match_dup 5) (match_dup 1))
		   (match_operand 2 "arm_adcimm_operand")))])]
  "TARGET_32BIT"
  {
    operands[3] = gen_rtx_REG (CC_Vmode, CC_REGNUM);
    rtx ccin = gen_rtx_REG (CC_Cmode, CC_REGNUM);
    operands[4] = gen_rtx_LTU (DImode, ccin, const0_rtx);
    operands[5] = gen_rtx_LTU (SImode, ccin, const0_rtx);
  }
)

(define_insn "*addsi3_cin_vout_imm_insn"
  [(set (reg:CC_V CC_REGNUM)
	(compare:CC_V
	 (plus:DI
	  (plus:DI
	   (match_operand:DI 3 "arm_carry_operation" "")
	   (sign_extend:DI (match_operand:SI 1 "s_register_operand" "r,r")))
	  (match_operand 2 "arm_adcimm_operand" "I,K"))
	 (sign_extend:DI
	  (plus:SI (plus:SI (match_operand:SI 4 "arm_carry_operation" "")
			    (match_dup 1))
		   (match_dup 2)))))
   (set (match_operand:SI 0 "s_register_operand" "=r,r")
	(plus:SI (plus:SI (match_dup 4) (match_dup 1))
		 (match_dup 2)))]
  "TARGET_32BIT"
  "@
   adcs%?\\t%0, %1, %2
   sbcs%?\\t%0, %1, #%B2"
  [(set_attr "type" "alus_imm")]
)

(define_expand "addsi3_cin_vout_0"
  [(parallel
    [(set (match_dup 2)
	  (compare:CC_V
	   (plus:DI (match_dup 3)
		    (sign_extend:DI (match_operand:SI 1 "s_register_operand")))
	   (sign_extend:DI (plus:SI (match_dup 4) (match_dup 1)))))
     (set (match_operand:SI 0 "s_register_operand")
	  (plus:SI (match_dup 4) (match_dup 1)))])]
  "TARGET_32BIT"
  {
    operands[2] = gen_rtx_REG (CC_Vmode, CC_REGNUM);
    rtx ccin = gen_rtx_REG (CC_Cmode, CC_REGNUM);
    operands[3] = gen_rtx_LTU (DImode, ccin, const0_rtx);
    operands[4] = gen_rtx_LTU (SImode, ccin, const0_rtx);
  }
)

(define_insn "*addsi3_cin_vout_0_insn"
  [(set (reg:CC_V CC_REGNUM)
	(compare:CC_V
	 (plus:DI
	  (match_operand:DI 2 "arm_carry_operation" "")
	  (sign_extend:DI (match_operand:SI 1 "s_register_operand" "r")))
	 (sign_extend:DI (plus:SI
			  (match_operand:SI 3 "arm_carry_operation" "")
			  (match_dup 1)))))
   (set (match_operand:SI 0 "s_register_operand" "=r")
	(plus:SI (match_dup 3) (match_dup 1)))]
  "TARGET_32BIT"
  "adcs%?\\t%0, %1, #0"
  [(set_attr "type" "alus_imm")]
)

(define_expand "uaddvsi4"
  [(match_operand:SI 0 "s_register_operand")
   (match_operand:SI 1 "s_register_operand")
   (match_operand:SI 2 "arm_add_operand")
   (match_operand 3 "")]
  "TARGET_32BIT"
{
  emit_insn (gen_addsi3_compare_op1 (operands[0], operands[1], operands[2]));
  arm_gen_unlikely_cbranch (LTU, CC_Cmode, operands[3]);

  DONE;
})

(define_expand "uaddvdi4"
  [(match_operand:DI 0 "s_register_operand")
   (match_operand:DI 1 "s_register_operand")
   (match_operand:DI 2 "reg_or_int_operand")
   (match_operand 3 "")]
  "TARGET_32BIT"
{
  rtx lo_result, hi_result;
  rtx lo_op1, hi_op1, lo_op2, hi_op2;
  arm_decompose_di_binop (operands[1], operands[2], &lo_op1, &hi_op1,
			  &lo_op2, &hi_op2);
  lo_result = gen_lowpart (SImode, operands[0]);
  hi_result = gen_highpart (SImode, operands[0]);

  if (lo_op2 == const0_rtx)
    {
      emit_move_insn (lo_result, lo_op1);
      if (!arm_add_operand (hi_op2, SImode))
	hi_op2 = force_reg (SImode, hi_op2);

      emit_insn (gen_uaddvsi4 (hi_result, hi_op1, hi_op2, operands[3]));
    }
  else
    {
      if (!arm_add_operand (lo_op2, SImode))
	lo_op2 = force_reg (SImode, lo_op2);
      if (!arm_not_operand (hi_op2, SImode))
	hi_op2 = force_reg (SImode, hi_op2);

      emit_insn (gen_addsi3_compare_op1 (lo_result, lo_op1, lo_op2));

      if (hi_op2 == const0_rtx)
        emit_insn (gen_addsi3_cin_cout_0 (hi_result, hi_op1));
      else if (CONST_INT_P (hi_op2))
        emit_insn (gen_addsi3_cin_cout_imm (hi_result, hi_op1, hi_op2));
      else
        emit_insn (gen_addsi3_cin_cout_reg (hi_result, hi_op1, hi_op2));

      arm_gen_unlikely_cbranch (GEU, CC_ADCmode, operands[3]);
    }

  DONE;
})

(define_expand "addsi3_cin_cout_reg"
  [(parallel
    [(set (match_dup 3)
	  (compare:CC_ADC
	   (plus:DI
	    (plus:DI (match_dup 4)
		     (zero_extend:DI (match_operand:SI 1 "s_register_operand")))
	    (zero_extend:DI (match_operand:SI 2 "s_register_operand")))
	   (const_int 4294967296)))
     (set (match_operand:SI 0 "s_register_operand")
	  (plus:SI (plus:SI (match_dup 5) (match_dup 1))
		   (match_dup 2)))])]
  "TARGET_32BIT"
  {
    operands[3] = gen_rtx_REG (CC_ADCmode, CC_REGNUM);
    rtx ccin = gen_rtx_REG (CC_Cmode, CC_REGNUM);
    operands[4] = gen_rtx_LTU (DImode, ccin, const0_rtx);
    operands[5] = gen_rtx_LTU (SImode, ccin, const0_rtx);
  }
)

(define_insn "*addsi3_cin_cout_reg_insn"
  [(set (reg:CC_ADC CC_REGNUM)
	(compare:CC_ADC
	 (plus:DI
	  (plus:DI
	   (match_operand:DI 3 "arm_carry_operation" "")
	   (zero_extend:DI (match_operand:SI 1 "s_register_operand" "%0,r")))
	  (zero_extend:DI (match_operand:SI 2 "s_register_operand" "l,r")))
	(const_int 4294967296)))
   (set (match_operand:SI 0 "s_register_operand" "=l,r")
	(plus:SI (plus:SI (match_operand:SI 4 "arm_carry_operation" "")
			  (match_dup 1))
		 (match_dup 2)))]
  "TARGET_32BIT"
  "@
   adcs%?\\t%0, %0, %2
   adcs%?\\t%0, %1, %2"
  [(set_attr "type" "alus_sreg")
   (set_attr "arch" "t2,*")
   (set_attr "length" "2,4")]
)

(define_expand "addsi3_cin_cout_imm"
  [(parallel
    [(set (match_dup 3)
	  (compare:CC_ADC
	   (plus:DI
	    (plus:DI (match_dup 4)
		     (zero_extend:DI (match_operand:SI 1 "s_register_operand")))
	    (match_dup 6))
	   (const_int 4294967296)))
     (set (match_operand:SI 0 "s_register_operand")
	  (plus:SI (plus:SI (match_dup 5) (match_dup 1))
		   (match_operand:SI 2 "arm_adcimm_operand")))])]
  "TARGET_32BIT"
  {
    operands[3] = gen_rtx_REG (CC_ADCmode, CC_REGNUM);
    rtx ccin = gen_rtx_REG (CC_Cmode, CC_REGNUM);
    operands[4] = gen_rtx_LTU (DImode, ccin, const0_rtx);
    operands[5] = gen_rtx_LTU (SImode, ccin, const0_rtx);
    operands[6] = GEN_INT (UINTVAL (operands[2]) & 0xffffffff);
  }
)

(define_insn "*addsi3_cin_cout_imm_insn"
  [(set (reg:CC_ADC CC_REGNUM)
	(compare:CC_ADC
	 (plus:DI
	  (plus:DI
	   (match_operand:DI 3 "arm_carry_operation" "")
	   (zero_extend:DI (match_operand:SI 1 "s_register_operand" "r,r")))
	  (match_operand:DI 5 "const_int_operand" "n,n"))
	(const_int 4294967296)))
   (set (match_operand:SI 0 "s_register_operand" "=r,r")
	(plus:SI (plus:SI (match_operand:SI 4 "arm_carry_operation" "")
			  (match_dup 1))
		 (match_operand:SI 2 "arm_adcimm_operand" "I,K")))]
  "TARGET_32BIT
   && (UINTVAL (operands[2]) & 0xffffffff) == UINTVAL (operands[5])"
  "@
   adcs%?\\t%0, %1, %2
   sbcs%?\\t%0, %1, #%B2"
  [(set_attr "type" "alus_imm")]
)

(define_expand "addsi3_cin_cout_0"
  [(parallel
    [(set (match_dup 2)
	  (compare:CC_ADC
	   (plus:DI (match_dup 3)
		    (zero_extend:DI (match_operand:SI 1 "s_register_operand")))
	   (const_int 4294967296)))
     (set (match_operand:SI 0 "s_register_operand")
	  (plus:SI (match_dup 4) (match_dup 1)))])]
  "TARGET_32BIT"
  {
    operands[2] = gen_rtx_REG (CC_ADCmode, CC_REGNUM);
    rtx ccin = gen_rtx_REG (CC_Cmode, CC_REGNUM);
    operands[3] = gen_rtx_LTU (DImode, ccin, const0_rtx);
    operands[4] = gen_rtx_LTU (SImode, ccin, const0_rtx);
  }
)

(define_insn "*addsi3_cin_cout_0_insn"
  [(set (reg:CC_ADC CC_REGNUM)
	(compare:CC_ADC
	 (plus:DI
	  (match_operand:DI 2 "arm_carry_operation" "")
	  (zero_extend:DI (match_operand:SI 1 "s_register_operand" "r")))
	(const_int 4294967296)))
   (set (match_operand:SI 0 "s_register_operand" "=r")
	(plus:SI (match_operand:SI 3 "arm_carry_operation" "") (match_dup 1)))]
  "TARGET_32BIT"
  "adcs%?\\t%0, %1, #0"
  [(set_attr "type" "alus_imm")]
)

(define_expand "addsi3"
  [(set (match_operand:SI          0 "s_register_operand")
	(plus:SI (match_operand:SI 1 "s_register_operand")
		 (match_operand:SI 2 "reg_or_int_operand")))]
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
  [(set (match_operand:SI          0 "s_register_operand" "=rk,l,l ,l ,r ,k ,r,k ,r ,k ,r ,k,k,r ,k ,r")
	(plus:SI (match_operand:SI 1 "s_register_operand" "%0 ,l,0 ,l ,rk,k ,r,r ,rk,k ,rk,k,r,rk,k ,rk")
		 (match_operand:SI 2 "reg_or_int_operand" "rk ,l,Py,Pd,rI,rI,k,rI,Pj,Pj,L ,L,L,PJ,PJ,?n")))]
  "TARGET_32BIT"
  "@
   add%?\\t%0, %0, %2
   add%?\\t%0, %1, %2
   add%?\\t%0, %1, %2
   add%?\\t%0, %1, %2
   add%?\\t%0, %1, %2
   add%?\\t%0, %1, %2
   add%?\\t%0, %2, %1
   add%?\\t%0, %1, %2
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
  [(set_attr "length" "2,4,4,4,4,4,4,4,4,4,4,4,4,4,4,16")
   (set_attr "predicable" "yes")
   (set_attr "predicable_short_it" "yes,yes,yes,yes,no,no,no,no,no,no,no,no,no,no,no,no")
   (set_attr "arch" "t2,t2,t2,t2,*,*,*,a,t2,t2,*,*,a,t2,t2,*")
   (set (attr "type") (if_then_else (match_operand 2 "const_int_operand" "")
		      (const_string "alu_imm")
		      (const_string "alu_sreg")))
 ]
)

(define_insn "addsi3_compareV_reg"
  [(set (reg:CC_V CC_REGNUM)
	(compare:CC_V
	  (plus:DI
	    (sign_extend:DI (match_operand:SI 1 "register_operand" "%l,0,r"))
	    (sign_extend:DI (match_operand:SI 2 "register_operand" "l,r,r")))
	  (sign_extend:DI (plus:SI (match_dup 1) (match_dup 2)))))
   (set (match_operand:SI 0 "register_operand" "=l,r,r")
	(plus:SI (match_dup 1) (match_dup 2)))]
  "TARGET_32BIT"
  "adds%?\\t%0, %1, %2"
  [(set_attr "conds" "set")
   (set_attr "arch" "t2,t2,*")
   (set_attr "length" "2,2,4")
   (set_attr "type" "alus_sreg")]
)

(define_insn "*addsi3_compareV_reg_nosum"
  [(set (reg:CC_V CC_REGNUM)
	(compare:CC_V
	  (plus:DI
	    (sign_extend:DI (match_operand:SI 0 "register_operand" "%l,r"))
	    (sign_extend:DI (match_operand:SI 1 "register_operand" "l,r")))
	  (sign_extend:DI (plus:SI (match_dup 0) (match_dup 1)))))]
  "TARGET_32BIT"
  "cmn%?\\t%0, %1"
  [(set_attr "conds" "set")
   (set_attr "arch" "t2,*")
   (set_attr "length" "2,4")
   (set_attr "type" "alus_sreg")]
)

(define_insn "subvsi3_intmin"
  [(set (reg:CC_V CC_REGNUM)
	(compare:CC_V
	  (plus:DI
	    (sign_extend:DI
	     (match_operand:SI 1 "register_operand" "r"))
	    (const_int 2147483648))
	  (sign_extend:DI (plus:SI (match_dup 1) (const_int -2147483648)))))
   (set (match_operand:SI 0 "register_operand" "=r")
	(plus:SI (match_dup 1) (const_int -2147483648)))]
  "TARGET_32BIT"
  "subs%?\\t%0, %1, #-2147483648"
  [(set_attr "conds" "set")
   (set_attr "type" "alus_imm")]
)

(define_insn "addsi3_compareV_imm"
  [(set (reg:CC_V CC_REGNUM)
	(compare:CC_V
	  (plus:DI
	    (sign_extend:DI
	     (match_operand:SI 1 "register_operand" "l,0,l,0,r,r"))
	    (match_operand 2 "arm_addimm_operand" "Pd,Py,Px,Pw,I,L"))
	  (sign_extend:DI (plus:SI (match_dup 1) (match_dup 2)))))
   (set (match_operand:SI 0 "register_operand" "=l,l,l,l,r,r")
	(plus:SI (match_dup 1) (match_dup 2)))]
  "TARGET_32BIT
   && INTVAL (operands[2]) == ARM_SIGN_EXTEND (INTVAL (operands[2]))"
  "@
   adds%?\\t%0, %1, %2
   adds%?\\t%0, %0, %2
   subs%?\\t%0, %1, #%n2
   subs%?\\t%0, %0, #%n2
   adds%?\\t%0, %1, %2
   subs%?\\t%0, %1, #%n2"
  [(set_attr "conds" "set")
   (set_attr "arch" "t2,t2,t2,t2,*,*")
   (set_attr "length" "2,2,2,2,4,4")
   (set_attr "type" "alus_imm")]
)

(define_insn "addsi3_compareV_imm_nosum"
  [(set (reg:CC_V CC_REGNUM)
	(compare:CC_V
	  (plus:DI
	    (sign_extend:DI
	     (match_operand:SI 0 "register_operand" "l,r,r"))
	    (match_operand 1 "arm_addimm_operand" "Pw,I,L"))
	  (sign_extend:DI (plus:SI (match_dup 0) (match_dup 1)))))]
  "TARGET_32BIT
   && INTVAL (operands[1]) == ARM_SIGN_EXTEND (INTVAL (operands[1]))"
  "@
   cmp%?\\t%0, #%n1
   cmn%?\\t%0, %1
   cmp%?\\t%0, #%n1"
  [(set_attr "conds" "set")
   (set_attr "arch" "t2,*,*")
   (set_attr "length" "2,4,4")
   (set_attr "type" "alus_imm")]
)

;; We can handle more constants efficently if we can clobber either a scratch
;; or the other source operand.  We deliberately leave this late as in
;; high register pressure situations it's not worth forcing any reloads.
(define_peephole2
  [(match_scratch:SI 2 "l")
   (set (reg:CC_V CC_REGNUM)
	(compare:CC_V
	  (plus:DI
	    (sign_extend:DI
	     (match_operand:SI 0 "low_register_operand"))
	    (match_operand 1 "const_int_operand"))
	  (sign_extend:DI (plus:SI (match_dup 0) (match_dup 1)))))]
  "TARGET_THUMB2
   && satisfies_constraint_Pd (operands[1])"
  [(parallel[
    (set (reg:CC_V CC_REGNUM)
	 (compare:CC_V
	  (plus:DI (sign_extend:DI (match_dup 0))
		   (sign_extend:DI (match_dup 1)))
	  (sign_extend:DI (plus:SI (match_dup 0) (match_dup 1)))))
    (set (match_dup 2) (plus:SI (match_dup 0) (match_dup 1)))])]
)

(define_peephole2
  [(set (reg:CC_V CC_REGNUM)
	(compare:CC_V
	  (plus:DI
	    (sign_extend:DI
	     (match_operand:SI 0 "low_register_operand"))
	    (match_operand 1 "const_int_operand"))
	  (sign_extend:DI (plus:SI (match_dup 0) (match_dup 1)))))]
  "TARGET_THUMB2
   && dead_or_set_p (peep2_next_insn (0), operands[0])
   && satisfies_constraint_Py (operands[1])"
  [(parallel[
    (set (reg:CC_V CC_REGNUM)
	 (compare:CC_V
	  (plus:DI (sign_extend:DI (match_dup 0))
		   (sign_extend:DI (match_dup 1)))
	  (sign_extend:DI (plus:SI (match_dup 0) (match_dup 1)))))
    (set (match_dup 0) (plus:SI (match_dup 0) (match_dup 1)))])]
)

(define_insn "addsi3_compare0"
  [(set (reg:CC_NZ CC_REGNUM)
	(compare:CC_NZ
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
  [(set (reg:CC_NZ CC_REGNUM)
	(compare:CC_NZ
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

;; This is the canonicalization of subsi3_compare when the
;; addend is a constant.
(define_insn "cmpsi2_addneg"
  [(set (reg:CC CC_REGNUM)
	(compare:CC
	 (match_operand:SI 1 "s_register_operand" "r,r")
	 (match_operand:SI 2 "arm_addimm_operand" "I,L")))
   (set (match_operand:SI 0 "s_register_operand" "=r,r")
	(plus:SI (match_dup 1)
		 (match_operand:SI 3 "arm_addimm_operand" "L,I")))]
  "TARGET_32BIT
   && (INTVAL (operands[2])
       == trunc_int_for_mode (-INTVAL (operands[3]), SImode))"
{
  /* For 0 and INT_MIN it is essential that we use subs, as adds will result
     in different condition codes (like cmn rather than like cmp), so that
     alternative comes first.  Both alternatives can match for any 0x??000000
     where except for 0 and INT_MIN it doesn't matter what we choose, and also
     for -1 and 1 with TARGET_THUMB2, in that case prefer instruction with #1
     as it is shorter.  */
  if (which_alternative == 0 && operands[3] != const1_rtx)
    return "subs%?\\t%0, %1, #%n3";
  else
    return "adds%?\\t%0, %1, %3";
}
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
(define_insn "addsi3_compare_op1"
  [(set (reg:CC_C CC_REGNUM)
	(compare:CC_C
	 (plus:SI (match_operand:SI 1 "s_register_operand" "l,0,l,0,rk,rk")
		  (match_operand:SI 2 "arm_add_operand" "lPd,Py,lPx,Pw,rkI,L"))
	 (match_dup 1)))
   (set (match_operand:SI 0 "s_register_operand" "=l,l,l,l,rk,rk")
	(plus:SI (match_dup 1) (match_dup 2)))]
  "TARGET_32BIT"
  "@
   adds%?\\t%0, %1, %2
   adds%?\\t%0, %0, %2
   subs%?\\t%0, %1, #%n2
   subs%?\\t%0, %0, #%n2
   adds%?\\t%0, %1, %2
   subs%?\\t%0, %1, #%n2"
  [(set_attr "conds" "set")
   (set_attr "arch" "t2,t2,t2,t2,*,*")
   (set_attr "length" "2,2,2,2,4,4")
   (set (attr "type")
	(if_then_else (match_operand 2 "const_int_operand")
		      (const_string "alu_imm")
		      (const_string "alu_sreg")))]
)

(define_insn "*addsi3_compare_op2"
  [(set (reg:CC_C CC_REGNUM)
	(compare:CC_C
	 (plus:SI (match_operand:SI 1 "s_register_operand" "l,0,l,0,r,r")
		  (match_operand:SI 2 "arm_add_operand" "lPd,Py,lPx,Pw,rI,L"))
	 (match_dup 2)))
   (set (match_operand:SI 0 "s_register_operand" "=l,l,l,l,r,r")
	(plus:SI (match_dup 1) (match_dup 2)))]
  "TARGET_32BIT"
  "@
   adds%?\\t%0, %1, %2
   adds%?\\t%0, %0, %2
   subs%?\\t%0, %1, #%n2
   subs%?\\t%0, %0, #%n2
   adds%?\\t%0, %1, %2
   subs%?\\t%0, %1, #%n2"
  [(set_attr "conds" "set")
   (set_attr "arch" "t2,t2,t2,t2,*,*")
   (set_attr "length" "2,2,2,2,4,4")
   (set (attr "type")
	(if_then_else (match_operand 2 "const_int_operand")
		      (const_string "alu_imm")
		      (const_string "alu_sreg")))]
)

(define_insn "*compare_addsi2_op0"
  [(set (reg:CC_C CC_REGNUM)
        (compare:CC_C
          (plus:SI (match_operand:SI 0 "s_register_operand" "l,l,r,r")
                   (match_operand:SI 1 "arm_add_operand"    "l,Pw,rI,L"))
          (match_dup 0)))]
  "TARGET_32BIT"
  "@
   cmn%?\\t%0, %1
   cmp%?\\t%0, #%n1
   cmn%?\\t%0, %1
   cmp%?\\t%0, #%n1"
  [(set_attr "conds" "set")
   (set_attr "predicable" "yes")
   (set_attr "arch" "t2,t2,*,*")
   (set_attr "predicable_short_it" "yes,yes,no,no")
   (set_attr "length" "2,2,4,4")
   (set (attr "type")
	(if_then_else (match_operand 1 "const_int_operand")
		      (const_string "alu_imm")
		      (const_string "alu_sreg")))]
)

(define_insn "*compare_addsi2_op1"
  [(set (reg:CC_C CC_REGNUM)
        (compare:CC_C
          (plus:SI (match_operand:SI 0 "s_register_operand" "l,l,r,r")
                   (match_operand:SI 1 "arm_add_operand" "l,Pw,rI,L"))
          (match_dup 1)))]
  "TARGET_32BIT"
  "@
   cmn%?\\t%0, %1
   cmp%?\\t%0, #%n1
   cmn%?\\t%0, %1
   cmp%?\\t%0, #%n1"
  [(set_attr "conds" "set")
   (set_attr "predicable" "yes")
   (set_attr "arch" "t2,t2,*,*")
   (set_attr "predicable_short_it" "yes,yes,no,no")
   (set_attr "length" "2,2,4,4")
   (set (attr "type")
	(if_then_else (match_operand 1 "const_int_operand")
		      (const_string "alu_imm")
		      (const_string "alu_sreg")))]
 )

(define_insn "addsi3_carryin"
  [(set (match_operand:SI 0 "s_register_operand" "=l,r,r")
        (plus:SI (plus:SI (match_operand:SI 1 "s_register_operand" "%l,r,r")
                          (match_operand:SI 2 "arm_not_operand" "0,rI,K"))
                 (match_operand:SI 3 "arm_carry_operation" "")))]
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

;; Canonicalization of the above when the immediate is zero.
(define_insn "add0si3_carryin"
  [(set (match_operand:SI 0 "s_register_operand" "=r")
	(plus:SI (match_operand:SI 2 "arm_carry_operation" "")
		 (match_operand:SI 1 "arm_not_operand" "r")))]
  "TARGET_32BIT"
  "adc%?\\t%0, %1, #0"
  [(set_attr "conds" "use")
   (set_attr "predicable" "yes")
   (set_attr "length" "4")
   (set_attr "type" "adc_imm")]
)

(define_insn "*addsi3_carryin_alt2"
  [(set (match_operand:SI 0 "s_register_operand" "=l,r,r")
        (plus:SI (plus:SI (match_operand:SI 3 "arm_carry_operation" "")
                          (match_operand:SI 1 "s_register_operand" "%l,r,r"))
                 (match_operand:SI 2 "arm_not_operand" "l,rI,K")))]
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

(define_insn "*addsi3_carryin_shift"
  [(set (match_operand:SI 0 "s_register_operand" "=r,r")
	(plus:SI (plus:SI
		  (match_operator:SI 2 "shift_operator"
		    [(match_operand:SI 3 "s_register_operand" "r,r")
		     (match_operand:SI 4 "shift_amount_operand" "M,r")])
		  (match_operand:SI 5 "arm_carry_operation" ""))
		 (match_operand:SI 1 "s_register_operand" "r,r")))]
  "TARGET_32BIT"
  "adc%?\\t%0, %1, %3%S2"
  [(set_attr "conds" "use")
   (set_attr "arch" "32,a")
   (set_attr "shift" "3")
   (set_attr "predicable" "yes")
   (set_attr "type" "alu_shift_imm,alu_shift_reg")]
)

(define_insn "*addsi3_carryin_clobercc"
  [(set (match_operand:SI 0 "s_register_operand" "=r")
	(plus:SI (plus:SI (match_operand:SI 1 "s_register_operand" "%r")
			  (match_operand:SI 2 "arm_rhs_operand" "rI"))
		 (match_operand:SI 3 "arm_carry_operation" "")))
   (clobber (reg:CC CC_REGNUM))]
   "TARGET_32BIT"
   "adcs%?\\t%0, %1, %2"
   [(set_attr "conds" "set")
    (set_attr "type" "adcs_reg")]
)

(define_expand "subvsi4"
  [(match_operand:SI 0 "s_register_operand")
   (match_operand:SI 1 "arm_rhs_operand")
   (match_operand:SI 2 "arm_add_operand")
   (match_operand 3 "")]
  "TARGET_32BIT"
{
  if (CONST_INT_P (operands[1]) && CONST_INT_P (operands[2]))
    {
      /* If both operands are constants we can decide the result statically.  */
      wi::overflow_type overflow;
      wide_int val = wi::sub (rtx_mode_t (operands[1], SImode),
			      rtx_mode_t (operands[2], SImode),
			      SIGNED, &overflow);
      emit_move_insn (operands[0], GEN_INT (val.to_shwi ()));
      if (overflow != wi::OVF_NONE)
	emit_jump_insn (gen_jump (operands[3]));
      DONE;
    }
  else if (CONST_INT_P (operands[2]))
    {
      operands[2] = GEN_INT (-INTVAL (operands[2]));
      /* Special case for INT_MIN.  */
      if (INTVAL (operands[2]) == 0x80000000)
	emit_insn (gen_subvsi3_intmin (operands[0], operands[1]));
      else
	emit_insn (gen_addsi3_compareV_imm (operands[0], operands[1],
					  operands[2]));
    }
  else if (CONST_INT_P (operands[1]))
    emit_insn (gen_subvsi3_imm1 (operands[0], operands[1], operands[2]));
  else
    emit_insn (gen_subvsi3 (operands[0], operands[1], operands[2]));

  arm_gen_unlikely_cbranch (NE, CC_Vmode, operands[3]);
  DONE;
})

(define_expand "subvdi4"
  [(match_operand:DI 0 "s_register_operand")
   (match_operand:DI 1 "reg_or_int_operand")
   (match_operand:DI 2 "reg_or_int_operand")
   (match_operand 3 "")]
  "TARGET_32BIT"
{
  rtx lo_result, hi_result;
  rtx lo_op1, hi_op1, lo_op2, hi_op2;
  lo_result = gen_lowpart (SImode, operands[0]);
  hi_result = gen_highpart (SImode, operands[0]);
  machine_mode mode = CCmode;

  if (CONST_INT_P (operands[1]) && CONST_INT_P (operands[2]))
    {
      /* If both operands are constants we can decide the result statically.  */
      wi::overflow_type overflow;
      wide_int val = wi::sub (rtx_mode_t (operands[1], DImode),
			      rtx_mode_t (operands[2], DImode),
			      SIGNED, &overflow);
      emit_move_insn (operands[0], GEN_INT (val.to_shwi ()));
      if (overflow != wi::OVF_NONE)
	emit_jump_insn (gen_jump (operands[3]));
      DONE;
    }
  else if (CONST_INT_P (operands[1]))
    {
      arm_decompose_di_binop (operands[2], operands[1], &lo_op2, &hi_op2,
			      &lo_op1, &hi_op1);
      if (const_ok_for_arm (INTVAL (lo_op1)))
	{
	  emit_insn (gen_rsb_imm_compare (lo_result, lo_op1, lo_op2,
					  GEN_INT (~UINTVAL (lo_op1))));
	  /* We could potentially use RSC here in Arm state, but not
	     in Thumb, so it's probably not worth the effort of handling
	     this.  */
	  hi_op1 = force_reg (SImode, hi_op1);
	  mode = CC_RSBmode;
	  goto highpart;
	}
      operands[1] = force_reg (DImode, operands[1]);
    }

  arm_decompose_di_binop (operands[1], operands[2], &lo_op1, &hi_op1,
			  &lo_op2, &hi_op2);
  if (lo_op2 == const0_rtx)
    {
      emit_move_insn (lo_result, lo_op1);
      if (!arm_add_operand (hi_op2, SImode))
        hi_op2 = force_reg (SImode, hi_op2);
      emit_insn (gen_subvsi4 (hi_result, hi_op1, hi_op2, operands[3]));
      DONE;
    }

  if (CONST_INT_P (lo_op2) && !arm_addimm_operand (lo_op2, SImode))
    lo_op2 = force_reg (SImode, lo_op2);
  if (CONST_INT_P (lo_op2))
    emit_insn (gen_cmpsi2_addneg (lo_result, lo_op1, lo_op2,
				  gen_int_mode (-INTVAL (lo_op2), SImode)));
  else
    emit_insn (gen_subsi3_compare1 (lo_result, lo_op1, lo_op2));

 highpart:
  if (!arm_not_operand (hi_op2, SImode))
    hi_op2 = force_reg (SImode, hi_op2);
  rtx ccreg = gen_rtx_REG (mode, CC_REGNUM);
  if (CONST_INT_P (hi_op2))
    emit_insn (gen_subvsi3_borrow_imm (hi_result, hi_op1, hi_op2,
				       gen_rtx_LTU (SImode, ccreg, const0_rtx),
				       gen_rtx_LTU (DImode, ccreg,
						    const0_rtx)));
  else
    emit_insn (gen_subvsi3_borrow (hi_result, hi_op1, hi_op2,
				   gen_rtx_LTU (SImode, ccreg, const0_rtx),
				   gen_rtx_LTU (DImode, ccreg, const0_rtx)));
  arm_gen_unlikely_cbranch (NE, CC_Vmode, operands[3]);

  DONE;
})

(define_expand "usubvsi4"
  [(match_operand:SI 0 "s_register_operand")
   (match_operand:SI 1 "arm_rhs_operand")
   (match_operand:SI 2 "arm_add_operand")
   (match_operand 3 "")]
  "TARGET_32BIT"
{
  machine_mode mode = CCmode;
  if (CONST_INT_P (operands[1]) && CONST_INT_P (operands[2]))
    {
      /* If both operands are constants we can decide the result statically.  */
      wi::overflow_type overflow;
      wide_int val = wi::sub (rtx_mode_t (operands[1], SImode),
			      rtx_mode_t (operands[2], SImode),
			      UNSIGNED, &overflow);
      emit_move_insn (operands[0], GEN_INT (val.to_shwi ()));
      if (overflow != wi::OVF_NONE)
	emit_jump_insn (gen_jump (operands[3]));
      DONE;
    }
  else if (CONST_INT_P (operands[2]))
    emit_insn (gen_cmpsi2_addneg (operands[0], operands[1], operands[2],
				  gen_int_mode (-INTVAL (operands[2]),
						SImode)));
  else if (CONST_INT_P (operands[1]))
    {
      mode = CC_RSBmode;
      emit_insn (gen_rsb_imm_compare (operands[0], operands[1], operands[2],
				      GEN_INT (~UINTVAL (operands[1]))));
    }
  else
    emit_insn (gen_subsi3_compare1 (operands[0], operands[1], operands[2]));
  arm_gen_unlikely_cbranch (LTU, mode, operands[3]);

  DONE;
})

(define_expand "usubvdi4"
  [(match_operand:DI 0 "s_register_operand")
   (match_operand:DI 1 "reg_or_int_operand")
   (match_operand:DI 2 "reg_or_int_operand")
   (match_operand 3 "")]
  "TARGET_32BIT"
{
  rtx lo_result, hi_result;
  rtx lo_op1, hi_op1, lo_op2, hi_op2;
  lo_result = gen_lowpart (SImode, operands[0]);
  hi_result = gen_highpart (SImode, operands[0]);
  machine_mode mode = CCmode;

  if (CONST_INT_P (operands[1]) && CONST_INT_P (operands[2]))
    {
      /* If both operands are constants we can decide the result statically.  */
      wi::overflow_type overflow;
      wide_int val = wi::sub (rtx_mode_t (operands[1], DImode),
			      rtx_mode_t (operands[2], DImode),
			      UNSIGNED, &overflow);
      emit_move_insn (operands[0], GEN_INT (val.to_shwi ()));
      if (overflow != wi::OVF_NONE)
	emit_jump_insn (gen_jump (operands[3]));
      DONE;
    }
  else if (CONST_INT_P (operands[1]))
    {
      arm_decompose_di_binop (operands[2], operands[1], &lo_op2, &hi_op2,
			      &lo_op1, &hi_op1);
      if (const_ok_for_arm (INTVAL (lo_op1)))
	{
	  emit_insn (gen_rsb_imm_compare (lo_result, lo_op1, lo_op2,
					  GEN_INT (~UINTVAL (lo_op1))));
	  /* We could potentially use RSC here in Arm state, but not
	     in Thumb, so it's probably not worth the effort of handling
	     this.  */
	  hi_op1 = force_reg (SImode, hi_op1);
	  mode = CC_RSBmode;
	  goto highpart;
	}
      operands[1] = force_reg (DImode, operands[1]);
    }

  arm_decompose_di_binop (operands[1], operands[2], &lo_op1, &hi_op1,
			  &lo_op2, &hi_op2);
  if (lo_op2 == const0_rtx)
    {
      emit_move_insn (lo_result, lo_op1);
      if (!arm_add_operand (hi_op2, SImode))
        hi_op2 = force_reg (SImode, hi_op2);
      emit_insn (gen_usubvsi4 (hi_result, hi_op1, hi_op2, operands[3]));
      DONE;
    }

  if (CONST_INT_P (lo_op2) && !arm_addimm_operand (lo_op2, SImode))
    lo_op2 = force_reg (SImode, lo_op2);
  if (CONST_INT_P (lo_op2))
    emit_insn (gen_cmpsi2_addneg (lo_result, lo_op1, lo_op2,
				  gen_int_mode (-INTVAL (lo_op2), SImode)));
  else
    emit_insn (gen_subsi3_compare1 (lo_result, lo_op1, lo_op2));

 highpart:
  if (!arm_not_operand (hi_op2, SImode))
    hi_op2 = force_reg (SImode, hi_op2);
  rtx ccreg = gen_rtx_REG (mode, CC_REGNUM);
  if (CONST_INT_P (hi_op2))
    emit_insn (gen_usubvsi3_borrow_imm (hi_result, hi_op1, hi_op2,
					GEN_INT (UINTVAL (hi_op2) & 0xffffffff),
					gen_rtx_LTU (SImode, ccreg, const0_rtx),
					gen_rtx_LTU (DImode, ccreg,
						     const0_rtx)));
  else
    emit_insn (gen_usubvsi3_borrow (hi_result, hi_op1, hi_op2,
				    gen_rtx_LTU (SImode, ccreg, const0_rtx),
				    gen_rtx_LTU (DImode, ccreg, const0_rtx)));
  arm_gen_unlikely_cbranch (LTU, CC_Bmode, operands[3]);

  DONE;
})

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

(define_insn "subvsi3"
  [(set (reg:CC_V CC_REGNUM)
	(compare:CC_V
	 (minus:DI
	  (sign_extend:DI (match_operand:SI 1 "s_register_operand" "l,r"))
	  (sign_extend:DI (match_operand:SI 2 "s_register_operand" "l,r")))
	 (sign_extend:DI (minus:SI (match_dup 1) (match_dup 2)))))
   (set (match_operand:SI 0 "s_register_operand" "=l,r")
	(minus:SI (match_dup 1) (match_dup 2)))]
  "TARGET_32BIT"
  "subs%?\\t%0, %1, %2"
  [(set_attr "conds" "set")
   (set_attr "arch" "t2,*")
   (set_attr "length" "2,4")
   (set_attr "type" "alus_sreg")]
)

(define_insn "subvsi3_imm1"
  [(set (reg:CC_V CC_REGNUM)
	(compare:CC_V
	 (minus:DI
	  (match_operand 1 "arm_immediate_operand" "I")
	  (sign_extend:DI (match_operand:SI 2 "s_register_operand" "r")))
	 (sign_extend:DI (minus:SI (match_dup 1) (match_dup 2)))))
   (set (match_operand:SI 0 "s_register_operand" "=r")
	(minus:SI (match_dup 1) (match_dup 2)))]
  "TARGET_32BIT"
  "rsbs%?\\t%0, %2, %1"
  [(set_attr "conds" "set")
   (set_attr "type" "alus_imm")]
)

(define_insn "subsi3_carryin"
  [(set (match_operand:SI 0 "s_register_operand" "=r,r,r")
	(minus:SI (minus:SI (match_operand:SI 1 "reg_or_int_operand" "r,I,Pz")
			    (match_operand:SI 2 "s_register_operand" "r,r,r"))
		  (match_operand:SI 3 "arm_borrow_operation" "")))]
  "TARGET_32BIT"
  "@
   sbc%?\\t%0, %1, %2
   rsc%?\\t%0, %2, %1
   sbc%?\\t%0, %2, %2, lsl #1"
  [(set_attr "conds" "use")
   (set_attr "arch" "*,a,t2")
   (set_attr "predicable" "yes")
   (set_attr "type" "adc_reg,adc_imm,alu_shift_imm")]
)

;; Special canonicalization of the above when operand1 == (const_int 1):
;; in this case the 'borrow' needs to treated like subtracting from the carry.
(define_insn "rsbsi_carryin_reg"
  [(set (match_operand:SI 0 "s_register_operand" "=r")
	(minus:SI (match_operand:SI 1 "arm_carry_operation" "")
		  (match_operand:SI 2 "s_register_operand" "r")))]
  "TARGET_ARM"
  "rsc%?\\t%0, %2, #1"
  [(set_attr "conds" "use")
   (set_attr "predicable" "yes")
   (set_attr "type" "adc_imm")]
)

;; SBC performs Rn - Rm - ~C, but -Rm = ~Rm + 1 => Rn + ~Rm + 1 - ~C
;; => Rn + ~Rm + C, which is essentially ADC Rd, Rn, ~Rm
(define_insn "*add_not_cin"
  [(set (match_operand:SI 0 "s_register_operand" "=r,r")
	(plus:SI
	 (plus:SI (not:SI (match_operand:SI 1 "s_register_operand" "r,r"))
		  (match_operand:SI 3 "arm_carry_operation" ""))
	 (match_operand:SI 2 "arm_rhs_operand" "r,I")))]
  "TARGET_ARM || (TARGET_THUMB2 && !CONST_INT_P (operands[2]))"
  "@
   sbc%?\\t%0, %2, %1
   rsc%?\\t%0, %1, %2"
  [(set_attr "conds" "use")
   (set_attr "predicable" "yes")
   (set_attr "arch" "*,a")
   (set_attr "type" "adc_reg,adc_imm")]
)

;; On Arm we can also use the same trick when the non-inverted operand is
;; shifted, using RSC.
(define_insn "add_not_shift_cin"
  [(set (match_operand:SI 0 "s_register_operand" "=r,r")
	(plus:SI
	 (plus:SI (match_operator:SI 3 "shift_operator"
		   [(match_operand:SI 1 "s_register_operand" "r,r")
		    (match_operand:SI 2 "shift_amount_operand" "M,r")])
		  (not:SI (match_operand:SI 4 "s_register_operand" "r,r")))
	 (match_operand:SI 5 "arm_carry_operation" "")))]
  "TARGET_ARM"
  "rsc%?\\t%0, %4, %1%S3"
  [(set_attr "conds" "use")
   (set_attr "predicable" "yes")
   (set_attr "type" "alu_shift_imm,alu_shift_reg")]
)

(define_insn "cmpsi3_carryin_<CC_EXTEND>out"
  [(set (reg:<CC_EXTEND> CC_REGNUM)
	(compare:<CC_EXTEND>
	 (SE:DI (match_operand:SI 1 "s_register_operand" "0,r"))
	 (plus:DI (match_operand:DI 3 "arm_borrow_operation" "")
		  (SE:DI (match_operand:SI 2 "s_register_operand" "l,r")))))
   (clobber (match_scratch:SI 0 "=l,r"))]
  "TARGET_32BIT"
  "sbcs\\t%0, %1, %2"
  [(set_attr "conds" "set")
   (set_attr "arch" "t2,*")
   (set_attr "length" "2,4")
   (set_attr "type" "adc_reg")]
)

;; Similar to the above, but handling a constant which has a different
;; canonicalization.
(define_insn "cmpsi3_imm_carryin_<CC_EXTEND>out"
  [(set (reg:<CC_EXTEND> CC_REGNUM)
	(compare:<CC_EXTEND>
	 (SE:DI (match_operand:SI 1 "s_register_operand" "r,r"))
	 (plus:DI (match_operand:DI 3 "arm_borrow_operation" "")
		  (match_operand:DI 2 "arm_adcimm_operand" "I,K"))))
   (clobber (match_scratch:SI 0 "=l,r"))]
  "TARGET_32BIT"
  "@
   sbcs\\t%0, %1, %2
   adcs\\t%0, %1, #%B2"
  [(set_attr "conds" "set")
   (set_attr "type" "adc_imm")]
)

;; Further canonicalization when the constant is zero.
(define_insn "cmpsi3_0_carryin_<CC_EXTEND>out"
  [(set (reg:<CC_EXTEND> CC_REGNUM)
	(compare:<CC_EXTEND>
	 (SE:DI (match_operand:SI 1 "s_register_operand" "r,r"))
	 (match_operand:DI 2 "arm_borrow_operation" "")))
   (clobber (match_scratch:SI 0 "=l,r"))]
  "TARGET_32BIT"
  "sbcs\\t%0, %1, #0"
  [(set_attr "conds" "set")
   (set_attr "type" "adc_imm")]
)

(define_insn "*subsi3_carryin_const"
  [(set (match_operand:SI 0 "s_register_operand" "=r")
	(minus:SI (plus:SI
		   (match_operand:SI 1 "s_register_operand" "r")
		   (match_operand:SI 2 "arm_neg_immediate_operand" "L"))
		  (match_operand:SI 3 "arm_borrow_operation" "")))]
  "TARGET_32BIT"
  "sbc\\t%0, %1, #%n2"
  [(set_attr "conds" "use")
   (set_attr "type" "adc_imm")]
)

(define_insn "*subsi3_carryin_const0"
  [(set (match_operand:SI 0 "s_register_operand" "=r")
	(minus:SI (match_operand:SI 1 "s_register_operand" "r")
		  (match_operand:SI 2 "arm_borrow_operation" "")))]
  "TARGET_32BIT"
  "sbc\\t%0, %1, #0"
  [(set_attr "conds" "use")
   (set_attr "type" "adc_imm")]
)

(define_insn "*subsi3_carryin_shift"
  [(set (match_operand:SI 0 "s_register_operand" "=r,r")
	(minus:SI (minus:SI
		   (match_operand:SI 1 "s_register_operand" "r,r")
		   (match_operator:SI 2 "shift_operator"
		    [(match_operand:SI 3 "s_register_operand" "r,r")
		     (match_operand:SI 4 "shift_amount_operand" "M,r")]))
		  (match_operand:SI 5 "arm_borrow_operation" "")))]
  "TARGET_32BIT"
  "sbc%?\\t%0, %1, %3%S2"
  [(set_attr "conds" "use")
   (set_attr "arch" "32,a")
   (set_attr "shift" "3")
   (set_attr "predicable" "yes")
   (set_attr "type" "alu_shift_imm,alu_shift_reg")]
)

(define_insn "*subsi3_carryin_shift_alt"
  [(set (match_operand:SI 0 "s_register_operand" "=r,r")
	(minus:SI (minus:SI
		   (match_operand:SI 1 "s_register_operand" "r,r")
		   (match_operand:SI 5 "arm_borrow_operation" ""))
		  (match_operator:SI 2 "shift_operator"
		   [(match_operand:SI 3 "s_register_operand" "r,r")
		    (match_operand:SI 4 "shift_amount_operand" "M,r")])))]
  "TARGET_32BIT"
  "sbc%?\\t%0, %1, %3%S2"
  [(set_attr "conds" "use")
   (set_attr "arch" "32,a")
   (set_attr "shift" "3")
   (set_attr "predicable" "yes")
   (set_attr "type" "alu_shift_imm,alu_shift_reg")]
)

;; No RSC in Thumb2
(define_insn "*rsbsi3_carryin_shift"
  [(set (match_operand:SI 0 "s_register_operand" "=r,r")
	(minus:SI (minus:SI
		   (match_operator:SI 2 "shift_operator"
		    [(match_operand:SI 3 "s_register_operand" "r,r")
		     (match_operand:SI 4 "shift_amount_operand" "M,r")])
		   (match_operand:SI 1 "s_register_operand" "r,r"))
		  (match_operand:SI 5 "arm_borrow_operation" "")))]
  "TARGET_ARM"
  "rsc%?\\t%0, %1, %3%S2"
  [(set_attr "conds" "use")
   (set_attr "predicable" "yes")
   (set_attr "type" "alu_shift_imm,alu_shift_reg")]
)

(define_insn "*rsbsi3_carryin_shift_alt"
  [(set (match_operand:SI 0 "s_register_operand" "=r,r")
	(minus:SI (minus:SI
		   (match_operator:SI 2 "shift_operator"
		    [(match_operand:SI 3 "s_register_operand" "r,r")
		     (match_operand:SI 4 "shift_amount_operand" "M,r")])
		    (match_operand:SI 5 "arm_borrow_operation" ""))
		  (match_operand:SI 1 "s_register_operand" "r,r")))]
  "TARGET_ARM"
  "rsc%?\\t%0, %1, %3%S2"
  [(set_attr "conds" "use")
   (set_attr "predicable" "yes")
   (set_attr "type" "alu_shift_imm,alu_shift_reg")]
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
  [(set (match_operand:SF          0 "s_register_operand")
	(plus:SF (match_operand:SF 1 "s_register_operand")
		 (match_operand:SF 2 "s_register_operand")))]
  "TARGET_32BIT && TARGET_HARD_FLOAT"
  "
")

(define_expand "adddf3"
  [(set (match_operand:DF          0 "s_register_operand")
	(plus:DF (match_operand:DF 1 "s_register_operand")
		 (match_operand:DF 2 "s_register_operand")))]
  "TARGET_32BIT && TARGET_HARD_FLOAT && !TARGET_VFP_SINGLE"
  "
")

(define_expand "subdi3"
 [(parallel
   [(set (match_operand:DI            0 "s_register_operand")
	  (minus:DI (match_operand:DI 1 "reg_or_int_operand")
		    (match_operand:DI 2 "s_register_operand")))
    (clobber (reg:CC CC_REGNUM))])]
  "TARGET_EITHER"
  "
  if (TARGET_THUMB1)
    {
      if (!REG_P (operands[1]))
	operands[1] = force_reg (DImode, operands[1]);
    }
  else
    {
      rtx lo_result, hi_result, lo_dest, hi_dest;
      rtx lo_op1, hi_op1, lo_op2, hi_op2;
      rtx condition;

      /* Since operands[1] may be an integer, pass it second, so that
	 any necessary simplifications will be done on the decomposed
	 constant.  */
      arm_decompose_di_binop (operands[2], operands[1], &lo_op2, &hi_op2,
			      &lo_op1, &hi_op1);
      lo_result = lo_dest = gen_lowpart (SImode, operands[0]);
      hi_result = hi_dest = gen_highpart (SImode, operands[0]);

      if (!arm_rhs_operand (lo_op1, SImode))
	lo_op1 = force_reg (SImode, lo_op1);

      if ((TARGET_THUMB2 && ! s_register_operand (hi_op1, SImode))
	  || !arm_rhs_operand (hi_op1, SImode))
	hi_op1 = force_reg (SImode, hi_op1);

      rtx cc_reg;
      if (lo_op1 == const0_rtx)
	{
	  cc_reg = gen_rtx_REG (CC_RSBmode, CC_REGNUM);
	  emit_insn (gen_negsi2_0compare (lo_dest, lo_op2));
	}
      else if (CONST_INT_P (lo_op1))
	{
	  cc_reg = gen_rtx_REG (CC_RSBmode, CC_REGNUM);
	  emit_insn (gen_rsb_imm_compare (lo_dest, lo_op1, lo_op2, 
					  GEN_INT (~UINTVAL (lo_op1))));
	}
      else
	{
	  cc_reg = gen_rtx_REG (CCmode, CC_REGNUM);
	  emit_insn (gen_subsi3_compare (lo_dest, lo_op1, lo_op2));
	}

      condition = gen_rtx_LTU (SImode, cc_reg, const0_rtx);

      if (hi_op1 == const0_rtx)
        emit_insn (gen_negsi2_carryin (hi_dest, hi_op2, condition));
      else
	emit_insn (gen_subsi3_carryin (hi_dest, hi_op1, hi_op2, condition));

      if (lo_result != lo_dest)
	emit_move_insn (lo_result, lo_dest);

      if (hi_result != hi_dest)
	emit_move_insn (hi_result, hi_dest);

      DONE;
    }
  "
)

(define_expand "subsi3"
  [(set (match_operand:SI           0 "s_register_operand")
	(minus:SI (match_operand:SI 1 "reg_or_int_operand")
		  (match_operand:SI 2 "s_register_operand")))]
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
  [(set (reg:CC_NZ CC_REGNUM)
	(compare:CC_NZ
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
   (set_attr "type" "alus_imm,alus_sreg,alus_imm")]
)

;; To keep the comparison in canonical form we express it as (~reg cmp ~0)
;; rather than (0 cmp reg).  This gives the same results for unsigned
;; and equality compares which is what we mostly need here.
(define_insn "rsb_imm_compare"
  [(set (reg:CC_RSB CC_REGNUM)
	(compare:CC_RSB (not:SI (match_operand:SI 2 "s_register_operand" "r"))
			(match_operand 3 "const_int_operand" "")))
   (set (match_operand:SI 0 "s_register_operand" "=r")
	(minus:SI (match_operand 1 "arm_immediate_operand" "I")
		  (match_dup 2)))]
  "TARGET_32BIT && ~UINTVAL (operands[1]) == UINTVAL (operands[3])"
  "rsbs\\t%0, %2, %1"
  [(set_attr "conds" "set")
   (set_attr "type" "alus_imm")]
)

;; Similarly, but the result is unused.
(define_insn "rsb_imm_compare_scratch"
  [(set (reg:CC_RSB CC_REGNUM)
	(compare:CC_RSB (not:SI (match_operand:SI 2 "s_register_operand" "r"))
			(match_operand 1 "arm_not_immediate_operand" "K")))
   (clobber (match_scratch:SI 0 "=r"))]
  "TARGET_32BIT"
  "rsbs\\t%0, %2, #%B1"
  [(set_attr "conds" "set")
   (set_attr "type" "alus_imm")]
)

;; Compare the sum of a value plus a carry against a constant.  Uses
;; RSC, so the result is swapped.  Only available on Arm
(define_insn "rscsi3_<CC_EXTEND>out_scratch"
  [(set (reg:CC_SWP CC_REGNUM)
	(compare:CC_SWP
	 (plus:DI (SE:DI (match_operand:SI 2 "s_register_operand" "r"))
		  (match_operand:DI 3 "arm_borrow_operation" ""))
	 (match_operand 1 "arm_immediate_operand" "I")))
   (clobber (match_scratch:SI 0 "=r"))]
  "TARGET_ARM"
  "rscs\\t%0, %2, %1"
  [(set_attr "conds" "set")
   (set_attr "type" "alus_imm")]
)

(define_insn "usubvsi3_borrow"
  [(set (reg:CC_B CC_REGNUM)
	(compare:CC_B
	 (zero_extend:DI (match_operand:SI 1 "s_register_operand" "0,r"))
	 (plus:DI (match_operand:DI 4 "arm_borrow_operation" "")
	          (zero_extend:DI
		   (match_operand:SI 2 "s_register_operand" "l,r")))))
   (set (match_operand:SI 0 "s_register_operand" "=l,r")
	(minus:SI (match_dup 1)
		  (plus:SI (match_operand:SI 3 "arm_borrow_operation" "")
			   (match_dup 2))))]
  "TARGET_32BIT"
  "sbcs%?\\t%0, %1, %2"
  [(set_attr "conds" "set")
   (set_attr "arch" "t2,*")
   (set_attr "length" "2,4")]
)

(define_insn "usubvsi3_borrow_imm"
  [(set (reg:CC_B CC_REGNUM)
	(compare:CC_B
	 (zero_extend:DI (match_operand:SI 1 "s_register_operand" "r,r"))
	 (plus:DI (match_operand:DI 5 "arm_borrow_operation" "")
		  (match_operand:DI 3 "const_int_operand" "n,n"))))
   (set (match_operand:SI 0 "s_register_operand" "=r,r")
	(minus:SI (match_dup 1)
		  (plus:SI (match_operand:SI 4 "arm_borrow_operation" "")
			   (match_operand:SI 2 "arm_adcimm_operand" "I,K"))))]
  "TARGET_32BIT
   && (UINTVAL (operands[2]) & 0xffffffff) == UINTVAL (operands[3])"
  "@
  sbcs%?\\t%0, %1, %2
  adcs%?\\t%0, %1, #%B2"
  [(set_attr "conds" "set")
   (set_attr "type" "alus_imm")]
)

(define_insn "subvsi3_borrow"
  [(set (reg:CC_V CC_REGNUM)
	(compare:CC_V
	 (minus:DI
	  (minus:DI
	   (sign_extend:DI (match_operand:SI 1 "s_register_operand" "0,r"))
	   (sign_extend:DI (match_operand:SI 2 "s_register_operand" "l,r")))
	  (match_operand:DI 4 "arm_borrow_operation" ""))
	 (sign_extend:DI
	  (minus:SI (minus:SI (match_dup 1) (match_dup 2))
		    (match_operand:SI 3 "arm_borrow_operation" "")))))
   (set (match_operand:SI 0 "s_register_operand" "=l,r")
	(minus:SI (minus:SI (match_dup 1) (match_dup 2))
		  (match_dup 3)))]
  "TARGET_32BIT"
  "sbcs%?\\t%0, %1, %2"
  [(set_attr "conds" "set")
   (set_attr "arch" "t2,*")
   (set_attr "length" "2,4")]
)

(define_insn "subvsi3_borrow_imm"
  [(set (reg:CC_V CC_REGNUM)
	(compare:CC_V
	 (minus:DI
	  (minus:DI
	   (sign_extend:DI (match_operand:SI 1 "s_register_operand" "r,r"))
	   (match_operand 2 "arm_adcimm_operand" "I,K"))
	  (match_operand:DI 4 "arm_borrow_operation" ""))
	 (sign_extend:DI
	  (minus:SI (minus:SI (match_dup 1) (match_dup 2))
		    (match_operand:SI 3 "arm_borrow_operation" "")))))
   (set (match_operand:SI 0 "s_register_operand" "=r,r")
	(minus:SI (minus:SI (match_dup 1) (match_dup 2))
		  (match_dup 3)))]
  "TARGET_32BIT
   && INTVAL (operands[2]) == ARM_SIGN_EXTEND (INTVAL (operands[2]))"
  "@
  sbcs%?\\t%0, %1, %2
  adcs%?\\t%0, %1, #%B2"
  [(set_attr "conds" "set")
   (set_attr "type" "alus_imm")]
)

(define_expand "subsf3"
  [(set (match_operand:SF           0 "s_register_operand")
	(minus:SF (match_operand:SF 1 "s_register_operand")
		  (match_operand:SF 2 "s_register_operand")))]
  "TARGET_32BIT && TARGET_HARD_FLOAT"
  "
")

(define_expand "subdf3"
  [(set (match_operand:DF           0 "s_register_operand")
	(minus:DF (match_operand:DF 1 "s_register_operand")
		  (match_operand:DF 2 "s_register_operand")))]
  "TARGET_32BIT && TARGET_HARD_FLOAT && !TARGET_VFP_SINGLE"
  "
")


;; Multiplication insns

(define_expand "mulhi3"
  [(set (match_operand:HI 0 "s_register_operand")
	(mult:HI (match_operand:HI 1 "s_register_operand")
		 (match_operand:HI 2 "s_register_operand")))]
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
  [(set (match_operand:SI          0 "s_register_operand")
	(mult:SI (match_operand:SI 2 "s_register_operand")
		 (match_operand:SI 1 "s_register_operand")))]
  "TARGET_EITHER"
  ""
)

;; Use `&' and then `0' to prevent operands 0 and 2 being the same
(define_insn "*mul"
  [(set (match_operand:SI          0 "s_register_operand" "=l,r,&r,&r")
	(mult:SI (match_operand:SI 2 "s_register_operand" "l,r,r,r")
		 (match_operand:SI 1 "s_register_operand" "%0,r,0,r")))]
  "TARGET_32BIT"
  "mul%?\\t%0, %2, %1"
  [(set_attr "type" "mul")
   (set_attr "predicable" "yes")
   (set_attr "arch" "t2,v6,nov6,nov6")
   (set_attr "length" "4")
   (set_attr "predicable_short_it" "yes,no,*,*")]
)

;; MLA and MLS instruction. Use operand 1 for the accumulator to prefer
;; reusing the same register.

(define_insn "*mla"
  [(set (match_operand:SI 0 "s_register_operand" "=r,&r,&r,&r")
	(plus:SI
	  (mult:SI (match_operand:SI 3 "s_register_operand" "r,r,r,r")
		   (match_operand:SI 2 "s_register_operand" "%r,r,0,r"))
	  (match_operand:SI 1 "s_register_operand" "r,0,r,r")))]
  "TARGET_32BIT"
  "mla%?\\t%0, %3, %2, %1"
  [(set_attr "type" "mla")
   (set_attr "predicable" "yes")
   (set_attr "arch" "v6,nov6,nov6,nov6")]
)

(define_insn "*mls"
  [(set (match_operand:SI 0 "s_register_operand" "=r")
	(minus:SI
	  (match_operand:SI 1 "s_register_operand" "r")
	  (mult:SI (match_operand:SI 3 "s_register_operand" "r")
		   (match_operand:SI 2 "s_register_operand" "r"))))]
  "TARGET_32BIT && arm_arch_thumb2"
  "mls%?\\t%0, %3, %2, %1"
  [(set_attr "type" "mla")
   (set_attr "predicable" "yes")]
)

(define_insn "*mulsi3_compare0"
  [(set (reg:CC_NZ CC_REGNUM)
	(compare:CC_NZ (mult:SI
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
  [(set (reg:CC_NZ CC_REGNUM)
	(compare:CC_NZ (mult:SI
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
  [(set (reg:CC_NZ CC_REGNUM)
	(compare:CC_NZ (mult:SI
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
  [(set (reg:CC_NZ CC_REGNUM)
	(compare:CC_NZ (mult:SI
			  (match_operand:SI 2 "s_register_operand" "r")
			  (match_operand:SI 1 "s_register_operand" "r"))
			 (const_int 0)))
   (clobber (match_scratch:SI 0 "=r"))]
  "TARGET_ARM && arm_arch6 && optimize_size"
  "muls%?\\t%0, %2, %1"
  [(set_attr "conds" "set")
   (set_attr "type" "muls")]
)

(define_insn "*mulsi3addsi_compare0"
  [(set (reg:CC_NZ CC_REGNUM)
	(compare:CC_NZ
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
  [(set (reg:CC_NZ CC_REGNUM)
	(compare:CC_NZ
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
  [(set (reg:CC_NZ CC_REGNUM)
	(compare:CC_NZ
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
  [(set (reg:CC_NZ CC_REGNUM)
	(compare:CC_NZ
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

;; 32x32->64 widening multiply.
;; The only difference between the v3-5 and v6+ versions is the requirement
;; that the output does not overlap with either input.

(define_expand "<Us>mulsidi3"
  [(set (match_operand:DI 0 "s_register_operand")
	(mult:DI
	 (SE:DI (match_operand:SI 1 "s_register_operand"))
	 (SE:DI (match_operand:SI 2 "s_register_operand"))))]
  "TARGET_32BIT"
  {
      emit_insn (gen_<US>mull (gen_lowpart (SImode, operands[0]),
			       gen_highpart (SImode, operands[0]),
			       operands[1], operands[2]));
      DONE;
  }
)

(define_insn "<US>mull"
  [(set (match_operand:SI 0 "s_register_operand" "=r,&r")
	(mult:SI
	 (match_operand:SI 2 "s_register_operand" "%r,r")
	 (match_operand:SI 3 "s_register_operand" "r,r")))
   (set (match_operand:SI 1 "s_register_operand" "=r,&r")
	(truncate:SI
	 (lshiftrt:DI
	  (mult:DI (SE:DI (match_dup 2)) (SE:DI (match_dup 3)))
	  (const_int 32))))]
  "TARGET_32BIT"
  "<US>mull%?\\t%0, %1, %2, %3"
  [(set_attr "type" "umull")
   (set_attr "predicable" "yes")
   (set_attr "arch" "v6,nov6")]
)

(define_expand "<Us>maddsidi4"
  [(set (match_operand:DI 0 "s_register_operand")
	(plus:DI
	 (mult:DI
	  (SE:DI (match_operand:SI 1 "s_register_operand"))
	  (SE:DI (match_operand:SI 2 "s_register_operand")))
	 (match_operand:DI 3 "s_register_operand")))]
  "TARGET_32BIT"
  {
      emit_insn (gen_<US>mlal (gen_lowpart (SImode, operands[0]),
			       gen_lowpart (SImode, operands[3]),
			       gen_highpart (SImode, operands[0]),
			       gen_highpart (SImode, operands[3]),
			       operands[1], operands[2]));
      DONE;
  }
)

(define_insn "<US>mlal"
  [(set (match_operand:SI 0 "s_register_operand" "=r,&r")
	(plus:SI
	 (mult:SI
	  (match_operand:SI 4 "s_register_operand" "%r,r")
	  (match_operand:SI 5 "s_register_operand" "r,r"))
	 (match_operand:SI 1 "s_register_operand" "0,0")))
   (set (match_operand:SI 2 "s_register_operand" "=r,&r")
	(plus:SI
	 (truncate:SI
	  (lshiftrt:DI
	   (plus:DI
	    (mult:DI (SE:DI (match_dup 4)) (SE:DI (match_dup 5)))
	    (zero_extend:DI (match_dup 1)))
	   (const_int 32)))
	 (match_operand:SI 3 "s_register_operand" "2,2")))]
  "TARGET_32BIT"
  "<US>mlal%?\\t%0, %2, %4, %5"
  [(set_attr "type" "umlal")
   (set_attr "predicable" "yes")
   (set_attr "arch" "v6,nov6")]
)

(define_expand "<US>mulsi3_highpart"
  [(parallel
    [(set (match_operand:SI 0 "s_register_operand")
	  (truncate:SI
	   (lshiftrt:DI
	    (mult:DI
	     (SE:DI (match_operand:SI 1 "s_register_operand"))
	     (SE:DI (match_operand:SI 2 "s_register_operand")))
	    (const_int 32))))
     (clobber (match_scratch:SI 3 ""))])]
  "TARGET_32BIT"
  ""
)

(define_insn "*<US>mull_high"
  [(set (match_operand:SI 0 "s_register_operand" "=r,&r,&r")
	(truncate:SI
	 (lshiftrt:DI
	  (mult:DI
	   (SE:DI (match_operand:SI 1 "s_register_operand" "%r,0,r"))
	   (SE:DI (match_operand:SI 2 "s_register_operand" "r,r,r")))
	  (const_int 32))))
   (clobber (match_scratch:SI 3 "=r,&r,&r"))]
  "TARGET_32BIT"
  "<US>mull%?\\t%3, %0, %2, %1"
  [(set_attr "type" "umull")
   (set_attr "predicable" "yes")
   (set_attr "arch" "v6,nov6,nov6")]
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
   (set_attr "predicable" "yes")]
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
   (set_attr "predicable" "yes")]
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
   (set_attr "predicable" "yes")]
)

(define_expand "maddhisi4"
  [(set (match_operand:SI 0 "s_register_operand")
	(plus:SI (mult:SI (sign_extend:SI
			   (match_operand:HI 1 "s_register_operand"))
			  (sign_extend:SI
			   (match_operand:HI 2 "s_register_operand")))
		 (match_operand:SI 3 "s_register_operand")))]
  "TARGET_DSP_MULTIPLY"
  {
    /* If this function reads the Q bit from ACLE intrinsics break up the
       multiplication and accumulation as an overflow during accumulation will
       clobber the Q flag.  */
    if (ARM_Q_BIT_READ)
      {
	rtx tmp = gen_reg_rtx (SImode);
	emit_insn (gen_mulhisi3 (tmp, operands[1], operands[2]));
	emit_insn (gen_addsi3 (operands[0], tmp, operands[3]));
	DONE;
      }
  }
)

(define_insn "*arm_maddhisi4"
  [(set (match_operand:SI 0 "s_register_operand" "=r")
	(plus:SI (mult:SI (sign_extend:SI
			   (match_operand:HI 1 "s_register_operand" "r"))
			  (sign_extend:SI
			   (match_operand:HI 2 "s_register_operand" "r")))
		 (match_operand:SI 3 "s_register_operand" "r")))]
  "TARGET_DSP_MULTIPLY && !ARM_Q_BIT_READ"
  "smlabb%?\\t%0, %1, %2, %3"
  [(set_attr "type" "smlaxy")
   (set_attr "predicable" "yes")]
)

(define_insn "arm_smlabb_setq"
  [(set (match_operand:SI 0 "s_register_operand" "=r")
	(plus:SI (mult:SI (sign_extend:SI
			   (match_operand:HI 1 "s_register_operand" "r"))
			  (sign_extend:SI
			   (match_operand:HI 2 "s_register_operand" "r")))
		 (match_operand:SI 3 "s_register_operand" "r")))
   (set (reg:CC APSRQ_REGNUM)
	(unspec:CC [(reg:CC APSRQ_REGNUM)] UNSPEC_Q_SET))]
  "TARGET_DSP_MULTIPLY"
  "smlabb%?\\t%0, %1, %2, %3"
  [(set_attr "type" "smlaxy")
   (set_attr "predicable" "yes")]
)

(define_expand "arm_smlabb"
 [(match_operand:SI 0 "s_register_operand")
  (match_operand:SI 1 "s_register_operand")
  (match_operand:SI 2 "s_register_operand")
  (match_operand:SI 3 "s_register_operand")]
  "TARGET_DSP_MULTIPLY"
  {
    rtx mult1 = gen_lowpart (HImode, operands[1]);
    rtx mult2 = gen_lowpart (HImode, operands[2]);
    if (ARM_Q_BIT_READ)
      emit_insn (gen_arm_smlabb_setq (operands[0], mult1, mult2, operands[3]));
    else
      emit_insn (gen_maddhisi4 (operands[0], mult1, mult2, operands[3]));
    DONE;
  }
)

;; Note: there is no maddhisi4ibt because this one is canonical form
(define_insn "maddhisi4tb"
  [(set (match_operand:SI 0 "s_register_operand" "=r")
	(plus:SI (mult:SI (ashiftrt:SI
			   (match_operand:SI 1 "s_register_operand" "r")
			   (const_int 16))
			  (sign_extend:SI
			   (match_operand:HI 2 "s_register_operand" "r")))
		 (match_operand:SI 3 "s_register_operand" "r")))]
  "TARGET_DSP_MULTIPLY && !ARM_Q_BIT_READ"
  "smlatb%?\\t%0, %1, %2, %3"
  [(set_attr "type" "smlaxy")
   (set_attr "predicable" "yes")]
)

(define_insn "arm_smlatb_setq"
  [(set (match_operand:SI 0 "s_register_operand" "=r")
	(plus:SI (mult:SI (ashiftrt:SI
			   (match_operand:SI 1 "s_register_operand" "r")
			   (const_int 16))
			  (sign_extend:SI
			   (match_operand:HI 2 "s_register_operand" "r")))
		 (match_operand:SI 3 "s_register_operand" "r")))
   (set (reg:CC APSRQ_REGNUM)
	(unspec:CC [(reg:CC APSRQ_REGNUM)] UNSPEC_Q_SET))]
  "TARGET_DSP_MULTIPLY"
  "smlatb%?\\t%0, %1, %2, %3"
  [(set_attr "type" "smlaxy")
   (set_attr "predicable" "yes")]
)

(define_expand "arm_smlatb"
 [(match_operand:SI 0 "s_register_operand")
  (match_operand:SI 1 "s_register_operand")
  (match_operand:SI 2 "s_register_operand")
  (match_operand:SI 3 "s_register_operand")]
  "TARGET_DSP_MULTIPLY"
  {
    rtx mult2 = gen_lowpart (HImode, operands[2]);
    if (ARM_Q_BIT_READ)
      emit_insn (gen_arm_smlatb_setq (operands[0], operands[1],
				      mult2, operands[3]));
    else
      emit_insn (gen_maddhisi4tb (operands[0], operands[1],
				  mult2, operands[3]));
    DONE;
  }
)

(define_insn "maddhisi4tt"
  [(set (match_operand:SI 0 "s_register_operand" "=r")
	(plus:SI (mult:SI (ashiftrt:SI
			   (match_operand:SI 1 "s_register_operand" "r")
			   (const_int 16))
			  (ashiftrt:SI
			   (match_operand:SI 2 "s_register_operand" "r")
			   (const_int 16)))
		 (match_operand:SI 3 "s_register_operand" "r")))]
  "TARGET_DSP_MULTIPLY && !ARM_Q_BIT_READ"
  "smlatt%?\\t%0, %1, %2, %3"
  [(set_attr "type" "smlaxy")
   (set_attr "predicable" "yes")]
)

(define_insn "arm_smlatt_setq"
  [(set (match_operand:SI 0 "s_register_operand" "=r")
	(plus:SI (mult:SI (ashiftrt:SI
			   (match_operand:SI 1 "s_register_operand" "r")
			   (const_int 16))
			  (ashiftrt:SI
			   (match_operand:SI 2 "s_register_operand" "r")
			   (const_int 16)))
		 (match_operand:SI 3 "s_register_operand" "r")))
   (set (reg:CC APSRQ_REGNUM)
	(unspec:CC [(reg:CC APSRQ_REGNUM)] UNSPEC_Q_SET))]
  "TARGET_DSP_MULTIPLY"
  "smlatt%?\\t%0, %1, %2, %3"
  [(set_attr "type" "smlaxy")
   (set_attr "predicable" "yes")]
)

(define_expand "arm_smlatt"
 [(match_operand:SI 0 "s_register_operand")
  (match_operand:SI 1 "s_register_operand")
  (match_operand:SI 2 "s_register_operand")
  (match_operand:SI 3 "s_register_operand")]
  "TARGET_DSP_MULTIPLY"
  {
    if (ARM_Q_BIT_READ)
      emit_insn (gen_arm_smlatt_setq (operands[0], operands[1],
				      operands[2], operands[3]));
    else
      emit_insn (gen_maddhisi4tt (operands[0], operands[1],
				  operands[2], operands[3]));
    DONE;
  }
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
   (set_attr "predicable" "yes")])

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
   (set_attr "predicable" "yes")])

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
   (set_attr "predicable" "yes")])

(define_insn "arm_<smlaw_op><add_clobber_q_name>_insn"
  [(set (match_operand:SI 0 "s_register_operand" "=r")
	(unspec:SI
	   [(match_operand:SI 1 "s_register_operand" "r")
	    (match_operand:SI 2 "s_register_operand" "r")
	    (match_operand:SI 3 "s_register_operand" "r")]
	   SMLAWBT))]
  "TARGET_DSP_MULTIPLY && <add_clobber_q_pred>"
  "<smlaw_op>%?\\t%0, %1, %2, %3"
  [(set_attr "type" "smlaxy")
   (set_attr "predicable" "yes")]
)

(define_expand "arm_<smlaw_op>"
  [(set (match_operand:SI 0 "s_register_operand")
	(unspec:SI
	   [(match_operand:SI 1 "s_register_operand")
	    (match_operand:SI 2 "s_register_operand")
	    (match_operand:SI 3 "s_register_operand")]
	   SMLAWBT))]
  "TARGET_DSP_MULTIPLY"
  {
    if (ARM_Q_BIT_READ)
      emit_insn (gen_arm_<smlaw_op>_setq_insn (operands[0], operands[1],
					       operands[2], operands[3]));
    else
      emit_insn (gen_arm_<smlaw_op>_insn (operands[0], operands[1],
					  operands[2], operands[3]));
    DONE;
  }
)

(define_expand "mulsf3"
  [(set (match_operand:SF          0 "s_register_operand")
	(mult:SF (match_operand:SF 1 "s_register_operand")
		 (match_operand:SF 2 "s_register_operand")))]
  "TARGET_32BIT && TARGET_HARD_FLOAT"
  "
")

(define_expand "muldf3"
  [(set (match_operand:DF          0 "s_register_operand")
	(mult:DF (match_operand:DF 1 "s_register_operand")
		 (match_operand:DF 2 "s_register_operand")))]
  "TARGET_32BIT && TARGET_HARD_FLOAT && !TARGET_VFP_SINGLE"
  "
")

;; Division insns

(define_expand "divsf3"
  [(set (match_operand:SF 0 "s_register_operand")
	(div:SF (match_operand:SF 1 "s_register_operand")
		(match_operand:SF 2 "s_register_operand")))]
  "TARGET_32BIT && TARGET_HARD_FLOAT"
  "")

(define_expand "divdf3"
  [(set (match_operand:DF 0 "s_register_operand")
	(div:DF (match_operand:DF 1 "s_register_operand")
		(match_operand:DF 2 "s_register_operand")))]
  "TARGET_32BIT && TARGET_HARD_FLOAT && TARGET_VFP_DOUBLE"
  "")


; Expand logical operations.  The mid-end expander does not split off memory
; operands or complex immediates, which leads to fewer LDRD/STRD instructions.
; So an explicit expander is needed to generate better code.

(define_expand "<LOGICAL:optab>di3"
  [(set (match_operand:DI	  0 "s_register_operand")
	(LOGICAL:DI (match_operand:DI 1 "s_register_operand")
		    (match_operand:DI 2 "arm_<optab>di_operand")))]
  "TARGET_32BIT"
  {
      rtx low  = simplify_gen_binary (<CODE>, SImode,
				      gen_lowpart (SImode, operands[1]),
				      gen_lowpart (SImode, operands[2]));
      rtx high = simplify_gen_binary (<CODE>, SImode,
				      gen_highpart (SImode, operands[1]),
				      gen_highpart_mode (SImode, DImode,
							 operands[2]));

      emit_insn (gen_rtx_SET (gen_lowpart (SImode, operands[0]), low));
      emit_insn (gen_rtx_SET (gen_highpart (SImode, operands[0]), high));
      DONE;
  }
)

(define_expand "one_cmpldi2"
  [(set (match_operand:DI 0 "s_register_operand")
	(not:DI (match_operand:DI 1 "s_register_operand")))]
  "TARGET_32BIT"
  {
      rtx low  = simplify_gen_unary (NOT, SImode,
				     gen_lowpart (SImode, operands[1]),
				     SImode);
      rtx high = simplify_gen_unary (NOT, SImode,
				     gen_highpart_mode (SImode, DImode,
							operands[1]),
				     SImode);

      emit_insn (gen_rtx_SET (gen_lowpart (SImode, operands[0]), low));
      emit_insn (gen_rtx_SET (gen_highpart (SImode, operands[0]), high));
      DONE;
  }
)

;; Split DImode and, ior, xor operations.  Simply perform the logical
;; operation on the upper and lower halves of the registers.
;; This is needed for atomic operations in arm_split_atomic_op.
;; Avoid splitting IWMMXT instructions.
(define_split
  [(set (match_operand:DI 0 "s_register_operand" "")
	(match_operator:DI 6 "logical_binary_operator"
	  [(match_operand:DI 1 "s_register_operand" "")
	   (match_operand:DI 2 "s_register_operand" "")]))]
  "TARGET_32BIT && reload_completed
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

;; Split DImode not (needed for atomic operations in arm_split_atomic_op).
;; Unconditionally split since there is no SIMD DImode NOT pattern.
(define_split
  [(set (match_operand:DI 0 "s_register_operand")
	(not:DI (match_operand:DI 1 "s_register_operand")))]
  "TARGET_32BIT"
  [(set (match_dup 0) (not:SI (match_dup 1)))
   (set (match_dup 2) (not:SI (match_dup 3)))]
  "
  {
    operands[2] = gen_highpart (SImode, operands[0]);
    operands[0] = gen_lowpart (SImode, operands[0]);
    operands[3] = gen_highpart (SImode, operands[1]);
    operands[1] = gen_lowpart (SImode, operands[1]);
  }"
)

(define_expand "andsi3"
  [(set (match_operand:SI         0 "s_register_operand")
	(and:SI (match_operand:SI 1 "s_register_operand")
		(match_operand:SI 2 "reg_or_int_operand")))]
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
  [(set (reg:CC_NZ CC_REGNUM)
	(compare:CC_NZ
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
  [(set (reg:CC_NZ CC_REGNUM)
	(compare:CC_NZ
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
  [(set (reg:CC_NZ CC_REGNUM)
	(compare:CC_NZ (zero_extract:SI
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
  [(parallel [(set (reg:CC_NZ CC_REGNUM)
		   (compare:CC_NZ (and:SI (match_dup 1) (match_dup 2))
				    (const_int 0)))
	      (set (match_dup 0) (and:SI (match_dup 1) (match_dup 2)))])
   (set (match_dup 0)
	(if_then_else:SI (eq (reg:CC_NZ CC_REGNUM) (const_int 0))
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
  [(parallel [(set (reg:CC_NZ CC_REGNUM)
		   (compare:CC_NZ (ashift:SI (match_dup 1) (match_dup 2))
				    (const_int 0)))
	      (set (match_dup 0) (ashift:SI (match_dup 1) (match_dup 2)))])
   (set (match_dup 0)
	(if_then_else:SI (eq (reg:CC_NZ CC_REGNUM) (const_int 0))
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
  [(parallel [(set (reg:CC_NZ CC_REGNUM)
		   (compare:CC_NZ (and:SI (match_dup 1) (match_dup 2))
				    (const_int 0)))
	      (set (match_dup 0) (and:SI (match_dup 1) (match_dup 2)))])
   (set (match_dup 0)
	(if_then_else:SI (eq (reg:CC_NZ CC_REGNUM) (const_int 0))
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
  [(parallel [(set (reg:CC_NZ CC_REGNUM)
		   (compare:CC_NZ (ashift:SI (match_dup 1) (match_dup 2))
				    (const_int 0)))
	      (set (match_dup 0) (ashift:SI (match_dup 1) (match_dup 2)))])
   (set (match_dup 0)
	(if_then_else:SI (eq (reg:CC_NZ CC_REGNUM) (const_int 0))
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
  [(set (zero_extract (match_operand 0 "nonimmediate_operand")
                      (match_operand 1 "general_operand")
                      (match_operand 2 "general_operand"))
        (match_operand 3 "reg_or_int_operand"))]
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
   (set_attr "type" "bfm")]
)

(define_insn "andsi_notsi_si"
  [(set (match_operand:SI 0 "s_register_operand" "=r")
	(and:SI (not:SI (match_operand:SI 2 "s_register_operand" "r"))
		(match_operand:SI 1 "s_register_operand" "r")))]
  "TARGET_32BIT"
  "bic%?\\t%0, %1, %2"
  [(set_attr "predicable" "yes")
   (set_attr "type" "logic_reg")]
)

(define_insn "andsi_not_shiftsi_si"
  [(set (match_operand:SI 0 "s_register_operand" "=r,r")
	(and:SI (not:SI (match_operator:SI 4 "shift_operator"
			 [(match_operand:SI 2 "s_register_operand" "r,r")
			  (match_operand:SI 3 "shift_amount_operand" "M,r")]))
		(match_operand:SI 1 "s_register_operand" "r,r")))]
  "TARGET_32BIT"
  "bic%?\\t%0, %1, %2%S4"
  [(set_attr "predicable" "yes")
   (set_attr "shift" "2")
   (set_attr "arch" "32,a")
   (set_attr "type" "logic_shift_imm,logic_shift_reg")]
)

;; Shifted bics pattern used to set up CC status register and not reusing
;; bics output.  Pattern restricts Thumb2 shift operand as bics for Thumb2
;; does not support shift by register.
(define_insn "andsi_not_shiftsi_si_scc_no_reuse"
  [(set (reg:CC_NZ CC_REGNUM)
	(compare:CC_NZ
		(and:SI (not:SI (match_operator:SI 0 "shift_operator"
			[(match_operand:SI 1 "s_register_operand" "r,r")
			 (match_operand:SI 2 "shift_amount_operand" "M,r")]))
			(match_operand:SI 3 "s_register_operand" "r,r"))
		(const_int 0)))
   (clobber (match_scratch:SI 4 "=r,r"))]
  "TARGET_32BIT"
  "bics%?\\t%4, %3, %1%S0"
  [(set_attr "predicable" "yes")
   (set_attr "arch" "32,a")
   (set_attr "conds" "set")
   (set_attr "shift" "1")
   (set_attr "type" "logic_shift_imm,logic_shift_reg")]
)

;; Same as andsi_not_shiftsi_si_scc_no_reuse, but the bics result is also
;; getting reused later.
(define_insn "andsi_not_shiftsi_si_scc"
  [(parallel [(set (reg:CC_NZ CC_REGNUM)
	(compare:CC_NZ
		(and:SI (not:SI (match_operator:SI 0 "shift_operator"
			[(match_operand:SI 1 "s_register_operand" "r,r")
			 (match_operand:SI 2 "shift_amount_operand" "M,r")]))
			(match_operand:SI 3 "s_register_operand" "r,r"))
		(const_int 0)))
	(set (match_operand:SI 4 "s_register_operand" "=r,r")
	     (and:SI (not:SI (match_op_dup 0
		     [(match_dup 1)
		      (match_dup 2)]))
		     (match_dup 3)))])]
  "TARGET_32BIT"
  "bics%?\\t%4, %3, %1%S0"
  [(set_attr "predicable" "yes")
   (set_attr "arch" "32,a")
   (set_attr "conds" "set")
   (set_attr "shift" "1")
   (set_attr "type" "logic_shift_imm,logic_shift_reg")]
)

(define_insn "*andsi_notsi_si_compare0"
  [(set (reg:CC_NZ CC_REGNUM)
	(compare:CC_NZ
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
  [(set (reg:CC_NZ CC_REGNUM)
	(compare:CC_NZ
	 (and:SI (not:SI (match_operand:SI 2 "s_register_operand" "r"))
		 (match_operand:SI 1 "s_register_operand" "r"))
	 (const_int 0)))
   (clobber (match_scratch:SI 0 "=r"))]
  "TARGET_32BIT"
  "bics\\t%0, %1, %2"
  [(set_attr "conds" "set")
   (set_attr "type" "logics_shift_reg")]
)

(define_expand "iorsi3"
  [(set (match_operand:SI         0 "s_register_operand")
	(ior:SI (match_operand:SI 1 "s_register_operand")
		(match_operand:SI 2 "reg_or_int_operand")))]
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
  [(set (reg:CC_NZ CC_REGNUM)
	(compare:CC_NZ
	 (ior:SI (match_operand:SI 1 "s_register_operand" "%r,0,r")
		 (match_operand:SI 2 "arm_rhs_operand" "I,l,r"))
	 (const_int 0)))
   (set (match_operand:SI 0 "s_register_operand" "=r,l,r")
	(ior:SI (match_dup 1) (match_dup 2)))]
  "TARGET_32BIT"
  "orrs%?\\t%0, %1, %2"
  [(set_attr "conds" "set")
   (set_attr "arch" "*,t2,*")
   (set_attr "length" "4,2,4")
   (set_attr "type" "logics_imm,logics_reg,logics_reg")]
)

(define_insn "*iorsi3_compare0_scratch"
  [(set (reg:CC_NZ CC_REGNUM)
	(compare:CC_NZ
	 (ior:SI (match_operand:SI 1 "s_register_operand" "%r,0,r")
		 (match_operand:SI 2 "arm_rhs_operand" "I,l,r"))
	 (const_int 0)))
   (clobber (match_scratch:SI 0 "=r,l,r"))]
  "TARGET_32BIT"
  "orrs%?\\t%0, %1, %2"
  [(set_attr "conds" "set")
   (set_attr "arch" "*,t2,*")
   (set_attr "length" "4,2,4")
   (set_attr "type" "logics_imm,logics_reg,logics_reg")]
)

(define_expand "xorsi3"
  [(set (match_operand:SI         0 "s_register_operand")
	(xor:SI (match_operand:SI 1 "s_register_operand")
		(match_operand:SI 2 "reg_or_int_operand")))]
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
  [(set (reg:CC_NZ CC_REGNUM)
	(compare:CC_NZ (xor:SI (match_operand:SI 1 "s_register_operand" "r,r")
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
  [(set (reg:CC_NZ CC_REGNUM)
	(compare:CC_NZ (xor:SI (match_operand:SI 0 "s_register_operand" "r,r")
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
    (set (match_operand:SI 0 "s_register_operand")
	 (smax:SI (match_operand:SI 1 "s_register_operand")
		  (match_operand:SI 2 "arm_rhs_operand")))
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
   (set_attr "type" "logic_shift_reg")]
)

(define_insn "*smax_m1"
  [(set (match_operand:SI 0 "s_register_operand" "=r")
	(smax:SI (match_operand:SI 1 "s_register_operand" "r")
		 (const_int -1)))]
  "TARGET_32BIT"
  "orr%?\\t%0, %1, %1, asr #31"
  [(set_attr "predicable" "yes")
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
    (set (match_operand:SI 0 "s_register_operand")
	 (smin:SI (match_operand:SI 1 "s_register_operand")
		  (match_operand:SI 2 "arm_rhs_operand")))
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
    (set (match_operand:SI 0 "s_register_operand")
	 (umax:SI (match_operand:SI 1 "s_register_operand")
		  (match_operand:SI 2 "arm_rhs_operand")))
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
   (set_attr "type" "store_4")]
)

(define_expand "uminsi3"
  [(parallel [
    (set (match_operand:SI 0 "s_register_operand")
	 (umin:SI (match_operand:SI 1 "s_register_operand")
		  (match_operand:SI 2 "arm_rhs_operand")))
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
   (set_attr "type" "store_4")]
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
   (set_attr "type" "store_4")]
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


(define_expand "arm_<ss_op>"
  [(set (match_operand:SI 0 "s_register_operand")
	(SSPLUSMINUS:SI (match_operand:SI 1 "s_register_operand")
			(match_operand:SI 2 "s_register_operand")))]
  "TARGET_DSP_MULTIPLY"
  {
    if (ARM_Q_BIT_READ)
      emit_insn (gen_arm_<ss_op>_setq_insn (operands[0],
					    operands[1], operands[2]));
    else
      emit_insn (gen_arm_<ss_op>_insn (operands[0], operands[1], operands[2]));
    DONE;
  }
)

(define_insn "arm_<ss_op><add_clobber_q_name>_insn"
  [(set (match_operand:SI 0 "s_register_operand" "=r")
	(SSPLUSMINUS:SI (match_operand:SI 1 "s_register_operand" "r")
			(match_operand:SI 2 "s_register_operand" "r")))]
  "TARGET_DSP_MULTIPLY && <add_clobber_q_pred>"
  "<ss_op>%?\t%0, %1, %2"
  [(set_attr "predicable" "yes")
   (set_attr "type" "alu_dsp_reg")]
)

(define_code_iterator SAT [smin smax])
(define_code_attr SATrev [(smin "smax") (smax "smin")])
(define_code_attr SATlo [(smin "1") (smax "2")])
(define_code_attr SAThi [(smin "2") (smax "1")])

(define_expand "arm_ssat"
  [(match_operand:SI 0 "s_register_operand")
   (match_operand:SI 1 "s_register_operand")
   (match_operand:SI 2 "const_int_operand")]
  "TARGET_32BIT && arm_arch6"
  {
    HOST_WIDE_INT val = INTVAL (operands[2]);
    /* The builtin checking code should have ensured the right
       range for the immediate.  */
    gcc_assert (IN_RANGE (val, 1, 32));
    HOST_WIDE_INT upper_bound = (HOST_WIDE_INT_1 << (val - 1)) - 1;
    HOST_WIDE_INT lower_bound = -upper_bound - 1;
    rtx up_rtx = gen_int_mode (upper_bound, SImode);
    rtx lo_rtx = gen_int_mode (lower_bound, SImode);
    if (ARM_Q_BIT_READ)
      emit_insn (gen_satsi_smin_setq (operands[0], lo_rtx,
				      up_rtx, operands[1]));
    else
      emit_insn (gen_satsi_smin (operands[0], lo_rtx, up_rtx, operands[1]));
    DONE;
  }
)

(define_expand "arm_usat"
  [(match_operand:SI 0 "s_register_operand")
   (match_operand:SI 1 "s_register_operand")
   (match_operand:SI 2 "const_int_operand")]
  "TARGET_32BIT && arm_arch6"
  {
    HOST_WIDE_INT val = INTVAL (operands[2]);
    /* The builtin checking code should have ensured the right
       range for the immediate.  */
    gcc_assert (IN_RANGE (val, 0, 31));
    HOST_WIDE_INT upper_bound = (HOST_WIDE_INT_1 << val) - 1;
    rtx up_rtx = gen_int_mode (upper_bound, SImode);
    rtx lo_rtx = CONST0_RTX (SImode);
    if (ARM_Q_BIT_READ)
      emit_insn (gen_satsi_smin_setq (operands[0], lo_rtx, up_rtx,
				      operands[1]));
    else
      emit_insn (gen_satsi_smin (operands[0], lo_rtx, up_rtx, operands[1]));
    DONE;
  }
)

(define_insn "arm_get_apsr"
  [(set (match_operand:SI 0 "s_register_operand" "=r")
	(unspec:SI [(reg:CC APSRQ_REGNUM)] UNSPEC_APSR_READ))]
  "TARGET_ARM_QBIT"
  "mrs%?\t%0, APSR"
  [(set_attr "predicable" "yes")
   (set_attr "conds" "use")]
)

(define_insn "arm_set_apsr"
  [(set (reg:CC APSRQ_REGNUM)
	(unspec_volatile:CC
	  [(match_operand:SI 0 "s_register_operand" "r")] VUNSPEC_APSR_WRITE))]
  "TARGET_ARM_QBIT"
  "msr%?\tAPSR_nzcvq, %0"
  [(set_attr "predicable" "yes")
   (set_attr "conds" "set")]
)

;; Read the APSR and extract the Q bit (bit 27)
(define_expand "arm_saturation_occurred"
  [(match_operand:SI 0 "s_register_operand")]
  "TARGET_ARM_QBIT"
  {
    rtx apsr = gen_reg_rtx (SImode);
    emit_insn (gen_arm_get_apsr (apsr));
    emit_insn (gen_extzv (operands[0], apsr, CONST1_RTX (SImode),
	       gen_int_mode (27, SImode)));
    DONE;
  }
)

;; Read the APSR and set the Q bit (bit position 27) according to operand 0
(define_expand "arm_set_saturation"
  [(match_operand:SI 0 "reg_or_int_operand")]
  "TARGET_ARM_QBIT"
  {
    rtx apsr = gen_reg_rtx (SImode);
    emit_insn (gen_arm_get_apsr (apsr));
    rtx to_insert = gen_reg_rtx (SImode);
    if (CONST_INT_P (operands[0]))
      emit_move_insn (to_insert, operands[0] == CONST0_RTX (SImode)
				 ? CONST0_RTX (SImode) : CONST1_RTX (SImode));
    else
      {
        rtx cmp = gen_rtx_NE (SImode, operands[0], CONST0_RTX (SImode));
        emit_insn (gen_cstoresi4 (to_insert, cmp, operands[0],
				  CONST0_RTX (SImode)));
      }
    emit_insn (gen_insv (apsr, CONST1_RTX (SImode),
	       gen_int_mode (27, SImode), to_insert));
    emit_insn (gen_arm_set_apsr (apsr));
    DONE;
  }
)

(define_insn "satsi_<SAT:code><add_clobber_q_name>"
  [(set (match_operand:SI 0 "s_register_operand" "=r")
        (SAT:SI (<SATrev>:SI (match_operand:SI 3 "s_register_operand" "r")
                           (match_operand:SI 1 "const_int_operand" "i"))
                (match_operand:SI 2 "const_int_operand" "i")))]
  "TARGET_32BIT && arm_arch6 && <add_clobber_q_pred>
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
   (set_attr "type" "alus_imm")]
)

(define_insn "*satsi_<SAT:code>_shift"
  [(set (match_operand:SI 0 "s_register_operand" "=r")
        (SAT:SI (<SATrev>:SI (match_operator:SI 3 "sat_shift_operator"
                             [(match_operand:SI 4 "s_register_operand" "r")
                              (match_operand:SI 5 "const_int_operand" "i")])
                           (match_operand:SI 1 "const_int_operand" "i"))
                (match_operand:SI 2 "const_int_operand" "i")))]
  "TARGET_32BIT && arm_arch6 && !ARM_Q_BIT_READ
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
   (set_attr "shift" "3")
   (set_attr "type" "logic_shift_reg")])

;; Custom Datapath Extension insns.
(define_insn "arm_cx1<mode>"
   [(set (match_operand:SIDI 0 "s_register_operand" "=r")
	 (unspec:SIDI [(match_operand:SI 1 "const_int_coproc_operand" "i")
	               (match_operand:SI 2 "const_int_ccde1_operand" "i")]
	    UNSPEC_CDE))]
   "TARGET_CDE"
   "cx1<cde_suffix>\\tp%c1, <cde_dest>, %2"
  [(set_attr "type" "coproc")]
)

(define_insn "arm_cx1a<mode>"
   [(set (match_operand:SIDI 0 "s_register_operand" "=r")
	 (unspec:SIDI [(match_operand:SI 1 "const_int_coproc_operand" "i")
		       (match_operand:SIDI 2 "s_register_operand" "0")
	               (match_operand:SI 3 "const_int_ccde1_operand" "i")]
	    UNSPEC_CDEA))]
   "TARGET_CDE"
   "cx1<cde_suffix>a\\tp%c1, <cde_dest>, %3"
  [(set_attr "type" "coproc")]
)

(define_insn "arm_cx2<mode>"
   [(set (match_operand:SIDI 0 "s_register_operand" "=r")
	 (unspec:SIDI [(match_operand:SI 1 "const_int_coproc_operand" "i")
		       (match_operand:SI 2 "s_register_operand" "r")
	               (match_operand:SI 3 "const_int_ccde2_operand" "i")]
	    UNSPEC_CDE))]
   "TARGET_CDE"
   "cx2<cde_suffix>\\tp%c1, <cde_dest>, %2, %3"
  [(set_attr "type" "coproc")]
)

(define_insn "arm_cx2a<mode>"
   [(set (match_operand:SIDI 0 "s_register_operand" "=r")
	 (unspec:SIDI [(match_operand:SI 1 "const_int_coproc_operand" "i")
		       (match_operand:SIDI 2 "s_register_operand" "0")
		       (match_operand:SI 3 "s_register_operand" "r")
	               (match_operand:SI 4 "const_int_ccde2_operand" "i")]
	    UNSPEC_CDEA))]
   "TARGET_CDE"
   "cx2<cde_suffix>a\\tp%c1, <cde_dest>, %3, %4"
  [(set_attr "type" "coproc")]
)

(define_insn "arm_cx3<mode>"
   [(set (match_operand:SIDI 0 "s_register_operand" "=r")
	 (unspec:SIDI [(match_operand:SI 1 "const_int_coproc_operand" "i")
		       (match_operand:SI 2 "s_register_operand" "r")
		       (match_operand:SI 3 "s_register_operand" "r")
	               (match_operand:SI 4 "const_int_ccde3_operand" "i")]
	    UNSPEC_CDE))]
   "TARGET_CDE"
   "cx3<cde_suffix>\\tp%c1, <cde_dest>, %2, %3, %4"
  [(set_attr "type" "coproc")]
)

(define_insn "arm_cx3a<mode>"
   [(set (match_operand:SIDI 0 "s_register_operand" "=r")
	 (unspec:SIDI [(match_operand:SI 1 "const_int_coproc_operand" "i")
		       (match_operand:SIDI 2 "s_register_operand" "0")
		       (match_operand:SI 3 "s_register_operand" "r")
		       (match_operand:SI 4 "s_register_operand" "r")
                       (match_operand:SI 5 "const_int_ccde3_operand" "i")]
	    UNSPEC_CDEA))]
   "TARGET_CDE"
   "cx3<cde_suffix>a\\tp%c1, <cde_dest>, %3, %4, %5"
  [(set_attr "type" "coproc")]
)

;; Shift and rotation insns

(define_expand "ashldi3"
  [(set (match_operand:DI            0 "s_register_operand")
        (ashift:DI (match_operand:DI 1 "s_register_operand")
                   (match_operand:SI 2 "reg_or_int_operand")))]
  "TARGET_32BIT"
  "
  if (TARGET_HAVE_MVE && !BYTES_BIG_ENDIAN)
    {
      if (!reg_or_int_operand (operands[2], SImode))
        operands[2] = force_reg (SImode, operands[2]);

      /* Armv8.1-M Mainline double shifts are not expanded.  */
      if (arm_reg_or_long_shift_imm (operands[2], GET_MODE (operands[2]))
	  && (REG_P (operands[2]) || INTVAL(operands[2]) != 32))
        {
	  if (!reg_overlap_mentioned_p(operands[0], operands[1]))
	    emit_insn (gen_movdi (operands[0], operands[1]));

	  emit_insn (gen_thumb2_lsll (operands[0], operands[2]));
	  DONE;
	}
    }

  arm_emit_coreregs_64bit_shift (ASHIFT, operands[0], operands[1],
				 operands[2], gen_reg_rtx (SImode),
				 gen_reg_rtx (SImode));
  DONE;
")

(define_expand "ashlsi3"
  [(set (match_operand:SI            0 "s_register_operand")
	(ashift:SI (match_operand:SI 1 "s_register_operand")
		   (match_operand:SI 2 "arm_rhs_operand")))]
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
  [(set (match_operand:DI              0 "s_register_operand")
        (ashiftrt:DI (match_operand:DI 1 "s_register_operand")
                     (match_operand:SI 2 "reg_or_int_operand")))]
  "TARGET_32BIT"
  "
  /* Armv8.1-M Mainline double shifts are not expanded.  */
  if (TARGET_HAVE_MVE && !BYTES_BIG_ENDIAN
      && arm_reg_or_long_shift_imm (operands[2], GET_MODE (operands[2])))
    {
      if (!reg_overlap_mentioned_p(operands[0], operands[1]))
	emit_insn (gen_movdi (operands[0], operands[1]));

      emit_insn (gen_thumb2_asrl (operands[0], operands[2]));
      DONE;
    }

  arm_emit_coreregs_64bit_shift (ASHIFTRT, operands[0], operands[1],
				 operands[2], gen_reg_rtx (SImode),
				 gen_reg_rtx (SImode));
  DONE;
")

(define_expand "ashrsi3"
  [(set (match_operand:SI              0 "s_register_operand")
	(ashiftrt:SI (match_operand:SI 1 "s_register_operand")
		     (match_operand:SI 2 "arm_rhs_operand")))]
  "TARGET_EITHER"
  "
  if (CONST_INT_P (operands[2])
      && UINTVAL (operands[2]) > 31)
    operands[2] = GEN_INT (31);
  "
)

(define_expand "lshrdi3"
  [(set (match_operand:DI              0 "s_register_operand")
        (lshiftrt:DI (match_operand:DI 1 "s_register_operand")
                     (match_operand:SI 2 "reg_or_int_operand")))]
  "TARGET_32BIT"
  "
  /* Armv8.1-M Mainline double shifts are not expanded.  */
  if (TARGET_HAVE_MVE && !BYTES_BIG_ENDIAN
    && long_shift_imm (operands[2], GET_MODE (operands[2])))
    {
      if (!reg_overlap_mentioned_p(operands[0], operands[1]))
        emit_insn (gen_movdi (operands[0], operands[1]));

      emit_insn (gen_thumb2_lsrl (operands[0], operands[2]));
      DONE;
    }

  arm_emit_coreregs_64bit_shift (LSHIFTRT, operands[0], operands[1],
				 operands[2], gen_reg_rtx (SImode),
				 gen_reg_rtx (SImode));
  DONE;
")

(define_expand "lshrsi3"
  [(set (match_operand:SI              0 "s_register_operand")
	(lshiftrt:SI (match_operand:SI 1 "s_register_operand")
		     (match_operand:SI 2 "arm_rhs_operand")))]
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
  [(set (match_operand:SI              0 "s_register_operand")
	(rotatert:SI (match_operand:SI 1 "s_register_operand")
		     (match_operand:SI 2 "reg_or_int_operand")))]
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
  [(set (match_operand:SI              0 "s_register_operand")
	(rotatert:SI (match_operand:SI 1 "s_register_operand")
		     (match_operand:SI 2 "arm_rhs_operand")))]
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
  [(set (reg:CC_NZ CC_REGNUM)
	(compare:CC_NZ (match_operator:SI 3 "shift_operator"
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
  [(set (reg:CC_NZ CC_REGNUM)
	(compare:CC_NZ (match_operator:SI 3 "shift_operator"
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
		  (match_operand:SI 2 "shift_amount_operand" "M,r")])))]
  "TARGET_32BIT"
  "mvn%?\\t%0, %1%S3"
  [(set_attr "predicable" "yes")
   (set_attr "shift" "1")
   (set_attr "arch" "32,a")
   (set_attr "type" "mvn_shift,mvn_shift_reg")])

(define_insn "*not_shiftsi_compare0"
  [(set (reg:CC_NZ CC_REGNUM)
	(compare:CC_NZ
	 (not:SI (match_operator:SI 3 "shift_operator"
		  [(match_operand:SI 1 "s_register_operand" "r,r")
		   (match_operand:SI 2 "shift_amount_operand" "M,r")]))
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
  [(set (reg:CC_NZ CC_REGNUM)
	(compare:CC_NZ
	 (not:SI (match_operator:SI 3 "shift_operator"
		  [(match_operand:SI 1 "s_register_operand" "r,r")
		   (match_operand:SI 2 "shift_amount_operand" "M,r")]))
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
  [(set (match_operand 0 "s_register_operand")
	(zero_extract (match_operand 1 "nonimmediate_operand")
		      (match_operand 2 "const_int_operand")
		      (match_operand 3 "const_int_operand")))]
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
  [(set (match_operand:SI 4 "s_register_operand")
	(ashift:SI (match_operand:SI 1 "nonimmediate_operand")
		   (match_operand:SI 2 "const_int_operand")))
   (set (match_operand:SI 0 "s_register_operand")
	(lshiftrt:SI (match_dup 4)
		     (match_operand:SI 3 "const_int_operand")))]
  "TARGET_THUMB1"
  "")

(define_expand "extv"
  [(set (match_operand 0 "s_register_operand")
	(sign_extract (match_operand 1 "nonimmediate_operand")
		      (match_operand 2 "const_int_operand")
		      (match_operand 3 "const_int_operand")))]
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
  [(set (match_operand:SI 0 "s_register_operand")
	(sign_extract:SI (match_operand:SI 1 "s_register_operand")
			 (match_operand 2 "const_int_operand")
			 (match_operand 3 "const_int_operand")))]
  ""
{
})

; ARMv6+ unaligned load/store instructions (used for packed structure accesses).

(define_insn "unaligned_loaddi"
  [(set (match_operand:DI 0 "s_register_operand" "=r")
	(unspec:DI [(match_operand:DI 1 "memory_operand" "m")]
		   UNSPEC_UNALIGNED_LOAD))]
  "TARGET_32BIT && TARGET_LDRD"
  "*
  return output_move_double (operands, true, NULL);
  "
  [(set_attr "length" "8")
   (set_attr "type" "load_8")])

(define_insn "unaligned_loadsi"
  [(set (match_operand:SI 0 "s_register_operand" "=l,l,r")
	(unspec:SI [(match_operand:SI 1 "memory_operand" "m,Uw,m")]
		   UNSPEC_UNALIGNED_LOAD))]
  "unaligned_access"
  "@
   ldr\t%0, %1\t@ unaligned
   ldr%?\t%0, %1\t@ unaligned
   ldr%?\t%0, %1\t@ unaligned"
  [(set_attr "arch" "t1,t2,32")
   (set_attr "length" "2,2,4")
   (set_attr "predicable" "no,yes,yes")
   (set_attr "predicable_short_it" "no,yes,no")
   (set_attr "type" "load_4")])

;; The 16-bit Thumb1 variant of ldrsh requires two registers in the
;; address (there's no immediate format).  That's tricky to support
;; here and we don't really need this pattern for that case, so only
;; enable for 32-bit ISAs.
(define_insn "unaligned_loadhis"
  [(set (match_operand:SI 0 "s_register_operand" "=r")
	(sign_extend:SI
	  (unspec:HI [(match_operand:HI 1 "memory_operand" "Uh")]
		     UNSPEC_UNALIGNED_LOAD)))]
  "unaligned_access && TARGET_32BIT"
  "ldrsh%?\t%0, %1\t@ unaligned"
  [(set_attr "predicable" "yes")
   (set_attr "type" "load_byte")])

(define_insn "unaligned_loadhiu"
  [(set (match_operand:SI 0 "s_register_operand" "=l,l,r")
	(zero_extend:SI
	  (unspec:HI [(match_operand:HI 1 "memory_operand" "m,Uw,m")]
		     UNSPEC_UNALIGNED_LOAD)))]
  "unaligned_access"
  "@
   ldrh\t%0, %1\t@ unaligned
   ldrh%?\t%0, %1\t@ unaligned
   ldrh%?\t%0, %1\t@ unaligned"
  [(set_attr "arch" "t1,t2,32")
   (set_attr "length" "2,2,4")
   (set_attr "predicable" "no,yes,yes")
   (set_attr "predicable_short_it" "no,yes,no")
   (set_attr "type" "load_byte")])

(define_insn "unaligned_storedi"
  [(set (match_operand:DI 0 "memory_operand" "=m")
	(unspec:DI [(match_operand:DI 1 "s_register_operand" "r")]
		   UNSPEC_UNALIGNED_STORE))]
  "TARGET_32BIT && TARGET_LDRD"
  "*
  return output_move_double (operands, true, NULL);
  "
  [(set_attr "length" "8")
   (set_attr "type" "store_8")])

(define_insn "unaligned_storesi"
  [(set (match_operand:SI 0 "memory_operand" "=m,Uw,m")
	(unspec:SI [(match_operand:SI 1 "s_register_operand" "l,l,r")]
		   UNSPEC_UNALIGNED_STORE))]
  "unaligned_access"
  "@
   str\t%1, %0\t@ unaligned
   str%?\t%1, %0\t@ unaligned
   str%?\t%1, %0\t@ unaligned"
  [(set_attr "arch" "t1,t2,32")
   (set_attr "length" "2,2,4")
   (set_attr "predicable" "no,yes,yes")
   (set_attr "predicable_short_it" "no,yes,no")
   (set_attr "type" "store_4")])

(define_insn "unaligned_storehi"
  [(set (match_operand:HI 0 "memory_operand" "=m,Uw,m")
	(unspec:HI [(match_operand:HI 1 "s_register_operand" "l,l,r")]
		   UNSPEC_UNALIGNED_STORE))]
  "unaligned_access"
  "@
   strh\t%1, %0\t@ unaligned
   strh%?\t%1, %0\t@ unaligned
   strh%?\t%1, %0\t@ unaligned"
  [(set_attr "arch" "t1,t2,32")
   (set_attr "length" "2,2,4")
   (set_attr "predicable" "no,yes,yes")
   (set_attr "predicable_short_it" "no,yes,no")
   (set_attr "type" "store_4")])


(define_insn "*extv_reg"
  [(set (match_operand:SI 0 "s_register_operand" "=r")
	(sign_extract:SI (match_operand:SI 1 "s_register_operand" "r")
			  (match_operand:SI 2 "const_int_operand" "n")
			  (match_operand:SI 3 "const_int_operand" "n")))]
  "arm_arch_thumb2
   && IN_RANGE (INTVAL (operands[3]), 0, 31)
   && IN_RANGE (INTVAL (operands[2]), 1, 32 - INTVAL (operands[3]))"
  "sbfx%?\t%0, %1, %3, %2"
  [(set_attr "length" "4")
   (set_attr "predicable" "yes")
   (set_attr "type" "bfm")]
)

(define_insn "extzv_t2"
  [(set (match_operand:SI 0 "s_register_operand" "=r")
	(zero_extract:SI (match_operand:SI 1 "s_register_operand" "r")
			  (match_operand:SI 2 "const_int_operand" "n")
			  (match_operand:SI 3 "const_int_operand" "n")))]
  "arm_arch_thumb2
   && IN_RANGE (INTVAL (operands[3]), 0, 31)
   && IN_RANGE (INTVAL (operands[2]), 1, 32 - INTVAL (operands[3]))"
  "ubfx%?\t%0, %1, %3, %2"
  [(set_attr "length" "4")
   (set_attr "predicable" "yes")
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
   (set_attr "type" "udiv")]
)


;; Unary arithmetic insns

(define_expand "negv<SIDI:mode>3"
  [(match_operand:SIDI 0 "s_register_operand")
   (match_operand:SIDI 1 "s_register_operand")
   (match_operand 2 "")]
  "TARGET_32BIT"
{
  emit_insn (gen_subv<mode>4 (operands[0], const0_rtx, operands[1],
			      operands[2]));
  DONE;
})

(define_expand "negsi2"
  [(set (match_operand:SI         0 "s_register_operand")
	(neg:SI (match_operand:SI 1 "s_register_operand")))]
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
   (set_attr "type" "alu_imm")]
)

;; To keep the comparison in canonical form we express it as (~reg cmp ~0)
;; rather than (0 cmp reg).  This gives the same results for unsigned
;; and equality compares which is what we mostly need here.
(define_insn "negsi2_0compare"
  [(set (reg:CC_RSB CC_REGNUM)
	(compare:CC_RSB (not:SI (match_operand:SI 1 "s_register_operand" "l,r"))
			(const_int -1)))
   (set (match_operand:SI 0 "s_register_operand" "=l,r")
	(neg:SI (match_dup 1)))]
  "TARGET_32BIT"
  "@
   negs\\t%0, %1
   rsbs\\t%0, %1, #0"
  [(set_attr "conds" "set")
   (set_attr "arch" "t2,*")
   (set_attr "length" "2,*")
   (set_attr "type" "alus_imm")]
)

(define_insn "negsi2_carryin"
  [(set (match_operand:SI 0 "s_register_operand" "=r,r")
	(minus:SI (neg:SI (match_operand:SI 1 "s_register_operand" "r,r"))
		  (match_operand:SI 2 "arm_borrow_operation" "")))]
  "TARGET_32BIT"
  "@
   rsc\\t%0, %1, #0
   sbc\\t%0, %1, %1, lsl #1"
  [(set_attr "conds" "use")
   (set_attr "arch" "a,t2")
   (set_attr "type" "adc_imm,adc_reg")]
)

(define_expand "negsf2"
  [(set (match_operand:SF         0 "s_register_operand")
	(neg:SF (match_operand:SF 1 "s_register_operand")))]
  "TARGET_32BIT && TARGET_HARD_FLOAT"
  ""
)

(define_expand "negdf2"
  [(set (match_operand:DF         0 "s_register_operand")
	(neg:DF (match_operand:DF 1 "s_register_operand")))]
  "TARGET_32BIT && TARGET_HARD_FLOAT && TARGET_VFP_DOUBLE"
  "")

;; abssi2 doesn't really clobber the condition codes if a different register
;; is being set.  To keep things simple, assume during rtl manipulations that
;; it does, but tell the final scan operator the truth.  Similarly for
;; (neg (abs...))

(define_expand "abssi2"
  [(parallel
    [(set (match_operand:SI         0 "s_register_operand")
	  (abs:SI (match_operand:SI 1 "s_register_operand")))
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
  [(set (match_operand:SF         0 "s_register_operand")
	(abs:SF (match_operand:SF 1 "s_register_operand")))]
  "TARGET_32BIT && TARGET_HARD_FLOAT"
  "")

(define_expand "absdf2"
  [(set (match_operand:DF         0 "s_register_operand")
	(abs:DF (match_operand:DF 1 "s_register_operand")))]
  "TARGET_32BIT && TARGET_HARD_FLOAT && !TARGET_VFP_SINGLE"
  "")

(define_expand "sqrtsf2"
  [(set (match_operand:SF 0 "s_register_operand")
	(sqrt:SF (match_operand:SF 1 "s_register_operand")))]
  "TARGET_32BIT && TARGET_HARD_FLOAT"
  "")

(define_expand "sqrtdf2"
  [(set (match_operand:DF 0 "s_register_operand")
	(sqrt:DF (match_operand:DF 1 "s_register_operand")))]
  "TARGET_32BIT && TARGET_HARD_FLOAT && TARGET_VFP_DOUBLE"
  "")

(define_expand "one_cmplsi2"
  [(set (match_operand:SI         0 "s_register_operand")
	(not:SI (match_operand:SI 1 "s_register_operand")))]
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
  [(set (reg:CC_NZ CC_REGNUM)
	(compare:CC_NZ (not:SI (match_operand:SI 1 "s_register_operand" "r"))
			 (const_int 0)))
   (set (match_operand:SI 0 "s_register_operand" "=r")
	(not:SI (match_dup 1)))]
  "TARGET_32BIT"
  "mvns%?\\t%0, %1"
  [(set_attr "conds" "set")
   (set_attr "type" "mvn_reg")]
)

(define_insn "*notsi_compare0_scratch"
  [(set (reg:CC_NZ CC_REGNUM)
	(compare:CC_NZ (not:SI (match_operand:SI 1 "s_register_operand" "r"))
			 (const_int 0)))
   (clobber (match_scratch:SI 0 "=r"))]
  "TARGET_32BIT"
  "mvns%?\\t%0, %1"
  [(set_attr "conds" "set")
   (set_attr "type" "mvn_reg")]
)

;; Fixed <--> Floating conversion insns

(define_expand "floatsihf2"
  [(set (match_operand:HF           0 "general_operand")
	(float:HF (match_operand:SI 1 "general_operand")))]
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
  [(set (match_operand:HF           0 "general_operand")
	(float:HF (match_operand:DI 1 "general_operand")))]
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
  [(set (match_operand:SF           0 "s_register_operand")
	(float:SF (match_operand:SI 1 "s_register_operand")))]
  "TARGET_32BIT && TARGET_HARD_FLOAT"
  "
")

(define_expand "floatsidf2"
  [(set (match_operand:DF           0 "s_register_operand")
	(float:DF (match_operand:SI 1 "s_register_operand")))]
  "TARGET_32BIT && TARGET_HARD_FLOAT && !TARGET_VFP_SINGLE"
  "
")

(define_expand "fix_trunchfsi2"
  [(set (match_operand:SI         0 "general_operand")
	(fix:SI (fix:HF (match_operand:HF 1 "general_operand"))))]
  "TARGET_EITHER"
  "
  {
    rtx op1 = convert_to_mode (SFmode, operands[1], 0);
    expand_fix (operands[0], op1, 0);
    DONE;
  }"
)

(define_expand "fix_trunchfdi2"
  [(set (match_operand:DI         0 "general_operand")
	(fix:DI (fix:HF (match_operand:HF 1 "general_operand"))))]
  "TARGET_EITHER"
  "
  {
    rtx op1 = convert_to_mode (SFmode, operands[1], 0);
    expand_fix (operands[0], op1, 0);
    DONE;
  }"
)

(define_expand "fix_truncsfsi2"
  [(set (match_operand:SI         0 "s_register_operand")
	(fix:SI (fix:SF (match_operand:SF 1 "s_register_operand"))))]
  "TARGET_32BIT && TARGET_HARD_FLOAT"
  "
")

(define_expand "fix_truncdfsi2"
  [(set (match_operand:SI         0 "s_register_operand")
	(fix:SI (fix:DF (match_operand:DF 1 "s_register_operand"))))]
  "TARGET_32BIT && TARGET_HARD_FLOAT && !TARGET_VFP_SINGLE"
  "
")

;; Truncation insns

(define_expand "truncdfsf2"
  [(set (match_operand:SF  0 "s_register_operand")
	(float_truncate:SF
	 (match_operand:DF 1 "s_register_operand")))]
  "TARGET_32BIT && TARGET_HARD_FLOAT && !TARGET_VFP_SINGLE"
  ""
)

;; DFmode to HFmode conversions on targets without a single-step hardware
;; instruction for it would have to go through SFmode.  This is dangerous
;; as it introduces double rounding.
;;
;; Disable this pattern unless we are in an unsafe math mode, or we have
;; a single-step instruction.

(define_expand "truncdfhf2"
  [(set (match_operand:HF  0 "s_register_operand")
	(float_truncate:HF
	 (match_operand:DF 1 "s_register_operand")))]
  "(TARGET_EITHER && flag_unsafe_math_optimizations)
   || (TARGET_32BIT && TARGET_FP16_TO_DOUBLE)"
{
  /* We don't have a direct instruction for this, so we must be in
     an unsafe math mode, and going via SFmode.  */

  if (!(TARGET_32BIT && TARGET_FP16_TO_DOUBLE))
    {
      rtx op1;
      op1 = convert_to_mode (SFmode, operands[1], 0);
      op1 = convert_to_mode (HFmode, op1, 0);
      emit_move_insn (operands[0], op1);
      DONE;
    }
  /* Otherwise, we will pick this up as a single instruction with
     no intermediary rounding.  */
}
)

;; Zero and sign extension instructions.

(define_expand "zero_extend<mode>di2"
  [(set (match_operand:DI 0 "s_register_operand" "")
	(zero_extend:DI (match_operand:QHSI 1 "<qhs_zextenddi_op>" "")))]
  "TARGET_32BIT <qhs_zextenddi_cond>"
  {
    rtx res_lo, res_hi, op0_lo, op0_hi;
    res_lo = gen_lowpart (SImode, operands[0]);
    res_hi = gen_highpart (SImode, operands[0]);
    if (can_create_pseudo_p ())
      {
	op0_lo = <MODE>mode == SImode ? operands[1] : gen_reg_rtx (SImode);
	op0_hi = gen_reg_rtx (SImode);
      }
    else
      {
	op0_lo = <MODE>mode == SImode ? operands[1] : res_lo;
	op0_hi = res_hi;
      }
    if (<MODE>mode != SImode)
      emit_insn (gen_rtx_SET (op0_lo,
			      gen_rtx_ZERO_EXTEND (SImode, operands[1])));
    emit_insn (gen_movsi (op0_hi, const0_rtx));
    if (res_lo != op0_lo)
      emit_move_insn (res_lo, op0_lo);
    if (res_hi != op0_hi)
      emit_move_insn (res_hi, op0_hi);
    DONE;
  }
)

(define_expand "extend<mode>di2"
  [(set (match_operand:DI 0 "s_register_operand" "")
	(sign_extend:DI (match_operand:QHSI 1 "<qhs_extenddi_op>" "")))]
  "TARGET_32BIT <qhs_sextenddi_cond>"
  {
    rtx res_lo, res_hi, op0_lo, op0_hi;
    res_lo = gen_lowpart (SImode, operands[0]);
    res_hi = gen_highpart (SImode, operands[0]);
    if (can_create_pseudo_p ())
      {
	op0_lo = <MODE>mode == SImode ? operands[1] : gen_reg_rtx (SImode);
	op0_hi = gen_reg_rtx (SImode);
      }
    else
      {
	op0_lo = <MODE>mode == SImode ? operands[1] : res_lo;
	op0_hi = res_hi;
      }
    if (<MODE>mode != SImode)
      emit_insn (gen_rtx_SET (op0_lo,
			      gen_rtx_SIGN_EXTEND (SImode, operands[1])));
    emit_insn (gen_ashrsi3 (op0_hi, op0_lo, GEN_INT (31)));
    if (res_lo != op0_lo)
      emit_move_insn (res_lo, op0_lo);
    if (res_hi != op0_hi)
      emit_move_insn (res_hi, op0_hi);
    DONE;
  }
)

;; Splits for all extensions to DImode
(define_split
  [(set (match_operand:DI 0 "s_register_operand" "")
        (zero_extend:DI (match_operand 1 "nonimmediate_operand" "")))]
  "TARGET_32BIT"
  [(set (match_dup 0) (match_dup 1))]
{
  rtx lo_part = gen_lowpart (SImode, operands[0]);
  machine_mode src_mode = GET_MODE (operands[1]);

  if (src_mode == SImode)
    emit_move_insn (lo_part, operands[1]);
  else
    emit_insn (gen_rtx_SET (lo_part,
			    gen_rtx_ZERO_EXTEND (SImode, operands[1])));
  operands[0] = gen_highpart (SImode, operands[0]);
  operands[1] = const0_rtx;
})

(define_split
  [(set (match_operand:DI 0 "s_register_operand" "")
        (sign_extend:DI (match_operand 1 "nonimmediate_operand" "")))]
  "TARGET_32BIT"
  [(set (match_dup 0) (ashiftrt:SI (match_dup 1) (const_int 31)))]
{
  rtx lo_part = gen_lowpart (SImode, operands[0]);
  machine_mode src_mode = GET_MODE (operands[1]);

  if (src_mode == SImode)
    emit_move_insn (lo_part, operands[1]);
  else
    emit_insn (gen_rtx_SET (lo_part,
			    gen_rtx_SIGN_EXTEND (SImode, operands[1])));
  operands[1] = lo_part;
  operands[0] = gen_highpart (SImode, operands[0]);
})

(define_expand "zero_extendhisi2"
  [(set (match_operand:SI 0 "s_register_operand")
	(zero_extend:SI (match_operand:HI 1 "nonimmediate_operand")))]
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
   (set_attr "predicable" "yes")]
)

(define_expand "zero_extendqisi2"
  [(set (match_operand:SI 0 "s_register_operand")
	(zero_extend:SI (match_operand:QI 1 "nonimmediate_operand")))]
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
   (set_attr "type" "logic_imm")]
)

(define_expand "extendhisi2"
  [(set (match_operand:SI 0 "s_register_operand")
	(sign_extend:SI (match_operand:HI 1 "nonimmediate_operand")))]
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
   (set_attr "predicable" "yes")]
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
	(ashift:SI (match_operand:QI 1 "arm_reg_or_extendqisi_mem_op")
		   (const_int 24)))
   (set (match_operand:HI 0 "s_register_operand")
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
  [(set (match_operand:SI 0 "s_register_operand")
	(sign_extend:SI (match_operand:QI 1 "arm_reg_or_extendqisi_mem_op")))]
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
   (set_attr "predicable" "yes")]
)

(define_insn "arm_<sup>xtb16"
  [(set (match_operand:SI 0 "s_register_operand" "=r")
	(unspec:SI
	  [(match_operand:SI 1 "s_register_operand" "r")] USXTB16))]
  "TARGET_INT_SIMD"
  "<sup>xtb16%?\\t%0, %1"
  [(set_attr "predicable" "yes")
   (set_attr "type" "alu_dsp_reg")])

(define_insn "arm_<simd32_op>"
  [(set (match_operand:SI 0 "s_register_operand" "=r")
	(unspec:SI
	  [(match_operand:SI 1 "s_register_operand" "r")
	   (match_operand:SI 2 "s_register_operand" "r")] SIMD32_NOGE_BINOP))]
  "TARGET_INT_SIMD"
  "<simd32_op>%?\\t%0, %1, %2"
  [(set_attr "predicable" "yes")
   (set_attr "type" "alu_dsp_reg")])

(define_insn "arm_usada8"
  [(set (match_operand:SI 0 "s_register_operand" "=r")
	(unspec:SI
	  [(match_operand:SI 1 "s_register_operand" "r")
	  (match_operand:SI 2 "s_register_operand" "r")
	  (match_operand:SI 3 "s_register_operand" "r")] UNSPEC_USADA8))]
  "TARGET_INT_SIMD"
  "usada8%?\\t%0, %1, %2, %3"
  [(set_attr "predicable" "yes")
   (set_attr "type" "alu_dsp_reg")])

(define_insn "arm_<simd32_op>"
  [(set (match_operand:DI 0 "s_register_operand" "=r")
	(unspec:DI
	  [(match_operand:SI 1 "s_register_operand" "r")
	   (match_operand:SI 2 "s_register_operand" "r")
	   (match_operand:DI 3 "s_register_operand" "0")] SIMD32_DIMODE))]
  "TARGET_INT_SIMD"
  "<simd32_op>%?\\t%Q0, %R0, %1, %2"
  [(set_attr "predicable" "yes")
   (set_attr "type" "smlald")])

(define_insn "arm_<simd32_op>"
  [(set (match_operand:SI 0 "s_register_operand" "=r")
	(unspec:SI
	  [(match_operand:SI 1 "s_register_operand" "r")
	   (match_operand:SI 2 "s_register_operand" "r")] SIMD32_GE))
   (set (reg:CC APSRGE_REGNUM)
	(unspec:CC [(reg:CC APSRGE_REGNUM)] UNSPEC_GE_SET))]
  "TARGET_INT_SIMD"
  "<simd32_op>%?\\t%0, %1, %2"
  [(set_attr "predicable" "yes")
   (set_attr "type" "alu_sreg")])

(define_insn "arm_<simd32_op><add_clobber_q_name>_insn"
  [(set (match_operand:SI 0 "s_register_operand" "=r")
	(unspec:SI
	  [(match_operand:SI 1 "s_register_operand" "r")
	   (match_operand:SI 2 "s_register_operand" "r")
	   (match_operand:SI 3 "s_register_operand" "r")] SIMD32_TERNOP_Q))]
  "TARGET_INT_SIMD && <add_clobber_q_pred>"
  "<simd32_op>%?\\t%0, %1, %2, %3"
  [(set_attr "predicable" "yes")
   (set_attr "type" "alu_sreg")])

(define_expand "arm_<simd32_op>"
  [(set (match_operand:SI 0 "s_register_operand")
	(unspec:SI
	  [(match_operand:SI 1 "s_register_operand")
	   (match_operand:SI 2 "s_register_operand")
	   (match_operand:SI 3 "s_register_operand")] SIMD32_TERNOP_Q))]
  "TARGET_INT_SIMD"
  {
    if (ARM_Q_BIT_READ)
      emit_insn (gen_arm_<simd32_op>_setq_insn (operands[0], operands[1],
						operands[2], operands[3]));
    else
      emit_insn (gen_arm_<simd32_op>_insn (operands[0], operands[1],
					   operands[2], operands[3]));
    DONE;
  }
)

(define_insn "arm_<simd32_op><add_clobber_q_name>_insn"
  [(set (match_operand:SI 0 "s_register_operand" "=r")
	(unspec:SI
	  [(match_operand:SI 1 "s_register_operand" "r")
	   (match_operand:SI 2 "s_register_operand" "r")] SIMD32_BINOP_Q))]
  "TARGET_INT_SIMD && <add_clobber_q_pred>"
  "<simd32_op>%?\\t%0, %1, %2"
  [(set_attr "predicable" "yes")
   (set_attr "type" "alu_sreg")])

(define_expand "arm_<simd32_op>"
  [(set (match_operand:SI 0 "s_register_operand")
	(unspec:SI
	  [(match_operand:SI 1 "s_register_operand")
	   (match_operand:SI 2 "s_register_operand")] SIMD32_BINOP_Q))]
  "TARGET_INT_SIMD"
  {
    if (ARM_Q_BIT_READ)
      emit_insn (gen_arm_<simd32_op>_setq_insn (operands[0], operands[1],
						operands[2]));
    else
      emit_insn (gen_arm_<simd32_op>_insn (operands[0], operands[1],
					   operands[2]));
    DONE;
  }
)

(define_insn "arm_<simd32_op><add_clobber_q_name>_insn"
  [(set (match_operand:SI 0 "s_register_operand" "=r")
	(unspec:SI
	  [(match_operand:SI 1 "s_register_operand" "r")
	   (match_operand:SI 2 "<sup>sat16_imm" "i")] USSAT16))]
  "TARGET_INT_SIMD && <add_clobber_q_pred>"
  "<simd32_op>%?\\t%0, %2, %1"
  [(set_attr "predicable" "yes")
   (set_attr "type" "alu_sreg")])

(define_expand "arm_<simd32_op>"
  [(set (match_operand:SI 0 "s_register_operand")
	(unspec:SI
	  [(match_operand:SI 1 "s_register_operand")
	   (match_operand:SI 2 "<sup>sat16_imm")] USSAT16))]
  "TARGET_INT_SIMD"
  {
    if (ARM_Q_BIT_READ)
      emit_insn (gen_arm_<simd32_op>_setq_insn (operands[0], operands[1],
						operands[2]));
    else
      emit_insn (gen_arm_<simd32_op>_insn (operands[0], operands[1],
					   operands[2]));
    DONE;
  }
)

(define_insn "arm_sel"
  [(set (match_operand:SI 0 "s_register_operand" "=r")
	(unspec:SI
	  [(match_operand:SI 1 "s_register_operand" "r")
	   (match_operand:SI 2 "s_register_operand" "r")
	   (reg:CC APSRGE_REGNUM)] UNSPEC_SEL))]
  "TARGET_INT_SIMD"
  "sel%?\\t%0, %1, %2"
  [(set_attr "predicable" "yes")
   (set_attr "type" "alu_sreg")])

(define_expand "extendsfdf2"
  [(set (match_operand:DF                  0 "s_register_operand")
	(float_extend:DF (match_operand:SF 1 "s_register_operand")))]
  "TARGET_32BIT && TARGET_HARD_FLOAT && !TARGET_VFP_SINGLE"
  ""
)

;; HFmode -> DFmode conversions where we don't have an instruction for it
;; must go through SFmode.
;;
;; This is always safe for an extend.

(define_expand "extendhfdf2"
  [(set (match_operand:DF		   0 "s_register_operand")
	(float_extend:DF (match_operand:HF 1 "s_register_operand")))]
  "TARGET_EITHER"
{
  /* We don't have a direct instruction for this, so go via SFmode.  */
  if (!(TARGET_32BIT && TARGET_FP16_TO_DOUBLE))
    {
      rtx op1;
      op1 = convert_to_mode (SFmode, operands[1], 0);
      op1 = convert_to_mode (DFmode, op1, 0);
      emit_insn (gen_movdf (operands[0], op1));
      DONE;
    }
  /* Otherwise, we're done producing RTL and will pick up the correct
     pattern to do this with one rounding-step in a single instruction.  */
}
)

;; Move insns (including loads and stores)

;; XXX Just some ideas about movti.
;; I don't think these are a good idea on the arm, there just aren't enough
;; registers
;;(define_expand "loadti"
;;  [(set (match_operand:TI 0 "s_register_operand")
;;	(mem:TI (match_operand:SI 1 "address_operand")))]
;;  "" "")

;;(define_expand "storeti"
;;  [(set (mem:TI (match_operand:TI 0 "address_operand"))
;;	(match_operand:TI 1 "s_register_operand"))]
;;  "" "")

;;(define_expand "movti"
;;  [(set (match_operand:TI 0 "general_operand")
;;	(match_operand:TI 1 "general_operand"))]
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
  [(set (match_operand:DI 0 "general_operand")
	(match_operand:DI 1 "general_operand"))]
  "TARGET_EITHER"
  "
  gcc_checking_assert (aligned_operand (operands[0], DImode));
  gcc_checking_assert (aligned_operand (operands[1], DImode));
  if (can_create_pseudo_p ())
    {
      if (!REG_P (operands[0]))
	operands[1] = force_reg (DImode, operands[1]);
    }
  if (REG_P (operands[0]) && REGNO (operands[0]) <= LAST_ARM_REGNUM
      && !targetm.hard_regno_mode_ok (REGNO (operands[0]), DImode))
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
	   && !targetm.hard_regno_mode_ok (REGNO (operands[1]), DImode))
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
  [(set (match_operand:DI 0 "nonimmediate_di_operand" "=r, r, r, r, m")
	(match_operand:DI 1 "di_operand"              "rDa,Db,Dc,mi,r"))]
  "TARGET_32BIT
   && !(TARGET_HARD_FLOAT)
   && !(TARGET_HAVE_MVE || TARGET_HAVE_MVE_FLOAT)
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
    case 3:
      /* Cannot load it directly, split to load it via MOV / MOVT.  */
      if (!MEM_P (operands[1]) && arm_disable_literal_pool)
	return \"#\";
      /* Fall through.  */
    default:
      return output_move_double (operands, true, NULL);
    }
  "
  [(set_attr "length" "8,12,16,8,8")
   (set_attr "type" "multiple,multiple,multiple,load_8,store_8")
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
   && (arm_disable_literal_pool
       || (arm_const_double_inline_cost (operands[1])
	   <= arm_max_const_double_inline_cost ()))"
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
  [(set (match_operand:ANY64_BF 0 "arm_general_register_operand" "")
	(match_operand:ANY64_BF 1 "arm_general_register_operand" ""))]
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
  [(set (match_operand:SI 0 "general_operand")
        (match_operand:SI 1 "general_operand"))]
  "TARGET_EITHER"
  "
  {
  rtx base, offset, tmp;

  gcc_checking_assert (aligned_operand (operands[0], SImode));
  gcc_checking_assert (aligned_operand (operands[1], SImode));
  if (TARGET_32BIT || TARGET_HAVE_MOVT)
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
  else /* Target doesn't have MOVT...  */
    {
      if (can_create_pseudo_p ())
        {
          if (!REG_P (operands[0]))
	    operands[1] = force_reg (SImode, operands[1]);
        }
    }

  split_const (operands[1], &base, &offset);
  if (INTVAL (offset) != 0
      && targetm.cannot_force_const_mem (SImode, operands[1]))
    {
      tmp = can_create_pseudo_p () ? gen_reg_rtx (SImode) : operands[0];
      emit_move_insn (tmp, base);
      emit_insn (gen_addsi3 (operands[0], tmp, offset));
      DONE;
    }

  tmp = can_create_pseudo_p () ? NULL_RTX : operands[0];

  /* Recognize the case where operand[1] is a reference to thread-local
     data and load its address to a register.  Offsets have been split off
     already.  */
  if (arm_tls_referenced_p (operands[1]))
    operands[1] = legitimize_tls_address (operands[1], tmp);
  else if (flag_pic
	   && (CONSTANT_P (operands[1])
	       || symbol_mentioned_p (operands[1])
	       || label_mentioned_p (operands[1])))
    operands[1] =
      legitimize_pic_address (operands[1], SImode, tmp, NULL_RTX, false);
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
   (set_attr "length" "4")
   (set_attr "type" "alu_sreg")]
)

(define_insn "*arm_movsi_insn"
  [(set (match_operand:SI 0 "nonimmediate_operand" "=rk,r,r,r,rk,m")
	(match_operand:SI 1 "general_operand"      "rk, I,K,j,mi,rk"))]
  "TARGET_ARM && !TARGET_IWMMXT && !TARGET_HARD_FLOAT
   && (   register_operand (operands[0], SImode)
       || register_operand (operands[1], SImode))"
  "@
   mov%?\\t%0, %1
   mov%?\\t%0, %1
   mvn%?\\t%0, #%B1
   movw%?\\t%0, %1
   ldr%?\\t%0, %1
   str%?\\t%1, %0"
  [(set_attr "type" "mov_reg,mov_imm,mvn_imm,mov_imm,load_4,store_4")
   (set_attr "predicable" "yes")
   (set_attr "arch" "*,*,*,v6t2,*,*")
   (set_attr "pool_range" "*,*,*,*,4096,*")
   (set_attr "neg_pool_range" "*,*,*,*,4084,*")]
)

(define_split
  [(set (match_operand:SI 0 "arm_general_register_operand" "")
	(match_operand:SI 1 "const_int_operand" ""))]
  "(TARGET_32BIT || TARGET_HAVE_MOVT)
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
   && !target_word_relocations
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
  [(set (match_operand:SI 0 "register_operand")
	(mem:SI (plus:SI (match_operand:SI 1 "register_operand")
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
 [(set_attr "type" "load_4,load_4,load_4")
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
  [(set_attr "type" "load_4")
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
  [(set_attr "type" "load_4")
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
   (set_attr "type" "load_4")]
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
  [(set_attr "type" "load_4")]
)

(define_expand "builtin_setjmp_receiver"
  [(label_ref (match_operand 0 "" ""))]
  "flag_pic"
  "
{
  /* r3 is clobbered by set/longjmp, so we can use it as a scratch
     register.  */
  if (arm_pic_register != INVALID_REGNUM)
    arm_load_pic_register (1UL << 3, NULL_RTX);
  DONE;
}")

;; If copying one reg to another we can set the condition codes according to
;; its value.  Such a move is common after a return from subroutine and the
;; result is being tested against zero.

(define_insn "*movsi_compare0"
  [(set (reg:CC CC_REGNUM)
	(compare:CC (match_operand:SI 1 "s_register_operand" "0,0,l,rk,rk")
		    (const_int 0)))
   (set (match_operand:SI 0 "s_register_operand" "=l,rk,l,r,rk")
	(match_dup 1))]
  "TARGET_32BIT"
  "@
   cmp%?\\t%0, #0
   cmp%?\\t%0, #0
   subs%?\\t%0, %1, #0
   subs%?\\t%0, %1, #0
   subs%?\\t%0, %1, #0"
  [(set_attr "conds" "set")
   (set_attr "arch" "t2,*,t2,t2,a")
   (set_attr "type" "alus_imm")
   (set_attr "length" "2,4,2,4,4")]
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
  [(set (match_operand:HI 0 "memory_operand")
	(match_operand:HI 1 "general_operand"))]
  "TARGET_32BIT && arm_arch4"
  "
  if (!s_register_operand (operands[1], HImode))
    operands[1] = copy_to_mode_reg (HImode, operands[1]);
  "
)

(define_expand "movhi"
  [(set (match_operand:HI 0 "general_operand")
	(match_operand:HI 1 "general_operand"))]
  "TARGET_EITHER"
  "
  gcc_checking_assert (aligned_operand (operands[0], HImode));
  gcc_checking_assert (aligned_operand (operands[1], HImode));
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
	(rotate:SI (subreg:SI (match_operand:HI 1 "memory_operand") 0)
		   (const_int 16)))
   (set (match_dup 3)
	(ashiftrt:SI (match_dup 2) (const_int 16)))
   (set (match_operand:HI 0 "s_register_operand")
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
   && arm_arch4 && !TARGET_HARD_FLOAT
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
                          (const_string "store_4")
                          (const_string "load_4")])]
)

(define_insn "*movhi_bytes"
  [(set (match_operand:HI 0 "s_register_operand" "=r,r,r")
	(match_operand:HI 1 "arm_rhs_operand"  "I,rk,K"))]
  "TARGET_ARM && !TARGET_HARD_FLOAT"
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
;; The reload_in<m> and reload_out<m> patterns require special constraints
;; to be correctly handled in default_secondary_reload function.
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
  [(set (match_operand:QI 0 "general_operand")
        (match_operand:QI 1 "general_operand"))]
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
  [(set_attr "type" "mov_reg,mov_reg,mov_imm,mov_imm,mvn_imm,load_4,store_4,load_4,store_4")
   (set_attr "predicable" "yes")
   (set_attr "predicable_short_it" "yes,yes,no,yes,no,no,no,no,no")
   (set_attr "arch" "t2,any,any,t2,any,t2,t2,any,any")
   (set_attr "length" "2,4,4,2,4,2,2,4,4")]
)

;; HFmode and BFmode moves.
(define_expand "mov<mode>"
  [(set (match_operand:HFBF 0 "general_operand")
	(match_operand:HFBF 1 "general_operand"))]
  "TARGET_EITHER"
  "
  gcc_checking_assert (aligned_operand (operands[0], <MODE>mode));
  gcc_checking_assert (aligned_operand (operands[1], <MODE>mode));
  if (TARGET_32BIT)
    {
      if (MEM_P (operands[0]))
	operands[1] = force_reg (<MODE>mode, operands[1]);
    }
  else /* TARGET_THUMB1 */
    {
      if (can_create_pseudo_p ())
        {
           if (!REG_P (operands[0]))
	     operands[1] = force_reg (<MODE>mode, operands[1]);
        }
    }
  "
)

(define_insn "*arm32_mov<mode>"
  [(set (match_operand:HFBF 0 "nonimmediate_operand" "=r,m,r,r")
	(match_operand:HFBF 1 "general_operand"	   " m,r,r,F"))]
  "TARGET_32BIT && !TARGET_HARD_FLOAT
   && (	  s_register_operand (operands[0], <MODE>mode)
       || s_register_operand (operands[1], <MODE>mode))"
  "*
  switch (which_alternative)
    {
    case 0:	/* ARM register from memory */
      return \"ldrh%?\\t%0, %1\\t%@ __<fporbf>\";
    case 1:	/* memory from ARM register */
      return \"strh%?\\t%1, %0\\t%@ __<fporbf>\";
    case 2:	/* ARM register from ARM register */
      return \"mov%?\\t%0, %1\\t%@ __<fporbf>\";
    case 3:	/* ARM register from constant */
      {
	long bits;
	rtx ops[4];

	bits = real_to_target (NULL, CONST_DOUBLE_REAL_VALUE (operands[1]),
			       <MODE>mode);
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
   (set_attr "type" "load_4,store_4,mov_reg,multiple")
   (set_attr "length" "4,4,4,8")
   (set_attr "predicable" "yes")]
)

(define_expand "movsf"
  [(set (match_operand:SF 0 "general_operand")
	(match_operand:SF 1 "general_operand"))]
  "TARGET_EITHER"
  "
  gcc_checking_assert (aligned_operand (operands[0], SFmode));
  gcc_checking_assert (aligned_operand (operands[1], SFmode));
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

  /* Cannot load it directly, generate a load with clobber so that it can be
     loaded via GPR with MOV / MOVT.  */
  if (arm_disable_literal_pool
      && (REG_P (operands[0]) || SUBREG_P (operands[0]))
      && CONST_DOUBLE_P (operands[1])
      && TARGET_HARD_FLOAT
      && !vfp3_const_double_rtx (operands[1]))
    {
      rtx clobreg = gen_reg_rtx (SFmode);
      emit_insn (gen_no_literal_pool_sf_immediate (operands[0], operands[1],
						   clobreg));
      DONE;
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
   && TARGET_SOFT_FLOAT && !TARGET_HAVE_MVE
   && (!MEM_P (operands[0])
       || register_operand (operands[1], SFmode))"
{
  switch (which_alternative)
    {
    case 0: return \"mov%?\\t%0, %1\";
    case 1:
      /* Cannot load it directly, split to load it via MOV / MOVT.  */
      if (!MEM_P (operands[1]) && arm_disable_literal_pool)
	return \"#\";
      return \"ldr%?\\t%0, %1\\t%@ float\";
    case 2: return \"str%?\\t%1, %0\\t%@ float\";
    default: gcc_unreachable ();
    }
}
  [(set_attr "predicable" "yes")
   (set_attr "type" "mov_reg,load_4,store_4")
   (set_attr "arm_pool_range" "*,4096,*")
   (set_attr "thumb2_pool_range" "*,4094,*")
   (set_attr "arm_neg_pool_range" "*,4084,*")
   (set_attr "thumb2_neg_pool_range" "*,0,*")]
)

;; Splitter for the above.
(define_split
  [(set (match_operand:SF 0 "s_register_operand")
	(match_operand:SF 1 "const_double_operand"))]
  "arm_disable_literal_pool && TARGET_SOFT_FLOAT"
  [(const_int 0)]
{
  long buf;
  real_to_target (&buf, CONST_DOUBLE_REAL_VALUE (operands[1]), SFmode);
  rtx cst = gen_int_mode (buf, SImode);
  emit_move_insn (simplify_gen_subreg (SImode, operands[0], SFmode, 0), cst);
  DONE;
}
)

(define_expand "movdf"
  [(set (match_operand:DF 0 "general_operand")
	(match_operand:DF 1 "general_operand"))]
  "TARGET_EITHER"
  "
  gcc_checking_assert (aligned_operand (operands[0], DFmode));
  gcc_checking_assert (aligned_operand (operands[1], DFmode));
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

  /* Cannot load it directly, generate a load with clobber so that it can be
     loaded via GPR with MOV / MOVT.  */
  if (arm_disable_literal_pool
      && (REG_P (operands[0]) || SUBREG_P (operands[0]))
      && CONSTANT_P (operands[1])
      && TARGET_HARD_FLOAT
      && !arm_const_double_rtx (operands[1])
      && !(TARGET_VFP_DOUBLE && vfp3_const_double_rtx (operands[1])))
    {
      rtx clobreg = gen_reg_rtx (DFmode);
      emit_insn (gen_no_literal_pool_df_immediate (operands[0], operands[1],
						   clobreg));
      DONE;
    }
  "
)

;; Reloading a df mode value stored in integer regs to memory can require a
;; scratch reg.
;; Another reload_out<m> pattern that requires special constraints.
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
  [(set (match_operand:DF 0 "nonimmediate_soft_df_operand" "=r,r,r,r,m")
       (match_operand:DF 1 "soft_df_operand" "rDa,Db,Dc,mF,r"))]
  "TARGET_32BIT && TARGET_SOFT_FLOAT && !TARGET_HAVE_MVE
   && (   register_operand (operands[0], DFmode)
       || register_operand (operands[1], DFmode))"
  "*
  switch (which_alternative)
    {
    case 0:
    case 1:
    case 2:
      return \"#\";
    case 3:
      /* Cannot load it directly, split to load it via MOV / MOVT.  */
      if (!MEM_P (operands[1]) && arm_disable_literal_pool)
	return \"#\";
      /* Fall through.  */
    default:
      return output_move_double (operands, true, NULL);
    }
  "
  [(set_attr "length" "8,12,16,8,8")
   (set_attr "type" "multiple,multiple,multiple,load_8,store_8")
   (set_attr "arm_pool_range" "*,*,*,1020,*")
   (set_attr "thumb2_pool_range" "*,*,*,1018,*")
   (set_attr "arm_neg_pool_range" "*,*,*,1004,*")
   (set_attr "thumb2_neg_pool_range" "*,*,*,0,*")]
)

;; Splitter for the above.
(define_split
  [(set (match_operand:DF 0 "s_register_operand")
	(match_operand:DF 1 "const_double_operand"))]
  "arm_disable_literal_pool && TARGET_SOFT_FLOAT"
  [(const_int 0)]
{
  long buf[2];
  int order = BYTES_BIG_ENDIAN ? 1 : 0;
  real_to_target (buf, CONST_DOUBLE_REAL_VALUE (operands[1]), DFmode);
  unsigned HOST_WIDE_INT ival = zext_hwi (buf[order], 32);
  ival |= (zext_hwi (buf[1 - order], 32) << 32);
  rtx cst = gen_int_mode (ival, DImode);
  emit_move_insn (simplify_gen_subreg (DImode, operands[0], DFmode, 0), cst);
  DONE;
}
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
  [(match_operand:BLK 0 "general_operand")
   (match_operand:SI 1 "const_int_operand")
   (match_operand:SI 2 "const_int_operand")
   (match_operand:SI 3 "const_int_operand")]
  "TARGET_32BIT"
{
  if (arm_gen_setmem (operands))
    DONE;

  FAIL;
})


;; Move a block of memory if it is word aligned and MORE than 2 words long.
;; We could let this apply for blocks of less than this, but it clobbers so
;; many registers that there is then probably a better way.

(define_expand "cpymemqi"
  [(match_operand:BLK 0 "general_operand")
   (match_operand:BLK 1 "general_operand")
   (match_operand:SI 2 "const_int_operand")
   (match_operand:SI 3 "const_int_operand")]
  ""
  "
  if (TARGET_32BIT)
    {
      if (TARGET_LDRD && current_tune->prefer_ldrd_strd
          && !optimize_function_for_size_p (cfun))
        {
          if (gen_cpymem_ldrd_strd (operands))
            DONE;
          FAIL;
        }

      if (arm_gen_cpymemqi (operands))
        DONE;
      FAIL;
    }
  else /* TARGET_THUMB1 */
    {
      if (   INTVAL (operands[3]) != 4
          || INTVAL (operands[2]) > 48)
        FAIL;

      thumb_expand_cpymemqi (operands);
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
;; In 16-bit Thumb these ranges are:
;; For a 'b'       pos_range = 2046, neg_range = -2048 giving (-2040->2048).
;; For a 'b<cond>' pos_range = 254,  neg_range = -256  giving (-250 ->256).

;; In 32-bit Thumb these ranges are:
;; For a 'b'       +/- 16MB is not checked for.
;; For a 'b<cond>' pos_range = 1048574,  neg_range = -1048576  giving
;; (-1048568 -> 1048576).

(define_expand "cbranchsi4"
  [(set (pc) (if_then_else
	      (match_operator 0 "expandable_comparison_operator"
	       [(match_operand:SI 1 "s_register_operand")
	        (match_operand:SI 2 "nonmemory_operand")])
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
	       [(match_operand:SF 1 "s_register_operand")
	        (match_operand:SF 2 "vfp_compare_operand")])
	      (label_ref (match_operand 3 "" ""))
	      (pc)))]
  "TARGET_32BIT && TARGET_HARD_FLOAT"
  "emit_jump_insn (gen_cbranch_cc (operands[0], operands[1], operands[2],
				   operands[3])); DONE;"
)

(define_expand "cbranchdf4"
  [(set (pc) (if_then_else
	      (match_operator 0 "expandable_comparison_operator"
	       [(match_operand:DF 1 "s_register_operand")
	        (match_operand:DF 2 "vfp_compare_operand")])
	      (label_ref (match_operand 3 "" ""))
	      (pc)))]
  "TARGET_32BIT && TARGET_HARD_FLOAT && !TARGET_VFP_SINGLE"
  "emit_jump_insn (gen_cbranch_cc (operands[0], operands[1], operands[2],
				   operands[3])); DONE;"
)

(define_expand "cbranchdi4"
  [(set (pc) (if_then_else
	      (match_operator 0 "expandable_comparison_operator"
	       [(match_operand:DI 1 "s_register_operand")
	        (match_operand:DI 2 "reg_or_int_operand")])
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
	(compare:CC (match_operand:SI   0 "s_register_operand" "r,r")
		    (match_operator:SI  3 "shift_operator"
		     [(match_operand:SI 1 "s_register_operand" "r,r")
		      (match_operand:SI 2 "shift_amount_operand" "M,r")])))]
  "TARGET_32BIT"
  "cmp\\t%0, %1%S3"
  [(set_attr "conds" "set")
   (set_attr "shift" "1")
   (set_attr "arch" "32,a")
   (set_attr "type" "alus_shift_imm,alus_shift_reg")])

(define_insn "*cmpsi_shiftsi_swp"
  [(set (reg:CC_SWP CC_REGNUM)
	(compare:CC_SWP (match_operator:SI 3 "shift_operator"
			 [(match_operand:SI 1 "s_register_operand" "r,r")
			  (match_operand:SI 2 "shift_amount_operand" "M,r")])
			(match_operand:SI 0 "s_register_operand" "r,r")))]
  "TARGET_32BIT"
  "cmp%?\\t%0, %1%S3"
  [(set_attr "conds" "set")
   (set_attr "shift" "1")
   (set_attr "arch" "32,a")
   (set_attr "type" "alus_shift_imm,alus_shift_reg")])

(define_insn "*arm_cmpsi_negshiftsi_si"
  [(set (reg:CC_Z CC_REGNUM)
	(compare:CC_Z
	 (neg:SI (match_operator:SI 1 "shift_operator"
		    [(match_operand:SI 2 "s_register_operand" "r,r")
		     (match_operand:SI 3 "shift_amount_operand" "M,r")]))
	 (match_operand:SI 0 "s_register_operand" "r,r")))]
  "TARGET_32BIT"
  "cmn%?\\t%0, %2%S1"
  [(set_attr "conds" "set")
   (set_attr "arch" "32,a")
   (set_attr "shift" "2")
   (set_attr "type" "alus_shift_imm,alus_shift_reg")
   (set_attr "predicable" "yes")]
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
  {
    if (arm_ccfsm_state == 1 || arm_ccfsm_state == 2)
    {
      arm_ccfsm_state += 2;
      return "";
    }
    switch (get_attr_length (insn))
      {
	case 2: /* Thumb2 16-bit b{cond}.  */
	case 4: /* Thumb2 32-bit b{cond} or A32 b{cond}.  */
	  return "b%d1\t%l0";
	  break;

	/* Thumb2 b{cond} out of range.  Use 16-bit b{cond} and
	   unconditional branch b.  */
	default: return arm_gen_far_branch (operands, 0, "Lbcond", "b%D1\t");
      }
  }
  [(set_attr "conds" "use")
   (set_attr "type" "branch")
   (set (attr "length")
    (if_then_else (match_test "!TARGET_THUMB2")

      ;;Target is not Thumb2, therefore is A32.  Generate b{cond}.
      (const_int 4)

      ;; Check if target is within 16-bit Thumb2 b{cond} range.
      (if_then_else (and (ge (minus (match_dup 0) (pc)) (const_int -250))
		         (le (minus (match_dup 0) (pc)) (const_int 256)))

	;; Target is Thumb2, within narrow range.
	;; Generate b{cond}.
	(const_int 2)

	;; Check if target is within 32-bit Thumb2 b{cond} range.
	(if_then_else (and (ge (minus (match_dup 0) (pc))(const_int -1048568))
			   (le (minus (match_dup 0) (pc)) (const_int 1048576)))

	  ;; Target is Thumb2, within wide range.
	  ;; Generate b{cond}
	  (const_int 4)
	  ;; Target is Thumb2, out of range.
	  ;; Generate narrow b{cond} and unconditional branch b.
	  (const_int 6)))))]
)

(define_insn "*arm_cond_branch_reversed"
  [(set (pc)
	(if_then_else (match_operator 1 "arm_comparison_operator"
		       [(match_operand 2 "cc_register" "") (const_int 0)])
		      (pc)
		      (label_ref (match_operand 0 "" ""))))]
  "TARGET_32BIT"
  {
    if (arm_ccfsm_state == 1 || arm_ccfsm_state == 2)
    {
      arm_ccfsm_state += 2;
      return "";
    }
    switch (get_attr_length (insn))
      {
	case 2: /* Thumb2 16-bit b{cond}.  */
	case 4: /* Thumb2 32-bit b{cond} or A32 b{cond}.  */
	  return "b%D1\t%l0";
	  break;

	/* Thumb2 b{cond} out of range.  Use 16-bit b{cond} and
	   unconditional branch b.  */
	default: return arm_gen_far_branch (operands, 0, "Lbcond", "b%d1\t");
      }
  }
  [(set_attr "conds" "use")
   (set_attr "type" "branch")
   (set (attr "length")
    (if_then_else (match_test "!TARGET_THUMB2")

      ;;Target is not Thumb2, therefore is A32.  Generate b{cond}.
      (const_int 4)

      ;; Check if target is within 16-bit Thumb2 b{cond} range.
      (if_then_else (and (ge (minus (match_dup 0) (pc)) (const_int -250))
			 (le (minus (match_dup 0) (pc)) (const_int 256)))

	;; Target is Thumb2, within narrow range.
	;; Generate b{cond}.
	(const_int 2)

	;; Check if target is within 32-bit Thumb2 b{cond} range.
	(if_then_else (and (ge (minus (match_dup 0) (pc))(const_int -1048568))
			   (le (minus (match_dup 0) (pc)) (const_int 1048576)))

	  ;; Target is Thumb2, within wide range.
	  ;; Generate b{cond}.
	  (const_int 4)
	  ;; Target is Thumb2, out of range.
	  ;; Generate narrow b{cond} and unconditional branch b.
	  (const_int 6)))))]
)



; scc insns

(define_expand "cstore_cc"
  [(set (match_operand:SI 0 "s_register_operand")
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

(define_insn "*negscc_borrow"
  [(set (match_operand:SI 0 "s_register_operand" "=r")
	(neg:SI (match_operand:SI 1 "arm_borrow_operation" "")))]
  "TARGET_32BIT"
  "sbc\\t%0, %0, %0"
  [(set_attr "conds" "use")
   (set_attr "length" "4")
   (set_attr "type" "adc_reg")]
)

(define_insn_and_split "*mov_negscc"
  [(set (match_operand:SI 0 "s_register_operand" "=r")
	(neg:SI (match_operator:SI 1 "arm_comparison_operator_mode"
		 [(match_operand 2 "cc_register" "") (const_int 0)])))]
  "TARGET_ARM && !arm_borrow_operation (operands[1], SImode)"
  "#"   ; "mov%D1\\t%0, #0\;mvn%d1\\t%0, #0"
  "&& true"
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
  [(set (match_operand:SI 0 "s_register_operand")
	(match_operator:SI 1 "expandable_comparison_operator"
	 [(match_operand:SI 2 "s_register_operand")
	  (match_operand:SI 3 "reg_or_int_operand")]))]
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

(define_expand "cstorehf4"
  [(set (match_operand:SI 0 "s_register_operand")
	(match_operator:SI 1 "expandable_comparison_operator"
	 [(match_operand:HF 2 "s_register_operand")
	  (match_operand:HF 3 "vfp_compare_operand")]))]
  "TARGET_VFP_FP16INST"
  {
    if (!arm_validize_comparison (&operands[1],
				  &operands[2],
				  &operands[3]))
       FAIL;

    emit_insn (gen_cstore_cc (operands[0], operands[1],
			      operands[2], operands[3]));
    DONE;
  }
)

(define_expand "cstoresf4"
  [(set (match_operand:SI 0 "s_register_operand")
	(match_operator:SI 1 "expandable_comparison_operator"
	 [(match_operand:SF 2 "s_register_operand")
	  (match_operand:SF 3 "vfp_compare_operand")]))]
  "TARGET_32BIT && TARGET_HARD_FLOAT"
  "emit_insn (gen_cstore_cc (operands[0], operands[1],
			     operands[2], operands[3])); DONE;"
)

(define_expand "cstoredf4"
  [(set (match_operand:SI 0 "s_register_operand")
	(match_operator:SI 1 "expandable_comparison_operator"
	 [(match_operand:DF 2 "s_register_operand")
	  (match_operand:DF 3 "vfp_compare_operand")]))]
  "TARGET_32BIT && TARGET_HARD_FLOAT && !TARGET_VFP_SINGLE"
  "emit_insn (gen_cstore_cc (operands[0], operands[1],
			     operands[2], operands[3])); DONE;"
)

(define_expand "cstoredi4"
  [(set (match_operand:SI 0 "s_register_operand")
	(match_operator:SI 1 "expandable_comparison_operator"
	 [(match_operand:DI 2 "s_register_operand")
	  (match_operand:DI 3 "reg_or_int_operand")]))]
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
  [(set (match_operand:SI 0 "s_register_operand")
	(if_then_else:SI (match_operand 1 "expandable_comparison_operator")
			 (match_operand:SI 2 "arm_not_operand")
			 (match_operand:SI 3 "arm_not_operand")))]
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

(define_expand "movhfcc"
  [(set (match_operand:HF 0 "s_register_operand")
	(if_then_else:HF (match_operand 1 "arm_cond_move_operator")
			 (match_operand:HF 2 "s_register_operand")
			 (match_operand:HF 3 "s_register_operand")))]
  "TARGET_VFP_FP16INST"
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

(define_expand "movsfcc"
  [(set (match_operand:SF 0 "s_register_operand")
	(if_then_else:SF (match_operand 1 "arm_cond_move_operator")
			 (match_operand:SF 2 "s_register_operand")
			 (match_operand:SF 3 "s_register_operand")))]
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
  [(set (match_operand:DF 0 "s_register_operand")
	(if_then_else:DF (match_operand 1 "arm_cond_move_operator")
			 (match_operand:DF 2 "s_register_operand")
			 (match_operand:DF 3 "s_register_operand")))]
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
  "TARGET_HARD_FLOAT && TARGET_VFP5 <vfp_double_cond>"
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

(define_insn "*cmovhf"
    [(set (match_operand:HF 0 "s_register_operand" "=t")
	(if_then_else:HF (match_operator 1 "arm_vsel_comparison_operator"
			 [(match_operand 2 "cc_register" "") (const_int 0)])
			  (match_operand:HF 3 "s_register_operand" "t")
			  (match_operand:HF 4 "s_register_operand" "t")))]
  "TARGET_VFP_FP16INST"
  "*
  {
    enum arm_cond_code code = maybe_get_arm_condition_code (operands[1]);
    switch (code)
      {
      case ARM_GE:
      case ARM_GT:
      case ARM_EQ:
      case ARM_VS:
	return \"vsel%d1.f16\\t%0, %3, %4\";
      case ARM_LT:
      case ARM_LE:
      case ARM_NE:
      case ARM_VC:
	return \"vsel%D1.f16\\t%0, %4, %3\";
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
  [(parallel [(call (match_operand 0 "memory_operand")
	            (match_operand 1 "general_operand"))
	      (use (match_operand 2 "" ""))
	      (clobber (reg:SI LR_REGNUM))])]
  "TARGET_EITHER"
  "
  {
    rtx callee, pat;
    tree addr = MEM_EXPR (operands[0]);
    
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

    if (TARGET_FDPIC && !SYMBOL_REF_P (XEXP (operands[0], 0)))
	/* Indirect call: set r9 with FDPIC value of callee.  */
	XEXP (operands[0], 0)
	  = arm_load_function_descriptor (XEXP (operands[0], 0));

    if (detect_cmse_nonsecure_call (addr))
      {
	pat = gen_nonsecure_call_internal (operands[0], operands[1],
					   operands[2]);
	emit_call_insn (pat);
      }
    else
      {
	pat = gen_call_internal (operands[0], operands[1], operands[2]);
	arm_emit_call_insn (pat, XEXP (operands[0], 0), false);
      }

    /* Restore FDPIC register (r9) after call.  */
    if (TARGET_FDPIC)
      {
	rtx fdpic_reg = gen_rtx_REG (Pmode, FDPIC_REGNUM);
	rtx initial_fdpic_reg
	    = get_hard_reg_initial_val (Pmode, FDPIC_REGNUM);

	emit_insn (gen_restore_pic_register_after_call (fdpic_reg,
							initial_fdpic_reg));
      }

    DONE;
  }"
)

(define_insn "restore_pic_register_after_call"
  [(set (match_operand:SI 0 "s_register_operand" "+r,r")
        (unspec:SI [(match_dup 0)
                    (match_operand:SI 1 "nonimmediate_operand" "r,m")]
                   UNSPEC_PIC_RESTORE))]
  ""
  "@
  mov\t%0, %1
  ldr\t%0, %1"
)

(define_expand "call_internal"
  [(parallel [(call (match_operand 0 "memory_operand")
	            (match_operand 1 "general_operand"))
	      (use (match_operand 2 "" ""))
	      (clobber (reg:SI LR_REGNUM))])])

(define_expand "nonsecure_call_internal"
  [(parallel [(call (unspec:SI [(match_operand 0 "memory_operand")]
			       UNSPEC_NONSECURE_MEM)
		    (match_operand 1 "general_operand"))
	      (use (match_operand 2 "" ""))
	      (clobber (reg:SI LR_REGNUM))])]
  "use_cmse"
  "
  {
    if (!TARGET_HAVE_FPCXT_CMSE)
      {
	rtx tmp =
	  copy_to_suggested_reg (XEXP (operands[0], 0),
				 gen_rtx_REG (SImode, R4_REGNUM),
				 SImode);

	operands[0] = replace_equiv_address (operands[0], tmp);
      }
  }")

(define_insn "*call_reg_armv5"
  [(call (mem:SI (match_operand:SI 0 "s_register_operand" "r"))
         (match_operand 1 "" ""))
   (use (match_operand 2 "" ""))
   (clobber (reg:SI LR_REGNUM))]
  "TARGET_ARM && arm_arch5t && !SIBLING_CALL_P (insn)"
  "blx%?\\t%0"
  [(set_attr "type" "call")]
)

(define_insn "*call_reg_arm"
  [(call (mem:SI (match_operand:SI 0 "s_register_operand" "r"))
         (match_operand 1 "" ""))
   (use (match_operand 2 "" ""))
   (clobber (reg:SI LR_REGNUM))]
  "TARGET_ARM && !arm_arch5t && !SIBLING_CALL_P (insn)"
  "*
  return output_call (operands);
  "
  ;; length is worst case, normally it is only two
  [(set_attr "length" "12")
   (set_attr "type" "call")]
)


(define_expand "call_value"
  [(parallel [(set (match_operand       0 "" "")
	           (call (match_operand 1 "memory_operand")
		         (match_operand 2 "general_operand")))
	      (use (match_operand 3 "" ""))
	      (clobber (reg:SI LR_REGNUM))])]
  "TARGET_EITHER"
  "
  {
    rtx pat, callee;
    tree addr = MEM_EXPR (operands[1]);
    
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

    if (TARGET_FDPIC && !SYMBOL_REF_P (XEXP (operands[1], 0)))
	/* Indirect call: set r9 with FDPIC value of callee.  */
	XEXP (operands[1], 0)
	  = arm_load_function_descriptor (XEXP (operands[1], 0));

    if (detect_cmse_nonsecure_call (addr))
      {
	pat = gen_nonsecure_call_value_internal (operands[0], operands[1],
						 operands[2], operands[3]);
	emit_call_insn (pat);
      }
    else
      {
	pat = gen_call_value_internal (operands[0], operands[1],
				       operands[2], operands[3]);
	arm_emit_call_insn (pat, XEXP (operands[1], 0), false);
      }

    /* Restore FDPIC register (r9) after call.  */
    if (TARGET_FDPIC)
      {
	rtx fdpic_reg = gen_rtx_REG (Pmode, FDPIC_REGNUM);
	rtx initial_fdpic_reg
	    = get_hard_reg_initial_val (Pmode, FDPIC_REGNUM);

	emit_insn (gen_restore_pic_register_after_call (fdpic_reg,
							initial_fdpic_reg));
      }

    DONE;
  }"
)

(define_expand "call_value_internal"
  [(parallel [(set (match_operand       0 "" "")
	           (call (match_operand 1 "memory_operand")
		         (match_operand 2 "general_operand")))
	      (use (match_operand 3 "" ""))
	      (clobber (reg:SI LR_REGNUM))])])

(define_expand "nonsecure_call_value_internal"
  [(parallel [(set (match_operand       0 "" "")
		   (call (unspec:SI [(match_operand 1 "memory_operand")]
				    UNSPEC_NONSECURE_MEM)
			 (match_operand 2 "general_operand")))
	      (use (match_operand 3 "" ""))
	      (clobber (reg:SI LR_REGNUM))])]
  "use_cmse"
  "
  {
    if (!TARGET_HAVE_FPCXT_CMSE)
      {
	rtx tmp =
	  copy_to_suggested_reg (XEXP (operands[1], 0),
				 gen_rtx_REG (SImode, R4_REGNUM),
				 SImode);

	operands[1] = replace_equiv_address (operands[1], tmp);
      }
  }")

(define_insn "*call_value_reg_armv5"
  [(set (match_operand 0 "" "")
        (call (mem:SI (match_operand:SI 1 "s_register_operand" "r"))
	      (match_operand 2 "" "")))
   (use (match_operand 3 "" ""))
   (clobber (reg:SI LR_REGNUM))]
  "TARGET_ARM && arm_arch5t && !SIBLING_CALL_P (insn)"
  "blx%?\\t%1"
  [(set_attr "type" "call")]
)

(define_insn "*call_value_reg_arm"
  [(set (match_operand 0 "" "")
        (call (mem:SI (match_operand:SI 1 "s_register_operand" "r"))
	      (match_operand 2 "" "")))
   (use (match_operand 3 "" ""))
   (clobber (reg:SI LR_REGNUM))]
  "TARGET_ARM && !arm_arch5t && !SIBLING_CALL_P (insn)"
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
	&& arm_arch5t && arm_change_mode_p (SYMBOL_REF_DECL (op)))
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
	&& arm_arch5t && arm_change_mode_p (SYMBOL_REF_DECL (op)))
      return NEED_PLT_RELOC ? \"blx%?\\t%a1(PLT)\" : \"blx%?\\t(%a1)\";

    return NEED_PLT_RELOC ? \"bl%?\\t%a1(PLT)\" : \"bl%?\\t%a1\";
  }"
  [(set_attr "type" "call")]
)

(define_expand "sibcall_internal"
  [(parallel [(call (match_operand 0 "memory_operand")
		    (match_operand 1 "general_operand"))
	      (return)
	      (use (match_operand 2 "" ""))])])

;; We may also be able to do sibcalls for Thumb, but it's much harder...
(define_expand "sibcall"
  [(parallel [(call (match_operand 0 "memory_operand")
		    (match_operand 1 "general_operand"))
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
		   (call (match_operand 1 "memory_operand")
			 (match_operand 2 "general_operand")))
	      (return)
	      (use (match_operand 3 "" ""))])])

(define_expand "sibcall_value"
  [(parallel [(set (match_operand 0 "" "")
		   (call (match_operand 1 "memory_operand")
			 (match_operand 2 "general_operand")))
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
      if (arm_arch5t || arm_arch4t)
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
      if (arm_arch5t || arm_arch4t)
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
  [(set_attr "type" "load_4")
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
   (set_attr "type" "load_4")]
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
   (set_attr "type" "load_4")]
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
      (compare:CC_NZ (unspec [(const_int 0)] UNSPEC_CHECK_ARCH)
		       (const_int 0)))
   (set (match_operand:SI 0 "s_register_operand")
      (if_then_else:SI (eq (match_dup 1) (const_int 0))
		       (const_int -1)
		       (const_int 67108860)))] ; 0x03fffffc
  "TARGET_ARM"
  "
  operands[1] = gen_rtx_REG (CC_NZmode, CC_REGNUM);
  ")

(define_insn "*check_arch2"
  [(set (match_operand:CC_NZ 0 "cc_register" "")
      (compare:CC_NZ (unspec [(const_int 0)] UNSPEC_CHECK_ARCH)
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
  "TARGET_EITHER && !TARGET_FDPIC"
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
  [(match_operand:BLK 0 "memory_operand")
   (match_operand 1 "" "")]
  "TARGET_EITHER && !TARGET_FDPIC"
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

;; Since we hard code r0 here use the 'o' constraint to prevent
;; provoking undefined behaviour in the hardware with putting out
;; auto-increment operations with potentially r0 as the base register.
(define_insn "probe_stack"
  [(set (match_operand:SI 0 "memory_operand" "=o")
        (unspec:SI [(const_int 0)] UNSPEC_PROBE_STACK))]
  "TARGET_32BIT"
  "str%?\\tr0, %0"
  [(set_attr "type" "store_4")
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

;; Named patterns for stack smashing protection.
(define_expand "stack_protect_combined_set"
  [(parallel
     [(set (match_operand:SI 0 "memory_operand")
	   (unspec:SI [(match_operand:SI 1 "guard_operand")]
		      UNSPEC_SP_SET))
      (clobber (match_scratch:SI 2 ""))
      (clobber (match_scratch:SI 3 ""))])]
  ""
  ""
)

;; Use a separate insn from the above expand to be able to have the mem outside
;; the operand #1 when register allocation comes. This is needed to avoid LRA
;; try to reload the guard since we need to control how PIC access is done in
;; the -fpic/-fPIC case (see COMPUTE_NOW parameter when calling
;; legitimize_pic_address ()).
(define_insn_and_split "*stack_protect_combined_set_insn"
  [(set (match_operand:SI 0 "memory_operand" "=m,m")
	(unspec:SI [(mem:SI (match_operand:SI 1 "guard_addr_operand" "X,X"))]
		   UNSPEC_SP_SET))
   (clobber (match_scratch:SI 2 "=&l,&r"))
   (clobber (match_scratch:SI 3 "=&l,&r"))]
  ""
  "#"
  "reload_completed"
  [(parallel [(set (match_dup 0) (unspec:SI [(mem:SI (match_dup 2))]
					    UNSPEC_SP_SET))
	      (clobber (match_dup 2))])]
  "
{
  if (flag_pic)
    {
      rtx pic_reg;

      if (TARGET_FDPIC)
	  pic_reg = gen_rtx_REG (Pmode, FDPIC_REGNUM);
      else
	  pic_reg = operands[3];

      /* Forces recomputing of GOT base now.  */
      legitimize_pic_address (operands[1], SImode, operands[2], pic_reg,
			      true /*compute_now*/);
    }
  else
    {
      if (address_operand (operands[1], SImode))
	operands[2] = operands[1];
      else
	{
	  rtx mem = XEXP (force_const_mem (SImode, operands[1]), 0);
	  emit_move_insn (operands[2], mem);
	}
    }
}"
  [(set_attr "arch" "t1,32")]
)

;; DO NOT SPLIT THIS INSN.  It's important for security reasons that the
;; canary value does not live beyond the life of this sequence.
(define_insn "*stack_protect_set_insn"
  [(set (match_operand:SI 0 "memory_operand" "=m,m")
	(unspec:SI [(mem:SI (match_operand:SI 1 "register_operand" "+&l,&r"))]
	 UNSPEC_SP_SET))
   (clobber (match_dup 1))]
  ""
  "@
   ldr\\t%1, [%1]\;str\\t%1, %0\;movs\t%1, #0
   ldr\\t%1, [%1]\;str\\t%1, %0\;mov\t%1, #0"
  [(set_attr "length" "8,12")
   (set_attr "conds" "clob,nocond")
   (set_attr "type" "multiple")
   (set_attr "arch" "t1,32")]
)

(define_expand "stack_protect_combined_test"
  [(parallel
     [(set (pc)
	   (if_then_else
		(eq (match_operand:SI 0 "memory_operand")
		    (unspec:SI [(match_operand:SI 1 "guard_operand")]
			       UNSPEC_SP_TEST))
		(label_ref (match_operand 2))
		(pc)))
      (clobber (match_scratch:SI 3 ""))
      (clobber (match_scratch:SI 4 ""))
      (clobber (reg:CC CC_REGNUM))])]
  ""
  ""
)

;; Use a separate insn from the above expand to be able to have the mem outside
;; the operand #1 when register allocation comes. This is needed to avoid LRA
;; try to reload the guard since we need to control how PIC access is done in
;; the -fpic/-fPIC case (see COMPUTE_NOW parameter when calling
;; legitimize_pic_address ()).
(define_insn_and_split "*stack_protect_combined_test_insn"
  [(set (pc)
	(if_then_else
		(eq (match_operand:SI 0 "memory_operand" "m,m")
		    (unspec:SI [(mem:SI (match_operand:SI 1 "guard_addr_operand" "X,X"))]
			       UNSPEC_SP_TEST))
		(label_ref (match_operand 2))
		(pc)))
   (clobber (match_scratch:SI 3 "=&l,&r"))
   (clobber (match_scratch:SI 4 "=&l,&r"))
   (clobber (reg:CC CC_REGNUM))]
  ""
  "#"
  "reload_completed"
  [(const_int 0)]
{
  rtx eq;

  if (flag_pic)
    {
      rtx pic_reg;

      if (TARGET_FDPIC)
	  pic_reg = gen_rtx_REG (Pmode, FDPIC_REGNUM);
      else
	  pic_reg = operands[4];

      /* Forces recomputing of GOT base now.  */
      legitimize_pic_address (operands[1], SImode, operands[3], pic_reg,
			      true /*compute_now*/);
    }
  else
    {
      if (address_operand (operands[1], SImode))
	operands[3] = operands[1];
      else
	{
	  rtx mem = XEXP (force_const_mem (SImode, operands[1]), 0);
	  emit_move_insn (operands[3], mem);
	}
    }
  if (TARGET_32BIT)
    {
      emit_insn (gen_arm_stack_protect_test_insn (operands[4], operands[0],
						  operands[3]));
      rtx cc_reg = gen_rtx_REG (CC_Zmode, CC_REGNUM);
      eq = gen_rtx_EQ (CC_Zmode, cc_reg, const0_rtx);
      emit_jump_insn (gen_arm_cond_branch (operands[2], eq, cc_reg));
    }
  else
    {
      emit_insn (gen_thumb1_stack_protect_test_insn (operands[4], operands[0],
						     operands[3]));
      eq = gen_rtx_EQ (VOIDmode, operands[4], const0_rtx);
      emit_jump_insn (gen_cbranchsi4 (eq, operands[4], const0_rtx,
				      operands[2]));
    }
  DONE;
}
  [(set_attr "arch" "t1,32")]
)

(define_insn "arm_stack_protect_test_insn"
  [(set (reg:CC_Z CC_REGNUM)
	(compare:CC_Z (unspec:SI [(match_operand:SI 1 "memory_operand" "m,m")
				  (mem:SI (match_operand:SI 2 "register_operand" "+l,r"))]
				 UNSPEC_SP_TEST)
		      (const_int 0)))
   (clobber (match_operand:SI 0 "register_operand" "=&l,&r"))
   (clobber (match_dup 2))]
  "TARGET_32BIT"
  "ldr\t%0, [%2]\;ldr\t%2, %1\;eors\t%0, %2, %0"
  [(set_attr "length" "8,12")
   (set_attr "conds" "set")
   (set_attr "type" "multiple")
   (set_attr "arch" "t,32")]
)

(define_expand "casesi"
  [(match_operand:SI 0 "s_register_operand")	; index to jump on
   (match_operand:SI 1 "const_int_operand")	; lower bound
   (match_operand:SI 2 "const_int_operand")	; total range
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
(define_expand "arm_casesi_internal"
  [(parallel [(set (pc)
	       (if_then_else
		(leu (match_operand:SI 0 "s_register_operand")
		     (match_operand:SI 1 "arm_rhs_operand"))
		(match_dup 4)
		(label_ref:SI (match_operand 3 ""))))
	      (clobber (reg:CC CC_REGNUM))
	      (use (label_ref:SI (match_operand 2 "")))])]
  "TARGET_ARM"
{
  operands[4] = gen_rtx_MULT (SImode, operands[0], GEN_INT (4));
  operands[4] = gen_rtx_PLUS (SImode, operands[4],
			      gen_rtx_LABEL_REF (SImode, operands[2]));
  operands[4] = gen_rtx_MEM (SImode, operands[4]);
  MEM_READONLY_P (operands[4]) = 1;
  MEM_NOTRAP_P (operands[4]) = 1;
})

(define_insn "*arm_casesi_internal"
  [(parallel [(set (pc)
	       (if_then_else
		(leu (match_operand:SI 0 "s_register_operand" "r")
		     (match_operand:SI 1 "arm_rhs_operand" "rI"))
		(mem:SI (plus:SI (mult:SI (match_dup 0) (const_int 4))
				 (label_ref:SI (match_operand 2 "" ""))))
		(label_ref:SI (match_operand 3 "" ""))))
	      (clobber (reg:CC CC_REGNUM))
	      (use (label_ref:SI (match_dup 2)))])]
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
	(match_operand:SI 0 "s_register_operand"))]
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
  [(set_attr "type" "load_4")
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
  [(set (reg:CC_NZ CC_REGNUM)
        (compare:CC_NZ
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
  [(set (reg:CC_NZ CC_REGNUM)
        (compare:CC_NZ
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
   (set_attr "predicable_short_it" "no")
   (set_attr "shift" "3")
   (set_attr "arch" "32,a")
   (set_attr "type" "alus_shift_imm,alus_shift_reg")])

(define_insn "*sub_shiftsi_compare0"
  [(set (reg:CC_NZ CC_REGNUM)
	(compare:CC_NZ
	 (minus:SI (match_operand:SI 1 "s_register_operand" "r,r")
		   (match_operator:SI 2 "shift_operator"
		    [(match_operand:SI 3 "s_register_operand" "r,r")
		     (match_operand:SI 4 "shift_amount_operand" "M,r")]))
	 (const_int 0)))
   (set (match_operand:SI 0 "s_register_operand" "=r,r")
	(minus:SI (match_dup 1)
		  (match_op_dup 2 [(match_dup 3) (match_dup 4)])))]
  "TARGET_32BIT"
  "subs%?\\t%0, %1, %3%S2"
  [(set_attr "conds" "set")
   (set_attr "shift" "3")
   (set_attr "arch" "32,a")
   (set_attr "type" "alus_shift_imm,alus_shift_reg")])

(define_insn "*sub_shiftsi_compare0_scratch"
  [(set (reg:CC_NZ CC_REGNUM)
	(compare:CC_NZ
	 (minus:SI (match_operand:SI 1 "s_register_operand" "r,r")
		   (match_operator:SI 2 "shift_operator"
		    [(match_operand:SI 3 "s_register_operand" "r,r")
		     (match_operand:SI 4 "shift_amount_operand" "M,r")]))
	 (const_int 0)))
   (clobber (match_scratch:SI 0 "=r,r"))]
  "TARGET_32BIT"
  "subs%?\\t%0, %1, %3%S2"
  [(set_attr "conds" "set")
   (set_attr "shift" "3")
   (set_attr "arch" "32,a")
   (set_attr "type" "alus_shift_imm,alus_shift_reg")])


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
  "arm_arch5t && TARGET_32BIT"
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
  operands[3] = gen_int_mode (-INTVAL (operands[2]), SImode);
})

(define_split
  [(set (match_operand:SI 0 "s_register_operand" "")
	(ne:SI (match_operand:SI 1 "s_register_operand" "")
	       (match_operand:SI 2 "arm_add_operand" "")))
   (clobber (reg:CC CC_REGNUM))]
  "TARGET_32BIT && reload_completed"
  [(parallel
    [(set (reg:CC_NZ CC_REGNUM)
	  (compare:CC_NZ (minus:SI (match_dup 1) (match_dup 2))
			   (const_int 0)))
     (set (match_dup 0) (minus:SI (match_dup 1) (match_dup 2)))])
   (cond_exec (ne:CC_NZ (reg:CC_NZ CC_REGNUM) (const_int 0))
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
  "arm_arch5t && TARGET_32BIT && peep2_regno_dead_p (3, CC_REGNUM)"
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
  "arm_arch5t && TARGET_32BIT && peep2_regno_dead_p (3, CC_REGNUM)
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
   (set_attr "enabled_for_short_it" "yes,no,no,no,no,no,no,no,no")
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
   (set_attr "enabled_for_short_it" "yes,no,no,no,no,no,no,no,no")
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
	        "l,l,l,r,r,r,r,r,r,r")
	    (match_operand:SI 1 "arm_add_operand"
	        "lPy,lPy,lPy,rI,L,r,rI,L,rI,L")])
	  (match_operator:SI 5 "arm_comparison_operator"
	   [(match_operand:SI 2 "s_register_operand"
	        "l,r,r,l,l,r,r,r,r,r")
	    (match_operand:SI 3 "arm_add_operand"
	        "lPy,rI,L,lPy,lPy,r,rI,rI,L,L")]))
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
    static const int cmp_idx[] = {CMP_CMP, CMP_CMP, CMP_CMN,
                                  CMP_CMP, CMN_CMP, CMP_CMP,
                                  CMP_CMP, CMN_CMP, CMP_CMN,
				  CMN_CMN};
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
   (set_attr "arch" "t2,t2,t2,t2,t2,t2,any,any,any,any")
   (set_attr "enabled_for_short_it" "yes,no,no,no,no,yes,no,no,no,no")
   (set_attr_alternative "length"
      [(const_int 6)
       (const_int 8)
       (const_int 8)
       (const_int 8)
       (const_int 8)
       (const_int 6)
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
	        "l,l,l,r,r,r,r,r,r,r")
	    (match_operand:SI 1 "arm_add_operand"
	        "lPy,lPy,lPy,rI,L,r,rI,L,rI,L")])
	  (match_operator:SI 5 "arm_comparison_operator"
	   [(match_operand:SI 2 "s_register_operand"
	        "l,r,r,l,l,r,r,r,r,r")
	    (match_operand:SI 3 "arm_add_operand"
	        "lPy,rI,L,lPy,lPy,r,rI,rI,L,L")]))
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
    static const int cmp_idx[] = {CMP_CMP, CMP_CMP, CMP_CMN,
                                  CMP_CMP, CMN_CMP, CMP_CMP,
				  CMP_CMP, CMN_CMP, CMP_CMN,
				  CMN_CMN};
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
   (set_attr "arch" "t2,t2,t2,t2,t2,t2,any,any,any,any")
   (set_attr "enabled_for_short_it" "yes,no,no,no,no,yes,no,no,no,no")
   (set_attr_alternative "length"
      [(const_int 6)
       (const_int 8)
       (const_int 8)
       (const_int 8)
       (const_int 8)
       (const_int 6)
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
  [(set (match_operand:SI 0 "s_register_operand" "=Ts,Ts")
	(ior:SI (match_operator:SI 3 "arm_comparison_operator"
		 [(match_operand:SI 1 "s_register_operand" "l,r")
		  (match_operand:SI 2 "arm_add_operand" "lPy,rIL")])
		(match_operator:SI 6 "arm_comparison_operator"
		 [(match_operand:SI 4 "s_register_operand" "l,r")
		  (match_operand:SI 5 "arm_add_operand" "lPy,rIL")])))
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
   (set_attr "enabled_for_short_it" "yes,no")
   (set_attr "length" "16")
   (set_attr "type" "multiple")]
)

; If the above pattern is followed by a CMP insn, then the compare is 
; redundant, since we can rework the conditional instruction that follows.
(define_insn_and_split "*ior_scc_scc_cmp"
  [(set (match_operand 0 "dominant_cc_register" "")
	(compare (ior:SI (match_operator:SI 3 "arm_comparison_operator"
			  [(match_operand:SI 1 "s_register_operand" "l,r")
			   (match_operand:SI 2 "arm_add_operand" "lPy,rIL")])
			 (match_operator:SI 6 "arm_comparison_operator"
			  [(match_operand:SI 4 "s_register_operand" "l,r")
			   (match_operand:SI 5 "arm_add_operand" "lPy,rIL")]))
		 (const_int 0)))
   (set (match_operand:SI 7 "s_register_operand" "=Ts,Ts")
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
   (set_attr "enabled_for_short_it" "yes,no")
   (set_attr "length" "16")
   (set_attr "type" "multiple")]
)

(define_insn_and_split "*and_scc_scc"
  [(set (match_operand:SI 0 "s_register_operand" "=Ts,Ts")
	(and:SI (match_operator:SI 3 "arm_comparison_operator"
		 [(match_operand:SI 1 "s_register_operand" "l,r")
		  (match_operand:SI 2 "arm_add_operand" "lPy,rIL")])
		(match_operator:SI 6 "arm_comparison_operator"
		 [(match_operand:SI 4 "s_register_operand" "l,r")
		  (match_operand:SI 5 "arm_add_operand" "lPy,rIL")])))
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
   (set_attr "enabled_for_short_it" "yes,no")
   (set_attr "length" "16")
   (set_attr "type" "multiple")]
)

; If the above pattern is followed by a CMP insn, then the compare is 
; redundant, since we can rework the conditional instruction that follows.
(define_insn_and_split "*and_scc_scc_cmp"
  [(set (match_operand 0 "dominant_cc_register" "")
	(compare (and:SI (match_operator:SI 3 "arm_comparison_operator"
			  [(match_operand:SI 1 "s_register_operand" "l,r")
			   (match_operand:SI 2 "arm_add_operand" "lPy,rIL")])
			 (match_operator:SI 6 "arm_comparison_operator"
			  [(match_operand:SI 4 "s_register_operand" "l,r")
			   (match_operand:SI 5 "arm_add_operand" "lPy,rIL")]))
		 (const_int 0)))
   (set (match_operand:SI 7 "s_register_operand" "=Ts,Ts")
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
   (set_attr "enabled_for_short_it" "yes,no")
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
  [(set (reg:CC_NZ CC_REGNUM)
	(compare:CC_NZ (ior:SI
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
   (set (reg:CC_NZ CC_REGNUM)
	(compare:CC_NZ (and:SI (match_dup 4) (const_int 1))
			 (const_int 0)))]
  "")

(define_split
  [(set (reg:CC_NZ CC_REGNUM)
	(compare:CC_NZ (ior:SI
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
   (set (reg:CC_NZ CC_REGNUM)
	(compare:CC_NZ (and:SI (match_dup 4) (const_int 1))
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
                                        gen_int_mode (-INTVAL (operands[2]),
						      SImode)));
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
  [(set (reg:CC_NZ CC_REGNUM)
	(compare:CC_NZ
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
   (set_attr "enabled_for_short_it" "no,yes,yes")
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
   (set_attr "enabled_for_short_it" "yes,no")
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
   (set_attr "enabled_for_short_it" "yes,no")
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
   (set_attr "type" "load_4")]
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
  [(use (match_operand:SI 0 "register_operand"))
   (use (match_operand:SI 1 "register_operand"))
   (use (match_operand:SI 2 "register_operand"))]
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
  [(set_attr "type" "store_16")
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
  [(set_attr "type" "load_16")
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
  [(set_attr "type" "load_16")
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
  [(set_attr "type" "load_16")
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
  [(set_attr "type" "load_4")
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
  "TARGET_32BIT && TARGET_VFP_BASE"
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
  [(set_attr "type" "load_16")
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
    scalar_float_mode float_mode;
    if (is_a <scalar_float_mode> (GET_MODE (x), &float_mode))
      assemble_real (*CONST_DOUBLE_REAL_VALUE (x), float_mode, BITS_PER_WORD);
    else
      {
	/* XXX: Sometimes gcc does something really dumb and ends up with
	   a HIGH in a constant pool entry, usually because it's trying to
	   load into a VFP register.  We know this will always be used in
	   combination with a LO_SUM which ignores the high bits, so just
	   strip off the HIGH.  */
	if (GET_CODE (x) == HIGH)
	  x = XEXP (x, 0);
        assemble_integer (x, 4, BITS_PER_WORD, 1);
	mark_symbol_refs_as_used (x);
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
    scalar_float_mode float_mode;
    if (is_a <scalar_float_mode> (GET_MODE (operands[0]), &float_mode))
      assemble_real (*CONST_DOUBLE_REAL_VALUE (operands[0]),
		     float_mode, BITS_PER_WORD);
    else
      assemble_integer (operands[0], 8, BITS_PER_WORD, 1);
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
    scalar_float_mode float_mode;
    if (is_a <scalar_float_mode> (GET_MODE (operands[0]), &float_mode))
      assemble_real (*CONST_DOUBLE_REAL_VALUE (operands[0]),
		     float_mode, BITS_PER_WORD);
    else
      assemble_integer (operands[0], 16, BITS_PER_WORD, 1);
    return \"\";
  }"
  [(set_attr "length" "16")
   (set_attr "type" "no_insn")]
)

;; V5 Instructions,

(define_insn "clzsi2"
  [(set (match_operand:SI 0 "s_register_operand" "=r")
	(clz:SI (match_operand:SI 1 "s_register_operand" "r")))]
  "TARGET_32BIT && arm_arch5t"
  "clz%?\\t%0, %1"
  [(set_attr "predicable" "yes")
   (set_attr "type" "clz")])

(define_insn "rbitsi2"
  [(set (match_operand:SI 0 "s_register_operand" "=r")
	(unspec:SI [(match_operand:SI 1 "s_register_operand" "r")] UNSPEC_RBIT))]
  "TARGET_32BIT && arm_arch_thumb2"
  "rbit%?\\t%0, %1"
  [(set_attr "predicable" "yes")
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
  "TARGET_32BIT && arm_arch5te"
  "pld\\t%a0"
  [(set_attr "type" "load_4")]
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
  [(use (match_operand 0 "general_operand"))]
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
(define_insn "load_tp_soft_fdpic"
  [(set (reg:SI 0) (unspec:SI [(const_int 0)] UNSPEC_TLS))
   (clobber (reg:SI FDPIC_REGNUM))
   (clobber (reg:SI LR_REGNUM))
   (clobber (reg:SI IP_REGNUM))
   (clobber (reg:CC CC_REGNUM))]
  "TARGET_SOFT_TP && TARGET_FDPIC"
  "bl\\t__aeabi_read_tp\\t@ load_tp_soft"
  [(set_attr "conds" "clob")
   (set_attr "type" "branch")]
)

;; Doesn't clobber R1-R3.  Must use r0 for the first operand.
(define_insn "load_tp_soft"
  [(set (reg:SI 0) (unspec:SI [(const_int 0)] UNSPEC_TLS))
   (clobber (reg:SI LR_REGNUM))
   (clobber (reg:SI IP_REGNUM))
   (clobber (reg:CC CC_REGNUM))]
  "TARGET_SOFT_TP && !TARGET_FDPIC"
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
  [(match_operand:SI 0 "s_register_operand")]
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
   (set_attr "type" "rev")]
)

(define_expand "arm_legacy_rev"
  [(set (match_operand:SI 2 "s_register_operand")
	(xor:SI (rotatert:SI (match_operand:SI 1 "s_register_operand")
			     (const_int 16))
		(match_dup 1)))
   (set (match_dup 2)
	(lshiftrt:SI (match_dup 2)
		     (const_int 8)))
   (set (match_operand:SI 3 "s_register_operand")
	(rotatert:SI (match_dup 1)
		     (const_int 8)))
   (set (match_dup 2)
	(and:SI (match_dup 2)
		(const_int -65281)))
   (set (match_operand:SI 0 "s_register_operand")
	(xor:SI (match_dup 3)
		(match_dup 2)))]
  "TARGET_32BIT"
  ""
)

;; Reuse temporaries to keep register pressure down.
(define_expand "thumb_legacy_rev"
  [(set (match_operand:SI 2 "s_register_operand")
     (ashift:SI (match_operand:SI 1 "s_register_operand")
                (const_int 24)))
   (set (match_operand:SI 3 "s_register_operand")
     (lshiftrt:SI (match_dup 1)
		  (const_int 24)))
   (set (match_dup 3)
     (ior:SI (match_dup 3)
	     (match_dup 2)))
   (set (match_operand:SI 4 "s_register_operand")
     (const_int 16))
   (set (match_operand:SI 5 "s_register_operand")
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
   (set (match_operand:SI 0 "s_register_operand")
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
  [(match_operand:SI 0 "register_operand")
   (match_operand:SI 1 "register_operand")
   (match_operand:SI 2 "const_int_operand")]
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
  [(set (match_operand:SI 0 "s_register_operand")
	(bswap:SI (match_operand:SI 1 "s_register_operand")))]
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
  [(set (match_operand:HI 0 "s_register_operand")
	(bswap:HI (match_operand:HI 1 "s_register_operand")))]
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
     && ((INTVAL (operands[2]) + 4) == INTVAL (operands[4]))
     && (operands_ok_ldrd_strd (operands[0], operands[3],
                                  operands[1], INTVAL (operands[2]),
                                  false, true))"
  "ldrd%?\t%0, %3, [%1, %2]"
  [(set_attr "type" "load_8")
   (set_attr "predicable" "yes")])

(define_insn "*thumb2_ldrd_base"
  [(set (match_operand:SI 0 "s_register_operand" "=r")
        (mem:SI (match_operand:SI 1 "s_register_operand" "rk")))
   (set (match_operand:SI 2 "s_register_operand" "=r")
        (mem:SI (plus:SI (match_dup 1)
                         (const_int 4))))]
  "TARGET_LDRD && TARGET_THUMB2 && reload_completed
     && (operands_ok_ldrd_strd (operands[0], operands[2],
                                  operands[1], 0, false, true))"
  "ldrd%?\t%0, %2, [%1]"
  [(set_attr "type" "load_8")
   (set_attr "predicable" "yes")])

(define_insn "*thumb2_ldrd_base_neg"
  [(set (match_operand:SI 0 "s_register_operand" "=r")
	(mem:SI (plus:SI (match_operand:SI 1 "s_register_operand" "rk")
                         (const_int -4))))
   (set (match_operand:SI 2 "s_register_operand" "=r")
        (mem:SI (match_dup 1)))]
  "TARGET_LDRD && TARGET_THUMB2 && reload_completed
     && (operands_ok_ldrd_strd (operands[0], operands[2],
                                  operands[1], -4, false, true))"
  "ldrd%?\t%0, %2, [%1, #-4]"
  [(set_attr "type" "load_8")
   (set_attr "predicable" "yes")])

(define_insn "*thumb2_strd"
  [(set (mem:SI (plus:SI (match_operand:SI 0 "s_register_operand" "rk")
                         (match_operand:SI 1 "ldrd_strd_offset_operand" "Do")))
        (match_operand:SI 2 "s_register_operand" "r"))
   (set (mem:SI (plus:SI (match_dup 0)
                         (match_operand:SI 3 "const_int_operand" "")))
        (match_operand:SI 4 "s_register_operand" "r"))]
  "TARGET_LDRD && TARGET_THUMB2 && reload_completed
     && ((INTVAL (operands[1]) + 4) == INTVAL (operands[3]))
     && (operands_ok_ldrd_strd (operands[2], operands[4],
                                  operands[0], INTVAL (operands[1]),
                                  false, false))"
  "strd%?\t%2, %4, [%0, %1]"
  [(set_attr "type" "store_8")
   (set_attr "predicable" "yes")])

(define_insn "*thumb2_strd_base"
  [(set (mem:SI (match_operand:SI 0 "s_register_operand" "rk"))
        (match_operand:SI 1 "s_register_operand" "r"))
   (set (mem:SI (plus:SI (match_dup 0)
                         (const_int 4)))
        (match_operand:SI 2 "s_register_operand" "r"))]
  "TARGET_LDRD && TARGET_THUMB2 && reload_completed
     && (operands_ok_ldrd_strd (operands[1], operands[2],
                                  operands[0], 0, false, false))"
  "strd%?\t%1, %2, [%0]"
  [(set_attr "type" "store_8")
   (set_attr "predicable" "yes")])

(define_insn "*thumb2_strd_base_neg"
  [(set (mem:SI (plus:SI (match_operand:SI 0 "s_register_operand" "rk")
                         (const_int -4)))
        (match_operand:SI 1 "s_register_operand" "r"))
   (set (mem:SI (match_dup 0))
        (match_operand:SI 2 "s_register_operand" "r"))]
  "TARGET_LDRD && TARGET_THUMB2 && reload_completed
     && (operands_ok_ldrd_strd (operands[1], operands[2],
                                  operands[0], -4, false, false))"
  "strd%?\t%1, %2, [%0, #-4]"
  [(set_attr "type" "store_8")
   (set_attr "predicable" "yes")])

;; ARMv8 CRC32 instructions.
(define_insn "arm_<crc_variant>"
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
;; The operands are validated through the load_multiple_operation
;; match_parallel predicate rather than through constraints so enable it only
;; after reload.
(define_insn "*load_multiple"
  [(match_parallel 0 "load_multiple_operation"
    [(set (match_operand:SI 2 "s_register_operand" "=rk")
          (mem:SI (match_operand:SI 1 "s_register_operand" "rk")))
        ])]
  "TARGET_32BIT && reload_completed"
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

(define_insn "arm_<cdp>"
  [(unspec_volatile [(match_operand:SI 0 "immediate_operand" "n")
		     (match_operand:SI 1 "immediate_operand" "n")
		     (match_operand:SI 2 "immediate_operand" "n")
		     (match_operand:SI 3 "immediate_operand" "n")
		     (match_operand:SI 4 "immediate_operand" "n")
		     (match_operand:SI 5 "immediate_operand" "n")] CDPI)]
  "arm_coproc_builtin_available (VUNSPEC_<CDP>)"
{
  arm_const_bounds (operands[0], 0, 16);
  arm_const_bounds (operands[1], 0, 16);
  arm_const_bounds (operands[2], 0, (1 << 5));
  arm_const_bounds (operands[3], 0, (1 << 5));
  arm_const_bounds (operands[4], 0, (1 << 5));
  arm_const_bounds (operands[5], 0, 8);
  return "<cdp>\\tp%c0, %1, CR%c2, CR%c3, CR%c4, %5";
}
  [(set_attr "length" "4")
   (set_attr "type" "coproc")])

(define_insn "*ldc"
  [(unspec_volatile [(match_operand:SI 0 "immediate_operand" "n")
		     (match_operand:SI 1 "immediate_operand" "n")
		     (match_operand:SI 2 "memory_operand" "Uz")] LDCI)]
  "arm_coproc_builtin_available (VUNSPEC_<LDC>)"
{
  arm_const_bounds (operands[0], 0, 16);
  arm_const_bounds (operands[1], 0, (1 << 5));
  return "<ldc>\\tp%c0, CR%c1, %2";
}
  [(set_attr "length" "4")
   (set_attr "type" "coproc")])

(define_insn "*stc"
  [(unspec_volatile [(match_operand:SI 0 "immediate_operand" "n")
		     (match_operand:SI 1 "immediate_operand" "n")
		     (match_operand:SI 2 "memory_operand" "=Uz")] STCI)]
  "arm_coproc_builtin_available (VUNSPEC_<STC>)"
{
  arm_const_bounds (operands[0], 0, 16);
  arm_const_bounds (operands[1], 0, (1 << 5));
  return "<stc>\\tp%c0, CR%c1, %2";
}
  [(set_attr "length" "4")
   (set_attr "type" "coproc")])

(define_expand "arm_<ldc>"
  [(unspec_volatile [(match_operand:SI 0 "immediate_operand")
		     (match_operand:SI 1 "immediate_operand")
		     (mem:SI (match_operand:SI 2 "s_register_operand"))] LDCI)]
  "arm_coproc_builtin_available (VUNSPEC_<LDC>)")

(define_expand "arm_<stc>"
  [(unspec_volatile [(match_operand:SI 0 "immediate_operand")
		     (match_operand:SI 1 "immediate_operand")
		     (mem:SI (match_operand:SI 2 "s_register_operand"))] STCI)]
  "arm_coproc_builtin_available (VUNSPEC_<STC>)")

(define_insn "arm_<mcr>"
  [(unspec_volatile [(match_operand:SI 0 "immediate_operand" "n")
		     (match_operand:SI 1 "immediate_operand" "n")
		     (match_operand:SI 2 "s_register_operand" "r")
		     (match_operand:SI 3 "immediate_operand" "n")
		     (match_operand:SI 4 "immediate_operand" "n")
		     (match_operand:SI 5 "immediate_operand" "n")] MCRI)
   (use (match_dup 2))]
  "arm_coproc_builtin_available (VUNSPEC_<MCR>)"
{
  arm_const_bounds (operands[0], 0, 16);
  arm_const_bounds (operands[1], 0, 8);
  arm_const_bounds (operands[3], 0, (1 << 5));
  arm_const_bounds (operands[4], 0, (1 << 5));
  arm_const_bounds (operands[5], 0, 8);
  return "<mcr>\\tp%c0, %1, %2, CR%c3, CR%c4, %5";
}
  [(set_attr "length" "4")
   (set_attr "type" "coproc")])

(define_insn "arm_<mrc>"
  [(set (match_operand:SI 0 "s_register_operand" "=r")
	(unspec_volatile:SI [(match_operand:SI 1 "immediate_operand" "n")
			  (match_operand:SI 2 "immediate_operand" "n")
			  (match_operand:SI 3 "immediate_operand" "n")
			  (match_operand:SI 4 "immediate_operand" "n")
			  (match_operand:SI 5 "immediate_operand" "n")] MRCI))]
  "arm_coproc_builtin_available (VUNSPEC_<MRC>)"
{
  arm_const_bounds (operands[1], 0, 16);
  arm_const_bounds (operands[2], 0, 8);
  arm_const_bounds (operands[3], 0, (1 << 5));
  arm_const_bounds (operands[4], 0, (1 << 5));
  arm_const_bounds (operands[5], 0, 8);
  return "<mrc>\\tp%c1, %2, %0, CR%c3, CR%c4, %5";
}
  [(set_attr "length" "4")
   (set_attr "type" "coproc")])

(define_insn "arm_<mcrr>"
  [(unspec_volatile [(match_operand:SI 0 "immediate_operand" "n")
		     (match_operand:SI 1 "immediate_operand" "n")
		     (match_operand:DI 2 "s_register_operand" "r")
		     (match_operand:SI 3 "immediate_operand" "n")] MCRRI)
   (use (match_dup 2))]
  "arm_coproc_builtin_available (VUNSPEC_<MCRR>)"
{
  arm_const_bounds (operands[0], 0, 16);
  arm_const_bounds (operands[1], 0, 8);
  arm_const_bounds (operands[3], 0, (1 << 5));
  return "<mcrr>\\tp%c0, %1, %Q2, %R2, CR%c3";
}
  [(set_attr "length" "4")
   (set_attr "type" "coproc")])

(define_insn "arm_<mrrc>"
  [(set (match_operand:DI 0 "s_register_operand" "=r")
	(unspec_volatile:DI [(match_operand:SI 1 "immediate_operand" "n")
			  (match_operand:SI 2 "immediate_operand" "n")
			  (match_operand:SI 3 "immediate_operand" "n")] MRRCI))]
  "arm_coproc_builtin_available (VUNSPEC_<MRRC>)"
{
  arm_const_bounds (operands[1], 0, 16);
  arm_const_bounds (operands[2], 0, 8);
  arm_const_bounds (operands[3], 0, (1 << 5));
  return "<mrrc>\\tp%c1, %2, %Q0, %R0, CR%c3";
}
  [(set_attr "length" "4")
   (set_attr "type" "coproc")])

(define_expand "speculation_barrier"
  [(unspec_volatile [(const_int 0)] VUNSPEC_SPECULATION_BARRIER)]
  "TARGET_EITHER"
  "
  /* For thumb1 (except Armv8 derivatives), and for pre-Armv7 we don't
     have a usable barrier (and probably don't need one in practice).
     But to be safe if such code is run on later architectures, call a
     helper function in libgcc that will do the thing for the active
     system.  */
  if (!(arm_arch7 || arm_arch8))
    {
      arm_emit_speculation_barrier_function ();
      DONE;
    }
  "
)

;; Generate a hard speculation barrier when we have not enabled speculation
;; tracking.
(define_insn "*speculation_barrier_insn"
  [(unspec_volatile [(const_int 0)] VUNSPEC_SPECULATION_BARRIER)]
  "arm_arch7 || arm_arch8"
  "isb\;dsb\\tsy"
  [(set_attr "type" "block")
   (set_attr "length" "8")]
)

;; Vector bits common to IWMMXT, Neon and MVE
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
;; M-profile Vector Extension
(include "mve.md")
