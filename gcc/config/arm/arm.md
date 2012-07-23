;;- Machine description for ARM for GNU compiler
;;  Copyright 1991, 1993, 1994, 1995, 1996, 1996, 1997, 1998, 1999, 2000,
;;  2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010, 2011, 2012
;;  Free Software Foundation, Inc.
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

;; UNSPEC Usage:
;; Note: sin and cos are no-longer used.
;; Unspec enumerators for Neon are defined in neon.md.
;; Unspec enumerators for iwmmxt2 are defined in iwmmxt2.md

(define_c_enum "unspec" [
  UNSPEC_PUSH_MULT      ; `push multiple' operation:
                        ;   operand 0 is the first register,
                        ;   subsequent registers are in parallel (use ...)
                        ;   expressions.
  UNSPEC_PIC_SYM        ; A symbol that has been treated properly for pic
                        ; usage, that is, we will add the pic_register
                        ; value to it before trying to dereference it.
  UNSPEC_PIC_BASE       ; Add PC and all but the last operand together,
                        ; The last operand is the number of a PIC_LABEL
                        ; that points at the containing instruction.
  UNSPEC_PRLG_STK       ; A special barrier that prevents frame accesses
                        ; being scheduled before the stack adjustment insn.
  UNSPEC_PROLOGUE_USE   ; As USE insns are not meaningful after reload,
                        ; this unspec is used to prevent the deletion of
                        ; instructions setting registers for EH handling
                        ; and stack frame generation.  Operand 0 is the
                        ; register to "use".
  UNSPEC_CHECK_ARCH     ; Set CCs to indicate 26-bit or 32-bit mode.
  UNSPEC_WSHUFH         ; Used by the intrinsic form of the iWMMXt WSHUFH instruction.
  UNSPEC_WACC           ; Used by the intrinsic form of the iWMMXt WACC instruction.
  UNSPEC_TMOVMSK        ; Used by the intrinsic form of the iWMMXt TMOVMSK instruction.
  UNSPEC_WSAD           ; Used by the intrinsic form of the iWMMXt WSAD instruction.
  UNSPEC_WSADZ          ; Used by the intrinsic form of the iWMMXt WSADZ instruction.
  UNSPEC_WMACS          ; Used by the intrinsic form of the iWMMXt WMACS instruction.
  UNSPEC_WMACU          ; Used by the intrinsic form of the iWMMXt WMACU instruction.
  UNSPEC_WMACSZ         ; Used by the intrinsic form of the iWMMXt WMACSZ instruction.
  UNSPEC_WMACUZ         ; Used by the intrinsic form of the iWMMXt WMACUZ instruction.
  UNSPEC_CLRDI          ; Used by the intrinsic form of the iWMMXt CLRDI instruction.
  UNSPEC_WALIGNI        ; Used by the intrinsic form of the iWMMXt WALIGN instruction.
  UNSPEC_TLS            ; A symbol that has been treated properly for TLS usage.
  UNSPEC_PIC_LABEL      ; A label used for PIC access that does not appear in the
                        ; instruction stream.
  UNSPEC_PIC_OFFSET     ; A symbolic 12-bit OFFSET that has been treated
                        ; correctly for PIC usage.
  UNSPEC_GOTSYM_OFF     ; The offset of the start of the GOT from a
                        ; a given symbolic address.
  UNSPEC_THUMB1_CASESI  ; A Thumb1 compressed dispatch-table call.
  UNSPEC_RBIT           ; rbit operation.
  UNSPEC_SYMBOL_OFFSET  ; The offset of the start of the symbol from
                        ; another symbolic address.
  UNSPEC_MEMORY_BARRIER ; Represent a memory barrier.
  UNSPEC_UNALIGNED_LOAD	; Used to represent ldr/ldrh instructions that access
			; unaligned locations, on architectures which support
			; that.
  UNSPEC_UNALIGNED_STORE ; Same for str/strh.
  UNSPEC_PIC_UNIFIED    ; Create a common pic addressing form.
  UNSPEC_LL		; Represent an unpaired load-register-exclusive.
])

;; UNSPEC_VOLATILE Usage:

(define_c_enum "unspecv" [
  VUNSPEC_BLOCKAGE      ; `blockage' insn to prevent scheduling across an
                        ;   insn in the code.
  VUNSPEC_EPILOGUE      ; `epilogue' insn, used to represent any part of the
                        ;   instruction epilogue sequence that isn't expanded
                        ;   into normal RTL.  Used for both normal and sibcall
                        ;   epilogues.
  VUNSPEC_THUMB1_INTERWORK ; `prologue_thumb1_interwork' insn, used to swap
			;   modes from arm to thumb.
  VUNSPEC_ALIGN         ; `align' insn.  Used at the head of a minipool table
                        ;   for inlined constants.
  VUNSPEC_POOL_END      ; `end-of-table'.  Used to mark the end of a minipool
                        ;   table.
  VUNSPEC_POOL_1        ; `pool-entry(1)'.  An entry in the constant pool for
                        ;   an 8-bit object.
  VUNSPEC_POOL_2        ; `pool-entry(2)'.  An entry in the constant pool for
                        ;   a 16-bit object.
  VUNSPEC_POOL_4        ; `pool-entry(4)'.  An entry in the constant pool for
                        ;   a 32-bit object.
  VUNSPEC_POOL_8        ; `pool-entry(8)'.  An entry in the constant pool for
                        ;   a 64-bit object.
  VUNSPEC_POOL_16       ; `pool-entry(16)'.  An entry in the constant pool for
                        ;   a 128-bit object.
  VUNSPEC_TMRC          ; Used by the iWMMXt TMRC instruction.
  VUNSPEC_TMCR          ; Used by the iWMMXt TMCR instruction.
  VUNSPEC_ALIGN8        ; 8-byte alignment version of VUNSPEC_ALIGN
  VUNSPEC_WCMP_EQ       ; Used by the iWMMXt WCMPEQ instructions
  VUNSPEC_WCMP_GTU      ; Used by the iWMMXt WCMPGTU instructions
  VUNSPEC_WCMP_GT       ; Used by the iwMMXT WCMPGT instructions
  VUNSPEC_EH_RETURN     ; Use to override the return address for exception
                        ; handling.
  VUNSPEC_ATOMIC_CAS	; Represent an atomic compare swap.
  VUNSPEC_ATOMIC_XCHG	; Represent an atomic exchange.
  VUNSPEC_ATOMIC_OP	; Represent an atomic operation.
  VUNSPEC_LL		; Represent a load-register-exclusive.
  VUNSPEC_SC		; Represent a store-register-exclusive.
])

;;---------------------------------------------------------------------------
;; Attributes

;; Processor type.  This is created automatically from arm-cores.def.
(include "arm-tune.md")

; IS_THUMB is set to 'yes' when we are generating Thumb code, and 'no' when
; generating ARM code.  This is used to control the length of some insn
; patterns that share the same RTL in both ARM and Thumb code.
(define_attr "is_thumb" "no,yes" (const (symbol_ref "thumb_code")))

; IS_ARCH6 is set to 'yes' when we are generating code form ARMv6.
(define_attr "is_arch6" "no,yes" (const (symbol_ref "arm_arch6")))

; IS_THUMB1 is set to 'yes' iff we are generating Thumb-1 code.
(define_attr "is_thumb1" "no,yes" (const (symbol_ref "thumb1_code")))

;; Operand number of an input operand that is shifted.  Zero if the
;; given instruction does not shift one of its input operands.
(define_attr "shift" "" (const_int 0))

; Floating Point Unit.  If we only have floating point emulation, then there
; is no point in scheduling the floating point insns.  (Well, for best
; performance we should try and group them together).
(define_attr "fpu" "none,vfp"
  (const (symbol_ref "arm_fpu_attr")))

; LENGTH of an instruction (in bytes)
(define_attr "length" ""
  (const_int 4))

; The architecture which supports the instruction (or alternative).
; This can be "a" for ARM, "t" for either of the Thumbs, "32" for
; TARGET_32BIT, "t1" or "t2" to specify a specific Thumb mode.  "v6"
; for ARM or Thumb-2 with arm_arch6, and nov6 for ARM without
; arm_arch6.  This attribute is used to compute attribute "enabled",
; use type "any" to enable an alternative in all cases.
(define_attr "arch" "any,a,t,32,t1,t2,v6,nov6,onlya8,neon_onlya8,nota8,neon_nota8,iwmmxt,iwmmxt2"
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

	 (and (eq_attr "arch" "onlya8")
	      (eq_attr "tune" "cortexa8"))
	 (const_string "yes")

	 (and (eq_attr "arch" "neon_onlya8")
	      (eq_attr "tune" "cortexa8")
	      (match_test "TARGET_NEON"))
	 (const_string "yes")

	 (and (eq_attr "arch" "nota8")
	      (not (eq_attr "tune" "cortexa8")))
	 (const_string "yes")

	 (and (eq_attr "arch" "neon_nota8")
	      (not (eq_attr "tune" "cortexa8"))
	      (match_test "TARGET_NEON"))
	 (const_string "yes")

	 (and (eq_attr "arch" "iwmmxt2")
	      (match_test "TARGET_REALLY_IWMMXT2"))
	 (const_string "yes")]

	(const_string "no")))

; Allows an insn to disable certain alternatives for reasons other than
; arch support.
(define_attr "insn_enabled" "no,yes"
  (const_string "yes"))

; Enable all alternatives that are both arch_enabled and insn_enabled.
 (define_attr "enabled" "no,yes"
   (if_then_else (eq_attr "insn_enabled" "yes")
               (if_then_else (eq_attr "arch_enabled" "yes")
                             (const_string "yes")
                             (const_string "no"))
                (const_string "no")))

; POOL_RANGE is how far away from a constant pool entry that this insn
; can be placed.  If the distance is zero, then this insn will never
; reference the pool.
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

;; The instruction used to implement a particular pattern.  This
;; information is used by pipeline descriptions to provide accurate
;; scheduling information.

(define_attr "insn"
        "mov,mvn,smulxy,smlaxy,smlalxy,smulwy,smlawx,mul,muls,mla,mlas,umull,umulls,umlal,umlals,smull,smulls,smlal,smlals,smlawy,smuad,smuadx,smlad,smladx,smusd,smusdx,smlsd,smlsdx,smmul,smmulr,smmla,umaal,smlald,smlsld,clz,mrs,msr,xtab,sdiv,udiv,sat,other"
        (const_string "other"))

; TYPE attribute is used to detect floating point instructions which, if
; running on a co-processor can run in parallel with other, basic instructions
; If write-buffer scheduling is enabled then it can also be used in the
; scheduling of writes.

; Classification of each insn
; Note: vfp.md has different meanings for some of these, and some further
; types as well.  See that file for details.
; alu		any alu  instruction that doesn't hit memory or fp
;		regs or have a shifted source operand
; alu_shift	any data instruction that doesn't hit memory or fp
;		regs, but has a source operand shifted by a constant
; alu_shift_reg	any data instruction that doesn't hit memory or fp
;		regs, but has a source operand shifted by a register value
; mult		a multiply instruction
; block		blockage insn, this blocks all functional units
; float		a floating point arithmetic operation (subject to expansion)
; fdivd		DFmode floating point division
; fdivs		SFmode floating point division
; f_load[sd]	A single/double load from memory. Used for VFP unit.
; f_store[sd]	A single/double store to memory. Used for VFP unit.
; f_flag	a transfer of co-processor flags to the CPSR
; f_2_r		transfer float to core (no memory needed)
; r_2_f		transfer core to float
; f_cvt		convert floating<->integral
; branch	a branch
; call		a subroutine call
; load_byte	load byte(s) from memory to arm registers
; load1		load 1 word from memory to arm registers
; load2         load 2 words from memory to arm registers
; load3         load 3 words from memory to arm registers
; load4         load 4 words from memory to arm registers
; store		store 1 word to memory from arm registers
; store2	store 2 words
; store3	store 3 words
; store4	store 4 (or more) words
;

(define_attr "type"
 "alu,\
  alu_shift,\
  alu_shift_reg,\
  mult,\
  block,\
  float,\
  fdivd,\
  fdivs,\
  fmuls,\
  fmuld,\
  fmacs,\
  fmacd,\
  f_flag,\
  f_loads,\
  f_loadd,\
  f_stores,\
  f_stored,\
  f_2_r,\
  r_2_f,\
  f_cvt,\
  branch,\
  call,\
  load_byte,\
  load1,\
  load2,\
  load3,\
  load4,\
  store1,\
  store2,\
  store3,\
  store4,\
  fconsts,\
  fconstd,\
  fadds,\
  faddd,\
  ffariths,\
  ffarithd,\
  fcmps,\
  fcmpd,\
  fcpys"
 (if_then_else 
    (eq_attr "insn" "smulxy,smlaxy,smlalxy,smulwy,smlawx,mul,muls,mla,mlas,\
	     	     umull,umulls,umlal,umlals,smull,smulls,smlal,smlals")
    (const_string "mult")
    (const_string "alu")))

; Is this an (integer side) multiply with a 64-bit result?
(define_attr "mul64" "no,yes"
  (if_then_else
    (eq_attr "insn"
     "smlalxy,umull,umulls,umlal,umlals,smull,smulls,smlal,smlals")
    (const_string "yes")
    (const_string "no")))

; wtype for WMMX insn scheduling purposes.
(define_attr "wtype"
        "none,wor,wxor,wand,wandn,wmov,tmcrr,tmrrc,wldr,wstr,tmcr,tmrc,wadd,wsub,wmul,wmac,wavg2,tinsr,textrm,wshufh,wcmpeq,wcmpgt,wmax,wmin,wpack,wunpckih,wunpckil,wunpckeh,wunpckel,wror,wsra,wsrl,wsll,wmadd,tmia,tmiaph,tmiaxy,tbcst,tmovmsk,wacc,waligni,walignr,tandc,textrc,torc,torvsc,wsad,wabs,wabsdiff,waddsubhx,wsubaddhx,wavg4,wmulw,wqmulm,wqmulwm,waddbhus,wqmiaxy,wmiaxy,wmiawxy,wmerge" (const_string "none"))

; Load scheduling, set from the arm_ld_sched variable
; initialized by arm_option_override()
(define_attr "ldsched" "no,yes" (const (symbol_ref "arm_ld_sched")))

;; Classification of NEON instructions for scheduling purposes.
(define_attr "neon_type"
   "neon_int_1,\
   neon_int_2,\
   neon_int_3,\
   neon_int_4,\
   neon_int_5,\
   neon_vqneg_vqabs,\
   neon_vmov,\
   neon_vaba,\
   neon_vsma,\
   neon_vaba_qqq,\
   neon_mul_ddd_8_16_qdd_16_8_long_32_16_long,\
   neon_mul_qqq_8_16_32_ddd_32,\
   neon_mul_qdd_64_32_long_qqd_16_ddd_32_scalar_64_32_long_scalar,\
   neon_mla_ddd_8_16_qdd_16_8_long_32_16_long,\
   neon_mla_qqq_8_16,\
   neon_mla_ddd_32_qqd_16_ddd_32_scalar_qdd_64_32_long_scalar_qdd_64_32_long,\
   neon_mla_qqq_32_qqd_32_scalar,\
   neon_mul_ddd_16_scalar_32_16_long_scalar,\
   neon_mul_qqd_32_scalar,\
   neon_mla_ddd_16_scalar_qdd_32_16_long_scalar,\
   neon_shift_1,\
   neon_shift_2,\
   neon_shift_3,\
   neon_vshl_ddd,\
   neon_vqshl_vrshl_vqrshl_qqq,\
   neon_vsra_vrsra,\
   neon_fp_vadd_ddd_vabs_dd,\
   neon_fp_vadd_qqq_vabs_qq,\
   neon_fp_vsum,\
   neon_fp_vmul_ddd,\
   neon_fp_vmul_qqd,\
   neon_fp_vmla_ddd,\
   neon_fp_vmla_qqq,\
   neon_fp_vmla_ddd_scalar,\
   neon_fp_vmla_qqq_scalar,\
   neon_fp_vrecps_vrsqrts_ddd,\
   neon_fp_vrecps_vrsqrts_qqq,\
   neon_bp_simple,\
   neon_bp_2cycle,\
   neon_bp_3cycle,\
   neon_ldr,\
   neon_str,\
   neon_vld1_1_2_regs,\
   neon_vld1_3_4_regs,\
   neon_vld2_2_regs_vld1_vld2_all_lanes,\
   neon_vld2_4_regs,\
   neon_vld3_vld4,\
   neon_vst1_1_2_regs_vst2_2_regs,\
   neon_vst1_3_4_regs,\
   neon_vst2_4_regs_vst3_vst4,\
   neon_vst3_vst4,\
   neon_vld1_vld2_lane,\
   neon_vld3_vld4_lane,\
   neon_vst1_vst2_lane,\
   neon_vst3_vst4_lane,\
   neon_vld3_vld4_all_lanes,\
   neon_mcr,\
   neon_mcr_2_mcrr,\
   neon_mrc,\
   neon_mrrc,\
   neon_ldm_2,\
   neon_stm_2,\
   none"
 (const_string "none"))

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
	 (if_then_else (eq_attr "neon_type" "none")
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
		 "alu,alu_shift,float,fdivd,fdivs")
		(const_string "single")
	        (const_string "multi")))

;; FAR_JUMP is "yes" if a BL instruction is used to generate a branch to a
;; distant label.  Only applicable to Thumb code.
(define_attr "far_jump" "yes,no" (const_string "no"))


;; The number of machine instructions this pattern expands to.
;; Used for Thumb-2 conditional execution.
(define_attr "ce_count" "" (const_int 1))

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
          (ior (eq_attr "tune" "fa526,fa626,fa606te,fa626te,fmp626,fa726te,arm926ejs,arm1020e,arm1026ejs,arm1136js,arm1136jfs,cortexa5,cortexa8,cortexa9,cortexa15,cortexm4")
	       (eq_attr "tune_cortexr4" "yes"))
          (const_string "no")
          (const_string "yes"))))

(define_attr "generic_vfp" "yes,no"
  (const (if_then_else
	  (and (eq_attr "fpu" "vfp")
	       (eq_attr "tune" "!arm1020e,arm1022e,cortexa5,cortexa8,cortexa9,cortexm4")
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
(include "cortex-a8.md")
(include "cortex-a9.md")
(include "cortex-a15.md")
(include "cortex-r4.md")
(include "cortex-r4f.md")
(include "cortex-m4.md")
(include "cortex-m4-fpu.md")
(include "vfp11.md")


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
      if (GET_CODE (operands[1]) != REG)
        operands[1] = force_reg (DImode, operands[1]);
      if (GET_CODE (operands[2]) != REG)
        operands[2] = force_reg (DImode, operands[2]);
     }
  "
)

(define_insn "*thumb1_adddi3"
  [(set (match_operand:DI          0 "register_operand" "=l")
	(plus:DI (match_operand:DI 1 "register_operand" "%0")
		 (match_operand:DI 2 "register_operand" "l")))
   (clobber (reg:CC CC_REGNUM))
  ]
  "TARGET_THUMB1"
  "add\\t%Q0, %Q0, %Q2\;adc\\t%R0, %R0, %R2"
  [(set_attr "length" "4")]
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
   (set_attr "length" "8")]
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
   (set_attr "length" "8")]
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
   (set_attr "length" "8")]
)

(define_expand "addsi3"
  [(set (match_operand:SI          0 "s_register_operand" "")
	(plus:SI (match_operand:SI 1 "s_register_operand" "")
		 (match_operand:SI 2 "reg_or_int_operand" "")))]
  "TARGET_EITHER"
  "
  if (TARGET_32BIT && GET_CODE (operands[2]) == CONST_INT)
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
  [(set (match_operand:SI          0 "s_register_operand" "=r, k,r,r, k, r, k,k,r, k, r")
	(plus:SI (match_operand:SI 1 "s_register_operand" "%rk,k,r,rk,k, rk,k,r,rk,k, rk")
		 (match_operand:SI 2 "reg_or_int_operand" "rI,rI,k,Pj,Pj,L, L,L,PJ,PJ,?n")))]
  "TARGET_32BIT"
  "@
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
   && GET_CODE (operands[2]) == CONST_INT
   && !const_ok_for_op (INTVAL (operands[2]), PLUS)
   && (reload_completed || !arm_eliminable_register (operands[1]))"
  [(clobber (const_int 0))]
  "
  arm_split_constant (PLUS, SImode, curr_insn,
	              INTVAL (operands[2]), operands[0],
		      operands[1], 0);
  DONE;
  "
  [(set_attr "length" "4,4,4,4,4,4,4,4,4,4,16")
   (set_attr "predicable" "yes")
   (set_attr "arch" "*,*,*,t2,t2,*,*,a,t2,t2,*")]
)

(define_insn_and_split "*thumb1_addsi3"
  [(set (match_operand:SI          0 "register_operand" "=l,l,l,*rk,*hk,l,k,l,l,l")
	(plus:SI (match_operand:SI 1 "register_operand" "%0,0,l,*0,*0,k,k,0,l,k")
		 (match_operand:SI 2 "nonmemory_operand" "I,J,lL,*hk,*rk,M,O,Pa,Pb,Pc")))]
  "TARGET_THUMB1"
  "*
   static const char * const asms[] = 
   {
     \"add\\t%0, %0, %2\",
     \"sub\\t%0, %0, #%n2\",
     \"add\\t%0, %1, %2\",
     \"add\\t%0, %0, %2\",
     \"add\\t%0, %0, %2\",
     \"add\\t%0, %1, %2\",
     \"add\\t%0, %1, %2\",
     \"#\",
     \"#\",
     \"#\"
   };
   if ((which_alternative == 2 || which_alternative == 6)
       && GET_CODE (operands[2]) == CONST_INT
       && INTVAL (operands[2]) < 0)
     return \"sub\\t%0, %1, #%n2\";
   return asms[which_alternative];
  "
  "&& reload_completed && CONST_INT_P (operands[2])
   && ((operands[1] != stack_pointer_rtx
        && (INTVAL (operands[2]) > 255 || INTVAL (operands[2]) < -255))
       || (operands[1] == stack_pointer_rtx
 	   && INTVAL (operands[2]) > 1020))"
  [(set (match_dup 0) (plus:SI (match_dup 1) (match_dup 2)))
   (set (match_dup 0) (plus:SI (match_dup 0) (match_dup 3)))]
  {
    HOST_WIDE_INT offset = INTVAL (operands[2]);
    if (operands[1] == stack_pointer_rtx)
      offset -= 1020;
    else
      {
        if (offset > 255)
	  offset = 255;
	else if (offset < -255)
	  offset = -255;
      }
    operands[3] = GEN_INT (offset);
    operands[2] = GEN_INT (INTVAL (operands[2]) - offset);
  }
  [(set_attr "length" "2,2,2,2,2,2,2,4,4,4")]
)

;; Reloading and elimination of the frame pointer can
;; sometimes cause this optimization to be missed.
(define_peephole2
  [(set (match_operand:SI 0 "arm_general_register_operand" "")
	(match_operand:SI 1 "const_int_operand" ""))
   (set (match_dup 0)
	(plus:SI (match_dup 0) (reg:SI SP_REGNUM)))]
  "TARGET_THUMB1
   && (unsigned HOST_WIDE_INT) (INTVAL (operands[1])) < 1024
   && (INTVAL (operands[1]) & 3) == 0"
  [(set (match_dup 0) (plus:SI (reg:SI SP_REGNUM) (match_dup 1)))]
  ""
)

(define_insn "addsi3_compare0"
  [(set (reg:CC_NOOV CC_REGNUM)
	(compare:CC_NOOV
	 (plus:SI (match_operand:SI 1 "s_register_operand" "r, r")
		  (match_operand:SI 2 "arm_add_operand"    "rI,L"))
	 (const_int 0)))
   (set (match_operand:SI 0 "s_register_operand" "=r,r")
	(plus:SI (match_dup 1) (match_dup 2)))]
  "TARGET_ARM"
  "@
   add%.\\t%0, %1, %2
   sub%.\\t%0, %1, #%n2"
  [(set_attr "conds" "set")]
)

(define_insn "*addsi3_compare0_scratch"
  [(set (reg:CC_NOOV CC_REGNUM)
	(compare:CC_NOOV
	 (plus:SI (match_operand:SI 0 "s_register_operand" "r, r")
		  (match_operand:SI 1 "arm_add_operand"    "rI,L"))
	 (const_int 0)))]
  "TARGET_ARM"
  "@
   cmn%?\\t%0, %1
   cmp%?\\t%0, #%n1"
  [(set_attr "conds" "set")
   (set_attr "predicable" "yes")]
)

(define_insn "*compare_negsi_si"
  [(set (reg:CC_Z CC_REGNUM)
	(compare:CC_Z
	 (neg:SI (match_operand:SI 0 "s_register_operand" "r"))
	 (match_operand:SI 1 "s_register_operand" "r")))]
  "TARGET_32BIT"
  "cmn%?\\t%1, %0"
  [(set_attr "conds" "set")
   (set_attr "predicable" "yes")]
)

;; This is the canonicalization of addsi3_compare0_for_combiner when the
;; addend is a constant.
(define_insn "*cmpsi2_addneg"
  [(set (reg:CC CC_REGNUM)
	(compare:CC
	 (match_operand:SI 1 "s_register_operand" "r,r")
	 (match_operand:SI 2 "arm_addimm_operand" "L,I")))
   (set (match_operand:SI 0 "s_register_operand" "=r,r")
	(plus:SI (match_dup 1)
		 (match_operand:SI 3 "arm_addimm_operand" "I,L")))]
  "TARGET_32BIT && INTVAL (operands[2]) == -INTVAL (operands[3])"
  "@
   add%.\\t%0, %1, %3
   sub%.\\t%0, %1, #%n3"
  [(set_attr "conds" "set")]
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
	 (plus:SI (match_operand:SI 1 "s_register_operand" "r,r")
		  (match_operand:SI 2 "arm_add_operand" "rI,L"))
	 (match_dup 1)))
   (set (match_operand:SI 0 "s_register_operand" "=r,r")
	(plus:SI (match_dup 1) (match_dup 2)))]
  "TARGET_32BIT"
  "@
   add%.\\t%0, %1, %2
   sub%.\\t%0, %1, #%n2"
  [(set_attr "conds" "set")]
)

(define_insn "*addsi3_compare_op2"
  [(set (reg:CC_C CC_REGNUM)
	(compare:CC_C
	 (plus:SI (match_operand:SI 1 "s_register_operand" "r,r")
		  (match_operand:SI 2 "arm_add_operand" "rI,L"))
	 (match_dup 2)))
   (set (match_operand:SI 0 "s_register_operand" "=r,r")
	(plus:SI (match_dup 1) (match_dup 2)))]
  "TARGET_32BIT"
  "@
   add%.\\t%0, %1, %2
   sub%.\\t%0, %1, #%n2"
  [(set_attr "conds" "set")]
)

(define_insn "*compare_addsi2_op0"
  [(set (reg:CC_C CC_REGNUM)
	(compare:CC_C
	 (plus:SI (match_operand:SI 0 "s_register_operand" "r,r")
		  (match_operand:SI 1 "arm_add_operand" "rI,L"))
	 (match_dup 0)))]
  "TARGET_32BIT"
  "@
   cmn%?\\t%0, %1
   cmp%?\\t%0, #%n1"
  [(set_attr "conds" "set")
   (set_attr "predicable" "yes")]
)

(define_insn "*compare_addsi2_op1"
  [(set (reg:CC_C CC_REGNUM)
	(compare:CC_C
	 (plus:SI (match_operand:SI 0 "s_register_operand" "r,r")
		  (match_operand:SI 1 "arm_add_operand" "rI,L"))
	 (match_dup 1)))]
  "TARGET_32BIT"
  "@
   cmn%?\\t%0, %1
   cmp%?\\t%0, #%n1"
  [(set_attr "conds" "set")
   (set_attr "predicable" "yes")]
)

(define_insn "*addsi3_carryin_<optab>"
  [(set (match_operand:SI 0 "s_register_operand" "=r,r")
	(plus:SI (plus:SI (match_operand:SI 1 "s_register_operand" "%r,r")
			  (match_operand:SI 2 "arm_not_operand" "rI,K"))
		 (LTUGEU:SI (reg:<cnb> CC_REGNUM) (const_int 0))))]
  "TARGET_32BIT"
  "@
   adc%?\\t%0, %1, %2
   sbc%?\\t%0, %1, #%B2"
  [(set_attr "conds" "use")]
)

(define_insn "*addsi3_carryin_alt2_<optab>"
  [(set (match_operand:SI 0 "s_register_operand" "=r,r")
	(plus:SI (plus:SI (LTUGEU:SI (reg:<cnb> CC_REGNUM) (const_int 0))
			  (match_operand:SI 1 "s_register_operand" "%r,r"))
		 (match_operand:SI 2 "arm_rhs_operand" "rI,K")))]
  "TARGET_32BIT"
  "@
   adc%?\\t%0, %1, %2
   sbc%?\\t%0, %1, #%B2"
  [(set_attr "conds" "use")]
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
   (set (attr "type") (if_then_else (match_operand 4 "const_int_operand" "")
		      (const_string "alu_shift")
		      (const_string "alu_shift_reg")))]
)

(define_insn "*addsi3_carryin_clobercc_<optab>"
  [(set (match_operand:SI 0 "s_register_operand" "=r")
	(plus:SI (plus:SI (match_operand:SI 1 "s_register_operand" "%r")
			  (match_operand:SI 2 "arm_rhs_operand" "rI"))
		 (LTUGEU:SI (reg:<cnb> CC_REGNUM) (const_int 0))))
   (clobber (reg:CC CC_REGNUM))]
   "TARGET_32BIT"
   "adc%.\\t%0, %1, %2"
   [(set_attr "conds" "set")]
)

(define_expand "incscc"
  [(set (match_operand:SI 0 "s_register_operand" "=r,r")
        (plus:SI (match_operator:SI 2 "arm_comparison_operator"
                    [(match_operand:CC 3 "cc_register" "") (const_int 0)])
                 (match_operand:SI 1 "s_register_operand" "0,?r")))]
  "TARGET_32BIT"
  ""
)

(define_insn "*arm_incscc"
  [(set (match_operand:SI 0 "s_register_operand" "=r,r")
        (plus:SI (match_operator:SI 2 "arm_comparison_operator"
                    [(match_operand:CC 3 "cc_register" "") (const_int 0)])
                 (match_operand:SI 1 "s_register_operand" "0,?r")))]
  "TARGET_ARM"
  "@
  add%d2\\t%0, %1, #1
  mov%D2\\t%0, %1\;add%d2\\t%0, %1, #1"
  [(set_attr "conds" "use")
   (set_attr "length" "4,8")]
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
      if (GET_CODE (operands[1]) != REG)
        operands[1] = force_reg (DImode, operands[1]);
      if (GET_CODE (operands[2]) != REG)
        operands[2] = force_reg (DImode, operands[2]);
     }	
  "
)

(define_insn "*arm_subdi3"
  [(set (match_operand:DI           0 "s_register_operand" "=&r,&r,&r")
	(minus:DI (match_operand:DI 1 "s_register_operand" "0,r,0")
		  (match_operand:DI 2 "s_register_operand" "r,0,0")))
   (clobber (reg:CC CC_REGNUM))]
  "TARGET_32BIT && !TARGET_NEON"
  "subs\\t%Q0, %Q1, %Q2\;sbc\\t%R0, %R1, %R2"
  [(set_attr "conds" "clob")
   (set_attr "length" "8")]
)

(define_insn "*thumb_subdi3"
  [(set (match_operand:DI           0 "register_operand" "=l")
	(minus:DI (match_operand:DI 1 "register_operand"  "0")
		  (match_operand:DI 2 "register_operand"  "l")))
   (clobber (reg:CC CC_REGNUM))]
  "TARGET_THUMB1"
  "sub\\t%Q0, %Q0, %Q2\;sbc\\t%R0, %R0, %R2"
  [(set_attr "length" "4")]
)

(define_insn "*subdi_di_zesidi"
  [(set (match_operand:DI           0 "s_register_operand" "=&r,&r")
	(minus:DI (match_operand:DI 1 "s_register_operand"  "0,r")
		  (zero_extend:DI
		   (match_operand:SI 2 "s_register_operand"  "r,r"))))
   (clobber (reg:CC CC_REGNUM))]
  "TARGET_32BIT"
  "subs\\t%Q0, %Q1, %2\;sbc\\t%R0, %R1, #0"
  [(set_attr "conds" "clob")
   (set_attr "length" "8")]
)

(define_insn "*subdi_di_sesidi"
  [(set (match_operand:DI            0 "s_register_operand" "=&r,&r")
	(minus:DI (match_operand:DI  1 "s_register_operand"  "0,r")
		  (sign_extend:DI
		   (match_operand:SI 2 "s_register_operand"  "r,r"))))
   (clobber (reg:CC CC_REGNUM))]
  "TARGET_32BIT"
  "subs\\t%Q0, %Q1, %2\;sbc\\t%R0, %R1, %2, asr #31"
  [(set_attr "conds" "clob")
   (set_attr "length" "8")]
)

(define_insn "*subdi_zesidi_di"
  [(set (match_operand:DI            0 "s_register_operand" "=&r,&r")
	(minus:DI (zero_extend:DI
		   (match_operand:SI 2 "s_register_operand"  "r,r"))
		  (match_operand:DI  1 "s_register_operand" "0,r")))
   (clobber (reg:CC CC_REGNUM))]
  "TARGET_ARM"
  "rsbs\\t%Q0, %Q1, %2\;rsc\\t%R0, %R1, #0"
  [(set_attr "conds" "clob")
   (set_attr "length" "8")]
)

(define_insn "*subdi_sesidi_di"
  [(set (match_operand:DI            0 "s_register_operand" "=&r,&r")
	(minus:DI (sign_extend:DI
		   (match_operand:SI 2 "s_register_operand"   "r,r"))
		  (match_operand:DI  1 "s_register_operand"  "0,r")))
   (clobber (reg:CC CC_REGNUM))]
  "TARGET_ARM"
  "rsbs\\t%Q0, %Q1, %2\;rsc\\t%R0, %R1, %2, asr #31"
  [(set_attr "conds" "clob")
   (set_attr "length" "8")]
)

(define_insn "*subdi_zesidi_zesidi"
  [(set (match_operand:DI            0 "s_register_operand" "=r")
	(minus:DI (zero_extend:DI
		   (match_operand:SI 1 "s_register_operand"  "r"))
		  (zero_extend:DI
		   (match_operand:SI 2 "s_register_operand"  "r"))))
   (clobber (reg:CC CC_REGNUM))]
  "TARGET_32BIT"
  "subs\\t%Q0, %1, %2\;sbc\\t%R0, %1, %1"
  [(set_attr "conds" "clob")
   (set_attr "length" "8")]
)

(define_expand "subsi3"
  [(set (match_operand:SI           0 "s_register_operand" "")
	(minus:SI (match_operand:SI 1 "reg_or_int_operand" "")
		  (match_operand:SI 2 "s_register_operand" "")))]
  "TARGET_EITHER"
  "
  if (GET_CODE (operands[1]) == CONST_INT)
    {
      if (TARGET_32BIT)
        {
          arm_split_constant (MINUS, SImode, NULL_RTX,
	                      INTVAL (operands[1]), operands[0],
	  		      operands[2], optimize && can_create_pseudo_p ());
          DONE;
	}
      else /* TARGET_THUMB1 */
        operands[1] = force_reg (SImode, operands[1]);
    }
  "
)

(define_insn "thumb1_subsi3_insn"
  [(set (match_operand:SI           0 "register_operand" "=l")
	(minus:SI (match_operand:SI 1 "register_operand" "l")
		  (match_operand:SI 2 "reg_or_int_operand" "lPd")))]
  "TARGET_THUMB1"
  "sub\\t%0, %1, %2"
  [(set_attr "length" "2")
   (set_attr "conds" "set")])

; ??? Check Thumb-2 split length
(define_insn_and_split "*arm_subsi3_insn"
  [(set (match_operand:SI           0 "s_register_operand" "=r,r,rk,r")
	(minus:SI (match_operand:SI 1 "reg_or_int_operand" "rI,r,k,?n")
		  (match_operand:SI 2 "reg_or_int_operand" "r,rI,r, r")))]
  "TARGET_32BIT"
  "@
   rsb%?\\t%0, %2, %1
   sub%?\\t%0, %1, %2
   sub%?\\t%0, %1, %2
   #"
  "&& (GET_CODE (operands[1]) == CONST_INT
       && !const_ok_for_arm (INTVAL (operands[1])))"
  [(clobber (const_int 0))]
  "
  arm_split_constant (MINUS, SImode, curr_insn,
                      INTVAL (operands[1]), operands[0], operands[2], 0);
  DONE;
  "
  [(set_attr "length" "4,4,4,16")
   (set_attr "predicable" "yes")]
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

(define_insn "*subsi3_compare0"
  [(set (reg:CC_NOOV CC_REGNUM)
	(compare:CC_NOOV
	 (minus:SI (match_operand:SI 1 "arm_rhs_operand" "r,I")
		   (match_operand:SI 2 "arm_rhs_operand" "rI,r"))
	 (const_int 0)))
   (set (match_operand:SI 0 "s_register_operand" "=r,r")
	(minus:SI (match_dup 1) (match_dup 2)))]
  "TARGET_32BIT"
  "@
   sub%.\\t%0, %1, %2
   rsb%.\\t%0, %2, %1"
  [(set_attr "conds" "set")]
)

(define_insn "*subsi3_compare"
  [(set (reg:CC CC_REGNUM)
	(compare:CC (match_operand:SI 1 "arm_rhs_operand" "r,I")
		    (match_operand:SI 2 "arm_rhs_operand" "rI,r")))
   (set (match_operand:SI 0 "s_register_operand" "=r,r")
	(minus:SI (match_dup 1) (match_dup 2)))]
  "TARGET_32BIT"
  "@
   sub%.\\t%0, %1, %2
   rsb%.\\t%0, %2, %1"
  [(set_attr "conds" "set")]
)

(define_expand "decscc"
  [(set (match_operand:SI            0 "s_register_operand" "=r,r")
        (minus:SI (match_operand:SI  1 "s_register_operand" "0,?r")
		  (match_operator:SI 2 "arm_comparison_operator"
                   [(match_operand   3 "cc_register" "") (const_int 0)])))]
  "TARGET_32BIT"
  ""
)

(define_insn "*arm_decscc"
  [(set (match_operand:SI            0 "s_register_operand" "=r,r")
        (minus:SI (match_operand:SI  1 "s_register_operand" "0,?r")
		  (match_operator:SI 2 "arm_comparison_operator"
                   [(match_operand   3 "cc_register" "") (const_int 0)])))]
  "TARGET_ARM"
  "@
   sub%d2\\t%0, %1, #1
   mov%D2\\t%0, %1\;sub%d2\\t%0, %1, #1"
  [(set_attr "conds" "use")
   (set_attr "length" "*,8")]
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
  [(set_attr "insn" "mul")
   (set_attr "predicable" "yes")]
)

(define_insn "*arm_mulsi3_v6"
  [(set (match_operand:SI          0 "s_register_operand" "=r")
	(mult:SI (match_operand:SI 1 "s_register_operand" "r")
		 (match_operand:SI 2 "s_register_operand" "r")))]
  "TARGET_32BIT && arm_arch6"
  "mul%?\\t%0, %1, %2"
  [(set_attr "insn" "mul")
   (set_attr "predicable" "yes")]
)

; Unfortunately with the Thumb the '&'/'0' trick can fails when operands 
; 1 and 2; are the same, because reload will make operand 0 match 
; operand 1 without realizing that this conflicts with operand 2.  We fix 
; this by adding another alternative to match this case, and then `reload' 
; it ourselves.  This alternative must come first.
(define_insn "*thumb_mulsi3"
  [(set (match_operand:SI          0 "register_operand" "=&l,&l,&l")
	(mult:SI (match_operand:SI 1 "register_operand" "%l,*h,0")
		 (match_operand:SI 2 "register_operand" "l,l,l")))]
  "TARGET_THUMB1 && !arm_arch6"
  "*
  if (which_alternative < 2)
    return \"mov\\t%0, %1\;mul\\t%0, %2\";
  else
    return \"mul\\t%0, %2\";
  "
  [(set_attr "length" "4,4,2")
   (set_attr "insn" "mul")]
)

(define_insn "*thumb_mulsi3_v6"
  [(set (match_operand:SI          0 "register_operand" "=l,l,l")
	(mult:SI (match_operand:SI 1 "register_operand" "0,l,0")
		 (match_operand:SI 2 "register_operand" "l,0,0")))]
  "TARGET_THUMB1 && arm_arch6"
  "@
   mul\\t%0, %2
   mul\\t%0, %1
   mul\\t%0, %1"
  [(set_attr "length" "2")
   (set_attr "insn" "mul")]
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
  "mul%.\\t%0, %2, %1"
  [(set_attr "conds" "set")
   (set_attr "insn" "muls")]
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
  "mul%.\\t%0, %2, %1"
  [(set_attr "conds" "set")
   (set_attr "insn" "muls")]
)

(define_insn "*mulsi_compare0_scratch"
  [(set (reg:CC_NOOV CC_REGNUM)
	(compare:CC_NOOV (mult:SI
			  (match_operand:SI 2 "s_register_operand" "r,r")
			  (match_operand:SI 1 "s_register_operand" "%0,r"))
			 (const_int 0)))
   (clobber (match_scratch:SI 0 "=&r,&r"))]
  "TARGET_ARM && !arm_arch6"
  "mul%.\\t%0, %2, %1"
  [(set_attr "conds" "set")
   (set_attr "insn" "muls")]
)

(define_insn "*mulsi_compare0_scratch_v6"
  [(set (reg:CC_NOOV CC_REGNUM)
	(compare:CC_NOOV (mult:SI
			  (match_operand:SI 2 "s_register_operand" "r")
			  (match_operand:SI 1 "s_register_operand" "r"))
			 (const_int 0)))
   (clobber (match_scratch:SI 0 "=r"))]
  "TARGET_ARM && arm_arch6 && optimize_size"
  "mul%.\\t%0, %2, %1"
  [(set_attr "conds" "set")
   (set_attr "insn" "muls")]
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
  [(set_attr "insn" "mla")
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
  [(set_attr "insn" "mla")
   (set_attr "predicable" "yes")]
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
  "mla%.\\t%0, %2, %1, %3"
  [(set_attr "conds" "set")
   (set_attr "insn" "mlas")]
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
  "mla%.\\t%0, %2, %1, %3"
  [(set_attr "conds" "set")
   (set_attr "insn" "mlas")]
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
  "mla%.\\t%0, %2, %1, %3"
  [(set_attr "conds" "set")
   (set_attr "insn" "mlas")]
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
  "mla%.\\t%0, %2, %1, %3"
  [(set_attr "conds" "set")
   (set_attr "insn" "mlas")]
)

(define_insn "*mulsi3subsi"
  [(set (match_operand:SI 0 "s_register_operand" "=r")
	(minus:SI
	  (match_operand:SI 3 "s_register_operand" "r")
	  (mult:SI (match_operand:SI 2 "s_register_operand" "r")
		   (match_operand:SI 1 "s_register_operand" "r"))))]
  "TARGET_32BIT && arm_arch_thumb2"
  "mls%?\\t%0, %2, %1, %3"
  [(set_attr "insn" "mla")
   (set_attr "predicable" "yes")]
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
  [(set_attr "insn" "smlal")
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
  [(set_attr "insn" "smlal")
   (set_attr "predicable" "yes")]
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
  [(set_attr "insn" "smull")
   (set_attr "predicable" "yes")]
)

(define_insn "*mulsidi3_v6"
  [(set (match_operand:DI 0 "s_register_operand" "=r")
	(mult:DI
	 (sign_extend:DI (match_operand:SI 1 "s_register_operand" "r"))
	 (sign_extend:DI (match_operand:SI 2 "s_register_operand" "r"))))]
  "TARGET_32BIT && arm_arch6"
  "smull%?\\t%Q0, %R0, %1, %2"
  [(set_attr "insn" "smull")
   (set_attr "predicable" "yes")]
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
  [(set_attr "insn" "umull")
   (set_attr "predicable" "yes")]
)

(define_insn "*umulsidi3_v6"
  [(set (match_operand:DI 0 "s_register_operand" "=r")
	(mult:DI
	 (zero_extend:DI (match_operand:SI 1 "s_register_operand" "r"))
	 (zero_extend:DI (match_operand:SI 2 "s_register_operand" "r"))))]
  "TARGET_32BIT && arm_arch6"
  "umull%?\\t%Q0, %R0, %1, %2"
  [(set_attr "insn" "umull")
   (set_attr "predicable" "yes")]
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
  [(set_attr "insn" "umlal")
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
  [(set_attr "insn" "umlal")
   (set_attr "predicable" "yes")]
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
  [(set_attr "insn" "smull")
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
  [(set_attr "insn" "smull")
   (set_attr "predicable" "yes")]
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
  [(set_attr "insn" "umull")
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
  [(set_attr "insn" "umull")
   (set_attr "predicable" "yes")]
)

(define_insn "mulhisi3"
  [(set (match_operand:SI 0 "s_register_operand" "=r")
	(mult:SI (sign_extend:SI
		  (match_operand:HI 1 "s_register_operand" "%r"))
		 (sign_extend:SI
		  (match_operand:HI 2 "s_register_operand" "r"))))]
  "TARGET_DSP_MULTIPLY"
  "smulbb%?\\t%0, %1, %2"
  [(set_attr "insn" "smulxy")
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
  [(set_attr "insn" "smulxy")
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
  [(set_attr "insn" "smulxy")
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
  [(set_attr "insn" "smulxy")
   (set_attr "predicable" "yes")]
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
  [(set_attr "insn" "smlaxy")
   (set_attr "predicable" "yes")]
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
  [(set_attr "insn" "smlaxy")
   (set_attr "predicable" "yes")]
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
  [(set_attr "insn" "smlaxy")
   (set_attr "predicable" "yes")]
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
  [(set_attr "insn" "smlalxy")
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
  [(set_attr "insn" "smlalxy")
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
  [(set_attr "insn" "smlalxy")
   (set_attr "predicable" "yes")])

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

(define_insn "*anddi3_insn"
  [(set (match_operand:DI         0 "s_register_operand" "=&r,&r")
	(and:DI (match_operand:DI 1 "s_register_operand"  "%0,r")
		(match_operand:DI 2 "s_register_operand"   "r,r")))]
  "TARGET_32BIT && !TARGET_IWMMXT && !TARGET_NEON"
  "#"
  [(set_attr "length" "8")]
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
  [(set_attr "length" "8")]
)

(define_insn "*anddi_sesdi_di"
  [(set (match_operand:DI          0 "s_register_operand" "=&r,&r")
	(and:DI (sign_extend:DI
		 (match_operand:SI 2 "s_register_operand" "r,r"))
		(match_operand:DI  1 "s_register_operand" "0,r")))]
  "TARGET_32BIT"
  "#"
  [(set_attr "length" "8")]
)

(define_expand "andsi3"
  [(set (match_operand:SI         0 "s_register_operand" "")
	(and:SI (match_operand:SI 1 "s_register_operand" "")
		(match_operand:SI 2 "reg_or_int_operand" "")))]
  "TARGET_EITHER"
  "
  if (TARGET_32BIT)
    {
      if (GET_CODE (operands[2]) == CONST_INT)
        {
	  if (INTVAL (operands[2]) == 255 && arm_arch6)
	    {
	      operands[1] = convert_to_mode (QImode, operands[1], 1);
	      emit_insn (gen_thumb2_zero_extendqisi2_v6 (operands[0],
							 operands[1]));
	    }
	  else
	    arm_split_constant (AND, SImode, NULL_RTX,
				INTVAL (operands[2]), operands[0],
				operands[1],
				optimize && can_create_pseudo_p ());

          DONE;
        }
    }
  else /* TARGET_THUMB1 */
    {
      if (GET_CODE (operands[2]) != CONST_INT)
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
	      if ((((HOST_WIDE_INT) 1) << i) - 1 == INTVAL (operands[2]))
	        {
	          emit_insn (gen_extzv (operands[0], operands[1], GEN_INT (i),
			 	        const0_rtx));
	          DONE;
	        }
	      else if ((((HOST_WIDE_INT) 1) << i) - 1
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
  [(set (match_operand:SI         0 "s_register_operand" "=r,r,r")
	(and:SI (match_operand:SI 1 "s_register_operand" "r,r,r")
		(match_operand:SI 2 "reg_or_int_operand" "rI,K,?n")))]
  "TARGET_32BIT"
  "@
   and%?\\t%0, %1, %2
   bic%?\\t%0, %1, #%B2
   #"
  "TARGET_32BIT
   && GET_CODE (operands[2]) == CONST_INT
   && !(const_ok_for_arm (INTVAL (operands[2]))
	|| const_ok_for_arm (~INTVAL (operands[2])))"
  [(clobber (const_int 0))]
  "
  arm_split_constant  (AND, SImode, curr_insn, 
	               INTVAL (operands[2]), operands[0], operands[1], 0);
  DONE;
  "
  [(set_attr "length" "4,4,16")
   (set_attr "predicable" "yes")]
)

(define_insn "*thumb1_andsi3_insn"
  [(set (match_operand:SI         0 "register_operand" "=l")
	(and:SI (match_operand:SI 1 "register_operand" "%0")
		(match_operand:SI 2 "register_operand" "l")))]
  "TARGET_THUMB1"
  "and\\t%0, %2"
  [(set_attr "length" "2")
   (set_attr "conds" "set")])

(define_insn "*andsi3_compare0"
  [(set (reg:CC_NOOV CC_REGNUM)
	(compare:CC_NOOV
	 (and:SI (match_operand:SI 1 "s_register_operand" "r,r")
		 (match_operand:SI 2 "arm_not_operand" "rI,K"))
	 (const_int 0)))
   (set (match_operand:SI          0 "s_register_operand" "=r,r")
	(and:SI (match_dup 1) (match_dup 2)))]
  "TARGET_32BIT"
  "@
   and%.\\t%0, %1, %2
   bic%.\\t%0, %1, #%B2"
  [(set_attr "conds" "set")]
)

(define_insn "*andsi3_compare0_scratch"
  [(set (reg:CC_NOOV CC_REGNUM)
	(compare:CC_NOOV
	 (and:SI (match_operand:SI 0 "s_register_operand" "r,r")
		 (match_operand:SI 1 "arm_not_operand" "rI,K"))
	 (const_int 0)))
   (clobber (match_scratch:SI 2 "=X,r"))]
  "TARGET_32BIT"
  "@
   tst%?\\t%0, %1
   bic%.\\t%2, %0, #%B1"
  [(set_attr "conds" "set")]
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
   (set_attr "predicable" "yes")]
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
		      (const_int 8)))]
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
   (set_attr "length" "8")]
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
   (set_attr "length" "8")]
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
   (set_attr "length" "8")]
)

(define_split
  [(set (match_operand:SI 0 "s_register_operand" "")
	(zero_extract:SI (match_operand:SI 1 "s_register_operand" "")
			 (match_operand:SI 2 "const_int_operand" "")
			 (match_operand:SI 3 "const_int_operand" "")))
   (clobber (match_operand:SI 4 "s_register_operand" ""))]
  "TARGET_THUMB1"
  [(set (match_dup 4) (ashift:SI (match_dup 1) (match_dup 2)))
   (set (match_dup 0) (lshiftrt:SI (match_dup 4) (match_dup 3)))]
  "{
     HOST_WIDE_INT temp = INTVAL (operands[2]);

     operands[2] = GEN_INT (32 - temp - INTVAL (operands[3]));
     operands[3] = GEN_INT (32 - temp);
   }"
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
	(sign_extract:SI (match_operand:SI 1 "s_register_operand" "")
			 (match_operand:SI 2 "const_int_operand" "")
			 (match_operand:SI 3 "const_int_operand" "")))]
  "TARGET_THUMB1"
  [(set (match_dup 0) (ashift:SI (match_dup 1) (match_dup 2)))
   (set (match_dup 0) (ashiftrt:SI (match_dup 0) (match_dup 3)))]
  "{
     HOST_WIDE_INT temp = INTVAL (operands[2]);

     operands[2] = GEN_INT (32 - temp - INTVAL (operands[3]));
     operands[3] = GEN_INT (32 - temp);
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
    HOST_WIDE_INT mask = (((HOST_WIDE_INT)1) << width) - 1;
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

	    if (GET_CODE (operands[3]) == CONST_INT)
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
		if (GET_CODE (operands[3]) != REG)
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

    if (GET_CODE (operands[3]) == CONST_INT)
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

	if (GET_CODE (op0) == CONST_INT
	    && (const_ok_for_arm (mask << start_bit)
		|| const_ok_for_arm (~(mask << start_bit))))
	  {
	    op0 = gen_int_mode (~(mask << start_bit), SImode);
	    emit_insn (gen_andsi3 (op2, operands[0], op0));
	  }
	else
	  {
	    if (GET_CODE (op0) == CONST_INT)
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
                         (match_operand:SI 1 "const_int_operand" "M")
                         (match_operand:SI 2 "const_int_operand" "M"))
        (const_int 0))]
  "arm_arch_thumb2"
  "bfc%?\t%0, %2, %1"
  [(set_attr "length" "4")
   (set_attr "predicable" "yes")]
)

(define_insn "insv_t2"
  [(set (zero_extract:SI (match_operand:SI 0 "s_register_operand" "+r")
                         (match_operand:SI 1 "const_int_operand" "M")
                         (match_operand:SI 2 "const_int_operand" "M"))
        (match_operand:SI 3 "s_register_operand" "r"))]
  "arm_arch_thumb2"
  "bfi%?\t%0, %3, %2, %1"
  [(set_attr "length" "4")
   (set_attr "predicable" "yes")]
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
   (set_attr "predicable" "yes")]
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
   (set_attr "predicable" "yes")]
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
   (set_attr "predicable" "yes")]
)
  
(define_insn "andsi_notsi_si"
  [(set (match_operand:SI 0 "s_register_operand" "=r")
	(and:SI (not:SI (match_operand:SI 2 "s_register_operand" "r"))
		(match_operand:SI 1 "s_register_operand" "r")))]
  "TARGET_32BIT"
  "bic%?\\t%0, %1, %2"
  [(set_attr "predicable" "yes")]
)

(define_insn "thumb1_bicsi3"
  [(set (match_operand:SI                 0 "register_operand" "=l")
	(and:SI (not:SI (match_operand:SI 1 "register_operand" "l"))
		(match_operand:SI         2 "register_operand" "0")))]
  "TARGET_THUMB1"
  "bic\\t%0, %1"
  [(set_attr "length" "2")
   (set_attr "conds" "set")])

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
		      (const_string "alu_shift")
		      (const_string "alu_shift_reg")))]
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
  "bic%.\\t%0, %1, %2"
  [(set_attr "conds" "set")]
)

(define_insn "*andsi_notsi_si_compare0_scratch"
  [(set (reg:CC_NOOV CC_REGNUM)
	(compare:CC_NOOV
	 (and:SI (not:SI (match_operand:SI 2 "s_register_operand" "r"))
		 (match_operand:SI 1 "s_register_operand" "r"))
	 (const_int 0)))
   (clobber (match_scratch:SI 0 "=r"))]
  "TARGET_32BIT"
  "bic%.\\t%0, %1, %2"
  [(set_attr "conds" "set")]
)

(define_expand "iordi3"
  [(set (match_operand:DI         0 "s_register_operand" "")
	(ior:DI (match_operand:DI 1 "s_register_operand" "")
		(match_operand:DI 2 "neon_logic_op2" "")))]
  "TARGET_32BIT"
  ""
)

(define_insn "*iordi3_insn"
  [(set (match_operand:DI         0 "s_register_operand" "=&r,&r")
	(ior:DI (match_operand:DI 1 "s_register_operand"  "%0,r")
		(match_operand:DI 2 "s_register_operand"   "r,r")))]
  "TARGET_32BIT && !TARGET_IWMMXT && !TARGET_NEON"
  "#"
  [(set_attr "length" "8")
   (set_attr "predicable" "yes")]
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
   (set_attr "predicable" "yes")]
)

(define_insn "*iordi_sesidi_di"
  [(set (match_operand:DI 0 "s_register_operand" "=&r,&r")
	(ior:DI (sign_extend:DI
		 (match_operand:SI 2 "s_register_operand" "r,r"))
		(match_operand:DI 1 "s_register_operand" "0,r")))]
  "TARGET_32BIT"
  "#"
  [(set_attr "length" "8")
   (set_attr "predicable" "yes")]
)

(define_expand "iorsi3"
  [(set (match_operand:SI         0 "s_register_operand" "")
	(ior:SI (match_operand:SI 1 "s_register_operand" "")
		(match_operand:SI 2 "reg_or_int_operand" "")))]
  "TARGET_EITHER"
  "
  if (GET_CODE (operands[2]) == CONST_INT)
    {
      if (TARGET_32BIT)
        {
          arm_split_constant (IOR, SImode, NULL_RTX,
	                      INTVAL (operands[2]), operands[0], operands[1],
			      optimize && can_create_pseudo_p ());
          DONE;
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
  [(set (match_operand:SI 0 "s_register_operand" "=r,r,r")
	(ior:SI (match_operand:SI 1 "s_register_operand" "%r,r,r")
		(match_operand:SI 2 "reg_or_int_operand" "rI,K,?n")))]
  "TARGET_32BIT"
  "@
   orr%?\\t%0, %1, %2
   orn%?\\t%0, %1, #%B2
   #"
  "TARGET_32BIT
   && GET_CODE (operands[2]) == CONST_INT
   && !(const_ok_for_arm (INTVAL (operands[2]))
        || (TARGET_THUMB2 && const_ok_for_arm (~INTVAL (operands[2]))))"
  [(clobber (const_int 0))]
{
  arm_split_constant (IOR, SImode, curr_insn, 
                      INTVAL (operands[2]), operands[0], operands[1], 0);
  DONE;
}
  [(set_attr "length" "4,4,16")
   (set_attr "arch" "32,t2,32")
   (set_attr "predicable" "yes")])

(define_insn "*thumb1_iorsi3_insn"
  [(set (match_operand:SI         0 "register_operand" "=l")
	(ior:SI (match_operand:SI 1 "register_operand" "%0")
		(match_operand:SI 2 "register_operand" "l")))]
  "TARGET_THUMB1"
  "orr\\t%0, %2"
  [(set_attr "length" "2")
   (set_attr "conds" "set")])

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
	(compare:CC_NOOV (ior:SI (match_operand:SI 1 "s_register_operand" "%r")
				 (match_operand:SI 2 "arm_rhs_operand" "rI"))
			 (const_int 0)))
   (set (match_operand:SI 0 "s_register_operand" "=r")
	(ior:SI (match_dup 1) (match_dup 2)))]
  "TARGET_32BIT"
  "orr%.\\t%0, %1, %2"
  [(set_attr "conds" "set")]
)

(define_insn "*iorsi3_compare0_scratch"
  [(set (reg:CC_NOOV CC_REGNUM)
	(compare:CC_NOOV (ior:SI (match_operand:SI 1 "s_register_operand" "%r")
				 (match_operand:SI 2 "arm_rhs_operand" "rI"))
			 (const_int 0)))
   (clobber (match_scratch:SI 0 "=r"))]
  "TARGET_32BIT"
  "orr%.\\t%0, %1, %2"
  [(set_attr "conds" "set")]
)

(define_expand "xordi3"
  [(set (match_operand:DI         0 "s_register_operand" "")
	(xor:DI (match_operand:DI 1 "s_register_operand" "")
		(match_operand:DI 2 "s_register_operand" "")))]
  "TARGET_32BIT"
  ""
)

(define_insn "*xordi3_insn"
  [(set (match_operand:DI         0 "s_register_operand" "=&r,&r")
	(xor:DI (match_operand:DI 1 "s_register_operand"  "%0,r")
		(match_operand:DI 2 "s_register_operand"   "r,r")))]
  "TARGET_32BIT && !TARGET_IWMMXT && !TARGET_NEON"
  "#"
  [(set_attr "length" "8")
   (set_attr "predicable" "yes")]
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
   (set_attr "predicable" "yes")]
)

(define_insn "*xordi_sesidi_di"
  [(set (match_operand:DI 0 "s_register_operand" "=&r,&r")
	(xor:DI (sign_extend:DI
		 (match_operand:SI 2 "s_register_operand" "r,r"))
		(match_operand:DI 1 "s_register_operand" "0,r")))]
  "TARGET_32BIT"
  "#"
  [(set_attr "length" "8")
   (set_attr "predicable" "yes")]
)

(define_expand "xorsi3"
  [(set (match_operand:SI         0 "s_register_operand" "")
	(xor:SI (match_operand:SI 1 "s_register_operand" "")
		(match_operand:SI 2 "reg_or_int_operand" "")))]
  "TARGET_EITHER"
  "if (GET_CODE (operands[2]) == CONST_INT)
    {
      if (TARGET_32BIT)
        {
          arm_split_constant (XOR, SImode, NULL_RTX,
	                      INTVAL (operands[2]), operands[0], operands[1],
			      optimize && can_create_pseudo_p ());
          DONE;
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
  [(set (match_operand:SI         0 "s_register_operand" "=r,r")
	(xor:SI (match_operand:SI 1 "s_register_operand" "%r,r")
		(match_operand:SI 2 "reg_or_int_operand" "rI,?n")))]
  "TARGET_32BIT"
  "@
   eor%?\\t%0, %1, %2
   #"
  "TARGET_32BIT
   && GET_CODE (operands[2]) == CONST_INT
   && !const_ok_for_arm (INTVAL (operands[2]))"
  [(clobber (const_int 0))]
{
  arm_split_constant (XOR, SImode, curr_insn,
                      INTVAL (operands[2]), operands[0], operands[1], 0);
  DONE;
}
  [(set_attr "length" "4,16")
   (set_attr "predicable" "yes")]
)

(define_insn "*thumb1_xorsi3_insn"
  [(set (match_operand:SI         0 "register_operand" "=l")
	(xor:SI (match_operand:SI 1 "register_operand" "%0")
		(match_operand:SI 2 "register_operand" "l")))]
  "TARGET_THUMB1"
  "eor\\t%0, %2"
  [(set_attr "length" "2")
   (set_attr "conds" "set")])

(define_insn "*xorsi3_compare0"
  [(set (reg:CC_NOOV CC_REGNUM)
	(compare:CC_NOOV (xor:SI (match_operand:SI 1 "s_register_operand" "r")
				 (match_operand:SI 2 "arm_rhs_operand" "rI"))
			 (const_int 0)))
   (set (match_operand:SI 0 "s_register_operand" "=r")
	(xor:SI (match_dup 1) (match_dup 2)))]
  "TARGET_32BIT"
  "eor%.\\t%0, %1, %2"
  [(set_attr "conds" "set")]
)

(define_insn "*xorsi3_compare0_scratch"
  [(set (reg:CC_NOOV CC_REGNUM)
	(compare:CC_NOOV (xor:SI (match_operand:SI 0 "s_register_operand" "r")
				 (match_operand:SI 1 "arm_rhs_operand" "rI"))
			 (const_int 0)))]
  "TARGET_32BIT"
  "teq%?\\t%0, %1"
  [(set_attr "conds" "set")]
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

(define_insn "*andsi_iorsi3_notsi"
  [(set (match_operand:SI 0 "s_register_operand" "=&r,&r,&r")
	(and:SI (ior:SI (match_operand:SI 1 "s_register_operand" "%0,r,r")
			(match_operand:SI 2 "arm_rhs_operand" "rI,0,rI"))
		(not:SI (match_operand:SI 3 "arm_rhs_operand" "rI,rI,rI"))))]
  "TARGET_32BIT"
  "orr%?\\t%0, %1, %2\;bic%?\\t%0, %0, %3"
  [(set_attr "length" "8")
   (set_attr "ce_count" "2")
   (set_attr "predicable" "yes")]
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
      emit_insn (gen_rtx_SET (VOIDmode, operands[0],
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
  [(set_attr "predicable" "yes")]
)

(define_insn "*smax_m1"
  [(set (match_operand:SI 0 "s_register_operand" "=r")
	(smax:SI (match_operand:SI 1 "s_register_operand" "r")
		 (const_int -1)))]
  "TARGET_32BIT"
  "orr%?\\t%0, %1, %1, asr #31"
  [(set_attr "predicable" "yes")]
)

(define_insn "*arm_smax_insn"
  [(set (match_operand:SI          0 "s_register_operand" "=r,r")
	(smax:SI (match_operand:SI 1 "s_register_operand"  "%0,?r")
		 (match_operand:SI 2 "arm_rhs_operand"    "rI,rI")))
   (clobber (reg:CC CC_REGNUM))]
  "TARGET_ARM"
  "@
   cmp\\t%1, %2\;movlt\\t%0, %2
   cmp\\t%1, %2\;movge\\t%0, %1\;movlt\\t%0, %2"
  [(set_attr "conds" "clob")
   (set_attr "length" "8,12")]
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
      emit_insn (gen_rtx_SET (VOIDmode, operands[0],
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
  [(set_attr "predicable" "yes")]
)

(define_insn "*arm_smin_insn"
  [(set (match_operand:SI 0 "s_register_operand" "=r,r")
	(smin:SI (match_operand:SI 1 "s_register_operand" "%0,?r")
		 (match_operand:SI 2 "arm_rhs_operand" "rI,rI")))
   (clobber (reg:CC CC_REGNUM))]
  "TARGET_ARM"
  "@
   cmp\\t%1, %2\;movge\\t%0, %2
   cmp\\t%1, %2\;movlt\\t%0, %1\;movge\\t%0, %2"
  [(set_attr "conds" "clob")
   (set_attr "length" "8,12")]
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

(define_insn "*arm_umaxsi3"
  [(set (match_operand:SI 0 "s_register_operand" "=r,r,r")
	(umax:SI (match_operand:SI 1 "s_register_operand" "0,r,?r")
		 (match_operand:SI 2 "arm_rhs_operand" "rI,0,rI")))
   (clobber (reg:CC CC_REGNUM))]
  "TARGET_ARM"
  "@
   cmp\\t%1, %2\;movcc\\t%0, %2
   cmp\\t%1, %2\;movcs\\t%0, %1
   cmp\\t%1, %2\;movcs\\t%0, %1\;movcc\\t%0, %2"
  [(set_attr "conds" "clob")
   (set_attr "length" "8,8,12")]
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

(define_insn "*arm_uminsi3"
  [(set (match_operand:SI 0 "s_register_operand" "=r,r,r")
	(umin:SI (match_operand:SI 1 "s_register_operand" "0,r,?r")
		 (match_operand:SI 2 "arm_rhs_operand" "rI,0,rI")))
   (clobber (reg:CC CC_REGNUM))]
  "TARGET_ARM"
  "@
   cmp\\t%1, %2\;movcs\\t%0, %2
   cmp\\t%1, %2\;movcc\\t%0, %1
   cmp\\t%1, %2\;movcc\\t%0, %1\;movcs\\t%0, %2"
  [(set_attr "conds" "clob")
   (set_attr "length" "8,8,12")]
)

(define_insn "*store_minmaxsi"
  [(set (match_operand:SI 0 "memory_operand" "=m")
	(match_operator:SI 3 "minmax_operator"
	 [(match_operand:SI 1 "s_register_operand" "r")
	  (match_operand:SI 2 "s_register_operand" "r")]))
   (clobber (reg:CC CC_REGNUM))]
  "TARGET_32BIT"
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
  "TARGET_32BIT && !arm_eliminable_register (operands[1])"
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
		      (const_int 12)))]
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
   (set_attr "insn" "sat")])

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
   (set_attr "insn" "sat")
   (set_attr "shift" "3")
   (set_attr "type" "alu_shift")])

;; Shift and rotation insns

(define_expand "ashldi3"
  [(set (match_operand:DI            0 "s_register_operand" "")
        (ashift:DI (match_operand:DI 1 "s_register_operand" "")
                   (match_operand:SI 2 "reg_or_int_operand" "")))]
  "TARGET_32BIT"
  "
  if (!CONST_INT_P (operands[2]) && TARGET_REALLY_IWMMXT)
    ; /* No special preparation statements; expand pattern as above.  */
  else
    {
      rtx scratch1, scratch2;

      if (CONST_INT_P (operands[2])
	  && (HOST_WIDE_INT) INTVAL (operands[2]) == 1)
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
   (set_attr "length" "8")]
)

(define_expand "ashlsi3"
  [(set (match_operand:SI            0 "s_register_operand" "")
	(ashift:SI (match_operand:SI 1 "s_register_operand" "")
		   (match_operand:SI 2 "arm_rhs_operand" "")))]
  "TARGET_EITHER"
  "
  if (GET_CODE (operands[2]) == CONST_INT
      && ((unsigned HOST_WIDE_INT) INTVAL (operands[2])) > 31)
    {
      emit_insn (gen_movsi (operands[0], const0_rtx));
      DONE;
    }
  "
)

(define_insn "*thumb1_ashlsi3"
  [(set (match_operand:SI            0 "register_operand" "=l,l")
	(ashift:SI (match_operand:SI 1 "register_operand" "l,0")
		   (match_operand:SI 2 "nonmemory_operand" "N,l")))]
  "TARGET_THUMB1"
  "lsl\\t%0, %1, %2"
  [(set_attr "length" "2")
   (set_attr "conds" "set")])

(define_expand "ashrdi3"
  [(set (match_operand:DI              0 "s_register_operand" "")
        (ashiftrt:DI (match_operand:DI 1 "s_register_operand" "")
                     (match_operand:SI 2 "reg_or_int_operand" "")))]
  "TARGET_32BIT"
  "
  if (!CONST_INT_P (operands[2]) && TARGET_REALLY_IWMMXT)
    ; /* No special preparation statements; expand pattern as above.  */
  else
    {
      rtx scratch1, scratch2;

      if (CONST_INT_P (operands[2])
	  && (HOST_WIDE_INT) INTVAL (operands[2]) == 1)
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
   (set_attr "insn" "mov")
   (set_attr "length" "8")]
)

(define_expand "ashrsi3"
  [(set (match_operand:SI              0 "s_register_operand" "")
	(ashiftrt:SI (match_operand:SI 1 "s_register_operand" "")
		     (match_operand:SI 2 "arm_rhs_operand" "")))]
  "TARGET_EITHER"
  "
  if (GET_CODE (operands[2]) == CONST_INT
      && ((unsigned HOST_WIDE_INT) INTVAL (operands[2])) > 31)
    operands[2] = GEN_INT (31);
  "
)

(define_insn "*thumb1_ashrsi3"
  [(set (match_operand:SI              0 "register_operand" "=l,l")
	(ashiftrt:SI (match_operand:SI 1 "register_operand" "l,0")
		     (match_operand:SI 2 "nonmemory_operand" "N,l")))]
  "TARGET_THUMB1"
  "asr\\t%0, %1, %2"
  [(set_attr "length" "2")
   (set_attr "conds" "set")])

(define_expand "lshrdi3"
  [(set (match_operand:DI              0 "s_register_operand" "")
        (lshiftrt:DI (match_operand:DI 1 "s_register_operand" "")
                     (match_operand:SI 2 "reg_or_int_operand" "")))]
  "TARGET_32BIT"
  "
  if (!CONST_INT_P (operands[2]) && TARGET_REALLY_IWMMXT)
    ; /* No special preparation statements; expand pattern as above.  */
  else
    {
      rtx scratch1, scratch2;

      if (CONST_INT_P (operands[2])
	  && (HOST_WIDE_INT) INTVAL (operands[2]) == 1)
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
   (set_attr "insn" "mov")
   (set_attr "length" "8")]
)

(define_expand "lshrsi3"
  [(set (match_operand:SI              0 "s_register_operand" "")
	(lshiftrt:SI (match_operand:SI 1 "s_register_operand" "")
		     (match_operand:SI 2 "arm_rhs_operand" "")))]
  "TARGET_EITHER"
  "
  if (GET_CODE (operands[2]) == CONST_INT
      && ((unsigned HOST_WIDE_INT) INTVAL (operands[2])) > 31)
    {
      emit_insn (gen_movsi (operands[0], const0_rtx));
      DONE;
    }
  "
)

(define_insn "*thumb1_lshrsi3"
  [(set (match_operand:SI              0 "register_operand" "=l,l")
	(lshiftrt:SI (match_operand:SI 1 "register_operand" "l,0")
		     (match_operand:SI 2 "nonmemory_operand" "N,l")))]
  "TARGET_THUMB1"
  "lsr\\t%0, %1, %2"
  [(set_attr "length" "2")
   (set_attr "conds" "set")])

(define_expand "rotlsi3"
  [(set (match_operand:SI              0 "s_register_operand" "")
	(rotatert:SI (match_operand:SI 1 "s_register_operand" "")
		     (match_operand:SI 2 "reg_or_int_operand" "")))]
  "TARGET_32BIT"
  "
  if (GET_CODE (operands[2]) == CONST_INT)
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
      if (GET_CODE (operands[2]) == CONST_INT
          && ((unsigned HOST_WIDE_INT) INTVAL (operands[2])) > 31)
        operands[2] = GEN_INT (INTVAL (operands[2]) % 32);
    }
  else /* TARGET_THUMB1 */
    {
      if (GET_CODE (operands [2]) == CONST_INT)
        operands [2] = force_reg (SImode, operands[2]);
    }
  "
)

(define_insn "*thumb1_rotrsi3"
  [(set (match_operand:SI              0 "register_operand" "=l")
	(rotatert:SI (match_operand:SI 1 "register_operand" "0")
		     (match_operand:SI 2 "register_operand" "l")))]
  "TARGET_THUMB1"
  "ror\\t%0, %0, %2"
  [(set_attr "length" "2")]
)

(define_insn "*arm_shiftsi3"
  [(set (match_operand:SI   0 "s_register_operand" "=r")
	(match_operator:SI  3 "shift_operator"
	 [(match_operand:SI 1 "s_register_operand"  "r")
	  (match_operand:SI 2 "reg_or_int_operand" "rM")]))]
  "TARGET_32BIT"
  "* return arm_output_shift(operands, 0);"
  [(set_attr "predicable" "yes")
   (set_attr "shift" "1")
   (set (attr "type") (if_then_else (match_operand 2 "const_int_operand" "")
		      (const_string "alu_shift")
		      (const_string "alu_shift_reg")))]
)

(define_insn "*shiftsi3_compare0"
  [(set (reg:CC_NOOV CC_REGNUM)
	(compare:CC_NOOV (match_operator:SI 3 "shift_operator"
			  [(match_operand:SI 1 "s_register_operand" "r")
			   (match_operand:SI 2 "arm_rhs_operand" "rM")])
			 (const_int 0)))
   (set (match_operand:SI 0 "s_register_operand" "=r")
	(match_op_dup 3 [(match_dup 1) (match_dup 2)]))]
  "TARGET_32BIT"
  "* return arm_output_shift(operands, 1);"
  [(set_attr "conds" "set")
   (set_attr "shift" "1")
   (set (attr "type") (if_then_else (match_operand 2 "const_int_operand" "")
		      (const_string "alu_shift")
		      (const_string "alu_shift_reg")))]
)

(define_insn "*shiftsi3_compare0_scratch"
  [(set (reg:CC_NOOV CC_REGNUM)
	(compare:CC_NOOV (match_operator:SI 3 "shift_operator"
			  [(match_operand:SI 1 "s_register_operand" "r")
			   (match_operand:SI 2 "arm_rhs_operand" "rM")])
			 (const_int 0)))
   (clobber (match_scratch:SI 0 "=r"))]
  "TARGET_32BIT"
  "* return arm_output_shift(operands, 1);"
  [(set_attr "conds" "set")
   (set_attr "shift" "1")]
)

(define_insn "*not_shiftsi"
  [(set (match_operand:SI 0 "s_register_operand" "=r,r")
	(not:SI (match_operator:SI 3 "shift_operator"
		 [(match_operand:SI 1 "s_register_operand" "r,r")
		  (match_operand:SI 2 "shift_amount_operand" "M,rM")])))]
  "TARGET_32BIT"
  "mvn%?\\t%0, %1%S3"
  [(set_attr "predicable" "yes")
   (set_attr "shift" "1")
   (set_attr "insn" "mvn")
   (set_attr "arch" "32,a")
   (set_attr "type" "alu_shift,alu_shift_reg")])

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
  "mvn%.\\t%0, %1%S3"
  [(set_attr "conds" "set")
   (set_attr "shift" "1")
   (set_attr "insn" "mvn")
   (set_attr "arch" "32,a")
   (set_attr "type" "alu_shift,alu_shift_reg")])

(define_insn "*not_shiftsi_compare0_scratch"
  [(set (reg:CC_NOOV CC_REGNUM)
	(compare:CC_NOOV
	 (not:SI (match_operator:SI 3 "shift_operator"
		  [(match_operand:SI 1 "s_register_operand" "r,r")
		   (match_operand:SI 2 "shift_amount_operand" "M,rM")]))
	 (const_int 0)))
   (clobber (match_scratch:SI 0 "=r,r"))]
  "TARGET_32BIT"
  "mvn%.\\t%0, %1%S3"
  [(set_attr "conds" "set")
   (set_attr "shift" "1")
   (set_attr "insn" "mvn")
   (set_attr "arch" "32,a")
   (set_attr "type" "alu_shift,alu_shift_reg")])

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
  "unaligned_access && TARGET_32BIT"
  "ldr%?\t%0, %1\t@ unaligned"
  [(set_attr "arch" "t2,any")
   (set_attr "length" "2,4")
   (set_attr "predicable" "yes")
   (set_attr "type" "load1")])

(define_insn "unaligned_loadhis"
  [(set (match_operand:SI 0 "s_register_operand" "=l,r")
	(sign_extend:SI
	  (unspec:HI [(match_operand:HI 1 "memory_operand" "Uw,m")]
		     UNSPEC_UNALIGNED_LOAD)))]
  "unaligned_access && TARGET_32BIT"
  "ldr%(sh%)\t%0, %1\t@ unaligned"
  [(set_attr "arch" "t2,any")
   (set_attr "length" "2,4")
   (set_attr "predicable" "yes")
   (set_attr "type" "load_byte")])

(define_insn "unaligned_loadhiu"
  [(set (match_operand:SI 0 "s_register_operand" "=l,r")
	(zero_extend:SI
	  (unspec:HI [(match_operand:HI 1 "memory_operand" "Uw,m")]
		     UNSPEC_UNALIGNED_LOAD)))]
  "unaligned_access && TARGET_32BIT"
  "ldr%(h%)\t%0, %1\t@ unaligned"
  [(set_attr "arch" "t2,any")
   (set_attr "length" "2,4")
   (set_attr "predicable" "yes")
   (set_attr "type" "load_byte")])

(define_insn "unaligned_storesi"
  [(set (match_operand:SI 0 "memory_operand" "=Uw,m")
	(unspec:SI [(match_operand:SI 1 "s_register_operand" "l,r")]
		   UNSPEC_UNALIGNED_STORE))]
  "unaligned_access && TARGET_32BIT"
  "str%?\t%1, %0\t@ unaligned"
  [(set_attr "arch" "t2,any")
   (set_attr "length" "2,4")
   (set_attr "predicable" "yes")
   (set_attr "type" "store1")])

(define_insn "unaligned_storehi"
  [(set (match_operand:HI 0 "memory_operand" "=Uw,m")
	(unspec:HI [(match_operand:HI 1 "s_register_operand" "l,r")]
		   UNSPEC_UNALIGNED_STORE))]
  "unaligned_access && TARGET_32BIT"
  "str%(h%)\t%1, %0\t@ unaligned"
  [(set_attr "arch" "t2,any")
   (set_attr "length" "2,4")
   (set_attr "predicable" "yes")
   (set_attr "type" "store1")])

(define_insn "*extv_reg"
  [(set (match_operand:SI 0 "s_register_operand" "=r")
	(sign_extract:SI (match_operand:SI 1 "s_register_operand" "r")
                         (match_operand:SI 2 "const_int_operand" "M")
                         (match_operand:SI 3 "const_int_operand" "M")))]
  "arm_arch_thumb2"
  "sbfx%?\t%0, %1, %3, %2"
  [(set_attr "length" "4")
   (set_attr "predicable" "yes")]
)

(define_insn "extzv_t2"
  [(set (match_operand:SI 0 "s_register_operand" "=r")
	(zero_extract:SI (match_operand:SI 1 "s_register_operand" "r")
                         (match_operand:SI 2 "const_int_operand" "M")
                         (match_operand:SI 3 "const_int_operand" "M")))]
  "arm_arch_thumb2"
  "ubfx%?\t%0, %1, %3, %2"
  [(set_attr "length" "4")
   (set_attr "predicable" "yes")]
)


;; Division instructions
(define_insn "divsi3"
  [(set (match_operand:SI	  0 "s_register_operand" "=r")
	(div:SI (match_operand:SI 1 "s_register_operand"  "r")
		(match_operand:SI 2 "s_register_operand"  "r")))]
  "TARGET_IDIV"
  "sdiv%?\t%0, %1, %2"
  [(set_attr "predicable" "yes")
   (set_attr "insn" "sdiv")]
)

(define_insn "udivsi3"
  [(set (match_operand:SI	   0 "s_register_operand" "=r")
	(udiv:SI (match_operand:SI 1 "s_register_operand"  "r")
		 (match_operand:SI 2 "s_register_operand"  "r")))]
  "TARGET_IDIV"
  "udiv%?\t%0, %1, %2"
  [(set_attr "predicable" "yes")
   (set_attr "insn" "udiv")]
)


;; Unary arithmetic insns

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
(define_insn "*arm_negdi2"
  [(set (match_operand:DI         0 "s_register_operand" "=r,&r")
	(neg:DI (match_operand:DI 1 "s_register_operand"  "0,r")))
   (clobber (reg:CC CC_REGNUM))]
  "TARGET_ARM"
  "rsbs\\t%Q0, %Q1, #0\;rsc\\t%R0, %R1, #0"
  [(set_attr "conds" "clob")
   (set_attr "length" "8")]
)

(define_insn "*thumb1_negdi2"
  [(set (match_operand:DI 0 "register_operand" "=&l")
	(neg:DI (match_operand:DI 1 "register_operand" "l")))
   (clobber (reg:CC CC_REGNUM))]
  "TARGET_THUMB1"
  "mov\\t%R0, #0\;neg\\t%Q0, %Q1\;sbc\\t%R0, %R1"
  [(set_attr "length" "6")]
)

(define_expand "negsi2"
  [(set (match_operand:SI         0 "s_register_operand" "")
	(neg:SI (match_operand:SI 1 "s_register_operand" "")))]
  "TARGET_EITHER"
  ""
)

(define_insn "*arm_negsi2"
  [(set (match_operand:SI         0 "s_register_operand" "=r")
	(neg:SI (match_operand:SI 1 "s_register_operand" "r")))]
  "TARGET_32BIT"
  "rsb%?\\t%0, %1, #0"
  [(set_attr "predicable" "yes")]
)

(define_insn "*thumb1_negsi2"
  [(set (match_operand:SI         0 "register_operand" "=l")
	(neg:SI (match_operand:SI 1 "register_operand" "l")))]
  "TARGET_THUMB1"
  "neg\\t%0, %1"
  [(set_attr "length" "2")]
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

(define_insn "*arm_abssi2"
  [(set (match_operand:SI 0 "s_register_operand" "=r,&r")
	(abs:SI (match_operand:SI 1 "s_register_operand" "0,r")))
   (clobber (reg:CC CC_REGNUM))]
  "TARGET_ARM"
  "@
   cmp\\t%0, #0\;rsblt\\t%0, %0, #0
   eor%?\\t%0, %1, %1, asr #31\;sub%?\\t%0, %0, %1, asr #31"
  [(set_attr "conds" "clob,*")
   (set_attr "shift" "1")
   ;; predicable can't be set based on the variant, so left as no
   (set_attr "length" "8")]
)

(define_insn_and_split "*thumb1_abssi2"
  [(set (match_operand:SI 0 "s_register_operand" "=l")
	(abs:SI (match_operand:SI 1 "s_register_operand" "l")))
   (clobber (match_scratch:SI 2 "=&l"))]
  "TARGET_THUMB1"
  "#"
  "TARGET_THUMB1 && reload_completed"
  [(set (match_dup 2) (ashiftrt:SI (match_dup 1) (const_int 31)))
   (set (match_dup 0) (plus:SI (match_dup 1) (match_dup 2)))
   (set (match_dup 0) (xor:SI (match_dup 0) (match_dup 2)))]
  ""
  [(set_attr "length" "6")]
)

(define_insn "*arm_neg_abssi2"
  [(set (match_operand:SI 0 "s_register_operand" "=r,&r")
	(neg:SI (abs:SI (match_operand:SI 1 "s_register_operand" "0,r"))))
   (clobber (reg:CC CC_REGNUM))]
  "TARGET_ARM"
  "@
   cmp\\t%0, #0\;rsbgt\\t%0, %0, #0
   eor%?\\t%0, %1, %1, asr #31\;rsb%?\\t%0, %0, %1, asr #31"
  [(set_attr "conds" "clob,*")
   (set_attr "shift" "1")
   ;; predicable can't be set based on the variant, so left as no
   (set_attr "length" "8")]
)

(define_insn_and_split "*thumb1_neg_abssi2"
  [(set (match_operand:SI 0 "s_register_operand" "=l")
	(neg:SI (abs:SI (match_operand:SI 1 "s_register_operand" "l"))))
   (clobber (match_scratch:SI 2 "=&l"))]
  "TARGET_THUMB1"
  "#"
  "TARGET_THUMB1 && reload_completed"
  [(set (match_dup 2) (ashiftrt:SI (match_dup 1) (const_int 31)))
   (set (match_dup 0) (minus:SI (match_dup 2) (match_dup 1)))
   (set (match_dup 0) (xor:SI (match_dup 0) (match_dup 2)))]
  ""
  [(set_attr "length" "6")]
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
   (set_attr "neon_type" "neon_int_1,*,*,neon_int_1")
   (set_attr "arch" "neon_nota8,*,*,neon_onlya8")]
)

(define_expand "one_cmplsi2"
  [(set (match_operand:SI         0 "s_register_operand" "")
	(not:SI (match_operand:SI 1 "s_register_operand" "")))]
  "TARGET_EITHER"
  ""
)

(define_insn "*arm_one_cmplsi2"
  [(set (match_operand:SI         0 "s_register_operand" "=r")
	(not:SI (match_operand:SI 1 "s_register_operand"  "r")))]
  "TARGET_32BIT"
  "mvn%?\\t%0, %1"
  [(set_attr "predicable" "yes")
   (set_attr "insn" "mvn")]
)

(define_insn "*thumb1_one_cmplsi2"
  [(set (match_operand:SI         0 "register_operand" "=l")
	(not:SI (match_operand:SI 1 "register_operand"  "l")))]
  "TARGET_THUMB1"
  "mvn\\t%0, %1"
  [(set_attr "length" "2")
   (set_attr "insn" "mvn")]
)

(define_insn "*notsi_compare0"
  [(set (reg:CC_NOOV CC_REGNUM)
	(compare:CC_NOOV (not:SI (match_operand:SI 1 "s_register_operand" "r"))
			 (const_int 0)))
   (set (match_operand:SI 0 "s_register_operand" "=r")
	(not:SI (match_dup 1)))]
  "TARGET_32BIT"
  "mvn%.\\t%0, %1"
  [(set_attr "conds" "set")
   (set_attr "insn" "mvn")]
)

(define_insn "*notsi_compare0_scratch"
  [(set (reg:CC_NOOV CC_REGNUM)
	(compare:CC_NOOV (not:SI (match_operand:SI 1 "s_register_operand" "r"))
			 (const_int 0)))
   (clobber (match_scratch:SI 0 "=r"))]
  "TARGET_32BIT"
  "mvn%.\\t%0, %1"
  [(set_attr "conds" "set")
   (set_attr "insn" "mvn")]
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
  [(set (match_operand:DI 0 "s_register_operand" "=r")
        (zero_extend:DI (match_operand:QHSI 1 "<qhs_zextenddi_op>"
					    "<qhs_zextenddi_cstr>")))]
  "TARGET_32BIT <qhs_zextenddi_cond>"
  "#"
  [(set_attr "length" "8")
   (set_attr "ce_count" "2")
   (set_attr "predicable" "yes")]
)

(define_insn "extend<mode>di2"
  [(set (match_operand:DI 0 "s_register_operand" "=r")
        (sign_extend:DI (match_operand:QHSI 1 "<qhs_extenddi_op>"
					    "<qhs_extenddi_cstr>")))]
  "TARGET_32BIT <qhs_sextenddi_cond>"
  "#"
  [(set_attr "length" "8")
   (set_attr "ce_count" "2")
   (set_attr "shift" "1")
   (set_attr "predicable" "yes")]
)

;; Splits for all extensions to DImode
(define_split
  [(set (match_operand:DI 0 "s_register_operand" "")
        (zero_extend:DI (match_operand 1 "nonimmediate_operand" "")))]
  "TARGET_32BIT"
  [(set (match_dup 0) (match_dup 1))]
{
  rtx lo_part = gen_lowpart (SImode, operands[0]);
  enum machine_mode src_mode = GET_MODE (operands[1]);

  if (REG_P (operands[0])
      && !reg_overlap_mentioned_p (operands[0], operands[1]))
    emit_clobber (operands[0]);
  if (!REG_P (lo_part) || src_mode != SImode
      || !rtx_equal_p (lo_part, operands[1]))
    {
      if (src_mode == SImode)
        emit_move_insn (lo_part, operands[1]);
      else
        emit_insn (gen_rtx_SET (VOIDmode, lo_part,
				gen_rtx_ZERO_EXTEND (SImode, operands[1])));
      operands[1] = lo_part;
    }
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
  enum machine_mode src_mode = GET_MODE (operands[1]);

  if (REG_P (operands[0])
      && !reg_overlap_mentioned_p (operands[0], operands[1]))
    emit_clobber (operands[0]);

  if (!REG_P (lo_part) || src_mode != SImode
      || !rtx_equal_p (lo_part, operands[1]))
    {
      if (src_mode == SImode)
        emit_move_insn (lo_part, operands[1]);
      else
        emit_insn (gen_rtx_SET (VOIDmode, lo_part,
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

(define_insn "*thumb1_zero_extendhisi2"
  [(set (match_operand:SI 0 "register_operand" "=l,l")
	(zero_extend:SI (match_operand:HI 1 "nonimmediate_operand" "l,m")))]
  "TARGET_THUMB1"
{
  rtx mem;

  if (which_alternative == 0 && arm_arch6)
    return "uxth\t%0, %1";
  if (which_alternative == 0)
    return "#";

  mem = XEXP (operands[1], 0);

  if (GET_CODE (mem) == CONST)
    mem = XEXP (mem, 0);
    
  if (GET_CODE (mem) == PLUS)
    {
      rtx a = XEXP (mem, 0);

      /* This can happen due to bugs in reload.  */
      if (GET_CODE (a) == REG && REGNO (a) == SP_REGNUM)
        {
          rtx ops[2];
          ops[0] = operands[0];
          ops[1] = a;
      
          output_asm_insn ("mov\t%0, %1", ops);

          XEXP (mem, 0) = operands[0];
       }
    }
    
  return "ldrh\t%0, %1";
}
  [(set_attr_alternative "length"
			 [(if_then_else (eq_attr "is_arch6" "yes")
				       (const_int 2) (const_int 4))
			 (const_int 4)])
   (set_attr "type" "alu_shift,load_byte")]
)

(define_insn "*arm_zero_extendhisi2"
  [(set (match_operand:SI 0 "s_register_operand" "=r,r")
	(zero_extend:SI (match_operand:HI 1 "nonimmediate_operand" "r,m")))]
  "TARGET_ARM && arm_arch4 && !arm_arch6"
  "@
   #
   ldr%(h%)\\t%0, %1"
  [(set_attr "type" "alu_shift,load_byte")
   (set_attr "predicable" "yes")]
)

(define_insn "*arm_zero_extendhisi2_v6"
  [(set (match_operand:SI 0 "s_register_operand" "=r,r")
	(zero_extend:SI (match_operand:HI 1 "nonimmediate_operand" "r,m")))]
  "TARGET_ARM && arm_arch6"
  "@
   uxth%?\\t%0, %1
   ldr%(h%)\\t%0, %1"
  [(set_attr "type" "alu_shift,load_byte")
   (set_attr "predicable" "yes")]
)

(define_insn "*arm_zero_extendhisi2addsi"
  [(set (match_operand:SI 0 "s_register_operand" "=r")
	(plus:SI (zero_extend:SI (match_operand:HI 1 "s_register_operand" "r"))
		 (match_operand:SI 2 "s_register_operand" "r")))]
  "TARGET_INT_SIMD"
  "uxtah%?\\t%0, %2, %1"
  [(set_attr "type" "alu_shift")
   (set_attr "predicable" "yes")]
)

(define_expand "zero_extendqisi2"
  [(set (match_operand:SI 0 "s_register_operand" "")
	(zero_extend:SI (match_operand:QI 1 "nonimmediate_operand" "")))]
  "TARGET_EITHER"
{
  if (TARGET_ARM && !arm_arch6 && GET_CODE (operands[1]) != MEM)
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

(define_insn "*thumb1_zero_extendqisi2"
  [(set (match_operand:SI 0 "register_operand" "=l,l")
	(zero_extend:SI (match_operand:QI 1 "nonimmediate_operand" "l,m")))]
  "TARGET_THUMB1 && !arm_arch6"
  "@
   #
   ldrb\\t%0, %1"
  [(set_attr "length" "4,2")
   (set_attr "type" "alu_shift,load_byte")
   (set_attr "pool_range" "*,32")]
)

(define_insn "*thumb1_zero_extendqisi2_v6"
  [(set (match_operand:SI 0 "register_operand" "=l,l")
	(zero_extend:SI (match_operand:QI 1 "nonimmediate_operand" "l,m")))]
  "TARGET_THUMB1 && arm_arch6"
  "@
   uxtb\\t%0, %1
   ldrb\\t%0, %1"
  [(set_attr "length" "2")
   (set_attr "type" "alu_shift,load_byte")]
)

(define_insn "*arm_zero_extendqisi2"
  [(set (match_operand:SI 0 "s_register_operand" "=r,r")
	(zero_extend:SI (match_operand:QI 1 "nonimmediate_operand" "r,m")))]
  "TARGET_ARM && !arm_arch6"
  "@
   #
   ldr%(b%)\\t%0, %1\\t%@ zero_extendqisi2"
  [(set_attr "length" "8,4")
   (set_attr "type" "alu_shift,load_byte")
   (set_attr "predicable" "yes")]
)

(define_insn "*arm_zero_extendqisi2_v6"
  [(set (match_operand:SI 0 "s_register_operand" "=r,r")
	(zero_extend:SI (match_operand:QI 1 "nonimmediate_operand" "r,m")))]
  "TARGET_ARM && arm_arch6"
  "@
   uxtb%(%)\\t%0, %1
   ldr%(b%)\\t%0, %1\\t%@ zero_extendqisi2"
  [(set_attr "type" "alu_shift,load_byte")
   (set_attr "predicable" "yes")]
)

(define_insn "*arm_zero_extendqisi2addsi"
  [(set (match_operand:SI 0 "s_register_operand" "=r")
	(plus:SI (zero_extend:SI (match_operand:QI 1 "s_register_operand" "r"))
		 (match_operand:SI 2 "s_register_operand" "r")))]
  "TARGET_INT_SIMD"
  "uxtab%?\\t%0, %2, %1"
  [(set_attr "predicable" "yes")
   (set_attr "insn" "xtab")
   (set_attr "type" "alu_shift")]
)

(define_split
  [(set (match_operand:SI 0 "s_register_operand" "")
	(zero_extend:SI (subreg:QI (match_operand:SI 1 "" "") 0)))
   (clobber (match_operand:SI 2 "s_register_operand" ""))]
  "TARGET_32BIT && (GET_CODE (operands[1]) != MEM) && ! BYTES_BIG_ENDIAN"
  [(set (match_dup 2) (match_dup 1))
   (set (match_dup 0) (and:SI (match_dup 2) (const_int 255)))]
  ""
)

(define_split
  [(set (match_operand:SI 0 "s_register_operand" "")
	(zero_extend:SI (subreg:QI (match_operand:SI 1 "" "") 3)))
   (clobber (match_operand:SI 2 "s_register_operand" ""))]
  "TARGET_32BIT && (GET_CODE (operands[1]) != MEM) && BYTES_BIG_ENDIAN"
  [(set (match_dup 2) (match_dup 1))
   (set (match_dup 0) (and:SI (match_dup 2) (const_int 255)))]
  ""
)


(define_split
  [(set (match_operand:SI 0 "s_register_operand" "")
	(ior_xor:SI (and:SI (ashift:SI
			     (match_operand:SI 1 "s_register_operand" "")
			     (match_operand:SI 2 "const_int_operand" ""))
			    (match_operand:SI 3 "const_int_operand" ""))
		    (zero_extend:SI
		     (match_operator 5 "subreg_lowpart_operator"
		      [(match_operand:SI 4 "s_register_operand" "")]))))]
  "TARGET_32BIT
   && ((unsigned HOST_WIDE_INT) INTVAL (operands[3])
       == (GET_MODE_MASK (GET_MODE (operands[5]))
           & (GET_MODE_MASK (GET_MODE (operands[5]))
	      << (INTVAL (operands[2])))))"
  [(set (match_dup 0) (ior_xor:SI (ashift:SI (match_dup 1) (match_dup 2))
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
   (set_attr "predicable" "yes")]
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

;; We used to have an early-clobber on the scratch register here.
;; However, there's a bug somewhere in reload which means that this
;; can be partially ignored during spill allocation if the memory
;; address also needs reloading; this causes us to die later on when
;; we try to verify the operands.  Fortunately, we don't really need
;; the early-clobber: we can always use operand 0 if operand 2
;; overlaps the address.
(define_insn "thumb1_extendhisi2"
  [(set (match_operand:SI 0 "register_operand" "=l,l")
	(sign_extend:SI (match_operand:HI 1 "nonimmediate_operand" "l,m")))
   (clobber (match_scratch:SI 2 "=X,l"))]
  "TARGET_THUMB1"
  "*
  {
    rtx ops[4];
    rtx mem;

    if (which_alternative == 0 && !arm_arch6)
      return \"#\";
    if (which_alternative == 0)
      return \"sxth\\t%0, %1\";

    mem = XEXP (operands[1], 0);

    /* This code used to try to use 'V', and fix the address only if it was
       offsettable, but this fails for e.g. REG+48 because 48 is outside the
       range of QImode offsets, and offsettable_address_p does a QImode
       address check.  */
       
    if (GET_CODE (mem) == CONST)
      mem = XEXP (mem, 0);
    
    if (GET_CODE (mem) == LABEL_REF)
      return \"ldr\\t%0, %1\";
    
    if (GET_CODE (mem) == PLUS)
      {
        rtx a = XEXP (mem, 0);
        rtx b = XEXP (mem, 1);

        if (GET_CODE (a) == LABEL_REF
	    && GET_CODE (b) == CONST_INT)
          return \"ldr\\t%0, %1\";

        if (GET_CODE (b) == REG)
          return \"ldrsh\\t%0, %1\";
	  
        ops[1] = a;
        ops[2] = b;
      }
    else
      {
        ops[1] = mem;
        ops[2] = const0_rtx;
      }
      
    gcc_assert (GET_CODE (ops[1]) == REG);

    ops[0] = operands[0];
    if (reg_mentioned_p (operands[2], ops[1]))
      ops[3] = ops[0];
    else
      ops[3] = operands[2];
    output_asm_insn (\"mov\\t%3, %2\;ldrsh\\t%0, [%1, %3]\", ops);
    return \"\";
  }"
  [(set_attr_alternative "length"
			 [(if_then_else (eq_attr "is_arch6" "yes")
					(const_int 2) (const_int 4))
			  (const_int 4)])
   (set_attr "type" "alu_shift,load_byte")
   (set_attr "pool_range" "*,1020")]
)

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
	(sign_extend:SI (match_operand:HI 1 "nonimmediate_operand" "r,m")))]
  "TARGET_ARM && arm_arch4 && !arm_arch6"
  "@
   #
   ldr%(sh%)\\t%0, %1"
  [(set_attr "length" "8,4")
   (set_attr "type" "alu_shift,load_byte")
   (set_attr "predicable" "yes")
   (set_attr "pool_range" "*,256")
   (set_attr "neg_pool_range" "*,244")]
)

;; ??? Check Thumb-2 pool range
(define_insn "*arm_extendhisi2_v6"
  [(set (match_operand:SI 0 "s_register_operand" "=r,r")
	(sign_extend:SI (match_operand:HI 1 "nonimmediate_operand" "r,m")))]
  "TARGET_32BIT && arm_arch6"
  "@
   sxth%?\\t%0, %1
   ldr%(sh%)\\t%0, %1"
  [(set_attr "type" "alu_shift,load_byte")
   (set_attr "predicable" "yes")
   (set_attr "pool_range" "*,256")
   (set_attr "neg_pool_range" "*,244")]
)

(define_insn "*arm_extendhisi2addsi"
  [(set (match_operand:SI 0 "s_register_operand" "=r")
	(plus:SI (sign_extend:SI (match_operand:HI 1 "s_register_operand" "r"))
		 (match_operand:SI 2 "s_register_operand" "r")))]
  "TARGET_INT_SIMD"
  "sxtah%?\\t%0, %2, %1"
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
    if (arm_arch4 && GET_CODE (operands[1]) == MEM)
      {
	emit_insn (gen_rtx_SET (VOIDmode,
				operands[0],
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
  "ldr%(sb%)\\t%0, %1"
  [(set_attr "type" "load_byte")
   (set_attr "predicable" "yes")
   (set_attr "pool_range" "256")
   (set_attr "neg_pool_range" "244")]
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
   ldr%(sb%)\\t%0, %1"
  [(set_attr "length" "8,4")
   (set_attr "type" "alu_shift,load_byte")
   (set_attr "predicable" "yes")
   (set_attr "pool_range" "*,256")
   (set_attr "neg_pool_range" "*,244")]
)

(define_insn "*arm_extendqisi_v6"
  [(set (match_operand:SI 0 "s_register_operand" "=r,r")
	(sign_extend:SI
	 (match_operand:QI 1 "arm_reg_or_extendqisi_mem_op" "r,Uq")))]
  "TARGET_ARM && arm_arch6"
  "@
   sxtb%?\\t%0, %1
   ldr%(sb%)\\t%0, %1"
  [(set_attr "type" "alu_shift,load_byte")
   (set_attr "predicable" "yes")
   (set_attr "pool_range" "*,256")
   (set_attr "neg_pool_range" "*,244")]
)

(define_insn "*arm_extendqisi2addsi"
  [(set (match_operand:SI 0 "s_register_operand" "=r")
	(plus:SI (sign_extend:SI (match_operand:QI 1 "s_register_operand" "r"))
		 (match_operand:SI 2 "s_register_operand" "r")))]
  "TARGET_INT_SIMD"
  "sxtab%?\\t%0, %2, %1"
  [(set_attr "type" "alu_shift")
   (set_attr "insn" "xtab")
   (set_attr "predicable" "yes")]
)

(define_split
  [(set (match_operand:SI 0 "register_operand" "")
	(sign_extend:SI (match_operand:QI 1 "memory_operand" "")))]
  "TARGET_THUMB1 && reload_completed"
  [(set (match_dup 0) (match_dup 2))
   (set (match_dup 0) (sign_extend:SI (match_dup 3)))]
{
  rtx addr = XEXP (operands[1], 0);

  if (GET_CODE (addr) == CONST)
    addr = XEXP (addr, 0);

  if (GET_CODE (addr) == PLUS
      && REG_P (XEXP (addr, 0)) && REG_P (XEXP (addr, 1)))
    /* No split necessary.  */
    FAIL;

  if (GET_CODE (addr) == PLUS
      && !REG_P (XEXP (addr, 0)) && !REG_P (XEXP (addr, 1)))
    FAIL;

  if (reg_overlap_mentioned_p (operands[0], addr))
    {
      rtx t = gen_lowpart (QImode, operands[0]);
      emit_move_insn (t, operands[1]);
      emit_insn (gen_thumb1_extendqisi2 (operands[0], t));
      DONE;
    }

  if (REG_P (addr))
    {
      addr = gen_rtx_PLUS (Pmode, addr, operands[0]);
      operands[2] = const0_rtx;
    }
  else if (GET_CODE (addr) != PLUS)
    FAIL;
  else if (REG_P (XEXP (addr, 0)))
    {
      operands[2] = XEXP (addr, 1);
      addr = gen_rtx_PLUS (Pmode, XEXP (addr, 0), operands[0]);
    }
  else
    {
      operands[2] = XEXP (addr, 0);
      addr = gen_rtx_PLUS (Pmode, XEXP (addr, 1), operands[0]);
    }

  operands[3] = change_address (operands[1], QImode, addr);
})

(define_peephole2
  [(set (match_operand:SI 0 "register_operand" "")
	(plus:SI (match_dup 0) (match_operand 1 "const_int_operand")))
   (set (match_operand:SI 2 "register_operand" "") (const_int 0))
   (set (match_operand:SI 3 "register_operand" "")
	(sign_extend:SI (match_operand:QI 4 "memory_operand" "")))]
  "TARGET_THUMB1
   && GET_CODE (XEXP (operands[4], 0)) == PLUS
   && rtx_equal_p (operands[0], XEXP (XEXP (operands[4], 0), 0))
   && rtx_equal_p (operands[2], XEXP (XEXP (operands[4], 0), 1))
   && (peep2_reg_dead_p (3, operands[0])
       || rtx_equal_p (operands[0], operands[3]))
   && (peep2_reg_dead_p (3, operands[2])
       || rtx_equal_p (operands[2], operands[3]))"
  [(set (match_dup 2) (match_dup 1))
   (set (match_dup 3) (sign_extend:SI (match_dup 4)))]
{
  rtx addr = gen_rtx_PLUS (Pmode, operands[0], operands[2]);
  operands[4] = change_address (operands[4], QImode, addr);
})

(define_insn "thumb1_extendqisi2"
  [(set (match_operand:SI 0 "register_operand" "=l,l,l")
	(sign_extend:SI (match_operand:QI 1 "nonimmediate_operand" "l,V,m")))]
  "TARGET_THUMB1"
{
  rtx addr;

  if (which_alternative == 0 && arm_arch6)
    return "sxtb\\t%0, %1";
  if (which_alternative == 0)
    return "#";

  addr = XEXP (operands[1], 0);
  if (GET_CODE (addr) == PLUS
      && REG_P (XEXP (addr, 0)) && REG_P (XEXP (addr, 1)))
    return "ldrsb\\t%0, %1";
      
  return "#";
}
  [(set_attr_alternative "length"
			 [(if_then_else (eq_attr "is_arch6" "yes")
					(const_int 2) (const_int 4))
			  (const_int 2)
			  (if_then_else (eq_attr "is_arch6" "yes")
					(const_int 4) (const_int 6))])
   (set_attr "type" "alu_shift,load_byte,load_byte")]
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
;;  if (GET_CODE (operands[0]) == MEM && GET_CODE (operands[1]) == MEM)
;;    operands[1] = copy_to_reg (operands[1]);
;;  if (GET_CODE (operands[0]) == MEM)
;;    insn = gen_storeti (XEXP (operands[0], 0), operands[1]);
;;  else if (GET_CODE (operands[1]) == MEM)
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
      if (GET_CODE (operands[0]) != REG)
	operands[1] = force_reg (DImode, operands[1]);
    }
  "
)

(define_insn "*arm_movdi"
  [(set (match_operand:DI 0 "nonimmediate_di_operand" "=r, r, r, r, m")
	(match_operand:DI 1 "di_operand"              "rDa,Db,Dc,mi,r"))]
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
   (set_attr "type" "*,*,*,load2,store2")
   (set_attr "arm_pool_range" "*,*,*,1020,*")
   (set_attr "arm_neg_pool_range" "*,*,*,1004,*")
   (set_attr "thumb2_pool_range" "*,*,*,4096,*")
   (set_attr "thumb2_neg_pool_range" "*,*,*,0,*")]
)

(define_split
  [(set (match_operand:ANY64 0 "arm_general_register_operand" "")
	(match_operand:ANY64 1 "const_double_operand" ""))]
  "TARGET_32BIT
   && reload_completed
   && (arm_const_double_inline_cost (operands[1])
       <= ((optimize_size || arm_ld_sched) ? 3 : 4))"
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

;;; ??? This should have alternatives for constants.
;;; ??? This was originally identical to the movdf_insn pattern.
;;; ??? The 'i' constraint looks funny, but it should always be replaced by
;;; thumb_reorg with a memory reference.
(define_insn "*thumb1_movdi_insn"
  [(set (match_operand:DI 0 "nonimmediate_operand" "=l,l,l,l,>,l, m,*r")
	(match_operand:DI 1 "general_operand"      "l, I,J,>,l,mi,l,*r"))]
  "TARGET_THUMB1
   && (   register_operand (operands[0], DImode)
       || register_operand (operands[1], DImode))"
  "*
  {
  switch (which_alternative)
    {
    default:
    case 0:
      if (REGNO (operands[1]) == REGNO (operands[0]) + 1)
	return \"add\\t%0,  %1,  #0\;add\\t%H0, %H1, #0\";
      return   \"add\\t%H0, %H1, #0\;add\\t%0,  %1,  #0\";
    case 1:
      return \"mov\\t%Q0, %1\;mov\\t%R0, #0\";
    case 2:
      operands[1] = GEN_INT (- INTVAL (operands[1]));
      return \"mov\\t%Q0, %1\;neg\\t%Q0, %Q0\;asr\\t%R0, %Q0, #31\";
    case 3:
      return \"ldmia\\t%1, {%0, %H0}\";
    case 4:
      return \"stmia\\t%0, {%1, %H1}\";
    case 5:
      return thumb_load_double_from_address (operands);
    case 6:
      operands[2] = gen_rtx_MEM (SImode,
			     plus_constant (Pmode, XEXP (operands[0], 0), 4));
      output_asm_insn (\"str\\t%1, %0\;str\\t%H1, %2\", operands);
      return \"\";
    case 7:
      if (REGNO (operands[1]) == REGNO (operands[0]) + 1)
	return \"mov\\t%0, %1\;mov\\t%H0, %H1\";
      return \"mov\\t%H0, %H1\;mov\\t%0, %1\";
    }
  }"
  [(set_attr "length" "4,4,6,2,2,6,4,4")
   (set_attr "type" "*,*,*,load2,store2,load2,store2,*")
   (set_attr "insn" "*,mov,*,*,*,*,*,mov")
   (set_attr "pool_range" "*,*,*,*,*,1020,*,*")]
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
      if (GET_CODE (operands[0]) == MEM)
        operands[1] = force_reg (SImode, operands[1]);
      if (arm_general_register_operand (operands[0], SImode)
	  && GET_CODE (operands[1]) == CONST_INT
          && !(const_ok_for_arm (INTVAL (operands[1]))
               || const_ok_for_arm (~INTVAL (operands[1]))))
        {
           arm_split_constant (SET, SImode, NULL_RTX,
	                       INTVAL (operands[1]), operands[0], NULL_RTX,
			       optimize && can_create_pseudo_p ());
          DONE;
        }
    }
  else /* TARGET_THUMB1...  */
    {
      if (can_create_pseudo_p ())
        {
          if (GET_CODE (operands[0]) != REG)
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
  [(set (match_operand:SI 0 "nonimmediate_operand" "=r")
	(lo_sum:SI (match_operand:SI 1 "nonimmediate_operand" "0")
		   (match_operand:SI 2 "general_operand"      "i")))]
  "arm_arch_thumb2"
  "movt%?\t%0, #:upper16:%c2"
  [(set_attr "predicable" "yes")
   (set_attr "length" "4")]
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
  [(set_attr "type" "*,*,*,*,load1,store1")
   (set_attr "insn" "mov,mov,mvn,mov,*,*")
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

;; Split symbol_refs at the later stage (after cprop), instead of generating
;; movt/movw pair directly at expand.  Otherwise corresponding high_sum
;; and lo_sum would be merged back into memory load at cprop.  However,
;; if the default is to prefer movt/movw rather than a load from the constant
;; pool, the performance is better.
(define_split
  [(set (match_operand:SI 0 "arm_general_register_operand" "")
       (match_operand:SI 1 "general_operand" ""))]
  "TARGET_32BIT
   && TARGET_USE_MOVT && GET_CODE (operands[1]) == SYMBOL_REF
   && !flag_pic && !target_word_relocations
   && !arm_tls_referenced_p (operands[1])"
  [(clobber (const_int 0))]
{
  arm_emit_movpair (operands[0], operands[1]);
  DONE;
})

(define_insn "*thumb1_movsi_insn"
  [(set (match_operand:SI 0 "nonimmediate_operand" "=l,l,l,l,l,>,l, m,*l*h*k")
	(match_operand:SI 1 "general_operand"      "l, I,J,K,>,l,mi,l,*l*h*k"))]
  "TARGET_THUMB1
   && (   register_operand (operands[0], SImode) 
       || register_operand (operands[1], SImode))"
  "@
   mov	%0, %1
   mov	%0, %1
   #
   #
   ldmia\\t%1, {%0}
   stmia\\t%0, {%1}
   ldr\\t%0, %1
   str\\t%1, %0
   mov\\t%0, %1"
  [(set_attr "length" "2,2,4,4,2,2,2,2,2")
   (set_attr "type" "*,*,*,*,load1,store1,load1,store1,*")
   (set_attr "pool_range" "*,*,*,*,*,*,1020,*,*")
   (set_attr "conds" "set,clob,*,*,nocond,nocond,nocond,nocond,nocond")])

(define_split 
  [(set (match_operand:SI 0 "register_operand" "")
	(match_operand:SI 1 "const_int_operand" ""))]
  "TARGET_THUMB1 && satisfies_constraint_J (operands[1])"
  [(set (match_dup 2) (match_dup 1))
   (set (match_dup 0) (neg:SI (match_dup 2)))]
  "
  {
    operands[1] = GEN_INT (- INTVAL (operands[1]));
    operands[2] = can_create_pseudo_p () ? gen_reg_rtx (SImode) : operands[0];
  }"
)

(define_split 
  [(set (match_operand:SI 0 "register_operand" "")
	(match_operand:SI 1 "const_int_operand" ""))]
  "TARGET_THUMB1 && satisfies_constraint_K (operands[1])"
  [(set (match_dup 2) (match_dup 1))
   (set (match_dup 0) (ashift:SI (match_dup 2) (match_dup 3)))]
  "
  {
    unsigned HOST_WIDE_INT val = INTVAL (operands[1]) & 0xffffffffu;
    unsigned HOST_WIDE_INT mask = 0xff;
    int i;
    
    for (i = 0; i < 25; i++)
      if ((val & (mask << i)) == val)
        break;

    /* Don't split if the shift is zero.  */
    if (i == 0)
      FAIL;

    operands[1] = GEN_INT (val >> i);
    operands[2] = can_create_pseudo_p () ? gen_reg_rtx (SImode) : operands[0];
    operands[3] = GEN_INT (i);
  }"
)

;; For thumb1 split imm move [256-510] into mov [1-255] and add #255
(define_split 
  [(set (match_operand:SI 0 "register_operand" "")
	(match_operand:SI 1 "const_int_operand" ""))]
  "TARGET_THUMB1 && satisfies_constraint_Pe (operands[1])"
  [(set (match_dup 2) (match_dup 1))
   (set (match_dup 0) (plus:SI (match_dup 2) (match_dup 3)))]
  "
  {
    operands[1] = GEN_INT (INTVAL (operands[1]) - 255);
    operands[2] = can_create_pseudo_p () ? gen_reg_rtx (SImode) : operands[0];
    operands[3] = GEN_INT (255);
  }"
)

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
  (set_attr "pool_range" "4096,4096,1024")
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
   (set_attr "pool_range" "4096")
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
   (set (attr "pool_range") (const_int 1024))]
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
  [(set_attr "length" "2")]
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
  [(set_attr "predicable" "yes")]
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
  [(set_attr "predicable" "yes")]
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
   sub%.\\t%0, %1, #0"
  [(set_attr "conds" "set")]
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

    if ((code == PLUS && GET_CODE (XEXP (addr, 1)) != CONST_INT)
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

    if ((code == PLUS && GET_CODE (XEXP (addr, 1)) != CONST_INT)
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

    if ((code == PLUS && GET_CODE (XEXP (addr, 1)) != CONST_INT)
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
          if (GET_CODE (operands[0]) == MEM)
	    {
	      if (arm_arch4)
	        {
	          emit_insn (gen_storehi_single_op (operands[0], operands[1]));
	          DONE;
	        }
	      if (GET_CODE (operands[1]) == CONST_INT)
	        emit_insn (gen_storeinthi (operands[0], operands[1]));
	      else
	        {
	          if (GET_CODE (operands[1]) == MEM)
		    operands[1] = force_reg (HImode, operands[1]);
	          if (BYTES_BIG_ENDIAN)
		    emit_insn (gen_storehi_bigend (operands[1], operands[0]));
	          else
		   emit_insn (gen_storehi (operands[1], operands[0]));
	        }
	      DONE;
	    }
          /* Sign extend a constant, and keep it in an SImode reg.  */
          else if (GET_CODE (operands[1]) == CONST_INT)
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
		   && GET_CODE (operands[1]) == MEM)
	    {
	      rtx reg = gen_reg_rtx (SImode);

	      emit_insn (gen_zero_extendhisi2 (reg, operands[1]));
	      operands[1] = gen_lowpart (HImode, reg);
	    }
          else if (!arm_arch4)
	    {
	      if (GET_CODE (operands[1]) == MEM)
	        {
		  rtx base;
		  rtx offset = const0_rtx;
		  rtx reg = gen_reg_rtx (SImode);

		  if ((GET_CODE (base = XEXP (operands[1], 0)) == REG
		       || (GET_CODE (base) == PLUS
			   && (GET_CODE (offset = XEXP (base, 1))
			       == CONST_INT)
                           && ((INTVAL(offset) & 1) != 1)
			   && GET_CODE (base = XEXP (base, 0)) == REG))
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
      else if (GET_CODE (operands[1]) == CONST_INT
	       && !const_ok_for_arm (INTVAL (operands[1]))
	       && !const_ok_for_arm (~INTVAL (operands[1])))
        {
          /* Writing a constant to memory needs a scratch, which should
	     be handled with SECONDARY_RELOADs.  */
          gcc_assert (GET_CODE (operands[0]) == REG);

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
	  if (GET_CODE (operands[0]) != REG)
	    operands[1] = force_reg (HImode, operands[1]);
          /* Zero extend a constant, and keep it in an SImode reg.  */
          else if (GET_CODE (operands[1]) == CONST_INT)
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
	  if (GET_CODE (operands[1]) == CONST_INT)
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
          if (GET_CODE (operands[0]) == MEM
	      && !memory_address_p (GET_MODE (operands[0]),
				    XEXP (operands[0], 0)))
	    operands[0]
	      = replace_equiv_address (operands[0],
				       copy_to_reg (XEXP (operands[0], 0)));
   
          if (GET_CODE (operands[1]) == MEM
	      && !memory_address_p (GET_MODE (operands[1]),
				    XEXP (operands[1], 0)))
	    operands[1]
	      = replace_equiv_address (operands[1],
				       copy_to_reg (XEXP (operands[1], 0)));

	  if (GET_CODE (operands[1]) == MEM && optimize > 0)
	    {
	      rtx reg = gen_reg_rtx (SImode);

	      emit_insn (gen_zero_extendhisi2 (reg, operands[1]));
	      operands[1] = gen_lowpart (HImode, reg);
	    }

          if (GET_CODE (operands[0]) == MEM)
	    operands[1] = force_reg (HImode, operands[1]);
        }
      else if (GET_CODE (operands[1]) == CONST_INT
	        && !satisfies_constraint_I (operands[1]))
        {
	  /* Handle loading a large integer during reload.  */

          /* Writing a constant to memory needs a scratch, which should
	     be handled with SECONDARY_RELOADs.  */
          gcc_assert (GET_CODE (operands[0]) == REG);

          operands[0] = gen_rtx_SUBREG (SImode, operands[0], 0);
          emit_insn (gen_movsi (operands[0], operands[1]));
          DONE;
        }
    }
  "
)

(define_insn "*thumb1_movhi_insn"
  [(set (match_operand:HI 0 "nonimmediate_operand" "=l,l,m,*r,*h,l")
	(match_operand:HI 1 "general_operand"       "l,m,l,*h,*r,I"))]
  "TARGET_THUMB1
   && (   register_operand (operands[0], HImode)
       || register_operand (operands[1], HImode))"
  "*
  switch (which_alternative)
    {
    case 0: return \"add	%0, %1, #0\";
    case 2: return \"strh	%1, %0\";
    case 3: return \"mov	%0, %1\";
    case 4: return \"mov	%0, %1\";
    case 5: return \"mov	%0, %1\";
    default: gcc_unreachable ();
    case 1:
      /* The stack pointer can end up being taken as an index register.
          Catch this case here and deal with it.  */
      if (GET_CODE (XEXP (operands[1], 0)) == PLUS
	  && GET_CODE (XEXP (XEXP (operands[1], 0), 0)) == REG
	  && REGNO    (XEXP (XEXP (operands[1], 0), 0)) == SP_REGNUM)
        {
	  rtx ops[2];
          ops[0] = operands[0];
          ops[1] = XEXP (XEXP (operands[1], 0), 0);
      
          output_asm_insn (\"mov	%0, %1\", ops);

          XEXP (XEXP (operands[1], 0), 0) = operands[0];
    
	}
      return \"ldrh	%0, %1\";
    }"
  [(set_attr "length" "2,4,2,2,2,2")
   (set_attr "type" "*,load1,store1,*,*,*")
   (set_attr "conds" "clob,nocond,nocond,nocond,nocond,clob")])


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
  [(set (match_operand:HI 0 "nonimmediate_operand" "=r,r,m,r")
	(match_operand:HI 1 "general_operand"      "rI,K,r,mi"))]
  "TARGET_ARM
   && arm_arch4
   && (register_operand (operands[0], HImode)
       || register_operand (operands[1], HImode))"
  "@
   mov%?\\t%0, %1\\t%@ movhi
   mvn%?\\t%0, #%B1\\t%@ movhi
   str%(h%)\\t%1, %0\\t%@ movhi
   ldr%(h%)\\t%0, %1\\t%@ movhi"
  [(set_attr "type" "*,*,store1,load1")
   (set_attr "predicable" "yes")
   (set_attr "insn" "mov,mvn,*,*")
   (set_attr "pool_range" "*,*,*,256")
   (set_attr "neg_pool_range" "*,*,*,244")]
)

(define_insn "*movhi_bytes"
  [(set (match_operand:HI 0 "s_register_operand" "=r,r")
	(match_operand:HI 1 "arm_rhs_operand"  "rI,K"))]
  "TARGET_ARM"
  "@
   mov%?\\t%0, %1\\t%@ movhi
   mvn%?\\t%0, #%B1\\t%@ movhi"
  [(set_attr "predicable" "yes")
   (set_attr "insn" "mov,mvn")]
)

(define_expand "thumb_movhi_clobber"
  [(set (match_operand:HI     0 "memory_operand"   "")
	(match_operand:HI     1 "register_operand" ""))
   (clobber (match_operand:DI 2 "register_operand" ""))]
  "TARGET_THUMB1"
  "
  if (strict_memory_address_p (HImode, XEXP (operands[0], 0))
      && REGNO (operands[1]) <= LAST_LO_REGNUM)
    {
      emit_insn (gen_movhi (operands[0], operands[1]));
      DONE;
    }
  /* XXX Fixme, need to handle other cases here as well.  */
  gcc_unreachable ();
  "
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
      if (GET_CODE (operands[1]) == CONST_INT)
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
          if (GET_CODE (operands[0]) == MEM
	      && !memory_address_p (GET_MODE (operands[0]),
		  		     XEXP (operands[0], 0)))
	    operands[0]
	      = replace_equiv_address (operands[0],
				       copy_to_reg (XEXP (operands[0], 0)));
          if (GET_CODE (operands[1]) == MEM
	      && !memory_address_p (GET_MODE (operands[1]),
				    XEXP (operands[1], 0)))
	     operands[1]
	       = replace_equiv_address (operands[1],
					copy_to_reg (XEXP (operands[1], 0)));
	}

      if (GET_CODE (operands[1]) == MEM && optimize > 0)
	{
	  rtx reg = gen_reg_rtx (SImode);

	  emit_insn (gen_zero_extendqisi2 (reg, operands[1]));
	  operands[1] = gen_lowpart (QImode, reg);
	}

      if (GET_CODE (operands[0]) == MEM)
	operands[1] = force_reg (QImode, operands[1]);
    }
  else if (TARGET_THUMB
	   && GET_CODE (operands[1]) == CONST_INT
	   && !satisfies_constraint_I (operands[1]))
    {
      /* Handle loading a large integer during reload.  */

      /* Writing a constant to memory needs a scratch, which should
	 be handled with SECONDARY_RELOADs.  */
      gcc_assert (GET_CODE (operands[0]) == REG);

      operands[0] = gen_rtx_SUBREG (SImode, operands[0], 0);
      emit_insn (gen_movsi (operands[0], operands[1]));
      DONE;
    }
  "
)


(define_insn "*arm_movqi_insn"
  [(set (match_operand:QI 0 "nonimmediate_operand" "=r,r,l,Uu,r,m")
	(match_operand:QI 1 "general_operand" "rI,K,Uu,l,m,r"))]
  "TARGET_32BIT
   && (   register_operand (operands[0], QImode)
       || register_operand (operands[1], QImode))"
  "@
   mov%?\\t%0, %1
   mvn%?\\t%0, #%B1
   ldr%(b%)\\t%0, %1
   str%(b%)\\t%1, %0
   ldr%(b%)\\t%0, %1
   str%(b%)\\t%1, %0"
  [(set_attr "type" "*,*,load1,store1,load1,store1")
   (set_attr "insn" "mov,mvn,*,*,*,*")
   (set_attr "predicable" "yes")
   (set_attr "arch" "any,any,t2,t2,any,any")
   (set_attr "length" "4,4,2,2,4,4")]
)

(define_insn "*thumb1_movqi_insn"
  [(set (match_operand:QI 0 "nonimmediate_operand" "=l,l,m,*r,*h,l")
	(match_operand:QI 1 "general_operand"      "l, m,l,*h,*r,I"))]
  "TARGET_THUMB1
   && (   register_operand (operands[0], QImode)
       || register_operand (operands[1], QImode))"
  "@
   add\\t%0, %1, #0
   ldrb\\t%0, %1
   strb\\t%1, %0
   mov\\t%0, %1
   mov\\t%0, %1
   mov\\t%0, %1"
  [(set_attr "length" "2")
   (set_attr "type" "*,load1,store1,*,*,*")
   (set_attr "insn" "*,*,*,mov,mov,mov")
   (set_attr "pool_range" "*,32,*,*,*,*")
   (set_attr "conds" "clob,nocond,nocond,nocond,nocond,clob")])

;; HFmode moves
(define_expand "movhf"
  [(set (match_operand:HF 0 "general_operand" "")
	(match_operand:HF 1 "general_operand" ""))]
  "TARGET_EITHER"
  "
  if (TARGET_32BIT)
    {
      if (GET_CODE (operands[0]) == MEM)
        operands[1] = force_reg (HFmode, operands[1]);
    }
  else /* TARGET_THUMB1 */
    {
      if (can_create_pseudo_p ())
        {
           if (GET_CODE (operands[0]) != REG)
	     operands[1] = force_reg (HFmode, operands[1]);
        }
    }
  "
)

(define_insn "*arm32_movhf"
  [(set (match_operand:HF 0 "nonimmediate_operand" "=r,m,r,r")
	(match_operand:HF 1 "general_operand"	   " m,r,r,F"))]
  "TARGET_32BIT && !(TARGET_HARD_FLOAT && TARGET_FP16)
   && (	  s_register_operand (operands[0], HFmode)
       || s_register_operand (operands[1], HFmode))"
  "*
  switch (which_alternative)
    {
    case 0:	/* ARM register from memory */
      return \"ldr%(h%)\\t%0, %1\\t%@ __fp16\";
    case 1:	/* memory from ARM register */
      return \"str%(h%)\\t%1, %0\\t%@ __fp16\";
    case 2:	/* ARM register from ARM register */
      return \"mov%?\\t%0, %1\\t%@ __fp16\";
    case 3:	/* ARM register from constant */
      {
	REAL_VALUE_TYPE r;
	long bits;
	rtx ops[4];

	REAL_VALUE_FROM_CONST_DOUBLE (r, operands[1]);
	bits = real_to_target (NULL, &r, HFmode);
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
   (set_attr "type" "load1,store1,*,*")
   (set_attr "insn" "*,*,mov,mov")
   (set_attr "length" "4,4,4,8")
   (set_attr "predicable" "yes")]
)

(define_insn "*thumb1_movhf"
  [(set (match_operand:HF     0 "nonimmediate_operand" "=l,l,m,*r,*h")
	(match_operand:HF     1 "general_operand"      "l,mF,l,*h,*r"))]
  "TARGET_THUMB1
   && (	  s_register_operand (operands[0], HFmode) 
       || s_register_operand (operands[1], HFmode))"
  "*
  switch (which_alternative)
    {
    case 1:
      {
	rtx addr;
	gcc_assert (GET_CODE(operands[1]) == MEM);
	addr = XEXP (operands[1], 0);
	if (GET_CODE (addr) == LABEL_REF
	    || (GET_CODE (addr) == CONST
		&& GET_CODE (XEXP (addr, 0)) == PLUS
		&& GET_CODE (XEXP (XEXP (addr, 0), 0)) == LABEL_REF
		&& GET_CODE (XEXP (XEXP (addr, 0), 1)) == CONST_INT))
	  {
	    /* Constant pool entry.  */
	    return \"ldr\\t%0, %1\";
	  }
	return \"ldrh\\t%0, %1\";
      }
    case 2: return \"strh\\t%1, %0\";
    default: return \"mov\\t%0, %1\";
    }
  "
  [(set_attr "length" "2")
   (set_attr "type" "*,load1,store1,*,*")
   (set_attr "insn" "mov,*,*,mov,mov")
   (set_attr "pool_range" "*,1020,*,*,*")
   (set_attr "conds" "clob,nocond,nocond,nocond,nocond")])

(define_expand "movsf"
  [(set (match_operand:SF 0 "general_operand" "")
	(match_operand:SF 1 "general_operand" ""))]
  "TARGET_EITHER"
  "
  if (TARGET_32BIT)
    {
      if (GET_CODE (operands[0]) == MEM)
        operands[1] = force_reg (SFmode, operands[1]);
    }
  else /* TARGET_THUMB1 */
    {
      if (can_create_pseudo_p ())
        {
           if (GET_CODE (operands[0]) != REG)
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
   && GET_CODE (operands[1]) == CONST_DOUBLE"
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
   && (GET_CODE (operands[0]) != MEM
       || register_operand (operands[1], SFmode))"
  "@
   mov%?\\t%0, %1
   ldr%?\\t%0, %1\\t%@ float
   str%?\\t%1, %0\\t%@ float"
  [(set_attr "predicable" "yes")
   (set_attr "type" "*,load1,store1")
   (set_attr "insn" "mov,*,*")
   (set_attr "pool_range" "*,4096,*")
   (set_attr "arm_neg_pool_range" "*,4084,*")
   (set_attr "thumb2_neg_pool_range" "*,0,*")]
)

;;; ??? This should have alternatives for constants.
(define_insn "*thumb1_movsf_insn"
  [(set (match_operand:SF     0 "nonimmediate_operand" "=l,l,>,l, m,*r,*h")
	(match_operand:SF     1 "general_operand"      "l, >,l,mF,l,*h,*r"))]
  "TARGET_THUMB1
   && (   register_operand (operands[0], SFmode) 
       || register_operand (operands[1], SFmode))"
  "@
   add\\t%0, %1, #0
   ldmia\\t%1, {%0}
   stmia\\t%0, {%1}
   ldr\\t%0, %1
   str\\t%1, %0
   mov\\t%0, %1
   mov\\t%0, %1"
  [(set_attr "length" "2")
   (set_attr "type" "*,load1,store1,load1,store1,*,*")
   (set_attr "pool_range" "*,*,*,1020,*,*,*")
   (set_attr "insn" "*,*,*,*,*,mov,mov")
   (set_attr "conds" "clob,nocond,nocond,nocond,nocond,nocond,nocond")]
)

(define_expand "movdf"
  [(set (match_operand:DF 0 "general_operand" "")
	(match_operand:DF 1 "general_operand" ""))]
  "TARGET_EITHER"
  "
  if (TARGET_32BIT)
    {
      if (GET_CODE (operands[0]) == MEM)
        operands[1] = force_reg (DFmode, operands[1]);
    }
  else /* TARGET_THUMB */
    {
      if (can_create_pseudo_p ())
        {
          if (GET_CODE (operands[0]) != REG)
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

    emit_insn (gen_rtx_SET (VOIDmode,
			    replace_equiv_address (operands[0], operands[2]),
			    operands[1]));

    if (code == POST_DEC)
      emit_insn (gen_addsi3 (operands[2], operands[2], GEN_INT (-8)));

    DONE;
  }"
)

(define_insn "*movdf_soft_insn"
  [(set (match_operand:DF 0 "nonimmediate_soft_df_operand" "=r,r,r,r,m")
	(match_operand:DF 1 "soft_df_operand" "rDa,Db,Dc,mF,r"))]
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
   (set_attr "type" "*,*,*,load2,store2")
   (set_attr "pool_range" "*,*,*,1020,*")
   (set_attr "arm_neg_pool_range" "*,*,*,1004,*")
   (set_attr "thumb2_neg_pool_range" "*,*,*,0,*")]
)

;;; ??? This should have alternatives for constants.
;;; ??? This was originally identical to the movdi_insn pattern.
;;; ??? The 'F' constraint looks funny, but it should always be replaced by
;;; thumb_reorg with a memory reference.
(define_insn "*thumb_movdf_insn"
  [(set (match_operand:DF 0 "nonimmediate_operand" "=l,l,>,l, m,*r")
	(match_operand:DF 1 "general_operand"      "l, >,l,mF,l,*r"))]
  "TARGET_THUMB1
   && (   register_operand (operands[0], DFmode)
       || register_operand (operands[1], DFmode))"
  "*
  switch (which_alternative)
    {
    default:
    case 0:
      if (REGNO (operands[1]) == REGNO (operands[0]) + 1)
	return \"add\\t%0, %1, #0\;add\\t%H0, %H1, #0\";
      return \"add\\t%H0, %H1, #0\;add\\t%0, %1, #0\";
    case 1:
      return \"ldmia\\t%1, {%0, %H0}\";
    case 2:
      return \"stmia\\t%0, {%1, %H1}\";
    case 3:
      return thumb_load_double_from_address (operands);
    case 4:
      operands[2] = gen_rtx_MEM (SImode,
				 plus_constant (Pmode,
						XEXP (operands[0], 0), 4));
      output_asm_insn (\"str\\t%1, %0\;str\\t%H1, %2\", operands);
      return \"\";
    case 5:
      if (REGNO (operands[1]) == REGNO (operands[0]) + 1)
	return \"mov\\t%0, %1\;mov\\t%H0, %H1\";
      return \"mov\\t%H0, %H1\;mov\\t%0, %1\";
    }
  "
  [(set_attr "length" "4,2,2,6,4,4")
   (set_attr "type" "*,load2,store2,load2,store2,*")
   (set_attr "insn" "*,*,*,*,*,mov")
   (set_attr "pool_range" "*,*,*,1020,*,*")]
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
  if (GET_CODE (operands[2]) != CONST_INT
      || INTVAL (operands[2]) > 14
      || INTVAL (operands[2]) < 2
      || GET_CODE (operands[1]) != MEM
      || GET_CODE (operands[0]) != REG
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
  if (GET_CODE (operands[2]) != CONST_INT
      || INTVAL (operands[2]) > 14
      || INTVAL (operands[2]) < 2
      || GET_CODE (operands[1]) != REG
      || GET_CODE (operands[0]) != MEM
      || REGNO (operands[1]) > (LAST_ARM_REGNUM - 1)
      || REGNO (operands[1]) + INTVAL (operands[2]) > LAST_ARM_REGNUM)
    FAIL;

  operands[3]
    = arm_gen_store_multiple (arm_regs_in_sequence + REGNO (operands[1]),
			      INTVAL (operands[2]),
			      force_reg (SImode, XEXP (operands[0], 0)),
			      FALSE, operands[0], &offset);
})


;; Move a block of memory if it is word aligned and MORE than 2 words long.
;; We could let this apply for blocks of less than this, but it clobbers so
;; many registers that there is then probably a better way.

(define_expand "movmemqi"
  [(match_operand:BLK 0 "general_operand" "")
   (match_operand:BLK 1 "general_operand" "")
   (match_operand:SI 2 "const_int_operand" "")
   (match_operand:SI 3 "const_int_operand" "")]
  "TARGET_EITHER"
  "
  if (TARGET_32BIT)
    {
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

;; Thumb block-move insns

(define_insn "movmem12b"
  [(set (mem:SI (match_operand:SI 2 "register_operand" "0"))
	(mem:SI (match_operand:SI 3 "register_operand" "1")))
   (set (mem:SI (plus:SI (match_dup 2) (const_int 4)))
	(mem:SI (plus:SI (match_dup 3) (const_int 4))))
   (set (mem:SI (plus:SI (match_dup 2) (const_int 8)))
	(mem:SI (plus:SI (match_dup 3) (const_int 8))))
   (set (match_operand:SI 0 "register_operand" "=l")
	(plus:SI (match_dup 2) (const_int 12)))
   (set (match_operand:SI 1 "register_operand" "=l")
	(plus:SI (match_dup 3) (const_int 12)))
   (clobber (match_scratch:SI 4 "=&l"))
   (clobber (match_scratch:SI 5 "=&l"))
   (clobber (match_scratch:SI 6 "=&l"))]
  "TARGET_THUMB1"
  "* return thumb_output_move_mem_multiple (3, operands);"
  [(set_attr "length" "4")
   ; This isn't entirely accurate...  It loads as well, but in terms of
   ; scheduling the following insn it is better to consider it as a store
   (set_attr "type" "store3")]
)

(define_insn "movmem8b"
  [(set (mem:SI (match_operand:SI 2 "register_operand" "0"))
	(mem:SI (match_operand:SI 3 "register_operand" "1")))
   (set (mem:SI (plus:SI (match_dup 2) (const_int 4)))
	(mem:SI (plus:SI (match_dup 3) (const_int 4))))
   (set (match_operand:SI 0 "register_operand" "=l")
	(plus:SI (match_dup 2) (const_int 8)))
   (set (match_operand:SI 1 "register_operand" "=l")
	(plus:SI (match_dup 3) (const_int 8)))
   (clobber (match_scratch:SI 4 "=&l"))
   (clobber (match_scratch:SI 5 "=&l"))]
  "TARGET_THUMB1"
  "* return thumb_output_move_mem_multiple (2, operands);"
  [(set_attr "length" "4")
   ; This isn't entirely accurate...  It loads as well, but in terms of
   ; scheduling the following insn it is better to consider it as a store
   (set_attr "type" "store2")]
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

;; A pattern to recognize a special situation and optimize for it.
;; On the thumb, zero-extension from memory is preferrable to sign-extension
;; due to the available addressing modes.  Hence, convert a signed comparison
;; with zero into an unsigned comparison with 127 if possible.
(define_expand "cbranchqi4"
  [(set (pc) (if_then_else
	      (match_operator 0 "lt_ge_comparison_operator"
	       [(match_operand:QI 1 "memory_operand" "")
	        (match_operand:QI 2 "const0_operand" "")])
	      (label_ref (match_operand 3 "" ""))
	      (pc)))]
  "TARGET_THUMB1"
{
  rtx xops[4];
  xops[1] = gen_reg_rtx (SImode);
  emit_insn (gen_zero_extendqisi2 (xops[1], operands[1]));
  xops[2] = GEN_INT (127);
  xops[0] = gen_rtx_fmt_ee (GET_CODE (operands[0]) == GE ? LEU : GTU,
			    VOIDmode, xops[1], xops[2]);
  xops[3] = operands[3];
  emit_insn (gen_cbranchsi4 (xops[0], xops[1], xops[2], xops[3]));
  DONE;
})

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
	       [(match_operand:DI 1 "cmpdi_operand" "")
	        (match_operand:DI 2 "cmpdi_operand" "")])
	      (label_ref (match_operand 3 "" ""))
	      (pc)))]
  "TARGET_32BIT"
  "{
     /* We should not have two constants.  */
     gcc_assert (GET_MODE (operands[1]) == DImode
		 || GET_MODE (operands[2]) == DImode);

     if (!arm_validize_comparison (&operands[0], &operands[1], &operands[2]))		 
       FAIL;
     emit_jump_insn (gen_cbranch_cc (operands[0], operands[1], operands[2],
				       operands[3]));
     DONE;
   }"
)

(define_insn "cbranchsi4_insn"
  [(set (pc) (if_then_else
	      (match_operator 0 "arm_comparison_operator"
	       [(match_operand:SI 1 "s_register_operand" "l,l*h")
	        (match_operand:SI 2 "thumb1_cmp_operand" "lI*h,*r")])
	      (label_ref (match_operand 3 "" ""))
	      (pc)))]
  "TARGET_THUMB1"
{
  rtx t = cfun->machine->thumb1_cc_insn;
  if (t != NULL_RTX)
    {
      if (!rtx_equal_p (cfun->machine->thumb1_cc_op0, operands[1])
	  || !rtx_equal_p (cfun->machine->thumb1_cc_op1, operands[2]))
	t = NULL_RTX;
      if (cfun->machine->thumb1_cc_mode == CC_NOOVmode)
	{
	  if (!noov_comparison_operator (operands[0], VOIDmode))
	    t = NULL_RTX;
	}
      else if (cfun->machine->thumb1_cc_mode != CCmode)
	t = NULL_RTX;
    }
  if (t == NULL_RTX)
    {
      output_asm_insn ("cmp\t%1, %2", operands);
      cfun->machine->thumb1_cc_insn = insn;
      cfun->machine->thumb1_cc_op0 = operands[1];
      cfun->machine->thumb1_cc_op1 = operands[2];
      cfun->machine->thumb1_cc_mode = CCmode;
    }
  else
    /* Ensure we emit the right type of condition code on the jump.  */
    XEXP (operands[0], 0) = gen_rtx_REG (cfun->machine->thumb1_cc_mode,
					 CC_REGNUM);

  switch (get_attr_length (insn))
    {
    case 4:  return \"b%d0\\t%l3\";
    case 6:  return \"b%D0\\t.LCB%=\;b\\t%l3\\t%@long jump\\n.LCB%=:\";
    default: return \"b%D0\\t.LCB%=\;bl\\t%l3\\t%@far jump\\n.LCB%=:\";
    }
}
  [(set (attr "far_jump")
        (if_then_else
	    (eq_attr "length" "8")
	    (const_string "yes")
            (const_string "no")))
   (set (attr "length") 
        (if_then_else
	    (and (ge (minus (match_dup 3) (pc)) (const_int -250))
	         (le (minus (match_dup 3) (pc)) (const_int 256)))
	    (const_int 4)
	    (if_then_else
	        (and (ge (minus (match_dup 3) (pc)) (const_int -2040))
		     (le (minus (match_dup 3) (pc)) (const_int 2048)))
		(const_int 6)
		(const_int 8))))]
)

(define_insn "cbranchsi4_scratch"
  [(set (pc) (if_then_else
	      (match_operator 4 "arm_comparison_operator"
	       [(match_operand:SI 1 "s_register_operand" "l,0")
	        (match_operand:SI 2 "thumb1_cmpneg_operand" "L,J")])
	      (label_ref (match_operand 3 "" ""))
	      (pc)))
   (clobber (match_scratch:SI 0 "=l,l"))]
  "TARGET_THUMB1"
  "*
  output_asm_insn (\"add\\t%0, %1, #%n2\", operands);

  switch (get_attr_length (insn))
    {
    case 4:  return \"b%d4\\t%l3\";
    case 6:  return \"b%D4\\t.LCB%=\;b\\t%l3\\t%@long jump\\n.LCB%=:\";
    default: return \"b%D4\\t.LCB%=\;bl\\t%l3\\t%@far jump\\n.LCB%=:\";
    }
  "
  [(set (attr "far_jump")
        (if_then_else
	    (eq_attr "length" "8")
	    (const_string "yes")
            (const_string "no")))
   (set (attr "length") 
        (if_then_else
	    (and (ge (minus (match_dup 3) (pc)) (const_int -250))
	         (le (minus (match_dup 3) (pc)) (const_int 256)))
	    (const_int 4)
	    (if_then_else
	        (and (ge (minus (match_dup 3) (pc)) (const_int -2040))
		     (le (minus (match_dup 3) (pc)) (const_int 2048)))
		(const_int 6)
		(const_int 8))))]
)

;; Two peepholes to generate subtract of 0 instead of a move if the
;; condition codes will be useful.
(define_peephole2
  [(set (match_operand:SI 0 "low_register_operand" "")
	(match_operand:SI 1 "low_register_operand" ""))
   (set (pc)
	(if_then_else (match_operator 2 "arm_comparison_operator"
		       [(match_dup 1) (const_int 0)])
		      (label_ref (match_operand 3 "" ""))
		      (pc)))]
  "TARGET_THUMB1"
  [(set (match_dup 0) (minus:SI (match_dup 1) (const_int 0)))
   (set (pc)
	(if_then_else (match_op_dup 2 [(match_dup 0) (const_int 0)])
		      (label_ref (match_dup 3))
		      (pc)))]
  "")

;; Sigh!  This variant shouldn't be needed, but combine often fails to
;; merge cases like this because the op1 is a hard register in
;; arm_class_likely_spilled_p.
(define_peephole2
  [(set (match_operand:SI 0 "low_register_operand" "")
	(match_operand:SI 1 "low_register_operand" ""))
   (set (pc)
	(if_then_else (match_operator 2 "arm_comparison_operator"
		       [(match_dup 0) (const_int 0)])
		      (label_ref (match_operand 3 "" ""))
		      (pc)))]
  "TARGET_THUMB1"
  [(set (match_dup 0) (minus:SI (match_dup 1) (const_int 0)))
   (set (pc)
	(if_then_else (match_op_dup 2 [(match_dup 0) (const_int 0)])
		      (label_ref (match_dup 3))
		      (pc)))]
  "")

(define_insn "*negated_cbranchsi4"
  [(set (pc)
	(if_then_else
	 (match_operator 0 "equality_operator"
	  [(match_operand:SI 1 "s_register_operand" "l")
	   (neg:SI (match_operand:SI 2 "s_register_operand" "l"))])
	 (label_ref (match_operand 3 "" ""))
	 (pc)))]
  "TARGET_THUMB1"
  "*
  output_asm_insn (\"cmn\\t%1, %2\", operands);
  switch (get_attr_length (insn))
    {
    case 4:  return \"b%d0\\t%l3\";
    case 6:  return \"b%D0\\t.LCB%=\;b\\t%l3\\t%@long jump\\n.LCB%=:\";
    default: return \"b%D0\\t.LCB%=\;bl\\t%l3\\t%@far jump\\n.LCB%=:\";
    }
  "
  [(set (attr "far_jump")
        (if_then_else
	    (eq_attr "length" "8")
	    (const_string "yes")
            (const_string "no")))
   (set (attr "length") 
        (if_then_else
	    (and (ge (minus (match_dup 3) (pc)) (const_int -250))
	         (le (minus (match_dup 3) (pc)) (const_int 256)))
	    (const_int 4)
	    (if_then_else
	        (and (ge (minus (match_dup 3) (pc)) (const_int -2040))
		     (le (minus (match_dup 3) (pc)) (const_int 2048)))
		(const_int 6)
		(const_int 8))))]
)

(define_insn "*tbit_cbranch"
  [(set (pc)
	(if_then_else
	 (match_operator 0 "equality_operator"
	  [(zero_extract:SI (match_operand:SI 1 "s_register_operand" "l")
			    (const_int 1)
			    (match_operand:SI 2 "const_int_operand" "i"))
	   (const_int 0)])
	 (label_ref (match_operand 3 "" ""))
	 (pc)))
   (clobber (match_scratch:SI 4 "=l"))]
  "TARGET_THUMB1"
  "*
  {
  rtx op[3];
  op[0] = operands[4];
  op[1] = operands[1];
  op[2] = GEN_INT (32 - 1 - INTVAL (operands[2]));

  output_asm_insn (\"lsl\\t%0, %1, %2\", op);
  switch (get_attr_length (insn))
    {
    case 4:  return \"b%d0\\t%l3\";
    case 6:  return \"b%D0\\t.LCB%=\;b\\t%l3\\t%@long jump\\n.LCB%=:\";
    default: return \"b%D0\\t.LCB%=\;bl\\t%l3\\t%@far jump\\n.LCB%=:\";
    }
  }"
  [(set (attr "far_jump")
        (if_then_else
	    (eq_attr "length" "8")
	    (const_string "yes")
            (const_string "no")))
   (set (attr "length") 
        (if_then_else
	    (and (ge (minus (match_dup 3) (pc)) (const_int -250))
	         (le (minus (match_dup 3) (pc)) (const_int 256)))
	    (const_int 4)
	    (if_then_else
	        (and (ge (minus (match_dup 3) (pc)) (const_int -2040))
		     (le (minus (match_dup 3) (pc)) (const_int 2048)))
		(const_int 6)
		(const_int 8))))]
)
  
(define_insn "*tlobits_cbranch"
  [(set (pc)
	(if_then_else
	 (match_operator 0 "equality_operator"
	  [(zero_extract:SI (match_operand:SI 1 "s_register_operand" "l")
			    (match_operand:SI 2 "const_int_operand" "i")
			    (const_int 0))
	   (const_int 0)])
	 (label_ref (match_operand 3 "" ""))
	 (pc)))
   (clobber (match_scratch:SI 4 "=l"))]
  "TARGET_THUMB1"
  "*
  {
  rtx op[3];
  op[0] = operands[4];
  op[1] = operands[1];
  op[2] = GEN_INT (32 - INTVAL (operands[2]));

  output_asm_insn (\"lsl\\t%0, %1, %2\", op);
  switch (get_attr_length (insn))
    {
    case 4:  return \"b%d0\\t%l3\";
    case 6:  return \"b%D0\\t.LCB%=\;b\\t%l3\\t%@long jump\\n.LCB%=:\";
    default: return \"b%D0\\t.LCB%=\;bl\\t%l3\\t%@far jump\\n.LCB%=:\";
    }
  }"
  [(set (attr "far_jump")
        (if_then_else
	    (eq_attr "length" "8")
	    (const_string "yes")
            (const_string "no")))
   (set (attr "length") 
        (if_then_else
	    (and (ge (minus (match_dup 3) (pc)) (const_int -250))
	         (le (minus (match_dup 3) (pc)) (const_int 256)))
	    (const_int 4)
	    (if_then_else
	        (and (ge (minus (match_dup 3) (pc)) (const_int -2040))
		     (le (minus (match_dup 3) (pc)) (const_int 2048)))
		(const_int 6)
		(const_int 8))))]
)

(define_insn "*tstsi3_cbranch"
  [(set (pc)
	(if_then_else
	 (match_operator 3 "equality_operator"
	  [(and:SI (match_operand:SI 0 "s_register_operand" "%l")
		   (match_operand:SI 1 "s_register_operand" "l"))
	   (const_int 0)])
	 (label_ref (match_operand 2 "" ""))
	 (pc)))]
  "TARGET_THUMB1"
  "*
  {
  output_asm_insn (\"tst\\t%0, %1\", operands);
  switch (get_attr_length (insn))
    {
    case 4:  return \"b%d3\\t%l2\";
    case 6:  return \"b%D3\\t.LCB%=\;b\\t%l2\\t%@long jump\\n.LCB%=:\";
    default: return \"b%D3\\t.LCB%=\;bl\\t%l2\\t%@far jump\\n.LCB%=:\";
    }
  }"
  [(set (attr "far_jump")
        (if_then_else
	    (eq_attr "length" "8")
	    (const_string "yes")
            (const_string "no")))
   (set (attr "length") 
        (if_then_else
	    (and (ge (minus (match_dup 2) (pc)) (const_int -250))
	         (le (minus (match_dup 2) (pc)) (const_int 256)))
	    (const_int 4)
	    (if_then_else
	        (and (ge (minus (match_dup 2) (pc)) (const_int -2040))
		     (le (minus (match_dup 2) (pc)) (const_int 2048)))
		(const_int 6)
		(const_int 8))))]
)
  
(define_insn "*cbranchne_decr1"
  [(set (pc)
	(if_then_else (match_operator 3 "equality_operator"
		       [(match_operand:SI 2 "s_register_operand" "l,l,1,l")
		        (const_int 0)])
		      (label_ref (match_operand 4 "" ""))
		      (pc)))
   (set (match_operand:SI 0 "thumb_cbrch_target_operand" "=l,*?h,*?m,*?m")
	(plus:SI (match_dup 2) (const_int -1)))
   (clobber (match_scratch:SI 1 "=X,l,&l,&l"))]
  "TARGET_THUMB1"
  "*
   {
     rtx cond[2];
     cond[0] = gen_rtx_fmt_ee ((GET_CODE (operands[3]) == NE
				? GEU : LTU),
			       VOIDmode, operands[2], const1_rtx);
     cond[1] = operands[4];

     if (which_alternative == 0)
       output_asm_insn (\"sub\\t%0, %2, #1\", operands);
     else if (which_alternative == 1)
       {
	 /* We must provide an alternative for a hi reg because reload 
	    cannot handle output reloads on a jump instruction, but we
	    can't subtract into that.  Fortunately a mov from lo to hi
	    does not clobber the condition codes.  */
	 output_asm_insn (\"sub\\t%1, %2, #1\", operands);
	 output_asm_insn (\"mov\\t%0, %1\", operands);
       }
     else
       {
	 /* Similarly, but the target is memory.  */
	 output_asm_insn (\"sub\\t%1, %2, #1\", operands);
	 output_asm_insn (\"str\\t%1, %0\", operands);
       }

     switch (get_attr_length (insn) - (which_alternative ? 2 : 0))
       {
	 case 4:
	   output_asm_insn (\"b%d0\\t%l1\", cond);
	   return \"\";
	 case 6:
	   output_asm_insn (\"b%D0\\t.LCB%=\", cond);
	   return \"b\\t%l4\\t%@long jump\\n.LCB%=:\";
	 default:
	   output_asm_insn (\"b%D0\\t.LCB%=\", cond);
	   return \"bl\\t%l4\\t%@far jump\\n.LCB%=:\";
       }
   }
  "
  [(set (attr "far_jump")
        (if_then_else
	    (ior (and (eq (symbol_ref ("which_alternative"))
	                  (const_int 0))
		      (eq_attr "length" "8"))
		 (eq_attr "length" "10"))
	    (const_string "yes")
            (const_string "no")))
   (set_attr_alternative "length"
      [
       ;; Alternative 0
       (if_then_else
	 (and (ge (minus (match_dup 4) (pc)) (const_int -250))
	      (le (minus (match_dup 4) (pc)) (const_int 256)))
	 (const_int 4)
	 (if_then_else
	   (and (ge (minus (match_dup 4) (pc)) (const_int -2040))
		(le (minus (match_dup 4) (pc)) (const_int 2048)))
	   (const_int 6)
	   (const_int 8)))
       ;; Alternative 1
       (if_then_else
	 (and (ge (minus (match_dup 4) (pc)) (const_int -248))
	      (le (minus (match_dup 4) (pc)) (const_int 256)))
	 (const_int 6)
	 (if_then_else
	   (and (ge (minus (match_dup 4) (pc)) (const_int -2038))
		(le (minus (match_dup 4) (pc)) (const_int 2048)))
	   (const_int 8)
	   (const_int 10)))
       ;; Alternative 2
       (if_then_else
	 (and (ge (minus (match_dup 4) (pc)) (const_int -248))
	      (le (minus (match_dup 4) (pc)) (const_int 256)))
	 (const_int 6)
	 (if_then_else
	   (and (ge (minus (match_dup 4) (pc)) (const_int -2038))
		(le (minus (match_dup 4) (pc)) (const_int 2048)))
	   (const_int 8)
	   (const_int 10)))
       ;; Alternative 3
       (if_then_else
	 (and (ge (minus (match_dup 4) (pc)) (const_int -248))
	      (le (minus (match_dup 4) (pc)) (const_int 256)))
	 (const_int 6)
	 (if_then_else
	   (and (ge (minus (match_dup 4) (pc)) (const_int -2038))
		(le (minus (match_dup 4) (pc)) (const_int 2048)))
	   (const_int 8)
	   (const_int 10)))])]
)

(define_insn "*addsi3_cbranch"
  [(set (pc)
	(if_then_else
	 (match_operator 4 "arm_comparison_operator"
	  [(plus:SI
	    (match_operand:SI 2 "s_register_operand" "%0,l,*l,1,1,1")
	    (match_operand:SI 3 "reg_or_int_operand" "IJ,lL,*l,lIJ,lIJ,lIJ"))
	   (const_int 0)])
	 (label_ref (match_operand 5 "" ""))
	 (pc)))
   (set
    (match_operand:SI 0 "thumb_cbrch_target_operand" "=l,l,*!h,*?h,*?m,*?m")
    (plus:SI (match_dup 2) (match_dup 3)))
   (clobber (match_scratch:SI 1 "=X,X,l,l,&l,&l"))]
  "TARGET_THUMB1
   && (GET_CODE (operands[4]) == EQ
       || GET_CODE (operands[4]) == NE
       || GET_CODE (operands[4]) == GE
       || GET_CODE (operands[4]) == LT)"
  "*
   {
     rtx cond[3];

     cond[0] = (which_alternative < 2) ? operands[0] : operands[1];
     cond[1] = operands[2];
     cond[2] = operands[3];

     if (GET_CODE (cond[2]) == CONST_INT && INTVAL (cond[2]) < 0)
       output_asm_insn (\"sub\\t%0, %1, #%n2\", cond);
     else
       output_asm_insn (\"add\\t%0, %1, %2\", cond);

     if (which_alternative >= 2
	 && which_alternative < 4)
       output_asm_insn (\"mov\\t%0, %1\", operands);
     else if (which_alternative >= 4)
       output_asm_insn (\"str\\t%1, %0\", operands);

     switch (get_attr_length (insn) - ((which_alternative >= 2) ? 2 : 0))
       {
	 case 4:
	   return \"b%d4\\t%l5\";
	 case 6:
	   return \"b%D4\\t.LCB%=\;b\\t%l5\\t%@long jump\\n.LCB%=:\";
	 default:
	   return \"b%D4\\t.LCB%=\;bl\\t%l5\\t%@far jump\\n.LCB%=:\";
       }
   }
  "
  [(set (attr "far_jump")
        (if_then_else
	    (ior (and (lt (symbol_ref ("which_alternative"))
	                  (const_int 2))
		      (eq_attr "length" "8"))
		 (eq_attr "length" "10"))
	    (const_string "yes")
            (const_string "no")))
   (set (attr "length")
     (if_then_else
       (lt (symbol_ref ("which_alternative"))
		       (const_int 2))
       (if_then_else
	 (and (ge (minus (match_dup 5) (pc)) (const_int -250))
	      (le (minus (match_dup 5) (pc)) (const_int 256)))
	 (const_int 4)
	 (if_then_else
	   (and (ge (minus (match_dup 5) (pc)) (const_int -2040))
		(le (minus (match_dup 5) (pc)) (const_int 2048)))
	   (const_int 6)
	   (const_int 8)))
       (if_then_else
	 (and (ge (minus (match_dup 5) (pc)) (const_int -248))
	      (le (minus (match_dup 5) (pc)) (const_int 256)))
	 (const_int 6)
	 (if_then_else
	   (and (ge (minus (match_dup 5) (pc)) (const_int -2038))
		(le (minus (match_dup 5) (pc)) (const_int 2048)))
	   (const_int 8)
	   (const_int 10)))))]
)

(define_insn "*addsi3_cbranch_scratch"
  [(set (pc)
	(if_then_else
	 (match_operator 3 "arm_comparison_operator"
	  [(plus:SI
	    (match_operand:SI 1 "s_register_operand" "%l,l,l,0")
	    (match_operand:SI 2 "reg_or_int_operand" "J,l,L,IJ"))
	   (const_int 0)])
	 (label_ref (match_operand 4 "" ""))
	 (pc)))
   (clobber (match_scratch:SI 0 "=X,X,l,l"))]
  "TARGET_THUMB1
   && (GET_CODE (operands[3]) == EQ
       || GET_CODE (operands[3]) == NE
       || GET_CODE (operands[3]) == GE
       || GET_CODE (operands[3]) == LT)"
  "*
   {
     switch (which_alternative)
       {
       case 0:
	 output_asm_insn (\"cmp\t%1, #%n2\", operands);
	 break;
       case 1:
	 output_asm_insn (\"cmn\t%1, %2\", operands);
	 break;
       case 2:
	 if (INTVAL (operands[2]) < 0)
	   output_asm_insn (\"sub\t%0, %1, %2\", operands);
	 else
	   output_asm_insn (\"add\t%0, %1, %2\", operands);
	 break;
       case 3:
	 if (INTVAL (operands[2]) < 0)
	   output_asm_insn (\"sub\t%0, %0, %2\", operands);
	 else
	   output_asm_insn (\"add\t%0, %0, %2\", operands);
	 break;
       }

     switch (get_attr_length (insn))
       {
	 case 4:
	   return \"b%d3\\t%l4\";
	 case 6:
	   return \"b%D3\\t.LCB%=\;b\\t%l4\\t%@long jump\\n.LCB%=:\";
	 default:
	   return \"b%D3\\t.LCB%=\;bl\\t%l4\\t%@far jump\\n.LCB%=:\";
       }
   }
  "
  [(set (attr "far_jump")
        (if_then_else
	    (eq_attr "length" "8")
	    (const_string "yes")
            (const_string "no")))
   (set (attr "length")
       (if_then_else
	 (and (ge (minus (match_dup 4) (pc)) (const_int -250))
	      (le (minus (match_dup 4) (pc)) (const_int 256)))
	 (const_int 4)
	 (if_then_else
	   (and (ge (minus (match_dup 4) (pc)) (const_int -2040))
		(le (minus (match_dup 4) (pc)) (const_int 2048)))
	   (const_int 6)
	   (const_int 8))))]
)


;; Comparison and test insns

(define_insn "*arm_cmpsi_insn"
  [(set (reg:CC CC_REGNUM)
	(compare:CC (match_operand:SI 0 "s_register_operand" "l,r,r,r")
		    (match_operand:SI 1 "arm_add_operand"    "Py,r,rI,L")))]
  "TARGET_32BIT"
  "@
   cmp%?\\t%0, %1
   cmp%?\\t%0, %1
   cmp%?\\t%0, %1
   cmn%?\\t%0, #%n1"
  [(set_attr "conds" "set")
   (set_attr "arch" "t2,t2,any,any")
   (set_attr "length" "2,2,4,4")
   (set_attr "predicable" "yes")]
)

(define_insn "*cmpsi_shiftsi"
  [(set (reg:CC CC_REGNUM)
	(compare:CC (match_operand:SI   0 "s_register_operand" "r,r")
		    (match_operator:SI  3 "shift_operator"
		     [(match_operand:SI 1 "s_register_operand" "r,r")
		      (match_operand:SI 2 "shift_amount_operand" "M,rM")])))]
  "TARGET_32BIT"
  "cmp%?\\t%0, %1%S3"
  [(set_attr "conds" "set")
   (set_attr "shift" "1")
   (set_attr "arch" "32,a")
   (set_attr "type" "alu_shift,alu_shift_reg")])

(define_insn "*cmpsi_shiftsi_swp"
  [(set (reg:CC_SWP CC_REGNUM)
	(compare:CC_SWP (match_operator:SI 3 "shift_operator"
			 [(match_operand:SI 1 "s_register_operand" "r,r")
			  (match_operand:SI 2 "shift_amount_operand" "M,rM")])
			(match_operand:SI 0 "s_register_operand" "r,r")))]
  "TARGET_32BIT"
  "cmp%?\\t%0, %1%S3"
  [(set_attr "conds" "set")
   (set_attr "shift" "1")
   (set_attr "arch" "32,a")
   (set_attr "type" "alu_shift,alu_shift_reg")])

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
				    (const_string "alu_shift")
				    (const_string "alu_shift_reg")))
   (set_attr "predicable" "yes")]
)

;; DImode comparisons.  The generic code generates branches that
;; if-conversion can not reduce to a conditional compare, so we do
;; that directly.

(define_insn "*arm_cmpdi_insn"
  [(set (reg:CC_NCV CC_REGNUM)
	(compare:CC_NCV (match_operand:DI 0 "s_register_operand" "r")
			(match_operand:DI 1 "arm_di_operand"	   "rDi")))
   (clobber (match_scratch:SI 2 "=r"))]
  "TARGET_32BIT"
  "cmp\\t%Q0, %Q1\;sbcs\\t%2, %R0, %R1"
  [(set_attr "conds" "set")
   (set_attr "length" "8")]
)

(define_insn "*arm_cmpdi_unsigned"
  [(set (reg:CC_CZ CC_REGNUM)
	(compare:CC_CZ (match_operand:DI 0 "s_register_operand" "r")
		       (match_operand:DI 1 "arm_di_operand"	"rDi")))]
  "TARGET_32BIT"
  "cmp\\t%R0, %R1\;it eq\;cmpeq\\t%Q0, %Q1"
  [(set_attr "conds" "set")
   (set_attr "length" "8")]
)

(define_insn "*arm_cmpdi_zero"
  [(set (reg:CC_Z CC_REGNUM)
	(compare:CC_Z (match_operand:DI 0 "s_register_operand" "r")
		      (const_int 0)))
   (clobber (match_scratch:SI 1 "=r"))]
  "TARGET_32BIT"
  "orr%.\\t%1, %Q0, %R0"
  [(set_attr "conds" "set")]
)

(define_insn "*thumb_cmpdi_zero"
  [(set (reg:CC_Z CC_REGNUM)
	(compare:CC_Z (match_operand:DI 0 "s_register_operand" "l")
		      (const_int 0)))
   (clobber (match_scratch:SI 1 "=l"))]
  "TARGET_THUMB1"
  "orr\\t%1, %Q0, %R0"
  [(set_attr "conds" "set")
   (set_attr "length" "2")]
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
   (set_attr "length" "0")]
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

(define_insn "*mov_scc"
  [(set (match_operand:SI 0 "s_register_operand" "=r")
	(match_operator:SI 1 "arm_comparison_operator"
	 [(match_operand 2 "cc_register" "") (const_int 0)]))]
  "TARGET_ARM"
  "mov%D1\\t%0, #0\;mov%d1\\t%0, #1"
  [(set_attr "conds" "use")
   (set_attr "insn" "mov")
   (set_attr "length" "8")]
)

(define_insn "*mov_negscc"
  [(set (match_operand:SI 0 "s_register_operand" "=r")
	(neg:SI (match_operator:SI 1 "arm_comparison_operator"
		 [(match_operand 2 "cc_register" "") (const_int 0)])))]
  "TARGET_ARM"
  "mov%D1\\t%0, #0\;mvn%d1\\t%0, #0"
  [(set_attr "conds" "use")
   (set_attr "insn" "mov")
   (set_attr "length" "8")]
)

(define_insn "*mov_notscc"
  [(set (match_operand:SI 0 "s_register_operand" "=r")
	(not:SI (match_operator:SI 1 "arm_comparison_operator"
		 [(match_operand 2 "cc_register" "") (const_int 0)])))]
  "TARGET_ARM"
  "mvn%D1\\t%0, #0\;mvn%d1\\t%0, #1"
  [(set_attr "conds" "use")
   (set_attr "insn" "mov")
   (set_attr "length" "8")]
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
	 [(match_operand:DI 2 "cmpdi_operand" "")
	  (match_operand:DI 3 "cmpdi_operand" "")]))]
  "TARGET_32BIT"
  "{
     /* We should not have two constants.  */
     gcc_assert (GET_MODE (operands[2]) == DImode
		 || GET_MODE (operands[3]) == DImode);

     if (!arm_validize_comparison (&operands[1],
     				   &operands[2],
				   &operands[3]))
       FAIL;
     emit_insn (gen_cstore_cc (operands[0], operands[1], operands[2],
		      	         operands[3]));
     DONE;
   }"
)

(define_expand "cstoresi_eq0_thumb1"
  [(parallel
    [(set (match_operand:SI 0 "s_register_operand" "")
	  (eq:SI (match_operand:SI 1 "s_register_operand" "")
		 (const_int 0)))
     (clobber (match_dup:SI 2))])]
  "TARGET_THUMB1"
  "operands[2] = gen_reg_rtx (SImode);"
)

(define_expand "cstoresi_ne0_thumb1"
  [(parallel
    [(set (match_operand:SI 0 "s_register_operand" "")
	  (ne:SI (match_operand:SI 1 "s_register_operand" "")
		 (const_int 0)))
     (clobber (match_dup:SI 2))])]
  "TARGET_THUMB1"
  "operands[2] = gen_reg_rtx (SImode);"
)

(define_insn "*cstoresi_eq0_thumb1_insn"
  [(set (match_operand:SI 0 "s_register_operand" "=&l,l")
	(eq:SI (match_operand:SI 1 "s_register_operand" "l,0")
	       (const_int 0)))
   (clobber (match_operand:SI 2 "s_register_operand" "=X,l"))]
  "TARGET_THUMB1"
  "@
   neg\\t%0, %1\;adc\\t%0, %0, %1
   neg\\t%2, %1\;adc\\t%0, %1, %2"
  [(set_attr "length" "4")]
)

(define_insn "*cstoresi_ne0_thumb1_insn"
  [(set (match_operand:SI 0 "s_register_operand" "=l")
	(ne:SI (match_operand:SI 1 "s_register_operand" "0")
	       (const_int 0)))
   (clobber (match_operand:SI 2 "s_register_operand" "=l"))]
  "TARGET_THUMB1"
  "sub\\t%2, %1, #1\;sbc\\t%0, %1, %2"
  [(set_attr "length" "4")]
)

;; Used as part of the expansion of thumb ltu and gtu sequences
(define_insn "cstoresi_nltu_thumb1"
  [(set (match_operand:SI 0 "s_register_operand" "=l,l")
        (neg:SI (ltu:SI (match_operand:SI 1 "s_register_operand" "l,*h")
			(match_operand:SI 2 "thumb1_cmp_operand" "lI*h,*r"))))]
  "TARGET_THUMB1"
  "cmp\\t%1, %2\;sbc\\t%0, %0, %0"
  [(set_attr "length" "4")]
)

(define_insn_and_split "cstoresi_ltu_thumb1"
  [(set (match_operand:SI 0 "s_register_operand" "=l,l")
        (ltu:SI (match_operand:SI 1 "s_register_operand" "l,*h")
		(match_operand:SI 2 "thumb1_cmp_operand" "lI*h,*r")))]
  "TARGET_THUMB1"
  "#"
  "TARGET_THUMB1"
  [(set (match_dup 3)
	(neg:SI (ltu:SI (match_dup 1) (match_dup 2))))
   (set (match_dup 0) (neg:SI (match_dup 3)))]
  "operands[3] = gen_reg_rtx (SImode);"
  [(set_attr "length" "4")]
)

;; Used as part of the expansion of thumb les sequence.
(define_insn "thumb1_addsi3_addgeu"
  [(set (match_operand:SI 0 "s_register_operand" "=l")
        (plus:SI (plus:SI (match_operand:SI 1 "s_register_operand" "%0")
			  (match_operand:SI 2 "s_register_operand" "l"))
		 (geu:SI (match_operand:SI 3 "s_register_operand" "l")
			 (match_operand:SI 4 "thumb1_cmp_operand" "lI"))))]
  "TARGET_THUMB1"
  "cmp\\t%3, %4\;adc\\t%0, %1, %2"
  [(set_attr "length" "4")]
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
	(if_then_else:SF (match_operand 1 "expandable_comparison_operator" "")
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
	(if_then_else:DF (match_operand 1 "expandable_comparison_operator" "")
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

(define_insn "*movsicc_insn"
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
   mov%d3\\t%0, %1\;mov%D3\\t%0, %2
   mov%d3\\t%0, %1\;mvn%D3\\t%0, #%B2
   mvn%d3\\t%0, #%B1\;mov%D3\\t%0, %2
   mvn%d3\\t%0, #%B1\;mvn%D3\\t%0, #%B2"
  [(set_attr "length" "4,4,4,4,8,8,8,8")
   (set_attr "conds" "use")
   (set_attr "insn" "mov,mvn,mov,mvn,mov,mov,mvn,mvn")]
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
   (set_attr "insn" "mov")]
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
	   (const_int 4)))]
)

(define_insn "*thumb_jump"
  [(set (pc)
	(label_ref (match_operand 0 "" "")))]
  "TARGET_THUMB1"
  "*
  if (get_attr_length (insn) == 2)
    return \"b\\t%l0\";
  return \"bl\\t%l0\\t%@ far jump\";
  "
  [(set (attr "far_jump")
        (if_then_else
	    (eq_attr "length" "4")
	    (const_string "yes")
	    (const_string "no")))
   (set (attr "length") 
        (if_then_else
	    (and (ge (minus (match_dup 0) (pc)) (const_int -2044))
		 (le (minus (match_dup 0) (pc)) (const_int 2048)))
  	    (const_int 2)
	    (const_int 4)))]
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
    arm_emit_call_insn (pat, XEXP (operands[0], 0));
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
  "TARGET_ARM && arm_arch5"
  "blx%?\\t%0"
  [(set_attr "type" "call")]
)

(define_insn "*call_reg_arm"
  [(call (mem:SI (match_operand:SI 0 "s_register_operand" "r"))
         (match_operand 1 "" ""))
   (use (match_operand 2 "" ""))
   (clobber (reg:SI LR_REGNUM))]
  "TARGET_ARM && !arm_arch5"
  "*
  return output_call (operands);
  "
  ;; length is worst case, normally it is only two
  [(set_attr "length" "12")
   (set_attr "type" "call")]
)


;; Note: not used for armv5+ because the sequence used (ldr pc, ...) is not
;; considered a function call by the branch predictor of some cores (PR40887).
;; Falls back to blx rN (*call_reg_armv5).

(define_insn "*call_mem"
  [(call (mem:SI (match_operand:SI 0 "call_memory_operand" "m"))
	 (match_operand 1 "" ""))
   (use (match_operand 2 "" ""))
   (clobber (reg:SI LR_REGNUM))]
  "TARGET_ARM && !arm_arch5"
  "*
  return output_call_mem (operands);
  "
  [(set_attr "length" "12")
   (set_attr "type" "call")]
)

(define_insn "*call_reg_thumb1_v5"
  [(call (mem:SI (match_operand:SI 0 "register_operand" "l*r"))
	 (match_operand 1 "" ""))
   (use (match_operand 2 "" ""))
   (clobber (reg:SI LR_REGNUM))]
  "TARGET_THUMB1 && arm_arch5"
  "blx\\t%0"
  [(set_attr "length" "2")
   (set_attr "type" "call")]
)

(define_insn "*call_reg_thumb1"
  [(call (mem:SI (match_operand:SI 0 "register_operand" "l*r"))
	 (match_operand 1 "" ""))
   (use (match_operand 2 "" ""))
   (clobber (reg:SI LR_REGNUM))]
  "TARGET_THUMB1 && !arm_arch5"
  "*
  {
    if (!TARGET_CALLER_INTERWORKING)
      return thumb_call_via_reg (operands[0]);
    else if (operands[1] == const0_rtx)
      return \"bl\\t%__interwork_call_via_%0\";
    else if (frame_pointer_needed)
      return \"bl\\t%__interwork_r7_call_via_%0\";
    else
      return \"bl\\t%__interwork_r11_call_via_%0\";
  }"
  [(set_attr "type" "call")]
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
    arm_emit_call_insn (pat, XEXP (operands[1], 0));
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
  "TARGET_ARM && arm_arch5"
  "blx%?\\t%1"
  [(set_attr "type" "call")]
)

(define_insn "*call_value_reg_arm"
  [(set (match_operand 0 "" "")
        (call (mem:SI (match_operand:SI 1 "s_register_operand" "r"))
	      (match_operand 2 "" "")))
   (use (match_operand 3 "" ""))
   (clobber (reg:SI LR_REGNUM))]
  "TARGET_ARM && !arm_arch5"
  "*
  return output_call (&operands[1]);
  "
  [(set_attr "length" "12")
   (set_attr "type" "call")]
)

;; Note: see *call_mem

(define_insn "*call_value_mem"
  [(set (match_operand 0 "" "")
	(call (mem:SI (match_operand:SI 1 "call_memory_operand" "m"))
	      (match_operand 2 "" "")))
   (use (match_operand 3 "" ""))
   (clobber (reg:SI LR_REGNUM))]
  "TARGET_ARM && !arm_arch5 && (!CONSTANT_ADDRESS_P (XEXP (operands[1], 0)))"
  "*
  return output_call_mem (&operands[1]);
  "
  [(set_attr "length" "12")
   (set_attr "type" "call")]
)

(define_insn "*call_value_reg_thumb1_v5"
  [(set (match_operand 0 "" "")
	(call (mem:SI (match_operand:SI 1 "register_operand" "l*r"))
	      (match_operand 2 "" "")))
   (use (match_operand 3 "" ""))
   (clobber (reg:SI LR_REGNUM))]
  "TARGET_THUMB1 && arm_arch5"
  "blx\\t%1"
  [(set_attr "length" "2")
   (set_attr "type" "call")]
)

(define_insn "*call_value_reg_thumb1"
  [(set (match_operand 0 "" "")
	(call (mem:SI (match_operand:SI 1 "register_operand" "l*r"))
	      (match_operand 2 "" "")))
   (use (match_operand 3 "" ""))
   (clobber (reg:SI LR_REGNUM))]
  "TARGET_THUMB1 && !arm_arch5"
  "*
  {
    if (!TARGET_CALLER_INTERWORKING)
      return thumb_call_via_reg (operands[1]);
    else if (operands[2] == const0_rtx)
      return \"bl\\t%__interwork_call_via_%1\";
    else if (frame_pointer_needed)
      return \"bl\\t%__interwork_r7_call_via_%1\";
    else
      return \"bl\\t%__interwork_r11_call_via_%1\";
  }"
  [(set_attr "type" "call")]
)

;; Allow calls to SYMBOL_REFs specially as they are not valid general addresses
;; The 'a' causes the operand to be treated as an address, i.e. no '#' output.

(define_insn "*call_symbol"
  [(call (mem:SI (match_operand:SI 0 "" ""))
	 (match_operand 1 "" ""))
   (use (match_operand 2 "" ""))
   (clobber (reg:SI LR_REGNUM))]
  "TARGET_32BIT
   && (GET_CODE (operands[0]) == SYMBOL_REF)
   && !arm_is_long_call_p (SYMBOL_REF_DECL (operands[0]))"
  "*
  {
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
   && (GET_CODE (operands[1]) == SYMBOL_REF)
   && !arm_is_long_call_p (SYMBOL_REF_DECL (operands[1]))"
  "*
  {
    return NEED_PLT_RELOC ? \"bl%?\\t%a1(PLT)\" : \"bl%?\\t%a1\";
  }"
  [(set_attr "type" "call")]
)

(define_insn "*call_insn"
  [(call (mem:SI (match_operand:SI 0 "" ""))
	 (match_operand:SI 1 "" ""))
   (use (match_operand 2 "" ""))
   (clobber (reg:SI LR_REGNUM))]
  "TARGET_THUMB1
   && GET_CODE (operands[0]) == SYMBOL_REF
   && !arm_is_long_call_p (SYMBOL_REF_DECL (operands[0]))"
  "bl\\t%a0"
  [(set_attr "length" "4")
   (set_attr "type" "call")]
)

(define_insn "*call_value_insn"
  [(set (match_operand 0 "" "")
	(call (mem:SI (match_operand 1 "" ""))
	      (match_operand 2 "" "")))
   (use (match_operand 3 "" ""))
   (clobber (reg:SI LR_REGNUM))]
  "TARGET_THUMB1
   && GET_CODE (operands[1]) == SYMBOL_REF
   && !arm_is_long_call_p (SYMBOL_REF_DECL (operands[1]))"
  "bl\\t%a1"
  [(set_attr "length" "4")
   (set_attr "type" "call")]
)

;; We may also be able to do sibcalls for Thumb, but it's much harder...
(define_expand "sibcall"
  [(parallel [(call (match_operand 0 "memory_operand" "")
		    (match_operand 1 "general_operand" ""))
	      (return)
	      (use (match_operand 2 "" ""))])]
  "TARGET_32BIT"
  "
  {
    if (operands[2] == NULL_RTX)
      operands[2] = const0_rtx;
  }"
)

(define_expand "sibcall_value"
  [(parallel [(set (match_operand 0 "" "")
		   (call (match_operand 1 "memory_operand" "")
			 (match_operand 2 "general_operand" "")))
	      (return)
	      (use (match_operand 3 "" ""))])]
  "TARGET_32BIT"
  "
  {
    if (operands[3] == NULL_RTX)
      operands[3] = const0_rtx;
  }"
)

(define_insn "*sibcall_insn"
 [(call (mem:SI (match_operand:SI 0 "" "X"))
	(match_operand 1 "" ""))
  (return)
  (use (match_operand 2 "" ""))]
  "TARGET_32BIT && GET_CODE (operands[0]) == SYMBOL_REF"
  "*
  return NEED_PLT_RELOC ? \"b%?\\t%a0(PLT)\" : \"b%?\\t%a0\";
  "
  [(set_attr "type" "call")]
)

(define_insn "*sibcall_value_insn"
 [(set (match_operand 0 "" "")
       (call (mem:SI (match_operand:SI 1 "" "X"))
	     (match_operand 2 "" "")))
  (return)
  (use (match_operand 3 "" ""))]
  "TARGET_32BIT && GET_CODE (operands[1]) == SYMBOL_REF"
  "*
  return NEED_PLT_RELOC ? \"b%?\\t%a1(PLT)\" : \"b%?\\t%a1\";
  "
  [(set_attr "type" "call")]
)

(define_expand "return"
  [(return)]
  "(TARGET_ARM || (TARGET_THUMB2
                   && ARM_FUNC_TYPE (arm_current_func_type ()) == ARM_FT_NORMAL
                   && !IS_STACKALIGN (arm_current_func_type ())))
    && USE_RETURN_INSN (FALSE)"
  "
  {
    if (TARGET_THUMB2)
      {
        thumb2_expand_return ();
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

(define_insn "*cond_return"
  [(set (pc)
        (if_then_else (match_operator 0 "arm_comparison_operator"
		       [(match_operand 1 "cc_register" "") (const_int 0)])
                      (return)
                      (pc)))]
  "TARGET_ARM && USE_RETURN_INSN (TRUE)"
  "*
  {
    if (arm_ccfsm_state == 2)
      {
        arm_ccfsm_state += 2;
        return \"\";
      }
    return output_return_instruction (operands[0], true, false, false);
  }"
  [(set_attr "conds" "use")
   (set_attr "length" "12")
   (set_attr "type" "load1")]
)

(define_insn "*cond_return_inverted"
  [(set (pc)
        (if_then_else (match_operator 0 "arm_comparison_operator"
		       [(match_operand 1 "cc_register" "") (const_int 0)])
                      (pc)
		      (return)))]
  "TARGET_ARM && USE_RETURN_INSN (TRUE)"
  "*
  {
    if (arm_ccfsm_state == 2)
      {
        arm_ccfsm_state += 2;
        return \"\";
      }
    return output_return_instruction (operands[0], true, true, false);
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
   (set_attr "conds" "set")]
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

    emit_call_insn (GEN_CALL_VALUE (par, operands[0], const0_rtx, NULL,
				    const0_rtx));

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

(define_expand "casesi"
  [(match_operand:SI 0 "s_register_operand" "")	; index to jump on
   (match_operand:SI 1 "const_int_operand" "")	; lower bound
   (match_operand:SI 2 "const_int_operand" "")	; total range
   (match_operand:SI 3 "" "")			; table label
   (match_operand:SI 4 "" "")]			; Out of range label
  "TARGET_32BIT || optimize_size || flag_pic"
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
   (set_attr "length" "12")]
)

(define_expand "thumb1_casesi_internal_pic"
  [(match_operand:SI 0 "s_register_operand" "")
   (match_operand:SI 1 "thumb1_cmp_operand" "")
   (match_operand 2 "" "")
   (match_operand 3 "" "")]
  "TARGET_THUMB1"
  {
    rtx reg0;
    rtx test = gen_rtx_GTU (VOIDmode, operands[0], operands[1]);
    emit_jump_insn (gen_cbranchsi4 (test, operands[0], operands[1],
				    operands[3]));
    reg0 = gen_rtx_REG (SImode, 0);
    emit_move_insn (reg0, operands[0]);
    emit_jump_insn (gen_thumb1_casesi_dispatch (operands[2]/*, operands[3]*/));
    DONE;
  }
)

(define_insn "thumb1_casesi_dispatch"
  [(parallel [(set (pc) (unspec [(reg:SI 0)
				 (label_ref (match_operand 0 "" ""))
;;				 (label_ref (match_operand 1 "" ""))
]
			 UNSPEC_THUMB1_CASESI))
	      (clobber (reg:SI IP_REGNUM))
              (clobber (reg:SI LR_REGNUM))])]
  "TARGET_THUMB1"
  "* return thumb1_output_casesi(operands);"
  [(set_attr "length" "4")]
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
  [(set_attr "predicable" "yes")]
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

;; NB Never uses BX.
(define_insn "*thumb1_indirect_jump"
  [(set (pc)
	(match_operand:SI 0 "register_operand" "l*r"))]
  "TARGET_THUMB1"
  "mov\\tpc, %0"
  [(set_attr "conds" "clob")
   (set_attr "length" "2")]
)


;; Misc insns

(define_insn "nop"
  [(const_int 0)]
  "TARGET_EITHER"
  "*
  if (TARGET_UNIFIED_ASM)
    return \"nop\";
  if (TARGET_ARM)
    return \"mov%?\\t%|r0, %|r0\\t%@ nop\";
  return  \"mov\\tr8, r8\";
  "
  [(set (attr "length")
	(if_then_else (eq_attr "is_thumb" "yes")
		      (const_int 2)
		      (const_int 4)))]
)


;; Patterns to allow combination of arithmetic, cond code and shifts

(define_insn "*arith_shiftsi"
  [(set (match_operand:SI 0 "s_register_operand" "=r,r,r,r")
        (match_operator:SI 1 "shiftable_operator"
          [(match_operator:SI 3 "shift_operator"
             [(match_operand:SI 4 "s_register_operand" "r,r,r,r")
              (match_operand:SI 5 "shift_amount_operand" "M,M,M,r")])
           (match_operand:SI 2 "s_register_operand" "rk,rk,r,rk")]))]
  "TARGET_32BIT"
  "%i1%?\\t%0, %2, %4%S3"
  [(set_attr "predicable" "yes")
   (set_attr "shift" "4")
   (set_attr "arch" "a,t2,t2,a")
   ;; Thumb2 doesn't allow the stack pointer to be used for 
   ;; operand1 for all operations other than add and sub. In this case 
   ;; the minus operation is a candidate for an rsub and hence needs
   ;; to be disabled.
   ;; We have to make sure to disable the fourth alternative if
   ;; the shift_operator is MULT, since otherwise the insn will
   ;; also match a multiply_accumulate pattern and validate_change
   ;; will allow a replacement of the constant with a register
   ;; despite the checks done in shift_operator.
   (set_attr_alternative "insn_enabled"
			 [(const_string "yes")
			  (if_then_else
			   (match_operand:SI 1 "add_operator" "")
			   (const_string "yes") (const_string "no"))
			  (const_string "yes")
			  (if_then_else
			   (match_operand:SI 3 "mult_operator" "")
			   (const_string "no") (const_string "yes"))])
   (set_attr "type" "alu_shift,alu_shift,alu_shift,alu_shift_reg")])

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
  "%i1%.\\t%0, %2, %4%S3"
  [(set_attr "conds" "set")
   (set_attr "shift" "4")
   (set_attr "arch" "32,a")
   (set_attr "type" "alu_shift,alu_shift_reg")])

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
  "%i1%.\\t%0, %2, %4%S3"
  [(set_attr "conds" "set")
   (set_attr "shift" "4")
   (set_attr "arch" "32,a")
   (set_attr "type" "alu_shift,alu_shift_reg")])

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
   (set_attr "type" "alu_shift,alu_shift_reg")])

(define_insn "*sub_shiftsi_compare0"
  [(set (reg:CC_NOOV CC_REGNUM)
	(compare:CC_NOOV
	 (minus:SI (match_operand:SI 1 "s_register_operand" "r,r")
		   (match_operator:SI 2 "shift_operator"
		    [(match_operand:SI 3 "s_register_operand" "r,r")
		     (match_operand:SI 4 "shift_amount_operand" "M,rM")]))
	 (const_int 0)))
   (set (match_operand:SI 0 "s_register_operand" "=r,r")
	(minus:SI (match_dup 1)
		  (match_op_dup 2 [(match_dup 3) (match_dup 4)])))]
  "TARGET_32BIT"
  "sub%.\\t%0, %1, %3%S2"
  [(set_attr "conds" "set")
   (set_attr "shift" "3")
   (set_attr "arch" "32,a")
   (set_attr "type" "alu_shift,alu_shift_reg")])

(define_insn "*sub_shiftsi_compare0_scratch"
  [(set (reg:CC_NOOV CC_REGNUM)
	(compare:CC_NOOV
	 (minus:SI (match_operand:SI 1 "s_register_operand" "r,r")
		   (match_operator:SI 2 "shift_operator"
		    [(match_operand:SI 3 "s_register_operand" "r,r")
		     (match_operand:SI 4 "shift_amount_operand" "M,rM")]))
	 (const_int 0)))
   (clobber (match_scratch:SI 0 "=r,r"))]
  "TARGET_32BIT"
  "sub%.\\t%0, %1, %3%S2"
  [(set_attr "conds" "set")
   (set_attr "shift" "3")
   (set_attr "arch" "32,a")
   (set_attr "type" "alu_shift,alu_shift_reg")])


(define_insn "*and_scc"
  [(set (match_operand:SI 0 "s_register_operand" "=r")
	(and:SI (match_operator:SI 1 "arm_comparison_operator"
		 [(match_operand 3 "cc_register" "") (const_int 0)])
		(match_operand:SI 2 "s_register_operand" "r")))]
  "TARGET_ARM"
  "mov%D1\\t%0, #0\;and%d1\\t%0, %2, #1"
  [(set_attr "conds" "use")
   (set_attr "insn" "mov")
   (set_attr "length" "8")]
)

(define_insn "*ior_scc"
  [(set (match_operand:SI 0 "s_register_operand" "=r,r")
	(ior:SI (match_operator:SI 2 "arm_comparison_operator"
		 [(match_operand 3 "cc_register" "") (const_int 0)])
		(match_operand:SI 1 "s_register_operand" "0,?r")))]
  "TARGET_ARM"
  "@
   orr%d2\\t%0, %1, #1
   mov%D2\\t%0, %1\;orr%d2\\t%0, %1, #1"
  [(set_attr "conds" "use")
   (set_attr "length" "4,8")]
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
  [(set (match_operand:SI 0 "s_register_operand" "=r,r")
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
  enum machine_mode mode = SELECT_CC_MODE (GET_CODE (operands[1]),
					   operands[2], operands[3]);
  enum rtx_code rc = GET_CODE (operands[1]);

  tmp1 = gen_rtx_REG (mode, CC_REGNUM);

  operands[5] = gen_rtx_fmt_ee (rc, VOIDmode, tmp1, const0_rtx);
  if (mode == CCFPmode || mode == CCFPEmode)
    rc = reverse_condition_maybe_unordered (rc);
  else
    rc = reverse_condition (rc);
  operands[4] = gen_rtx_fmt_ee (rc, VOIDmode, tmp1, const0_rtx);
})

;; Attempt to improve the sequence generated by the compare_scc splitters
;; not to use conditional execution.
(define_peephole2
  [(set (reg:CC CC_REGNUM)
	(compare:CC (match_operand:SI 1 "register_operand" "")
		    (match_operand:SI 2 "arm_rhs_operand" "")))
   (cond_exec (ne (reg:CC CC_REGNUM) (const_int 0))
	      (set (match_operand:SI 0 "register_operand" "") (const_int 0)))
   (cond_exec (eq (reg:CC CC_REGNUM) (const_int 0))
	      (set (match_dup 0) (const_int 1)))
   (match_scratch:SI 3 "r")]
  "TARGET_32BIT"
  [(parallel
    [(set (reg:CC CC_REGNUM)
	  (compare:CC (match_dup 1) (match_dup 2)))
     (set (match_dup 3) (minus:SI (match_dup 1) (match_dup 2)))])
   (parallel
    [(set (reg:CC CC_REGNUM)
	  (compare:CC (const_int 0) (match_dup 3)))
     (set (match_dup 0) (minus:SI (const_int 0) (match_dup 3)))])
   (parallel
    [(set (match_dup 0)
	  (plus:SI (plus:SI (match_dup 0) (match_dup 3))
		   (geu:SI (reg:CC CC_REGNUM) (const_int 0))))
     (clobber (reg:CC CC_REGNUM))])])

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
   (set_attr "insn" "mov")
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
   (set_attr "length" "12")]
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
   (set_attr "length" "8,12")]
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
           (const_int 10))])]
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
           (const_int 10))])]
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
           (const_int 10))])]
)

(define_insn_and_split "*ior_scc_scc"
  [(set (match_operand:SI 0 "s_register_operand" "=r")
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
   (set_attr "length" "16")])

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
   (set (match_operand:SI 7 "s_register_operand" "=r")
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
   (set_attr "length" "16")])

(define_insn_and_split "*and_scc_scc"
  [(set (match_operand:SI 0 "s_register_operand" "=r")
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
   (set_attr "length" "16")])

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
   (set (match_operand:SI 7 "s_register_operand" "=r")
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
   (set_attr "length" "16")])

;; If there is no dominance in the comparison, then we can still save an
;; instruction in the AND case, since we can know that the second compare
;; need only zero the value if false (if true, then the value is already
;; correct).
(define_insn_and_split "*and_scc_scc_nodom"
  [(set (match_operand:SI 0 "s_register_operand" "=&r,&r,&r")
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
   (set_attr "length" "20")])

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

(define_insn "*negscc"
  [(set (match_operand:SI 0 "s_register_operand" "=r")
	(neg:SI (match_operator 3 "arm_comparison_operator"
		 [(match_operand:SI 1 "s_register_operand" "r")
		  (match_operand:SI 2 "arm_rhs_operand" "rI")])))
   (clobber (reg:CC CC_REGNUM))]
  "TARGET_ARM"
  "*
  if (GET_CODE (operands[3]) == LT && operands[2] == const0_rtx)
    return \"mov\\t%0, %1, asr #31\";

  if (GET_CODE (operands[3]) == NE)
    return \"subs\\t%0, %1, %2\;mvnne\\t%0, #0\";

  output_asm_insn (\"cmp\\t%1, %2\", operands);
  output_asm_insn (\"mov%D3\\t%0, #0\", operands);
  return \"mvn%d3\\t%0, #0\";
  "
  [(set_attr "conds" "clob")
   (set_attr "length" "12")]
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
      if (which_alternative != 1 && GET_CODE (operands[1]) == REG)
	{
	  if (operands[2] == const0_rtx)
	    return \"and\\t%0, %1, %3, asr #31\";
	  return \"ands\\t%0, %1, %3, asr #32\;movcc\\t%0, %2\";
	}
      else if (which_alternative != 0 && GET_CODE (operands[2]) == REG)
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
      if (which_alternative != 1 && GET_CODE (operands[1]) == REG)
	{
	  if (operands[2] == const0_rtx)
	    return \"bic\\t%0, %1, %3, asr #31\";
	  return \"bics\\t%0, %1, %3, asr #32\;movcs\\t%0, %2\";
	}
      else if (which_alternative != 0 && GET_CODE (operands[2]) == REG)
	{
	  if (operands[1] == const0_rtx)
	    return \"and\\t%0, %2, %3, asr #31\";
	  return \"ands\\t%0, %2, %3, asr #32\;movcc\\t%0, %1\";
	}
      /* The only case that falls through to here is when both ops 1 & 2
	 are constants.  */
    }
  if (GET_CODE (operands[4]) == CONST_INT
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
   (set_attr "length" "8,8,12")]
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
   (set_attr "length" "8,12")]
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
   (set_attr "type" "*,*,*,*")]
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
   (set_attr "length" "8,12")]
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
   (set_attr "type" "*,*,*,*")]
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
   (set_attr "length" "12")]
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
   (set_attr "length" "8")]
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
      && GET_CODE (operands[5]) == REG
      && GET_CODE (operands[1]) == REG 
      && REGNO (operands[1]) == REGNO (operands[4])
      && REGNO (operands[4]) != REGNO (operands[0]))
    {
      if (GET_CODE (operands[6]) == LT)
	return \"and\\t%0, %5, %2, asr #31\;%I7\\t%0, %4, %0\";
      else if (GET_CODE (operands[6]) == GE)
	return \"bic\\t%0, %5, %2, asr #31\;%I7\\t%0, %4, %0\";
    }
  if (GET_CODE (operands[3]) == CONST_INT
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
   (set_attr "length" "8,12")]
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
   (set_attr "type" "*,*")]
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
      && GET_CODE (operands[3]) == REG
      && GET_CODE (operands[1]) == REG 
      && REGNO (operands[1]) == REGNO (operands[2])
      && REGNO (operands[2]) != REGNO (operands[0]))
    {
      if (GET_CODE (operands[6]) == GE)
	return \"and\\t%0, %3, %4, asr #31\;%I7\\t%0, %2, %0\";
      else if (GET_CODE (operands[6]) == LT)
	return \"bic\\t%0, %3, %4, asr #31\;%I7\\t%0, %2, %0\";
    }

  if (GET_CODE (operands[5]) == CONST_INT
      && !const_ok_for_arm (INTVAL (operands[5])))
    output_asm_insn (\"cmn\\t%4, #%n5\", operands);
  else
    output_asm_insn (\"cmp\\t%4, %5\", operands);

  if (which_alternative != 0)
    output_asm_insn (\"mov%d6\\t%0, %1\", operands);
  return \"%I7%D6\\t%0, %2, %3\";
  "
  [(set_attr "conds" "clob")
   (set_attr "length" "8,12")]
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
   (set_attr "type" "*,*")]
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
   (set_attr "length" "8,12")]
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
   (set_attr "insn" "mvn")
   (set_attr "length" "4,8,8")]
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
   (set_attr "length" "8,12")]
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
   (set_attr "insn" "mvn")
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
   (set_attr "length" "8,12")]
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
   (set_attr "insn" "mov")
   (set (attr "type") (if_then_else (match_operand 3 "const_int_operand" "")
		      (const_string "alu_shift")
		      (const_string "alu_shift_reg")))]
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
   (set_attr "length" "8,12")]
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
   (set_attr "insn" "mov")
   (set (attr "type") (if_then_else (match_operand 3 "const_int_operand" "")
		      (const_string "alu_shift")
		      (const_string "alu_shift_reg")))]
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
   (set_attr "length" "12")]
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
   (set_attr "insn" "mov")
   (set (attr "type") (if_then_else
		        (and (match_operand 2 "const_int_operand" "")
                             (match_operand 4 "const_int_operand" ""))
		      (const_string "alu_shift")
		      (const_string "alu_shift_reg")))]
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
   (set_attr "length" "12")]
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
   (set_attr "insn" "mvn")
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
   (set_attr "length" "12")]
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
   (set_attr "insn" "mvn")
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
   (set_attr "length" "8,12")]
)

(define_insn "*if_neg_move"
  [(set (match_operand:SI 0 "s_register_operand" "=r,r,r")
	(if_then_else:SI
	 (match_operator 4 "arm_comparison_operator"
	  [(match_operand 3 "cc_register" "") (const_int 0)])
	 (neg:SI (match_operand:SI 2 "s_register_operand" "r,r,r"))
	 (match_operand:SI 1 "arm_not_operand" "0,?rI,K")))]
  "TARGET_ARM"
  "@
   rsb%d4\\t%0, %2, #0
   mov%D4\\t%0, %1\;rsb%d4\\t%0, %2, #0
   mvn%D4\\t%0, #%B1\;rsb%d4\\t%0, %2, #0"
  [(set_attr "conds" "use")
   (set_attr "length" "4,8,8")]
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
   (set_attr "length" "8,12")]
)

(define_insn "*if_move_neg"
  [(set (match_operand:SI 0 "s_register_operand" "=r,r,r")
	(if_then_else:SI
	 (match_operator 4 "arm_comparison_operator"
	  [(match_operand 3 "cc_register" "") (const_int 0)])
	 (match_operand:SI 1 "arm_not_operand" "0,?rI,K")
	 (neg:SI (match_operand:SI 2 "s_register_operand" "r,r,r"))))]
  "TARGET_ARM"
  "@
   rsb%D4\\t%0, %2, #0
   mov%d4\\t%0, %1\;rsb%D4\\t%0, %2, #0
   mvn%d4\\t%0, #%B1\;rsb%D4\\t%0, %2, #0"
  [(set_attr "conds" "use")
   (set_attr "length" "4,8,8")]
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
	  output_asm_insn (\"ldm%(ib%)\\t%0, {%1, %2}\", ldm);
	else if (const_ok_for_arm (val1) || const_ok_for_arm (-val1))
	  {
	    ldm[0] = ops[0] = operands[4];
	    ops[1] = base_reg;
	    ops[2] = GEN_INT (val1);
	    output_add_immediate (ops);
	    if (val1 < val2)
	      output_asm_insn (\"ldm%(ia%)\\t%0, {%1, %2}\", ldm);
	    else
	      output_asm_insn (\"ldm%(da%)\\t%0, {%1, %2}\", ldm);
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
	  output_asm_insn (\"ldm%(da%)\\t%0, {%1, %2}\", ldm);
	else
	  output_asm_insn (\"ldm%(ia%)\\t%0, {%1, %2}\", ldm);
      }
    else
      {
	if (val1 < val2)
	  output_asm_insn (\"ldm%(ia%)\\t%0, {%1, %2}\", ldm);
	else
	  output_asm_insn (\"ldm%(da%)\\t%0, {%1, %2}\", ldm);
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
   && (((unsigned HOST_WIDE_INT) INTVAL (operands[1]))
       == (((unsigned HOST_WIDE_INT) INTVAL (operands[1])) >> 24) << 24)"
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
    emit_insn (gen_prologue_use (gen_rtx_REG (Pmode, 2)));
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

(define_insn "prologue_thumb1_interwork"
  [(unspec_volatile [(const_int 0)] VUNSPEC_THUMB1_INTERWORK)]
  "TARGET_THUMB1"
  "* return thumb1_output_interwork ();"
  [(set_attr "length" "8")]
)

;; Note - although unspec_volatile's USE all hard registers,
;; USEs are ignored after relaod has completed.  Thus we need
;; to add an unspec of the link register to ensure that flow
;; does not think that it is unused by the sibcall branch that
;; will replace the standard function epilogue.
(define_expand "sibcall_epilogue"
   [(parallel [(unspec:SI [(reg:SI LR_REGNUM)] UNSPEC_PROLOGUE_USE)
               (unspec_volatile [(return)] VUNSPEC_EPILOGUE)])]
   "TARGET_32BIT"
   "
   arm_expand_epilogue (false);
   DONE;
   "
)

(define_insn "*epilogue_insns"
  [(unspec_volatile [(return)] VUNSPEC_EPILOGUE)]
  "TARGET_THUMB1"
  "*
    return thumb1_unexpanded_epilogue ();
  "
  ; Length is absolute worst case
  [(set_attr "length" "44")
   (set_attr "type" "block")
   ;; We don't clobber the conditions, but the potential length of this
   ;; operation is sufficient to make conditionalizing the sequence 
   ;; unlikely to be profitable.
   (set_attr "conds" "clob")]
)

(define_expand "eh_epilogue"
  [(use (match_operand:SI 0 "register_operand" ""))
   (use (match_operand:SI 1 "register_operand" ""))
   (use (match_operand:SI 2 "register_operand" ""))]
  "TARGET_EITHER"
  "
  {
    cfun->machine->eh_epilogue_sp_ofs = operands[1];
    if (GET_CODE (operands[2]) != REG || REGNO (operands[2]) != 2)
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
    enum machine_mode mode = SELECT_CC_MODE (GET_CODE (operands[1]),
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
    enum machine_mode mode = SELECT_CC_MODE (GET_CODE (operands[1]),
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
    enum machine_mode mode = SELECT_CC_MODE (GET_CODE (operands[1]),
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
    enum machine_mode mode = SELECT_CC_MODE (GET_CODE (operands[1]),
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
   (set_attr "insn" "mvn")
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
   (set_attr "length" "8")]
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
   (set_attr "length" "12")]
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

	if (TARGET_ARM)
	    strcpy (pattern, \"stm%(fd%)\\t%m0!, {%1\");
	else if (TARGET_THUMB2)
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
  [(set_attr "length" "0")]
)

;; Pop (as used in epilogue RTL)
;;
(define_insn "*load_multiple_with_writeback"
  [(match_parallel 0 "load_multiple_operation"
    [(set (match_operand:SI 1 "s_register_operand" "+rk")
          (plus:SI (match_dup 1)
                   (match_operand:SI 2 "const_int_operand" "I")))
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
   (set_attr "predicable" "yes")]
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
                   (match_operand:SI 2 "const_int_operand" "I")))
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
   (set_attr "predicable" "yes")]
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
   (set_attr "predicable" "yes")]
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
                   (match_operand:SI 2 "const_int_operand" "I")))
     (set (match_operand:DF 3 "arm_hard_register_operand" "")
          (mem:DF (match_dup 1)))])]
  "TARGET_32BIT && TARGET_HARD_FLOAT && TARGET_VFP"
  "*
  {
    int num_regs = XVECLEN (operands[0], 0);
    char pattern[100];
    rtx op_list[2];
    strcpy (pattern, \"fldmfdd\\t\");
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
)

(define_insn "align_8"
  [(unspec_volatile [(const_int 0)] VUNSPEC_ALIGN8)]
  "TARGET_EITHER"
  "*
  assemble_align (64);
  return \"\";
  "
)

(define_insn "consttable_end"
  [(unspec_volatile [(const_int 0)] VUNSPEC_POOL_END)]
  "TARGET_EITHER"
  "*
  making_const_table = FALSE;
  return \"\";
  "
)

(define_insn "consttable_1"
  [(unspec_volatile [(match_operand 0 "" "")] VUNSPEC_POOL_1)]
  "TARGET_THUMB1"
  "*
  making_const_table = TRUE;
  assemble_integer (operands[0], 1, BITS_PER_WORD, 1);
  assemble_zeros (3);
  return \"\";
  "
  [(set_attr "length" "4")]
)

(define_insn "consttable_2"
  [(unspec_volatile [(match_operand 0 "" "")] VUNSPEC_POOL_2)]
  "TARGET_THUMB1"
  "*
  making_const_table = TRUE;
  gcc_assert (GET_MODE_CLASS (GET_MODE (operands[0])) != MODE_FLOAT);
  assemble_integer (operands[0], 2, BITS_PER_WORD, 1);
  assemble_zeros (2);
  return \"\";
  "
  [(set_attr "length" "4")]
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
 	if (GET_MODE (x) == HFmode)
 	  arm_emit_fp16_const (x);
 	else
 	  {
 	    REAL_VALUE_TYPE r;
 	    REAL_VALUE_FROM_CONST_DOUBLE (r, x);
 	    assemble_real (r, GET_MODE (x), BITS_PER_WORD);
 	  }
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
  [(set_attr "length" "4")]
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
        {
          REAL_VALUE_TYPE r;
          REAL_VALUE_FROM_CONST_DOUBLE (r, operands[0]);
          assemble_real (r, GET_MODE (operands[0]), BITS_PER_WORD);
          break;
        }
      default:
        assemble_integer (operands[0], 8, BITS_PER_WORD, 1);
        break;
      }
    return \"\";
  }"
  [(set_attr "length" "8")]
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
        {
          REAL_VALUE_TYPE r;
          REAL_VALUE_FROM_CONST_DOUBLE (r, operands[0]);
          assemble_real (r, GET_MODE (operands[0]), BITS_PER_WORD);
          break;
        }
      default:
        assemble_integer (operands[0], 16, BITS_PER_WORD, 1);
        break;
      }
    return \"\";
  }"
  [(set_attr "length" "16")]
)

;; Miscellaneous Thumb patterns

(define_expand "tablejump"
  [(parallel [(set (pc) (match_operand:SI 0 "register_operand" ""))
	      (use (label_ref (match_operand 1 "" "")))])]
  "TARGET_THUMB1"
  "
  if (flag_pic)
    {
      /* Hopefully, CSE will eliminate this copy.  */
      rtx reg1 = copy_addr_to_reg (gen_rtx_LABEL_REF (Pmode, operands[1]));
      rtx reg2 = gen_reg_rtx (SImode);

      emit_insn (gen_addsi3 (reg2, operands[0], reg1));
      operands[0] = reg2;
    }
  "
)

;; NB never uses BX.
(define_insn "*thumb1_tablejump"
  [(set (pc) (match_operand:SI 0 "register_operand" "l*r"))
   (use (label_ref (match_operand 1 "" "")))]
  "TARGET_THUMB1"
  "mov\\t%|pc, %0"
  [(set_attr "length" "2")]
)

;; V5 Instructions,

(define_insn "clzsi2"
  [(set (match_operand:SI 0 "s_register_operand" "=r")
	(clz:SI (match_operand:SI 1 "s_register_operand" "r")))]
  "TARGET_32BIT && arm_arch5"
  "clz%?\\t%0, %1"
  [(set_attr "predicable" "yes")
   (set_attr "insn" "clz")])

(define_insn "rbitsi2"
  [(set (match_operand:SI 0 "s_register_operand" "=r")
	(unspec:SI [(match_operand:SI 1 "s_register_operand" "r")] UNSPEC_RBIT))]
  "TARGET_32BIT && arm_arch_thumb2"
  "rbit%?\\t%0, %1"
  [(set_attr "predicable" "yes")
   (set_attr "insn" "clz")])

(define_expand "ctzsi2"
 [(set (match_operand:SI           0 "s_register_operand" "")
       (ctz:SI (match_operand:SI  1 "s_register_operand" "")))]
  "TARGET_32BIT && arm_arch_thumb2"
  "
   {
     rtx tmp = gen_reg_rtx (SImode); 
     emit_insn (gen_rbitsi2 (tmp, operands[1]));
     emit_insn (gen_clzsi2 (operands[0], tmp));
   }
   DONE;
  "
)

;; V5E instructions.

(define_insn "prefetch"
  [(prefetch (match_operand:SI 0 "address_operand" "p")
	     (match_operand:SI 1 "" "")
	     (match_operand:SI 2 "" ""))]
  "TARGET_32BIT && arm_arch5e"
  "pld\\t%a0")

;; General predication pattern

(define_cond_exec
  [(match_operator 0 "arm_comparison_operator"
    [(match_operand 1 "cc_register" "")
     (const_int 0)])]
  "TARGET_32BIT"
  ""
)

(define_insn "prologue_use"
  [(unspec:SI [(match_operand:SI 0 "register_operand" "")] UNSPEC_PROLOGUE_USE)]
  ""
  "%@ %0 needed for prologue"
  [(set_attr "length" "0")]
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

(define_insn_and_split "thumb_eh_return"
  [(unspec_volatile [(match_operand:SI 0 "s_register_operand" "l")]
		    VUNSPEC_EH_RETURN)
   (clobber (match_scratch:SI 1 "=&l"))]
  "TARGET_THUMB1"
  "#"
  "&& reload_completed"
  [(const_int 0)]
  "
  {
    thumb_set_return_address (operands[0], operands[1]);
    DONE;
  }"
)


;; TLS support

(define_insn "load_tp_hard"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(unspec:SI [(const_int 0)] UNSPEC_TLS))]
  "TARGET_HARD_TP"
  "mrc%?\\tp15, 0, %0, c13, c0, 3\\t@ load_tp_hard"
  [(set_attr "predicable" "yes")]
)

;; Doesn't clobber R1-R3.  Must use r0 for the first operand.
(define_insn "load_tp_soft"
  [(set (reg:SI 0) (unspec:SI [(const_int 0)] UNSPEC_TLS))
   (clobber (reg:SI LR_REGNUM))
   (clobber (reg:SI IP_REGNUM))
   (clobber (reg:CC CC_REGNUM))]
  "TARGET_SOFT_TP"
  "bl\\t__aeabi_read_tp\\t@ load_tp_soft"
  [(set_attr "conds" "clob")]
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
   (set_attr "length" "4")]
)

;;

;; We only care about the lower 16 bits of the constant 
;; being inserted into the upper 16 bits of the register.
(define_insn "*arm_movtas_ze" 
  [(set (zero_extract:SI (match_operand:SI 0 "s_register_operand" "+r")
                   (const_int 16)
                   (const_int 16))
        (match_operand:SI 1 "const_int_operand" ""))]
  "arm_arch_thumb2"
  "movt%?\t%0, %L1"
 [(set_attr "predicable" "yes")
   (set_attr "length" "4")]
)

(define_insn "*arm_rev"
  [(set (match_operand:SI 0 "s_register_operand" "=r")
	(bswap:SI (match_operand:SI 1 "s_register_operand" "r")))]
  "TARGET_32BIT && arm_arch6"
  "rev%?\t%0, %1"
  [(set_attr "predicable" "yes")
   (set_attr "length" "4")]
)

(define_insn "*thumb1_rev"
  [(set (match_operand:SI 0 "s_register_operand" "=l")
	(bswap:SI (match_operand:SI 1 "s_register_operand" "l")))]
  "TARGET_THUMB1 && arm_arch6"
   "rev\t%0, %1"
  [(set_attr "length" "2")]
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

;; Vector bits common to IWMMXT and Neon
(include "vec-common.md")
;; Load the Intel Wireless Multimedia Extension patterns
(include "iwmmxt.md")
;; Load the VFP co-processor patterns
(include "vfp.md")
;; Thumb-2 patterns
(include "thumb2.md")
;; Neon patterns
(include "neon.md")
;; Synchronization Primitives
(include "sync.md")
;; Fixed-point patterns
(include "arm-fixed.md")
