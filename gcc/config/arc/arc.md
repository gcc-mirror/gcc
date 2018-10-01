;; Machine description of the Synopsys DesignWare ARC cpu for GNU C compiler
;; Copyright (C) 1994-2018 Free Software Foundation, Inc.

;; Sources derived from work done by Sankhya Technologies (www.sankhya.com) on
;; behalf of Synopsys Inc.

;;    Position Independent Code support added,Code cleaned up,
;;    Comments and Support For ARC700 instructions added by
;;    Saurabh Verma (saurabh.verma@codito.com)
;;    Ramana Radhakrishnan(ramana.radhakrishnan@codito.com)
;;
;;    Performance improvements by
;;    Joern Rennecke (joern.rennecke@embecosm.com)
;;

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

;; See file "rtl.def" for documentation on define_insn, match_*, et. al.

;; <op> dest, src         Two operand instruction's syntax
;; <op> dest, src1, src2  Three operand instruction's syntax

;; ARC and ARCompact PREDICATES:
;;
;;   comparison_operator   LT, GT, LE, GE, LTU, GTU, LEU, GEU, EQ, NE
;;   memory_operand        memory                         [m]
;;   immediate_operand     immediate constant             [IKLMNOP]
;;   register_operand      register                       [rq]
;;   general_operand       register, memory, constant     [rqmIKLMNOP]

;;  Note that the predicates are only used when selecting a pattern
;;  to determine if an operand is valid.

;;  The constraints then select which of the possible valid operands
;;  is present (and guide register selection). The actual assembly
;;  instruction is then selected on the basis of the constraints.

;; ARC and ARCompact CONSTRAINTS:
;;
;;   b  stack pointer                           r28
;;   f  frame pointer                           r27
;;   Rgp global pointer                         r26
;;   g  general reg, memory, constant
;;   m  memory
;;   p  memory address
;;   q  registers commonly used in
;;      16-bit insns                            r0-r3, r12-r15
;;   c  core registers				r0-r60, ap, pcl
;;   r  general registers                       r0-r28, blink, ap, pcl
;;
;;   H  fp 16-bit constant
;;   I signed 12-bit immediate (for ARCompact)
;;   K  unsigned 3-bit immediate (for ARCompact)
;;   L  unsigned 6-bit immediate (for ARCompact)
;;   M  unsinged 5-bit immediate (for ARCompact)
;;   O  unsinged 7-bit immediate (for ARCompact)
;;   P  unsinged 8-bit immediate (for ARCompact)
;;   N  constant '1' (for ARCompact)


;; TODO:
;; -> prefetch instruction

;;  -----------------------------------------------------------------------------

;; Include DFA scheduluers
(include ("arc600.md"))
(include ("arc700.md"))
(include ("arcEM.md"))
(include ("arcHS.md"))
(include ("arcHS4x.md"))

;; Predicates

(include ("predicates.md"))
(include ("constraints.md"))
;;  -----------------------------------------------------------------------------

;; UNSPEC Usage:
;; ~~~~~~~~~~~~
;;  -----------------------------------------------------------------------------
;;  Symbolic name  Value              Desc.
;;  -----------------------------------------------------------------------------
;;  UNSPEC_PLT       3        symbol to be referenced through the PLT
;;  UNSPEC_GOT       4        symbol to be rerenced through the GOT
;;  UNSPEC_GOTOFF    5        Local symbol.To be referenced relative to the
;;                            GOTBASE.(Referenced as @GOTOFF)
;;  UNSPEC_GOTOFFPC  6        Local symbol.  To be referenced pc-relative.
;;  ----------------------------------------------------------------------------

(define_c_enum "unspec" [
  DUMMY_0
  DUMMY_1
  DUMMY_2
  ARC_UNSPEC_PLT
  ARC_UNSPEC_GOT
  ARC_UNSPEC_GOTOFF
  ARC_UNSPEC_GOTOFFPC
  UNSPEC_TLS_GD
  UNSPEC_TLS_LD
  UNSPEC_TLS_IE
  UNSPEC_TLS_OFF
  UNSPEC_ARC_NORM
  UNSPEC_ARC_NORMW
  UNSPEC_ARC_SWAP
  UNSPEC_ARC_DIVAW
  UNSPEC_ARC_DIRECT
  UNSPEC_ARC_LP
  UNSPEC_ARC_CASESI
  UNSPEC_ARC_FFS
  UNSPEC_ARC_FLS
  UNSPEC_ARC_MEMBAR
  UNSPEC_ARC_DMACH
  UNSPEC_ARC_DMACHU
  UNSPEC_ARC_DMACWH
  UNSPEC_ARC_DMACWHU
  UNSPEC_ARC_QMACH
  UNSPEC_ARC_QMACHU
  UNSPEC_ARC_QMPYH
  UNSPEC_ARC_QMPYHU
  UNSPEC_ARC_VMAC2H
  UNSPEC_ARC_VMAC2HU
  UNSPEC_ARC_VMPY2H
  UNSPEC_ARC_VMPY2HU
  UNSPEC_ARC_STKTIE

  VUNSPEC_ARC_RTIE
  VUNSPEC_ARC_SYNC
  VUNSPEC_ARC_BRK
  VUNSPEC_ARC_FLAG
  VUNSPEC_ARC_SLEEP
  VUNSPEC_ARC_SWI
  VUNSPEC_ARC_CORE_READ
  VUNSPEC_ARC_CORE_WRITE
  VUNSPEC_ARC_LR
  VUNSPEC_ARC_SR
  VUNSPEC_ARC_TRAP_S
  VUNSPEC_ARC_UNIMP_S
  VUNSPEC_ARC_KFLAG
  VUNSPEC_ARC_CLRI
  VUNSPEC_ARC_SETI
  VUNSPEC_ARC_NOP
  VUNSPEC_ARC_STACK_IRQ
  VUNSPEC_ARC_DEXCL
  VUNSPEC_ARC_DEXCL_NORES
  VUNSPEC_ARC_LR_HIGH
  VUNSPEC_ARC_EX
  VUNSPEC_ARC_CAS
  VUNSPEC_ARC_SC
  VUNSPEC_ARC_LL
  VUNSPEC_ARC_BLOCKAGE
  ])

(define_constants
  [(R0_REG 0)
   (R1_REG 1)
   (R2_REG 2)
   (R3_REG 3)
   (R10_REG 10)
   (R12_REG 12)
   (SP_REG 28)
   (ILINK1_REGNUM 29)
   (ILINK2_REGNUM 30)
   (RETURN_ADDR_REGNUM 31)
   (MUL64_OUT_REG 58)
   (MUL32x16_REG 56)
   (ARCV2_ACC 58)

   (LP_COUNT 60)
   (CC_REG 61)
   (LP_START 144)
   (LP_END 145)
  ]
)

(define_attr "is_sfunc" "no,yes" (const_string "no"))

;; Insn type.  Used to default other attribute values.
; While the attribute is_sfunc is set for any call of a special function,
; the instruction type sfunc is used only for the special call sequence
; that loads the (pc-relative) function address into r12 and then calls
; via r12.

(define_attr "type"
  "move,load,store,cmove,unary,binary,compare,shift,uncond_branch,jump,branch,
   brcc,brcc_no_delay_slot,call,sfunc,call_no_delay_slot,
   multi,umulti, two_cycle_core,lr,sr,divaw,loop_setup,loop_end,return,
   misc,spfp,dpfp_mult,dpfp_addsub,mulmac_600,cc_arith,
   simd_vload, simd_vload128, simd_vstore, simd_vmove, simd_vmove_else_zero,
   simd_vmove_with_acc, simd_varith_1cycle, simd_varith_2cycle,
   simd_varith_with_acc, simd_vlogic, simd_vlogic_with_acc,
   simd_vcompare, simd_vpermute, simd_vpack, simd_vpack_with_acc,
   simd_valign, simd_valign_with_acc, simd_vcontrol,
   simd_vspecial_3cycle, simd_vspecial_4cycle, simd_dma, mul16_em, div_rem,
   fpu, fpu_fuse, fpu_sdiv, fpu_ddiv, fpu_cvt, block"
  (cond [(eq_attr "is_sfunc" "yes")
	 (cond [(match_test "!TARGET_LONG_CALLS_SET && (!TARGET_MEDIUM_CALLS || GET_CODE (PATTERN (insn)) != COND_EXEC)") (const_string "call")
		(match_test "flag_pic") (const_string "sfunc")]
	       (const_string "call_no_delay_slot"))]
	(const_string "binary")))

;; The following three attributes are mixed case so that they can be
;; used conveniently with the CALL_ATTR macro.
(define_attr "is_CALL" "no,yes"
  (cond [(eq_attr "is_sfunc" "yes") (const_string "yes")
	 (eq_attr "type" "call,call_no_delay_slot") (const_string "yes")]
	(const_string "no")))

(define_attr "is_SIBCALL" "no,yes" (const_string "no"))

(define_attr "is_NON_SIBCALL" "no,yes"
  (cond [(eq_attr "is_SIBCALL" "yes") (const_string "no")
	 (eq_attr "is_CALL" "yes") (const_string "yes")]
	(const_string "no")))

;; true for compact instructions (those with _s suffix)
;; "maybe" means compact unless we conditionalize the insn.
(define_attr "iscompact" "true,maybe,true_limm,maybe_limm,false"
  (cond [(eq_attr "type" "sfunc")
	 (const_string "maybe")]
	(const_string "false")))


; Is there an instruction that we are actually putting into the delay slot?
(define_attr "delay_slot_filled" "no,yes"
  (cond [(match_test "NEXT_INSN (PREV_INSN (insn)) == insn")
	 (const_string "no")
	 (match_test "!TARGET_AT_DBR_CONDEXEC
		      && JUMP_P (insn)
		      && INSN_ANNULLED_BRANCH_P (insn)
		      && !INSN_FROM_TARGET_P (NEXT_INSN (insn))")
	 (const_string "no")]
	(const_string "yes")))

; Is a delay slot present for purposes of shorten_branches?
; We have to take the length of this insn into account for forward branches
; even if we don't put the insn actually into a delay slot.
(define_attr "delay_slot_present" "no,yes"
  (cond [(match_test "NEXT_INSN (PREV_INSN (insn)) == insn")
	 (const_string "no")]
	(const_string "yes")))

; We can't use get_attr_length (NEXT_INSN (insn)) because this gives the
; length of a different insn with the same uid.
(define_attr "delay_slot_length" ""
  (cond [(match_test "NEXT_INSN (PREV_INSN (insn)) == insn")
	 (const_int 0)]
	(symbol_ref "get_attr_length (NEXT_INSN (PREV_INSN (insn)))
		     - get_attr_length (insn)")))

; for ARCv2 we need to disable/enable different instruction alternatives
(define_attr "cpu_facility" "std,av1,av2,fpx,cd"
  (const_string "std"))

; We should consider all the instructions enabled until otherwise
(define_attr "enabled" "no,yes"
  (cond [(and (eq_attr "cpu_facility" "av1")
	      (match_test "TARGET_V2"))
	 (const_string "no")

	 (and (eq_attr "cpu_facility" "av2")
	      (not (match_test "TARGET_V2")))
	 (const_string "no")

	 (and (eq_attr "cpu_facility" "fpx")
	      (match_test "TARGET_FP_DP_AX"))
	 (const_string "no")

	 (and (eq_attr "cpu_facility" "cd")
	      (not (and (match_test "TARGET_V2")
			(match_test "TARGET_CODE_DENSITY"))))
	 (const_string "no")
	 ]
	(const_string "yes")))

(define_attr "predicable" "no,yes" (const_string "no"))
;; if 'predicable' were not so brain-dead, we would specify:
;; (cond [(eq_attr "cond" "!canuse") (const_string "no")
;;        (eq_attr "iscompact" "maybe") (const_string "no")]
;;       (const_string "yes"))
;; and then for everything but calls, we could just set the cond attribute.

;; Condition codes: this one is used by final_prescan_insn to speed up
;; conditionalizing instructions.  It saves having to scan the rtl to see if
;; it uses or alters the condition codes.

;; USE: This insn uses the condition codes (eg: a conditional branch).
;; CANUSE: This insn can use the condition codes (for conditional execution).
;; SET: All condition codes are set by this insn.
;; SET_ZN: the Z and N flags are set by this insn.
;; SET_ZNC: the Z, N, and C flags are set by this insn.
;; CLOB: The condition codes are set to unknown values by this insn.
;; NOCOND: This insn can't use and doesn't affect the condition codes.

(define_attr "cond" "use,canuse,canuse_limm,canuse_limm_add,set,set_zn,clob,nocond"
  (cond
    [(and (eq_attr "predicable" "yes")
	  (eq_attr "is_sfunc" "no")
	  (eq_attr "delay_slot_filled" "no"))
     (const_string "canuse")

     (eq_attr "type" "call")
     (cond [(eq_attr "delay_slot_filled" "yes") (const_string "nocond")
	    (match_test "!flag_pic") (const_string "canuse_limm")]
	   (const_string "nocond"))

     (eq_attr "iscompact" "maybe,false")
     (cond [ (and (eq_attr "type" "move")
		  (match_operand 1 "immediate_operand" ""))
	     (if_then_else
		(ior (match_operand 1 "u6_immediate_operand" "")
		     (match_operand 1 "long_immediate_operand" ""))
		(const_string "canuse")
		(const_string "canuse_limm"))

	     (eq_attr "type" "binary")
	     (cond [(ne (symbol_ref "REGNO (operands[0])")
			(symbol_ref "REGNO (operands[1])"))
		    (const_string "nocond")
		    (match_operand 2 "register_operand" "")
		    (const_string "canuse")
		    (match_operand 2 "u6_immediate_operand" "")
		    (const_string "canuse")
		    (match_operand 2 "long_immediate_operand" "")
		    (const_string "canuse")
		    (match_operand 2 "const_int_operand" "")
		    (const_string "canuse_limm")]
		   (const_string "nocond"))

	     (eq_attr "type" "compare")
	     (const_string "set")

	     (eq_attr "type" "cmove,branch")
	     (const_string "use")

	     (eq_attr "is_sfunc" "yes")
	     (cond [(match_test "(TARGET_MEDIUM_CALLS
				  && !TARGET_LONG_CALLS_SET
				  && flag_pic)")
		    (const_string "canuse_limm_add")
		    (match_test "(TARGET_MEDIUM_CALLS
				  && !TARGET_LONG_CALLS_SET)")
		    (const_string "canuse_limm")]
		   (const_string "canuse"))

	    ]

	    (const_string "nocond"))]

      (cond [(eq_attr "type" "compare")
	     (const_string "set")

	     (eq_attr "type" "cmove,branch")
	     (const_string "use")

	    ]

	    (const_string "nocond"))))

/* ??? Having all these patterns gives ifcvt more freedom to generate
   inefficient code.  It seem to operate on the premise that
   register-register copies and registers are free.  I see better code
   with -fno-if-convert now than without.  */
(define_cond_exec
  [(match_operator 0 "proper_comparison_operator"
     [(reg CC_REG) (const_int 0)])]
  "true"
  "")

;; Length (in # of bytes, long immediate constants counted too).
;; ??? There's a nasty interaction between the conditional execution fsm
;; and insn lengths: insns with shimm values cannot be conditionally executed.
(define_attr "length" ""
  (cond
    [(eq_attr "iscompact" "true")
      (const_int 2)

     (eq_attr "iscompact" "maybe")
     (cond
       [(eq_attr "type" "sfunc")
	(cond [(match_test "GET_CODE (PATTERN (insn)) == COND_EXEC")
	       (const_int 12)]
	      (const_int 10))
	(match_test "GET_CODE (PATTERN (insn)) == COND_EXEC") (const_int 4)
	(match_test "find_reg_note (insn, REG_SAVE_NOTE, GEN_INT (1))")
	(const_int 4)]
      (const_int 2))

    (eq_attr "iscompact" "true_limm")
    (const_int 6)

    (eq_attr "iscompact" "maybe_limm")
    (cond [(match_test "GET_CODE (PATTERN (insn)) == COND_EXEC") (const_int 8)]
	  (const_int 6))

    (eq_attr "type" "load")
    (if_then_else
       (match_operand 1 "long_immediate_loadstore_operand" "")
       (const_int 8) (const_int 4))

    (eq_attr "type" "store")
    (if_then_else
      (ior (match_operand 0 "long_immediate_loadstore_operand" "")
	   (match_operand 1 "immediate_operand" ""))
      (const_int 8) (const_int 4))

    (eq_attr "type" "move,unary")
    (cond
      [(match_operand 1 "u6_immediate_operand" "") (const_int 4)
       (match_operand 1 "register_operand" "") (const_int 4)
       (match_operand 1 "long_immediate_operand" "") (const_int 8)
       (match_test "GET_CODE (PATTERN (insn)) == COND_EXEC") (const_int 8)]
      (const_int 4))

    (and (eq_attr "type" "shift")
	 (match_operand 1 "immediate_operand"))
		 (const_int 8)
    (eq_attr "type" "binary,shift")
    (if_then_else
       (ior (match_operand 2 "long_immediate_operand" "")
	    (and (ne (symbol_ref "REGNO (operands[0])")
		     (symbol_ref "REGNO (operands[1])"))
		 (eq (match_operand 2 "u6_immediate_operand" "")
		     (const_int 0))))

       (const_int 8) (const_int 4))

    (eq_attr "type" "cmove")
       (if_then_else (match_operand 1 "register_operand" "")
		     (const_int 4) (const_int 8))

    (eq_attr "type" "call_no_delay_slot") (const_int 8)
   ]

   (const_int 4))
)

;; The length here is the length of a single asm.  Unfortunately it might be
;; 4 or 8 so we must allow for 8.  That's ok though.  How often will users
;; lament asm's not being put in delay slots?
;;
(define_asm_attributes
  [(set_attr "length" "8")
   (set_attr "type" "multi")
   (set_attr "cond" "clob") ])

;; Delay slots.
;; The first two cond clauses and the default are necessary for correctness;
;; the remaining cond clause is mainly an optimization, as otherwise nops
;; would be inserted; however, if we didn't do this optimization, we would
;; have to be more conservative in our length calculations.

(define_attr "in_delay_slot" "false,true"
  (cond [(eq_attr "type" "uncond_branch,jump,branch,
			  call,sfunc,call_no_delay_slot,
			  brcc, brcc_no_delay_slot,loop_setup,loop_end")
	 (const_string "false")
	 (match_test "arc_write_ext_corereg (insn)")
	 (const_string "false")
	 (gt (symbol_ref "arc_hazard (prev_active_insn (insn),
				      next_active_insn (insn))")
	     (symbol_ref "(arc_hazard (prev_active_insn (insn), insn)
			   + arc_hazard (insn, next_active_insn (insn)))"))
	 (const_string "false")
	 (match_test "find_reg_note (insn, REG_SAVE_NOTE, GEN_INT (2))")
	 (const_string "false")
	 (eq_attr "iscompact" "maybe") (const_string "true")
	 ]

	 (if_then_else (eq_attr "length" "2,4")
		       (const_string "true")
		       (const_string "false"))))

; must not put an insn inside that refers to blink.
(define_attr "in_call_delay_slot" "false,true"
  (cond [(eq_attr "in_delay_slot" "false")
	 (const_string "false")
	 (match_test "arc_regno_use_in (RETURN_ADDR_REGNUM, PATTERN (insn))")
	 (const_string "false")]
	(const_string "true")))

(define_attr "in_sfunc_delay_slot" "false,true"
  (cond [(eq_attr "in_call_delay_slot" "false")
	 (const_string "false")
	 (match_test "arc_regno_use_in (12, PATTERN (insn))")
	 (const_string "false")]
	(const_string "true")))

;; Instructions that we can put into a delay slot and conditionalize.
(define_attr "cond_delay_insn" "no,yes"
  (cond [(eq_attr "cond" "!canuse") (const_string "no")
	 (eq_attr "type" "call,branch,uncond_branch,jump,brcc")
	 (const_string "no")
	 (match_test "find_reg_note (insn, REG_SAVE_NOTE, GEN_INT (2))")
	 (const_string "no")
	 (eq_attr "length" "2,4") (const_string "yes")]
	(const_string "no")))

(define_attr "in_ret_delay_slot" "no,yes"
  (cond [(eq_attr "in_delay_slot" "false")
	 (const_string "no")
	 (match_test "regno_clobbered_p
			(arc_return_address_register
			  (arc_compute_function_type (cfun)),
			 insn, SImode, 1)")
	 (const_string "no")]
	(const_string "yes")))

(define_attr "cond_ret_delay_insn" "no,yes"
  (cond [(eq_attr "in_ret_delay_slot" "no") (const_string "no")
	 (eq_attr "cond_delay_insn" "no") (const_string "no")]
	(const_string "yes")))

(define_attr "annul_ret_delay_insn" "no,yes"
  (cond [(eq_attr "cond_ret_delay_insn" "yes") (const_string "yes")
	 (match_test "TARGET_AT_DBR_CONDEXEC") (const_string "no")
	 (eq_attr "type" "!call,branch,uncond_branch,jump,brcc,return,sfunc")
	   (const_string "yes")]
   (const_string "no")))


;; Delay slot definition for ARCompact ISA
;; ??? FIXME:
;; When outputting an annul-true insn elegible for cond-exec
;; in a cbranch delay slot, unless optimizing for size, we use cond-exec
;; for ARC600; we could also use this for ARC700 if the branch can't be
;; unaligned and is at least somewhat likely (add parameter for this).

(define_delay (eq_attr "type" "call")
  [(eq_attr "in_call_delay_slot" "true")
   (eq_attr "in_call_delay_slot" "true")
   (nil)])

(define_delay (and (match_test "!TARGET_AT_DBR_CONDEXEC")
		   (eq_attr "type" "brcc"))
  [(eq_attr "in_delay_slot" "true")
   (eq_attr "in_delay_slot" "true")
   (nil)])

(define_delay (and (match_test "TARGET_AT_DBR_CONDEXEC")
		   (eq_attr "type" "brcc"))
  [(eq_attr "in_delay_slot" "true")
   (nil)
   (nil)])

(define_delay
  (eq_attr "type" "return")
  [(eq_attr "in_ret_delay_slot" "yes")
   (eq_attr "annul_ret_delay_insn" "yes")
   (eq_attr "cond_ret_delay_insn" "yes")])

(define_delay (eq_attr "type" "loop_end")
  [(eq_attr "in_delay_slot" "true")
   (eq_attr "in_delay_slot" "true")
   (nil)])

;; For ARC600, unexposing the delay sloy incurs a penalty also in the
;; non-taken case, so the only meaningful way to have an annull-true
;; filled delay slot is to conditionalize the delay slot insn.
(define_delay (and (match_test "TARGET_AT_DBR_CONDEXEC")
		   (eq_attr "type" "branch,uncond_branch,jump")
		   (match_test "!optimize_size"))
  [(eq_attr "in_delay_slot" "true")
   (eq_attr "cond_delay_insn" "yes")
   (eq_attr "cond_delay_insn" "yes")])

;; For ARC700, anything goes for annulled-true insns, since there is no
;; penalty for the unexposed delay slot when the branch is not taken,
;; however, we must avoid things that have a delay slot themselvese to
;; avoid confusing gcc.
(define_delay (and (match_test "!TARGET_AT_DBR_CONDEXEC")
		   (eq_attr "type" "branch,uncond_branch,jump")
		   (match_test "!optimize_size"))
  [(eq_attr "in_delay_slot" "true")
   (eq_attr "type" "!call,branch,uncond_branch,jump,brcc,return,sfunc")
   (eq_attr "cond_delay_insn" "yes")])

;; -mlongcall -fpic sfuncs use r12 to load the function address
(define_delay (eq_attr "type" "sfunc")
  [(eq_attr "in_sfunc_delay_slot" "true")
   (eq_attr "in_sfunc_delay_slot" "true")
   (nil)])
;; ??? need to use a working strategy for canuse_limm:
;; - either canuse_limm is not eligible for delay slots, and has no
;;   delay slots, or arc_reorg has to treat them as nocond, or it has to
;;   somehow modify them to become inelegible for delay slots if a decision
;;   is made that makes conditional execution required.

(define_attr "tune" "none,arc600,arc700_4_2_std,arc700_4_2_xmac, core_3, \
archs4x, archs4xd, archs4xd_slow"
  (const
   (cond [(symbol_ref "arc_tune == TUNE_ARC600")
	  (const_string "arc600")
	  (symbol_ref "arc_tune == TUNE_ARC700_4_2_STD")
	  (const_string "arc700_4_2_std")
	  (symbol_ref "arc_tune == TUNE_ARC700_4_2_XMAC")
	  (const_string "arc700_4_2_xmac")
	  (symbol_ref "arc_tune == ARC_TUNE_CORE_3")
	  (const_string "core_3")
	  (symbol_ref "arc_tune == TUNE_ARCHS4X")
	  (const_string "archs4x")
	  (ior (symbol_ref "arc_tune == TUNE_ARCHS4XD")
	       (symbol_ref "arc_tune == TUNE_ARCHS4XD_SLOW"))
	  (const_string "archs4xd")]
	 (const_string "none"))))

(define_attr "tune_arc700" "false,true"
  (if_then_else (eq_attr "tune" "arc700_4_2_std, arc700_4_2_xmac")
		(const_string "true")
		(const_string "false")))

(define_attr "tune_dspmpy" "none, slow, fast"
  (const
  (cond [(ior (symbol_ref "arc_tune == TUNE_ARCHS4X")
	      (symbol_ref "arc_tune == TUNE_ARCHS4XD"))
	 (const_string "fast")
	 (symbol_ref "arc_tune == TUNE_ARCHS4XD_SLOW")
	 (const_string "slow")]
	(const_string "none"))))

;; Move instructions.
(define_expand "movqi"
  [(set (match_operand:QI 0 "move_dest_operand" "")
	(match_operand:QI 1 "general_operand" ""))]
  ""
  "if (prepare_move_operands (operands, QImode)) DONE;")

; In order to allow the ccfsm machinery to do its work, the leading compact
; alternatives say 'canuse' - there is another alternative that will match
; when the condition codes are used.
; Rcq won't match if the condition is actually used; to avoid a spurious match
; via q, q is inactivated as constraint there.
; Likewise, the length of an alternative that might be shifted to conditional
; execution must reflect this, lest out-of-range branches are created.
; The iscompact attribute allows the epilogue expander to know for which
; insns it should lengthen the return insn.
(define_insn "*movqi_insn"
  [(set (match_operand:QI 0 "move_dest_operand" "=Rcq,Rcq#q,    w,Rcq#q,   h,w*l,w*l,???w,h,w*l,Rcq,  S,!*x,  r,r, Ucm,m,???m,  m,Usc")
	(match_operand:QI 1 "move_src_operand"  "  cL,   cP,Rcq#q,    P,hCm1, cL,  I,?Rac,i, ?i,  T,Rcq,Usd,Ucm,m,?Rac,c,?Rac,Cm3,i"))]
  "register_operand (operands[0], QImode)
   || register_operand (operands[1], QImode)"
  "@
   mov%? %0,%1%&
   mov%? %0,%1%&
   mov%? %0,%1%&
   mov%? %0,%1%&
   mov%? %0,%1%&
   mov%? %0,%1
   mov%? %0,%1
   mov%? %0,%1
   mov%? %0,%1
   mov%? %0,%1
   ldb%? %0,%1%&
   stb%? %1,%0%&
   ldb%? %0,%1%&
   xldb%U1 %0,%1
   ldb%U1%V1 %0,%1
   xstb%U0 %1,%0
   stb%U0%V0 %1,%0
   stb%U0%V0 %1,%0
   stb%U0%V0 %1,%0
   stb%U0%V0 %1,%0"
  [(set_attr "type" "move,move,move,move,move,move,move,move,move,move,load,store,load,load,load,store,store,store,store,store")
   (set_attr "iscompact" "maybe,maybe,maybe,true,true,false,false,false,maybe_limm,false,true,true,true,false,false,false,false,false,false,false")
   (set_attr "predicable" "yes,no,yes,no,no,yes,no,yes,yes,yes,no,no,no,no,no,no,no,no,no,no")
   (set_attr "cpu_facility" "av1,av1,av1,av2,av2,*,*,*,*,*,*,*,*,*,*,*,*,*,*,*")])

(define_expand "movhi"
  [(set (match_operand:HI 0 "move_dest_operand" "")
	(match_operand:HI 1 "general_operand" ""))]
  ""
  "if (prepare_move_operands (operands, HImode)) DONE;")

(define_insn "*movhi_insn"
  [(set (match_operand:HI 0 "move_dest_operand" "=Rcq,Rcq#q,    w,Rcq#q,   h,w*l,w*l,???w,Rcq#q,h,w*l,Rcq,  S,  r,r, Ucm,m,???m,  m,VUsc")
	(match_operand:HI 1 "move_src_operand" "   cL,   cP,Rcq#q,    P,hCm1, cL,  I,?Rac,    i,i, ?i,  T,Rcq,Ucm,m,?Rac,c,?Rac,Cm3,i"))]
  "register_operand (operands[0], HImode)
   || register_operand (operands[1], HImode)
   || (CONSTANT_P (operands[1])
       /* Don't use a LIMM that we could load with a single insn - we loose
	  delay-slot filling opportunities.  */
       && !satisfies_constraint_I (operands[1])
       && satisfies_constraint_Usc (operands[0]))"
  "@
   mov%? %0,%1%&
   mov%? %0,%1%&
   mov%? %0,%1%&
   mov%? %0,%1%&
   mov%? %0,%1%&
   mov%? %0,%1
   mov%? %0,%1
   mov%? %0,%1
   mov%? %0,%1%&
   mov%? %0,%1
   mov%? %0,%1
   ld%_%? %0,%1%&
   st%_%? %1,%0%&
   xld%_%U1 %0,%1
   ld%_%U1%V1 %0,%1
   xst%_%U0 %1,%0
   st%_%U0%V0 %1,%0
   st%_%U0%V0 %1,%0
   st%_%U0%V0 %1,%0
   st%_%U0%V0 %1,%0"
  [(set_attr "type" "move,move,move,move,move,move,move,move,move,move,move,load,store,load,load,store,store,store,store,store")
   (set_attr "iscompact" "maybe,maybe,maybe,true,true,false,false,false,maybe_limm,maybe_limm,false,true,true,false,false,false,false,false,false,false")
   (set_attr "predicable" "yes,no,yes,no,no,yes,no,yes,yes,yes,yes,no,no,no,no,no,no,no,no,no")
   (set_attr "cpu_facility" "av1,av1,av1,av2,av2,*,*,*,*,*,*,*,*,*,*,*,*,*,av2,*")])

(define_expand "movsi"
  [(set (match_operand:SI 0 "move_dest_operand" "")
	(match_operand:SI 1 "general_operand" ""))]
  ""
  "if (prepare_move_operands (operands, SImode)) DONE;")

; In order to allow the ccfsm machinery to do its work, the leading compact
; alternatives say 'canuse' - there is another alternative that will match
; when the condition codes are used.
; Rcq won't match if the condition is actually used; to avoid a spurious match
; via q, q is inactivated as constraint there.
; Likewise, the length of an alternative that might be shifted to conditional
; execution must reflect this, lest out-of-range branches are created.
; the iscompact attribute allows the epilogue expander to know for which
; insns it should lengthen the return insn.
; N.B. operand 1 of alternative 7 expands into pcl,symbol@gotpc .
(define_insn "*movsi_insn"                      ;   0     1     2     3    4  5    6   7   8   9   10    11  12  13    14  15   16  17  18     19     20  21  22    23    24 25 26    27 28  29  30   31
  [(set (match_operand:SI 0 "move_dest_operand" "=Rcq,Rcq#q,    w,Rcq#q,   h,w*l,w*l,  w,  w,  w,  w,  ???w, ?w,  w,Rcq#q,  h, w*l,Rcq,  S,   Us<,RcqRck,!*x,  r,!*Rsd,!*Rcd,r,Ucm,  Usd,m,???m,  m,VUsc")
	(match_operand:SI 1 "move_src_operand"  "  cL,   cP,Rcq#q,    P,hCm1, cL,  I,Crr,Clo,Chi,Cbi,?Rac*l,Cpc,Clb, ?Cal,Cal,?Cal,Uts,Rcq,RcqRck,   Us>,Usd,Ucm,  Usd,  Ucd,m,  w,!*Rzd,c,?Rac,Cm3, C32"))]
  "register_operand (operands[0], SImode)
   || register_operand (operands[1], SImode)
   || (CONSTANT_P (operands[1])
       /* Don't use a LIMM that we could load with a single insn - we loose
	  delay-slot filling opportunities.  */
       && !satisfies_constraint_I (operands[1])
       && satisfies_constraint_Usc (operands[0]))
   || (satisfies_constraint_Cm3 (operands[1])
      && memory_operand (operands[0], SImode))"
  "@
   mov%? %0,%1%&	;0
   mov%? %0,%1%&	;1
   mov%? %0,%1%&	;2
   mov%? %0,%1%&	;3
   mov%? %0,%1%&	;4
   mov%? %0,%1		;5
   mov%? %0,%1		;6
   ror %0,((%1*2+1) & 0x3f) ;7
   movl.cl %0,%1	;8
   movh.cl %0,%L1>>16   ;9
   * return INTVAL (operands[1]) & 0xffffff ? \"movbi.cl %0,%1 >> %p1,%p1,8;10\" : \"movbi.cl %0,%L1 >> 24,24,8;10\";
   mov%? %0,%1		;11
   add %0,%1		;12
   add %0,pcl,%1@pcl    ;13
   mov%? %0,%j1 	;14
   mov%? %0,%j1		;15
   mov%? %0,%j1		;16
   ld%? %0,%1		;17
   st%? %1,%0%&		;18
   * return arc_short_long (insn, \"push%? %1%&\", \"st%U0 %1,%0%&\");
   * return arc_short_long (insn, \"pop%? %0%&\",  \"ld%U1 %0,%1%&\");
   ld%? %0,%1%&		;21
   xld%U1 %0,%1		;22
   ld%? %0,%1%&		;23
   ld%? %0,%1%&		;24
   ld%U1%V1 %0,%1	;25
   xst%U0 %1,%0		;26
   st%? %1,%0%&		;27
   st%U0%V0 %1,%0	;28
   st%U0%V0 %1,%0	;29
   st%U0%V0 %1,%0	;30
   st%U0%V0 %1,%0	;31"
   ;                         0     1     2     3    4    5      6       7           8     9    10     11    12    13           14        15    16   17    18    19   20    21    22   23  24    25    26    27    28    29   30   31
  [(set_attr "type"       "move, move, move,move,move, move, move,two_cycle_core,shift,shift,shift, move,binary,binary,      move,      move, move,load,store,store,load,load, load,load,load, load,store,store,store,store,store,store")
   (set_attr "iscompact" "maybe,maybe,maybe,true,true,false,false,         false,false,false,false,false, false, false,maybe_limm,maybe_limm,false,true, true, true,true,true,false,true,true,false,false, true,false,false,false,false")
   ; Use default length for iscompact to allow for COND_EXEC.  But set length
   ; of Crr to 4.
   (set_attr "length" "*,*,*,*,*,4,4,4,4,4,4,4,8,8,*,*,*,*,*,*,*,*,4,*,4,*,*,*,*,*,*,8")
   (set_attr "predicable" "yes,no,yes,no,no,yes,no,no,no,no,no,yes,no,no,yes,yes,yes,no,no,no,no,no,no,no,no,no,no,no,no,no,no,no")
   (set_attr "cpu_facility" "av1,av1,av1,av2,av2,*,*,*,*,*,*,*,*,*,*,*,*,*,*,*,*,*,*,av2,av2,*,*,av2,*,*,av2,*")])

;; Sometimes generated by the epilogue code.  We don't want to
;; recognize these addresses in general, because the limm is costly,
;; and we can't use them for stores.  */
(define_insn "*movsi_pre_mod"
  [(set (match_operand:SI 0 "register_operand" "=w")
	(mem:SI (pre_modify
		  (reg:SI SP_REG)
		  (plus:SI (reg:SI SP_REG)
			   (match_operand 1 "immediate_operand" "Cal")))))]
  "reload_completed"
  "ld.a %0,[sp,%1]"
  [(set_attr "type" "load")
   (set_attr "length" "8")])

;; Store a value to directly to memory.  The location might also be cached.
;; Since the cached copy can cause a write-back at unpredictable times,
;; we first write cached, then we write uncached.
(define_insn "store_direct"
  [(set (match_operand:SI 0 "move_dest_operand" "=m")
      (unspec:SI [(match_operand:SI 1 "register_operand" "c")]
       UNSPEC_ARC_DIRECT))]
  ""
  "st%U0 %1,%0\;st%U0.di %1,%0"
  [(set_attr "type" "store")])

;; Combiner patterns for compare with zero
(define_mode_iterator SQH [QI HI])
(define_mode_attr SQH_postfix [(QI "b") (HI "%_")])

(define_code_iterator SEZ [sign_extend zero_extend])
(define_code_attr SEZ_prefix [(sign_extend "sex") (zero_extend "ext")])

(define_insn "*<SEZ_prefix>xt<SQH_postfix>_cmp0_noout"
  [(set (match_operand 0 "cc_set_register" "")
	(compare:CC_ZN (SEZ:SI (match_operand:SQH 1 "register_operand" "r"))
		       (const_int 0)))]
  ""
  "<SEZ_prefix><SQH_postfix>.f\\t0,%1"
  [(set_attr "type" "compare")
   (set_attr "cond" "set_zn")])

(define_insn "*<SEZ_prefix>xt<SQH_postfix>_cmp0"
  [(set (match_operand 0 "cc_set_register" "")
	(compare:CC_ZN (SEZ:SI (match_operand:SQH 1 "register_operand" "r"))
		       (const_int 0)))
   (set (match_operand:SI 2 "register_operand" "=r")
	(SEZ:SI (match_dup 1)))]
  ""
  "<SEZ_prefix><SQH_postfix>.f\\t%2,%1"
  [(set_attr "type" "compare")
   (set_attr "cond" "set_zn")])

(define_insn "*xbfu_cmp0_noout"
  [(set (match_operand 0 "cc_set_register" "")
	(compare:CC_Z
	 (zero_extract:SI
	  (match_operand:SI 1 "register_operand"  "  r,r")
	  (match_operand:SI 2 "const_int_operand" "C3p,n")
	  (match_operand:SI 3 "const_int_operand" "  n,n"))
	 (const_int 0)))]
  "TARGET_HS && TARGET_BARREL_SHIFTER"
  {
   int assemble_op2 = (((INTVAL (operands[2]) - 1) & 0x1f) << 5) | (INTVAL (operands[3]) & 0x1f);
   operands[2] = GEN_INT (assemble_op2);
   return "xbfu%?.f\\t0,%1,%2";
  }
  [(set_attr "type"       "shift")
   (set_attr "iscompact"  "false")
   (set_attr "length"     "4,8")
   (set_attr "predicable" "no")
   (set_attr "cond"       "set_zn")])

(define_insn "*xbfu_cmp0"
  [(set (match_operand 4 "cc_set_register" "")
	(compare:CC_Z
	 (zero_extract:SI
	  (match_operand:SI 1 "register_operand"  "0  ,r,0")
	  (match_operand:SI 2 "const_int_operand" "C3p,n,n")
	  (match_operand:SI 3 "const_int_operand" "n  ,n,n"))
	 (const_int 0)))
   (set (match_operand:SI 0 "register_operand"  "=r,r,r")
	(zero_extract:SI (match_dup 1) (match_dup 2) (match_dup 3)))]
  "TARGET_HS && TARGET_BARREL_SHIFTER"
  {
   int assemble_op2 = (((INTVAL (operands[2]) - 1) & 0x1f) << 5) | (INTVAL (operands[3]) & 0x1f);
   operands[2] = GEN_INT (assemble_op2);
   return "xbfu%?.f\\t%0,%1,%2";
  }
  [(set_attr "type"       "shift")
   (set_attr "iscompact"  "false")
   (set_attr "length"     "4,8,8")
   (set_attr "predicable" "yes,no,yes")
   (set_attr "cond"       "set_zn")])

; splitting to 'tst' allows short insns and combination into brcc.
(define_insn_and_split "*movsi_set_cc_insn"
  [(set (match_operand 2 "cc_set_register" "")
	(match_operator 3 "zn_compare_operator"
			[(match_operand:SI 1 "nonmemory_operand" "rL,rI,Cal")
			 (const_int 0)]))
   (set (match_operand:SI 0 "register_operand" "=r,r,r")
	(match_dup 1))]
  ""
  "mov%?.f\\t%0,%1"
  "reload_completed && operands_match_p (operands[0], operands[1])"
  [(set (match_dup 2) (match_dup 3))]
  ""
  [(set_attr "type" "compare")
   (set_attr "predicable" "yes,no,yes")
   (set_attr "cond" "set_zn")
   (set_attr "length" "4,4,8")])

(define_insn "unary_comparison"
  [(set (match_operand:CC_ZN 0 "cc_set_register" "")
	(match_operator:CC_ZN 3 "zn_compare_operator"
	  [(match_operator:SI 2 "unary_operator"
	     [(match_operand:SI 1 "register_operand" "c")])
	   (const_int 0)]))]
  ""
  "%O2.f 0,%1"
  [(set_attr "type" "compare")
   (set_attr "cond" "set_zn")])


; this pattern is needed by combiner for cases like if (c=(~b)) { ... }
(define_insn "*unary_comparison_result_used"
  [(set (match_operand 2 "cc_register" "")
	(match_operator 4 "zn_compare_operator"
	  [(match_operator:SI 3 "unary_operator"
	     [(match_operand:SI 1 "register_operand" "c")])
	       (const_int 0)]))
   (set (match_operand:SI 0 "register_operand" "=w")
	(match_dup 3))]
  ""
  "%O3.f %0,%1"
  [(set_attr "type" "compare")
   (set_attr "cond" "set_zn")
   (set_attr "length" "4")])

; reload is too stingy with reloads for Rrq/Cbf/Rrq when it sees
; a c/???Cal/X alternative, so we say it's c/???Cal/c instead,
; even if we don't need the clobber.
(define_insn_and_split "*tst_movb"
  [(set
     (match_operand 0 "cc_register" "")
     (match_operator 4 "zn_compare_operator"
       [(and:SI
	  (match_operand:SI 1 "register_operand"  "%Rcq,Rcq, c,  c,  c,  c,Rrq,  3,  c")
	  (match_operand:SI 2 "nonmemory_operand"  "Rcq,C0p,cI,C1p,Ccp,Chs,Cbf,Cbf,???Cal"))
	(const_int 0)]))
   (clobber (match_scratch:SI 3 "=X,X,X,X,X,X,Rrq,Rrq,c"))]
  "TARGET_NPS_BITOPS"
  "movb.f.cl %3,%1,%p2,%p2,%s2"
  "TARGET_NPS_BITOPS && reload_completed
   && (extract_constrain_insn_cached (insn), (which_alternative & ~1) != 6)"
  [(set (match_dup 0) (match_dup 4))])

(define_insn "*tst"
  [(set
     (match_operand 0 "cc_register" "")
     (match_operator 3 "zn_compare_operator"
       [(and:SI
	  (match_operand:SI 1 "register_operand"
	   "%Rcq,Rcq, c, c, c,  c,  c,  c")
	  (match_operand:SI 2 "nonmemory_operand"
	   " Rcq,C0p,cI,cL,C1p,Ccp,Chs,Cal"))
	(const_int 0)]))]
  "reload_completed
   || !satisfies_constraint_Cbf (operands[2])
   || satisfies_constraint_C0p (operands[2])
   || satisfies_constraint_I (operands[2])
   || satisfies_constraint_C1p (operands[2])
   || satisfies_constraint_Chs (operands[2])"
  "*
    switch (which_alternative)
    {
    case 0: case 2: case 3: case 7:
      return \"tst%? %1,%2\";
    case 1:
      return \"btst%? %1,%z2\";
    case 4:
      return \"bmsk%?.f 0,%1,%Z2%&\";
    case 5:
      return \"bclr%?.f 0,%1,%M2%&\";
    case 6:
      return \"asr.f 0,%1,%p2\";
    default:
      gcc_unreachable ();
    }
  "
  [(set_attr "iscompact" "maybe,maybe,false,false,false,false,false,false")
   (set_attr "type" "compare,compare,compare,compare,compare,compare,binary,compare")
   (set_attr "length" "*,*,4,4,4,4,4,8")
   (set_attr "predicable" "no,yes,no,yes,no,no,no,yes")
   (set_attr "cond" "set_zn")])

; ??? Sometimes, if an AND with a constant can be expressed as a zero_extract,
; combine will do that and not try the AND.

; It would take 66 constraint combinations to describe the zero_extract
; constants that are covered by the 12-bit signed constant for tst
; (excluding the ones that are better done by mov or btst).
; so we rather use an extra pattern for tst;
; since this is about constants, reload shouldn't care.
(define_insn "*tst_bitfield_tst"
  [(set (match_operand:CC_ZN 0 "cc_set_register" "")
	(match_operator 4 "zn_compare_operator"
	  [(zero_extract:SI
	     (match_operand:SI 1 "register_operand"  "c")
	     (match_operand:SI 2 "const_int_operand" "n")
	     (match_operand:SI 3 "const_int_operand" "n"))
	   (const_int 0)]))]
  "INTVAL (operands[2]) > 1
   && (INTVAL (operands[3]) + INTVAL (operands[2]) <= 11
       || (INTVAL (operands[3]) <= 11
	   && INTVAL (operands[3]) + INTVAL (operands[2]) == 32))"
  "tst %1,((1<<%2)-1)<<%3"
  [(set_attr "type" "compare")
   (set_attr "cond" "set_zn")
   (set_attr "length" "4")])

; Likewise for asr.f.
(define_insn "*tst_bitfield_asr"
  [(set (match_operand:CC_ZN 0 "cc_set_register" "")
	(match_operator 4 "zn_compare_operator"
	  [(zero_extract:SI
	     (match_operand:SI 1 "register_operand"  "c")
	     (match_operand:SI 2 "const_int_operand" "n")
	     (match_operand:SI 3 "const_int_operand" "n"))
	   (const_int 0)]))]
  "INTVAL (operands[2]) > 1
   && INTVAL (operands[3]) + INTVAL (operands[2]) == 32"
  "asr.f 0,%1,%3"
  [(set_attr "type" "shift")
   (set_attr "cond" "set_zn")
   (set_attr "length" "4")])

(define_insn "*tst_bitfield"
  [(set (match_operand:CC_ZN 0 "cc_set_register" "")
	(match_operator 5 "zn_compare_operator"
	  [(zero_extract:SI
	     (match_operand:SI 1 "register_operand" "%Rcqq,c,  c,Rrq,c")
	     (match_operand:SI 2 "const_int_operand"    "N,N,  n,Cbn,n")
	     (match_operand:SI 3 "const_int_operand"    "n,n,C_0,Cbn,n"))
	   (const_int 0)]))
   (clobber (match_scratch:SI 4 "=X,X,X,Rrq,X"))]
  ""
  "@
   btst%? %1,%3
   btst %1,%3
   bmsk.f 0,%1,%2-1
   movb.f.cl %4,%1,%3,%3,%2
   and.f 0,%1,((1<<%2)-1)<<%3"
  [(set_attr "iscompact" "maybe,false,false,false,false")
   (set_attr "type" "compare,compare,compare,shift,compare")
   (set_attr "cond" "set_zn")
   (set_attr "length" "*,4,4,4,8")])

;; The next two patterns are for plos, ior, xor, and, and mult.
(define_insn "*commutative_binary_cmp0_noout"
  [(set (match_operand 0 "cc_set_register" "")
	(match_operator 4 "zn_compare_operator"
	  [(match_operator:SI 3 "commutative_operator"
	     [(match_operand:SI 1 "register_operand" "%r,r")
	      (match_operand:SI 2 "nonmemory_operand" "rL,Cal")])
	   (const_int 0)]))]
  ""
  "%O3.f\\t0,%1,%2"
  [(set_attr "type" "compare")
   (set_attr "cond" "set_zn")
   (set_attr "length" "4,8")])

(define_insn "*commutative_binary_cmp0"
  [(set (match_operand 3 "cc_set_register" "")
	(match_operator 5 "zn_compare_operator"
	  [(match_operator:SI 4 "commutative_operator"
	     [(match_operand:SI 1 "register_operand"  "%0, 0,r,r")
	      (match_operand:SI 2 "nonmemory_operand" "rL,rI,r,Cal")])
	   (const_int 0)]))
   (set (match_operand:SI 0 "register_operand" "=r,r,r,r")
	(match_dup 4))]
  ""
  "%O4.f\\t%0,%1,%2"
  [(set_attr "type" "compare")
   (set_attr "cond" "set_zn")
   (set_attr "predicable" "yes,yes,no,no")
   (set_attr "length" "4,4,4,8")])

; for flag setting 'add' instructions like if (a+b) { ...}
; the combiner needs this pattern
(define_insn "*addsi_compare"
  [(set (reg:CC_ZN CC_REG)
	(compare:CC_ZN (match_operand:SI 0 "register_operand" "c")
		       (neg:SI (match_operand:SI 1 "register_operand" "c"))))]
  ""
  "add.f 0,%0,%1"
  [(set_attr "cond" "set")
   (set_attr "type" "compare")
   (set_attr "length" "4")])

; for flag setting 'add' instructions like if (a+b < a) { ...}
; the combiner needs this pattern
(define_insn "addsi_compare_2"
  [(set (reg:CC_C CC_REG)
	(compare:CC_C (plus:SI (match_operand:SI 0 "register_operand" "c,c")
			       (match_operand:SI 1 "nonmemory_operand" "cL,Cal"))
		      (match_dup 0)))]
  ""
  "add.f 0,%0,%1"
  [(set_attr "cond" "set")
   (set_attr "type" "compare")
   (set_attr "length" "4,8")])

(define_insn "*addsi_compare_3"
  [(set (reg:CC_C CC_REG)
	(compare:CC_C (plus:SI (match_operand:SI 0 "register_operand" "c")
			       (match_operand:SI 1 "register_operand" "c"))
		      (match_dup 1)))]
  ""
  "add.f 0,%0,%1"
  [(set_attr "cond" "set")
   (set_attr "type" "compare")
   (set_attr "length" "4")])

; this pattern is needed by combiner for cases like if (c=a+b) { ... }
(define_insn "*commutative_binary_comparison_result_used"
  [(set (match_operand 3 "cc_register" "")
	(match_operator 5 "zn_compare_operator"
	  ; We can accept any commutative operator except mult because
	  ; our 'w' class below could try to use LP_COUNT.
	  [(match_operator:SI 4 "commutative_operator_sans_mult"
	     [(match_operand:SI 1 "register_operand" "c,0,c")
	      (match_operand:SI 2 "nonmemory_operand" "cL,I,?Cal")])
	   (const_int 0)]))
   (set (match_operand:SI 0 "register_operand" "=w,w,w")
	(match_dup 4))]
  ""
  "%O4.f %0,%1,%2 ; non-mult commutative"
  [(set_attr "type" "compare,compare,compare")
   (set_attr "cond" "set_zn,set_zn,set_zn")
   (set_attr "length" "4,4,8")])

; a MULT-specific version of this pattern to avoid touching the
; LP_COUNT register
(define_insn "*commutative_binary_mult_comparison_result_used"
  [(set (match_operand 3 "cc_register" "")
	(match_operator 5 "zn_compare_operator"
	  [(match_operator:SI 4 "mult_operator"
	     [(match_operand:SI 1 "register_operand" "c,0,c")
	      (match_operand:SI 2 "nonmemory_operand" "cL,I,?Cal")])
	   (const_int 0)]))
	; Make sure to use the W class to not touch LP_COUNT.
   (set (match_operand:SI 0 "register_operand" "=W,W,W")
	(match_dup 4))]
  "!TARGET_ARC600_FAMILY"
  "%O4.f %0,%1,%2 ; mult commutative"
  [(set_attr "type" "compare,compare,compare")
   (set_attr "cond" "set_zn,set_zn,set_zn")
   (set_attr "length" "4,4,8")])

(define_insn "*noncommutative_binary_cmp0"
  [(set (match_operand 3 "cc_set_register" "")
	(match_operator 5 "zn_compare_operator"
	  [(match_operator:SI 4 "noncommutative_operator"
	     [(match_operand:SI 1 "register_operand"   "0,r,0,  0,r")
	      (match_operand:SI 2 "nonmemory_operand" "rL,r,I,Cal,Cal")])
	   (const_int 0)]))
   (set (match_operand:SI 0 "register_operand" "=r,r,r,r,r")
	(match_dup 4))]
  ""
  "%O4%?.f\\t%0,%1,%2"
  [(set_attr "type" "compare")
   (set_attr "cond" "set_zn")
   (set_attr "predicable" "yes,no,no,yes,no")
   (set_attr "length" "4,4,4,8,8")])

(define_insn "*noncommutative_binary_cmp0_noout"
  [(set (match_operand 0 "cc_set_register" "")
	(match_operator 3 "zn_compare_operator"
	  [(match_operator:SI 4 "noncommutative_operator"
	     [(match_operand:SI 1 "register_operand"   "r,r")
	      (match_operand:SI 2 "nonmemory_operand" "rL,Cal")])
	   (const_int 0)]))]
  ""
  "%O4.f\\t0,%1,%2"
  [(set_attr "type" "compare")
   (set_attr "cond" "set_zn")
   (set_attr "length" "4,8")])

;;rsub variants
(define_insn "*rsub_cmp0"
  [(set (match_operand 4 "cc_set_register" "")
	(match_operator 3 "zn_compare_operator"
	  [(minus:SI
	    (match_operand:SI 1 "nonmemory_operand" "rL,Cal")
	    (match_operand:SI 2 "register_operand"   "r,r"))
	   (const_int 0)]))
   (set (match_operand:SI 0 "register_operand" "=r,r")
	(minus:SI (match_dup 1) (match_dup 2)))]
  ""
  "rsub.f\\t%0,%2,%1"
  [(set_attr "type" "compare")
   (set_attr "cond" "set_zn")
   (set_attr "length" "4,8")])

(define_insn "*rsub_cmp0_noout"
  [(set (match_operand 0 "cc_set_register" "")
	(match_operator 3 "zn_compare_operator"
	  [(minus:SI
	    (match_operand:SI 1 "nonmemory_operand" "rL,Cal")
	    (match_operand:SI 2 "register_operand"   "r,r"))
	   (const_int 0)]))]
  ""
  "rsub.f\\t0,%2,%1"
  [(set_attr "type" "compare")
   (set_attr "cond" "set_zn")
   (set_attr "length" "4,8")])

(define_expand "bic_f_zn"
  [(parallel
     [(set (reg:CC_ZN CC_REG)
	   (compare:CC_ZN
	     (and:SI (match_operand:SI 1 "register_operand" "")
		     (not:SI (match_operand:SI 2 "nonmemory_operand" "")))
	   (const_int 0)))
      (set (match_operand:SI 0 "register_operand" "")
	   (and:SI (match_dup 1) (not:SI (match_dup 2))))])]
  "")

(define_insn "*bic_f"
  [(set (match_operand 3 "cc_register" "=Rcc,Rcc,Rcc")
	(match_operator 4 "zn_compare_operator"
	  [(and:SI (match_operand:SI 1 "register_operand" "c,0,c")
		   (not:SI
		     (match_operand:SI 2 "nonmemory_operand" "cL,I,?Cal")))
	   (const_int 0)]))
   (set (match_operand:SI 0 "register_operand" "=w,w,w")
	(and:SI (match_dup 1) (not:SI (match_dup 2))))]
  ""
  "bic.f %0,%1,%2"
  [(set_attr "type" "compare,compare,compare")
   (set_attr "cond" "set_zn,set_zn,set_zn")
   (set_attr "length" "4,4,8")])

(define_expand "movdi"
  [(set (match_operand:DI 0 "move_dest_operand" "")
	(match_operand:DI 1 "general_operand" ""))]
  ""
  "
  if (prepare_move_operands (operands, DImode))
    DONE;
  ")

(define_insn_and_split "*movdi_insn"
  [(set (match_operand:DI 0 "move_dest_operand"      "=w, w,r,   m")
	(match_operand:DI 1 "move_double_src_operand" "c,Hi,m,cCm3"))]
  "register_operand (operands[0], DImode)
   || register_operand (operands[1], DImode)
   || (satisfies_constraint_Cm3 (operands[1])
      && memory_operand (operands[0], DImode))"
  "*
{
  switch (which_alternative)
    {
    default:
      return \"#\";

    case 2:
    if (TARGET_LL64
        && memory_operand (operands[1], DImode)
	&& even_register_operand (operands[0], DImode))
      return \"ldd%U1%V1 %0,%1%&\";
    return \"#\";

    case 3:
    if (TARGET_LL64
	&& memory_operand (operands[0], DImode)
	&& (even_register_operand (operands[1], DImode)
	    || satisfies_constraint_Cm3 (operands[1])))
     return \"std%U0%V0 %1,%0\";
    return \"#\";
    }
}"
  "reload_completed"
  [(const_int 0)]
  {
   arc_split_move (operands);
   DONE;
  }
  [(set_attr "type" "move,move,load,store")
   ;; ??? The ld/st values could be 4 if it's [reg,bignum].
   (set_attr "length" "8,16,*,*")])


;; Floating point move insns.

(define_expand "movsf"
  [(set (match_operand:SF 0 "move_dest_operand" "")
	(match_operand:SF 1 "general_operand" ""))]
  ""
  "if (prepare_move_operands (operands, SFmode)) DONE;")

(define_insn "*movsf_insn"
  [(set (match_operand:SF 0 "move_dest_operand"   "=h,h,   r,r,  q,S,Usc,r,m")
	(match_operand:SF 1 "move_src_operand"  "hCfZ,E,rCfZ,E,Uts,q,  E,m,r"))]
  "register_operand (operands[0], SFmode)
   || register_operand (operands[1], SFmode)"
  "@
   mov%?\\t%0,%1
   mov%?\\t%0,%1 ; %A1
   mov%?\\t%0,%1
   mov%?\\t%0,%1 ; %A1
   ld%?%U1\\t%0,%1
   st%?\\t%1,%0
   st%U0%V0\\t%1,%0
   ld%U1%V1\\t%0,%1
   st%U0%V0\\t%1,%0"
  [(set_attr "type" "move,move,move,move,load,store,store,load,store")
   (set_attr "predicable" "no,no,yes,yes,no,no,no,no,no")
   (set_attr "length" "*,*,4,*,*,*,*,*,*")
   (set_attr "iscompact" "true,true_limm,false,false,true,true,false,false,false")])

(define_expand "movdf"
  [(set (match_operand:DF 0 "move_dest_operand" "")
	(match_operand:DF 1 "general_operand" ""))]
  ""
  "if (prepare_move_operands (operands, DFmode)) DONE;")

(define_insn_and_split "*movdf_insn"
  [(set (match_operand:DF 0 "move_dest_operand"      "=D,r,c,c,r,m")
	(match_operand:DF 1 "move_double_src_operand" "r,D,c,E,m,c"))]
  "register_operand (operands[0], DFmode) || register_operand (operands[1], DFmode)"
  "*
{
 switch (which_alternative)
   {
    default:
      return \"#\";
    case 4:
    if (TARGET_LL64
	&& ((even_register_operand (operands[0], DFmode)
	     && memory_operand (operands[1], DFmode))
	    || (memory_operand (operands[0], DFmode)
	        && even_register_operand (operands[1], DFmode))))
      return \"ldd%U1%V1 %0,%1%&\";
    return \"#\";

    case 5:
    if (TARGET_LL64
	&& ((even_register_operand (operands[0], DFmode)
	     && memory_operand (operands[1], DFmode))
	    || (memory_operand (operands[0], DFmode)
		&& even_register_operand (operands[1], DFmode))))
     return \"std%U0%V0 %1,%0\";
    return \"#\";
   }
}"
  "reload_completed"
  [(const_int 0)]
  {
   arc_split_move (operands);
   DONE;
  }
  [(set_attr "type" "move,move,move,move,load,store")
   (set_attr "predicable" "no,no,yes,yes,no,no")
   ;; ??? The ld/st values could be 16 if it's [reg,bignum].
   (set_attr "length" "4,16,8,16,16,16")])

(define_insn_and_split "*movdf_insn_nolrsr"
  [(set (match_operand:DF 0 "register_operand"       "=r")
	(match_operand:DF 1 "arc_double_register_operand" "D"))
   (use (match_operand:SI 2 "" "N")) ; aka const1_rtx
   ]
  "TARGET_DPFP && TARGET_DPFP_DISABLE_LRSR"
  "#"
  "&& 1"
  [
    ; mov r0, 0
    (set (match_dup 0) (match_dup 3))

    ; daddh?? r1, r0, r0
    (parallel [
    	(set (match_dup 1) (plus:DF (match_dup 1) (match_dup 0)))
    	(use (const_int 1))
    	(use (const_int 1))
	(use (match_dup 0)) ; used to block can_combine_p
    	(set (match_dup 0) (plus:DF (match_dup 1) (match_dup 0))) ; r1 in op 0
    ])

    ; We have to do this twice, once to read the value into R0 and
    ; second time to put back the contents which the first DEXCLx
    ; will have overwritten
    ; dexcl2 r0, r1, r0
    (parallel [
	       (set (match_dup 4) ; aka r0result
				  ; aka DF, r1, r0
		    (unspec_volatile:SI [(match_dup 5) (match_dup 4)]
					VUNSPEC_ARC_DEXCL))
	       (clobber (match_dup 1))
	       ])
    ; Generate the second, which makes sure operand5 and operand4 values
    ; are put back in the Dx register properly.
    (set (match_dup 1) (unspec_volatile:DF
			[(match_dup 5) (match_dup 4)]
			VUNSPEC_ARC_DEXCL_NORES))

    ; Note: we cannot use a (clobber (match_scratch)) here because
    ; the combine pass will end up replacing uses of it with 0
  ]
  "operands[3] = CONST0_RTX (DFmode);
   operands[4] = simplify_gen_subreg (SImode, operands[0], DFmode, 0);
   operands[5] = simplify_gen_subreg (SImode, operands[0], DFmode, 4);"
  [(set_attr "type" "move")])

;; Load/Store with update instructions.
;;
;; Some of these we can get by using pre-decrement or pre-increment, but the
;; hardware can also do cases where the increment is not the size of the
;; object.
;;
;; In all these cases, we use operands 0 and 1 for the register being
;; incremented because those are the operands that local-alloc will
;; tie and these are the pair most likely to be tieable (and the ones
;; that will benefit the most).
;;
;; We use match_operator here because we need to know whether the memory
;; object is volatile or not.


;; Note: loadqi_update has no 16-bit variant
(define_insn "*loadqi_update"
  [(set (match_operand:QI 3 "dest_reg_operand" "=r,r")
        (match_operator:QI 4 "any_mem_operand"
         [(plus:SI (match_operand:SI 1 "register_operand" "0,0")
                   (match_operand:SI 2 "nonmemory_operand" "rCm2,Cal"))]))
   (set (match_operand:SI 0 "dest_reg_operand" "=r,r")
	(plus:SI (match_dup 1) (match_dup 2)))]
  ""
  "ldb.a%V4 %3,[%0,%2]"
  [(set_attr "type" "load,load")
   (set_attr "length" "4,8")])

(define_insn "*load_zeroextendqisi_update"
  [(set (match_operand:SI 3 "dest_reg_operand" "=r,r")
	(zero_extend:SI (match_operator:QI 4 "any_mem_operand"
			 [(plus:SI (match_operand:SI 1 "register_operand" "0,0")
			           (match_operand:SI 2 "nonmemory_operand" "rCm2,Cal"))])))
   (set (match_operand:SI 0 "dest_reg_operand" "=r,r")
	(plus:SI (match_dup 1) (match_dup 2)))]
  ""
  "ldb.a%V4 %3,[%0,%2]"
  [(set_attr "type" "load,load")
   (set_attr "length" "4,8")])

(define_insn "*load_signextendqisi_update"
  [(set (match_operand:SI 3 "dest_reg_operand" "=r,r")
	(sign_extend:SI (match_operator:QI 4 "any_mem_operand"
			 [(plus:SI (match_operand:SI 1 "register_operand" "0,0")
			           (match_operand:SI 2 "nonmemory_operand" "rCm2,Cal"))])))
   (set (match_operand:SI 0 "dest_reg_operand" "=r,r")
	(plus:SI (match_dup 1) (match_dup 2)))]
  ""
  "ldb.x.a%V4 %3,[%0,%2]"
  [(set_attr "type" "load,load")
   (set_attr "length" "4,8")])

(define_insn "*storeqi_update"
  [(set (match_operator:QI 4 "any_mem_operand"
	 [(plus:SI (match_operand:SI 1 "register_operand" "0")
	           (match_operand:SI 2 "short_immediate_operand" "I"))])
	(match_operand:QI 3 "register_operand" "c"))
   (set (match_operand:SI 0 "dest_reg_operand" "=w")
	(plus:SI (match_dup 1) (match_dup 2)))]
  ""
  "stb.a%V4 %3,[%0,%2]"
  [(set_attr "type" "store")
   (set_attr "length" "4")])

;; ??? pattern may have to be re-written
;; Note: no 16-bit variant for this pattern
(define_insn "*loadhi_update"
  [(set (match_operand:HI 3 "dest_reg_operand" "=r,r")
	(match_operator:HI 4 "any_mem_operand"
	 [(plus:SI (match_operand:SI 1 "register_operand" "0,0")
	           (match_operand:SI 2 "nonmemory_operand" "rCm2,Cal"))]))
   (set (match_operand:SI 0 "dest_reg_operand" "=w,w")
	(plus:SI (match_dup 1) (match_dup 2)))]
  ""
  "ld%_.a%V4 %3,[%0,%2]"
  [(set_attr "type" "load,load")
   (set_attr "length" "4,8")])

(define_insn "*load_zeroextendhisi_update"
  [(set (match_operand:SI 3 "dest_reg_operand" "=r,r")
	(zero_extend:SI (match_operator:HI 4 "any_mem_operand"
			 [(plus:SI (match_operand:SI 1 "register_operand" "0,0")
			           (match_operand:SI 2 "nonmemory_operand" "rCm2,Cal"))])))
   (set (match_operand:SI 0 "dest_reg_operand" "=r,r")
	(plus:SI (match_dup 1) (match_dup 2)))]
  ""
  "ld%_.a%V4 %3,[%0,%2]"
  [(set_attr "type" "load,load")
   (set_attr "length" "4,8")])

;; Note: no 16-bit variant for this instruction
(define_insn "*load_signextendhisi_update"
  [(set (match_operand:SI 3 "dest_reg_operand" "=r,r")
	(sign_extend:SI (match_operator:HI 4 "any_mem_operand"
			 [(plus:SI (match_operand:SI 1 "register_operand" "0,0")
			           (match_operand:SI 2 "nonmemory_operand" "rCm2,Cal"))])))
   (set (match_operand:SI 0 "dest_reg_operand" "=w,w")
	(plus:SI (match_dup 1) (match_dup 2)))]
  ""
  "ld%_.x.a%V4 %3,[%0,%2]"
  [(set_attr "type" "load,load")
   (set_attr "length" "4,8")])

(define_insn "*storehi_update"
  [(set (match_operator:HI 4 "any_mem_operand"
	 [(plus:SI (match_operand:SI 1 "register_operand" "0")
	           (match_operand:SI 2 "short_immediate_operand" "I"))])
	(match_operand:HI 3 "register_operand" "c"))
   (set (match_operand:SI 0 "dest_reg_operand" "=w")
	(plus:SI (match_dup 1) (match_dup 2)))]
  ""
  "st%_.a%V4 %3,[%0,%2]"
  [(set_attr "type" "store")
   (set_attr "length" "4")])

;; No 16-bit variant for this instruction pattern
(define_insn "*loadsi_update"
  [(set (match_operand:SI 3 "dest_reg_operand" "=r,r")
	(match_operator:SI 4 "any_mem_operand"
	 [(plus:SI (match_operand:SI 1 "register_operand" "0,0")
	           (match_operand:SI 2 "nonmemory_operand" "rCm2,Cal"))]))
   (set (match_operand:SI 0 "dest_reg_operand" "=w,w")
	(plus:SI (match_dup 1) (match_dup 2)))]
  ""
  "ld.a%V4 %3,[%0,%2]"
  [(set_attr "type" "load,load")
   (set_attr "length" "4,8")])

(define_insn "*storesi_update"
  [(set (match_operator:SI 4 "any_mem_operand"
	 [(plus:SI (match_operand:SI 1 "register_operand" "0")
	           (match_operand:SI 2 "short_immediate_operand" "I"))])
	(match_operand:SI 3 "register_operand" "c"))
   (set (match_operand:SI 0 "dest_reg_operand" "=w")
	(plus:SI (match_dup 1) (match_dup 2)))]
  ""
  "st.a%V4 %3,[%0,%2]"
  [(set_attr "type" "store")
   (set_attr "length" "4")])

(define_insn "*loadsf_update"
  [(set (match_operand:SF 3 "dest_reg_operand" "=r,r")
	(match_operator:SF 4 "any_mem_operand"
	 [(plus:SI (match_operand:SI 1 "register_operand" "0,0")
	           (match_operand:SI 2 "nonmemory_operand" "rCm2,Cal"))]))
   (set (match_operand:SI 0 "dest_reg_operand" "=w,w")
	(plus:SI (match_dup 1) (match_dup 2)))]
  ""
  "ld.a%V4 %3,[%0,%2]"
  [(set_attr "type" "load,load")
   (set_attr "length" "4,8")])

(define_insn "*storesf_update"
  [(set (match_operator:SF 4 "any_mem_operand"
	 [(plus:SI (match_operand:SI 1 "register_operand" "0")
	           (match_operand:SI 2 "short_immediate_operand" "I"))])
	(match_operand:SF 3 "register_operand" "c"))
   (set (match_operand:SI 0 "dest_reg_operand" "=w")
	(plus:SI (match_dup 1) (match_dup 2)))]
  ""
  "st.a%V4 %3,[%0,%2]"
  [(set_attr "type" "store")
   (set_attr "length" "4")])

;; Conditional move instructions.

(define_expand "movsicc"
  [(set (match_operand:SI 0 "dest_reg_operand" "")
	(if_then_else:SI (match_operand 1 "comparison_operator" "")
		         (match_operand:SI 2 "nonmemory_operand" "")
 		         (match_operand:SI 3 "register_operand" "")))]
  ""
  "operands[1] = gen_compare_reg (operands[1], VOIDmode);")


(define_expand "movdicc"
  [(set (match_operand:DI 0 "dest_reg_operand" "")
	(if_then_else:DI(match_operand 1 "comparison_operator" "")
		        (match_operand:DI 2 "nonmemory_operand" "")
		        (match_operand:DI 3 "register_operand" "")))]
  ""
  "operands[1] = gen_compare_reg (operands[1], VOIDmode);")


(define_expand "movsfcc"
  [(set (match_operand:SF 0 "dest_reg_operand" "")
	(if_then_else:SF (match_operand 1 "comparison_operator" "")
		      (match_operand:SF 2 "nonmemory_operand" "")
		      (match_operand:SF 3 "register_operand" "")))]
  ""
  "operands[1] = gen_compare_reg (operands[1], VOIDmode);")

(define_expand "movdfcc"
  [(set (match_operand:DF 0 "dest_reg_operand" "")
	(if_then_else:DF (match_operand 1 "comparison_operator" "")
		      (match_operand:DF 2 "nonmemory_operand" "")
		      (match_operand:DF 3 "register_operand" "")))]
  ""
  "operands[1] = gen_compare_reg (operands[1], VOIDmode);")

(define_insn "*movsicc_insn"
  [(set (match_operand:SI 0 "dest_reg_operand" "=w,w")
  	(if_then_else:SI (match_operator 3 "proper_comparison_operator"
  		       [(match_operand 4 "cc_register" "") (const_int 0)])
  		      (match_operand:SI 1 "nonmemory_operand" "cL,Cal")
  		      (match_operand:SI 2 "register_operand" "0,0")))]
  ""
{
  if (rtx_equal_p (operands[1], const0_rtx) && GET_CODE (operands[3]) == NE
      && satisfies_constraint_Rcq (operands[0]))
    return "sub%?.ne %0,%0,%0";
  /* ??? might be good for speed on ARC600 too, *if* properly scheduled.  */
  if ((optimize_size && (!TARGET_ARC600_FAMILY))
      && rtx_equal_p (operands[1], constm1_rtx)
      && GET_CODE (operands[3]) == LTU)
    return "sbc.cs %0,%0,%0";
  return "mov.%d3 %0,%1";
}
  [(set_attr "type" "cmove,cmove")
   (set_attr "length" "4,8")])

;; When there's a mask of a single bit, and then a compare to 0 or 1,
;; if the single bit is the sign bit, then GCC likes to convert this
;; into a sign extend and a compare less than, or greater to zero.
;; This is usually fine, except for the NXP400 where we have access to
;; a bit test instruction, along with a special short load instruction
;; (from CMEM), that doesn't support sign-extension on load.
;;
;; This peephole optimisation attempts to restore the use of bit-test
;; in those cases where it is useful to do so.
(define_peephole2
  [(set (match_operand:SI 0 "register_operand" "")
		(sign_extend:SI
		(match_operand:QI 1 "any_mem_operand" "")))
   (set (reg:CC_ZN CC_REG)
	(compare:CC_ZN (match_dup 0)
		       (const_int 0)))
   (set (pc)
	(if_then_else (match_operator 2 "ge_lt_comparison_operator"
		       [(reg:CC_ZN CC_REG) (const_int 0)])
		      (match_operand 3 "" "")
		      (match_operand 4 "" "")))]
  "TARGET_NPS_CMEM
   && cmem_address (XEXP (operands[1], 0), SImode)
   && peep2_reg_dead_p (2, operands[0])
   && peep2_regno_dead_p (3, CC_REG)"
  [(set (match_dup 0)
	(zero_extend:SI
	(match_dup 1)))
   (set (reg:CC_ZN CC_REG)
	(compare:CC_ZN (zero_extract:SI
			 (match_dup 0)
			 (const_int 1)
			 (const_int 7))
		       (const_int 0)))
   (set (pc)
	(if_then_else (match_dup 2)
		      (match_dup 3)
		      (match_dup 4)))]
  "if (GET_CODE (operands[2]) == GE)
     operands[2] = gen_rtx_EQ (VOIDmode, gen_rtx_REG (CC_ZNmode, 61), const0_rtx);
   else
     operands[2] = gen_rtx_NE (VOIDmode, gen_rtx_REG (CC_ZNmode, 61), const0_rtx);")

; Try to generate more short moves, and/or less limms, by substituting a
; conditional move with a conditional sub.
(define_peephole2
  [(set (match_operand:SI 0 "compact_register_operand")
	(match_operand:SI 1 "const_int_operand"))
   (set (match_dup 0)
  	(if_then_else:SI (match_operator 3 "proper_comparison_operator"
			   [(match_operand 4 "cc_register" "") (const_int 0)])
			    (match_operand:SI 2 "const_int_operand" "")
  		      (match_dup 0)))]
  "!satisfies_constraint_P (operands[1])
   && satisfies_constraint_P (operands[2])
   && UNSIGNED_INT6 (INTVAL (operands[2]) - INTVAL (operands[1]))"
  [(set (match_dup 0) (match_dup 2))
   (cond_exec
     (match_dup 3)
     (set (match_dup 0)
	  (plus:SI (match_dup 0) (match_dup 1))))]
  "operands[3] = gen_rtx_fmt_ee (REVERSE_CONDITION (GET_CODE (operands[3]),
						    GET_MODE (operands[4])),
				 VOIDmode, operands[4], const0_rtx);
   operands[1] = GEN_INT (INTVAL (operands[1]) - INTVAL (operands[2]));")

(define_insn "*movdicc_insn"
  [(set (match_operand:DI 0 "dest_reg_operand" "=&w,w")
	(if_then_else:DI (match_operator 3 "proper_comparison_operator"
			[(match_operand 4 "cc_register" "") (const_int 0)])
		      (match_operand:DI 1 "nonmemory_operand" "c,i")
		      (match_operand:DI 2 "register_operand" "0,0")))]
   ""
   "*
{
   switch (which_alternative)
     {
     default:
     case 0 :
       /* We normally copy the low-numbered register first.  However, if
 	 the first register operand 0 is the same as the second register of
 	 operand 1, we must copy in the opposite order.  */
       if (REGNO (operands[0]) == REGNO (operands[1]) + 1)
 	return \"mov.%d3 %R0,%R1\;mov.%d3 %0,%1\";
       else
 	return \"mov.%d3 %0,%1\;mov.%d3 %R0,%R1\";
     case 1 :
	return \"mov.%d3 %L0,%L1\;mov.%d3 %H0,%H1\";


     }
}"
  [(set_attr "type" "cmove,cmove")
   (set_attr "length" "8,16")])


(define_insn "*movsfcc_insn"
  [(set (match_operand:SF 0 "dest_reg_operand" "=w,w")
	(if_then_else:SF (match_operator 3 "proper_comparison_operator"
		       [(match_operand 4 "cc_register" "") (const_int 0)])
		      (match_operand:SF 1 "nonmemory_operand" "c,E")
		      (match_operand:SF 2 "register_operand" "0,0")))]
  ""
  "@
   mov.%d3 %0,%1
   mov.%d3 %0,%1 ; %A1"
  [(set_attr "type" "cmove,cmove")])

(define_insn "*movdfcc_insn"
  [(set (match_operand:DF 0 "dest_reg_operand" "=w,w")
	(if_then_else:DF (match_operator 1 "proper_comparison_operator"
		 [(match_operand 4 "cc_register" "") (const_int 0)])
		      (match_operand:DF 2 "nonmemory_operand" "c,E")
		      (match_operand:DF 3 "register_operand" "0,0")))]
  ""
  "*
{
  switch (which_alternative)
    {
    default:
    case 0 :
      /* We normally copy the low-numbered register first.  However, if
	 the first register operand 0 is the same as the second register of
	 operand 1, we must copy in the opposite order.  */
      if (REGNO (operands[0]) == REGNO (operands[2]) + 1)
	return \"mov.%d1 %R0,%R2\;mov.%d1 %0,%2\";
      else
	return \"mov.%d1 %0,%2\;mov.%d1 %R0,%R2\";
    case 1 :
	      return \"mov.%d1 %L0,%L2\;mov.%d1 %H0,%H2; %A2 \";

    }
}"
  [(set_attr "type" "cmove,cmove")
   (set_attr "length" "8,16")])


(define_insn "*zero_extendqihi2_i"
  [(set (match_operand:HI 0 "dest_reg_operand" "=Rcq,Rcq#q,Rcw,w,r,r")
	(zero_extend:HI (match_operand:QI 1 "nonvol_nonimm_operand" "0,Rcq#q,0,c,Ucm,m")))]
  ""
  "@
   extb%? %0,%1%&
   extb%? %0,%1%&
   bmsk%? %0,%1,7
   extb %0,%1
   xldb%U1 %0,%1
   ldb%U1 %0,%1"
  [(set_attr "type" "unary,unary,unary,unary,load,load")
   (set_attr "iscompact" "maybe,true,false,false,false,false")
   (set_attr "predicable" "no,no,yes,no,no,no")])

(define_expand "zero_extendqihi2"
  [(set (match_operand:HI 0 "dest_reg_operand" "")
	(zero_extend:HI (match_operand:QI 1 "nonvol_nonimm_operand" "")))]
  ""
  ""
)

(define_insn "*zero_extendqisi2_ac"
  [(set (match_operand:SI 0 "dest_reg_operand"    "=Rcq,Rcq#q,Rcw,w,qRcq,!*x,r,r")
	(zero_extend:SI (match_operand:QI 1 "nonvol_nonimm_operand" "0,Rcq#q,0,c,T,Usd,Ucm,m")))]
  ""
  "@
   extb%? %0,%1%&
   extb%? %0,%1%&
   bmsk%? %0,%1,7
   extb %0,%1
   ldb%? %0,%1%&
   ldb%? %0,%1%&
   xldb%U1 %0,%1
   ldb%U1 %0,%1"
  [(set_attr "type" "unary,unary,unary,unary,load,load,load,load")
   (set_attr "iscompact" "maybe,true,false,false,true,true,false,false")
   (set_attr "predicable" "no,no,yes,no,no,no,no,no")])

(define_expand "zero_extendqisi2"
  [(set (match_operand:SI 0 "dest_reg_operand" "")
	(zero_extend:SI (match_operand:QI 1 "nonvol_nonimm_operand" "")))]
  ""
  ""
)

(define_insn "*zero_extendhisi2_i"
  [(set (match_operand:SI 0 "dest_reg_operand" "=Rcq,q,Rcw,w,!x,Rcqq,r,r")
	(zero_extend:SI (match_operand:HI 1 "nonvol_nonimm_operand" "0,q,0,c,Usd,T,Ucm,m")))]
  ""
  "@
   ext%_%? %0,%1%&
   ext%_%? %0,%1%&
   bmsk%? %0,%1,15
   ext%_ %0,%1
   ld%_%? %0,%1
   ld%_%? %0,%1
   * return TARGET_EM ? \"xldh%U1%V1 %0,%1\" : \"xldw%U1 %0,%1\";
   ld%_%U1%V1 %0,%1"
  [(set_attr "type" "unary,unary,unary,unary,load,load,load,load")
   (set_attr "iscompact" "maybe,true,false,false,true,true,false,false")
   (set_attr "predicable" "no,no,yes,no,no,no,no,no")])


(define_expand "zero_extendhisi2"
  [(set (match_operand:SI 0 "dest_reg_operand" "")
	(zero_extend:SI (match_operand:HI 1 "nonvol_nonimm_operand" "")))]
  ""
  ""
)

;; Sign extension instructions.

(define_insn "*extendqihi2_i"
  [(set (match_operand:HI 0 "dest_reg_operand" "=Rcqq,r,r,r")
	(sign_extend:HI (match_operand:QI 1 "nonvol_nonimm_operand" "Rcqq,r,Uex,m")))]
  ""
  "@
   sexb%? %0,%1%&
   sexb %0,%1
   ldb.x%U1 %0,%1
   ldb.x%U1 %0,%1"
  [(set_attr "type" "unary,unary,load,load")
   (set_attr "iscompact" "true,false,false,false")
   (set_attr "length" "*,*,*,8")])


(define_expand "extendqihi2"
  [(set (match_operand:HI 0 "dest_reg_operand" "")
	(sign_extend:HI (match_operand:QI 1 "nonvol_nonimm_operand" "")))]
  ""
  ""
)

(define_insn "*extendqisi2_ac"
  [(set (match_operand:SI 0 "dest_reg_operand" "=Rcqq,w,r,r")
	(sign_extend:SI (match_operand:QI 1 "nonvol_nonimm_operand" "Rcqq,c,Uex,m")))]
  ""
  "@
   sexb%? %0,%1%&
   sexb %0,%1
   ldb.x%U1 %0,%1
   ldb.x%U1 %0,%1"
  [(set_attr "type" "unary,unary,load,load")
   (set_attr "iscompact" "true,false,false,false")
   (set_attr "length" "*,*,*,8")])

(define_expand "extendqisi2"
  [(set (match_operand:SI 0 "dest_reg_operand" "")
	(sign_extend:SI (match_operand:QI 1 "nonvol_nonimm_operand" "")))]
  ""
  ""
)

(define_insn "*extendhisi2_i"
  [(set (match_operand:SI 0 "dest_reg_operand" "=Rcqq,w,Rcqq,r,r")
	(sign_extend:SI (match_operand:HI 1 "nonvol_nonimm_operand" "Rcqq,c,Ucd,Uex,m")))]
  ""
  "@
   sex%_%? %0,%1%&
   sex%_ %0,%1
   ldh%?.x %0,%1%&
   ld%_.x%U1%V1 %0,%1
   ld%_.x%U1%V1 %0,%1"
  [(set_attr "type" "unary,unary,load,load,load")
   (set_attr "iscompact" "true,false,true,false,false")
   (set_attr "length" "*,*,*,4,8")])

(define_expand "extendhisi2"
  [(set (match_operand:SI 0 "dest_reg_operand" "")
	(sign_extend:SI (match_operand:HI 1 "nonvol_nonimm_operand" "")))]
  ""
  ""
)

;; Unary arithmetic insns

;; We allow constant operands to enable late constant propagation, but it is
;; not worth while to have more than one dedicated alternative to output them -
;; if we are really worried about getting these the maximum benefit of all
;; the available alternatives, we should add an extra pass to fold such
;; operations to movsi.

;; Absolute instructions

(define_insn "*abssi2_mixed"
  [(set (match_operand:SI 0 "compact_register_operand" "=q")
	(abs:SI (match_operand:SI 1 "compact_register_operand" "q")))]
  "TARGET_MIXED_CODE"
  "abs%? %0,%1%&"
  [(set_attr "type" "two_cycle_core")
   (set_attr "iscompact" "true")])

(define_insn "abssi2"
  [(set (match_operand:SI 0 "dest_reg_operand" "=Rcq#q,w,w")
	(abs:SI (match_operand:SI 1 "nonmemory_operand" "Rcq#q,cL,Cal")))]
  ""
  "abs%? %0,%1%&"
  [(set_attr "type" "two_cycle_core")
   (set_attr "length" "*,4,8")
   (set_attr "iscompact" "true,false,false")])

;; Maximum and minimum insns

(define_insn "smaxsi3"
   [(set (match_operand:SI 0 "dest_reg_operand"         "=Rcw, w,  w")
	 (smax:SI (match_operand:SI 1 "register_operand"  "%0, c,  c")
		  (match_operand:SI 2 "nonmemory_operand" "cL,cL,Cal")))]
  ""
  "max%? %0,%1,%2"
  [(set_attr "type" "two_cycle_core")
   (set_attr "length" "4,4,8")
   (set_attr "predicable" "yes,no,no")]
)

(define_insn "sminsi3"
   [(set (match_operand:SI 0 "dest_reg_operand"         "=Rcw, w,  w")
	 (smin:SI (match_operand:SI 1 "register_operand"  "%0, c,  c")
		  (match_operand:SI 2 "nonmemory_operand" "cL,cL,Cal")))]
  ""
  "min%? %0,%1,%2"
  [(set_attr "type" "two_cycle_core")
   (set_attr "length" "4,4,8")
   (set_attr "predicable" "yes,no,no")]
)

;; Arithmetic instructions.

; We say an insn can be conditionalized if this doesn't introduce a long
; immediate.  We set the type such that we still have good scheduling if the
; insn is conditionalized.
; ??? It would make sense to allow introduction of long immediates, but
;     we'd need to communicate to the ccfsm machinery the extra cost.
; The alternatives in the constraints still serve three purposes:
; - estimate insn size assuming conditional execution
; - guide reload to re-order the second and third operand to get a better fit.
; - give tentative insn type to guide scheduling
;   N.B. "%" for commutativity doesn't help when there is another matching
;   (but longer) alternative.
; We avoid letting this pattern use LP_COUNT as a register by specifying
;  register class 'W' instead of 'w'.
(define_insn_and_split "*addsi3_mixed"
  ;;                                                      0       1    2    3   4   5   6     7    8   9   a    b     c   d e   f  10  11  12
  [(set (match_operand:SI 0 "dest_reg_operand"          "=Rcq#q,Rcq,   h,!*Rsd,Rcq,Rcb,Rcq, Rcqq,Rcqq,Rcw,Rcw, Rcw,    W,  W,W,  W,Rcqq,Rcw,  W")
	(plus:SI (match_operand:SI 1 "register_operand" "%0,      c,   0, Rcqq,  0,  0,Rcb, Rcqq,   0,  0,  c,   0,    c,  c,0,  0,   0,  0,  c")
		 (match_operand:SI 2 "nonmemory_operand" "cL,     0, Cm1,    L,CL2,Csp,CM4,RcqqK,  cO, cL,  0,cCca,cLCmL,Cca,I,C2a, Cal,Cal,Cal")))]
  ""
{
  arc_output_addsi (operands, arc_ccfsm_cond_exec_p (), true);
  return "";
}
  "&& reload_completed && get_attr_length (insn) == 8
   && satisfies_constraint_I (operands[2])
   && GET_CODE (PATTERN (insn)) != COND_EXEC"
  [(set (match_dup 0) (match_dup 3)) (set (match_dup 0) (match_dup 4))]
  "split_addsi (operands);"
  [(set_attr "type" "*,*,*,*,two_cycle_core,two_cycle_core,*,*,*,*,*,two_cycle_core,*,two_cycle_core,*,two_cycle_core,*,*,*")
   (set (attr "iscompact")
	(cond [(match_test "~arc_output_addsi (operands, false, false) & 2")
	       (const_string "false")
	       (match_operand 2 "long_immediate_operand" "")
	       (const_string "maybe_limm")]
	      (const_string "maybe")))
   (set_attr "length"     "*,*,*,*,*,*,*,*,*,4,4,4,4,4,4,4,*,8,8")
   (set_attr "predicable" "no,no,no,no,no,no,no,no,no,yes,yes,yes,no,no,no,no,no,yes,no")
   (set_attr "cond"       "canuse,nocond,nocond,nocond,canuse,canuse,nocond,nocond,nocond,canuse,canuse,canuse,nocond,nocond,canuse_limm,canuse_limm,canuse,canuse,nocond")
])

;; ARCv2 MPYW and MPYUW
(define_expand "mulhisi3"
  [(set (match_operand:SI 0 "register_operand"                           "")
	(mult:SI (sign_extend:SI (match_operand:HI 1 "register_operand"  ""))
		 (sign_extend:SI (match_operand:HI 2 "nonmemory_operand" ""))))]
  "TARGET_MPYW"
  "{
    if (CONSTANT_P (operands[2]))
    {
      emit_insn (gen_mulhisi3_imm (operands[0], operands[1], operands[2]));
      DONE;
    }
   }"
)

(define_insn "mulhisi3_imm"
  [(set (match_operand:SI 0 "register_operand"                         "=r,r,r,  r,  r")
	(mult:SI (sign_extend:SI (match_operand:HI 1 "register_operand" "0,r,0,  0,  r"))
		 (match_operand:HI 2 "short_const_int_operand"          "L,L,I,C16,C16")))]
  "TARGET_MPYW"
  "mpyw%? %0,%1,%2"
  [(set_attr "length" "4,4,4,8,8")
   (set_attr "iscompact" "false")
   (set_attr "type" "mul16_em")
   (set_attr "predicable" "yes,no,no,yes,no")
   (set_attr "cond" "canuse,nocond,nocond,canuse_limm,nocond")
   ])

(define_insn "mulhisi3_reg"
  [(set (match_operand:SI 0 "register_operand"                          "=Rcqq,r,r")
	(mult:SI (sign_extend:SI (match_operand:HI 1 "register_operand"  "   0,0,r"))
		 (sign_extend:SI (match_operand:HI 2 "nonmemory_operand" "Rcqq,r,r"))))]
  "TARGET_MPYW"
  "mpyw%? %0,%1,%2"
  [(set_attr "length" "*,4,4")
   (set_attr "iscompact" "maybe,false,false")
   (set_attr "type" "mul16_em")
   (set_attr "predicable" "yes,yes,no")
   (set_attr "cond" "canuse,canuse,nocond")
   ])

(define_expand "umulhisi3"
  [(set (match_operand:SI 0 "register_operand"                           "")
	(mult:SI (zero_extend:SI (match_operand:HI 1 "register_operand"  ""))
		 (zero_extend:SI (match_operand:HI 2 "arc_short_operand" ""))))]
  "TARGET_MPYW"
  "{
    if (CONSTANT_P (operands[2]))
    {
      emit_insn (gen_umulhisi3_imm (operands[0], operands[1], operands[2]));
      DONE;
    }
  }"
)

(define_insn "umulhisi3_imm"
  [(set (match_operand:SI 0 "register_operand"                          "=r, r,  r,  r,  r")
	(mult:SI (zero_extend:SI (match_operand:HI 1 "register_operand" "%0, r,  0,  0,  r"))
		 (match_operand:HI 2 "short_unsigned_const_operand"     " L, L,J12,J16,J16")))]
  "TARGET_MPYW"
  "mpyuw%? %0,%1,%2"
  [(set_attr "length" "4,4,4,8,8")
   (set_attr "iscompact" "false")
   (set_attr "type" "mul16_em")
   (set_attr "predicable" "yes,no,no,yes,no")
   (set_attr "cond" "canuse,nocond,nocond,canuse_limm,nocond")
   ])

(define_insn "umulhisi3_reg"
  [(set (match_operand:SI 0 "register_operand"                          "=Rcqq, r, r")
	(mult:SI (zero_extend:SI (match_operand:HI 1 "register_operand" "   %0, 0, r"))
		 (zero_extend:SI (match_operand:HI 2 "register_operand" " Rcqq, r, r"))))]
  "TARGET_MPYW"
  "mpyuw%? %0,%1,%2"
  [(set_attr "length" "*,4,4")
   (set_attr "iscompact" "maybe,false,false")
   (set_attr "type" "mul16_em")
   (set_attr "predicable" "yes,yes,no")
   (set_attr "cond" "canuse,canuse,nocond")
   ])

;; ARC700/ARC600/V2 multiply
;; SI <- SI * SI

(define_expand "mulsi3"
 [(set (match_operand:SI 0 "nonimmediate_operand"            "")
	(mult:SI (match_operand:SI 1 "register_operand"  "")
		 (match_operand:SI 2 "nonmemory_operand" "")))]
  ""
{
  if (TARGET_MPY)
    {
      if (!register_operand (operands[0], SImode))
	{
	  rtx result = gen_reg_rtx (SImode);

	  emit_insn (gen_mulsi3 (result, operands[1], operands[2]));
	  emit_move_insn (operands[0], result);
	  DONE;
	}
    }
  else if (TARGET_MUL64_SET)
    {
     rtx tmp = gen_reg_rtx (SImode);
     emit_insn (gen_mulsi64 (tmp, operands[1], operands[2]));
     emit_move_insn (operands[0], tmp);
     DONE;
    }
  else if (TARGET_MULMAC_32BY16_SET)
    {
     rtx tmp = gen_reg_rtx (SImode);
     emit_insn (gen_mulsi32x16 (tmp, operands[1], operands[2]));
     emit_move_insn (operands[0], tmp);
     DONE;
    }
  else
    {
      emit_move_insn (gen_rtx_REG (SImode, R0_REG), operands[1]);
      emit_move_insn (gen_rtx_REG (SImode, R1_REG), operands[2]);
      emit_insn (gen_mulsi3_600_lib ());
      emit_move_insn (operands[0], gen_rtx_REG (SImode, R0_REG));
      DONE;
    }
})

(define_insn_and_split "mulsi32x16"
 [(set (match_operand:SI 0 "register_operand"            "=w")
	(mult:SI (match_operand:SI 1 "register_operand"  "%c")
		 (match_operand:SI 2 "nonmemory_operand" "ci")))
  (clobber (reg:DI MUL32x16_REG))]
 "TARGET_MULMAC_32BY16_SET"
 "#"
 "TARGET_MULMAC_32BY16_SET && reload_completed"
 [(const_int 0)]
 {
  if (immediate_operand (operands[2], SImode)
    && INTVAL (operands[2]) >= 0
    && INTVAL (operands[2]) <= 65535)
     {
      emit_insn (gen_umul_600 (operands[1], operands[2],
				       gen_acc2 (), gen_acc1 ()));
      emit_move_insn (operands[0], gen_acc2 ());
      DONE;
     }
   emit_insn (gen_umul_600 (operands[1], operands[2],
				   gen_acc2 (), gen_acc1 ()));
   emit_insn (gen_mac_600 (operands[1], operands[2],
				   gen_acc2 (), gen_acc1 ()));
   emit_move_insn (operands[0], gen_acc2 ());
   DONE;
  }
 [(set_attr "type" "multi")
  (set_attr "length" "8")])

; mululw conditional execution without a LIMM clobbers an input register;
; we'd need a different pattern to describe this.
; To make the conditional execution valid for the LIMM alternative, we
; have to emit the LIMM before the register operand.
(define_insn "umul_600"
  [(set (match_operand:SI 2 "acc2_operand" "")
	(mult:SI (match_operand:SI 0 "register_operand"  "c,c,c")
		 (zero_extract:SI (match_operand:SI 1 "nonmemory_operand"
							 "c,L,Cal")
				  (const_int 16)
				  (const_int 0))))
   (clobber (match_operand:SI 3 "acc1_operand" ""))]
  "TARGET_MULMAC_32BY16_SET"
  "mululw 0, %0, %1"
  [(set_attr "length" "4,4,8")
   (set_attr "type" "mulmac_600")
   (set_attr "predicable" "no")
   (set_attr "cond" "nocond")])

(define_insn "mac_600"
  [(set (match_operand:SI 2 "acc2_operand" "")
	(plus:SI
	  (mult:SI (match_operand:SI 0 "register_operand" "c,c,c")
		   (ashift:SI
		     (zero_extract:SI (match_operand:SI 1 "nonmemory_operand" "c,L,Cal")
				      (const_int 16)
				      (const_int 16))
		     (const_int 16)))
	  (match_dup 2)))
   (clobber (match_operand:SI 3 "acc1_operand" ""))]
  "TARGET_MULMAC_32BY16_SET"
  "machlw%? 0, %0, %1"
  [(set_attr "length" "4,4,8")
   (set_attr "type" "mulmac_600, mulmac_600, mulmac_600")
   (set_attr "predicable" "no, no, yes")
   (set_attr "cond" "nocond, canuse_limm, canuse")])

(define_insn_and_split "mulsi64"
 [(set (match_operand:SI 0 "register_operand"            "=w")
	(mult:SI (match_operand:SI 1 "register_operand"  "%c")
		 (match_operand:SI 2 "nonmemory_operand" "ci")))
  (clobber (reg:DI MUL64_OUT_REG))]
 "TARGET_MUL64_SET"
 "#"
 "TARGET_MUL64_SET && reload_completed"
  [(const_int 0)]
{
  emit_insn (gen_mulsi_600 (operands[1], operands[2],
			gen_mlo (), gen_mhi ()));
  emit_move_insn (operands[0], gen_mlo ());
  DONE;
}
  [(set_attr "type" "multi")
   (set_attr "length" "8")])

(define_insn "mulsi_600"
  [(set (match_operand:SI 2 "mlo_operand" "")
	(mult:SI (match_operand:SI 0 "register_operand"  "%Rcq#q,c,c,c")
		 (match_operand:SI 1 "nonmemory_operand" "Rcq#q,cL,I,Cal")))
   (clobber (match_operand:SI 3 "mhi_operand" ""))]
  "TARGET_MUL64_SET"
; The assembler mis-assembles mul64 / mulu64 with "I" constraint constants,
; using a machine code pattern that only allows "L" constraint constants.
;  "mul64%? \t0, %0, %1%&"
{
  if (satisfies_constraint_I (operands[1])
      && !satisfies_constraint_L (operands[1]))
    {
      /* MUL64 <0,>b,s12 00101bbb10000100 0BBBssssssSSSSSS  */
      int n = true_regnum (operands[0]);
      int i = INTVAL (operands[1]);
      asm_fprintf (asm_out_file, "\t.short %d`", 0x2884 + ((n & 7) << 8));
      asm_fprintf (asm_out_file, "\t.short %d`",
		   ((i & 0x3f) << 6) + ((i >> 6) & 0x3f) + ((n & 070) << 9));
      return "; mul64%? \t0, %0, %1%&";
    }
  return "mul64%? \t0, %0, %1%&";
}
  [(set_attr "length" "*,4,4,8")
   (set_attr "iscompact" "maybe,false,false,false")
   (set_attr "type" "multi,multi,multi,multi")
   (set_attr "predicable" "yes,yes,no,yes")
   (set_attr "cond" "canuse,canuse,canuse_limm,canuse")])

; If we compile without an mul option enabled, but link with libraries
; for a mul option, we'll see clobbers of multiplier output registers.
; There is also an implementation using norm that clobbers the loop registers.
(define_insn "mulsi3_600_lib"
  [(set (reg:SI R0_REG)
	(mult:SI (reg:SI R0_REG) (reg:SI R1_REG)))
   (clobber (reg:SI RETURN_ADDR_REGNUM))
   (clobber (reg:SI R1_REG))
   (clobber (reg:SI R2_REG))
   (clobber (reg:SI R3_REG))
   (clobber (reg:DI MUL64_OUT_REG))
   (clobber (reg:SI LP_COUNT))
   (clobber (reg:SI LP_START))
   (clobber (reg:SI LP_END))
   (clobber (reg:CC CC_REG))]
  "!TARGET_ANY_MPY
   && SFUNC_CHECK_PREDICABLE"
  "*return arc_output_libcall (\"__mulsi3\");"
  [(set_attr "is_sfunc" "yes")
   (set_attr "predicable" "yes")])

(define_insn_and_split "mulsidi_600"
  [(set (match_operand:DI 0 "register_operand"                               "=c, c,c,  c")
	(mult:DI (sign_extend:DI (match_operand:SI 1 "register_operand"  "%Rcq#q, c,c,  c"))
		 (sign_extend:DI (match_operand:SI 2 "nonmemory_operand"  "Rcq#q,cL,L,C32"))))
   (clobber (reg:DI MUL64_OUT_REG))]
  "TARGET_MUL64_SET"
  "#"
  "TARGET_MUL64_SET"
  [(const_int 0)]
  "emit_insn (gen_mul64 (operands[1], operands[2]));
   emit_move_insn (operands[0], gen_rtx_REG (DImode, MUL64_OUT_REG));
   DONE;"
  [(set_attr "type" "multi")
   (set_attr "length" "8")])

(define_insn "mul64"
  [(set (reg:DI MUL64_OUT_REG)
	(mult:DI
	 (sign_extend:DI (match_operand:SI 0 "register_operand" "%Rcq#q, c,c,  c"))
	 (sign_extend:DI (match_operand:SI 1 "nonmemory_operand" "Rcq#q,cL,L,C32"))))]
  "TARGET_MUL64_SET"
  "mul64%? \t0, %0, %1%&"
  [(set_attr "length" "*,4,4,8")
   (set_attr "iscompact" "maybe,false,false,false")
   (set_attr "type" "multi,multi,multi,multi")
   (set_attr "predicable" "yes,yes,no,yes")
   (set_attr "cond" "canuse,canuse,canuse_limm,canuse")])

(define_insn_and_split "umulsidi_600"
  [(set (match_operand:DI 0 "register_operand"                            "=c,c, c")
	(mult:DI (zero_extend:DI (match_operand:SI 1 "register_operand"   "%c,c, c"))
		 (sign_extend:DI (match_operand:SI 2 "nonmemory_operand"  "cL,L,C32"))))
   (clobber (reg:DI MUL64_OUT_REG))]
  "TARGET_MUL64_SET"
  "#"
  "TARGET_MUL64_SET"
  [(const_int 0)]
  "emit_insn (gen_mulu64 (operands[1], operands[2]));
   emit_move_insn (operands[0], gen_rtx_REG (DImode, MUL64_OUT_REG));
   DONE;"
  [(set_attr "type" "umulti")
   (set_attr "length" "8")])

(define_insn "mulu64"
  [(set (reg:DI MUL64_OUT_REG)
	(mult:DI
	 (zero_extend:DI (match_operand:SI 0 "register_operand"  "%c,c,c"))
	 (zero_extend:DI (match_operand:SI 1 "nonmemory_operand" "cL,L,C32"))))]
  "TARGET_MUL64_SET"
  "mulu64%? \t0, %0, %1%&"
  [(set_attr "length" "4,4,8")
   (set_attr "iscompact" "false")
   (set_attr "type" "umulti")
   (set_attr "predicable" "yes,no,yes")
   (set_attr "cond" "canuse,canuse_limm,canuse")])

; ARC700 mpy* instructions: This is a multi-cycle extension, and thus 'w'
; may not be used as destination constraint.

; The result of mpy and mpyu is the same except for flag setting (if enabled),
; but mpyu is faster for the standard multiplier.
; Note: we must make sure LP_COUNT is not one of the destination
; registers, since it cannot be the destination of a multi-cycle insn
; like MPY or MPYU.
(define_insn "mulsi3_700"
 [(set (match_operand:SI 0 "mpy_dest_reg_operand"        "=Rcr,r,r,Rcr,r")
	(mult:SI (match_operand:SI 1 "register_operand"  "%0,c,0,0,c")
		 (match_operand:SI 2 "nonmemory_operand" "cL,cL,I,Cal,Cal")))]
 "TARGET_ARC700_MPY"
  "mpyu%? %0,%1,%2"
  [(set_attr "length" "4,4,4,8,8")
   (set_attr "type" "umulti")
   (set_attr "predicable" "yes,no,no,yes,no")
   (set_attr "cond" "canuse,nocond,canuse_limm,canuse,nocond")])

; ARCv2 has no penalties between mpy and mpyu. So, we use mpy because of its
; short variant. LP_COUNT constraints are still valid.
(define_insn "mulsi3_v2"
 [(set (match_operand:SI 0 "mpy_dest_reg_operand"        "=Rcqq,Rcr, r,r,Rcr,  r")
	(mult:SI (match_operand:SI 1 "register_operand"     "%0,  0, c,0,  0,  c")
		 (match_operand:SI 2 "nonmemory_operand" " Rcqq, cL,cL,I,Cal,Cal")))]
 "TARGET_MULTI"
 "mpy%? %0,%1,%2"
 [(set_attr "length" "*,4,4,4,8,8")
  (set_attr "iscompact" "maybe,false,false,false,false,false")
  (set_attr "type" "umulti")
  (set_attr "predicable" "no,yes,no,no,yes,no")
  (set_attr "cond" "nocond,canuse,nocond,canuse_limm,canuse,nocond")])

(define_expand "mulsidi3"
  [(set (match_operand:DI 0 "register_operand" "")
	(mult:DI (sign_extend:DI (match_operand:SI 1 "register_operand" ""))
		 (sign_extend:DI (match_operand:SI 2 "nonmemory_operand" ""))))]
  "TARGET_ANY_MPY"
  {
  if (TARGET_PLUS_MACD)
    {
     if (CONST_INT_P (operands[2]))
       {
	emit_insn (gen_mpyd_imm_arcv2hs (operands[0], operands[1], operands[2]));
       }
     else
       {
	emit_insn (gen_mpyd_arcv2hs (operands[0], operands[1], operands[2]));
       }
     DONE;
    }
  if (TARGET_MPY)
    {
      operands[2] = force_reg (SImode, operands[2]);
      if (!register_operand (operands[0], DImode))
	{
	  rtx result = gen_reg_rtx (DImode);

	  operands[2] = force_reg (SImode, operands[2]);
	  emit_insn (gen_mulsidi3 (result, operands[1], operands[2]));
	  emit_move_insn (operands[0], result);
	  DONE;
	}
    }
  else if (TARGET_MUL64_SET)
    {
      emit_insn (gen_mulsidi_600 (operands[0], operands[1], operands[2]));
      DONE;
    }
  else if (TARGET_MULMAC_32BY16_SET)
    {
      operands[2] = force_reg (SImode, operands[2]);
      emit_insn (gen_mulsidi64 (operands[0], operands[1], operands[2]));
      DONE;
    }
  operands[2] = force_reg (SImode, operands[2]);
  })

(define_insn_and_split "mulsidi64"
  [(set (match_operand:DI 0 "register_operand" "=w")
	(mult:DI (sign_extend:DI (match_operand:SI 1 "register_operand" "%c"))
		 (sign_extend:DI (match_operand:SI 2 "extend_operand" "ci"))))
   (clobber (reg:DI MUL32x16_REG))]
  "TARGET_MULMAC_32BY16_SET"
  "#"
  "TARGET_MULMAC_32BY16_SET && reload_completed"
  [(const_int 0)]
  {
   rtx result_hi = gen_highpart (SImode, operands[0]);
   rtx result_low = gen_lowpart (SImode, operands[0]);

   emit_insn (gen_mul64_600 (operands[1], operands[2]));
   emit_insn (gen_mac64_600 (result_hi, operands[1], operands[2]));
   emit_move_insn (result_low, gen_acc2 ());
   DONE;
  }
  [(set_attr "type" "multi")
   (set_attr "length" "8")])


(define_insn "mul64_600"
  [(set (reg:DI MUL32x16_REG)
	(mult:DI (sign_extend:DI (match_operand:SI 0 "register_operand"
				  "c,c,c"))
		 (zero_extract:DI (match_operand:SI 1 "nonmemory_operand"
				  "c,L,Cal")
				  (const_int 16)
				  (const_int 0))))
  ]
  "TARGET_MULMAC_32BY16_SET"
  "mullw%? 0, %0, %1"
  [(set_attr "length" "4,4,8")
   (set_attr "type" "mulmac_600")
   (set_attr "predicable" "no,no,yes")
   (set_attr "cond" "nocond, canuse_limm, canuse")])


;; ??? check if this is canonical rtl
(define_insn "mac64_600"
  [(set (reg:DI MUL32x16_REG)
	(plus:DI
	  (mult:DI (sign_extend:DI (match_operand:SI 1 "register_operand" "c,c,c"))
		   (ashift:DI
		     (sign_extract:DI (match_operand:SI 2 "nonmemory_operand" "c,L,Cal")
				      (const_int 16) (const_int 16))
		     (const_int 16)))
	  (reg:DI MUL32x16_REG)))
   (set (match_operand:SI 0 "register_operand" "=w,w,w")
	(zero_extract:SI
	  (plus:DI
	    (mult:DI (sign_extend:DI (match_dup 1))
		     (ashift:DI
		       (sign_extract:DI (match_dup 2)
					(const_int 16) (const_int 16))
			  (const_int 16)))
	    (reg:DI MUL32x16_REG))
	  (const_int 32) (const_int 32)))]
  "TARGET_MULMAC_32BY16_SET"
  "machlw%? %0, %1, %2"
  [(set_attr "length" "4,4,8")
   (set_attr "type" "mulmac_600")
   (set_attr "predicable" "no,no,yes")
   (set_attr "cond" "nocond, canuse_limm, canuse")])


;; DI <- DI(signed SI) * DI(signed SI)
(define_insn_and_split "mulsidi3_700"
  [(set (match_operand:DI 0 "register_operand" "=&r")
	(mult:DI (sign_extend:DI (match_operand:SI 1 "register_operand" "%c"))
		 (sign_extend:DI (match_operand:SI 2 "extend_operand" "cL"))))]
  "TARGET_MPY && !TARGET_PLUS_MACD"
  "#"
  "&& reload_completed"
  [(const_int 0)]
{
  int hi = TARGET_BIG_ENDIAN ? 0 : UNITS_PER_WORD;
  int lo = TARGET_BIG_ENDIAN ? UNITS_PER_WORD : 0;
  rtx l0 = simplify_gen_subreg (word_mode, operands[0], DImode, lo);
  rtx h0 = simplify_gen_subreg (word_mode, operands[0], DImode, hi);
  emit_insn (gen_mulsi3_highpart (h0, operands[1], operands[2]));
  emit_insn (gen_mulsi3 (l0, operands[1], operands[2]));
  DONE;
}
  [(set_attr "type" "multi")
   (set_attr "length" "8")])

(define_insn "mulsi3_highpart"
  [(set (match_operand:SI 0 "register_operand"                  "=Rcr,r,Rcr,r")
	(truncate:SI
	 (lshiftrt:DI
	  (mult:DI
	   (sign_extend:DI (match_operand:SI 1 "register_operand" "%0,c,  0,c"))
	   (sign_extend:DI (match_operand:SI 2 "extend_operand"    "c,c,  i,i")))
	  (const_int 32))))]
  "TARGET_MPY"
  "mpy%+%? %0,%1,%2"
  [(set_attr "length" "4,4,8,8")
   (set_attr "type" "multi")
   (set_attr "predicable" "yes,no,yes,no")
   (set_attr "cond" "canuse,nocond,canuse,nocond")])

; Note that mpyhu has the same latency as mpy / mpyh,
; thus we use the type multi.
(define_insn "*umulsi3_highpart_i"
  [(set (match_operand:SI 0 "register_operand"                  "=Rcr,r,Rcr,r")
	(truncate:SI
	 (lshiftrt:DI
	  (mult:DI
	   (zero_extend:DI (match_operand:SI 1 "register_operand" "%0,c,  0,c"))
	   (zero_extend:DI (match_operand:SI 2 "extend_operand"    "c,c,  i,i")))
	  (const_int 32))))]
  "TARGET_MPY"
  "mpy%+u%? %0,%1,%2"
  [(set_attr "length" "4,4,8,8")
   (set_attr "type" "multi")
   (set_attr "predicable" "yes,no,yes,no")
   (set_attr "cond" "canuse,nocond,canuse,nocond")])

; Implementations include additional labels for umulsidi3, so we got all
; the same clobbers - plus one for the result low part.  */
(define_insn "umulsi3_highpart_600_lib_le"
  [(set (reg:SI R1_REG)
	(truncate:SI
	 (lshiftrt:DI
	  (mult:DI (zero_extend:DI (reg:SI R0_REG))
		   (zero_extend:DI (reg:SI R1_REG)))
	  (const_int 32))))
   (clobber (reg:SI RETURN_ADDR_REGNUM))
   (clobber (reg:SI R0_REG))
   (clobber (reg:DI R2_REG))
   (clobber (reg:SI R12_REG))
   (clobber (reg:DI MUL64_OUT_REG))
   (clobber (reg:CC CC_REG))]
  "!TARGET_BIG_ENDIAN
   && !TARGET_ANY_MPY
   && SFUNC_CHECK_PREDICABLE"
  "*return arc_output_libcall (\"__umulsi3_highpart\");"
  [(set_attr "is_sfunc" "yes")
   (set_attr "predicable" "yes")])

(define_insn "umulsi3_highpart_600_lib_be"
  [(set (reg:SI R0_REG)
	(truncate:SI
	 (lshiftrt:DI
	  (mult:DI (zero_extend:DI (reg:SI R0_REG))
		   (zero_extend:DI (reg:SI R1_REG)))
	  (const_int 32))))
   (clobber (reg:SI RETURN_ADDR_REGNUM))
   (clobber (reg:SI R1_REG))
   (clobber (reg:DI R2_REG))
   (clobber (reg:SI R12_REG))
   (clobber (reg:DI MUL64_OUT_REG))
   (clobber (reg:CC CC_REG))]
  "TARGET_BIG_ENDIAN
   && !TARGET_ANY_MPY
   && SFUNC_CHECK_PREDICABLE"
  "*return arc_output_libcall (\"__umulsi3_highpart\");"
  [(set_attr "is_sfunc" "yes")
   (set_attr "predicable" "yes")])

;; (zero_extend:DI (const_int)) leads to internal errors in combine, so we
;; need a separate pattern for immediates
;; ??? This is fine for combine, but not for reload.
(define_insn "umulsi3_highpart_int"
  [(set (match_operand:SI 0 "register_operand"            "=Rcr, r, r,Rcr,  r")
	(truncate:SI
	 (lshiftrt:DI
	  (mult:DI
	   (zero_extend:DI (match_operand:SI 1 "register_operand"  " 0, c, 0,  0,  c"))
	   (match_operand:DI 2 "immediate_usidi_operand" "L, L, I, Cal, Cal"))
	  (const_int 32))))]
  "TARGET_MPY"
  "mpy%+u%? %0,%1,%2"
  [(set_attr "length" "4,4,4,8,8")
   (set_attr "type" "multi")
   (set_attr "predicable" "yes,no,no,yes,no")
   (set_attr "cond" "canuse,nocond,canuse_limm,canuse,nocond")])

(define_expand "umulsi3_highpart"
  [(set (match_operand:SI 0 "general_operand"  "")
	(truncate:SI
	 (lshiftrt:DI
	  (mult:DI
	   (zero_extend:DI (match_operand:SI 1 "register_operand" ""))
	   (zero_extend:DI (match_operand:SI 2 "nonmemory_operand" "")))
	  (const_int 32))))]
  "!TARGET_MUL64_SET && !TARGET_MULMAC_32BY16_SET"
  "
{
  rtx target = operands[0];

  if (!TARGET_MPY)
    {
      emit_move_insn (gen_rtx_REG (SImode, 0), operands[1]);
      emit_move_insn (gen_rtx_REG (SImode, 1), operands[2]);
      if (TARGET_BIG_ENDIAN)
	emit_insn (gen_umulsi3_highpart_600_lib_be ());
      else
	emit_insn (gen_umulsi3_highpart_600_lib_le ());
      emit_move_insn (target, gen_rtx_REG (SImode, 0));
      DONE;
    }

  if (!register_operand (target, SImode))
    target = gen_reg_rtx (SImode);

  if (CONST_INT_P (operands[2]) && INTVAL (operands[2]) < 0)
    operands[2] = simplify_const_unary_operation (ZERO_EXTEND, DImode,
						  operands[2], SImode);
  else if (!immediate_operand (operands[2], SImode))
    operands[2] = gen_rtx_ZERO_EXTEND (DImode, operands[2]);
  emit_insn (gen_umulsi3_highpart_int (target, operands[1], operands[2]));
  if (target != operands[0])
    emit_move_insn (operands[0], target);
  DONE;
}")

(define_expand "umulsidi3"
  [(set (match_operand:DI 0 "register_operand" "")
	(mult:DI (zero_extend:DI (match_operand:SI 1 "register_operand" ""))
		 (zero_extend:DI (match_operand:SI 2 "nonmemory_operand" ""))))]
  ""
{
  if (TARGET_PLUS_MACD)
    {
     if (CONST_INT_P (operands[2]))
       {
	emit_insn (gen_mpydu_imm_arcv2hs (operands[0], operands[1], operands[2]));
       }
     else
       {
	emit_insn (gen_mpydu_arcv2hs (operands[0], operands[1], operands[2]));
       }
     DONE;
    }
  if (TARGET_MPY)
    {
      operands[2] = force_reg (SImode, operands[2]);
      if (!register_operand (operands[0], DImode))
	{
	  rtx result = gen_reg_rtx (DImode);

	  emit_insn (gen_umulsidi3 (result, operands[1], operands[2]));
	  emit_move_insn (operands[0], result);
	  DONE;
	}
    }
  else if (TARGET_MUL64_SET)
    {
     operands[2] = force_reg (SImode, operands[2]);
     emit_insn (gen_umulsidi_600 (operands[0], operands[1], operands[2]));
      DONE;
    }
  else if (TARGET_MULMAC_32BY16_SET)
    {
     operands[2] = force_reg (SImode, operands[2]);
     emit_insn (gen_umulsidi64 (operands[0], operands[1], operands[2]));
      DONE;
    }
  else
    {
      emit_move_insn (gen_rtx_REG (SImode, R0_REG), operands[1]);
      emit_move_insn (gen_rtx_REG (SImode, R1_REG), operands[2]);
      emit_insn (gen_umulsidi3_600_lib ());
      emit_move_insn (operands[0], gen_rtx_REG (DImode, R0_REG));
      DONE;
    }
})

(define_insn_and_split "umulsidi64"
  [(set (match_operand:DI 0 "register_operand" "=w")
	(mult:DI (zero_extend:DI (match_operand:SI 1 "register_operand" "%c"))
		 (zero_extend:DI (match_operand:SI 2 "extend_operand" "ci"))))
   (clobber (reg:DI MUL32x16_REG))]
  "TARGET_MULMAC_32BY16_SET"
  "#"
  "TARGET_MULMAC_32BY16_SET && reload_completed"
  [(const_int 0)]
  {
   rtx result_hi;
   rtx result_low;

   result_hi = gen_highpart (SImode, operands[0]);
   result_low = gen_lowpart (SImode, operands[0]);

   emit_insn (gen_umul64_600 (operands[1], operands[2]));
   emit_insn (gen_umac64_600 (result_hi, operands[1], operands[2]));
   emit_move_insn (result_low, gen_acc2 ());
   DONE;
   }
  [(set_attr "type" "multi")
   (set_attr "length" "8")])

(define_insn "umul64_600"
  [(set (reg:DI MUL32x16_REG)
	(mult:DI (zero_extend:DI (match_operand:SI 0 "register_operand"
				  "c,c,c"))
		 (zero_extract:DI (match_operand:SI 1 "nonmemory_operand"
				  "c,L,Cal")
				  (const_int 16)
				  (const_int 0))))
  ]
  "TARGET_MULMAC_32BY16_SET"
  "mululw 0, %0, %1"
  [(set_attr "length" "4,4,8")
   (set_attr "type" "mulmac_600")
   (set_attr "predicable" "no")
   (set_attr "cond" "nocond")])


(define_insn "umac64_600"
  [(set (reg:DI MUL32x16_REG)
	(plus:DI
	  (mult:DI (zero_extend:DI (match_operand:SI 1 "register_operand" "c,c,c"))
		   (ashift:DI
		     (zero_extract:DI (match_operand:SI 2 "nonmemory_operand" "c,L,Cal")
				      (const_int 16) (const_int 16))
		     (const_int 16)))
	  (reg:DI MUL32x16_REG)))
   (set (match_operand:SI 0 "register_operand" "=w,w,w")
	(zero_extract:SI
	  (plus:DI
	    (mult:DI (zero_extend:DI (match_dup 1))
		     (ashift:DI
		       (zero_extract:DI (match_dup 2)
					(const_int 16) (const_int 16))
			  (const_int 16)))
	    (reg:DI MUL32x16_REG))
	  (const_int 32) (const_int 32)))]
  "TARGET_MULMAC_32BY16_SET"
  "machulw%? %0, %1, %2"
  [(set_attr "length" "4,4,8")
   (set_attr "type" "mulmac_600")
   (set_attr "predicable" "no,no,yes")
   (set_attr "cond" "nocond, canuse_limm, canuse")])

;; DI <- DI(unsigned SI) * DI(unsigned SI)
(define_insn_and_split "umulsidi3_700"
  [(set (match_operand:DI 0 "dest_reg_operand" "=&r")
	(mult:DI (zero_extend:DI (match_operand:SI 1 "register_operand" "%c"))
		 (zero_extend:DI (match_operand:SI 2 "extend_operand" "cL"))))]
  "TARGET_MPY && !TARGET_PLUS_MACD"
  "#"
  "reload_completed"
  [(const_int 0)]
{
  int hi = !TARGET_BIG_ENDIAN;
  int lo = !hi;
  rtx l0 = operand_subword (operands[0], lo, 0, DImode);
  rtx h0 = operand_subword (operands[0], hi, 0, DImode);
  emit_insn (gen_umulsi3_highpart (h0, operands[1], operands[2]));
  emit_insn (gen_mulsi3 (l0, operands[1], operands[2]));
  DONE;
}
  [(set_attr "type" "umulti")
  (set_attr "length" "8")])

(define_insn "umulsidi3_600_lib"
  [(set (reg:DI R0_REG)
	(mult:DI (zero_extend:DI (reg:SI R0_REG))
		 (zero_extend:DI (reg:SI R1_REG))))
   (clobber (reg:SI RETURN_ADDR_REGNUM))
   (clobber (reg:DI R2_REG))
   (clobber (reg:SI R12_REG))
   (clobber (reg:DI MUL64_OUT_REG))
   (clobber (reg:CC CC_REG))]
   "!TARGET_ANY_MPY
   && SFUNC_CHECK_PREDICABLE"
  "*return arc_output_libcall (\"__umulsidi3\");"
  [(set_attr "is_sfunc" "yes")
   (set_attr "predicable" "yes")])

(define_peephole2
  [(parallel
     [(set (reg:DI R0_REG)
	   (mult:DI (zero_extend:DI (reg:SI R0_REG))
		    (zero_extend:DI (reg:SI R1_REG))))
      (clobber (reg:SI RETURN_ADDR_REGNUM))
      (clobber (reg:DI R2_REG))
      (clobber (reg:SI R12_REG))
      (clobber (reg:DI MUL64_OUT_REG))
      (clobber (reg:CC CC_REG))])]
  "!TARGET_ANY_MPY
   && peep2_regno_dead_p (1, TARGET_BIG_ENDIAN ? R1_REG : R0_REG)"
  [(pc)]
{
  if (TARGET_BIG_ENDIAN)
    emit_insn (gen_umulsi3_highpart_600_lib_be ());
  else
    emit_insn (gen_umulsi3_highpart_600_lib_le ());
  DONE;
})

(define_expand "addsi3"
  [(set (match_operand:SI 0 "dest_reg_operand" "")
	(plus:SI (match_operand:SI 1 "register_operand" "")
		 (match_operand:SI 2 "nonmemory_operand" "")))]
  ""
  "if (flag_pic && arc_raw_symbolic_reference_mentioned_p (operands[2], false))
     {
       operands[2]=force_reg(SImode, operands[2]);
     }
  ")

(define_expand "adddi3"
  [(parallel [(set (match_operand:DI 0 "dest_reg_operand" "")
		   (plus:DI (match_operand:DI 1 "register_operand" "")
			    (match_operand:DI 2 "nonmemory_operand" "")))
	      (clobber (reg:CC CC_REG))])]
  ""
{})

; This assumes that there can be no strictly partial overlap between
; operands[1] and operands[2].
(define_insn_and_split "*adddi3_i"
  [(set (match_operand:DI 0 "dest_reg_operand" "=&w,w,w")
	(plus:DI (match_operand:DI 1 "register_operand" "%c,0,c")
		 (match_operand:DI 2 "nonmemory_operand" "ci,ci,!i")))
   (clobber (reg:CC CC_REG))]
  ""
  "#"
  "reload_completed"
  [(const_int 0)]
{
  int hi = !TARGET_BIG_ENDIAN;
  int lo = !hi;
  rtx l0 = operand_subword (operands[0], lo, 0, DImode);
  rtx h0 = operand_subword (operands[0], hi, 0, DImode);
  rtx l1 = operand_subword (operands[1], lo, 0, DImode);
  rtx h1 = operand_subword (operands[1], hi, 0, DImode);
  rtx l2 = operand_subword (operands[2], lo, 0, DImode);
  rtx h2 = operand_subword (operands[2], hi, 0, DImode);


  if (l2 == const0_rtx)
    {
      if (!rtx_equal_p (l0, l1) && !rtx_equal_p (l0, h1))
	emit_move_insn (l0, l1);
      emit_insn (gen_addsi3 (h0, h1, h2));
      if (!rtx_equal_p (l0, l1) && rtx_equal_p (l0, h1))
	emit_move_insn (l0, l1);
      DONE;
    }
  if (CONST_INT_P (operands[2]) && INTVAL (operands[2]) < 0
      && INTVAL (operands[2]) >= -0x7fffffff)
    {
      emit_insn (gen_subdi3_i (operands[0], operands[1],
		 GEN_INT (-INTVAL (operands[2]))));
      DONE;
    }
  if (rtx_equal_p (l0, h1))
    {
      if (h2 != const0_rtx)
	emit_insn (gen_addsi3 (h0, h1, h2));
      else if (!rtx_equal_p (h0, h1))
	emit_move_insn (h0, h1);
      emit_insn (gen_add_f (l0, l1, l2));
      emit_insn
	(gen_rtx_COND_EXEC
	  (VOIDmode,
	   gen_rtx_LTU (VOIDmode, gen_rtx_REG (CC_Cmode, CC_REG), GEN_INT (0)),
	   gen_rtx_SET (h0, plus_constant (SImode, h0, 1))));
      DONE;
    }
  emit_insn (gen_add_f (l0, l1, l2));
  emit_insn (gen_adc (h0, h1, h2));
  DONE;
}
  [(set_attr "cond" "clob")
   (set_attr "type" "binary")
   (set_attr "length" "16,16,20")])

(define_insn "add_f"
  [(set (reg:CC_C CC_REG)
	(compare:CC_C
	  (plus:SI (match_operand:SI 1 "register_operand" "c,0,c")
		   (match_operand:SI 2 "nonmemory_operand" "cL,I,cCal"))
	  (match_dup 1)))
   (set (match_operand:SI 0 "dest_reg_operand" "=w,Rcw,w")
	(plus:SI (match_dup 1) (match_dup 2)))]
  ""
  "add.f %0,%1,%2"
  [(set_attr "cond" "set")
   (set_attr "type" "compare")
   (set_attr "length" "4,4,8")])

(define_insn "*add_f_2"
  [(set (reg:CC_C CC_REG)
	(compare:CC_C
	  (plus:SI (match_operand:SI 1 "register_operand" "c,0,c")
		   (match_operand:SI 2 "nonmemory_operand" "cL,I,cCal"))
	  (match_dup 2)))
   (set (match_operand:SI 0 "dest_reg_operand" "=w,Rcw,w")
	(plus:SI (match_dup 1) (match_dup 2)))]
  ""
  "add.f %0,%1,%2"
  [(set_attr "cond" "set")
   (set_attr "type" "compare")
   (set_attr "length" "4,4,8")])

; w/c/c comes first (rather than w/0/C_0) to prevent the middle-end
; needlessly prioritizing the matching constraint.
; Rcw/0/C_0 comes before w/c/L so that the lower latency conditional
; execution is used where possible.
(define_insn_and_split "adc"
  [(set (match_operand:SI 0 "dest_reg_operand" "=w,Rcw,w,Rcw,w")
	(plus:SI (plus:SI (ltu:SI (reg:CC_C CC_REG) (const_int 0))
			  (match_operand:SI 1 "nonmemory_operand"
							 "%c,0,c,0,cCal"))
		 (match_operand:SI 2 "nonmemory_operand" "c,C_0,L,I,cCal")))]
  "register_operand (operands[1], SImode)
   || register_operand (operands[2], SImode)"
  "@
	adc %0,%1,%2
	add.cs %0,%1,1
	adc %0,%1,%2
	adc %0,%1,%2
	adc %0,%1,%2"
  ; if we have a bad schedule after sched2, split.
  "reload_completed
   && !optimize_size && (!TARGET_ARC600_FAMILY)
   && arc_scheduling_not_expected ()
   && arc_sets_cc_p (prev_nonnote_insn (insn))
   /* If next comes a return or other insn that needs a delay slot,
      expect the adc to get into the delay slot.  */
   && next_nonnote_insn (insn)
   && !arc_need_delay (next_nonnote_insn (insn))
   /* Restore operands before emitting.  */
   && (extract_insn_cached (insn), 1)"
  [(set (match_dup 0) (match_dup 3))
   (cond_exec
     (ltu (reg:CC_C CC_REG) (const_int 0))
     (set (match_dup 0) (plus:SI (match_dup 0) (const_int 1))))]
  "operands[3] = simplify_gen_binary (PLUS, SImode, operands[1], operands[2]);"
  [(set_attr "cond" "use")
   (set_attr "type" "cc_arith")
   (set_attr "length" "4,4,4,4,8")])

; combiner-splitter cmp / scc -> cmp / adc
(define_split
  [(set (match_operand:SI 0 "dest_reg_operand" "")
	(gtu:SI (match_operand:SI 1 "register_operand" "")
		(match_operand:SI 2 "register_operand" "")))
   (clobber (reg CC_REG))]
  ""
  [(set (reg:CC_C CC_REG) (compare:CC_C (match_dup 2) (match_dup 1)))
   (set (match_dup 0) (ltu:SI (reg:CC_C CC_REG) (const_int 0)))])

; combine won't work when an intermediate result is used later...
; add %0,%1,%2 ` cmp %0,%[12] -> add.f %0,%1,%2
(define_peephole2
  [(set (match_operand:SI 0 "dest_reg_operand" "")
	(plus:SI (match_operand:SI 1 "register_operand" "")
		 (match_operand:SI 2 "nonmemory_operand" "")))
   (set (reg:CC_C CC_REG)
	(compare:CC_C (match_dup 0)
		      (match_operand:SI 3 "nonmemory_operand" "")))]
  "rtx_equal_p (operands[1], operands[3])
   || rtx_equal_p (operands[2], operands[3])"
  [(parallel
     [(set (reg:CC_C CC_REG)
	   (compare:CC_C (plus:SI (match_dup 1) (match_dup 2)) (match_dup 1)))
      (set (match_dup 0)
	   (plus:SI (match_dup 1) (match_dup 2)))])])

; ??? need to delve into combine to find out why this is not useful.
; We'd like to be able to grok various C idioms for carry bit usage.
;(define_insn "*adc_0"
;  [(set (match_operand:SI 0 "dest_reg_operand" "=w")
;	(plus:SI (ltu:SI (reg:CC_C CC_REG) (const_int 0))
;		 (match_operand:SI 1 "register_operand" "c")))]
;  ""
;  "adc %0,%1,0"
;  [(set_attr "cond" "use")
;   (set_attr "type" "cc_arith")
;   (set_attr "length" "4")])
;
;(define_split
;  [(set (match_operand:SI 0 "dest_reg_operand" "=w")
;	(plus:SI (gtu:SI (match_operand:SI 1 "register_operand" "c")
;			 (match_operand:SI 2 "register_operand" "c"))
;		 (match_operand:SI 3 "register_operand" "c")))
;   (clobber (reg CC_REG))]
;  ""
;  [(set (reg:CC_C CC_REG) (compare:CC_C (match_dup 2) (match_dup 1)))
;   (set (match_dup 0)
;	(plus:SI (ltu:SI (reg:CC_C CC_REG) (const_int 0))
;		 (match_dup 3)))])

(define_expand "subsi3"
  [(set (match_operand:SI 0 "dest_reg_operand" "")
	(minus:SI (match_operand:SI 1 "nonmemory_operand" "")
		 (match_operand:SI 2 "nonmemory_operand" "")))]
  ""
  "
{
  int c = 1;

  if (!register_operand (operands[2], SImode))
    {
      operands[1] = force_reg (SImode, operands[1]);
      c = 2;
    }
  if (flag_pic && arc_raw_symbolic_reference_mentioned_p (operands[c], false))
    operands[c] = force_reg (SImode, operands[c]);
}")

; the casesi expander might generate a sub of zero, so we have to recognize it.
; combine should make such an insn go away.
(define_insn_and_split "subsi3_insn"
  [(set (match_operand:SI 0 "dest_reg_operand"           "=Rcqq,Rcqq,Rcw,Rcw,w,w,w,  w,  w,  w")
	(minus:SI (match_operand:SI 1 "nonmemory_operand"    "0,Rcqq,  0, cL,c,L,I,Cal,Cal,  c")
		  (match_operand:SI 2 "nonmemory_operand" "Rcqq,Rcqq,  c,  0,c,c,0,  0,  c,Cal")))]
  "register_operand (operands[1], SImode)
   || register_operand (operands[2], SImode)"
  "@
    sub%? %0,%1,%2%&
    sub%? %0,%1,%2%&
    sub%? %0,%1,%2
    rsub%? %0,%2,%1
    sub %0,%1,%2
    rsub %0,%2,%1
    rsub %0,%2,%1
    rsub%? %0,%2,%1
    rsub %0,%2,%1
    sub %0,%1,%2"
  "reload_completed && get_attr_length (insn) == 8
   && satisfies_constraint_I (operands[1])
   && GET_CODE (PATTERN (insn)) != COND_EXEC"
  [(set (match_dup 0) (match_dup 3)) (set (match_dup 0) (match_dup 4))]
  "split_subsi (operands);"
  [(set_attr "iscompact" "maybe,maybe,false,false,false,false,false,false,false, false")
  (set_attr "length" "*,*,4,4,4,4,4,8,8,8")
  (set_attr "predicable" "yes,no,yes,yes,no,no,no,yes,no,no")
  (set_attr "cond" "canuse,nocond,canuse,canuse,nocond,nocond,canuse_limm,canuse,nocond,nocond")
  (set_attr "cpu_facility" "*,cd,*,*,*,*,*,*,*,*")
  ])

(define_expand "subdi3"
  [(parallel [(set (match_operand:DI 0 "dest_reg_operand" "")
		   (minus:DI (match_operand:DI 1 "nonmemory_operand" "")
			     (match_operand:DI 2 "nonmemory_operand" "")))
	      (clobber (reg:CC CC_REG))])]
  ""
{
  if (!register_operand (operands[2], DImode))
    operands[1] = force_reg (DImode, operands[1]);
})

(define_insn_and_split "subdi3_i"
  [(set (match_operand:DI 0 "dest_reg_operand" "=&w,w,w,w,w")
	(minus:DI (match_operand:DI 1 "nonmemory_operand" "ci,0,ci,c,!i")
		  (match_operand:DI 2 "nonmemory_operand" "ci,ci,0,!i,c")))
   (clobber (reg:CC CC_REG))]
  "register_operand (operands[1], DImode)
   || register_operand (operands[2], DImode)"
  "#"
  "reload_completed"
  [(const_int 0)]
{
  int hi = !TARGET_BIG_ENDIAN;
  int lo = !hi;
  rtx l0 = operand_subword (operands[0], lo, 0, DImode);
  rtx h0 = operand_subword (operands[0], hi, 0, DImode);
  rtx l1 = operand_subword (operands[1], lo, 0, DImode);
  rtx h1 = operand_subword (operands[1], hi, 0, DImode);
  rtx l2 = operand_subword (operands[2], lo, 0, DImode);
  rtx h2 = operand_subword (operands[2], hi, 0, DImode);

  if (rtx_equal_p (l0, h1) || rtx_equal_p (l0, h2))
    {
      h1 = simplify_gen_binary (MINUS, SImode, h1, h2);
      if (!rtx_equal_p (h0, h1))
	emit_insn (gen_rtx_SET (h0, h1));
      emit_insn (gen_sub_f (l0, l1, l2));
      emit_insn
	(gen_rtx_COND_EXEC
	  (VOIDmode,
	   gen_rtx_LTU (VOIDmode, gen_rtx_REG (CC_Cmode, CC_REG), GEN_INT (0)),
	   gen_rtx_SET (h0, plus_constant (SImode, h0, -1))));
      DONE;
    }
  emit_insn (gen_sub_f (l0, l1, l2));
  emit_insn (gen_sbc (h0, h1, h2, gen_rtx_REG (CCmode, CC_REG)));
  DONE;
}
  [(set_attr "cond" "clob")
   (set_attr "length" "16,16,16,20,20")])

(define_insn "*sbc_0"
  [(set (match_operand:SI 0 "dest_reg_operand" "=w")
	(minus:SI (match_operand:SI 1 "register_operand" "c")
		  (ltu:SI (match_operand:CC_C 2 "cc_use_register")
			  (const_int 0))))]
  ""
  "sbc %0,%1,0"
  [(set_attr "cond" "use")
   (set_attr "type" "cc_arith")
   (set_attr "length" "4")])

; w/c/c comes first (rather than Rcw/0/C_0) to prevent the middle-end
; needlessly prioritizing the matching constraint.
; Rcw/0/C_0 comes before w/c/L so that the lower latency conditional execution
; is used where possible.
(define_insn_and_split "sbc"
  [(set (match_operand:SI 0 "dest_reg_operand" "=w,Rcw,w,Rcw,w")
	(minus:SI (minus:SI (match_operand:SI 1 "nonmemory_operand"
						"c,0,c,0,cCal")
			    (ltu:SI (match_operand:CC_C 3 "cc_use_register")
				    (const_int 0)))
		  (match_operand:SI 2 "nonmemory_operand" "c,C_0,L,I,cCal")))]
  "register_operand (operands[1], SImode)
   || register_operand (operands[2], SImode)"
  "@
	sbc %0,%1,%2
	sub.cs %0,%1,1
	sbc %0,%1,%2
	sbc %0,%1,%2
	sbc %0,%1,%2"
  ; if we have a bad schedule after sched2, split.
  "reload_completed
   && !optimize_size && (!TARGET_ARC600_FAMILY)
   && arc_scheduling_not_expected ()
   && arc_sets_cc_p (prev_nonnote_insn (insn))
   /* If next comes a return or other insn that needs a delay slot,
      expect the adc to get into the delay slot.  */
   && next_nonnote_insn (insn)
   && !arc_need_delay (next_nonnote_insn (insn))
   /* Restore operands before emitting.  */
   && (extract_insn_cached (insn), 1)"
  [(set (match_dup 0) (match_dup 4))
   (cond_exec
     (ltu (reg:CC_C CC_REG) (const_int 0))
     (set (match_dup 0) (plus:SI (match_dup 0) (const_int -1))))]
  "operands[4] = simplify_gen_binary (MINUS, SImode, operands[1], operands[2]);"
  [(set_attr "cond" "use")
   (set_attr "type" "cc_arith")
   (set_attr "length" "4,4,4,4,8")])

(define_insn "sub_f"
  [(set (reg:CC CC_REG)
	(compare:CC (match_operand:SI 1 "nonmemory_operand" " c,L,0,I,c,Cal")
		    (match_operand:SI 2 "nonmemory_operand" "cL,c,I,0,Cal,c")))
   (set (match_operand:SI 0 "dest_reg_operand" "=w,w,Rcw,Rcw,w,w")
	(minus:SI (match_dup 1) (match_dup 2)))]
  "register_operand (operands[1], SImode)
   || register_operand (operands[2], SImode)"
  "@
	sub.f %0,%1,%2
	rsub.f %0,%2,%1
	sub.f %0,%1,%2
	rsub.f %0,%2,%1
	sub.f %0,%1,%2
	sub.f %0,%1,%2"
  [(set_attr "type" "compare")
   (set_attr "length" "4,4,4,4,8,8")])

; combine won't work when an intermediate result is used later...
; add %0,%1,%2 ` cmp %0,%[12] -> add.f %0,%1,%2
(define_peephole2
  [(set (reg:CC CC_REG)
	(compare:CC (match_operand:SI 1 "register_operand" "")
		    (match_operand:SI 2 "nonmemory_operand" "")))
   (set (match_operand:SI 0 "dest_reg_operand" "")
	(minus:SI (match_dup 1) (match_dup 2)))]
  ""
  [(parallel
     [(set (reg:CC CC_REG) (compare:CC (match_dup 1) (match_dup 2)))
      (set (match_dup 0) (minus:SI (match_dup 1) (match_dup 2)))])])

(define_peephole2
  [(set (reg:CC CC_REG)
	(compare:CC (match_operand:SI 1 "register_operand" "")
		    (match_operand:SI 2 "nonmemory_operand" "")))
   (set (match_operand 3 "" "") (match_operand 4 "" ""))
   (set (match_operand:SI 0 "dest_reg_operand" "")
	(minus:SI (match_dup 1) (match_dup 2)))]
  "!reg_overlap_mentioned_p (operands[3], operands[1])
   && !reg_overlap_mentioned_p (operands[3], operands[2])
   && !reg_overlap_mentioned_p (operands[0], operands[4])
   && !reg_overlap_mentioned_p (operands[0], operands[3])"
  [(parallel
     [(set (reg:CC CC_REG) (compare:CC (match_dup 1) (match_dup 2)))
      (set (match_dup 0) (minus:SI (match_dup 1) (match_dup 2)))])
   (set (match_dup 3) (match_dup 4))])

(define_insn "*add_n"
  [(set (match_operand:SI 0 "dest_reg_operand" "=q,r,r")
	(plus:SI (mult:SI (match_operand:SI 1 "register_operand" "q,r,r")
			  (match_operand:SI 2 "_2_4_8_operand" ""))
		 (match_operand:SI 3 "nonmemory_operand" "0,r,Csz")))]
  ""
  "add%z2%?\\t%0,%3,%1%&"
  [(set_attr "type" "shift")
   (set_attr "length" "*,4,8")
   (set_attr "predicable" "yes,no,no")
   (set_attr "cond" "canuse,nocond,nocond")
   (set_attr "iscompact" "maybe,false,false")])

;; N.B. sub[123] has the operands of the MINUS in the opposite order from
;; what synth_mult likes.
(define_insn "*sub_n"
  [(set (match_operand:SI 0 "dest_reg_operand" "=Rcw,w,w")
	(minus:SI (match_operand:SI 1 "nonmemory_operand" "0,c,?Cal")
		  (ashift:SI (match_operand:SI 2 "register_operand" "c,c,c")
			     (match_operand:SI 3 "_1_2_3_operand" ""))))]
  ""
  "sub%c3%? %0,%1,%2"
  [(set_attr "type" "shift")
   (set_attr "length" "4,4,8")
   (set_attr "predicable" "yes,no,no")
   (set_attr "cond" "canuse,nocond,nocond")
   (set_attr "iscompact" "false")])

(define_insn "*sub_n"
  [(set (match_operand:SI 0 "dest_reg_operand" "=Rcw,w,w")
	(minus:SI (match_operand:SI 1 "nonmemory_operand" "0,c,?Cal")
		  (mult:SI (match_operand:SI 2 "register_operand" "c,c,c")
			   (match_operand:SI 3 "_2_4_8_operand" ""))))]
  ""
  "sub%z3%? %0,%1,%2"
  [(set_attr "type" "shift")
   (set_attr "length" "4,4,8")
   (set_attr "predicable" "yes,no,no")
   (set_attr "cond" "canuse,nocond,nocond")
   (set_attr "iscompact" "false")])

; ??? check if combine matches this.
(define_insn "*bset"
  [(set (match_operand:SI 0 "dest_reg_operand" "=Rcw,w,w")
	(ior:SI (ashift:SI (const_int 1)
			   (match_operand:SI 1 "nonmemory_operand" "cL,cL,c"))
		(match_operand:SI 2 "nonmemory_operand" "0,c,Cal")))]
  ""
  "bset%? %0,%2,%1"
  [(set_attr "length" "4,4,8")
   (set_attr "predicable" "yes,no,no")
   (set_attr "cond" "canuse,nocond,nocond")]
)

; ??? check if combine matches this.
(define_insn "*bxor"
  [(set (match_operand:SI 0 "dest_reg_operand" "=Rcw,w,w")
	(xor:SI (ashift:SI (const_int 1)
			   (match_operand:SI 1 "nonmemory_operand" "cL,cL,c"))
		(match_operand:SI 2 "nonmemory_operand" "0,c,Cal")))]
  ""
  "bxor%? %0,%2,%1"
  [(set_attr "length" "4,4,8")
   (set_attr "predicable" "yes,no,no")
   (set_attr "cond" "canuse,nocond,nocond")]
)

; ??? check if combine matches this.
(define_insn "*bclr"
  [(set (match_operand:SI 0 "dest_reg_operand" "=Rcw,w,w")
	(and:SI (not:SI (ashift:SI (const_int 1)
				   (match_operand:SI 1 "nonmemory_operand" "cL,cL,c")))
		(match_operand:SI 2 "nonmemory_operand" "0,c,Cal")))]
  ""
  "bclr%? %0,%2,%1"
  [(set_attr "length" "4,4,8")
   (set_attr "predicable" "yes,no,no")
   (set_attr "cond" "canuse,nocond,nocond")]
)

; ??? FIXME: find combine patterns for bmsk.

;;Following are the define_insns added for the purpose of peephole2's

; see also iorsi3 for use with constant bit number.
(define_insn "*bset_insn"
  [(set (match_operand:SI 0 "dest_reg_operand" "=Rcw,w,w")
	(ior:SI (match_operand:SI 1 "nonmemory_operand" "0,c,Cal")
		(ashift:SI (const_int 1)
			   (match_operand:SI 2 "nonmemory_operand" "cL,cL,c"))) ) ]
  ""
  "@
     bset%? %0,%1,%2 ;;peep2, constr 1
     bset %0,%1,%2 ;;peep2, constr 2
     bset %0,%1,%2 ;;peep2, constr 3"
  [(set_attr "length" "4,4,8")
   (set_attr "predicable" "yes,no,no")
   (set_attr "cond" "canuse,nocond,nocond")]
)

; see also xorsi3 for use with constant bit number.
(define_insn "*bxor_insn"
  [(set (match_operand:SI 0 "dest_reg_operand" "=Rcw,w,w")
	(xor:SI (match_operand:SI 1 "nonmemory_operand" "0,c,Cal")
		(ashift:SI (const_int 1)
			(match_operand:SI 2 "nonmemory_operand" "cL,cL,c"))) ) ]
  ""
  "@
     bxor%? %0,%1,%2
     bxor %0,%1,%2
     bxor %0,%1,%2"
  [(set_attr "length" "4,4,8")
   (set_attr "predicable" "yes,no,no")
   (set_attr "cond" "canuse,nocond,nocond")]
)

; see also andsi3 for use with constant bit number.
(define_insn "*bclr_insn"
  [(set (match_operand:SI 0 "dest_reg_operand" "=Rcw,w,w")
	(and:SI (not:SI (ashift:SI (const_int 1)
				   (match_operand:SI 2 "nonmemory_operand" "cL,rL,r")))
		(match_operand:SI 1 "nonmemory_operand" "0,c,Cal")))]
  ""
  "@
     bclr%? %0,%1,%2
     bclr %0,%1,%2
     bclr %0,%1,%2"
  [(set_attr "length" "4,4,8")
   (set_attr "predicable" "yes,no,no")
   (set_attr "cond" "canuse,nocond,nocond")]
)

; see also andsi3 for use with constant bit number.
(define_insn "*bmsk_insn"
  [(set (match_operand:SI 0 "dest_reg_operand" "=Rcw,w,w")
	(and:SI (match_operand:SI 1 "nonmemory_operand" "0,c,Cal")
		(plus:SI (ashift:SI (const_int 1)
				    (plus:SI (match_operand:SI 2 "nonmemory_operand" "rL,rL,r")
					     (const_int 1)))
			 (const_int -1))))]
  ""
  "@
     bmsk%? %0,%1,%2
     bmsk %0,%1,%2
     bmsk %0,%1,%2"
  [(set_attr "length" "4,4,8")
   (set_attr "predicable" "yes,no,no")
   (set_attr "cond" "canuse,nocond,nocond")]
)

;;Instructions added for peephole2s end

;; Boolean instructions.

(define_expand "andsi3"
  [(set (match_operand:SI 0 "dest_reg_operand" "")
	(and:SI (match_operand:SI 1 "nonimmediate_operand" "")
		(match_operand:SI 2 "nonmemory_operand" "")))]
  ""
  "if (!satisfies_constraint_Cux (operands[2]))
     operands[1] = force_reg (SImode, operands[1]);
  ")

(define_insn "andsi3_i"
  [(set (match_operand:SI 0 "dest_reg_operand"          "=Rcqq,Rcq,Rcqq,Rcqq,Rcqq,Rcw,Rcw,   Rcw,Rcw,Rcw,Rcw, w,     w,  w,  w,Rrq,w,Rcw,  w,W")
	(and:SI (match_operand:SI 1 "nonimmediate_operand" "%0,Rcq,   0,   0,Rcqq,  0,  c,     0,  0,  0,  0, c,     c,  c,  c,Rrq,0,  0,  c,o")
		(match_operand:SI 2 "nonmemory_operand"  "Rcqq,  0, C1p, Ccp, Cux, cL,  0,C2pC1p,Ccp,CnL,  I,Lc,C2pC1p,Ccp,CnL,Cbf,I,Cal,Cal,Cux")))]
  "(register_operand (operands[1], SImode)
    && nonmemory_operand (operands[2], SImode))
   || (memory_operand (operands[1], SImode)
       && satisfies_constraint_Cux (operands[2]))"
{
  switch (which_alternative)
    {
    case 0: case 5: case 10: case 11: case 16: case 17: case 18:
      return "and%? %0,%1,%2%&";
    case 1: case 6:
      return "and%? %0,%2,%1%&";
    case 2:
      return "bmsk%? %0,%1,%Z2%&";
    case 7: case 12:
     if (satisfies_constraint_C2p (operands[2]))
       {
	operands[2] = GEN_INT ((~INTVAL (operands[2])));
	return "bmskn%? %0,%1,%Z2%&";
       }
     else
       {
	return "bmsk%? %0,%1,%Z2%&";
       }
    case 3: case 8: case 13:
      return "bclr%? %0,%1,%M2%&";
    case 4:
      return (INTVAL (operands[2]) == 0xff
	      ? "extb%? %0,%1%&" : "ext%_%? %0,%1%&");
    case 9: case 14: return \"bic%? %0,%1,%n2-1\";
    case 15:
      return "movb.cl %0,%1,%p2,%p2,%s2";

    case 19:
      const char *tmpl;

      if (satisfies_constraint_Ucm (operands[1]))
	tmpl = (INTVAL (operands[2]) == 0xff
		? "xldb%U1 %0,%1" : "xld%_%U1 %0,%1");
      else
	tmpl = INTVAL (operands[2]) == 0xff ? "ldb %0,%1" : "ld%_ %0,%1";

      if (TARGET_BIG_ENDIAN)
	{
	  rtx xop[2];

	  xop[0] = operands[0];
	  xop[1] = adjust_address (operands[1], QImode,
				   INTVAL (operands[2]) == 0xff ? 3 : 2);
	  output_asm_insn (tmpl, xop);
	  return "";
	}
      return tmpl;
    default:
      gcc_unreachable ();
    }
}
  [(set_attr "iscompact" "maybe,maybe,maybe,maybe,true,false,false,false,false,false,false,false,false,false,false,false,false,false,false,false")
   (set_attr "type" "binary,binary,binary,binary,binary,binary,binary,binary,binary,binary,binary,binary,binary,binary,binary,shift,binary,binary,binary,load")
   (set_attr "length" "*,*,*,*,*,4,4,4,4,4,4,4,4,4,4,4,4,8,8,*")
   (set_attr "predicable" "no,no,no,no,no,yes,yes,yes,yes,yes,no,no,no,no,no,no,no,yes,no,no")
   (set_attr "cond" "canuse,canuse,canuse,canuse,nocond,canuse,canuse,canuse,canuse,canuse,canuse_limm,nocond,nocond,nocond,nocond,nocond,canuse_limm,canuse,nocond,nocond")])

; combiner splitter, pattern found in ldtoa.c .
; and op3,op0,op1 / cmp op3,op2 -> add op3,op0,op4 / bmsk.f 0,op3,op1
(define_split
  [(set (reg:CC_Z CC_REG)
	(compare:CC_Z (and:SI (match_operand:SI 0 "register_operand" "")
			      (match_operand 1 "const_int_operand" ""))
		      (match_operand 2 "const_int_operand" "")))
   (clobber (match_operand:SI 3 "register_operand" ""))]
  "((INTVAL (operands[1]) + 1) & INTVAL (operands[1])) == 0"
  [(set (match_dup 3)
	(plus:SI (match_dup 0) (match_dup 4)))
   (set (reg:CC_Z CC_REG)
	(compare:CC_Z (and:SI (match_dup 3) (match_dup 1))
		      (const_int 0)))]
  "operands[4] = GEN_INT ( -(~INTVAL (operands[1]) | INTVAL (operands[2])));")

;;bic define_insn that allows limm to be the first operand
(define_insn "*bicsi3_insn"
   [(set (match_operand:SI 0 "dest_reg_operand" "=Rcqq,Rcw,Rcw,Rcw,w,w,w")
 	(and:SI	(not:SI (match_operand:SI 1 "nonmemory_operand" "Rcqq,Lc,I,Cal,Lc,Cal,c"))
 		(match_operand:SI 2 "nonmemory_operand" "0,0,0,0,c,c,Cal")))]
  ""
  "@
   bic%? %0, %2, %1%& ;;constraint 0
   bic%? %0,%2,%1  ;;constraint 1
   bic %0,%2,%1    ;;constraint 2, FIXME: will it ever get generated ???
   bic%? %0,%2,%1  ;;constraint 3, FIXME: will it ever get generated ???
   bic %0,%2,%1    ;;constraint 4
   bic %0,%2,%1    ;;constraint 5, FIXME: will it ever get generated ???
   bic %0,%2,%1    ;;constraint 6"
  [(set_attr "length" "*,4,4,8,4,8,8")
  (set_attr "iscompact" "maybe, false, false, false, false, false, false")
  (set_attr "predicable" "no,yes,no,yes,no,no,no")
  (set_attr "cond" "canuse,canuse,canuse_limm,canuse,nocond,nocond,nocond")])

(define_insn "iorsi3"
  [(set (match_operand:SI 0 "dest_reg_operand"        "=Rcqq,Rcq,Rcqq,Rcw,Rcw,Rcw,Rcw,w,  w,w,Rcw,  w")
	(ior:SI (match_operand:SI 1 "nonmemory_operand" "% 0,Rcq,   0,  0,  c,  0, 0, c,  c,0,  0,  c")
		(match_operand:SI 2 "nonmemory_operand" "Rcqq, 0, C0p, cL,  0,C0p, I,cL,C0p,I,Cal,Cal")))]
  ""
  "*
  switch (which_alternative)
    {
    case 0: case 3: case 6: case 7: case 9: case 10: case 11:
      return \"or%? %0,%1,%2%&\";
    case 1: case 4:
      return \"or%? %0,%2,%1%&\";
    case 2: case 5: case 8:
      return \"bset%? %0,%1,%z2%&\";
    default:
      gcc_unreachable ();
    }"
  [(set_attr "iscompact" "maybe,maybe,maybe,false,false,false,false,false,false,false,false,false")
   (set_attr "length" "*,*,*,4,4,4,4,4,4,4,8,8")
   (set_attr "predicable" "no,no,no,yes,yes,yes,no,no,no,no,yes,no")
   (set_attr "cond" "canuse,canuse,canuse,canuse,canuse,canuse,canuse_limm,nocond,nocond,canuse_limm,canuse,nocond")])

(define_insn "xorsi3"
  [(set (match_operand:SI 0 "dest_reg_operand"          "=Rcqq,Rcq,Rcw,Rcw,Rcw,Rcw, w,  w,w,  w,  w")
	(xor:SI (match_operand:SI 1 "register_operand"  "%0,   Rcq,  0,  c,  0,  0, c,  c,0,  0,  c")
		(match_operand:SI 2 "nonmemory_operand" " Rcqq,  0, cL,  0,C0p,  I,cL,C0p,I,Cal,Cal")))]
  ""
  "*
  switch (which_alternative)
    {
    case 0: case 2: case 5: case 6: case 8: case 9: case 10:
      return \"xor%? %0,%1,%2%&\";
    case 1: case 3:
      return \"xor%? %0,%2,%1%&\";
    case 4: case 7:
      return \"bxor%? %0,%1,%z2\";
    default:
      gcc_unreachable ();
    }
  "
  [(set_attr "iscompact" "maybe,maybe,false,false,false,false,false,false,false,false,false")
   (set_attr "type" "binary")
   (set_attr "length" "*,*,4,4,4,4,4,4,4,8,8")
   (set_attr "predicable" "no,no,yes,yes,yes,no,no,no,no,yes,no")
   (set_attr "cond" "canuse,canuse,canuse,canuse,canuse,canuse_limm,nocond,nocond,canuse_limm,canuse,nocond")])

(define_insn "negsi2"
  [(set (match_operand:SI 0 "dest_reg_operand" "=Rcqq,Rcqq,Rcw,w")
	(neg:SI (match_operand:SI 1 "register_operand" "0,Rcqq,0,c")))]
  ""
  "neg%? %0,%1%&"
  [(set_attr "type" "unary")
   (set_attr "iscompact" "maybe,true,false,false")
   (set_attr "predicable" "no,no,yes,no")])

(define_insn "one_cmplsi2"
  [(set (match_operand:SI 0 "dest_reg_operand" "=Rcqq,w")
	(not:SI (match_operand:SI 1 "register_operand" "Rcqq,c")))]
  ""
  "not%? %0,%1%&"
  [(set_attr "type" "unary,unary")
   (set_attr "iscompact" "true,false")])

(define_insn_and_split "one_cmpldi2"
  [(set (match_operand:DI 0 "dest_reg_operand" "=q,w")
	(not:DI (match_operand:DI 1 "register_operand" "q,c")))]
  ""
  "#"
  "&& reload_completed"
  [(set (match_dup 2) (not:SI (match_dup 3)))
   (set (match_dup 4) (not:SI (match_dup 5)))]
{
  int swap = (true_regnum (operands[0]) == true_regnum (operands[1]) + 1);

  operands[2] = operand_subword (operands[0], 0+swap, 0, DImode);
  operands[3] = operand_subword (operands[1], 0+swap, 0, DImode);
  operands[4] = operand_subword (operands[0], 1-swap, 0, DImode);
  operands[5] = operand_subword (operands[1], 1-swap, 0, DImode);
}
  [(set_attr "type" "unary,unary")
   (set_attr "cond" "nocond,nocond")
   (set_attr "length" "4,8")])

;; Shift instructions.

(define_expand "ashlsi3"
  [(set (match_operand:SI 0 "dest_reg_operand" "")
	(ashift:SI (match_operand:SI 1 "register_operand" "")
		   (match_operand:SI 2 "nonmemory_operand" "")))]
  ""
  "
{
  if (!TARGET_BARREL_SHIFTER)
    {
      emit_shift (ASHIFT, operands[0], operands[1], operands[2]);
      DONE;
    }
}")

(define_expand "ashrsi3"
  [(set (match_operand:SI 0 "dest_reg_operand" "")
	(ashiftrt:SI (match_operand:SI 1 "register_operand" "")
		     (match_operand:SI 2 "nonmemory_operand" "")))]
  ""
  "
{
  if (!TARGET_BARREL_SHIFTER)
    {
      emit_shift (ASHIFTRT, operands[0], operands[1], operands[2]);
      DONE;
    }
}")

(define_expand "lshrsi3"
  [(set (match_operand:SI 0 "dest_reg_operand" "")
	(lshiftrt:SI (match_operand:SI 1 "register_operand" "")
		     (match_operand:SI 2 "nonmemory_operand" "")))]
  ""
  "
{
  if (!TARGET_BARREL_SHIFTER)
    {
      emit_shift (LSHIFTRT, operands[0], operands[1], operands[2]);
      DONE;
    }
}")

(define_insn "shift_si3"
  [(set (match_operand:SI 0 "dest_reg_operand" "=r")
	(match_operator:SI 3 "shift4_operator"
			   [(match_operand:SI 1 "register_operand" "0")
			    (match_operand:SI 2 "const_int_operand" "n")]))
   (clobber (match_scratch:SI 4 "=&r"))
   (clobber (reg:CC CC_REG))
  ]
  "!TARGET_BARREL_SHIFTER"
  "* return output_shift (operands);"
  [(set_attr "type" "shift")
   (set_attr "length" "16")])

(define_insn "shift_si3_loop"
  [(set (match_operand:SI 0 "dest_reg_operand" "=r,r")
	(match_operator:SI 3 "shift_operator"
			   [(match_operand:SI 1 "register_operand" "0,0")
			    (match_operand:SI 2 "nonmemory_operand" "rn,Cal")]))
   (clobber (match_scratch:SI 4 "=X,X"))
   (clobber (reg:SI LP_COUNT))
   (clobber (reg:SI LP_START))
   (clobber (reg:SI LP_END))
   (clobber (reg:CC CC_REG))
  ]
  "!TARGET_BARREL_SHIFTER"
  "* return output_shift (operands);"
  [(set_attr "type" "shift")
   (set_attr "length" "16,20")])

; asl, asr, lsr patterns:
; There is no point in including an 'I' alternative since only the lowest 5
; bits are used for the shift.  OTOH Cal can be useful if the shift amount
; is defined in an external symbol, as we don't have special relocations
; to truncate a symbol in a u6 immediate; but that's rather exotic, so only
; provide one alternatice for this, without condexec support.
(define_insn "*ashlsi3_insn"
  [(set (match_operand:SI 0 "dest_reg_operand"           "=Rcq,Rcqq,Rcqq,Rcw, w,   w")
	(ashift:SI (match_operand:SI 1 "nonmemory_operand" "!0,Rcqq,   0,  0, c,cCsz")
		   (match_operand:SI 2 "nonmemory_operand"  "K,  K,RcqqM, cL,cL,cCal")))]
  "TARGET_BARREL_SHIFTER
   && (register_operand (operands[1], SImode)
       || register_operand (operands[2], SImode))"
  "asl%? %0,%1,%2%&"
  [(set_attr "type" "shift")
   (set_attr "iscompact" "maybe,maybe,maybe,false,false,false")
   (set_attr "predicable" "no,no,no,yes,no,no")
   (set_attr "cond" "canuse,nocond,canuse,canuse,nocond,nocond")])

(define_insn "*ashrsi3_insn"
  [(set (match_operand:SI 0 "dest_reg_operand"             "=Rcq,Rcqq,Rcqq,Rcw, w,   w")
	(ashiftrt:SI (match_operand:SI 1 "nonmemory_operand" "!0,Rcqq,   0,  0, c,cCsz")
		     (match_operand:SI 2 "nonmemory_operand"  "K,  K,RcqqM, cL,cL,cCal")))]
  "TARGET_BARREL_SHIFTER
   && (register_operand (operands[1], SImode)
       || register_operand (operands[2], SImode))"
  "asr%? %0,%1,%2%&"
  [(set_attr "type" "shift")
   (set_attr "iscompact" "maybe,maybe,maybe,false,false,false")
   (set_attr "predicable" "no,no,no,yes,no,no")
   (set_attr "cond" "canuse,nocond,canuse,canuse,nocond,nocond")])

(define_insn "*lshrsi3_insn"
  [(set (match_operand:SI 0 "dest_reg_operand"             "=Rcq,Rcqq,Rcqq,Rcw, w,   w")
	(lshiftrt:SI (match_operand:SI 1 "nonmemory_operand" "!0,Rcqq,   0,  0, c,cCal")
		     (match_operand:SI 2 "nonmemory_operand"  "N,  N,RcqqM, cL,cL,cCal")))]
  "TARGET_BARREL_SHIFTER
   && (register_operand (operands[1], SImode)
       || register_operand (operands[2], SImode))"
  "*return (which_alternative <= 1 && !arc_ccfsm_cond_exec_p ()
	    ?  \"lsr%? %0,%1%&\" : \"lsr%? %0,%1,%2%&\");"
  [(set_attr "type" "shift")
   (set_attr "iscompact" "maybe,maybe,maybe,false,false,false")
   (set_attr "predicable" "no,no,no,yes,no,no")
   (set_attr "cond" "canuse,nocond,canuse,canuse,nocond,nocond")])

(define_insn "rotrsi3"
  [(set (match_operand:SI 0 "dest_reg_operand"             "=Rcw, w,   w")
	(rotatert:SI (match_operand:SI 1 "register_operand"  " 0,cL,cCsz")
		     (match_operand:SI 2 "nonmemory_operand" "cL,cL,cCal")))]
  "TARGET_BARREL_SHIFTER"
  "ror%? %0,%1,%2"
  [(set_attr "type" "shift,shift,shift")
   (set_attr "predicable" "yes,no,no")
   (set_attr "length" "4,4,8")])

;; Compare / branch instructions.

(define_expand "cbranchsi4"
  [(set (reg:CC CC_REG)
	(compare:CC (match_operand:SI 1 "nonmemory_operand" "")
		    (match_operand:SI 2 "nonmemory_operand" "")))
   (set (pc)
	(if_then_else
	      (match_operator 0 "ordered_comparison_operator" [(reg CC_REG)
							       (const_int 0)])
	      (label_ref (match_operand 3 "" ""))
	      (pc)))]
  ""
{
  gcc_assert (XEXP (operands[0], 0) == operands[1]);
  gcc_assert (XEXP (operands[0], 1) == operands[2]);
  operands[0] = gen_compare_reg (operands[0], VOIDmode);
  emit_jump_insn (gen_branch_insn (operands[3], operands[0]));
  DONE;
})

;; ??? Could add a peephole to generate compare with swapped operands and
;; modifed cc user if second, but not first operand is a compact register.
(define_insn "cmpsi_cc_insn_mixed"
  [(set (reg:CC CC_REG)
	(compare:CC (match_operand:SI 0 "register_operand" "Rcq#q,Rcqq,  h, c, c,qRcq,c")
		    (match_operand:SI 1 "nonmemory_operand"   "cO,  hO,Cm1,cI,cL, Cal,Cal")))]
  ""
  "cmp%? %0,%B1%&"
  [(set_attr "type" "compare")
   (set_attr "iscompact" "true,true,true,false,false,true_limm,false")
   (set_attr "predicable" "no,no,no,no,yes,no,yes")
   (set_attr "cond" "set")
   (set_attr "length" "*,*,*,4,4,*,8")
   (set_attr "cpu_facility" "av1,av2,*,*,*,*,*")])

(define_insn "*cmpsi_cc_zn_insn"
  [(set (reg:CC_ZN CC_REG)
	(compare:CC_ZN (match_operand:SI 0 "register_operand"  "qRcq,c")
		       (const_int 0)))]
  ""
  "tst%? %0,%0%&"
  [(set_attr "type" "compare,compare")
   (set_attr "iscompact" "true,false")
   (set_attr "predicable" "no,yes")
   (set_attr "cond" "set_zn")
   (set_attr "length" "*,4")])

; combiner pattern observed for unwind-dw2-fde.c:linear_search_fdes.
(define_insn "*btst"
  [(set (reg:CC_ZN CC_REG)
	(compare:CC_ZN
	  (zero_extract:SI (match_operand:SI 0 "register_operand" "Rcqq,c")
			   (const_int 1)
			   (match_operand:SI 1 "nonmemory_operand" "L,Lc"))
	  (const_int 0)))]
  ""
  "btst%? %0,%1"
  [(set_attr "iscompact" "true,false")
   (set_attr "predicable" "no,yes")
   (set_attr "cond" "set")
   (set_attr "type" "compare")
   (set_attr "length" "*,4")])

; combine suffers from 'simplifications' that replace a one-bit zero
; extract with a shift if it can prove that the upper bits are zero.
; arc_reorg sees the code after sched2, which can have caused our
; inputs to be clobbered even if they were not clobbered before.
; Therefore, add a third way to convert btst / b{eq,ne} to bbit{0,1}
; OTOH, this is somewhat marginal, and can leat to out-of-range
; bbit (i.e. bad scheduling) and missed conditional execution,
; so make this an option.
(define_peephole2
  [(set (reg:CC_ZN CC_REG)
	(compare:CC_ZN
	  (zero_extract:SI (match_operand:SI 0 "register_operand" "")
			   (const_int 1)
			   (match_operand:SI 1 "nonmemory_operand" ""))
	  (const_int 0)))
   (set (pc)
	(if_then_else (match_operator 3 "equality_comparison_operator"
				      [(reg:CC_ZN CC_REG) (const_int 0)])
		      (label_ref (match_operand 2 "" ""))
		      (pc)))]
  "TARGET_BBIT_PEEPHOLE && peep2_regno_dead_p (2, CC_REG)"
  [(parallel [(set (pc)
		   (if_then_else
		     (match_op_dup 3
		       [(zero_extract:SI (match_dup 0)
					 (const_int 1) (match_dup 1))
			(const_int 0)])
		     (label_ref (match_dup 2))
		     (pc)))
	      (clobber (reg:CC_ZN CC_REG))])])

(define_insn "*cmpsi_cc_z_insn"
  [(set (reg:CC_Z CC_REG)
	(compare:CC_Z (match_operand:SI 0 "register_operand"  "qRcq,c")
		      (match_operand:SI 1 "p2_immediate_operand"  "O,n")))]
  ""
  "@
	cmp%? %0,%1%&
	bxor.f 0,%0,%z1"
  [(set_attr "type" "compare,compare")
   (set_attr "iscompact" "true,false")
   (set_attr "cond" "set,set_zn")
   (set_attr "length" "*,4")])

(define_insn "*cmpsi_cc_c_insn"
  [(set (reg:CC_C CC_REG)
	(compare:CC_C (match_operand:SI 0 "register_operand"  "Rcqq,Rcqq,  h, c,Rcqq,  c")
		      (match_operand:SI 1 "nonmemory_operand"   "cO,  hO,Cm1,cI, Cal,Cal")))]
  ""
  "cmp%? %0,%1%&"
  [(set_attr "type" "compare")
   (set_attr "iscompact" "true,true,true,false,true_limm,false")
   (set_attr "cond" "set")
   (set_attr "length" "*,*,*,4,*,8")
   (set_attr "cpu_facility" "av1,av2,*,*,*,*")])

;; Next come the scc insns.

(define_expand "cstoresi4"
  [(set (match_operand:SI 0 "dest_reg_operand" "")
	(match_operator:SI 1 "ordered_comparison_operator"
			   [(match_operand:SI 2 "nonmemory_operand" "")
			    (match_operand:SI 3 "nonmemory_operand" "")]))]
  ""
{
  if (!TARGET_CODE_DENSITY)
  {
   gcc_assert (XEXP (operands[1], 0) == operands[2]);
   gcc_assert (XEXP (operands[1], 1) == operands[3]);
   operands[1] = gen_compare_reg (operands[1], SImode);
   emit_insn (gen_scc_insn (operands[0], operands[1]));
   DONE;
  }
  if (!register_operand (operands[2], SImode))
    operands[2] = force_reg (SImode, operands[2]);

})

(define_mode_iterator SDF [(SF "TARGET_FP_SP_BASE || TARGET_OPTFPE")
			   (DF "TARGET_OPTFPE")])

(define_expand "cstore<mode>4"
  [(set (reg:CC CC_REG)
	(compare:CC (match_operand:SDF 2 "register_operand" "")
		    (match_operand:SDF 3 "register_operand" "")))
   (set (match_operand:SI 0 "dest_reg_operand" "")
	(match_operator:SI 1 "comparison_operator" [(reg CC_REG)
						    (const_int 0)]))]

  "TARGET_FP_SP_BASE || TARGET_OPTFPE"
{
  gcc_assert (XEXP (operands[1], 0) == operands[2]);
  gcc_assert (XEXP (operands[1], 1) == operands[3]);
  operands[1] = gen_compare_reg (operands[1], SImode);
  emit_insn (gen_scc_insn (operands[0], operands[1]));
  DONE;
})

; We need a separate expander for this lest we loose the mode of CC_REG
; when match_operator substitutes the literal operand into the comparison.
(define_expand "scc_insn"
  [(set (match_operand:SI 0 "dest_reg_operand" "=w") (match_operand:SI 1 ""))])

(define_insn_and_split "*scc_insn"
  [(set (match_operand:SI 0 "dest_reg_operand" "=w")
	(match_operator:SI 1 "proper_comparison_operator" [(reg CC_REG) (const_int 0)]))]
  ""
  "#"
  "reload_completed"
  [(set (match_dup 0) (const_int 1))
   (cond_exec
     (match_dup 1)
     (set (match_dup 0) (const_int 0)))]
{
  operands[1]
    = gen_rtx_fmt_ee (REVERSE_CONDITION (GET_CODE (operands[1]),
					 GET_MODE (XEXP (operands[1], 0))),
		      VOIDmode,
		      XEXP (operands[1], 0), XEXP (operands[1], 1));
}
  [(set_attr "type" "unary")])

;; ??? At least for ARC600, we should use sbc b,b,s12 if we want a value
;; that is one lower if the carry flag is set.

;; ??? Look up negscc insn.  See pa.md for example.
(define_insn "*neg_scc_insn"
  [(set (match_operand:SI 0 "dest_reg_operand" "=w")
	(neg:SI (match_operator:SI 1 "proper_comparison_operator"
		 [(reg CC_REG) (const_int 0)])))]
  ""
  "mov %0,-1\;sub.%D1 %0,%0,%0"
  [(set_attr "type" "unary")
   (set_attr "length" "8")])

(define_insn "*not_scc_insn"
  [(set (match_operand:SI 0 "dest_reg_operand" "=w")
	(not:SI (match_operator:SI 1 "proper_comparison_operator"
		 [(reg CC_REG) (const_int 0)])))]
  ""
  "mov %0,1\;sub.%d1 %0,%0,%0"
  [(set_attr "type" "unary")
   (set_attr "length" "8")])

; cond_exec patterns
(define_insn "*movsi_ne"
  [(cond_exec
     (ne (match_operand:CC_Z 2 "cc_use_register"    "Rcc,  Rcc,  Rcc,Rcc,Rcc") (const_int 0))
     (set (match_operand:SI 0 "dest_reg_operand" "=Rcq#q,Rcq#q,Rcq#q,  w,w")
	  (match_operand:SI 1 "nonmemory_operand"   "C_0,    h, ?Cal, Lc,?Cal")))]
  ""
  "@
	* current_insn_predicate = 0; return \"sub%?.ne %0,%0,%0%&\";
	* current_insn_predicate = 0; return \"mov%?.ne %0,%1\";
	* current_insn_predicate = 0; return \"mov%?.ne %0,%1\";
	mov.ne %0,%1
	mov.ne %0,%1"
  [(set_attr "type" "cmove")
   (set_attr "iscompact" "true,true,true_limm,false,false")
   (set_attr "length" "2,2,6,4,8")
   (set_attr "cpu_facility" "*,av2,av2,*,*")])

(define_insn "*movsi_cond_exec"
  [(cond_exec
     (match_operator 3 "proper_comparison_operator"
       [(match_operand 2 "cc_register" "Rcc,Rcc") (const_int 0)])
     (set (match_operand:SI 0 "dest_reg_operand" "=w,w")
	  (match_operand:SI 1 "nonmemory_operand" "LRac,?Cal")))]
  ""
  "mov.%d3 %0,%1"
  [(set_attr "type" "cmove")
   (set_attr "length" "4,8")])

(define_insn "*commutative_cond_exec"
  [(cond_exec
     (match_operator 5 "proper_comparison_operator"
       [(match_operand 4 "cc_register" "Rcc,Rcc") (const_int 0)])
     (set (match_operand:SI 0 "dest_reg_operand" "=w,w")
	  (match_operator:SI 3 "commutative_operator"
	    [(match_operand:SI 1 "register_operand" "%0,0")
	     (match_operand:SI 2 "nonmemory_operand" "cL,?Cal")])))]
  ""
{
  arc_output_commutative_cond_exec (operands, true);
  return "";
}
  [(set_attr "cond" "use")
   (set_attr "type" "cmove")
   (set_attr_alternative "length"
     [(const_int 4)
      (cond
	[(eq (symbol_ref "arc_output_commutative_cond_exec (operands, false)")
	     (const_int 4))
	 (const_int 4)]
	(const_int 8))])])

(define_insn "*sub_cond_exec"
  [(cond_exec
     (match_operator 4 "proper_comparison_operator"
       [(match_operand 3 "cc_register" "Rcc,Rcc,Rcc") (const_int 0)])
     (set (match_operand:SI 0 "dest_reg_operand" "=w,w,w")
	  (minus:SI (match_operand:SI 1 "nonmemory_operand" "0,cL,Cal")
		    (match_operand:SI 2 "nonmemory_operand" "cL,0,0"))))]
  ""
  "@
	sub.%d4 %0,%1,%2
	rsub.%d4 %0,%2,%1
	rsub.%d4 %0,%2,%1"
  [(set_attr "cond" "use")
   (set_attr "type" "cmove")
   (set_attr "length" "4,4,8")])

(define_insn "*noncommutative_cond_exec"
  [(cond_exec
     (match_operator 5 "proper_comparison_operator"
       [(match_operand 4 "cc_register" "Rcc,Rcc") (const_int 0)])
     (set (match_operand:SI 0 "dest_reg_operand" "=w,w")
	  (match_operator:SI 3 "noncommutative_operator"
	    [(match_operand:SI 1 "register_operand" "0,0")
	     (match_operand:SI 2 "nonmemory_operand" "cL,Cal")])))]
  ""
  "%O3.%d5 %0,%1,%2"
  [(set_attr "cond" "use")
   (set_attr "type" "cmove")
   (set_attr "length" "4,8")])

;; These control RTL generation for conditional jump insns
;; Match both normal and inverted jump.

; We need a separate expander for this lest we loose the mode of CC_REG
; when match_operator substitutes the literal operand into the comparison.
(define_expand "branch_insn"
  [(set (pc)
	(if_then_else (match_operand 1 "" "")
		      (label_ref (match_operand 0 "" ""))
		      (pc)))])

; When estimating sizes during arc_reorg, when optimizing for speed, there
; are three reasons why we need to consider branches to be length 6:
; - annull-false delay slot insns are implemented using conditional execution,
;   thus preventing short insn formation where used.
; - for ARC600: annull-true delay slot isnns are implemented where possile
;   using conditional execution, preventing short insn formation where used.
; - for ARC700: likely or somewhat likely taken branches are made long and
;   unaligned if possible to avoid branch penalty.
(define_insn "*branch_insn"
  [(set (pc)
	(if_then_else (match_operator 1 "proper_comparison_operator"
				      [(reg CC_REG) (const_int 0)])
		      (label_ref (match_operand 0 "" ""))
		      (pc)))]
  ""
  "*
{
  if (arc_ccfsm_branch_deleted_p ())
    {
      arc_ccfsm_record_branch_deleted ();
      return \"; branch deleted, next insns conditionalized\";
    }
  else
    {
      arc_ccfsm_record_condition (operands[1], false, insn, 0);
      if (get_attr_length (insn) == 2)
	 return \"b%d1%? %^%l0%&\";
      else
	 return \"b%d1%# %^%l0\";
    }
}"
  [(set_attr "type" "branch")
   (set
     (attr "length")
     (cond [
       (eq_attr "delay_slot_filled" "yes")
       (const_int 4)

       (ne
	 (if_then_else
	   (match_operand 1 "equality_comparison_operator" "")
	   (ior (lt (minus (match_dup 0) (pc)) (const_int -512))
		(gt (minus (match_dup 0) (pc))
		    (minus (const_int 506)
			   (symbol_ref "get_attr_delay_slot_length (insn)"))))
	   (ior (match_test "!arc_short_comparison_p (operands[1], -64)")
		(lt (minus (match_dup 0) (pc)) (const_int -64))
		(gt (minus (match_dup 0) (pc))
		    (minus (const_int 58)
			   (symbol_ref "get_attr_delay_slot_length (insn)")))))
	 (const_int 0))
       (const_int 4)]
      (const_int 2)))

   (set (attr "iscompact")
	(cond [(match_test "get_attr_length (insn) == 2") (const_string "true")]
	      (const_string "false")))])

(define_insn "*rev_branch_insn"
  [(set (pc)
	(if_then_else (match_operator 1 "proper_comparison_operator"
				      [(reg CC_REG) (const_int 0)])
		      (pc)
		      (label_ref (match_operand 0 "" ""))))]
  "REVERSIBLE_CC_MODE (GET_MODE (XEXP (operands[1], 0)))"
  "*
{
  if (arc_ccfsm_branch_deleted_p ())
    {
      arc_ccfsm_record_branch_deleted ();
      return \"; branch deleted, next insns conditionalized\";
    }
  else
    {
      arc_ccfsm_record_condition (operands[1], true, insn, 0);
      if (get_attr_length (insn) == 2)
	 return \"b%D1%? %^%l0\";
      else
	 return \"b%D1%# %^%l0\";
    }
}"
  [(set_attr "type" "branch")
   (set
     (attr "length")
     (cond [
       (eq_attr "delay_slot_filled" "yes")
       (const_int 4)

       (ne
	 (if_then_else
	   (match_operand 1 "equality_comparison_operator" "")
	   (ior (lt (minus (match_dup 0) (pc)) (const_int -512))
		(gt (minus (match_dup 0) (pc))
		    (minus (const_int 506)
			   (symbol_ref "get_attr_delay_slot_length (insn)"))))
	   (ior (match_test "!arc_short_comparison_p (operands[1], -64)")
		(lt (minus (match_dup 0) (pc)) (const_int -64))
		(gt (minus (match_dup 0) (pc))
		    (minus (const_int 58)
			   (symbol_ref "get_attr_delay_slot_length (insn)")))))
	 (const_int 0))
       (const_int 4)]
      (const_int 2)))

   (set (attr "iscompact")
	(cond [(match_test "get_attr_length (insn) == 2") (const_string "true")]
	      (const_string "false")))])

;; Unconditional and other jump instructions.

(define_expand "jump"
  [(set (pc) (label_ref (match_operand 0 "" "")))]
  ""
  "")

(define_insn "jump_i"
  [(set (pc) (label_ref (match_operand 0 "" "")))]
  "!TARGET_LONG_CALLS_SET || !CROSSING_JUMP_P (insn)"
  "b%!%* %^%l0%&"
  [(set_attr "type" "uncond_branch")
   (set (attr "iscompact")
	(if_then_else (match_test "get_attr_length (insn) == 2")
		      (const_string "true") (const_string "false")))
   (set_attr "cond" "canuse")
   (set (attr "length")
	(cond [
	  ; In arc_reorg we just guesstimate; might be more or less than 4.
	  (match_test "arc_branch_size_unknown_p ()")
	  (const_int 4)

	  (eq_attr "delay_slot_filled" "yes")
	  (const_int 4)

	  (match_test "CROSSING_JUMP_P (insn)")
	  (const_int 4)

	  (ior (lt (minus (match_dup 0) (pc)) (const_int -512))
	       (gt (minus (match_dup 0) (pc))
		   (minus (const_int 506)
			  (symbol_ref "get_attr_delay_slot_length (insn)"))))
	  (const_int 4)]
	 (const_int 2)))])

(define_insn "indirect_jump"
  [(set (pc) (match_operand:SI 0 "nonmemory_operand" "L,I,Cal,Rcqq,r"))]
  ""
  "@
   j%!%* %0%&
   j%!%* %0%&
   j%!%* %0%&
   j%!%* [%0]%&
   j%!%* [%0]%&"
  [(set_attr "type" "jump")
   (set_attr "iscompact" "false,false,false,maybe,false")
   (set_attr "cond" "canuse,canuse_limm,canuse,canuse,canuse")])

;; Implement a switch statement.

(define_expand "casesi"
  [(set (match_dup 5)
	(minus:SI (match_operand:SI 0 "register_operand" "")
		  (match_operand:SI 1 "nonmemory_operand" "")))
   (set (reg:CC CC_REG)
	(compare:CC (match_dup 5)
		    (match_operand:SI 2 "nonmemory_operand" "")))
   (set (pc)
	(if_then_else (gtu (reg:CC CC_REG)
			   (const_int 0))
		      (label_ref (match_operand 4 "" ""))
		      (pc)))
   (set (match_dup 6)
	(unspec:SI [(match_operand 3 "" "")
		    (match_dup 5) (match_dup 7)] UNSPEC_ARC_CASESI))
   (parallel [(set (pc) (match_dup 6)) (use (match_dup 7))])]
  ""
  "
{
  rtx x;

  operands[5] = gen_reg_rtx (SImode);
  operands[6] = gen_reg_rtx (SImode);
  operands[7] = operands[3];
  emit_insn (gen_subsi3 (operands[5], operands[0], operands[1]));
  emit_insn (gen_cmpsi_cc_insn_mixed (operands[5], operands[2]));
  x = gen_rtx_GTU (VOIDmode, gen_rtx_REG (CCmode, CC_REG), const0_rtx);
  x = gen_rtx_IF_THEN_ELSE (VOIDmode, x,
			    gen_rtx_LABEL_REF (VOIDmode, operands[4]), pc_rtx);
  emit_jump_insn (gen_rtx_SET (pc_rtx, x));
  if (TARGET_COMPACT_CASESI)
    {
      emit_jump_insn (gen_casesi_compact_jump (operands[5], operands[7]));
    }
  else
    {
      operands[3] = gen_rtx_LABEL_REF (VOIDmode, operands[3]);
      if (flag_pic || !cse_not_expected)
	operands[3] = force_reg (Pmode, operands[3]);
      emit_insn (gen_casesi_load (operands[6],
				  operands[3], operands[5], operands[7]));
      if (CASE_VECTOR_PC_RELATIVE || flag_pic)
	emit_insn (gen_addsi3 (operands[6], operands[6], operands[3]));
      emit_jump_insn (gen_casesi_jump (operands[6], operands[7]));
    }
  DONE;
}")

(define_insn "casesi_load"
  [(set (match_operand:SI 0 "register_operand"             "=Rcq,r,r")
	(unspec:SI [(match_operand:SI 1 "nonmemory_operand" "Rcq,c,Cal")
		    (match_operand:SI 2 "register_operand"  "Rcq,c,c")
		    (label_ref (match_operand 3 "" ""))] UNSPEC_ARC_CASESI))]
  ""
  "*
{
  rtx diff_vec = PATTERN (next_nonnote_insn (as_a<rtx_insn *> (operands[3])));

  if (GET_CODE (diff_vec) != ADDR_DIFF_VEC)
    {
      gcc_assert (GET_CODE (diff_vec) == ADDR_VEC);
      gcc_assert (GET_MODE (diff_vec) == SImode);
      gcc_assert (!CASE_VECTOR_PC_RELATIVE && !flag_pic);
    }

  switch (GET_MODE (diff_vec))
    {
    case E_SImode:
      return \"ld.as %0,[%1,%2]%&\";
    case E_HImode:
      if (ADDR_DIFF_VEC_FLAGS (diff_vec).offset_unsigned)
	return \"ld%_.as %0,[%1,%2]\";
      return \"ld%_.x.as %0,[%1,%2]\";
    case E_QImode:
      if (ADDR_DIFF_VEC_FLAGS (diff_vec).offset_unsigned)
	return \"ldb%? %0,[%1,%2]%&\";
      return \"ldb.x %0,[%1,%2]\";
    default:
      gcc_unreachable ();
    }
}"
  [(set_attr "type" "load")
   (set_attr_alternative "iscompact"
     [(cond
	[(ne (symbol_ref "GET_MODE (PATTERN (next_nonnote_insn
					       (as_a<rtx_insn *> (operands[3]))))")
	     (symbol_ref "QImode"))
	 (const_string "false")
	 (match_test "!ADDR_DIFF_VEC_FLAGS (PATTERN (next_nonnote_insn
						       (as_a<rtx_insn *> (operands[3])))).offset_unsigned")
	 (const_string "false")]
	(const_string "true"))
      (const_string "false")
      (const_string "false")])
   (set_attr_alternative "length"
     [(cond
	[(eq_attr "iscompact" "false") (const_int 4)
	; We have to mention (match_dup 3) to convince genattrtab.c that this
	; is a varying length insn.
	 (eq (symbol_ref "1+1") (const_int 2)) (const_int 2)
	 (gt (minus (match_dup 3) (pc)) (const_int 42)) (const_int 4)]
	(const_int 2))
      (const_int 4)
      (const_int 8)])])

; Unlike the canonical tablejump, this pattern always uses a jump address,
; even for CASE_VECTOR_PC_RELATIVE.
(define_insn "casesi_jump"
  [(set (pc) (match_operand:SI 0 "register_operand" "Cal,Rcqq,c"))
   (use (label_ref (match_operand 1 "" "")))]
  ""
  "j%!%* [%0]%&"
  [(set_attr "type" "jump")
   (set_attr "iscompact" "false,maybe,false")
   (set_attr "cond" "canuse")])

(define_insn "casesi_compact_jump"
  [(set (pc)
	(unspec:SI [(match_operand:SI 0 "register_operand" "c,q")]
		   UNSPEC_ARC_CASESI))
   (use (label_ref (match_operand 1 "" "")))
   (clobber (match_scratch:SI 2 "=q,0"))]
  "TARGET_COMPACT_CASESI"
  "*
{
  rtx diff_vec = PATTERN (next_nonnote_insn (as_a<rtx_insn *> (operands[1])));
  int unalign = arc_get_unalign ();
  rtx xop[3];
  const char *s;

  xop[0] = operands[0];
  xop[2] = operands[2];
  gcc_assert (GET_CODE (diff_vec) == ADDR_DIFF_VEC);

  switch (GET_MODE (diff_vec))
    {
    case E_SImode:
      /* Max length can be 12 in this case, but this is OK because
	 2 of these are for alignment, and are anticipated in the length
	 of the ADDR_DIFF_VEC.  */
      if (unalign && !satisfies_constraint_Rcq (xop[0]))
	s = \"add2 %2,pcl,%0\n\tld_s %2,[%2,12]\";
      else if (unalign)
	s = \"add_s %2,%0,2\n\tld.as %2,[pcl,%2]\";
      else
	s = \"add %2,%0,2\n\tld.as %2,[pcl,%2]\";
      arc_clear_unalign ();
      break;
    case E_HImode:
      if (ADDR_DIFF_VEC_FLAGS (diff_vec).offset_unsigned)
	{
	  if (satisfies_constraint_Rcq (xop[0]))
	    {
	      s = \"add_s %2,%0,%1\n\tld%_.as %2,[pcl,%2]\";
	      xop[1] = GEN_INT ((10 - unalign) / 2U);
	    }
	  else
	    {
	      s = \"add1 %2,pcl,%0\n\tld%__s %2,[%2,%1]\";
	      xop[1] = GEN_INT (10 + unalign);
	    }
	}
      else
	{
	  if (satisfies_constraint_Rcq (xop[0]))
	    {
	      s = \"add_s %2,%0,%1\n\tld%_.x.as %2,[pcl,%2]\";
	      xop[1] = GEN_INT ((10 - unalign) / 2U);
	    }
	  else
	    {
	      s = \"add1 %2,pcl,%0\n\tld%__s.x %2,[%2,%1]\";
	      xop[1] = GEN_INT (10 + unalign);
	    }
	}
      arc_toggle_unalign ();
      break;
    case E_QImode:
      if (ADDR_DIFF_VEC_FLAGS (diff_vec).offset_unsigned)
	{
	  if ((rtx_equal_p (xop[2], xop[0])
	       || find_reg_note (insn, REG_DEAD, xop[0]))
	      && satisfies_constraint_Rcq (xop[0]))
	    {
	      s = \"add_s %0,%0,pcl\n\tldb_s %2,[%0,%1]\";
	      xop[1] = GEN_INT (8 + unalign);
	    }
	  else
	    {
	      s = \"add %2,%0,pcl\n\tldb_s %2,[%2,%1]\";
	      xop[1] = GEN_INT (10 + unalign);
	      arc_toggle_unalign ();
	    }
	}
      else if ((rtx_equal_p (xop[0], xop[2])
		|| find_reg_note (insn, REG_DEAD, xop[0]))
	       && satisfies_constraint_Rcq (xop[0]))
	{
	  s = \"add_s %0,%0,%1\n\tldb.x %2,[pcl,%0]\";
	  xop[1] = GEN_INT (10 - unalign);
	  arc_toggle_unalign ();
	}
      else
	{
	  /* ??? Length is 12.  */
	  s = \"add %2,%0,%1\n\tldb.x %2,[pcl,%2]\";
	  xop[1] = GEN_INT (8 + unalign);
	}
      break;
    default:
      gcc_unreachable ();
    }
  output_asm_insn (s, xop);
  return \"add_s %2,%2,pcl\n\tj_s%* [%2]\";
}"
  [(set_attr "length" "10")
   (set_attr "type" "jump")
   (set_attr "iscompact" "true")
   (set_attr "cond" "nocond")])

(define_expand "call"
  ;; operands[1] is stack_size_rtx
  ;; operands[2] is next_arg_register
  [(parallel [(call (match_operand:SI 0 "call_operand" "")
		    (match_operand 1 "" ""))
	     (clobber (reg:SI 31))])]
  ""
  "{
    rtx callee;

    gcc_assert (MEM_P (operands[0]));
    callee  = XEXP (operands[0], 0);
    /* This is to decide if we should generate indirect calls by loading the
       32 bit address of the callee into a register before performing the
       branch and link - this exposes cse opportunities.
       Also, in weird cases like compile/20010107-1.c, we may get a PLUS.  */
    if (GET_CODE (callee) != REG
	&& (GET_CODE (callee) == PLUS || arc_is_longcall_p (callee)))
      XEXP (operands[0], 0) = force_reg (Pmode, callee);
  }
")

; Rcq, which is used in alternative 0, checks for conditional execution.
; At instruction output time, if it doesn't match and we end up with
; alternative 1 ("q"), that means that we can't use the short form.
(define_insn "*call_i"
  [(call (mem:SI (match_operand:SI 0
		  "call_address_operand" "Rcq,q,c,Cji,Csc,Cbp,Cbr,L,I,Cal"))
	 (match_operand 1 "" ""))
   (clobber (reg:SI 31))]
  ""
  "@
   jl%!%* [%0]%&
   jl%!%* [%0]%&
   jl%!%* [%0]
   jli_s %S0
   sjli  %S0
   bl%!%* %P0
   bl%!%* %P0
   jl%!%* %0
   jl%* %0
   jl%! %0"
  [(set_attr "type" "call,call,call,call_no_delay_slot,call_no_delay_slot,call,call,call,call,call_no_delay_slot")
   (set_attr "iscompact" "maybe,false,*,true,*,*,*,*,*,*")
   (set_attr "predicable" "no,no,yes,no,no,yes,no,yes,no,yes")
   (set_attr "length" "*,*,4,2,4,4,4,4,4,8")])

(define_expand "call_value"
  ;; operand 2 is stack_size_rtx
  ;; operand 3 is next_arg_register
  [(parallel [(set (match_operand 0 "dest_reg_operand" "=r")
		   (call (match_operand:SI 1 "call_operand" "")
			 (match_operand 2 "" "")))
	     (clobber (reg:SI 31))])]
  ""
  "
  {
    rtx callee;

    gcc_assert (MEM_P (operands[1]));
    callee = XEXP (operands[1], 0);
     /* See the comment in define_expand \"call\".  */
    if (GET_CODE (callee) != REG
	&& (GET_CODE (callee) == PLUS || arc_is_longcall_p (callee)))
      XEXP (operands[1], 0) = force_reg (Pmode, callee);
  }")

; Rcq, which is used in alternative 0, checks for conditional execution.
; At instruction output time, if it doesn't match and we end up with
; alternative 1 ("q"), that means that we can't use the short form.
(define_insn "*call_value_i"
  [(set (match_operand 0 "dest_reg_operand"  "=Rcq,q,w,  w,  w,  w,  w,w,w,  w")
	(call (mem:SI (match_operand:SI 1
		       "call_address_operand" "Rcq,q,c,Cji,Csc,Cbp,Cbr,L,I,Cal"))
	      (match_operand 2 "" "")))
   (clobber (reg:SI 31))]
  ""
  "@
   jl%!%* [%1]%&
   jl%!%* [%1]%&
   jl%!%* [%1]
   jli_s %S1
   sjli  %S1
   bl%!%* %P1;1
   bl%!%* %P1;1
   jl%!%* %1
   jl%* %1
   jl%! %1"
  [(set_attr "type" "call,call,call,call_no_delay_slot,call_no_delay_slot,call,call,call,call,call_no_delay_slot")
   (set_attr "iscompact" "maybe,false,*,true,false,*,*,*,*,*")
   (set_attr "predicable" "no,no,yes,no,no,yes,no,yes,no,yes")
   (set_attr "length" "*,*,4,2,4,4,4,4,4,8")])

; There is a bl_s instruction (16 bit opcode branch-and-link), but we can't
; use it for lack of inter-procedural branch shortening.
; Link-time relaxation would help...

(define_insn "trap"
  [(trap_if (const_int 1) (const_int 0))]
  "!TARGET_ARC600_FAMILY"
  "trap_s\\t5"
  [(set_attr "type" "misc")
   (set_attr "length" "2")])

(define_insn "nop"
  [(const_int 0)]
  ""
  "nop%?"
  [(set_attr "type" "misc")
   (set_attr "iscompact" "true")
   (set_attr "cond" "canuse")
   (set_attr "length" "2")])

(define_insn "nopv"
  [(unspec_volatile [(const_int 0)] VUNSPEC_ARC_NOP)]
  ""
  "nop%?"
  [(set_attr "type" "misc")
   (set_attr "iscompact" "maybe")
   (set_attr "length" "*")])

(define_insn "blockage"
  [(unspec_volatile [(const_int 0)] VUNSPEC_ARC_BLOCKAGE)]
  ""
  ""
  [(set_attr "length" "0")
   (set_attr "type" "block")]
)

;; Split up troublesome insns for better scheduling.

;; Peepholes go at the end.
;;asl followed by add can be replaced by an add{1,2,3}
;; Three define_peepholes have been added for this optimization
;; ??? This used to target non-canonical rtl.  Now we use add_n, which
;; can be generated by combine.  Check if these peepholes still provide
;; any benefit.

;; -------------------------------------------------------------
;; Pattern 1 : r0 = r1 << {i}
;;             r3 = r4/INT + r0     ;;and commutative
;;                 ||
;;                 \/
;;             add{i} r3,r4/INT,r1
;; -------------------------------------------------------------
;; ??? This should be covered by combine, alas, at times combine gets
;; too clever for it's own good: when the shifted input is known to be
;; either 0 or 1, the operation will be made into an if-then-else, and
;; thus fail to match the add_n pattern.  Example: _mktm_r, line 85 in
;; newlib/libc/time/mktm_r.c .

(define_peephole2
  [(set (match_operand:SI 0 "dest_reg_operand" "")
	(ashift:SI (match_operand:SI 1 "register_operand" "")
		   (match_operand:SI 2 "_1_2_3_operand" "")))
  (set (match_operand:SI 3 "dest_reg_operand" "")
       (plus:SI (match_operand:SI 4 "nonmemory_operand" "")
		(match_operand:SI 5 "nonmemory_operand" "")))]
  "(true_regnum (operands[4]) == true_regnum (operands[0])
       || true_regnum (operands[5]) == true_regnum (operands[0]))
   && (peep2_reg_dead_p (2, operands[0])
       || (true_regnum (operands[3]) == true_regnum (operands[0])))
   && !(optimize_size && satisfies_constraint_I (operands[4]))
   && !(optimize_size && satisfies_constraint_I (operands[5]))"
 ;; the preparation statements take care to put proper operand in operands[4]
 ;; operands[4] will always contain the correct operand. This is added to satisfy commutativity
  [(set (match_dup 3)
	(plus:SI (mult:SI (match_dup 1)
			  (match_dup 2))
		 (match_dup 4)))]
  "if (true_regnum (operands[4]) == true_regnum (operands[0]))
      operands[4] = operands[5];
   operands[2] = GEN_INT (1 << INTVAL (operands[2]));"
)

;; -------------------------------------------------------------
;; Pattern 1 : r0 = r1 << {i}
;;             r3 = r4 - r0
;;                 ||
;;                 \/
;;             sub{i} r3,r4,r1
;; -------------------------------------------------------------
;; ??? This should be covered by combine, alas, at times combine gets
;; too clever for it's own good: when the shifted input is known to be
;; either 0 or 1, the operation will be made into an if-then-else, and
;; thus fail to match the sub_n pattern.  Example: __ieee754_yn, line 239 in
;; newlib/libm/math/e_jn.c .

(define_peephole2
  [(set (match_operand:SI 0 "dest_reg_operand" "")
	(ashift:SI (match_operand:SI 1 "register_operand" "")
		   (match_operand:SI 2 "const_int_operand" "")))
   (set (match_operand:SI 3 "dest_reg_operand" "")
	(minus:SI (match_operand:SI 4 "nonmemory_operand" "")
		  (match_dup 0)))]
  "(INTVAL (operands[2]) == 1
    || INTVAL (operands[2]) == 2
    || INTVAL (operands[2]) == 3)
   && (peep2_reg_dead_p (2, operands[0])
       || (true_regnum (operands[3]) == true_regnum (operands[0])))"
  [(set (match_dup 3)
	(minus:SI (match_dup 4)
		  (mult:SI (match_dup 1)
			   (match_dup 2))))]
  "operands[2] = GEN_INT (1 << INTVAL (operands[2]));"
)



; When using the high single bit, the result of a multiply is either
; the original number or zero.  But MPY costs 4 cycles, which we
; can replace with the 2 cycles for the pair of TST_S and ADD.NE.
(define_peephole2
  [(set (match_operand:SI 0 "dest_reg_operand" "")
 	(lshiftrt:SI (match_operand:SI 1 "register_operand" "")
		     (const_int 31)))
   (set (match_operand:SI 4 "register_operand" "")
  	(mult:SI (match_operand:SI 2 "register_operand")
		 (match_operand:SI 3 "nonmemory_operand" "")))]
  "TARGET_ARC700_MPY
   && (rtx_equal_p (operands[0], operands[2])
       || rtx_equal_p (operands[0], operands[3]))
   && peep2_regno_dead_p (0, CC_REG)
   && (rtx_equal_p (operands[0], operands[4])
       || (peep2_reg_dead_p (2, operands[0])
	  && peep2_reg_dead_p (1, operands[4])))"
  [(parallel [(set (reg:CC_Z CC_REG)
		   (compare:CC_Z (lshiftrt:SI (match_dup 1) (const_int 31))
				 (const_int 0)))
	      (set (match_dup 4) (lshiftrt:SI (match_dup 1) (const_int 31)))])
   (cond_exec
     (ne (reg:CC_Z CC_REG) (const_int 0))
     (set (match_dup 4) (match_dup 5)))]
{
  if (!rtx_equal_p (operands[0], operands[2]))
    operands[5] = operands[2];
  else if (!rtx_equal_p (operands[0], operands[3]))
    operands[5] = operands[3];
  else
    operands[5] = operands[4]; /* Actually a no-op... presumably rare.  */
})

(define_peephole2
  [(set (match_operand:SI 0 "dest_reg_operand" "")
 	(lshiftrt:SI (match_operand:SI 1 "register_operand" "")
		     (const_int 31)))
   (set (match_operand:SI 4 "register_operand" "")
  	(mult:SI (match_operand:SI 2 "register_operand")
		 (match_operand:SI 3 "nonmemory_operand" "")))]
  "TARGET_ARC700_MPY
   && (rtx_equal_p (operands[0], operands[2])
       || rtx_equal_p (operands[0], operands[3]))
   && peep2_regno_dead_p (2, CC_REG)"
  [(parallel [(set (reg:CC_Z CC_REG)
		   (compare:CC_Z (lshiftrt:SI (match_dup 1) (const_int 31))
				 (const_int 0)))
	      (set (match_dup 0) (lshiftrt:SI (match_dup 1) (const_int 31)))])
   (set (match_dup 4) (match_dup 5))
   (cond_exec
     (eq (reg:CC_Z CC_REG) (const_int 0))
     (set (match_dup 4) (const_int 0)))]
 "operands[5] = operands[rtx_equal_p (operands[0], operands[2]) ? 3 : 2];")

;; Instructions generated through builtins

(define_insn "clrsbsi2"
  [(set (match_operand:SI  0 "dest_reg_operand" "=w,w")
	(clrsb:SI (match_operand:SI 1 "general_operand" "cL,Cal")))]
  "TARGET_NORM"
  "@
   norm \t%0, %1
   norm \t%0, %1"
  [(set_attr "length" "4,8")
   (set_attr "type" "two_cycle_core,two_cycle_core")])

(define_insn "norm_f"
  [(set (match_operand:SI  0 "dest_reg_operand" "=w,w")
	(clrsb:SI (match_operand:SI 1 "general_operand" "cL,Cal")))
   (set (reg:CC_ZN CC_REG)
	(compare:CC_ZN (match_dup 1) (const_int 0)))]
  "TARGET_NORM"
  "@
   norm.f\t%0, %1
   norm.f\t%0, %1"
  [(set_attr "length" "4,8")
   (set_attr "type" "two_cycle_core,two_cycle_core")])

(define_insn_and_split "clrsbhi2"
  [(set (match_operand:HI  0 "dest_reg_operand" "=w,w")
	(clrsb:HI (match_operand:HI 1 "general_operand" "cL,Cal")))]
  "TARGET_NORM"
  "#"
  "reload_completed"
  [(set (match_dup 0) (zero_extend:SI (clrsb:HI (match_dup 1))))]
  "operands[0] = simplify_gen_subreg (SImode, operands[0], HImode, 0);")

(define_insn "normw"
  [(set (match_operand:SI  0 "dest_reg_operand" "=w,w")
	(zero_extend:SI
	  (clrsb:HI (match_operand:HI 1 "general_operand" "cL,Cal"))))]
  "TARGET_NORM"
  "@
   norm%_ \t%0, %1
   norm%_ \t%0, %1"
  [(set_attr "length" "4,8")
   (set_attr "type" "two_cycle_core,two_cycle_core")])

(define_expand "clzsi2"
  [(parallel
    [(set (match_operand:SI 0 "register_operand" "")
	  (clz:SI (match_operand:SI 1 "register_operand" "")))
     (clobber (match_dup 2))])]
  "TARGET_NORM"
  "operands[2] = gen_rtx_REG (CC_ZNmode, CC_REG);")

(define_insn_and_split "*arc_clzsi2"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(clz:SI (match_operand:SI 1 "register_operand" "r")))
   (clobber (reg:CC_ZN CC_REG))]
  "TARGET_NORM"
  "#"
  "reload_completed"
  [(const_int 0)]
{
  emit_insn (gen_norm_f (operands[0], operands[1]));
  emit_insn
    (gen_rtx_COND_EXEC
      (VOIDmode,
       gen_rtx_LT (VOIDmode, gen_rtx_REG (CC_ZNmode, CC_REG), const0_rtx),
       gen_rtx_SET (operands[0], const0_rtx)));
  emit_insn
    (gen_rtx_COND_EXEC
      (VOIDmode,
       gen_rtx_GE (VOIDmode, gen_rtx_REG (CC_ZNmode, CC_REG), const0_rtx),
       gen_rtx_SET (operands[0], plus_constant (SImode, operands[0], 1))));
  DONE;
}
[(set_attr "type" "unary")
 (set_attr "length" "12")])

(define_expand "ctzsi2"
  [(match_operand:SI 0 "register_operand" "")
   (match_operand:SI 1 "register_operand" "")]
  "TARGET_NORM"
  "
  emit_insn (gen_arc_ctzsi2 (operands[0], operands[1]));
  DONE;
")

(define_insn_and_split "arc_ctzsi2"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(ctz:SI (match_operand:SI 1 "register_operand" "r")))
   (clobber (reg:CC_ZN CC_REG))
   (clobber (match_scratch:SI 2 "=&r"))]
  "TARGET_NORM"
  "#"
  "reload_completed"
  [(const_int 0)]
{
  rtx temp = operands[0];

  if (reg_overlap_mentioned_p (temp, operands[1])
      || (REGNO (temp) < FIRST_PSEUDO_REGISTER
	  && !TEST_HARD_REG_BIT (reg_class_contents[GENERAL_REGS],
				 REGNO (temp))))
    temp = operands[2];
  emit_insn (gen_addsi3 (temp, operands[1], constm1_rtx));
  emit_insn (gen_bic_f_zn (temp, temp, operands[1]));
  emit_insn (gen_clrsbsi2 (operands[0], temp));
  emit_insn
    (gen_rtx_COND_EXEC
      (VOIDmode,
       gen_rtx_LT (VOIDmode, gen_rtx_REG (CC_ZNmode, CC_REG), const0_rtx),
       gen_rtx_SET (operands[0], GEN_INT (32))));
  emit_insn
    (gen_rtx_COND_EXEC
      (VOIDmode,
       gen_rtx_GE (VOIDmode, gen_rtx_REG (CC_ZNmode, CC_REG), const0_rtx),
       gen_rtx_SET (operands[0], gen_rtx_MINUS (SImode, GEN_INT (31),
						operands[0]))));
  DONE;
}
[(set_attr "type" "unary")
 (set_attr "length" "20")])

(define_insn "swap"
  [(set (match_operand:SI  0 "dest_reg_operand" "=w,w,w")
	(unspec:SI [(match_operand:SI 1 "general_operand" "L,Cal,c")]
			    UNSPEC_ARC_SWAP))]
  "TARGET_SWAP"
  "@
   swap \t%0, %1
   swap \t%0, %1
   swap \t%0, %1"
  [(set_attr "length" "4,8,4")
   (set_attr "type" "two_cycle_core,two_cycle_core,two_cycle_core")])

(define_insn "divaw"
  [(set (match_operand:SI 0 "dest_reg_operand" "=&w,&w,&w")
			  (unspec:SI [(div:SI (match_operand:SI 1 "general_operand" "r,Cal,r")
					   (match_operand:SI 2 "general_operand" "r,r,Cal"))]
					   UNSPEC_ARC_DIVAW))]
  "TARGET_ARC700 || TARGET_EA_SET"
  "@
   divaw \t%0, %1, %2
   divaw \t%0, %1, %2
   divaw \t%0, %1, %2"
  [(set_attr "length" "4,8,8")
   (set_attr "type" "divaw,divaw,divaw")])

(define_insn "flag"
  [(unspec_volatile [(match_operand:SI 0 "nonmemory_operand" "rL,I,Cal")]
		   VUNSPEC_ARC_FLAG)]
  ""
  "@
    flag%? %0
    flag %0
    flag%? %0"
  [(set_attr "length" "4,4,8")
   (set_attr "type" "misc,misc,misc")
   (set_attr "predicable" "yes,no,yes")
   (set_attr "cond" "clob,clob,clob")])

(define_insn "brk"
  [(unspec_volatile [(match_operand:SI 0 "immediate_operand" "N")]
		   VUNSPEC_ARC_BRK)]
  ""
  "brk"
  [(set_attr "length" "4")
  (set_attr "type" "misc")])

(define_insn "rtie"
  [(unspec_volatile [(match_operand:SI 0 "immediate_operand" "N")]
		   VUNSPEC_ARC_RTIE)]
  ""
  "rtie"
  [(set_attr "length" "4")
  (set_attr "type" "misc")
  (set_attr "cond" "clob")])

(define_insn "sync"
  [(unspec_volatile [(match_operand:SI 0 "immediate_operand" "N")]
		   VUNSPEC_ARC_SYNC)]
  ""
  "sync"
  [(set_attr "length" "4")
  (set_attr "type" "misc")])

(define_insn "swi"
  [(unspec_volatile [(match_operand:SI 0 "immediate_operand" "N")]
		   VUNSPEC_ARC_SWI)]
  ""
  "*
{
    if(TARGET_ARC700)
	return \"trap0\";
    else
	return \"swi\";
}"
  [(set_attr "length" "4")
  (set_attr "type" "misc")])


(define_insn "sleep"
  [(unspec_volatile [(match_operand:SI 0 "nonmemory_operand" "Lr")]
		   VUNSPEC_ARC_SLEEP)]
  ""
  "sleep %0"
  [(set_attr "length" "4")
  (set_attr "type" "misc")])

(define_insn "core_read"
  [(set (match_operand:SI  0 "dest_reg_operand" "=r,r")
	(unspec_volatile:SI [(match_operand:SI 1 "general_operand" "Hn,!r")]
			    VUNSPEC_ARC_CORE_READ))]
  ""
  "*
    if (check_if_valid_regno_const (operands, 1))
      return \"mov \t%0, r%1\";
    return \"mov \t%0, r%1\";
  "
  [(set_attr "length" "4")
   (set_attr "type" "unary")])

(define_insn "core_write"
  [(unspec_volatile [(match_operand:SI 0 "general_operand" "r,r")
		     (match_operand:SI 1 "general_operand" "Hn,!r")]
		   VUNSPEC_ARC_CORE_WRITE)]
  ""
  "*
    if (check_if_valid_regno_const (operands, 1))
      return \"mov \tr%1, %0\";
    return \"mov \tr%1, %0\";
  "
  [(set_attr "length" "4")
   (set_attr "type" "unary")])

(define_insn "lr"
  [(set (match_operand:SI  0 "dest_reg_operand" "=r,r,r,r")
	(unspec_volatile:SI [(match_operand:SI 1 "general_operand" "I,HCal,r,D")]
			    VUNSPEC_ARC_LR))]
  ""
  "lr\t%0, [%1]"
  [(set_attr "length" "4,8,4,8")
   (set_attr "type" "lr,lr,lr,lr")])

(define_insn "sr"
  [(unspec_volatile [(match_operand:SI 0 "general_operand" "Cal,r,r,r")
		     (match_operand:SI 1 "general_operand" "Ir,I,HCal,r")]
		   VUNSPEC_ARC_SR)]
  ""
  "sr\t%0, [%1]"
  [(set_attr "length" "8,4,8,4")
   (set_attr "type" "sr,sr,sr,sr")])

(define_insn "trap_s"
  [(unspec_volatile [(match_operand:SI 0 "immediate_operand" "L,Cal")]
		   VUNSPEC_ARC_TRAP_S)]
  "!TARGET_ARC600_FAMILY"
{
  if (which_alternative == 0)
    {
      arc_toggle_unalign ();
      return \"trap_s %0\";
    }

  /* Keep this message in sync with the one in arc.c:arc_expand_builtin,
     because *.md files do not get scanned by exgettext.  */
  fatal_error (input_location,
	       \"operand to trap_s should be an unsigned 6-bit value\");
}
  [(set_attr "length" "2")
  (set_attr "type" "misc")])

(define_insn "unimp_s"
  [(unspec_volatile [(match_operand:SI 0 "immediate_operand" "N")]
		   VUNSPEC_ARC_UNIMP_S)]
  "!TARGET_ARC600_FAMILY"
  "unimp_s"
  [(set_attr "length" "4")
  (set_attr "type" "misc")])

;; End of instructions generated through builtins

; Since the demise of REG_N_SETS as reliable data readily available to the
; target, it is no longer possible to find out
; in the prologue / epilogue expanders how many times blink is set.
; Using df_regs_ever_live_p to decide if blink needs saving means that
; any explicit use of blink will cause it to be saved; hence we cannot
; represent the blink use in return / sibcall instructions themselves, and
; instead have to show it in EPILOGUE_USES and must explicitly
; forbid instructions that change blink in the return / sibcall delay slot.
(define_expand "sibcall"
  [(parallel [(call (match_operand 0 "memory_operand" "")
		    (match_operand 1 "general_operand" ""))
	      (simple_return)
	      (use (match_operand 2 "" ""))])]
  ""
  "
  {
    rtx callee = XEXP (operands[0], 0);

    if (operands[2] == NULL_RTX)
      operands[2] = const0_rtx;
    if (GET_CODE (callee) != REG
	&& (GET_CODE (callee) == PLUS || arc_is_longcall_p (callee)))
      XEXP (operands[0], 0) = force_reg (Pmode, callee);
  }"
)

(define_expand "sibcall_value"
  [(parallel [(set (match_operand 0 "dest_reg_operand" "")
		   (call (match_operand 1 "memory_operand" "")
			 (match_operand 2 "general_operand" "")))
	      (simple_return)
	      (use (match_operand 3 "" ""))])]
  ""
  "
  {
    rtx callee = XEXP (operands[1], 0);

    if (operands[3] == NULL_RTX)
      operands[3] = const0_rtx;
    if (GET_CODE (callee) != REG && arc_is_longcall_p (callee))
      XEXP (operands[1], 0) = force_reg (Pmode, callee);
  }"
)

(define_insn "*sibcall_insn"
 [(call (mem:SI (match_operand:SI 0 "call_address_operand"
		 "Cbp,Cbr,Rs5,Rsc,Cal"))
	(match_operand 1 "" ""))
  (simple_return)
  (use (match_operand 2 "" ""))]
  ""
  "@
   b%!%* %P0
   b%!%* %P0
   j%!%* [%0]%&
   j%!%* [%0]
   j%! %P0"
  [(set_attr "type" "call,call,call,call,call_no_delay_slot")
   (set_attr "predicable" "yes,no,no,yes,yes")
   (set_attr "iscompact" "false,false,maybe,false,false")
   (set_attr "is_SIBCALL" "yes")]
)

(define_insn "*sibcall_value_insn"
 [(set (match_operand 0 "dest_reg_operand" "")
       (call (mem:SI (match_operand:SI 1 "call_address_operand"
	      "Cbp,Cbr,Rs5,Rsc,Cal"))
	     (match_operand 2 "" "")))
  (simple_return)
  (use (match_operand 3 "" ""))]
  ""
  "@
   b%!%* %P1
   b%!%* %P1
   j%!%* [%1]%&
   j%!%* [%1]
   j%! %P1"
  [(set_attr "type" "call,call,call,call,call_no_delay_slot")
   (set_attr "predicable" "yes,no,no,yes,yes")
   (set_attr "iscompact" "false,false,maybe,false,false")
   (set_attr "is_SIBCALL" "yes")]
)

(define_expand "prologue"
  [(pc)]
  ""
{
  arc_expand_prologue ();
  DONE;
})

(define_expand "epilogue"
  [(pc)]
  ""
{
  arc_expand_epilogue (0);
  DONE;
})

(define_expand "sibcall_epilogue"
  [(pc)]
  ""
{
  arc_expand_epilogue (1);
  DONE;
})

; Since the demise of REG_N_SETS, it is no longer possible to find out
; in the prologue / epilogue expanders how many times blink is set.
; Using df_regs_ever_live_p to decide if blink needs saving means that
; any explicit use of blink will cause it to be saved; hence we cannot
; represent the blink use in return / sibcall instructions themselves, and
; instead have to show it in EPILOGUE_USES and must explicitly
; forbid instructions that change blink in the return / sibcall delay slot.
(define_insn "simple_return"
  [(simple_return)]
  "reload_completed"
{
  rtx reg
    = gen_rtx_REG (Pmode,
		   arc_return_address_register (arc_compute_function_type
						(cfun)));

  if (TARGET_V2
      && ARC_INTERRUPT_P (arc_compute_function_type (cfun)))
  {
    return \"rtie\";
  }
  output_asm_insn (\"j%!%* [%0]%&\", &reg);
  return \"\";
}
  [(set (attr "type")
	(cond [(and (match_test "ARC_INTERRUPT_P (arc_compute_function_type (cfun))")
		    (match_test "TARGET_V2"))
	       (const_string "brcc_no_delay_slot")]
	      (const_string "return")))
   ; predicable won't help here since the canonical rtl looks different
   ; for branches.
   (set (attr "cond")
	(cond [(and (eq (symbol_ref "arc_compute_function_type (cfun)")
			(symbol_ref "ARC_FUNCTION_ILINK1"))
		    (match_test "TARGET_V2"))
	       (const_string "nocond")]
	      (const_string "canuse")))
  (set (attr "iscompact")
	(cond [(eq (symbol_ref "arc_compute_function_type (cfun)")
		   (symbol_ref "ARC_FUNCTION_NORMAL"))
	       (const_string "maybe")]
	      (const_string "false")))
   (set (attr "length")
	(cond [(ne (symbol_ref "arc_compute_function_type (cfun)")
		   (symbol_ref "ARC_FUNCTION_NORMAL"))
	       (const_int 4)]
	      (const_int 2)))])

(define_insn "p_return_i"
  [(set (pc)
	(if_then_else (match_operator 0 "proper_comparison_operator"
				      [(reg CC_REG) (const_int 0)])
		      (simple_return) (pc)))]
  "reload_completed
   && !(TARGET_V2
     && ARC_INTERRUPT_P (arc_compute_function_type (cfun)))"
{
  rtx xop[2];
  xop[0] = operands[0];
  xop[1]
    = gen_rtx_REG (Pmode,
		   arc_return_address_register (arc_compute_function_type
						(cfun)));

  output_asm_insn (\"j%d0%!%# [%1]%&\", xop);
  /* record the condition in case there is a delay insn.  */
  arc_ccfsm_record_condition (xop[0], false, insn, 0);
  return \"\";
}
  [(set_attr "type" "return")
   (set_attr "cond" "use")
   (set (attr "iscompact")
	(cond [(eq (symbol_ref "arc_compute_function_type (cfun)")
		   (symbol_ref "ARC_FUNCTION_NORMAL"))
	       (const_string "maybe")]
	      (const_string "false")))
   (set (attr "length")
	(cond [(ne (symbol_ref "arc_compute_function_type (cfun)")
		   (symbol_ref "ARC_FUNCTION_NORMAL"))
	       (const_int 4)
	       (not (match_operand 0 "equality_comparison_operator" ""))
	       (const_int 4)
	       (eq_attr "delay_slot_filled" "yes")
	       (const_int 4)]
	      (const_int 2)))])

;; ??? #ifdefs in function.c require the presence of this pattern, with a
;; non-constant predicate.
(define_expand "return"
  [(return)]
  "optimize < 0")

 ;; Comment in final.c (insn_current_reference_address) says
 ;; forward branch addresses are calculated from the next insn after branch
 ;; and for backward branches, it is calculated from the branch insn start.
 ;; The shortening logic here is tuned to accomodate this behavior
;; ??? This should be grokked by the ccfsm machinery.
(define_insn "cbranchsi4_scratch"
  [(set (pc)
	(if_then_else (match_operator 0 "proper_comparison_operator"
			[(match_operand:SI 1 "register_operand" "c,c, c")
			 (match_operand:SI 2 "nonmemory_operand" "L,c,?Cal")])
		      (label_ref (match_operand 3 "" ""))
		      (pc)))
   (clobber (match_operand 4 "cc_register" ""))]
   "(reload_completed
     || (TARGET_EARLY_CBRANCHSI
	 && brcc_nolimm_operator (operands[0], VOIDmode)))
    && !CROSSING_JUMP_P (insn)"
   "*
     switch (get_attr_length (insn))
     {
       case 2: return \"br%d0%? %1, %2, %^%l3%&\";
       case 4: return \"br%d0%* %1, %B2, %^%l3\";
       case 8: if (!brcc_nolimm_operator (operands[0], VOIDmode))
		 return \"br%d0%* %1, %B2, %^%l3\";
       /* FALLTHRU */
       case 6: case 10:
       case 12:return \"cmp%? %1, %B2\\n\\tb%d0%* %^%l3%& ;br%d0 out of range\";
       default: fprintf (stderr, \"unexpected length %d\\n\", get_attr_length (insn)); fflush (stderr); gcc_unreachable ();
     }
   "
  [(set_attr "cond" "clob, clob, clob")
   (set (attr "type")
	(if_then_else
	  (match_test "valid_brcc_with_delay_p (operands)")
	  (const_string "brcc")
	  (const_string "brcc_no_delay_slot")))
   ; For forward branches, we need to account not only for the distance to
   ; the target, but also the difference between pcl and pc, the instruction
   ; length, and any delay insn, if present.
   (set
     (attr "length")
     (cond ; the outer cond does a test independent of branch shortening.
       [(match_operand 0 "brcc_nolimm_operator" "")
	(cond
	  [(and (match_operand:CC_Z 4 "cc_register")
		(eq_attr "delay_slot_filled" "no")
		(ge (minus (match_dup 3) (pc)) (const_int -128))
		(le (minus (match_dup 3) (pc))
		    (minus (const_int 122)
			   (symbol_ref "get_attr_delay_slot_length (insn)"))))
	   (const_int 2)
	   (and (ge (minus (match_dup 3) (pc)) (const_int -256))
		(le (minus (match_dup 3) (pc))
		    (minus (const_int 244)
			   (symbol_ref "get_attr_delay_slot_length (insn)"))))
	   (const_int 4)
	   (and (match_operand:SI 1 "compact_register_operand" "")
		(match_operand:SI 2 "compact_hreg_operand" ""))
	   (const_int 6)]
	  (const_int 8))]
	 (cond [(and (ge (minus (match_dup 3) (pc)) (const_int -256))
		     (le (minus (match_dup 3) (pc)) (const_int 244)))
		(const_int 8)
		(and (match_operand:SI 1 "compact_register_operand" "")
		     (match_operand:SI 2 "compact_hreg_operand" ""))
		(const_int 10)]
	       (const_int 12))))
   (set (attr "iscompact")
	(if_then_else (match_test "get_attr_length (insn) & 2")
		      (const_string "true") (const_string "false")))])

; combiner pattern observed for unwind-dw2-fde.c:linear_search_fdes.
(define_insn "*bbit"
  [(set (pc)
	(if_then_else
	  (match_operator 3 "equality_comparison_operator"
	    [(zero_extract:SI (match_operand:SI 1 "register_operand" "Rcqq,c")
			      (const_int 1)
			      (match_operand:SI 2 "nonmemory_operand" "L,Lc"))
	     (const_int 0)])
	  (label_ref (match_operand 0 "" ""))
	  (pc)))
   (clobber (reg:CC_ZN CC_REG))]
  "!CROSSING_JUMP_P (insn)"
{
  switch (get_attr_length (insn))
    {
      case 4: return (GET_CODE (operands[3]) == EQ
		      ? \"bbit0%* %1,%2,%0\" : \"bbit1%* %1,%2,%0\");
      case 6:
      case 8: return \"btst%? %1,%2\n\tb%d3%* %0; bbit out of range\";
      default: gcc_unreachable ();
    }
}
  [(set_attr "type" "brcc")
   (set_attr "cond" "clob")
   (set (attr "length")
	(cond [(and (ge (minus (match_dup 0) (pc)) (const_int -254))
		    (le (minus (match_dup 0) (pc))
		    (minus (const_int 248)
			   (symbol_ref "get_attr_delay_slot_length (insn)"))))
	       (const_int 4)
	       (eq (symbol_ref "which_alternative") (const_int 0))
	       (const_int 6)]
	      (const_int 8)))
   (set (attr "iscompact")
	(if_then_else (match_test "get_attr_length (insn) == 6")
		      (const_string "true") (const_string "false")))])

; ??? When testing a bit from a DImode register, combine creates a
; zero_extract in DImode.  This goes via an AND with a DImode constant,
; so can only be observed on 64 bit hosts.
(define_insn_and_split "*bbit_di"
  [(set (pc)
	(if_then_else
	  (match_operator 3 "equality_comparison_operator"
	    [(zero_extract:DI (match_operand:SI 1 "register_operand" "Rcqq,c")
			      (const_int 1)
			      (match_operand 2 "immediate_operand" "L,L"))
	     (const_int 0)])
	  (label_ref (match_operand 0 "" ""))
	  (pc)))
   (clobber (reg:CC_ZN CC_REG))]
  "!CROSSING_JUMP_P (insn)"
  "#"
  ""
  [(parallel
     [(set (pc) (if_then_else (match_dup 3) (label_ref (match_dup 0)) (pc)))
      (clobber (reg:CC_ZN CC_REG))])]
{
  rtx xtr;

  xtr = gen_rtx_ZERO_EXTRACT (SImode, operands[1], const1_rtx, operands[2]);
  operands[3] = gen_rtx_fmt_ee (GET_CODE (operands[3]), GET_MODE (operands[3]),
				xtr, const0_rtx);
})

;; -------------------------------------------------------------------
;; Hardware loop
;; -------------------------------------------------------------------

; operand 0 is the loop count pseudo register
; operand 1 is the label to jump to at the top of the loop
(define_expand "doloop_end"
  [(parallel [(set (pc)
		   (if_then_else
		    (ne (match_operand 0 "" "")
			(const_int 1))
		    (label_ref (match_operand 1 "" ""))
		    (pc)))
	      (set (match_dup 0) (plus (match_dup 0) (const_int -1)))
	      (unspec [(const_int 0)] UNSPEC_ARC_LP)
	      (clobber (match_dup 2))])]
  ""
{
 if (GET_MODE (operands[0]) != SImode)
   FAIL;
 operands[2] = gen_rtx_SCRATCH (SImode);
})

(define_insn "arc_lp"
  [(unspec:SI [(match_operand:SI 0 "register_operand" "l")]
	      UNSPEC_ARC_LP)
   (use (label_ref (match_operand 1 "" "")))
   (use (label_ref (match_operand 2 "" "")))]
  ""
  "lp\\t@%l2\\t; %0:@%l1->@%l2"
  [(set_attr "type" "loop_setup")
   (set_attr "length" "4")])

;; if by any chance the lp_count is not used, then use an 'r'
;; register, instead of going to memory.
(define_insn "loop_end"
  [(set (pc)
	(if_then_else (ne (match_operand:SI 2 "nonimmediate_operand" "0,0")
			  (const_int 1))
		      (label_ref (match_operand 1 "" ""))
		      (pc)))
   (set (match_operand:SI 0 "nonimmediate_operand" "=l!r,m")
	(plus (match_dup 2) (const_int -1)))
   (unspec [(const_int 0)] UNSPEC_ARC_LP)
   (clobber (match_scratch:SI 3 "=X,&r"))]
  ""
  "\\t;%0 %1 %2"
  [(set_attr "length" "0")
   (set_attr "predicable" "no")
   (set_attr "type" "loop_end")])

;; split pattern for the very slim chance when the loop register is
;; memory.
(define_split
  [(set (pc)
	(if_then_else (ne (match_operand:SI 0 "memory_operand")
			  (const_int 1))
		      (label_ref (match_operand 1 ""))
		      (pc)))
   (set (match_dup 0) (plus (match_dup 0) (const_int -1)))
   (unspec [(const_int 0)] UNSPEC_ARC_LP)
   (clobber (match_scratch:SI 2))]
  "memory_operand (operands[0], SImode)"
  [(set (match_dup 2) (match_dup 0))
   (set (match_dup 2) (plus:SI (match_dup 2) (const_int -1)))
   (set (match_dup 0) (match_dup 2))
   (set (reg:CC CC_REG) (compare:CC (match_dup 2) (const_int 0)))
   (set (pc)
	(if_then_else (ne (reg:CC CC_REG)
			  (const_int 0))
		      (label_ref (match_dup 1))
		      (pc)))]
  "")

(define_insn "loop_fail"
  [(set (reg:SI LP_COUNT)
	(plus:SI (reg:SI LP_COUNT) (const_int -1)))
   (set (reg:CC_ZN CC_REG)
	(compare:CC_ZN (plus:SI (reg:SI LP_COUNT) (const_int -1))
		       (const_int 0)))]
  ""
  "sub.f%?\\tlp_count,lp_count,1"
  [(set_attr "iscompact" "false")
   (set_attr "type" "compare")
   (set_attr "cond" "set_zn")
   (set_attr "length" "4")
   (set_attr "predicable" "yes")])

(define_insn_and_split "dbnz"
  [(set (pc)
	(if_then_else
	 (ne (plus:SI (match_operand:SI 0 "nonimmediate_operand" "+r!l,m")
		      (const_int -1))
	     (const_int 0))
	 (label_ref (match_operand 1 "" ""))
	 (pc)))
   (set (match_dup 0)
	(plus:SI (match_dup 0)
		 (const_int -1)))
   (clobber (match_scratch:SI 2 "=X,r"))]
  "TARGET_DBNZ"
  "@
   dbnz%#\\t%0,%l1
   #"
  "TARGET_DBNZ && reload_completed && memory_operand (operands[0], SImode)"
  [(set (match_dup 2) (match_dup 0))
   (set (match_dup 2) (plus:SI (match_dup 2) (const_int -1)))
   (set (reg:CC CC_REG) (compare:CC (match_dup 2) (const_int 0)))
   (set (match_dup 0) (match_dup 2))
   (set (pc) (if_then_else (ge (reg:CC CC_REG)
			       (const_int 0))
			   (label_ref (match_dup 1))
			   (pc)))]
  ""
  [(set_attr "iscompact" "false")
   (set_attr "type" "loop_end")
   (set_attr "length" "4,20")])

(define_expand "movmemsi"
  [(match_operand:BLK 0 "" "")
   (match_operand:BLK 1 "" "")
   (match_operand:SI 2 "nonmemory_operand" "")
   (match_operand 3 "immediate_operand" "")]
  ""
  "if (arc_expand_movmem (operands)) DONE; else FAIL;")

;; Close http://gcc.gnu.org/bugzilla/show_bug.cgi?id=35803 if this works
;; to the point that we can generate cmove instructions.
(define_expand "cbranch<mode>4"
  [(set (reg:CC CC_REG)
	(compare:CC (match_operand:SDF 1 "register_operand" "")
		    (match_operand:SDF 2 "register_operand" "")))
   (set (pc)
	(if_then_else
	 (match_operator 0 "comparison_operator" [(reg CC_REG)
						      (const_int 0)])
	 (label_ref (match_operand 3 "" ""))
	 (pc)))]

  "TARGET_FP_SP_BASE || TARGET_OPTFPE"
{
  gcc_assert (XEXP (operands[0], 0) == operands[1]);
  gcc_assert (XEXP (operands[0], 1) == operands[2]);
  operands[0] = gen_compare_reg (operands[0], VOIDmode);
  emit_jump_insn (gen_branch_insn (operands[3], operands[0]));
  DONE;
})

(define_expand "cmp_float"
  [(parallel [(set (match_operand 0 "") (match_operand 1 ""))
	      (clobber (reg:SI RETURN_ADDR_REGNUM))
	      (clobber (reg:SI R12_REG))])]
  ""
  "")

(define_mode_iterator OPTFPE_CMP [CC_Z CC_FP_GT CC_FP_GE CC_FP_UNEQ CC_FP_ORD])
(define_mode_attr cmp [(CC_Z "eq") (CC_FP_GT "gt") (CC_FP_GE "ge")
		       (CC_FP_UNEQ "uneq") (CC_FP_ORD "ord")])

(define_insn "*cmpsf_<cmp>"
  [(set (reg:OPTFPE_CMP CC_REG) (compare:OPTFPE_CMP (reg:SF 0) (reg:SF 1)))
   (clobber (reg:SI RETURN_ADDR_REGNUM))
   (clobber (reg:SI R12_REG))]
  "TARGET_OPTFPE && (!TARGET_ARGONAUT_SET || !TARGET_SPFP)
   && SFUNC_CHECK_PREDICABLE"
  "*return arc_output_libcall (\"__<cmp>sf2\");"
  [(set_attr "is_sfunc" "yes")
   (set_attr "predicable" "yes")])

;; N.B. for "*cmpdf_ord":
;; double precision fpx sets bit 31 for NaNs.  We need bit 51 set
;; for the floating point emulation to recognize the NaN.
(define_insn "*cmpdf_<cmp>"
  [(set (reg:OPTFPE_CMP CC_REG) (compare:OPTFPE_CMP (reg:DF 0) (reg:DF 2)))
   (clobber (reg:SI RETURN_ADDR_REGNUM))
   (clobber (reg:SI R12_REG))]
  "TARGET_OPTFPE && (!TARGET_ARGONAUT_SET || !TARGET_DPFP)
   && SFUNC_CHECK_PREDICABLE"
  "*return arc_output_libcall (\"__<cmp>df2\");"
  [(set_attr "is_sfunc" "yes")
   (set_attr "predicable" "yes")])

(define_insn "abssf2"
  [(set (match_operand:SF 0 "dest_reg_operand"    "=Rcq#q,Rcw,w")
	(abs:SF (match_operand:SF 1 "register_operand" "0,  0,c")))]
  ""
  "bclr%? %0,%1,31%&"
  [(set_attr "type" "unary")
   (set_attr "iscompact" "maybe,false,false")
   (set_attr "length" "2,4,4")
   (set_attr "predicable" "no,yes,no")])

(define_insn "negsf2"
  [(set (match_operand:SF 0 "dest_reg_operand" "=Rcw,w")
	(neg:SF (match_operand:SF 1 "register_operand" "0,c")))]
  ""
  "bxor%? %0,%1,31"
  [(set_attr "type" "unary")
   (set_attr "predicable" "yes,no")])

;; ??? Should this use arc_output_libcall and set is_sfunc?
(define_insn "*millicode_thunk_st"
  [(match_parallel 0 "millicode_store_operation"
		   [(set (mem:SI (reg:SI SP_REG)) (reg:SI 13))])]
  ""
{
  output_asm_insn ("bl%* __st_r13_to_%0",
		   &SET_SRC (XVECEXP (operands[0], 0,
				      XVECLEN (operands[0], 0) - 2)));
  return "";
}
  [(set_attr "type" "call")])

(define_insn "*millicode_thunk_ld"
  [(match_parallel 0 "millicode_load_clob_operation"
		   [(set (reg:SI 13) (mem:SI (reg:SI SP_REG)))])]
  ""
{
  output_asm_insn ("bl%* __ld_r13_to_%0",
		   &SET_DEST (XVECEXP (operands[0], 0,
				       XVECLEN (operands[0], 0) - 2)));
  return "";
}
  [(set_attr "type" "call")])

; the sibthunk restores blink, so we use the return rtx.
(define_insn "*millicode_sibthunk_ld"
  [(match_parallel 0 "millicode_load_operation"
		   [(return)
		    (set (reg:SI SP_REG) (plus:SI (reg:SI SP_REG) (reg:SI 12)))
		    (set (reg:SI 13) (mem:SI (reg:SI SP_REG)))])]
  ""
{
  output_asm_insn ("b%* __ld_r13_to_%0_ret",
		   &SET_DEST (XVECEXP (operands[0], 0,
				       XVECLEN (operands[0], 0) - 1)));
  return "";
}
  [(set_attr "type" "call")
   (set_attr "is_SIBCALL" "yes")])

(define_insn "tls_load_tp_soft"
  [(set (reg:SI R0_REG) (unspec:SI [(const_int 0)] UNSPEC_TLS_OFF))
   (clobber (reg:SI RETURN_ADDR_REGNUM))]
  ""
  "*return arc_output_libcall (\"__read_tp\");"
  [(set_attr "is_sfunc" "yes")
   (set_attr "predicable" "yes")])

(define_insn "tls_gd_get_addr"
  [(set (reg:SI R0_REG)
	(call:SI (mem:SI (unspec:SI [(match_operand:SI 0
				      "symbolic_operand" "X,X")]
			  UNSPEC_TLS_GD))
		 (const_int 0)))
   (clobber (reg:SI RETURN_ADDR_REGNUM))]
  ""
  ".tls_gd_ld %0`bl%* __tls_get_addr@plt"
  [(set_attr "type" "call")
   ; With TARGET_MEDIUM_CALLS, plt calls are not predicable.
   (set_attr "predicable" "no")])

;; For thread pointer builtins
(define_expand "get_thread_pointersi"
  [(set (match_operand:SI 0 "register_operand") (match_dup 1))]
 ""
 "operands[1] = gen_rtx_REG (Pmode, arc_tp_regno);")

(define_expand "set_thread_pointersi"
  [(set (match_dup 1) (match_operand:SI 0 "register_operand"))]
 ""
 "operands[1] = gen_rtx_REG (Pmode, arc_tp_regno);")

;; If hardware floating point is available, don't define a negdf pattern;
;; it would be something like:
;;(define_insn "negdf2"
;;  [(set (match_operand:DF 0 "register_operand" "=w,w,D,?r")
;;	(neg:DF (match_operand:DF 1 "register_operand" "0,c,D,D")))
;;   (clobber (match_scratch:DF 2 "=X,X,X,X,D1"))]
;;  ""
;;  "@
;;   bxor%? %H0,%H1,31
;;   bxor %H0,%H1,31 ` mov %L0,%L1
;;   drsubh%F0%F1 0,0,0
;;   drsubh%F2%F1 %H0,0,0 ` dexcl%F2 %L0,%H0,%L0"
;;  [(set_attr "type" "unary,unary,dpfp_addsub,dpfp_addsub")
;;   (set_attr "iscompact" "false,false,false,false")
;;   (set_attr "length" "4,4,8,12")
;;   (set_attr "cond" "canuse,nocond,nocond,nocond")])
;; and this suffers from always requiring a long immediate when using
;; the floating point hardware.
;; We then want the sub[sd]f patterns to be used, so that we can load the
;; constant zero efficiently into a register when we want to do the
;; computation using the floating point hardware.  There should be a special
;; subdf alternative that matches a zero operand 1, which then can allow
;; to use bxor to flip the high bit of an integer register.
;; ??? we actually can't use the floating point hardware for neg, because
;; this would not work right for -0.  OTOH optabs.c has already code
;; to synthesyze negate by flipping the sign bit.

;;V2 instructions
(define_insn "bswapsi2"
  [(set (match_operand:SI 0 "register_operand"           "= r,r")
	(bswap:SI (match_operand:SI 1 "nonmemory_operand" "rL,Cal")))]
  "TARGET_V2 && TARGET_SWAP"
  "swape %0, %1"
  [(set_attr "length" "4,8")
   (set_attr "type" "two_cycle_core")])

(define_expand "prefetch"
  [(prefetch (match_operand:SI 0 "address_operand" "")
	     (match_operand:SI 1 "const_int_operand" "")
	     (match_operand:SI 2 "const_int_operand" ""))]
  "TARGET_HS"
  "")

(define_insn "prefetch_1"
  [(prefetch (match_operand:SI 0 "register_operand" "r")
	     (match_operand:SI 1 "const_int_operand" "n")
	     (match_operand:SI 2 "const_int_operand" "n"))]
  "TARGET_HS"
  {
   if (INTVAL (operands[1]))
      return "prefetchw [%0]";
   else
      return "prefetch [%0]";
  }
  [(set_attr "type" "load")
   (set_attr "length" "4")])

(define_insn "prefetch_2"
  [(prefetch (plus:SI (match_operand:SI 0 "register_operand" "r,r,r")
		      (match_operand:SI 1 "nonmemory_operand" "r,Cm2,Cal"))
	     (match_operand:SI 2 "const_int_operand" "n,n,n")
	     (match_operand:SI 3 "const_int_operand" "n,n,n"))]
  "TARGET_HS"
  {
   if (INTVAL (operands[2]))
      return "prefetchw [%0, %1]";
   else
      return "prefetch [%0, %1]";
  }
  [(set_attr "type" "load")
   (set_attr "length" "4,4,8")])

(define_insn "prefetch_3"
  [(prefetch (match_operand:SI 0 "address_operand" "p")
	     (match_operand:SI 1 "const_int_operand" "n")
	     (match_operand:SI 2 "const_int_operand" "n"))]
  "TARGET_HS"
  {
   operands[0] = gen_rtx_MEM (SImode, operands[0]);
   if (INTVAL (operands[1]))
      return "prefetchw%U0 %0";
   else
      return "prefetch%U0 %0";
   }
  [(set_attr "type" "load")
   (set_attr "length" "8")])

(define_insn "divsi3"
  [(set (match_operand:SI 0 "register_operand"         "=r,r,  r,r,r,r,  r,  r")
	(div:SI (match_operand:SI 1 "nonmemory_operand" "0,r,Cal,0,r,0,  0,  r")
		(match_operand:SI 2 "nonmemory_operand" "r,r,  r,L,L,I,Cal,Cal")))]
  "TARGET_DIVREM"
  "div%? %0, %1, %2"
  [(set_attr "length" "4,4,8,4,4,4,8,8")
   (set_attr "iscompact" "false")
   (set_attr "type" "div_rem")
   (set_attr "predicable" "yes,no,no,yes,no,no,yes,no")
   (set_attr "cond" "canuse,nocond,nocond,canuse,nocond,nocond,canuse,nocond")
   ])

(define_insn "udivsi3"
  [(set (match_operand:SI 0 "register_operand"          "=r,r,  r,r,r,r,  r,  r")
	(udiv:SI (match_operand:SI 1 "nonmemory_operand" "0,r,Cal,0,r,0,  0,  r")
		 (match_operand:SI 2 "nonmemory_operand" "r,r,  r,L,L,I,Cal,Cal")))]
  "TARGET_DIVREM"
  "divu%? %0, %1, %2"
  [(set_attr "length" "4,4,8,4,4,4,8,8")
   (set_attr "iscompact" "false")
   (set_attr "type" "div_rem")
   (set_attr "predicable" "yes,no,no,yes,no,no,yes,no")
   (set_attr "cond" "canuse,nocond,nocond,canuse,nocond,nocond,canuse,nocond")
   ])

(define_insn "modsi3"
  [(set (match_operand:SI 0 "register_operand"         "=r,r,  r,r,r,r,  r,  r")
	(mod:SI (match_operand:SI 1 "nonmemory_operand" "0,r,Cal,0,r,0,  0,  r")
		(match_operand:SI 2 "nonmemory_operand" "r,r,  r,L,L,I,Cal,Cal")))]
  "TARGET_DIVREM"
  "rem%? %0, %1, %2"
  [(set_attr "length" "4,4,8,4,4,4,8,8")
   (set_attr "iscompact" "false")
   (set_attr "type" "div_rem")
   (set_attr "predicable" "yes,no,no,yes,no,no,yes,no")
   (set_attr "cond" "canuse,nocond,nocond,canuse,nocond,nocond,canuse,nocond")
   ])

(define_insn "umodsi3"
  [(set (match_operand:SI 0 "register_operand"          "=r,r,  r,r,r,r,  r,  r")
	(umod:SI (match_operand:SI 1 "nonmemory_operand" "0,r,Cal,0,r,0,  0,  r")
		 (match_operand:SI 2 "nonmemory_operand" "r,r,  r,L,L,I,Cal,Cal")))]
  "TARGET_DIVREM"
  "remu%? %0, %1, %2"
  [(set_attr "length" "4,4,8,4,4,4,8,8")
   (set_attr "iscompact" "false")
   (set_attr "type" "div_rem")
   (set_attr "predicable" "yes,no,no,yes,no,no,yes,no")
   (set_attr "cond" "canuse,nocond,nocond,canuse,nocond,nocond,canuse,nocond")
   ])

;; SETcc instructions
(define_code_iterator arcCC_cond [eq ne gt lt ge le])

(define_insn "arcset<code>"
  [(set (match_operand:SI 0 "register_operand"                "=r,r,r,r,r,r,r")
	(arcCC_cond:SI (match_operand:SI 1 "register_operand"  "0,r,0,r,0,0,r")
		       (match_operand:SI 2 "nonmemory_operand" "r,r,L,L,I,n,n")))]
  "TARGET_V2 && TARGET_CODE_DENSITY"
  "set<code>%? %0, %1, %2"
  [(set_attr "length" "4,4,4,4,4,8,8")
   (set_attr "iscompact" "false")
   (set_attr "type" "compare")
   (set_attr "predicable" "yes,no,yes,no,no,yes,no")
   (set_attr "cond" "canuse,nocond,canuse,nocond,nocond,canuse,nocond")
   ])

(define_insn "arcsetltu"
  [(set (match_operand:SI 0 "register_operand"         "=r,r,r,r,r,  r,  r")
	(ltu:SI (match_operand:SI 1 "register_operand"  "0,r,0,r,0,  0,  r")
		(match_operand:SI 2 "nonmemory_operand" "r,r,L,L,I,  n,  n")))]
  "TARGET_V2 && TARGET_CODE_DENSITY"
  "setlo%? %0, %1, %2"
  [(set_attr "length" "4,4,4,4,4,8,8")
   (set_attr "iscompact" "false")
   (set_attr "type" "compare")
   (set_attr "predicable" "yes,no,yes,no,no,yes,no")
   (set_attr "cond" "canuse,nocond,canuse,nocond,nocond,canuse,nocond")
   ])

(define_insn "arcsetgeu"
  [(set (match_operand:SI 0 "register_operand"         "=r,r,r,r,r,  r,  r")
	(geu:SI (match_operand:SI 1 "register_operand"  "0,r,0,r,0,  0,  r")
		(match_operand:SI 2 "nonmemory_operand" "r,r,L,L,I,  n,  n")))]
  "TARGET_V2 && TARGET_CODE_DENSITY"
  "seths%? %0, %1, %2"
  [(set_attr "length" "4,4,4,4,4,8,8")
   (set_attr "iscompact" "false")
   (set_attr "type" "compare")
   (set_attr "predicable" "yes,no,yes,no,no,yes,no")
   (set_attr "cond" "canuse,nocond,canuse,nocond,nocond,canuse,nocond")
   ])

;; Special cases of SETCC
(define_insn_and_split "arcsethi"
  [(set (match_operand:SI 0 "register_operand"         "=r,r,  r,r")
	(gtu:SI (match_operand:SI 1 "register_operand"  "r,r,  r,r")
		(match_operand:SI 2 "nonmemory_operand" "0,r,C62,n")))]
  "TARGET_V2 && TARGET_CODE_DENSITY"
  "setlo%? %0, %2, %1"
  "reload_completed
   && CONST_INT_P (operands[2])
   && satisfies_constraint_C62 (operands[2])"
  [(const_int 0)]
  "{
    /* sethi a,b,u6 => seths a,b,u6 + 1.  */
    operands[2] = GEN_INT (INTVAL (operands[2]) + 1);
    emit_insn (gen_arcsetgeu (operands[0], operands[1], operands[2]));
    DONE;
 }"
 [(set_attr "length" "4,4,4,8")
   (set_attr "iscompact" "false")
   (set_attr "type" "compare")
   (set_attr "predicable" "yes,no,no,no")
   (set_attr "cond" "canuse,nocond,nocond,nocond")]
)

(define_insn_and_split "arcsetls"
  [(set (match_operand:SI 0 "register_operand"         "=r,r,  r,r")
	(leu:SI (match_operand:SI 1 "register_operand"  "r,r,  r,r")
		(match_operand:SI 2 "nonmemory_operand" "0,r,C62,n")))]
  "TARGET_V2 && TARGET_CODE_DENSITY"
  "seths%? %0, %2, %1"
  "reload_completed
   && CONST_INT_P (operands[2])
   && satisfies_constraint_C62 (operands[2])"
  [(const_int 0)]
  "{
    /* setls a,b,u6 => setlo a,b,u6 + 1.  */
    operands[2] = GEN_INT (INTVAL (operands[2]) + 1);
    emit_insn (gen_arcsetltu (operands[0], operands[1], operands[2]));
    DONE;
 }"
 [(set_attr "length" "4,4,4,8")
   (set_attr "iscompact" "false")
   (set_attr "type" "compare")
   (set_attr "predicable" "yes,no,no,no")
   (set_attr "cond" "canuse,nocond,nocond,nocond")]
)

; Any mode that needs to be solved by secondary reload
(define_mode_iterator SRI [QI HI])

(define_expand "reload_<mode>_load"
  [(parallel [(match_operand:SRI 0 "register_operand" "=r")
	      (match_operand:SRI 1 "memory_operand" "m")
	      (match_operand:SI 2 "register_operand" "=&r")])]
  ""
{
 arc_secondary_reload_conv (operands[0], operands[1], operands[2], false);
 DONE;
})

(define_expand "reload_<mode>_store"
  [(parallel [(match_operand:SRI 0 "memory_operand" "=m")
	      (match_operand:SRI 1 "register_operand" "r")
	      (match_operand:SI 2 "register_operand" "=&r")])]
  ""
{
 arc_secondary_reload_conv (operands[1], operands[0], operands[2], true);
 DONE;
})

(define_insn "extzvsi"
  [(set (match_operand:SI 0 "register_operand"                  "=r  ,  r,r,r")
	(zero_extract:SI (match_operand:SI 1 "register_operand"  "0  ,  r,r,0")
			 (match_operand:SI 2 "const_int_operand" "C3p,C3p,n,n")
			 (match_operand:SI 3 "const_int_operand" "n  ,  n,n,n")))]
  "TARGET_HS && TARGET_BARREL_SHIFTER"
  {
   int assemble_op2 = (((INTVAL (operands[2]) - 1) & 0x1f) << 5) | (INTVAL (operands[3]) & 0x1f);
   operands[2] = GEN_INT (assemble_op2);
   return "xbfu%?\\t%0,%1,%2";
  }
  [(set_attr "type"       "shift")
   (set_attr "iscompact"  "false")
   (set_attr "length"     "4,4,8,8")
   (set_attr "predicable" "yes,no,no,yes")
   (set_attr "cond"       "canuse,nocond,nocond,canuse_limm")])

(define_insn "kflag"
  [(unspec_volatile [(match_operand:SI 0 "nonmemory_operand" "rL,I,Cal")]
		   VUNSPEC_ARC_KFLAG)]
  "TARGET_V2"
  "@
    kflag%? %0
    kflag %0
    kflag%? %0"
  [(set_attr "length" "4,4,8")
   (set_attr "type" "misc,misc,misc")
   (set_attr "predicable" "yes,no,yes")
   (set_attr "cond" "clob,clob,clob")])

(define_insn "clri"
  [(set (match_operand:SI  0 "dest_reg_operand" "=r")
	(unspec_volatile:SI [(match_operand:SI 1 "immediate_operand" "N")]
			    VUNSPEC_ARC_CLRI))]
  "TARGET_V2"
  "clri  %0"
  [(set_attr "length" "4")
   (set_attr "type" "misc")])

(define_insn "ffs"
  [(set (match_operand:SI  0 "dest_reg_operand" "=w,w")
	(unspec:SI [(match_operand:SI 1 "general_operand" "cL,Cal")]
			    UNSPEC_ARC_FFS))]
  "TARGET_NORM && TARGET_V2"
  "@
   ffs \t%0, %1
   ffs \t%0, %1"
  [(set_attr "length" "4,8")
   (set_attr "type" "two_cycle_core,two_cycle_core")])

(define_insn "ffs_f"
  [(set (match_operand:SI  0 "dest_reg_operand" "=w,w")
	(unspec:SI [(match_operand:SI 1 "general_operand" "cL,Cal")]
			    UNSPEC_ARC_FFS))
   (set (reg:CC_ZN CC_REG)
	(compare:CC_ZN (match_dup 1) (const_int 0)))]
  "TARGET_NORM && TARGET_V2"
  "@
   ffs.f\t%0, %1
   ffs.f\t%0, %1"
  [(set_attr "length" "4,8")
   (set_attr "type" "two_cycle_core,two_cycle_core")])

(define_expand "ffssi2"
  [(parallel [(set (match_dup 2)
		   (unspec:SI [(match_operand:SI 1 "register_operand" "")]
			      UNSPEC_ARC_FFS))
	      (set (reg:CC_ZN CC_REG)
		   (compare:CC_ZN (match_dup 1) (const_int 0)))])
   (set (match_dup 2) (plus:SI (match_dup 2) (const_int 1)))
   (set (match_operand:SI 0 "dest_reg_operand" "")
	(if_then_else:SI (eq:SI (reg:CC_ZN CC_REG) (const_int 0))
			 (const_int 0)
			 (match_dup 2)))]
  "TARGET_NORM && TARGET_V2"
  {
   operands[2] = gen_reg_rtx (SImode);
   })

(define_insn "fls"
  [(set (match_operand:SI  0 "register_operand" "=r,r")
	(unspec:SI [(match_operand:SI 1 "nonmemory_operand" "rL,Cal")]
			    UNSPEC_ARC_FLS))]
  "TARGET_NORM && TARGET_V2"
  "fls\\t%0,%1"
  [(set_attr "length" "4,8")
   (set_attr "type" "two_cycle_core,two_cycle_core")])

(define_insn "seti"
  [(unspec_volatile:SI [(match_operand:SI 0 "nonmemory_operand" "rL")]
		       VUNSPEC_ARC_SETI)]
  "TARGET_V2"
  "seti\\t%0"
  [(set_attr "length" "4")
   (set_attr "type" "misc")])

;; FPU/FPX expands

;;add
(define_expand "addsf3"
  [(set (match_operand:SF 0 "register_operand"           "")
	(plus:SF (match_operand:SF 1 "nonmemory_operand" "")
		 (match_operand:SF 2 "nonmemory_operand" "")))]
  "TARGET_FP_SP_BASE || TARGET_SPFP"
  "
  if (!register_operand (operands[1], SFmode)
      && !register_operand (operands[2], SFmode))
    operands[1] = force_reg (SFmode, operands[1]);
  ")

;;sub
(define_expand "subsf3"
  [(set (match_operand:SF 0 "register_operand"            "")
	(minus:SF (match_operand:SF 1 "nonmemory_operand" "")
		  (match_operand:SF 2 "nonmemory_operand" "")))]
  "TARGET_FP_SP_BASE || TARGET_SPFP"
  "
  if (!register_operand (operands[1], SFmode)
      && !register_operand (operands[2], SFmode))
    operands[1] = force_reg (SFmode, operands[1]);
  ")

;;mul
(define_expand "mulsf3"
  [(set (match_operand:SF 0 "register_operand"           "")
	(mult:SF (match_operand:SF 1 "nonmemory_operand" "")
		 (match_operand:SF 2 "nonmemory_operand" "")))]
  "TARGET_FP_SP_BASE || TARGET_SPFP"
  "
  if (!register_operand (operands[1], SFmode)
      && !register_operand (operands[2], SFmode))
    operands[1] = force_reg (SFmode, operands[1]);
  ")

;;add
(define_expand "adddf3"
  [(set (match_operand:DF 0 "double_register_operand"           "")
	(plus:DF (match_operand:DF 1 "double_register_operand"  "")
		 (match_operand:DF 2 "nonmemory_operand" "")))]
 "TARGET_FP_DP_BASE || TARGET_DPFP"
 "
  if (TARGET_DPFP)
   {
    if (GET_CODE (operands[2]) == CONST_DOUBLE)
     {
        rtx first, second, tmp;
        split_double (operands[2], &first, &second);
        tmp = force_reg (SImode, TARGET_BIG_ENDIAN ? first : second);
        emit_insn (gen_adddf3_insn (operands[0], operands[1],
                                    operands[2], tmp, const0_rtx));
     }
    else
     emit_insn (gen_adddf3_insn (operands[0], operands[1],
                                 operands[2], const1_rtx, const1_rtx));
   DONE;
  }
 else if (TARGET_FP_DP_BASE)
  {
   if (!even_register_operand (operands[2], DFmode))
      operands[2] = force_reg (DFmode, operands[2]);

   if (!even_register_operand (operands[1], DFmode))
      operands[1] = force_reg (DFmode, operands[1]);
  }
 else
  gcc_unreachable ();
 ")

;;sub
(define_expand "subdf3"
  [(set (match_operand:DF 0 "double_register_operand"            "")
	(minus:DF (match_operand:DF 1 "nonmemory_operand" "")
		  (match_operand:DF 2 "nonmemory_operand" "")))]
  "TARGET_FP_DP_BASE || TARGET_DPFP"
  "
   if (TARGET_DPFP)
    {
     if (TARGET_FP_DP_AX && (GET_CODE (operands[1]) == CONST_DOUBLE))
       operands[1] = force_reg (DFmode, operands[1]);
     if ((GET_CODE (operands[1]) == CONST_DOUBLE)
          || GET_CODE (operands[2]) == CONST_DOUBLE)
      {
        rtx first, second, tmp;
        int const_index = ((GET_CODE (operands[1]) == CONST_DOUBLE) ? 1 : 2);
        split_double (operands[const_index], &first, &second);
        tmp = force_reg (SImode, TARGET_BIG_ENDIAN ? first : second);
        emit_insn (gen_subdf3_insn (operands[0], operands[1],
                                    operands[2], tmp, const0_rtx));
      }
    else
     emit_insn (gen_subdf3_insn (operands[0], operands[1],
                                 operands[2], const1_rtx, const1_rtx));
    DONE;
   }
  else if (TARGET_FP_DP_BASE)
   {
    if (!even_register_operand (operands[2], DFmode))
       operands[2] = force_reg (DFmode, operands[2]);

    if (!even_register_operand (operands[1], DFmode))
       operands[1] = force_reg (DFmode, operands[1]);
   }
  else
   gcc_unreachable ();
  ")

;;mul
(define_expand "muldf3"
  [(set (match_operand:DF 0 "double_register_operand"           "")
	(mult:DF (match_operand:DF 1 "double_register_operand"  "")
		 (match_operand:DF 2 "nonmemory_operand" "")))]
  "TARGET_FP_DP_BASE || TARGET_DPFP"
  "
   if (TARGET_DPFP)
    {
     if (GET_CODE (operands[2]) == CONST_DOUBLE)
      {
        rtx first, second, tmp;
        split_double (operands[2], &first, &second);
        tmp = force_reg (SImode, TARGET_BIG_ENDIAN ? first : second);
        emit_insn (gen_muldf3_insn (operands[0], operands[1],
                                    operands[2], tmp, const0_rtx));
      }
     else
      emit_insn (gen_muldf3_insn (operands[0], operands[1],
                                  operands[2], const1_rtx, const1_rtx));
    DONE;
   }
  else if (TARGET_FP_DP_BASE)
   {
    if (!even_register_operand (operands[2], DFmode))
       operands[2] = force_reg (DFmode, operands[2]);

    if (!even_register_operand (operands[1], DFmode))
       operands[1] = force_reg (DFmode, operands[1]);
   }
  else
   gcc_unreachable ();
 ")

;;div
(define_expand "divsf3"
  [(set (match_operand:SF 0 "register_operand"        "")
	(div:SF (match_operand:SF 1 "nonmemory_operand" "")
		(match_operand:SF 2 "nonmemory_operand" "")))]
  "TARGET_FPX_QUARK || TARGET_FP_SP_SQRT"
  "
  if (TARGET_FPX_QUARK)
   {
     operands[1] = force_reg (SFmode, operands[1]);
     operands[2] = force_reg (SFmode, operands[2]);
   }
  else
   {
     if (!register_operand (operands[1], SFmode)
        && !register_operand (operands[2], SFmode))
       operands[1] = force_reg (SFmode, operands[1]);
   }
  ")

;; Square root
(define_expand "sqrtsf2"
  [(set (match_operand:SF 0 "register_operand"           "")
	(sqrt:SF (match_operand:SF 1 "nonmemory_operand" "")))]
  "TARGET_FPX_QUARK || TARGET_FP_SP_SQRT"
  "
  if (TARGET_FPX_QUARK)
   {
     operands[1] = force_reg (SFmode, operands[1]);
   }
")

;; SF->SI (using rounding towards zero)
(define_expand "fix_truncsfsi2"
  [(set (match_operand:SI 0 "register_operand"                "")
	(fix:SI (fix:SF (match_operand:SF 1 "register_operand" ""))))]
  "TARGET_FPX_QUARK || TARGET_FP_SP_CONV"
  "")

;; SI->SF
(define_expand "floatsisf2"
  [(set (match_operand:SF 0 "register_operand"            "")
	(float:SF (match_operand:SI 1 "register_operand" "")))]
  "TARGET_FPX_QUARK || TARGET_FP_SP_CONV"
  "")

(define_expand "extzv"
  [(set (match_operand:SI 0 "register_operand" "")
	(zero_extract:SI (match_operand:SI 1 "register_operand" "")
			 (match_operand:SI 2 "const_int_operand" "")
			 (match_operand:SI 3 "const_int_operand" "")))]
  "TARGET_NPS_BITOPS")

; We need a sanity check in the instuction predicate because combine
; will throw any old rubbish at us and see what sticks.
(define_insn "*extzv_i"
  [(set (match_operand:SI 0 "register_operand" "=Rrq")
	(zero_extract:SI (match_operand:SI 1 "register_operand" "Rrq")
			 (match_operand:SI 2 "const_int_operand" "n")
			 (match_operand:SI 3 "const_int_operand" "n")))]
  "TARGET_NPS_BITOPS && INTVAL (operands[2]) + INTVAL (operands[3]) <= 32"
  "movb.cl %0,%1,0,%3,%2"
  [(set_attr "type" "shift")
   (set_attr "length" "4")])

(define_expand "insv"
  [(set (zero_extract:SI (match_operand:SI 0 "register_operand" "")
			 (match_operand:SI 1 "const_int_operand" "")
			 (match_operand:SI 2 "const_int_operand" ""))
	(match_operand:SI 3 "nonmemory_operand" ""))]
  "TARGET_NPS_BITOPS"
{
  int size = INTVAL (operands[1]);

  if (size != 1 && size != 2 && size != 4 && size != 8)
    operands[3] = force_reg (SImode, operands[3]);
})

(define_insn "*insv_i"
  [(set (zero_extract:SI (match_operand:SI 0 "register_operand" "+w,Rrq")
			 (match_operand:SI 1 "const_int_operand" "C18,n")
			 (match_operand:SI 2 "const_int_operand" "n,n"))
	(match_operand:SI 3 "nonmemory_operand" "P,Rrq"))]
  "TARGET_NPS_BITOPS
   && (register_operand (operands[3], SImode)
       || satisfies_constraint_C18 (operands[1]))"
  "@
   movbi %0,%0,%3,%2,%1
   movb %0,%0,%3,%2,0,%1"
  [(set_attr "type" "shift")
   (set_attr "length" "4")])

(define_insn "*movb"
  [(set (zero_extract:SI (match_operand:SI 0 "register_operand" "+Rrq")
			 (match_operand:SI 1 "const_int_operand" "n")
			 (match_operand:SI 2 "const_int_operand" "n"))
	(zero_extract:SI (match_operand:SI 3 "register_operand" "Rrq")
			 (match_dup 1)
			 (match_operand:SI 4 "const_int_operand" "n")))]
  "TARGET_NPS_BITOPS"
  "movb %0,%0,%3,%2,%4,%1"
  [(set_attr "type" "shift")
   (set_attr "length" "4")])

(define_insn "*movb_signed"
  [(set (zero_extract:SI (match_operand:SI 0 "register_operand" "+Rrq")
			 (match_operand:SI 1 "const_int_operand" "n")
			 (match_operand:SI 2 "const_int_operand" "n"))
	(sign_extract:SI (match_operand:SI 3 "register_operand" "Rrq")
			 (match_dup 1)
			 (match_operand:SI 4 "const_int_operand" "n")))]
  "TARGET_NPS_BITOPS"
  "movb %0,%0,%3,%2,%4,%1"
  [(set_attr "type" "shift")
   (set_attr "length" "4")])

(define_insn "*movb_high"
  [(set (zero_extract:SI (match_operand:SI 0 "register_operand" "+Rrq")
			 (match_operand:SI 1 "const_int_operand" "n")
			 (match_operand:SI 2 "const_int_operand" "n"))
	(lshiftrt:SI (match_operand:SI 3 "register_operand" "Rrq")
		     (match_operand:SI 4 "const_int_operand" "n")))]
  "TARGET_NPS_BITOPS
   && INTVAL (operands[4]) + INTVAL (operands[1]) <= 32"
  "movb %0,%0,%3,%2,%4,%1"
  [(set_attr "type" "shift")
   (set_attr "length" "4")])

; N.B.: when processing signed bitfields that fit in the top half of
; a word, gcc will use a narrow sign extending load, and in this case
; we will see INTVAL (operands[4]) + INTVAL (operands[1]) == 16 (or 8)
(define_insn "*movb_high_signed"
  [(set (zero_extract:SI (match_operand:SI 0 "register_operand" "+Rrq")
			 (match_operand:SI 1 "const_int_operand" "n")
			 (match_operand:SI 2 "const_int_operand" "n"))
	(ashiftrt:SI (match_operand:SI 3 "register_operand" "Rrq")
		     (match_operand:SI 4 "const_int_operand" "n")))]
  "TARGET_NPS_BITOPS
   && INTVAL (operands[4]) + INTVAL (operands[1]) <= 32"
  "movb %0,%0,%3,%2,%4,%1"
  [(set_attr "type" "shift")
   (set_attr "length" "4")])

(define_split
  [(set (match_operand:SI 0 "register_operand" "")
	(ior:SI (ashift:SI (match_operand:SI 1 "register_operand" "")
			   (match_operand:SI 2 "const_int_operand" ""))
		(subreg:SI (match_operand 3 "") 0)))]
  "TARGET_NPS_BITOPS
   && GET_MODE_BITSIZE (GET_MODE (operands[3])) <= INTVAL (operands[2])
   && !reg_overlap_mentioned_p (operands[0], operands[1])"
  [(set (match_dup 0) (zero_extend:SI (match_dup 3)))
   (set (zero_extract:SI (match_dup 0) (match_dup 4) (match_dup 2))
	(match_dup 1))]
  "operands[4] = GEN_INT (32 - INTVAL (operands[2]));")

(define_insn "*mrgb"
  [(set (zero_extract:SI (match_operand:SI 0 "register_operand" "+Rrq")
			 (match_operand:SI 1 "const_int_operand" "n")
			 (match_operand:SI 2 "const_int_operand" "n"))
	(zero_extract:SI (match_dup 0) (match_dup 1)
			 (match_operand:SI 3 "const_int_operand" "n")))
   (set (zero_extract:SI (match_dup 0)
			 (match_operand:SI 4 "const_int_operand" "n")
			 (match_operand:SI 5 "const_int_operand" "n"))
	(zero_extract:SI (match_operand:SI 6 "register_operand" "Rrq")
			 (match_dup 4)
			 (match_operand:SI 7 "const_int_operand" "n")))]
  "TARGET_NPS_BITOPS"
{
  output_asm_insn ("mrgb %0,%0,%6,%2,%3,%1,%5,%7,%4", operands);
  /* The ;%? updates the known unalignment.  */
  return arc_short_long (insn, ";%?", "nop_s");
}
  [(set_attr "type" "shift")
   (set_attr "length" "6")
   (set_attr "iscompact" "true")])

;; combine fumbles combination of two movb patterns, and then the
;; combination is rejected by combinable_i3pat.
;; Thus, we can only use a peephole2 to combine two such insns.

(define_peephole2
  [(set (match_operand:SI 0 "register_operand" "")
	(match_operand:SI 1 "register_operand" ""))
   (set (zero_extract:SI (match_dup 0)
			 (match_operand:SI 2 "const_int_operand" "")
			 (match_operand:SI 3 "const_int_operand" ""))
	(zero_extract:SI (match_dup 1)
			 (match_dup 2)
			 (match_operand:SI 4 "const_int_operand" "")))
   (match_operand 9) ; unrelated insn scheduled here
   (set (zero_extract:SI (match_dup 0)
			 (match_operand:SI 5 "const_int_operand" "")
			 (match_operand:SI 6 "const_int_operand" ""))
	(zero_extract:SI (match_operand:SI 7 "register_operand" "")
			 (match_dup 5)
			 (match_operand:SI 8 "const_int_operand" "")))]
  "TARGET_NPS_BITOPS
   // Check that the second movb doesn't clobber an input of the extra insn.
   && !reg_overlap_mentioned_p (operands[0], operands[9])
   // And vice versa.
   && !reg_set_p (operands[0], operands[9])
   && !reg_set_p (operands[7], operands[9])"
  [(set (match_dup 0) (match_dup 1))
   (parallel [(set (zero_extract:SI (match_dup 0) (match_dup 1) (match_dup 2))
		   (zero_extract:SI (match_dup 3) (match_dup 1) (match_dup 4)))
	      (set (zero_extract:SI (match_dup 0) (match_dup 1) (match_dup 2))
		   (zero_extract:SI (match_dup 3) (match_dup 1) (match_dup 4)))])
   (match_dup 9)])

(define_peephole2
  [(set (match_operand:SI 0 "register_operand" "")
	(match_operand:SI 1 "register_operand" ""))
   (set (zero_extract:SI (match_dup 0)
			 (match_operand:SI 2 "const_int_operand" "")
			 (match_operand:SI 3 "const_int_operand" ""))
	(zero_extract:SI (match_dup 1)
			 (match_dup 2)
			 (match_operand:SI 4 "const_int_operand" "")))
   (set (match_dup 1) (match_operand 8))
   (set (zero_extract:SI (match_dup 0)
			 (match_operand:SI 5 "const_int_operand" "")
			 (match_operand:SI 6 "const_int_operand" ""))
	(zero_extract:SI (match_dup 1) (match_dup 5)
			 (match_operand:SI 7 "const_int_operand" "")))]
  "TARGET_NPS_BITOPS
   && !reg_overlap_mentioned_p (operands[0], operands[8])"
  [(set (match_dup 0) (match_dup 1))
   (set (match_dup 1) (match_dup 8))
   (parallel [(set (zero_extract:SI (match_dup 0) (match_dup 2) (match_dup 3))
		   (zero_extract:SI (match_dup 0) (match_dup 2) (match_dup 4)))
	      (set (zero_extract:SI (match_dup 0) (match_dup 5) (match_dup 6))
		   (zero_extract:SI (match_dup 1) (match_dup 5) (match_dup 7)))])
   (match_dup 1)])

(define_insn "*rotrsi3_cnt1"
  [(set (match_operand:SI 0 "dest_reg_operand"             "=w")
	(rotatert:SI (match_operand:SI 1 "register_operand" "c")
		     (const_int 1)))]
  ""
  "ror %0,%1%&"
  [(set_attr "type" "shift")
   (set_attr "predicable" "no")
   (set_attr "length" "4")])

(define_insn "*ashlsi2_cnt1"
  [(set (match_operand:SI 0 "dest_reg_operand"           "=Rcqq,w")
	(ashift:SI (match_operand:SI 1 "register_operand" "Rcqq,c")
		   (const_int 1)))]
  ""
  "asl%? %0,%1%&"
  [(set_attr "type" "shift")
   (set_attr "iscompact" "maybe,false")
   (set_attr "predicable" "no,no")])

(define_insn "*lshrsi3_cnt1"
  [(set (match_operand:SI 0 "dest_reg_operand"             "=Rcqq,w")
	(lshiftrt:SI (match_operand:SI 1 "register_operand" "Rcqq,c")
		     (const_int 1)))]
  ""
  "lsr%? %0,%1%&"
  [(set_attr "type" "shift")
   (set_attr "iscompact" "maybe,false")
   (set_attr "predicable" "no,no")])

(define_insn "*ashrsi3_cnt1"
  [(set (match_operand:SI 0 "dest_reg_operand"             "=Rcqq,w")
	(ashiftrt:SI (match_operand:SI 1 "register_operand" "Rcqq,c")
		     (const_int 1)))]
  ""
  "asr%? %0,%1%&"
  [(set_attr "type" "shift")
   (set_attr "iscompact" "maybe,false")
   (set_attr "predicable" "no,no")])

(define_peephole2
  [(set (match_operand:SI 0 "register_operand" "")
	(zero_extract:SI (match_dup 0)
			 (match_operand:SI 1 "const_int_operand" "")
			 (match_operand:SI 2 "const_int_operand" "")))
   (set (zero_extract:SI (match_operand:SI 3 "register_operand" "")
			 (match_dup 1)
			 (match_dup 2))
	(match_dup 0))]
  "TARGET_NPS_BITOPS
   && !reg_overlap_mentioned_p (operands[0], operands[3])"
  [(set (zero_extract:SI (match_dup 3) (match_dup 1) (match_dup 2))
	(zero_extract:SI (match_dup 0) (match_dup 1) (match_dup 2)))])

;; Dummy pattern used as a place holder for automatically saved
;; registers.
(define_insn "stack_irq_dwarf"
  [(unspec_volatile [(const_int 1)] VUNSPEC_ARC_STACK_IRQ)]
  ""
  ""
  [(set_attr "length" "0")])

;; MAC and DMPY instructions
(define_expand "maddsidi4"
  [(match_operand:DI 0 "register_operand" "")
   (match_operand:SI 1 "register_operand" "")
   (match_operand:SI 2 "extend_operand"   "")
   (match_operand:DI 3 "register_operand" "")]
  "TARGET_PLUS_DMPY"
  "{
   emit_insn (gen_maddsidi4_split (operands[0], operands[1], operands[2], operands[3]));
   DONE;
  }")

(define_insn_and_split "maddsidi4_split"
  [(set (match_operand:DI 0 "register_operand" "=r")
	(plus:DI
	 (mult:DI
	  (sign_extend:DI (match_operand:SI 1 "register_operand" "%r"))
	  (sign_extend:DI (match_operand:SI 2 "extend_operand" "ri")))
	 (match_operand:DI 3 "register_operand" "r")))
   (clobber (reg:DI ARCV2_ACC))]
  "TARGET_PLUS_DMPY"
  "#"
  "TARGET_PLUS_DMPY && reload_completed"
  [(const_int 0)]
  "{
   rtx acc_reg = gen_rtx_REG (DImode, ACC_REG_FIRST);
   emit_move_insn (acc_reg, operands[3]);
   if (TARGET_PLUS_MACD && even_register_operand (operands[0], DImode))
     emit_insn (gen_macd (operands[0], operands[1], operands[2]));
   else
     {
      emit_insn (gen_mac (operands[1], operands[2]));
      emit_move_insn (operands[0], acc_reg);
     }
   DONE;
   }"
  [(set_attr "type" "multi")
   (set_attr "length" "36")])

(define_insn "macd"
  [(set (match_operand:DI 0 "even_register_operand"	       "=Rcr,r,r")
	(plus:DI
	 (mult:DI
	  (sign_extend:DI (match_operand:SI 1 "register_operand" "%0,c,c"))
	  (sign_extend:DI (match_operand:SI 2 "extend_operand" " c,cI,Cal")))
	 (reg:DI ARCV2_ACC)))
   (set (reg:DI ARCV2_ACC)
	(plus:DI
	 (mult:DI (sign_extend:DI (match_dup 1))
		  (sign_extend:DI (match_dup 2)))
	 (reg:DI ARCV2_ACC)))]
 "TARGET_PLUS_MACD"
 "macd %0,%1,%2"
  [(set_attr "length" "4,4,8")
   (set_attr "type" "multi")
   (set_attr "predicable" "yes,no,no")
   (set_attr "cond" "canuse,nocond,nocond")])

(define_insn "mac"
  [(set (reg:DI ARCV2_ACC)
	(plus:DI
	 (mult:DI (sign_extend:DI (match_operand:SI 0 "register_operand" "%r,r"))
		  (sign_extend:DI (match_operand:SI 1 "extend_operand" "rI,i")))
	 (reg:DI ARCV2_ACC)))]
 "TARGET_PLUS_DMPY"
 "mac 0,%0,%1"
  [(set_attr "length" "4,8")
   (set_attr "type" "multi")
   (set_attr "predicable" "no")
   (set_attr "cond" "nocond")])

(define_peephole2
  [(set (reg:DI ARCV2_ACC)
	(plus:DI
	 (mult:DI (sign_extend:DI (match_operand:SI 0 "register_operand" ""))
		  (sign_extend:DI (match_operand:SI 1 "extend_operand" "")))
	 (reg:DI ARCV2_ACC)))
   (set (match_operand:SI 2 "register_operand" "")
	(match_operand:SI 3 "accl_operand" ""))]
 "TARGET_PLUS_DMPY"
 [(const_int 0)]
 {
  emit_insn (gen_mac_r (operands[2], operands[0], operands[1]));
  DONE;
 })

(define_insn "mac_r"
  [(set (match_operand:SI 0 "register_operand" "=r,r")
	(truncate:SI
	 (plus:DI
	  (mult:DI (sign_extend:DI (match_operand:SI 1 "register_operand" "%r,r"))
		   (sign_extend:DI (match_operand:SI 2 "extend_operand" "rI,i")))
	  (reg:DI ARCV2_ACC))))
   (clobber (reg:DI ARCV2_ACC))]
 "TARGET_PLUS_DMPY"
 "mac %0,%1,%2"
  [(set_attr "length" "4,8")
   (set_attr "type" "multi")
   (set_attr "predicable" "no")
   (set_attr "cond" "nocond")])

(define_expand "umaddsidi4"
  [(match_operand:DI 0 "register_operand" "")
   (match_operand:SI 1 "register_operand" "")
   (match_operand:SI 2 "extend_operand"   "")
   (match_operand:DI 3 "register_operand" "")]
  "TARGET_PLUS_DMPY"
  "{
   emit_insn (gen_umaddsidi4_split (operands[0], operands[1], operands[2], operands[3]));
   DONE;
  }")

(define_insn_and_split "umaddsidi4_split"
  [(set (match_operand:DI 0 "register_operand" "=r")
	(plus:DI
	 (mult:DI
	  (zero_extend:DI (match_operand:SI 1 "register_operand" "%r"))
	  (zero_extend:DI (match_operand:SI 2 "extend_operand" "ri")))
	 (match_operand:DI 3 "register_operand" "r")))
   (clobber (reg:DI ARCV2_ACC))]
  "TARGET_PLUS_DMPY"
  "#"
  "TARGET_PLUS_DMPY && reload_completed"
  [(const_int 0)]
  "{
   rtx acc_reg = gen_rtx_REG (DImode, ACC_REG_FIRST);
   emit_move_insn (acc_reg, operands[3]);
   if (TARGET_PLUS_MACD && even_register_operand (operands[0], DImode))
     emit_insn (gen_macdu (operands[0], operands[1], operands[2]));
   else
     {
      emit_insn (gen_macu (operands[1], operands[2]));
      emit_move_insn (operands[0], acc_reg);
     }
   DONE;
   }"
  [(set_attr "type" "multi")
   (set_attr "length" "36")])

(define_insn "macdu"
  [(set (match_operand:DI 0 "even_register_operand"	       "=Rcr,r,r")
	(plus:DI
	 (mult:DI
	  (zero_extend:DI (match_operand:SI 1 "register_operand" "%0,c,c"))
	  (zero_extend:DI (match_operand:SI 2 "extend_operand" " c,cI,i")))
	 (reg:DI ARCV2_ACC)))
   (set (reg:DI ARCV2_ACC)
	(plus:DI
	 (mult:DI (zero_extend:DI (match_dup 1))
		  (zero_extend:DI (match_dup 2)))
	 (reg:DI ARCV2_ACC)))]
 "TARGET_PLUS_MACD"
 "macdu %0,%1,%2"
  [(set_attr "length" "4,4,8")
   (set_attr "type" "multi")
   (set_attr "predicable" "yes,no,no")
   (set_attr "cond" "canuse,nocond,nocond")])

(define_insn "macu"
  [(set (reg:DI ARCV2_ACC)
	(plus:DI
	 (mult:DI (zero_extend:DI (match_operand:SI 0 "register_operand" "%r,r"))
		  (zero_extend:DI (match_operand:SI 1 "extend_operand" "rI,i")))
	 (reg:DI ARCV2_ACC)))]
 "TARGET_PLUS_DMPY"
 "macu 0,%0,%1"
  [(set_attr "length" "4,8")
   (set_attr "type" "multi")
   (set_attr "predicable" "no")
   (set_attr "cond" "nocond")])

(define_peephole2
  [(set (reg:DI ARCV2_ACC)
	(plus:DI
	 (mult:DI (zero_extend:DI (match_operand:SI 0 "register_operand" ""))
		  (zero_extend:DI (match_operand:SI 1 "extend_operand" "")))
	 (reg:DI ARCV2_ACC)))
   (set (match_operand:SI 2 "register_operand" "")
	(match_operand:SI 3 "accl_operand" ""))]
 "TARGET_PLUS_DMPY"
 [(const_int 0)]
 {
  emit_insn (gen_macu_r (operands[2], operands[0], operands[1]));
  DONE;
 })

(define_insn "macu_r"
  [(set (match_operand:SI 0 "register_operand" "=r,r")
	(truncate:SI
	 (plus:DI
	  (mult:DI (zero_extend:DI (match_operand:SI 1 "register_operand" "%r,r"))
		   (zero_extend:DI (match_operand:SI 2 "extend_operand" "rI,i")))
	  (reg:DI ARCV2_ACC))))
   (clobber (reg:DI ARCV2_ACC))]
 "TARGET_PLUS_DMPY"
 "macu %0,%1,%2"
  [(set_attr "length" "4,8")
   (set_attr "type" "multi")
   (set_attr "predicable" "no")
   (set_attr "cond" "nocond")])

(define_insn "mpyd_arcv2hs"
  [(set (match_operand:DI 0 "even_register_operand"			"=Rcr, r")
	(mult:DI (sign_extend:DI (match_operand:SI 1 "register_operand"	 "  0, c"))
		 (sign_extend:DI (match_operand:SI 2 "register_operand"	 "  c, c"))))
   (set (reg:DI ARCV2_ACC)
	(mult:DI
	  (sign_extend:DI (match_dup 1))
	  (sign_extend:DI (match_dup 2))))]
  "TARGET_PLUS_MACD"
  "mpyd%? %0,%1,%2"
  [(set_attr "length" "4,4")
  (set_attr "iscompact" "false")
  (set_attr "type" "multi")
  (set_attr "predicable" "yes,no")
  (set_attr "cond" "canuse,nocond")])

(define_insn "mpyd_imm_arcv2hs"
  [(set (match_operand:DI 0 "even_register_operand"			"=Rcr, r,r,Rcr,	 r")
	(mult:DI (sign_extend:DI (match_operand:SI 1 "register_operand"	 "  0, c,0,  0,	 c"))
		 (match_operand 2		    "immediate_operand"	 "  L, L,I,Cal,Cal")))
   (set (reg:DI ARCV2_ACC)
	(mult:DI (sign_extend:DI (match_dup 1))
		 (match_dup 2)))]
  "TARGET_PLUS_MACD"
  "mpyd%? %0,%1,%2"
  [(set_attr "length" "4,4,4,8,8")
  (set_attr "iscompact" "false")
  (set_attr "type" "multi")
  (set_attr "predicable" "yes,no,no,yes,no")
  (set_attr "cond" "canuse,nocond,nocond,canuse_limm,nocond")])

(define_insn "mpydu_arcv2hs"
  [(set (match_operand:DI 0 "even_register_operand"			"=Rcr, r")
	(mult:DI (zero_extend:DI (match_operand:SI 1 "register_operand"	 "  0, c"))
		 (zero_extend:DI (match_operand:SI 2 "register_operand" "   c, c"))))
   (set (reg:DI ARCV2_ACC)
	(mult:DI (zero_extend:DI (match_dup 1))
		 (zero_extend:DI (match_dup 2))))]
  "TARGET_PLUS_MACD"
  "mpydu%? %0,%1,%2"
  [(set_attr "length" "4,4")
  (set_attr "iscompact" "false")
  (set_attr "type" "multi")
  (set_attr "predicable" "yes,no")
  (set_attr "cond" "canuse,nocond")])

(define_insn "mpydu_imm_arcv2hs"
  [(set (match_operand:DI 0 "even_register_operand"			"=Rcr, r,r,Rcr,	 r")
	(mult:DI (zero_extend:DI (match_operand:SI 1 "register_operand"	 "  0, c,0,  0,	 c"))
		 (match_operand 2		    "immediate_operand"	 "  L, L,I,Cal,Cal")))
   (set (reg:DI ARCV2_ACC)
	(mult:DI (zero_extend:DI (match_dup 1))
		 (match_dup 2)))]
  "TARGET_PLUS_MACD"
  "mpydu%? %0,%1,%2"
  [(set_attr "length" "4,4,4,8,8")
  (set_attr "iscompact" "false")
  (set_attr "type" "multi")
  (set_attr "predicable" "yes,no,no,yes,no")
  (set_attr "cond" "canuse,nocond,nocond,canuse_limm,nocond")])

(define_insn "stack_tie"
  [(set (mem:BLK (scratch))
	(unspec:BLK [(match_operand:SI 0 "register_operand" "rb")
		     (match_operand:SI 1 "register_operand" "rb")]
		    UNSPEC_ARC_STKTIE))]
  ""
  ""
  [(set_attr "length" "0")
   (set_attr "iscompact" "false")
   (set_attr "type" "block")]
  )

(define_insn "*add_shift"
  [(set (match_operand:SI 0 "register_operand" "=q,r,r")
	(plus:SI (ashift:SI (match_operand:SI 1 "register_operand" "q,r,r")
			    (match_operand:SI 2 "_1_2_3_operand" ""))
		 (match_operand:SI 3 "nonmemory_operand"  "0,r,Csz")))]
  ""
  "add%2%?\\t%0,%3,%1"
  [(set_attr "length" "*,4,8")
   (set_attr "predicable" "yes,no,no")
   (set_attr "iscompact" "maybe,false,false")
   (set_attr "cond" "canuse,nocond,nocond")])

(define_insn "*add_shift2"
  [(set (match_operand:SI 0 "register_operand" "=q,r,r")
	(plus:SI (match_operand:SI 1 "nonmemory_operand"  "0,r,Cal")
		 (ashift:SI (match_operand:SI 2 "register_operand" "q,r,r")
			    (match_operand:SI 3 "_1_2_3_operand" ""))))]
  ""
  "add%3%?\\t%0,%1,%2"
  [(set_attr "length" "*,4,8")
   (set_attr "predicable" "yes,no,no")
   (set_attr "iscompact" "maybe,false,false")
   (set_attr "cond" "canuse,nocond,nocond")])

(define_insn "*sub_shift"
  [(set (match_operand:SI 0"register_operand" "=r,r,r")
	 (minus:SI (match_operand:SI 1 "nonmemory_operand" "0,r,Cal")
		   (ashift:SI (match_operand:SI 2 "register_operand" "r,r,r")
			      (match_operand:SI 3 "_1_2_3_operand" ""))))]
  ""
  "sub%3\\t%0,%1,%2"
  [(set_attr "length" "4,4,8")
   (set_attr "cond" "canuse,nocond,nocond")
   (set_attr "predicable" "yes,no,no")])

(define_insn "*sub_shift_cmp0_noout"
  [(set (match_operand 0 "cc_set_register" "")
	(compare:CC
	 (minus:SI (match_operand:SI 1 "register_operand" "r")
		   (ashift:SI (match_operand:SI 2 "register_operand" "r")
			      (match_operand:SI 3 "_1_2_3_operand" "")))
	 (const_int 0)))]
  ""
  "sub%3.f\\t0,%1,%2"
  [(set_attr "length" "4")])

(define_insn "*compare_si_ashiftsi"
  [(set (match_operand 0 "cc_set_register" "")
	(compare:CC (match_operand:SI 1 "register_operand" "r")
		    (ashift:SI (match_operand:SI 2 "register_operand" "r")
			       (match_operand:SI 3 "_1_2_3_operand" ""))))]
  ""
  "sub%3.f\\t0,%1,%2"
  [(set_attr "length" "4")])

;; Convert the sequence
;;  asl rd,rn,_1_2_3
;;  cmp ra,rd
;; into
;;  sub{123}.f 0,ra,rn
(define_peephole2
  [(set (match_operand:SI 0 "register_operand" "")
	(ashift:SI (match_operand:SI 1 "register_operand" "")
		   (match_operand:SI 2 "_1_2_3_operand" "")))
   (set (reg:CC CC_REG)
	(compare:CC (match_operand:SI 3 "register_operand" "")
		    (match_dup 0)))]
  "peep2_reg_dead_p (2, operands[0])"
  [(set (reg:CC CC_REG) (compare:CC (match_dup 3)
				    (ashift:SI (match_dup 1) (match_dup 2))))])

;; include the arc-FPX instructions
(include "fpx.md")

;; include the arc-FPU instructions
(include "fpu.md")

(include "simdext.md")

;; include atomic extensions
(include "atomic.md")
