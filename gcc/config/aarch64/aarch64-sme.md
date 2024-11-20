;; Machine description for AArch64 SME.
;; Copyright (C) 2023-2024 Free Software Foundation, Inc.
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

;; The file is organised into the following sections (search for the full
;; line):
;;
;; == State management
;; ---- Test current state
;; ---- PSTATE.SM management
;; ---- PSTATE.ZA management
;;
;; == Loads, stores and moves
;; ---- Single-vector loads
;; ---- Table loads
;; ---- Single-vector stores
;; ---- Table stores
;; ---- Single-vector moves
;; ---- Multi-vector moves
;; ---- Zeroing
;;
;; == Binary arithmetic
;; ---- Binary arithmetic on ZA tile
;; ---- Binary arithmetic on ZA slice
;; ---- Binary arithmetic, writing to ZA slice
;;
;; == Ternary arithmetic
;; ---- [INT] Dot product
;; ---- [INT] Ternary widening arithmetic on ZA slice
;; ---- [INT] Sum of outer products
;; ---- [FP] Dot product
;; ---- [FP] Ternary arithmetic on ZA slice
;; ---- [FP] Ternary widening arithmetic on ZA slice
;; ---- [FP] Sum of outer products
;;
;; == Table lookup
;; ---- Table lookup

;; =========================================================================
;; == State management
;; =========================================================================
;;
;; Many of the instructions in this section are only valid when SME is
;; present.  However, they don't have a TARGET_SME condition since
;; (a) they are only emitted under direct control of aarch64 code and
;; (b) they are sometimes used conditionally, particularly in streaming-
;; compatible code.
;;
;; =========================================================================

;; -------------------------------------------------------------------------
;; ---- Test current state
;; -------------------------------------------------------------------------

(define_c_enum "unspec" [
  UNSPEC_OLD_VG_SAVED
  UNSPEC_UPDATE_VG
  UNSPEC_GET_SME_STATE
  UNSPEC_READ_SVCR
])

;; A marker instruction to say that the old value of the DWARF VG register
;; has been saved to the stack, for CFI purposes.  Operand 0 is the old
;; value of the register and operand 1 is the save slot.
(define_insn "aarch64_old_vg_saved"
  [(set (reg:DI VG_REGNUM)
	(unspec:DI [(match_operand 0)
		    (match_operand 1)] UNSPEC_OLD_VG_SAVED))]
  ""
  ""
  [(set_attr "type" "no_insn")]
)

;; A marker to indicate places where a call temporarily changes VG.
(define_insn "aarch64_update_vg"
  [(set (reg:DI VG_REGNUM)
	(unspec:DI [(reg:DI VG_REGNUM)] UNSPEC_UPDATE_VG))]
  ""
  ""
  [(set_attr "type" "no_insn")]
)

(define_insn "aarch64_get_sme_state"
  [(set (reg:TI R0_REGNUM)
	(unspec_volatile:TI [(const_int 0)] UNSPEC_GET_SME_STATE))
   (clobber (reg:DI R16_REGNUM))
   (clobber (reg:DI R17_REGNUM))
   (clobber (reg:DI R18_REGNUM))
   (clobber (reg:DI R30_REGNUM))
   (clobber (reg:CC CC_REGNUM))]
  ""
  "bl\t__arm_sme_state"
  [(set_attr "is_call" "yes")]
)

(define_insn "aarch64_read_svcr"
  [(set (match_operand:DI 0 "register_operand" "=r")
	(unspec_volatile:DI [(const_int 0)] UNSPEC_READ_SVCR))]
  ""
  "mrs\t%0, svcr"
)

;; -------------------------------------------------------------------------
;; ---- PSTATE.SM management
;; -------------------------------------------------------------------------
;; Includes:
;; - SMSTART SM
;; - SMSTOP SM
;; -------------------------------------------------------------------------

(define_c_enum "unspec" [
  UNSPEC_SMSTART_SM
  UNSPEC_SMSTOP_SM
])

;; Turn on streaming mode.  This clobbers all SVE state.
;;
;; Depend on VG_REGNUM to ensure that the VG save slot has already been
;; initialized.
(define_insn "aarch64_smstart_sm"
  [(unspec_volatile [(const_int 0)] UNSPEC_SMSTART_SM)
   (use (reg:DI VG_REGNUM))
   (clobber (reg:V4x16QI V0_REGNUM))
   (clobber (reg:V4x16QI V4_REGNUM))
   (clobber (reg:V4x16QI V8_REGNUM))
   (clobber (reg:V4x16QI V12_REGNUM))
   (clobber (reg:V4x16QI V16_REGNUM))
   (clobber (reg:V4x16QI V20_REGNUM))
   (clobber (reg:V4x16QI V24_REGNUM))
   (clobber (reg:V4x16QI V28_REGNUM))
   (clobber (reg:VNx16BI P0_REGNUM))
   (clobber (reg:VNx16BI P1_REGNUM))
   (clobber (reg:VNx16BI P2_REGNUM))
   (clobber (reg:VNx16BI P3_REGNUM))
   (clobber (reg:VNx16BI P4_REGNUM))
   (clobber (reg:VNx16BI P5_REGNUM))
   (clobber (reg:VNx16BI P6_REGNUM))
   (clobber (reg:VNx16BI P7_REGNUM))
   (clobber (reg:VNx16BI P8_REGNUM))
   (clobber (reg:VNx16BI P9_REGNUM))
   (clobber (reg:VNx16BI P10_REGNUM))
   (clobber (reg:VNx16BI P11_REGNUM))
   (clobber (reg:VNx16BI P12_REGNUM))
   (clobber (reg:VNx16BI P13_REGNUM))
   (clobber (reg:VNx16BI P14_REGNUM))
   (clobber (reg:VNx16BI P15_REGNUM))]
  ""
  "smstart\tsm"
)

;; Turn off streaming mode.  This clobbers all SVE state.
;;
;; Depend on VG_REGNUM to ensure that the VG save slot has already been
;; initialized.
(define_insn "aarch64_smstop_sm"
  [(unspec_volatile [(const_int 0)] UNSPEC_SMSTOP_SM)
   (use (reg:DI VG_REGNUM))
   (clobber (reg:V4x16QI V0_REGNUM))
   (clobber (reg:V4x16QI V4_REGNUM))
   (clobber (reg:V4x16QI V8_REGNUM))
   (clobber (reg:V4x16QI V12_REGNUM))
   (clobber (reg:V4x16QI V16_REGNUM))
   (clobber (reg:V4x16QI V20_REGNUM))
   (clobber (reg:V4x16QI V24_REGNUM))
   (clobber (reg:V4x16QI V28_REGNUM))
   (clobber (reg:VNx16BI P0_REGNUM))
   (clobber (reg:VNx16BI P1_REGNUM))
   (clobber (reg:VNx16BI P2_REGNUM))
   (clobber (reg:VNx16BI P3_REGNUM))
   (clobber (reg:VNx16BI P4_REGNUM))
   (clobber (reg:VNx16BI P5_REGNUM))
   (clobber (reg:VNx16BI P6_REGNUM))
   (clobber (reg:VNx16BI P7_REGNUM))
   (clobber (reg:VNx16BI P8_REGNUM))
   (clobber (reg:VNx16BI P9_REGNUM))
   (clobber (reg:VNx16BI P10_REGNUM))
   (clobber (reg:VNx16BI P11_REGNUM))
   (clobber (reg:VNx16BI P12_REGNUM))
   (clobber (reg:VNx16BI P13_REGNUM))
   (clobber (reg:VNx16BI P14_REGNUM))
   (clobber (reg:VNx16BI P15_REGNUM))]
  ""
  "smstop\tsm"
)

;; -------------------------------------------------------------------------
;; ---- PSTATE.ZA management
;; -------------------------------------------------------------------------
;; Includes:
;; - SMSTART ZA
;; - SMSTOP ZA
;; plus calls to support routines.
;; -------------------------------------------------------------------------

(define_c_enum "unspec" [
  UNSPEC_SMSTOP_ZA
  UNSPEC_INITIAL_ZERO_ZA
  UNSPEC_TPIDR2_SAVE
  UNSPEC_TPIDR2_RESTORE
  UNSPEC_READ_TPIDR2
  UNSPEC_WRITE_TPIDR2
  UNSPEC_SETUP_LOCAL_TPIDR2
  UNSPEC_RESTORE_ZA
  UNSPEC_START_PRIVATE_ZA_CALL
  UNSPEC_END_PRIVATE_ZA_CALL
  UNSPEC_COMMIT_LAZY_SAVE
])

(define_c_enum "unspecv" [
  UNSPECV_ASM_UPDATE_ZA
  UNSPECV_ASM_UPDATE_ZT0
])

;; Use the ABI-defined routine to commit an uncommitted lazy save.
;; This relies on the current PSTATE.ZA, so depends on SME_STATE_REGNUM.
;; The fake TPIDR2_SETUP_REGNUM register initially holds the incoming
;; value of the architected TPIDR2_EL0.
(define_insn "aarch64_tpidr2_save"
  [(set (reg:DI ZA_FREE_REGNUM)
	(unspec:DI [(reg:DI SME_STATE_REGNUM)
		    (reg:DI TPIDR2_SETUP_REGNUM)] UNSPEC_TPIDR2_SAVE))
   (clobber (reg:DI R14_REGNUM))
   (clobber (reg:DI R15_REGNUM))
   (clobber (reg:DI R16_REGNUM))
   (clobber (reg:DI R17_REGNUM))
   (clobber (reg:DI R18_REGNUM))
   (clobber (reg:DI R30_REGNUM))
   (clobber (reg:CC CC_REGNUM))]
  ""
  "bl\t__arm_tpidr2_save"
  [(set_attr "is_call" "yes")]
)

;; Set PSTATE.ZA to 1.  If ZA was previously dormant or active,
;; it remains in the same state afterwards, with the same contents.
;; Otherwise, it goes from off to on with zeroed contents.
;;
;; Later writes of TPIDR2_EL0 to a nonzero value must not be moved
;; up past this instruction, since that could create an invalid
;; combination of having an active lazy save while ZA is off.
;; Create an anti-dependence by reading the current contents
;; of TPIDR2_SETUP_REGNUM.
;;
;; Making this depend on ZA_FREE_REGNUM ensures that contents belonging
;; to the caller have already been saved.  That isn't necessary for this
;; instruction itself, since PSTATE.ZA is already 1 if it contains data.
;; But doing this here means that other uses of ZA can just depend on
;; SME_STATE_REGNUM, rather than both SME_STATE_REGNUM and ZA_FREE_REGNUM.
(define_insn "aarch64_smstart_za"
  [(set (reg:DI SME_STATE_REGNUM)
	(const_int 1))
   (use (reg:DI TPIDR2_SETUP_REGNUM))
   (use (reg:DI ZA_FREE_REGNUM))]
  ""
  "smstart\tza"
)

;; Disable ZA and discard its current contents.
;;
;; The ABI says that the ZA save buffer must be null whenever PSTATE.ZA
;; is zero, so earlier writes to TPIDR2_EL0 must not be moved down past
;; this instruction.  Depend on TPIDR2_SETUP_REGNUM to ensure this.
;;
;; We can only turn ZA off once we know that it is free (i.e. doesn't
;; contain data belonging to the caller).  Depend on ZA_FREE_REGNUM
;; to ensure this.
;;
;; We only turn ZA off when the current function's ZA state is dead,
;; or perhaps if we're sure that the contents are saved.  Either way,
;; we know whether ZA is saved or not.
(define_insn "aarch64_smstop_za"
  [(set (reg:DI SME_STATE_REGNUM)
	(const_int 0))
   (set (reg:DI ZA_SAVED_REGNUM)
	(unspec:DI [(reg:DI TPIDR2_SETUP_REGNUM)
		    (reg:DI ZA_FREE_REGNUM)] UNSPEC_SMSTOP_ZA))]
  ""
  "smstop\tza"
)

;; Zero ZA after committing a lazy save.  The sequencing is enforced
;; by reading ZA_FREE_REGNUM.
(define_insn "aarch64_initial_zero_za"
  [(set (reg:DI ZA_REGNUM)
	(unspec:DI [(reg:DI SME_STATE_REGNUM)
		    (reg:DI ZA_FREE_REGNUM)] UNSPEC_INITIAL_ZERO_ZA))]
  ""
  "zero\t{ za }"
)

;; Initialize the abstract TPIDR2_BLOCK_REGNUM from the contents of
;; the current function's TPIDR2 block.  Other instructions can then
;; depend on TPIDR2_BLOCK_REGNUM rather than on the memory block.
(define_insn "aarch64_setup_local_tpidr2"
  [(set (reg:DI TPIDR2_BLOCK_REGNUM)
	(unspec:DI [(match_operand:V16QI 0 "memory_operand" "m")]
		   UNSPEC_SETUP_LOCAL_TPIDR2))]
  ""
  ""
  [(set_attr "type" "no_insn")]
)

;; Clear TPIDR2_EL0, cancelling any uncommitted lazy save.
(define_insn "aarch64_clear_tpidr2"
  [(set (reg:DI TPIDR2_SETUP_REGNUM)
	(const_int 0))]
  ""
  "msr\ttpidr2_el0, xzr"
)

;; Point TPIDR2_EL0 to the current function's TPIDR2 block, whose address
;; is given by operand 0.  TPIDR2_BLOCK_REGNUM represents the contents of the
;; pointed-to block.
(define_insn "aarch64_write_tpidr2"
  [(set (reg:DI TPIDR2_SETUP_REGNUM)
	(unspec:DI [(match_operand 0 "pmode_register_operand" "r")
		    (reg:DI TPIDR2_BLOCK_REGNUM)] UNSPEC_WRITE_TPIDR2))]
  ""
  "msr\ttpidr2_el0, %0"
)

;; Check whether ZA has been saved.  The system depends on the value that
;; we wrote to TPIDR2_EL0 previously, so it depends on TPDIR2_SETUP_REGNUM.
(define_insn "aarch64_read_tpidr2"
  [(set (match_operand:DI 0 "register_operand" "=r")
	(unspec:DI [(reg:DI TPIDR2_SETUP_REGNUM)
		    (reg:DI ZA_SAVED_REGNUM)] UNSPEC_READ_TPIDR2))]
  ""
  "mrs\t%0, tpidr2_el0"
)

;; Use the ABI-defined routine to restore lazy-saved ZA contents
;; from the TPIDR2 block pointed to by X0.  ZA must already be active.
(define_insn "aarch64_tpidr2_restore"
  [(set (reg:DI ZA_SAVED_REGNUM)
	(unspec:DI [(reg:DI R0_REGNUM)] UNSPEC_TPIDR2_RESTORE))
   (set (reg:DI SME_STATE_REGNUM)
	(unspec:DI [(reg:DI SME_STATE_REGNUM)] UNSPEC_TPIDR2_RESTORE))
   (clobber (reg:DI R14_REGNUM))
   (clobber (reg:DI R15_REGNUM))
   (clobber (reg:DI R16_REGNUM))
   (clobber (reg:DI R17_REGNUM))
   (clobber (reg:DI R18_REGNUM))
   (clobber (reg:DI R30_REGNUM))
   (clobber (reg:CC CC_REGNUM))]
  ""
  "bl\t__arm_tpidr2_restore"
  [(set_attr "is_call" "yes")]
)

;; Check whether a lazy save set up by aarch64_save_za was committed
;; and restore the saved contents if so.
;;
;; Operand 0 is the address of the current function's TPIDR2 block.
(define_insn_and_split "aarch64_restore_za"
  [(set (reg:DI ZA_SAVED_REGNUM)
	(unspec:DI [(match_operand 0 "pmode_register_operand" "r")
		    (reg:DI SME_STATE_REGNUM)
		    (reg:DI TPIDR2_SETUP_REGNUM)
		    (reg:DI ZA_SAVED_REGNUM)] UNSPEC_RESTORE_ZA))
   (clobber (reg:DI R0_REGNUM))
   (clobber (reg:DI R14_REGNUM))
   (clobber (reg:DI R15_REGNUM))
   (clobber (reg:DI R16_REGNUM))
   (clobber (reg:DI R17_REGNUM))
   (clobber (reg:DI R18_REGNUM))
   (clobber (reg:DI R30_REGNUM))
   (clobber (reg:CC CC_REGNUM))]
  ""
  "#"
  "&& epilogue_completed"
  [(const_int 0)]
  {
    auto label = gen_label_rtx ();
    auto tpidr2 = gen_rtx_REG (DImode, R16_REGNUM);
    emit_insn (gen_aarch64_read_tpidr2 (tpidr2));
    auto jump = emit_likely_jump_insn (gen_aarch64_cbnedi1 (tpidr2, label));
    JUMP_LABEL (jump) = label;

    aarch64_restore_za (operands[0]);
    emit_label (label);
    DONE;
  }
)

;; This instruction is emitted after asms that alter ZA, in order to model
;; the effect on dataflow.  The asm itself can't have ZA as an input or
;; an output, since there is no associated data type.  Instead it retains
;; the original "za" clobber, which on its own would indicate that ZA
;; is dead.
;;
;; The operand is a unique identifier.
(define_insn "aarch64_asm_update_za"
  [(set (reg:VNx16QI ZA_REGNUM)
	(unspec_volatile:VNx16QI
	  [(reg:VNx16QI ZA_REGNUM)
	   (reg:DI SME_STATE_REGNUM)
	   (match_operand 0 "const_int_operand")]
	  UNSPECV_ASM_UPDATE_ZA))]
  ""
  ""
  [(set_attr "type" "no_insn")]
)

;; A similar pattern for ZT0.
(define_insn "aarch64_asm_update_zt0"
  [(set (reg:V8DI ZT0_REGNUM)
	(unspec_volatile:V8DI
	  [(reg:V8DI ZT0_REGNUM)
	   (reg:DI SME_STATE_REGNUM)
	   (match_operand 0 "const_int_operand")]
	  UNSPECV_ASM_UPDATE_ZT0))]
  ""
  ""
  [(set_attr "type" "no_insn")]
)

;; This pseudo-instruction is emitted as part of a call to a private-ZA
;; function from a function with ZA state.  It marks a natural place to set
;; up a lazy save, if that turns out to be necessary.  The save itself
;; is managed by the mode-switching pass.
(define_insn "aarch64_start_private_za_call"
  [(set (reg:DI LOWERING_REGNUM)
	(unspec:DI [(reg:DI LOWERING_REGNUM)] UNSPEC_START_PRIVATE_ZA_CALL))]
  ""
  ""
  [(set_attr "type" "no_insn")]
)

;; This pseudo-instruction is emitted as part of a call to a private-ZA
;; function from a function with ZA state.  It marks a natural place to restore
;; the current function's ZA contents from the lazy save buffer, if that
;; turns out to be necessary.  The save itself is managed by the
;; mode-switching pass.
(define_insn "aarch64_end_private_za_call"
  [(set (reg:DI LOWERING_REGNUM)
	(unspec:DI [(reg:DI LOWERING_REGNUM)] UNSPEC_END_PRIVATE_ZA_CALL))]
  ""
  ""
  [(set_attr "type" "no_insn")]
)

;; =========================================================================
;; == Loads, stores and moves
;; =========================================================================

;; -------------------------------------------------------------------------
;; ---- Single-vector loads
;; -------------------------------------------------------------------------
;; Includes:
;; - LD1
;; - LDR
;; -------------------------------------------------------------------------

(define_c_enum "unspec" [
  UNSPEC_SME_LDR
])

(define_insn "@aarch64_sme_<optab><mode>"
  [(set (reg:SME_ZA_I ZA_REGNUM)
	(unspec:SME_ZA_I
	  [(reg:SME_ZA_I ZA_REGNUM)
	   (reg:DI SME_STATE_REGNUM)
	   (match_operand:DI 0 "const_int_operand")
	   (match_operand:SI 1 "register_operand" "Ucj")
	   (match_operand:<VPRED> 2 "register_operand" "Upl")
	   (match_operand:SME_ZA_I 3 "aarch64_sve_ldff1_operand" "Utf")]
	  SME_LD1))]
  "TARGET_STREAMING"
  "ld1<Vesize>\t{ za%0<hv>.<Vetype>[%w1, 0] }, %2/z, %3"
)

(define_insn "@aarch64_sme_<optab><mode>_plus"
  [(set (reg:SME_ZA_I ZA_REGNUM)
	(unspec:SME_ZA_I
	  [(reg:SME_ZA_I ZA_REGNUM)
	   (reg:DI SME_STATE_REGNUM)
	   (match_operand:DI 0 "const_int_operand")
	   (plus:SI (match_operand:SI 1 "register_operand" "Ucj")
		    (match_operand:SI 2 "const_int_operand"))
	   (match_operand:<VPRED> 3 "register_operand" "Upl")
	   (match_operand:SME_ZA_I 4 "aarch64_sve_ldff1_operand" "Utf")]
	  SME_LD1))]
  "TARGET_STREAMING
   && UINTVAL (operands[2]) < 128 / <elem_bits>"
  "ld1<Vesize>\t{ za%0<hv>.<Vetype>[%w1, %2] }, %3/z, %4"
)

(define_insn "aarch64_sme_ldr0"
  [(set (reg:VNx16QI ZA_REGNUM)
	(unspec:VNx16QI
	  [(reg:VNx16QI ZA_REGNUM)
	   (reg:DI SME_STATE_REGNUM)
	   (match_operand:SI 0 "register_operand" "Ucj")
	   (mem:VNx16QI (match_operand 1 "pmode_register_operand" "rk"))]
	  UNSPEC_SME_LDR))]
  "TARGET_SME"
  "ldr\tza[%w0, 0], [%1, #0, mul vl]"
)

(define_insn "@aarch64_sme_ldrn<mode>"
  [(set (reg:VNx16QI ZA_REGNUM)
	(unspec:VNx16QI
	  [(reg:VNx16QI ZA_REGNUM)
	   (reg:DI SME_STATE_REGNUM)
	   (plus:SI (match_operand:SI 0 "register_operand" "Ucj")
		    (match_operand:SI 1 "const_int_operand"))
	   (mem:VNx16QI
	     (plus:P (match_operand:P 2 "register_operand" "rk")
		     (match_operand:P 3 "aarch64_mov_operand")))]
	  UNSPEC_SME_LDR))]
  "TARGET_SME
   && aarch64_sme_ldr_vnum_offset_p (operands[1], operands[3])"
  "ldr\tza[%w0, %1], [%2, #%1, mul vl]"
)

;; -------------------------------------------------------------------------
;; ---- Table loads
;; -------------------------------------------------------------------------
;; Includes:
;; - LDR
;; -------------------------------------------------------------------------

(define_c_enum "unspec" [
  UNSPEC_RESTORE_ZT0
])

(define_insn "aarch64_sme_ldr_zt0"
  [(set (reg:V8DI ZT0_REGNUM)
	(match_operand:V8DI 0 "aarch64_sync_memory_operand" "Q"))
   (use (reg:DI SME_STATE_REGNUM))]
  "TARGET_SME2"
  "ldr\tzt0, %0"
)

;; This version is used after calls to private-ZA functions.  Since ZT0_REGNUM
;; represents the current function's state, it isn't clobbered by private-ZA
;; functions, so we need to make it depend on the ZA reinitialization code.
(define_insn "aarch64_restore_zt0"
  [(set (reg:V8DI ZT0_REGNUM)
	(unspec:V8DI
	  [(reg:DI SME_STATE_REGNUM)
	   (match_operand:V8DI 0 "aarch64_sync_memory_operand" "Q")]
	  UNSPEC_RESTORE_ZT0))]
  "TARGET_SME2"
  "ldr\tzt0, %0"
)

;; -------------------------------------------------------------------------
;; ---- Single-vector stores
;; -------------------------------------------------------------------------
;; Includes:
;; - ST1
;; - STR
;; -------------------------------------------------------------------------

(define_c_enum "unspec" [
  UNSPEC_SME_STR
])

(define_insn "@aarch64_sme_<optab><mode>"
  [(set (match_operand:SME_ZA_I 0 "aarch64_sve_ldff1_operand" "+Utf")
	(unspec:SME_ZA_I
	  [(reg:SME_ZA_I ZA_REGNUM)
	   (reg:DI SME_STATE_REGNUM)
	   (match_dup 0)
	   (match_operand:DI 1 "const_int_operand")
	   (match_operand:SI 2 "register_operand" "Ucj")
	   (match_operand:<VPRED> 3 "register_operand" "Upl")]
	  SME_ST1))]
  "TARGET_STREAMING"
  "st1<Vesize>\t{ za%1<hv>.<Vetype>[%w2, 0] }, %3, %0"
)

(define_insn "@aarch64_sme_<optab><mode>_plus"
  [(set (match_operand:SME_ZA_I 0 "aarch64_sve_ldff1_operand" "+Utf")
	(unspec:SME_ZA_I
	  [(reg:SME_ZA_I ZA_REGNUM)
	   (reg:DI SME_STATE_REGNUM)
	   (match_dup 0)
	   (match_operand:DI 1 "const_int_operand")
	   (plus:SI (match_operand:SI 2 "register_operand" "Ucj")
		    (match_operand:SI 3 "const_int_operand"))
	   (match_operand:<VPRED> 4 "register_operand" "Upl")]
	  SME_ST1))]
  "TARGET_STREAMING
   && UINTVAL (operands[3]) < 128 / <elem_bits>"
  "st1<Vesize>\t{ za%1<hv>.<Vetype>[%w2, %3] }, %4, %0"
)

(define_insn "aarch64_sme_str0"
  [(set (mem:VNx16QI (match_operand 1 "pmode_register_operand" "rk"))
	(unspec:VNx16QI
	  [(reg:VNx16QI ZA_REGNUM)
	   (reg:DI SME_STATE_REGNUM)
	   (mem:VNx16QI (match_dup 1))
	   (match_operand:SI 0 "register_operand" "Ucj")]
	  UNSPEC_SME_STR))]
  "TARGET_SME"
  "str\tza[%w0, 0], [%1, #0, mul vl]"
)

(define_insn "@aarch64_sme_strn<mode>"
  [(set (mem:VNx16QI
	  (plus:P (match_operand:P 2 "register_operand" "rk")
		  (match_operand:P 3 "aarch64_mov_operand")))
	(unspec:VNx16QI
	  [(reg:VNx16QI ZA_REGNUM)
	   (reg:DI SME_STATE_REGNUM)
	   (mem:VNx16QI (plus:P (match_dup 2) (match_dup 3)))
	   (plus:SI (match_operand:SI 0 "register_operand" "Ucj")
		    (match_operand:SI 1 "const_int_operand"))]
	  UNSPEC_SME_STR))]
  "TARGET_SME
   && aarch64_sme_ldr_vnum_offset_p (operands[1], operands[3])"
  "str\tza[%w0, %1], [%2, #%1, mul vl]"
)

;; -------------------------------------------------------------------------
;; ---- Table stores
;; -------------------------------------------------------------------------
;; Includes:
;; - STR
;; -------------------------------------------------------------------------

(define_insn "aarch64_sme_str_zt0"
  [(set (match_operand:V8DI 0 "aarch64_sync_memory_operand" "=Q")
	(reg:V8DI ZT0_REGNUM))
   (use (reg:DI SME_STATE_REGNUM))]
  "TARGET_SME2"
  "str\tzt0, %0"
)

;; -------------------------------------------------------------------------
;; ---- Single-vector moves
;; -------------------------------------------------------------------------
;; Includes:
;; - MOVA
;; - MOVAZ
;; -------------------------------------------------------------------------

(define_insn "@aarch64_sme_<optab><v_int_container><mode>"
  [(set (match_operand:SVE_FULL 0 "register_operand" "=w")
	(unspec:SVE_FULL
	  [(reg:<V_INT_CONTAINER> ZA_REGNUM)
	   (reg:DI SME_STATE_REGNUM)
	   (match_operand:SVE_FULL 1 "register_operand" "0")
	   (match_operand:<VPRED> 2 "register_operand" "Upl")
	   (match_operand:DI 3 "const_int_operand")
	   (match_operand:SI 4 "register_operand" "Ucj")]
	  SME_READ_HV))]
  "TARGET_STREAMING"
  "mova\t%0.<Vetype>, %2/m, za%3<hv>.<Vetype>[%w4, 0]"
)

(define_insn "*aarch64_sme_<optab><v_int_container><mode>_plus"
  [(set (match_operand:SVE_FULL 0 "register_operand" "=w")
	(unspec:SVE_FULL
	  [(reg:<V_INT_CONTAINER> ZA_REGNUM)
	   (reg:DI SME_STATE_REGNUM)
	   (match_operand:SVE_FULL 1 "register_operand" "0")
	   (match_operand:<VPRED> 2 "register_operand" "Upl")
	   (match_operand:DI 3 "const_int_operand")
	   (plus:SI (match_operand:SI 4 "register_operand" "Ucj")
		    (match_operand:SI 5 "const_int_operand"))]
	  SME_READ_HV))]
  "TARGET_STREAMING
   && UINTVAL (operands[5]) < 128 / <elem_bits>"
  "mova\t%0.<Vetype>, %2/m, za%3<hv>.<Vetype>[%w4, %5]"
)

(define_insn "@aarch64_sme_<optab><VNx1TI_ONLY:mode><SVE_FULL:mode>"
  [(set (match_operand:SVE_FULL 0 "register_operand" "=w")
	(unspec:SVE_FULL
	  [(reg:VNx1TI_ONLY ZA_REGNUM)
	   (reg:DI SME_STATE_REGNUM)
	   (match_operand:SVE_FULL 1 "register_operand" "0")
	   (match_operand:VNx2BI 2 "register_operand" "Upl")
	   (match_operand:DI 3 "const_int_operand")
	   (match_operand:SI 4 "register_operand" "Ucj")]
	  SME_READ_HV))]
  "TARGET_STREAMING"
  "mova\t%0.q, %2/m, za%3<hv>.q[%w4, 0]"
)

(define_insn "@aarch64_sme_<optab><v_int_container><mode>"
  [(set (match_operand:SVE_FULL 0 "register_operand" "=w")
	(unspec:SVE_FULL
	  [(reg:<V_INT_CONTAINER> ZA_REGNUM)
	   (reg:DI SME_STATE_REGNUM)
	   (match_operand:DI 1 "const_int_operand")
	   (match_operand:SI 2 "register_operand" "Ucj")
	   (const_int 0)]
	  SME_READZ_HV))
   (set (reg:<V_INT_CONTAINER> ZA_REGNUM)
	(unspec:<V_INT_CONTAINER>
	  [(reg:<V_INT_CONTAINER> ZA_REGNUM)
	   (reg:DI SME_STATE_REGNUM)
	   (match_dup 1)
	   (match_dup 2)
	   (const_int 1)]
	  SME_READZ_HV))]
  "TARGET_STREAMING_SME2p1"
  "movaz\t%0.<Vetype>, za%1<hv>.<Vetype>[%w2, 0]"
)

(define_insn "*aarch64_sme_<optab><v_int_container><mode>_plus"
  [(set (match_operand:SVE_FULL 0 "register_operand" "=w")
	(unspec:SVE_FULL
	  [(reg:<V_INT_CONTAINER> ZA_REGNUM)
	   (reg:DI SME_STATE_REGNUM)
	   (match_operand:DI 1 "const_int_operand")
	   (plus:SI (match_operand:SI 2 "register_operand" "Ucj")
		    (match_operand:SI 3 "const_int_operand"))
	   (const_int 0)]
	  SME_READZ_HV))
   (set (reg:<V_INT_CONTAINER> ZA_REGNUM)
	(unspec:<V_INT_CONTAINER>
	  [(reg:<V_INT_CONTAINER> ZA_REGNUM)
	   (reg:DI SME_STATE_REGNUM)
	   (match_dup 1)
	   (plus:SI (match_dup 2)
		    (match_dup 3))
	   (const_int 1)]
	  SME_READZ_HV))]
  "TARGET_STREAMING_SME2p1
   && UINTVAL (operands[3]) < 128 / <elem_bits>"
  "movaz\t%0.<Vetype>, za%1<hv>.<Vetype>[%w2, %3]"
)

(define_insn "@aarch64_sme_<optab><VNx1TI_ONLY:mode><SVE_FULL:mode>"
  [(set (match_operand:SVE_FULL 0 "register_operand" "=w")
	(unspec:SVE_FULL
	  [(reg:VNx1TI_ONLY ZA_REGNUM)
	   (reg:DI SME_STATE_REGNUM)
	   (match_operand:DI 1 "const_int_operand")
	   (match_operand:SI 2 "register_operand" "Ucj")
	   (const_int 0)]
	  SME_READZ_HV))
   (set (reg:VNx1TI_ONLY ZA_REGNUM)
	(unspec:VNx1TI_ONLY
	  [(reg:VNx1TI_ONLY ZA_REGNUM)
	   (reg:DI SME_STATE_REGNUM)
	   (match_dup 1)
	   (match_dup 2)
	   (const_int 0)]
	  SME_READZ_HV))]
  "TARGET_STREAMING_SME2p1"
  "movaz\t%0.q, za%1<hv>.q[%w2, 0]"
)

(define_insn "@aarch64_sme_<optab><v_int_container><mode>"
  [(set (reg:<V_INT_CONTAINER> ZA_REGNUM)
	(unspec:<V_INT_CONTAINER>
	  [(reg:SVE_FULL ZA_REGNUM)
	   (reg:DI SME_STATE_REGNUM)
	   (match_operand:DI 0 "const_int_operand")
	   (match_operand:SI 1 "register_operand" "Ucj")
	   (match_operand:<VPRED> 2 "register_operand" "Upl")
	   (match_operand:SVE_FULL 3 "register_operand" "w")]
	  SME_WRITE_HV))]
  "TARGET_STREAMING"
  "mova\tza%0<hv>.<Vetype>[%w1, 0], %2/m, %3.<Vetype>"
)

(define_insn "*aarch64_sme_<optab><v_int_container><mode>_plus"
  [(set (reg:<V_INT_CONTAINER> ZA_REGNUM)
	(unspec:<V_INT_CONTAINER>
	  [(reg:SVE_FULL ZA_REGNUM)
	   (reg:DI SME_STATE_REGNUM)
	   (match_operand:DI 0 "const_int_operand")
	   (plus:SI (match_operand:SI 1 "register_operand" "Ucj")
		    (match_operand:SI 2 "const_int_operand"))
	   (match_operand:<VPRED> 3 "register_operand" "Upl")
	   (match_operand:SVE_FULL 4 "register_operand" "w")]
	  SME_WRITE_HV))]
  "TARGET_STREAMING
   && UINTVAL (operands[2]) < 128 / <elem_bits>"
  "mova\tza%0<hv>.<Vetype>[%w1, %2], %3/m, %4.<Vetype>"
)

(define_insn "@aarch64_sme_<optab><VNx1TI_ONLY:mode><SVE_FULL:mode>"
  [(set (reg:VNx1TI_ONLY ZA_REGNUM)
	(unspec:VNx1TI_ONLY
	  [(reg:VNx1TI_ONLY ZA_REGNUM)
	   (reg:DI SME_STATE_REGNUM)
	   (match_operand:DI 0 "const_int_operand")
	   (match_operand:SI 1 "register_operand" "Ucj")
	   (match_operand:VNx2BI 2 "register_operand" "Upl")
	   (match_operand:SVE_FULL 3 "register_operand" "w")]
	  SME_WRITE_HV))]
  "TARGET_STREAMING"
  "mova\tza%0<hv>.q[%w1, 0], %2/m, %3.q"
)

;; -------------------------------------------------------------------------
;; ---- Multi-vector moves
;; -------------------------------------------------------------------------
;; Includes:
;; - MOVA
;; - MOVAZ
;; -------------------------------------------------------------------------

(define_insn "@aarch64_sme_<optab><mode><mode>"
  [(set (match_operand:SVE_FULLx24 0 "aligned_register_operand" "=Uw<vector_count>")
	(unspec:SVE_FULLx24
	  [(reg:SVE_FULLx24 ZA_REGNUM)
	   (reg:DI SME_STATE_REGNUM)
	   (match_operand:DI 1 "const_int_operand")
	   (match_operand:SI 2 "register_operand" "Ucj")]
	  SME_READ_HV))]
  "TARGET_STREAMING_SME2"
  {
    operands[3] = GEN_INT (<vector_count> - 1);
    return "mova\t%0, za%1<hv>.<Vetype>[%w2, 0:%3]";
  }
)

(define_insn "*aarch64_sme_<optab><mode><mode>_plus"
  [(set (match_operand:SVE_FULLx24 0 "aligned_register_operand" "=Uw<vector_count>")
	(unspec:SVE_FULLx24
	  [(reg:SVE_FULLx24 ZA_REGNUM)
	   (reg:DI SME_STATE_REGNUM)
	   (match_operand:DI 1 "const_int_operand")
	   (plus:SI
	     (match_operand:SI 2 "register_operand" "Ucj")
	     (match_operand:SI 3 "const_int_operand"))]
	  SME_READ_HV))]
  "TARGET_STREAMING_SME2
   && UINTVAL (operands[3]) % <vector_count> == 0
   && UINTVAL (operands[3]) < 128 / <elem_bits>"
  {
    operands[4] = GEN_INT (INTVAL (operands[3]) + <vector_count> - 1);
    return "mova\t%0, za%1<hv>.<Vetype>[%w2, %3:%4]";
  }
)

(define_insn "@aarch64_sme_<optab><mode><mode>"
  [(set (match_operand:SVE_FULLx24 0 "aligned_register_operand" "=Uw<vector_count>")
	(unspec:SVE_FULLx24
	  [(reg:SVE_FULLx24 ZA_REGNUM)
	   (reg:DI SME_STATE_REGNUM)
	   (match_operand:DI 1 "const_int_operand")
	   (match_operand:SI 2 "register_operand" "Ucj")
	   (const_int 0)]
	  SME_READZ_HV))
   (set (reg:SVE_FULLx24 ZA_REGNUM)
	(unspec:SVE_FULLx24
	  [(reg:SVE_FULLx24 ZA_REGNUM)
	   (reg:DI SME_STATE_REGNUM)
	   (match_dup 1)
	   (match_dup 2)
	   (const_int 1)]
	  SME_READZ_HV))]
  "TARGET_STREAMING_SME2p1"
  {
    operands[3] = GEN_INT (<vector_count> - 1);
    return "movaz\t%0, za%1<hv>.<Vetype>[%w2, 0:%3]";
  }
)

(define_insn "*aarch64_sme_<optab><mode><mode>_plus"
  [(set (match_operand:SVE_FULLx24 0 "aligned_register_operand" "=Uw<vector_count>")
	(unspec:SVE_FULLx24
	  [(reg:SVE_FULLx24 ZA_REGNUM)
	   (reg:DI SME_STATE_REGNUM)
	   (match_operand:DI 1 "const_int_operand")
	   (plus:SI
	     (match_operand:SI 2 "register_operand" "Ucj")
	     (match_operand:SI 3 "const_int_operand"))
	   (const_int 0)]
	  SME_READZ_HV))
   (set (reg:SVE_FULLx24 ZA_REGNUM)
	(unspec:SVE_FULLx24
	  [(reg:SVE_FULLx24 ZA_REGNUM)
	   (reg:DI SME_STATE_REGNUM)
	   (match_dup 1)
	   (plus:SI
	     (match_dup 2)
	     (match_dup 3))
	   (const_int 1)]
	  SME_READZ_HV))]
  "TARGET_STREAMING_SME2p1
   && UINTVAL (operands[3]) % <vector_count> == 0
   && UINTVAL (operands[3]) < 128 / <elem_bits>"
  {
    operands[4] = GEN_INT (INTVAL (operands[3]) + <vector_count> - 1);
    return "movaz\t%0, za%1<hv>.<Vetype>[%w2, %3:%4]";
  }
)

(define_insn "@aarch64_sme_read<mode>"
  [(set (match_operand:SVE_DIx24 0 "aligned_register_operand" "=Uw<vector_count>")
	(unspec:SVE_DIx24
	  [(reg:SVE_DIx24 ZA_REGNUM)
	   (reg:DI SME_STATE_REGNUM)
	   (match_operand:SI 1 "register_operand" "Uci")]
	  UNSPEC_SME_READ))]
  "TARGET_STREAMING_SME2"
  "mova\t%0, za.d[%w1, 0, vgx<vector_count>]"
)

(define_insn "*aarch64_sme_read<mode>_plus"
  [(set (match_operand:SVE_DIx24 0 "aligned_register_operand" "=Uw<vector_count>")
	(unspec:SVE_DIx24
	  [(reg:SVE_DIx24 ZA_REGNUM)
	   (reg:DI SME_STATE_REGNUM)
	   (plus:SI (match_operand:SI 1 "register_operand" "Uci")
		    (match_operand:SI 2 "const_0_to_7_operand"))]
	  UNSPEC_SME_READ))]
  "TARGET_STREAMING_SME2"
  "mova\t%0, za.d[%w1, %2, vgx<vector_count>]"
)

(define_insn "@aarch64_sme_readz<mode>"
  [(set (match_operand:SVE_DIx24 0 "aligned_register_operand" "=Uw<vector_count>")
	(unspec:SVE_DIx24
	  [(reg:SVE_DIx24 ZA_REGNUM)
	   (reg:DI SME_STATE_REGNUM)
	   (match_operand:SI 1 "register_operand" "Uci")
	   (const_int 0)]
	  UNSPEC_SME_READZ))
   (set (reg:SVE_DIx24 ZA_REGNUM)
	(unspec:SVE_DIx24
	  [(reg:SVE_DIx24 ZA_REGNUM)
	   (reg:DI SME_STATE_REGNUM)
	   (match_dup 1)
	   (const_int 1)]
	  UNSPEC_SME_READZ))]
  "TARGET_STREAMING_SME2p1"
  "movaz\t%0, za.d[%w1, 0, vgx<vector_count>]"
)

(define_insn "*aarch64_sme_readz<mode>_plus"
  [(set (match_operand:SVE_DIx24 0 "aligned_register_operand" "=Uw<vector_count>")
	(unspec:SVE_DIx24
	  [(reg:SVE_DIx24 ZA_REGNUM)
	   (reg:DI SME_STATE_REGNUM)
	   (plus:SI (match_operand:SI 1 "register_operand" "Uci")
		    (match_operand:SI 2 "const_0_to_7_operand"))
	   (const_int 0)]
	  UNSPEC_SME_READZ))
   (set (reg:SVE_DIx24 ZA_REGNUM)
	(unspec:SVE_DIx24
	  [(reg:SVE_DIx24 ZA_REGNUM)
	   (reg:DI SME_STATE_REGNUM)
	   (plus:SI (match_dup 1)
		    (match_dup 2))
	   (const_int 1)]
	  UNSPEC_SME_READZ))]
  "TARGET_STREAMING_SME2p1"
  "movaz\t%0, za.d[%w1, %2, vgx<vector_count>]"
)

(define_insn "@aarch64_sme_<optab><mode><mode>"
  [(set (reg:SVE_FULLx24 ZA_REGNUM)
	(unspec:SVE_FULLx24
	  [(reg:SVE_FULLx24 ZA_REGNUM)
	   (reg:DI SME_STATE_REGNUM)
	   (match_operand:DI 0 "const_int_operand")
	   (match_operand:SI 1 "register_operand" "Ucj")
	   (match_operand:SVE_FULLx24 2 "aligned_register_operand" "Uw<vector_count>")]
	  SME_WRITE_HV))]
  "TARGET_STREAMING_SME2"
  {
    operands[3] = GEN_INT (<vector_count> - 1);
    return "mova\tza%0<hv>.<Vetype>[%w1, 0:%3], %2";
  }
)

(define_insn "*aarch64_sme_<optab><mode><mode>_plus"
  [(set (reg:SVE_FULLx24 ZA_REGNUM)
	(unspec:SVE_FULLx24
	  [(reg:SVE_FULLx24 ZA_REGNUM)
	   (reg:DI SME_STATE_REGNUM)
	   (match_operand:DI 0 "const_int_operand")
	   (plus:SI
	     (match_operand:SI 1 "register_operand" "Ucj")
	     (match_operand:SI 2 "const_int_operand"))
	   (match_operand:SVE_FULLx24 3 "aligned_register_operand" "Uw<vector_count>")]
	  SME_WRITE_HV))]
  "TARGET_STREAMING_SME2
   && UINTVAL (operands[2]) % <vector_count> == 0
   && UINTVAL (operands[2]) < 128 / <elem_bits>"
  {
    operands[4] = GEN_INT (INTVAL (operands[2]) + <vector_count> - 1);
    return "mova\tza%0<hv>.<Vetype>[%w1, %2:%4], %3";
  }
)

(define_insn "@aarch64_sme_write<mode>"
  [(set (reg:SVE_DIx24 ZA_REGNUM)
	(unspec:SVE_DIx24
	  [(reg:SVE_DIx24 ZA_REGNUM)
	   (reg:DI SME_STATE_REGNUM)
	   (match_operand:SI 0 "register_operand" "Uci")
	   (match_operand:SVE_DIx24 1 "aligned_register_operand" "Uw<vector_count>")]
	  UNSPEC_SME_WRITE))]
  "TARGET_STREAMING_SME2"
  "mova\tza.d[%w0, 0, vgx<vector_count>], %1"
)

(define_insn "*aarch64_sme_write<mode>_plus"
  [(set (reg:SVE_DIx24 ZA_REGNUM)
	(unspec:SVE_DIx24
	  [(reg:SVE_DIx24 ZA_REGNUM)
	   (reg:DI SME_STATE_REGNUM)
	   (plus:SI (match_operand:SI 0 "register_operand" "Uci")
		    (match_operand:SI 1 "const_0_to_7_operand"))
	   (match_operand:SVE_DIx24 2 "aligned_register_operand" "Uw<vector_count>")]
	  UNSPEC_SME_WRITE))]
  "TARGET_STREAMING_SME2"
  "mova\tza.d[%w0, %1, vgx<vector_count>], %2"
)

;; -------------------------------------------------------------------------
;; ---- Zeroing
;; -------------------------------------------------------------------------
;; Includes:
;; - ZERO
;; -------------------------------------------------------------------------

(define_c_enum "unspec" [UNSPEC_SME_ZERO UNSPEC_SME_ZERO_SLICES])

(define_insn "aarch64_sme_zero_za"
  [(set (reg:VNx16QI ZA_REGNUM)
	(unspec:VNx16QI [(reg:VNx16QI ZA_REGNUM)
			 (reg:DI SME_STATE_REGNUM)
			 (match_operand:DI 0 "const_int_operand")]
			UNSPEC_SME_ZERO))]
  "TARGET_SME"
  {
    return aarch64_output_sme_zero_za (operands[0]);
  }
)

(define_insn "@aarch64_sme_zero_za_slices<mode>"
  [(set (reg:VNx16QI ZA_REGNUM)
	(unspec:VNx16QI
	  [(reg:VNx16QI ZA_REGNUM)
	   (reg:DI SME_STATE_REGNUM)
	   (scratch:SME_ZA_SDIx24)
	   (match_operand:SI 0 "register_operand" "Uci")]
	  UNSPEC_SME_ZERO_SLICES))]
  "TARGET_STREAMING_SME2p1"
  "zero\tza.d[%w0, 0, vgx<vector_count>]"
)

(define_insn "*aarch64_sme_zero_za_slices<mode>_plus"
  [(set (reg:VNx16QI ZA_REGNUM)
	(unspec:VNx16QI
	  [(reg:VNx16QI ZA_REGNUM)
	   (reg:DI SME_STATE_REGNUM)
	   (scratch:SME_ZA_SDIx24)
	   (plus:SI (match_operand:SI 0 "register_operand" "Uci")
		    (match_operand:SI 1 "const_0_to_7_operand"))]
	  UNSPEC_SME_ZERO_SLICES))]
  "TARGET_STREAMING_SME2p1"
  "zero\tza.d[%w0, %1, vgx<vector_count>]"
)

(define_insn "@aarch64_sme_zero_za_slices<mode>"
  [(set (reg:VNx16QI ZA_REGNUM)
	(unspec:VNx16QI
	  [(reg:VNx16QI ZA_REGNUM)
	   (reg:DI SME_STATE_REGNUM)
	   (scratch:SME_ZA_BHIx124)
	   (match_operand:SI 0 "register_operand" "Uci")]
	  UNSPEC_SME_ZERO_SLICES))]
  "TARGET_STREAMING_SME2p1"
  "zero\tza.d[%w0, 0:<za32_last_offset><vg_modifier>]"
)

(define_insn "*aarch64_sme_zero_za_slices<mode>_plus"
  [(set (reg:VNx16QI ZA_REGNUM)
	(unspec:VNx16QI
	  [(reg:VNx16QI ZA_REGNUM)
	   (reg:DI SME_STATE_REGNUM)
	   (scratch:SME_ZA_BHIx124)
	   (plus:SI (match_operand:SI 0 "register_operand" "Uci")
		    (match_operand:SI 1 "const_<za32_offset_range>_operand"))]
	  UNSPEC_SME_ZERO_SLICES))]
  "TARGET_STREAMING_SME2p1"
  {
    operands[2] = GEN_INT (INTVAL (operands[1]) + <za32_last_offset>);
    return "zero\tza.d[%w0, %1:%2<vg_modifier>]";
  }
)

(define_insn "aarch64_sme_zero_zt0"
  [(set (reg:V8DI ZT0_REGNUM)
	(const_int 0))
   (use (reg:DI SME_STATE_REGNUM))]
  "TARGET_SME2"
  "zero\t{ zt0 }"
)

;; =========================================================================
;; == Binary arithmetic
;; =========================================================================

;; -------------------------------------------------------------------------
;; ---- Binary arithmetic on ZA tile
;; -------------------------------------------------------------------------
;; Includes:
;; - ADDHA
;; - ADDVA
;; -------------------------------------------------------------------------

(define_insn "@aarch64_sme_<optab><mode>"
  [(set (reg:SME_ZA_SDI ZA_REGNUM)
	(unspec:SME_ZA_SDI
	  [(reg:SME_ZA_SDI ZA_REGNUM)
	   (reg:DI SME_STATE_REGNUM)
	   (match_operand:DI 0 "const_int_operand")
	   (match_operand:<VPRED> 1 "register_operand" "Upl")
	   (match_operand:<VPRED> 2 "register_operand" "Upl")
	   (match_operand:SME_ZA_SDI 3 "register_operand" "w")]
	  SME_BINARY_SDI))]
  "TARGET_STREAMING"
  "<optab>\tza%0.<Vetype>, %1/m, %2/m, %3.<Vetype>"
)

;; -------------------------------------------------------------------------
;; ---- Binary arithmetic on ZA slice
;; -------------------------------------------------------------------------
;; Includes:
;; - ADD
;; - BFADD
;; - BFSUB
;; - FADD
;; - FSUB
;; - SUB
;; -------------------------------------------------------------------------

(define_insn "@aarch64_sme_<optab><mode>"
  [(set (reg:SME_ZA_SDIx24 ZA_REGNUM)
	(unspec:SME_ZA_SDIx24
	  [(reg:SME_ZA_SDIx24 ZA_REGNUM)
	   (reg:DI SME_STATE_REGNUM)
	   (match_operand:SI 0 "register_operand" "Uci")
	   (match_operand:SME_ZA_SDIx24 1 "aligned_register_operand" "Uw<vector_count>")]
	  SME_BINARY_SLICE_SDI))]
  "TARGET_STREAMING_SME2"
  "<optab>\tza.<Vetype>[%w0, 0, vgx<vector_count>], %1"
)

(define_insn "*aarch64_sme_<optab><mode>_plus"
  [(set (reg:SME_ZA_SDIx24 ZA_REGNUM)
	(unspec:SME_ZA_SDIx24
	  [(reg:SME_ZA_SDIx24 ZA_REGNUM)
	   (reg:DI SME_STATE_REGNUM)
	   (plus:SI (match_operand:SI 0 "register_operand" "Uci")
		    (match_operand:SI 1 "const_0_to_7_operand"))
	   (match_operand:SME_ZA_SDIx24 2 "aligned_register_operand" "Uw<vector_count>")]
	  SME_BINARY_SLICE_SDI))]
  "TARGET_STREAMING_SME2"
  "<optab>\tza.<Vetype>[%w0, %1, vgx<vector_count>], %2"
)

(define_insn "@aarch64_sme_<optab><mode>"
  [(set (reg:SME_ZA_HSDFx24 ZA_REGNUM)
	(unspec:SME_ZA_HSDFx24
	  [(reg:SME_ZA_HSDFx24 ZA_REGNUM)
	   (reg:DI SME_STATE_REGNUM)
	   (match_operand:SI 0 "register_operand" "Uci")
	   (match_operand:SME_ZA_HSDFx24 1 "aligned_register_operand" "Uw<vector_count>")]
	  SME_BINARY_SLICE_HSDF))]
  "TARGET_STREAMING_SME2"
  "<b><optab>\tza.<Vetype>[%w0, 0, vgx<vector_count>], %1"
)

(define_insn "*aarch64_sme_<optab><mode>_plus"
  [(set (reg:SME_ZA_HSDFx24 ZA_REGNUM)
	(unspec:SME_ZA_HSDFx24
	  [(reg:SME_ZA_HSDFx24 ZA_REGNUM)
	   (reg:DI SME_STATE_REGNUM)
	   (plus:SI (match_operand:SI 0 "register_operand" "Uci")
		    (match_operand:SI 1 "const_0_to_7_operand"))
	   (match_operand:SME_ZA_HSDFx24 2 "aligned_register_operand" "Uw<vector_count>")]
	  SME_BINARY_SLICE_HSDF))]
  "TARGET_STREAMING_SME2"
  "<b><optab>\tza.<Vetype>[%w0, %1, vgx<vector_count>], %2"
)

;; -------------------------------------------------------------------------
;; ---- Binary arithmetic, writing to ZA slice
;; -------------------------------------------------------------------------
;; Includes:
;; - ADD
;; - SUB
;; -------------------------------------------------------------------------

(define_insn "@aarch64_sme_<optab><mode>"
  [(set (reg:SME_ZA_SDIx24 ZA_REGNUM)
	(unspec:SME_ZA_SDIx24
	  [(reg:SME_ZA_SDIx24 ZA_REGNUM)
	   (reg:DI SME_STATE_REGNUM)
	   (match_operand:SI 0 "register_operand" "Uci")
	   (match_operand:SME_ZA_SDIx24 1 "aligned_register_operand" "Uw<vector_count>")
	   (match_operand:SME_ZA_SDIx24 2 "aligned_register_operand" "Uw<vector_count>")]
	  SME_BINARY_WRITE_SLICE_SDI))]
  "TARGET_STREAMING_SME2"
  "<sme_int_op>\tza.<Vetype>[%w0, 0, vgx<vector_count>], %1, %2"
)

(define_insn "*aarch64_sme_<optab><mode>_plus"
  [(set (reg:SME_ZA_SDIx24 ZA_REGNUM)
	(unspec:SME_ZA_SDIx24
	  [(reg:SME_ZA_SDIx24 ZA_REGNUM)
	   (reg:DI SME_STATE_REGNUM)
	   (plus:SI (match_operand:SI 0 "register_operand" "Uci")
		    (match_operand:SI 1 "const_0_to_7_operand"))
	   (match_operand:SME_ZA_SDIx24 2 "aligned_register_operand" "Uw<vector_count>")
	   (match_operand:SME_ZA_SDIx24 3 "aligned_register_operand" "Uw<vector_count>")]
	  SME_BINARY_WRITE_SLICE_SDI))]
  "TARGET_STREAMING_SME2"
  "<sme_int_op>\tza.<Vetype>[%w0, %1, vgx<vector_count>], %2, %3"
)

(define_insn "@aarch64_sme_single_<optab><mode>"
  [(set (reg:SME_ZA_SDIx24 ZA_REGNUM)
	(unspec:SME_ZA_SDIx24
	  [(reg:SME_ZA_SDIx24 ZA_REGNUM)
	   (reg:DI SME_STATE_REGNUM)
	   (match_operand:SI 0 "register_operand" "Uci")
	   (match_operand:SME_ZA_SDIx24 1 "register_operand" "w")
	   (vec_duplicate:SME_ZA_SDIx24
	     (match_operand:<VSINGLE> 2 "register_operand" "x"))]
	  SME_BINARY_WRITE_SLICE_SDI))]
  "TARGET_STREAMING_SME2"
  "<sme_int_op>\tza.<Vetype>[%w0, 0, vgx<vector_count>], %1, %2.<Vetype>"
)

(define_insn "*aarch64_sme_single_<optab><mode>_plus"
  [(set (reg:SME_ZA_SDIx24 ZA_REGNUM)
	(unspec:SME_ZA_SDIx24
	  [(reg:SME_ZA_SDIx24 ZA_REGNUM)
	   (reg:DI SME_STATE_REGNUM)
	   (plus:SI (match_operand:SI 0 "register_operand" "Uci")
		    (match_operand:SI 1 "const_0_to_7_operand"))
	   (match_operand:SME_ZA_SDIx24 2 "register_operand" "w")
	   (vec_duplicate:SME_ZA_SDIx24
	     (match_operand:<VSINGLE> 3 "register_operand" "x"))]
	  SME_BINARY_WRITE_SLICE_SDI))]
  "TARGET_STREAMING_SME2"
  "<sme_int_op>\tza.<Vetype>[%w0, %1, vgx<vector_count>], %2, %3.<Vetype>"
)

;; =========================================================================
;; == Ternary arithmetic
;; =========================================================================

;; -------------------------------------------------------------------------
;; ---- [INT] Dot product
;; -------------------------------------------------------------------------
;; Includes:
;; - SDOT
;; - SUDOT
;; - UDOT
;; - USDOT
;; -------------------------------------------------------------------------

(define_insn "@aarch64_sme_<optab><SME_ZA_SDI:mode><SME_ZA_BHIx24:mode>"
  [(set (reg:SME_ZA_SDI ZA_REGNUM)
	(unspec:SME_ZA_SDI
	  [(reg:SME_ZA_SDI ZA_REGNUM)
	   (reg:DI SME_STATE_REGNUM)
	   (match_operand:SI 0 "register_operand" "Uci")
	   (match_operand:SME_ZA_BHIx24 1 "aligned_register_operand" "Uw<vector_count>")
	   (match_operand:SME_ZA_BHIx24 2 "aligned_register_operand" "Uw<vector_count>")]
	  SME_INT_DOTPROD))]
  "TARGET_STREAMING_SME2
   && (<SME_ZA_SDI:elem_bits> == 32 || <SME_ZA_BHIx24:elem_bits> == 16)
   && (<SME_ZA_BHIx24:elem_bits> == 8 || <has_16bit_form>)"
  "<optab>\tza.<SME_ZA_SDI:Vetype>[%w0, 0, vgx<vector_count>], %1, %2"
)

(define_insn "*aarch64_sme_<optab><SME_ZA_SDI:mode><SME_ZA_BHIx24:mode>_plus"
  [(set (reg:SME_ZA_SDI ZA_REGNUM)
	(unspec:SME_ZA_SDI
	  [(reg:SME_ZA_SDI ZA_REGNUM)
	   (reg:DI SME_STATE_REGNUM)
	   (plus:SI (match_operand:SI 0 "register_operand" "Uci")
		    (match_operand:SI 1 "const_0_to_7_operand"))
	   (match_operand:SME_ZA_BHIx24 2 "aligned_register_operand" "Uw<vector_count>")
	   (match_operand:SME_ZA_BHIx24 3 "aligned_register_operand" "Uw<vector_count>")]
	  SME_INT_DOTPROD))]
  "TARGET_STREAMING_SME2
   && (<SME_ZA_SDI:elem_bits> == 32 || <SME_ZA_BHIx24:elem_bits> == 16)
   && (<SME_ZA_BHIx24:elem_bits> == 8 || <has_16bit_form>)"
  "<optab>\tza.<SME_ZA_SDI:Vetype>[%w0, %1, vgx<vector_count>], %2, %3"
)

(define_insn "@aarch64_sme_single_<optab><SME_ZA_SDI:mode><SME_ZA_BHIx24:mode>"
  [(set (reg:SME_ZA_SDI ZA_REGNUM)
	(unspec:SME_ZA_SDI
	  [(reg:SME_ZA_SDI ZA_REGNUM)
	   (reg:DI SME_STATE_REGNUM)
	   (match_operand:SI 0 "register_operand" "Uci")
	   (match_operand:SME_ZA_BHIx24 1 "register_operand" "w")
	   (vec_duplicate:SME_ZA_BHIx24
	     (match_operand:<VSINGLE> 2 "register_operand" "x"))]
	  SME_INT_DOTPROD))]
  "TARGET_STREAMING_SME2
   && (<SME_ZA_SDI:elem_bits> == 32 || <SME_ZA_BHIx24:elem_bits> == 16)
   && (<SME_ZA_BHIx24:elem_bits> == 8 || <has_16bit_form>)"
  "<optab>\tza.<SME_ZA_SDI:Vetype>[%w0, 0, vgx<vector_count>], %1, %2.<SME_ZA_BHIx24:Vetype>"
)

(define_insn "*aarch64_sme_single_<optab><SME_ZA_SDI:mode><SME_ZA_BHIx24:mode>_plus"
  [(set (reg:SME_ZA_SDI ZA_REGNUM)
	(unspec:SME_ZA_SDI
	  [(reg:SME_ZA_SDI ZA_REGNUM)
	   (reg:DI SME_STATE_REGNUM)
	   (plus:SI (match_operand:SI 0 "register_operand" "Uci")
		    (match_operand:SI 1 "const_0_to_7_operand"))
	   (match_operand:SME_ZA_BHIx24 2 "register_operand" "w")
	   (vec_duplicate:SME_ZA_BHIx24
	     (match_operand:<VSINGLE> 3 "register_operand" "x"))]
	  SME_INT_DOTPROD))]
  "TARGET_STREAMING_SME2
   && (<SME_ZA_SDI:elem_bits> == 32 || <SME_ZA_BHIx24:elem_bits> == 16)
   && (<SME_ZA_BHIx24:elem_bits> == 8 || <has_16bit_form>)"
  "<optab>\tza.<SME_ZA_SDI:Vetype>[%w0, %1, vgx<vector_count>], %2, %3.<SME_ZA_BHIx24:Vetype>"
)

;; SUDOT is USDOT with the operands swapped.
(define_insn "@aarch64_sme_single_sudot<VNx4SI_ONLY:mode><SME_ZA_BIx24:mode>"
  [(set (reg:VNx4SI_ONLY ZA_REGNUM)
	(unspec:VNx4SI_ONLY
	  [(reg:VNx4SI_ONLY ZA_REGNUM)
	   (reg:DI SME_STATE_REGNUM)
	   (match_operand:SI 0 "register_operand" "Uci")
	   (vec_duplicate:SME_ZA_BIx24
	     (match_operand:<VSINGLE> 2 "register_operand" "x"))
	   (match_operand:SME_ZA_BIx24 1 "register_operand" "w")]
	  UNSPEC_SME_USDOT))]
  "TARGET_STREAMING_SME2"
  "sudot\tza.s[%w0, 0, vgx<vector_count>], %1, %2.b"
)

(define_insn "*aarch64_sme_single_sudot<VNx4SI_ONLY:mode><SME_ZA_BIx24:mode>_plus"
  [(set (reg:VNx4SI_ONLY ZA_REGNUM)
	(unspec:VNx4SI_ONLY
	  [(reg:VNx4SI_ONLY ZA_REGNUM)
	   (reg:DI SME_STATE_REGNUM)
	   (plus:SI (match_operand:SI 0 "register_operand" "Uci")
		    (match_operand:SI 1 "const_0_to_7_operand"))
	   (vec_duplicate:SME_ZA_BIx24
	     (match_operand:<VSINGLE> 3 "register_operand" "x"))
	   (match_operand:SME_ZA_BIx24 2 "register_operand" "w")]
	  UNSPEC_SME_USDOT))]
  "TARGET_STREAMING_SME2"
  "sudot\tza.s[%w0, %1, vgx<vector_count>], %2, %3.b"
)

(define_insn "@aarch64_sme_lane_<optab><SME_ZA_SDI:mode><SME_ZA_BHIx24:mode>"
  [(set (reg:SME_ZA_SDI ZA_REGNUM)
	(unspec:SME_ZA_SDI
	  [(reg:SME_ZA_SDI ZA_REGNUM)
	   (reg:DI SME_STATE_REGNUM)
	   (match_operand:SI 0 "register_operand" "Uci")
	   (match_operand:SME_ZA_BHIx24 1 "aligned_register_operand" "Uw<vector_count>")
	   (unspec:SME_ZA_BHIx24
	     [(match_operand:<VSINGLE> 2 "register_operand" "x")
	      (match_operand:SI 3 "const_int_operand")]
	     UNSPEC_SVE_LANE_SELECT)]
	  SME_INT_DOTPROD_LANE))]
  "TARGET_STREAMING_SME2
   && (<SME_ZA_SDI:elem_bits> == 32 || <SME_ZA_BHIx24:elem_bits> == 16)
   && (<SME_ZA_BHIx24:elem_bits> == 8 || <has_16bit_form>)"
  "<optab>\tza.<SME_ZA_SDI:Vetype>[%w0, 0, vgx<vector_count>], %1, %2.<SME_ZA_BHIx24:Vetype>[%3]"
)

(define_insn "*aarch64_sme_lane_<optab><SME_ZA_SDI:mode><SME_ZA_BHIx24:mode>_plus"
  [(set (reg:SME_ZA_SDI ZA_REGNUM)
	(unspec:SME_ZA_SDI
	  [(reg:SME_ZA_SDI ZA_REGNUM)
	   (reg:DI SME_STATE_REGNUM)
	   (plus:SI (match_operand:SI 0 "register_operand" "Uci")
		    (match_operand:SI 1 "const_0_to_7_operand"))
	   (match_operand:SME_ZA_BHIx24 2 "aligned_register_operand" "Uw<vector_count>")
	   (unspec:SME_ZA_BHIx24
	     [(match_operand:<VSINGLE> 3 "register_operand" "x")
	      (match_operand:SI 4 "const_int_operand")]
	     UNSPEC_SVE_LANE_SELECT)]
	  SME_INT_DOTPROD_LANE))]
  "TARGET_STREAMING_SME2
   && (<SME_ZA_SDI:elem_bits> == 32 || <SME_ZA_BHIx24:elem_bits> == 16)
   && (<SME_ZA_BHIx24:elem_bits> == 8 || <has_16bit_form>)"
  "<optab>\tza.<SME_ZA_SDI:Vetype>[%w0, %1, vgx<vector_count>], %2, %3.<SME_ZA_BHIx24:Vetype>[%4]"
)

;; -------------------------------------------------------------------------
;; ---- [INT] Ternary widening arithmetic on ZA slice
;; -------------------------------------------------------------------------
;; Includes:
;; - SMLA
;; - SMLS
;; - UMLA
;; - UMLS
;; -------------------------------------------------------------------------

(define_insn "@aarch64_sme_<optab><VNx4SI_ONLY:mode><SVE_FULL_BHI:mode>"
  [(set (reg:VNx4SI_ONLY ZA_REGNUM)
	(unspec:VNx4SI_ONLY
	  [(reg:VNx4SI_ONLY ZA_REGNUM)
	   (reg:DI SME_STATE_REGNUM)
	   (match_operand:SI 0 "register_operand" "Uci")
	   (match_operand:SVE_FULL_BHI 1 "register_operand" "w")
	   (match_operand:SVE_FULL_BHI 2 "register_operand" "x")]
	  SME_INT_TERNARY_SLICE))]
  "TARGET_STREAMING_SME2"
  "<optab><za32_long>\tza.s[%w0, 0:<za32_last_offset>], %1.<SVE_FULL_BHI:Vetype>, %2.<SVE_FULL_BHI:Vetype>"
)

(define_insn "*aarch64_sme_<optab><VNx4SI_ONLY:mode><SVE_FULL_BHI:mode>_plus"
  [(set (reg:VNx4SI_ONLY ZA_REGNUM)
	(unspec:VNx4SI_ONLY
	  [(reg:VNx4SI_ONLY ZA_REGNUM)
	   (reg:DI SME_STATE_REGNUM)
	   (plus:SI (match_operand:SI 0 "register_operand" "Uci")
		    (match_operand:SI 1 "const_<za32_offset_range>_operand"))
	   (match_operand:SVE_FULL_BHI 2 "register_operand" "w")
	   (match_operand:SVE_FULL_BHI 3 "register_operand" "x")]
	  SME_INT_TERNARY_SLICE))]
  "TARGET_STREAMING_SME2"
  {
    operands[4] = GEN_INT (INTVAL (operands[1]) + <za32_last_offset>);
    return "<optab><za32_long>\tza.s[%w0, %1:%4], %2.<SVE_FULL_BHI:Vetype>, %3.<SVE_FULL_BHI:Vetype>";
  }
)

(define_insn "@aarch64_sme_<optab><VNx4SI_ONLY:mode><SME_ZA_BHIx24:mode>"
  [(set (reg:VNx4SI_ONLY ZA_REGNUM)
	(unspec:VNx4SI_ONLY
	  [(reg:VNx4SI_ONLY ZA_REGNUM)
	   (reg:DI SME_STATE_REGNUM)
	   (match_operand:SI 0 "register_operand" "Uci")
	   (match_operand:SME_ZA_BHIx24 1 "aligned_register_operand" "Uw<vector_count>")
	   (match_operand:SME_ZA_BHIx24 2 "aligned_register_operand" "Uw<vector_count>")]
	  SME_INT_TERNARY_SLICE))]
  "TARGET_STREAMING_SME2"
  "<optab><za32_long>\tza.s[%w0, 0:<za32_last_offset>, vgx<vector_count>], %1, %2"
)

(define_insn "*aarch64_sme_<optab><VNx4SI_ONLY:mode><SME_ZA_BHIx24:mode>_plus"
  [(set (reg:VNx4SI_ONLY ZA_REGNUM)
	(unspec:VNx4SI_ONLY
	  [(reg:VNx4SI_ONLY ZA_REGNUM)
	   (reg:DI SME_STATE_REGNUM)
	   (plus:SI (match_operand:SI 0 "register_operand" "Uci")
		    (match_operand:SI 1 "const_<za32_offset_range>_operand"))
	   (match_operand:SME_ZA_BHIx24 2 "aligned_register_operand" "Uw<vector_count>")
	   (match_operand:SME_ZA_BHIx24 3 "aligned_register_operand" "Uw<vector_count>")]
	  SME_INT_TERNARY_SLICE))]
  "TARGET_STREAMING_SME2"
  {
    operands[4] = GEN_INT (INTVAL (operands[1]) + <za32_last_offset>);
    return "<optab><za32_long>\tza.s[%w0, %1:%4, vgx<vector_count>], %2, %3";
  }
)

(define_insn "@aarch64_sme_single_<optab><VNx4SI_ONLY:mode><SME_ZA_BHIx24:mode>"
  [(set (reg:VNx4SI_ONLY ZA_REGNUM)
	(unspec:VNx4SI_ONLY
	  [(reg:VNx4SI_ONLY ZA_REGNUM)
	   (reg:DI SME_STATE_REGNUM)
	   (match_operand:SI 0 "register_operand" "Uci")
	   (match_operand:SME_ZA_BHIx24 1 "register_operand" "w")
	   (vec_duplicate:SME_ZA_BHIx24
	     (match_operand:<SME_ZA_BHIx24:VSINGLE> 2 "register_operand" "x"))]
	  SME_INT_TERNARY_SLICE))]
  "TARGET_STREAMING_SME2"
  "<optab><za32_long>\tza.s[%w0, 0:<za32_last_offset>, vgx<vector_count>], %1, %2.<SME_ZA_BHIx24:Vetype>"
)

(define_insn "*aarch64_sme_single_<optab><VNx4SI_ONLY:mode><SME_ZA_BHIx24:mode>_plus"
  [(set (reg:VNx4SI_ONLY ZA_REGNUM)
	(unspec:VNx4SI_ONLY
	  [(reg:VNx4SI_ONLY ZA_REGNUM)
	   (reg:DI SME_STATE_REGNUM)
	   (plus:SI (match_operand:SI 0 "register_operand" "Uci")
		    (match_operand:SI 1 "const_<za32_offset_range>_operand"))
	   (match_operand:SME_ZA_BHIx24 2 "register_operand" "w")
	   (vec_duplicate:SME_ZA_BHIx24
	     (match_operand:<SME_ZA_BHIx24:VSINGLE> 3 "register_operand" "x"))]
	  SME_INT_TERNARY_SLICE))]
  "TARGET_STREAMING_SME2"
  {
    operands[4] = GEN_INT (INTVAL (operands[1]) + <za32_last_offset>);
    return "<optab><za32_long>\tza.s[%w0, %1:%4, vgx<vector_count>], %2, %3.<SME_ZA_BHIx24:Vetype>";
  }
)

(define_insn "@aarch64_sme_lane_<optab><VNx4SI_ONLY:mode><SME_ZA_BHIx124:mode>"
  [(set (reg:VNx4SI_ONLY ZA_REGNUM)
	(unspec:VNx4SI_ONLY
	  [(reg:VNx4SI_ONLY ZA_REGNUM)
	   (reg:DI SME_STATE_REGNUM)
	   (match_operand:SI 0 "register_operand" "Uci")
	   (match_operand:SME_ZA_BHIx124 1 "<aligned_operand>" "<aligned_fpr>")
	   (unspec:SME_ZA_BHIx124
	     [(match_operand:<VSINGLE> 2 "register_operand" "x")
	      (match_operand:SI 3 "const_int_operand")]
	     UNSPEC_SVE_LANE_SELECT)]
	  SME_INT_TERNARY_SLICE))]
  "TARGET_STREAMING_SME2"
  "<optab><za32_long>\tza.s[%w0, 0:<za32_last_offset><vg_modifier>], %1<z_suffix>, %2.<SME_ZA_BHIx124:Vetype>[%3]"
)

(define_insn "*aarch64_sme_lane_<optab><VNx4SI_ONLY:mode><SME_ZA_BHIx124:mode>"
  [(set (reg:VNx4SI_ONLY ZA_REGNUM)
	(unspec:VNx4SI_ONLY
	  [(reg:VNx4SI_ONLY ZA_REGNUM)
	   (reg:DI SME_STATE_REGNUM)
	   (plus:SI (match_operand:SI 0 "register_operand" "Uci")
		    (match_operand:SI 1 "const_<za32_offset_range>_operand"))
	   (match_operand:SME_ZA_BHIx124 2 "<aligned_operand>" "<aligned_fpr>")
	   (unspec:SME_ZA_BHIx124
	     [(match_operand:<VSINGLE> 3 "register_operand" "x")
	      (match_operand:SI 4 "const_int_operand")]
	     UNSPEC_SVE_LANE_SELECT)]
	  SME_INT_TERNARY_SLICE))]
  "TARGET_STREAMING_SME2"
  {
    operands[5] = GEN_INT (INTVAL (operands[1]) + <za32_last_offset>);
    return "<optab><za32_long>\tza.s[%w0, %1:%5<vg_modifier>], %2<z_suffix>, %3.<SME_ZA_BHIx124:Vetype>[%4]";
  }
)

(define_insn "@aarch64_sme_<optab><VNx2DI_ONLY:mode><VNx8HI_ONLY:mode>"
  [(set (reg:VNx2DI_ONLY ZA_REGNUM)
	(unspec:VNx2DI_ONLY
	  [(reg:VNx2DI_ONLY ZA_REGNUM)
	   (reg:DI SME_STATE_REGNUM)
	   (match_operand:SI 0 "register_operand" "Uci")
	   (match_operand:VNx8HI_ONLY 1 "register_operand" "w")
	   (match_operand:VNx8HI_ONLY 2 "register_operand" "x")]
	  SME_INT_TERNARY_SLICE))]
  "TARGET_STREAMING_SME2 && TARGET_SME_I16I64"
  "<optab>ll\tza.d[%w0, 0:3], %1.h, %2.h"
)

(define_insn "*aarch64_sme_<optab><VNx2DI_ONLY:mode><VNx8HI_ONLY:mode>_plus"
  [(set (reg:VNx2DI_ONLY ZA_REGNUM)
	(unspec:VNx2DI_ONLY
	  [(reg:VNx2DI_ONLY ZA_REGNUM)
	   (reg:DI SME_STATE_REGNUM)
	   (plus:SI (match_operand:SI 0 "register_operand" "Uci")
		    (match_operand:SI 1 "const_<za64_offset_range>_operand"))
	   (match_operand:VNx8HI_ONLY 2 "register_operand" "w")
	   (match_operand:VNx8HI_ONLY 3 "register_operand" "x")]
	  SME_INT_TERNARY_SLICE))]
  "TARGET_STREAMING_SME2 && TARGET_SME_I16I64"
  {
    operands[4] = GEN_INT (INTVAL (operands[1]) + 3);
    return "<optab>ll\tza.d[%w0, %1:%4], %2.h, %3.h";
  }
)

(define_insn "@aarch64_sme_<optab><VNx2DI_ONLY:mode><SME_ZA_HIx24:mode>"
  [(set (reg:VNx2DI_ONLY ZA_REGNUM)
	(unspec:VNx2DI_ONLY
	  [(reg:VNx2DI_ONLY ZA_REGNUM)
	   (reg:DI SME_STATE_REGNUM)
	   (match_operand:SI 0 "register_operand" "Uci")
	   (match_operand:SME_ZA_HIx24 1 "aligned_register_operand" "Uw<vector_count>")
	   (match_operand:SME_ZA_HIx24 2 "aligned_register_operand" "Uw<vector_count>")]
	  SME_INT_TERNARY_SLICE))]
  "TARGET_STREAMING_SME2 && TARGET_SME_I16I64"
  "<optab>ll\tza.d[%w0, 0:3, vgx<vector_count>], %1, %2"
)

(define_insn "*aarch64_sme_<optab><VNx2DI_ONLY:mode><SME_ZA_HIx24:mode>_plus"
  [(set (reg:VNx2DI_ONLY ZA_REGNUM)
	(unspec:VNx2DI_ONLY
	  [(reg:VNx2DI_ONLY ZA_REGNUM)
	   (reg:DI SME_STATE_REGNUM)
	   (plus:SI (match_operand:SI 0 "register_operand" "Uci")
		    (match_operand:SI 1 "const_<za64_offset_range>_operand"))
	   (match_operand:SME_ZA_HIx24 2 "aligned_register_operand" "Uw<vector_count>")
	   (match_operand:SME_ZA_HIx24 3 "aligned_register_operand" "Uw<vector_count>")]
	  SME_INT_TERNARY_SLICE))]
  "TARGET_STREAMING_SME2 && TARGET_SME_I16I64"
  {
    operands[4] = GEN_INT (INTVAL (operands[1]) + 3);
    return "<optab>ll\tza.d[%w0, %1:%4, vgx<vector_count>], %2, %3";
  }
)

(define_insn "@aarch64_sme_single_<optab><VNx2DI_ONLY:mode><SME_ZA_HIx24:mode>"
  [(set (reg:VNx2DI_ONLY ZA_REGNUM)
	(unspec:VNx2DI_ONLY
	  [(reg:VNx2DI_ONLY ZA_REGNUM)
	   (reg:DI SME_STATE_REGNUM)
	   (match_operand:SI 0 "register_operand" "Uci")
	   (match_operand:SME_ZA_HIx24 1 "register_operand" "w")
	   (vec_duplicate:SME_ZA_HIx24
	     (match_operand:<SME_ZA_HIx24:VSINGLE> 2 "register_operand" "x"))]
	  SME_INT_TERNARY_SLICE))]
  "TARGET_STREAMING_SME2 && TARGET_SME_I16I64"
  "<optab>ll\tza.d[%w0, 0:3, vgx<vector_count>], %1, %2.h"
)

(define_insn "*aarch64_sme_single_<optab><VNx2DI_ONLY:mode><SME_ZA_HIx24:mode>_plus"
  [(set (reg:VNx2DI_ONLY ZA_REGNUM)
	(unspec:VNx2DI_ONLY
	  [(reg:VNx2DI_ONLY ZA_REGNUM)
	   (reg:DI SME_STATE_REGNUM)
	   (plus:SI (match_operand:SI 0 "register_operand" "Uci")
		    (match_operand:SI 1 "const_<za64_offset_range>_operand"))
	   (match_operand:SME_ZA_HIx24 2 "register_operand" "w")
	   (vec_duplicate:SME_ZA_HIx24
	     (match_operand:<SME_ZA_HIx24:VSINGLE> 3 "register_operand" "x"))]
	  SME_INT_TERNARY_SLICE))]
  "TARGET_STREAMING_SME2 && TARGET_SME_I16I64"
  {
    operands[4] = GEN_INT (INTVAL (operands[1]) + 3);
    return "<optab>ll\tza.d[%w0, %1:%4, vgx<vector_count>], %2, %3.h";
  }
)

(define_insn "@aarch64_sme_lane_<optab><VNx2DI_ONLY:mode><SME_ZA_HIx124:mode>"
  [(set (reg:VNx2DI_ONLY ZA_REGNUM)
	(unspec:VNx2DI_ONLY
	  [(reg:VNx2DI_ONLY ZA_REGNUM)
	   (reg:DI SME_STATE_REGNUM)
	   (match_operand:SI 0 "register_operand" "Uci")
	   (match_operand:SME_ZA_HIx124 1 "<aligned_operand>" "<aligned_fpr>")
	   (unspec:SME_ZA_HIx124
	     [(match_operand:<VSINGLE> 2 "register_operand" "x")
	      (match_operand:SI 3 "const_int_operand")]
	     UNSPEC_SVE_LANE_SELECT)]
	  SME_INT_TERNARY_SLICE))]
  "TARGET_STREAMING_SME2 && TARGET_SME_I16I64"
  "<optab>ll\tza.d[%w0, 0:3<vg_modifier>], %1<z_suffix>, %2.h[%3]"
)

(define_insn "*aarch64_sme_lane_<optab><VNx2DI_ONLY:mode><SME_ZA_HIx124:mode>"
  [(set (reg:VNx2DI_ONLY ZA_REGNUM)
	(unspec:VNx2DI_ONLY
	  [(reg:VNx2DI_ONLY ZA_REGNUM)
	   (reg:DI SME_STATE_REGNUM)
	   (plus:SI (match_operand:SI 0 "register_operand" "Uci")
		    (match_operand:SI 1 "const_<za64_offset_range>_operand"))
	   (match_operand:SME_ZA_HIx124 2 "<aligned_operand>" "<aligned_fpr>")
	   (unspec:SME_ZA_HIx124
	     [(match_operand:<VSINGLE> 3 "register_operand" "x")
	      (match_operand:SI 4 "const_int_operand")]
	     UNSPEC_SVE_LANE_SELECT)]
	  SME_INT_TERNARY_SLICE))]
  "TARGET_STREAMING_SME2 && TARGET_SME_I16I64"
  {
    operands[5] = GEN_INT (INTVAL (operands[1]) + 3);
    return "<optab>ll\tza.d[%w0, %1:%5<vg_modifier>], %2<z_suffix>, %3.h[%4]";
  }
)

;; -------------------------------------------------------------------------
;; ---- [INT] Sum of outer products
;; -------------------------------------------------------------------------
;; - BMOPA
;; - BMOPS
;; - SMOPA
;; - SMOPS
;; - SUMOPA
;; - SUMOPS
;; - UMOPA
;; - UMOPS
;; - USMOPA
;; - USMOPS
;; -------------------------------------------------------------------------

(define_insn "@aarch64_sme_<optab><VNx4SI_ONLY:mode><VNx16QI_ONLY:mode>"
  [(set (reg:VNx4SI_ONLY ZA_REGNUM)
	(unspec:VNx4SI_ONLY
	  [(reg:VNx4SI_ONLY ZA_REGNUM)
	   (reg:DI SME_STATE_REGNUM)
	   (match_operand:DI 0 "const_int_operand")
	   (match_operand:<VNx4SI_ONLY:VPRED> 1 "register_operand" "Upl")
	   (match_operand:<VNx4SI_ONLY:VPRED> 2 "register_operand" "Upl")
	   (match_operand:VNx16QI_ONLY 3 "register_operand" "w")
	   (match_operand:VNx16QI_ONLY 4 "register_operand" "w")]
	  SME_INT_MOP))]
  "TARGET_STREAMING"
  "<optab>\tza%0.s, %1/m, %2/m, %3.b, %4.b"
)

(define_insn "@aarch64_sme_<optab><VNx2DI_ONLY:mode><VNx8HI_ONLY:mode>"
  [(set (reg:VNx2DI_ONLY ZA_REGNUM)
	(unspec:VNx2DI_ONLY
	  [(reg:VNx2DI_ONLY ZA_REGNUM)
	   (reg:DI SME_STATE_REGNUM)
	   (match_operand:DI 0 "const_int_operand")
	   (match_operand:<VNx2DI_ONLY:VPRED> 1 "register_operand" "Upl")
	   (match_operand:<VNx2DI_ONLY:VPRED> 2 "register_operand" "Upl")
	   (match_operand:VNx8HI_ONLY 3 "register_operand" "w")
	   (match_operand:VNx8HI_ONLY 4 "register_operand" "w")]
	  SME_INT_MOP))]
  "TARGET_STREAMING && TARGET_SME_I16I64"
  "<optab>\tza%0.d, %1/m, %2/m, %3.h, %4.h"
)

(define_insn "@aarch64_sme_<optab><VNx4SI_ONLY:mode><VNx8HI_ONLY:mode>"
  [(set (reg:VNx4SI_ONLY ZA_REGNUM)
	(unspec:VNx4SI_ONLY
	  [(reg:VNx4SI_ONLY ZA_REGNUM)
	   (reg:DI SME_STATE_REGNUM)
	   (match_operand:DI 0 "const_int_operand")
	   (match_operand:<VNx4SI_ONLY:VPRED> 1 "register_operand" "Upl")
	   (match_operand:<VNx4SI_ONLY:VPRED> 2 "register_operand" "Upl")
	   (match_operand:VNx8HI_ONLY 3 "register_operand" "w")
	   (match_operand:VNx8HI_ONLY 4 "register_operand" "w")]
	  SME2_INT_MOP))]
  "TARGET_STREAMING_SME2"
  "<optab>\tza%0.s, %1/m, %2/m, %3.h, %4.h"
)

(define_insn "@aarch64_sme_<optab><VNx4SI_ONLY:mode><VNx4SI_ONLY:mode>"
  [(set (reg:VNx4SI_ONLY ZA_REGNUM)
	(unspec:VNx4SI_ONLY
	  [(reg:VNx4SI_ONLY ZA_REGNUM)
	   (reg:DI SME_STATE_REGNUM)
	   (match_operand:DI 0 "const_int_operand")
	   (match_operand:<VNx4SI_ONLY:VPRED> 1 "register_operand" "Upl")
	   (match_operand:<VNx4SI_ONLY:VPRED> 2 "register_operand" "Upl")
	   (match_operand:VNx4SI_ONLY 3 "register_operand" "w")
	   (match_operand:VNx4SI_ONLY 4 "register_operand" "w")]
	  SME2_BMOP))]
  "TARGET_STREAMING_SME2"
  "<optab>\tza%0.s, %1/m, %2/m, %3.s, %4.s"
)

;; -------------------------------------------------------------------------
;; ---- [FP] Dot product
;; -------------------------------------------------------------------------
;; Includes:
;; - BFDOT
;; - FDOT
;; -------------------------------------------------------------------------

(define_insn "@aarch64_sme_<optab><VNx4SI_ONLY:mode><SME_ZA_HFx24:mode>"
  [(set (reg:VNx4SI_ONLY ZA_REGNUM)
	(unspec:VNx4SI_ONLY
	  [(reg:VNx4SI_ONLY ZA_REGNUM)
	   (reg:DI SME_STATE_REGNUM)
	   (match_operand:SI 0 "register_operand" "Uci")
	   (match_operand:SME_ZA_HFx24 1 "aligned_register_operand" "Uw<vector_count>")
	   (match_operand:SME_ZA_HFx24 2 "aligned_register_operand" "Uw<vector_count>")]
	  SME_FP_DOTPROD))]
  "TARGET_STREAMING_SME2"
  "<b><optab>\tza.s[%w0, 0, vgx<vector_count>], %1, %2"
)

(define_insn "*aarch64_sme_<optab><VNx4SI_ONLY:mode><SME_ZA_HFx24:mode>_plus"
  [(set (reg:VNx4SI_ONLY ZA_REGNUM)
	(unspec:VNx4SI_ONLY
	  [(reg:VNx4SI_ONLY ZA_REGNUM)
	   (reg:DI SME_STATE_REGNUM)
	   (plus:SI (match_operand:SI 0 "register_operand" "Uci")
		    (match_operand:SI 1 "const_0_to_7_operand"))
	   (match_operand:SME_ZA_HFx24 2 "aligned_register_operand" "Uw<vector_count>")
	   (match_operand:SME_ZA_HFx24 3 "aligned_register_operand" "Uw<vector_count>")]
	  SME_FP_DOTPROD))]
  "TARGET_STREAMING_SME2"
  "<b><optab>\tza.s[%w0, %1, vgx<vector_count>], %2, %3"
)

(define_insn "@aarch64_sme_single_<optab><VNx4SI_ONLY:mode><SME_ZA_HFx24:mode>"
  [(set (reg:VNx4SI_ONLY ZA_REGNUM)
	(unspec:VNx4SI_ONLY
	  [(reg:VNx4SI_ONLY ZA_REGNUM)
	   (reg:DI SME_STATE_REGNUM)
	   (match_operand:SI 0 "register_operand" "Uci")
	   (match_operand:SME_ZA_HFx24 1 "register_operand" "w")
	   (vec_duplicate:SME_ZA_HFx24
	     (match_operand:<VSINGLE> 2 "register_operand" "x"))]
	  SME_FP_DOTPROD))]
  "TARGET_STREAMING_SME2"
  "<b><optab>\tza.s[%w0, 0, vgx<vector_count>], %1, %2.h"
)

(define_insn "*aarch64_sme_single_<optab><VNx4SI_ONLY:mode><SME_ZA_HFx24:mode>_plus"
  [(set (reg:VNx4SI_ONLY ZA_REGNUM)
	(unspec:VNx4SI_ONLY
	  [(reg:VNx4SI_ONLY ZA_REGNUM)
	   (reg:DI SME_STATE_REGNUM)
	   (plus:SI (match_operand:SI 0 "register_operand" "Uci")
		    (match_operand:SI 1 "const_0_to_7_operand"))
	   (match_operand:SME_ZA_HFx24 2 "register_operand" "w")
	   (vec_duplicate:SME_ZA_HFx24
	     (match_operand:<VSINGLE> 3 "register_operand" "x"))]
	  SME_FP_DOTPROD))]
  "TARGET_STREAMING_SME2"
  "<b><optab>\tza.s[%w0, %1, vgx<vector_count>], %2, %3.h"
)

(define_insn "@aarch64_sme_lane_<optab><VNx4SI_ONLY:mode><SME_ZA_HFx24:mode>"
  [(set (reg:VNx4SI_ONLY ZA_REGNUM)
	(unspec:VNx4SI_ONLY
	  [(reg:VNx4SI_ONLY ZA_REGNUM)
	   (reg:DI SME_STATE_REGNUM)
	   (match_operand:SI 0 "register_operand" "Uci")
	   (match_operand:SME_ZA_HFx24 1 "aligned_register_operand" "Uw<vector_count>")
	   (unspec:SME_ZA_HFx24
	     [(match_operand:<VSINGLE> 2 "register_operand" "x")
	      (match_operand:SI 3 "const_int_operand")]
	     UNSPEC_SVE_LANE_SELECT)]
	  SME_FP_DOTPROD_LANE))]
  "TARGET_STREAMING_SME2"
  "<b><optab>\tza.s[%w0, 0, vgx<vector_count>], %1, %2.h[%3]"
)

(define_insn "*aarch64_sme_lane_<optab><VNx4SI_ONLY:mode><SME_ZA_HFx24:mode>_plus"
  [(set (reg:VNx4SI_ONLY ZA_REGNUM)
	(unspec:VNx4SI_ONLY
	  [(reg:VNx4SI_ONLY ZA_REGNUM)
	   (reg:DI SME_STATE_REGNUM)
	   (plus:SI (match_operand:SI 0 "register_operand" "Uci")
		    (match_operand:SI 1 "const_0_to_7_operand"))
	   (match_operand:SME_ZA_HFx24 2 "aligned_register_operand" "Uw<vector_count>")
	   (unspec:SME_ZA_HFx24
	     [(match_operand:<VSINGLE> 3 "register_operand" "x")
	      (match_operand:SI 4 "const_int_operand")]
	     UNSPEC_SVE_LANE_SELECT)]
	  SME_FP_DOTPROD_LANE))]
  "TARGET_STREAMING_SME2"
  "<b><optab>\tza.s[%w0, %1, vgx<vector_count>], %2, %3.h[%4]"
)

;; -------------------------------------------------------------------------
;; ---- [FP] Ternary arithmetic on ZA slice
;; -------------------------------------------------------------------------
;; Includes:
;; - BFMLA
;; - BFMLS
;; - FMLA
;; - FMLS
;; -------------------------------------------------------------------------

(define_insn "@aarch64_sme_<optab><mode><mode>"
  [(set (reg:SME_ZA_HSDFx24 ZA_REGNUM)
	(unspec:SME_ZA_HSDFx24
	  [(reg:SME_ZA_HSDFx24 ZA_REGNUM)
	   (reg:DI SME_STATE_REGNUM)
	   (match_operand:SI 0 "register_operand" "Uci")
	   (match_operand:SME_ZA_HSDFx24 1 "aligned_register_operand" "Uw<vector_count>")
	   (match_operand:SME_ZA_HSDFx24 2 "aligned_register_operand" "Uw<vector_count>")]
	  SME_FP_TERNARY_SLICE))]
  "TARGET_STREAMING_SME2"
  "<b><optab>\tza.<Vetype>[%w0, 0, vgx<vector_count>], %1, %2"
)

(define_insn "*aarch64_sme_<optab><mode><mode>_plus"
  [(set (reg:SME_ZA_HSDFx24 ZA_REGNUM)
	(unspec:SME_ZA_HSDFx24
	  [(reg:SME_ZA_HSDFx24 ZA_REGNUM)
	   (reg:DI SME_STATE_REGNUM)
	   (plus:SI (match_operand:SI 0 "register_operand" "Uci")
		    (match_operand:SI 1 "const_0_to_7_operand"))
	   (match_operand:SME_ZA_HSDFx24 2 "aligned_register_operand" "Uw<vector_count>")
	   (match_operand:SME_ZA_HSDFx24 3 "aligned_register_operand" "Uw<vector_count>")]
	  SME_FP_TERNARY_SLICE))]
  "TARGET_STREAMING_SME2"
  "<b><optab>\tza.<Vetype>[%w0, %1, vgx<vector_count>], %2, %3"
)

(define_insn "@aarch64_sme_single_<optab><mode><mode>"
  [(set (reg:SME_ZA_HSDFx24 ZA_REGNUM)
	(unspec:SME_ZA_HSDFx24
	  [(reg:SME_ZA_HSDFx24 ZA_REGNUM)
	   (reg:DI SME_STATE_REGNUM)
	   (match_operand:SI 0 "register_operand" "Uci")
	   (match_operand:SME_ZA_HSDFx24 1 "register_operand" "w")
	   (vec_duplicate:SME_ZA_HSDFx24
	     (match_operand:<SME_ZA_HSDFx24:VSINGLE> 2 "register_operand" "x"))]
	  SME_FP_TERNARY_SLICE))]
  "TARGET_STREAMING_SME2"
  "<b><optab>\tza.<Vetype>[%w0, 0, vgx<vector_count>], %1, %2.<Vetype>"
)

(define_insn "*aarch64_sme_single_<optab><mode><mode>_plus"
  [(set (reg:SME_ZA_HSDFx24 ZA_REGNUM)
	(unspec:SME_ZA_HSDFx24
	  [(reg:SME_ZA_HSDFx24 ZA_REGNUM)
	   (reg:DI SME_STATE_REGNUM)
	   (plus:SI (match_operand:SI 0 "register_operand" "Uci")
		    (match_operand:SI 1 "const_0_to_7_operand"))
	   (match_operand:SME_ZA_HSDFx24 2 "register_operand" "w")
	   (vec_duplicate:SME_ZA_HSDFx24
	     (match_operand:<SME_ZA_HSDFx24:VSINGLE> 3 "register_operand" "x"))]
	  SME_FP_TERNARY_SLICE))]
  "TARGET_STREAMING_SME2"
  "<b><optab>\tza.<Vetype>[%w0, %1, vgx<vector_count>], %2, %3.<Vetype>"
)

(define_insn "@aarch64_sme_lane_<optab><mode><mode>"
  [(set (reg:SME_ZA_HSDFx24 ZA_REGNUM)
	(unspec:SME_ZA_HSDFx24
	  [(reg:SME_ZA_HSDFx24 ZA_REGNUM)
	   (reg:DI SME_STATE_REGNUM)
	   (match_operand:SI 0 "register_operand" "Uci")
	   (match_operand:SME_ZA_HSDFx24 1 "aligned_register_operand" "Uw<vector_count>")
	   (unspec:SME_ZA_HSDFx24
	     [(match_operand:<SME_ZA_HSDFx24:VSINGLE> 2 "register_operand" "x")
	      (match_operand:SI 3 "const_int_operand")]
	     UNSPEC_SVE_LANE_SELECT)]
	  SME_FP_TERNARY_SLICE))]
  "TARGET_STREAMING_SME2"
  "<b><optab>\tza.<Vetype>[%w0, 0, vgx<vector_count>], %1, %2.<Vetype>[%3]"
)

(define_insn "*aarch64_sme_lane_<optab><mode><mode>"
  [(set (reg:SME_ZA_HSDFx24 ZA_REGNUM)
	(unspec:SME_ZA_HSDFx24
	  [(reg:SME_ZA_HSDFx24 ZA_REGNUM)
	   (reg:DI SME_STATE_REGNUM)
	   (plus:SI (match_operand:SI 0 "register_operand" "Uci")
		    (match_operand:SI 1 "const_0_to_7_operand"))
	   (match_operand:SME_ZA_HSDFx24 2 "aligned_register_operand" "Uw<vector_count>")
	   (unspec:SME_ZA_HSDFx24
	     [(match_operand:<SME_ZA_HSDFx24:VSINGLE> 3 "register_operand" "x")
	      (match_operand:SI 4 "const_int_operand")]
	     UNSPEC_SVE_LANE_SELECT)]
	  SME_FP_TERNARY_SLICE))]
  "TARGET_STREAMING_SME2"
  "<b><optab>\tza.<Vetype>[%w0, %1, vgx<vector_count>], %2, %3.<Vetype>[%4]"
)

;; -------------------------------------------------------------------------
;; ---- [FP] Ternary widening arithmetic on ZA slice
;; -------------------------------------------------------------------------
;; Includes:
;; - BFMLAL
;; - BFMLSL
;; - FMLAL
;; - FMLSL
;; -------------------------------------------------------------------------

(define_insn "@aarch64_sme_<optab><VNx4SI_ONLY:mode><SVE_FULL_HF:mode>"
  [(set (reg:VNx4SI_ONLY ZA_REGNUM)
	(unspec:VNx4SI_ONLY
	  [(reg:VNx4SI_ONLY ZA_REGNUM)
	   (reg:DI SME_STATE_REGNUM)
	   (match_operand:SI 0 "register_operand" "Uci")
	   (match_operand:SVE_FULL_HF 1 "register_operand" "w")
	   (match_operand:SVE_FULL_HF 2 "register_operand" "x")]
	  SME_FP_TERNARY_SLICE))]
  "TARGET_STREAMING_SME2"
  "<b><optab>l\tza.s[%w0, 0:1], %1.h, %2.h"
)

(define_insn "*aarch64_sme_<optab><VNx4SI_ONLY:mode><SVE_FULL_HF:mode>_plus"
  [(set (reg:VNx4SI_ONLY ZA_REGNUM)
	(unspec:VNx4SI_ONLY
	  [(reg:VNx4SI_ONLY ZA_REGNUM)
	   (reg:DI SME_STATE_REGNUM)
	   (plus:SI (match_operand:SI 0 "register_operand" "Uci")
		    (match_operand:SI 1 "const_<za32_offset_range>_operand"))
	   (match_operand:SVE_FULL_HF 2 "register_operand" "w")
	   (match_operand:SVE_FULL_HF 3 "register_operand" "x")]
	  SME_FP_TERNARY_SLICE))]
  "TARGET_STREAMING_SME2"
  {
    operands[4] = GEN_INT (INTVAL (operands[1]) + 1);
    return "<b><optab>l\tza.s[%w0, %1:%4], %2.h, %3.h";
  }
)

(define_insn "@aarch64_sme_<optab><VNx4SI_ONLY:mode><SME_ZA_HFx24:mode>"
  [(set (reg:VNx4SI_ONLY ZA_REGNUM)
	(unspec:VNx4SI_ONLY
	  [(reg:VNx4SI_ONLY ZA_REGNUM)
	   (reg:DI SME_STATE_REGNUM)
	   (match_operand:SI 0 "register_operand" "Uci")
	   (match_operand:SME_ZA_HFx24 1 "aligned_register_operand" "Uw<vector_count>")
	   (match_operand:SME_ZA_HFx24 2 "aligned_register_operand" "Uw<vector_count>")]
	  SME_FP_TERNARY_SLICE))]
  "TARGET_STREAMING_SME2"
  "<b><optab>l\tza.s[%w0, 0:1, vgx<vector_count>], %1, %2"
)

(define_insn "*aarch64_sme_<optab><VNx4SI_ONLY:mode><SME_ZA_HFx24:mode>_plus"
  [(set (reg:VNx4SI_ONLY ZA_REGNUM)
	(unspec:VNx4SI_ONLY
	  [(reg:VNx4SI_ONLY ZA_REGNUM)
	   (reg:DI SME_STATE_REGNUM)
	   (plus:SI (match_operand:SI 0 "register_operand" "Uci")
		    (match_operand:SI 1 "const_<za32_offset_range>_operand"))
	   (match_operand:SME_ZA_HFx24 2 "aligned_register_operand" "Uw<vector_count>")
	   (match_operand:SME_ZA_HFx24 3 "aligned_register_operand" "Uw<vector_count>")]
	  SME_FP_TERNARY_SLICE))]
  "TARGET_STREAMING_SME2"
  {
    operands[4] = GEN_INT (INTVAL (operands[1]) + 1);
    return "<b><optab>l\tza.s[%w0, %1:%4, vgx<vector_count>], %2, %3";
  }
)

(define_insn "@aarch64_sme_single_<optab><VNx4SI_ONLY:mode><SME_ZA_HFx24:mode>"
  [(set (reg:VNx4SI_ONLY ZA_REGNUM)
	(unspec:VNx4SI_ONLY
	  [(reg:VNx4SI_ONLY ZA_REGNUM)
	   (reg:DI SME_STATE_REGNUM)
	   (match_operand:SI 0 "register_operand" "Uci")
	   (match_operand:SME_ZA_HFx24 1 "register_operand" "w")
	   (vec_duplicate:SME_ZA_HFx24
	     (match_operand:<SME_ZA_HFx24:VSINGLE> 2 "register_operand" "x"))]
	  SME_FP_TERNARY_SLICE))]
  "TARGET_STREAMING_SME2"
  "<b><optab>l\tza.s[%w0, 0:1, vgx<vector_count>], %1, %2.h"
)

(define_insn "*aarch64_sme_single_<optab><VNx4SI_ONLY:mode><SME_ZA_HFx24:mode>_plus"
  [(set (reg:VNx4SI_ONLY ZA_REGNUM)
	(unspec:VNx4SI_ONLY
	  [(reg:VNx4SI_ONLY ZA_REGNUM)
	   (reg:DI SME_STATE_REGNUM)
	   (plus:SI (match_operand:SI 0 "register_operand" "Uci")
		    (match_operand:SI 1 "const_<za32_offset_range>_operand"))
	   (match_operand:SME_ZA_HFx24 2 "register_operand" "w")
	   (vec_duplicate:SME_ZA_HFx24
	     (match_operand:<SME_ZA_HFx24:VSINGLE> 3 "register_operand" "x"))]
	  SME_FP_TERNARY_SLICE))]
  "TARGET_STREAMING_SME2"
  {
    operands[4] = GEN_INT (INTVAL (operands[1]) + 1);
    return "<b><optab>l\tza.s[%w0, %1:%4, vgx<vector_count>], %2, %3.h";
  }
)

(define_insn "@aarch64_sme_lane_<optab><VNx4SI_ONLY:mode><SME_ZA_HFx124:mode>"
  [(set (reg:VNx4SI_ONLY ZA_REGNUM)
	(unspec:VNx4SI_ONLY
	  [(reg:VNx4SI_ONLY ZA_REGNUM)
	   (reg:DI SME_STATE_REGNUM)
	   (match_operand:SI 0 "register_operand" "Uci")
	   (match_operand:SME_ZA_HFx124 1 "<aligned_operand>" "<aligned_fpr>")
	   (unspec:SME_ZA_HFx124
	     [(match_operand:<VSINGLE> 2 "register_operand" "x")
	      (match_operand:SI 3 "const_int_operand")]
	     UNSPEC_SVE_LANE_SELECT)]
	  SME_FP_TERNARY_SLICE))]
  "TARGET_STREAMING_SME2"
  "<b><optab>l\tza.s[%w0, 0:1<vg_modifier>], %1<z_suffix>, %2.h[%3]"
)

(define_insn "*aarch64_sme_lane_<optab><VNx4SI_ONLY:mode><SME_ZA_HFx124:mode>"
  [(set (reg:VNx4SI_ONLY ZA_REGNUM)
	(unspec:VNx4SI_ONLY
	  [(reg:VNx4SI_ONLY ZA_REGNUM)
	   (reg:DI SME_STATE_REGNUM)
	   (plus:SI (match_operand:SI 0 "register_operand" "Uci")
		    (match_operand:SI 1 "const_<za32_offset_range>_operand"))
	   (match_operand:SME_ZA_HFx124 2 "<aligned_operand>" "<aligned_fpr>")
	   (unspec:SME_ZA_HFx124
	     [(match_operand:<VSINGLE> 3 "register_operand" "x")
	      (match_operand:SI 4 "const_int_operand")]
	     UNSPEC_SVE_LANE_SELECT)]
	  SME_FP_TERNARY_SLICE))]
  "TARGET_STREAMING_SME2"
  {
    operands[5] = GEN_INT (INTVAL (operands[1]) + 1);
    return "<b><optab>l\tza.s[%w0, %1:%5<vg_modifier>], %2<z_suffix>, %3.h[%4]";
  }
)

;; -------------------------------------------------------------------------
;; ---- [FP] Sum of outer products
;; -------------------------------------------------------------------------
;; Includes:
;; - BFMOPA
;; - BFMOPS
;; - FMOPA
;; - FMOPS
;; -------------------------------------------------------------------------

(define_insn "@aarch64_sme_<optab><mode><mode>"
  [(set (reg:SME_MOP_HSDF ZA_REGNUM)
	(unspec:SME_MOP_HSDF
	  [(reg:SME_MOP_HSDF ZA_REGNUM)
	   (reg:DI SME_STATE_REGNUM)
	   (match_operand:DI 0 "const_int_operand")
	   (match_operand:<VPRED> 1 "register_operand" "Upl")
	   (match_operand:<VPRED> 2 "register_operand" "Upl")
	   (match_operand:SME_MOP_HSDF 3 "register_operand" "w")
	   (match_operand:SME_MOP_HSDF 4 "register_operand" "w")]
	  SME_FP_MOP))]
  "TARGET_STREAMING"
  "<b><optab>\tza%0.<Vetype>, %1/m, %2/m, %3.<Vetype>, %4.<Vetype>"
)

(define_insn "@aarch64_sme_<optab><VNx4SI_ONLY:mode><SVE_FULL_HF:mode>"
  [(set (reg:VNx4SI_ONLY ZA_REGNUM)
	(unspec:VNx4SI_ONLY
	  [(reg:VNx4SI_ONLY ZA_REGNUM)
	   (reg:DI SME_STATE_REGNUM)
	   (match_operand:DI 0 "const_int_operand")
	   (match_operand:<VNx4SI_ONLY:VPRED> 1 "register_operand" "Upl")
	   (match_operand:<VNx4SI_ONLY:VPRED> 2 "register_operand" "Upl")
	   (match_operand:SVE_FULL_HF 3 "register_operand" "w")
	   (match_operand:SVE_FULL_HF 4 "register_operand" "w")]
	  SME_FP_MOP))]
  "TARGET_STREAMING"
  "<b><optab>\tza%0.<VNx4SI_ONLY:Vetype>, %1/m, %2/m, %3.<SVE_FULL_HF:Vetype>, %4.<SVE_FULL_HF:Vetype>"
)

;; =========================================================================
;; == Table lookup
;; =========================================================================

;; -------------------------------------------------------------------------
;; ---- Table lookup
;; -------------------------------------------------------------------------
;; Includes:
;; - LUTI2
;; - LUTI4
;; -------------------------------------------------------------------------

(define_c_enum "unspec" [
  UNSPEC_SME_LUTI
])

(define_insn "@aarch64_sme_lut<LUTI_BITS><mode>"
  [(set (match_operand:SVE_FULL_BHS 0 "register_operand" "=w")
	(unspec:SVE_FULL_BHS
	  [(reg:V8DI ZT0_REGNUM)
	   (reg:DI SME_STATE_REGNUM)
	   (match_operand:VNx16QI 1 "register_operand" "w")
	   (match_operand:DI 2 "const_int_operand")
	   (const_int LUTI_BITS)]
	  UNSPEC_SME_LUTI))]
  "TARGET_STREAMING_SME2"
  "luti<LUTI_BITS>\t%0.<Vetype>, zt0, %1[%2]"
)

(define_insn "@aarch64_sme_lut<LUTI_BITS><mode>"
  [(set (match_operand:SVE_BHSx24 0 "aligned_register_operand" "=Uw<vector_count>")
	(unspec:SVE_BHSx24
	  [(reg:V8DI ZT0_REGNUM)
	   (reg:DI SME_STATE_REGNUM)
	   (match_operand:VNx16QI 1 "register_operand" "w")
	   (match_operand:DI 2 "const_int_operand")
	   (const_int LUTI_BITS)]
	  UNSPEC_SME_LUTI))]
  "TARGET_STREAMING_SME2
   && !(<LUTI_BITS> == 4 && <vector_count> == 4 && <elem_bits> == 8)"
  "luti<LUTI_BITS>\t%0, zt0, %1[%2]"
)
