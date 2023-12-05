;; Machine description for AArch64 SME.
;; Copyright (C) 2023 Free Software Foundation, Inc.
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

;; This pseudo-instruction is emitted before a private-ZA function uses
;; PSTATE.ZA state for the first time.  The instruction checks whether
;; ZA currently contains data belonging to a caller and commits the
;; lazy save if so.
;;
;; Operand 0 is the incoming value of TPIDR2_EL0.  Operand 1 is nonzero
;; if ZA is live, and should therefore be zeroed after committing a save.
;;
;; The instruction is generated by the mode-switching pass.  It is a
;; define_insn_and_split rather than a define_expand because of the
;; internal control flow.
(define_insn_and_split "aarch64_commit_lazy_save"
  [(set (reg:DI ZA_FREE_REGNUM)
	(unspec:DI [(match_operand 0 "pmode_register_operand" "r")
		    (match_operand 1 "const_int_operand")
		    (reg:DI SME_STATE_REGNUM)
		    (reg:DI TPIDR2_SETUP_REGNUM)
		    (reg:VNx16QI ZA_REGNUM)] UNSPEC_COMMIT_LAZY_SAVE))
   (set (reg:DI ZA_REGNUM)
	(unspec:DI [(reg:DI SME_STATE_REGNUM)
		    (reg:DI ZA_FREE_REGNUM)] UNSPEC_INITIAL_ZERO_ZA))
   (clobber (reg:DI R14_REGNUM))
   (clobber (reg:DI R15_REGNUM))
   (clobber (reg:DI R16_REGNUM))
   (clobber (reg:DI R17_REGNUM))
   (clobber (reg:DI R18_REGNUM))
   (clobber (reg:DI R30_REGNUM))
   (clobber (reg:CC CC_REGNUM))]
  ""
  "#"
  "true"
  [(const_int 0)]
  {
    auto label = gen_label_rtx ();
    auto jump = emit_jump_insn (gen_aarch64_cbeqdi1 (operands[0], label));
    JUMP_LABEL (jump) = label;
    emit_insn (gen_aarch64_tpidr2_save ());
    emit_insn (gen_aarch64_clear_tpidr2 ());
    if (INTVAL (operands[1]) != 0)
      emit_insn (gen_aarch64_initial_zero_za ());
    emit_label (label);
    DONE;
  }
)
